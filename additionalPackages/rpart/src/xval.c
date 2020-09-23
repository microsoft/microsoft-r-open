/*
 * Cross validate a model.  This routine is responsible for filling in
 *  two vectors -- xrisk = cross-validated risk estimate
 *                 xstd  = std of xrisk
 *
 * Basic method is to use a stratified partitioning of the data (NOT random)
 *  into n_xval subgroups.  One by one, each of these groups is left out of
 *  the partitioning by setting 'which' to 0.  After partitioning, the risk
 *  of each left out subject is determined, under each of the unique
 *  complexity parameters.
 * The x-groups are set by the calling S-routine, so they can actually be
 *  random, non-random, or whatever, as far as this routine is concerned.
 *
 *  n_xval: number of cross-validation subsets
 *  cptable: head of the complexity parameter table, were results will be
 *              stored
 *  x_grp(n): defines the groups.  Integers from 1 to n_xval
 *  maxcat  : max # categories, in any given categorical variable
 *  errmsg   : possible error message
 *  parms   : vector of input parameters, initializers for the splitting rule
 *  savesort: saved version of rp.sorts
 */
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

#ifndef DEBUG
# define DEBUG 0
#endif
#if DEBUG > 1
static int debug = 0;           /*if it is odd, print out every tree */
			/*if >= 2, print out every risk value we see */
#endif

void
xval(int n_xval, CpTable cptable_head, int *x_grp,
     int maxcat, char **errmsg, double *parms, int *savesort)
{
    int i, j, k, ii, jj;
    int last;
    int xgroup;
    double *xtemp, *xpred;
    int *savew;
    double *cp;
    double alphasave;
    pNode xtree;
    CpTable cplist;
    double temp;
    double old_wt, total_wt;

    alphasave = rp.alpha;

   /*
    * Allocate a set of temporary arrays
    */
    xtemp = (double *) CALLOC(3 * rp.num_unique_cp, sizeof(double));
    xpred = xtemp + rp.num_unique_cp;
    cp = xpred + rp.num_unique_cp;
    savew = (int *) CALLOC(rp.n, sizeof(int));
    for (i = 0; i < rp.n; i++)
	savew[i] = rp.which[i]; /* restore at the end */

   /*
    * Make the list of CPs that I will compare against
    */
    cp[0] = 10 * cptable_head->cp;      /* close enough to infinity */
    for (cplist = cptable_head, i = 1; i < rp.num_unique_cp;
	 cplist = cplist->forward, i++)
	cp[i] = sqrt(cplist->cp * (cplist->forward)->cp);
    total_wt = 0;
    for (i = 0; i < rp.n; i++)
	total_wt += rp.wt[i];
    old_wt = total_wt;

   /*
    * do the validations
    */
    k = 0;                      /* -Wall */
    for (xgroup = 0; xgroup < n_xval; xgroup++) {
       /*
	* restore rp.sorts, with the data for this run at the top
	* this requires one pass per variable
	*/
	for (j = 0; j < rp.nvar; j++) {
	    k = 0;
	    for (i = 0; i < rp.n; i++) {
		ii = savesort[j * rp.n + i];
		if (ii < 0)
		    ii = -(1 + ii);     /* missings move too */
		if (x_grp[ii] != xgroup + 1) {
		   /*
		    * this obs is left in --
		    *  copy to the front half of rp.sorts
		    */
		    rp.sorts[j][k] = savesort[j * rp.n + i];
		    k++;
		}
	    }
	}

       /*
	*  Fix up the y vector, and save a list of "left out" obs *   in
	* the tail, unused end of rp.sorts[0][i];
	*/
	last = k;
	k = 0;
	temp = 0;
	for (i = 0; i < rp.n; i++) {
	    rp.which[i] = 1;    /* everyone starts in group 1 */
	    if (x_grp[i] == xgroup + 1) {
		rp.sorts[0][last] = i;
		last++;
	    } else {
		rp.ytemp[k] = rp.ydata[i];
		rp.wtemp[k] = rp.wt[i];
		temp += rp.wt[i];
		k++;
	    }
	}

       /* at this point k = #obs in the xval group */
       /* rescale the cp */
	for (j = 0; j < rp.num_unique_cp; j++)
	    cp[j] *= temp / old_wt;
	rp.alpha *= temp / old_wt;
	old_wt = temp;


       /*
	* partition the new tree
	*/
	xtree = (pNode) CALLOC(1, nodesize);
	xtree->num_obs = k;
	(*rp_init) (k, rp.ytemp, maxcat, errmsg, parms, &temp, 2, rp.wtemp);
	(*rp_eval) (k, rp.ytemp, xtree->response_est, &(xtree->risk), rp.wtemp);
	xtree->complexity = xtree->risk;
	partition(1, xtree, &temp, 0, k);
	fix_cp(xtree, xtree->complexity);

       /*
	* run the extra data down the new tree
	*/
	for (i = k; i < rp.n; i++) {
	    j = rp.sorts[0][i];
	    rundown(xtree, j, cp, xpred, xtemp);
#if DEBUG > 1
	    if (debug > 1) {
		jj = j + 1;
		Rprintf("\nObs %d, y=%f \n", jj, rp.ydata[j][0]);
	    }
#endif
	   /* add it in to the risk */
	    cplist = cptable_head;
	    for (jj = 0; jj < rp.num_unique_cp; jj++) {
		cplist->xrisk += xtemp[jj] * rp.wt[j];
		cplist->xstd += xtemp[jj] * xtemp[jj] * rp.wt[j];
#if DEBUG > 1
		if (debug > 1)
		    Rprintf("  cp=%f, pred=%f, xtemp=%f\n",
			    cp[jj] / old_wt, xpred[jj], xtemp[jj]);
#endif
		cplist = cplist->forward;
	    }
	}
	free_tree(xtree, 1);    // Calloc-ed
	R_CheckUserInterrupt();
    }

    for (cplist = cptable_head; cplist; cplist = cplist->forward) {
	cplist->xstd = sqrt(cplist->xstd -
			    cplist->xrisk * cplist->xrisk / total_wt);
    }
    rp.alpha = alphasave;
    for (i = 0; i < rp.n; i++)
	rp.which[i] = savew[i];
    Free(savew);
    Free(xtemp);
}
