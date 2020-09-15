/*
 *  This has almost exactly the same inputs are rpart, but returns
 *   cross-validated predictions instead of the fitted tree.
 *
 * Input variables:
 *      ncat    = # categories for each var, 0 for continuous variables.
 *      method  = 1 - anova
 *                2 - exponential survival
 *		  3 - classification
 *	          4 - user defined callback
 *      opt     = vector of options.  Same order as rpart.control, as a vector
 *                   of doubles.
 *      parms   = extra parameters for the split function, e.g. poissoninit
 *      xvals    = number of cross-validations to do
 *      xgrp     = indices for the cross-validations
 *      ymat    = vector of response variables
 *      xmat    = matrix of continuous variables
 *      ny      = number of columns of the y matrix (it is passed in as a
 *                  vector)
 *      wt       = vector of case weights
 *      all     = if 1 return all the predictions, otherwise just the
 *                 first element
 *      cp      = vector of cp values to use as cut points
 *      toprisk = risk for the top node of the tree
 *      nresp   = number of response values
 */
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "func_table.h"
#include "rpartproto.h"

SEXP
xpred(SEXP ncat2, SEXP method2, SEXP opt2,
      SEXP parms2, SEXP xvals2, SEXP xgrp2,
      SEXP ymat2, SEXP xmat2, SEXP wt2,
      SEXP ny2, SEXP cost2, SEXP all2, SEXP cp2, SEXP toprisk2, SEXP nresp2)
{
    char *errmsg;
    int i, j, k, n;
    int last, ii;
    int maxcat, ncp;
    int xgroup;
    double temp, total_wt, old_wt;
    int *savesort;
    double *dptr;               /* temp */
    int nresp;
    double toprisk;

    pNode xtree;
    /*
     * pointers to R objects
     */
    int *ncat, *xgrp;
    int xvals;
    double *wt, *parms;
    double *predict;
    double *cp;

    /*
     *        Return objects for R
     */
    SEXP predict2;

    /*
     *  the first half of the routine is almost identical to rpart.c
     * first get copies of some input variables
     */
    ncat = INTEGER(ncat2);
    xgrp = INTEGER(xgrp2);
    xvals = asInteger(xvals2);
    wt = REAL(wt2);
    parms = REAL(parms2);
    ncp = LENGTH(cp2);
    cp = REAL(cp2);
    toprisk = asReal(toprisk2);

    /*
     * initialize the splitting functions from the function table
     */
    if (asInteger(method2) <= NUM_METHODS) {
	i = asInteger(method2) - 1;
	rp_init = func_table[i].init_split;
	rp_choose = func_table[i].choose_split;
	rp_eval = func_table[i].eval;
	rp_error = func_table[i].error;
	rp.num_y = asInteger(ny2);
    } else
	error(_("Invalid value for 'method'"));

    /*
     * set some other parameters
     */
    dptr = REAL(opt2);
    rp.min_node = (int) dptr[1];
    rp.min_split = (int) dptr[0];
    rp.complexity = dptr[2];
    rp.maxpri = (int) dptr[3] + 1;      /* max primary splits = max
					 * competitors + 1 */
    if (rp.maxpri < 1)
	rp.maxpri = 1;
    rp.maxsur = (int) dptr[4];
    rp.usesurrogate = (int) dptr[5];
    rp.sur_agree = (int) dptr[6];
    rp.maxnode = (int) pow((double) 2.0, (double) dptr[7]) - 1;
    rp.n = nrows(xmat2);
    n = rp.n;                   /* I get tired of typing "rp.n" 100 times
				 * below */
    rp.nvar = ncols(xmat2);
    rp.numcat = INTEGER(ncat2);
    rp.wt = wt;
    rp.iscale = 0.0;
    rp.vcost = REAL(cost2);
    rp.num_resp = asInteger(nresp2);

    /*
     * create the "ragged array" pointers to the matrix
     *   x and missmat are in column major order
     *   y is in row major order
     */
    dptr = REAL(xmat2);
    rp.xdata = (double **) ALLOC(rp.nvar, sizeof(double *));
    for (i = 0; i < rp.nvar; i++) {
	rp.xdata[i] = dptr;
	dptr += n;
    }
    rp.ydata = (double **) ALLOC(n, sizeof(double *));

    dptr = REAL(ymat2);
    for (i = 0; i < n; i++) {
	rp.ydata[i] = dptr;
	dptr += rp.num_y;
    }
    /*
     * allocate some scratch
     */
    rp.tempvec = (int *) ALLOC(n, sizeof(int));
    rp.xtemp = (double *) ALLOC(n, sizeof(double));
    rp.ytemp = (double **) ALLOC(n, sizeof(double *));
    rp.wtemp = (double *) ALLOC(n, sizeof(double));

    /*
     * create a matrix of sort indices, one for each continuous variable
     *   This sort is "once and for all".
     * I don't have to sort the categoricals.
     */
    rp.sorts = (int **) ALLOC(rp.nvar, sizeof(int *));
    rp.sorts[0] = (int *) ALLOC(n * rp.nvar, sizeof(int));
    maxcat = 0;
    for (i = 0; i < rp.nvar; i++) {
	rp.sorts[i] = rp.sorts[0] + i * n;
	for (k = 0; k < n; k++) {
	    if (!R_FINITE(rp.xdata[i][k])) {
		rp.tempvec[k] = -(k + 1);       /* this variable is missing */
		rp.xtemp[k] = 0;        /* avoid weird numerics in S's NA */
	    } else {
		rp.tempvec[k] = k;
		rp.xtemp[k] = rp.xdata[i][k];
	    }
	}
	if (ncat[i] == 0)
	    mysort(0, n - 1, rp.xtemp, rp.tempvec);
	else if (ncat[i] > maxcat)
	    maxcat = ncat[i];
	for (k = 0; k < n; k++)
	    rp.sorts[i][k] = rp.tempvec[k];
    }

    /*
     * save away a copy of the rp.sorts
     */
    savesort = (int *) ALLOC(n * rp.nvar, sizeof(int));
    memcpy(savesort, rp.sorts[0], n * rp.nvar * sizeof(int));

    /*
     * And now the last of my scratch space
     */
    if (maxcat > 0) {
	rp.csplit = (int *) ALLOC(3 * maxcat, sizeof(int));
	rp.lwt = (double *) ALLOC(2 * maxcat, sizeof(double));
	rp.left = rp.csplit + maxcat;
	rp.right = rp.left + maxcat;
	rp.rwt = rp.lwt + maxcat;
    } else
	rp.csplit = (int *) ALLOC(1, sizeof(int));

    /*
     * Initialize the top node
     */

    rp.which = (int *) ALLOC(n, sizeof(int));
    xtree = (pNode) ALLOC(1, nodesize);
    (*rp_init) (n, rp.ydata, maxcat, &errmsg, parms, &rp.num_resp, 1, wt);

    /*
     * From this point on we look much more like xval.c
     */
    rp.alpha = rp.complexity * toprisk;
    for (i = 0; i < ncp; i++)
	cp[i] *= toprisk;       /* scale to internal units */

    /*
     *        allocate the output vector
     */
    if (asInteger(all2) == 1)
	nresp = rp.num_resp;    /* number returned */
    else
	nresp = 1;
    predict2 = PROTECT(allocVector(REALSXP, n * ncp * nresp));
    predict = REAL(predict2);

    /*
     * do the validations
     */
    total_wt = 0;
    for (i = 0; i < rp.n; i++)
	total_wt += rp.wt[i];
    old_wt = total_wt;

    k = 0;                      /* -Wall */
    for (xgroup = 0; xgroup < xvals; xgroup++) {
	/*
	 * restore rp.sorts, with the data for this run at the top
	 * this requires one pass per variable
	 */
	for (j = 0; j < rp.nvar; j++) {
	    k = 0;
	    for (i = 0; i < rp.n; i++) {
		ii = savesort[j * n + i];       /* walk through the variables
						 * in order */
		if (ii < 0)
		    ii = -(1 + ii);     /* missings move too */
		if (xgrp[ii] != xgroup + 1) {
		    /*
		     * this obs is left in --
		     * copy to the front half of rp.sorts
		     */
		    rp.sorts[j][k] = savesort[j * n + i];
		    k++;
		}
	    }
	}

	/*
	 * Fix up the y vector, and save a list of "left out" obs in
	 * the tail, unused end of rp.sorts[0][i];
	 */
	last = k;
	k = 0;
	temp = 0;
	for (i = 0; i < n; i++) {
	    rp.which[i] = 1;    /* everyone starts in the top node */
	    if (xgrp[i] == xgroup + 1) {
		rp.sorts[0][last] = i;
		last++;
	    } else {
		rp.ytemp[k] = rp.ydata[i];
		rp.wtemp[k] = rp.wt[i];
		temp += rp.wt[i];
		k++;
	    }
	}

	/* at this point k = #obs in the prediction group */
	/* rescale the cp */
	for (j = 0; j < rp.num_unique_cp; j++)
	    cp[j] *= temp / old_wt;
	rp.alpha *= temp / old_wt;
	old_wt = temp;

	/*
	 * partition the new tree
	 */
	xtree->num_obs = k;
	(*rp_init) (k, rp.ytemp, maxcat, &errmsg, parms, &ii, 2, rp.wtemp);
	(*rp_eval) (k, rp.ytemp, xtree->response_est, &(xtree->risk), rp.wtemp);
	xtree->complexity = xtree->risk;
	partition(1, xtree, &temp, 0, k);
	fix_cp(xtree, xtree->complexity);

	//print_tree(xtree, 1, 0, 0, 0);        /* debug line */
	/*
	 * run the extra data down the new tree
	 */
	for (i = k; i < rp.n; i++) {
	    j = rp.sorts[0][i];
	    rundown2(xtree, j, cp, (predict + j * ncp * nresp), nresp);
	}

	free_tree(xtree, 0);
	R_CheckUserInterrupt();
    }

    UNPROTECT(1);
    return predict2;
}
