/*
 * The main entry point for recursive partitioning routines.
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
 *
 * Returned: a list with elements
 *      which = vector of final node numbers for each input obs
 *      cptable = the complexity table
 *      dsplit = for each split, numeric variables (doubles)
 *      isplit = for each split, integer variables
 *      dnode =  for each node, numeric variables
 *      inode =  for each node, integer variables
 *
 * Naming convention: ncat = pointer to an integer vector, ncat2 = the
 *   input R object (SEXP) containing that vector, ncat3 = an output S object
 *   containing that vector.
 */
#define MAINRP
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "func_table.h"
#include "rpartproto.h"

SEXP
rpart(SEXP ncat2, SEXP method2, SEXP opt2,
      SEXP parms2, SEXP xvals2, SEXP xgrp2,
      SEXP ymat2, SEXP xmat2, SEXP wt2, SEXP ny2, SEXP cost2)
{

    pNode tree;          /* top node of the tree */
    char *errmsg;
    int i, j, k, n;
    int maxcat;
    double temp;
    int *savesort = NULL /* -Wall */ ;
    double *dptr;               /* temp */
    int *iptr;
    /*
     * pointers to R objects
     */
    int *ncat, *xgrp;
    int xvals;
    double *wt, *parms;

    /*
     * Return objects for R -- end in "3" to avoid overlap with internal names
     */
    SEXP which3, cptable3, dsplit3, isplit3, csplit3 = R_NilValue, /* -Wall */
	dnode3, inode3;

    /* work arrays for the return process */
    int nodecount, catcount, splitcount;
    double **ddnode, *ddsplit[3];
    int *iinode[6], *iisplit[3];
    int **ccsplit;
    double scale;
    CpTable cp;

    ncat = INTEGER(ncat2);
    xgrp = INTEGER(xgrp2);
    xvals = asInteger(xvals2);
    wt = REAL(wt2);
    parms = REAL(parms2);
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
    rp.maxpri = (int) dptr[3] + 1;      /* max primary splits =
					   max competitors + 1 */
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
     * save away a copy of the rp.sorts, if needed for xval
     */
    if (xvals > 1) {
	savesort = (int *) ALLOC(n * rp.nvar, sizeof(int));
	memcpy(savesort, rp.sorts[0], n * rp.nvar * sizeof(int));
    }

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
     * initialize the top node of the tree
     */
    errmsg = _("unknown error");
    which3 = PROTECT(allocVector(INTSXP, n));
    rp.which = INTEGER(which3);
    temp = 0;
    for (i = 0; i < n; i++) {
	rp.which[i] = 1;
	temp += wt[i];
    }
    i = (*rp_init) (n, rp.ydata, maxcat, &errmsg, parms, &rp.num_resp, 1, wt);
    if (i > 0)
	error(errmsg);

    nodesize = sizeof(Node) + (rp.num_resp - 20) * sizeof(double);
    tree = (pNode) ALLOC(1, nodesize);
    memset(tree, 0, nodesize);
    tree->num_obs = n;
    tree->sum_wt = temp;

    (*rp_eval) (n, rp.ydata, tree->response_est, &(tree->risk), wt);
    tree->complexity = tree->risk;
    rp.alpha = rp.complexity * tree->risk;

    /*
     * Do the basic tree
     */
    partition(1, tree, &temp, 0, n);
    CpTable cptable = (CpTable) ALLOC(1, sizeof(cpTable));
    cptable->cp = tree->complexity;
    cptable->risk = tree->risk;
    cptable->nsplit = 0;
    cptable->forward = 0;
    cptable->xrisk = 0;
    cptable->xstd = 0;
    rp.num_unique_cp = 1;

    if (tree->rightson) {
	make_cp_list(tree, tree->complexity, cptable);
	make_cp_table(tree, tree->complexity, 0);
	if (xvals > 1) {
	    xval(xvals, cptable, xgrp, maxcat, &errmsg, parms, savesort);
	}
    }
    /*
     * all done, create the return list for R
     * first the cp table
     */
    scale = 1 / tree->risk;
    i = 0;
    cptable3 = PROTECT(allocMatrix(REALSXP, xvals > 1 ? 5 : 3,
				   rp.num_unique_cp));
    dptr = REAL(cptable3);
    for (cp = cptable; cp; cp = cp->forward) {
	dptr[i++] = cp->cp * scale;
	dptr[i++] = cp->nsplit;
	dptr[i++] = cp->risk * scale;
	if (xvals > 1) {
	    dptr[i++] = cp->xrisk * scale;
	    dptr[i++] = cp->xstd * scale;
	}
    }

    /*
     * Return the body of the tree
     *  For each component we first create a vector to hold the
     *  result, then a ragged array index into the vector.
     * The rpmatrix routine then fills everything in.
     */
    rpcountup(tree, &nodecount, &splitcount, &catcount);
    dnode3 = PROTECT(allocMatrix(REALSXP, nodecount, (3 + rp.num_resp)));
    ddnode = (double **) ALLOC(3 + rp.num_resp, sizeof(double *));
    dptr = REAL(dnode3);
    for (i = 0; i < 3 + rp.num_resp; i++) {
	ddnode[i] = dptr;
	dptr += nodecount;
    }

    dsplit3 = PROTECT(allocMatrix(REALSXP, splitcount, 3));
    dptr = REAL(dsplit3);
    for (i = 0; i < 3; i++) {
	ddsplit[i] = dptr;
	dptr += splitcount;
	for (j = 0; j < splitcount; j++)
	    ddsplit[i][j] = 0.0;
    }

    inode3 = PROTECT(allocMatrix(INTSXP, nodecount, 6));
    iptr = INTEGER(inode3);
    for (i = 0; i < 6; i++) {
	iinode[i] = iptr;
	iptr += nodecount;
    }

    isplit3 = PROTECT(allocMatrix(INTSXP, splitcount, 3));
    iptr = INTEGER(isplit3);
    for (i = 0; i < 3; i++) {
	iisplit[i] = iptr;
	iptr += splitcount;
    }

    if (catcount > 0) {
	csplit3 = PROTECT(allocMatrix(INTSXP, catcount, maxcat));
	ccsplit = (int **) ALLOC(maxcat, sizeof(int *));
	iptr = INTEGER(csplit3);
	for (i = 0; i < maxcat; i++) {
	    ccsplit[i] = iptr;
	    iptr += catcount;
	    for (j = 0; j < catcount; j++)
		ccsplit[i][j] = 0;      /* zero it out */
	}
    } else
	ccsplit = NULL;

    rpmatrix(tree, rp.numcat, ddsplit, iisplit, ccsplit, ddnode, iinode, 1);
    free_tree(tree, 0);         /* let the memory go */

    /*
     * Fix up the 'which' array
     *  Nodes are sometimes trimmed during the
     *  tree building, and 'which' is not updated in that case
     */
    for (i = 0; i < n; i++) {
	k = rp.which[i];
	do {
	    for (j = 0; j < nodecount; j++)
		if (iinode[0][j] == k) {
		    rp.which[i] = j + 1;
		    break;
		}
	    k /= 2;
	} while (j >= nodecount);
    }

    /* Create the output list */
    int nout = catcount > 0 ? 7 : 6;
    SEXP rlist = PROTECT(allocVector(VECSXP, nout));
    SEXP rname = allocVector(STRSXP, nout);
    setAttrib(rlist, R_NamesSymbol, rname);
    SET_VECTOR_ELT(rlist, 0, which3);
    SET_STRING_ELT(rname, 0, mkChar("which"));
    SET_VECTOR_ELT(rlist, 1, cptable3);
    SET_STRING_ELT(rname, 1, mkChar("cptable"));
    SET_VECTOR_ELT(rlist, 2, dsplit3);
    SET_STRING_ELT(rname, 2, mkChar("dsplit"));
    SET_VECTOR_ELT(rlist, 3, isplit3);
    SET_STRING_ELT(rname, 3, mkChar("isplit"));
    SET_VECTOR_ELT(rlist, 4, dnode3);
    SET_STRING_ELT(rname, 4, mkChar("dnode"));
    SET_VECTOR_ELT(rlist, 5, inode3);
    SET_STRING_ELT(rname, 5, mkChar("inode"));
    if (catcount > 0) {
	SET_VECTOR_ELT(rlist, 6, csplit3);
	SET_STRING_ELT(rname, 6, mkChar("csplit"));
    }

    UNPROTECT(1 + nout);
    return rlist;
}
