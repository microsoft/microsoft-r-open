/*
 * prototypes for all of the rpart functions
 *   This helps the ansi compiler do tight checking.
 *
 */
#include "node.h"

pNode branch(pNode tree, int obs);

void bsplit(pNode me, int n1, int n2);

void choose_surg(int n1, int n2, int *y, double *x, int *order,
		 int ncat, double *agreement, double *split, int *csplit,
		 double ltot, double rtot, double *adj);

void fix_cp(pNode me, double parent_cp);

void free_tree(pNode node, int freenode);

void graycode_init0(int maxcat);
void graycode_init1(int numcat, int *count);
void graycode_init2(int numcat, int *count, double *val);
int graycode(void);

pSplit insert_split(pSplit *listhead, int ncat, double improve, int max);

void make_cp_list(pNode me, double parent, CpTable cptable_head);

CpTable make_cp_table(pNode me, double parent, int nsplit);

void mysort(int start, int stop, double *x, int *cvec);

void nodesplit(pNode me, int nodenum, int n1, int n2, int *nleft, int *nright);

int partition(int nodenum, pNode splitnode, double *sumrisk, int n1, int n2);

int print_tree(pNode me, int maxdepth);

SEXP rpart(SEXP ncat2, SEXP method2, SEXP opt2, SEXP parms2, SEXP ymat2,
	   SEXP xmat2, SEXP xvals2, SEXP xgrp2, SEXP wt2, SEXP ny2, SEXP cost2);

void rpart_callback0(int *nr);
void rpart_callback1(int n, double *y[], double *wt, double *z);
void rpart_callback2(int n, int ncat, double *y[], double *wt,
		     double *x, double *good);
void rpcountup(pNode me, int *nnode, int *nsplit, int *ncat);

void rpmatrix(pNode me, int *numcat, double **dsplit, int **isplit,
	      int **csplit, double **dnode, int **inode, int id);

void rundown(pNode tree, int obs, double *cp, double *xpred, double *xtemp);

void rundown2(pNode tree, int obs, double *cp, double *xpred, int nresp);

void surrogate(pNode me, int n1, int n2);

SEXP xpred(SEXP ncat2, SEXP method2, SEXP opt2, SEXP parms2, SEXP xvals2,
	   SEXP xgrp2, SEXP ymat2, SEXP xmat2, SEXP wt2, SEXP ny2,
	   SEXP cost2, SEXP all2, SEXP cp2, SEXP toprisk2, SEXP nresp2);

void xval(int n_xval, CpTable cptable_head, int *x_grp, int maxcat,
	  char **error, double *parms, int *savesort);
