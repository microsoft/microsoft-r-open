/*
 * Calculate the surrogate splits for a node and its primary
 *    (This routine is an awful lot like bsplit)
 *
 * Input :      node
 *              start and stop indices for the arrays (which obs apply)
 *
 * Output:      Fills in the node's
 *                      surrogate splits
 *                      lastsurrogate value
 *
 * Uses:        The global vector tempvec (integer) as a temporary, assumed
 *                to be of length n.
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void
surrogate(pNode me, int n1, int n2)
{
    int i, j, k;
    int var;                    /* the primary split variable */
    double split;
    double improve;
    double lcount, rcount;      /* weight sent left and right by
				 * primary */
    int extra;
    pSplit ss;
    int *index;
    int *tempy;
    int **sorts;
    double **xdata;
    int ncat;
    double adj_agree;

    tempy = rp.tempvec;
    sorts = rp.sorts;
    xdata = rp.xdata;
    /*
     * First construct, in tempy, the "y" variable for this calculation.
     * It will be LEFT:goes left, 0:missing, RIGHT:goes right.
     *  Count up the number of obs the primary sends to the left, as my
     *  last surrogate (or to the right, if larger).
     */
    var = (me->primary)->var_num;
    if (rp.numcat[var] == 0) {  /* continuous variable */
	split = (me->primary)->spoint;
	extra = (me->primary)->csplit[0];
	for (i = n1; i < n2; i++) {
	    j = sorts[var][i];
	    if (j < 0)
		tempy[-(j + 1)] = 0;
	    else
		tempy[j] = (xdata[var][j] < split) ? extra : -extra;
	}
    } else {                    /* categorical variable */
	index = (me->primary)->csplit;
	for (i = n1; i < n2; i++) {
	    j = sorts[var][i];
	    if (j < 0)
		tempy[-(j + 1)] = 0;
	    else
		tempy[j] = index[(int) xdata[var][j] - 1];
	}
    }

    /* count the total number sent left and right */
    lcount = 0;
    rcount = 0;
    for (i = n1; i < n2; i++) {
	j = sorts[var][i];
	if (j < 0)
	    j = -(1 + j);
	switch (tempy[j]) {
	case LEFT:
	    lcount += rp.wt[j];
	    break;
	case RIGHT:
	    rcount += rp.wt[j];
	    break;
	default:
	    break;
	}
    }

    if (lcount < rcount)
	me->lastsurrogate = RIGHT;
    else {
	if (lcount > rcount)
	    me->lastsurrogate = LEFT;
	else
	    me->lastsurrogate = 0;      /* no default */
    }

    /*
     * Now walk through the variables
     */
    me->surrogate = (pSplit) NULL;
    for (i = 0; i < rp.nvar; i++) {
	if (var == i)
	    continue;
	ncat = rp.numcat[i];

	choose_surg(n1, n2, tempy, xdata[i], sorts[i], ncat,
		    &improve, &split, rp.csplit, lcount, rcount, &adj_agree);

	if (adj_agree <= 1e-10)    /* was 0 */
	    continue;           /* no better than default */

	/* sort it onto the list of surrogates */
	ss = insert_split(&(me->surrogate), ncat, improve, rp.maxsur);
	if (ss) {
	    ss->improve = improve;
	    ss->var_num = i;
	    ss->count = 0;      /* corrected by nodesplit() */
	    ss->adj = adj_agree;
	    if (rp.numcat[i] == 0) {
		ss->spoint = split;
		ss->csplit[0] = rp.csplit[0];
	    } else
		for (k = 0; k < rp.numcat[i]; k++)
		    ss->csplit[k] = rp.csplit[k];
	}
    }
}
