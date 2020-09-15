/*
 * The main workhorse of the recursive partitioning module.  When called
 *   with a node, it partitions it and then calls itself to partition the
 *   children it has created.
 * If the node is not splittable (too few people, or complexity is too small)
 *   it simply returns.  The routine may not be able to discover that the
 *   complexity is too small until after the children have been partitioned,
 *   so it needs to check this at the end.
 * The vector who[n] indexes which observations are in this node, to speed
 *   up the routine.
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

int
partition(int nodenum, pNode splitnode, double *sumrisk, int n1, int n2)
{
    pNode me;
    double tempcp;
    int i, j, k;
    double tempcp2;
    double left_risk, right_risk;
    int left_split, right_split;
    double twt;
    int nleft, nright;
    int n;

    me = splitnode;
    n = n2 - n1;                /* total number of observations */

    if (nodenum > 1) {
	twt = 0;
	k = 0;
	for (i = n1; i < n2; i++) {
	    j = rp.sorts[0][i]; /* any variable would do, use first */
	    if (j < 0)
		j = -(1 + j);   /* if missing, value = -(1+ true index) */
	    rp.wtemp[k] = rp.wt[j];
	    rp.ytemp[k] = rp.ydata[j];
	    twt += rp.wt[j];
	    k++;
	}
	(*rp_eval) (n, rp.ytemp, me->response_est, &(me->risk), rp.wtemp);
	me->num_obs = n;
	me->sum_wt = twt;
	tempcp = me->risk;
	if (tempcp > me->complexity)
	    tempcp = me->complexity;
    } else
	tempcp = me->risk;

    /*
     * Can I quit now ?
     */
    if (me->num_obs < rp.min_split || tempcp <= rp.alpha ||
	nodenum > rp.maxnode) {
	me->complexity = rp.alpha;
	*sumrisk = me->risk;
	/*
	 * make sure the split doesn't have random pointers to somewhere
	 * i.e., don't trust that whoever allocated memory set it to zero
	 */
	me->leftson = (pNode)  NULL;
	me->rightson = (pNode) NULL;
	me->primary = (pSplit) NULL;
	me->surrogate = (pSplit) NULL;
	return 0;
    }
    /*
     * Guess I have to do the split
     */
    bsplit(me, n1, n2);
    if (!me->primary) {
	/*
	 * This is rather rare -- but I couldn't find a split worth doing
	 */
	me->complexity = rp.alpha;
	me->leftson = (pNode) NULL;
	me->rightson = (pNode) NULL;
	me->primary = (pSplit) NULL;
	me->surrogate = (pSplit) NULL;
	*sumrisk = me->risk;
	return 0;
    }
#ifdef DEBUG
    print_tree(me, 2);
#endif
    if (rp.maxsur > 0)
	surrogate(me, n1, n2);
    else
	me->surrogate = (pSplit) NULL;
    nodesplit(me, nodenum, n1, n2, &nleft, &nright);

    /*
     * split the leftson
     */
    me->leftson = (pNode) CALLOC(1, nodesize);
    (me->leftson)->complexity = tempcp - rp.alpha;
    left_split =
	partition(2 * nodenum, me->leftson, &left_risk, n1, n1 + nleft);

    /*
     * Update my estimate of cp, and split the right son.
     */
    tempcp = (me->risk - left_risk) / (left_split + 1);
    tempcp2 = (me->risk - (me->leftson)->risk);
    if (tempcp < tempcp2)
	tempcp = tempcp2;
    if (tempcp > me->complexity)
	tempcp = me->complexity;

    me->rightson = (pNode) CALLOC(1, nodesize);
    (me->rightson)->complexity = tempcp - rp.alpha;
    right_split = partition(1 + 2 * nodenum, me->rightson, &right_risk,
			    n1 + nleft, n1 + nleft + nright);

    /*
     * Now calculate my actual C.P., which depends on children nodes, and
     *  on grandchildren who do not collapse before the children.
     * The calculation is done assuming that I am the top node of the
     *  whole tree, an assumption to be fixed up later.
     */
    tempcp = (me->risk - (left_risk + right_risk)) /
	(left_split + right_split + 1);

    /* Who goes first -- minimum of tempcp, leftson, and rightson */
    if ((me->rightson)->complexity > (me->leftson)->complexity) {
	if (tempcp > (me->leftson)->complexity) {
	    /* leftson collapses first */
	    left_risk = (me->leftson)->risk;
	    left_split = 0;

	    tempcp = (me->risk - (left_risk + right_risk)) /
		(left_split + right_split + 1);
	    if (tempcp > (me->rightson)->complexity) {
		/* right one goes too */
		right_risk = (me->rightson)->risk;
		right_split = 0;
	    }
	}
    } else if (tempcp > (me->rightson)->complexity) {
	/* right hand child goes first */
	right_split = 0;
	right_risk = (me->rightson)->risk;

	tempcp = (me->risk - (left_risk + right_risk)) /
	    (left_split + right_split + 1);
	if (tempcp > (me->leftson)->complexity) {
	    /* left one goes too */
	    left_risk = (me->leftson)->risk;
	    left_split = 0;
	}
    }
    me->complexity = (me->risk - (left_risk + right_risk)) /
	(left_split + right_split + 1);

    if (me->complexity <= rp.alpha) {
	/*
	 * All was in vain!  This node doesn't split after all.
	 */
	free_tree(me, 0);
	*sumrisk = me->risk;
	for (i = n1; i < n2; i++) {
	    j = rp.sorts[0][i];
	    if (j < 0)
		j = -(1 + j);
	    rp.which[j] = nodenum;      /* revert to the old nodenumber */
	}
	return 0;               /* return # of splits */
    } else {
	*sumrisk = left_risk + right_risk;
	return left_split + right_split + 1;
    }
}
