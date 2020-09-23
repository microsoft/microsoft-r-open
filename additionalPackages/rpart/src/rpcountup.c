/*
 * count up the number of nodes and splits in the final result
 *
 *  Gather the counts for myself, add in those of my children, and
 *    pass the total back to my parent
 */
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void
rpcountup(pNode me, int *nnode, int *nsplit, int *ncat)
{
    int node2, split2;
    int cat2;
    int i, j, k;
    pSplit ss;

    if (me->complexity <= rp.alpha || me->leftson == 0) {       /* no kids */
	*nnode = 1;
	*nsplit = 0;
	*ncat = 0;
    } else {
	i = 0;
	j = 0;
	k = 0;
	for (ss = me->primary; ss; ss = ss->nextsplit) {
	    i++;
	    if (rp.numcat[ss->var_num] > 0)
		k++;
	}
	for (ss = me->surrogate; ss; ss = ss->nextsplit) {
	    j++;
	    if (rp.numcat[ss->var_num] > 0)
		k++;
	}

	rpcountup(me->leftson, nnode, nsplit, ncat);
	rpcountup(me->rightson, &node2, &split2, &cat2);
	*nnode += 1 + node2;
	*nsplit += i + j + split2;
	*ncat += k + cat2;
    }
}
