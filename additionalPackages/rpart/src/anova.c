/*
* The four routines for anova splitting
*/
#include "rpart.h"
#include "rpartproto.h"

static double *mean, *sums;
static double *wts;
static int *countn;
static int *tsplit;

int
anovainit(int n, double *y[], int maxcat, char **error,
	  double *parm, int *size, int who, double *wt)
{
    if (who == 1 && maxcat > 0) {
	graycode_init0(maxcat);
	countn = (int *) ALLOC(2 * maxcat, sizeof(int));
	tsplit = countn + maxcat;
	mean = (double *) ALLOC(3 * maxcat, sizeof(double));
	wts = mean + maxcat;
	sums = wts + maxcat;
    }
    *size = 1;
    return 0;
}

/*
* The anova evaluation function.  Return the mean and the ss.
*/
void
anovass(int n, double *y[], double *value, double *risk, double *wt)
{
    int i;
    double temp = 0., twt = 0.; /* sum of the weights */
    double mean, ss;

    for (i = 0; i < n; i++) {
	temp += *y[i] * wt[i];
	twt += wt[i];
    }
    mean = temp / twt;

    ss = 0;
    for (i = 0; i < n; i++) {
	temp = *y[i] - mean;
	ss += temp * temp * wt[i];
    }

    *value = mean;
    *risk = ss;
}

/*
 * The anova splitting function.  Find that split point in x such that
 *  the sum of squares of y within the two groups is decreased as much
 *  as possible.  It is not necessary to actually calculate the SS, the
 *  improvement involves only means in the two groups.
 */
void
anova(int n, double *y[], double *x, int nclass,
      int edge, double *improve, double *split, int *csplit,
      double myrisk, double *wt)
{
    int i, j;
    double temp;
    double left_sum, right_sum;
    double left_wt, right_wt;
    int left_n, right_n;
    double grandmean, best;
    int direction = LEFT;
    int where = 0;

   /*
    * The improvement of a node is SS - (SS_L + SS_R), where
    *   SS = sum of squares in a node = \sum w_i (x_i - \bar x)^2, where
    * of course \bar x is a weighted mean \sum w_i x_i / \sum w_i
    * Using the identity
    *    \sum w_i(x_ - \bar x)^2 = \sum w_i (x_i-c)^2 - (\sum w_i)(c-\bar x)^2
    * the improvement = w_l*(left mean - grand mean)^2
    *                  +w_r*(right mean- grand mean)^2
    * where w_l is the sum of weights in the left node, w_r similarly.
    */
    right_wt = 0;
    right_n = n;
    right_sum = 0;
    for (i = 0; i < n; i++) {
	right_sum += *y[i] * wt[i];
	right_wt += wt[i];
    }
    grandmean = right_sum / right_wt;

    if (nclass == 0) {          /* continuous predictor */
	left_sum = 0;           /* No data in left branch, to start */
	left_wt = 0;
	left_n = 0;
	right_sum = 0;          /* after subracting grand mean, it's zero */
	best = 0;
	for (i = 0; right_n > edge; i++) {
	    left_wt += wt[i];
	    right_wt -= wt[i];
	    left_n++;
	    right_n--;
	    temp = (*y[i] - grandmean) * wt[i];
	    left_sum += temp;
	    right_sum -= temp;
	    if (x[i + 1] != x[i] && left_n >= edge) {
		temp = left_sum * left_sum / left_wt +
		    right_sum * right_sum / right_wt;
		if (temp > best) {
		    best = temp;
		    where = i;
		    if (left_sum < right_sum)
			direction = LEFT;
		    else
			direction = RIGHT;
		}
	    }
	}

	*improve = best / myrisk;
	if (best > 0) {         /* found something */
	    csplit[0] = direction;
	    *split = (x[where] + x[where + 1]) / 2;
	}
    }
    /*
     * Categorical predictor
     */
    else {
	for (i = 0; i < nclass; i++) {
	    sums[i] = 0;
	    countn[i] = 0;
	    wts[i] = 0;
	}

       /* rank the classes by their mean y value */
	for (i = 0; i < n; i++) {
	    j = (int) x[i] - 1;
	    countn[j]++;
	    wts[j] += wt[i];
	    sums[j] += (*y[i] - grandmean) * wt[i];
	}
	for (i = 0; i < nclass; i++) {
	    if (countn[i] > 0) {
		tsplit[i] = RIGHT;
		mean[i] = sums[i] / wts[i];
	    } else
		tsplit[i] = 0;
	}
	graycode_init2(nclass, countn, mean);

	/*
	 * Now find the split that we want
	 */
	left_wt = 0;
	left_sum = 0;
	right_sum = 0;
	left_n = 0;
	best = 0;
	where = 0;
	while ((j = graycode()) < nclass) {
	    tsplit[j] = LEFT;
	    left_n += countn[j];
	    right_n -= countn[j];
	    left_wt += wts[j];
	    right_wt -= wts[j];
	    left_sum += sums[j];
	    right_sum -= sums[j];
	    if (left_n >= edge && right_n >= edge) {
		temp = left_sum * left_sum / left_wt +
		    right_sum * right_sum / right_wt;
		if (temp > best) {
		    best = temp;
		    if ((left_sum / left_wt) > (right_sum / right_wt))
			for (i = 0; i < nclass; i++) csplit[i] = -tsplit[i];
		    else
			for (i = 0; i < nclass; i++) csplit[i] = tsplit[i];
		}
	    }
	}

	*improve = best / myrisk;       /* % improvement */
    }
}
