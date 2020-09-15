/*
 *  The functions for poisson based regression
 */
#include <math.h>
#include "rpart.h"

static double exp_alpha, exp_beta;
static double *death, *wtime, *rate;
static int *countn, *order, *order2;
static int which_pred;
/*
 * initialize the necessary common variables for poisson fits
 */
int
poissoninit(int n, double *y[], int maxcat, char **error,
	    double *param, int *size, int who, double *wt)
{
    int i;
    double event, time;

    /* allocate memory for scratch */
    if (who == 1 && maxcat > 0) {
	death = (double *) ALLOC(3 * maxcat, sizeof(double));
	rate = death + maxcat;
	wtime = rate + maxcat;
	order = (int *) ALLOC(3 * maxcat, sizeof(int));
	order2 = order + maxcat;
	countn = order2 + maxcat;
    }
    /* check data */
    if (who == 1) {
	for (i = 0; i < n; i++) {
	    if (y[i][0] <= 0) {
		*error = _("Invalid time point");
		return 1;
	    }
	    if (y[i][1] < 0) {
		*error = _("Invalid event count");
		return 1;
	    }
	}
    }
    /* compute the overall hazard rate */
    event = 0;
    time = 0;
    for (i = 0; i < n; i++) {
	event += y[i][1] * wt[i];
	time += y[i][0] * wt[i];
    }

    /*
     * Param[0] will contain the desired CV.  If is is <=0, no shrinking
     *   is desired.  The CV determines alpha, and beta is set so that
     *   the gamma prior has the correct mean.
     */
    if (param[0] <= 0) {
	exp_alpha = 0;
	exp_beta = 0;
    } else {
	exp_alpha = 1 / (param[0] * param[0]);
	exp_beta = exp_alpha / (event / time);
    }
    /*
     * Param[1] contains the xval rule:  1=deviance, 2=square root
     */
    which_pred = (int) param[1];
    if (param[1] != 1 && param[1] != 2) {
	*error = _("Invalid error rule");
	return 1;
    }
    *size = 2;
    return 0;
}

/*
 *  Compute the error of prediction
 */
double
poissonpred(double *y, double *lambda)
{
    double temp, dev;

    if (which_pred == 1) {
	temp = y[1];
	dev = temp - *lambda * y[0];
	if (temp > 0)
	    dev += temp * log(*lambda * y[0] / temp);

	return -2 * dev;
    } else {
	/*
	 * A version based on square roots, which is the variance
	 * stabilizing transform
	 */
	temp = sqrt(y[1]) - sqrt(*lambda * y[0]);       /* sqrt(obs) - sqrt(exp) */
	return temp * temp;
    }
}

/*
 * Compute the predicted response rate (empirical Bayes) and the
 *   contribution to the deviance under that rate.
 * Deviance = \sum w_i[ d_i \log(d_i/ p_i) - (d_i - p_i) ]
 *   where p_i = predicted # events = \lambda t_i
 */
void
poissondev(int n, double **y, double *value, double *risk, double *wt)
{
    int i;
    double death = 0, time = 0, lambda, dev = 0, temp;

    /*
     * first get the overall estimate of lambda
     */
    for (i = 0; i < n; i++) {
	death += y[i][1] * wt[i];
	time += y[i][0] * wt[i];
    }
    lambda = (death + exp_alpha) / (time + exp_beta);

    for (i = 0; i < n; i++) {
	temp = y[i][1];
	dev -= (lambda * y[i][0] - temp) * wt[i];
	if (temp > 0)
	    dev += (temp * log(lambda * y[i][0] / temp)) * wt[i];
    }

    value[0] = lambda;
    value[1] = death;
    *risk = -2 * dev;
}

/*
 * The poisson splitting function.  Find that split point in x such that
 *  the dev within the two groups is decreased as much
 *  as possible.  It is not necessary to actually calculate the devs,
 *  as nearly everything cancels.  The search for a split does not use the
 *  Bayes estimate, for speed reasons.
 * With \hat\lambda = (\sum w_i d_i) / (\sum w_i t_i)
 *  we have  deviance(total) - [ deviance(left son) + deviance(right son)]
 *              =  d_l \lambda_l +  d_r \lambda_r -  d_t \lambda_t,
 *   where d_l = weigthed sum of deaths for the left son, d_r = right,
 *   d_t = total, and lambda_l etc are the estimated response rates
*/
void
poisson(int n, double **y, double *x, int nclass,
	int edge, double *improve, double *split,
	int *csplit, double my_risk, double *wt)
{
    int i, j;
    int left_n, right_n;
    double left_time, right_time;
    double left_d, right_d;
    double dev;                 /* dev of the parent node (me) */
    double lambda1, lambda2;
    double best, temp;
    int direction = LEFT;
    int where = 0;
    int ncat;

    /*
     * Get the total deaths and the total time
     */
    right_d = 0;
    right_time = 0;
    right_n = n;
    for (i = 0; i < n; i++) {
	right_d += y[i][1] * wt[i];
	right_time += y[i][0] * wt[i];
    }

    /*
     * Compute the overall lambda and dev
     */
    lambda2 = right_d / right_time;
    if (lambda2 == 0) {
	*improve = 0;           /* no deaths to split! */
	return;
    }
    dev = right_d * log(lambda2);

    /*
     * at this point we split into 2 disjoint paths
     */
    if (nclass > 0)
	goto categorical;

    left_time = 0;
    left_d = 0;
    where = -1;
    best = dev;
    for (i = 0; i < n - edge; i++) {
	left_d += y[i][1] * wt[i];
	right_d -= y[i][1] * wt[i];
	left_time += y[i][0] * wt[i];
	right_time -= y[i][0] * wt[i];

	if (x[i + 1] != x[i] && (1 + i) >= edge) {
	    lambda1 = left_d / left_time;
	    lambda2 = right_d / right_time;
	    temp = 0;
	    if (lambda1 > 0)
		temp += left_d * log(lambda1);
	    if (lambda2 > 0)
		temp += right_d * log(lambda2);
	    if (temp > best) {
		best = temp;
		where = i;
		direction = (lambda1 < lambda2) ? LEFT : RIGHT;
	    }
	}
    }

    *improve = -2 * (dev - best);
    if (where >= 0) {           /* found something */
	csplit[0] = direction;
	*split = (x[where] + x[where + 1]) / 2;
    }
    return;

categorical:;
    for (i = 0; i < nclass; i++) {
	wtime[i] = 0;
	death[i] = 0;
	countn[i] = 0;
    }

    for (i = 0; i < n; i++) {
	j = (int) (x[i] - 1);
	countn[j]++;            /* number per group */
	death[j] += y[i][1] * wt[i];
	wtime[j] += y[i][0] * wt[i];    /* sum of time */
    }

    /*
     * Rank the rates  - each is scored as the number of others that it
     * is smaller than.  Ignore the categories which had no representatives.
     */
    ncat = 0;                   /* may be less than nclass if not all
				 * categories are present */
    for (i = 0; i < nclass; i++) {
	order[i] = 0;
	if (countn[i] > 0) {
	    ncat++;
	    rate[i] = death[i] / wtime[i];
	    for (j = i - 1; j >= 0; j--) {
		if (countn[j] > 0) {
		    if (rate[i] > rate[j])
			order[j]++;
		    else
			order[i]++;
		}
	    }
	}
    }
    /*
     * order2 will point to the largest, second largest, etc
     */
    for (i = 0; i < nclass; i++)
	if (countn[i] > 0)
	    order2[order[i]] = i;

    /*
     * Now find the split that we want
     * starting with everyone in the right hand group
     */
    left_n = 0;
    left_d = 0;
    left_time = 0;
    best = dev;
    where = 0;
    for (i = 0; i < ncat - 1; i++) {
	j = order2[i];
	left_n += countn[j];
	right_n -= countn[j];
	left_time += wtime[j];
	right_time -= wtime[j];
	left_d += death[j];
	right_d -= death[j];
	if (left_n >= edge && right_n >= edge) {
	    lambda1 = left_d / left_time;
	    lambda2 = right_d / right_time;
	    temp = 0;
	    if (lambda1 > 0)
		temp += left_d * log(lambda1);
	    if (lambda2 > 0)
		temp += right_d * log(lambda2);
	    if (temp > best) {
		best = temp;
		where = i;
		direction = (lambda1 < lambda2) ?  LEFT : RIGHT;
	    }
	}
    }

    *improve = -2 * (dev - best);

    /* if improve = 0, csplit will never be looked at by the calling routine */
    for (i = 0; i < nclass; i++)
	csplit[i] = 0;
    for (i = 0; i <= where; i++)
	csplit[order2[i]] = direction;
    for (; i < ncat; i++)
	csplit[order2[i]] = -direction;
}
