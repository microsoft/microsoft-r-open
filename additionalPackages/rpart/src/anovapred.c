/*
 * The error function for anova splitting
 */
double
anovapred(double *y, double *yhat)
{
    double temp = y[0] - *yhat;
    return temp * temp;
}
