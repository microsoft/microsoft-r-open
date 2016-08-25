# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"contrMat.stress" <- function()
{
    
    ## problem reported by Christian Ritz on R-help, Tue, 20 Mar 2007 14:55:38
    
    ## Dataset PestSci from package drc
    PestSci <-
            data.frame(CURVE = rep(1:5, each=21),
                    HERBICIDE = rep(c("bentazon", "diuron"), times=c(63, 42)),
                    DOSE = rep(c(0, 0.62, 1.85, 5.56, 16.67, 50, 150, 0, 0.62,
                                    1.85, 5.56, 16.67, 50, 150, 0, 0.15, 0.59, 2.34, 9.38, 37.5,
                                    150, 0, 0.01, 0.03, 0.1, 0.3, 1, 3, 0, 0.01, 0.03, 0.1, 0.3, 1, 3),
                            each = 3),
                    SLOPE = c(1.81295, 1.86704, 1.95606, 1.39073, 1.15721, 1.06126,
                            0.99409, 0.83298, 0.8334, 0.72513, 0.69548, 0.65299, 0.49855,
                            0.36873, 0.42617, 0.26666, 0.26896, 0.25989, 0.16074, 0.16404,
                            0.1475, 1.02654, 0.91306, 0.89371, 0.59074, 0.669, 0.5965,
                            0.37561, 0.44823, 0.42093, 0.31874, 0.27342, 0.2725, 0.27182,
                            0.21752, 0.19981, 0.17332, 0.17949, 0.15623, 0.12855, 0.14524,
                            0.11533, 1.03872, 1.0917, 1.10324, 0.94274, 0.91256, 1.02352,
                            0.78689, 0.69706, 0.65989, 0.5372, 0.51324, 0.54981, 0.37401,
                            0.34033, 0.32491, 0.30518, 0.24593, 0.289, 0.17414, 0.12275,
                            0.14788, 2.20963, 2.27931, 2.14703, 2.18831, 2.08863, 2.06676,
                            2.18827, 2.10748, 1.84474, 1.78805, 1.75547, 1.61381, 0.70295,
                            0.6983, 0.74045, 0.20673, 0.20784, 0.22402, 0.05268, 0.06519,
                            0.09258, 1.94033, 1.80193, 1.71586, 1.71586, 1.98471, 1.74905,
                            1.87795, 1.64081, 1.53094, 1.50709, 1.41035, 1.35367, 0.64427,
                            0.62185, 0.60337, 0.14073, 0.12928, 0.15016, 8e-05, 0.00262,
                            0.00303))
    
    
    library(nlme)
    sv <- c(0.43355869, 2.49963220, 0.05861799, 1.73290589, 0.38153146, 0.24316978)
    
    nlme(SLOPE ~ c + (d-c)/(1+exp(b*(log(DOSE)-log(e)))),
            fixed = list(b ~ factor(HERBICIDE)-1,
                    c ~ 1,
                    d ~ 1,
                    e ~ factor(HERBICIDE)-1),
            random = d ~ 1 | CURVE,
            start = sv, data = PestSci)
    
    ## failed in contrMat in 3.1-78
}

"test.contrMat.stress" <- function()
{
    res <- try(contrMat.stress())
    checkTrue(!is(res, "try-error"), msg="contrMat stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

