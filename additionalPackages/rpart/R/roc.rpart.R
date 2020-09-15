roc.rpart <- function(object, plot.ok = TRUE, x.orient = 1L)
{

    if (!inherits(object, "rpart") || object$method != "class" ||
       length(attr(object, "ylevels")) != 2L)
        stop('Not legitimate \"rpart\" tree and endpoint not a 2 level-factor')

    ss.compare <- function(a, b) a >= b

    endnodes <- object$frame$splits[, 1L] == ""
    truth <- object$frame$yval2[endnodes, ]
    cutoffs <- sort(unique(c(0, 1, object$frame$yprob[endnodes, 2L])))
    pred.np <- outer(cutoffs, object$frame$yprob[endnodes, 2L], ss.compare)

    last.r <- dim(pred.np)[1L]
    last.c <- dim(pred.np)[2L]
    if (sum(pred.np[1L, ]  ) > 0L) {
        pred.np <- rbind(matrix(FALSE, nrow = 1L, ncol = last.c), pred.np)
	cutoffs <- c(NA, cutoffs)
    }
    last.r <- dim(pred.np)[1L]
    last.c <- dim(pred.np)[2L]
    if (sum(pred.np[last.r, ])  < last.c) {
        pred.np <- rbind(pred.np, matrix(TRUE, nrow = 1L, ncol = last.c))
	cutoffs <- c(cutoffs, NA)
    }

    cutoff.n <- length(cutoffs)
    ## set up some empty matrices ##
    sensitivity <- matrix(0, nrow = cutoff.n, ncol = 1L)
    specificity <- matrix(0, nrow = cutoff.n, ncol = 1L)
    negpred <- matrix(0, nrow = cutoff.n, ncol = 1L)
    pospred <- matrix(0, nrow = cutoff.n, ncol = 1L)
    tpcp <- matrix(0, nrow = cutoff.n, ncol = 1L)
    tncp <- matrix(0, nrow = cutoff.n, ncol = 1L)
    tpcn <- matrix(0, nrow = cutoff.n, ncol = 1L)
    tncn <- matrix(0, nrow = cutoff.n, ncol = 1L)
    ss.table <- array(0, c(cutoff.n, 2L, 2L))

    for (i in 1:cutoff.n) {
        ss.table <- matrix(0, nrow = 2L, ncol = 2L)

        ss.table[1L, 1L] <- sum(truth[pred.np[i, ], 1L])
        ss.table[2L, 1L] <- sum(truth[!pred.np[i, ], 1L])
        ss.table[1L, 2L] <- sum(truth[pred.np[i, ], 2L])
        ss.table[2L, 2L] <- sum(truth[!pred.np[i, ], 2L])

        sensitivity[i] <- ss.table[2L, 2L]/(ss.table[2L, 2L] + ss.table[1L, 2L])
        specificity[i] <- ss.table[1L, 1L]/(ss.table[1L, 1L] + ss.table[2L, 1L])
        negpred[i] <- ss.table[1L, 1L]/(ss.table[1L, 1L] + ss.table[1L, 2L])
        pospred[i] <- ss.table[2L, 2L]/(ss.table[2L, 2L] + ss.table[2L, 1L])
        tpcp[i] <- ss.table[2L, 2L]
        tncp[i] <- ss.table[2L, 1L]
        tpcn[i] <- ss.table[1L, 2L]
        tncn[i] <- ss.table[1L, 1L]
    }

    if (plot.ok) {
        o.par <- par(pty = "s")
        on.exit(par(o.par))
        if (x.orient == 1L){
            plot(1-specificity, sensitivity, type = "o", xlim = c(0, 1),
                 ylim = c(0, 1), ylab = "Sensitivity", xlab = "1-Specificity")
        }
        if (x.orient == 2L){
            plot(specificity, sensitivity, type = "o", xlim = c(0, 1),
                 ylim = c(0, 1), ylab = "Sensitivity", xlab = "Specificity")
        }
    }

    data.frame(cutoffs = format(round(cutoffs, 3L)), tpcp, tncp, tpcn, tncn,
               sensitivity = format(round(sensitivity, 2L)),
               specificity = format(round(specificity, 2L)),
               pospred = format(round(pospred, 2L)),
               negpred = format(round(negpred, 2L)))
}

