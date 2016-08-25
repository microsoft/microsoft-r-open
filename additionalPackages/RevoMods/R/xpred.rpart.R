"xpred.rpart" <- function(fit, ...)
{
    UseMethod("xpred.rpart")
}

"xpred.rpart.rxDTree" <- function(fit,...)
{
    stop("xpred not implemented for rxDTree objects")
}

"xpred.rpart.default" <- rpart::xpred.rpart	