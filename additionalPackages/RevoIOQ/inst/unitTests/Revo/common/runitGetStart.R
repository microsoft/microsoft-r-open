## runitGetStart.R -- Unit Test for Getting Started Document
## Copyright (c) 2015 Microsoft Corporation All Rights Reserved
## 
## This test is part of the Revolution R Enterprise Validation Suite
##
## Run through examples in tutorial.tex

if (compareVersion(paste(R.version$major, R.version$minor, sep=".") ,"2.12.0") < 0) {
    load(system.file(file.path("unitTestData", "getstart.Rdata"), package="RevoIOQ"))
} else {
      load(system.file(file.path("unitTestData", "getstart_2.12.1.Rdata"), package="RevoIOQ"))
}
michelson <-  c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880,
        1000, 980, 930, 650, 760, 810, 1000, 1000, 960, 960)
RNGkind("default", "default")
set.seed(14)
normalDat <- rnorm(25)
normalSat <- rnorm(25, mean=450, sd=100)
uniformDat <- runif(25)
uniformPerc <- runif(25, min=0, max=100)


test.tutorial <- function(){
	checkEquals(michelson.input.example,  c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880, 1000, 980, 930, 650, 760, 810, 1000, 1000, 960, 960), 
        msg="Michelson input comparison", tolerance=10e-06)
	checkEquals(michelson.test, michelson, msg="Michelson comparison", 
        tolerance=10e-06)
    checkEquals(normal.dat.test, normalDat, msg="normal.dat comparison",
        tolerance=10e-06)
    checkEquals(normal.sat.test, normalSat, msg="normal.sat comparison",
        tolerance=10e-06)
    checkEquals(uniform.dat.test, uniformDat, 
        msg="uniform.dat comparison", 
        tolerance=10e-06)
    checkEquals(uniform.perc.test, uniformPerc,
        msg="uniform.perc comparison",
        tolerance=10e-06)
    checkEquals(simple.seq, 1:10, msg="Simple sequence comparison",
        tolerance=10e-06)
    checkEquals(simple.seq.funcall, seq(length=11, from=10, to=30), 
        msg="Simple sequence function comparison",
        tolerance=10e-06)
    checkEquals(simple.seq.funcall2, seq(from=10, length=20, by=4),
        msg="Simple sequence function comparison",
        tolerance=10e-06)
    checkEquals(michelson.mean, mean(michelson),
        msg="mean comparison",
        tolerance=10e-06)
    checkEquals(michelson.median, median(michelson),
        msg="median comparison",
        tolerance=10e-06)
    checkEquals(michelson.sd, sd(michelson),
        msg="sd comparison",
        tolerance=10e-06)
    checkEquals(michelson.var, var(michelson),
        msg="var comparison",
        tolerance=10e-06)
    checkEquals(michelson.summary, summary(michelson),
        msg="summary comparison",
        tolerance=10e-06)
    checkEquals(attitude.lm1, lm(rating ~ complaints, data=attitude),
        msg="lm comparison",
        tolerance=10e-06)
    checkEquals(attitude.lm1.summary, summary(attitude.lm1),
        msg="lm summary comparison",
        tolerance=10e-06)
    checkEquals(A, matrix(c(3, 5, 7, 9, 13, 15, 8, 4, 2), ncol=3),
        msg="matrix A comparison",
        tolerance=10e-06)
    checkEquals(B, matrix(c(4, 7, 9, 5, 8, 6), ncol=3),
        msg="matrix B comparison",
        tolerance=10e-06)
    checkEquals(A.sum, A+A, msg="matrix add",
        tolerance=10e-06)
    checkEquals(A.product, A*A, msg="element by element matrix multiply", 
        tolerance=10e-06)
    checkEquals(A.matproduct, A %*% A, 
        msg="matrix multiply A %*%",
        tolerance=10e-06)
    checkEquals(BA.matproduct, B %*% A,
        msg="matrix multiply B %*% A",
        tolerance=10e-06)
    checkException(A %*% B, msg="Conformance check")
    checkEquals(A.colprod, apply(A,2,prod),
        msg="prod apply to columns of A",
        tolerance=10e-06)
    checkEquals(A.rowprod, apply(A,1,prod),
        msg="prod apply to rows of A",
        tolerance=10e-06)
    checkEquals(A.colsort, apply(A, 2, sort),
        msg="sort apply to columns of A",
        tolerance=10e-06)
    checkEquals(list1, list(x=1:10, y=c("Tami", "Victor", "Emily"),
        z=matrix(c(3, 5, 4, 7), nrow=2)),
        msg="list comparison")
    checkEquals(list1.lapplytest, lapply(list1, length),
        msg="list apply")
}

test.tutorial.graphics <- function()
{
    defplot <- function(){
        pdf(file="defplot.test.pdf")
        par(mfrow=c(2,2))
        plot(michelson)
        plot(normalDat)
        plot(uniformPerc)
        plot(1:10)
        dev.off()
    }
    res <- try(defplot())
    checkTrue(!is(res, "try-error"), "Basic plot stress test")
    michhist <- function(){
        pdf(file="michhist.test.pdf")
        par(mfrow=c(1,2))
        hist(michelson)
        hist(michelson, nclass=5)
        dev.off
    }
    res <- try(michhist())
    checkTrue(!is(res, "try-error"), "Histogram stress test")
    michqq <- function(){
        pdf(file="michqq.test.pdf")
        qqnorm(michelson)
        dev.off()
    }
    res <- try(michqq())
    checkTrue(!is(res, "try-error"), "QQplot stress test")
    datbox <- function(){
        pdf(file="datbox.test.pdf")
        boxplot(normalDat, uniformDat)
        dev.off()
    }
    res <- try(datbox())
    checkTrue(!is(res, "try-error"), "Boxplot stress test")
    attiplot <- function(){
        pdf(file="attiplot.test.pdf")
        plot(attitude)
        dev.off()
    }
    res <- try(attiplot())
    checkTrue(!is(res, "try-error"), "Pairs plot stress test")
    attilmplot <- function(){
        pdf(file="attitude.lm1.plot.test.pdf")
        par(mfrow=c(2,2))
        plot(attitude.lm1)
        dev.off()
    }
    res <- try(attilmplot())
    checkTrue(!is(res, "try-error"), "lm plot stress test")
}


test.getstart.numeric <- function()
{
    set.seed(14)
    x <- matrix(rnorm(1000000), nrow=1000)
    xout <- numeric(20)
    res <- try(for (i in 1:20) xout[i] <- system.time(eigen(x))[3])
    checkTrue(!is(res, "try-error"), "Eigen stress test")
    xout2 <- numeric(20)
    res <- try(for (i in 1:20) xout2[i] <- system.time(svd(x))[3])
    checkTrue(!is(res, "try-error"), "svd stress test")
    xout3 <- numeric(20)
    res <- try(for (i in 1:20) xout3[i] <- system.time(qr(x))[3])
    checkTrue(!is(res, "try-error"), "qr stress test")
    xout4 <- numeric(20)
    res <- try(for (i in 1:20) xout3[i] <- system.time(lm(x[,i]~x[,-i]))[3])
    checkTrue(!is(res, "try-error"), "lm stress test")
    xout5 <- numeric(20)
    res <- try(for (i in 1:20) xout3[i] <- system.time(t(x) %*% x)[3])
    checkTrue(!is(res, "try-error"), "matrix multiply stress test")
    checkTrue(is.infinite(bignum), "infinity check")
    checkEquals(lilnum, 0, "test of small numbers")
    checkEquals(badex, (11/10 - 1)*10 - 1, "test of floating-point arithmetic")
    checkEquals(diffex, (11/10)*10 - 1*10 - 1, "test 2 of floating-point arithmetic")
    x <- rnorm(100000)
    checkEquals(noassoc, sum(x) - sum(sort(x)), "test of associativity")
}
