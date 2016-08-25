# Setup script for tutorial test
# 
getstart.saved <- c("michelson.input.example", "michelson.test", 
    "michelson.new.test", "normal.dat.test", "normal.sat.test", 
    "uniform.dat.test", "uniform.perc.test", "simple.seq",
    "simple.seq.funcall", "simple.seq.funcall2", "michelson.mean",
    "michelson.median", "michelson.sd", "michelson.var", "michelson.summary",
    "attitude.lm1", "attitude.lm1.summary", "A", "B", "A.sum", "A.product",
    "A.matproduct", "BA.matproduct", "A.colprod", "A.rowprod", "A.colsort",
    "list1", "list1.lapplytest", "bignum", "lilnum", "badex", "diffex", "noassoc")
michelson.input.example <-
 c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880,
        1000, 980, 930, 650, 760, 810, 1000, 1000, 960, 960)
michelson.test <- c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880,
        1000, 980, 930, 650, 760, 810, 1000, 1000, 960, 960)

michelson <- c(850, 740, 900, 1070, 930, 850, 950, 980, 980, 880,
        1000, 980, 930, 650, 760, 810, 1000, 1000, 960, 960)

michelson.new.test <- c(michelson, 850, 930, 940, 970, 870)

# set.seed(14) for reproducibility
set.seed(14)
normal.dat.test <- rnorm(25)
normal.sat.test <- rnorm(25, mean=450, sd=100)
uniform.dat.test <- runif(25)
uniform.perc.test <- runif(25, min=0, max=100)

simple.seq <- 1:10
simple.seq.funcall <- seq(length=11, from=10, to=30)
simple.seq.funcall2 <-  seq(from=10,length=20, by=4)
michelson.mean <- mean(michelson)
michelson.median <- median(michelson)
michelson.sd <-  sd(michelson)
michelson.var <- var(michelson)
michelson.summary <- summary(michelson)
# ms.stats <- read.table("ms.stats.txt", header=TRUE)
attitude.lm1 <- lm(rating ~ complaints, data=attitude)
attitude.lm1.summary <- summary(attitude.lm1)

A <- matrix(c(3,5,7,9,13,15,8,4,2), ncol=3)
B <- matrix(c(4,7,9,5,8,6), ncol=3)
A.sum <-  A+A
A.product <- A*A
A.matproduct <-  A %*% A
BA.matproduct <- B %*% A
A.colprod <- apply(A,2,prod)
A.rowprod <- apply(A,1,prod)
A.colsort <- apply(A,2,sort)
list1 <- list(x=1:10, y=c("Tami", "Victor", "Emily"), z=matrix(c(3,5,4,7),
             nrow=2))
list1.lapplytest <- lapply(list1, length)

bignum <-  10^308 * 10
lilnum <-  2^(-1074) / 2
 eps <- 1
 while ((1 + eps/2) != 1) { eps <- eps/2 }

badex <- (11/10 - 1)*10 - 1
diffex <-  (11/10)*10 - 1*10 - 1
x <- rnorm(100000)
noassoc <- sum(x) - sum(sort(x))

Rver <- paste(R.version$major, R.version$minor, sep=".")
save(list=getstart.saved, file=paste("../inst/unitTestData/getstart", "_", Rver, ".Rdata", sep=""))

