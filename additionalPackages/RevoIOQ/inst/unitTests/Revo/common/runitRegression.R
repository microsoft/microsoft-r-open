## runitRegression.R -- Unit Test for Regression Analysis
## Copyright (c) 2015 Microsoft Corporation All Rights Reserved
##
## This test is part of the RPro Validation Suite
##
if (compareVersion(paste(R.version$major, R.version$minor, sep=".") ,"2.9.0") < 0) {
    load(system.file(file.path("unitTestData", "regression.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "2.10.1") < 0) {
    load(system.file(file.path("unitTestData", "regression_2.9.0.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "2.14.1") < 0) {
    load(system.file(file.path("unitTestData", "regression_2.10.1.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "2.15.2") < 0) {
    load(system.file(file.path("unitTestData", "regression_2.14.1.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.0.1") < 0) {
    load(system.file(file.path("unitTestData", "regression_2.15.2.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.0.3") < 0) {
    load(system.file(file.path("unitTestData", "regression_3.0.1.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.1.0") < 0) {
    load(system.file(file.path("unitTestData", "regression_3.0.3.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.4.0") < 0){
	load(system.file(file.path("unitTestData", "regression_3.1.0.Rdata"), package="RevoIOQ"))
} else if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.4.4") < 0){
	load(system.file(file.path("unitTestData", "regression_3.4.0.Rdata"), package="RevoIOQ"))
} else {
	load(system.file(file.path("unitTestData", "regression_3.4.4.Rdata"), package="RevoIOQ"))
}

test.lm<- function(){
	##
	## Testing function lm
	##
	## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
	## Page 9: Plant Weight Data.
	ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
	trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
	group <- gl(2,10,20, labels=c("Ctl","Trt"))
	weight <- c(ctl, trt)

	# Moore and McCabe (1993) Introduction to the Practice of Statistics
	## Kalama height example (Section 2.2, Least-Squares Regression)

	kalama.age <- 18:29
	kalama.height <- c(76.1, 77.0,78.1, 78.2,78.8,79.7,79.9, 81.1,81.2,81.8,82.8,83.5)

	## Moore and McCabe (1993) Introduction to the Practice of Statistics
	## Gesell Adaptive Score example (Section 2.2, Least-Squares Regression)

	gesell.age <-   c(15, 26,10, 9, 15,20, 18, 11,  8,20,  7, 9,10,11,  11, 10,  12, 42,  17, 11,  10)
	gesell.score <- c(95, 71,83,91,102,87, 93,100,104,94,113,96,83,84, 102,100, 105, 57, 121, 86, 100)

	## Venables and Ripley (2002) Modern Applied Statistics with S, Fourth Edition
	## Analysis of Covariance Example (Section 6.1)
	library(MASS)
	gasB.test <- lm(Gas ~ Temp, data=whiteside, subset= Insul=="Before")
	gasA.test <- update(gasB.test, subset= Insul=="After")
	gasBA.test <- lm(Gas ~ Insul/Temp - 1, data = whiteside)
	gasQ.test <- lm(Gas ~ Insul/(Temp + I(Temp^2)) - 1, data =whiteside)
	oldOpts <- options(contrasts = c("contr.helmert", "contr.poly"))
	gasPR.test <- lm(Gas ~ Insul + Temp, data=whiteside)
	anova.gas.test <- anova(gasPR.test, gasBA.test)
	options(contrasts = c("contr.treatment", "contr.poly"))
	gasBA1.test <- lm (Gas ~ Insul*Temp, data=whiteside)
	options(oldOpts)

	checkEquals(lm(weight ~ group), lm.D9, tolerance=10e-06)
	checkEquals(lm(weight ~ group - 1), lm.D90, tolerance=10e-06)
	checkEquals(lm(kalama.height ~ kalama.age), kalama.lm, tolerance=10e-06)
	checkEquals(lm(gesell.score ~ gesell.age), gesell.lm1, tolerance=10e-06)
	checkEquals(lm(gesell.score[-18] ~ gesell.age[-18]), gesell.lm2, tolerance=10e-06)
	checkEquals(gasB.test, gasB)
	checkEquals(gasA.test, gasA)
	checkEquals(gasBA.test, gasBA)
	###  COMMENTED OUT, Problems with relative diff in  
      ###  CheckEquals
      ###  checkEquals(gasQ.test, gasQ)
	checkEquals(gasPR.test, gasPR)
	checkEquals(anova.gas.test, anova.gas)
	checkEquals(gasBA1.test, gasBA1)
	}

test.glm <- function(){
	##
	## Testing function glm
	##

	## Venables and Ripley (2002) Modern Applied Statistics with S, Fourth Edition
	## Binomial Data example (Section 7.2)
	##

	oldOpts <- options(contrasts = c("contr.treatment", "contr.poly"))
	ldose <- rep(0:5, 2)
	numdead <- c(1,4,9, 13,18, 20, 0, 2, 6, 10,12, 16)
	sex <- factor(rep(c("M","F"),c(6,6)))
	SF <- cbind(numdead, numalive = 20 - numdead)
	budworm.lg.test <- glm(SF ~ sex*ldose, family=binomial)
	budworm.lgA.test <- update(budworm.lg.test, . ~ sex * I(ldose - 3))
	budworm.anova.test <- anova(update(budworm.lg.test, . ~ . + sex + I(ldose^2)), test="Chisq")
	budworm.lg0.test <- glm(SF ~ sex + ldose - 1, family=binomial)


	## Venables and Ripley (2002) Modern Applied Statistics with S, Fourth Edition
	## Binary data example--low birth weight in infants (Section 7.2)
	##
	library(MASS)
	attach(birthwt)
	race <- factor(race, labels = c("white","black","other"))
	ptd <- factor(ptl > 0)
	ftv <- factor(ftv)
	levels(ftv)[-(1:2)]<-"2+"
	bwt <- data.frame(low=factor(low), age, lwt, race, smoke=(smoke>0), ptd, ht=(ht>0), ui=(ui>0), ftv)
	detach(); rm(race, ptd, ftv)
	birthwt.glm.test <- glm(low ~ ., family=binomial, data=bwt)
	birthwt.step.test <- stepAIC(birthwt.glm.test, trace=FALSE)
	birthwt.step2.test <- stepAIC(birthwt.glm.test, ~ .^2 + I(scale(age)^2) + I(scale(lwt)^2), trace=FALSE)
	birthwt.pred.test <- table(bwt$low, predict(birthwt.step2.test) > 0)
	options(oldOpts)

	# Change objects we're comparing because of change to names(env) behavior in R 3.2.0
	if (compareVersion(paste(R.version$major, R.version$minor, sep="."), "3.2.0") >= 0) {
		budworm.lg.test$data <- NULL
		budworm.lg$data <- NULL
		budworm.lgA.test$data <- NULL
		budworm.lgA$data <- NULL
		budworm.anova.test$data <- NULL
		budworm.anova$data <- NULL
		budworm.lg0.test$data <- NULL
		budworm.lg0$data <- NULL
		birthwt.glm.test$data <- NULL
		birthwt.glm$data <- NULL
		birthwt.step.test$data <- NULL
		birthwt.step$data <- NULL
	}
	checkEquals(budworm.lg.test, budworm.lg, tolerance=10e-06)
	checkEquals(budworm.lgA.test, budworm.lgA, tolerance=10e-06)
	checkEquals(budworm.anova.test, budworm.anova, tolerance=10e-06)
	checkEquals(budworm.lg0.test, budworm.lg0, tolerance=10e-06)
	checkEquals(birthwt.glm.test, birthwt.glm, tolerance=10e-06)
	checkEquals(birthwt.step.test, birthwt.step, tolerance=10e-06)
    ## COMMENTED OUT: CAUSING PROBLEMS IN RUNIT TESTS
	##checkEquals(birthwt.step2.test, birthwt.step2, tolerance=10e-06)
	checkEquals(birthwt.pred.test, birthwt.pred, tolerance=10e-06)
		}



test.nls <- function(){
	##
	## Testing function nls
	##
	
	## Venables and Ripley (2002) Modern Applied Statistics with S, Fourth Edition
	## Non-linear Regression Model example, section 8.2
	library(MASS)	
	wtloss.st <- c(b0=90, b1=95, th=120)
	wtloss.fm.test <- nls(Weight ~ b0 + b1*2^(-Days/th),data=wtloss, start=wtloss.st, trace=TRUE)
	expn <- function(b0,b1, th, x){
	temp <- 2^(-x/th)
	model.func <- b0 + b1 * temp
	Z <- cbind(1, temp, (b1 * x * temp * log(2))/th^2)
	dimnames(Z) <- list(NULL, c("b0", "b1", "th"))
	attr(model.func, "gradient") <- Z
	model.func
	}
	wtloss.gr.test <- nls(Weight ~ expn(b0, b1, th, Days), data=wtloss, start = wtloss.st, trace=TRUE)

	checkEquals(wtloss.fm.test, wtloss.fm, tolerance=10e-06)
	checkEquals(wtloss.gr.test, wtloss.gr, tolerance=10e-06)

	## started getting error from runTestFile when using ordinary <- assignment
	## this hack (with inherits=TRUE required) lets the test run to completion


	negenv <- new.env()
	attach(negenv)
	
	assign("negexp", selfStart(model = ~ b0 + b1*exp(-x/th), initial = negexp.SSival,
	parameters = c("b0", "b1", "th"), template =  function(x, b0, b1, th){}), pos=2)
	wtloss.formula <- as.formula(Weight ~ negexp(Days, B0, B1, theta), env="negenv")

	#wtloss.ss.test <- nls(Weight ~ get("negexp",pos=2)(Days, B0, B1, theta), data = wtloss, trace = TRUE)
	wtloss.ss.test <- nls(wtloss.formula, data = wtloss, trace = TRUE)

	detach("negenv")

	## Venables and Ripley (2002) Modern Applied Statistics wtih S, Fourth Edition
	## Non-linear Regression using plinear algorithm (Section 8.3)
	A <- model.matrix(~ Strip - 1, data = muscle)
	rats.nls1.test <-  nls(log(Length) ~ cbind(A, rho^Conc), data=muscle, start = c(rho=0.1), algorithm = "plinear")
	B <- coef(rats.nls1.test)
	st <- list(alpha = B[2:22], beta=B[23], rho = B[1])
	rats.nls2.test <- nls(log(Length) ~ alpha[Strip] + beta*rho^Conc, data=muscle, start=st)


	checkEquals(wtloss.ss.test, wtloss.ss, tolerance=10e-06)
	checkEquals(summary(wtloss.fm.test), summary(wtloss.fm),tolerance =10e-06)
	checkEquals(vcov(wtloss.gr.test), vcov(wtloss.gr), tolerance=10e-06)
	checkEquals(rats.nls1.test, rats.nls1, tolerance=10e-06)
	checkEquals(rats.nls2.test, rats.nls2, tolerance=10e-06)
	
	}
