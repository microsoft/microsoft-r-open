## runitHypothesis.R -- Unit Test for Hypothesis Testing
## Copyright (c) 2015 Microsoft Corporation All Rights Reserved
##
## This test is part of the Revolution R Enterprise Validation Suite
if (compareVersion(paste(R.version$major, R.version$minor, sep=".") ,"2.13.0") < 0) {
    load(system.file(file.path("unitTestData", "htest.Rdata"), package="RevoIOQ"))
} else {
      load(system.file(file.path("unitTestData", "htest_2.13.0.Rdata"), package="RevoIOQ"))
}

test.binomial <- function(){
	## Moore and McCabe (1993) Introduction to the Practice of Statistics, Second Edition
	## Example 8.4 page 580
	checkEquals(binom.test(2048, 4040, p=.5), buffoncoin, tolerance=10e-06) 
	## Exercise 8.4
	checkEquals(binom.test(41, 216, p=41/216, conf.level=0.99),sycamore, tolerance=10e-06)


	## Morris DeGroot (1975) Probability and Statistics
	## Section 3.2, Exercise 5
	## Random variable X has binomial distribution with n=16 and p=0.5. Find Pr(X<6).
	checkEquals(pbinom(5,16,0.5, lower.tail=TRUE), 0.1050568, tolerance=10e-06)
	## Section 3.2, Exercise 6
	## Random variable X has binomial distribution with n=8 and p=0.7. Find Pr(X >= 5).
	checkEquals(1- pbinom(4,8,0.7,lower.tail=TRUE), 0.8058956, tolerance=10e-06)


	## Larsen and Marx (1981) An Introduction to Mathematical Statistics and its Applications
	## Review Exercises for Chapter 6, #1
	## Birthday effect: 8% of 747 published obituaries were of people who died in 3 months preceding their birthday
	checkEquals(binom.test(60, 747, p=1/4, alternative="less", conf.level=0.99), birthday.effect, tolerance=10e-06)
	## Review Exercises for Chapter 6, #2
	## Mr. Cosmo correctly guesses Zodiacal signs for 10 of 100 individuals. Within chance?
	checkEquals(binom.test(10, 100, p=1/12, alternative="greater", conf.level=0.95), zodiac.effect, tolerance=10e-06)
	

	## Peter Dalgaard (2002) Introductory Statistics with R
	## asthma example p.130
	checkEquals(prop.test(39, 215, 0.15), asthma.prop, tolerance=10e-06) # prop.test approximation
	checkEquals(binom.test(39, 215, 0.15), asthma.binom, tolerance=10e-06) # binom.test exact calculation
}	

test.prop <- function(){
	## Test of two independent proportions
	## Peter Dalgaard (2002) Introductory Statistics with R
	## Lewitt-Machin example, p. 131
	lewitt.machin.success <- c(9,4)
	lewitt.machin.total <- c(12, 13)
	checkEquals(prop.test(lewitt.machin.success, lewitt.machin.total), lewitt.machin.prop, tolerance=10e-06)
	
	## Test of k independent proportions
	## Dalgaard (2002) Introductory Statistics with R
	## Caesarean-shoe size example, p. 133
	caesar.shoe <- matrix(c(5,7,6,7,8,10, 17, 28,36, 41, 46,140),nrow=2, byrow=TRUE)
	colnames(caesar.shoe) <- c("<4", "4", "4.5", "5", "5.5", "6+")
	rownames(caesar.shoe) <- c("Yes", "No")
	caesar.shoe.yes <- caesar.shoe["Yes",]
	caesar.shoe.total <- margin.table(caesar.shoe, 2)
	checkEquals(prop.test(caesar.shoe.yes, caesar.shoe.total), caesar.prop, tolerance=10e-06)
}	

test.chisquare <- function(){
	## Test of chisquare test of independence
	## Peter Dalgaard (2002) Introductory Statistics with R
	## Caffeine consumption and Marital Status example, p. 136
	caff.marital <- matrix(c(652, 1537, 598, 242, 36, 46, 38, 21, 218, 327, 106, 67), nrow=3, byrow=TRUE)
	colnames (caff.marital) <- c("0", "1-150", "151-300", ">300")
	rownames(caff.marital) <- c("Married", "Prev.married", "Single")
	caff.marital.test <- chisq.test(caff.marital)
	E <- caff.marital.test$expected
	O <- caff.marital.test$observed
	contributions.test <- (O-E)^2/E
	checkEquals(caff.marital.test, caff.marital.chisq, tolerance=10e-06)
	checkEquals(contributions.test, contributions, tolerance=10e-06)

	## Brian Everitt and Torsten Hothorn (2006) A Handbook of Statistical Analyses Using R
	## Piston Ring example, pg 25 and 35ff
 	pistonrings <- matrix(c(17,17,12,11,9,13,11,8,19,14,7,28), nrow=4, byrow=TRUE)
	colnames(pistonrings) <- c("North", "Centre", "South")
	rownames(pistonrings) <- c("C1", "C2", "C3", "C4")
	checkEquals(chisq.test(pistonrings), pistonrings.chisq, tolerance=10e-06)

	## Michael Crawley (2007) The R Book
	## Contingency Table example p. 302ff
	eyehair <- matrix(c(38,14,11,51),nrow=2)
	colnames(eyehair) <- c("Blue eyes", "Brown eyes")
	rownames(eyehair) <- c("Fair hair", "Dark hair")
	checkEquals(chisq.test(eyehair), eyehair.chisq, tolerance=10e-06)	
	checkEquals(chisq.test(eyehair, correct=FALSE), eyehair.chisq2, tolerance=10e-06)

	## Larsen and Marx (1981) An Introduction to Mathematical Statistics and Its Applications
	## Unlisted numbers vs home ownership example, p376-377
	phonedata <- matrix(c(628,172,146,54), ncol=2)
	colnames(phonedata) <- c("Listed", "Unlisted")
	rownames(phonedata) <- c("Own", "Rent")
	checkEquals(chisq.test(phonedata), phonedata.chisq, tolerance=10e-06)

	## Delinquency and birth order example, pg 378-379
	delinquency <- matrix(c(24,29,25,23,450,312,211,70),ncol=4, byrow=TRUE)
	colnames(delinquency) <- c("Oldest", "In Between", "Youngest", "Only Child")
	rownames(delinquency) <- c("Delinquent", "Not Delinquent")
	checkEquals(chisq.test(delinquency), delinquency.chisq, tolerance=10e-06)
	}

test.fisher <- function(){
	## Test of Fisher's exact test of independence
	## Peter Dalgaard (2002) Introductory Statistics with R
	## Lewitt-Machin example, pg 132
	lewitt.machin.mat <- matrix(c(9,4,3,9),ncol=2)
	checkEquals(fisher.test(lewitt.machin.mat), lewitt.machin.fisher, tolerance=10e-06)

	## Exercise 7.3--Campbell-Machin peptic ulcer data
	ulcer <- matrix(c(23,7,18,13),ncol=2, byrow=TRUE)
	colnames(ulcer) <- c("Healed", "Not healed")
	rownames(ulcer) <- c("Pirenzepine", "Trithiozine")
	checkEquals(fisher.test(ulcer), ulcer.fisher, tolerance=10e-06)

	## Michael Crawley (2007) The R Book
	## Ant's nest example, p. 308,309
	ants <- matrix(c(6,2,4,8), ncol=2, byrow=TRUE)
	checkEquals(fisher.test(ants), ants.fisher, tolerance=10e-06)
}

test.Student <- function(){
	## Test of the t.test function
	## 1 sample tests
	## Peter Dalgaard (2002) Introductory Statistics with R
	## Daily energy intake example, p. 83
	daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
	checkEquals(t.test(daily.intake, mu=7725), daily.intake.t, tolerance=10e-06)

	## Larsen and Marx (1981) An Introduction to Mathematical Statistics and Its Applications
	## Example 7.3--Shoshoni Indians Golden Ratio example
	shoshoni.rectangles <- c(0.693, 0.749, 0.654, 0.670,0.662, 0.672, 0.615, 0.606, 0.690, 0.628,
				 0.668, 0.611, 0.606, 0.609, 0.601, 0.553, 0.570, 0.844, 0.576, 0.933)
	checkEquals(t.test(shoshoni.rectangles, mu=0.618), shoshoni.rectangles.t, tolerance=10e-06)

	## Chapter 7, Review Exercise 57: Bacillus subtilis enzyme effects
	fevvc.ratio <- c(0.61,0.70, 0.63, 0.76, 0.67, 0.72, 0.64, 0.82, 0.88, 0.82, 0.78, 0.84, 0.83, 0.82, 0.74, 0.85, 0.73, 0.85, 0.87)
	checkEquals(t.test(fevvc.ratio, mu=0.80, alt="less", conf.level=0.975), fevvc.ratio.t, tolerance=10e-06)

	## Morris DeGroot (1975) Probability and Statistics
	## Fiber length example, pg. 415
	## Note: Example provides mean and S^2; fibers data set was randomly generated to match those parameters closely, but not exactly.
	fibers <- c(5.84, 5.15, 5.63, 5.02, 5.14, 5.52, 4.84, 4.91, 5.79, 5.24, 5.39, 6.25, 5.98, 4.99, 5.37)
	checkEquals(t.test(fibers, mu=5.2, alt="greater", conf.level=.95), fibers.t, tolerance=10e-06)

	## 2 sample tests
	## Peter Dalgaard (2002) Introductory Statistics with R
	## Energy expenditure example, p. 87
	expend.obese <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
	expend.lean <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.40, 10.88, 6.13, 7.90, 7.05, 7.48, 7.58, 8.11)
	checkEquals(t.test(expend.obese, expend.lean), expend.t, tolerance=10e-06)
	checkEquals(t.test(expend.obese, expend.lean, var.equal=TRUE), expend.t.vareq, tolerance=10e-06)

	## Larsen and Marx (1981) An Introduction to Mathematical Statistics and Its Applications)
	## Twain/Snodgrass example, pg 324-327
	twain <- c(0.225, 0.262, 0.217, 0.240, 0.230, 0.229, 0.235, 0.217)
	snodgrass <- c(0.209, 0.205, 0.196, 0.210, 0.202, 0.207, 0.224, 0.223, 0.220, 0.201)
	checkEquals(t.test(twain, snodgrass, var.equal=TRUE), twain.snodgrass.t, tolerance=10e-06)
	
	## Chapter 8 Review Exercise 1: Do Short Presidents Live longer?
	short <- c(85, 79, 67, 90, 80)
	tall <- c(68, 53, 65, 63, 70, 88, 74, 64, 66, 60,60, 78, 71, 67, 90, 73, 71, 77, 72, 57, 78, 67, 56, 63, 64,83)
	checkEquals(t.test(short, tall, alt="greater", conf.level=.95, var.equal=TRUE), presidents.t, tolerance=10e-06)
}
