newcomb <- c(28, 22, 36, 26, 28, 28, 
             26, 24, 32, 30, 27, 24,
             33, 21, 36, 32, 31, 25,
             24, 25, 28, 36, 27, 32,
             34, 30, 25, 26, 26, 25,
            -44, 23, 21, 30, 33, 29,
             27, 29, 28, 22, 26, 27,
             16, 31, 29, 36, 32, 28,
             40, 19, 37, 23, 32, 29,
             -2, 24, 25, 27, 24, 16,
             29, 20, 28, 27, 39, 23)
newcomb2 <- c(28, 22, NA, 26, 28, 28, 
             26, 24, 32, 30, 27, 24,
             33, 21, 36, 32, 31, 25,
             24, 25, 28, 36, 27, 32,
             34, 30, 25, NA, 26, 25,
            -44, 23, 21, 30, 33, 29,
             27, 29, 28, 22, 26, 27,
             16, 31, 29, 36, 32, 28,
             40, 19, 37, 23, 32, 29,
             NA, 24, 25, 27, 24, 16,
             29, 20, 28, 27, 39, 23)
             
test.mean <- function(){
	checkEquals(mean(newcomb), 26.21212, tolerance=10e-06)
	checkEquals(mean(newcomb, trim=0.2), 27.35, tolerance=10e-06)
	checkEquals(mean(newcomb,trim=0.5), 27, tolerance=10e-06)
	checkEquals(mean(newcomb, trim=0.6), 27, tolerance=10e-06)
	checkEquals(mean(newcomb, trim=0), 26.21212, tolerance=10e-06)
	checkEquals(mean(newcomb, trim=-1), 26.21212, tolerance=10e-06)
	checkTrue(is.na(mean(newcomb2)))
	checkEquals(mean(newcomb2, na.rm=TRUE),26.50794, tolerance=10e-06)
	}
	
test.median <- function() {
	checkEquals(median(newcomb), 27, tolerance=10e-06)
	checkTrue(is.na(median(newcomb2)))
	checkEquals(median(newcomb2, na.rm=TRUE), 27, tolerance=10e-06)
	}
	
test.sd <- function() {
	checkEquals(sd(newcomb), 10.74532, tolerance=10e-06)
  	checkEquals(sd(newcomb2, na.rm=TRUE), 10.32359, tolerance=10e-06)
  	}
	
test.var <- function() {
	checkEquals(var(newcomb), 115.462, tolerance=10e-06)
    checkEquals(var(newcomb2, na.rm=TRUE),106.5765, tolerance=10e-06)
	}
	   
