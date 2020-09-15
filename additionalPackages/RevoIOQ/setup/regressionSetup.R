savedObjects <- c("birthwt.glm","birthwt.pred","birthwt.step", "birthwt.step2", "budworm.anova", "budworm.lg", "budworm.lg0", "budworm.lgA", "anova.gas", "gasA", "gasB", "gasBA", "gasBA1", "gasPR", "gasQ", "gesell.lm1", "gesell.lm2", "kalama.lm", "lm.D9", "lm.D90", "wtloss.fm", "wtloss.gr", "wtloss.ss","rats.nls1", "rats.nls2")
library(MASS)
gasB <- lm(Gas ~ Temp, data=whiteside, subset= Insul=="Before")
gasA <- update(gasB, subset= Insul=="After")
gasBA <- lm(Gas ~ Insul/Temp - 1, data = whiteside)
gasQ <- lm(Gas ~ Insul/(Temp + I(Temp^2)) - 1, data =whiteside)
oldOpts <- options(contrasts = c("contr.helmert", "contr.poly"))
gasPR <- lm(Gas ~ Insul + Temp, data=whiteside)
anova.gas <- anova(gasPR, gasBA)
options(contrasts = c("contr.treatment", "contr.poly"))
gasBA1 <- lm (Gas ~ Insul*Temp, data=whiteside)
options(oldOpts)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)

lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1)

# Moore and McCabe (1993) Introduction to the Practice of Statistics
## Kalama height example (Section 2.2, Least-Squares Regression)

kalama.age <- 18:29
kalama.height <- c(76.1, 77.0,78.1, 78.2,78.8,79.7,79.9, 81.1,81.2,81.8,82.8,83.5)

## Moore and McCabe (1993) Introduction to the Practice of Statistics
## Gesell Adaptive Score example (Section 2.2, Least-Squares Regression)

gesell.age <-   c(15, 26,10, 9, 15,20, 18, 11,  8,20,  7, 9,10,11,  11, 10,  12, 42,  17, 11,  10)
gesell.score <- c(95, 71,83,91,102,87, 93,100,104,94,113,96,83,84, 102,100, 105, 57, 121, 86, 100)

kalama.lm <- lm(kalama.height ~ kalama.age)
gesell.lm1 <- lm(gesell.score ~ gesell.age)
gesell.lm2 <- lm(gesell.score[-18] ~ gesell.age[-18])

## Venables and Ripley, Modern Applied Statistics with S, 4th Ed.
## Section 7.2, Binomial Data, Pages 190-197

oldOpts <- options(contrasts = c("contr.treatment", "contr.poly"))
ldose <- rep(0:5, 2)
numdead <- c(1,4,9, 13,18, 20, 0, 2, 6, 10,12, 16)
sex <- factor(rep(c("M","F"),c(6,6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg <- glm(SF ~ sex*ldose, family=binomial)
budworm.lgA <- update(budworm.lg, . ~ sex * I(ldose - 3))
budworm.anova <- anova(update(budworm.lg, . ~ . + sex + I(ldose^2)), test="Chisq")
budworm.lg0 <- glm(SF ~ sex + ldose - 1, family=binomial)
attach(birthwt)
race <- factor(race, labels = c("white","black","other"))
ptd <- factor(ptl > 0)
ftv <- factor(ftv)
levels(ftv)[-(1:2)]<-"2+"
bwt <- data.frame(low=factor(low), age, lwt, race, smoke=(smoke>0), ptd, ht=(ht>0), ui=(ui>0), ftv)
detach(); rm(race, ptd, ftv)
birthwt.glm <- glm(low ~ ., family=binomial, data=bwt)
birthwt.step <- stepAIC(birthwt.glm, trace=FALSE)
birthwt.step2 <- stepAIC(birthwt.glm, ~ .^2 + I(scale(age)^2) + I(scale(lwt)^2), trace=FALSE)
birthwt.pred <- table(bwt$low, predict(birthwt.step2) > 0)
options(oldOpts)

wtloss.st <- c(b0=90, b1=95, th=120)
wtloss.fm <- nls(Weight ~ b0 + b1*2^(-Days/th),data=wtloss, start=wtloss.st, trace=TRUE)
expn <- function(b0,b1, th, x){
    temp <- 2^(-x/th)
    model.func <- b0 + b1 * temp
    Z <- cbind(1, temp, (b1 * x * temp * log(2))/th^2)
    dimnames(Z) <- list(NULL, c("b0", "b1", "th"))
    attr(model.func, "gradient") <- Z
    model.func
    }
wtloss.gr <- nls(Weight ~ expn(b0, b1, th, Days), data=wtloss, start = wtloss.st, trace=TRUE)

negenv <- new.env()
attach(negenv)
           
assign("negexp", selfStart(model = ~ b0 + b1*exp(-x/th), 
    initial = negexp.SSival, parameters = c("b0", "b1", "th"), 
    template =  function(x, b0, b1, th){}), pos=2)
wtloss.formula <- as.formula(Weight ~ negexp(Days, B0, B1, theta), 
    env="negenv")

wtloss.ss <- nls(wtloss.formula, data = wtloss, trace = TRUE)
detach("negenv")

A <- model.matrix(~ Strip - 1, data = muscle)
rats.nls1 <-  nls(log(Length) ~ cbind(A, rho^Conc), data=muscle, start = c(rho=0.1), algorithm = "plinear")
B <- coef(rats.nls1)
st <- list(alpha = B[2:22], beta=B[23], rho = B[1])
rats.nls2 <- nls(log(Length) ~ alpha[Strip] + beta*rho^Conc, data=muscle, start=st)

Rver <- paste(R.version$major, R.version$minor, sep=".")
save(list=savedObjects, file=paste("../inst/unitTestData/regression", "_", Rver, ".Rdata", sep=""))

