[Profile Likelihood Confidence Intervals for GLM's](http://www.math.umt.edu/patterson/ProfileLikelihoodCI.pdf)
bid <- c(1,5,10,20,30,40,50,75,100,150,200)
n <- c(31,29,27,25,23,21,19,17,15,15,15)
y <- c(0,3,6,7,9,13,17,12,11,14,13)
m1 <- glm(y/n~log(bid),weights=n,family=binomial)
k <- 200
b2 <- seq(.7,2,length=k)
w <- rep(0,k)
for(i in 1:k){
    mm <- glm(y/n~1,offset=b2[i]*log(bid),weights=n,family=binomial)
    w[i] <- logLik(mm)
}
f <- function(b2,n,x,y,maxloglik){
      mm <- glm(y/n~1,offset=b2*x,weights=n,family=binomial)
      logLik(mm) - maxloglik + qchisq(.95,1)/2
  }


library(MASS)
# The quine data are analysed in Section 7.
data(quine)
# We fit a model with just the main effect
squine.nb1 <- glm.nb(Days ~ Eth + Sex + Age + Lrn, data = quine)
# The data list includes the design matrix and the response 
vectorquinedata <-list(X=model.matrix(quine.nb1), y=quine$Days)
# Let us define the various functions
# Log likelihood, log linklog
LikNbin <- function(theta,data){y <- data$y
X <- data$X
eta <- X %*% theta[1:ncol(X)]
mu <- exp(eta)
alpha <- theta[ncol(X)+1]
l <- sum(lgamma(y + alpha) + y * log(mu) - (alpha + y) * log(alpha + mu)- lgamma(alpha) + alpha * log(alpha))
return(l)
}
# Score function 
gradLikNbin <- function(theta,data){
	y <- data$yX <- data$X
	eta <- X %*% theta[1:ncol(X)]
	mu <- exp(eta)
	alpha <- theta[ncol(X)+1]
	g <-rep(0,ncol(X)+1)
	g[1:ncol(X)] <- t(y - (alpha+y)*mu / (alpha+mu)) %*% Xg[ncol(X)+1] <- sum(digamma( y + alpha) - log(alpha + mu) - (alpha + y) / (alpha + mu)- digamma(alpha) + 1 + log(alpha))
	return(g)}
# Data generatorgen
DataNbin<- function(theta,data){
	out <- dataX <- data$Xeta <- X %*% theta[1:ncol(X)]
	mu <- exp(eta)
	out$y <- rnegbin(length(data$y), mu=mu, theta=theta[ncol(X)+1])
	return(out)
}

update.packages(repos='https://cran.rstudio.com/', ask=FALSE, checkBuilt=TRUE)

data(sleep)
difference <- sleep$extra[11:20]-sleep$extra[1:10]
Lp <- function(mu, x) {n <- length(x); mean( (x-mu)**2 )**(-n/2) }
mu <- seq(0,3, length=501)
plot(mu, sapply(mu, Lp, x = difference), type="l")

http://people.reed.edu/~jones/Courses/GammaMLE.R
# profile likelihood
prof.LLa <- function(a,X,mle){
# compute profile log likelihood on values in a, max over beta
  n <- length(a)
  Cov <- solve(mle$hessian)
  seb <- sqrt(diag(Cov))[2]
  r <- mle$est[2] + c(-1,1)*3*seb
  profLL <- rep(0,n)
  for(i in 1:n){
      profLL[i] <- optimize(LLprofa,interval=r,alpha = a[i],X=X)$objective
  }

library(MASS)
options(contrasts = c("contr.treatment", "contr.poly"))
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
pr1 <- profile(budworm.lg)
plot(pr1)
pairs(pr1)

xx <- profilelike.glm(y ~ x1 + x2, data=dataglm, profile.theta="group",family=binomial(link="logit"), length=500, round=2)
library(ProfileLikelihood)
data(dataglm)
xx <- profilelike.glm(y ~ x1, data=dataglm, profile.theta="group",family=binomial(link="logit"), length=500, round=2)

m <- model.frame(y ~ x1, dataglm)
X <- model.matrix(y ~ x1, m)
profile.theta <- dataglm$group
theta.off <- data[, names(dataglm) == profile.theta]

profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2) 

function (formula, data, profile.theta, family = gaussian, offset.glm = NULL, 
    lo.theta = NULL, hi.theta = NULL, length = 300, round = 2, 
    subset = NULL, weights = NULL, offset = NULL, ...) 
{
                                    if (!is.null(subset)) {
                                        stop("Warning message: 'subset' should not be provided")
                                    }
                                    if (!is.null(weights)) {
                                        stop("Warning message: 'weights' should not be provided")
                                    }
                                    if (!is.null(offset)) {
                                        stop("Warning message: do not use 'offset'; use 'offset.glm' instead of 'offset' ")
                                    }
    m <- model.frame(formula, data)
    X <- model.matrix(formula, m)
    y <- model.response(m)
    theta.off <- data[, names(data) == profile.theta]
    if (!is.numeric(theta.off)) {
        stop("Warning message: 'profile.theta' must be a numeric variable")
    }
    if ((length(theta.off) != length(y) | length(theta.off) != 
        length(X[, 1]) | length(y) != length(X[, 1]))) {
        cat("Warning message: remove missing data \n")
    }
    if ((is.null(lo.theta) | is.null(hi.theta))) {
        cat("Warning message: provide lo.theta and hi.theta \n")
        fit <- glm(y ~ -1 + X + theta.off, family = family, na.action = na.fail)
        mle <- summary(fit)$coefficient["theta.off", 1]
        se <- summary(fit)$coefficient["theta.off", 2]
        lo.theta <- round(mle - 4 * se, round)
        hi.theta <- round(mle + 4 * se, round)
    }
    theta <- seq(from = lo.theta, to = hi.theta, length = length)
    log.lik <- rep(NA, length)
    for (i in 1:length) {
        pi <- theta[i]
        fit <- glm(y ~ -1 + X + offset(pi * theta.off), family = family, 
            na.action = na.fail)
        if (!is.null(offset.glm)) {
            glm.off <- data[, names(data) == offset.glm]
            fit <- glm(y ~ -1 + X + offset(pi * theta.off) + 
                offset(glm.off), family = family, na.action = na.fail)
        }
        log.lik[i] <- logLik(fit)
    }
    theta <- theta[is.na(log.lik) != 1]
    log.lik <- log.lik[is.na(log.lik) != 1]
    profile.lik <- exp(log.lik)
    mm <- max(log.lik, na.rm = TRUE)
    log.norm.lik <- log.lik - mm
    profile.lik.norm <- exp(log.norm.lik)
    return(list(theta = theta, profile.lik = profile.lik, profile.lik.norm = profile.lik.norm))

[Profile Likelihood](https://freakonometrics.hypotheses.org/20573)

set.seed(1)
x=exp(rnorm(100))

library(MASS)
F=fitdistr(x,"gamma")
     shape       rate   
  1.4214497   0.8619969 
 (0.1822570) (0.1320717)
F$estimate[1]+c(-1,1)*1.96*F$sd[1]


> With this added precision, we can see that the confint.default() function in the MASS library generates the Wald confidence limits, while the confint() function produces the profile-likelihood limits. This also explains the confint() comment "Waiting for profiling to be done..." Thus neither CI from the MASS library is incorrect, though the profile-likelihood method is thought to be superior, especially for small sample sizes. Little practical difference is seen here. 

    exp(confint(glmmod))

    library(MASS)
    exp(confint.default(glmmod))
