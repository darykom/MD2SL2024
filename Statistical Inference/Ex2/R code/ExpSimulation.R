rm(list=ls())

library(latex2exp)

n = 5
lambda = 3
N = 10000 # number of samples

barX = rep (0, length = N)
lambdaMLE = rep (0, length = N)
for(k in 1:N)
{
  x = rexp(n, lambda)
  barX[k] = mean(x)
  lambdaMLE[k] = n/sum(x)
}

barLambdaMLE = mean(lambdaMLE)
bias = mean(lambdaMLE) - lambda

barMSE = mean((lambdaMLE-lambda)^2)
S2MLE = var(lambdaMLE)
hatvarMLE = barMSE - 9/16

lambdaPdf <- function(x,lambda,n)
{
  v = n*lambda/x
  pdf = 1/(x*gamma(n)) *v^n * exp(-v)
  return ( pdf )
}

hist(barX, breaks="Freedman-Diaconis", probability = TRUE,
     border = "darkorange1", col="tan1",
     main=TeX(r"(Sampling distribution of mean $\hat{\mu}$ for exp. distrib.)"),
     xlab = TeX(r"($\hat{\mu}$)"))
title(TeX(r"(\small{$n=5$ samples, $\lambda=3$})"), line=1)
grid(lty="solid", lwd=0.5, col="white", nx=NA, ny=NULL)
curve(dgamma(x, shape=n, scale=1/(n*lambda)),
      from=0,to=2, n = 2001,
      ylab="density", lwd=2,col="blue", add=T)
legend("topright", bty = "n",
       legend = c(TeX(r"(simulated $\hat{\mu}$ histogram)"),
                  TeX(r"($\hat{\mu} \sim \Gamma(n,1/(n\lambda)$))")),
       col = c("tan1", "blue"), pch = c(15, NA), lty = c(NA, 1), lwd=c(NA,2),
       inset = 0.03)

hist(lambdaMLE, breaks="Freedman-Diaconis", probability = TRUE,
     border = "darkorange1", col="tan1",
     main=TeX(r"(Sampling distribution of rate $\hat{\lambda}$ for exp. distrib.)"),
     xlab = TeX(r"($\hat{\lambda}$)"))
title(TeX(r"(\small{$n=5$ samples, $\lambda=3$})"), line=1)
grid(lty="solid", lwd=0.5, col="white", nx=NA, ny=NULL)
curve(1/x^2*dgamma(1/x, shape=n, scale=1/(n*lambda)), #trovare Gamma(lambda^-1)
# curve(lambdaPdf(x, lambda=3,n=n), 
      from=0,to=60, n = 2001,
      ylab="density", lwd=2,col="blue", add=T)
legend("topright", bty = "n",
       legend = c(TeX(r"(simulated $\hat{\lambda}$ histogram)"),
                  TeX(r"($\hat{\lambda} \sim \frac{\left(n\lambda/\hat{\lambda}\right)^n}{\hat{\lambda}\, \Gamma(n)} e^{n\lambda/\hat{\lambda}} $)")),
       col = c("tan1", "blue"), pch = c(15, NA), lty = c(NA, 1), lwd=c(NA,2),
       inset = 0.03)
