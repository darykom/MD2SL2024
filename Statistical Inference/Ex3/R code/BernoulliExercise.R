rm(list=ls())
library(latex2exp)
library(pracma)

n = 100
s = 2

hatp = s/n
delta95 = 1.96 * sqrt(hatp*(1-hatp)/n)
(rangeInfN95 = hatp-delta95)
(rangeSupN95 = hatp+delta95)

#--------------------------------------

r <- function(p,hatp,s,n)
{
  return (s*log(p/hatp) + (n-s)*log((1-p)/(1-hatp)))
}

rint <- function(p,hatp,s,n,thr)
{
  return( r(p,hatp,s,n)-thr)
}

thr = -qchisq(p=0.95, df=1)/2
rangeInfLR95 = bisect(rint, a=0.0, b=hatp, hatp=hatp, s=s, n=n ,thr=thr)
rangeSupLR95 = bisect(rint, a=hatp, b=0.5,  hatp=hatp, s=s, n=n, thr=thr)
(pinf = rangeInfLR95[[1]])
(psup = rangeSupLR95[[1]])

###########################################################################
#                                PLOT VARI

L <- function(p,s,n)
{
  return (p^s*(1-p)^(n-s))
}


# log likelihood
curve(r(x, hatp, s, n),
      from=0,to=0.1, n = 2001,
      bty="n",
      xlab = "p", ylab="r(p)", main="Bernoulli relative log-Likelihood",
      lwd=2,col="royalblue")
title("(95% likelihood interval)", line=0.5)
grid(lty="dotted", col="gray")
abline(coef = c(thr,0), col="olivedrab", lty="dashed")
points(x=c(rangeInfLR95[1], rangeSupLR95[1]),y=c(thr, thr), lwd=2, type="o", col="olivedrab")
points(x=0.02,y=0, type="o", col="royalblue", pch=16)
# 
# Gauss vs Likelihood interval
curve(dnorm(x, mean=hatp, sd=sqrt(hatp*(1-hatp)/n)),
      from=-0.02,to=0.1, n = 2001, ylim=c(0,100),
      bty="n", col="orange", lwd=2,
      main="Gaussian vs. Likelihood intervals",
      ylab = "density", xlab="p"
)
title("s=2, n=100", line=0.5)
x <- seq(-0.02,0.1,length=2001)
yn <- dnorm(x,mean=hatp, sd=sqrt(hatp*(1-hatp)/n))
idIn = which(rangeInfN95<=x & x<=rangeSupN95)
polygon(c(x[idIn[1]], x[idIn], x[idIn[length(idIn)]]), c(0, yn[idIn], 0),
        col=rgb(1, 0.65, 0,0.5), border = NA)
yl = L(x,s,n)/beta(-s+n+1,s+1)
curve(L(x,s,n)/beta(-s+n+1,s+1),
      from=0,to=0.1, n = 2001, ylim=c(0,100),
      bty="n", col="royalblue", add=T, lwd=2,
)
idIl = which(rangeInfLR95[[1]]<=x & x<=rangeSupLR95[[1]])
polygon(c(x[idIl[1]], x[idIl], x[idIl[length(idIl)]]), c(0, yl[idIl], 0),
        col=rgb(0.25, 0.41, 0.88,0.25), border = NA)
grid(lty="dotted", col="gray")
legend("topright", bty = "n",
       legend = c(TeX(r"( N(\hat{p}, \hat{p}(1-\hat{p})/n) )"),
                  "Norm. Lik."),
       col = c("orange", "royalblue"), pch = c(NA, NA),
       lty = c(1, 1), lwd=c(2,2), inset = 0.03)

# comparazione Binom vs Gauss
curve(dnorm(x, mean=hatp, sd=sqrt(hatp*(1-hatp)/n)),
      from=0, to=0.1, n=2001, ylim=c(0,100), bty="n",
      col="orange",lwd=2,
      main="Distribution comparison", xlab="p", ylab="density")
# binom
y = n*dbinom(0:10, size = n, prob = hatp)
x = seq(from=0, to=0.1, by=0.01)
points(x,y, type="h", col="purple")
legend("topright", bty = "n",
       legend = c(TeX(r"( 1/n Bin(n,\hat{p}))"), "Gaussian"),
       col = c("purple", "orange"), pch = c(NA, NA),
       lty = c(1, 1), lwd=c(2,2), inset = 0.03)

###########################################################################
### cambio valori per discussione su intervallo Gauss vs Likelihood

n = 1000
s = 20
hatp = s/n
delta95 = 1.96 * sqrt(hatp*(1-hatp)/n)
(rangeInfN95 = hatp-delta95)
(rangeSupN95 = hatp+delta95)
rangeInfLR95 = bisect(rint, a=0.0, b=hatp, hatp=hatp, s=s, n=n ,thr=thr)
rangeSupLR95 = bisect(rint, a=hatp, b=0.5,  hatp=hatp, s=s, n=n, thr=thr)
(pinf = rangeInfLR95[[1]])
(psup = rangeSupLR95[[1]])

# Gauss vs Likelihood interval
curve(dnorm(x, mean=hatp, sd=sqrt(hatp*(1-hatp)/n)),
      from=-0.02,to=0.1, n = 2001, ylim=c(0,100),
      bty="n", col="orange", lwd=2,
      main="Gaussian vs. Likelihood intervals",
      ylab = "density", xlab="p"
)
title("s=20, n=1000", line=0.5)
x <- seq(-0.02,0.1,length=2001)
yn <- dnorm(x,mean=hatp, sd=sqrt(hatp*(1-hatp)/n))
idIn = which(rangeInfN95<=x & x<=rangeSupN95)
polygon(c(x[idIn[1]], x[idIn], x[idIn[length(idIn)]]), c(0, yn[idIn], 0),
        col=rgb(1, 0.65, 0,0.5), border = NA)
yl = L(x,s,n)/beta(-s+n+1,s+1)
curve(L(x,s,n)/beta(-s+n+1,s+1),
      from=0,to=0.1, n = 2001, ylim=c(0,100),
      bty="n", col="royalblue", add=T, lwd=2,
)
idIl = which(rangeInfLR95[[1]]<=x & x<=rangeSupLR95[[1]])
polygon(c(x[idIl[1]], x[idIl], x[idIl[length(idIl)]]), c(0, yl[idIl], 0),
        col=rgb(0.25, 0.41, 0.88,0.25), border = NA)
grid(lty="dotted", col="gray")
legend("topright", bty = "n",
       legend = c(TeX(r"( N(\hat{p}, \hat{p}(1-\hat{p})/n) )"),
                  "Norm. Lik."),
       col = c("orange", "royalblue"), pch = c(NA, NA),
       lty = c(1, 1), lwd=c(2,2), inset = 0.03)

# comparazione Binom vs Gauss
curve(dnorm(x, mean=hatp, sd=sqrt(hatp*(1-hatp)/n)),
      from=0, to=0.1, n=2001, ylim=c(0,100), bty="n",
      col="orange",lwd=2,
      main="Distribution comparison", xlab="p", ylab="density")
# binom
y = n*dbinom(0:100, size = n, prob = hatp)
x = seq(from=0, to=0.1, by=0.001)
points(x,y, type="h", col="purple")
legend("topright", bty = "n",
       legend = c(TeX(r"( 1/n Bin(n,\hat{p}))"), "Gaussian"),
       col = c("purple", "orange"), pch = c(NA, NA),
       lty = c(1, 1), lwd=c(2,2), inset = 0.03)

###########################################################################
n = 100
s = 20
hatp = s/n
delta95 = 1.96 * sqrt(hatp*(1-hatp)/n)
(rangeInfN95 = hatp-delta95)
(rangeSupN95 = hatp+delta95)
rangeInfLR95 = bisect(rint, a=0.0, b=hatp, hatp=hatp, s=s, n=n ,thr=thr)
rangeSupLR95 = bisect(rint, a=hatp, b=0.5,  hatp=hatp, s=s, n=n, thr=thr)
(pinf = rangeInfLR95[[1]])
(psup = rangeSupLR95[[1]])

# Gauss vs Likelihood interval
curve(dnorm(x, mean=hatp, sd=sqrt(hatp*(1-hatp)/n)),
      from=0,to=0.5, n = 2001, ylim=c(0,10),
      bty="n", col="orange", lwd=2,
      main="Gaussian vs. Likelihood intervals",
      ylab = "density", xlab="p"
)
title("s=20, n=100", line=0.5)
x <- seq(0,0.5,length=2001)
yn <- dnorm(x,mean=hatp, sd=sqrt(hatp*(1-hatp)/n))
idIn = which(rangeInfN95<=x & x<=rangeSupN95)
polygon(c(x[idIn[1]], x[idIn], x[idIn[length(idIn)]]), c(0, yn[idIn], 0),
        col=rgb(1, 0.65, 0,0.5), border = NA)
yl = L(x,s,n)/beta(-s+n+1,s+1)
curve(L(x,s,n)/beta(-s+n+1,s+1),
      from=0,to=0.5, n = 2001, ylim=c(0,10),
      bty="n", col="royalblue", add=T, lwd=2,
)
idIl = which(rangeInfLR95[[1]]<=x & x<=rangeSupLR95[[1]])
polygon(c(x[idIl[1]], x[idIl], x[idIl[length(idIl)]]), c(0, yl[idIl], 0),
        col=rgb(0.25, 0.41, 0.88,0.25), border = NA)
grid(lty="dotted", col="gray")
legend("topright", bty = "n",
       legend = c(TeX(r"( N(\hat{p}, \hat{p}(1-\hat{p})/n) )"),
                  "Norm. Lik."),
       col = c("orange", "royalblue"), pch = c(NA, NA),
       lty = c(1, 1), lwd=c(2,2), inset = 0.03)

###########################################################################
