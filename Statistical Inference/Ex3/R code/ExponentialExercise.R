rm(list=ls())
library(latex2exp)
library(pracma)

y = c(41.53, 18.73, 2.99, 30.34, 12.33, 117.52, 73.02, 223.63, 4.00, 26.78)

n = length(y)
hatmu = mean(y)
s =sum(y)

tauU = qgamma(0.975, shape=n, scale=1)
tauL = qgamma(0.025, shape=n, scale=1)

#U = tauU*hatmu/n
#L = tauL*hatmu/n
(U= n*hatmu/tauL)
(L= n*hatmu/tauU)

gaussL = hatmu*(1-1.96/sqrt(n))
gaussU = hatmu*(1+1.96/sqrt(n))

#--------------------

pdf <-  function(x,n,s) 
{
  v = s/x
  f = 1/(s*gamma(n)) * (v)^(n+1) * exp(-v)
  return (f)
}

cdf <- function(x,n,s)
{
  F = integrate(pdf, lower = 0, upper = x, n=n, s=s)
  return(F[[1]])
}

x = seq(from=1/100, to=200, by=1/200)
for (k in 1:numel(x))
{
  F[k] = cdf(x[k],n,s)
}

quantileFinder <- function (x,n,s, prob)
{
  return (cdf(x,n,s)-prob)
}

rangeInf95 = bisect(quantileFinder, a=10, b=50, n=n,s=s, prob=0.025)
rangeSup95 = bisect(quantileFinder, a=100, b=150, n=n,s=s, prob=0.975)
q = c(rangeInf95[[1]], rangeSup95[[1]])

plot(x=x, y=F,
     bty="n", type="l", 
     xlab = "y", ylab="Probability", main="Exact 95% confidence interval",
     pch=, lwd=2,col="royalblue")
lines(x=c(0,210), y=c(0.025,0.025), col="olivedrab", lty="dashed")
lines(x=c(0,210), y=c(0.975,0.975), col="olivedrab", lty="dashed")
points(x=q, y=c(0.025,0.975), col="royalblue",pch="o")
#
idq = q[1]<=x & x<=q[2]
polygon(c(q[1], x[idq], q[2]), c(0, F[idq], 0), 
        col=rgb(0.25, 0.41, 0.88,0.25), border = NA)
#
grid(lty="dotted", col="gray")
#
legend("topright", bty = "n",
       legend = c(TeX(r"( F\mu(y) )")),
       col = c("royalblue"), pch = c(NA), 
       lty = 1, lwd=2, inset = 0.04)



relLik <- function(x,s,n)
{
  #return (n*log(x) -s*x) #l(\lambda)
  #return (-n*log(x) -s/x) #l(\mu)
  #return ((-n*log(x) -s/x)/(-n*log(s/n) -s/(s/n))) #R(\mu)
  return ( (s/(n*x))^n * exp(n-s/x)  ) #R(\mu)
  #return (log((-n*log(x) -s/x)/(-n*log(s/n) -s/(s/n)) )) #r(\mu)
  #return (n*(1 + log(s)) - (log(n*x) + s/x)) #r(\mu)
  
}

yq = relLik(q,s,n)

curve(relLik(x,s=s,n=n),
      #from=0.01815310,to=0.01815311, n = 2001, #ylim=c(1e,1e),
      from=20,to=120, n = 2001, #ylim=c(1,1.05),
      bty="n", col="royalblue", lwd=2,
      xaxp = c(20,120,10),
      main = "Likelihood confidence interval", 
      xlab = TeX(r"( \mu )"), ylab=TeX(r"( R(\mu) )")
)
lines(x=c(0,210), y=c(0.15,0.15), col="olivedrab", lty="dashed")
points(x=q, y=yq, col="royalblue",pch="o")
grid(lty="dotted", col="gray")
