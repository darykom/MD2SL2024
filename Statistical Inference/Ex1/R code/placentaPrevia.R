rm(list=ls())

# data
N = 980 # casi di placenta previa
s = 437 # figlie nate nei casi di placenta previa
theta_t = 0.485 # prob di confronto

# Ciascuna nascita è una variabile binaria (maschio=0/femmina=1)
# Ogni nascita è indipendente
# La probabilità di avere s=437 femmine su N=980 nascite è modellata 
# attraverso la distribuzione Binomiale, con pdf:
# $\comb{N}{s} p^s (1-p)^(N-s)$
# dove $p$ è la probabilità per una singola nascita che si tratti di una 
# figlia femmina nella popolazione formata dai casi di placenta previa

# La likelihood per $p$ nel caso di distribuzione Binomiale vale
# $L(p) = p^s (1-p)^{N-s}
# Log-likelihood: $l(p) = s \ln p + (N-s) \ln(1-p)$

# definizione funzioni di interesse
bernObj.Pdf <- function(theta, N, s)
{
  return (choose(N,s) * theta^s * (1-theta)^(N-s))
}

bernObj.Likelihood <- function(theta, N, s)
{
  return (theta^s * (1-theta)^(N-s))
}

bernObj.LogLikelihood <- function(theta, N, s)
{
  return (s * log(theta) + (N-s) * log(1-theta))
}

bernObj.Information <- function(theta, N, s)
{
  return (s/theta^2 + (N-s)/(1-theta)^2)
  #return (N/(theta*(1-theta)))
}

bernObj.RelativeLikelihood <- function(theta, theta_Ref, N, s)
{
  return (bernObj.Likelihood(theta,N,s)/bernObj.Likelihood(theta_Ref,N,s))
}


bernObj.LogRelativeLikelihood <- function(theta, theta_Ref, N, s)
{
  return (log(bernObj.RelativeLikelihood(theta, theta_Ref, N, s)))
  #return (s*log(p/pRef) + (N-s)*log((1-p)/(1-pRef)))
}


theta_hat = s/N
obsInfo = bernObj.Information(theta_hat, N, s)
relL = bernObj.RelativeLikelihood(theta_t, theta_hat, N, s)
logRelL = bernObj.LogRelativeLikelihood(theta_t, theta_hat, N, s)

par(mfrow=c(3, 1), mar=c(0,5,2,1))

curve(bernObj.Likelihood(theta=x,N,s), 
      from=0, to=1, n = 2001, 
      lwd=2, col="orange", 
      bty="n", xaxt = "n",
      xlab="", ylab=expression(L(theta)), 
      main="Placenta Previa Model Estimation (N=980, s=437)")
points(x=theta_hat, bernObj.Likelihood(theta_hat,N,s), col="blue")
points(x=theta_t, bernObj.Likelihood(theta_t,N,s), col="red")
axis(1, at=seq(0,1,by=0.1), labels = FALSE)
grid(lty=3, col="gray")

par(mar=c(0,5,2,1))
curve(bernObj.LogLikelihood(theta=x,N,s), 
      from=0, to=1, n = 2001, 
      lwd=2, col="orange", 
      bty="n", xaxt = "n", ylim=c(-1000,-500),
      xlab="", ylab=expression(l(theta)))
points(x=theta_hat, bernObj.LogLikelihood(theta_hat,N,s), col="blue")
points(x=theta_t, bernObj.LogLikelihood(theta_t,N,s), col="red")
axis(1, at=seq(0,1,by=0.1), labels = FALSE)
grid(lty=3, col="gray")

par(mar=c(5,5,2,1))
curve(bernObj.Information(theta=x,N,s), 
      from=0, to=1, n = 2001, 
      lwd=2, col="orange", ylim=c(0,10000),
      bty="n",
      xlab=expression(theta), ylab=expression(j(theta)))

# curve(bernObj.Information(p=x,100,11),
#       from=0, to=1, n = 2001,
#       lwd=2, col="green", add=T,
#       bty="n", ylim=c(0,5000),
#       xlab="prob. p", ylab="obs. information j(p) (log scale)")


points(x=theta_hat, bernObj.Information(theta_hat,N,s), col="blue")#, ylog=T)
points(x=theta_t, bernObj.Information(theta_t,N,s), col="red")
axis(1, at=seq(0,1,by=0.1))
grid(lty=3, col="gray")

# 
# par(mfrow=c(1, 1), mar=c(5,5,2,1))
# curve(bernObj.RelativeLikelihood(theta=x,theta_hat,N,s),
#       from=0, to=1, n = 2001,
#       lwd=2, col="orange",
#       bty = "n",
#       main = "Relative likelihood",
#       xlab=expression(theta), ylab=expression(R(theta,hat(theta))))
# # points(x=pTest, bernObj.RelativeLikelihood(pTest,hatp,N,s), col="blue")
# axis(1, at=seq(0,1,by=0.1))
# grid(lty=3, col="gray")

# 
# bernObj.Information(11/100, 100, 11)
# par(mar=c(5,5,2,1))
# curve(bernObj.Information(p=x,100,11), 
#       from=0, to=1, n = 2001, 
#       lwd=2, col="orange", 
#       bty="n", ylim=c(0,5000),
#       xlab="prob. p", ylab="obs. information j(p) (log scale)")
# points(x=11/100, bernObj.Information(11/100,100,11), col="blue")
# points(x=s/N, bernObj.Information(s/N,N,s), col="red")
# axis(1, at=seq(0,1,by=0.1))
# grid(lty=3, col="gray")
