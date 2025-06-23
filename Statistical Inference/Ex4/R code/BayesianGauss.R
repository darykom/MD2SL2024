rm(list=ls())
#library(latex2exp)
#library(pracma)

PosteriorParams <- function(theta0, sigma0, n, ybar, sigma)
{
   w0 = 1/sigma0^2
   wn = n/sigma^2
   ws = w0 + wn
   thetaPost = (w0*theta0 + wn*ybar)/ws
   sigma2Post = 1/ws
   return (c(thetaPost, sigma2Post))
}

ybar = 850
sigma = 40

theta0A = 900
sigma0A = 20
(postParsA_1   = PosteriorParams(theta0A, sigma0A,  1, ybar, sigma))
(postParsA_15  = PosteriorParams(theta0A, sigma0A, 15, ybar, sigma))
(postParsA_100 = PosteriorParams(theta0A, sigma0A,100, ybar, sigma))

theta0B = 800
sigma0B = 80
(postParsB_1   = PosteriorParams(theta0B, sigma0B,  1, ybar, sigma))
(postParsB_15  = PosteriorParams(theta0B, sigma0B, 15, ybar, sigma))
(postParsB_100 = PosteriorParams(theta0B, sigma0B,100, ybar, sigma))

sigma2Lik1   = sigma^2
sigma2Lik15  = (sigma^2)/15
sigma2Lik100 = (sigma^2)/100
