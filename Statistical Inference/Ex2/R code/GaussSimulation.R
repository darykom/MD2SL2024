rm(list=ls())

library(latex2exp)

n = 100
mu = 170
sigma = 20
N = 10000 # number of samples

# Check that the sampling distribution of the mean is normal
# Check using simulation that the mean and the standard deviation of
# the sampling distribution of the mean correspond to the theoretical values
barX = rep (0, length = N)
S2 = rep (0, length = N)
for(k in 1:N)
{
  x = rnorm(n, mean = mu, sd = sigma)
  barX[k] = mean(x)
  S2[k] = var(x)
}

# \hat{\mu}_{\bar{X}}: estimated mean of the sampling distribution for \mu
hatmu_barX = mean(barX) 
# \hat{\sigma}^2_{\bar{X}}: estimated mean of the sampling distribution for \mu
hatsigma2_barX = var(barX) 
# \bar{S^2}: average value for S^2
barS2 = mean(S2) 


hist(barX, breaks="Freedman-Diaconis", probability = TRUE,
     border = "darkorange1", col="tan1",
     main=TeX(r"(Sampling distribution of mean $\bar{X}$)"),
     xlab = TeX(r"($\bar{X}$)"))
title(TeX(r"(\small{$n=100$ samples, $\mu=170$, $\sigma=20$ ($N=10000$ trials)})"), 
      line=1)
grid(lty="solid", lwd=0.5, col="white", nx=NA, ny=NULL)
curve(dnorm(x,mean=mu,sd=sigma/sqrt(n)),
  from=100,to=240, n = 2001,
  ylab="density", lwd=2,col="blue", add=T)
legend("topright", bty = "n", 
  legend = c(TeX(r"(simulated $\bar{X}$ histogram)"), 
             TeX(r"($\bar{X} \sim \textit{N}(\mu,\sigma^2/n$))")),
  col = c("tan1", "blue"), pch = c(15, NA), lty = c(NA, 1), lwd=c(NA,2),
  inset = 0.03
)

# Study using simulation the sampling distribution of the variance 𝑆2
# solution: Y=(n-1)*S²/σ² ~ χ²(n-1)
hist(S2, breaks="Freedman-Diaconis", probability = TRUE,
     border = "darkorange1", col="tan1",
     main=TeX(r"(Sampling distribution of variance $S^2$)"),
     xlab = TeX(r"($S^2$)"))
grid(lty="solid", lwd=0.5, col="white", nx=NA, ny=NULL)
title(TeX(r"(\small{$n=100$ samples, $\mu=170$, $\sigma=20$ ($N=10000$ trials)})"), 
      line=1)
curve((n-1)/sigma^2*dchisq(x*(n-1)/sigma^2, df=n-1),
  from=100,to=700, n = 2001,
  ylab="density", lwd=2,col="blue", add=T)
legend("topright", bty = "n", 
       legend = c(TeX(r"(simulated $S^2$ histogram)"), 
                  TeX(r"($S^2 \sim \frac{\sigma^2}{n-1}\chi^2_{n-1}$)")),
       col = c("tan1", "blue"), pch = c(15, NA), lty = c(NA, 1), lwd=c(NA,2),
       inset = 0.03,
)