rm(list=ls())
library(latex2exp)
library(pracma)

z = c(40 , 38, 40, 35, 36, 37, 41, 40, 37, 38, 40, 38)
y = c(2.968, 2.795, 3.163, 2.925, 2.625, 2.847, 3.292, 3.473, 2.628, 3.176, 3.421, 2.975)

n = numel(z)
barz = mean(z)
bary = mean(y)

numBeta = sum((z-barz)*(y-bary))
denBeta = sum((z-barz)^2)
hatbeta = numBeta/denBeta
hatalpha = bary - hatbeta*barz

t_n = qt(0.975, n-2)
fity = hatalpha + hatbeta*z;
S2 = sum((y-fity)^2)/(n-2)
se = sqrt(S2)/sqrt(sum((z-barz)^2))
(betainf = hatbeta -t_n*se)
(betasup = hatbeta +t_n*se)

tobs = hatbeta/(se)
(pH0 = 1 - pt(q=tobs, df=10))
(pval = 2*pH0)


plot(z,y, bty="n",ylim=c(2.5,3.6))
abline(coef = c(hatalpha,hatbeta), col="olivedrab", lty="dashed", lwd=2)
grid(lty="dotted", col="gray")

mod = lm(y~z)
summary(mod)
confint(mod)
