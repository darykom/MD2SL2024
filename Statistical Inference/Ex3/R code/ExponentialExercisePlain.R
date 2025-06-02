rm(list=ls())
y = c(41.53, 18.73, 2.99, 30.34, 12.33, 117.52, 73.02, 223.63, 4.00, 26.78)

n = length(y)
hatmu = mean(y)

(tauA = qgamma(0.975, shape=n, scale=1))
(tauB = qgamma(0.025, shape=n, scale=1))

(L= n*hatmu/tauA)
(U= n*hatmu/tauB)

(gaussL = hatmu*(1-1.96/sqrt(n)))
(gaussU = hatmu*(1+1.96/sqrt(n)))