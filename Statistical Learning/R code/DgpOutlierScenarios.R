# slide 8

rm(list = ls())

library(MASS) # per lmsreg con method 'LMS' (Least Median of Squares)
library(latex2exp) #per label/titoli con LaTeX

setwd("C:/Users/dario/Documents/Projects/Statistical Learning/R")
source('LMedSReg.R') #mia implementazione Least Median of Squares
source('Palette.R')  #mia palette preferita di colori
source('PlotFuncs.R')  #mie funzioni per plot vari
source('ModelTests.R') #singolo trial Monte Carlo per vari stimatori
source('DGPs.R') # file con Data Generating Process per vari scenari

#------------------------------------------------------------------------------


save2pdf = F # per salvare figure in pdf

outperc = 0.333
b0 = 0.4
b1 = 0.5 
sigma_e = 0.5


#scenario1
#dgpUniformY=T DGP genera y unif; dgpUniform=F DGP genera y normale; 
dgpUniformY = T 
urange = c(6,10)


N = 49
set.seed(111)

scen = DGP_Outliers_b0b1(3, N,sigma_e, b0,b1, outperc, dgpUniformY, urange)
modLS = lm(scen$Data[,1]~scen$Data[,2])

set.seed(112)
y=scen$Data[,1]
Xdf = data.frame(x=scen$Data[,2])
modLMedS = LMedSReg(y=y, X=Xdf, forceExhaustive=T)
modRLS = RLSreg(y=y, X=Xdf, modLMedS$Scale, modLMedS$Model)


iout = scen$Outliers
pred = predict(modLMedS$Model, newdata = Xdf)
res = y -pred
s0 = modLMedS$Scale
inl0 = which(abs(res/s0)<=2.5)
scal = sqrt(sum(res[inl0]*res[inl0])/(length(inl0)-2))
inl = which(abs(res/scal)<=2.5)


if (save2pdf) pdf('./Figs/RLSexample10.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   text(1,3.5,'LS',col=darkYellow); text(9.5,4,'LMedS',col=darkOrange)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   abline(b0,b1, col='black', lty='dashed')
   abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   abline(coef(modLMedS$Model)[1], coef(modLMedS$Model)[2], col=mediumOrange, lwd=2)
   grid(lty='dotted')
}
if (save2pdf) dev.off()

if (save2pdf) pdf('./Figs/RLSexample11.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   text(1,3.5,'LS',col=darkYellow); text(9.5,4.3,'LMedS',col=darkOrange)
   text(9.5,5.7,'RLS',col=darkGreen)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   points(y=scen$Data[inl0,1], x=scen$Data[inl0,2], col=lightOrange, pch=1,lwd=1.5)
   abline(b0,b1, col='black', lty='dashed')
   abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   abline(coef(modLMedS$Model)[1], coef(modLMedS$Model)[2], col=mediumOrange, lwd=2)
   abline(coef(modRLS$Model)[1], coef(modRLS$Model)[2], col=mediumGreen, lwd=2)
   #abline(coef(modLMedS$Model)[1]-2.5*s0, coef(modLMedS$Model)[2], col=mediumOrange, lwd=1, lty='dashed')
   #abline(coef(modLMedS$Model)[1]+2.5*s0, coef(modLMedS$Model)[2], col=mediumOrange, lwd=1, lty='dashed')
   grid(lty='dotted')
}
if (save2pdf) dev.off()


if (save2pdf) pdf('./Figs/RLSexample12.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   text(1,3.5,'LS',col=darkYellow); text(9.5,4,'LMedS',col=darkOrange)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   points(y=scen$Data[inl,1], x=scen$Data[inl,2], col=lightOrange, pch=1,lwd=1.5)
   abline(b0,b1, col='black', lty='dashed')
   abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   abline(coef(modLMedS$Model)[1], coef(modLMedS$Model)[2], col=mediumOrange, lwd=2)
   abline(coef(modLMedS$Model)[1]-2.5*scal, coef(modLMedS$Model)[2], col=mediumOrange, lwd=1, lty='dashed')
   abline(coef(modLMedS$Model)[1]+2.5*scal, coef(modLMedS$Model)[2], col=mediumOrange, lwd=1, lty='dashed')
   grid(lty='dotted')
}
if (save2pdf) dev.off()



if (save2pdf) pdf('./Figs/RLSexample13.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   text(1,3.5,'LS',col=darkYellow); text(9.5,4.5,'RLS',col=darkGreen)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   points(y=scen$Data[inl,1], x=scen$Data[inl,2], col=lightGreen, pch=1,lwd=1.5)
   abline(b0,b1, col='black', lty='dashed')
   abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   abline(coef(modRLS$Model)[1], coef(modRLS$Model)[2], col=mediumGreen, lwd=2)
   grid(lty='dotted')
}
if (save2pdf) dev.off()

#-------------------------------------------------------------------------------
# scenario 2
dgpUniformY = T 
urange = c(1,5)

N = 49
set.seed(111)

scen = DGP_Outliers_b0b1(3, N,sigma_e, b0,b1, outperc, dgpUniformY, urange)
modLS = lm(scen$Data[,1]~scen$Data[,2])

set.seed(112)
y=scen$Data[,1]
Xdf = data.frame(x=scen$Data[,2])
modLMedS = LMedSReg(y=y, X=Xdf, forceExhaustive=T)
modRLS = RLSreg(y=y, X=Xdf, modLMedS$Scale, modLMedS$Model)


iout = scen$Outliers
pred = predict(modLMedS$Model, newdata = Xdf)
res = y -pred
s0 = modLMedS$Scale
inl0 = which(abs(res/s0)<=2.5)
scal = sqrt(sum(res[inl0]*res[inl0])/(length(inl0)-2))
inl = which(abs(res/scal)<=2.5)


if (save2pdf) pdf('./Figs/RLSexample20.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   #text(1,2.75,'LS',col=darkYellow); text(9.5,5,'LMedS',col=darkOrange)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   abline(b0,b1, col='black', lty='dashed')
   #abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   #abline(coef(modLMedS$Model)[1], coef(modLMedS$Model)[2], col=mediumOrange, lwd=2)
   grid(lty='dotted')
}
if (save2pdf) dev.off()


#-------------------------------------------------------------------------------
# scenario 3
dgpUniformY = F 
cluster = F

N = 49
set.seed(111)

scen = DGP_Outliers_b0b1(3, N,sigma_e, b0,b1, outperc, dgpUniformY, cluster=cluster)
modLS = lm(scen$Data[,1]~scen$Data[,2])

set.seed(112)
y=scen$Data[,1]
Xdf = data.frame(x=scen$Data[,2])
modLMedS = LMedSReg(y=y, X=Xdf, forceExhaustive=T)
modRLS = RLSreg(y=y, X=Xdf, modLMedS$Scale, modLMedS$Model)


iout = scen$Outliers
pred = predict(modLMedS$Model, newdata = Xdf)
res = y -pred
s0 = modLMedS$Scale
inl0 = which(abs(res/s0)<=2.5)
scal = sqrt(sum(res[inl0]*res[inl0])/(length(inl0)-2))
inl = which(abs(res/scal)<=2.5)


if (save2pdf) pdf('./Figs/RLSexample30.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   #text(1,2.75,'LS',col=darkYellow); text(9.5,5,'LMedS',col=darkOrange)
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   abline(b0,b1, col='black', lty='dashed')
   #abline(coef(modLS)[1], coef(modLS)[2], col=mediumYellow, lwd=2)
   #abline(coef(modLMedS$Model)[1], coef(modLMedS$Model)[2], col=mediumOrange, lwd=2)
   grid(lty='dotted')
}
if (save2pdf) dev.off()


#-------------------------------------------------------------------------------
# scenario 4
dgpUniformY = F 
cluster = T

N = 49
set.seed(111)

scen = DGP_Outliers_b0b1(3, N,sigma_e, b0,b1, outperc, dgpUniformY, cluster=cluster)
modLS = lm(scen$Data[,1]~scen$Data[,2])

set.seed(112)
y=scen$Data[,1]
Xdf = data.frame(x=scen$Data[,2])
modLMedS = LMedSReg(y=y, X=Xdf, forceExhaustive=T)
modRLS = RLSreg(y=y, X=Xdf, modLMedS$Scale, modLMedS$Model)


iout = scen$Outliers
pred = predict(modLMedS$Model, newdata = Xdf)
res = y -pred
s0 = modLMedS$Scale
inl0 = which(abs(res/s0)<=2.5)
scal = sqrt(sum(res[inl0]*res[inl0])/(length(inl0)-2))
inl = which(abs(res/scal)<=2.5)


if (save2pdf) pdf('./Figs/RLSexample40.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
{
   plot(y=scen$Data[,1], x=scen$Data[,2], col=mediumGray, pch=16,
        xlim=c(0,10),ylim=c(0,10), xlab='x',ylab='y', bty='n')
   points(y=scen$Data[iout,1], x=scen$Data[iout,2], col=lightGray, pch=16)
   abline(b0,b1, col='black', lty='dashed')
   grid(lty='dotted')
}
if (save2pdf) dev.off()