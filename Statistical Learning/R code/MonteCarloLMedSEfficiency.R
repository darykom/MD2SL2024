rm(list = ls())

library(MASS) # per lmsreg con method 'LMS' (Least Median of Squares)
library(latex2exp) #per label/titoli con LaTeX
library(future)
library(future.apply) # per parallelizzare

setwd("C:/Users/dario/Documents/Projects/Statistical Learning/R")
source('LMedSReg.R') #mia implementazione Least Median of Squares
source('Palette.R')  #mia palette preferita di colori
source('PlotFuncs.R')  #mie funzioni per plot vari
source('ModelTests.R') #singolo trial Monte Carlo per vari stimatori



#------------------------------------------------------------------------------
plan(multisession)


save2pdf = F # per salvare figure in pdf

# simulation setting 
nsim = 10000
b0 = 0.4
b1 = 0.5 
sigma_e = 0.5

# scenario setting
Ntests = c(15, 49, 99)
dgpUniform = F #dgpUniform=T DGP genera x unif; dgpUniform=F DGP genera x normale; 

dgpType = 'Uniform'
if (dgpUniform == F)
   dgpType = 'Normal'

for (N in Ntests)
{
   cat('Data Size :', N, '\n') 

   set.seed(111)
   cat('Generating ', nsim, 'trials\n')
   allData = lapply(1:nsim, function(s){DGP_NoOutliers_b0b1(s, N,sigma_e, b0,b1, dgpUniform)}) 

   set.seed(112)
   cat('Computing LS results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LS = future_lapply(allData, function(s) TestLS1(s[,1],s[,2]))
   estLSb0 = sapply(LS, function(mod) ExtractCoeffList(mod,1))
   estLSb1 = sapply(LS, function(mod) ExtractCoeffList(mod,2))
   estLS = data.frame(b0=estLSb0, b1=estLSb1)
   print(round(c(mean(estLSb0),sd(estLSb0),  mean(estLSb1),sd(estLSb1)),4))

   set.seed(112)
   cat('Computing LMedS results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LMedS = future_lapply(allData, function(s) TestLMedS1(s[,1],s[,2]), future.seed=T)
   estLMedSb0 = sapply(LMedS, function(mod) ExtractCoeffList(mod,1))
   estLMedSb1 = sapply(LMedS, function(mod) ExtractCoeffList(mod,2))
   estLMedS = data.frame(b0=estLMedSb0, b1=estLMedSb1)
   print(round(c(mean(estLMedSb0),sd(estLMedSb0),  mean(estLMedSb1),sd(estLMedSb1)),4))

   
   set.seed(112)
   cat('Computing LMS results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LMS = future_lapply(allData, function(s) TestLMS1(s[,1],s[,2]))
   estLMSb0 = sapply(LMS, function(mod) ExtractCoeffList(mod,1))
   estLMSb1 = sapply(LMS, function(mod) ExtractCoeffList(mod,2))
   estLMS = data.frame(b0=estLMSb0, b1=estLMSb1)
   print(round(c(mean(estLMSb0),sd(estLMSb0),  mean(estLMSb1),sd(estLMSb1)),4))

   set.seed(112)
   cat('Computing LMedS+RLS results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LmedsRLS = future_lapply(allData, function(s) TestLmedsRLS1(s[,1],s[,2]), future.seed=T)
   estLmedsRLSb0 = sapply(LmedsRLS, function(mod) ExtractCoeffList(mod,1))
   estLmedsRLSb1 = sapply(LmedsRLS, function(mod) ExtractCoeffList(mod,2))
   estLmedsRLS = data.frame(b0=estLmedsRLSb0, b1=estLmedsRLSb1)
   print(round(c(mean(estLmedsRLSb0),sd(estLmedsRLSb0),  mean(estLmedsRLSb1),sd(estLmedsRLSb1)),4))

    
   set.seed(112)
   cat('Computing LMedS+RLS (s0 uncomp.) results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LmedsRLSunc = future_lapply(allData, function(s) TestLmedsRLSunc1(s[,1],s[,2]), future.seed=T)
   estLmedsRLSuncb0 = sapply(LmedsRLSunc, function(mod) ExtractCoeffList(mod,1))
   estLmedsRLSuncb1 = sapply(LmedsRLSunc, function(mod) ExtractCoeffList(mod,2))
   estLmedsRLSunc = data.frame(b0=estLmedsRLSuncb0, b1=estLmedsRLSuncb1)
   print(round(c(mean(estLmedsRLSuncb0),sd(estLmedsRLSuncb0),  mean(estLmedsRLSuncb1),sd(estLmedsRLSuncb1)),4))
   
   set.seed(112)
   cat('Computing LMS+RLS results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LmsRLS = future_lapply(allData, function(s) TestLmsRLS1(s[,1],s[,2]))
   estLmsRLSb0 = sapply(LmsRLS, function(mod) ExtractCoeffList(mod,1))
   estLmsRLSb1 = sapply(LmsRLS, function(mod) ExtractCoeffList(mod,2))
   estLmsRLS = data.frame(b0=estLmsRLSb0, b1=estLmsRLSb1)
   print(round(c(mean(estLmsRLSb0),sd(estLmsRLSb0),  mean(estLmsRLSb1),sd(estLmsRLSb1)),4))
   

   set.seed(112)
   cat('Computing LMS+RLS (s0 uncomp.) results: beta0(mu,sigma), beta1(mu,sigma)\n')
   LmsRLSunc = future_lapply(allData, function(s) TestLmsRLSunc1(s[,1],s[,2]))
   estLmsRLSuncb0 = sapply(LmsRLSunc, function(mod) ExtractCoeffList(mod,1))
   estLmsRLSuncb1 = sapply(LmsRLSunc, function(mod) ExtractCoeffList(mod,2))
   estLmsRLSunc = data.frame(b0=estLmsRLSuncb0, b1=estLmsRLSuncb1)
   print(round(c(mean(estLmsRLSuncb0),sd(estLmsRLSuncb0),  mean(estLmsRLSuncb1),sd(estLmsRLSuncb1)),4))
   
   #---------------------------------------------------------------------------

   # estremi assi istogrammi
   bmin0 = -3;    bmax0 =  5 
   bmin1 = -0.5;  bmax1 =  1.5 
   ymax0 = 3; ymax1 = 15

   sdb0 = round(sd(estLSb0),4)
   sdb1 = round(sd(estLSb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLs', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLS,
                 b0,b1,
                 colbar=lightYellow,
                 colborder=mediumYellow,
                 coldens=darkRed,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LS')
   if (save2pdf) dev.off()
   
   
   sdb0 = round(sd(estLMedSb0),4)
   sdb1 = round(sd(estLMedSb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLmeds', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLMedS,
                 b0,b1,
                 colbar=lightOrange,
                 colborder=mediumOrange,
                 coldens=darkPurple,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMedS')
   if (save2pdf) dev.off()
   
   
   sdb0 = round(sd(estLMSb0),4)
   sdb1 = round(sd(estLMSb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLms', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLMS,
                 b0,b1,
                 colbar=lightPink,
                 colborder=mediumPink,
                 coldens=darkBlue,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMS')
   if (save2pdf) dev.off()
   
   
   sdb0 = round(sd(estLmedsRLSb0),4)
   sdb1 = round(sd(estLmedsRLSb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLmedsRls', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLmedsRLS,
                 b0,b1,
                 colbar=lightGreen,
                 colborder=mediumGreen,
                 coldens=darkRed,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMedS+RLS')
   if (save2pdf) dev.off()
   
   sdb0 = round(sd(estLmedsRLSuncb0),4)
   sdb1 = round(sd(estLmedsRLSuncb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLmedsRlsunc', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLmedsRLSunc,
                 b0,b1,
                 colbar=lightGreen,
                 colborder=mediumGreen,
                 coldens=darkRed,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMedS+RLSunc')
   if (save2pdf) dev.off()
   
   
   sdb0 = round(sd(estLmsRLSb0),4)
   sdb1 = round(sd(estLmsRLSb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLmsRls', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLmsRLS,
                 b0,b1,
                 colbar=lightBrown,
                 colborder=mediumBrown,
                 coldens=darkBlue,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMS+RLS')
   if (save2pdf) dev.off()
   
   
   sdb0 = round(sd(estLmsRLSuncb0),4)
   sdb1 = round(sd(estLmsRLSuncb1),4)
   xlab0str = paste0("$sd(\\beta_0)=", sdb0, "$")
   xlab1str = paste0("$sd(\\beta_1)=", sdb1, "$")
   fname = paste0('./Figs/McEfficencyTestLmsRlsunc', dgpType, 'N', N, '.pdf')
   if (save2pdf) pdf(fname, width=5,height=3.4)
   PlotHist_b0b1(estLmsRLSunc,
                 b0,b1,
                 colbar=lightBrown,
                 colborder=mediumBrown,
                 coldens=darkBlue,
                 xlim0=c(bmin0,bmax0), ylim0=c(0,ymax0),
                 xlim1=c(bmin1,bmax1), ylim1=c(0,ymax1),
                 xlab0=TeX(xlab0str),
                 xlab1=TeX(xlab1str),
                 tlab='LMS+RLSunc')
   if (save2pdf) dev.off()
   
}

plan(sequential)
