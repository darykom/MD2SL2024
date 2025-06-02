# Test Monte Carlo e funzioni a latere

CheckOutlierDetection <- function(t, mcData, mcResults)
{
   trialData = mcData[[t]]
   N = dim(trialData$Data)[1]
   outlGt = trialData$Outliers
   inlGt = setdiff(1:N,outlGt)
   #
   mcTrial = mcResults[[t]]
   inlRls = mcTrial$Inliers
   outlRls = setdiff(1:N,inlRls)
   
   pos = length(inlGt)
   neg = length(outlGt)
   truePos  = length(intersect(inlGt,inlRls))
   falseNeg = length(intersect(inlGt,outlRls))
   falsePos = length(intersect(outlGt,inlRls))
   trueNeg  = length(intersect(outlGt,outlRls))
   
   #precision = truePos/(truePos+falsePos)
   #recall = truePos/(truePos+falseNeg)
   
   sensitivity = truePos/pos #true positive rate
   specificity = trueNeg/neg #true negative rate
   
   return (c(sensitivity,specificity))
   
   #inlinl = length(intersect(inlGt,inlRls))/length(inlGt)
   #outout = length(intersect(outlGt,outlRls))/length(outlGt)
   #return (c(inlinl, outout))
}



ExtractCoeffList <- function (model, coefIndex)
{
   return (model$coefficients[coefIndex])
}


TestLS1 <- function(y,x)
{
   mod = lm(y ~ x)
   return (mod)
}


TestLMS1 <- function(y,x)
{
   mod = lmsreg(formula=y ~ x,  method='lms')
   return (mod)
}

TestLMedS1 <- function(y,x)
{
   future.seed=T
   mod = LMedSReg(y=y, X=data.frame(x=x))
   return (mod$Model)
}


#cooks_distance <- cooks.distance(my_lm)
#A common rule of thumb is that observations with Cook's distance greater 
# than 4/p (where p is the number of fitted parameters) might be influential 
# and warrant further investigation

TestLsRLS1 <- function(y,x)
{
   Xdf = data.frame(x=x)
   mod0 = lm(y ~ ., data=Xdf)
   sg = summary(mod0)$sigma
   mod = RLSreg(y=y, X=Xdf, sg, mod0, computeRes=F)
}



TestLmedsRLS1 <- function(y,x)
{
   Xdf = data.frame(x=x)
   mod0 = LMedSReg(y=y, X=Xdf)
   mod = RLSreg(y=y, X=Xdf, mod0$Scale, mod0$Model)
   #return (mod$Model)
   return (list(Model=mod$Model, Inliers=mod$Inliers))
}

TestLmsRLS1 <- function(y,x)
{
   Xdf = data.frame(x=x)
   mod0 = lmsreg(formula=y ~ ., data=Xdf,  method='lms')
   
   p = dim(Xdf)[2]+1
   N = length(y)
   medr2 = median(mod0$residuals*mod0$residuals);
   s0 = 1.4826*(1+5/(N-p))*sqrt(medr2)

   mod = RLSreg(y=y, X=Xdf, s0, mod0, computeRes=F)

   return (list(Model=mod$Model, Inliers=mod$Inliers))
   #return (mod$Model)
}


TestLmedsRLSunc1 <- function(y,x)
{
   Xdf = data.frame(x=x)
   mod0 = LMedSReg(y=y, X=Xdf)
   
   p = dim(Xdf)[2]+1
   N = length(y)
   fact =(1 + 5/(N-p)) 
   s0 = mod0$Scale/fact
   #s0 = 1.4826 * (1+5/(N-p)) * sqrt(bestMed)
   mod = RLSreg(y=y, X=Xdf, s0, mod0$Model)
   return (mod$Model)
}



TestLmsRLSunc1 <- function(y,x)
{
   Xdf = data.frame(x=x)
   mod0 = lmsreg(formula=y ~ ., data=Xdf,  method='lms')
   
   p = dim(Xdf)[2]+1
   N = length(y)
   medr2 = median(mod0$residuals*mod0$residuals);
   s0 = 1.4826*sqrt(medr2)
   
   mod = RLSreg(y=y, X=Xdf, s0, mod0, computeRes=F)
   return (mod$Model)
}
