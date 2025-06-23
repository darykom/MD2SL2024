# Implementazione di LMedS e RLS
# P.J. Rousseeuw and A. Leroy. 
# Robust Regression and Outlier Detection. 
# Wiley Series in Probability and Statistics. Wiley, 1987

SubDfByRows <- function(X,sel)
{
   Xout = X[sel,]
   if (is.vector(Xout)==T)
   {
      Xout = as.data.frame(Xout)
      names(Xout)[[1]] = names(X)[[1]]
   }
   return (Xout)
}


CoreEstimate <- function(idSel, y, P)
{
   # verifico che la p-pla produca una matrice M quadrata di rango massimo,
   # per avere soluzione univoca, attraverso la decomposizione QR di M
   M = P[idSel,] #as.matrix(cbind(Intercept, X[idSel,]))
   qrM = qr(M)
   if (qrM$rank<dim(P)[2]) #ho p-pla non buona
   {
      return (Inf)
   }

   # Risolvo il sistema sfruttando la QR
   beta = qr.coef(qrM,y[idSel])
   # calcolo residui e mediana dei residui al quadrato
   res = y - P %*% beta;
   medRes = median(res*res)
   #medRes = median(abs(res))

   return (medRes)
}

SelectExploration <- function(N,p)
{
   exhaustive = F
   iter = 3000
   if (p<7)
   {
      switch(p,
             {exhaustive = NA; iter=0; print('deterministic location algorithm not implemented')},#p=1
             {exhaustive = N<51; if (exhaustive==F) iter=1000}, #p=2
             {exhaustive = N<23; if (exhaustive==F) iter=1500}, #p=3
             {exhaustive = N<17; if (exhaustive==F) iter=2000}, #p=4
             {exhaustive = N<16; if (exhaustive==F) iter=2500}, #p=5
             {exhaustive = N<15; if (exhaustive==F) iter=3000}, #p=6
      )
      if (exhaustive == T)
         iter = -1
   }
   return (iter)
}



LMedSReg <- function (y, X, forceExhaustive=F)
{
   p = dim(X)[2]+1
   N = length(y)
   
   if (forceExhaustive==T)
      Iter=-1
   else
      Iter = SelectExploration(N,p)
   if (Iter<0)
   {
      #Iter = factorial(N)/(factorial(p)*factorial(N-p))
      combos = combn(1:N, p)
      Iter = dim(combos)[2]
   }
   else
   {
      combos = replicate(Iter, sort(sample(1:N, size=p, replace=F)))
   }
   
   Intercept = rep(1, N)
   P = as.matrix(cbind(Intercept, X))

   medRes = apply(combos,2, function(combo){CoreEstimate(combo, y, P)})

   best_i = which.min(medRes)
   bestId = combos[,best_i]
   bestMed = medRes[best_i]
   #print(bestId)
   
   bestModel = lm(y[bestId] ~ ., data=SubDfByRows(X,bestId))
   s0 = 1.4826 * (1+5/(N-p)) * sqrt(bestMed)

   return (list(Scale=s0, Model=bestModel))
}



RLSreg <- function(y, X, scal0, initModel, computeRes=T)
{
   #RLS p44-45
   p = dim(X)[2]+1
   if (computeRes == T)
   {
      pred = predict(initModel, newdata = X)
      res = y -pred
   }
   else
      res = initModel$residuals
   
   inl0 = which(abs(res/scal0)<=2.5)
   scal = sqrt(sum(res[inl0]*res[inl0])/(length(inl0)-p))
   inl = which(abs(res/scal)<=2.5)
   
   bestModel = lm(y[inl] ~ ., data = SubDfByRows(X,inl))

   return (list(Scale=scal, Model=bestModel, Inliers=inl))
}



