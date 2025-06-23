# Data Generating Process per y = b0 + b1x senza outliers
DGP_NoOutliers_b0b1 <- function(i, N, sigma_e, b0, b1, dgpUniform=T)
{
   set.seed(13+i)
   
   if (dgpUniform)
      x1 = runif(N, 1, 8)
   else
      x1 = rnorm(N,4.5,1.79)#1.15
   
   e = rnorm(N,0,sigma_e)
   y = b0 + b1*x1 + e
   
   return (cbind(y,x1))
}



# Data Generating Process per y = b0 + b1x CON outliers
DGP_Outliers_b0b1 <- function(i, N, sigma_e, b0, b1, outperc, dgpUniformY=T, urange=NA,cluster=F)
{
   set.seed(13+i)
   
   x1 = runif(N, 1, 8)
   e = rnorm(N,0,sigma_e)
   y = b0 + b1*x1 + e
   
   Nout = round(outperc*N)
   
   
   if (dgpUniformY)
   {
      iOut = sample(1:N, size=Nout, replace=F)
      y[iOut] = runif(Nout, urange[1], urange[2])
   }
   else if (cluster==F)
   {
      iOut = sample(1:N, size=Nout, replace=F)
      x1[iOut] = rnorm(Nout,4.5,1.79)
      y[iOut] = rnorm(Nout,1.5,0.8*sigma_e)
   }
   else
   {
      iOut = sample(1:N, size=Nout, replace=F)
      x1[iOut] = rnorm(Nout,1.0,0.7*sigma_e)
      y[iOut] = rnorm(Nout,8,0.8*sigma_e)
   }
   
   return (list(Data=cbind(y,x1), Outliers=iOut))
}
