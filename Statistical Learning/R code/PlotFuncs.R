# Funzioni di plot
setwd("C:/Users/dario/Documents/Projects/Statistical Learning/R")
source('Palette.R')

SpecificitySensibilityPlot <-function(sens,spec, tlab, maxOccPerc=-1)
{
   # poichè i valori sono discreti , i punti possono sovrapporsi   
   pointCounts = table(x=sens,y=spec)
   dimPtsCounts = dim(pointCounts)

   rowValues = dimnames(pointCounts)[[1]]
   colValues = dimnames(pointCounts)[[2]]
   
   ptsDf = expand.grid(x = rowValues, y = colValues, z=NA)
   for (p in 1:dim(ptsDf)[1])
   {
      xval = ptsDf[p,'x']
      yval = ptsDf[p,'y']
      zval = pointCounts[x=xval, y=yval]
      ptsDf[p,'z'] = zval
   }
   ptsDf = ptsDf[ptsDf$z>0,]
   ptsMatrix = as.matrix(ptsDf)
   xvals = as.double(ptsMatrix[,1])
   yvals = as.double(ptsMatrix[,2])

   par(mar=c(4,4,1,1))
   {
      plot(x = xvals, y = yvals, xlim = c(0,1), ylim = c(0,1), bty='n',
           pch = 16,  cex = 0.75, col=darkRed, 
           main=tlab ,xlab = 'sensitivity (TPR)', ylab = 'specificity (TNR)')
      grid(lty='dotted')
   }
}


PlotHist_b0b1 <- function(est, b0gt,b1gt, colbar,colborder,coldens, 
                          xlim0,ylim0, xlim1,ylim1, xlab0,xlab1,tlab)
{
   par(mfrow=c(1,2), mar=c(4,4,1,1)) #margini bottom, left, top, right
   {
      PlotHistDens(data=est$b0, b0gt, tlab, xlab0, colbar,colborder,coldens, xlim0,ylim0)
      PlotHistDens(data=est$b1, b1gt, tlab, xlab1, colbar,colborder,coldens, xlim1,ylim1)
   }
}

PlotHistDens <- function(data, bgt, tlab, xlab, colbar,colborder,coldens, xlim,ylim)
{
   hist(x=data, prob=T, main=tlab, xlab=xlab, breaks='FD',
        col=colbar,border=colborder,
        xlim=xlim, ylim=ylim)
   grid(lty="solid", lwd=0.5, col="white", nx=NA, ny=NULL)
   
   lines(x=c(bgt,bgt),y=ylim, col=colborder, lty='dashed', lwd=2)
   mn = mean(data); lines(x=c(mn,mn),y=ylim, col=coldens)
   text(mn, ylim[2], paste0(' ',round(mn,4)), adj=0, col=coldens)
   
   lines(density(data), col=coldens, lwd=2)
}