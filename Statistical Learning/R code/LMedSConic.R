library(fitConic)

rm(list = ls())

source('Palette.R')

save2pdf = F # per salvare figure in pdf

LMedSConic <- function(x,y)
{
   Iter = 3000
   p = 5
   N = length(x)
   
   pts = cbind(x,y)
   
   combos = replicate(Iter, sort(sample(1:N, size=p, replace=F)))
   
   medRes = apply(combos,2, function(combo){ConicCoreFit(combo, pts)})
   
   best_i = which.min(medRes)
   bestId = combos[,best_i]
   bestMed = medRes[best_i]
   
   s0 = 1.4826*(1+5/(N-p))*bestMed
   mod0 = fitConic(x[bestId], y[bestId])
   absRes = SampsonDistance(mod0$parA, pts)
   inl0 = which(absRes<2.5*s0)

   scal = sqrt(sum(absRes[inl0]^2)/(length(inl0)-p))
   inl = which(absRes<2.5*scal)
   
   mod = fitConic(x[inl], y[inl])
}

ConicCoreFit <- function(idSel, pts)
{
   xs = pts[idSel,1]
   ys = pts[idSel,2]
   n = length(idSel)
   M = cbind(xs^2, xs*ys, ys^2, xs, ys, rep(1,n));
   qrM = qr(M)
   if (qrM$rank<dim(M)[2]-1) #ho p-pla non buona
   {
      return (Inf)
   }
   
   
   #mod = fitConic(xs, ys, conicType='e')
   mod = bootEllipse(xs, ys)
   absdst = SampsonDistance(mod$parA, pts)
   
   med = median(absdst)
   
   return (med)
}

SampsonDistance <- function(parA, pts)
{
   # Ax^2 + Bxy + Cy^2 +Dx + Ey +F = 0 
   a = parA[1]
   b = parA[2]
   c = parA[3]
   d = parA[4]
   e = parA[5]
   f = parA[6]
   
   N = dim(pts)[1]
   cp = c(a, b/2, d/2, b/2, c, e/2, d/2, e/2, f)
   C = matrix(data=cp,nrow=3,ncol=3)
   pts = t(cbind(pts, rep(1,N)))
   u = C%*%pts;
   num = abs(diag(t(pts)%*%u))
   den = 2*sqrt(u[1,]^2 + u[2,]^2)
   dst = num/den
   return(dst)
}


#------------------------------------------------------------------------------
set.seed(111)



# create noisy ellipse
parElipGr <- c(-2.3,4.2,5,3,pi/4)
xe <-seq(-9,9,by=.1)
sep = -3.9
inl = which(xe>sep)
outl = which(xe<=sep)

elipGrn <- createConic(xe[inl], parElipGr, 'e',ranFun=rnorm, noise=0.1)
parCircleGr <- c(-3.3,3.5,3.1,3.1,0)
circleGrn <- createConic(xe[outl], parCircleGr, 'e',ranFun=rnorm, noise=0.1)


pts = rbind(elipGrn, circleGrn)
delenda = which(is.na(pts[,2]))
pts = pts[setdiff(1:dim(pts)[1],delenda),]

modLS = fitConic(pts[,1], pts[,2])
modLMedS = LMedSConic(pts[,1], pts[,2])


if (save2pdf) pdf('./Figs/EllipseFit.pdf', width=5,height=5)
par(mar=c(4,4,1,0.5)) #margini bottom, left, top, right
{
   xp = seq(-7,2,by=.001)
   pGT = createConic(x=xp, param=parElipGr, conicType='e') 
   pLS = createConic(x=xp, param=modLS$parA, conicType='e') 
   pLMedS = createConic(x=xp, param=modLMedS$parA, conicType='e') 
   
   plot(elipGrn, pch=16, cex=0.75, col=lightBlue, bty='n', asp=T, xlim = c(-6.5,2.0))
   points(circleGrn, pch=16, cex=0.75, col=lightPink)
   lines(pLMedS, col=darkGreen,  lwd=1)
   lines(pLS,    col=darkYellow, lwd=1)
   lines(pGT,    col=darkBlue,   lwd=1, lty='dashed')
   grid(lty='dotted')
   text(-5.8,-0.1,'LMedS+RLS', col=darkGreen)
   text(-4,7.5,'LS', col=darkYellow)
}
if (save2pdf) dev.off()
