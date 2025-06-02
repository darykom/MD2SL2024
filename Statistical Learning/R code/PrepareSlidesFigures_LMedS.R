# FIGS slide 4

rm(list = ls())
library(latex2exp) #per label/titoli con LaTeX
library(plot3D) # per mappe 2D

setwd("C:/Users/dario/Documents/Projects/Statistical Learning/R")

source('Palette.R') # miei colori "preferiti" (S.Few, "Show me the numbers")
source('LMedSReg.R') #my Least Median of Squares implementation

save2pdf = F


set.seed(111)
N = 15
sigma_e = 0.1
b0 = 0.4
b1 = 0.5
ofs_out = 8 #shift outlier
x = sort(runif(N, 1, 8))
ygt = b0 + b1*x

e <- rnorm(N,0,sigma_e)
y = ygt + e

Nout = 5
sigma_out = 0.08
xout = rnorm(Nout,1,sigma_out)
yout = rnorm(Nout,ofs_out,sigma_out)
xold = x[1:N]; yold=y[1:N];
x[1:Nout] = xout
y[1:Nout] = yout
modLS = lm(y ~ x)

#plot(x,y)
#abline(coef(modLS)[1], coef(modLS)[2], col=mediumBlue, lwd=2)

if (save2pdf) pdf('./Figs/SimpleRegMultiOutLS01.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
points(xold,yold, pch=1, col=mediumBlue)
grid(lty='dotted')
abline(b0,b1, col='black', lty='dashed')
abline(coef(modLS)[1], coef(modLS)[2], col=mediumBlue, lwd=2)
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)
text(3, 5.5, 'y = 8.704 + -1.246x', adj=0, col = darkBlue)
text(x[2], y[2], TeX(r'(  (X,Y) $\sim$ N($[1~8]^T$, $\sigma_o^2$I)   $\sigma_o=0.08$)'), adj=0, col='black')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

ErrorMapMeanLS <- function(lat,long,pts)
{
   a = cos(lat*pi/180)*sin(long*pi/180)
   b = cos(lat*pi/180)*cos(long*pi/180)
   c = sin(lat*pi/180)
   #res = a*pts$x + b*pts$y + c
   m = -a/b
   q = -c/b
   res = pts$y-(m*pts$x+q)
   return (mean(res*res))
}
colorscale = c(darkPurple, mediumBlue, 'cornsilk', mediumOrange, darkRed)
terrainPaletteGenerator<-colorRampPalette(colorscale)
terrainPalette = terrainPaletteGenerator(255)


lat = seq(-90.25, 90, by = 0.5)
long = seq(-90.25, 90, by = 0.5)
error_data = matrix(NA, nrow=length(lat), ncol=length(long))
pts = data.frame(x=x, y=y)
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMeanLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
#
if (save2pdf) pdf('./Figs/SimpleRegMultiOutMapLS01.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, mean_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
long_rad = atan2(modLS$coefficients[2],-1); 
lat_rad = atan2(modLS$coefficients[1],-1/cos(long_rad))
points(x=180+long_rad*180/pi, y=-lat_rad*180/pi, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

ErrorMapMedianLS <- function(lat,long,pts)
{
   a = cos(lat*pi/180)*sin(long*pi/180)
   b = cos(lat*pi/180)*cos(long*pi/180)
   c = sin(lat*pi/180)
   #res = a*pts$x + b*pts$y + c
   m = -a/b
   q = -c/b
   res = pts$y-(m*pts$x+q)
   return (median(res*res))
}

lat = seq(-90.125, 90, by = 0.2)
long = seq(-90.125, 90, by = 0.2)
error_data = matrix(NA, nrow=length(lat), ncol=length(long))
pts = data.frame(x=x, y=y)
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMedianLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
min_ids = which(logmap==min(logmap), arr.ind=T)
lat_argmin = lat[min_ids[1]]
long_argmin = long[min_ids[2]]
#
if (save2pdf) pdf('./Figs/SimpleRegMultiOutMapLMedS01.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, median_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
points(x=long_argmin, y=lat_argmin, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
# 
aLMedS = cos(lat_argmin*pi/180)*sin(long_argmin*pi/180)
bLMedS = cos(lat_argmin*pi/180)*cos(long_argmin*pi/180)
cLMedS = sin(lat_argmin*pi/180)
mLMedS = -aLMedS/bLMedS
qLMedS = -cLMedS/bLMedS

if (save2pdf) pdf('./Figs/SimpleRegMultiOutLMedS01.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(xold,yold, pch=1, col=mediumBlue)
abline(b0,b1, col='black', lty='dashed')
abline(qLMedS, mLMedS, col=mediumBlue, lwd=2)
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)
text(10.0,6, TeX(r'(min $median_k\, \{r_k^2\}$: $8.793 \cdot 10^{-4}$)'), col=darkBlue, adj=1)
text(8,5, 'y = 0.079 + 0.580x', adj=1, col=darkBlue)
text(x[2], y[2], TeX(r'(  (X,Y) $\sim$ N($[1~8]^T$, $\sigma_o^2$I)   $\sigma_o=0.08$)'), adj=0, col='black')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

#modLMedS = LMedSReg(y, data.frame(x=x), forceExhaustive=T)
#print(summary(modLMedS$Model))

m = (y[11] - y[1])/(x[11]-x[1])
q = y[1] - m*x[1]
r = y- (m*x+q)
print(median(r*r))
if (save2pdf) pdf('./Figs/SimpleRegMultiOutLMedS02.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(xold,yold, pch=1, col=mediumBlue)
points(x[c(1,11)],y[c(1,11)], pch=16, col=mediumPink)
abline(b0,b1, col='black', lty='dashed')
abline(qLMedS, mLMedS, col=mediumBlue, lwd=1)
abline(q, m, col=mediumPink, lwd=2)
#text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)
text(10.0,6.0, TeX(r'(min $median_k\, \{r_k^2\}$: $8.793 \cdot 10^{-4}$)'), col=darkBlue, adj=1)
text(10.0,1.0, TeX(r'($median_k\, \{r_k^2\}$: 0.110)'), col=darkPink, adj=1)
#text(10, 6, 'y = 0.079 + 0.580x', adj=1, col=darkBlue)
text(x[2], y[2], TeX(r'(  (X,Y) $\sim$ N($[1~8]^T$, $\sigma_o^2$I)   $\sigma_o=0.08$)'), adj=0, col='black')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


if (save2pdf) pdf('./Figs/SimpleRegMultiOutMapLMedS02.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, median_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
#
long_rad = atan2(m,-1); lat_rad = atan2(q,-1/cos(long_rad))
points(x=180+long_rad*180/pi, y=-lat_rad*180/pi, pch=16, col=mediumPink)

points(x=long_argmin, y=lat_argmin, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


#-----------------------------------------------------------------------------
m = (y[6] - y[3])/(x[6]-x[3])
q = y[3] - m*x[3]
r = y- (m*x+q)
print(median(r*r))
if (save2pdf) pdf('./Figs/SimpleRegMultiOutLMedS03.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(xold,yold, pch=1, col=mediumBlue)
points(x[c(3,6)],y[c(3,6)], pch=16, col=mediumPink)
abline(b0,b1, col='black', lty='dashed')
abline(qLMedS, mLMedS, col=mediumBlue, lwd=1)
abline(q, m, col=mediumPink, lwd=2)
#text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)
text(10.0,6.0, TeX(r'(min $median_k\, \{r_k^2\}$: $8.793 \cdot 10^{-4}$)'), col=darkBlue, adj=1)
text(4.5,1.0, TeX(r'($median_k\, \{r_k^2\}$: 1.048)'), col=darkPink, adj=0)
#text(10, 6, 'y = 0.079 + 0.580x', adj=1, col=darkBlue)
text(x[2], y[2], TeX(r'(  (X,Y) $\sim$ N($[1~8]^T$, $\sigma_o^2$I)   $\sigma_o=0.08$)'), adj=0, col='black')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

if (save2pdf) pdf('./Figs/SimpleRegMultiOutMapLMedS03.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, median_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
#
long_rad = atan2(m,-1); lat_rad = atan2(q,-1/cos(long_rad))
points(x=180+long_rad*180/pi, y=-lat_rad*180/pi, pch=16, col=mediumPink)

points(x=long_argmin, y=lat_argmin, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


#-----------------------------------------------------------------------------
m = (y[11] - y[9])/(x[11]-x[9])
q = y[9] - m*x[9]
r = y- (m*x+q)
print(median(r*r))
if (save2pdf) pdf('./Figs/SimpleRegMultiOutLMedS04.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(xold,yold, pch=1, col=mediumBlue)
points(x[c(9,11)],y[c(9,11)], pch=16, col=mediumPink)
abline(b0,b1, col='black', lty='dashed')
abline(qLMedS, mLMedS, col=mediumBlue, lwd=1)
abline(q, m, col=mediumPink, lwd=2)
#text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)
text(10.0,6.0, TeX(r'(min $median_k\, \{r_k^2\}$: $8.793 \cdot 10^{-4}$)'), col=darkBlue, adj=1)
text(2.0,1.0, TeX(r'($median_k\, \{r_k^2\}$: $9.288\cdot 10^{-4}$)'), col=darkPink, adj=0)
#text(10, 6, 'y = 0.079 + 0.580x', adj=1, col=darkBlue)
text(x[2], y[2], TeX(r'(  (X,Y) $\sim$ N($[1~8]^T$, $\sigma_o^2$I)   $\sigma_o=0.08$)'), adj=0, col='black')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

if (save2pdf) pdf('./Figs/SimpleRegMultiOutMapLMedS04.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, median_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
#
long_rad = atan2(m,-1); lat_rad = atan2(q,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch=16, col=mediumPink)

points(x=long_argmin, y=lat_argmin, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

modLMedS = LMedSReg(y, data.frame(x=x), forceExhaustive=T)


