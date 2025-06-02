# FIGS slides 3-4


rm(list = ls())
library(latex2exp) #per label/titoli con LaTeX
library(plot3D) # per mappe 2D
library(RColorBrewer) # per colori palette
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

if (save2pdf) pdf('./Figs/SimpleReg00.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,ygt,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
abline(b0,b1, col='black', lty='dashed')
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)#, col = "red", cex = 1.2
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

e <- rnorm(N,0,sigma_e)
y = ygt + e
modLS = lm(y ~ x)

if (save2pdf) pdf('./Figs/SimpleReg01.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
abline(b0,b1, col='black', lty='dashed')
abline(coef(modLS)[1], coef(modLS)[2], col=mediumBlue, lwd=2)
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)#, col = "red", cex = 1.2
text(8, 5, 'y = 0.39 + 0.51x', adj=1, col = darkBlue)
#text(8, 4, 'y = 2.63 + 0.03x', adj=1)#, col = "red", cex = 1.2
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

y[2] = y[2] + ofs_out
modLS = lm(y ~ x)
if (save2pdf) pdf('./Figs/SimpleReg02.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
colpts = rep(mediumBlue,1,N); colpts[2] = mediumBlue
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(x[2],y[2]-8,pch=1,col=mediumBlue)
abline(b0,b1, col='black', lty='dashed')
abline(coef(modLS)[1], coef(modLS)[2], col=mediumBlue, lwd=2)
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)#, col = "red", cex = 1.2
text(6, 2.5, 'y = 2.63 + 0.03x', adj=0, col=darkBlue)
text(x[2], y[2], '  punto di leva', adj=0, col='black')
#text(8, 4, 'y = 2.63 + 0.03x', adj=1)#, col = "red", cex = 1.2
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

# ritolgo l'outlier
y[2] = y[2] - ofs_out; modLS = lm(y ~ x)
lat = seq(-90.25, 90, by = 0.5)
long = seq(0.5, 360, by = 1)
#long = seq(-90.25, 90, by = 0.5)#
error_data = matrix(NA, nrow=length(lat), ncol=length(long))
pts = data.frame(x=x, y=y)
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMeanLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
#write.table(logmap, file="my_data.txt", sep=' ', row.names=F, col.names=F)
#
#colorscale <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(25))
#colorscale = c(darkPurple, mediumBlue, lightPink, mediumOrange, darkRed)
colorscale = c(darkPurple, mediumBlue, 'cornsilk', mediumOrange, darkRed)
terrainPaletteGenerator<-colorRampPalette(colorscale)
terrainPalette = terrainPaletteGenerator(255)
if (save2pdf) pdf('./Figs/SimpleRegMapLS01_long360.pdf', width=10,height=5)
par(mar=c(4,4,2,1)) #margini bottom, left, top, right
#x, y: Vectors or matrix with x and y values. 
#If a vector x should be of length equal to nrow(z) 
#and y should be of length equal to ncol(z).
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, mean_n\{r_n^2\}$)'))
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

lat = seq(-90.25, 90, by = 0.5)
long = seq(-90.25, 90, by = 0.5)
error_data = matrix(NA, nrow=length(lat), ncol=length(long))
pts = data.frame(x=x, y=y)
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMeanLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
#
if (save2pdf) pdf('./Figs/SimpleRegMapLS01.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, mean_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
long_rad = atan2(modLS$coefficients[2],-1); lat_rad = atan2(modLS$coefficients[1],-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

#rimetto l'outlier
y[2] = y[2] + ofs_out; modLS = lm(y ~ x)
pts = data.frame(x=x, y=y)
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMeanLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
#
if (save2pdf) pdf('./Figs/SimpleRegMapLS02_out.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, mean_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
long_rad = atan2(modLS$coefficients[2],-1); lat_rad = atan2(modLS$coefficients[1],-1/cos(long_rad))
points(x=180-long_rad*180/pi, y=-lat_rad*180/pi, pch=1,col='white')
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
for (klat in 1:length(lat))
   for (klong in 1:length(long))
      error_data[klat,klong] = ErrorMapMedianLS(lat[klat],long[klong],pts)
logmap = log10(error_data)
min_ids = which(logmap==min(logmap), arr.ind=T)
lat_argmin = lat[min_ids[1]]
long_argmin = long[min_ids[2]]
#
if (save2pdf) pdf('./Figs/SimpleRegMapLMedS02_out.pdf', width=5,height=5)
par(mar=c(4,4,2,2)) #margini bottom, left, top, right
image2D(z=t(logmap), x=long, y=lat, contour=F, col=terrainPalette, zlim=c(-3,9),
        xlab=TeX(r'(longitude \varphi)'), ylab=TeX(r'(latitude \lambda)'), clab=TeX(r'($log_{10}\, median_n\{r_n^2\}$)'))
long_rad = atan2(b1,-1); lat_rad = atan2(b0,-1/cos(long_rad))
points(x=-180+long_rad*180/pi, y=-lat_rad*180/pi, pch='+',col='white')
points(x=long_argmin, y=lat_argmin, pch=1,col='white')
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


aLMedS = cos(lat_argmin*pi/180)*sin(long_argmin*pi/180)
bLMedS = cos(lat_argmin*pi/180)*cos(long_argmin*pi/180)
cLMedS = sin(lat_argmin*pi/180)
mLMedS = -aLMedS/bLMedS
qLMedS = -cLMedS/bLMedS
#
if (save2pdf) pdf('./Figs/SimpleRegLMedS02.pdf', width=5,height=5)
par(mar=c(4,4,0,0.5)) #margini bottom, left, top, right
plot(x,y,xlim=c(0,10),ylim=c(0,10),bty = 'n',
     pch=16, col=mediumBlue,
     xlab='x', ylab='y')
grid(lty='dotted')
points(x[2],y[2]-8,pch=1,col=mediumBlue)
abline(b0,b1, col='black', lty='dashed')
abline(qLMedS, mLMedS, col=mediumBlue, lwd=2)
text(1, 0.5, 'y = 0.4 + 0.5x', adj=0)#, col = "red", cex = 1.2
text(10, 6, 'y = 0.253 + 0.544x', adj=1, col=darkBlue)
text(x[2], y[2], '  punto di leva', adj=0, col='black')
#text(8, 4, 'y = 2.63 + 0.03x', adj=1)#, col = "red", cex = 1.2
if (save2pdf) dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


#modLMS = LMedSReg(y, X=data.frame(x=x), forceExhaustive=T)
