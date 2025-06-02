
#setup
library(latex2exp) #per label/titoli con LaTeX
library(pracma) # per funzioni numeriche varie (bisect, integrate)
library(leaps) # per backward/forward elimination
library(car) # per VIS


rm(list = ls())
setwd('C:/Users/dario/Documents/Projects/Statistical Modeling/Esame/R')

source('PlotFuncs.R') #file con funzioni riusabili per plot 

savePlot = F




#InMichelin Food Decor Service Price
data = read.table('MichelinNY.txt',  header=T, sep="\t")
# nessun dato mancante tramite ispezione visiva

idInMic = which(data$InMichelin==1)
idNoMic = which(data$InMichelin==0)
if (savePlot)
  cairo_pdf('ScatterMatrixInMichExA.pdf', width=7)
pairs(data[idInMic,c(5,2,3,4)], 
      lower.panel = panel.cor,
      upper.panel = upperInMich.panel)
if (savePlot)
  dev.off()
#
if (savePlot)
  cairo_pdf('ScatterMatrixNoMichExA.pdf', width=7)
pairs(data[idNoMic,c(5,2,3,4)], 
      lower.panel = panel.cor,
      upper.panel = upperNoMich.panel)
if (savePlot)
  dev.off()


outliers = c(9,14,142)
(data[outliers,])
dataInl = data[-outliers,]

# indici per ristoranti Michelin/NoMichelin
idInMic = which(dataInl$InMichelin==1)
idNoMic = which(dataInl$InMichelin==0)


#media per Food Decor Service
(FoodBar    = mean(dataInl$Food))
(DecorBar   = mean(dataInl$Decor))
(ServiceBar = mean(dataInl$Service))

# centratura per Food, Decor e Service
FoodTilde    = dataInl$Food-FoodBar
DecorTilde   = dataInl$Decor-DecorBar
ServiceTilde = dataInl$Service-ServiceBar

Michelin = dataInl$InMichelin

############# plot dei dati centrati ################
if (savePlot)
  pdf('DeltaFoodDecoServiceVsPriceInMich.pdf', width=7, height=3)
layout(matrix(c(1,2,3), nrow=1))
par(mar=c(4,4,1,1)) #margini bottom, left, top, right
plot(x=FoodTilde[idInMic], y=dataInl$Price[idInMic], col=colInMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Food})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=DecorTilde[idInMic], y=dataInl$Price[idInMic], col=colInMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=ServiceTilde[idInMic], y=dataInl$Price[idInMic], col=colInMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Service})'), ylab='Price'); 
grid(lty='dotted')
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
layout( matrix(c(1,1), nrow=1))
par(mfrow = c(1, 1))  # Reset to default layout

############# plot dei dati centrati ################
if (savePlot)
  pdf('DeltaFoodDecoServiceVsPriceNoMich.pdf', width=7, height=3)
layout(matrix(c(1,2,3), nrow=1))
par(mar=c(4,4,1,1)) #margini bottom, left, top, right
plot(x=FoodTilde[idNoMic], y=dataInl$Price[idNoMic], col=colNoMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Food})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=DecorTilde[idNoMic], y=dataInl$Price[idNoMic], col=colNoMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=ServiceTilde[idNoMic], y=dataInl$Price[idNoMic], col=colNoMichDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Service})'), ylab='Price'); 
grid(lty='dotted')
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
layout( matrix(c(1,1), nrow=1))
par(mfrow = c(1, 1))  # Reset to default layout



# Modello matriciale
y = dataInl$Price
n = length(y)

dmInMichSrv2 = Michelin*ServiceTilde*ServiceTilde

modLmQuadr = lm(y ~ DecorTilde + ServiceTilde + dmInMichSrv2) #AdjR^2:0.835
summary(modLmQuadr)


stdres = rstandard(modLmQuadr)
bw = 2*IQR(stdres)*(length(stdres))^(-1/3)
nb = ceil(max(abs(stdres))/bw)
maxbinrange = nb*bw+bw/2

PlotHistPlusNormal(data=stdres, 'StdResLinModQuadraticExA', 
                   dlim=c(-3.1,3.1), hlim = c(0,0.5),
                   breaks=seq(from=-maxbinrange, to=maxbinrange, by=bw), 
                   colBorder=colGenericDark, colFill=colGenericMedium, 
                   save2pdf=savePlot)


if (savePlot)
  pdf('ResVsFitLinModQuadraticExA.pdf', width=7)
par(mar=c(4,4.2,1.5,0.6)) #margini bottom, left, top, right
plot(modLmQuadr, which = 1, lwd=4, col=colGenericDark)
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

if (savePlot)
  pdf('QQplotLinModQuadraticExA.pdf', width=7)
par(mar=c(4,4.2,1.5,0.6)) #margini bottom, left, top, right
plot(modLmQuadr, which = 2, col=colGenericDark)
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

res = modLmQuadr$residuals
cols = rep(colNoMichMedium,n) # Inizializzo i colori a rosso
cols[idInMic] = colInMichMedium # verde per i ristoranti Michelin
if (savePlot)
  pdf('ResidualLinModQuadraticVsXiExA.pdf', width=7)

layout(matrix(c(1,2,
                3,4,
                5,6), nrow=3,ncol=2, byrow = TRUE))
par(mar=c(4.2,4.2,2,0.6)) #margini bottom, left, top, right
plot(x=DecorTilde, y=res, col=colGenericMedium, 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='residuals') 
grid(lty='dotted')
plot(x=ServiceTilde, y=res, col=colGenericMedium, 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Service})'), ylab='residuals')
grid(lty='dotted')
#
plot(x=DecorTilde[idNoMic], y=res[idNoMic], col=cols[idNoMic], 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='residuals', title('No Michelin')) 
grid(lty='dotted')
#
plot(x=ServiceTilde[idNoMic], y=res[idNoMic], col=cols[idNoMic], 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Service})'), ylab='residuals', title('No Michelin'))
grid(lty='dotted')
#
plot(x=DecorTilde[idInMic], y=res[idInMic], col=cols[idInMic], 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='residuals', title('In Michelin'))
grid(lty='dotted')
#
plot(x=ServiceTilde[idInMic], y=res[idInMic], col=cols[idInMic], 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Service})'), ylab='residuals', title('In Michelin'))
grid(lty='dotted')
if (savePlot)
  dev.off()

par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
layout( matrix(c(1,1), nrow=1))
par(mfrow = c(1, 1))  # Reset to default layout

