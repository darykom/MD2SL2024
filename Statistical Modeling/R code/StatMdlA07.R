#setup
library(latex2exp) #per label/titoli con LaTeX
library(pracma) # per funzioni numeriche varie (bisect, integrate)
library(leaps) # per backward/forward elimination
library(car) # per VIS
library(ash) # per Average Shifted Histogram
#library(ggformula) # per Average Shifted Histogram
library(olsrr)
library(MASS)


rm(list = ls())
setwd('C:/Users/dario/Documents/Projects/Statistical Modeling/Esame/R')

source('PlotFuncs.R') #file con funzioni riusabili per plot 

#InMichelin Food Decor Service Price
data = read.table('MichelinNY.txt',  header=T, sep="\t")
# nessun dato mancante tramite ispezione visiva

########## plot preliminare dei dati
#
savePlot = F # salva i plot direttamente in pdf, senza output a schermo
# se savePlot = F, massimizzare l'area del pannello Plots 
# (altrimenti per alcune figure si ha "figure margins too large")
#
########## InMichelin
#
nIn = length(which(data$InMichelin==1))
nNo = length(which(data$InMichelin==0))
Plot2Bar(vals=c(nIn, nNo), names=c('In Michelin', 'No Michelin'), 
         cols=c(colInMichMedium, colNoMichMedium), save2pdf = savePlot,
         colsTxt=c(colInMichDark, colNoMichDark), xlim=c(0,100), 
         tit='Dataset MichelinNY.txt', xlab='numero di ristoranti')
#
########## PRICE
#
# per gli istogrammi uso gli stessi bin sulla variabile nel suo complesso che
# poi separando i valori quando InMichelin=0 o 1, per agevolare il confronto
# visivo
plotdata = data$Price
breaks = ComputeBreaks(data$Price)
stopBin = breaks[length(breaks)]
#hlim = c(0,0.06) #impostata a posteriori se hist=pdf
hlim = c(0,40) #impostata a posteriori 
#
PlotHist(data=plotdata, name='Price', save2pdf=savePlot,
         breaks=breaks, dlim=c(0,stopBin), hlim=hlim,
         colFill=colGenericMedium, colBorder=colGenericDark)
hlim = c(0,30) #impostata a posteriori 
PlotHist(data=plotdata[which(data$InMichelin==1)],  save2pdf=savePlot,
         name='Price (In Michelin)',  breaks=breaks, hlim=hlim,
         dlim=c(0,stopBin), colFill=colInMichMedium, colBorder=colInMichDark)
PlotHist(data=plotdata[which(data$InMichelin==0)],  save2pdf=savePlot,
         name='Price (No Michelin)',  breaks=breaks, hlim=hlim,
         dlim=c(0,stopBin), colFill=colNoMichMedium, colBorder=colNoMichDark)
#
########## Food, Decor e Service
dlim=c(10,30);
breaks = seq(from=10.5,to=31,by=1)
#hlim = c(0,0.3) #impostata a posteriori se hist=pdf
hlim = c(0,40) #impostata a posteriori se hist= occorrenze
Plot3Scores(data[,c(2,3,4)], 
            c('Food','Decor','Service'), save2pdf=savePlot,
            breaks, dlim, hlim, colFill=colGenericMedium, colBorder=colGenericDark)
hlim = c(0,25) #impostata a posteriori se hist= occorrenze
Plot3Scores(data[which(data$InMichelin==1),c(2,3,4)], save2pdf=savePlot,
            c('Food (In Michelin)','Decor (In Michelin)','Service (In Michelin)'),
            breaks, dlim, hlim, colFill=colInMichMedium, colBorder=colInMichDark)
Plot3Scores(data[which(data$InMichelin==0),c(2,3,4)], save2pdf=savePlot,
            c('Food (No Michelin)','Decor (No Michelin)','Service (No Michelin)'),
            breaks, dlim, hlim, colFill=colNoMichMedium, colBorder=colNoMichDark)
#
############### Matrix ScatterPlot
if (savePlot)
  cairo_pdf('ScatterMatrixExA.pdf', width=7)
pairs(data[,c(5,2,3,4)], 
      lower.panel = panel.cor,
      upper.panel = upperGenerig.panel)
if (savePlot)
 dev.off()
#


####################
#      Preparazione delle variabli
####################

corrTable <- cor(data[,c(5,2,3,4)])
(round(corrTable, 2))


#rimozione outliers
outliers = which(data$Price>150)
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


############# plot dei dati centrati ################
if (savePlot)
  pdf('DeltaFoodDecoServiceVsPrice.pdf', width=7, height=3)
layout(matrix(c(1,2,3), nrow=1))
par(mar=c(4,4,1,1)) #margini bottom, left, top, right
plot(x=FoodTilde, y=dataInl$Price, col=colGenericDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Food})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=DecorTilde, y=dataInl$Price, col=colGenericDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='Price'); 
grid(lty='dotted')
#
plot(x=ServiceTilde, y=dataInl$Price, col=colGenericDark, cex=0.75,
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(0,120), 
     xlab=TeX(r'(\Delta{Service})'), ylab='Price'); 
grid(lty='dotted')
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
layout( matrix(c(1,1), nrow=1))
par(mfrow = c(1, 1))  # Reset to default layout

##################################################

############## Stima modello #####################
Michelin = dataInl$InMichelin
# Modello matriciale
y = dataInl$Price
n = length(y)
# design matrix X
X = cbind(rep(1,n), FoodTilde, DecorTilde, ServiceTilde, Michelin)

# Least Squares tramite Equazioni Normali
(betahat = solve(t(X)%*%X) %*% t(X) %*% y)

# Predizione
pred = X%*%betahat
# Residui
res = y-pred
# Residual sum of squares
RSS = sum(res^2)

# stima errori standard
(S2 = RSS / (n-ncol(X)))
V = S2 * solve(t(X)%*%X)
(se = sqrt(diag(V)))

(t = betahat / se)
# p-values
(pvalue = 2*(pt(abs(t), n-ncol(X), lower.tail = FALSE)))

#  R^2
(SST = sum((y - mean(y))^2) )
(SSreg = SST - RSS)
(R2 = SSreg/SST)

(R2adj = 1-RSS/SST*(n-1)/(n-1-4))

# verifica con lm()
modLm = lm(y ~ FoodTilde + DecorTilde + ServiceTilde + Michelin)
summary(modLm)

# Rimozione FoodTilde per p-value elevato
modLmRed = lm(y ~ DecorTilde + ServiceTilde + Michelin)
summary(modLmRed)



# Analisi dei residui
#


stdres = rstandard(modLmRed)
#binwidth (Freedman–Diaconis rule)
bw = 2*IQR(stdres)*(length(stdres))^(-1/3)
nb = ceil(max(abs(stdres))/bw)
maxbinrange = nb*bw+bw/2


PlotHistPlusNormal(data=stdres, 'StdResLinModReducedExA', 
                   dlim=c(-3.1,3.1), hlim = c(0,0.5),
                   breaks=seq(from=-maxbinrange, to=maxbinrange, by=bw), 
                   colBorder=colGenericDark, colFill=colGenericMedium, 
                   save2pdf=savePlot)
  
if (savePlot)
  pdf('ResVsFitLinModReducedExA.pdf', width=7)
par(mar=c(4,4.2,1.5,0.6)) #margini bottom, left, top, right
plot(modLmRed, which = 1, lwd=4, col=colGenericDark)
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins

if (savePlot)
  pdf('QQplotLinModReducedExA.pdf', width=7)
par(mar=c(4,4.2,1.5,0.6)) #margini bottom, left, top, right
plot(modLmRed, which = 2, col=colGenericDark)
if (savePlot)
  dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins


#par(mfrow = c(2, 2)); plot(modLm); par(mfrow = c(1, 1))


res = modLmRed$residuals
cols = rep(colNoMichDark,n) # Inizializzo i colori a rosso
cols[idInMic] = colInMichDark # verde per i ristoranti Michelin
if (savePlot)
  pdf('ResidualLinModVsXiExA.pdf', width=7)

layout(matrix(c(1,2,
                3,4,
                5,6), nrow=3,ncol=2, byrow = TRUE))
par(mar=c(4.2,4.2,2,0.6)) #margini bottom, left, top, right
plot(x=DecorTilde, y=res, col=colGenericDark, 
     pch = 16, bty='n', xlim=c(-10,10),ylim=c(-20,21), 
     xlab=TeX(r'(\Delta{Decor})'), ylab='residuals') 
grid(lty='dotted')
plot(x=ServiceTilde, y=res, col=colGenericDark, 
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
