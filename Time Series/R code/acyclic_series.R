rm(list=ls())
setwd("C:/Users/dario/Documents/Projects/Master/Time Series/R_Time_Series/")

to_pdf = F # TRUE per salvare i grafici in pdf

library(readxl)
library(tseries)  ## for unit root tests
library(forecast) ## for auto.arima and cross-validation


# da https://seriestoriche.istat.it/index.php?id=1&no_cache=1&tx_usercento_centofe%5Bcategoria%5D=13&tx_usercento_centofe%5Baction%5D=show&tx_usercento_centofe%5Bcontroller%5D=Categoria&cHash=e3503d8195dd4231ff53ba078ad5c124
# Produzione media delle principali coltivazioni agricole - Anni 1921-2015: 
# https://seriestoriche.istat.it/fileadmin/documenti/Tavola_13.16.xls
#dataset = read_excel("Tavola_13.16.xls")

# Leggo il file Excel e salto le righe inutili
raw_data_1 = read_excel("Tavola_13.16.xls", sheet = "Tavola 13.16", skip = 5)
raw_data_1 = raw_data_1[-c(1, 47:49),] #rimuovo celle non numeriche o vuote dal 1° foglio
raw_data_2 = read_excel("Tavola_13.16.xls", sheet = "Tavola 13.16 (segue)", skip = 5)
raw_data_2 = raw_data_2[-c(1, 48:51),] #rimuovo celle non numeriche o vuote dal 2° foglio
raw_data_3 = read_excel("Tavola_13.16.xls", sheet = "Tavola 13.16 (segue 2)", skip = 5)
raw_data_3 = raw_data_3[-c(1, 6:11),] #rimuovo celle non numeriche o vuote dal 3° foglio

raw_data = rbind(raw_data_1, raw_data_2, raw_data_3)

df <- data.frame(raw_data)
colnames(df) <- names(raw_data)

df[df == "...."] <- NA

# Pulizia dei dati
df = df[-c(1:25, 83:96),]
df[] = lapply(df, as.numeric) # Converto tutte le colonne in numeriche

# Visualizzo i dati puliti
head(df)
summary(df)

y0 = df$Patata
y <- ts(approx(x=1:length(y0), y=y0, xout=1:length(y0))$y)

# interpolo 1991 e 1992 (outlier)
id_out = which(df$ANNI %in% c(1991,1992))
id_line = which(df$ANNI %in% c(1990,1993))
# y-y0 = m(x-x0); y = m*x + y0-m*x0 = m*x+q
m = (y[id_line[2]] - y[id_line[1]])/(id_line[2]-id_line[1])
q = y[id_line[1]] - m*id_line[1] 
y[id_out] = m*id_out + q


wfig = 5.5
hfig = wfig*0.618
if (to_pdf)
{
   # per la serie senza dati rimossi e imputati
   #pdf("../LaTeX/Figure/potatoes_1921_2015.pdf",  width=7, height=4)
   
   # per la serie 1946-2002, con imputazione al 1991-1992
   pdf("../LaTeX/Figure/potatoes_1946_2002.pdf",  width=wfig, height=hfig)
}
par(mar = c(4, 4, 1.0, 0.1))
plot(df$ANNI, y, bty = 'n', type = 'l', 
     xlab = 'Year', ylab = 'Potatoes (quintals per hectare)', 
     main = 'Annual production')
points(df$ANNI, y, pch=16, cex=0.5)
par(mar=c(5,4,4,2)+0.1)
grid(lty = 1, col = 'gray90')
if (to_pdf) {dev.off()}


if (to_pdf) {pdf("../LaTeX/Figure/potatoes_diff1.pdf",  width=wfig, height=hfig)}
par(mar = c(4, 4, 1.0, 0.1))
plot(diff(y), bty = 'n', type='l', main='Differentiated time series')
points(diff(y), pch=16, cex=0.5)
grid(lty = 1, col = 'gray90')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

visual_check <- function(y, my, reslim, figname, main_str, asp) #1.413
{
   fy = fitted(my)
   ry = residuals(my)
   
   savepath = paste0("../LaTeX/Figure/", figname, ".pdf")
   if (to_pdf) {pdf(savepath,  width=wfig, height=asp*hfig)}
   par(mfrow=c(2,3), mar=c(4,4,3,1))
   
   plot(ry, main=main_str, ylim=reslim, bty='n');abline(h=0)
   
   Acf(ry, bty='n');
   Acf(ry^2, bty='n');
   #
   qqnorm(ry, ylim=reslim, bty='n'); qqline(ry)
   
   plot(c(fy), c(ry), xlab='fitted values', ylab='residuals', 
        main=main_str, ylim=reslim, bty='n'); abline(h=0)
   
   plot(c(fy), c(ry^2), xlab='fitted values', ylab='sq. residuals', 
        main=paste0(main_str,'^2'), ylim=c(0,max(abs(reslim))^2), bty='n')
   
   par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
   if (to_pdf) {dev.off()}
}


adf.test(y)   ## ADF: ipotesi nulla di non stazionarietà NON rifiutata
kpss.test(y)  ## KPSS: ipotesi nulla di stazionarietà rifiutata

# faccio i test anche sulla serie differenziata
dy = diff(y)
adf.test(dy)   ## ADF: ipotesi nulla di non stazionarietà rifiutata
kpss.test(dy)  ## KPSS: il test suggerisce che i dati sono probabilmente stationari

if (to_pdf) {pdf("../LaTeX/Figure/potatoes_diff1_acf_pacf.pdf",  width=wfig, height=2*hfig)}
par(mfrow=c(2,1), mar=c(4.0,4.5,3.5,1))
Acf(diff(y), bty='n') 
Pacf(diff(y), bty='n')
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

# ARIMA

# modello ARIMA(4,1,0) senza drift e con
mdl410_ = Arima(y, order=c(4,1,0), include.constant=F); mdl410_  # AICc=412.43
mdl410d = Arima(y, order=c(4,1,0), include.constant=T); mdl410d  # AICc=397.61

# modelli con random walk (senza e con drift)
mdl010_ = Arima(y, order=c(0,1,0), include.constant=F); mdl010_ # AICc=403.81
mdl010d = Arima(y, order=c(0,1,0), include.constant=T); mdl010d # AICc=397.31

# modelli con autoarima (senza e con drift)
mdl010__ = auto.arima(y, seasonal=F, ic="aicc", stepwise=F, allowdrift=F); mdl010__ # AICc=403.81
mdl012d  = auto.arima(y, seasonal=F, ic="aicc", stepwise=F, allowdrift=T); mdl012d  # AICc=395.33

# -> Vincono mdl010d e mdl012d: 
# mdl012d ha AICc più basso, ma mdl010d ha AICc comparabile ed è + semplice

# test di significatività
(mdl010d$coef/sqrt(diag(mdl010d$var.coef)))
(mdl012d$coef/sqrt(diag(mdl012d$var.coef)))


# verifica radici |L_i| > 1
(roots_012d   = polyroot(c(1,mdl012d$coef['ma1'],mdl012d$coef['ma2'])))


# Analisi dei residui
visual_check(y, mdl010d,   c(-20,20), 'potatoes_res_010d',   'arima(0,1,0) ry',    1.33) 
visual_check(y, mdl012d,   c(-20,20), 'potatoes_res_012d',   'arima(0,1,2) ry',    1.33) 

Box.test(residuals(mdl010d),   lag=11) 
Box.test(residuals(mdl010d)^2, lag=16) 

Box.test(residuals(mdl012d),   lag=11) 
Box.test(residuals(mdl012d)^2, lag=10) 


# ARMA con trend lineare
time = 1:length(y)
mdl100dLt = auto.arima(y, d=0, xreg=time, seasonal=F, ic="aicc", stepwise=F); mdl100dLt  ##  ARMA(1,0) con trend lineare
(mdl100dLt$coef/sqrt(diag(mdl100dLt$var.coef)))  ## il trend lineare è significativo

# conversione coeffs omega e delta
phi_1  = mdl100dLt$coef['ar1']
eta = mdl100dLt$coef['xreg']
mu = mdl100dLt$coef['intercept']
omega = mu + phi_1*eta
delta = eta*(1-phi_1)
print(c(omega, delta))

visual_check(y, mdl100dLt, c(-20,20), 'potatoes_res_100dLt', 'arma(1,0)+LT ry', 1.33)

Box.test(residuals(mdl100dLt),   lag=11) 
Box.test(residuals(mdl100dLt)^2, lag=13) 



# forecast
horiz = 10  ## horizon
#t0 = 25      ## starting time point

err010d   = tsCV(y, function(data,h){forecast(Arima(data,model=mdl010d), h=h)}, h=horiz)
err012d   = tsCV(y, function(data,h){forecast(Arima(data,model=mdl012d), h=h)}, h=horiz)
err100dLt = tsCV(y, 
                 function(data,h,xreg,newxreg){forecast(Arima(data,model=mdl100dLt,xreg=xreg),xreg=newxreg,h=h)}, 
                 xreg=time, h=horiz)

metrics = c("RMSE","MAE","MAPE")
accFun <- function(err,obs){accuracy(obs-err,obs)[1,metrics]}

tab010d   = do.call(rbind,lapply(err010d,   accFun, obs=y))
tab012d   = do.call(rbind,lapply(err012d,   accFun, obs=y))
tab100dLt = do.call(rbind,lapply(err100dLt, accFun, obs=y))


# computation of scaled error metrics
m0 <- Arima(y, order=c(0,1,0))  ## naive predictor
err0 <- tsCV(y, function(data,h){
   forecast(Arima(data,model=m0),h=h)
}, h=horiz)
tab0 <- do.call(rbind,lapply(err0, accFun, obs=y))
scaled_tab010d   = tab010d  [,1:2]/tab0[,1:2]
scaled_tab012d   = tab012d  [,1:2]/tab0[,1:2]
scaled_tab100dLt = tab100dLt[,1:2]/tab0[,1:2]



cols = c('dodgerblue', 'olivedrab', 'firebrick3')

if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_rmse.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 1 #RMSE
ylim=range(tab010d[,id_m], tab012d[,id_m], tab100dLt[,id_m])
plot(  tab010d[,id_m],   col=cols[1], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab010d[,id_m],   col=cols[1]); 
points(tab012d[,id_m],   col=cols[2]); lines(tab012d[,id_m],   col=cols[2])
points(tab100dLt[,id_m], col=cols[3]); lines(tab100dLt[,id_m], col=cols[3])
grid(lty = 1, col = 'gray90')
legend('topleft', lty=1, col=cols, cex=0.8, bty='n',
       legend=c('arima(0,1,0)','arima(0,1,2)', 'arma(1,0)+LT'))
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}


if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_mae.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 2 #MAE
ylim=range(tab010d[,id_m], tab012d[,id_m], tab100dLt[,id_m])
plot(  tab010d[,id_m],   col=cols[1], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab010d[,id_m],   col=cols[1]); 
points(tab012d[,id_m],   col=cols[2]); lines(tab012d[,id_m],   col=cols[2])
points(tab100dLt[,id_m], col=cols[3]); lines(tab100dLt[,id_m], col=cols[3])
grid(lty = 1, col = 'gray90')
legend('topleft', lty=1, col=cols, cex=0.8, bty='n',
       legend=c('arima(0,1,0)','arima(0,1,2)', 'arma(1,0)+LT'))
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}


if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_mape.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 3 #MAPE
ylim=range(tab010d[,id_m], tab012d[,id_m], tab100dLt[,id_m])
plot(  tab010d[,id_m],   col=cols[1], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab010d[,id_m],   col=cols[1]); 
points(tab012d[,id_m],   col=cols[2]); lines(tab012d[,id_m],   col=cols[2])
points(tab100dLt[,id_m], col=cols[3]); lines(tab100dLt[,id_m], col=cols[3])
grid(lty = 1, col = 'gray90')
legend('topleft', lty=1, col=cols, cex=0.8, bty='n',
       legend=c('arima(0,1,0)','arima(0,1,2)', 'arma(1,0)+LT'))
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}


if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_scaledrmse.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 1 #RMSE
ylim=range(scaled_tab010d[,id_m], scaled_tab012d[,id_m], scaled_tab100dLt[,id_m])
plot(  scaled_tab010d[,id_m],   col=cols[1], xlab="h", ylab=paste('Scal.', metrics[id_m]), ylim=ylim, bty='n', type='l')
points(scaled_tab010d[,id_m],   col=cols[1]); 
points(scaled_tab012d[,id_m],   col=cols[2]); lines(scaled_tab012d[,id_m],   col=cols[2])
points(scaled_tab100dLt[,id_m], col=cols[3]); lines(scaled_tab100dLt[,id_m], col=cols[4])
grid(lty = 1, col = 'gray90')
legend('topleft', lty=1, col=cols, cex=0.8, bty='n',
       legend=c('arima(0,1,0)','arima(0,1,2)', 'arma(1,0)+LT'))
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}



if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_scaledmae.pdf",  width=wfig, height=hfig)}
par(mar=c(4.0,4,0.5,0.5))
id_m = 2 #MAE
ylim=range(scaled_tab010d[,id_m], scaled_tab012d[,id_m], scaled_tab100dLt[,id_m])
plot(  scaled_tab010d[,id_m],   col=cols[1], xlab="h", ylab=paste('Scal.', metrics[id_m]), ylim=ylim, bty='n', type='l')
points(scaled_tab010d[,id_m],   col=cols[1]); 
points(scaled_tab012d[,id_m],   col=cols[2]); lines(scaled_tab012d[,id_m],   col=cols[2])
points(scaled_tab100dLt[,id_m], col=cols[3]); lines(scaled_tab100dLt[,id_m], col=cols[3])
grid(lty = 1, col = 'gray90')
legend('topright', lty=1, col=cols, cex=0.8, bty='n',
       legend=c('arima(0,1,0)','arima(0,1,2)', 'arma(1,0)+LT'))
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}



# display forecasts at up to 10 years (h=1,...,10)
pred010d   = forecast(mdl010d,   h=horiz, level=95)
pred012d   = forecast(mdl012d,   h=horiz, level=95)
pred100dLt = forecast(mdl100dLt, xreg=length(y)+1:10, level=95)

library(ggplot2)  ## includes the function auto.plot
if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_010d.pdf",  width=wfig, height=hfig)}
autoplot(y) +
   autolayer(pred010d,        series='ARIMA(0,1,0)', color=cols[1], showgap=F, alpha=0.5) +
   autolayer(fitted(mdl010d), series='ARIMA(0,1,0)', color=cols[1], lty=1) +
   xlab("Index [years]") + ylab("Potatoes (quintals per hectare)")  + theme_minimal() +
   ggtitle('arima(0,1,0)') + theme(legend.position='none')
if (to_pdf) {dev.off()}
# 
if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_012d.pdf",  width=wfig, height=hfig)}
autoplot(y) +
   autolayer(pred012d,        series='ARIMA(0,1,2)', showgap=F, alpha=0.5, color=cols[2]) +
   autolayer(fitted(mdl012d), series='ARIMA(0,1,2)', lty=1, color=cols[2]) +
   xlab("Index [years]") + ylab("Potatoes (quintals per hectare)") + theme_minimal() +
   ggtitle('arima(0,1,2)') + theme(legend.position='none')
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/potatoes_forecast_100dLt.pdf",  width=wfig, height=hfig)}
autoplot(y) +
   autolayer(pred100dLt,        series='ARMA(1,0)+LT', showgap=F, alpha=0.5, color=cols[3]) +
   autolayer(fitted(mdl100dLt), series='ARMA(1,0)+LT', lty=1, color=cols[3]) +
   xlab("Index [years]") + ylab("Potatoes (quintals per hectare)") + theme_minimal() +
   ggtitle('arma(1,0)+LT') + theme(legend.position='none')
if (to_pdf) {dev.off()}




