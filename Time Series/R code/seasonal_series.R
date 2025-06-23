rm(list=ls())
setwd("C:/Users/dario/Documents/Projects/Master/Time Series/R_Time_Series/")

library(tseries)  ## for unit root tests
library(forecast) ## for auto.arima and cross-validation
library(TSA)      ## includes the function BoxCox.ar and some datasets
library(readxl)

to_pdf = F # TRUE per salvare i grafici in pdf

visual_check <- function(y, my, reslim, figname, main_str, asp) #1.413
{
   fy = fitted(my)
   ry = residuals(my)

   savepath = paste0("../LaTeX/Figure/", figname, ".pdf")
   if (to_pdf) pdf(savepath,  width=wfig, height=asp*hfig)
   par(mfrow=c(2,3), mar=c(4,4,3,1))
   
   plot(ry, main=main_str, ylim=reslim, bty='n');abline(h=0)
   
   Acf(ry, bty='n', lag.max=maxlag); 
   Acf(ry^2, bty='n', lag.max=maxlag); 
   #
   qqnorm(ry, ylim=reslim, bty='n', cex=0.5); qqline(ry)
   
   plot(c(fy), c(ry), xlab='fitted values', ylab='residuals', cex=0.5,
        ylim=reslim, bty='n'); abline(h=0) 
   
   plot(c(fy), c(ry^2), xlab='fitted values', ylab='sq. residuals', cex=0.5,
        ylim=c(0,max(abs(reslim))^2), bty='n') 
   
   par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
   if (to_pdf) dev.off();
}

show_Box_pvals <- function(fname, y, mdl, mdlLabel, LBoxtest)
{
   if (to_pdf) pdf(paste0("../LaTeX/Figure/", fname, ".pdf"),  width=wfig, height=2*hfig)
   
   res = residuals(mdl)
   
   par(mfrow=c(2,1), mar=c(4,4,3,0.1))
   
   p = vector('numeric', LBoxtest)
   p2 = vector('numeric', LBoxtest)
   for (l in 1:LBoxtest)
   {
      boxt = Box.test(res,   lag=l)
      boxt2 = Box.test(res^2, lag=l)
      p[[l]] = boxt$p.value
      p2[[l]] = boxt2$p.value
   }
   plot(1:LBoxtest,p, bty='n',  ylab='p-value', xlab='lag', cex=0.5,
        main= paste('Box test on residuals ', mdlLabel)); grid()
   abline(h = 0.05, col='red', lty = 'solid')
   
   plot(1:LBoxtest,p2, bty='n',  ylab='p-value', xlab='lag', cex=0.5,
        main=paste('Box test on sq. residuals ', mdlLabel)); grid()
   abline(h = 0.05, col='red', lty = 'solid')
   
   par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
   if (to_pdf) dev.off();
}


# https://www.kaggle.com/datasets/sulphatet/daily-weather-data-40-years
raw_data = read_excel("ICRISAT Weather 1978 to 2018.xlsx")

raw_data$Date = as.Date(raw_data$Date, format = '%d/%m/%Y')

raw_data$Mese = format(raw_data$Date, '%Y-%m')
monthly_avg = tapply(raw_data$MaxT, raw_data$Mese, mean, na.rm = TRUE)


df = data.frame(MaxT_avg = as.vector(monthly_avg))

# Converto la colonna "month" in formato data per l'asse x
month = names(monthly_avg)
df$Date = as.Date(paste0(month, '-01'), format ='%Y-%m-%d') # Aggiungo il giorno 01 per creare una data valida
years = unique(format(df$Date, '%Y')) # Estraggo gli anni unici


wfig = 5.5
hfig = wfig*0.618
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_daily.pdf",  width=wfig, height=0.555*hfig)}
par(mar = c(1.8, 4, 1.0, 0.75))
plot(raw_data$Date, raw_data$MaxT, 
     bty = 'n',
     type = 'l', 
     xlab = 'year', 
     ylab = 'MaxT', 
     main = 'daily observation')
abline(v = as.Date(paste0(years,'-01-01')), col='lightgray', lty='dotted')
abline(h = c(30,35,40), col='lightgray', lty = 'dotted')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_monthly.pdf",  width=wfig, height=0.555*hfig)}
par(mar = c(1.8, 4, 1.0, 0.75))
plot(df$Date, df$MaxT_avg,
     bty = 'n',
     type = 'l',
     xlab = 'year',
     ylab = 'MaxT',
     main = 'monthly average')
points(df$Date, df$MaxT_avg, pch=16, cex=0.5)
# griglia anno per anno
abline(v = as.Date(paste0(years,'-01-01')), col='lightgray', lty='dotted')
abline(h = c(30,35,40), col='lightgray', lty = 'dotted')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

y <- ts(df[,'MaxT_avg'], frequency=12, start='1978')

maxlag = 120

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_y_acf060.pdf",  width=wfig, height=hfig)}
par(mar = c(4, 4, 1.0, 0.1))
acf(y, lag.max=60, bty='n') 
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_y_acf120.pdf",  width=wfig, height=hfig)}
par(mar = c(4, 4, 1.0, 0.1))
acf(y, lag.max=maxlag, bty='n') 
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

adf.test(y)        
kpss.test(y)       

diff_seasonal_y = diff(y, lag = 12)
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonaly.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.75))
plot(diff(y, lag=12), type='l', bty='n')
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

adf.test(diff_seasonal_y)       
kpss.test(diff_seasonal_y)      

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonaly_acf060.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
acf(diff_seasonal_y, lag.max=60, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonaly_acf120.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
acf(diff_seasonal_y, lag.max=maxlag, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonaly_pacf060.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
pacf(diff_seasonal_y, lag.max=60, bty='n');  grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonaly_pacf120.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
pacf(diff_seasonal_y, lag.max=maxlag, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}


# # mdl102s211_ = Arima(y, order=c(1,0,2), seasonal=c(2,1,1), include.constant=F); mdl102s211_ # AICc= 1455.49 #ARIMA(1,0,2)(2,1,1)[12]
mdl102s211d = Arima(y, order=c(1,0,2), seasonal=c(2,1,1), include.constant=T); mdl102s211d # AICc= 1452.27 #ARIMA(1,0,2)(2,1,1)[12]+drift
# # mdl202s211_ = Arima(y, order=c(2,0,2), seasonal=c(2,1,1), include.constant=F); mdl202s211_ # AICc= 1457.51 #ARIMA(2,0,2)(2,1,1)[12]
# # mdl202s211d = Arima(y, order=c(2,0,2), seasonal=c(2,1,1), include.constant=T); mdl202s211d # AICc= 1454.25 #ARIMA(2,0,2)(2,1,1)[12]+drift
# # mdl102s511_ = Arima(y, order=c(1,0,2), seasonal=c(5,1,1), include.constant=F); mdl102s511_ # AICc= 1455.46 #ARIMA(1,0,2)(5,1,1)[12]
# # mdl102s511d = Arima(y, order=c(1,0,2), seasonal=c(5,1,1), include.constant=T); mdl102s511d # AICc= 1452.79 #ARIMA(1,0,2)(5,1,1)[12]+drift
# # mdl202s511_ = Arima(y, order=c(2,0,2), seasonal=c(5,1,1), include.constant=F); # AICc= KO #ARIMA(2,0,2)(5,1,1)[12]
# # mdl202s511d = Arima(y, order=c(2,0,2), seasonal=c(5,1,1), include.constant=T); # AICc= KO #ARIMA(2,0,2)(5,1,1)[12]+drift
# # mdl102s811_ = Arima(y, order=c(1,0,2), seasonal=c(8,1,1), include.constant=F); # AICc= KO #ARIMA(1,0,2)(8,1,1)[12]
# # mdl102s811d = Arima(y, order=c(1,0,2), seasonal=c(8,1,1), include.constant=T); # AICc= KO #ARIMA(1,0,2)(8,1,1)[12]+drift
# # mdl202s811_ = Arima(y, order=c(2,0,2), seasonal=c(8,1,1), include.constant=F); # AICc= KO #ARIMA(2,0,2)(8,1,1)[12]
# # mdl202s811d = Arima(y, order=c(2,0,2), seasonal=c(8,1,1), include.constant=T); # AICc= KO #ARIMA(2,0,2)(8,1,1)[12]+drift
visual_check(y, mdl102s211d, c(-6,3), 'tmax7818_r102s211d', 'arima(1,0,2)(2,1,1)[12]+dr.', 1.33)

mdl001s212d = auto.arima(y, d=0, D=1, ic="aicc", stepwise=F, allowdrift = T, max.P=8); mdl001s212d
mdl001s212d$coef/sqrt(diag(mdl001s212d$var.coef))  ## z statistics
visual_check(y, mdl001s212d, c(-6,3), 'tmax7818_r001s212d', 'arima(0,0,1)(2,1,1)[12]+dr. + dr.', 1.33)


LBoxtest = 120
show_Box_pvals('tmax7818_001s212d_Boxtest', y, mdl001s212d, 'arima(0,0,1)(2,1,1)[12]+dr.', LBoxtest)


########################## SERIE TRASFORMATA ###################################

yo = y # memorizzo la serie originale, non si sa mai...

#imputo
id_may89 = (1989-1978)*12+5
id_may90 = (1990-1978)*12+5
id_may91 = (1991-1978)*12+5
y[id_may90] = (y[id_may89]+y[id_may91])/2

# calcolo trasformazione
lambdaVal = BoxCox.ar(y)$mle; lambdaVal

ybc = BoxCox(y, lambdaVal)
diff_seasonal_ybc = diff(ybc, lag = 12)

# plot vari ACF/PACF sulla serie trasformata
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonalyBC.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.75))
plot(diff(ybc, lag=12), type='l', bty='n')
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonalyBC_acf060.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
acf(diff_seasonal_ybc, lag.max=60, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonalyBC_acf120.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
acf(diff_seasonal_ybc, lag.max=maxlag, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonalyBC_pacf060.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
pacf(diff_seasonal_ybc, lag.max=60, bty='n');  grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_diffseasonalyBC_pacf120.pdf",  width=wfig, height=hfig)}
#
par(mar = c(4, 4, 1.0, 0.1))
pacf(diff_seasonal_ybc, lag.max=maxlag, bty='n'); grid()
par(mar=c(5,4,4,2)+0.1)
#
if (to_pdf) {dev.off()}

#mdlBC202s511_  = Arima(y, order=c( 2,0,2), seasonal=c(5,1,1), include.constant=F, lambda=lambdaVal); mdlBC202s511_  #AICc= -4852.37
mdlBC202s511d  = Arima(y, order=c( 2,0,2), seasonal=c(5,1,1), include.constant=T, lambda=lambdaVal); mdlBC202s511d  #AICc= -4853.99 
mdlBC202s511d$coef/sqrt(diag(mdlBC202s511d$var.coef))  ## z statistics
visual_check(y, mdlBC202s511d, c(-0.005,0.005), 'tmax7818_r202s511d_BC', 'arima(2,0,2)(5,1,1)[12]+dr.', 1.33)


mdlBC001s311_ = auto.arima(y, d=0, D=1, ic="aicc", stepwise=F, allowdrift=T, max.P=13, lambda=lambdaVal); mdlBC001s311_
mdlBC001s311_$coef/sqrt(diag(mdlBC001s311_$var.coef))  ## z statistics
visual_check(y, mdlBC001s311_, c(-0.005,0.005), 'tmax7818_r001s311_BC', 'arima(0,0,1)(3,1,1)[12]', 1.33)

show_Box_pvals('tmax7818_BC202s511d_Boxtest', y, mdlBC202s511d, 'arima(2,0,2)(5,1,1)[12]+dr.', LBoxtest)
show_Box_pvals('tmax7818_BC001s311__Boxtest', y, mdlBC001s311_, 'arima(0,0,1)(3,1,1)[12]', LBoxtest)

#######################################à
metrics = c("RMSE","MAE","MAPE")

# cross-validation forecast error at up to one year (h=1,...,12)
#   data are % index numbers: interpret RMSE and MAE in %
horiz <- 12  ## horizon
# t0 <- 12     ## starting time point
err1 <- tsCV(y, function(data,h){
   forecast(Arima(data,model=mdlBC202s511d),h=h)}, h=horiz)#, initial=t0)
accFun <- function(err,obs){accuracy(obs-err,obs)[1,metrics]}
tab1 <- do.call(rbind,lapply(err1, accFun, obs=y))
 
 
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_forecast_rmse.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 1 #RMSE
ylim=range(tab1[,id_m])
plot(  tab1[,id_m], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab1[,id_m]);
grid(lty = 1, col = 'gray90')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}
 
 
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_forecast_mae.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 2 #MAE
ylim=range(tab1[,id_m])
plot(  tab1[,id_m], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab1[,id_m]);
grid(lty = 1, col = 'gray90')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_forecast_mape.pdf",  width=wfig, height=hfig)}
par(mar=c(4,4,0.5,0.5))
id_m = 3 #MAPE
ylim=range(tab1[,id_m])
plot(  tab1[,id_m], xlab="h", ylab=metrics[id_m], ylim=ylim, bty='n', type='l')
points(tab1[,id_m]);
grid(lty = 1, col = 'gray90')
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

# display forecasts at up to one year (h=1,...,12)
if (to_pdf) {pdf("../LaTeX/Figure/tmax7818_forecast_BC202s511d.pdf",  width=wfig, height=hfig)}
par(mar=c(2,2,1,0.3))
pred1 = forecast(mdlBC202s511d, h=12)
plot(pred1, showgap=F, bty='n')        ## forecasts
grid(lty = 1, col = 'gray90')
lines(fitted(mdlBC202s511d), lty=2, col='dodgerblue')  ## fitted values
par(mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

