rm(list=ls())
setwd("C:/Users/dario/Documents/Projects/Master/Time Series/R_Time_Series/")

to_pdf = F # TRUE per salvare i grafici in pdf
wfig = 5.5
hfig = wfig*0.618

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

y0 = df$Patata
y <- ts(approx(x=1:length(y0), y=y0, xout=1:length(y0))$y)


visual_check <- function(y, my, reslim, figname, main_str, asp) #1.413
{
   fy = fitted(my)
   ry = residuals(my)
   
   #
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

if (to_pdf) {pdf("../LaTeX/Figure/potatoes_diff1_acf_pacf_noimp.pdf",  width=wfig, height=2*hfig)}
par(mfrow=c(2,1), mar=c(4.0,4.5,3.5,1))
Acf(diff(y), bty='n') 
Pacf(diff(y), bty='n')
par(mfrow=c(1,1), mar=c(5,4,4,2)+0.1)
if (to_pdf) {dev.off()}

# ARIMA

# modello ARIMA(4,1,0) senza drift e con
mdl410_ = Arima(y, order=c(4,1,0), include.constant=F); mdl410_  # AICc=425.25
mdl410d = Arima(y, order=c(4,1,0), include.constant=T); mdl410d  # AICc=411.89

# modelli con random walk (senza e con drift)
mdl010_ = Arima(y, order=c(0,1,0), include.constant=F); mdl010_ # AICc=418.05
mdl010d = Arima(y, order=c(0,1,0), include.constant=T); mdl010d # AICc=413.62

# modelli con autoarima (senza e con drift)
mdl010__ = auto.arima(y, seasonal=F, ic="aicc", stepwise=F, allowdrift=F); mdl010__ # AICc=418.05
mdl012d  = auto.arima(y, seasonal=F, ic="aicc", stepwise=F, allowdrift=T); mdl012d  # AICc=408.99
# test di significatività
(mdl012d$coef/sqrt(diag(mdl012d$var.coef))) #phi_2 non dissimile da 0 -> semplifico in ARIMA(0,1,1)

# -> Vincono mdl010d e mdl012d: 
# mdl012d ha AICc più basso, ma mdl010d ha AICc comparabile ed è + semplice

mdl011d = Arima(y, order=c(0,1,1), include.constant=T); mdl011d # AICc=409.47
(mdl011d$coef/sqrt(diag(mdl011d$var.coef)))


# Analisi dei residui
visual_check(y, mdl011d,   c(-16,34), 'potatoes_res_011d_noimp',   'arima(0,1,1) ry',    1.33)

Box.test(residuals(mdl011d),   lag=10)
Box.test(residuals(mdl011d),   lag=11)

Box.test(residuals(mdl011d)^2,   lag=10)
Box.test(residuals(mdl011d)^2,   lag=11)

