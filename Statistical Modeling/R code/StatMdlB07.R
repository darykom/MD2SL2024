#setup
library(latex2exp) #per label/titoli con LaTeX
library(pracma) # per funzioni numeriche varie (bisect, integrate)
library(leaps) # per backward/forward elimination
library(car) # per VIS
library(MASS) # per forward/backward
library(mclust)
library(readxl)


rm(list = ls())
setwd("C:/Users/dario/Documents/Projects/Statistical Modeling/Esame/R")

source('Palette.R') #file con palette colori
source('PlotFuncs.R') #file con funzioni riusabili per plot 

savePlot = F # salva i plot direttamente in pdf, senza output a schermo
# se savePlot = F, massimizzare l'area del pannello Plots 
# (altrimenti per alcune figure si ha errore)


################# recupero informazioni a latere dal file xls #################

## carica il file xls
datax = read_excel("efige.xls")
# ordinamento e categorie univoche
uniqueSectors = sort(unique(datax$sector2))
# estrazione del settore e della prima etichetta
ExtractSectorLabel <- function(sector) 
{
  sector_rows = datax[datax$sector2 == sector,]
  return(c(sector, sector_rows$sector[1]))
}
# applico la funzione a ciascun settore univoco
sector_label_pairs = sapply(uniqueSectors, ExtractSectorLabel)
# Traspongo il risultato (per print a video)
sector_label_pairs = t(sector_label_pairs)
print(sector_label_pairs)

regx = unique(datax$region)

###############################################################################


data = read.table("efige.txt",  header=T, sep="\t")

# selezione dei campi indicati nella legenda efige.xls
efigevar = c(
  'sector',
  'region',
  'north',
  'centre',
  'south_isl',
#  'rel_weight',
  'age',
  'group',
  'individual_first_shr',
  'foreign_first_shr',
  'decentr_manag',
  'employees',
  'RD_empl_share',
  'grad_empl_share',
  'labour_flex',
  #'age_ceo',
  'female_ceo',
  'fam_ceo',
  'prod_inn',
  'proc_inn',
  'patent',
  'RD_inv',
  'direct_export',
  'dir_export_share',
  'dir_export_eu',
  'dir_export_outside_eu',
  'import_goods',
  'import_share_goods',
  'import_goods_china_india',
  'deloc_fdi',
  'deloc_fdi_share',
  'deloc_fdi_china_india',
  'qual_cert',
  'competitors_from_abroad',
  'widened_prod_range',
  'increase_margins',
  'external_financing',
  'local_bank',
  'national_bank',
  'foreign_bank',
  'banks_number',
  'credit_requested',
  'credit_denied',
  'totalassets',
  'sales',
  'addedvalue',
  'ebit',
  'roa',
  'roi',
  'ros',
  'roe',
  'leverage'
#  'tfp_va'
)

# variabili quantitative
qvarnames = c(#'rel_weight', 
              'age', 
              'employees', 
              #'age_ceo',
              'RD_empl_share', 
              'grad_empl_share',
              'dir_export_share', 
              'import_share_goods', 
              'deloc_fdi_share', 
              'banks_number', 
              'totalassets', 
              'sales', 
              'addedvalue', 
              'ebit', 
              'roa', 
              'roi', 
              'ros', 
              'roe', 
              'leverage'
#              'tfp_va'
)

# risposta d'interesse: deloc_fdi

#dataset iniziale df0
df0 = data[ , (names(data) %in% efigevar)]
(table(df0$deloc_fdi))



###############################################################################
#                     Analisi variabili categoriche
###############################################################################

delocON0 = df0[df0$deloc_fdi == 1,]
dim(delocON0)
# Tabella di contingenza per settori e regioni t.c. deloc_fdi = 1
ctable = table(delocON0$sector, delocON0$region)
print(ctable)
print(rowSums(ctable))
print(colSums(ctable))

sects = sort(unique(delocON0$sector))
CountSectorOccurrences <- function(sect) 
{
   sectorRows = which(delocON0$sector == sect)
   return(c(sect, length(sectorRows)))
}
sectorCounts = sapply(sects, CountSectorOccurrences)
print(t(sectorCounts)) #trasposta per print

regs = sort(unique(delocON0$region))
CountRegionOccurrences <- function(reg) 
{
   regionRows = which(delocON0$region == reg)
   return(c(reg, length(regionRows)))
}
regionCounts = sapply(regs, CountRegionOccurrences)
regionCounts = t(regionCounts)
regSort = sort(regionCounts[,2], index.return=T, decreasing=F)


###############################################################################
#                                 PLOTS
###############################################################################

if (savePlot)
   cairo_pdf('RegionExB.pdf', width=7)
par(mar=c(4,8,2,1)) #margini bottom, left, top, right
(idreg = regionCounts[regSort$ix,1])
(regnames = paste(regx[idreg], '  cod.', idreg))
cSouthBar  = mediumYellow; cSouthTxt=darkYellow
cNorthBar  = mediumRed;    cNorthTxt=darkRed
cCentreBar = mediumOrange; cCentreTxt=darkOrange
colsRegBar = c(cSouthBar, rep(cCentreBar,1,3), rep(cNorthBar,1,5))
colsRegTxt = c(cSouthTxt, rep(cCentreTxt,1,3), rep(cNorthTxt,1,5))
barplot(height=regSort$x, names=regnames, horiz=T, col=colsRegBar,
        xlab='occorrenze per deloc_fdi=1', main = 'region', las=1,xlim=c(0,13.5))
grid(lty="solid", lwd=0.75, col="white", ny=NA, nx=NULL)
text(x = regSort$x+0.3, y = seq(0.75,18,1.2), labels = regSort$x, col = colsRegTxt)
legend('bottomright', bty = 'n', 
       legend = c('nord', 'centro', 'sud+isole'), 
       fill = c(mediumRed, mediumOrange, mediumYellow), inset = 0.03)
#
if (savePlot)
   dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
###############################################################################


###############################################################################
#                   ANALISI VARIABILI DISCRETE (dataset df1)
###############################################################################
df1 = df0
df1 = df1[,names(df1)!='region']
df1 = df1[,names(df1)!='north']
df1 = df1[,names(df1)!='centre']

#settori dove almeno un'azienda ha investito all'estero
sectsDeloc = c(28, 27, 14, 13, 25, 32, 30, 26, 23, 22, 20, 31, 21, 18, 10)
sects = unique(df1$sector)
ids0Deloc = which(!(sects %in% sectsDeloc))
sects0 = sects[ids0Deloc] # settori a 0 investimenti

for (s in sects0)
{
   ids = which(df1$sector == s)
   df1$sector[ids] = 0
}
df1$sector = as.factor(df1$sector)

######################### analisi variabili binarie ###########################

#lista varibili discrete (binarie) esplicative
bvarnames = (names(df1))[!(names(df1) %in% qvarnames)]
bvarnames = bvarnames[bvarnames!='deloc_fdi']
bvarnames = bvarnames[bvarnames!='sector']

# test indip. con y=deloc_fdi
fTestOr = c()
fTestPval = c()
c2TestPval = c()
for (idk in bvarnames)
{
   x_k = df1[,idk]
   chi2 = chisq.test(df1$deloc_fdi, x_k)
   if (any(chi2$expected < 5))
   {
     print(paste(idk, ' chi2$expected < 5'))
   }
   c2TestPval[idk] = chi2$p.value
   
   ctable = table(df1$deloc_fdi, x_k)
   # Test di Fisher per l'odds ratio
   fTest = fisher.test(ctable, conf.level=0.95)
   fTestOr[idk] =  fTest$estimate
   fTestPval[idk] = fTest$p.value
}
indpy = sort(fTestOr, index.return=T, decreasing=T)
ix = indpy$ix
smrIndpy = cbind('chi2 pval'=c2TestPval[ix], 'OR'=fTestOr[ix], 'OR pval'=fTestPval[ix])
print(smrIndpy)
print(round(smrIndpy,3))

delendaPriorBin = c('deloc_fdi_china_india', 'national_bank')
delendaIndp = c('fam_ceo', 'external_financing', 'proc_inn', 'decentr_manag', 
                'credit_requested', 'foreign_first_shr', 'increase_margins', 
                'local_bank', 'female_ceo', 'credit_denied', 'south_isl')
delendaRedun = c('dir_export_outside_eu', 'dir_export_eu', 
                 'import_goods_china_india',
                 'widened_prod_range')
bvarnames = bvarnames[!(bvarnames %in% delendaPriorBin)]
bvarnames = bvarnames[!(bvarnames %in% delendaIndp )]
bvarnames = bvarnames[!(bvarnames %in% delendaRedun)]

nbin = length(bvarnames)
mor =matrix(ncol=nbin, nrow=nbin)
morbin = matrix(0,ncol=nbin, nrow=nbin)
for (r in 1:(nbin-1))
{
   rowName = bvarnames[r]
   for (c in (r+1):nbin)
   {
      colName = bvarnames[c]; 
      ctable = table(df1[,rowName], df1[,colName])
      fTest = fisher.test(ctable, conf.level=0.95)
      
      mor[r,c] = fTest$estimate
      
      if (fTest$p.value>=0.05)
         mor[c,r] = 1
      else # variabili DIPENDENTI se pvalue<0.5
         mor[c,r] = 0
   }
}
mor = round(mor, 2)
print(cbind(bvarnames, mor))

###############################################################################
#            ANALISI VARIABILI QUANTITATIVE (dataset df2)
###############################################################################
df2 = df1
df2 = df2[,!(names(df2) %in% delendaPriorBin)]
df2 = df2[,!(names(df2) %in% delendaIndp) ]
df2 = df2[,!(names(df2) %in% delendaRedun)]

#################### banks_number
breaks = -0.5:31
stopBin = breaks[length(breaks)]
hlim = c(0,252) 
PlotHist(data=df2$banks_number[which(df2$deloc_fdi==0)],
         name='banks_number (deloc_fdi=0)', save2pdf=savePlot,
         breaks=breaks, dlim=c(0,31), hlim=hlim,
         colFill=lightBrown, colBorder=darkBrown)
hlim = c(0,6)
PlotHist(data=df2$banks_number[which(df2$deloc_fdi==1)], 
         name='banks_number (deloc_fdi=1)', save2pdf=savePlot,
         breaks=breaks, dlim=c(0,31), hlim=hlim,
         colFill=lightBrown, colBorder=darkBrown)
# Nessuna differenza visibile in termini di caratteristiche: elimino banks_number


########################## variabili continue 

cvarnames = qvarnames[qvarnames!='banks_number']
cvarnames = cvarnames[cvarnames!='deloc_fdi_share']

###### rimozione outliers
inl = rep(T,1,nrow(df2))
for (n in 1:length(cvarnames))
{
   xn = cvarnames[n]
   dn = na.omit(df2[,xn])
   linf = quantile(dn, probs = 0.005);
   lsup = quantile(dn, probs = 0.995);
   inls = linf <= df2[,xn]
   inli = df2[,xn]<= lsup
   inln = inls & inli
   inl = inl & inln
}
idout = which(!inl)
df2 = df2[-idout,]

idout = which(df2$sales > 150000)
df2 = df2[-idout,]
#
idout = which(df2$ebit > 10000)
df2 = df2[-idout,]
#
idout = which(df2$totalassets > 250000)
df2 = df2[-idout,]

###############################################################################
#                                  PLOTS
###############################################################################

##################### Plot istogrammi variabili continue 
df2c = df2[,names(df2) %in% cvarnames]
breaks = ComputeBreaks(log10(df2c$totalassets))
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=log10(df2c$totalassets), name='log10(totalassets)', save2pdf=savePlot,
         breaks=breaks, dlim=c(2,6), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(log10(abs(df2c$leverage)))
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=log10(abs(df2c$leverage)), name='log10(abs(leverage))', save2pdf=savePlot,
         breaks=breaks, dlim=c(-0.5,2.5), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(log10(1+abs(na.omit(df2c$ebit))))
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=log10(1+abs(df2c$ebit)), name='log10(abs(ebit))', save2pdf=savePlot,
         breaks=breaks, dlim=c(0,stopBin), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(log10(df2c$sales))
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=log10(df2c$sales), name='log10(sales)', save2pdf=savePlot,
         breaks=breaks, dlim=c(2,stopBin), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(df2c$roa)
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=df2c$roa, name='roa', save2pdf=savePlot,
         breaks=breaks, dlim=c(-0.3,0.4), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(df2c$roe)
stopBin = breaks[length(breaks)]
hlim = c(0,152) 
PlotHist(data=df2c$roe, name='roe', save2pdf=savePlot,
         breaks=breaks, dlim=c(-stopBin,stopBin), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(na.omit(df2c$ros))
stopBin = breaks[length(breaks)]
hlim = c(0,127) 
PlotHist(data=df2c$ros, name='ros', save2pdf=savePlot,
         breaks=breaks, dlim=c(-0.4,0.4), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)
#
breaks = ComputeBreaks(na.omit(df2c$roi))
stopBin = breaks[length(breaks)]
hlim = c(0,127) 
PlotHist(data=df2c$roi, name='roi', save2pdf=savePlot,
         breaks=breaks, dlim=c(-0.25,stopBin), hlim=hlim,
         colFill=lightPink, colBorder=darkPink)

########################## Plot scatter plots 
id0 = which(df2$deloc_fdi==0)
id1 = which(df2$deloc_fdi==1)
col0 = mediumBlue
col1 = darkOrange
#
if (savePlot)
   cairo_pdf('ScatterMatrixTotalassetsSalesAddedvalueEbit.pdf', width=7)
pairs(df2c[,c('employees', 'totalassets', 'sales','addedvalue','ebit')], 
      lower.panel = panel.cor,
      upper.panel = upperNoJitterDelocfdi.panel)
if (savePlot)
   dev.off()
#
if (savePlot)
   cairo_pdf('ScatterMatrixEbitRox.pdf', width=7)
pairs(df2c[,c('ebit', 'roa', 'roi','ros','roe')], 
      lower.panel = panel.cor,
      upper.panel = upperNoJitterDelocfdi.panel)
if (savePlot)
   dev.off()
#
if (savePlot)
   cairo_pdf('ScatterMatrixEmployeesRdshareGradshare.pdf', width=7)
par(mar=c(0,0,1,1)) #margini bottom, left, top, right
pairs(df2c[,c('employees', 'grad_empl_share','RD_empl_share')], 
      lower.panel = panel.cor,
      upper.panel = upperJitterDelocfdi.panel)
legend('bottom', bty = 'n', 
       legend = c('deloc_fdi=0','deloc_fdi=1'), col=c(col0,col1),
       lty = NA, pch='o',lwd=2, inset = 0.03)
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
if (savePlot)
   dev.off()
# perturbo leggermente i dati in cui deloc_fdi=1 per visualizzarli meglio
df2c$RD_empl_share[id1] = jitter(x=df2c$RD_empl_share[id1],amount=0)
df2c$grad_empl_share[id1] = jitter(x=df2c$grad_empl_share[id1],amount=0)
if (savePlot)
   cairo_pdf('ScatterMatrixExSalesShareB.pdf', width=7)
par(mar=c(4,4,0,0)) #margini bottom, left, top, right
plot(df2c[id0,c('dir_export_share','import_share_goods')], col=col0,  bty='n',
     ylim=c(0,100), xlim=c(0,100))
points(x=df2c$grad_empl_share[id1], y=df2c$RD_empl_share[id1], col=col1)
grid(lty='dotted')
legend('topright', bty = 'n', 
       legend = c('deloc_fdi=0','deloc_fdi=1'), col=c(col0,col1),
       lty = NA, pch='o',lwd=2, inset = 0.03)
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
if (savePlot)
   dev.off()
#
if (savePlot)
   cairo_pdf('DelocFdiShare_RdEmpShare.pdf', width=7)
par(mar=c(4,4,0,0)) #margini bottom, left, top, right
cols = rep(mediumBlue,1,nrow(df2c))
cols[id1] = darkOrange
plot(x=df2$RD_empl_share, y=df2$deloc_fdi_share, col=cols, bty='n',
     xlim=c(0,100), ylim=c(0,1), pch=16,
     xlab = 'RD_empl_share', ylab='deloc_fdi_share') 
grid(lty='dotted')
legend('topright', bty = 'n', 
       legend = c('deloc_fdi=0','deloc_fdi=1'), col=c(col0,col1),
       lty = NA, pch=16,lwd=2, inset = 0.03)
if (savePlot)
   dev.off()
#
if (savePlot)
   cairo_pdf('DelocFdiShare_GradEmpShare.pdf', width=7)
par(mar=c(4,4,0,0)) #margini bottom, left, top, right
plot(x=df2$grad_empl_share, y=df2$deloc_fdi_share, col=cols, bty='n',
     xlim=c(0,100), ylim=c(0,1), pch=16,
     xlab = 'grad_empl_share', ylab='deloc_fdi_share')
grid(lty='dotted')
legend('topright', bty = 'n', 
       legend = c('deloc_fdi=0','deloc_fdi=1'), col=c(col0,col1),
       lty = NA, pch=16,lwd=2, inset = 0.03)
if (savePlot)
   dev.off()
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
#
if (savePlot)
   cairo_pdf('ScatterMatrixRemaining.pdf', width=7)
par(mar=c(0,0,1,1)) #margini bottom, left, top, right
pairs(df2c[,c('age', 'dir_export_share','RD_empl_share', 'import_share_goods',
              'leverage')],
      lower.panel = panel.cor,
      upper.panel = upperNoJitterDelocfdi.panel)
legend('bottom', bty = 'n', 
       legend = c('deloc_fdi=0','deloc_fdi=1'), col=c(col0,col1),
       lty = NA, pch='o',lwd=2, inset = 0.03)
par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
if (savePlot)
   dev.off()
###############################################################################


########################## correlazione 

cormat = cor(df2[, cvarnames], use='pairwise.complete.obs')
thrCorr = 0.3
nc = length(cvarnames)
for (r in 1:nc)
   cormat[r,r] = NA
for (r in 1:(nc-1))
{
   for (c in (r+1):nc)
   {
      if (abs(cormat[r,c]) > thrCorr)
         cormat[c,r] = 1
      else
         cormat[c,r] = 0
   }
}
cormat = round(cormat,2)
print(cormat)

#deloc_fdi_share troppo legata a deloc_fdi
delendaPriorQtv = c('banks_number', 'deloc_fdi_share') 
# variabili correlate
delendaCorr = c('employees', 'totalassets', 'sales', 'ebit',
                'roa', 'ros', 'roe', 'RD_empl_share')


###############################################################################
#            CONFRONTO VARIABILI BINARIE/QUANTITATIVE (dataset df3)
###############################################################################
df3 = df2;
df3 = df3[,!(names(df3) %in% delendaPriorQtv)]
df3 = df3[,!(names(df3) %in% delendaCorr)]

print(names(df3))

delendaFinal = c('import_goods', 'dir_export', 'RD_inv')

###############################################################################
#                             PRUNING NA (dataset df4)
###############################################################################
df4 = df3;
df4 = df4[,!(names(df4) %in% delendaFinal)]
df4 = na.omit(df4)

print(dim(df4))
print(length(which(df4$deloc_fdi==1)))

###############################################################################



###############################################################################
#                            STIMA DEL MODELLO
###############################################################################

TryNested <- function(mdl, delenda, df) 
{
   xnames = all.vars(mdl$formula)
   xnames = xnames[-which(xnames=='deloc_fdi')]
   xnames = xnames[-which(xnames==delenda)]
   fmla = as.formula(paste('deloc_fdi ~ ', paste(xnames, collapse= '+')))
   nest = glm(formula = fmla, data=df, family = binomial (link="logit"))
   print(anova(nest, mdl, test = "Chisq"))
   return (nest)
}

############ inizializzazione tramite backward selection

xnames = names(df4)
xnames = xnames[-which(xnames=='deloc_fdi')]
fmla = as.formula(paste('deloc_fdi ~ ', paste(xnames, collapse= '+')))
print(fmla)
initMdl = glm(formula = fmla, data=df4, family = binomial (link="logit"))
print(summary(initMdl))
stepMdl <- MASS::stepAIC(initMdl, data=df4, direction = 'both', trace = F)
print(summary(stepMdl))

############ modelli nested:

# provo senza labour_flex              1.245e+00  1.044e+00   1.193  0.23281
nest1 = TryNested(mdl=stepMdl, delenda='labour_flex', df=df4)
print(summary(nest1))
# provo senza addedvalue               4.052e-05  2.359e-05   1.718  0.08586
nest2 = TryNested(mdl=nest1, delenda='addedvalue', df=df4)
print(summary(nest2)) # tutti coef con pvalue < 0.5

print(anova(nest2, stepMdl, test = "Chisq")) # confronto con modello iniziale

# ODDS RATIO modello finale
print(exp(nest2$coefficients))


