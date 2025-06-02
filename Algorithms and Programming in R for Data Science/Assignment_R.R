rm(list=ls()) # manca

# I. GENERAZIONE DEL DATASET

set.seed(314)
n = 200000

infoSchool = list(
   N_Ls = 30,
   N_It = 70
)
infoStud = list(
   probFemale = 0.4,
   meanVoteLsByMales = 7.3,
   meanVoteLsByFemales = 6.4,
   meanVoteItByMales = 7.3-0.5,
   meanVoteItByFemales = 6.4-0.5,
   stdevVoteByMales = 1.7,
   stdevVoteByFemales = 0.6
)


CreateDataFrame <- function(n, infoStud, infoSchool)
{
   N = infoSchool$N_Ls + infoSchool$N_It
   probLs = infoSchool$N_Ls/N
   
   isLyceum = rbinom(n,1, probLs)
   schoolType = ifelse(isLyceum == 1, 'Ls','It')
   
   isFemale = rbinom(n,1,infoStud$probFemale) #variabile binaria con X==1 per le femmine
   gender = ifelse(isFemale == 1, 'F', 'M')
   
   idLs = which(schoolType=='Ls')
   idIt = which(schoolType=='It')
   
   idMales = which(gender=='M')
   idFemales = which(gender=='F')
   
   idLsM = intersect(idLs, idMales)
   idLsF = intersect(idLs, idFemales)
   idItM = intersect(idIt, idMales)
   idItF = intersect(idIt, idFemales)
   votes = rep(NA,n)
   votes[idLsM] = rnorm(length(idLsM),infoStud$meanVoteLsByMales,   infoStud$stdevVoteByMales)
   votes[idLsF] = rnorm(length(idLsF),infoStud$meanVoteLsByFemales, infoStud$stdevVoteByFemales)
   votes[idItM] = rnorm(length(idItM),infoStud$meanVoteItByMales,   infoStud$stdevVoteByMales)
   votes[idItF] = rnorm(length(idItF),infoStud$meanVoteItByFemales, infoStud$stdevVoteByFemales)
   
   # genero le label per le varie scuole come 'Ls01', 'Ls02', ..., 'Ls30' per i licei,
   schoolLs = sapply(1:infoSchool$N_Ls, function(k)  sprintf("Ls%02d", k))
   # ...e 'It01', 'It02', ..., 'It70' per gli istituti tecnici
   schoolIt = sapply(1:infoSchool$N_It, function(k)  sprintf("It%02d", k)) 
   #
   goesTo = rep(NA,n)
   goesTo[idLs] = AssignStudentsToSchool(length(idLs), schoolLs)
   goesTo[idIt] = AssignStudentsToSchool(length(idIt), schoolIt)
   
   dfStudents = data.frame(SchoolType = as.factor(schoolType),
                           School=as.factor(goesTo), 
                           Gender=as.factor(gender), Vote=votes)
   return (dfStudents)
}

AssignStudentsToSchool <- function(nStuds, schools)
{
   goesto = sample(x=schools, size=nStuds, replace=T)
   return (goesto)
}

print('1) Generazione del dataset') 
dfStudents = CreateDataFrame(n, infoStud, infoSchool)

#-------------------------------------------------------------------------------

# II: ANALISI DEI DATI

#2a Contare quanti studenti maschi e femmine ci sono per ogni scuola e per ogni
#   tipo di scuola
print('2a) Contare quanti studenti maschi e femmine ci sono per ogni scuola e per ogni tipo di scuola')

print(table(dfStudents$School, dfStudents$Gender))
print(table(dfStudents$SchoolType, dfStudents$Gender))

#-------------------------------------------------------------------------------

#2.b Calcolare media, mediana, quartili, deviazione standard per il voto in 
#    matematica a livello di studente, distinguendo per genere, e a livello di 
#    scuola, distinguendo per tipo di scuola.

print('2b) Calcolare media, mediana, quartili, deviazione standard per il voto in  matematica a livello di studente, distinguendo per genere, e a livello di scuola, distinguendo per tipo di scuola')
# con dplyr
library(dplyr, warn.conflicts = FALSE)
print('con dplyr:')
recapVoteByGender = dfStudents %>%
   group_by(Gender) %>%
   summarise(avg=mean(Vote),
             med=median(Vote),
             q25=quantile(Vote,0.25),
             q75=quantile(Vote,0.75),
             sd=sd(Vote))
print(recapVoteByGender)

recapVoteBySchoolType = dfStudents %>%
   group_by(SchoolType) %>%
   summarise(avg = mean(Vote),
             med=median(Vote),
             q25= quantile(Vote,0.25),
             q75= quantile(Vote,0.75),
             sd=sd(Vote))
print(recapVoteBySchoolType)

# con data.table
library(data.table)
print('con data.table:')
print(data.table(dfStudents)[, list(avg=mean(Vote),
                                       med=median(Vote),
                                       q25= quantile(Vote,0.25),
                                       q75= quantile(Vote,0.75),
                                       sd=sd(Vote)), by = .(Gender)])
print(data.table(dfStudents)[, list(avg=mean(Vote),
                                       med=median(Vote),
                                       q25= quantile(Vote,0.25),
                                       q75= quantile(Vote,0.75),
                                       sd=sd(Vote)), by = .(SchoolType)])

#-------------------------------------------------------------------------------

# 2.c Rappresentare tramite un opportuno grafico il voto in matematica in modo 
#     da consentire il confronto tra maschi e femmine.
print('2c) Rappresentare tramite un opportuno grafico il voto in matematica in modo da consentire il confronto tra maschi e femmine.')
print('Soluzione: vedi Grafico')
# opportuni colori
colM = 'lightskyblue'; color_data = col2rgb(colM)
colM_tr = rgb(color_data[1]/255, color_data[2]/255, color_data[3]/255, alpha = 0.5)
colF = 'pink'; color_data = col2rgb(colF)
colF_tr = rgb(color_data[1]/255, color_data[2]/255, color_data[3]/255, alpha = 0.5)
gcol= c(colF, colM)
bcol = c('pink3', 'lightskyblue4')

op = par('mfrow', 'mar', 'oma') #salvo il default
# split figura e margini
par(mfrow=c(1,2), mar=c(4.5,4.0,1.5,0.5)) #margini bottom, left, top, right

# con boxplot
boxplot(formula=dfStudents$Vote ~ dfStudents$Gender, data=dfStudents,
        frame.plot=F, col=gcol, border=bcol,
        ylab='vote', xlab='gender', main='comparison by boxplots')

# con istogrammi di densità
xM = dfStudents$Vote[dfStudents$Gender=='M']
xF = dfStudents$Vote[dfStudents$Gender=='F']
hist(x=xF, freq=F, breaks='FD', col=colF_tr,border=NA,
     xlim=c(min(xM), max(xM)),
     xlab='vote', main='comparison by histograms')
hist(x=xM, freq=F, breaks='FD', col=colM_tr,border=NA, add=T)
grid(lty='solid', lwd=0.5, col='white', nx=NA, ny=NULL)
legend('topright', c('F', 'M'), col=c(colF,colM), pch=15)

par(mfrow=op$mfrow, mar=op$mar, oma=op$oma) #ripristino default

#-------------------------------------------------------------------------------

# 2.d Rappresentare tramite un opportuno grafico il voto in matematica in modo 
#     da consentire il confronto tra licei scientifici e istituti tecnici.
print('2.d Rappresentare tramite un opportuno grafico il voto in matematica in modo da consentire il confronto tra licei scientifici e istituti tecnici')
print('Soluzione: vedi Grafico')
colLs = 'olivedrab3'; color_data = col2rgb(colLs)
colLs_tr = rgb(color_data[1]/255, color_data[2]/255, color_data[3]/255, alpha = 0.5)
colIt = 'orange'; color_data = col2rgb(colIt)
colIt_tr = rgb(color_data[1]/255, color_data[2]/255, color_data[3]/255, alpha = 0.5)
scol = c(colLs, colIt)
bcol = c('olivedrab4', 'sienna')

par(mfrow=c(1,2), mar=c(4.5,4.0,1.5,0.5)) #margini bottom, left, top, right
boxplot(formula=dfStudents$Vote ~ dfStudents$SchoolType, data=dfStudents,
        frame.plot=F, col=scol, border=bcol,
        ylab='vote', xlab='School type', main='comparison by boxplots')

xIt = dfStudents$Vote[dfStudents$SchoolType=='It']
xLs = dfStudents$Vote[dfStudents$SchoolType=='Ls']
hist(x=xIt, freq=F, breaks='FD', col=colLs_tr, border=NA,
     xlab='vote', main='comparison by histograms')
hist(x=xLs, freq=F, breaks='FD', col=colIt_tr, border=NA, add=T)
grid(lty='solid', lwd=0.5, col='white', nx=NA, ny=NULL)
legend('topright', c('It', 'Ls'), col=c(colLs,colIt), pch=15)

par(mfrow=op$mfrow, mar=op$mar, oma=op$oma) #ripristino default

#-------------------------------------------------------------------------------

# 2.e Creare una nuova variabile che riporti il voto in matematica in classi di
#     valori, distinguendo tra voto gravemente insufficiente (tra 0 e 5 
#     escluso), insufficiente (tra 5 incluso e 6 escluso), sufficiente (tra 6 
#     incluso e 7 escluso), buono (tra 7 incluso e 8.5 escluso), ottimo (almeno
#     8.5).

print('2e) Creare una nuova variabile che riporti il voto in matematica in classi di valori, distinguendo tra voto gravemente insufficiente (tra 0 e 5  escluso), insufficiente (tra 5 incluso e 6 escluso), sufficiente (tra 6  incluso e 7 escluso), buono (tra 7 incluso e 8.5 escluso), ottimo (almeno 8.5).')

# metodo 1
LabeledVote = as.ordered(0 + 1*(dfStudents$Vote>=5)
                         + 1*(dfStudents$Vote>=6)
                         + 1*(dfStudents$Vote>=7)
                         + 1*(dfStudents$Vote>=8.5))
levels(LabeledVote)=c('gravemente insuff.','insufficiente','sufficiente','buono','ottimo')
print(table(LabeledVote))
dfStudents$LabeledVote = LabeledVote

# metodo 2
LabeledVote_bycut = as.ordered(cut(dfStudents$Vote, breaks=c(-Inf,5,6,7,8.5,Inf), right=F))
levels(LabeledVote_bycut) = c('gravemente insuff.', 'insufficiente', 'sufficiente', 'buono', 'ottimo')
print(table(LabeledVote_bycut))

#-------------------------------------------------------------------------------

# 2.f Distinguendo in base al genere, determinare la distribuzione di frequenza
# (frequenze assolute e relative, semplici e cumulate) per la nuova variabile 
# di cui al punto e) e rappresentarla graficamente.

print('2.f) Distinguendo in base al genere, determinare la distribuzione di frequenza (frequenze assolute e relative, semplici e cumulate) per la nuova variabile di cui al punto e) e rappresentarla graficamente')

absFreqsTable_simple = as.data.frame.matrix(table(dfStudents$LabeledVote, dfStudents$Gender))
absFreqsTable_cumul = apply(absFreqsTable_simple, 2, cumsum)
divisor = colSums(absFreqsTable_simple)
relFreqsTable_simple = absFreqsTable_simple/divisor[col(absFreqsTable_simple)]
relFreqsTable_cumul = absFreqsTable_cumul/divisor[col(absFreqsTable_cumul)]

print('Frequenze assolute semplici')
print(absFreqsTable_simple)
#
print('Frequenze assolute cumulate')
print(absFreqsTable_cumul)
#
print('Frequenzs relative semplici')
print(relFreqsTable_simple)
#
print('Frequenza relative cumulate')
print(relFreqsTable_cumul)

print('Grafici distrib. freq.')

layout(mat=matrix(c(1,2,3,4,5,5), nrow=3,ncol=2, byrow=T), heights=c(0.23,0.23,0.1))
par(mar=c(2.0,3.0,2.0,0.5), oma=c(0, 0, 2.5, 0)) #margini bottom, left, top, right
colorVotes = c('dodgerblue', 'darkorange', 'limegreen', 'hotpink', 'gold')
barplot(as.matrix(absFreqsTable_simple), beside = T,
        col=colorVotes, border='ivory2', main='Absolute Frequencies (simple)')
grid(lty='solid', lwd=0.1, col='white', nx=NA, ny=NULL)
#
barplot(as.matrix(absFreqsTable_cumul), beside = T,
        col=colorVotes, border='ivory2', main='Absolute Frequencies (cumul.)')
grid(lty='solid', lwd=0.5, col='white', nx=NA, ny=NULL)
barplot(as.matrix(relFreqsTable_simple), beside = T,
        col=colorVotes, border='ivory2', main='Relative Frequencies (simple)', ylim=c(0,1))
grid(lty='solid', lwd=0.5, col='white', nx=NA, ny=NULL)
#
barplot(as.matrix(relFreqsTable_cumul), beside = T,
        col=colorVotes, border='ivory2', main='Relative Frequencies (cumul.)')
grid(lty='solid', lwd=0.5, col='white', nx=NA, ny=NULL)
# aggiungo plot "fake/dummy" per inserire la legenda
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend('top', rownames(absFreqsTable_simple), col=colorVotes, pch=15, cex=1.1, horiz=T, bty='n')
mtext("Labeled Vote Frequencies", side = 3, outer = T, font=1.25, cex=1.25)
#
par(mfrow=op$mfrow, mar=op$mar, oma=op$oma) #ripristino default

#-------------------------------------------------------------------------------

# 2.g Creare una nuova variabile binaria per un voto in matematica ottimo (
#     almeno pari a 8.5) e costruire la tabella a doppia entrata che riporta le
#     frequenze relative condizionate degli studenti rispetto al voto in 
#     matematica (ottimo vs. non ottimo), dato il tipo di scuola.

print('2.g Creare una nuova variabile binaria per un voto in matematica ottimo (almeno pari a 8.5) e costruire la tabella a doppia entrata che riporta le frequenze relative condizionate degli studenti rispetto al voto in matematica (ottimo vs. non ottimo), dato il tipo di scuola.')

dfStudents$IsOptimus = as.numeric(dfStudents$Vote >= 8.5)
optimusTable = as.data.frame.matrix(table(dfStudents$IsOptimus, dfStudents$SchoolType))
divisor = colSums(optimusTable)
optimusTable = optimusTable/divisor[col(optimusTable)]
print(optimusTable)