library(latex2exp) #per label/titoli con LaTeX


colGenericMedium = rgb(250/255, 164/255,  58/255)#'darkorange'
colInMichMedium  = rgb( 93/255, 165/255, 218/255)#'skyblue3'
colNoMichMedium  = rgb( 96/255, 189/255, 104/255)#'firebrick2'
colGenericDark   = rgb(223/255,  92/255,  36/255)#'darkorange3'
colInMichDark    = rgb( 38/255,  93/255, 171/255)#'steelblue4'
colNoMichDark    = rgb(  5/255, 151/255,  72/255)#'darkred'
colLine          = rgb(178/255, 118/255, 178/255) #'purple'


Plot2Bar <- function(vals, names, cols, colsTxt, xlim, tit, xlab, save2pdf=F)
{
  if (save2pdf)
    pdf('BarplotInNoMichelin.pdf', width=7,height=4)
  
  nIn = vals[1]; nNo=vals[2]
  barplot(height=vals, names=names, col=cols,
          xlim = xlim, horiz=T, xlab=xlab, main = tit
  )
  text(x = nIn+5, y = 0.75, labels = nIn, col = colsTxt[1])
  text(x = nNo+5, y = 1.95, labels = nNo, col = colsTxt[2])
  
  if (save2pdf)
    dev.off()
}



ComputeBreaks <- function(mydata)
{
  #binwidth (Freedman–Diaconis rule)
  bw = 2*IQR(mydata)*(length(mydata))^(-1/3)
  nb = ceil(max(abs(mydata))/bw)
  maxbinrange = nb*bw+bw/2
  breaks = seq(from=-maxbinrange, to=maxbinrange, by=bw)
  return (breaks)
}

PlotHist <- function(data, name, dlim, breaks, hlim, colBorder, colFill, 
                     save2pdf=F)
{
  
  if (save2pdf)
  {
    fname = paste('Hist', name, '.pdf', sep='')
    pdf(fname)
  }
  
  layout( matrix(c(1,2), nrow=2))
  par(mar=c(0,4,2,0)) #margini bottom, left, top, right

  hist(x=data, probability=F, breaks, 
       border=colBorder, col=colFill, xlim=dlim, ylim=hlim,
       main = name)#xlab=NA
  grid(lty="solid", lwd=0.75, col="white", nx=NA, ny=NULL)

  par(mar=c(11,4,0,0))
  boxplot(x=data, horizontal=T, ylim=dlim, boxwex=0.4, 
          border=colBorder, col=colFill,  frame=F,  axes=F);
  
  if (save2pdf)
    dev.off()
  
  par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
  layout( matrix(c(1,1), nrow=1))
  par(mfrow = c(1, 1))  # Reset to default layout
}


Plot3Scores <- function(data3, names3, breaks, dlim, hlim, colFill, colBorder, 
                        save2pdf = F)
{
  if (save2pdf)
  {
    fname = paste('Hist3', paste(names3,collapse=''), '.pdf', sep='')
    pdf(fname, width=7)# ,height=7)
  }
  
  
  layout( matrix(c(1,4,
                   2,5,
                   3,6), ncol=3, nrow=2))
  par(mar=c(0,4,2,0)) #margini bottom, left, top, right
  datap = data3[,1]; name=names3[1]
  hist(x=datap, probability=F, breaks, 
       border=colBorder, col=colFill, xlim=dlim, ylim=hlim,
       main = name, xlab=NA)
  grid(lty="solid", lwd=0.75, col="white", nx=NA, ny=NULL)
  #
  datap = data3[,2]; name=names3[2]
  hist(x=datap, probability=F, breaks, 
       border=colBorder, col=colFill, xlim=dlim, ylim=hlim,
       main = name, xlab=NA)
  grid(lty="solid", lwd=0.75, col="white", nx=NA, ny=NULL)
  #
  datap = data3[,3]; name=names3[3]
  hist(x=datap, probability=F, breaks, 
       border=colBorder, col=colFill, xlim=dlim, ylim=hlim, 
       main = name, xlab=NA)
  grid(lty="solid", lwd=0.75, col="white", nx=NA, ny=NULL)
  #
  # seconda riga del plot
  #
  par(mar=c(21,4,0,0)) #margini bottom, left, top, right
  datap = data3[,1]; name=names3[1]
  boxplot(x=datap, horizontal=T, ylim=dlim, boxwex=0.4, 
          border=colBorder, col=colFill,  frame=F,  axes=F);
  #
  datap = data3[,2]; name=names3[2]
  boxplot(x=datap, horizontal=T, ylim=dlim, boxwex=0.4, 
          border=colBorder, col=colFill,  frame=F,  axes=F);
  #
  datap = data3[,3]; name=names3[3]
  boxplot(x=datap, horizontal=T, ylim=dlim, boxwex=0.4, 
          border=colBorder, col=colFill,  frame=F,  axes=F);
  
  if (save2pdf)
    dev.off()
  
  par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
  layout( matrix(c(1,1), nrow=1))
  par(mfrow = c(1, 1))  # Reset to default layout
}



# Correlation panel
panel.cor <- function(x, y)
{
  usr <- par('usr'); #on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y, use='pairwise.complete.obs'), digits=2)
  #txt <- paste0('R = ', r)
  rhotxt = TeX(r"( \rho )")
  #txt <- paste(rhotxt, ' = ', r)
  txt <- paste('ρ=', r)
  #cex.cor <- 0.5/strwidth(txt)
  #text(0.5, 0.5, txt, cex = cex.cor * r)
  text(0.5, 0.5, txt, cex=2.0)
}
# Customize upper panel
upperGenerig.panel<-function(x, y)
{
  points(x,y, pch = 19, cex=0.5, col=colGenericDark, 
         ylim = c(10, 30),xlim = c(10, 30))
}
upperInMich.panel<-function(x, y)
{
  points(x,y, pch = 19, cex=0.5, col=colInMichDark)
}
upperNoMich.panel<-function(x, y)
{
  points(x,y, pch = 19, cex=0.5, col=colNoMichDark)
}

PlotHistPlusNormal <- function(data, name, dlim, breaks, hlim, colBorder, colFill, 
                     save2pdf=F)
{
  
  if (save2pdf)
  {
    fname = paste('Hist', name, '.pdf', sep='')
    pdf(fname)
  }
  
  layout( matrix(c(1,2), nrow=2))
  par(mar=c(0,4,0,0)) #margini bottom, left, top, right
  
  hist(x=data, probability=T, breaks, 
       border=colBorder, col=colFill, xlim=dlim, ylim=hlim,
       main = '')#xlab=NA
  grid(lty="solid", lwd=0.75, col="white", nx=NA, ny=NULL)
  curve(add=T,dnorm(x,mean=0,sd=1),from=-3.1,to=3.1, n = 101, col=colLine,lwd=3)
  legend('topright', bty = 'n', 
         legend = 'Normale std.', 
         col = colLine, lty = 1, lwd=2, inset = 0.03)
  
  
  par(mar=c(11,4,0,0))
  boxplot(x=data, horizontal=T, ylim=dlim, boxwex=0.4, 
          border=colBorder, col=colFill,  frame=F,  axes=F);

  if (save2pdf)
    dev.off()
  
  par(mar=c(5,4,4,2) + 0.1) #Reset to default margins
  layout( matrix(c(1,1), nrow=1))
  par(mfrow = c(1, 1))  # Reset to default layout
}

#################
upperNoJitterDelocfdi.panel<-function(x, y)
{
   points(x[id0],y[id0], cex=0.5, col=col0, 
          ylim = c(0, 100),xlim = c(0, 100))
   points(x[id1],y[id1], cex=0.5, col=col1)
   grid(lty='dotted')
}



upperJitterDelocfdi.panel<-function(x, y)
{
   points(x[id0],y[id0], cex=0.5, col=col0, 
          ylim = c(0, 100),xlim = c(0, 100))
   xn = jitter(x[id1],amount=0)
   yn = jitter(y[id1],amount=0)
   points(xn,yn, cex=0.5, col=col1)
   grid(lty='dotted')
}



