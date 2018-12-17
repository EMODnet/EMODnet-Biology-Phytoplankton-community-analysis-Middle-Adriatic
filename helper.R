load("Phytoplankton.Rdata")
load("multivSpecies.Rdata")
doc<-HTML(readLines('about.html'))

ldc<-3

plotspecs<-function(datset,species,transf){
  tt<-get(datset)
  if(species=="")species<-names(tt)[ldc+1]
  if(!species%in%names(tt))species<-names(tt)[ldc+1]
  ttt<-data.frame(y=tt[,'year'],sp=tt[,species])
  if(transf) ttt$sp<-sqrt(sqrt(ttt$sp))
  miy<-min(ttt$y)
  may<-max(ttt$y)
  boxplot(sp~y,data=ttt,main=species,ylab='abundance',outline=T,
          xlim=c(1,15),at=(miy-2001):(may-2001))
}
plotseas<-function(datset,species,transf){
  tt<-get(datset)
  if(species=="")species<-names(tt)[ldc+1]
  if(!species%in%names(tt))species<-names(tt)[ldc+1]
  ttt<-data.frame(m=tt[,'month'],sp=tt[,species])
  if(transf) ttt$sp<-sqrt(sqrt(ttt$sp))
  miy<-min(ttt$m)
  may<-max(ttt$m)
  boxplot(sp~m,data=ttt,main=species,ylab='abundance',outline=T,xlim=c(1,12),at=miy:may)
}


plotmultv<-function (datset,specname){
  if (datset=="Phytoplankton"){
      par(pty="s")
      plot(yrmns$ax1,yrmns$ax2,col='red',xlim=c(-6,4),ylim=c(-6,4),type="l",asp=1,xlab="",ylab="",
           axes=FALSE,frame.plot=TRUE,main=paste("PCA biplot,",specname,"in green"))
      Axis(side=1, labels=FALSE)
      Axis(side=2, labels=FALSE)
      abline(h=0)
      abline(v=0)
      text(yrmns$ax1,yrmns$ax2,yrmns$labs,col='red',cex=.7)
      lines(momns$ax1,momns$ax2,col='blue')
      text(momns$ax1,momns$ax2,momns$labs,col='blue')
      arrows(-4,4,4,1.5,col='red',lwd=2,code=2)
      text(2,3,'Trend',col='red',cex=1)
      arrows(-.5,2,.5,-2,col='blue',lwd=2,code=3)
      text(.5,-3,'Season',col='blue',cex=1)
      colv<-rep('lightgray',80)
      lwdv<-rep(0.1,80)
      
      if(!specname%in%spseldf$displayName){
        colv[which(spseldf$displayClass==specname)]<-'green'
        lwdv[which(spseldf$displayClass==specname)]<-2
      }
      arrows(0,0,spseldf$ax1*5,spseldf$ax2*5,col=colv,lwd=lwdv,code=0)
      if(specname%in%spseldf$displayName){
        spec=which(spseldf$displayName==specname)
        arrows(0,0,spseldf$ax1[spec]*5,spseldf$ax2[spec]*5,col="green",lwd=3,code=0)
        points(spseldf$ax1[spec]*5,spseldf$ax2[spec]*5,col="green",cex=2,pch=16)
      }
   }else{
    frame()
  }
}

