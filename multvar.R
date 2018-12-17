library(ade4)

pnact<-dcast(pna,formula=date~scientificname_accepted,fun.aggregate = sum,
             value.var = "measurementvalue")
# make dataframe with species names and their displayclass
spsel<-as.data.frame(unique(cbind(pna$scientificname_accepted,pna$displayclass)))
names(spsel)<-c("displayName","displayClass")

#taxa that correspond to displayClasses are confusing. Remove
weg<-which(names(pnact)%in%spsel$displayClass | names(pnact)=="Dinoflagellata")
pnact<-pnact[,-weg]

# select the most frequent species
spfr<-apply(pnact[,2:ncol(pnact)],2,FUN=function(x) length(x[x>0]))
pnact<-pnact[,c(1,which(spfr>15)+1)]

#make species list
splist<-names(pnact)[-1]
nsp<-length(splist)
weg<-which(!spsel$displayName%in%splist)
spsel<-spsel[-weg,]


dates<-pnact[,1]
pnact<-pnact[,-1]

row.names(pnact)<-dates
phsp<-sqrt(sqrt(pnact))
pca_phsp<-dudi.pca(phsp,scannf=F,nf=2)

years<-as.numeric(as.character(dates,format="%Y"))
months<-as.numeric(as.character(dates,format="%m"))

mnsax1<-as.vector(by(pca_phsp$li[,1],years,mean))
mnsax2<-as.vector(by(pca_phsp$li[,2],years,mean))
mmnsax1<-as.vector(by(pca_phsp$li[,1],months,mean))
mmnsax2<-as.vector(by(pca_phsp$li[,2],months,mean))

yrmns<-data.frame(ax1=mnsax1,ax2=mnsax2,labs=as.character(2002:2016))
momns<-data.frame(ax1=mmnsax1,ax2=mmnsax2,labs=as.character(1:12))
spseldf<-data.frame(ax1=pca_phsp$co[,1],ax2=pca_phsp$co[,2],displayName=splist)
spseldf<-merge(spseldf,spsel,by="displayName")

plotmultv<-function (specname){
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
}

pdf("multvarplots.pdf")
for(i in 1:7)plotmultv(unique(spseldf$displayClass)[i])
for(i in 1:length(splist))plotmultv(splist[i])
dev.off()