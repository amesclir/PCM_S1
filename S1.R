library(phytools)
library(geiger)
Barbetdata<-read.csv("Barbetdata.csv",row.names=1)
head(Barbetdata)

## set margins of the plot
par(mar=c(5.1,5.1,1.1,1.1))
## create scatterplot
plot(wing~Lnalt,data=Barbetdata,xlab="Lnalt",ylab= "wing",pch=21,bg="gray",cex=1.2,log="xy",las=1,cex.axis=0.7,cex.lab=0.9,bty="n")

Barbet.tree<-read.nexus("BarbetTree.nex")
## plot phylogeny of Barbets
plotTree(Barbet.tree,ftype="i",fsize=0.7,lwd=1)
## add node labels to the plotted tree
nodelabels(bg="white",cex=0.5,frame="circle")

library(geiger)
chk <- name.check(Barbet.tree, Barbetdata)
chk
Barbet.tree <- drop.tip(Barbet.tree,chk$tree_not_data)

## pull our home range and body mass as
## numeric vectors
wing<-setNames(Barbetdata[,"wing"],rownames(Barbetdata))
Lnalt<-setNames(Barbetdata[,"Lnalt"],rownames(Barbetdata))
## compute PICs for home range and body size
pic.wing<-pic(log(wing),Barbet.tree)
pic.Lnalt<-pic(log(Lnalt),Barbet.tree)

head(pic.wing,n=20)
head(pic.Lnalt,n=20)

fit.pic1<-lm(pic.wing~pic.Lnalt+0)
fit.pic1
summary(fit.pic1)

fit.pic2<-lm(pic.Lnalt~pic.wing+0)
fit.pic2
summary(fit.pic2)
