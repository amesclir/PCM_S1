set.seed(1001)

## set starting tree to NULL
tree<-NULL
## repeat simulation until non-NULL (i.e., non-## extinct) tree is obtained
while(is.null(tree))
  tree<-pbtree(n=100,b=1,d=0,extant.only=TRUE)
## plot the simulated tree
plotTree(tree,ftype="off",color="darkgray",lwd=1)

x<-fastBM(tree)
y<-fastBM(tree)

## set figure margins
par(mar=c(5.1,4.1,1.1,1.1))
## create scatterplot of x & y
plot(x,y,cex=1.2,pch=21,bg="gray",las=1,cex.axis=0.7,cex.lab=0.9,bty="n")
## add grid lines to the plot
grid()
## abbreviate the plotting area to match
## the range of our variables
clip(min(x),max(x),min(y),max(y))
## fit our linear model using OLS
fit.ols<-lm(y~x)
## add our fitted regression line to the plot
abline(fit.ols,lwd=2,col="darkgray")

fit.ols
summary(fit.ols)

## set plotting margins
par(mar=c(5.1,5.1,1.1,1.1),cex.axis=0.7,cex.lab=0.9)
## graph phylomorphospace projection
phylomorphospace(tree,cbind(x,y),label="off",node.size=c(0,0),bty="n",las=1)
## overlay points onto the phylomorphospace plot
points(x,y,pch=21,bg="gray",cex=1.2)
## add gridlines
grid()
## clip plot
clip(min(x),max(x),min(y),max(y))
## add fitted regression line
abline(fit.ols,lwd=2)

## compute PICs for x and y
ix<-pic(x,tree)
iy<-pic(y,tree)
## fit PIC regression through the origin
fit.pic<-lm(iy~ix+0)
fit.pic

## set plotting margins
par(mar=c(5.1,4.1,1.1,1.1))
## graph scatterplot of PICs
plot(ix,iy,cex=1.2,pch=21,bg="gray",las=1,xlab="PICs for x",ylab="PICs for y",cex.axis=0.7,cex.lab=0.9,bty="n")
## add gridlines to plot
grid()
## clip plotting area
clip(min(ix),max(ix),min(iy),max(iy))
## add fitted regression line
abline(fit.pic,lwd=2,col="darkgray")

summary(fit.pic)

