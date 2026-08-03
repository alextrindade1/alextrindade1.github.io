## lag plot
"lagplot" <- function(x,lag=1){
if(is.matrix(x)) x <- x[,1]
par(mfcol=c(1,1))
nT <- length(x)
c1 <- paste("lag-",lag)
r <- cor(x[1:(nT-lag)],x[(lag+1):nT])
c2 <- paste("Correlation: ",round(r,4))
plot(x[1:(nT-lag)],x[(lag+1):nT],xlab=c1,ylab='x(t)',main=c2)
##cat("correlation with lag-",lag," is ",r,"\n")
}