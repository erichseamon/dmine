my_hist3d <- function(x, y, freq=FALSE, nclass="auto") {
  n<-length(x)
  if (nclass == "auto") { nclass<-ceiling(sqrt(nclass.Sturges(x))) }
  breaks.x <- seq(min(x),max(x),length=(nclass+1))
  breaks.y <- seq(min(y),max(y),length=(nclass+1))
  h <- NULL
  for (i in 1:nclass) 
    for (j in 1:nclass) 
      h <- c(h, sum(x <= breaks.x[j+1] & x >= breaks.x[j] & y <= breaks.y[i+1] & y >= breaks.y[i] ) )
  if (freq) h <- h / n
  xx <- as.factor(round(mean(breaks.x[1:2])+(0:(nclass-1))*diff(breaks.x[1:2]), 1))
  yy <- as.factor(round(mean(breaks.y[1:2])+(0:(nclass-1))*diff(breaks.y[1:2]), 1))
  res <- cbind(expand.grid(xx,yy), h)
  colnames(res) <- c(deparse(substitute(x)),deparse(substitute(y)),'Frequency')
  formu <- as.formula(paste("Frequency ~ ", paste(colnames(res)[1:2], collapse= "+")))
  cloud(formu, res, panel.3d.cloud=panel.3dbars, col.facet='lightblue', 
        xbase=1, ybase=1, scales=list(arrows=FALSE, col=1), 
        par.settings = list(axis.line = list(col = "transparent")))
}


library(latticeExtra)

height <- rbeta(2000, 2, 5)
weight <- rgamma(2000, 10)
my_hist3d(height, weight, nclass=10)

