for (j in 1:nlayers(erichet)) {
  plotz <- subset(erichet, j)
  plotz <- get(paste("erichet$layer.", j, sep=""))
  plot(plotz)
  
}
