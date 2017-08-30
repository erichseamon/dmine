  design_matrix_construction <- function(state1, county1, commodity1, damage1, climate_variable, response)   {
    #library(plotly)
    #packageVersion('plotly')
    
     #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss
  
  #monthlist is jan-dec, each repeated 12 times 
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))  
  #numlist is 12 months, repeated 12 times
  numlist <- as.data.frame(rep((1:12), times = 12))
  #monthnumlist puts month names and 1-12 together, which results in a vector of 144 that is the matrix of 12 by 12
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  #renaming columns
  colnames(monthnumlist) <- c("month", "monthcode")
  #put the monthlist all together in one vector
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  #rename the vector
  climmonth <- monthnumlist$combined
  
  designmatrix <- matrix(NA, ncol=12, nrow=12)
  
  #create an empty 144 vector to fill up with correlations between loss and climate
  dmvector <- as.data.frame(rep(NA, times=144))
  
  cl = 0
  for (ppp in climmonth) {
    cl = cl +1
  
  
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
    file1 <- read.csv(paste(state1, "_", county1, "_", commodity1, "_", damage1, "_", ppp, ".csv", sep=""))
    climatevar <- as.data.frame(cbind((file1[climate_variable]), file1[2]))
  
  
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
    file2 <- as.data.frame(read.csv(paste(state1, "_", county1, "_", commodity1, "_", damage1, "_", response, ".csv", sep="")))
    file2 <- subset(file2, state == state1)
    file2 <- subset(file2, county == county1)
    
   climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
   colnames(climatevar[3]) <- "zscore"
    kor <- join(climatevar, file2, by = "year")
    kor2 <- subset(kor, damagecause != "NA")
   #colnames(kor2)[9] <- "zscore"
    kor3 <- cor(kor2[3], kor2[9])
  
   #insert kor3 into designmatrix iteratively
  
   dmvector[cl,] <- kor3

  }
  
  dmvector <- as.data.frame(dmvector)
  colnames(dmvector) <- "correlations"
  dmvector2 <- t(matrix(dmvector$correlations, 12, 12, TRUE) ) 
  dmvector2 <- dmvector2[nrow(dmvector2):1, ]
  dmvector3 <- dmvector2[4:12,]
  
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 144)
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_pngs")
 
  #png(filename=paste(state1, "_", county1, "_", damage1, "_", climate_variable, "_designmatrix.png", sep=""))
  lblcol <- c(9:1)
  nba_heatmap <- heatmap.2(dmvector3, Rowv=NA, Colv=NA, col=topo.colors(16), scale="none", dendrogram = "none", tracecol = NA, key = TRUE, labRow=lblcol, key.ylab = "", key.xlab = "loss correlation range", key.title = "", main=paste(climate_variable, " - ", county1, " County \n", state1, " ", damage1, sep=""))
  #dev.off()
  
  }
  
  #library(RColorBrewer)
  #coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
  #heatmap(dmvector3, Rowv=NA, Colv=NA, col = coul)

  
  