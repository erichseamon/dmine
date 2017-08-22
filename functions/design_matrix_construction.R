design_matrix_construction <- function(state1, county1, commodity1, damage1, climate_variable, response) {
  
  #response may be: loss_month, loss_per_acres, loss_year, total_acres_harvested, and total_acres_loss
  monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))   
  numlist <- as.data.frame(rep((1:12), times = 12))
  monthnumlist <- as.data.frame(cbind(monthlist, numlist))
  colnames(monthnumlist) <- c("month", "monthcode")
  monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
  climmonth <- monthnumlist$combined
  
  designmatrix <- matrix(NA, ncol=12, nrow=12)
  
  for (ppp in climmonth) {
  
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
  file1 <- read.csv(paste(state1, "_", county1, "_", commodity1, "_", damage1, "_", ppp, ".csv", sep=""))
  climatevar <- as.data.frame(cbind((file1[climate_variable]), file1[2]))
  
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
  file2 <- as.data.frame(read.csv(paste(state1, "_", county1, "_", commodity1, "_", damage1, "_", response, ".csv", sep="")))
  
  kor <- join(climatevar, file2, by = "year")
  kor2 <- subset(kor, cube_acres != "NA")
  
  kor3 <- cor(kor2[1], kor2[8])
  
  #insert kor3 into designmatrix iteratively
  
  designmatrix[2:1[1],5] <- 1
  
  
  }
  