setwd("/agmesh-scenarios/scenario_52177")
combined.df <- data.frame(read.csv("2001_2015_usda_gridmet_WA"))
combined.split <- split(combined.df, combined.df$commoditycode)
setwd("/nethome/erichs/")
commodityspan = unique(combined.df$commoditycode)

lapply(names(combined.split), function(funct){write.table(combined.split[[funct]], file = paste("output", funct, sep = ""))})
