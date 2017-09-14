setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations_xy/")
files  <- list.files(pattern = 'pet')
#filey <- do.call(rbind, strsplit(files, '[_]'))


tables <- lapply(files, read.csv, header = TRUE)

tables <- lapply(tables, function(x) { x["X"] <- NULL; x }) #--remove first index row from each list
list <- unlist(tables, recursive = FALSE)
tables2 <- do.call("rbind", tables)

tables2$clim_zscore <- scale(tables2[1], center = TRUE, scale = TRUE)
tables2$loss_zscore <- scale(tables2[2], center = TRUE, scale = TRUE)

plot(tables2$clim_zscore, tables2$loss_zscore, type="p")
