getNCDFClimate <- function(var, year){ # Two arguments define the climate variable and year of the netCDF
  ncfname <- paste("/reacchspace/obj1/netcdf-161017/GRIDMET/data/", var, "_", year, ".nc", sep="")
  ncin <- nc_open(ncfname)
  var.array <- ncvar_get(ncin,"precipitation_amount") # get the climate variable values
  nc_close(ncin)
  var.vec.long <- as.vector(var.array) # convert from array to vector
  return(var.vec.long)
}