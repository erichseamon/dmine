#------------------------------------------------------------------------#
# TITLE:        agmesh_analysis_ET.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Raster Generation - Stage 3
#
# COMMENTS:     This is the agmesh analyis script for ET.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. This 
#               agmesh analysis script uses the data generated in 
#               stages 1 and 2 to create ET raster bricks for the
#               range of years input.
#
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#


#-------Prepping for Raster calculations for evapotranspiration---------#

print("Generating arrays for evapotranspiration calculation ...")

nrow <- nrow(tmmnbrick)
ncol <- ncol(tmmnbrick)
nlayers <- nlayers(tmmnbrick)

#extraTraster <- raster(extraT, layer=1)
dayspan <- c(1:nlayers)
dayspan_array <- array(data=NaN, c(nrow,ncol,nlayers))
ETlat_array <- array(data=NaN, c(nrow,ncol,nlayers))
extrat_array <- array(data=NaN, c(nrow,ncol,nlayers))

extratncell <- c(1:15260)

for (i in yearspan) {
  print(paste("Looping through yearly variable preparation for:", i))
    for (k in dayspan) {
      #for (j in ncolz) {
      #for (k in nrowz) {
      dayspan_array[,,k] <- i
      loopvalue <- tmmnbrick
      #loopvalue <- get(paste(j, i, "brick", sep=""))
      ETlat_array[,,k] <- yFromRow(loopvalue)
      #ETlat_array[j,k,i] <- yFromRow(srad2007brick, row=1:109(srad2007brick))

ETlat<- yFromRow(loopvalue)
ETlat <- t(ETlat)
ETlat_matrix <- matrix(ETlat,nrow=140,ncol=length(ETlat),byrow=TRUE)
ETlat_matrix <- t(ETlat_matrix)
#ETlat_array <- as.array(ETlat_matrix)
ETlat_raster <- setValues(loopvalue, ETlat_array)
ETlat_raster <- raster(ETlat_raster)
ETlatrasterbrick <- brick(ETlat_raster)
ETlatrasterbrickfinal <- brick(ETlat_raster, ETlat_raster, 
                               ETlat_raster, ETlat_raster,ETlat_raster, 
                               ETlat_raster, ETlat_raster, ETlat_raster)
ETlatrasterbrickSet <- setValues(loopvalue, ETlat_array)

   
    }
}