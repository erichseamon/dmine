setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Oregon")


Idaho_list1 <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai")
Washington_list1 <- c("Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin")
Oregon_list1 <- c("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries/")

for (i in statez) {
 list <- get(paste(i, "_list1", sep=""))
  for (ii in list) {
    i2 <- toupper(state.abb[match(i,state.name)])
    climate_cropcombo(i2, ii, "WHEAT", "Drought")
  }
}


 
 
 
 
