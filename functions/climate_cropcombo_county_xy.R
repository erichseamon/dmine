 
#------------------------------------------------------------------------#
# TITLE:        dmine-gridmet-monthly.R
# AUTHOR:       Erich Seamon
# INSTITUITON:  College of Natural Resources
#               University of Idaho
# DATE:         May-June. 2016
# STAGE:        DMINE extraction and subsetting of UI GRIDMET data for machine
#               learning analysis
#
# COMMENTS:     The purpose of this script is to allow a user to input a range
#               of years, and a lat/long bounding box - and the code will extract all variables
#               for that time period and location.  The second portion of the script steps thru
#               all counties within the US (or a subset), and extracts daily data from each county
#               and averages it for the month.  Each monthly average, for all variables, 
#               for each county, by the month, is then put into a matrix for machine learning analysis.
#                
#               More on the dmine design: dmine.io
#------------------------------------------------------------------------#

climate_cropcombo_county_xy <- function(commodity1, damage1, climate_variable, response, matrixnumber) {

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)

#alllist <- c("Idaho", "Oregon", "Washington")


#--Oregon

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
kk="Oregon"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
OR_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
#countyfiploop <- counties@data$FIPS

#--data frame of county fip list
#countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
#countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
#countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
#countylistrows <- 12 * nrow(countylist)



#---Washington



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
kk="Washington"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
WA_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#-----Idaho


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
kk="Idaho"
#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
ID_counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

counties <- rbind(ID_counties, WA_counties, OR_counties)

c_statelist <- paste(counties$STATE_NAME, "_", counties$NAME, sep="")
varr <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")
dmvectorappend <- data.frame(c(1), c(1), c(1), c(1), c(1), c(1), c(1))
#for (jj in varr) {
for (ll in c_statelist) {
  

  statey <- strsplit(ll, "_" )[[1]]
  state1 <- statey[1]
  county1 <- statey[2]
  state2 <- state.abb[match(statey[1],state.name)]
  
  #jj = "pet"
  #commodity1 <- "WHEAT"
  #damage1 <- "Drought"
  #response <- "cube_root_loss"
  #climate_variable <- jj
  ppp = matrixnumber
  
  
  #design_matrix_construction <- function(state1, county1, commodity1, damage1, climate_variable, response)   {
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
    dmvector <- data.frame(rep(NA, times=144))
    dmvectora <- data.frame(rep(NA, times=144))
    
    
    dmvector <- cbind(dmvector, dmvectora)
    
    
    cl = 0
    #for (ppp in climmonth) {
      cl = cl +1
      
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix")
      file1 <- read.csv(paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", ppp, ".csv", sep=""))
      climatevar <- as.data.frame(cbind((file1[climate_variable]), file1[2]))
      
      
      setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries")
      file2 <- as.data.frame(read.csv(paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", response, ".csv", sep="")))
      file2 <- subset(file2, state == state2)
      file2 <- subset(file2, county == county1)
      

      
      climatevar$zscore <- scale(climatevar[1], center = TRUE, scale = TRUE)
      colnames(climatevar[3]) <- "zscore"
      kor <- join(climatevar, file2, by = "year")
      kor2 <- subset(kor, damagecause != "NA")
      colnames(kor2)[3] <- "zscore"
      kor3 <- cor(kor2[1], kor2[9])
      
      #insert kor3 into designmatrix iteratively
      
      dmvector <- cbind(kor2[1], kor2[9], kor2[2])
      
      
  
    
    dmvector <- as.data.frame(dmvector)
    colnames(dmvector) <- c(climate_variable, "loss", "year")
    
  
    
  #}
  
  #library(RColorBrewer)
  #coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
  #heatmap(dmvector3, Rowv=NA, Colv=NA, col = coul)
  #design_matrix_construction(state1a, county1, commodity1, damage1, climate_variable, response)
  
  setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations_xy")
  dmvector$state <- state2
  dmvector$county <- county1
  dmvector$commodity <- commodity1
  dmvector$matrixnumber <- matrixnumber
  #write.csv(dmvector, file = paste(state2, "_", county1, "_", commodity1, "_", damage1, "_", climate_variable, "_", response, "_", ppp, "_correlations_xy.csv", sep=""))
  

    
  names(dmvectorappend) <- names(dmvector)
  dmvectorappend <- rbind(dmvectorappend, dmvector)
  
}

dmvectorappend <- dmvectorappend[-1, ]

return(dmvectorappend)

}

dmvectorappend <- climate_cropcombo_county_xy("WHEAT", "Drought", "pet", "cube_root_loss", "jul3")


dmvectorappend$clim_zscore <- scale(dmvectorappend[1], center = TRUE, scale = TRUE)
dmvectorappend$loss_zscore <- scale(dmvectorappend[2], center = TRUE, scale = TRUE)



corr <- cor(dmvectorappend[,8], dmvectorappend[,9])

plot(dmvectorappend[,8], dmvectorappend[,9], type="p", main = paste("Zscore space correlation: ", input$climate_variable, " vs ", input$predictor, "\n correlation: ", corr, sep=""), xlab = paste("zscore ", input$climate_variable, sep=""), ylab = "zscore cubed crop loss")





#}