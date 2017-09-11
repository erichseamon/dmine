setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_correlations/")
files  <- list.files(pattern = 'pet')
filey <- do.call(rbind, strsplit(files, '[_]'))


tables <- lapply(files, read.csv, header = TRUE)

tables <- lapply(tables, function(x) { x["X"] <- NULL; x }) #--remove first index row from each list

tables <- lapply(tables, function(x) arrange(x, -row_number())) #--(flips matrix - puts jan as 1st row and sept as 9th row)

#!!!!!!fix-row by column, or number of months by ending month
table2 <- lapply(tables, function(x) x[9,5])


table3 <- data.frame(matrix(unlist(table2), nrow=26, byrow=T))
colnames(table3) <- "correlations"
#combined <- do.call(rbind , tables)

table4 <- cbind(filey, table3)

table5 <- table4[c(2:5,10)]

colnames(table5) <- c("NAME", "COMMODITY", "DAMAGE", "climate", "correlations")

#table5$STATE_NAME <-  state.name[match(table5[,1],state.abb)]





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




counties2 <- merge(counties, table5, by = "NAME" )






colorbrew <- list(color = brewer.pal(26, c("green", "blue", "yellow")))
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 108)


colorss = colorRampPalette(brewer.pal(9,"YlGnBu"))

finalcol <- colorss(len <- length(counties2$correlations))
finalcol2 <- colorss(length(counties2$correlations))[order(order(counties2$correlations))]



plot(counties2, col = finalcol2)
text(coordinates(counties2), labels=round(counties2$correlations, 2), cex=0.7)

