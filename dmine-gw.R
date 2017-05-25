library(maptools)
library(leaflet)
library(ggplot2)
library(gridExtra)

#--add Norwest boundary processing units
setwd("/dmine/data/USGS_groundwater/gwlevels_WA/")

gw_wa <- readShapePoints('NWISMapperExport.shp', 
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add Norwest Predicted stream temperature points for the WA Coast region
#--add Norwest Predicted stream temperature lines for the WA Coast region

#--add the US states boundary
setwd("/dmine/data/states/")

states <- readShapePoly('states.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add the US streams data if necessary
#setwd("/dmine/data/streams/")

#streams <- readShapeLines('USAstreams.shp', 
#                        proj4string=CRS
#                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#load the WACoast observed temperature locations - all locations.
setwd("/dmine/data/USGS_groundwater/gwlevels_WA/")
#load the WACoas attribute data file for the WACoast - Monthly Summaries
options(scipen = 50)

gw_wa_att <- data.frame(read.csv("gwlevels_washington_revised.csv", header=TRUE))

colnames(gw_wa_att)[2] <- c("SITENO")
library(stringr)
gw_wa_att <- cbind(gw_wa_att, str_split_fixed(gw_wa_att$lev_dt, "/", 3))
colnames(gw_wa_att)[19] <- c("year")
colnames(gw_wa_att)[18] <- c("day")
colnames(gw_wa_att)[17] <- c("month")

gw_wa_att <- subset(gw_wa_att, year == 95)
gw_wa_att <- subset(gw_wa_att, month == 6)

gw_wa_joined <- merge(gw_wa, gw_wa_att, by = "SITENO", duplicateGeoms = TRUE)

gw_wa_joined_unique <- unique(gw_wa_joined$lev_va)
ramper <- colorRampPalette(c("red", "blue"), length(gw_wa_joined_unique))


#-the range of years and months for our loop of every time step of stream temperature data
monthyearspan <- unique(streamtemp_att$MonthYear)


for (i in monthyearspan) {
stm <- subset(streamtemp_att, MonthYear == i)

  
lengthh <- length(unique(gw_wa_joined$lev_va))
data_seq = seq(min(na.omit(gw_wa_joined$lev_va)), max(na.omit(gw_wa_joined$lev_va)), length=length(gw_wa_joined))
col_pal = colorRampPalette(c('darkblue', 'red'))(lengthh)
cols = col_pal[ cut(gw_wa_joined$lev_va, data_seq, include.lowest=T) ]



#layout(matrix(2:1,nrow=2), width = c(1,2),height = c(1,1))
plotz <- plot(subset(units, GNLCCunit == "WACO"))
statez <- plot(subset(states, STATE_ABBR == "WA"))

points(gw_wa_joined, col = cols, pch=16, cex=.5)



#legend_image <- as.raster(matrix(cols4), ncol=1)
#plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
#text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
#rasterImage(legend_image, 0, 0, .25 ,1)

 MonthYearz <- streamtemp_att$MonthYear
streamtemp_att$sort_order <- streamtemp_att$SampleYear *100 + streamtemp_att$SampleMonth
p <- ggplot(streamtemp_att) + geom_boxplot(aes(x=reorder(MonthYear, sort_order), y=MonthlyMean))
#p <- ggplot(streamtemp_att) + geom_boxplot(aes(x = MonthYear, y = MonthlyMean, group = MonthYear, las=3))
p1 <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()



   
}


#streamtemp_joined <- data.frame(streamtemp_joined)


