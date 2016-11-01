setdir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "drought", sep="")
install.packages("devtools")
devtools::install_github("Eflores89/nasadata")
library(nasadata)
categories <- eonet_categories()
names(categories)
categories[,1:2]
sources <- eonet_sources()
names(sources)
sources[,1:2]


an_event <- earth_event(status = "all", 
                        sources = "all", 
                        category_id = "18", 
                        limit = 10000,
                        LimitType = "days")
                        
class(an_event)

names(an_event)

an_event$Events$event_id
an_event$Events$event_title
an_event$Sources$source_url

an_event$Geography

#Events: Gives us an overview of each event(s) in a data.frame. This includes id, title, description, link.
#Sources: Tells us the sources by event id in a data.frame.
#Categories: Categories by event id (also in a data.frame).
#Geography: Gives us the coordinates or polygon where the event took place. This can be a list with lists. For example, if there are several coordinates and times (if it is an event that was prolonged or moved, like a hurricane).
#Meta: Some metadata related to the query, including the string used.



#-----

coord_long <- (-121.85318)
coord_lat <- 37.10632
key <- "ATGUaoA6VinlXFOqyn4hoTyVFtDaXiHoZbWnUFsM"


images <- earth_asset(key, 
                      lon = coord_long, 
                      lat = coord_lat, 
                      start_date = "2016-09-01", 
                      end_date = "2016-10-05")

names(images)
images$date

img <- earth_image(key, 
                   lon = coord_long, 
                   lat = coord_lat, 
                   date = "2016-10-01", plot = TRUE)

