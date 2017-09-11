 
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

climate_cropcombo <- function(state1, county1, commodity1, damage1) {


library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("MASS")
library("raster")
#library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
#library("Evapotranspiration")
#library("plyr")
library("data.table")
#library("sirad")
library("rgeos")

library("stringr")
#library("car")
library("sp")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach") 
#detach(package:tidyr)
library(Hmisc)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$state[match(st.x$state,st.codes$full)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}


#Loading Agricultural Data
    
    #loading palouse summary counts
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
    sumcount1 <-  read.csv("Palouse_summary_counts.csv")
    #sumcount2 <- subset(sumcount1, state == state1)
    #sumcount2 <- subset(sumcount2, county == county1)
    sumcount2 <- subset(sumcount1, commodity == commodity1)
    claimaggcount_final2 <- subset(sumcount2, damagecause == damage1)
    
    #loading palouse summary mean loss
    
    meanloss1 <- read.csv("Palouse_summary_meanloss.csv")
    meanloss2 <- subset(meanloss1, state == state1)
    meanloss2 <- subset(meanloss2, county == county1)
    meanloss2 <- subset(meanloss2, commodity == commodity1)
    claimaggmean_final2 <- subset(meanloss2, damagecause == damage1)
  
    #loading palouse summary loss
    
    sumloss1 <- read.csv("Palouse_summary_sumloss.csv")
    #sumloss2 <- subset(sumloss1, state == state1)
    #sumloss2 <- subset(sumloss2, county == county1)
    sumloss2 <- subset(sumloss1, commodity == commodity1)
    claimaggloss_final2 <- subset(sumloss2, damagecause == damage1)
    
    #loading palouse summary acres
    
    sumacres1 <- read.csv("Palouse_summary_sumacres.csv")
    #sumacres2 <- subset(sumacres1, state == state1)
    #sumacres2 <- subset(sumacres2, county == county1)
    sumacres2 <- subset(sumacres1, commodity == commodity1)
    claimaggacres_final2 <- subset(sumacres2, damagecause == damage1)
    
    #--merge loss and acres for loss to acres calc
    cliamaggloss_and_acres_final2 <- merge(claimaggacres_final2, claimaggloss_final2, by = c("year", "month", "state", "county"))
    
    #aggregating loss by month
    
    claimaggloss_aggy <- aggregate(loss ~ month + damagecause + county + state + commodity, claimaggloss_final2, sum)
    yearr <- as.data.frame(toupper(month.abb))
    colnames(yearr) <- "month"
    claimaggloss_aggy2 <- join(yearr, claimaggloss_aggy)
    
    
    claimaggcount_aggy <- aggregate(count ~ month + damagecause + county + state + commodity, claimaggcount_final2, sum)
    yearr <- as.data.frame(toupper(month.abb))
    colnames(yearr) <- "month"
    claimaggcount_aggy2 <- join(yearr, claimaggcount_aggy)
    
    
    
    
    #assign(paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss_month.csv", sep=""), claimaggloss_aggy2)
    
    
    
    #loading palouse summary all file
    
    sumall1 <- read.csv("palouse_summary_all.csv")
    #sumall2 <- subset(sumall1, state == state1)
    #sumall2 <- subset(sumall2, county == county1)
    sumall2 <- subset(sumall1, commodity == commodity1)
    claimaggall_final2 <- subset(sumall2, damagecause == damage1)
    claimaggall_final2 <- subset(claimaggall_final2, month == "JAN" | month == "FEB" | month == "MAR" | month == "APR" | month == "MAY" | month == "JUN" | month == "JUL" | month == "AUG" | month == "SEP" )
    
    
    #nass
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries")
    nassacrescdl <- read.csv("palouse_2007-2015_NASS_WHEAT_acres.csv")
    #nassacrescdl$state <- state.name(nassacrescdl$state)
    nassacrescdl$state <- state.abb[match(nassacrescdl$state,state.name)]
    colnames(nassacrescdl)[2] <- "totalacres"
    
    #cube root function
    
    Math.cbrt <- function(x) {
      sign(x) * abs(x)^(1/3)
    }
    
    #cube root transformation for acres, loss, and total acres from nass
    
    
    
    #claimaggcount_final2
    claimaggloss_final2$cube_loss <- Math.cbrt(claimaggloss_final2$loss)
    claimaggacres_final2$cube_acres <- Math.cbrt(claimaggacres_final2$acres)
    
    
    claimaggall_final2$cube_acres <- Math.cbrt(claimaggall_final2$acres)
    claimaggall_final2$cube_loss <- Math.cbrt(claimaggall_final2$loss)
    
    nassacrescdl$cube_totalacres <- Math.cbrt(nassacrescdl$totalacres)
    
    
    #--by county for all years
    claimaggall_aggy_loss <- aggregate(cube_loss ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_acres <- aggregate(cube_acres ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_lossacres <- aggregate((cube_loss/cube_acres) ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    
    claimaggcount_aggy_loss <- aggregate(count ~ county + damagecause + state + commodity, claimaggcount_final2, sum)
    claimaggloss_aggy_loss <- aggregate(cube_loss ~ county + damagecause + state + commodity, claimaggloss_final2, sum)
    claimaggacres_aggy_loss <- aggregate(cube_acres ~ county + damagecause + state + commodity, claimaggacres_final2, sum)
    claimaggacres_aggy_lossacres <- aggregate((loss/acres) ~ county + damagecause.x + state + commodity.x, cliamaggloss_and_acres_final2, sum)
    
    
    
    #--by year for all counties
    claimaggall_aggy_lossacres_year <- aggregate(cube_acres ~  year + damagecause + county + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_loss_year <- aggregate(cube_loss ~  year + damagecause + county + state + commodity, claimaggall_final2, sum)
    claimaggall_agg_count <- aggregate(count ~  year + damagecause + county + state + commodity, claimaggall_final2, sum)
    
    
    #--individual response variable files.  above all file may be wrong 08.26.2017 es
    claimaggacres_aggy_lossacres_year <- aggregate(cube_acres ~  year + damagecause + county + state + commodity, claimaggacres_final2, sum)
    claimaggacres_aggy_lossacres_year_nocube <- aggregate(acres ~  year + damagecause + county + state + commodity, claimaggacres_final2, sum)
    
    claimaggcount_aggy_loss_year <- aggregate(count ~  year + damagecause + county + state + commodity, claimaggcount_final2, sum)
    claimaggloss_agg_loss <- aggregate(cube_loss ~  year + damagecause + county + state + commodity, claimaggloss_final2, sum)
    claimaggloss_agg_loss_nocube <- aggregate(loss ~  year + damagecause + county + state + commodity, claimaggloss_final2, sum)
    claimaggacres_agg_lossacres <- aggregate((loss/acres) ~  year + damagecause.x + county + state + commodity.x, cliamaggloss_and_acres_final2, sum)
    
    
    
    
    #--construction of merged file of acres with nass total acres.  No transforms
    
    combined <- merge(claimaggacres_aggy_lossacres_year_nocube, nassacrescdl, by = c("state", "county", "year"))
    
    #--creation of new variable for percent of loss acreage as compared to total area harvested
    
    combined$pct_act <- combined$acres / combined$totalacres
    
    #combined2 <- subset(combined, year >= startyear | year <= endyear)
    #colnames(combined) <- c("state", "county", "year", "damagecause", "commodity", "acres", "cube_acresNASS_totalacres", "percent_acres_loss" )
    #combined2 <- combined[-7]
    
    combined4 <- subset(combined, state == state1)
    combined4 <- subset(combined4, county == county1)
    #--Create a vector of equal length to climate matrix for correlation comparison
    year <- as.data.frame(c(2007:2015))
    colnames(year) <- c("year")
    
    #--combined4a is 2007-2015 acres harvested with Nass connected - by year
    combined4a <- join(year, combined4, by="year")
    
    #-total acres loss per year, no transform
  
    combined5 <- subset(claimaggacres_aggy_lossacres_year_nocube, state == state1)
    
    combined5a <- subset(combined5, county == county1)
    
    
    #combined5a <- join(year, combined5, by="year")
    
    
    ### cube root loss per year
    combined6 <- subset(claimaggloss_agg_loss, state == state1)
    
    combined6a <- subset(combined6, county == county1)
    
    #combined6a <- join(year, combined6, by="year")
    
    ### loss no transform

    combined7 <- subset(claimaggloss_agg_loss_nocube, state == state1)
    
    combined7a <- subset(combined7, county == county1)
    
    ### loss per acres, no transform
    
    combined8 <- subset(claimaggacres_agg_lossacres, state == state1)
  
    combined8a <- subset(combined8, county == county1)
    
    #cube acres - 2001-2015 - acres do not go back beyound 2001
    
    combined9 <- subset(claimaggacres_aggy_lossacres_year, state == state1)
  
    combined9a <- subset(combined9, county == county1)
    
    
    
    
    #--
    #climvar <- c("pr", "th", "pdsi", "pet", "erc", "rmin", "rmax",  "tmmn",  "tmmx",  "srad",  "sph", "vs", "fm1000",  "fm100")
    #startingmonth <- tolower(month.abb)
    #monthrange <- rev(c(1:12))
    
    #for (n in climvar) {}
    
      #for (o in startingmonth) {}
    
    
    #Loading Climate Data
    
    
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries5")
    files  <- list.files(pattern = 'Idaho')
    tables <- lapply(files, read.csv, header = TRUE)
    combined_idaho.df <- do.call(rbind , tables)
    combined_idaho.df$state <- "Idaho"
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries5")
    files  <- list.files(pattern = 'Oregon')
    tables <- lapply(files, read.csv, header = TRUE)
    combined_oregon.df <- do.call(rbind , tables)
    combined_oregon.df$state <- "Oregon"
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries5")
    files  <- list.files(pattern = 'Washington')
    tables <- lapply(files, read.csv, header = TRUE)
    combined_washington.df <- do.call(rbind , tables)
    combined_washington.df$state <- "Washington"
    #----sums <- read.csv(paste(kk, "_", i, "_palouse_summary", sep=""))
    
    combined.df <- rbind(combined_idaho.df, combined_washington.df, combined_oregon.df)
    
    library(maps)
    data(county.fips)
    colnames(combined.df)[16] <- "fips"
    library(stringr)
    county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
    colnames(county.fips2) <- c("state", "county")
    county.fips3 <- cbind(county.fips, county.fips2)
    combined1.df <- merge(combined.df,county.fips3, by = 'fips')
    
    gridmetmonthly <- data.frame(combined1.df)
    colnames(gridmetmonthly)[19] <- "state"
    
    countyz = simpleCap(county1)
    countyz = tolower(countyz)
    statez1a = state.name[grep(simpleCap(state1), state.abb)]
    gridmetmonthly <- subset(gridmetmonthly, state == statez1a)
    gridmetmonthly <- subset(gridmetmonthly, county == countyz)
    
    gridmetmonthly$monthchar <- as.character(gridmetmonthly$month)
    gridmetmonthly$monthchar <- trimws(gridmetmonthly$monthchar)
    library(Hmisc)
    gridmetmonthly$monthchar <- capitalize(gridmetmonthly$monthchar)
    gridmetmonthly$monthchar <- factor(gridmetmonthly$monthchar, levels=month.abb)
    #gridmetmonthly$monthchar <- as.numeric(gridmetmonthly$monthchar)
    gridmetmonthly <- gridmetmonthly[order(gridmetmonthly[,18], gridmetmonthly[,23]),]
    #gridmetmonthly$monthyear <- paste(as.numeric(gridmetmonthly$monthchar), ".", gridmetmonthly$year, sep="")
    gridmetmonthly$ID<-seq.int(nrow(gridmetmonthly))
    
    gridmetmonthly$month <- trimws(gridmetmonthly$month)
    #---05.18.17 need to create loop thru all claims and assign short term and long term drought variables.  mar 2009 is go back 3 and 6.  June 2009 is go back 6 and 9
    
    gridmetmonthly <- data.frame(gridmetmonthly)
    
    
    
    ymll <- c(1990:2015)
    
     yearmatchlist <- c(1990,1990,1991,1991,1992,1992,1993,1993,1994,1994,1995,1995,1996,1996,1997,1997,1998,1998,1999,1999,2000,2000,2001,2001,2002,2002,2003,2003,2004,2004,2005,2005,2006,2006,2007,2007,2008,2008,2009,2009,2010,2010,2011,2011,2012,2012,2013,2013,2014,2014,2015,2015)
     yml <- data.frame(yearmatchlist)
     colnames(yml)  <- "yearmatch"
         
      monthlist <- as.data.frame(rep(tolower(month.abb), each = 12))   
      numlist <- as.data.frame(rep((1:12), times = 12))
      monthnumlist <- as.data.frame(cbind(monthlist, numlist))
      colnames(monthnumlist) <- c("month", "monthcode")
      monthnumlist$combined <- paste(monthnumlist$month, monthnumlist$monthcode, sep="")
      climmonth <- monthnumlist$combined  
         
         
      jan1 <- subset(gridmetmonthly, month == "jan"  | month == "dec")
      jan1 <- head(jan1, -1)
      jan1 <- tail(jan1, -1)
      jan2 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov")
      jan2 <- head(jan2, -2)
      jan2 <- tail(jan2, -1)
      jan3 <- subset(gridmetmonthly, month == "jan"  | month == "dec"| month == "nov" | month == "oct")
      jan3 <- head(jan3, -3)
      jan3 <- tail(jan3, -1)
      jan4 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jan4 <- head(jan4, -4)
      jan4 <- tail(jan4, -1)
      jan5 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jan5 <- head(jan5, -5)
      jan5 <- tail(jan5, -1)
      jan6 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      jan6 <- head(jan6, -6)
      jan6 <- tail(jan6, -1)
      jan7 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      jan7 <- head(jan7, -7)
      jan7 <- tail(jan7, -1)
      jan8 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      jan8 <- head(jan8, -8)
      jan8 <- tail(jan8, -1)
      jan9 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      jan9 <- head(jan9, -9)
      jan9 <- tail(jan9, -1)
      jan10 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      jan10 <- head(jan10, -10)
      jan10 <- tail(jan10, -1)
      jan11 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jan11 <- head(jan11, -11)
      jan11 <- tail(jan11, -1)
      jan12 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" )
      jan12 <- head(jan12, -12)
      #jan12 <- tail(jan12, -1)
      
      
      feb1 <- subset(gridmetmonthly, month == "feb"  | month == "jan")
      feb1 <- tail(feb1, -2)
      feb2 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec")
      feb2 <- head(feb2, -1)
      feb2 <- tail(feb2, -2)
      feb3 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov")
      feb3 <- head(feb3, -2)
      feb3 <- tail(feb3, -2)
      feb4 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      feb4 <- head(feb4, -3)
      feb4 <- tail(feb4, -2)
      feb5 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      feb5 <- head(feb5, -4)
      feb5 <- tail(feb5, -2)
      feb6 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      feb6 <- head(feb6, -5)
      feb6 <- tail(feb6, -2)
      feb7 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      feb7 <- head(feb7, -6)
      feb7 <- tail(feb7, -2)
      feb8 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      feb8 <- head(feb8, -7)
      feb8 <- tail(feb8, -2)
      feb9 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      feb9 <- head(feb9, -8)
      feb9 <- tail(feb9, -2)
      feb10 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      feb10 <- head(feb10, -9)
      feb10 <- tail(feb10, -2)
      feb11 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      feb11 <- head(feb11, -10)
      feb11 <- tail(feb11, -2)
      feb12 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" )
      feb12 <- head(feb12, -11)
      feb12 <- tail(feb12, -1)
      
      mar1 <- subset(gridmetmonthly, month == "mar"  | month == "feb")
      mar1 <- tail(mar1, -2)
      mar2 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan")
      mar2 <- tail(mar2, -3)
      mar3 <- subset(gridmetmonthly, month == "mar"  | month == "feb"| month == "jan" | month == "dec")
      mar3 <- head(mar3, -1)
      mar3 <- tail(mar3, -3)
      mar4 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      mar4 <- head(mar4, -2)
      mar4 <- tail(mar4, -3)
      mar5 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      mar5 <- head(mar5, -3)
      mar5 <- tail(mar5, -3)
      mar6 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      mar6 <- head(mar6, -4)
      mar6 <- tail(mar6, -3)
      mar7 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      mar7 <- head(mar7, -5)
      mar7 <- tail(mar7, -3)
      mar8 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      mar8 <- head(mar8, -6)
      mar8 <- tail(mar8, -3)
      mar9 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      mar9 <- head(mar9, -7)
      mar9 <- tail(mar9, -3)
      mar10 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      mar10 <- head(mar10, -8)
      mar10 <- tail(mar10, -3)
      mar11 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      mar11 <- head(mar11, -9)
      mar11 <- tail(mar11, -3)
      mar12 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" )
      mar12 <- head(mar12, -10)
      mar12 <- tail(mar12, -2)
     
      
      apr1 <- subset(gridmetmonthly, month == "apr"  | month == "mar")
      apr1 <- tail(apr1, -2)
      apr2 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb")
      apr2 <- tail(apr2, -3)
      apr3 <- subset(gridmetmonthly, month == "apr"  | month == "mar"| month == "feb" | month == "jan")
      apr3 <- tail(apr3, -4)
      apr4 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      apr4 <- head(apr4, -1)
      apr4 <- tail(apr4, -4)
      apr5 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      apr5 <- head(apr5, -2)
      apr5 <- tail(apr5, -4)
      apr6 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      apr6 <- head(apr6, -3)
      apr6 <- tail(apr6, -4)
      apr7 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      apr7 <- head(apr7, -4)
      apr7 <- tail(apr7, -4)
      apr8 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      apr8 <- head(apr8, -5)
      apr8 <- tail(apr8, -4)
      apr9 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      apr9 <- head(apr9, -6)
      apr9 <- tail(apr9, -4)
      apr10 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      apr10 <- head(apr10, -7)
      apr10 <- tail(apr10, -4)
      apr11 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      apr11 <- head(apr11, -8)
      apr11 <- tail(apr11, -4)
      apr12 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" )
      apr12 <- head(apr12, -9)
      apr12 <- tail(apr12, -3)
      
      may1 <- subset(gridmetmonthly, month == "may"  | month == "apr")
      may1 <- tail(may1, -2)
      may2 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar")
      may2 <- tail(may2, -3)
      may3 <- subset(gridmetmonthly, month == "may"  | month == "apr"| month == "mar" | month == "feb")
      may3 <- tail(may3, -4)
      may4 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      may4 <- tail(may4, -5)
      may5 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      may5 <- head(may5, -1)
      may5 <- tail(may5, -5)
      may6 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      may6 <- head(may6, -2)
      may6 <- tail(may6, -5)
      may7 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      may7 <- head(may7, -3)
      may7 <- tail(may7, -5)
      may8 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      may8 <- head(may8, -4)
      may8 <- tail(may8, -5)
      may9 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      may9 <- head(may9, -5)
      may9 <- tail(may9, -5)
      may10 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      may10 <- head(may10, -6)
      may10 <- tail(may10, -5)
      may11 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      may11 <- head(may11, -7)
      may11 <- tail(may11, -5)
      may12 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" )
      may12 <- head(may12, -8)
      may12 <- tail(may12, -4)
      
      jun1 <- subset(gridmetmonthly, month == "jun"  | month == "may")
      jun1 <- tail(jun1, -2)
      jun2 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr")
      jun2 <- tail(jun2, -3)
      jun3 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar")
      jun3 <- tail(jun3, -4)
      jun4 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jun4 <- tail(jun4, -5)
      jun5 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      jun5 <- tail(jun5, -6)
      jun6 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      jun6 <- head(jun6, -1)
      jun6 <- tail(jun6, -6)
      jun7 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      jun7 <- head(jun7, -2)
      jun7 <- tail(jun7, -6)
      jun8 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      jun8 <- head(jun8, -3)
      jun8 <- tail(jun8, -6)
      jun9 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jun9 <- head(jun9, -4)
      jun9 <- tail(jun9, -6)
      jun10 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jun10 <- head(jun10, -5)
      jun10 <- tail(jun10, -6)
      jun11 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      jun11 <- head(jun11, -6)
      jun11 <- tail(jun11, -6)
      jun12 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" )
      jun12 <- head(jun12, -7)
      jun12 <- tail(jun12, -5)
      
      jul1 <- subset(gridmetmonthly, month == "jul"  | month == "jun")
      jul1 <- tail(jul1, -2)
      jul2 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may")
      jul2 <- tail(jul2, -3)
      jul3 <- subset(gridmetmonthly, month == "jul"  | month == "jun"| month == "may" | month == "apr")
      jul3 <- tail(jul3, -4)
      jul4 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar")
      jul4 <- tail(jul4, -5)
      jul5 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jul5 <- tail(jul5, -6)
      jul6 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      jul6 <- tail(jul6, -7)
      jul7 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      jul7 <- head(jul7, -1)
      jul7 <- tail(jul7, -7)
      jul8 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      jul8 <- head(jul8, -2)
      jul8 <- tail(jul8, -7)
      jul9 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      jul9 <- head(jul9, -3)
      jul9 <- tail(jul9, -7)
      jul10 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jul10 <- head(jul10, -4)
      jul10 <- tail(jul10, -7)
      jul11 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jul11 <- head(jul11, -5)
      jul11 <- tail(jul11, -7)
      jul12 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" )
      jul12 <- head(jul12, -6)
      jul12 <- tail(jul12, -6)
      
      aug1 <- subset(gridmetmonthly, month == "aug"  | month == "jul")
      aug1 <- tail(aug1, -2)
      aug2 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun")
      aug2 <- tail(aug2, -3)
      aug3 <- subset(gridmetmonthly, month == "aug"  | month == "jul"| month == "jun" | month == "may")
      aug3 <- tail(aug3, -4)
      aug4 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr")
      aug4 <- tail(aug4, -5)
      aug5 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      aug5 <- tail(aug5, -6)
      aug6 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      aug6 <- tail(aug6, -7)
      aug7 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      aug7 <- tail(aug7, -8)
      aug8 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      aug8 <- head(aug8, -1)
      aug8 <- tail(aug8, -8)
      aug9 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      aug9 <- head(aug9, -2)
      aug9 <- tail(aug9, -8)
      aug10 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      aug10 <- head(aug10, -3)
      aug10 <- tail(aug10, -8)
      aug11 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      aug11 <- head(aug11, -4)
      aug11 <- tail(aug11, -8)
      aug12 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" )
      aug12 <- head(aug12, -5)
      aug12 <- tail(aug12, -7)
      
      
      sep1 <- subset(gridmetmonthly, month == "sep"  | month == "aug")
      sep1 <- tail(sep1, -2)
      sep2 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul")
      sep2 <- tail(sep2, -3)
      sep3 <- subset(gridmetmonthly, month == "sep"  | month == "aug"| month == "jul" | month == "jun")
      sep3 <- tail(sep3, -4)
      sep4 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may")
      sep4 <- tail(sep4, -5)
      sep5 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      sep5 <- tail(sep5, -6)
      sep6 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      sep6 <- tail(sep6, -7)
      sep7 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      sep7 <- tail(sep7, -8)
      sep8 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      sep8 <- tail(sep8, -9)
      sep9 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      sep9 <- head(sep9, -1)
      sep9 <- tail(sep9, -9)
      sep10 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      sep10 <- head(sep10, -2)
      sep10 <- tail(sep10, -9)
      sep11 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      sep11 <- head(sep11, -3)
      sep11 <- tail(sep11, -9)
      sep12 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" )
      sep12 <- head(sep12, -4)
      sep12 <- tail(sep12, -8)
      
      
      oct1 <- subset(gridmetmonthly, month == "oct"  | month == "sep")
      oct1 <- tail(oct1, -2)
      oct2 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug")
      oct2 <- tail(oct2, -3)
      oct3 <- subset(gridmetmonthly, month == "oct"  | month == "sep"| month == "aug" | month == "jul")
      oct3 <- tail(oct3, -4)
      oct4 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      oct4 <- tail(oct4, -5)
      oct5 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      oct5 <- tail(oct5, -6)
      oct6 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      oct6 <- tail(oct6, -7)
      oct7 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      oct7 <- tail(oct7, -8)
      oct8 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      oct8 <- tail(oct8, -9)
      oct9 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      oct9 <- tail(oct9, -10)
      oct10 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      oct10 <- head(oct10, -1)
      oct10 <- tail(oct10, -10)
      oct11 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      oct11 <- head(oct11, -2)
      oct11 <- tail(oct11, -10)
      oct12 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" )
      oct12 <- head(oct12, -3)
      oct12 <- tail(oct12, -9)
      
      nov1 <- subset(gridmetmonthly, month == "nov"  | month == "oct")
      nov1 <- tail(nov1, -2)
      nov2 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep")
      nov2 <- tail(nov2, -3)
      nov3 <- subset(gridmetmonthly, month == "nov"  | month == "oct"| month == "sep" | month == "aug")
      nov3 <- tail(nov3, -4)
      nov4 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      nov4 <- tail(nov4, -5)
      nov5 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      nov5 <- tail(nov5, -6)
      nov6 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      nov6 <- tail(nov6, -7)
      nov7 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      nov7 <- tail(nov7, -8)
      nov8 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      nov8 <- tail(nov8, -9)
      nov9 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      nov9 <- tail(nov9, -10)
      nov10 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      nov10 <- tail(nov10, -11)
      nov11 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      nov11 <- head(nov11, -1)
      nov11 <- tail(nov11, -11)
      nov12 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" )
      nov12 <- head(nov12, -2)
      nov12 <- tail(nov12, -10)
      
      dec1 <- subset(gridmetmonthly, month == "dec"  | month == "nov")
      dec1 <- tail(dec1, -2)
      dec2 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct")
      dec2 <- tail(dec2, -3)
      dec3 <- subset(gridmetmonthly, month == "dec"  | month == "nov"| month == "oct" | month == "sep")
      dec3 <- tail(dec3, -4)
      dec4 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      dec4 <- tail(dec4, -5)
      dec5 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      dec5 <- tail(dec5, -6)
      dec6 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      dec6 <- tail(dec6, -7)
      dec7 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      dec7 <- tail(dec7, -8)
      dec8 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      dec8 <- tail(dec8, -9)
      dec9 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      dec9 <- tail(dec9, -10)
      dec10 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      dec10 <- tail(dec10, -11)
      dec11 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      dec11 <- tail(dec11, -12)
      dec12 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" )
      #dec12 <- head(dec12, -1)
      dec12 <- tail(dec12, -12)
      
      
      #loop thru every design matrix option, Jan1-12.  While we are only using months 1-9 - i am constructing that data in anyway.
      #a file is created for each 
         
       for (p in climmonth){  
         
         monthnum <- 1
         pp <- get(p)
         num1 <- length(unique(pp$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         p2 <-tail(pp, -monthnum)
         monthnum2 <- num1 - monthnum
         if (monthnum2 > 0) {p2 <- head(p2, -(monthnum2))} 
         p3 <- cbind(pp, num2)         
         colnames(p3)[25] <- "yearmatch"
         p4 <- aggregate(p3[, 2:16], list(p3$yearmatch), mean)
         setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix/")
         colnames(p4)[1] <- "year"
         write.csv(p4, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_", p, ".csv", sep="") )
         
         #writing aggregated loss by month for the palouse region.
         setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/climatematrix_summaries/")
         write.csv(claimaggloss_aggy2, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss_month.csv", sep=""))
         
         write.csv(claimaggall_aggy_lossacres_year, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss_per_acres.csv", sep=""))
         write.csv(claimaggall_aggy_loss_year, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss_year.csv", sep=""))
         
         write.csv(combined4a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_total_acres_harvested.csv", sep=""))
         write.csv(combined5a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_acres_loss.csv", sep=""))
         write.csv(combined6a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_cube_root_loss.csv", sep=""))
         write.csv(combined7a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss.csv", sep=""))
         write.csv(combined8a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_loss_per_acre.csv", sep=""))
         write.csv(combined9a, file = paste(state1, "_", county1, "_", commodity1, "_", damage1, "_cube_root_acres.csv", sep=""))
         
         
       }
      #writing aggregated loss by month for the palouse region.
     
      
      
      
      
}