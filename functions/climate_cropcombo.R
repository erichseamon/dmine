 
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
      jan2 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov")
      jan3 <- subset(gridmetmonthly, month == "jan"  | month == "dec"| month == "nov" | month == "oct")
      jan4 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jan5 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jan6 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      jan7 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      jan8 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      jan9 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      jan10 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      jan11 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jan12 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" )
    
      feb1 <- subset(gridmetmonthly, month == "feb"  | month == "jan")
      feb2 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec")
      feb3 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov")
      feb4 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      feb5 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      feb6 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      feb7 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      feb8 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      feb9 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      feb10 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      feb11 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      feb12 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" )
      
      mar1 <- subset(gridmetmonthly, month == "mar"  | month == "feb")
      mar2 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan")
      mar3 <- subset(gridmetmonthly, month == "mar"  | month == "feb"| month == "jan" | month == "dec")
      mar4 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      mar5 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      mar6 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      mar7 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      mar8 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      mar9 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      mar10 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      mar11 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      mar12 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" )
     
      
      apr1 <- subset(gridmetmonthly, month == "apr"  | month == "mar")
      apr2 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb")
      apr3 <- subset(gridmetmonthly, month == "apr"  | month == "mar"| month == "feb" | month == "jan")
      apr4 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      apr5 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      apr6 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      apr7 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      apr8 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      apr9 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      apr10 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      apr11 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      apr12 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" )
    
      
      may1 <- subset(gridmetmonthly, month == "may"  | month == "apr")
      may2 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar")
      may3 <- subset(gridmetmonthly, month == "may"  | month == "apr"| month == "mar" | month == "feb")
      may4 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      may5 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      may6 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      may7 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      may8 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      may9 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      may10 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      may11 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      may12 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" )

      
      jun1 <- subset(gridmetmonthly, month == "jun"  | month == "may")
      jun2 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr")
      jun3 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar")
      jun4 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jun5 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      jun6 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      jun7 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      jun8 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      jun9 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jun10 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jun11 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      jun12 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" )
      
      
      jul1 <- subset(gridmetmonthly, month == "jul"  | month == "jun")
      jul2 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may")
      jul3 <- subset(gridmetmonthly, month == "jul"  | month == "jun"| month == "may" | month == "apr")
      jul4 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar")
      jul5 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      jul6 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      jul7 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      jul8 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      jul9 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      jul10 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      jul11 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      jul12 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" )
  
      
      aug1 <- subset(gridmetmonthly, month == "aug"  | month == "jul")
      aug2 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun")
      aug3 <- subset(gridmetmonthly, month == "aug"  | month == "jul"| month == "jun" | month == "may")
      aug4 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr")
      aug5 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      aug6 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      aug7 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      aug8 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      aug9 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      aug10 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      aug11 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
      aug12 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" )
      
      
      
      sep1 <- subset(gridmetmonthly, month == "sep"  | month == "aug")
      sep2 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul")
      sep3 <- subset(gridmetmonthly, month == "sep"  | month == "aug"| month == "jul" | month == "jun")
      sep4 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may")
      sep5 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      sep6 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      sep7 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      sep8 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      sep9 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      sep10 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      sep11 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
      sep12 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" )
     
      oct1 <- subset(gridmetmonthly, month == "oct"  | month == "sep")
      oct2 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug")
      oct3 <- subset(gridmetmonthly, month == "oct"  | month == "sep"| month == "aug" | month == "jul")
      oct4 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      oct5 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      oct6 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      oct7 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      oct8 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      oct9 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      oct10 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      oct11 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
      oct12 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" )
    
      
      nov1 <- subset(gridmetmonthly, month == "nov"  | month == "oct")
      nov2 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep")
      nov3 <- subset(gridmetmonthly, month == "nov"  | month == "oct"| month == "sep" | month == "aug")
      nov4 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      nov5 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      nov6 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      nov7 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      nov8 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      nov9 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      nov10 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      nov11 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
      nov12 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" )
     
      
      dec1 <- subset(gridmetmonthly, month == "dec"  | month == "nov")
      dec2 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct")
      dec3 <- subset(gridmetmonthly, month == "dec"  | month == "nov"| month == "oct" | month == "sep")
      dec4 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug")
      dec5 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
      dec6 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
      dec7 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
      dec8 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
      dec9 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
      dec10 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
      dec11 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
      dec12 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" )
 
      
      
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
         p3 <- cbind(p2, num2)         
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