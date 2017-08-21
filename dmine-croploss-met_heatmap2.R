 
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

climate_cropcombo <- function(state1, county1, commodity1, damage1, climvariable, startyear, endyear) {


library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
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
library("MASS")
library("stringr")
#library("car")
library("sp")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach") 
detach(package:tidyr)
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

    
    #------
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
    sumcount1 <-  read.csv("Palouse_summary_counts.csv")
    sumcount2 <- subset(sumcount1, state == state1)
    sumcount2 <- subset(sumcount2, county == county1)
    sumcount2 <- subset(sumcount2, commodity == commodity1)
    claimaggcount_final2 <- subset(sumcount2, damagecause == damage1)
    
    meanloss1 <- read.csv("Palouse_summary_meanloss.csv")
    meanloss2 <- subset(meanloss1, state == statez1)
    meanloss2 <- subset(meanloss2, county == county1)
    meanloss2 <- subset(meanloss2, commodity == commodity1)
    claimaggmean_final2 <- subset(meanloss2, damagecause == damage1)
    
    sumloss1 <- read.csv("Palouse_summary_sumloss.csv")
    sumloss2 <- subset(sumloss1, state == state1)
    sumloss2 <- subset(sumloss2, county == county1)
    sumloss2 <- subset(sumloss2, commodity == commodity1)
    claimaggloss_final2 <- subset(sumloss2, damagecause == damage1)
    
    sumacres1 <- read.csv("Palouse_summary_sumacres.csv")
    sumacres2 <- subset(sumacres1, state == state1)
    sumacres2 <- subset(sumacres2, county == county1)
    sumacres2 <- subset(sumacres2, commodity == commodity1)
    claimaggacres_final2 <- subset(sumacres2, damagecause == damage1)
    
    claimaggloss_aggy <- aggregate(loss ~ month + damagecause + county + state + commodity, claimaggloss_final2, sum)
    
    yearr <- as.data.frame(toupper(month.abb))
    colnames(yearr) <- "month"
    claimaggloss_aggy2 <- join(yearr, claimaggloss_aggy)
    
    sumall1 <- read.csv("palouse_summary_all.csv")
    sumall2 <- subset(sumall1, state == state1)
    sumall2 <- subset(sumall2, county == county1)
    sumall2 <- subset(sumall1, commodity == commodity1)
    claimaggall_final2 <- subset(sumall2, damagecause == damage1)
    claimaggall_final2 <- subset(claimaggall_final2, month == "JAN" | month == "FEB" | month == "MAR" | month == "APR" | month == "MAY" | month == "JUN" | month == "JUL" | month == "AUG" | month == "SEP" )
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries")
    nassacrescdl <- read.csv("palouse_2007-2015_NASS_WHEAT_acres.csv")
    #nassacrescdl$state <- state.name(nassacrescdl$state)
    nassacrescdl$state <- state.abb[match(nassacrescdl$state,state.name)]
    colnames(nassacrescdl)[2] <- "totalacres"
    
    
    Math.cbrt <- function(x) {
      sign(x) * abs(x)^(1/3)
    }
    
    claimaggall_final2$cube_acres <- Math.cbrt(claimaggall_final2$acres)
    claimaggall_final2$cube_loss <- Math.cbrt(claimaggall_final2$loss)
    nassacrescdl$cube_totalacres <- Math.cbrt(nassacrescdl$totalacres)
    
    
    #--by county
    claimaggall_aggy_loss <- aggregate(cube_loss ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_acres <- aggregate(cube_acres ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_lossacres <- aggregate((cube_loss/cube_acres) ~ county + damagecause + state + commodity, claimaggall_final2, sum)
    
    #--by year
    claimaggall_aggy_lossacres_year <- aggregate(cube_acres ~  year + damagecause + county + state + commodity, claimaggall_final2, sum)
    claimaggall_aggy_loss_year <- aggregate(cube_loss ~  year + damagecause + county + state + commodity, claimaggall_final2, sum)
    
    
    combined <- merge(claimaggall_aggy_lossacres_year, nassacrescdl, by = c("state", "county", "year"))
    combined$percent_cube_acres_loss <- combined$cube_acres / combined$cube_totalacres
    
    combined2 <- subset(combined, year >= startyear | year <= endyear)
    colnames(combined2) <- c("state", "county", "year", "damagecause", "commodity", "acres", "ID", "NASS_totalacres", "percent_acres_loss" )
    combined2 <- combined2[-7]
    
    
    
    #--
    #climvar <- c("pr", "th", "pdsi", "pet", "erc", "rmin", "rmax",  "tmmn",  "tmmx",  "srad",  "sph", "vs", "fm1000",  "fm100")
    #startingmonth <- tolower(month.abb)
    #monthrange <- rev(c(1:12))
    
    #for (n in climvar) {}
    
      #for (o in startingmonth) {}
    
    
    
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
    
    
    gridmetmonthly <- data.frame(combined1.df)
    colnames(gridmetmonthly)[19] <- "state"
    
    countyz = simpleCap(countyz)
    countyz = tolower(county1)
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
 
      
      
      
         
       for (p in climmonth[1:12]){  
         
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
       }
         
         
         
         
         
         
         
         
         jan1 <- subset(gridmetmonthly, month == "jan"  | month == "dec")
         num1 <- length(unique(jan1$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan1 <- jan1[-1,]
         jan1 <- head(jan1, -(num1-1))
         jan1 <- cbind(jan1, num2)
         jan1 <- aggregate(jan1[, 2:16], list(jan1$yearmatch), mean)
         
         jan2 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov")
         num1 <- length(unique(jan2$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan2 <- jan2[-1,]
         jan2 <- head(jan2, -(num1-1))
         jan2 <- cbind(jan2, num2)
         jan2 <- aggregate(jan2[, 2:16], list(jan2$yearmatch), mean)
         
         jan3 <- subset(gridmetmonthly, month == "jan"  | month == "dec"| month == "nov" | month == "oct")
         num1 <- length(unique(jan3$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan3 <- jan3[-1,]
         jan3 <- head(jan3, -(num1-1))
         jan3 <- cbind(jan3, num2)
         jan3 <- aggregate(jan3[, 2:16], list(jan3$yearmatch), mean)
         
         jan4 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         num1 <- length(unique(jan4$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan4 <- jan4[-1,]
         jan4 <- head(jan4, -(num1-1))
         jan4 <- cbind(jan4, num2)
         jan4 <- aggregate(jan4[, 2:16], list(jan4$yearmatch), mean)
         
         jan5 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         num1 <- length(unique(jan5$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan5 <- jan5[-1,]
         jan5 <- head(jan5, -(num1-1))
         jan5 <- cbind(jan5, num2)
         jan5 <- aggregate(jan5[, 2:16], list(jan5$yearmatch), mean)
         
         jan6 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         num1 <- length(unique(jan6$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan6 <- jan6[-1,]
         jan6 <- head(jan6, -(num1-1))
         jan6 <- cbind(jan6, num2)
         jan6 <- aggregate(jan6[, 2:16], list(jan6$yearmatch), mean)
         
         jan7 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         num1 <- length(unique(jan7$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan7 <- jan7[-1,]
         jan7 <- head(jan7, -(num1-1))
         jan7 <- cbind(jan7, num2)
         jan7 <- aggregate(jan7[, 2:16], list(jan7$yearmatch), mean)
         
         jan8 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         num1 <- length(unique(jan8$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan8 <- jan8[-1,]
         jan8 <- head(jan8, -(num1-1))
         jan8 <- cbind(jan8, num2)
         jan8 <- aggregate(jan8[, 2:16], list(jan8$yearmatch), mean)
         
         jan9 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         num1 <- length(unique(jan9$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan9 <- jan9[-1,]
         jan9 <- head(jan9, -(num1-1))
         jan9 <- cbind(jan9, num2)
         jan9 <- aggregate(jan9[, 2:16], list(jan9$yearmatch), mean)
         
         jan10 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         num1 <- length(unique(jan10$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan10 <- jan10[-1,]
         jan10 <- head(jan10, -(num1-1))
         jan10 <- cbind(jan10, num2)
         jan10 <- aggregate(jan10[, 2:16], list(jan10$yearmatch), mean)
         
         jan11 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         num1 <- length(unique(jan11$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan11 <- jan11[-1,]
         jan11 <- head(jan11, -(num1-1))
         jan11 <- cbind(jan11, num2)
         jan11 <- aggregate(jan11[, 2:16], list(jan11$yearmatch), mean)
         
         jan12 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" )
         monthnum <- 1
         num1 <- length(unique(jan12$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         jan12 <-tail(jan12, -monthnum)
         monthnum2 <- num1 - monthnum
         if (monthnum2 > 0) {jan12 <- head(jan12, -(monthnum2))} 
         jan12 <- cbind(jan12, num2)
         jan12 <- aggregate(jan12[, 2:16], list(jan12$yearmatch), mean)
         
         feb1 <- subset(gridmetmonthly, month == "feb"  | month == "jan")
         monthnum <- 2
         num1 <- length(unique(feb1$month))
         num2 <- as.data.frame(rep(ymll, each = num1))
         colnames(num2) <- "yearmatch"
         feb1 <-tail(feb1, -monthnum)
         monthnum2 <- num1 - monthnum
         if (monthnum2 > 0) {feb1 <- head(feb1, -(monthnum2))} 
         feb1 <- cbind(feb1, num2)
         feb1 <- aggregate(feb1[, 2:16], list(feb1$yearmatch), mean)
         
         feb2 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec")
       
      
         feb3 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov")
         jan1 <- jan1[-1,]
         jan1 <- jan2[-nrow(jan1),]
         jan1 <- cbind(jan1, yml)
         jan1 <- aggregate(jan1[, 2:16], list(jan1$yearmatch), mean)
         
         feb4 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         jan1 <- jan1[-1,]
         jan1 <- jan2[-nrow(jan1),]
         jan1 <- cbind(jan1, yml)
         jan1 <- aggregate(jan1[, 2:16], list(jan1$yearmatch), mean)
         
      
         feb5 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         feb5 <- mean(getElement(feb5, climvariable))
         feb6 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         feb6 <- mean(getElement(feb6, climvariable))
         feb7 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         feb7 <- mean(getElement(feb7, climvariable))
         feb8 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         feb8 <- mean(getElement(feb8, climvariable))
         feb9 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         feb9 <- mean(getElement(feb9, climvariable))
         feb10 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         feb10 <- mean(getElement(feb10, climvariable))
         feb11 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         feb11 <- mean(getElement(feb11, climvariable))
         feb12 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" )
         feb12 <- mean(getElement(feb12, climvariable))
         febfull <- c(feb1, feb2, feb3, feb4, feb5, feb6, feb7, feb8, feb9, feb10, feb11, feb12)
         
         mar1 <- subset(gridmetmonthly, month == "mar"  | month == "feb")
         mar1 <- mean(getElement(mar1, climvariable))
         mar2 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan")
         mar2 <- mean(getElement(mar2, climvariable))
         mar3 <- subset(gridmetmonthly, month == "mar"  | month == "feb"| month == "jan" | month == "dec")
         mar3 <- mean(getElement(mar3, climvariable))
         mar4 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         mar4 <- mean(getElement(mar4, climvariable))
         mar5 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         mar5 <- mean(getElement(mar5, climvariable))
         mar6 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         mar6 <- mean(getElement(mar6, climvariable))
         mar7 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         mar7 <- mean(getElement(mar7, climvariable))
         mar8 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         mar8 <- mean(getElement(mar8, climvariable))
         mar9 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         mar9 <- mean(getElement(mar9, climvariable))
         mar10 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         mar10 <- mean(getElement(mar10, climvariable))
         mar11 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         mar11 <- mean(getElement(mar11, climvariable))
         mar12 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" )
         mar12 <- mean(getElement(mar12, climvariable))
         marfull <- c(mar1, mar2, mar3, mar4, mar5, mar6, mar7, mar8, mar9, mar10, mar11, mar12)
         
         
         apr1 <- subset(gridmetmonthly, month == "apr"  | month == "mar")
         apr1 <- mean(getElement(apr1, climvariable))
         apr2 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb")
         apr2 <- mean(getElement(apr2, climvariable))
         apr3 <- subset(gridmetmonthly, month == "apr"  | month == "mar"| month == "feb" | month == "jan")
         apr3 <- mean(getElement(apr3, climvariable))
         apr4 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         apr4 <- mean(getElement(apr4, climvariable))
         apr5 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         apr5 <- mean(getElement(apr5, climvariable))
         apr6 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         apr6 <- mean(getElement(apr6, climvariable))
         apr7 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         apr7 <- mean(getElement(apr7, climvariable))
         apr8 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         apr8 <- mean(getElement(apr8, climvariable))
         apr9 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         apr9 <- mean(getElement(apr9, climvariable))
         apr10 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         apr10 <- mean(getElement(apr10, climvariable))
         apr11 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         apr11 <- mean(getElement(apr11, climvariable))
         apr12 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" )
         apr12 <- mean(getElement(apr12, climvariable))
         aprfull <- c(apr1, apr2, apr3, apr4, apr5, apr6, apr7, apr8, apr9, apr10, apr11, apr12)
         
         
         may1 <- subset(gridmetmonthly, month == "may"  | month == "apr")
         may1 <- mean(getElement(may1, climvariable))
         may2 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar")
         may2 <- mean(getElement(may2, climvariable))
         may3 <- subset(gridmetmonthly, month == "may"  | month == "apr"| month == "mar" | month == "feb")
         may3 <- mean(getElement(may3, climvariable))
         may4 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         may4 <- mean(getElement(may4, climvariable))
         may5 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         may5 <- mean(getElement(may5, climvariable))
         may6 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         may6 <- mean(getElement(may6, climvariable))
         may7 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         may7 <- mean(getElement(may7, climvariable))
         may8 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         may8 <- mean(getElement(may8, climvariable))
         may9 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         may9 <- mean(getElement(may9, climvariable))
         may10 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         may10 <- mean(getElement(may10, climvariable))
         may11 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         may11 <- mean(getElement(may11, climvariable))
         may12 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" )
         may12 <- mean(getElement(may12, climvariable))
         mayfull <- c(may1, may2, may3, may4, may5, may6, may7, may8, may9, may10, may11, may12)
         
         
         jun1 <- subset(gridmetmonthly, month == "jun"  | month == "may")
         jun1 <- mean(getElement(jun1, climvariable))
         jun2 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr")
         jun2 <- mean(getElement(jun2, climvariable))
         jun3 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar")
         jun3 <- mean(getElement(jun3, climvariable))
         jun4 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb")
         jun4 <- mean(getElement(jun4, climvariable))
         jun5 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         jun5 <- mean(getElement(jun5, climvariable))
         jun6 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         jun6 <- mean(getElement(jun6, climvariable))
         jun7 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         jun7 <- mean(getElement(jun7, climvariable))
         jun8 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         jun8 <- mean(getElement(jun8, climvariable))
         jun9 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         jun9 <- mean(getElement(jun9, climvariable))
         jun10 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         jun10 <- mean(getElement(jun10, climvariable))
         jun11 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         jun11 <- mean(getElement(jun11, climvariable))
         jun12 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" )
         jun12 <- mean(getElement(jun12, climvariable))
         junfull <- c(jun1, jun2, jun3, jun4, jun5, jun6, jun7, jun8, jun9, jun10, jun11, jun12)
         
         
         jul1 <- subset(gridmetmonthly, month == "jul"  | month == "jun")
         jul1 <- mean(getElement(jul1, climvariable))
         jul2 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may")
         jul2 <- mean(getElement(jul2, climvariable))
         jul3 <- subset(gridmetmonthly, month == "jul"  | month == "jun"| month == "may" | month == "apr")
         jul3 <- mean(getElement(jul3, climvariable))
         jul4 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar")
         jul4 <- mean(getElement(jul4, climvariable))
         jul5 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         jul5 <- mean(getElement(jul5, climvariable))
         jul6 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         jul6 <- mean(getElement(jul6, climvariable))
         jul7 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         jul7 <- mean(getElement(jul7, climvariable))
         jul8 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         jul8 <- mean(getElement(jul8, climvariable))
         jul9 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         jul9 <- mean(getElement(jul9, climvariable))
         jul10 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         jul10 <- mean(getElement(jul10, climvariable))
         jul11 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         jul11 <- mean(getElement(jul11, climvariable))
         jul12 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" )
         jul12 <- mean(getElement(jul12, climvariable))
         julfull <- c(jul1, jul2, jul3, jul4, jul5, jul6, jul7, jul8, jul9, jul10, jul11, jul12)
         
         aug1 <- subset(gridmetmonthly, month == "aug"  | month == "jul")
         aug1 <- mean(getElement(aug1, climvariable))
         aug2 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun")
         aug2 <- mean(getElement(aug2, climvariable))
         aug3 <- subset(gridmetmonthly, month == "aug"  | month == "jul"| month == "jun" | month == "may")
         aug3 <- mean(getElement(aug3, climvariable))
         aug4 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr")
         aug4 <- mean(getElement(aug4, climvariable))
         aug5 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         aug5 <- mean(getElement(aug5, climvariable))
         aug6 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         aug6 <- mean(getElement(aug6, climvariable))
         aug7 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         aug7 <- mean(getElement(aug7, climvariable))
         aug8 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         aug8 <- mean(getElement(aug8, climvariable))
         aug9 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         aug9 <- mean(getElement(aug9, climvariable))
         aug10 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         aug10 <- mean(getElement(aug10, climvariable))
         aug11 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         aug11 <- mean(getElement(aug11, climvariable))
         aug12 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" )
         aug12 <- mean(getElement(aug12, climvariable))
         augfull <- c(aug1, aug2, aug3, aug4, aug5, aug6, aug7, aug8, aug9, aug10, aug11, aug12)
         
         
         sep1 <- subset(gridmetmonthly, month == "sep"  | month == "aug")
         sep1 <- mean(getElement(sep1, climvariable))
         sep2 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul")
         sep2 <- mean(getElement(sep2, climvariable))
         sep3 <- subset(gridmetmonthly, month == "sep"  | month == "aug"| month == "jul" | month == "jun")
         sep3 <- mean(getElement(sep3, climvariable))
         sep4 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may")
         sep4 <- mean(getElement(sep4, climvariable))
         sep5 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         sep5 <- mean(getElement(sep5, climvariable))
         sep6 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         sep6 <- mean(getElement(sep6, climvariable))
         sep7 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         sep7 <- mean(getElement(sep7, climvariable))
         sep8 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         sep8 <- mean(getElement(sep8, climvariable))
         sep9 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         sep9 <- mean(getElement(sep9, climvariable))
         sep10 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         sep10 <- mean(getElement(sep10, climvariable))
         sep11 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         sep11 <- mean(getElement(sep11, climvariable))
         sep12 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" )
         sep12 <- mean(getElement(sep12, climvariable))
         sepfull <- c(sep1, sep2, sep3, sep4, sep5, sep6, sep7, sep8, sep9, sep10, sep11, sep12)
         
         
         oct1 <- subset(gridmetmonthly, month == "oct"  | month == "sep")
         oct1 <- mean(getElement(oct1, climvariable))
         oct2 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug")
         oct2 <- mean(getElement(oct2, climvariable))
         oct3 <- subset(gridmetmonthly, month == "oct"  | month == "sep"| month == "aug" | month == "jul")
         oct3 <- mean(getElement(oct3, climvariable))
         oct4 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         oct4 <- mean(getElement(oct4, climvariable))
         oct5 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         oct5 <- mean(getElement(oct5, climvariable))
         oct6 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         oct6 <- mean(getElement(oct6, climvariable))
         oct7 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         oct7 <- mean(getElement(oct7, climvariable))
         oct8 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         oct8 <- mean(getElement(oct8, climvariable))
         oct9 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         oct9 <- mean(getElement(oct9, climvariable))
         oct10 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         oct10 <- mean(getElement(oct10, climvariable))
         oct11 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         oct11 <- mean(getElement(oct11, climvariable))
         oct12 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" )
         oct12 <- mean(getElement(oct12, climvariable))
         octfull <- c(oct1, oct2, oct3, oct4, oct5, oct6, oct7, oct8, oct9, oct10, oct11, oct12)
         
         
         nov1 <- subset(gridmetmonthly, month == "nov"  | month == "oct")
         nov1 <- mean(getElement(nov1, climvariable))
         nov2 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep")
         nov2 <- mean(getElement(nov2, climvariable))
         nov3 <- subset(gridmetmonthly, month == "nov"  | month == "oct"| month == "sep" | month == "aug")
         nov3 <- mean(getElement(nov3, climvariable))
         nov4 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         nov4 <- mean(getElement(nov4, climvariable))
         nov5 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         nov5 <- mean(getElement(nov5, climvariable))
         nov6 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         nov6 <- mean(getElement(nov6, climvariable))
         nov7 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         nov7 <- mean(getElement(nov7, climvariable))
         nov8 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         nov8 <- mean(getElement(nov8, climvariable))
         nov9 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         nov9 <- mean(getElement(nov9, climvariable))
         nov10 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         nov10 <- mean(getElement(nov10, climvariable))
         nov11 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         nov11 <- mean(getElement(nov11, climvariable))
         nov12 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" )
         nov12 <- mean(getElement(nov12, climvariable))
         novfull <- c(nov1, nov2, nov3, nov4, nov5, nov6, nov7, nov8, nov8, nov10, nov11, nov12)
         
         
         dec1 <- subset(gridmetmonthly, month == "dec"  | month == "nov")
         dec1 <- mean(getElement(dec1, climvariable))
         dec2 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct")
         dec2 <- mean(getElement(dec2, climvariable))
         dec3 <- subset(gridmetmonthly, month == "dec"  | month == "nov"| month == "oct" | month == "sep")
         dec3 <- mean(getElement(dec3, climvariable))
         dec4 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         dec4 <- mean(getElement(dec4, climvariable))
         dec5 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         dec5 <- mean(getElement(dec5, climvariable))
         dec6 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         dec6 <- mean(getElement(dec6, climvariable))
         dec7 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         dec7 <- mean(getElement(dec7, climvariable))
         dec8 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         dec8 <- mean(getElement(dec8, climvariable))
         dec9 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         dec9 <- mean(getElement(dec9, climvariable))
         dec10 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         dec10 <- mean(getElement(dec10, climvariable))
         dec11 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         dec11 <- mean(getElement(dec11, climvariable))
         dec12 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" )
         dec12 <- mean(getElement(dec12, climvariable))
         decfull <- c(dec1, dec2, dec3, dec4, dec5, dec6, dec7, dec8, dec9, dec10, dec11, dec12)
         
         monthfull <- rbind(janfull, febfull, marfull, aprfull, mayfull, junfull, julfull, augfull, sepfull, octfull, novfull, decfull)
         monthfull2 <- monthfull[1:9,]
         rownames(monthfull2) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
         colnames(monthfull2) <- c(1:12)
         
         
         
          combined3 <- as.data.frame(rep(combined2$cube_acres[1], time = 9))
          colnames(combined3) <- "NASS_totalacres"
         
         x <- cbind(as.data.frame(monthfull2), as.data.frame(combined3))
         
     
         pairs(x)
    
    climmeanlongterm1989<- mean(longterm1989[,2])
    wheatdrought1989 <- subset(wheatdroughtclaim, year == 1989)
    wheatdroughtclaim_all1989 <- subset(wheatdroughtclaim_allall_final, year == 1989)
    wheatclaimlosssum1989 <- sum(wheatdrought1989$loss)
    wheatclaimacressum1989 <- sum(wheatdrought1989$acres)
    wheatclaimcounttotal1989 <- nrow(wheatdrought1989)/nrow(wheatdroughtclaim_all1989)
    wheatclaimcountsum1989 <- nrow(wheatdrought1989)
    
    
    longterm1990 <- gridmetmonthly[10:18,]
    shortterm1990 <- gridmetmonthly[15:18,]
    climmeanlongterm1990 <- mean(longterm1990[,2])
    climmeanshortterm1990 <- mean(shortterm1990[,2])
    wheatdrought1990 <- subset(wheatdroughtclaim, year == 1990)
    wheatdroughtclaim_all1990 <- subset(wheatdroughtclaim_allall_final, year == 1990)
    wheatclaimlosssum1990 <- sum(wheatdrought1990$loss)
    wheatclaimacressum1990 <- sum(wheatdrought1990$acres)
    wheatclaimcounttotal1990 <- nrow(wheatdrought1990)/nrow(wheatdroughtclaim_all1990)
    wheatclaimcountsum1990 <- nrow(wheatdrought1990)
    
    longterm1991<- gridmetmonthly[22:30,]
    shortterm1991 <- gridmetmonthly[27:30,]
    climmeanlongterm1991 <- mean(longterm1991[,2])
    climmeanshortterm1991 <- mean(shortterm1991[,2])
    wheatdrought1991 <- subset(wheatdroughtclaim, year == 1991)
    wheatdroughtclaim_all1991 <- subset(wheatdroughtclaim_allall_final, year == 1991)
    wheatclaimlosssum1991 <- sum(wheatdrought1991$loss)
    wheatclaimacressum1991 <- sum(wheatdrought1991$acres)
    wheatclaimcounttotal1991 <- nrow(wheatdrought1991)/nrow(wheatdroughtclaim_all1991)
    wheatclaimcountsum1991 <- nrow(wheatdrought1991)
    
    longterm1992 <- gridmetmonthly[34:42,]
    shortterm1992 <- gridmetmonthly[39:42,]
    climmeanlongterm1992 <- mean(longterm1992[,2])
    climmeanshortterm1992 <- mean(shortterm1992[,2])
    wheatdrought1992 <- subset(wheatdroughtclaim, year == 1992)
    wheatdroughtclaim_all1992 <- subset(wheatdroughtclaim_allall_final, year == 1992)
    wheatclaimlosssum1992 <- sum(wheatdrought1992$loss)
    wheatclaimacressum1992 <- sum(wheatdrought1992$acres)
    wheatclaimcounttotal1992 <- nrow(wheatdrought1992)/nrow(wheatdroughtclaim_all1992)
    wheatclaimcountsum1992 <- nrow(wheatdrought1992)
    
    longterm1993 <- gridmetmonthly[46:54,]
    shortterm1993 <- gridmetmonthly[51:54,]
    climmeanlongterm1993 <- mean(longterm1993[,2])
    climmeanshortterm1993 <- mean(shortterm1993[,2])
    wheatdrought1993 <- subset(wheatdroughtclaim, year == 1993)
    wheatdroughtclaim_all1993 <- subset(wheatdroughtclaim_allall_final, year == 1993)
    wheatclaimlosssum1993 <- sum(wheatdrought1993$loss)
    wheatclaimacressum1993 <- sum(wheatdrought1993$acres)
    wheatclaimcounttotal1993 <- nrow(wheatdrought1993)/nrow(wheatdroughtclaim_all1993)
    wheatclaimcountsum1993 <- nrow(wheatdrought1993)
    
    longterm1994 <- gridmetmonthly[58:66,]
    shortterm1994 <- gridmetmonthly[63:66,]
    climmeanlongterm1994 <- mean(longterm1994[,2])
    climmeanshortterm1994 <- mean(shortterm1994[,2])
    wheatdrought1994 <- subset(wheatdroughtclaim, year == 1994)
    wheatdroughtclaim_all1994 <- subset(wheatdroughtclaim_allall_final, year == 1994)
    wheatclaimlosssum1994 <- sum(wheatdrought1994$loss)
    wheatclaimacressum1994 <- sum(wheatdrought1994$acres)
    wheatclaimcounttotal1994 <- nrow(wheatdrought1994)/nrow(wheatdroughtclaim_all1994)
    wheatclaimcountsum1994 <- nrow(wheatdrought1994)
    
    longterm1995 <- gridmetmonthly[70:78,]
    shortterm1995 <- gridmetmonthly[75:78,]
    climmeanlongterm1995 <- mean(longterm1995[,2])
    climmeanshortterm1995 <- mean(shortterm1995[,2])
    wheatdrought1995 <- subset(wheatdroughtclaim, year == 1995)
    wheatdroughtclaim_all1995 <- subset(wheatdroughtclaim_allall_final, year == 1995)
    wheatclaimlosssum1995 <- sum(wheatdrought1995$loss)
    wheatclaimacressum1995 <- sum(wheatdrought1995$acres)
    wheatclaimcounttotal1995 <- nrow(wheatdrought1995)/nrow(wheatdroughtclaim_all1995)
    wheatclaimcountsum1995 <- nrow(wheatdrought1995)
    
    longterm1996 <- gridmetmonthly[82:90,]
    shortterm1996 <- gridmetmonthly[87:90,]
    climmeanlongterm1996 <- mean(longterm1996[,2])
    climmeanshortterm1996 <- mean(shortterm1996[,2])
    wheatdrought1996 <- subset(wheatdroughtclaim, year == 1996)
    wheatdroughtclaim_all1996 <- subset(wheatdroughtclaim_allall_final, year == 1996)
    wheatclaimlosssum1996 <- sum(wheatdrought1996$loss)
    wheatclaimacressum1996 <- sum(wheatdrought1996$acres)
    wheatclaimcounttotal1996 <- nrow(wheatdrought1996)/nrow(wheatdroughtclaim_all1996)
    wheatclaimcountsum1996 <- nrow(wheatdrought1996)
    
    longterm1997 <- gridmetmonthly[94:102,]
    shortterm1997  <- gridmetmonthly[99:102,]
    climmeanlongterm1997  <- mean(longterm1997[,2])
    climmeanshortterm1997  <- mean(shortterm1997[,2])
    wheatdrought1997  <- subset(wheatdroughtclaim, year == 1997)
    wheatdroughtclaim_all1997  <- subset(wheatdroughtclaim_allall_final, year == 1997)
    wheatclaimlosssum1997  <- sum(wheatdrought1997$loss)
    wheatclaimacressum1997 <- sum(wheatdrought1997$acres)
    wheatclaimcounttotal1997 <- nrow(wheatdrought1997)/nrow(wheatdroughtclaim_all1997)
    wheatclaimcountsum1997 <- nrow(wheatdrought1997)
    
    longterm1998 <- gridmetmonthly[106:114,]
    shortterm1998 <- gridmetmonthly[111:114,]
    climmeanlongterm1998 <- mean(longterm1998[,2])
    climmeanshortterm1998 <- mean(shortterm1998[,2])
    wheatdrought1998 <- subset(wheatdroughtclaim, year == 1998)
    wheatdroughtclaim_all1998 <- subset(wheatdroughtclaim_allall_final, year == 1998)
    wheatclaimlosssum1998 <- sum(wheatdrought1998$loss)
    wheatclaimacressum1998 <- sum(wheatdrought1998$acres)
    wheatclaimcounttotal1998 <- nrow(wheatdrought1998)/nrow(wheatdroughtclaim_all1998)
    wheatclaimcountsum1998 <- nrow(wheatdrought1998)
    
    longterm1999 <- gridmetmonthly[118:126,]
    shortterm1999 <- gridmetmonthly[123:126,]
    climmeanlongterm1999 <- mean(longterm1999[,2])
    climmeanshortterm1999 <- mean(shortterm1999[,2])
    wheatdrought1999 <- subset(wheatdroughtclaim, year == 1999)
    wheatdroughtclaim_all1999 <- subset(wheatdroughtclaim_allall_final, year == 1999)
    wheatclaimlosssum1999 <- sum(wheatdrought1999$loss)
    wheatclaimacressum1999 <- sum(wheatdrought1999$acres)
    wheatclaimcounttotal1999 <- nrow(wheatdrought1999)/nrow(wheatdroughtclaim_all1999)
    wheatclaimcountsum1999 <- nrow(wheatdrought1999)
    
    
    longterm2000 <- gridmetmonthly[130:138,]
    shortterm2000 <- gridmetmonthly[135:138,]
    climmeanlongterm2000 <- mean(longterm2000[,2])
    climmeanshortterm2000 <- mean(shortterm2000[,2])
    wheatdrought2000 <- subset(wheatdroughtclaim, year == 2000)
    wheatdroughtclaim_all2000 <- subset(wheatdroughtclaim_allall_final, year == 2000)
    wheatclaimlosssum2000 <- sum(wheatdrought2000$loss)
    wheatclaimacressum2000 <- sum(wheatdrought2000$acres)
    wheatclaimcounttotal2000 <- nrow(wheatdrought2000)/nrow(wheatdroughtclaim_all2000)
    wheatclaimcountsum2000 <- nrow(wheatdrought2000)
    
  
    
    #--
    
    longterm2001 <- gridmetmonthly[142:150,]
    shortterm2001 <- gridmetmonthly[147:150,]
    climmeanlongterm2001 <- mean(longterm2001[,2])
    climmeanshortterm2001 <- mean(shortterm2001[,2])
    wheatdrought2001 <- subset(wheatdroughtclaim, year == 2001)
    wheatdroughtclaim_all2001 <- subset(wheatdroughtclaim_allall_final, year == 2001)
    wheatclaimlosssum2001 <- sum(wheatdrought2001$loss)
    wheatclaimacressum2001 <- sum(wheatdrought2001$acres)
    wheatclaimcounttotal2001 <- nrow(wheatdrought2001)/nrow(wheatdroughtclaim_all2001)
    wheatclaimcountsum2001 <- nrow(wheatdrought2001)
    
    
    longterm2002 <- gridmetmonthly[154:162,]
    shortterm2002 <- gridmetmonthly[159:162,]
    climmeanlongterm2002 <- mean(longterm2002[,2])
    climmeanshortterm2002 <- mean(shortterm2002[,2])
    wheatdrought2002 <- subset(wheatdroughtclaim, year == 2002)
    wheatdroughtclaim_all2002 <- subset(wheatdroughtclaim_allall_final, year == 2002)
    wheatclaimlosssum2002 <- sum(wheatdrought2002$loss)
    wheatclaimacressum2002 <- sum(wheatdrought2002$acres)
    wheatclaimcounttotal2002 <- nrow(wheatdrought2002)/nrow(wheatdroughtclaim_all2002)
    wheatclaimcountsum2002 <- nrow(wheatdrought2002)
    
    longterm2003 <- gridmetmonthly[166:174,]
    shortterm2003 <- gridmetmonthly[171:174,]
    climmeanlongterm2003 <- mean(longterm2003[,2])
    climmeanshortterm2003 <- mean(shortterm2003[,2])
    wheatdrought2003 <- subset(wheatdroughtclaim, year == 2003)
    wheatdroughtclaim_all2003 <- subset(wheatdroughtclaim_allall_final, year == 2003)
    wheatclaimlosssum2003 <- sum(wheatdrought2003$loss)
    wheatclaimacressum2003 <- sum(wheatdrought2003$acres)
    wheatclaimcounttotal2003 <- nrow(wheatdrought2003)/nrow(wheatdroughtclaim_all2003)
    wheatclaimcountsum2003 <- nrow(wheatdrought2003)
    
    longterm2004 <- gridmetmonthly[178:186,]
    shortterm2004 <- gridmetmonthly[183:186,]
    climmeanlongterm2004 <- mean(longterm2004[,2])
    climmeanshortterm2004 <- mean(shortterm2004[,2])
    wheatdrought2004 <- subset(wheatdroughtclaim, year == 2004)
    wheatdroughtclaim_all2004 <- subset(wheatdroughtclaim_allall_final, year == 2004)
    wheatclaimlosssum2004 <- sum(wheatdrought2004$loss)
    wheatclaimacressum2004 <- sum(wheatdrought2004$acres)
    wheatclaimcounttotal2004 <- nrow(wheatdrought2004)/nrow(wheatdroughtclaim_all2004)
    wheatclaimcountsum2004 <- nrow(wheatdrought2004)
    
    longterm2005 <- gridmetmonthly[190:198,]
    shortterm2005 <- gridmetmonthly[195:198,]
    climmeanlongterm2005 <- mean(longterm2005[,2])
    climmeanshortterm2005 <- mean(shortterm2005[,2])
    wheatdrought2005 <- subset(wheatdroughtclaim, year == 2005)
    wheatdroughtclaim_all2005 <- subset(wheatdroughtclaim_allall_final, year == 2005)
    wheatclaimlosssum2005 <- sum(wheatdrought2005$loss)
    wheatclaimacressum2005 <- sum(wheatdrought2005$acres)
    wheatclaimcounttotal2005 <- nrow(wheatdrought2005)/nrow(wheatdroughtclaim_all2005)
    wheatclaimcountsum2005 <- nrow(wheatdrought2005)
    
    longterm2006 <- gridmetmonthly[202:210,]
    shortterm2006 <- gridmetmonthly[207:210,]
    climmeanlongterm2006 <- mean(longterm2006[,2])
    climmeanshortterm2006 <- mean(shortterm2006[,2])
    wheatdrought2006 <- subset(wheatdroughtclaim, year == 2006)
    wheatdroughtclaim_all2006 <- subset(wheatdroughtclaim_allall_final, year == 2006)
    wheatclaimlosssum2006 <- sum(wheatdrought2006$loss)
    wheatclaimacressum2006 <- sum(wheatdrought2006$acres)
    wheatclaimcounttotal2006 <- nrow(wheatdrought2006)/nrow(wheatdroughtclaim_all2006)
    wheatclaimcountsum2006 <- nrow(wheatdrought2006)
    
    longterm2007 <- gridmetmonthly[214:222,]
    shortterm2007 <- gridmetmonthly[219:222,]
    climmeanlongterm2007 <- mean(longterm2007[,2])
    climmeanshortterm2007 <- mean(shortterm2007[,2])
    wheatdrought2007 <- subset(wheatdroughtclaim, year == 2007)
    wheatdroughtclaim_all2007 <- subset(wheatdroughtclaim_allall_final, year == 2007)
    wheatclaimlosssum2007 <- sum(wheatdrought2007$loss)
    wheatclaimacressum2007 <- sum(wheatdrought2007$acres)
    wheatclaimcounttotal2007 <- nrow(wheatdrought2007)/nrow(wheatdroughtclaim_all2007)
    wheatclaimcountsum2007 <- nrow(wheatdrought2007)
    
    longterm2008 <- gridmetmonthly[226:234,]
    shortterm2008 <- gridmetmonthly[231:234,]
    climmeanlongterm2008 <- mean(longterm2008[,2])
    climmeanshortterm2008 <- mean(shortterm2008[,2])
    wheatdrought2008 <- subset(wheatdroughtclaim, year == 2008)
    wheatdroughtclaim_all2008 <- subset(wheatdroughtclaim_allall_final, year == 2008)
    wheatclaimlosssum2008 <- sum(wheatdrought2008$loss)
    wheatclaimacressum2008 <- sum(wheatdrought2008$acres)
    wheatclaimcounttotal2008 <- nrow(wheatdrought2008)/nrow(wheatdroughtclaim_all2008)
    wheatclaimcountsum2008 <- nrow(wheatdrought2008)
    
    longterm2009 <- gridmetmonthly[238:246,]
    shortterm2009 <- gridmetmonthly[243:246,]
    climmeanlongterm2009 <- mean(longterm2009[,2])
    climmeanshortterm2009 <- mean(shortterm2009[,2])
    wheatdrought2009 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2009 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2009 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2009 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2009 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2009)
    wheatclaimcountsum2009 <- nrow(wheatdrought2009)

    longterm2010 <- gridmetmonthly[250:258,]
    shortterm2010 <- gridmetmonthly[255:258,]
    climmeanlongterm2010 <- mean(longterm2010[,2])
    climmeanshortterm2010 <- mean(shortterm2010[,2])
    wheatdrought2010 <- subset(wheatdroughtclaim, year == 2010)
    wheatdroughtclaim_all2010 <- subset(wheatdroughtclaim_allall_final, year == 2010)
    wheatclaimlosssum2010 <- sum(wheatdrought2010$loss)
    wheatclaimacressum2010 <- sum(wheatdrought2010$acres)
    wheatclaimcounttotal2010 <- nrow(wheatdrought2010)/nrow(wheatdroughtclaim_all2010)
    wheatclaimcountsum2010 <- nrow(wheatdrought2010)
    
    longterm2011 <- gridmetmonthly[262:270,]
    shortterm2011 <- gridmetmonthly[267:270,]
    climmeanlongterm2011 <- mean(longterm2011[,2])
    climmeanshortterm2011 <- mean(shortterm2011[,2])
    wheatdrought2011 <- subset(wheatdroughtclaim, year == 2011)
    wheatdroughtclaim_all2011 <- subset(wheatdroughtclaim_allall_final, year == 2011)
    wheatclaimlosssum2011 <- sum(wheatdrought2011$loss)
    wheatclaimacressum2011 <- sum(wheatdrought2011$acres)
    wheatclaimcounttotal2011 <- nrow(wheatdrought2011)/nrow(wheatdroughtclaim_all2011)
    wheatclaimcountsum2011 <- nrow(wheatdrought2011)
    
    
    longterm2012 <- gridmetmonthly[274:282,]
    shortterm2012 <- gridmetmonthly[279:282,]
    climmeanlongterm2012 <- mean(longterm2012[,2])
    climmeanshortterm2012 <- mean(shortterm2012[,2])
    wheatdrought2012 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2012 <- subset(wheatdroughtclaim_allall_final, year == 2012)
    wheatclaimlosssum2012 <- sum(wheatdrought2012$loss)
    wheatclaimacressum2012 <- sum(wheatdrought2012$acres)
    wheatclaimcounttotal2012 <- nrow(wheatdrought2012)/nrow(wheatdroughtclaim_all2012)
    wheatclaimcountsum2012 <- nrow(wheatdrought2012)
    
    longterm2013 <- gridmetmonthly[286:294,]
    shortterm2013 <- gridmetmonthly[291:294,]
    climmeanlongterm2013 <- mean(longterm2013[,2])
    climmeanshortterm2013 <- mean(shortterm2013[,2])
    wheatdrought2013 <- subset(wheatdroughtclaim, year == 2013)
    wheatdroughtclaim_all2013 <- subset(wheatdroughtclaim_allall_final, year == 2013)
    wheatclaimlosssum2013 <- sum(wheatdrought2013$loss)
    wheatclaimacressum2013 <- sum(wheatdrought2013$acres)
    wheatclaimcounttotal2013 <- nrow(wheatdrought2013)/nrow(wheatdroughtclaim_all2013)
    wheatclaimcountsum2013 <- nrow(wheatdrought2013)
    
    longterm2014 <- gridmetmonthly[298:306,]
    shortterm2014 <- gridmetmonthly[303:306,]
    climmeanlongterm2014 <- mean(longterm2014[,2])
    climmeanshortterm2014 <- mean(shortterm2014[,2])
    wheatdrought2014 <- subset(wheatdroughtclaim, year == 2014)
    wheatdroughtclaim_all2014 <- subset(wheatdroughtclaim_allall_final, year == 2014)
    wheatclaimlosssum2014 <- sum(wheatdrought2014$loss)
    wheatclaimacressum2014 <- sum(wheatdrought2014$acres)
    wheatclaimcounttotal2014 <- nrow(wheatdrought2014)/nrow(wheatdroughtclaim_all2014)
    wheatclaimcountsum2014 <- nrow(wheatdrought2014)
    
    longterm2015 <- gridmetmonthly[310:318,]
    shortterm2015 <- gridmetmonthly[315:318,]
    climmeanlongterm2015 <- mean(longterm2015[,2])
    climmeanshortterm2015 <- mean(shortterm2015[,2])
    wheatdrought2015 <- subset(wheatdroughtclaim, year == 2015)
    wheatdroughtclaim_all2015 <- subset(wheatdroughtclaim_allall_final, year == 2015)
    wheatclaimlosssum2015 <- sum(wheatdrought2015$loss)
    wheatclaimacressum2015 <- sum(wheatdrought2015$acres)
    wheatclaimcounttotal2015 <- nrow(wheatdrought2015)/nrow(wheatdroughtclaim_all2015)
    wheatclaimcountsum2015 <- nrow(wheatdrought2015)
    
    
    wls <- rbind(wheatclaimlosssum1989, wheatclaimlosssum1990, wheatclaimlosssum1991, wheatclaimlosssum1992, wheatclaimlosssum1993, wheatclaimlosssum1994, wheatclaimlosssum1995, wheatclaimlosssum1996, wheatclaimlosssum1997, wheatclaimlosssum1998, wheatclaimlosssum1999, wheatclaimlosssum2000, wheatclaimlosssum2001, wheatclaimlosssum2002, wheatclaimlosssum2003, wheatclaimlosssum2004, wheatclaimlosssum2005,wheatclaimlosssum2006, wheatclaimlosssum2007, wheatclaimlosssum2008, wheatclaimlosssum2009,wheatclaimlosssum2010,wheatclaimlosssum2011,wheatclaimlosssum2012,wheatclaimlosssum2013,wheatclaimlosssum2014,wheatclaimlosssum2015)
    wla <- rbind(wheatclaimacressum1989, wheatclaimacressum1990, wheatclaimacressum1991, wheatclaimacressum1992, wheatclaimacressum1993, wheatclaimacressum1994, wheatclaimacressum1995, wheatclaimacressum1996, wheatclaimacressum1997, wheatclaimacressum1998, wheatclaimacressum1999, wheatclaimacressum2000, wheatclaimacressum2001, wheatclaimacressum2002,wheatclaimacressum2003,wheatclaimacressum2004, wheatclaimacressum2005, wheatclaimacressum2006, wheatclaimacressum2007, wheatclaimacressum2008,wheatclaimacressum2009,wheatclaimacressum2010,wheatclaimacressum2011,wheatclaimacressum2012,wheatclaimacressum2013,wheatclaimacressum2014,wheatclaimacressum2015)
    wlc <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    climmeanlongterm1 <- rbind(climmeanlongterm1989, climmeanlongterm1990, climmeanlongterm1991, climmeanlongterm1992, climmeanlongterm1993, climmeanlongterm1994, climmeanlongterm1995, climmeanlongterm1996, climmeanlongterm1997, climmeanlongterm1998, climmeanlongterm1999, climmeanlongterm2000, climmeanlongterm2001, climmeanlongterm2002, climmeanlongterm2003, climmeanlongterm2004, climmeanlongterm2005, climmeanlongterm2006, climmeanlongterm2007,climmeanlongterm2008,climmeanlongterm2009,climmeanlongterm2010,climmeanlongterm2011,climmeanlongterm2012,climmeanlongterm2013,climmeanlongterm2014,climmeanlongterm2015)
    climmeanshortterm1 <- rbind(climmeanshortterm1989, climmeanshortterm1990, climmeanshortterm1991, climmeanshortterm1992, climmeanshortterm1993, climmeanshortterm1994, climmeanshortterm1995, climmeanshortterm1996, climmeanshortterm1997, climmeanshortterm1998, climmeanshortterm1999, climmeanshortterm2000, climmeanshortterm2001, climmeanshortterm2002, climmeanshortterm2003, climmeanshortterm2004, climmeanshortterm2005, climmeanshortterm2006, climmeanshortterm2007,climmeanshortterm2008,climmeanshortterm2009,climmeanshortterm2010,climmeanshortterm2011,climmeanshortterm2012,climmeanshortterm2013,climmeanshortterm2014,climmeanshortterm2015)
    wheatclaimcounttotal <- rbind(wheatclaimcounttotal1989, wheatclaimcounttotal1990, wheatclaimcounttotal1991, wheatclaimcounttotal1992, wheatclaimcounttotal1993, wheatclaimcounttotal1994, wheatclaimcounttotal1995, wheatclaimcounttotal1996, wheatclaimcounttotal1997, wheatclaimcounttotal1998, wheatclaimcounttotal1999, wheatclaimcounttotal2000, wheatclaimcounttotal2001, wheatclaimcounttotal2002, wheatclaimcounttotal2003, wheatclaimcounttotal2004, wheatclaimcounttotal2005,  wheatclaimcounttotal2006, wheatclaimcounttotal2007, wheatclaimcounttotal2008,wheatclaimcounttotal2009, wheatclaimcounttotal2010, wheatclaimcounttotal2011, wheatclaimcounttotal2012, wheatclaimcounttotal2013, wheatclaimcounttotal2014, wheatclaimcounttotal2015)
    wheatclaimcountsumtotal <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    
    finalz <- cbind(climmeanlongterm1,wls, wla, wlc, wheatclaimcounttotal, climmeanshortterm1)
    finalz <- data.frame(finalz)
    names(finalz)[1] <- c("soil_moisture_longterm")
    names(finalz)[2] <- c("loss")
    names(finalz)[3] <- c("acres")
    names(finalz)[4] <- c("count")
    names(finalz)[5] <- c("countratio")
    names(finalz)[6] <- c("soil_moisture_shorterm")
    
    rownames(finalz) <- c(1989:2015)
    finalz[7]<- c(1989:2015)
    finalz[8] <- c(pu)
    names(finalz)[7] <- c("year")
    names(finalz)[8] <- c("county")
    finalz[9] <- q
    finalz[10] <- m
    names(finalz)[10] <- c("damagecause")
    names(finalz)[9] <- c("state")
    
    finalz_soil <- finalz
    
    ###--------
    
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3")
    files  <- list.files(pattern = '\\_palouse_summary$')
    tables <- lapply(files, read.csv, header = TRUE)
    combined.df <- do.call(rbind , tables)
    #----sums <- read.csv(paste(kk, "_", i, "_palouse_summary", sep=""))
    
    
    
    #---construct a county fips name file
    library(maps)
    data(county.fips)
    colnames(combined.df)[16] <- "fips"
    library(stringr)
    county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
    colnames(county.fips2) <- c("state", "county")
    county.fips3 <- cbind(county.fips, county.fips2)
    combined1.df <- merge(combined.df,county.fips3, by = 'fips')
    
    #--create unique values for each county grouping for palouse region
    lister <- unique(combined1.df$county)
    listerstate <- unique(combined1.df$state)
    listeridaho<- paste(lister[1:7], "_", listerstate[1], sep="")
    listeroregon <- paste(lister[8:14], "_", listerstate[2], sep="")
    listerwashington <- paste(lister[15:26], "_", listerstate[3], sep="")
    listercomb <- c(listeridaho, listeroregon, listerwashington)
    
    capFirst <- function(s) {
      paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    }
    
    
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
    p <- tolower(p)
    countyz <- tolower(countyz)
    #statez <- toupper(statez)
    
    files  <- list.files(pattern = '\\_palouse_summary$')
    tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
    gridmetmonthly <- do.call(rbind , tables)
    
    gridmetmonthly <- data.frame(gridmetmonthly)
    
    library(maps)
    data(county.fips)
    colnames(gridmetmonthly)[16] <- "fips"
    library(stringr)
    county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
    colnames(county.fips2) <- c("state", "county")
    county.fips3 <- cbind(county.fips, county.fips2)
    gridmetmonthly <- merge(gridmetmonthly,county.fips3, by = 'fips')
    
    
    #statez = simpleCap(statez)
    #countyz = simpleCap(countyz)
    #statez = state.abb[grep(statez, state.name)]
    gridmetmonthly <- subset(gridmetmonthly, state == statez)
    gridmetmonthly <- subset(gridmetmonthly, county == countyz)
    
    gridmetmonthly$monthchar <- as.character(gridmetmonthly$month)
    
    library(Hmisc)
    gridmetmonthly$monthchar <- capitalize(gridmetmonthly$monthchar)
    
    gridmetmonthly$monthchar <- factor(gridmetmonthly$monthchar, levels=month.abb)
    #gridmetmonthly$monthchar <- as.numeric(gridmetmonthly$monthchar)
    
    gridmetmonthly <- gridmetmonthly[order(gridmetmonthly[,18], gridmetmonthly[,22]),]
    
    #gridmetmonthly$monthyear <- paste(as.numeric(gridmetmonthly$monthchar), ".", gridmetmonthly$year, sep="")
    gridmetmonthly$ID<-seq.int(nrow(gridmetmonthly))
    
    #---05.18.17 need to create loop thru all claims and assign short term and long term drought variables.  mar 2009 is go back 3 and 6.  June 2009 is go back 6 and 9
    
    
    
    
    
    #usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    #usda <- read.csv(usda, header=FALSE, sep="|")
    #usda <- data.frame(usda)
    #gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    #colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
    #usda$county <- trimws(usda$county)
    listersplit <- unlist(strsplit(p, "[_]"))
    countyz <- listersplit[1]
    countyz <- simpleCap(countyz)
    
    statez <- listersplit[2]
    #countyz <- capitalize(countyz)
    statez1 <- stateFromLower(statez)
    statez1 <- as.vector(statez1)
    #usda_state <- subset(usda, state == statez)
    #usda_county <- subset(usda, county == countyz)
    #usda_county$ID<-seq.int(nrow(usda_county))
    
    #--
    
    longterm1989 <- gridmetmonthly[1:6,]
    shortterm1989 <- gridmetmonthly[3:6,]
    climmeanlongterm1989<- colMeans(longterm1989[,3:16])
    climmeanshortterm1989 <- colMeans(shortterm1989[,3:16])
    wheatdrought1989 <- subset(wheatdroughtclaim, year == 1989)
    wheatdroughtclaim_all1989 <- subset(wheatdroughtclaim_allall_final, year == 1989)
    wheatclaimlosssum1989 <- sum(wheatdrought1989$loss)
    wheatclaimacressum1989 <- sum(wheatdrought1989$acres)
    wheatclaimcounttotal1989 <- nrow(wheatdrought1989)/nrow(wheatdroughtclaim_all1989)
    wheatclaimcountsum1989 <- nrow(wheatdrought1989)
    
    
    longterm1990 <- gridmetmonthly[10:18,]
    shortterm1990 <- gridmetmonthly[15:18,]
    climmeanlongterm1990 <- colMeans(longterm1990[,3:16])
    climmeanshortterm1990 <- colMeans(shortterm1990[,3:16])
    wheatdrought1990 <- subset(wheatdroughtclaim, year == 1990)
    wheatdroughtclaim_all1990 <- subset(wheatdroughtclaim_allall_final, year == 1990)
    wheatclaimlosssum1990 <- sum(wheatdrought1990$loss)
    wheatclaimacressum1990 <- sum(wheatdrought1990$acres)
    wheatclaimcounttotal1990 <- nrow(wheatdrought1990)/nrow(wheatdroughtclaim_all1990)
    wheatclaimcountsum1990 <- nrow(wheatdrought1990)
    
    longterm1991<- gridmetmonthly[22:30,]
    shortterm1991 <- gridmetmonthly[27:30,]
    climmeanlongterm1991 <- colMeans(longterm1991[,3:16])
    climmeanshortterm1991 <- colMeans(shortterm1991[,3:16])
    wheatdrought1991 <- subset(wheatdroughtclaim, year == 1991)
    wheatdroughtclaim_all1991 <- subset(wheatdroughtclaim_allall_final, year == 1991)
    wheatclaimlosssum1991 <- sum(wheatdrought1991$loss)
    wheatclaimacressum1991 <- sum(wheatdrought1991$acres)
    wheatclaimcounttotal1991 <- nrow(wheatdrought1991)/nrow(wheatdroughtclaim_all1991)
    wheatclaimcountsum1991 <- nrow(wheatdrought1991)
    
    longterm1992 <- gridmetmonthly[34:42,]
    shortterm1992 <- gridmetmonthly[39:42,]
    climmeanlongterm1992 <- colMeans(longterm1992[,3:16])
    climmeanshortterm1992 <- colMeans(shortterm1992[,3:16])
    wheatdrought1992 <- subset(wheatdroughtclaim, year == 1992)
    wheatdroughtclaim_all1992 <- subset(wheatdroughtclaim_allall_final, year == 1992)
    wheatclaimlosssum1992 <- sum(wheatdrought1992$loss)
    wheatclaimacressum1992 <- sum(wheatdrought1992$acres)
    wheatclaimcounttotal1992 <- nrow(wheatdrought1992)/nrow(wheatdroughtclaim_all1992)
    wheatclaimcountsum1992 <- nrow(wheatdrought1992)
    
    longterm1993 <- gridmetmonthly[46:54,]
    shortterm1993 <- gridmetmonthly[51:54,]
    climmeanlongterm1993 <- colMeans(longterm1993[,3:16])
    climmeanshortterm1993 <- colMeans(shortterm1993[,3:16])
    wheatdrought1993 <- subset(wheatdroughtclaim, year == 1993)
    wheatdroughtclaim_all1993 <- subset(wheatdroughtclaim_allall_final, year == 1993)
    wheatclaimlosssum1993 <- sum(wheatdrought1993$loss)
    wheatclaimacressum1993 <- sum(wheatdrought1993$acres)
    wheatclaimcounttotal1993 <- nrow(wheatdrought1993)/nrow(wheatdroughtclaim_all1993)
    wheatclaimcountsum1993 <- nrow(wheatdrought1993)
    
    longterm1994 <- gridmetmonthly[58:66,]
    shortterm1994 <- gridmetmonthly[63:66,]
    climmeanlongterm1994 <- colMeans(longterm1994[,3:16])
    climmeanshortterm1994 <- colMeans(shortterm1994[,3:16])
    wheatdrought1994 <- subset(wheatdroughtclaim, year == 1994)
    wheatdroughtclaim_all1994 <- subset(wheatdroughtclaim_allall_final, year == 1994)
    wheatclaimlosssum1994 <- sum(wheatdrought1994$loss)
    wheatclaimacressum1994 <- sum(wheatdrought1994$acres)
    wheatclaimcounttotal1994 <- nrow(wheatdrought1994)/nrow(wheatdroughtclaim_all1994)
    wheatclaimcountsum1994 <- nrow(wheatdrought1994)
    
    longterm1995 <- gridmetmonthly[70:78,]
    shortterm1995 <- gridmetmonthly[75:78,]
    climmeanlongterm1995 <- colMeans(longterm1995[,3:16])
    climmeanshortterm1995 <- colMeans(shortterm1995[,3:16])
    wheatdrought1995 <- subset(wheatdroughtclaim, year == 1995)
    wheatdroughtclaim_all1995 <- subset(wheatdroughtclaim_allall_final, year == 1995)
    wheatclaimlosssum1995 <- sum(wheatdrought1995$loss)
    wheatclaimacressum1995 <- sum(wheatdrought1995$acres)
    wheatclaimcounttotal1995 <- nrow(wheatdrought1995)/nrow(wheatdroughtclaim_all1995)
    wheatclaimcountsum1995 <- nrow(wheatdrought1995)
    
    longterm1996 <- gridmetmonthly[82:90,]
    shortterm1996 <- gridmetmonthly[87:90,]
    climmeanlongterm1996 <- colMeans(longterm1996[,3:16])
    climmeanshortterm1996 <- colMeans(shortterm1996[,3:16])
    wheatdrought1996 <- subset(wheatdroughtclaim, year == 1996)
    wheatdroughtclaim_all1996 <- subset(wheatdroughtclaim_allall_final, year == 1996)
    wheatclaimlosssum1996 <- sum(wheatdrought1996$loss)
    wheatclaimacressum1996 <- sum(wheatdrought1996$acres)
    wheatclaimcounttotal1996 <- nrow(wheatdrought1996)/nrow(wheatdroughtclaim_all1996)
    wheatclaimcountsum1996 <- nrow(wheatdrought1996)
    
    longterm1997 <- gridmetmonthly[94:102,]
    shortterm1997  <- gridmetmonthly[99:102,]
    climmeanlongterm1997  <- colMeans(longterm1997[,3:16])
    climmeanshortterm1997  <- colMeans(shortterm1997[,3:16])
    wheatdrought1997  <- subset(wheatdroughtclaim, year == 1997)
    wheatdroughtclaim_all1997  <- subset(wheatdroughtclaim_allall_final, year == 1997)
    wheatclaimlosssum1997  <- sum(wheatdrought1997$loss)
    wheatclaimacressum1997 <- sum(wheatdrought1997$acres)
    wheatclaimcounttotal1997 <- nrow(wheatdrought1997)/nrow(wheatdroughtclaim_all1997)
    wheatclaimcountsum1997 <- nrow(wheatdrought1997)
    
    longterm1998 <- gridmetmonthly[106:114,]
    shortterm1998 <- gridmetmonthly[111:114,]
    climmeanlongterm1998 <- colMeans(longterm1998[,3:16])
    climmeanshortterm1998 <- colMeans(shortterm1998[,3:16])
    wheatdrought1998 <- subset(wheatdroughtclaim, year == 1998)
    wheatdroughtclaim_all1998 <- subset(wheatdroughtclaim_allall_final, year == 1998)
    wheatclaimlosssum1998 <- sum(wheatdrought1998$loss)
    wheatclaimacressum1998 <- sum(wheatdrought1998$acres)
    wheatclaimcounttotal1998 <- nrow(wheatdrought1998)/nrow(wheatdroughtclaim_all1998)
    wheatclaimcountsum1998 <- nrow(wheatdrought1998)
    
    longterm1999 <- gridmetmonthly[118:126,]
    shortterm1999 <- gridmetmonthly[123:126,]
    climmeanlongterm1999 <- colMeans(longterm1999[,3:16])
    climmeanshortterm1999 <- colMeans(shortterm1999[,3:16])
    wheatdrought1999 <- subset(wheatdroughtclaim, year == 1999)
    wheatdroughtclaim_all1999 <- subset(wheatdroughtclaim_allall_final, year == 1999)
    wheatclaimlosssum1999 <- sum(wheatdrought1999$loss)
    wheatclaimacressum1999 <- sum(wheatdrought1999$acres)
    wheatclaimcounttotal1999 <- nrow(wheatdrought1999)/nrow(wheatdroughtclaim_all1999)
    wheatclaimcountsum1999 <- nrow(wheatdrought1999)
    
    
    longterm2000 <- gridmetmonthly[130:138,]
    shortterm2000 <- gridmetmonthly[135:138,]
    climmeanlongterm2000 <- colMeans(longterm2000[,3:16])
    climmeanshortterm2000 <- colMeans(shortterm2000[,3:16])
    wheatdrought2000 <- subset(wheatdroughtclaim, year == 2000)
    wheatdroughtclaim_all2000 <- subset(wheatdroughtclaim_allall_final, year == 2000)
    wheatclaimlosssum2000 <- sum(wheatdrought2000$loss)
    wheatclaimacressum2000 <- sum(wheatdrought2000$acres)
    wheatclaimcounttotal2000 <- nrow(wheatdrought2000)/nrow(wheatdroughtclaim_all2000)
    wheatclaimcountsum2000 <- nrow(wheatdrought2000)
    
    
    
    #--
    
    longterm2001 <- gridmetmonthly[142:150,]
    shortterm2001 <- gridmetmonthly[147:150,]
    climmeanlongterm2001 <- colMeans(longterm2001[,3:16])
    climmeanshortterm2001 <- colMeans(shortterm2001[,3:16])
    wheatdrought2001 <- subset(wheatdroughtclaim, year == 2001)
    wheatdroughtclaim_all2001 <- subset(wheatdroughtclaim_allall_final, year == 2001)
    wheatclaimlosssum2001 <- sum(wheatdrought2001$loss)
    wheatclaimacressum2001 <- sum(wheatdrought2001$acres)
    wheatclaimcounttotal2001 <- nrow(wheatdrought2001)/nrow(wheatdroughtclaim_all2001)
    wheatclaimcountsum2001 <- nrow(wheatdrought2001)
    
    
    longterm2002 <- gridmetmonthly[154:162,]
    shortterm2002 <- gridmetmonthly[159:162,]
    climmeanlongterm2002 <- colMeans(longterm2002[,3:16])
    climmeanshortterm2002 <- colMeans(shortterm2002[,3:16])
    wheatdrought2002 <- subset(wheatdroughtclaim, year == 2002)
    wheatdroughtclaim_all2002 <- subset(wheatdroughtclaim_allall_final, year == 2002)
    wheatclaimlosssum2002 <- sum(wheatdrought2002$loss)
    wheatclaimacressum2002 <- sum(wheatdrought2002$acres)
    wheatclaimcounttotal2002 <- nrow(wheatdrought2002)/nrow(wheatdroughtclaim_all2002)
    wheatclaimcountsum2002 <- nrow(wheatdrought2002)
    
    longterm2003 <- gridmetmonthly[166:174,]
    shortterm2003 <- gridmetmonthly[171:174,]
    climmeanlongterm2003 <- colMeans(longterm2003[,3:16])
    climmeanshortterm2003 <- colMeans(shortterm2003[,3:16])
    wheatdrought2003 <- subset(wheatdroughtclaim, year == 2003)
    wheatdroughtclaim_all2003 <- subset(wheatdroughtclaim_allall_final, year == 2003)
    wheatclaimlosssum2003 <- sum(wheatdrought2003$loss)
    wheatclaimacressum2003 <- sum(wheatdrought2003$acres)
    wheatclaimcounttotal2003 <- nrow(wheatdrought2003)/nrow(wheatdroughtclaim_all2003)
    wheatclaimcountsum2003 <- nrow(wheatdrought2003)
    
    longterm2004 <- gridmetmonthly[178:186,]
    shortterm2004 <- gridmetmonthly[183:186,]
    climmeanlongterm2004 <- colMeans(longterm2004[,3:16])
    climmeanshortterm2004 <- colMeans(shortterm2004[,3:16])
    wheatdrought2004 <- subset(wheatdroughtclaim, year == 2004)
    wheatdroughtclaim_all2004 <- subset(wheatdroughtclaim_allall_final, year == 2004)
    wheatclaimlosssum2004 <- sum(wheatdrought2004$loss)
    wheatclaimacressum2004 <- sum(wheatdrought2004$acres)
    wheatclaimcounttotal2004 <- nrow(wheatdrought2004)/nrow(wheatdroughtclaim_all2004)
    wheatclaimcountsum2004 <- nrow(wheatdrought2004)
    
    longterm2005 <- gridmetmonthly[190:198,]
    shortterm2005 <- gridmetmonthly[195:198,]
    climmeanlongterm2005 <- colMeans(longterm2005[,3:16])
    climmeanshortterm2005 <- colMeans(shortterm2005[,3:16])
    wheatdrought2005 <- subset(wheatdroughtclaim, year == 2005)
    wheatdroughtclaim_all2005 <- subset(wheatdroughtclaim_allall_final, year == 2005)
    wheatclaimlosssum2005 <- sum(wheatdrought2005$loss)
    wheatclaimacressum2005 <- sum(wheatdrought2005$acres)
    wheatclaimcounttotal2005 <- nrow(wheatdrought2005)/nrow(wheatdroughtclaim_all2005)
    wheatclaimcountsum2005 <- nrow(wheatdrought2005)
    
    longterm2006 <- gridmetmonthly[202:210,]
    shortterm2006 <- gridmetmonthly[207:210,]
    climmeanlongterm2006 <- colMeans(longterm2006[,3:16])
    climmeanshortterm2006 <- colMeans(shortterm2006[,3:16])
    wheatdrought2006 <- subset(wheatdroughtclaim, year == 2006)
    wheatdroughtclaim_all2006 <- subset(wheatdroughtclaim_allall_final, year == 2006)
    wheatclaimlosssum2006 <- sum(wheatdrought2006$loss)
    wheatclaimacressum2006 <- sum(wheatdrought2006$acres)
    wheatclaimcounttotal2006 <- nrow(wheatdrought2006)/nrow(wheatdroughtclaim_all2006)
    wheatclaimcountsum2006 <- nrow(wheatdrought2006)
    
    longterm2007 <- gridmetmonthly[214:222,]
    shortterm2007 <- gridmetmonthly[219:222,]
    climmeanlongterm2007 <- colMeans(longterm2007[,3:16])
    climmeanshortterm2007 <- colMeans(shortterm2007[,3:16])
    wheatdrought2007 <- subset(wheatdroughtclaim, year == 2007)
    wheatdroughtclaim_all2007 <- subset(wheatdroughtclaim_allall_final, year == 2007)
    wheatclaimlosssum2007 <- sum(wheatdrought2007$loss)
    wheatclaimacressum2007 <- sum(wheatdrought2007$acres)
    wheatclaimcounttotal2007 <- nrow(wheatdrought2007)/nrow(wheatdroughtclaim_all2007)
    wheatclaimcountsum2007 <- nrow(wheatdrought2007)
    
    longterm2008 <- gridmetmonthly[226:234,]
    shortterm2008 <- gridmetmonthly[231:234,]
    climmeanlongterm2008 <- colMeans(longterm2008[,3:16])
    climmeanshortterm2008 <- colMeans(shortterm2008[,3:16])
    wheatdrought2008 <- subset(wheatdroughtclaim, year == 2008)
    wheatdroughtclaim_all2008 <- subset(wheatdroughtclaim_allall_final, year == 2008)
    wheatclaimlosssum2008 <- sum(wheatdrought2008$loss)
    wheatclaimacressum2008 <- sum(wheatdrought2008$acres)
    wheatclaimcounttotal2008 <- nrow(wheatdrought2008)/nrow(wheatdroughtclaim_all2008)
    wheatclaimcountsum2008 <- nrow(wheatdrought2008)
    
    longterm2009 <- gridmetmonthly[238:246,]
    shortterm2009 <- gridmetmonthly[243:246,]
    climmeanlongterm2009 <- colMeans(longterm2009[,3:16])
    climmeanshortterm2009 <- colMeans(shortterm2009[,3:16])
    wheatdrought2009 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2009 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2009 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2009 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2009 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2009)
    wheatclaimcountsum2009 <- nrow(wheatdrought2009)
    
    longterm2010 <- gridmetmonthly[250:258,]
    shortterm2010 <- gridmetmonthly[255:258,]
    climmeanlongterm2010 <- colMeans(longterm2010[,3:16])
    climmeanshortterm2010 <- colMeans(shortterm2010[,3:16])
    wheatdrought2010 <- subset(wheatdroughtclaim, year == 2010)
    wheatdroughtclaim_all2010 <- subset(wheatdroughtclaim_allall_final, year == 2010)
    wheatclaimlosssum2010 <- sum(wheatdrought2010$loss)
    wheatclaimacressum2010 <- sum(wheatdrought2010$acres)
    wheatclaimcounttotal2010 <- nrow(wheatdrought2010)/nrow(wheatdroughtclaim_all2010)
    wheatclaimcountsum2010 <- nrow(wheatdrought2010)
    
    longterm2011 <- gridmetmonthly[262:270,]
    shortterm2011 <- gridmetmonthly[267:270,]
    climmeanlongterm2011 <- colMeans(longterm2011[,3:16])
    climmeanshortterm2011 <- colMeans(shortterm2011[,3:16])
    wheatdrought2011 <- subset(wheatdroughtclaim, year == 2011)
    wheatdroughtclaim_all2011 <- subset(wheatdroughtclaim_allall_final, year == 2011)
    wheatclaimlosssum2011 <- sum(wheatdrought2011$loss)
    wheatclaimacressum2011 <- sum(wheatdrought2011$acres)
    wheatclaimcounttotal2011 <- nrow(wheatdrought2011)/nrow(wheatdroughtclaim_all2011)
    wheatclaimcountsum2011 <- nrow(wheatdrought2011)
    
    
    longterm2012 <- gridmetmonthly[274:282,]
    shortterm2012 <- gridmetmonthly[279:282,]
    climmeanlongterm2012 <- colMeans(longterm2012[,3:16])
    climmeanshortterm2012 <- colMeans(shortterm2012[,3:16])
    wheatdrought2012 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2012 <- subset(wheatdroughtclaim_allall_final, year == 2012)
    wheatclaimlosssum2012 <- sum(wheatdrought2012$loss)
    wheatclaimacressum2012 <- sum(wheatdrought2012$acres)
    wheatclaimcounttotal2012 <- nrow(wheatdrought2012)/nrow(wheatdroughtclaim_all2012)
    wheatclaimcountsum2012 <- nrow(wheatdrought2012)
    
    longterm2013 <- gridmetmonthly[286:294,]
    shortterm2013 <- gridmetmonthly[291:294,]
    climmeanlongterm2013 <- colMeans(longterm2013[,3:16])
    climmeanshortterm2013 <- colMeans(shortterm2013[,3:16])
    wheatdrought2013 <- subset(wheatdroughtclaim, year == 2013)
    wheatdroughtclaim_all2013 <- subset(wheatdroughtclaim_allall_final, year == 2013)
    wheatclaimlosssum2013 <- sum(wheatdrought2013$loss)
    wheatclaimacressum2013 <- sum(wheatdrought2013$acres)
    wheatclaimcounttotal2013 <- nrow(wheatdrought2013)/nrow(wheatdroughtclaim_all2013)
    wheatclaimcountsum2013 <- nrow(wheatdrought2013)
    
    longterm2014 <- gridmetmonthly[298:306,]
    shortterm2014 <- gridmetmonthly[303:306,]
    climmeanlongterm2014 <- colMeans(longterm2014[,3:16])
    climmeanshortterm2014 <- colMeans(shortterm2014[,3:16])
    wheatdrought2014 <- subset(wheatdroughtclaim, year == 2014)
    wheatdroughtclaim_all2014 <- subset(wheatdroughtclaim_allall_final, year == 2014)
    wheatclaimlosssum2014 <- sum(wheatdrought2014$loss)
    wheatclaimacressum2014 <- sum(wheatdrought2014$acres)
    wheatclaimcounttotal2014 <- nrow(wheatdrought2014)/nrow(wheatdroughtclaim_all2014)
    wheatclaimcountsum2014 <- nrow(wheatdrought2014)
    
    longterm2015 <- gridmetmonthly[310:318,]
    shortterm2015 <- gridmetmonthly[315:318,]
    climmeanlongterm2015 <- colMeans(longterm2015[,3:16])
    climmeanshortterm2015 <- colMeans(shortterm2015[,3:16])
    wheatdrought2015 <- subset(wheatdroughtclaim, year == 2015)
    wheatdroughtclaim_all2015 <- subset(wheatdroughtclaim_allall_final, year == 2015)
    wheatclaimlosssum2015 <- sum(wheatdrought2015$loss)
    wheatclaimacressum2015 <- sum(wheatdrought2015$acres)
    wheatclaimcounttotal2015 <- nrow(wheatdrought2015)/nrow(wheatdroughtclaim_all2015)
    wheatclaimcountsum2015 <- nrow(wheatdrought2015)
    
    wls <- rbind(wheatclaimlosssum1989, wheatclaimlosssum1990, wheatclaimlosssum1991, wheatclaimlosssum1992, wheatclaimlosssum1993, wheatclaimlosssum1994, wheatclaimlosssum1995, wheatclaimlosssum1996, wheatclaimlosssum1997, wheatclaimlosssum1998, wheatclaimlosssum1999, wheatclaimlosssum2000, wheatclaimlosssum2001, wheatclaimlosssum2002, wheatclaimlosssum2003, wheatclaimlosssum2004, wheatclaimlosssum2005,wheatclaimlosssum2006, wheatclaimlosssum2007, wheatclaimlosssum2008, wheatclaimlosssum2009,wheatclaimlosssum2010,wheatclaimlosssum2011,wheatclaimlosssum2012,wheatclaimlosssum2013,wheatclaimlosssum2014,wheatclaimlosssum2015)
    wla <- rbind(wheatclaimacressum1989, wheatclaimacressum1990, wheatclaimacressum1991, wheatclaimacressum1992, wheatclaimacressum1993, wheatclaimacressum1994, wheatclaimacressum1995, wheatclaimacressum1996, wheatclaimacressum1997, wheatclaimacressum1998, wheatclaimacressum1999, wheatclaimacressum2000, wheatclaimacressum2001, wheatclaimacressum2002,wheatclaimacressum2003,wheatclaimacressum2004, wheatclaimacressum2005, wheatclaimacressum2006, wheatclaimacressum2007, wheatclaimacressum2008,wheatclaimacressum2009,wheatclaimacressum2010,wheatclaimacressum2011,wheatclaimacressum2012,wheatclaimacressum2013,wheatclaimacressum2014,wheatclaimacressum2015)
    wlc <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    climmeanlongterm1 <- rbind(climmeanlongterm1989, climmeanlongterm1990, climmeanlongterm1991, climmeanlongterm1992, climmeanlongterm1993, climmeanlongterm1994, climmeanlongterm1995, climmeanlongterm1996, climmeanlongterm1997, climmeanlongterm1998, climmeanlongterm1999, climmeanlongterm2000, climmeanlongterm2001, climmeanlongterm2002, climmeanlongterm2003, climmeanlongterm2004, climmeanlongterm2005, climmeanlongterm2006, climmeanlongterm2007,climmeanlongterm2008,climmeanlongterm2009,climmeanlongterm2010,climmeanlongterm2011,climmeanlongterm2012,climmeanlongterm2013,climmeanlongterm2014,climmeanlongterm2015)
    climmeanshortterm1 <- rbind(climmeanshortterm1989, climmeanshortterm1990, climmeanshortterm1991, climmeanshortterm1992, climmeanshortterm1993, climmeanshortterm1994, climmeanshortterm1995, climmeanshortterm1996, climmeanshortterm1997, climmeanshortterm1998, climmeanshortterm1999, climmeanshortterm2000, climmeanshortterm2001, climmeanshortterm2002, climmeanshortterm2003, climmeanshortterm2004, climmeanshortterm2005, climmeanshortterm2006, climmeanshortterm2007,climmeanshortterm2008,climmeanshortterm2009,climmeanshortterm2010,climmeanshortterm2011,climmeanshortterm2012,climmeanshortterm2013,climmeanshortterm2014,climmeanshortterm2015)
    wheatclaimcounttotal <- rbind(wheatclaimcounttotal1989, wheatclaimcounttotal1990, wheatclaimcounttotal1991, wheatclaimcounttotal1992, wheatclaimcounttotal1993, wheatclaimcounttotal1994, wheatclaimcounttotal1995, wheatclaimcounttotal1996, wheatclaimcounttotal1997, wheatclaimcounttotal1998, wheatclaimcounttotal1999, wheatclaimcounttotal2000, wheatclaimcounttotal2001, wheatclaimcounttotal2002, wheatclaimcounttotal2003, wheatclaimcounttotal2004, wheatclaimcounttotal2005,  wheatclaimcounttotal2006, wheatclaimcounttotal2007, wheatclaimcounttotal2008,wheatclaimcounttotal2009, wheatclaimcounttotal2010, wheatclaimcounttotal2011, wheatclaimcounttotal2012, wheatclaimcounttotal2013, wheatclaimcounttotal2014, wheatclaimcounttotal2015)
    wheatclaimcountsumtotal <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    
    finalz <- cbind(climmeanlongterm1,wls, wla, wlc, wheatclaimcounttotal, climmeanshortterm1)
    finalz <- data.frame(finalz)
    names(finalz)[16] <- c("loss")
    names(finalz)[17] <- c("acres")
    names(finalz)[18] <- c("count")
    names(finalz)[19] <- c("countratio")
    rownames(finalz) <- c(1989:2015)
    finalz[33]<- c(1989:2015)
    finalz[34] <- c(pu)
    names(finalz)[33] <- c("year")
    names(finalz)[34] <- c("county")
    finalz[35] <- q
    finalz[36] <- m
    names(finalz)[36] <- c("damagecause")
    finalz[37] <- statez
    
    colnames(finalz)[15] <- c("loss")
    colnames(finalz)[16] <- c("acres")
    colnames(finalz)[17] <- c("count")
    colnames(finalz)[18] <- c("countratio")
    colnames(finalz)[19] <- c("pr")
    
    names(finalz)[37] <- c(statez)
    
    
    names(finalz)[19:32] <- c("pr_short", "th_short", "pdsi_short", "pet_short", "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short", "sph_short", "vs_short", "fm_1000_short", "fm_100_short")
    
    finalzz <- merge(finalz, finalz_soil, by = "year")
     
    #names(finalz)[20:34] <- c("bi_short", "pr_short", "th_short", "pdsi_short", "pet_short", "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short", "sph_short", "vs_short", "fm_1000_short", "fm_100_short")
    
    #finalz <- cbind(finalz, wheatclaimcounttotal)
    #names(finalz)[37] <- c("countratio")
    
    #usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    #usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    #usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    #usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    #gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    #df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))

    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/")
    
    m <- gsub("/", "-", m)
    m <- gsub(" ", "-", m)
    name = paste("Annual_climate_crop_", p, "_", q, "_", m, sep="")
    write.csv(finalzz, file=name)   
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
     }
   }
}

dirname4 <- "/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/"
files <- list.files(dirname4)

for (ll in files) {
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/")
fileopen <- read.csv(ll, header=TRUE)
#colnames(fileopen) <- vectorr
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
write.table(fileopen, file="1989-2015_combined.csv",
            append=TRUE,
            col.names = FALSE,
            sep = ',')
}

vectorr <- c("X", "ID", "year", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax",  "tmmn",  "tmmx",  "srad",  "sph", "vs", "fm1000",  "fm100",  "loss",  "acres",               
             "count", "countratio", "pr_short", "th_short", "pdsi_short", "pet_short",             
             "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short",            
             "sph_short", "vs_short", "fm_1000_short", "fm_100_short",  "county", "commodity",                   
             "damagecause", "state",  "soil_moisture_longterm", "loss.y", "acres.y", "count.y",               
             "countratio.y",  "soil_moisture_shorterm", "county.y",  "commodity.y",  "damagecause.y")


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
combined_allall <- read.csv("1989-2015_combined.csv", header = FALSE)
colnames(combined_allall) <- vectorr
#combined2_allall <- combined_allall[2:58016,]

#combined_allall$countratio.x[combined_allall$countratio.x== "<NA>"] <- 0

combined_allall$countratio[is.na(combined_allall$countratio)] <- 0

combined2_allall <- combined_allall[3:16]
combined2a_allall <- combined_allall[22:35]

combined2c_allall <- combined_allall[40]
combined2d_allall <- combined_allall[45]
combined2e_allall <- combined_allall[18:21]
combined2f_allall <- combined_allall[36:39]

combined4_allall <- cbind(combined2_allall, combined2a_allall, combined2c_allall, combined2d_allall, combined2e_allall, combined2f_allall)

combined5_allall <- subset(combined4_allall, loss != 0)

colnames(combined4_allall)[37] <- "commodity"
colnames(combined4_allall)[38] <- "damagecause"
colnames(combined4_allall)[36] <- "county"
colnames(combined4_allall)[21] <- "countratio"
colnames(combined4_allall)[20] <- "count"
colnames(combined4_allall)[19] <- "acres"
colnames(combined4_allall)[18] <- "loss"

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
write.table(combined5_allall, file="1989-2015_combined_revised.csv",
            col.names = TRUE,
            sep = ',')

  #setwd(dirname)
  #files <- list.files(dirname, pattern = 'monthly_usda_gridmet_post2001')
  #tables <- lapply(files, read.csv, header=TRUE)
  #combined.df <- do.call(rbind, tables)
  #name2 = paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep="")
  #write.matrix(combined.df, file=name2, sep=",")
  
  #} 
    
  #}


##-move files to appropriate locations



#system(paste("mkdir", scen_state))


#system(paste("mv *usda_monthly*", " ./summaries", sep=""))
system(paste("mv *usda_gridmet*", " ./summaries", sep=""))


#---test on - files created above for summary state

setwd(paste(dirname, "/month_png", sep=""))
system(paste("mkdir", dcause))

print("Finished climate variables, moving to USDA Ag data...")
#--break here put text notification


#-------------------  

library(RColorBrewer)
library(ggplot2)
library(rasterVis)
library(data.table)
library(rgdal)
library(maptools)
library(rasterVis)
library(maptools)
library(SDMTools)
library(fields)
library(dplyr)
library(tidyr)

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries", sep=""))
combined.df <- data.frame(read.csv(paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep=""), strip.white = TRUE))

#-remove all other variables to allow for datasets based on year, month, county, and commodity - loss and acres
combined.df2 <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

#-convert to a data table

combined.df <- data.table(combined.df)

ttt <- function (x) sub("\\s+$", "", x)

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/commodity_csv/", sep=""))
#function(commoditynames) sub("\\s+$", "", commoditynames)
#--strip white space off so file names are right
#lapply(combined.df$commodity, function(funct){sub("\\s+$", "", combined.df$commodity[[funct]])})
commoditynames <- unique(combined.df$commodity)
ttt <- function (x) sub("\\s+$", "", x)
#commoditytrim <- unique(ttt(combined.df$commodity))
commoditytrim <- unique(combined.df$commodity)

for (i in commoditytrim) {
  x <- subset(combined.df, combined.df$commodity == i)
  write.csv(x, file=paste(scen1, "_", scen2, "_", scen_state, i, ".csv", sep=""))
}


#-order the columns by commodity, then year, month, and county

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]

combined.df <- subset(combined.df, commoditycode != "NA")

#-sum the acres and loss columns for all common rows  This merges all rows that have the same values exept for acres and loss.  We sum those to create a geographic
#-representation for each commodity - for each county, year, and month.  This will be use to convert to a raster for comparison to meterological data.

combined.df <- combined.df[, lapply(.SD, sum), by=list(year,county,commoditycode,monthcode,commodity)]
#combined.df <- data.frame(lapply(combined.df, trimws))
#--replacing commoditycode with commodity name

#profession.code <- c(Apples=54, Wheat=11, Barley=91, SugarBeets=39, Cherries=57, Grapes=53, AdjustedGrossRevenue=63, 
#                     GreenPeas=64, AllOtherCrops=99, Pears=89, Canola=15, SweetCorn=42, Mint=74, Potatoes=84, 
#                     DryPeas=67, ProcessingBeans=46, DryBeans=47, Onions=13, Cranberries=58, Corn=41, 
#                     Oats=16, AlfalfaSeed=107, FreshApricots=218, FreshFreestonePeaches=223, Nursery=73, 
#                     Mustard=69, Bluberries=12, AdjustedGrossRevenuelite=61, Plums=92, Soybeans=81, 
#                     WholeFarmRevenueProtection=76, Buckwheat=114)

#combined.df$commodity <- names(profession.code)[match(combined.df$commoditycode, profession.code)]

#-----
combined.df$year <- as.numeric(as.character(combined.df$year))

vect <- c(N1:N2)
combined.df2 <- subset(combined.df, year > 2000) 
combined.df3 <- subset(combined.df2, year < 2017) 

combined.df <- combined.df3

combined.yearmonth <- split(combined.df,list(combined.df$year,combined.df$monthcode, combined.df$commodity))
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
lapply(names(combined.yearmonth), function(funct){write.csv(combined.yearmonth[[funct]], file = paste(funct, ".csv", sep = ""))})

system("mv *ADJUSTED* ../commodity_csv_agr/")
system("rm *NA*")
system("rm *..*")

monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
setwd(monthdir)
system("find month -type f -size +100c -exec cp -nv {} month_positive/ \\;")

