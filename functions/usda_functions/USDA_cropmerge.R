USDA_cropmerge <- function(startyear, endyear, damage) {

yearspan <- c(startyear:endyear)
#--merge usda data with gridmet data
#for (qq in statespan) {
#for (i in yearspan) {

usda1 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2001.txt", sep="")
usda1 <- read.csv(usda1, header=FALSE, sep="|")
usda1 <- data.frame(usda1)
usda2 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2002.txt", sep="")
usda2 <- read.csv(usda2, header=FALSE, sep="|")
usda2 <- data.frame(usda2)
usda3 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2003.txt", sep="")
usda3 <- read.csv(usda3, header=FALSE, sep="|")
usda3 <- data.frame(usda3)
usda4 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2004.txt", sep="")
usda4 <- read.csv(usda4, header=FALSE, sep="|")
usda4 <- data.frame(usda4)
usda5 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2005.txt", sep="")
usda5 <- read.csv(usda5, header=FALSE, sep="|")
usda5 <- data.frame(usda5)
usda6 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2006.txt", sep="")
usda6 <- read.csv(usda6, header=FALSE, sep="|")
usda6 <- data.frame(usda6)
usda7 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2007.txt", sep="")
usda7 <- read.csv(usda7, header=FALSE, sep="|")
usda7 <- data.frame(usda7)
usda8 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2008.txt", sep="")
usda8 <- read.csv(usda8, header=FALSE, sep="|")
usda8 <- data.frame(usda8)
usda9 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2009.txt", sep="")
usda9 <- read.csv(usda9, header=FALSE, sep="|")
usda9 <- data.frame(usda9)
usda10 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2010.txt", sep="")
usda10 <- read.csv(usda10, header=FALSE, sep="|")
usda10 <- data.frame(usda10)
usda11 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2011.txt", sep="")
usda11 <- read.csv(usda11, header=FALSE, sep="|")
usda11 <- data.frame(usda11)
usda12 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2012.txt", sep="")
usda12 <- read.csv(usda12, header=FALSE, sep="|")
usda12 <- data.frame(usda12)
usda13 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2013.txt", sep="")
usda13 <- read.csv(usda13, header=FALSE, sep="|")
usda13 <- data.frame(usda13)
usda14 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2014.txt", sep="")
usda14 <- read.csv(usda14, header=FALSE, sep="|")
usda14 <- data.frame(usda14)
usda15 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2015.txt", sep="")
usda15 <- read.csv(usda15, header=FALSE, sep="|")
usda15 <- data.frame(usda15)

usdabound <- rbind(usda1,usda2,usda3,usda4,usda5,usda6,usda7,usda8,usda9,usda10,usda11,usda12,usda13,usda14,usda15)

colnames(usdabound) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
usdabound$county <- trimws(usdabound$county)
usdabound$commodity <- trimws(usdabound$commodity)
usdabound$damagecause <- trimws(usdabound$damagecause)



for (p in listercomb) {
  
  listersplitter <- unlist(strsplit(p, "[_]"))
  countyz <- listersplitter[1]
  statez <- listersplitter[2]
  pu <- capitalize(countyz)
  pu <- simpleCap(pu)
  p <- tolower(p)
  
  pp <- stateFromLower(statez)
  pp <- as.vector(pp)
  
  
  usdaboundsub <- subset(usdabound, state == pp)
  usdaboundsub <- subset(usdaboundsub, county == pu)
  commodityspan <- c(unique(usdaboundsub$commodity))
  for (q in commodityspan) {
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
    p <- tolower(p)
    countyz <- tolower(countyz)
    gridmetmonthly <- paste("2001_2015_palouse_", countyz, "_", statez, sep="")
    
    
    
    
    
    
    
    
    #usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    #usda <- read.csv(usda, header=FALSE, sep="|")
    #usda <- data.frame(usda)
    gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
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
    
    #usda_county$commodity <- trimws(usda_county$commodity)
    
    wheatdroughtclaim_allID <- subset(usdabound, state == "ID")
    wheatdroughtclaim_allID <- subset(wheatdroughtclaim_allID, county == "Idaho" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Lewis")
    wheatdroughtclaim_allWA <- subset(usdabound, state == "WA")
    wheatdroughtclaim_allWA <- subset(wheatdroughtclaim_allWA, county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin")
    wheatdroughtclaim_allOR <- subset(usdabound, state == "OR")
    wheatdroughtclaim_allOR <- subset(wheatdroughtclaim_allOR, county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")
    wheatdroughtclaim_allall <- rbind(wheatdroughtclaim_allWA, wheatdroughtclaim_allOR, wheatdroughtclaim_allID)
    
    wheatdroughtclaim_allall_comm <- subset(wheatdroughtclaim_allall, commodity == q)
    wheatdroughtclaim_allall_drought <- subset(wheatdroughtclaim_allall_comm, damagecause == damage)
    wheatdroughtclaim_allall_final <- subset(wheatdroughtclaim_allall_drought, monthcode == 3 | monthcode == 4 | monthcode == 5 | monthcode == 6 | monthcode == 7 | monthcode == 8 | monthcode == 9 | monthcode == 10)
    
    
    
    wheatdroughtclaim1 <- subset(usdabound, state == statez1)
    wheatdroughtclaim2 <- subset(wheatdroughtclaim1, county == countyz)
    wheatdroughtclaim3 <- subset(wheatdroughtclaim2, commodity == q)
    wheatdroughtclaim4 <- subset(wheatdroughtclaim3, damagecause == damage)
    wheatdroughtclaim <- subset(wheatdroughtclaim4, monthcode == 3 | monthcode == 4 | monthcode == 5 | monthcode == 6 | monthcode == 7 | monthcode == 8 | monthcode == 9 | monthcode == 10)
    
    longterm2001 <- gridmetmonthly[1:6,]
    shortterm2001 <- gridmetmonthly[3:6,]
    climmeanlongterm2001 <- colMeans(longterm2001[,3:17])
    climmeanshortterm2001 <- colMeans(shortterm2001[,3:17])
    wheatdrought2001 <- subset(wheatdroughtclaim, year == 2001)
    wheatdroughtclaim_all2001 <- subset(wheatdroughtclaim_allall_final, year == 2001)
    wheatclaimlosssum2001 <- sum(wheatdrought2001$loss)
    wheatclaimacressum2001 <- sum(wheatdrought2001$acres)
    wheatclaimcounttotal2001 <- nrow(wheatdrought2001)/nrow(wheatdroughtclaim_all2001)
    wheatclaimcountsum2001 <- nrow(wheatdrought2001)
    
    
    longterm2002 <- gridmetmonthly[10:18,]
    shortterm2002 <- gridmetmonthly[15:18,]
    climmeanlongterm2002 <- colMeans(longterm2002[,3:17])
    climmeanshortterm2002 <- colMeans(shortterm2002[,3:17])
    wheatdrought2002 <- subset(wheatdroughtclaim, year == 2002)
    wheatdroughtclaim_all2002 <- subset(wheatdroughtclaim_allall_final, year == 2002)
    wheatclaimlosssum2002 <- sum(wheatdrought2002$loss)
    wheatclaimacressum2002 <- sum(wheatdrought2002$acres)
    wheatclaimcounttotal2002 <- nrow(wheatdrought2002)/nrow(wheatdroughtclaim_all2002)
    wheatclaimcountsum2002 <- nrow(wheatdrought2002)
    
    longterm2003 <- gridmetmonthly[22:30,]
    shortterm2003 <- gridmetmonthly[27:30,]
    climmeanlongterm2003 <- colMeans(longterm2003[,3:17])
    climmeanshortterm2003 <- colMeans(shortterm2003[,3:17])
    wheatdrought2003 <- subset(wheatdroughtclaim, year == 2003)
    wheatdroughtclaim_all2003 <- subset(wheatdroughtclaim_allall_final, year == 2003)
    wheatclaimlosssum2003 <- sum(wheatdrought2003$loss)
    wheatclaimacressum2003 <- sum(wheatdrought2003$acres)
    wheatclaimcounttotal2003 <- nrow(wheatdrought2003)/nrow(wheatdroughtclaim_all2003)
    wheatclaimcountsum2003 <- nrow(wheatdrought2003)
    
    longterm2004 <- gridmetmonthly[34:42,]
    shortterm2004 <- gridmetmonthly[39:42,]
    climmeanlongterm2004 <- colMeans(longterm2004[,3:17])
    climmeanshortterm2004 <- colMeans(shortterm2004[,3:17])
    wheatdrought2004 <- subset(wheatdroughtclaim, year == 2004)
    wheatdroughtclaim_all2004 <- subset(wheatdroughtclaim_allall_final, year == 2004)
    wheatclaimlosssum2004 <- sum(wheatdrought2004$loss)
    wheatclaimacressum2004 <- sum(wheatdrought2004$acres)
    wheatclaimcounttotal2004 <- nrow(wheatdrought2004)/nrow(wheatdroughtclaim_all2004)
    wheatclaimcountsum2004 <- nrow(wheatdrought2004)
    
    longterm2005 <- gridmetmonthly[46:54,]
    shortterm2005 <- gridmetmonthly[51:54,]
    climmeanlongterm2005 <- colMeans(longterm2005[,3:17])
    climmeanshortterm2005 <- colMeans(shortterm2005[,3:17])
    wheatdrought2005 <- subset(wheatdroughtclaim, year == 2005)
    wheatdroughtclaim_all2005 <- subset(wheatdroughtclaim_allall_final, year == 2005)
    wheatclaimlosssum2005 <- sum(wheatdrought2005$loss)
    wheatclaimacressum2005 <- sum(wheatdrought2005$acres)
    wheatclaimcounttotal2005 <- nrow(wheatdrought2005)/nrow(wheatdroughtclaim_all2005)
    wheatclaimcountsum2005 <- nrow(wheatdrought2005)
    
    longterm2006 <- gridmetmonthly[58:66,]
    shortterm2006 <- gridmetmonthly[63:66,]
    climmeanlongterm2006 <- colMeans(longterm2006[,3:17])
    climmeanshortterm2006 <- colMeans(shortterm2006[,3:17])
    wheatdrought2006 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2006 <- subset(wheatdroughtclaim_allall_final, year == 2006)
    wheatclaimlosssum2006 <- sum(wheatdrought2006$loss)
    wheatclaimacressum2006 <- sum(wheatdrought2006$acres)
    wheatclaimcounttotal2006 <- nrow(wheatdrought2006)/nrow(wheatdroughtclaim_all2006)
    wheatclaimcountsum2006 <- nrow(wheatdrought2006)
    
    longterm2007 <- gridmetmonthly[70:78,]
    shortterm2007 <- gridmetmonthly[75:78,]
    climmeanlongterm2007 <- colMeans(longterm2007[,3:17])
    climmeanshortterm2007 <- colMeans(shortterm2007[,3:17])
    wheatdrought2007 <- subset(wheatdroughtclaim, year == 2007)
    wheatdroughtclaim_all2007 <- subset(wheatdroughtclaim_allall_final, year == 2007)
    wheatclaimlosssum2007 <- sum(wheatdrought2007$loss)
    wheatclaimacressum2007 <- sum(wheatdrought2007$acres)
    wheatclaimcounttotal2007 <- nrow(wheatdrought2007)/nrow(wheatdroughtclaim_all2007)
    wheatclaimcountsum2007 <- nrow(wheatdrought2007)
    
    longterm2008 <- gridmetmonthly[82:90,]
    shortterm2008 <- gridmetmonthly[87:90,]
    climmeanlongterm2008 <- colMeans(longterm2008[,3:17])
    climmeanshortterm2008 <- colMeans(shortterm2008[,3:17])
    wheatdrought2008 <- subset(wheatdroughtclaim, year == 2008)
    wheatdroughtclaim_all2008 <- subset(wheatdroughtclaim_allall_final, year == 2008)
    wheatclaimlosssum2008 <- sum(wheatdrought2008$loss)
    wheatclaimacressum2008 <- sum(wheatdrought2008$acres)
    wheatclaimcounttotal2008 <- nrow(wheatdrought2008)/nrow(wheatdroughtclaim_all2008)
    wheatclaimcountsum2008 <- nrow(wheatdrought2008)
    
    longterm2009 <- gridmetmonthly[94:102,]
    shortterm2009 <- gridmetmonthly[99:102,]
    climmeanlongterm2009 <- colMeans(longterm2009[,3:17])
    climmeanshortterm2009 <- colMeans(shortterm2009[,3:17])
    wheatdrought2009 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2009 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2009 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2009 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2009 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2009)
    wheatclaimcountsum2009 <- nrow(wheatdrought2009)
    
    longterm2010 <- gridmetmonthly[106:114,]
    shortterm2010 <- gridmetmonthly[111:114,]
    climmeanlongterm2010 <- colMeans(longterm2010[,3:17])
    climmeanshortterm2010 <- colMeans(shortterm2010[,3:17])
    wheatdrought2010 <- subset(wheatdroughtclaim, year == 2010)
    wheatdroughtclaim_all2010 <- subset(wheatdroughtclaim_allall_final, year == 2010)
    wheatclaimlosssum2010 <- sum(wheatdrought2010$loss)
    wheatclaimacressum2010 <- sum(wheatdrought2010$acres)
    wheatclaimcounttotal2010 <- nrow(wheatdrought2010)/nrow(wheatdroughtclaim_all2010)
    wheatclaimcountsum2010 <- nrow(wheatdrought2010)
    
    longterm2011 <- gridmetmonthly[118:126,]
    shortterm2011 <- gridmetmonthly[123:126,]
    climmeanlongterm2011 <- colMeans(longterm2009[,3:17])
    climmeanshortterm2011 <- colMeans(shortterm2011[,3:17])
    wheatdrought2011 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2011 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2011 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2011 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2011 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2011)
    wheatclaimcountsum2011 <- nrow(wheatdrought2009)
    
    
    longterm2012 <- gridmetmonthly[130:138,]
    shortterm2012 <- gridmetmonthly[135:138,]
    climmeanlongterm2012 <- colMeans(longterm2012[,3:17])
    climmeanshortterm2012 <- colMeans(shortterm2012[,3:17])
    wheatdrought2012 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2012 <- subset(wheatdroughtclaim_allall_final, year == 2012)
    wheatclaimlosssum2012 <- sum(wheatdrought2012$loss)
    wheatclaimacressum2012 <- sum(wheatdrought2012$acres)
    wheatclaimcounttotal2012 <- nrow(wheatdrought2012)/nrow(wheatdroughtclaim_all2012)
    wheatclaimcountsum2012 <- nrow(wheatdrought2012)
    
    longterm2013 <- gridmetmonthly[142:150,]
    shortterm2013 <- gridmetmonthly[147:150,]
    climmeanlongterm2013 <- colMeans(longterm2013[,3:17])
    climmeanshortterm2013 <- colMeans(shortterm2013[,3:17])
    wheatdrought2013 <- subset(wheatdroughtclaim, year == 2013)
    wheatdroughtclaim_all2013 <- subset(wheatdroughtclaim_allall_final, year == 2013)
    wheatclaimlosssum2013 <- sum(wheatdrought2013$loss)
    wheatclaimacressum2013 <- sum(wheatdrought2013$acres)
    wheatclaimcounttotal2013 <- nrow(wheatdrought2013)/nrow(wheatdroughtclaim_all2013)
    wheatclaimcountsum2013 <- nrow(wheatdrought2013)
    
    longterm2014 <- gridmetmonthly[154:162,]
    shortterm2014 <- gridmetmonthly[159:162,]
    climmeanlongterm2014 <- colMeans(longterm2014[,3:17])
    climmeanshortterm2014 <- colMeans(shortterm2014[,3:17])
    wheatdrought2014 <- subset(wheatdroughtclaim, year == 2014)
    wheatdroughtclaim_all2014 <- subset(wheatdroughtclaim_allall_final, year == 2014)
    wheatclaimlosssum2014 <- sum(wheatdrought2014$loss)
    wheatclaimacressum2014 <- sum(wheatdrought2014$acres)
    wheatclaimcounttotal2014 <- nrow(wheatdrought2014)/nrow(wheatdroughtclaim_all2014)
    wheatclaimcountsum2014 <- nrow(wheatdrought2014)
    
    longterm2015 <- gridmetmonthly[166:174,]
    shortterm2015 <- gridmetmonthly[171:174,]
    climmeanlongterm2015 <- colMeans(longterm2015[,3:17])
    climmeanshortterm2015 <- colMeans(shortterm2015[,3:17])
    wheatdrought2015 <- subset(wheatdroughtclaim, year == 2015)
    wheatdroughtclaim_all2015 <- subset(wheatdroughtclaim_allall_final, year == 2015)
    wheatclaimlosssum2015 <- sum(wheatdrought2015$loss)
    wheatclaimacressum2015 <- sum(wheatdrought2015$acres)
    wheatclaimcounttotal2015 <- nrow(wheatdrought2015)/nrow(wheatdroughtclaim_all2015)
    wheatclaimcountsum2015 <- nrow(wheatdrought2015)
    
    
    wls <- rbind(wheatclaimlosssum2001, wheatclaimlosssum2002, wheatclaimlosssum2003, wheatclaimlosssum2004, wheatclaimlosssum2005,wheatclaimlosssum2006, wheatclaimlosssum2007, wheatclaimlosssum2008, wheatclaimlosssum2009,wheatclaimlosssum2010,wheatclaimlosssum2011,wheatclaimlosssum2012,wheatclaimlosssum2013,wheatclaimlosssum2014,wheatclaimlosssum2015)
    wla <- rbind(wheatclaimacressum2001, wheatclaimacressum2002,wheatclaimacressum2003,wheatclaimacressum2004, wheatclaimacressum2005, wheatclaimacressum2006, wheatclaimacressum2007, wheatclaimacressum2008,wheatclaimacressum2009,wheatclaimacressum2010,wheatclaimacressum2011,wheatclaimacressum2012,wheatclaimacressum2013,wheatclaimacressum2014,wheatclaimacressum2015)
    wlc <- rbind(wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    climmeanlongterm1 <- rbind(climmeanlongterm2001, climmeanlongterm2002, climmeanlongterm2003, climmeanlongterm2004, climmeanlongterm2005, climmeanlongterm2006, climmeanlongterm2007,climmeanlongterm2008,climmeanlongterm2009,climmeanlongterm2010,climmeanlongterm2011,climmeanlongterm2012,climmeanlongterm2013,climmeanlongterm2014,climmeanlongterm2015)
    climmeanshortterm1 <- rbind(climmeanshortterm2001, climmeanshortterm2002, climmeanshortterm2003, climmeanshortterm2004, climmeanshortterm2005, climmeanshortterm2006, climmeanshortterm2007,climmeanshortterm2008,climmeanshortterm2009,climmeanshortterm2010,climmeanshortterm2011,climmeanshortterm2012,climmeanshortterm2013,climmeanshortterm2014,climmeanshortterm2015)
    wheatclaimcounttotal <- rbind(wheatclaimcounttotal2001, wheatclaimcounttotal2002, wheatclaimcounttotal2003, wheatclaimcounttotal2004, wheatclaimcounttotal2005,  wheatclaimcounttotal2006, wheatclaimcounttotal2007, wheatclaimcounttotal2008,wheatclaimcounttotal2009, wheatclaimcounttotal2010, wheatclaimcounttotal2011, wheatclaimcounttotal2012, wheatclaimcounttotal2013, wheatclaimcounttotal2014, wheatclaimcounttotal2015)
    wheatclaimcountsumtotal <- rbind(wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    
    finalz <- cbind(climmeanlongterm1,wls, wla, wlc, wheatclaimcounttotal, climmeanshortterm1)
    finalz <- data.frame(finalz)
    names(finalz)[16] <- c("loss")
    names(finalz)[17] <- c("acres")
    names(finalz)[18] <- c("count")
    names(finalz)[19] <- c("countratio")
    rownames(finalz) <- c(2001:2015)
    finalz[35]<- c(2001:2015)
    finalz[36] <- c(pu)
    names(finalz)[35] <- c("year")
    names(finalz)[36] <- c("county")
    
    names(finalz)[20:34] <- c("bi_short", "pr_short", "th_short", "pdsi_short", "pet_short", "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short", "sph_short", "vs_short", "fm_1000_short", "fm_100_short")
    
    #finalz <- cbind(finalz, wheatclaimcounttotal)
    #names(finalz)[37] <- c("countratio")
    
    #usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    #usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    #usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    #usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    #gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    #df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries/")
    name = paste("Annual_climate_crop_", p, "_", q, "_drought", sep="")
    write.csv(finalz, file=name)
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
  }
}


}

