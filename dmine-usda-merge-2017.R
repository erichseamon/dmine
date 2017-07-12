usda0 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2000.txt", sep="")
usda0 <- read.csv(usda0, header=FALSE, sep="|")
usda0 <- data.frame(usda0)
colnames(usda0) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
usda_WA_0 <- subset(usda0, state == "WA")
usda_ID_0 <- subset(usda0, state == "ID")
usda_OR_0 <- subset(usda0, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_0, file="2000_monthly_usda_Washington_summary")
write.csv(usda_ID_0, file="2000_monthly_usda_Idaho_summary")
write.csv(usda_OR_0, file="2000_monthly_usda_Oregon_summary")


#---------------begin



usda89 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1989.txt", sep="")
usda89 <- read.csv(usda89, header=FALSE, sep="|")
usda89 <- data.frame(usda89)
colnames(usda89) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_89 <- subset(usda89, state == "WA")
usda_ID_89 <- subset(usda89, state == "ID")
usda_OR_89 <- subset(usda89, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_89, file="1989_monthly_usda_Washington_summary")
write.csv(usda_ID_89, file="1989_monthly_usda_Idaho_summary")
write.csv(usda_OR_89, file="1989_monthly_usda_Oregon_summary")





usda90 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1990.txt", sep="")
usda90 <- read.csv(usda90, header=FALSE, sep="|")
usda90 <- data.frame(usda90)
colnames(usda90) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_90 <- subset(usda90, state == "WA")
usda_ID_90 <- subset(usda90, state == "ID")
usda_OR_90 <- subset(usda90, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_90, file="1990_monthly_usda_Washington_summary")
write.csv(usda_ID_90, file="1990_monthly_usda_Idaho_summary")
write.csv(usda_OR_90, file="1990_monthly_usda_Oregon_summary")


usda91 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1991.txt", sep="")
usda91 <- read.csv(usda91, header=FALSE, sep="|")
usda91 <- data.frame(usda91)
colnames(usda91) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_91 <- subset(usda91, state == "WA")
usda_ID_91 <- subset(usda91, state == "ID")
usda_OR_91 <- subset(usda91, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_91, file="1991_monthly_usda_Washington_summary")
write.csv(usda_ID_91, file="1991_monthly_usda_Idaho_summary")
write.csv(usda_OR_91, file="1991_monthly_usda_Oregon_summary")

usda92 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1992.txt", sep="")
usda92 <- read.csv(usda92, header=FALSE, sep="|")
usda92 <- data.frame(usda92)
colnames(usda92) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_92 <- subset(usda92, state == "WA")
usda_ID_92 <- subset(usda92, state == "ID")
usda_OR_92 <- subset(usda92, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_92, file="1992_monthly_usda_Washington_summary")
write.csv(usda_ID_92, file="1992_monthly_usda_Idaho_summary")
write.csv(usda_OR_92, file="1992_monthly_usda_Oregon_summary")

usda93 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1993.txt", sep="")
usda93 <- read.csv(usda93, header=FALSE, sep="|")
usda93 <- data.frame(usda93)
colnames(usda93) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_93 <- subset(usda93, state == "WA")
usda_ID_93 <- subset(usda93, state == "ID")
usda_OR_93 <- subset(usda93, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_93, file="1993_monthly_usda_Washington_summary")
write.csv(usda_ID_93, file="1993_monthly_usda_Idaho_summary")
write.csv(usda_OR_93, file="1993_monthly_usda_Oregon_summary")

usda94 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1994.txt", sep="")
usda94 <- read.csv(usda94, header=FALSE, sep="|")
usda94 <- data.frame(usda94)
colnames(usda94) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_94 <- subset(usda94, state == "WA")
usda_ID_94 <- subset(usda94, state == "ID")
usda_OR_94 <- subset(usda94, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_94, file="1994_monthly_usda_Washington_summary")
write.csv(usda_ID_94, file="1994_monthly_usda_Idaho_summary")
write.csv(usda_OR_94, file="1994_monthly_usda_Oregon_summary")


usda95 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1995.txt", sep="")
usda95 <- read.csv(usda95, header=FALSE, sep="|")
usda95 <- data.frame(usda95)
colnames(usda95) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_95 <- subset(usda95, state == "WA")
usda_ID_95 <- subset(usda95, state == "ID")
usda_OR_95 <- subset(usda95, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_95, file="1995_monthly_usda_Washington_summary")
write.csv(usda_ID_95, file="1995_monthly_usda_Idaho_summary")
write.csv(usda_OR_95, file="1995_monthly_usda_Oregon_summary")

usda96 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1996.txt", sep="")
usda96 <- read.csv(usda96, header=FALSE, sep="|")
usda96 <- data.frame(usda96)
colnames(usda96) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_96 <- subset(usda96, state == "WA")
usda_ID_96 <- subset(usda96, state == "ID")
usda_OR_96 <- subset(usda96, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_96, file="1996_monthly_usda_Washington_summary")
write.csv(usda_ID_96, file="1996_monthly_usda_Idaho_summary")
write.csv(usda_OR_96, file="1996_monthly_usda_Oregon_summary")

usda97 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1997.txt", sep="")
usda97 <- read.csv(usda97, header=FALSE, sep="|")
usda97 <- data.frame(usda97)
colnames(usda97) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_97 <- subset(usda97, state == "WA")
usda_ID_97 <- subset(usda97, state == "ID")
usda_OR_97 <- subset(usda97, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_97, file="1997_monthly_usda_Washington_summary")
write.csv(usda_ID_97, file="1997_monthly_usda_Idaho_summary")
write.csv(usda_OR_97, file="1997_monthly_usda_Oregon_summary")

usda98 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1998.txt", sep="")
usda98 <- read.csv(usda98, header=FALSE, sep="|")
usda98 <- data.frame(usda98)
colnames(usda98) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_98 <- subset(usda98, state == "WA")
usda_ID_98 <- subset(usda98, state == "ID")
usda_OR_98 <- subset(usda98, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_98, file="1998_monthly_usda_Washington_summary")
write.csv(usda_ID_98, file="1998_monthly_usda_Idaho_summary")
write.csv(usda_OR_98, file="1998_monthly_usda_Oregon_summary")

usda99 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1999.txt", sep="")
usda99 <- read.csv(usda99, header=FALSE, sep="|")
usda99 <- data.frame(usda99)
colnames(usda99) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_99 <- subset(usda99, state == "WA")
usda_ID_99 <- subset(usda99, state == "ID")
usda_OR_99 <- subset(usda99, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_99, file="1999_monthly_usda_Washington_summary")
write.csv(usda_ID_99, file="1999_monthly_usda_Idaho_summary")
write.csv(usda_OR_99, file="1999_monthly_usda_Oregon_summary")

usda0 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2000.txt", sep="")
usda0 <- read.csv(usda0, header=FALSE, sep="|")
usda0 <- data.frame(usda0)
colnames(usda0) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_0 <- subset(usda0, state == "WA")
usda_ID_0 <- subset(usda0, state == "ID")
usda_OR_0 <- subset(usda0, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_0, file="2000_monthly_usda_Washington_summary")
write.csv(usda_ID_0, file="2000_monthly_usda_Idaho_summary")
write.csv(usda_OR_0, file="2000_monthly_usda_Oregon_summary")







usda1 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2001.txt", sep="")
usda1 <- read.csv(usda1, header=FALSE, sep="|")
usda1 <- data.frame(usda1)
colnames(usda1) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_1 <- subset(usda1, state == "WA")
usda_ID_1 <- subset(usda1, state == "ID")
usda_OR_1 <- subset(usda1, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_1, file="2001_monthly_usda_Washington_summary")
write.csv(usda_ID_1, file="2001_monthly_usda_Idaho_summary")
write.csv(usda_OR_1, file="2001_monthly_usda_Oregon_summary")



usda2 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2002.txt", sep="")
usda2 <- read.csv(usda2, header=FALSE, sep="|")
usda2 <- data.frame(usda2)
colnames(usda2) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_2 <- subset(usda2, state == "WA")
usda_ID_2 <- subset(usda2, state == "ID")
usda_OR_2 <- subset(usda2, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_2, file="2002_monthly_usda_Washington_summary")
write.csv(usda_ID_2, file="2002_monthly_usda_Idaho_summary")
write.csv(usda_OR_2, file="2002_monthly_usda_Oregon_summary")



usda3 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2003.txt", sep="")
usda3 <- read.csv(usda3, header=FALSE, sep="|")
usda3 <- data.frame(usda3)
colnames(usda3) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_3 <- subset(usda3, state == "WA")
usda_ID_3 <- subset(usda3, state == "ID")
usda_OR_3 <- subset(usda3, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_3, file="2003_monthly_usda_Washington_summary")
write.csv(usda_ID_3, file="2003_monthly_usda_Idaho_summary")
write.csv(usda_OR_3, file="2003_monthly_usda_Oregon_summary")


usda4 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2004.txt", sep="")
usda4 <- read.csv(usda4, header=FALSE, sep="|")
usda4 <- data.frame(usda4)
colnames(usda4) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_4 <- subset(usda4, state == "WA")
usda_ID_4 <- subset(usda4, state == "ID")
usda_OR_4 <- subset(usda4, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_4, file="2004_monthly_usda_Washington_summary")
write.csv(usda_ID_4, file="2004_monthly_usda_Idaho_summary")
write.csv(usda_OR_4, file="2004_monthly_usda_Oregon_summary")

usda5 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2005.txt", sep="")
usda5 <- read.csv(usda5, header=FALSE, sep="|")
usda5 <- data.frame(usda5)
colnames(usda5) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_5 <- subset(usda5, state == "WA")
usda_ID_5 <- subset(usda5, state == "ID")
usda_OR_5 <- subset(usda5, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_5, file="2005_monthly_usda_Washington_summary")
write.csv(usda_ID_5, file="2005_monthly_usda_Idaho_summary")
write.csv(usda_OR_5, file="2005_monthly_usda_Oregon_summary")



usda6 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2006.txt", sep="")
usda6 <- read.csv(usda6, header=FALSE, sep="|")
usda6 <- data.frame(usda6)
colnames(usda6) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_6 <- subset(usda6, state == "WA")
usda_ID_6 <- subset(usda6, state == "ID")
usda_OR_6 <- subset(usda6, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_6, file="2006_monthly_usda_Washington_summary")
write.csv(usda_ID_6, file="2006_monthly_usda_Idaho_summary")
write.csv(usda_OR_6, file="2006_monthly_usda_Oregon_summary")

usda7 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2007.txt", sep="")
usda7 <- read.csv(usda7, header=FALSE, sep="|")
usda7 <- data.frame(usda7)
colnames(usda7) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_7 <- subset(usda7, state == "WA")
usda_ID_7 <- subset(usda7, state == "ID")
usda_OR_7 <- subset(usda7, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_7, file="2007_monthly_usda_Washington_summary")
write.csv(usda_ID_7, file="2007_monthly_usda_Idaho_summary")
write.csv(usda_OR_7, file="2007_monthly_usda_Oregon_summary")


usda8 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2008.txt", sep="")
usda8 <- read.csv(usda8, header=FALSE, sep="|")
usda8 <- data.frame(usda8)
colnames(usda8) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_8 <- subset(usda8, state == "WA")
usda_ID_8 <- subset(usda8, state == "ID")
usda_OR_8 <- subset(usda8, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_8, file="2008_monthly_usda_Washington_summary")
write.csv(usda_ID_8, file="2008_monthly_usda_Idaho_summary")
write.csv(usda_OR_8, file="2008_monthly_usda_Oregon_summary")


usda9 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2009.txt", sep="")
usda9 <- read.csv(usda9, header=FALSE, sep="|")
usda9 <- data.frame(usda9)
colnames(usda9) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_9 <- subset(usda9, state == "WA")
usda_ID_9 <- subset(usda9, state == "ID")
usda_OR_9 <- subset(usda9, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_9, file="2009_monthly_usda_Washington_summary")
write.csv(usda_ID_9, file="2009_monthly_usda_Idaho_summary")
write.csv(usda_OR_9, file="2009_monthly_usda_Oregon_summary")

usda10 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2010.txt", sep="")
usda10 <- read.csv(usda10, header=FALSE, sep="|")
usda10 <- data.frame(usda10)
colnames(usda10) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_10 <- subset(usda10, state == "WA")
usda_ID_10 <- subset(usda10, state == "ID")
usda_OR_10 <- subset(usda10, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_10, file="2010_monthly_usda_Washington_summary")
write.csv(usda_ID_10, file="2010_monthly_usda_Idaho_summary")
write.csv(usda_OR_10, file="2010_monthly_usda_Oregon_summary")

usda11 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2011.txt", sep="")
usda11 <- read.csv(usda11, header=FALSE, sep="|")
usda11 <- data.frame(usda11)
colnames(usda11) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_11 <- subset(usda11, state == "WA")
usda_ID_11 <- subset(usda11, state == "ID")
usda_OR_11 <- subset(usda11, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_11, file="2011_monthly_usda_Washington_summary")
write.csv(usda_ID_11, file="2011_monthly_usda_Idaho_summary")
write.csv(usda_OR_11, file="2011_monthly_usda_Oregon_summary")

usda12 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2012.txt", sep="")
usda12 <- read.csv(usda12, header=FALSE, sep="|")
usda12 <- data.frame(usda12)
colnames(usda12) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_12 <- subset(usda12, state == "WA")
usda_ID_12 <- subset(usda12, state == "ID")
usda_OR_12 <- subset(usda12, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_12, file="2012_monthly_usda_Washington_summary")
write.csv(usda_ID_12, file="2012_monthly_usda_Idaho_summary")
write.csv(usda_OR_12, file="2012_monthly_usda_Oregon_summary")

usda13 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2013.txt", sep="")
usda13 <- read.csv(usda13, header=FALSE, sep="|")
usda13 <- data.frame(usda13)
colnames(usda13) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_13 <- subset(usda13, state == "WA")
usda_ID_13 <- subset(usda13, state == "ID")
usda_OR_13 <- subset(usda13, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_13, file="2013_monthly_usda_Washington_summary")
write.csv(usda_ID_13, file="2013_monthly_usda_Idaho_summary")
write.csv(usda_OR_13, file="2013_monthly_usda_Oregon_summary")

usda14 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2014.txt", sep="")
usda14 <- read.csv(usda14, header=FALSE, sep="|")
usda14 <- data.frame(usda14)
colnames(usda14) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_14 <- subset(usda14, state == "WA")
usda_ID_14 <- subset(usda14, state == "ID")
usda_OR_14 <- subset(usda14, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_14, file="2014_monthly_usda_Washington_summary")
write.csv(usda_ID_14, file="2014_monthly_usda_Idaho_summary")
write.csv(usda_OR_14, file="2014_monthly_usda_Oregon_summary")

usda15 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2015.txt", sep="")
usda15 <- read.csv(usda15, header=FALSE, sep="|")
usda15 <- data.frame(usda15)
colnames(usda15) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usda_WA_15 <- subset(usda15, state == "WA")
usda_ID_15 <- subset(usda15, state == "ID")
usda_OR_15 <- subset(usda15, state == "OR")
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
write.csv(usda_WA_15, file="2015_monthly_usda_Washington_summary")
write.csv(usda_ID_15, file="2015_monthly_usda_Idaho_summary")
write.csv(usda_OR_15, file="2015_monthly_usda_Oregon_summary")

