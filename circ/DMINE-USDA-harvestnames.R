library(XML)
library(httr)
library(rvest)
library(plyr)

setwd("/git/data/USDA/pdfs/")
path <- setwd("/git/data/USDA/pdfs/")

wget_string = "wget -N --no-parent http://www.rma.usda.gov/data/indemnity"
system(wget_string,intern=TRUE)

system("find /git/data/USDA -name '*table.pdf' -exec cp {} /git/data/USDA/pdfs/ \\;")

# make a vector of PDF file names
myfiles <- list.files(path = path, pattern = "table.pdf",  full.names = TRUE)

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
lapply(myfiles, function(i) system(paste('"pdftotext"', '"', '"-raw"', 
                                         paste0('"', i, '"')), wait = FALSE) )

totalrows <- nrow(x)
totalpages <- (totalrows/7)/61


x <- data.frame(read.table("021616table.txt", header=F, fill=T, sep=" "))
#x <- data.frame(read.table("021616table.txt", header=T))
#x <- data.frame(read.table("82911table.txt"))
x <- data.frame(x[-grep("\\Page",x$V1),])
x <- data.frame(x[-grep("\\(As",x$V1),])

x <- data.frame(x[-grep("\\cd",x$x..grep.....As...x.x..grep....Page...x.V1........),])


--
totalrows <- nrow(x)
totalpages <- (totalrows/7)/61
  
  
n <- 62
sequence_x <- seq_along(x)
d1 <- split(x, ceiling(sequence_x/n))

library(XML)
doc <- xmlParse(x)
sapply(xpathApply(doc, "//*/Page"), xmlValue)


