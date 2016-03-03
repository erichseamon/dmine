# folder with 1000s of PDFs
dest <- "/nethome/erichs/pdfminer"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# note that my pdftotext.exe is in a different location to yours
 x <- system(paste('"pdftotext"',
             paste0('"', i, '"')), wait = FALSE) )

#lapply(myfiles, function(i) system(paste('"pdftotext"',
#                                         paste0('"', i, '"')), wait = FALSE) )



x <- data.frame(read.table("82911table.txt"))
x <- data.frame(x[-grep("\\Page",x$V1),])
x <- data.frame(x[-grep("\\(As",x$V1),])

rename(x, c("V1"))
