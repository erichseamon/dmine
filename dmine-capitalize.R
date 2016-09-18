

DTnew <- data.frame(tolower(DT$commodity))

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

DTnew2 <- data.frame(capFirst(DTnew$tolower.DT.commodity.))
colnames(DTnew2) <- c("commodity2")
DTnew2$commodity <- as.character(DTnew2$commodity)

DTnew2$commodity[DTnew2$commodity == "All other crops"] <- "All Other Crops"

DTnew3 <- cbind(DT, DTnew2)
DTnew3$commodity <- DTnew3$commodity2
