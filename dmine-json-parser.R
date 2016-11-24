library(partykit) 
json_prsr <- function(tree, node = 1, node_stats = NULL){
  
  # Checking the decision tree object
  if(!is(tree, c("constparty","party")))
    tree <- partykit::as.party(tree)
  
  # Parsing into json format
  str  <- ""
  rule <- partykit:::.list.rules.party(tree, node)
  
  
  if(is.null(node_stats))
    node_stats <- table(tree$fitted[1])
  children <- partykit::nodeids(tree, node)
  
  if (length(children) == 1) {
    ct  <- node_stats[as.character(children)]
    str <- paste("{","'name': '",children,"','size':",ct,",'rule':'",rule,"'}", sep='')
  } else {
    
    str <- paste("{","'name': '", node,"', 'rule': '", rule, "', 'children': [", sep='')
    for(child in children){
      check <- paste("{'name': '", child, "'", sep='')
      if(child != node & ( !grepl(check, str, fixed=TRUE) ) ) {
        child_str <- json_prsr(tree, child, node_stats)
        str <- child_str
        #str <- paste(str, child_str, ',', sep='')
      }
    }
    str <- substr(str, 1, nchar(str)-1) #Remove the comma
    str <- paste(str,"]}", sep='')
  }
  return(str)
}