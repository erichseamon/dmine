######### Function to set up GAM models
CreateGAMFormula <- function(data, y, s=0.6, type="regspline"){
  names <- names(data[,!(names(data) %in% y)])
  if (length(names)>0){
    for (i in 1:length(names)){
      if (i==1){
        if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
          Formula <- paste0(y," ~", names[i])     
        } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
          Formula <- paste0(y," ~", names[i])     
        } else{
          if (type=="loess"){
            Formula <- paste0(y," ~ lo(", names[i],",span=", s, ")") 
          } else if (type=="regspline"){
            Formula <- paste0(y," ~ s(", names[i],",bs='ps'",",sp=", s, ")") 
          } else{
            Formula <- paste0(y," ~ s(", names[i],",bs='ps')") 
          }
        }
      } else{
        if (is.factor(data[[names[i]]]) | is.character(data[[names[i]]])){
          Formula <- paste0(Formula, "+ ",names[i])
        } else if (is.binary(data[[names[i]]]) | length(unique(data[[names[i]]]))<4){
          Formula <- paste0(Formula, "+ ",names[i])
        } else{
          if (type=="loess"){
            Formula <- paste0(Formula, "+ lo(",names[i],",span=",s,")")  
          } else if (type=="regspline"){
            Formula <- paste0(Formula, "+ s(",names[i],",bs='ps'",",sp=",s,")")  
          } else{
            Formula <- paste0(Formula, "+ s(",names[i],",bs='ps')")  
          }
        }
      }
    }
  } 
  return(as.formula(Formula))
}
