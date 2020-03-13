has_pinochle <- function(vec, i){
  if("JD" %in% vec[[i]] & "QS" %in% vec[[i]] & length(grep("QS", vec[[i]])) + length(grep("JD", vec[[i]])) != 4){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}