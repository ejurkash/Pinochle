has_pinochle <- function(vec, i){
  if("JD" %in% vec[[i]] & "QS" %in% vec[[i]] & length(grep("QS", vec[[i]])) + length(grep("JD", vec[[i]])) != 4){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}




double_pinochle <- function(vec, i){
  if(length(grep("QS", vec[[i]])) == 2 & length(grep("JD", vec[[i]])) == 2){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}



marriages <- function(vec, i){
  count = 0
  if("KS" %in% vec[[i]] & "QS" %in% vec[[i]]){
    if(length(grep("KS", vec[[i]])) + length(grep("QS", vec[[i]])) == 4){
      count = count + 2
    }
    else{
      count = count + 1
    }
  }
  if("KH" %in% vec[[i]] & "QH" %in% vec[[i]]){
    if(length(grep("KH", vec[[i]])) + length(grep("QH", vec[[i]])) == 4){
      count = count + 2
    }
    else{
      count = count + 1
    }
  }
  if("KC" %in% vec[[i]] & "QC" %in% vec[[i]]){
    if(length(grep("KC", vec[[i]])) + length(grep("QC", vec[[i]])) == 4){
      count = count + 2
    }
    else{
      count = count + 1
    }
  }
  if("KD" %in% vec[[i]] & "QD" %in% vec[[i]]){
    if(length(grep("KD", vec[[i]])) + length(grep("QD", vec[[i]])) == 4){
      count = count + 2
    }
    else{
      count = count + 1
    }
  }
  return(count)
}



jacks <- function(vec, i){
  if("JS" %in% vec[[i]] & "JD" %in% vec[[i]] & "JC" %in% vec[[i]] & "JH" %in% vec[[i]] & length(grep("J", vec[[i]])) != 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}




forty_jacks <- function(vec, i){
  if(length(grep("J", vec[[i]])) == 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}





queens <- function(vec, i){
  if("QS" %in% vec[[i]] & "QD" %in% vec[[i]] & "QC" %in% vec[[i]] & "QH" %in% vec[[i]] & length(grep("Q", vec[[i]])) != 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}




sixty_queens <- function(vec, i){
  if(length(grep("Q", vec[[i]])) == 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}






kings <- function(vec, i){
  if("KS" %in% vec[[i]] & "KD" %in% vec[[i]] & "KC" %in% vec[[i]] & "KH" %in% vec[[i]] & length(grep("K", vec[[i]])) != 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


eighty_kings <- function(vec, i){
  if(length(grep("K", vec[[i]])) == 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}







aces <- function(vec, i){
  if("AS" %in% vec[[i]] & "AD" %in% vec[[i]] & "AC" %in% vec[[i]] & "AH" %in% vec[[i]] & length(grep("A", vec[[i]])) != 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}






hundred_aces <- function(vec, i){
  if(length(grep("A", vec[[i]])) == 8){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}





run <- function(vec, i){
  if(("AS" %in% vec[[i]] & "10S" %in% vec[[i]] & "KS" %in% vec[[i]] & "QS" %in% vec[[i]] & "JS" %in% vec[[i]]) | ("AD" %in% vec[[i]] & "10D" %in% vec[[i]] & "KD" %in% vec[[i]] & "QD" %in% vec[[i]] & "JD" %in% vec[[i]]) | ("AC" %in% vec[[i]] & "10C" %in% vec[[i]] & "KC" %in% vec[[i]] & "QC" %in% vec[[i]] & "JC" %in% vec[[i]]) | ("AH" %in% vec[[i]] & "10H" %in% vec[[i]] & "KH" %in% vec[[i]] & "QH" %in% vec[[i]] & "JH" %in% vec[[i]])){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}





double_run <- function(vec, i){
  if(length(grep("AS", vec[[i]])) + length(grep("10S", vec[[i]])) + length(grep("KS", vec[[i]])) + length(grep("QS", vec[[i]])) + length(grep("JS", vec[[i]])) == 10 | length(grep("AD", vec[[i]])) + length(grep("10D", vec[[i]])) + length(grep("KD", vec[[i]])) + length(grep("QD", vec[[i]])) + length(grep("JD", vec[[i]])) == 10 | length(grep("AC", vec[[i]])) + length(grep("10C", vec[[i]])) + length(grep("KC", vec[[i]])) + length(grep("QC", vec[[i]])) + length(grep("JC", vec[[i]])) == 10 | length(grep("AH", vec[[i]])) + length(grep("10H", vec[[i]])) + length(grep("KH", vec[[i]])) + length(grep("QH", vec[[i]])) + length(grep("JH", vec[[i]])) == 10){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}





meld <- function(hand = deal()){
  score <- c(0, 0, 0)
  for(i in 1:3){
    if(has_pinochle(hand, i)){
      score[i] = score[i] + 4
    }
    if(double_pinochle(hand, i)){
      score[i] = score[i] + 30
    }
    if(jacks(hand, i)){
      score[i] = score[i] + 4
    }
    if(forty_jacks(hand, i)){
      score[i] = score[i] + 40
    }
    if(queens(hand, i)){
      score[i] = score[i] + 6
    }
    if(sixty_queens(hand, i)){
      score[i] = score[i] + 60
    }
    if(kings(hand, i)){
      score[i] = score[i] + 8
    }
    if(eighty_kings(hand, i)){
      score[i] = score[i] +80
    }
    if(aces(hand, i)){
      score[i] = score[i] + 10
    }
    if(hundred_aces(hand, i)){
      score[i] = score[i] + 10
    }
    #Subtract the 2 from one of the marriages
    if(run(hand, i)){
      score[i] = score[i] + 15 - 2
    }
    
    #For Double Run, subtract 15 for the run and 4 for the double marriage now part of the run
    if(double_run(hand, i)){
      score[i] = score[i] + 150 - 15 - 4
    }
    
    #Adding in the points from marriages
    score[i] = score[i] + marriages(hand, i)*2
  }
  hand
  return(score)
}