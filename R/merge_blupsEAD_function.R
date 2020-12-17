#' @title Merge blupAD + blupEAD effects
#' @description 
#' @author HFC
#' @param x data frame of AD blups
#' @param y data frame of EAD blups
#' 

merge_blupsEAD <- function(x, y){
  
  # parameters
  AD  <- x
  EAD <- y
  
  blup <- vector()
  for(i in 1:dim(EAD)[1]){
    for(j in 1:dim(AD)[1]){
      ifelse(EAD[i,2]==rownames(AD)[j],
             blupM <- EAD[i,1] + AD[j,1],
             next)
      blup <- c(blup, blupM)
    }
  }
    
  blupSs<-data.frame(ID = names(blup), blup)
  
  return(blupSs)
  
}

