base2 <- function(x){

  if(x == 0){
    return(list(0,0))
  }
  if(x == 1){
    return(list(1,1))
  }
  
  coef <- 0
  i <- 1
  
  while(x %/% 2 > 0){
    coef[i] <- x %% 2
    x <- x %/% 2
    i <- i + 1
  }
  
  coef <- c(coef,1)
  
  # swap coefficients
  coef_temp <- coef
  n <- length(coef)
  
  for(i in 1: n){
    coef[i] <- coef_temp[n+1-i]
  }
  
  # retrieve number from coefficients
  result <- 0
  for(i in 1: n){
    result = coef[n+1-i]*2^(i-1) + result
  }
  
  result <- list(result, coef)
  return(result)
  
}
