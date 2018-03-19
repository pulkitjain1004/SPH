odd_between <- function(l,r){
  k=1
  result=0
  
  for (i in (l+1):(r-1)){
    
    if((i %% 2) == 1){
      result[k] <- i
      k=k+1
    }
  }
  return(result)
  
}


number_position <- function(arr){
  
  num <- arr[1]
  for(i in 2:length(arr)){
    
    if(num == arr[i]){
      return(i)
    }
  }
}
