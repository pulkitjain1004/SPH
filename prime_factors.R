
is.prime <- function(num) {
  
  
  
  if (num == 2) {
    1
  } else if (any(num %% 2:(num-1) == 0)) {
    0
  } else { 
    1
  }
}

prime_factors <- function(x){
  
  is.prime <- function(num) {
    if (num == 2) {
      1
    } else if (any(num %% 2:(num-1) == 0)) {
      0
    } else { 
      1
    }
  }
  
  a=0  
  k=1
  for(i in 1:x){
    if(x %% i == 0){
      if(is.prime(i) == 1){
         a[k] = i
         # print(i)
         k= k+1
      }
    }
  }
  return(a)
}


