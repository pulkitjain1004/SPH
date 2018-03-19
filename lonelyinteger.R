lonelyinteger <- function(s){
  s = sort(s)
  for (i in seq(1,length(s),2)) {
    
    if(i == length(s)){
      return(s[i])
    }
  
    if(any(s[i] == s[(i+1):length(s)]) ){
      
    } else{
      return(s[i])
    } 
  
  }
}
