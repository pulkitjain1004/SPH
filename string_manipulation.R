# practice strings

# remove everything but base

string_swap <- function(s1){
  
  n <- nchar(s1)
  s1 <- strsplit(s1, '')[[1]]
  s2 <- s1
  for(i in 1:n){
    s2[i] <- s1[n+1-i]
  }
  
  s2 <- paste(s2, sep = "", collapse = "")
  
  return(s2)
}
