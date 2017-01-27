#this works
#s1 = "hello"
  #readline(prompt="Enter First String: ")
#s2 = "hell"
  #readline(prompt="Enter Second String: ")

s.lev.dist = function(s1,s2){

if (nchar(s1) == 0){
	dist = nchar(s2)
} else if (nchar(s2) == 0){
	dist = nchar(s1)
} else
   
   m = nchar(s1)
   n = nchar(s2)
    
   t1 = strsplit(s1, "")[[1]]
   t2 = strsplit(s2, "")[[1]]
   
   D = matrix(0,m+1,n+1)
   D[,1] = 0:m
   D[1,] = 0:n
   
	for (i in 1:m){	
		for (j in 1:n){
			a = D[i,j+1]+1
			b = D[i+1,j]+1
			c= D[i,j] + ifelse(t1[i]==t2[j], 0, 1)
		
			D[i+1,j+1] = min(a,b,c)
		}
	}
  return(D[m+1,n+1])
}

s.lev.dist("Hell","hello")