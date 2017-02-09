s.lev.dist = function(s1,s2){
#s1="compliment"; s2="complicated"
   
   m = nchar(s1)
   n = nchar(s2)
    
   t1 = strsplit(s1, "")[[1]]
   t2 = strsplit(s2, "")[[1]]
   
   D = matrix(0,m+1,n+1)
   D[,1] = 0:m
   D[1,] = 0:n
   
   rownames(D) = c("",t1)
   colnames(D) = c("",t2)
   
	for (i in 1:m){	
		for (j in 1:n){
			a = D[i,j+1]+1
			b = D[i+1,j]+1
			c= D[i,j] + ifelse(t1[i]==t2[j], 0, 1)
		
			D[i+1,j+1] = min(a,b,c)
		}
	}
  
   
  
i=m+1
j=n+1
k=1
t1temp = 1:max(m,n)
toString(t1temp)
t2temp = 1:max(m,n)
toString(t2temp)

print('Traceback direction from last character')

while(i>1 || j>1){
  
  if((D[i-1,j-1]<=D[i-1,j]) && (D[i-1,j-1]<=D[i,j-1]) ){
    
    #diagnol = ifelse(D[i,j]>D[i-1,j-1],0,1); 
    print("diagonal")
    i=i-1;j=j-1
    t1temp[k]=t1[i]
    t2temp[k]=t2[j]
    k=k+1
    
  } else if(D[i-1,j] < D[i,j-1]){
    i=i-1;j=j;
    print("up")
    t1temp[k]=t1[i]
    t2temp[k]="_"
    k = k+1
  } else {
    i=i;j=j-1
    print("left")
    t1temp[k]="_"
    t2temp[k]=t2[j]
    k=k+1
    }
}

t1new = 1:max(m,n)
toString(t1new)
t2new = 1:max(m,n)
toString(t2new)

for (i in 1:k-1){

  t1new[i] = t1temp[k-i] 
  t2new[i] = t2temp[k-i]
  }
results = list(Lev_Distance = D[m+1,n+1], Lev_Distance_Matrix =D, t1new, t2new )
return(results)

}

s.lev.dist("compliment","complicated")