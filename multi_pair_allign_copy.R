m.lev.dist = function(s1,s2,s3){
#s1="saturday"; s2="sunday"; s3 = "saturdays"
   
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
t1temp = 1:max(m,n)   #R will automaticall add columns if required
toString(t1temp)
t2temp = 1:max(m,n)   #R will automaticall add columns if required
toString(t2temp)

# print('Traceback direction from last character')

while(i>1 || j>1){
  
  if(i>1 && j > 1){
    if(D[i-1,j-1]<=D[i-1,j] && D[i-1,j-1]<=D[i,j-1] ){
      
      #diagnol = ifelse(D[i,j]>D[i-1,j-1],0,1); 
      print("diagonal")
      i=i-1;j=j-1
      t1temp[k]=t1[i]
      t2temp[k]=t2[j]
      k=k+1
    }
    
    else if(D[i-1,j] < D[i,j-1]){
      i=i-1;j=j;
      print("up")
      t1temp[k]=t1[i]
      t2temp[k]="_"
      k = k+1
      
    }
    else {
      i=i;j=j-1
      print("left")
      t1temp[k]="_"
      t2temp[k]=t2[j]
      k=k+1
    }
    next
  }
  if(i==1){
    i=i;j=j-1
    print("left")
    t1temp[k]="_"
    t2temp[k]=t2[j]
    k=k+1
  }
  if(j==1){
    i=i-1;j=j;
    print("up")
    t1temp[k]=t1[i]
    t2temp[k]="_"
    k = k+1
  }
}

elements = k-1

t1new = 1:elements
toString(t1new)
t2new = 1:elements
toString(t2new)

#swap
for (i in 1:elements){

  t1new[i] = t1temp[k-i] 
  t2new[i] = t2temp[k-i]
}


# create profile
ch.list = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','_')

prof = matrix(0,27,elements)
rownames(prof) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','_')

for (i in 1:elements){
  
  pos1 = grep(t1new[i],ch.list)
  prof[pos1,i]=prof[pos1,i]+1
  
  pos2 = grep(t2new[i],ch.list)
  prof[pos2,i]=prof[pos2,i]+1
  
}

#Compare prof with new string


m.new = nchar(s3) 
n.new = ncol(prof)

t3.new = strsplit(s3, "")[[1]]

D.new = matrix(0,m.new+1,n.new+1)
D.new[,1] = 0:m.new
D.new[1,] = 0:n.new

rownames(D.new) = c("",t3.new)

for (i in 1:m.new){	
  for (j in 1:n.new){
    
    cost = 0
    for (k in 1:27){
      
      if(t3.new[i]!=ch.list[k]){
        cost = cost+prof[k,j]
      }
    }
    cost = cost/max(colSums(prof))
    
    D.new[i+1,j+1] = cost+min(D.new[i,j+1],D.new[i+1,j],D.new[i,j])
  }
}


#new traceback

i=m.new+1
j=n.new+1
k=1

p1temp = matrix(0,27,max(m.new,n.new))   #R will automaticall add columns if required
#toString(p1temp)
rownames(p1temp) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','_')

p2temp = matrix(0,27,max(m.new,n.new))   #R will automaticall add columns if required
#toString(p2temp)
rownames(p2temp) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','_')

# print('Traceback direction from last character')

while(i>1 || j>1){
  
  if(i>1 && j>1){
    if((D.new[i-1,j-1]<=D.new[i-1,j]) && (D.new[i-1,j-1]<=D.new[i,j-1]) ){
    
    #diagnol = ifelse(D[i,j]>D[i-1,j-1],0,1); 
    print("diagonals")
    i=i-1;j=j-1
    
    #p1temp[k]=t3[i]
    pos1.temp = grep(t3.new[i],ch.list)
    p1temp[pos1.temp,k]=p1temp[pos1.temp,k]+1
    
    
    p2temp[,k]=prof[,j]
    k=k+1
    
    } else if(D.new[i-1,j] < D.new[i,j-1]){
      i=i-1;j=j;
      print("ups")
    
      #t1temp[k]=t1[i]
    
      pos1.temp = grep(t3.new[i],ch.list)
      p1temp[pos1.temp,k]=p1temp[pos1.temp,k]+1
    
      p2temp[27,k]=p2temp[27,k]+1
      k = k+1
    } else {
      i=i;j=j-1
      print("lefts")
    
      #t1temp[k]="_"
      p1temp[27,k] = p1temp[27,k]+1
    
      #t2temp[k]=t2[j]
      k
      j
      p2temp[,k]=prof[,j]
    
      k=k+1
    }
    next
  }
  if(i==1){
    i=i;j=j-1
    print("lefts")
    
    #t1temp[k]="_"
    p1temp[27,k] = p1temp[27,k]+1
    
    #t2temp[k]=t2[j]
    k
    j
    p2temp[,k]=prof[,j]
    
    k=k+1
    next
  }
  if(j==1){
    i=i-1;j=j;
    print("ups")
    
    #t1temp[k]=t1[i]
    
    pos1.temp = grep(t3.new[i],ch.list)
    p1temp[pos1.temp,k]=p1temp[pos1.temp,k]+1
    
    p2temp[27,k]=p2temp[27,k]+1
    k = k+1
  }
}

elements.new = k-1
prof.new = matrix(0,27,elements.new)
rownames(prof.new) = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','_')


for (i in 1:elements.new){

  prof.new[,i] = p1temp[,k-i] + p2temp[,k-i]

}


results = list(D, t1new, t2new, prof, D.new, p1temp, p2temp, prof.new )
return(results)

}

m.lev.dist("adfg","adf","d")
