env_convex_naif <- function(list,t){
  M=list
  f=c()
  i=1
  bonx=c()
  bony=c()
  plot(M[1,],M[2,])
  while (i<length(M[1,])+1){
    l=c()
    v1=c(M[,i])
    v2=v1
    j=1
    while (j<length(M[1,])+1){
      if (j!=i){
        l=c()
        v2=c(M[,j])
        h=1
        while (h<length(M[1,])+1){
          if (h!=i){
            if (h!=j){
              v3=c(M[,h])
              l=c(l,(M[1,j]-M[1,i])*(M[2,h]-M[2,i])-(M[1,h]-M[1,i])*(M[2,j]-M[2,i]))
            }}
          h=h+1 
        }
        cnt_pos=0
        cnt_neg=0
        for (s in l){
          if (s>=0){
            cnt_pos=cnt_pos+1
          }}
        for (s in l){
          if (s<=0){
            cnt_neg=cnt_neg+1
          }}
        if (cnt_pos==length(l)){
          f=c(f,v1)
          lines(c(v1[1],v2[1]),c(v1[2],v2[2]),'l')
        }
        if (cnt_neg==length(l)){
          f=c(f,v1)
          lines(c(v1[1],v2[1]),c(v1[2],v2[2]),'l')
        }
        
      }
      j=j+1
    }
    i=i+1
  }
  f=matrix(f,nrow = 2)
  k=1
  while (k<length(f[1,])+1){
    k2=k+1
    while (k2<length(f[1,])+1){
      if (f[1,k]==f[1,k2] && f[2,k]==f[2,k2]){
        f=f[,-k2]
        k2=k2-1
      }
      k2=k2+1
    }
    k=k+1
  }
  k=1
  while (k<length(f[1,])){
    k2=k+1
    d=c()
    while (k2<length(f[1,])+1){
      d=c(d,((f[1,k]-f[1,k2])^2 +(f[2,k]-f[2,k2])^2))
      k2=k2+1
    }
    ind=1
    g=1
    while (g<length(d)+1){
      if (d[1]>d[g]){
        ind=g
        u=d[1]
        d[1]=d[g]
        d[g]=u
      }
      g=g+1
    }
    vec=f[,k+ind]
    vec2=f[,k+1]
    f[1,k+1]=vec[1]
    f[2,k+1]=vec[2]
    f[1,k+ind]=vec2[1]
    f[2,k+ind]=vec2[2]
    k=k+1
  }
  return(f)
}

dessin_env_naif <- function(l){
  env=env_convex_naif(l)
  plot(l[1,],l[2,])
  lines(env[1,],env[2,],'l')
  lines(c(env[1,length(env[1,])],env[1,1]),c(env[2,length(env[1,])],env[2,1]),'l')
}
