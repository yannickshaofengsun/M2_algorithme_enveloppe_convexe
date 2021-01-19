# Input X have formate ncol = 2

min_X<-function(X){
  first=which.min(X[,1])
  test=X[first,]
  if (length(first)==1){first}
  else {first=which.min(test[,2])}
  return (first)
}

#A=min_X(X)

produit_vectoriel<-function(A, B, C)
{
  pv=(B[1] - A[1]) * (C[2] - A[2]) - (C[1] - A[1]) * (B[2] - A[2])
  if (pv >0) {q=1}
  else if (pv<0){q=-1}
  else {q=0}
  return (q)
}


next_point<-function(X,i){
  if (i==1) {k=2}  # on initialise par un vecteur autre que i
  else {k=1}
  for (j in 1:length(X[,1])){
    if (produit_vectoriel(X[i,],X[j,],X[k,])>0)
    {k=j}
  }
  return (k)
}


env_conv <- function(X){
  first = min_X(X)
  nextpt = next_point(X,first)
  env = c(first,nextpt)
  while (nextpt!=first){
    nextpt = next_point(X,nextpt)
    env = c(env,nextpt)
    }
  res = matrix(nrow=length(env),ncol=2)
  
  for (i in 1:length(env))
    res[i,] = X[env[i],]
  return (res)
}

