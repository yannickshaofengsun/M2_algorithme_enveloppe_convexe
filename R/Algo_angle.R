angle_2dim = function(l){                          
  l_ordonnée=l[2,]
  i=1
  j=c(1)
  min=l_ordonnée[1]
  while (i<length(l_ordonnée)+1){
    if (min>l_ordonnée[i]){
      min=l_ordonnée[i]
      j=c(i)
    }
    i=i+1
  }
  i=1
  mins=c()
  while (i<length(l_ordonnée)+1){
    if (min==l_ordonnée[i]){
      mins=c(mins,i)
    }
    i=i+1
  }
  i=1
  minabs=l[1,mins[1]]
  if (length(mins)>1){
      while (i<length(mins)+1){
        if (minabs>l[1,mins[i]]){
          minabs=l[1,mins[i]]
          j=mins[i]
        }
        i=i+1
    }
    }
  i=1
  p=l[,j]
  list_angle=matrix(nrow = 2,ncol = length(l[1,]))
  while (i<length(l[1,])+1){
    if (i == j){
      list_angle[,i]=c(NA,i)
      }
    else if (l[1,i]==p[1]){
      list_angle[,i]=c(Inf,i)
    }
    else{
      list_angle[,i]=c((l[2,i]-p[2])/(l[1,i]-p[1]),i)
    }
    i=i+1
  }
  i=1
  while (i<length(list_angle[1,])+1){
    if (is.na(list_angle[1,i])){
      list_angle=list_angle[,-i]
    }
    i=i+1
  }
  i=1
  if (length(list_angle[1,])==1){
    list_angle=t(list_angle)
  }
  return(list_angle)
}


