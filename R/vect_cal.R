
vect_dis <- function (v1, v2){
  res =array(dim = c(length(v1)))
  for (i in 1:length(v1)){
    res[i] = v2[i] - v1[i]
  }
  return (res)
}

vect_angle<- function(v){
  ang = c()
  for (i in 1:dim(v)[1]){
    div = v[i,][2]/v[i,][1]
    ang = c(ang, atan(div))
  }
  return (ang)
}

vect_ang <- function (v){
  div = atan(v[1,][2]/v[1,][1])
  return (div)
}
# match permet de trouver les index de cordonne des points, donc nous donne l'ordre des points

vect_produit <- function (v1, v2, v3){
  # v1 est le point milieu
  s = (v2[1,][1] - v1[1,][1])*(v3[1,][2] - v1[1,][2]) - (v2[1,][2] - v1[2])*(v3[1] - v1[1])
  return (s)
}



graham_ini <- function(v){
  dims <- dim(v)
  pot_y <- c(1) ;   pot_x = c(1)
  # trouver le ou les plus petits y
  for (i in 2:dims[1]){
    if (v[,2][i] < v[,2][pot_y[1]]){
      pot_y = i
    }

    else if (v[,2][i] == v[,2][pot_y[1]]){
      pot_y = c(pot_y, i)
    }
  }

  # s'il y a plusieur plus petit y, on cherche le plus petit x
  if (length(pot_y) == 1){
    return (v[pot_y,])
  }
  else{
    for (j in 1:length(pot_y)){
      if(v[,1][pot_y[j]] < v[,1][pot_x])
        pot_x = j
    }
    return (v[pot_x,])
  }
}


mmerge <- function(a,b) {
    r<-array(dim = c((dim(a)[1]+dim(b)[1]),))
    ai<-1; bi<-1; j<-1;
    for(j in 1:dim(r)[1]) {
      if((ai<=dim(a)[1] && vect_ang(a[ai,])<b[bi]) || bi>dim(b)[1]) {
        r[j,] <- a[ai,]
        ai <- ai+1
      } else {
        r[j,] <- b[bi,]
        bi <- bi+1
      }
    }
    r
}

mmergesort <- function(A) {
  if(length(A)>2) {
    q <- ceiling(dim(A)[1]/2)
    a <- mmergesort(A[1:q,])
    b <- mmergesort(A[(q+1):dim(A)[1],])
    mmerge(a,b)
  } else {
    array
  }
}

vect_trans <- function (v){
  if (length(v) == 2){
    v = array(v, dim= c(1,2))
  }
  else{v}
  v
}
