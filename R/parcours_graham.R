parcours_graham <- function(l){
  z=labeltri(l)[,2]
  l=l[,z]
  p=l[,1:2]
  i=3
  while (i<length(l[1,])+1){
    while (length(p[1,]) >= 2 && det(matrix(c(p[,length(p[1,])][1]-p[,length(p[1,])-1][1],p[,length(p[1,])][2]-p[,length(p[1,])-1][2],l[,i][1]-p[,length(p[1,])][1],l[,i][2]-p[,length(p[1,])][2]),nrow = 2))<0){
      p=p[,-length(p[1,])]
      
    }
    p=cbind(p,l[,i])
    i=i+1
  }
  return(p)
}

parcours_graham_cpp <- function(l){
  z=labeltri_rcpp(l)[,2]
  l=l[,z]
  p=l[,1:2]
  i=3
  while (i<length(l[1,])+1){
    while (length(p[1,]) >= 2 && det(matrix(c(p[,length(p[1,])][1]-p[,length(p[1,])-1][1],p[,length(p[1,])][2]-p[,length(p[1,])-1][2],l[,i][1]-p[,length(p[1,])][1],l[,i][2]-p[,length(p[1,])][2]),nrow = 2))<0){
      p=p[,-length(p[1,])]
      
    }
    p=cbind(p,l[,i])
    i=i+1
  }
  return(p)
}


dessin_env_graham <- function(l){
  p=parcours_graham(l)
  plot(l[1,], l[2,],
       xlab = "x", ylab = "y",main = "Enveloppe convexe des points affichés par Graham",
       pch = 19, frame = FALSE)
  lines(p[1,],p[2,],'l')
  lines(c(p[1,length(p[1,])],p[1,1]),c(p[2,length(p[1,])],p[2,1]),'l')
}

dessin_env_javis <- function(l){
  p=t(env_convex_javis_cpp(t(l)))
  plot(l[1,], l[2,],
       xlab = "x", ylab = "y",main = "Enveloppe convexe des points affichés par Javis",
       pch = 19, frame = FALSE)
  lines(p[1,],p[2,],'l')
  lines(c(p[1,length(p[1,])],p[1,1]),c(p[2,length(p[1,])],p[2,1]),'l')
}

dessin_env_naiv<- function(l){
  p=t(env_convex_javis_cpp(t(l)))
  plot(l[1,], l[2,],
       xlab = "x", ylab = "y",main = "Enveloppe convexe des points affichés par methode naif",
       pch = 19, frame = FALSE)
  lines(p[1,],p[2,],'l')
  lines(c(p[1,length(p[1,])],p[1,1]),c(p[2,length(p[1,])],p[2,1]),'l')
}
