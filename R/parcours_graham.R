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
  i=1
  plot(l[1,], l[2,],
       xlab = "x", ylab = "y",main = "Enveloppe convexe des points affichés",
       pch = 19, frame = FALSE)
  lines(p[1,],p[2,],'l')
  lines(c(p[1,length(p[1,])],p[1,1]),c(p[2,length(p[1,])],p[2,1]),'l')
  return(p)
}


