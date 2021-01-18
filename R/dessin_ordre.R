labeltri<-function(l) {
  M=trifusion(angle_2dim(l))
  M2=M[,M[1,]>=0]
  M2=matrix(M2,nrow = 2 )
  M3=M[,M[1,]<0]
  M3=matrix(M3,nrow  = 2)
  M=cbind(M2,M3)
  i=1
  vect1=matrix(ncol = 1,nrow = length(M[1,])+1)
  vect2=matrix(ncol = 1,nrow = length(M[1,])+1)
  while (i<length(M[1,])+1){
    vect1[M[2,i]]=i+1
    vect2[i+1]=M[2,i]
    i=i+1
  }
  vect1[is.na(vect1)]=1
  i=1
  while (i<length(M[1,])+2){
    if (vect1[i]==1){
      vect2[1]=i
    }
    i=i+1
  }
  return(matrix(c(vect1,vect2),ncol = 2)) }

dessin_ordre<-function(l){
  v=labeltri(l)[,1]
  plot(l[1,], l[2,],
       xlab = "x", ylab = "y",
       pch = 19, frame = FALSE)
  text(l[1,], l[2,], labels=v, cex= 1,pos=4)
}
