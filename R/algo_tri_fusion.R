fusion<-function(a,b) {
  r2<-numeric(length(a[1,])+length(b[1,]))
  r<-numeric(length(a[1,])+length(b[1,]))
  ai<-1
  bi<-1 
  j<-1
  for(j in 1:length(r)) {
    if((ai<=length(a[1,]) && a[1,][ai]<b[1,][bi]) || bi>length(b[1,])) {
      r[j] <- a[1,][ai]
      r2[j] <- a[2,][ai]
      ai <- ai+1
    } else {
      r[j] <- b[1,][bi]
      r2[j] <- b[2,][bi]
      bi <- bi+1          
    }
  }
  t(matrix(c(r,r2),ncol = 2))
}

trifusion<-function(A) {
  if(length(A[1,])>1) {
    q <- ceiling(length(A[1,])/2)
    a <- trifusion(matrix(A[,1:q],nrow = 2))
    b <- trifusion(matrix(A[,(q+1):length(A[1,])],nrow = 2))
    fusion(a,b)
  } else {
    A
  }
}
