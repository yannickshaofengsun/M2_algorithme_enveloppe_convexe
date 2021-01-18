M=matrix( runif( 2 * 100), nrow=2)
f=env_convex_naif(M,1)
dessin_env_naif(M)
i=5
t=0
while (i<500){ 
  start_time <- Sys.time()
  M=matrix( runif( 2 * i), nrow=2)
  parcours_graham(M)
  end_time <- Sys.time()
  t=c(t,end_time - start_time)
  i=i+1
}
u=seq(1,500,length.out = length(t))
u=seq(1,length(t)+1,length.out = length(t))
dev.off()
plot(u,t)
lm(t~0+u)
lines(u,0.0001108*u,col='red')
lm(t~0+u*log(u))
lines(u,-1.472e-04*u+1.175e-03*log(u)+0.00004047*u*log(u),col='green')

