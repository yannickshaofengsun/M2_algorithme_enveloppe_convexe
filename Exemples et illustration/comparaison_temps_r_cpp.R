temps_r_cpp <- function(n){
  i=10
  t_r=0
  t_cpp=0
  while (i<n){
    list=matrix( runif( 2 * i), nrow=2)
    start_time_r<- Sys.time()
    env_convex_naif(list)
    end_time_r<-Sys.time()
    t_r = t_r + end_time_r - start_time_r
    start_time_cpp<- Sys.time()
    naif_convex_rcpp(list)
    end_time_cpp<-Sys.time()
    t_cpp = t_cpp + end_time_cpp - start_time_cpp
    i=i+1
  }
  return(t_r/ t_cpp)
}