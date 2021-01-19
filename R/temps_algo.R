temps_javis_rcpp <- function(l){
  start_time <- Sys.time()
  env_convex_javis_cpp(t(M))
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}
  
temps_javis_r <- function(l){
  start_time <- Sys.time()
  env_conv(t(M))
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_naive_rcpp <- function(l){
  start_time <- Sys.time()
  naif_convex_rcpp(M)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_naive<- function(l){
  start_time <- Sys.time()
  env_convex_naif(M)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_graham_rcpp <- function(l){
  start_time <- Sys.time()
  parcours_graham_cpp(M)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_graham <- function(l){
  start_time <- Sys.time()
  parcours_graham(M)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}
