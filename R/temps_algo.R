temps_javis_rcpp <- function(l){
  start_time <- Sys.time()
  env_convex_javis_cpp(t(l))
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}
  
temps_javis_r <- function(l){
  start_time <- Sys.time()
  env_conv(t(l))
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_naive_rcpp <- function(l){
  start_time <- Sys.time()
  naif_convex_rcpp(l)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_naive<- function(l){
  start_time <- Sys.time()
  env_convex_naif(l)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_graham_rcpp <- function(l){
  start_time <- Sys.time()
  parcours_graham_cpp(l)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}

temps_graham <- function(l){
  start_time <- Sys.time()
  parcours_graham(l)
  end_time <- Sys.time()
  time = end_time - start_time
  return(time)}
