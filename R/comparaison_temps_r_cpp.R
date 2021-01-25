temps_r_cpp_graham <- function(n){
  dt = data.frame()
  graham = {}
  graham_c = {}
  for (i in 3:n){
    list=matrix( runif( 2 * i), nrow=2)
    graham = c(graham, temps_graham(list))
    graham_c = c(graham_c,  temps_graham_rcpp(list))
  }
  df = data.frame( "Grham_r" = graham, "Graham_c++" = graham_c)
  return(df)
}

temps_r_cpp_javis <- function(n){
  dt = data.frame()
  javis = {}
  javis_c ={}
  for (i in 3:n){
    list=matrix( runif( 2 * i), nrow=2)
    javis = c(javis, temps_javis_r(list))
    javis_c = c(javis_c, temps_javis_rcpp(list))
  }
  df = data.frame( "javis_r" = javis, "javis_c++" = javis_c)
  return(df)
}

temps_r_cpp_naif <- function(n){
  dt = data.frame()
  naive = {}
  naive_c = {}
  for (i in 3:n){
    list=matrix( runif( 2 * i), nrow=2)
    naive = c(naive, temps_naive(list))
    naive_c = c(naive_c, temps_naive_rcpp(list))
  }
  df = data.frame( "naive_r" = naive, "naivs_c++" = naive_c)
  return(df)
}
