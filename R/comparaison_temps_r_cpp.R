temps_r_cpp <- function(n){
  dt = data.frame()
  graham = {}
  graham_c = {}
  naive = {}
  naive_c = {}
  javis = {}
  javis_c ={}
  for (i in 1:n){
    list=matrix( runif( 2 * i), nrow=2)
    graham = c(graham, temps_graham(list))
    graham_c = c(graham_c,  temps_graham_rcpp(list))
    naive = c(naive, temps_naive(list))
    naive_c = c(naive_c, temps_naive_rcpp(list))
    javis = c(javis, temps_javis_r(list))
    javis_c = c(javis_c, temps_javis_rcpp(list))
  }
  df = data.frame( "Grham_r" = graham, "Graham_c++" = graham_c,
                   "naive_r" = naive, "naivs_c++" = naive_c,
                   "javis_r" = javis, "javis_c++" = javis_c)
  return(df)
}
