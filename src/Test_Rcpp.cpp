#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericMatrix col_erase2(NumericMatrix x, int colID) {
  NumericMatrix x2(Dimension(x.nrow(), x.ncol()- 1));
  int dec = 0; 
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID) {
      x2(_,i-dec) = x(_,i);
    }
    else{dec=1;}
    }
  return x2;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo2(42)
*/
