#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object

using namespace std;
#include<vector> //to use std::vector<double>
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
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


std::vector<vector<double>> dist_Rcpp(std::vector<vector<double>> v1, std::vector<vector<double>> v2)
{
  vector<vector<double>> res;
  for(int i=0;i<v1.size();i++){
    for(int j=0;j<v1[i].size();j++){
      res[i][j] = v2[i][j] - v1[i][j];
    }
  }
  return res;
}



std::vector<double> fusion_sort(std::vector<double> v) // time complexity nlog(n)
{
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)

*/



