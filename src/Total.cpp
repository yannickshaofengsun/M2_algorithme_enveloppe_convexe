#include <Rcpp.h>
using namespace Rcpp;
#include<vector>
#include <limits> // for defining infinity values
using namespace std;

NumericMatrix col_erase(NumericMatrix x, int colID) {
  NumericMatrix x2(Dimension(x.nrow(), x.ncol()- 1));
  int dec = 0;
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID) {
      x2(_,i-dec) = x(_,i);
    }
    else{
      dec=1;
    }
  }
  return x2;
}

NumericVector add(NumericVector v1, NumericVector v2){
  for(int i = 0; i< v2.length(); i++){
    v1.push_back(v2[i]);
  }
  return (v1);
}

// [[Rcpp::export()]]
NumericMatrix fusion_cpp(NumericMatrix  m1, NumericMatrix m2){
  int m1col = m1.cols();
  int m2col = m2.cols();
  
  NumericVector v1 (m1col + m2col);
  NumericVector v2 (m1col + m2col);
  NumericVector s;
  int ai = 0, bi = 0 ;
  for (int j = 0; j < v1.length(); j++ ){
    if(( (ai < m1col) && (m1(0, ai) < m2(0, bi))) || bi>=m2col ){
      double x = m1(0,ai), y = m1(1,ai);
      v1[j] = x;
      v2[j] = y;
      ai = ai+1;
    } else {
      double x = m2(0,bi), y = m2(1,bi);
      v1[j] = x;
      v2[j] = y;
      bi = bi+1;
    }
  }
  s = add(v1, v2);
  NumericMatrix res (v1.length(), 2, s.begin());
  return (transpose(res));
}

// [[Rcpp::export()]]
NumericMatrix trifusion_cpp(NumericMatrix t){
  int m_col = t.cols();
  if (m_col > 1){
    int q = ceil(m_col/2);
    NumericMatrix A = t(Range(0,1), Range(0,q-1));
    NumericMatrix At = trifusion_cpp(A);
    NumericMatrix B = t(Range(0,1), Range(q,m_col-1));
    NumericMatrix Bt = trifusion_cpp(B);
    t = fusion_cpp(At, Bt);
    return (t);
  }
  else{
    return (t);
  }
}

// [[Rcpp::export()]]
NumericMatrix listAngle_cpp(NumericMatrix l){
  NumericVector l_ord = l(1,_);
  int i = 0, j = 0;
  double min = l_ord[0];
  while (i < l_ord.size()){
    if (min > l_ord[i]){
      min = l_ord[i];
      j = i;
    }
    i++;
  }
  i = 0;
  NumericVector min_s;
  while ( i < l_ord.size()){
    if (l_ord[i] == min){
      min_s.push_back(i);
    }
    i++;
  }
  int j_mins;
  double minabs = l(0, min_s[0]);
  i = 0;
  if (min_s.size() > 1){
    while (i < min_s.size()){
      if (minabs > l(0, min_s[i])){
        minabs = l(0, min_s[i]);
        j_mins = min_s[i];
      }
      i++;
    }
  }else{
    j_mins = j;
  }
  i = 0;
  NumericVector na_list;
  NumericVector p = l(_, j_mins);
  NumericMatrix list_angle = NumericMatrix (2, l(0, _).size());
  while (i < l(0, _).size()){
    double i_const = i;
    if (i_const == j_mins){
      NumericVector temp1 = {R_NaN, i_const};
      list_angle(_,i_const) = temp1;
      na_list.push_back(i);
    }
    else if (l(0,i_const) == p[0]){
      NumericVector temp2 = {R_PosInf, i_const};
      list_angle(_,i_const) = temp2;
    }
    else {
      NumericVector temp3 = {(l(1, i_const)-p[1])/(l(0, i_const)-p[0]), i_const};
      list_angle(_,i_const)=temp3;
    }
    i++;
  }
  for (int x = 0; x<na_list.size(); x++){
    list_angle = col_erase(list_angle, na_list[x]);
  }
  i = 0;
  if (list_angle(0,_).size()==1){
    list_angle=transpose(list_angle);
  }
  return (list_angle);
}

// [[Rcpp::export()]]
int min_X_cpp(NumericMatrix X){
  NumericVector l_abs = X(_,0);
  int i = 0, j = 0, minord = 0;
  double min = l_abs[0];
  while (i < l_abs.size()){
    if (min > l_abs[i]){
      min = l_abs[i];
      j = i;
    }
    i++;
  }
  i = 0;
  NumericVector min_s;
  while (i < l_abs.size()){
    if (l_abs[i] == min){
      min_s.push_back(i);
    }
    i++;
  }
  i = 0;
  if (min_s.size() > 1){
    while (i < min_s.size()){
      if (minord > X(min_s[i], 1)){
        minord = X(min_s[i], 1);
        j = min_s[i];
      }
      i++;
    }
  }
  return (j);
}

// [[Rcpp::export()]]
int produit_vect_cpp(NumericVector A, NumericVector B, NumericVector C) {
  double prod_v;
  int dir;
  prod_v = (B[0] - A[0]) * (C[1] - A[1]) - (C[0] - A[0]) * (B[1] - A[1]);
  if (prod_v > 0){
    dir = 1;
  } else if ( prod_v < 0){
    dir = -1;
  }else{
    dir = 0;
  }
  return (dir);
}

// [[Rcpp::export()]]
int next_point_cpp (NumericMatrix X, int i){
  int k;
  if (i == 1){
    k = 2;
  }else {
    k = 1;
  }
  for (int j = 0; j<X(_, 0).size(); j++){
    if (produit_vect_cpp(X(i,_), X(j,_), X(k,_)) > 0){
      k = j;
    }
  }
  return (k);
}

// [[Rcpp::export()]]

NumericMatrix env_convex_javis_cpp (NumericMatrix X){
  double first = min_X_cpp(X);
  double nextpt = next_point_cpp (X, first);
  NumericVector tmpv = {first, nextpt};
  NumericVector env = tmpv;
  while (nextpt != first){
    nextpt = next_point_cpp(X,nextpt);
    env.push_back(nextpt);
  }
  NumericMatrix res = NumericMatrix(env.size(), 2);
  for (int i = 0; i < env.size(); i++){
    NumericVector tmp = X(env[i],_);
    res(i, _) = tmp;
  }
  return(res);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//