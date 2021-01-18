#include <Rcpp.h>
using namespace Rcpp;
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
// [[Rcpp::export]]
NumericMatrix naif_convex_rcpp(NumericMatrix list){
  NumericMatrix M=list;
  NumericVector f;
  f={};
  int i;
  i=0;
  int cnt_pos;
  int cnt_neg;
  int s;
  int j;
  int h;
  int cntmat;
  int cntmat2;
  int k;
  int k2;
  int ind;
  int g;
  float u;
  NumericVector l;
  l={};
  NumericVector v1;
  NumericVector d;
  d={};
  NumericVector vec;
  NumericVector vec2;
  while (i<M.ncol()) {
    v1=M(_,i);
    j=0;
    while (j<M.ncol()){
      if (j!=i){
        l={};
        h=0;
        while (h<M.ncol()){
          if (h!=i){
            if (h!=j){
              l.push_back((M(0,j)-M(0,i))*(M(1,h)-M(1,i))-(M(0,h)-M(0,i))*(M(1,j)-M(1,i)));
            }}
          h=h+1; 
        }
        cnt_pos=0;
        cnt_neg=0;
        s=0;
        while (s<l.size()){
          if (l(s)>=0){
            cnt_pos=cnt_pos+1;
          }
          s=s+1;
          }
        s=0;
        while (s<l.size()){
          if (l(s)<=0){
            cnt_neg=cnt_neg+1;
          }
          s=s+1;
          }
        if (cnt_pos==l.size()){
          f.push_back(v1(0));
          f.push_back(v1(1));
        }
        if (cnt_neg==l.size()){
          f.push_back(v1(0));
          f.push_back(v1(1));
        }
      }
      j=j+1;
    }
    i=i+1;
  }
  cntmat=0;
  cntmat2=0;
  NumericMatrix F(2,(f.size())/2);
  while (cntmat<f.size()){
    F(0,cntmat2)=f(cntmat);
    F(1,cntmat2)=f(cntmat+1);
    cntmat=cntmat+2;
    cntmat2=cntmat2+1;
  }
  k=0;
  while (k<(F(1,_).size())){
    k2=k+1;
    while (k2<(F(1,_).size())){
      if (F(0,k)==F(0,k2) && F(1,k)==F(1,k2)){
        F=col_erase(F,k2);
        k2=k2-1;
      }
      k2=k2+1;
    }
    k=k+1;
  }
  k=0;
  while (k<F(1,_).size()-1){
    k2=k+1;
    d={};
    while (k2<F(1,_).size()){
      d.push_back((F(0,k)-F(0,k2))*(F(0,k)-F(0,k2)) +(F(1,k)-F(1,k2))*(F(1,k)-F(1,k2)));
      k2=k2+1;
    }
    ind=0;
    g=0;
    while (g<d.size()){
      if (d(0)>d(g)){
        ind=g;
        u=d(0);
        d(0)=d(g);
        d(g)=u;
      }
      g=g+1;
    }
    vec=F(_,k+ind);
    vec2=F(_,k+1);
    F(0,k+1)=vec(0);
    F(1,k+1)=vec(1);
    F(0,k+ind)=vec2(0);
    F(1,k+ind)=vec2(1);
    k=k+1;
  }
  return(F);
}