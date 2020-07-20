#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
//' Matrix multiplication
//'
//' @param A a matrix.
//' @param B a matrix.
//' @export matmult
// [[Rcpp::export]]
arma::mat matmult(arma::mat A, arma::mat B) {
  return A * B;
}


