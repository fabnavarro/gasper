#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;
//' Spectral decomposition of a symetric matrix
//'
//' Eigen decomposition of dense symmetric/hermitian matrix M
//' using standard or divide-and-conquer methods.
//' the divide-and-conquer method provides slightly different results
//' than the standard method, but is considerably faster for large matrices
//'
//' @param M a matrix.
//' //@param method "dc" indicates divide-and-conquer method (par).
//' //              "std" indicates standard method.
//' @export eigendec
// [[Rcpp::export]]
List eigendec(arma::mat M) {
  arma::vec eigval;
  arma::mat eigvec;
  eig_sym(eigval, eigvec, M, "dc");
  return List::create(Named("evalues") = eigval,
                      Named("evectors") = eigvec);
}

/***R
# warning cpp outup is a column vector (to be modified)
M = matrix(c(1,2,2,1), ncol = 2)
eigendec(M)
eigen(M)
*/

