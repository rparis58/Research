library (inline)
g <- cxxfunction( signature (vs = "numeric"),
plugin = "RcppArmadillo", body = '
arma::vec v = Rcpp::as < arma::vec > ( vs );
arma::mat op = v * v. t () ;
double ip = arma::as_scalar( v.t() * v) ;
return Rcpp::List::create ( Rcpp::Named ("outer") =op,
Rcpp::Named ("inner") = ip );
')
