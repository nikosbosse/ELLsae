#' ELLsae: A package for small area estimation
#' @details brief overview over the usage of the package
#' @section ELLsae functions:
#' Delivered are two functions that do about the same thing ...
#' @description The method for small area estimation added by this package is largely based on an approach developed 
#' by C. Elbers, J. Lanjouw and P. Lanjouw (2003). It can be used to impute a missing variable 
#' from a smaller survey dataset into a census. The imputation is based on a 
#' linear model and bootstrap samples from a normal distribution. As of now the correction for heteroscedasticity 
#' is left omitted. 
#' @author Nikos Bosse [aut, cre], Felix Suettmann [aut, cre]
#' @import Rcpp (>= 0.12.18), RcppEigen (>= 0.3.3.4.0), doParallel, foreach, data.table, bigstatsr, BH, Rdpack, dqrng
#' @docType package
#' @name ELLsae
#' @references 
#'  \insertRef{sae1}{ELLsae}
#'  \insertRef{SAEcomparison}{ELLsae}
#'  Elbers, C., Lanjouw, J. O. and Lanjouw, P. (2003). \emph{Micro-Level Estimation of Poverty and Inequality}. 
#'   In: Econometrica 71.1, pp. 355-364, Jan 2003
# @importFrom Rdpack reprompt
"_PACKAGE"