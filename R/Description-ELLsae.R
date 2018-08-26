#' ELLsae: A package for small area estimation
#' @section ELLsae functions:
#' Delivered are two functions that do about the same thing ...
#' @description A method for small area estimation.
#' 
#' @details  The method for small area estimation added by this package is largely based on an approach developed 
#' by C. Elbers, J. Lanjouw and P. Lanjouw (2003). It can be used to impute a missing variable 
#' from a smaller survey dataset into a census. The imputation is based on a 
#' linear model and bootstrap samples from a normal distribution. As of now the correction for heteroscedasticity 
#' is left omitted. +
#' @author Nikos Bosse [aut, cre], 
#' Felix Suettmann [aut, cre]
#' @docType package
#' @name ELLsae
#' @references Elbers, C., Lanjouw, J. O. and Lanjouw, P. (2003). \emph{Micro-Level Estimation of Poverty and Inequality}. 
#'   In: Econometrica 71.1, pp. 355-364, Jan 2003
#' @seealso Other small area estimation methods can also be found in the package \code{sae}.
#' @import RcppEigen
#' @import foreach
#' @import data.table
#' @importFrom Rdpack reprompt
NULL