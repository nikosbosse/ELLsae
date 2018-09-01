#' ELLsae: A package for small area estimation
#' @section ELLsae functions:
#' Contains two functions one for normal use cases called \code{ellsae} and a 
#' second one called \code{ellsae_big} that can be used if RAM consumption 
#' becomes an issue but which is slower that the basic version.
#' @description A method for small area estimation.
#' @details  The method for small area estimation added by this package is 
#' largely based on an approach developed 
#' by C. Elbers, J. Lanjouw and P. Lanjouw (2003). It can be used to impute a 
#' missing variable 
#' from a smaller survey dataset into a census. The imputation is based on a 
#' linear model and bootstrap samples from a normal distribution. As of now the
#'  correction for heteroscedasticity 
#' is left omitted. +
#' @author Nikos Bosse [aut, cre], Felix Suettmann [aut, cre]
#' @docType package
#' @name ELLsae-package
#' @references Elbers, C., Lanjouw, J. O. and Lanjouw, P. (2003). 
#' \emph{Micro-Level Estimation of Poverty and Inequality}. 
#'   In: Econometrica 71.1, pp. 355-364, Jan 2003
#' @seealso Other small area estimation methods can also be found in the 
#' package \code{sae}.
#' @keywords internals
#' @import data.table
#' @import stats
#' @importFrom MASS mvrnorm
"_PACKAGE"