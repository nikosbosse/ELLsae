#'Subset of the 2010 census in Brazil
#'
#' The dataset contains 350,696 individual observations from the 2010 census in
#'  Brazil. This is only a subset of variables and observations of the original 
#'  census that can be used as an example for the functionality of the ELLsae 
#'  package. The full documentation can be found here: 
#' \url{http://microdata.worldbank.org/index.php/catalog/2078/study-description}.
#'
#'@format A data frame with 350696 rows and 11 variables:
#'\describe{
#'  \item{hh_inc}{household income}
#'  \item{geo2_br}{inconsistent level two boundaries of Brazil, 
#'  Municipality 1980 - 2010}
#'  \item{year}{year of the census}
#'  \item{age}{The age of a person in years}
#'  \item{sex}{sex (discrete)}
#'  \item{adults}{number of adults}
#'  \item{children}{number of children}
#'  \item{religion}{indicates the person's religion, including "none"}
#'  \item{computer}{computer in the household (discrete)}
#'  \item{trash}{trash disposal (discrete)}
#'  \item{urban}{Urban-rural status (discrete)}
#'}
#'@source \url{https://international.ipums.org/international/index.shtml}
#'@docType data
#'@keywords datasets
#'@name brazil
#'@usage data(brazil)
#'@references Brazilian Institute of Geography and Statistics (IBGE). Brazil 
#'Demographic Census 2010. Rio de Janeiro, Brazil: 
#'Brazilian Institute of Geography and Statistics (IBGE), 2012.
#'
# seealso Random data can also be generated using the \code{randomSAEdata} 
# function contained in this package.
NULL
