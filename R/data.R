#' Dependencies of CRAN packages
#'
#' A dataset containing the dependencies of various types (Imports, Depends, Suggests, LinkingTo, and their reverse counterparts) of more than 14600 packages available on CRAN as of 2020-05-09.
#'
#' @format A data frame with 211408 rows and 4 variables:
#' \describe{
#'   \item{from}{the name of the package that introduced the dependencies}
#'   \item{to}{the name of the package that the dependency is directed towards}
#'   \item{type}{the type of dependency, which can take the follow values (all in lowercase): "depends", "imports", "linking_to", "suggests"}
#'   \item{reverse}{a boolean representing whether the dependency is a reverse one (TRUE) or a forward one (FALSE)}
#' }
#' @source The CRAN pages of all the packages available on \url{https://cran.r-project.org/web/packages/available_packages_by_name.html}
"cran_dependencies"