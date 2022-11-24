
#' @title reduce
#' @description Roept recursief een functie aan op een vector. Vervanger voor de `purrr::reduce()`, maar dan recursief.
#' @param x Een list of vector waarop je de functie wil uitvoeren
#' @param func functie die je wil uitvoeren
#' @param ... Extra argumenten voor de functie
#' @returns De waarde na recursief uitvoeren van de functie op de hele vector / list.
#' @examples
#'\dontrun{
#' table_1 <- data.table(a = c(1,2,3), b = c(1,1,2))
#' table_2 <- data.table(n = 3, b = 2)
#' table_3 <- data.table(b = 1, l = 9)
#' full outer join, meerdere kolommen:
#' reduce(list(table_1, table_2, table_3), func = merge,  by = 'b', all = TRUE)
#' =>
#'    b a  n  l
#'    1 1 NA  9
#'    1 2 NA  9
#'    2 3  3 NA
#' }
reduce <- function(x, func, ...){
  
  length_of_x <- length(x)
  innerfunc <- function( x , positie){
    
    if(positie == length_of_x){
      return(x[[positie]])
    }
    else {
      return(func(x[[positie]], innerfunc(x, positie + 1), ...))
    }
  }
  innerfunc(x, pos = 1)
  
}
