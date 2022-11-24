#' @title interacalate 
#' @description Inserts the element of sep between the items of list/vector iterable. \cr\cr
#' @param iterable an iterable object, for example a vector or a list.
#' @param sep The element to place between the elements of iterable. Should be of the same class as elements in iterable.
#' @examples
#'\dontrun{
#' intercalate(c(1, 2, 3), 9) -> c(1, 9, 2, 9, 3) \cr
#' intercalate(list(1, 2, 3), 9) -> list(1, 9, 2, 9, 3) \cr
#' intercalate(list("a", "b", "c"), "-") -> list("a", "-", "b", "-", "c")
#' }

intercalate <- function(iterable, sep){
  vector_length <- 2 * length(iterable) - 1
  if(vector_length <= 1){
    return(iterable)
  }
  vec <- vector( class(iterable), vector_length)

  for(item in seq_len(vector_length)){
    if (item %% 2 == 0){
      vec[item] <- sep
    }
    else {
      vec[item] <- iterable[(item + 1)/2]
    }
  }
  return(vec)
}
