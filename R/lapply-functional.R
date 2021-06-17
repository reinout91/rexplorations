summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  return(lapply(funs, function(f) f(x, na.rm = TRUE)))
}

a <- c(1,2,5,9)
print(summary(a))