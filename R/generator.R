library(lambda.r)

seq.gen(start) %as% {
  value <- start - 5L
  function() {
    value <<- value + 5L
    return(value)
  }
}


foo <- seq.gen(0)

for(i in 1:10) print(foo())