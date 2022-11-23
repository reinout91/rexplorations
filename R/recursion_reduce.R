library(data.table)

hub <- data.table(a= c(1,2,3), b = c(1,1,2))

hub2 <- data.table(n = 3, b = 2)

hub3 <- data.table(b = 1, l = 9)

reduce <- function(lijst, functie, ...){
  
  func <- function( lijst, result , pos){
    
    if(pos == length(lijst)){
      return(lijst[[pos]])
    }
    else {
      return(functie(lijst[[pos]], func(lijst, result, pos + 1), ...))
    }
  }
  func(lijst, result = NULL , pos = 1)
  
}

reduce(list(hub, hub2, hub3), functie = merge,  by = 'b', all = TRUE)

