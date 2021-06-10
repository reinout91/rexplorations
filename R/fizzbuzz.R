library(lambda.r)
fizz(n) %::% numeric : character
fizz(n) %when% {n %% 15 == 0} %as% {"fizzbuzz"}
fizz(n) %when% {n  %% 3  == 0}  %as% {"fizz"}
fizz(n) %when% {n  %% 5  == 0}  %as% {"buzz"}
fizz(n) %as% {as.character(n)}

print(sapply(1:100, fizz))