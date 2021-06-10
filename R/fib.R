library(lambda.r)

fib(0) %as% 1
fib(1) %as% 1
fib(n) %as% {fib(n-2) + fib(n-1)}

print(sapply(1:15, fib))