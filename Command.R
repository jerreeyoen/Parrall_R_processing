library(rbenchmark)

data = rnorm(1e6, 0, 1)

func1 <- function() {
  total = 0
  for(i in data)
    total = total + (i * 2.0)
  return(total) 
}

func2 <- function() {
  return(sum(data*2.0))
}

print(benchmark(func1(), func2(), order='relative', replications=50))
