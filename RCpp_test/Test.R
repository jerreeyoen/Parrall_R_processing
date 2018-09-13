library(rbenchmark)
library(Rcpp)

cppFunction(' 
double sum_cpp(NumericVector data) {
  double result = 0.0;
  for(size_t i = 0; i < data.size(); ++i)
    result += data[i] * 2.0;
  return result;
}
')

data = rnorm(1e6, 0, 1)

func2 <- function() {
  return(sum(data*2.0))
}

print(benchmark(func2(), sum_cpp(data), order='relative', replications=50))
