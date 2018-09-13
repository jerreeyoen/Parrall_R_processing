Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

library(Rcpp)
library(rbenchmark)

sourceCpp("test.cpp")

#simple linear search written in R
linear_int_search_r <-function(N, nv) {
  idx = 1;
  for(i in nv) {
    if(i == N)
     return(idx);
    idx = idx+1;
  }
  return(-1);
}

size = 10000
numbers = runif(size, 0, 1000);
numbers[((size/2)+ 100)] = -1;

#linear search in R vs linear search in c++
res <- benchmark(linear_int_search_r(-1, numbers), linear_int_search(-1, numbers), order='relative')
print(res)

size = 100000000
numbers = runif(size, 0, 1000);
numbers[(size/2)+ 100] = -1;
#more samples - linear search in c++ 2 threads vs 1 thread
res <- benchmark(simple_threaded_search(-1, numbers, 2), linear_int_search(-1, numbers), order='relative')

print(res)
