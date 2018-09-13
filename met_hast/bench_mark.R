Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

library(rbenchmark)
library(Rcpp)

#this tells R to compile the file and expose the function to the R interpreter
sourceCpp("met-hast.cpp")

#unused - but used to clarify
rtarget <- function(x) {
  flag = rbinom(x, 1, 0.7);
  return((flag * rnorm(x, 7, 0.5)) + ((1-flag) * rnorm(x, 10, 0.5)));
}

#metropolis-hastings written in R
met_hast_r <- function(target, instru, instru_d, start, sample_count) {
  x = start

  sample_index = 1;
  samples = vector(length=sample_count);

  while(TRUE) {
    proposal = instru(x);
    r = ( target(proposal) * instru_d(x ,proposal)) / ( target(x) * instru_d(proposal, x) ); 
    if((r>=1) || (threadsafe_runif(0,1) < r)) { #threadsafe_runif is defined in our c++ code
      x = proposal;
      samples[sample_index] = proposal;
      sample_index = sample_index + 1;
      if(sample_index>sample_count) {
        return(samples);
      }
      next;
    }
  }
}

start = c(0,6,7,8,15);
sample_count = 1000;

#a function that calculates our MCMH in a R (always will be single threaded)
r_test <- function() {
  samples = matrix(nrow=sample_count, ncol=length(start)) 
  for(i in (1:length(start)))
    samples[,i] = met_hast_r(f, g, g_d, start[i], sample_count); #f,g,g_d are defined in our c++ code to keep our implementations consistent
  return(samples);
}

#our cpp function but called in a single threads.  This is a sort of control example
cpp_single_threaded <- function() {
  samples = matrix(nrow=sample_count, ncol=length(start))
  for(i in (1:length(start))) 
    samples[,i] = single_test(start[i], sample_count);
  return(samples);
}

#runs N=threads threads to calculate our MHMC using the starting positions given.  Each chain will be of length samples_count
#result matrix will be nrow=samples_count ncol=length(start)
cpp_multi_threaded <- function(threads) {
  samples = parallel_test(start, sample_count, threads);
  return(samples);
}

#helper function to plot bar graphs
barplot_mine <- function(res, title, fname) {
  png(fname)
  barplot(res$elapsed, names.arg=res$test, ylab='time in seconds', main=title)
  dev.off()
}

# R is very slow - R vs cpp single threaded
res = benchmark(r_test(), cpp_single_threaded(), order='relative', replications=50)
barplot_mine(res, '5 chains 1000 samples', './1.png')
print(res)

#one c++ thread vs 2 c++ threads
res = benchmark(cpp_single_threaded(), cpp_multi_threaded(2), order='relative', replications=50)
barplot_mine(res, '5 chains 1000 samples', './2.png')
print(res)

#up the sample count - now 2 threads performs better
sample_count = 10000
res = benchmark(cpp_single_threaded(), cpp_multi_threaded(2), order='relative', replications=50)
barplot_mine(res, '5 chains 10000 samples', './3.png')
print(res)

#add more chains:
start = c(0,3,5,6,7,8,10,12,13,15);

#Intel(R) Core(TM) i7 CPU       M 620  @ 2.67GHz
#see how different number of threads perform vs each other.
res = vector(length=length((1:10)))
for(i in (1:10))
  res[i] = benchmark(cpp_multi_threaded(i))$elapsed[1]

png('./4.png')
plot(res, type='o', xlab='threads', ylab='time in seconds', main='10 chains 10000 samples')
dev.off()

#see how different number of threads perform vs each other - increase sample count
samples = 100000
res = vector(length=length((1:10)))
for(i in (1:10))
  res[i] = benchmark(cpp_multi_threaded(i))$elapsed[1]

png('./5.png')
plot(res, type='o', xlab='threads', ylab='time in seconds', main='10 chains 100000 samples')
dev.off()

#see how different number of threads perform vs each other - decrease sample count
samples = 1000
res = vector(length=length((1:10)))
for(i in (1:10))
  res[i] = benchmark(cpp_multi_threaded(i))$elapsed[1]

png('./6.png')
plot(res, type='o', xlab='threads', ylab='time in seconds', main='10 chains 1000 samples')
dev.off()
