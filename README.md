# Parrall_R_processing
This is build by ALex and Hanjie for a school project at JHU.
This is a paralleling processing using R.


R, by default is a single thread processing language. This package will help you to use R more efficiently by tunning it into a multiple thread processing. We test it with Monte Carlo Simulation. As you may download and test with different number of thread assign, you will be find out the maximum performance is not using all threads available. try different # of threads with small dataset to find the best # of thread for you machine before implement it in real project.



Build Steps:

  install a c++ compiler - I used g++ (Debian 4.9.2-10) 4.9.2 with -std=c++0x (c++11) and -O2 (compiler optimization level2)

  set up your R makevars: https://stackoverflow.com/questions/43597632/understanding-the-contents-of-the-makevars-file-in-r-macros-variables-r-ma

  install.packages('Rcpp') # this will compile and install Rcpp, so if your c++ compiler is not set up correctly this will not work
  install.packages('RcppParallel') # this will compile and install RcppParallel, so if your c++ compiler is not set up correctly this will not work

Now, the easiest way to use Rcpp is to use the "inline" compilation.  Essentially you tell Rcpp to load and compile a cpp file (c++ source code) at startup.  This isn't the way you'd use a final lib, but it's perfect for dev and our project.  Take a look at the example subdir for an easy example. 

structure:
  ParallelProcessing.R - testing of library(parallel) 
  example - Rcpp with thread example I wrote to do parallel search.  Just to test that I could get the building blocks working.  No need to review.
  slow_command - R code that shows how writing a for-loop yourself is slow. 
  rcpp_example - simple Rcpp example showing the performance vs c++ versus R 
  met-hast - the final project, running benchmark.R will compile the c++ and run all the tests and generate 1.png through 6.png.  If you don't have the right libraries or your c++ compiler set up properly this will not work
