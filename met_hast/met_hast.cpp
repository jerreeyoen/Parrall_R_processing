#include <iostream>
#include <unistd.h>
#include <vector>
#include <queue>
#include <thread>
#include <future>
#include <atomic>
#include <cmath>
#include <math.h>
#include <random>
#include <chrono>

#include <Rcpp.h>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

std::default_random_engine _generator(std::chrono::system_clock::now().time_since_epoch().count());

//threadsafe runif as described by the presentation
// [[Rcpp::export]]
double threadsafe_runif(double start, double stop) {
  std::uniform_real_distribution<double> distribution(start, stop);
  return distribution(_generator);
}

//the met-hast function implemented 
template < typename F1, typename F2, typename F3 >
void met_hast(F1 && target, F2 && instru, F3 && instru_d, double start, size_t sample_count,
              RcppParallel::RMatrix<double>::Column result) {
  double x = start;

  size_t sample_index = 0;

  //TODO - create bail condition - this is a problem with this implementation, a thread could run forever if you pick a degenerative starting point
  while(true) {
    double proposal = instru(x);
    double r = ( target(proposal) * instru_d(x, proposal)) 
                              / 
                            ( target(x) * instru_d(proposal, x) ); 

    if ((r>=1) || ( threadsafe_runif(0,1) < r )) {
      x = proposal;
      result[sample_index] = proposal;
      sample_index = sample_index + 1;
      if(sample_index>=sample_count)
         return; 
    }
  }
} 

# define M_PI           3.14159265358979323846  /* pi */

//threadsafe dnorm as described by the presentation
// [[Rcpp::export]]
double threadsafe_dnorm(const double x, const double mean, const double sigma) {
  double var = ::pow(sigma,2);
  return ( 1.0 / ::sqrt( 2.0 * M_PI * var ) ) * exp(-(::pow((x-mean),2)/ (2*var))); 
}

//threadsafe rnorm as described by the presentation
// [[Rcpp::export]]
double threadsafe_rnorm(double mean, double sd) {
  std::normal_distribution<double> distribution(mean, sd);
  return distribution(_generator);
}

/*
 * The next three functions are the c++ implementations of the density and target dists
 * we cannot use R for this because R cannot be safetly called in multiple threads.
 */

// [[Rcpp::export]]
double f(const double x) {
  return((.7*threadsafe_dnorm(x, 7, 0.5)) + ((1-0.7)*threadsafe_dnorm(x, 10, 0.5)));
}

// [[Rcpp::export]]
double g(const double x) {
  return(threadsafe_rnorm(x, 0.01));
}

// [[Rcpp::export]]
double g_d(const double x, const double u) {
  return(threadsafe_dnorm(x, u, 0.01));
}

template < typename F1, typename F2, typename F3 >
Rcpp::NumericMatrix parallel_met_hast(Rcpp::NumericVector starting_points,
F1 && target, F2 && instru, F3 && instru_d, RcppParallel::RMatrix<double> & result, size_t num_threads) {

  //dont run more threads than we have jobs to do
  num_threads = std::min(num_threads, (size_t)starting_points.size()); //naughty cast 

  /* Below is a simple threadpool implementation.  This might be hard to understand if you aren't experieced at c++
   * basically num_threads threads are spawned and they pick up the chains and run them.  
   * when all jobs are done the function returns.
   */

  std::queue< std::function<void()> > tasks;
  std::mutex queue_mutex;
  std::condition_variable condition;
  bool running = true;

  std::vector<std::thread> threads;
  for(size_t i = 0; i < num_threads; ++i) { 
    threads.emplace_back([&]{
        while(true) {
          std::function<void()> task;
          {
            std::unique_lock<std::mutex> lock(queue_mutex);  
            condition.wait(lock, [&](){ return !running || !tasks.empty(); });
            if(!running && tasks.empty())
              return;
 
            task = std::move(tasks.front());
            tasks.pop();
          }
          task(); //start the job
        }
      });
  }

  for(size_t i = 0; i < starting_points.size(); ++i) { 
    {
      std::unique_lock<std::mutex> lock(queue_mutex);
      tasks.emplace(std::bind([&](const size_t index) {
        met_hast(target, instru, instru_d, starting_points[index], result.nrow(),
        result.column(index));
      }, i));

    }
    condition.notify_one();
  }

  {
    std::unique_lock<std::mutex> lock(queue_mutex);
    running = false;
  }

  condition.notify_all();
  for(auto & thread : threads)
    thread.join();
}

// [[Rcpp::export]] 
Rcpp::NumericMatrix parallel_test(Rcpp::NumericVector starting_points, size_t sample_count, size_t num_threads) {
  //run N met_hast functions in parallel.  The below code makes the result matrix threadsafe and then calculates met-hast 
  Rcpp::NumericMatrix result(sample_count, starting_points.size());
  RcppParallel::RMatrix<double> safe_result(result);
  parallel_met_hast(starting_points, f, g, g_d, safe_result, num_threads);
  return result;
}

// [[Rcpp::export]] 
Rcpp::NumericVector single_test(double starting_point, size_t sample_count) {
  //run one met_hast function.  The below code makes the result matrix threadsafe, then calculates met-hast
  Rcpp::NumericMatrix result(sample_count, 1);
  RcppParallel::RMatrix<double> safe_result(result);
  met_hast(f, g, g_d, starting_point, sample_count, safe_result.column(0));
  return result;
}
