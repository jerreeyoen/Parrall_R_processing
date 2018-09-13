#include <vector>
#include <thread>
#include <atomic>

#include <Rcpp.h>

using namespace Rcpp;

double linear_int_search_range(const int N, const NumericVector & nv, size_t start, size_t end, std::atomic<bool>& running) {
  for(size_t i = start; i < end; ++i) {
    if(!running)
      break;

    if(nv[i] == N)
      return i;
  }
  return -1;
}

// [[Rcpp::export]]
double linear_int_search(const int N, const NumericVector & nv) {
  std::atomic<bool> running(true);
  return linear_int_search_range(N, nv, 0, nv.size(), running);
}

// [[Rcpp::export]]
size_t simple_threaded_search(const int N, const NumericVector &nv, int number_of_threads) {
  size_t result = -1;  
  std::atomic<bool> running(true);

  std::vector<std::thread> threads;
  size_t search_size = nv.size() / (double)number_of_threads; //might have a rounding error
  size_t search_start = 0;
  for(size_t i = 0; i < number_of_threads; ++i) { 
    threads.push_back(std::thread([&](int w) {
      int i = w;
    }, 5));

    threads.push_back(std::thread([&](const int N, size_t start, size_t end, const NumericVector& nv) {
       auto index = linear_int_search_range(N, nv, start, end, running); 
       if(index >= 0 && running) {  
         result = index;
         running = false;
       }
    }, N, search_start, search_start + search_size, nv));
    search_start += search_size; 
  }

  for(auto & thread : threads)
    thread.join();

  return result;
}
