#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export("get_dist_cpp")]]
List get_dist(DataFrame x, int node, int d, int max_n){
  // assumption: from and to have same domain
  IntegerVector from = x["from"];
  IntegerVector to = x["to"];

  IntegerVector distance(max_n+1, R_NaInt);
  std::set<int> current;

  current.insert(node);

  // maybe a better iterator
  int dc = 1;
  while (dc <= d && current.size() > 0){
    std::set<int> next;
    for (int i = 0; i < from.length(); i++){
      int f = from[i];
      int t = to[i];
      if (current.count(f) == 1 && IntegerVector::is_na(distance[t])){
        next.insert(t);
        distance[t] = dc;
      }
    }
    dc += 1;
    current = next;
  }

  return List::create(
    _["x"] = x,
    _["node"] = node,
    _["d"] = d,
    _["distance"] = distance
  );
}


/*** R
x <- data.frame(
  from = factor(c("A", "A", "B"), levels=LETTERS[1:4]),
  to = factor(c("B","C","D"), levels=LETTERS[1:4])
)

get_dist_cpp(x, node = 1, d = 2, max_n = 4)

*/
