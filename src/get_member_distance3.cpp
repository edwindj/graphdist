#include "../inst/include/RcppSpam.h"

using namespace Rcpp;

int get_num_threads(){
#ifdef _OPENMP
  return omp_get_max_threads();
#endif
  return 1;
}

// [[Rcpp::export]]
List rcpp_get_member_distance3(
    const S4 E,
    LogicalVector member,
    std::vector<size_t> from_nodes,
    int max_d = 3,
    int nthreads = -1
  ){

  if (nthreads < 0){
    nthreads = get_num_threads();
  }

  NumericVector rowpointers = E.slot("rowpointers");
  NumericVector colindices = E.slot("colindices");
  NumericVector entries = E.slot("entries");

  IntegerMatrix nodes_at_d(from_nodes.size(),max_d);
  IntegerMatrix members_at_d(from_nodes.size(),max_d);

  // assume from_nodes and member have same domain
  int N = member.size();

#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
  for (int i = 0; i < from_nodes.size(); i++){
    auto node = from_nodes[i] - 1;

    std::vector<size_t> current = {node};
    std::vector<int> distance(N, R_NaInt);

    distance[node] = 0;

    int d_c = 0;
    while (d_c < max_d && current.size() > 0){
      std::vector<size_t> next;
      for (auto from = current.begin(); from != current.end(); from++){
        size_t begin = rowpointers[*from] - 1;
        size_t end = rowpointers[*from + 1] - 1;

        for (size_t j = begin; j < end; j++){
          size_t to = colindices[j] - 1;
          if (IntegerVector::is_na(distance[to])){
            distance[to] = d_c + 1;
            nodes_at_d(i, d_c) += 1;
            members_at_d(i, d_c) += member[to];
            next.push_back(to);
          }
        }
      }
      d_c += 1;
      current = next;
    }
  }

  return List::create(
    _["rowpointers"] = rowpointers,
    _["colindices"] = colindices,
    // _["from_nodes"] = from_nodes,
    // _["member"] = member,
    // _["max_d"] = max_d,
    _["entries"] = entries,
    _["nodes_at_d"] = nodes_at_d,
    _["members_at_d"] = members_at_d
  );
}


/*** R
set.seed(1)
library(spam)
library(spam64)
options("spam.force64" = TRUE)

N <- 1e4
M <- spam_random(nrow = N, density = 0.02)
member <- (runif(N) > 0.9) # i.e. 10% chance of being a member

system.time({
  r <- graphdist:::rcpp_get_member_distance3(M, member, from_nodes = 1:10, max_d=4, nthreads = 4)
})

r
hist(r$distance)

system.time({
  m <- graphdist:::rcpp_member_distance2(M, member, seq_len(10), max_d=5)
})

(m$n_members/m$n_nodes) |> round(2)


E <- rcpp_to_spam(c(1,1,3), c(3,2,1), N=4)
str(E)
as.matrix(E)

E <- rcpp_to_spam_sorted(c(1,1,3), c(2,3,1), N=4)
str(E)

member <- c(TRUE, FALSE, FALSE, TRUE)
rcpp_member_distance2(E, member, seq_len(4), max_d=5)

graphdist:::rcpp_member_distance_par(E, member, seq_len(4), max_d=5)

*/
