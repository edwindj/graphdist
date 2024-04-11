#include <RcppCommon.h>

// forward declare classes
namespace RcppSpam {
class Matrix;
}  // namespace RcppSparse

// forward declare Rcpp::as<> Exporter
namespace Rcpp {

namespace traits {

template <>
class Exporter<RcppSpam::Matrix>;
}  // namespace traits
}  // namespace Rcpp

#include <Rcpp.h>

// //[[Rcpp::plugins(openmp)]]
// #ifdef _OPENMP
// #include <omp.h>
// #endif

namespace RcppSpam {
class Matrix {
public:
  // public member objects
  Rcpp::NumericVector entries, colindices, rowpointers;
  Rcpp::IntegerVector dimension;

  // constructors
  Matrix(Rcpp::NumericVector entries, Rcpp::NumericVector colindices, Rcpp::NumericVector rowpointers, Rcpp::IntegerVector dimension) : entries(entries), colindices(colindices), rowpointers(rowpointers), dimension(dimension) {}
  Matrix(const Rcpp::S4& s) {
    if (!s.hasSlot("entries") || !s.hasSlot("colindices") || !s.hasSlot("rowpointers") || !s.hasSlot("dimension"))
      throw std::invalid_argument("Cannot construct RcppSparse::Matrix from this S4 object");

    entries = s.slot("entries");
    colindices = s.slot("colindices");
    rowpointers = s.slot("rowpointers");
    dimension = s.slot("dimension");
  }
  Matrix() {}

  unsigned int rows() { return dimension[0]; }
  unsigned int cols() { return dimension[1]; }
  unsigned int nrow() { return dimension[0]; }
  unsigned int ncol() { return dimension[1]; }
  unsigned int n_nonzero() { return entries.size(); };
  Rcpp::NumericVector& nonzeros() { return entries; };
  Rcpp::NumericVector& innerIndexPtr() { return colindices; };
  Rcpp::NumericVector& outerIndexPtr() { return rowpointers; };

  // create a deep copy of an R object
  Matrix clone() {
    Rcpp::NumericVector entries_ = Rcpp::clone(entries);
    Rcpp::NumericVector colindices_ = Rcpp::clone(colindices);
    Rcpp::NumericVector rowpointers_ = Rcpp::clone(rowpointers);
    Rcpp::IntegerVector dimension_ = Rcpp::clone(dimension);
    return Matrix(entries_, colindices_, rowpointers_, dimension_);
  }

  // element lookup at specific index
  double at(int row, int col) const {
    for (R_xlen_t j = rowpointers[row]; j < rowpointers[row + 1]; ++j) {
      if (colindices[j] == col)
        return entries[j];
      else if (colindices[j] > col)
        break;
    }
    return 0.0;
  }
  double operator()(int row, int col) const { return at(row, col); };
  double operator[](R_xlen_t index) const { return entries[index]; };

  // subview clones
  Rcpp::NumericVector operator()(int row, Rcpp::IntegerVector& col) {
    Rcpp::NumericVector res(col.size());
    for (int j = 0; j < col.size(); ++j) res[j] = at(row, col[j]);
    return res;
  };
  Rcpp::NumericVector operator()(Rcpp::IntegerVector& row, int col) {
    Rcpp::NumericVector res(row.size());
    for (int j = 0; j < row.size(); ++j) res[j] = at(row[j], col);
    return res;
  };
  Rcpp::NumericMatrix operator()(Rcpp::IntegerVector& row, Rcpp::IntegerVector& col) {
    Rcpp::NumericMatrix res(row.size(), col.size());
    for (int j = 0; j < row.size(); ++j)
      for (int k = 0; k < col.size(); ++k)
        res(j, k) = at(row[j], col[k]);
    return res;
  };

  // column access (copy)
  Rcpp::NumericVector col(int col) {
    Rcpp::NumericVector c(dimension[0], 0.0);
    for (int row = 0; row < dimension[0]; ++row){
      for(R_xlen_t i = rowpointers[row]; i < rowpointers[row+1]; ++i){
        if (colindices[i] == col){
          c[col] = entries[i];
        } else if (colindices[i] > col){
          break;
        }
      }
    }
    return c;
  }

  Rcpp::NumericMatrix col(Rcpp::IntegerVector& c) {
    Rcpp::NumericMatrix res(dimension[0], c.size());
    for (int j = 0; j < c.size(); ++j) {
      res.column(j) = col(c[j]);
    }
    return res;
  }

  // row access (copy)
  Rcpp::NumericVector row(int row) {
    Rcpp::NumericVector r(dimension[1], 0.0);
    for (R_xlen_t i = rowpointers[row]; i < rowpointers[row+1]; ++i){
      r[colindices[i]] = entries[i];
    }
    return r;
  }
  Rcpp::NumericMatrix row(Rcpp::IntegerVector& r) {
    Rcpp::NumericMatrix res(r.size(), dimension[1]);
    for (int j = 0; j < r.size(); ++j) {
      res.row(j) = row(r[j]);
    }
    return res;
  }

  // colSums and rowSums family
  Rcpp::NumericVector colSums() {
    Rcpp::NumericVector sums(dimension[1]);
    for (int row = 0; row < dimension[0]; ++row)
      for (R_xlen_t i = rowpointers[row]; i < rowpointers[row + 1]; ++i)
        sums(colindices[i]) += entries[i];
    return sums;
  }
  Rcpp::NumericVector rowSums() {
    Rcpp::NumericVector sums(dimension[0]);
    for (int row = 0; row < dimension[0]; ++row)
      for (R_xlen_t j = rowpointers[row]; j < rowpointers[row + 1]; ++j)
        sums(row) += entries[j];
    return sums;
  }
  Rcpp::NumericVector colMeans() {
    Rcpp::NumericVector sums = colSums();
    for (int i = 0; i < sums.size(); ++i)
      sums[i] = sums[i] / dimension[0];
    return sums;
  };
  Rcpp::NumericVector rowMeans() {
    Rcpp::NumericVector sums = rowSums();
    for (int i = 0; i < sums.size(); ++i)
      sums[i] = sums[i] / dimension[1];
    return sums;
  };

  // crossprod
//   Rcpp::NumericMatrix crossprod() {
//     Rcpp::NumericMatrix res(dimension[1], dimension[1]);
// // #ifdef _OPENMP
// // #pragma omp parallel for
// // #endif
//     for (int col1 = 0; col1 < Dim[1]; ++col1) {
//       for (int col2 = col1; col2 < Dim[1]; ++col2) {
//         if (col1 == col2) {
//           for (int j = p[col1]; j < p[col1 + 1]; ++j)
//             res(col1, col1) += x[j] * x[j];
//         } else {
//           int col1_ind = p[col1], col1_max = p[col1 + 1];
//           int col2_ind = p[col2], col2_max = p[col2 + 1];
//           while (col1_ind < col1_max && col2_ind < col2_max) {
//             int row1 = i[col1_ind];
//             int row2 = i[col2_ind];
//             if (row1 == row2) {
//               res(col1, col2) += x[col1_ind] * x[col2_ind];
//               ++col1_ind;
//               ++col2_ind;
//             } else if (row1 < row2) {
//               do {
//                 ++col1_ind;
//               } while (i[col1_ind] < row2 && col1_ind < col1_max);
//             } else if (row2 < row1) {
//               do {
//                 ++col2_ind;
//               } while (i[col2_ind] < row1 && col2_ind < col2_max);
//             }
//           }
//           res(col2, col1) = res(col1, col2);
//         }
//       }
//     }
//     return res;
//   }

  // return indices of columns with nonzero values for a given row
  // this function is similar to Rcpp::Range, but unlike Rcpp::Range it is thread-safe
  std::vector<unsigned int> InnerIndices(int row) {
    std::vector<unsigned int> v(rowpointers[row + 1] - rowpointers[row]);
    for (R_xlen_t i = 0, it = rowpointers[row]; it < rowpointers[row + 1]; ++i, ++it)
      v[i] = (unsigned int)colindices[it];
    return v;
  }

  // return indices of columns with zeros values for a given row
  std::vector<unsigned int> emptyInnerIndices(int row) {
    // first get indices of non-zeros
    std::vector<unsigned int> nonzeros = InnerIndices(row);
    std::vector<unsigned int> all_vals(dimension[1]);
    std::iota(all_vals.begin(), all_vals.end(), 0);
    std::vector<unsigned int> zeros;
    std::set_difference(all_vals.begin(), all_vals.end(), nonzeros.begin(), nonzeros.end(),
                        std::inserter(zeros, zeros.begin()));
    return zeros;
  }

  // const row iterator
  class InnerIterator {
  public:
    InnerIterator(Matrix& ptr, int row) : ptr(ptr), row_(row), index(ptr.rowpointers[row]), max_index(ptr.rowpointers[row + 1]) {}
    operator bool() const { return (index < max_index); }
    InnerIterator& operator++() {
      ++index;
      return *this;
    }
    const double& value() const { return ptr.entries[index]; }
    int col() const { return ptr.colindices[index]; }
    int row() const { return row_; }

  private:
    Matrix& ptr;
    int row_;
    R_xlen_t index, max_index;
  };

  // equivalent to the "Forward Range" concept in two boost::ForwardTraversalIterator
  // iterates over non-zero values in `ptr.col(col)` at rows in `s`
  // `s` must be sorted in ascending order
  class InnerIteratorInRange {
  public:
    InnerIteratorInRange(Matrix& ptr, int row, std::vector<unsigned int>& s) : ptr(ptr), s(s), row_(row), index(ptr.rowpointers[row]), max_index(ptr.rowpointers[row + 1] - 1), s_max_index(s.size() - 1) {
      // decrement max_index and s_max_index to last case where ptr.i intersects with s
      while ((unsigned int)ptr.colindices[max_index] != s[s_max_index] && max_index >= index && s_max_index >= 0)
        s[s_max_index] > (unsigned int)ptr.colindices[max_index] ? --s_max_index : --max_index;
      // increment index to the first case where ptr.i intersects with s
      while ((unsigned int)ptr.colindices[index] != s[s_index] && index <= max_index && s_index <= s_max_index)
        s[s_index] < (unsigned int)ptr.colindices[index] ? ++s_index : ++index;
    }
    operator bool() const { return (index <= max_index && s_index <= s_max_index); }
    InnerIteratorInRange& operator++() {
      ++index;
      ++s_index;
      while (index <= max_index && s_index <= s_max_index && (unsigned int)ptr.colindices[index] != s[s_index])
        s[s_index] < (unsigned int)ptr.colindices[index] ? ++s_index : ++index;
      return *this;
    }
    const double& value() const { return ptr.entries[index]; }
    int col() const { return ptr.colindices[index]; }
    int row() const { return row_; }

  private:
    Matrix& ptr;
    const std::vector<unsigned int>& s;
    R_xlen_t index, max_index, s_max_index, s_index = 0; //, s_size;
    int row_;
  };

  // iterates over non-zero values in ptr.col(col) not at rows in s_
  // basically, turn this into InnerIteratorInRange by computing a vector `s` of non-intersecting
  //    non-zero rows in ptr at time of initialization
  // s must be sorted in ascending order
  class InnerIteratorNotInRange {
  public:
    InnerIteratorNotInRange(Matrix& ptr, int row, std::vector<unsigned int>& s_) : ptr(ptr), row_(row), index(ptr.rowpointers[row]), max_index(ptr.rowpointers[row + 1] - 1) {
      s = std::vector<unsigned int>(ptr.rowpointers[row_ + 1] - ptr.rowpointers[row]);
      if (s.size() > 0) {
        for (int j = 0, it = ptr.rowpointers[row_]; it < ptr.rowpointers[row_ + 1]; ++j, ++it)
          s[j] = (unsigned int)ptr.colindices[it];
        if (s_.size() > 0) {
          // remove intersecting values in s_ from s
          unsigned int si = 0, s_i = 0, z_i = 0;
          std::vector<unsigned int> z = s;
          while (si < s.size()) {
            if (s_i > s_.size() || s_[s_i] > s[si]) {
              z[z_i] = s[si];
              ++si;
              ++z_i;
            } else if (s_[s_i] == s[si]) {
              ++si;
              ++s_i;
            } else
              ++s_i;
          }
          z.resize(z_i);
          s = z;
        }
      }
      s_max_index = s.size() - 1;

      // decrement max_index and s_max_index to last case where ptr.i intersects with s
      while ((unsigned int)ptr.colindices[max_index] != s[s_max_index] && max_index >= index && s_max_index >= 0)
        s[s_max_index] > (unsigned int)ptr.colindices[max_index] ? --s_max_index : --max_index;
      // increment index to the first case where ptr.i intersects with s
      while ((unsigned int)ptr.colindices[index] != s[s_index] && index <= max_index && s_index <= s_max_index)
        s[s_index] < (unsigned int)ptr.colindices[index] ? ++s_index : ++index;
    }
    operator bool() const { return (index <= max_index && s_index <= s_max_index); }
    InnerIteratorNotInRange& operator++() {
      ++index;
      ++s_index;
      while (index <= max_index && s_index <= s_max_index && (unsigned int)ptr.colindices[index] != s[s_index])
        s[s_index] < (unsigned int)ptr.colindices[index] ? ++s_index : ++index;
      return *this;
    }
    const double& value() const { return ptr.entries[index]; }
    int col() const { return ptr.colindices[index]; }
    int row() const { return row_; }

  private:
    Matrix& ptr;
    std::vector<unsigned int> s;
    R_xlen_t index, max_index, s_max_index, s_index = 0; //, s_size;
    int row_;
  };

  // const col iterator
  class InnerColIterator {
  public:
    InnerColIterator(Matrix& ptr, int j) : ptr(ptr) {
      for (; index < ptr.dimension[0]; ++index) {
        if (ptr.colindices[index] == j) break;
      }
      for (int r = 0; r < ptr.colindices.size(); ++r)
        if (ptr.colindices[r] == j) max_index = r;
    }
    operator bool() const { return index <= max_index; };
    InnerColIterator& operator++() {
      ++index;
      for (; index <= max_index; ++index) {
        if (ptr.colindices[index] == col_) break;
      }
      return *this;
    };
    int row() {
      int j = 0;
      for (; j < ptr.rowpointers.size(); ++j) {
        if (ptr.rowpointers[j] > index) break;
      }
      return j;
    };
    int col() { return col_; }
    double& value() const { return ptr.entries[index]; };

  private:
    Matrix& ptr;
    int col_ = 0;
    R_xlen_t index = 0, max_index = 0;
  };

  // number of nonzeros in a row
  unsigned int InnerNNZs(int row) {
    return rowpointers[row + 1] - rowpointers[row];
  }

  // is approximately symmetric
  bool isAppxSymmetric() {
    if (dimension[0] == dimension[1]) {
      InnerIterator row_it(*this, 0);
      InnerColIterator col_it(*this, 0);
      while (++row_it && ++col_it) {
        if (row_it.value() != col_it.value())
          return false;
      }
      return true;
    }
    return false;
  }

  // Matrix transpose() {
  //   Rcpp::S4 s(std::string("dgCMatrix"));
  //   s.slot("i") = i;
  //   s.slot("p") = p;
  //   s.slot("x") = x;
  //   s.slot("Dim") = Dim;
  //   Rcpp::Environment base = Rcpp::Environment::namespace_env("Matrix");
  //   Rcpp::Function t_r = base["t"];
  //   Rcpp::S4 At = t_r(Rcpp::_["x"] = s);
  //   return Matrix(At);
  // };

  Rcpp::S4 wrap() {
    Rcpp::S4 s(std::string("spam"));
    s.slot("entries") = entries;
    s.slot("colindices") = colindices;
    s.slot("rowpointers") = rowpointers;
    s.slot("dimension") = dimension;
    return s;
  }
};
}  // namespace RcppSpam

namespace Rcpp {
namespace traits {

template <>
class Exporter<RcppSpam::Matrix> {
  Rcpp::NumericVector entries_, colindices, rowpointers;
  Rcpp::IntegerVector dimension;

public:
  Exporter(SEXP x) {
    Rcpp::S4 s(x);
    if (!s.hasSlot("entries") || !s.hasSlot("rowpointers") || !s.hasSlot("colindices") || !s.hasSlot("dimension"))
      throw std::invalid_argument("Cannot construct RcppSpam::Matrix from this S4 object");
    entries_ = s.slot("entries");
    colindices = s.slot("colindices");
    rowpointers = s.slot("rowpointers");
    dimension = s.slot("dimension");
  }

  RcppSpam::Matrix get() {
    return RcppSpam::Matrix(entries_, colindices, rowpointers, dimension);
  }
};

}  // namespace traits
}  // namespace Rcpp
