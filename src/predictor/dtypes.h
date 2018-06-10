#ifndef DTYPES_INCLUDED
#define DTYPES_INCLUDED

#include "bignums.h"

#include <iostream>
#include <vector>
#include <string>

#define _S(text) std::string(text)

namespace itp {
  using Symbol_t = unsigned char;
  using Double_t = Big_double<3, 6>;
  //using Double_t = long double;

  using Group = std::vector<std::string>;
  template <typename T>
  using Time_series = std::vector<T>;
  using Real_time_series = std::vector<double>;
  using Discrete_time_series = std::vector<Symbol_t>;

  using Names = std::vector<std::string>;
} // of itp

#endif // DTYPES_INCLUDED
