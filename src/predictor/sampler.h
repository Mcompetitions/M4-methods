/**
 * Functions for sampling and desampling real time series.
 */
#ifndef SAMPLER_H
#define SAMPLER_H

#include "dtypes.h"

#include <vector>
#include <utility>
#include <cassert>
#include <memory>

namespace itp {
  class Sampler;
  using Sampler_ptr = std::shared_ptr<Sampler>;

  class Desample_info {
  public:
    Desample_info() = default;
  private:
    Desample_info(const std::vector<Real_time_series::value_type> &);
    Desample_info(std::vector<Real_time_series::value_type> &&);

    friend class Sampler;

    friend std::pair<Discrete_time_series, Desample_info>
    sample(const Real_time_series &, size_t, Real_time_series::value_type);

    friend std::pair<Discrete_time_series, Desample_info>
    normalize(const Discrete_time_series &);

    friend Real_time_series::value_type
    desample(Discrete_time_series::value_type s, const Desample_info &info);

    std::vector<Real_time_series::value_type> table;
  };

  struct Sampling_result {
    Discrete_time_series time_series;
    Sampler_ptr sampler;
    Desample_info desample_info;
  };

  class Sampler {
  public:
    Sampler() = default;
    explicit Sampler(double indent);

    void set_indent(double new_ident);
    double get_indent() const;

    virtual Sampler* clone();

    virtual Sampling_result sample(const Real_time_series &, size_t);
    virtual Sampling_result normalize(const Discrete_time_series &);
    virtual Real_time_series::value_type
    desample(Discrete_time_series::value_type s, const Desample_info &info);
  private:
    double indent = 0.1;
  };
} // of itp

#endif // SAMPLER_H
