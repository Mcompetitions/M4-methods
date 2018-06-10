/**
 * @file   builders.h
 * @author Konstantin <user10101@user10101-Satellite-L855>
 * @date   Fri Jun  8 21:26:30 2018
 *
 * @brief  Contains auxilary functions and classes to facilitate creation of algorithm
 * with specific parameters.
 *
 *
 */
#ifndef BUILDERS_H_INCLUDED
#define BUILDERS_H_INCLUDED

#include <predictor.h>

#include <functional>
#include <memory>

/**
 * Removes empty symbols from start and end of a line.
 *
 * @param line String in which empty symbols should be removed.
 * @param is_empty_char Function to determine which character is empty or not
 * (return not zero if empty).
 *
 * @return Trimmed line.
 */
std::string trim_line(const std::string &line, const std::function<int(int)> &is_empty_char);

template <typename T>
class Forecasting_algorithm {
  static_assert(std::is_arithmetic<T>::value, "T should be an arithmetic type");
public:
  std::map<std::string, std::vector<double>>
    operator () (const std::vector<T> &time_series,
                 const itp::Names &compressors_groups, size_t horizont,
                 size_t difference, int sparse);

protected:

  /**
 * Factory method.
 *
 */
  virtual itp::Pointwise_predictor_ptr<T> make_predictor(itp::Codes_lengths_computer_ptr computer,
                                                         itp::Sampler_ptr sampler,
							 size_t difference) const = 0;
};

template <typename T>
std::map<std::string, std::vector<double>>
  Forecasting_algorithm<T>::operator () (const std::vector<T> &time_series,
               const itp::Names &compressors_groups, size_t horizont,
               size_t difference, int sparse) {
  auto computer {std::make_shared<itp::Codes_lengths_computer>()};
  auto indent {0.1};
  auto sampler {std::make_shared<itp::Sampler>(indent)};
  std::vector<itp::Names> compressors {
    itp::split_concatenated_names(compressors_groups)};
  itp::Pointwise_predictor_ptr<T> pointwise_predictor = make_predictor(computer, sampler, difference);
  if (sparse > 0) {
    pointwise_predictor = std::make_shared<itp::Sparse_predictor<T>>(pointwise_predictor, sparse);
  }

  itp::Forecast res = pointwise_predictor->predict(time_series, horizont, compressors);
  std::map<std::string, std::vector<double>> ret;
  for (const auto &compressor : res.get_index()) {
    ret[compressor] = std::vector<double>(horizont);
    for (size_t i = 0; i < horizont; ++i) {
      ret[compressor][i] = res(compressor, i).point;
    }
  }

  return ret;
}

/**
 * Forecast originally discrete time series.
 *
 */
class Forecasting_algorithm_discrete : public Forecasting_algorithm<unsigned char> {
protected:
  itp::Pointwise_predictor_ptr<unsigned char> make_predictor(itp::Codes_lengths_computer_ptr computer,
                                                             itp::Sampler_ptr sampler, size_t difference) const override;
};

/**
 * Forecast originally real-valued time series with discretization (discretize with
 * only one partition cardinality).
 *
 */
class Forecasting_algorithm_real : public Forecasting_algorithm<double> {
public:
  void set_quants_count(size_t n);
protected:
itp::Pointwise_predictor_ptr<double> make_predictor(itp::Codes_lengths_computer_ptr computer,
                                                    itp::Sampler_ptr sampler, size_t difference) const override;
  size_t quants_count;
};

class Forecasting_algorithm_multialphabet : public Forecasting_algorithm_real {
protected:
  itp::Pointwise_predictor_ptr<double> make_predictor(itp::Codes_lengths_computer_ptr computer,
						      itp::Sampler_ptr sampler,
						      size_t difference) const override;
};

void check_args(size_t horizont, size_t difference, size_t quants_count, int sparse);

std::map<std::string, std::vector<double>>
make_forecast_real(const std::vector<double> &time_series,
                   const itp::Names &compressors_groups, size_t horizont,
                   size_t difference, size_t quants_count, int sparse);

std::map<std::string, std::vector<double>>
make_forecast_multialphabet(const std::vector<double> &history,
                            const itp::Names &compressors_groups, size_t horizont,
                            size_t difference, size_t max_quants_count, int sparse);

std::map<std::string, std::vector<double>>
make_forecast_discrete(const std::vector<unsigned char> &history,
                       const std::vector<std::string> &compressors_groups, size_t horizont,
                       size_t difference, int sparse);
#endif // BUILDERS_H_INCLUDED
