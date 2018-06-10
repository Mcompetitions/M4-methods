#include "builders.h"

std::string trim_line(const std::string &line, const std::function<int(int)> &is_empty_char) {
  if (line.empty()) {
    return line;
  }

  size_t first_non_empty = 0;
  while (is_empty_char(line[first_non_empty])) {
    ++first_non_empty;
    if (first_non_empty == line.size()) {
      return std::string();
    }
  }

  size_t last_non_empty = line.size();
  while (is_empty_char(line[--last_non_empty]));

  return line.substr(first_non_empty, last_non_empty - first_non_empty + 1);
}

itp::Pointwise_predictor_ptr<unsigned char> Forecasting_algorithm_discrete::make_predictor(itp::Codes_lengths_computer_ptr computer, itp::Sampler_ptr sampler, size_t difference) const {
  auto dpredictor = std::make_shared<itp::Discrete_distribution_predictor>(computer, sampler,
                                                                           difference);
  return std::make_shared<itp::Basic_pointwise_predictor<unsigned char>>(dpredictor);
}

void Forecasting_algorithm_real::set_quants_count(size_t n){
  quants_count = n;
}

itp::Pointwise_predictor_ptr<double> Forecasting_algorithm_real::make_predictor(itp::Codes_lengths_computer_ptr computer,
                                                                        itp::Sampler_ptr sampler, size_t difference) const {
  auto dpredictor = std::make_shared<itp::Real_distribution_predictor>(computer, sampler, quants_count, difference);
  return std::make_shared<itp::Basic_pointwise_predictor<double>>(dpredictor);
}

itp::Pointwise_predictor_ptr<double> Forecasting_algorithm_multialphabet::make_predictor(itp::Codes_lengths_computer_ptr computer, itp::Sampler_ptr sampler, size_t difference) const {
  auto dpredictor = std::make_shared<itp::Multialphabet_distribution_predictor>(computer, sampler,
                                                                                quants_count,
                                                                                difference);
  return std::make_shared<itp::Basic_pointwise_predictor<double>>(dpredictor);
}

void check_args(size_t horizont, size_t difference, int sparse) {
  if (50 < horizont) {
    throw std::invalid_argument("Forecasting horizont is too long (> 50)");
  }

  if (10 < difference) {
    throw std::invalid_argument("Difference order is too big (> 10)");
  }

  if (20 < sparse) {
    throw std::invalid_argument("Sparse value is too big (> 20)");
  }
}

inline void check_quants_count_range(size_t quants_count) {
  if ((0 == quants_count) || (256 < quants_count)) {
    throw std::invalid_argument("Quants count should be greater than zero and not greater than 256");
  }
}

std::map<std::string, std::vector<double>>
  make_forecast_real(const std::vector<double> &time_series,
                     const itp::Names &compressors_groups, size_t horizont,
                     size_t difference, size_t quants_count, int sparse) {
  check_args(horizont, difference, sparse);
  check_quants_count_range(quants_count);
  
  Forecasting_algorithm_real make_forecast;
  make_forecast.set_quants_count(quants_count);
  return make_forecast(time_series, compressors_groups, horizont, difference, sparse);
}

std::map<std::string, std::vector<double>>
  make_forecast_multialphabet(const std::vector<double> &history,
                              const itp::Names &compressors_groups, size_t horizont,
                              size_t difference, size_t max_quants_count, int sparse) {
  check_args(horizont, difference, sparse);
  check_quants_count_range(max_quants_count);
  if (!itp::is_power_of_two(max_quants_count)) {
    throw std::invalid_argument("Max quants count should be greater a power of two.");
  }
  
  Forecasting_algorithm_multialphabet make_forecast;
  make_forecast.set_quants_count(max_quants_count);
  return make_forecast(history, compressors_groups, horizont, difference, sparse);
}

std::map<std::string, std::vector<double>>
  make_forecast_discrete(const std::vector<unsigned char> &history,
                         const std::vector<std::string> &compressors_groups, size_t horizont,
                         size_t difference, int sparse) {
  check_args(horizont, difference, sparse);
  
  Forecasting_algorithm_discrete make_forecast;
  return make_forecast(history, compressors_groups, horizont, difference, sparse);
}
