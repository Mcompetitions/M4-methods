#ifndef PREDICTOR_PREDICTOR_H
#define PREDICTOR_PREDICTOR_H

#include "continuation.h"

#include <iostream>
#include <memory>
#include <cmath>

// Interface of the library.
namespace itp {
  Names split_concatenated_names(std::string concatenated_names,
                                 char separator = '_');

  /**
   * Splits vector of strings, in which each string consists of compressors's names
   * separated by the separator character,
   * to a vector of vectors with compressors names.
   *
   * @param concatenated_names Vector of concatenated compressor's names.
   * @param separator Names in concatenated_names should be separated by that
   * character.
   *
   * @return Vector of vectors of names, splitted by the separator.
   */
  std::vector<Names> split_concatenated_names(const std::vector<std::string>
                                              &concatenated_names,
                                              char separator = '_');

  /**
   * Concatenates vector of names to a single name using separator.
   *
   * @param compressors Vector of compressor's names.
   * @param separator Separator for names in a single string.
   *
   * @return Concatenated name.
   */
  std::string concatenate(const Names &compressors, char separator = '_');

  /**
   * Find all distinct compressor's name in the vector of vectors of names.
   *
   * @param compressors Vector of vector of names.
   *
   * @return Vector of unique names.
   */
  Names find_all_distinct_names(const std::vector<Names> &compressors);

  inline bool is_power_of_two(std::size_t n) {
    return (n > 0 && ((n & (n - 1)) == 0));
  }

  template <typename T>
    struct Diff_result;

  /**
   * Memento pattern implementation. Contains information necessary to
   * integrate differentized time series.
   *
   */
  template <typename T>
    class Integration_info {
  public:
    Integration_info() = default;
  private:
  Integration_info(size_t difference, const std::vector<T> &last_values)
    : difference{difference}, last_values{last_values} {}

    size_t difference;
    std::vector<T> last_values;

    template <typename U>
      friend Diff_result<U> diff_n(Time_series<U> x, size_t n);

    template <typename U>
      friend void integrate(Forecast &forecast, const Integration_info<U> &info);
  };

  template <typename T>
    struct Diff_result {
      Time_series<T> time_series;
      Integration_info<T> integration_info;
    };

  /**
   * Takes n-th difference of the specified time series.
   *
   * @param x Time series to differentization.
   * @param n Differentize the time series n times.
   *
   * @return Differentized time series.
   */
  template <typename T>
    Diff_result<T> diff_n(Time_series<T> x, size_t n) {
    std::vector<T> last_values;
    for (size_t i = 1; i <= n; ++i) {
      last_values.push_back(x[x.size()-i]);
      for (size_t j = 0; j < x.size() - i; ++j) {
        x[j] = x[j+1] - x[j];
      }
    }
    x.erase(end(x)-n, end(x));

    assert(last_values.size() == n);

    Integration_info<T> info{n, last_values};
    return Diff_result<T>{x, info};
  }

  template <typename T>
    void integrate(Forecast &forecast, const Integration_info<T> &info) {
    assert(info.last_values.size() == info.difference);

    size_t index = info.last_values.size();
    for (size_t i = 1; i <= info.difference; ++i) {
      --index;
      for (const auto &compressor : forecast.get_index()) {
        forecast(compressor, 0).point += info.last_values[index];
        forecast(compressor, 0).left_border += info.last_values[index];
        forecast(compressor, 0).right_border += info.last_values[index];
        for (size_t j = 1; j < forecast.factors_size(); ++j) {
          forecast(compressor, j).point += forecast(compressor, j - 1).point;
          forecast(compressor, j).left_border +=
            forecast(compressor, j - 1).left_border;
          forecast(compressor, j).right_border +=
            forecast(compressor, j - 1).right_border;
        }
      }
    }
  }

  template <typename Forward_iterator>
    Codes_table::Value_type min_value_of_all_tables(Forward_iterator first, Forward_iterator last) {
    Codes_table::Value_type global_minimum {-1};
    while (first != last) {
      auto local_minimum = *std::min_element(begin(*first), end(*first));
      ++first;
      if ((local_minimum < global_minimum) || (global_minimum < 0)) {
        global_minimum = local_minimum;
      }
    }

    return global_minimum;
  }

  template <typename Forward_iterator, typename T>
    void add_value_to_each(Forward_iterator first, Forward_iterator last, T value) {
    while (first != last) {
      *first += value;
      ++first;
    }
  }

  class Codes_lengths_computer {
  public:
    using Trajectories = std::vector<Continuation<Symbol_t>>;

    virtual ~Codes_lengths_computer() = default;

    virtual Codes_table append_each_trajectory_and_compute(const Discrete_time_series &history,
                                                           size_t alphabet,
                                                           size_t length_of_continuation,
                                                           const Names &compressors_to_compute,
                                                           const Trajectories &possible_continuations) const;
    virtual Codes_table append_each_trajectory_and_compute(const Discrete_time_series &history,
                                                           size_t alphabet,
                                                           size_t length_of_continuation,
                                                           const Names &compressors_to_compute) const;
  private:
    static constexpr size_t BITS_IN_BYTE = 8;
  };

  class Weights_generator {
  public:
    virtual ~Weights_generator() = default;

    virtual std::vector<Double_t> generate(size_t n) const;
  };

  class Countable_weights_generator : public Weights_generator {
  public:
    virtual std::vector<Double_t> generate(size_t n) const;
  };

  using Codes_lengths_computer_ptr = std::shared_ptr<Codes_lengths_computer>;
  using Sampler_ptr = std::shared_ptr<Sampler>;
  using Weights_generator_ptr = std::shared_ptr<Weights_generator>;

  template <typename Forward_iterator>
    inline void to_code_probabilities(Forward_iterator first, Forward_iterator last) {
    typename Forward_iterator::value_type tmp;
    while (first != last) {
      *first = pow(2, -(*first));
      ++first;
    }
  }
  
  void form_group_forecasts(Codes_table &code_probabilities,
                            const std::vector<Names> &compressors_groups,
                            Weights_generator_ptr weights_generator);
  Codes_table to_probabilities(Codes_table code_probabilities);
  Codes_table merge(const std::vector<Codes_table> &tables,
                    const std::vector<size_t> &alphabets,
                    const std::vector<Double_t> &weights);
  Forecast to_pointwise_forecasts(const Codes_table &, size_t, Sampler_ptr,
                                  const Desample_info &, double = 0.95);
  Symbols_distributions cumulated_for_step(const Codes_table &table,
                                           std::size_t step);

  template <typename T>
    struct Code_probabilities_result {
      Codes_table code_probabilities;
      Sampler_ptr sampler;
      Desample_info desample_info;
      Integration_info<T> integration_info;
    };

  template <typename T>
    class Distribution_predictor {
    static_assert(std::is_arithmetic<T>::value, "T should be an arithmetic type");
  public:
    virtual ~Distribution_predictor() = default;

    virtual Code_probabilities_result<T>
      predict(const Time_series<T> &ts, size_t horizont,
	      const std::vector<Names> &compressors) const = 0;
  };

  template <typename T>
    class Compression_based_predictor : public Distribution_predictor<T> {
  public:
    explicit Compression_based_predictor(size_t difference_order=0);
    explicit Compression_based_predictor(Weights_generator_ptr weights_generator,
                                         size_t difference_order = 0);

    Code_probabilities_result<T> predict(const Time_series<T> &history, size_t horizont,
                                         const std::vector<Names> &compressors) const override final;

    void set_difference_order(size_t n);
    size_t get_difference_order() const;

  protected:
    virtual Code_probabilities_result<T> obtain_code_probabilities(const Time_series<T> &history,
                                                                   size_t horizont,
                                                                   const Names &compressors) const = 0;
  private:
    Weights_generator_ptr weights_generator;
    size_t difference_order;
  };

  class Multialphabet_distribution_predictor : public Compression_based_predictor<double> {
  public:
    Multialphabet_distribution_predictor() = delete;
    Multialphabet_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
					 Sampler_ptr sampler, size_t max_q, size_t difference=0);

    Code_probabilities_result<double>
      obtain_code_probabilities(const Time_series<double> &ts, size_t horizont,
				const Group &compressors) const override;
  private:
    Codes_lengths_computer_ptr codes_lengths_computer;
    Sampler_ptr sampler;
    size_t log2_max_partition_cardinality;
    Weights_generator_ptr partitions_weights_gen;
  };

  struct Sampling_result_with_alphabet {
    Sampling_result sampling_result;
    size_t alphabet;
  };

  template <typename T>
    class Single_alphabet_distribution_predictor : public Compression_based_predictor<T> {
  public:
    Single_alphabet_distribution_predictor() = delete;
    explicit Single_alphabet_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
                                                    size_t difference=0);
  protected:
    Code_probabilities_result<T> obtain_code_probabilities(const Time_series<T> &history,
                                                           size_t horizont,
                                                           const Names &compressors) const override final;
    virtual Sampling_result_with_alphabet sample(const Time_series<T> &history) const = 0;
  private:
    Codes_lengths_computer_ptr codes_lengths_computer;
  };

  class Real_distribution_predictor : public Single_alphabet_distribution_predictor<double> {
  public:
    Real_distribution_predictor() = delete;
    Real_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
                                Sampler_ptr sampler, size_t partition_cardinality,
                                size_t difference=0);
  protected:
    Sampling_result_with_alphabet sample(const Time_series<double> &history) const override;
  private:
    Sampler_ptr sampler;
    size_t partition_cardinality;
  };

  class Discrete_distribution_predictor : public Single_alphabet_distribution_predictor<Symbol_t> {
  public:
    Discrete_distribution_predictor() = delete;
    Discrete_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
                                    Sampler_ptr sampler, size_t difference=0);
  protected:
    Sampling_result_with_alphabet sample(const Time_series<Symbol_t> &history) const override;
  private:
    Sampler_ptr sampler;
  };

  template <typename T> using Distribution_predictor_ptr = std::shared_ptr<Distribution_predictor<T>>;
  using Discrete_distribution_predictor_ptr = std::shared_ptr<Discrete_distribution_predictor>;
  using Real_distribution_predictor_ptr = std::shared_ptr<Real_distribution_predictor>;

  template <typename T>
    class Pointwise_predictor {
    static_assert(std::is_arithmetic<T>::value, "T should be an arithmetic type");
  public:
    virtual ~Pointwise_predictor() = default;

    virtual Forecast predict(const Time_series<T> &ts, size_t horizont,
                             const std::vector<Names> &compressors) const = 0;
  };

  template <typename T> using Pointwise_predictor_ptr = std::shared_ptr<Pointwise_predictor<T>>;

  template <typename T>
    class Basic_pointwise_predictor : public Pointwise_predictor<T> {
  public:
    Basic_pointwise_predictor(Distribution_predictor_ptr<T> distribution_predictor);
    Forecast predict(const Time_series<T> &ts, size_t horizont,
                     const std::vector<Names> &compressors) const override;
  private:
    Distribution_predictor_ptr<T> distribution_predictor;
  };

  /**
   * Decorator pattern implementation.
   *
   */
  template <typename T>
    class Sparse_predictor : public Pointwise_predictor<T> {
  public:
    Sparse_predictor(Pointwise_predictor_ptr<T> pointwise_predictor, size_t sparse);

    Forecast predict(const Time_series<T> &history, size_t horizont,
                     const std::vector<Names> &compressors) const override final;
  private:
    Pointwise_predictor_ptr<T> pointwise_predictor;
    size_t sparse;
  };

  template <typename T>
    Compression_based_predictor<T>::Compression_based_predictor(size_t difference_order)
    : Compression_based_predictor<T> {std::make_shared<Weights_generator>(), difference_order} {}

  template <typename T>
    Compression_based_predictor<T>::Compression_based_predictor(Weights_generator_ptr
								weights_generator,
								size_t difference_order)
    : weights_generator {weights_generator}, difference_order {difference_order} {}

  template <typename T>
    void Compression_based_predictor<T>::set_difference_order(size_t n) {
    difference_order = n;
  }

  template <typename T>
    size_t Compression_based_predictor<T>::get_difference_order() const {
    return difference_order;
  }

  template <typename T>
    Code_probabilities_result<T> Compression_based_predictor<T>::predict(const Time_series<T> &history,
									 size_t horizont,
									 const std::vector<Names> &compressors) const  {
    auto differentized_history = diff_n(history, difference_order);
    auto distinct_single_compressors = find_all_distinct_names(compressors);
    auto code_probabilities_result =
      obtain_code_probabilities(differentized_history.time_series, horizont,
                                distinct_single_compressors);
    form_group_forecasts(code_probabilities_result.code_probabilities, compressors, weights_generator);
    auto probabilities = to_probabilities(code_probabilities_result.code_probabilities);
    return {probabilities, code_probabilities_result.sampler,
        code_probabilities_result.desample_info,
        differentized_history.integration_info};
  }

  template <typename T>
    Single_alphabet_distribution_predictor<T>::Single_alphabet_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
										      size_t difference)
    : codes_lengths_computer {codes_lengths_computer} {}

  template <typename T>
    Code_probabilities_result<T>
    Single_alphabet_distribution_predictor<T>::obtain_code_probabilities(const Time_series<T> &history,
									 size_t horizont,
									 const Names &compressors) const {
    auto sampling_result_with_alphabet = sample(history);
    auto &time_series = sampling_result_with_alphabet.sampling_result.time_series;
    auto &desample_info = sampling_result_with_alphabet.sampling_result.desample_info;
    auto &alphabet = sampling_result_with_alphabet.alphabet;
    auto table = codes_lengths_computer->append_each_trajectory_and_compute(time_series,
                                                                            alphabet,
                                                                            horizont, compressors);
    to_code_probabilities(begin(table), end(table));
    return {table, sampling_result_with_alphabet.sampling_result.sampler,
        desample_info};
  }

  template <typename T>
    Basic_pointwise_predictor<T>::Basic_pointwise_predictor(Distribution_predictor_ptr<T> distribution_predictor)
    : distribution_predictor {distribution_predictor} {}

  template <typename T>
    Forecast Basic_pointwise_predictor<T>::predict(const Time_series<T> &ts, size_t horizont,
						   const std::vector<Names> &compressors) const {
    auto distribution = distribution_predictor->predict(ts, horizont, compressors);
    auto forecasts = to_pointwise_forecasts(distribution.code_probabilities,
                                            horizont,
                                            distribution.sampler,
                                            distribution.desample_info);
    integrate(forecasts, distribution.integration_info);
    return forecasts;
  }

  template <typename T>
    Sparse_predictor<T>::Sparse_predictor(Pointwise_predictor_ptr<T> pointwise_predictor,
					  size_t sparse)
    : pointwise_predictor {pointwise_predictor}, sparse {sparse} {
    assert(pointwise_predictor != nullptr);
  }

  template <typename T>
    Forecast Sparse_predictor<T>::predict(const Time_series<T> &history, size_t horizont,
					  const std::vector<Names> &compressors) const {
    std::vector<Forecast> results(sparse);
    size_t sparsed_horizont = ceil(horizont / static_cast<double>(sparse));
    for (size_t i = 0; i < sparse; ++i) {
      Time_series<T> sparse_ts_data;
      for (size_t j = i; j < history.size(); j += sparse) {
        sparse_ts_data.push_back(history[j]);
      }
      results[i] = pointwise_predictor->predict(sparse_ts_data, sparsed_horizont, compressors);
    }

    Forecast full_first_steps = pointwise_predictor->predict(history, sparsed_horizont, compressors);
    Forecast result;
    for (size_t i = 0; i < sparsed_horizont; ++i) {
      for (const auto &compressor : full_first_steps.get_index()) {
        result(compressor, i) = full_first_steps(compressor, i);
      }
    }

    for (size_t i = 0; i < sparsed_horizont; ++i) {
      for (size_t j = 0; j < sparse; ++j) {
        if ((i*sparse+j >= sparsed_horizont) && (i*sparse+j < horizont)) {
          for (const auto compressor : full_first_steps.get_index()) {
            result(compressor, i*sparse+j) = results[j](compressor, i);
          }
        }
      }
    }
    return result;
  }
}

#endif //PREDICTOR_PREDICTOR_H
