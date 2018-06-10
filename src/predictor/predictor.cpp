#include "predictor.h"
#include "sampler.h"
#include "compressors.h"

#include <cassert>
#include <fstream>
#include <algorithm>
#include <sstream>

namespace itp {
  Names split_concatenated_names(std::string concatenated_names, char separator) {
    std::istringstream iss(concatenated_names);
    Names result;
    std::string name;
    while (std::getline(iss, name, separator)) {
      result.push_back(name);
    }

    return result;
  }

  std::vector<Names> split_concatenated_names(const std::vector<std::string>
                                              &concatenated_names,
                                              char separator) {
    std::vector<Names> result;
    for (const auto &names : concatenated_names) {
      result.push_back(split_concatenated_names(names));
    }

    return result;
  }

  std::string concatenate(const Names &compressors, char separator) {
    if (compressors.size() == 0) {
      return std::string {};
    }
    std::ostringstream oss;
    oss << compressors[0];
    for (size_t i = 1; i < compressors.size(); ++i) {
      oss << separator << compressors[i];
    }

    return oss.str();
  }

  Names find_all_distinct_names(const std::vector<Names> &compressors) {
    Names list_of_all_names;
    std::for_each(begin(compressors), end(compressors), [&list_of_all_names](Names names) {
        std::copy(std::begin(names), std::end(names),
                  std::back_inserter(list_of_all_names));
      });
    std::sort(begin(list_of_all_names), end(list_of_all_names));

    Names unique_names;
    std::unique_copy(begin(list_of_all_names), end(list_of_all_names), std::back_inserter(unique_names));

    return unique_names;
  }

  Codes_table
  Codes_lengths_computer::append_each_trajectory_and_compute(const Discrete_time_series &history,
                                                             size_t alphabet,
                                                             size_t length_of_continuation,
                                                             const Names &compressors_to_compute,
                                                             const Trajectories &possible_continuations) const {
    assert(length_of_continuation <= 100);
    assert(alphabet > 0);

    Codes_table result(begin(possible_continuations), end(possible_continuations),
                       begin(compressors_to_compute), end(compressors_to_compute));
    size_t full_series_length = history.size() + length_of_continuation;
    std::unique_ptr<Symbol_t[]> buffer(new Symbol_t[full_series_length]);
    std::copy(history.cbegin(), history.cend(), buffer.get());
    for (const auto &continuation : possible_continuations) {
      std::copy(continuation.cbegin(), continuation.cend(), buffer.get()+history.size());

      for (size_t j = 0; j < result.factors_size(); ++j) {
        result(continuation, compressors_to_compute[j]) =
          Compressors_pool::get_instance()(compressors_to_compute[j], buffer.get(),
                                           history.size()+length_of_continuation)*BITS_IN_BYTE;
      }
    }

    return result;
  }

  Codes_table
  Codes_lengths_computer::append_each_trajectory_and_compute(const Discrete_time_series &history,
                                                             size_t alphabet,
                                                             size_t length_of_continuation,
                                                             const Names &compressors_to_compute) const {
    std::vector<Continuation<Symbol_t>> possible_continuations;
    Continuation<Symbol_t> continuation(alphabet, length_of_continuation);
    for (size_t i = 0; i < pow(alphabet, length_of_continuation); ++i) {
      possible_continuations.push_back(continuation++);
    }

    return append_each_trajectory_and_compute(history, alphabet, length_of_continuation,
                                              compressors_to_compute, possible_continuations);
  }

  std::vector<Double_t> Weights_generator::generate(size_t n) const {
    assert(0 < n);
    std::vector<Double_t> result(n);
    std::fill(begin(result), end(result), 1./n);
    return result;
  }

  std::vector<Double_t> Countable_weights_generator::generate(size_t n) const {
    assert(0 < n);

    std::vector<Double_t> result(n);
    for (size_t i = 0; i < n - 1; ++i) {
      result[i] = 1. / (i + 1.) - 1. / (i + 2.);
    }
    result[n - 1] = 1. / n;

    return result;
  }

  void form_group_forecasts(Codes_table &code_probabilities,
                            const std::vector<Names> &compressors_groups,
                            Weights_generator_ptr weights_generator) {
    for (const auto &group : compressors_groups) {
      if (group.size() > 1) {
        auto group_composite_name = concatenate(group);
        auto weights = weights_generator->generate(group.size());
        for (const auto &continuation : code_probabilities.get_index()) {
          code_probabilities(continuation, group_composite_name) = 0;
          for (size_t i = 0; i < group.size(); ++i) {
            code_probabilities(continuation, group_composite_name) +=
              code_probabilities(continuation, group[i]) * weights[i];
          }
        }
      }
    }
  }

  Codes_table to_probabilities(Codes_table code_probabilities) {
    Double_t cumulated_sum;
    for (const auto &compressor : code_probabilities.get_factors()) {
      cumulated_sum = .0;
      for (const auto &continuation : code_probabilities.get_index()) {
        cumulated_sum += code_probabilities(continuation, compressor);
      }

      for (const auto &continuation : code_probabilities.get_index()) {
        code_probabilities(continuation, compressor) /= cumulated_sum;
      }
    }

    return code_probabilities;
  }

  Codes_table merge(const std::vector<Codes_table> &tables, const std::vector<size_t> &alphabets,
                    const std::vector<Double_t> &weights)
  {
    assert(tables.size() == weights.size());
    assert(std::is_sorted(begin(alphabets), end(alphabets)));
    assert(std::all_of(begin(alphabets), end(alphabets), is_power_of_two));

    std::vector<size_t> steps(tables.size());
    auto maximal_alphabet = alphabets[alphabets.size() - 1];
    std::transform(begin(alphabets), end(alphabets), begin(steps), [&maximal_alphabet](size_t item) {
        return maximal_alphabet / item;
      });

    Codes_table result(tables[tables.size()-1]);
    for (const auto &continuation : result.get_index()) {
      for (const auto &compressor : result.get_factors()) {
        result(continuation, compressor) = .0;
        for (size_t i = 0; i < tables.size(); ++i) {
          result(continuation, compressor) += tables[i](continuation/steps[i], compressor)*weights[i];
        }
      }
    }

    return result;
  }

  Forecast to_pointwise_forecasts(const Codes_table &table, size_t h,
                                  Sampler_ptr sampler, const Desample_info &info,
                                  double confidence_probability) {
    Forecast result;
    for (size_t i = 0; i < h; ++i) {
      Symbols_distributions d = cumulated_for_step(table, i);
      for (auto compressor : d.get_factors()) {
        auto confidence_interval = find_confidence_interval(d, confidence_probability);
        result(compressor, i).point = mean(d, compressor, sampler, info);
        result(compressor, i).left_border = sampler->desample(confidence_interval.first, info);
        result(compressor, i).right_border = sampler->desample(confidence_interval.second, info);
      }
    }

    return result;
  }

  Symbols_distributions cumulated_for_step(const Codes_table &table, std::size_t step) {
    assert(step <= 1000);

    Symbols_distributions result;
    for (const auto &continuation : table.get_index()) {
      for (const auto &compressor : table.get_factors()) {
        result(continuation[step], compressor) = 0;
      }
    }

    for (const auto &continuation : table.get_index()) {
      for (const auto &compressor : table.get_factors()) {
        result(continuation[step], compressor) += table(continuation, compressor);
      }
    }

    return result;
  }

  Multialphabet_distribution_predictor::Multialphabet_distribution_predictor(Codes_lengths_computer_ptr
                                                                             codes_lengths_computer,
                                                                             Sampler_ptr sampler,
                                                                             size_t max_q,
                                                                             size_t difference_order)
    : Compression_based_predictor<double>{difference_order},
    codes_lengths_computer{codes_lengths_computer}, sampler{sampler},
    partitions_weights_gen {std::make_shared<Countable_weights_generator>()} {
      assert(codes_lengths_computer != nullptr);
      assert(sampler != nullptr);
      assert(max_q != 0);
      assert(is_power_of_two(max_q));
      log2_max_partition_cardinality = log2(max_q);
    }

  Code_probabilities_result<double>
  Multialphabet_distribution_predictor::obtain_code_probabilities(const Time_series<double> &history,
                                                                  size_t horizont,
                                                                  const Group &archivers) const {
    size_t N = log2_max_partition_cardinality;
    std::vector<Codes_table> tables(N);
    std::vector<size_t> alphabets(N);
    Desample_info desample_info;
    for (size_t i = 0; i < N; ++i) {
      alphabets[i] = static_cast<size_t>(pow(2, i+1));
      auto sampled_ts = sampler->sample(history, alphabets[i]);
      if (i == N - 1) {
        desample_info = std::move(sampled_ts.desample_info);
      }
      tables[i] = codes_lengths_computer->append_each_trajectory_and_compute(sampled_ts.time_series,
                                                                             static_cast<size_t>(pow(2, i + 1)),
                                                                             horizont, archivers);
    }

    auto message_length = history.size() + horizont;
    for (size_t i = 0; i < N; ++i) {
      add_value_to_each(begin(tables[i]), end(tables[i]), (N - i - 1) * message_length);
    }
    auto global_minimal_code_length = min_value_of_all_tables(begin(tables), end(tables));
    for (auto &table : tables) {
      add_value_to_each(begin(table), end(table), -global_minimal_code_length);
      to_code_probabilities(begin(table), end(table));
    }

    auto table = merge(tables, alphabets, partitions_weights_gen->generate(N));

    return {table, sampler, desample_info};
  }

  Real_distribution_predictor::Real_distribution_predictor(Codes_lengths_computer_ptr codes_lengths_computer,
                                                           Sampler_ptr sampler, size_t partition_cardinality,
                                                           size_t difference_order)
    : Single_alphabet_distribution_predictor {codes_lengths_computer, difference_order}, sampler{sampler},
      partition_cardinality {partition_cardinality} {}

  Sampling_result_with_alphabet
  Real_distribution_predictor::sample(const Time_series<double> &history) const {
    auto sampling_result = sampler->sample(history, partition_cardinality);
    return {sampling_result, partition_cardinality};
  }

  Discrete_distribution_predictor::Discrete_distribution_predictor(Codes_lengths_computer_ptr
                                                                   codes_lengths_computer,
                                                                   Sampler_ptr sampler,
                                                                   size_t difference_order)
    : Single_alphabet_distribution_predictor {codes_lengths_computer, difference_order}, sampler{sampler} {}

  Sampling_result_with_alphabet
  Discrete_distribution_predictor::sample(const Time_series<Symbol_t> &history) const {
    auto sampling_result = sampler->normalize(history);
    size_t alphabet_size = *std::max_element(begin(sampling_result.time_series),
					     end(sampling_result.time_series)) + 1;
    return {sampling_result, alphabet_size};
  }
}
