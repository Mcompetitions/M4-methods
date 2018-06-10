#include "sampler.h"

#include <algorithm>
#include <cmath>
#include <functional>

itp::Desample_info::Desample_info(const std::vector<Real_time_series::value_type> &decoding_table)
        : table(decoding_table) {}

itp::Desample_info::Desample_info(std::vector<Real_time_series::value_type> &&decoding_table)
        : table(std::move(decoding_table)) {}

itp::Sampler::Sampler(double indent)
  : indent {indent} {}

void itp::Sampler::set_indent(double new_indent) {
  indent = new_indent;
}

double itp::Sampler::get_indent() const {
  return indent;
}

itp::Sampler* itp::Sampler::clone() {
  return new Sampler(indent);
}

itp::Sampling_result itp::Sampler::sample(const Real_time_series &points, size_t N) {
    Real_time_series::value_type min {*std::min_element(begin(points), end(points))};
    Real_time_series::value_type max {*std::max_element(begin(points), end(points))};
    auto width = fabs(max - min);
    min -= (width * indent);
    max += (width * indent);
    auto delta = (max - min) / N;

    Discrete_time_series result(points.size());
    for (size_t i = 0; i < points.size(); ++i) {
        result[i] = static_cast<Discrete_time_series::value_type>(floor((points[i]- min) / delta));

        // Это событие обязательно произойдёт для максимального члена
        // временного ряда.
        if (result[i] > N - 1) {
            result[i] = static_cast<Discrete_time_series::value_type>(N - 1);
        }
    }

    std::vector<Real_time_series::value_type> desample_table(N);
    for (size_t i = 0; i < N; ++i) {
        desample_table[i] = min + i * delta + delta / 2;
    }

    return Sampling_result{result, Sampler_ptr(clone()),
        Desample_info{desample_table}};
}

itp::Real_time_series::value_type
itp::Sampler::desample(Discrete_time_series::value_type s, const
                       Desample_info &info) {
  assert(!info.table.empty());
  assert(s < info.table.size());

  return info.table[s];
}

itp::Sampling_result itp::Sampler::normalize(const Discrete_time_series &points) {
  using namespace std::placeholders;

  auto min_point = *std::min_element(begin(points), end(points));
  auto max_point = *std::max_element(begin(points), end(points));
  Discrete_time_series normalized_points(points.size());
  std::transform(begin(points), end(points), begin(normalized_points),
                 std::bind(std::minus<Discrete_time_series::value_type>(), _1,
                           min_point));
  assert(*std::min_element(begin(normalized_points),
                           end(normalized_points)) == 0);
  std::vector<Real_time_series::value_type> desample_table(max_point -
                                                           min_point + 1);
  for (size_t i = 0; i < desample_table.size(); ++i) {
    desample_table[i] = i + min_point;
  }

  return Sampling_result{normalized_points, Sampler_ptr(clone()),
      Desample_info{desample_table}};
}
