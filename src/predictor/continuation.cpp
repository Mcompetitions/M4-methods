#include "continuation.h"

bool itp::increment(std::vector<Symbol_t> &sequence, size_t min, size_t max) {
  if (min > max) {
    throw std::invalid_argument(_S("In increment function: min should not be greater than max"
				   "(provided values are ") +
                                std::to_string(min) + _S(" and ") + std::to_string(max) + _S("."));
  }

  assert(max <= 10000);
  assert(sequence.size() > 0);

  for (size_t i = 0; i < sequence.size(); ++i) {
    if (sequence[i] < min) {
      throw std::invalid_argument(_S("In increment function: passed sequence has an element less"
				     "than minimal value: ") +
                                  std::to_string(sequence[i]) + _S(" while min is: ") +
				  std::to_string(min) + _S("."));
    }

    if (sequence[i] + 1 < max) {
      ++sequence[i];
      return true;
    }

    sequence[i] = min;
  }

  return false;
}

std::ostream& itp::operator << (std::ostream &ost, const Continuation<Symbol_t> &cont) {
  for (size_t i = 0; i < cont.size(); ++i) {
    ost << (int)cont[i];
  }

  return ost;
}

std::ostream& itp::operator << (std::ostream &ost, const Codes_table &table) {
  ost << "-\t";
  for (const auto &compressor : table.get_factors()) {
    ost << compressor << '\t';
  }
  ost << '\n';
  for (const auto &continuation : table.get_index()) {
    ost << continuation << '\t';
    for (const auto &compressor : table.get_factors()) {
      ost << table(continuation, compressor) << '\t';
    }
    ost << '\n';
  }

  return ost;
}

bool itp::operator < (const std::pair<Continuation<Symbol_t>, Double_t> &lhs,
		      const std::pair<Continuation<Symbol_t>, Double_t> &rhs) {
  return lhs.second < rhs.second;
}
    
std::pair<itp::Symbol_t, itp::Symbol_t> itp::find_confidence_interval(const Symbols_distributions &d,
								      double confidence_probability) {
  return std::make_pair(0, 0);
}

itp::Double_t itp::mean(const Symbols_distributions &d,
                        const Symbols_distributions::Factor_type &compressor,
                        Sampler_ptr sampler, const Desample_info &info) {
  Double_t sum {0};
  for (auto interval_no : d.get_index()) {
    sum += d(interval_no, compressor) * sampler->desample(interval_no, info);
  }

  return sum;
}
