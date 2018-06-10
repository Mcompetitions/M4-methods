#ifndef PREDICTOR_DFRAME_H
#define PREDICTOR_DFRAME_H

#include <vector>
#include <string>
#include <stdexcept>
#include <map>
#include <algorithm>
#include <iterator>
#include <backward/hashtable.h>

template <typename Value, bool is_const = false>
class Data_frame_iterator {
public:
  using value_type = Value;
  using difference_type = size_t;
  using pointer = typename std::conditional<is_const, const Value*, Value*>::type;
  using reference = typename std::conditional<is_const, const Value&, Value&>::type;
  using iterator_category = std::bidirectional_iterator_tag;
  using Container_ptr_type = typename std::conditional<is_const,const std::vector<std::vector<Value>> *,
                                                       std::vector<std::vector<Value>> *>::type;

  Data_frame_iterator() = default;
  explicit Data_frame_iterator(Container_ptr_type, size_t = 0, size_t = 0);

  reference operator*() const;

  Data_frame_iterator & operator++();
  Data_frame_iterator & operator++(int);

  Data_frame_iterator & operator--();
  Data_frame_iterator & operator--(int);

  bool operator==(const Data_frame_iterator &) const;
  bool operator!=(const Data_frame_iterator &) const;

private:
  Container_ptr_type data = nullptr;

  size_t curr_row_pos = 0;
  size_t curr_col_pos = 0;
};

template <typename Value, bool is_const>
Data_frame_iterator<Value, is_const>::Data_frame_iterator(Container_ptr_type data_ptr,
                                                          size_t init_row, size_t init_col)
  : data(data_ptr), curr_row_pos(init_row), curr_col_pos(init_col) {}

template <typename Value, bool is_const>
typename Data_frame_iterator<Value, is_const>::reference Data_frame_iterator<Value, is_const>::operator*() const {
  return (*data)[curr_row_pos][curr_col_pos];
}

template <typename Value, bool is_const>
Data_frame_iterator<Value, is_const> & Data_frame_iterator<Value, is_const>::operator++() {
  if (curr_col_pos < (*data)[curr_row_pos].size() - 1) {
    ++curr_col_pos;
  } else {
    ++curr_row_pos;
    curr_col_pos = 0;
  }

  return *this;
}

template <typename Value, bool is_const>
Data_frame_iterator<Value, is_const> & Data_frame_iterator<Value, is_const>::operator++(int) {
  auto tmp = *this;
  this->operator++();
  return tmp;
}

template<typename Value, bool is_const>
Data_frame_iterator<Value, is_const> & Data_frame_iterator<Value, is_const>::operator--() {
  if (curr_col_pos > 0) {
    --curr_col_pos;
  } else {
    --curr_row_pos;
    curr_col_pos = (*data)[curr_row_pos].size() - 1;
  }

  return *this;
}

template<typename Value, bool is_const>
Data_frame_iterator<Value, is_const> & Data_frame_iterator<Value, is_const>::operator--(int) {
  auto tmp = *this;
  this->operator++();
  return tmp;
}

template<typename Value, bool is_const>
bool Data_frame_iterator<Value, is_const>::operator==(const Data_frame_iterator &other) const {
  return ((curr_row_pos == other.curr_row_pos) &&
          (curr_col_pos == other.curr_col_pos) &&
          (data == other.data));
}

template<typename Value, bool is_const>
bool Data_frame_iterator<Value, is_const>::operator!=(const Data_frame_iterator &other) const {
  return !(*this == other);
}

template <typename Index, typename Factor, typename Value>
class Data_frame {
public:
  using Index_type = Index;
  using Factor_type = Factor;
  using Value_type = Value;
  using iterator = Data_frame_iterator<Value, false>;
  using const_iterator = Data_frame_iterator<Value, true>;

  Data_frame() = default;
  Data_frame(std::initializer_list<Index_type>, std::initializer_list<Factor_type> = {});
  template <typename Index_generator> Data_frame(Index_generator, size_t);
  template <typename Index_generator, typename Factors_generator>
  Data_frame(Index_generator, size_t, Factors_generator, size_t);
  template <typename Forward_iterator> Data_frame(Forward_iterator, Forward_iterator);
  template <typename Forward_iterator1, typename Forward_iterator2>
  Data_frame(Forward_iterator1, Forward_iterator1, Forward_iterator2, Forward_iterator2);

  void add_index(const Index_type &);

  template<typename Generator>
  void add_index(Generator, size_t);

  template <typename Forward_iterator>
  void add_index(Forward_iterator, Forward_iterator);

  void add_factor(const Factor_type &);

  template<typename Generator>
  void add_factor(Generator, size_t);

  template <typename Forward_iterator>
  void add_factor(Forward_iterator, Forward_iterator);

  size_t index_size() const;
  size_t factors_size() const;

  void join(const Data_frame &);

  std::vector<Index_type> get_index() const;
  std::vector<Factor_type> get_factors() const;

  void replace_index_name(const std::string &prev_name,
                          const std::string &new_name);
  void replace_factor_name(const std::string &prev_name,
                           const std::string &new_name);

  Value_type & operator()(const Index_type &, const Factor_type &);
  const Value_type & operator()(const Index_type &, const Factor_type &) const;

  iterator begin();
  const_iterator begin() const;

  iterator end();
  const_iterator end() const;

private:
  std::vector<std::vector<Value_type>> data;

  std::map<Index_type, size_t> ind_to_row;
  std::map<Factor_type, size_t> fac_to_column;

  std::vector<Index_type> indexes;
  std::vector<Factor_type> factors;

};

template <typename Index, typename Factor, typename Value>
Data_frame<Index, Factor, Value>::Data_frame(std::initializer_list<Index_type> init_index,
                                             std::initializer_list<Factor_type> init_factors) {
  add_index(std::begin(init_index), std::end(init_index));
  add_factor(std::begin(init_factors), std::end(init_factors));
}

template <typename Index, typename Factor, typename Value>
template <typename Index_generator>
Data_frame<Index, Factor, Value>::Data_frame(Index_generator i_generator, size_t i_count) {
  add_index(i_generator, i_count);
}

template <typename Index, typename Factor, typename Value>
template <typename Index_generator, typename Factors_generator>
Data_frame<Index, Factor, Value>::Data_frame(Index_generator i_generator, size_t i_count,
                                             Factors_generator f_generator, size_t f_count) {
  add_index(i_generator, i_count);
  add_factor(f_generator, f_count);
}

template <typename Index, typename Factor, typename Value>
template <typename Forward_iterator>
Data_frame<Index, Factor, Value>::Data_frame(Forward_iterator first, Forward_iterator last) {
  add_index(first, last);
}

template <typename Index, typename Factor, typename Value>
template <typename Forward_iterator1, typename Forward_iterator2>
Data_frame<Index, Factor, Value>::Data_frame(Forward_iterator1 first1, Forward_iterator1 last1,
                                             Forward_iterator2 first2, Forward_iterator2 last2) {
  add_index(first1, last1);
  add_factor(first2, last2);
}

template <typename Index, typename Factor, typename Value>
void Data_frame<Index, Factor, Value>::add_index(const Index_type &index) {
  ind_to_row[index] = indexes.size();
  indexes.push_back(index);
  data.emplace_back(factors_size(), Value());
}

template <typename Index, typename Factor, typename Value>
template<typename Generator>
void Data_frame<Index, Factor, Value>::add_index(Generator g, size_t count) {
  auto prev_size = indexes.size();
  indexes.resize(prev_size + count);
  data.resize(prev_size + count, std::vector<Value>(factors_size(), Value()));

  for (size_t i = 0; i < count; ++i) {
    indexes[prev_size + i] = g();
    ind_to_row[indexes[prev_size + i]] = prev_size + i;
  }
}

template <typename Index, typename Factor, typename Value>
template <typename Forward_iterator>
void Data_frame<Index, Factor, Value>::add_index(Forward_iterator first, Forward_iterator last) {
  for (auto iter = first; iter != last; ++iter) {
    indexes.push_back(*iter);
    data.emplace_back(factors_size(), Value());
    ind_to_row[*iter] = data.size() - 1;
  }
}

template <typename Index, typename Factor, typename Value>
void Data_frame<Index, Factor, Value>::add_factor(const Factor_type &factor) {
  fac_to_column[factor] = factors_size();
  factors.push_back(factor);
  for (auto &row : data) {
    row.push_back(Value());
  }
}

template <typename Index, typename Factor, typename Value>
template<typename Generator>
void Data_frame<Index, Factor, Value>::add_factor(Generator g, size_t count) {
  auto prev_size = factors.size();
  factors.resize(factors.size() + count);
  for (size_t i = 0; i < count; ++i) {
    factors[prev_size + i] = g();
    fac_to_column[factors[prev_size + i]] = prev_size + i;
  }

  for (auto &row : data) {
    row.resize(prev_size + count, Value());
  }
}

template <typename Index, typename Factor, typename Value>
template <typename Forward_iterator>
void Data_frame<Index, Factor, Value>::add_factor(Forward_iterator first, Forward_iterator last) {
  size_t sequence_len = 0;
  for (auto iter = first; iter != last; ++iter, ++sequence_len) {
    fac_to_column[*iter] = factors.size();
    factors.push_back(*iter);
  }
  for (auto &row : data) {
    row.resize(row.size() + sequence_len, Value());
  }
}

template <typename Index, typename Factor, typename Value>
size_t Data_frame<Index, Factor, Value>::index_size() const {
  return indexes.size();
}

template <typename Index, typename Factor, typename Value>
size_t Data_frame<Index, Factor, Value>::factors_size() const {
  return factors.size();
}

template <typename Index, typename Factor, typename Value>
void Data_frame<Index, Factor, Value>::join(const Data_frame &other) {
  for (const auto &index : other.indexes) {
    auto row_num = ind_to_row.find(index);
    if (row_num == std::end(ind_to_row)) {
      add_index(index);
    }
  }

  for (const auto &factor : other.factors) {
    auto col_num = fac_to_column.find(factor);
    if (col_num == std::end(fac_to_column)) {
      add_factor(factor);
    }
  }

  for (const auto &index : other.indexes) {
    for (const auto &factor : other.factors) {
      data[ind_to_row[index]][fac_to_column[factor]] = other(index, factor);
    }
  }
}

template <typename Index, typename Factor, typename Value>
std::vector<typename Data_frame<Index, Factor, Value>::Index_type>
Data_frame<Index, Factor, Value>::get_index() const {
  return indexes;
}

template <typename Index, typename Factor, typename Value>
std::vector<typename Data_frame<Index, Factor, Value>::Factor_type>
Data_frame<Index, Factor, Value>::get_factors() const {
  return factors;
}

template <typename Index, typename Factor, typename Value>
void Data_frame<Index, Factor, Value>::replace_index_name(const std::string &old_name,
                                                          const std::string &new_name) {
  ind_to_row[new_name] = ind_to_row[old_name];
  ind_to_row.erase(old_name);
  std::replace(std::begin(indexes), std::end(indexes), old_name, new_name);
}

template <typename Index, typename Factor, typename Value>
void Data_frame<Index, Factor, Value>::replace_factor_name(const std::string &old_name,
                         const std::string &new_name) {
  fac_to_column[new_name] = fac_to_column[old_name];
  fac_to_column.erase(old_name);
  std::replace(std::begin(factors), std::end(factors), old_name, new_name);
}

template <typename Index, typename Factor, typename Value>
typename Data_frame<Index, Factor, Value>::Value_type &
Data_frame<Index, Factor, Value>::operator()(const Index_type &index, const Factor_type &factor) {
  auto row_ind = ind_to_row.find(index);
  auto col_ind = fac_to_column.find(factor);

  if ((row_ind != std::end(ind_to_row)) && (col_ind != std::end(fac_to_column))) {
    return data[row_ind->second][col_ind->second];
  }

  if (row_ind == std::end(ind_to_row)) {
    add_index(index);
  }

  if (col_ind == std::end(fac_to_column)) {
    add_factor(factor);
  }

  return data[ind_to_row[index]][fac_to_column[factor]];
}

template <typename Index, typename Factor, typename Value>
const typename Data_frame<Index, Factor, Value>::Value_type &
Data_frame<Index, Factor, Value>::operator()(const Index_type &index,
                                             const Factor_type &factor) const {
  auto row_ind = ind_to_row.find(index);
  auto col_ind = fac_to_column.find(factor);

  if (row_ind == std::end(ind_to_row)) {
    throw std::range_error("Index out of range.");
  }

  if (col_ind == std::end(fac_to_column)) {
    throw std::range_error("Factor out of range.");
  }

  return data[row_ind->second][col_ind->second];
}

template <typename Index, typename Factor, typename Value>
typename Data_frame<Index, Factor, Value>::iterator Data_frame<Index, Factor, Value>::begin() {
  return iterator{&data, 0, 0};
}

template <typename Index, typename Factor, typename Value>
typename Data_frame<Index, Factor, Value>::const_iterator Data_frame<Index, Factor, Value>::begin() const {
  return const_iterator{&data, 0, 0};
}

template <typename Index, typename Factor, typename Value>
typename Data_frame<Index, Factor, Value>::iterator Data_frame<Index, Factor, Value>::end() {
  return iterator{&data, index_size(), 0};
}

template <typename Index, typename Factor, typename Value>
typename Data_frame<Index, Factor, Value>::const_iterator Data_frame<Index, Factor, Value>::end() const {
  return const_iterator{&data, index_size(), 0};
}

#endif //PREDICTOR_DFRAME_H
