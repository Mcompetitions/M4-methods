//
// Created by user10101 on 21.03.18.
//
#include "compressors.h"

#include <cassert>
#include <iostream>

itp::Zstd_compressor::Zstd_compressor() {
  context = ZSTD_createCCtx();
}

itp::Zstd_compressor::~Zstd_compressor() {
  ZSTD_freeCCtx(context);
}

size_t itp::Zstd_compressor::operator ()(const unsigned char *data, size_t size,
					 unsigned char **tmp_buffer,
					 size_t *buffer_size) {
  assert(buffer_size != nullptr);

  size_t dst_capacity = ZSTD_compressBound(size);
  fit_buffer(dst_capacity, tmp_buffer, buffer_size);
  return ZSTD_compressCCtx(context, *tmp_buffer, *buffer_size, data, size, ZSTD_maxCLevel());
}

size_t itp::Zlib_compressor::operator ()(const unsigned char *data, size_t size,
					 unsigned char **tmp_buffer, size_t *buffer_size) {
  size_t dst_capacity = compressBound(size * sizeof(Symbol_t));
  fit_buffer(dst_capacity, tmp_buffer, buffer_size);
  if (compress2(*tmp_buffer, &dst_capacity,
		data, size, Z_BEST_COMPRESSION) != Z_OK) {
    throw std::runtime_error("zlib: an error occured.");
  }
  return dst_capacity;
}

size_t itp::Ppm_compressor::operator ()(const unsigned char *data, size_t size,
					unsigned char **tmp_buffer, size_t *buffer_size) {
  size_t dst_capacity = Ppmd::compress_bound(size);
  fit_buffer(dst_capacity, tmp_buffer, buffer_size);
  auto result = Ppmd::ppmd_compress(*tmp_buffer, *buffer_size, data, size);
  return result;
}

size_t itp::Rp_compressor::operator ()(const unsigned char *data, size_t size, unsigned char **,
				       size_t *) {
  return Rp::rp_compress(data, size);
}

size_t itp::Bzip2_compressor::operator ()(const unsigned char *data, size_t size,
					  unsigned char **tmp_buffer, size_t *buffer_size) {
  // according to documentation, such capacity guaranties that the compressed data will fit in
  // the buffer
  std::unique_ptr<char[]> src(new char[size]);
  memcpy(src.get(), data, size * sizeof(Symbol_t));
  uint dst_capacity = static_cast<uint>(size * sizeof(Symbol_t) + ceil(size * sizeof(Symbol_t) * 0.01) + 600);
  fit_buffer(dst_capacity, tmp_buffer, buffer_size);
  if (BZ2_bzBuffToBuffCompress(reinterpret_cast<char *>(*tmp_buffer), &dst_capacity,
			       src.get(), static_cast<uint>(size), 9, 0, 30) != BZ_OK) {
    throw std::runtime_error("Bzip2: an error occured.");
  }

  return dst_capacity;
}

size_t itp::Lca_compressor::operator ()(const unsigned char *data, size_t size, unsigned char **,
					size_t *) {
  return Lcacomp::lcacomp_compress(data, size);
}

itp::Compressors_pool::~Compressors_pool() {
  delete[] buffer;
  for (auto &pair : compressor_instances) {
    delete pair.second;
  }
}

itp::Compressors_pool::Compressors_pool() {
  compressor_instances.emplace("lcacomp", new Lca_compressor);
  compressor_instances.emplace("rp", new Rp_compressor);
  compressor_instances.emplace("zstd", new Zstd_compressor);
  compressor_instances.emplace("bzip2", new Bzip2_compressor);
  compressor_instances.emplace("zlib", new Zlib_compressor);
  compressor_instances.emplace("ppmd", new Ppm_compressor);
}

size_t itp::Compressors_pool::operator()(const std::string &compressor_name,
					 const unsigned char *data, size_t size) {
  try {
    return compressor_instances.at(compressor_name)->operator()(data, size, &buffer, &buffer_size);
  }
  
  catch (const std::out_of_range &e) {
    throw std::invalid_argument(std::string("Incorrect compressor name ") + compressor_name);
  }
}
