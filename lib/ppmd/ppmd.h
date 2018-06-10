#ifndef PREDICTOR_PPMD_H
#define PREDICTOR_PPMD_H

#include <iostream>

namespace Ppmd {
    size_t compress_bound(size_t);
    size_t ppmd_compress(unsigned char *dst, size_t dst_size, const unsigned char *data, size_t size);
}

#endif //PREDICTOR_PPMD_H
