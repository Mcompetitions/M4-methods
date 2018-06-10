#include "common.inc"
#include "coro2b.inc"
#include "libpmd.inc"
#include "ppmd.h"

#include <cmath>

size_t Ppmd::compress_bound(size_t size) {
  // because often short sequences should be compressed, extra space is added
  // for safety
    return static_cast<size_t>(ceil(2*size));
}

size_t Ppmd::ppmd_compress(unsigned char *dst, size_t dst_size,
                           const unsigned char *data, size_t size) {
    unsigned char *src = new unsigned char[size];
    memcpy(src, data, size);

    ALIGN(4096) pmd_codec C;

    uint pmd_args1[] = {12, 256, 1, static_cast<uint>(size)};
    C.Init(0, pmd_args1);

    C.addout(dst, dst_size);
    C.addinp(src, size);
    C.coro_call(&C);
    size_t result = C.getoutsize();
    C.Quit();
    delete[] src;
    return result;
}
