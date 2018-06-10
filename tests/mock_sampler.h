#ifndef MOCK_SAMPLER_INCLUDED
#define MOCK_SAMPLER_INCLUDED

#include "sampler.h"

#include <gmock/gmock.h>

namespace itp {
  class SamplerMock : public Sampler {
    MOCK_METHOD3(sample, Sampling_result(const Real_time_series &, size_t,
                                                  Real_time_series::value_type));
    MOCK_METHOD1(normalize, Sampling_result(const Discrete_time_series &));
    MOCK_METHOD2(desample, Real_time_series::value_type(Discrete_time_series::value_type s,
                                                        const Desample_info &info));
  };
}

#endif
