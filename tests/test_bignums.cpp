#include <bignums.h>

#include <gtest/gtest.h>

TEST(BigNumsTest, Constructors) {
    Big_double<2, 4> d{5};
    EXPECT_DOUBLE_EQ(d, 5.);

    Big_double<3, 6> dd{5};
    EXPECT_DOUBLE_EQ(d, dd);

    double sd = d;
    EXPECT_DOUBLE_EQ(sd, 5.0);
}

TEST(BigNumsTest, Arithmetic) {
    //EXPECT_THROW(pow(Big_double<2,4>{10e100}, Big_double<2,4>{10e100}), Arithmetic_carry_exception);
    /*EXPECT_THROW((Big_double<2,4>{1e-1000000000000} / Big_double<2,4>{10000000000000e1000000000000000000000000000000000000000}),
            Arithmetic_carry_exception);*/
    //std::cout << (Big_double<2,4>{1e-1000000000000} / Big_double<2,4>{10000000000000e1000000000000000000000000000000000000000}).str() << std::endl;
}

TEST(BigNumsTest, Utilities) {
    Big_double<2, 4> b{5}, p{2};
    EXPECT_DOUBLE_EQ(pow(b, p), 25.);
    EXPECT_DOUBLE_EQ(pow(b, -p), 1./25.);
    EXPECT_EQ(abs(-p), p);
}