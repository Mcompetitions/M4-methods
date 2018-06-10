/**
 * Adapter for ttmath library (it's Big class's interface differs from standard double and such objects cannot be
 * replaced by boost::mpfr objects or standard doubles without modification of code).
 */
#ifndef PREDICTOR_BIGNUMS_H
#define PREDICTOR_BIGNUMS_H

#include <ttmath.h>

#include <exception>

class Arithmetic_carry_exception : public std::runtime_error {
public:
    explicit Arithmetic_carry_exception(const std::string &msg)
            : runtime_error(msg) {}

    template <size_t Exp1, size_t Man1, size_t Exp2, size_t Man2>
    Arithmetic_carry_exception(const std::string &msg, const ttmath::Big<Exp1, Man1> &lhs,
            const ttmath::Big<Exp2, Man2> &rhs)
            : runtime_error(std::string(msg + "\nNumbers: ") + lhs.ToString() + std::string(" and ") + rhs.ToString() +
            std::string(".")) {}
};

class Invalid_base_exception : public std::runtime_error {
public:
    explicit Invalid_base_exception(const std::string &msg)
            : runtime_error(msg) {}

    template <size_t Exp, size_t Man>
    Invalid_base_exception(const std::string &msg, const ttmath::Big<Exp, Man> &base)
            : runtime_error(std::string(msg + "\nBase: ") + base.ToString() + std::string(".")) {}
};

template <size_t Exp, size_t Man>
class Big_double {
public:
    Big_double() : base{0} {}

    template <typename T>
    Big_double(T num) : base{num} {}

    Big_double<Exp, Man> & operator += (const Big_double<Exp, Man> &other) {
        if (!base.Add(other.base)) {
            //throw Arithmetic_carry_exception("Carry flag while adding two numbers.", base, other.base);
        }

        return *this;
    }

    Big_double<Exp, Man> & operator -= (const Big_double<Exp, Man> &other) {
        if (!base.Sub(other.base)) {
            //throw Arithmetic_carry_exception("Carry flag while subtracting two numbers.", base, other.base);
        }

        return *this;
    }

    Big_double<Exp, Man> & operator *= (const Big_double<Exp, Man> &other) {
        if (!base.Mul(other.base)) {
            //throw Arithmetic_carry_exception("Carry flag while multiplying two numbers.", base, other.base);
        }

        return *this;
    }

    Big_double<Exp, Man> & operator /= (const Big_double<Exp, Man> &other) {
        if (!base.Div(other.base)) {
            //throw Arithmetic_carry_exception("Carry flag while dividing two numbers.", base, other.base);
        }

        return *this;
    }

    operator double () const {
        return base.ToDouble();
    }

    template <size_t E, size_t M> friend Big_double<E, M> pow(const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> log(const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> operator + (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> operator - (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> operator * (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> operator / (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend bool operator < (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend bool operator <= (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend bool operator > (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend bool operator >= (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend bool operator == (const Big_double<E, M> &, const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> abs(const Big_double<E, M> &);
    template <size_t E, size_t M> friend Big_double<E, M> operator - (const Big_double<E, M> &);
  template <size_t E, size_t M> friend std::string to_string (const Big_double<E, M> &);
private:
    ttmath::Big<Exp, Man> base;
};

template <size_t Exp, size_t Man>
Big_double<Exp, Man> operator + (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    Big_double<Exp, Man> result = lhs;
    result += rhs;

    return result;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> operator - (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    Big_double<Exp, Man> result = lhs;
    result -= rhs;

    return result;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> operator * (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    Big_double<Exp, Man> result = lhs;
    result *= rhs;

    return result;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> operator / (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    Big_double<Exp, Man> result = lhs;
    result /= rhs;

    return result;
}

template <size_t Exp, size_t Man>
bool operator < (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    return lhs.base < rhs.base;
}

template <size_t Exp, size_t Man>
bool operator <= (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    return lhs.base <= rhs.base;
}

template <size_t Exp, size_t Man>
bool operator > (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    return lhs.base > rhs.base;
}

template <size_t Exp, size_t Man>
bool operator >= (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    return lhs.base >= rhs.base;
}

template <size_t Exp, size_t Man>
bool operator == (const Big_double<Exp, Man> &lhs, const Big_double<Exp, Man> &rhs) {
    return lhs.base == rhs.base;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> pow(const Big_double<Exp, Man> &base, const Big_double<Exp, Man> &power) {
    auto tmp = base;

    auto err = tmp.base.PowFrac(power.base);
    if (err == 1) {
        throw Arithmetic_carry_exception("Carry flag while pow the first number to the second number.", base.base, power.base);
    } else if (err == 2) {
        throw Invalid_base_exception("Invalid base in pow.", base.base);
    }

    return tmp;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> log(const Big_double<Exp, Man> &base, const Big_double<Exp, Man> &x) {
    auto tmp = base;
    ttmath::ErrorCode err;
    auto res = ttmath::Log(base.base, x, &err);
    if (err == ttmath::err_overflow) {
        throw Arithmetic_carry_exception("Overflow while computing lograrithm of the second number with base of the first number.", base.base, x.base);
    } else if (err == 2) {
        throw Invalid_base_exception("Invalid base in pow.", base.base);
    }

    return tmp;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> abs(const Big_double<Exp, Man> &num) {
    auto tmp = num;
    tmp.base.Abs();

    return tmp;
}

template <size_t Exp, size_t Man>
Big_double<Exp, Man> operator - (const Big_double<Exp, Man> &num) {
  auto tmp = num;
  tmp.base = -tmp.base;

  return tmp;
}

template <size_t Exp, size_t Man>
std::string to_string(const Big_double<Exp, Man> &num) {
  return num.base.ToString();
}

#endif //PREDICTOR_BIGNUMS_H
