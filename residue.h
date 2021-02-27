#include <iostream>
#include <vector>
#include <cmath>

template<unsigned N, unsigned K>
struct have_divisor {
    static const bool value = (N % K == 0) ? true : have_divisor<N, K - 1>::value;
};

template<unsigned N>
struct have_divisor<N, 1> {
    static const bool value = false;
};

template<unsigned N>
struct is_prime {
    static const bool value = !have_divisor<N, (unsigned)sqrt(N)>::value;
};

template<>
struct is_prime<1> {
    static const bool value = false;
};

template<unsigned N>
const bool is_prime_v = is_prime<N>::value;

int gcd(int a, int b) {
    if (a == 0)
        return b;
    return gcd(b % a, a);
}

template<unsigned N, unsigned R, unsigned K>
struct find_minimal_divisor {
    static const int value = (N % K == 0 ? K : find_minimal_divisor<N, R, K + 2>::value);
};

template<unsigned N, unsigned R>
struct find_minimal_divisor<N, R, R> {
    static const unsigned value = N % R == 0 ? R : N;
};

template<unsigned N>
struct minimal_divisor {
    static const unsigned value = N % 2 == 0 ? 2 : \
            find_minimal_divisor<N, (int(sqrt(N)) / 2) * 2 + 5, 3>::value;
};

template<>
struct minimal_divisor<1> {
    static const unsigned value = 1;
};

template<unsigned N>
const unsigned minimal_divisor_v = minimal_divisor<N>::value;

template<unsigned P, unsigned N>
struct is_power_of_prime {
    static const bool value = N % P != 0 ? false : is_power_of_prime<P, N / P>::value;
};

template<unsigned P>
struct is_power_of_prime <P, 0> {
    static const bool value = true;
};

template<unsigned p>
struct is_power_of_prime <p, 1> {
    static const bool value = true;
};

template<unsigned N>
struct has_primitive_root {
    static const bool value = (N % 2 == 0 ? (minimal_divisor_v<N / 2> % 2 == 0 ? false : \
                                                is_power_of_prime<minimal_divisor_v<N / 2>, N / 2>::value) : \
                                                is_power_of_prime<minimal_divisor_v<N>, N>::value);
};

template<>
struct has_primitive_root<1> {
    static const bool value = true;
};

template<>
struct has_primitive_root<2> {
    static const bool value = true;
};

template<>
struct has_primitive_root<4> {
    static const bool value = true;
};

template<unsigned N>
const bool has_primitive_root_v = has_primitive_root<N>::value;

template<unsigned N, unsigned P>
struct divide_all {
    static const unsigned value = N % P != 0 ? N : divide_all<N / P, P>::value;
};

template<unsigned P>
struct divide_all<0, P> {
    static const unsigned value = 0;
};

template<unsigned N>
struct divide_all<N, 1> {
    static const unsigned value = N;
};

template<unsigned N>
struct phi {
    static const int value = N / (divide_all<N, minimal_divisor_v<N> >::value) \
                                / minimal_divisor_v<N> * (minimal_divisor_v<N> - 1) \
                                 * (phi<divide_all<N, minimal_divisor_v<N>>::value>::value);
};

template<>
struct phi<1> {
    static const int value = 1;
};

template<unsigned N>
class Residue {
    friend std::istream& operator>>(std::istream& in, Residue<N>& r) {
        int x;
        in >> x;
        r.val = ((x % int(N)) + N) % N;
        return in;
    }
    unsigned value;
public:
    explicit Residue<N> (int x): value(((x % int(N)) + N) % N) {}
    explicit operator int() const {
        return value;
    }

    Residue<N> pow(signed k) const = delete;

    Residue<N> pow(unsigned k) const {
        if (k == 0)
            return Residue<N>(1);
        if (k % 2 == 0) {
            Residue<N> x = pow(k / 2);
            return x * x;
        }
        return (*this) * pow(k - 1);
    }

    Residue<N>& operator+=(const Residue<N>& a) {
        value += a.value;
        if (value >= N)
            value -= N;
        return *this;
    }

    Residue<N>& operator-=(const Residue<N>& a) {
        value += N - a.value;
        if (value >= N)
            value -= N;
        return *this;
    }

    Residue<N>& operator*=(const Residue<N>& a) {
        value = (1LL * value * a.value) % N;
        return *this;
    }

    int order() const {
        if (::gcd(value, N) != 1)
            return 0;

        unsigned f = phi<N>::value;
        unsigned ans = N;
        for (unsigned i = 1; i * i <= f; ++i) {
            if (f % i != 0)
                continue;
            if (pow(i).value == 1)
                return i;
            if (pow(f / i).value == 1)
                ans = std::min(ans, f / i);
        }
        return ans;
    }

    Residue<N>& operator/=(const Residue<N>& a);
    Residue<N> getInverse() const;

    static Residue<N> getPrimitiveRoot();
};

template<unsigned N, unsigned T>
struct inv {
    static Residue<N> getInverse(const Residue<N>& r) {
        return r.pow(N - 2);
    }
    static void divide(Residue<N>& r1, const Residue<N>& r2) {
        r1 *= getInverse(r2);
    }

    static Residue<N> getPrimitiveRoot() {
        if (N == 1)
            return Residue<N>(1);
        if (N == 2)
            return Residue<N>(1);
        if (N == 4)
            return Residue<N>(3);
        unsigned fi = 1;
        unsigned N2 = N;
        if (N2 % 2 == 0)
            N2 /= 2;
        if (N % 2 == 0) {
            int x = minimal_divisor_v<N / 2>;
            fi = (N / 2 / x) * (x - 1);
        }
        else {
            int x = minimal_divisor_v<N>;
            fi = (N / x) * (x - 1);
        }
        std::vector<int> primeDivisors;
        unsigned N1 = fi;
        for (unsigned i = 2; i * i <= N1; ++i) {
            int k = 0;
            while (N1 % i == 0) {
                N1 /= i;
                k++;
            }
            if (k == 0)
                continue;
            primeDivisors.push_back(i);
        }
        if (N1 != 1)
            primeDivisors.push_back(N1);
        Residue<N> t(1);
        for (unsigned x = 1; x < N; ++x) {
            t = Residue<N>(x);
            if (int(t.pow(fi)) != 1)
                continue;
            bool is = true;
            for (size_t i = 0; i < primeDivisors.size(); ++i) {
                if (int(t.pow(fi / primeDivisors[i])) == 1) {
                    is = false;
                    break;
                }
            }
            if (is)
                break;
        }
        return t;
    }
};
template<unsigned N>
struct inv<N, 0> {};

template<unsigned N>
Residue<N> Residue<N>::getPrimitiveRoot() {
    return inv<N, has_primitive_root_v<N> >::getPrimitiveRoot();
}

template<unsigned N>
Residue<N> Residue<N>::getInverse() const {
    return inv<N, is_prime_v<N> >::getInverse(*this);
}

template<unsigned N>
Residue<N>& Residue<N>::operator/=(const Residue<N>& r) {
    inv<N, is_prime_v<N> >::divide(*this, r);
    return *this;
}

template<unsigned N>
Residue<N> operator/(const Residue<N>& r1, const Residue<N>& r2) {
    Residue<N> r = r1;
    r /= r2;
    return r;
}

template<unsigned N>
Residue<N> operator+(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> c = a;
    c += b;
    return c;
}

template<unsigned N>
Residue<N> operator-(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> c = a;
    c -= b;
    return c;
}

template<unsigned N>
Residue<N> operator*(const Residue<N>& a, const Residue<N>& b) {
    Residue<N> c = a;
    c *= b;
    return c;
}
