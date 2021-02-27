#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <complex>
using std::cerr;

using ld = long double;
using cld = std::complex<ld>;
const ld pi = acos(-1);

std::string outWithPrec(int);

int flip(int x, int n) {
    int ans = 0;
    n /= 2;
    while (n > 0) {
        ans += (x % 2) * n;
        x /= 2;
        n /= 2;
    }
    return ans;
}

void fft(std::vector<cld> &a, int rev) {
    if (a.size() == 1)
        return;
    int n = a.size();
    for (int i = 0; i < n; ++i)
        if (i < flip(i, n))
            std::swap(a[flip(i, n)], a[i]);
    int l = 1;
    while (l < n) {
        l *= 2;
        for (int i = 0; i < n; i += l) {
            ld ang = rev * 2 * pi / l;
            cld w(cos(ang), sin(ang));
            cld r = 1;
            for (int j = i; j < i + l / 2; ++j) {
                cld u = a[j];
                cld v = r * a[j + l / 2];
                a[j] = u + v;
                a[j + l / 2] = u - v;
                r *= w;
            }
        }
    }
}

class BigInteger;

bool operator==(const BigInteger&, const BigInteger&);
bool operator!=(const BigInteger&, const BigInteger&);
bool operator<(const BigInteger&, const BigInteger&);
bool operator<=(const BigInteger&, const BigInteger&);
bool operator>(const BigInteger&, const BigInteger&);
bool operator>=(const BigInteger&, const BigInteger&);
BigInteger operator*(const BigInteger&, const BigInteger&);
BigInteger operator+(const BigInteger&, const BigInteger&);

class BigInteger {
private:
    friend std::ostream& operator<<(std::ostream& out, const BigInteger& integer) {
        if (integer.sign == 0) {
            out << "0";
            return out;
        }
        if (integer.sign == -1)
            out << "-";
        out << integer.digits[integer.size() - 1];
        for (int i = integer.size() - 2; i >= 0; --i)
            out << outWithPrec(integer.digits[i]);
        return out;
    }

    int sign;
    std::vector<int> digits;

    void toSize(int n) {
        while (size() < n)
            digits.push_back(0);
    }

    void decreaseSize() {
        while (size() > 0 && digits[size() - 1] == 0)
            digits.pop_back();
        if (size() == 0)
            sign = 0;
    }

    void addSame(const BigInteger& integer) {
        if (integer.sign == 0)
            return;
        if (sign == 0) {
            *this = integer;
            return;
        }
        toSize(integer.size());
        for (int i = 0; i < integer.size(); ++i)
            digits[i] += integer.digits[i];

        for (int i = 0; i < size(); ++i) {
            digits[i + 1] += digits[i] / base;
            digits[i] %= base;
        }
        if (digits.back() != 0)
            digits.push_back(0);
    }

    BigInteger& division(const BigInteger& integer, bool fl) {
        if (fl) {
            if (this == &integer) {
                *this = 1;
                return *this;
            }
            if (sign == 0 || lessAbs(integer)) {
                clear();
                return *this;
            }

            BigInteger ans = 0;
            BigInteger temp = 0;
            for (int i = size() - 1; i >= 0; --i) {
                temp.mult_base();
                temp += digits[i];
                if (temp.lessAbs(integer)) {
                    ans.mult_base();
                    continue;
                }
                BigInteger prom = integer;

                int L = 0, R = base;
                while (R - L > 1) {
                    int m = (L + R) / 2;
                    if (temp.lessAbs(prom + m * integer))
                        R = m;
                    else
                        L = m;
                }
                int dig = R + 1;
                prom += (dig - 2) * integer;
                ans.mult_base();
                ans += dig - 1;
                temp -= (prom.sign >= 0 ? prom : -prom);
            }
            ans.sign = sign * integer.sign;
            decreaseSize();
            swap(ans);
        }
        else {
            if (this == &integer) {
                *this = 0;
                return *this;
            }
            if (sign == 0) {
                clear();
                return *this;
            }
            if (lessAbs(integer))
                return *this;
            BigInteger ans = 0;
            BigInteger temp = 0;
            for (int i = size() - 1; i >= 0; --i) {
                temp.mult_base();
                temp += digits[i];
                if (temp.lessAbs(integer)) {
                    ans.mult_base();
                    continue;
                }
                BigInteger prom = integer;
                for (int dig = 1; dig <= base; ++dig) {
                    if (temp.lessAbs(prom)) {
                        ans.mult_base();
                        ans += dig - 1;
                        prom -= integer;
                        temp -= (prom.sign >= 0 ? prom : -prom);
                        break;
                    }
                    prom += integer;
                }
            }

            if (sign < 0)
                temp.changeSign();
            swap(temp);
        }
        return *this;
    }

    void subSame(const BigInteger& integer, bool less) {
        toSize(integer.size());
        for (int i = 0; i < size(); ++i) {
            if (i < integer.size())
                digits[i] -= (less ? integer.digits[i] : \
                              2 * digits[i] - integer.digits[i]);
            if (digits[i] < 0) {
                digits[i] += base;
                less ? --digits[i + 1] : ++digits[i + 1];
            }
        }
        decreaseSize();
    }

    void swap(const BigInteger& integer) {
        digits = integer.digits;
        sign = integer.sign;
    }

    bool lessAbs(const BigInteger& integer) const {
        if (sign == 0 && integer.sign == 0)
            return false;
        if (sign == 0)
            return true;
        if (integer.sign == 0)
            return false;
        if (size() < integer.size())
            return true;
        if (size() > integer.size())
            return false;
        for (int i = size() - 1; i >= 0; --i)
            if (digits[i] != integer.digits[i])
                return (digits[i] < integer.digits[i]);
        return false;
    }

    void mult_base() {
        digits.push_back(0);
        for (int i = size() - 1; i >= 0; i--)
            digits[i] = digits[i - 1];
        digits[0] = 0;
        decreaseSize();
    }

public:
    static const int base = 1000;
    static const int numberOfDigits = 3;

    BigInteger(long long x = 0): sign(x > 0? 1 : -1) {
        if (x == 0) {
            digits.push_back(0);
            sign = 0;
            return;
        }
        x *= sign;
        while (x > 0) {
            digits.push_back(x % base);
            x /= base;
        }
        digits.push_back(0);
    }

    BigInteger(const BigInteger& integer): sign(integer.sign), digits(integer.digits) {}

    BigInteger(const std::string& s) {
        sign = 1;
        int id = 0;
        if (s[0] == '-') {
            sign = -1;
            id = 1;
        }
        int lengh = s.size() - id;
        toSize((lengh + numberOfDigits - 1) / numberOfDigits);
        int pos = 0;
        for (int i = s.size() - 1; i >= id; i -= numberOfDigits) {
            for (int j = std::max(i - numberOfDigits + 1, id); j <= i; ++j) {
                digits[pos] *= 10;
                digits[pos] += s[j] - '0';
            }
            ++pos;
        }
    }

    BigInteger& operator=(const BigInteger& integer) {
        swap(integer);
        return *this;
    }

    int size() const {
        return digits.size() - 1;
    }

    BigInteger& operator+=(const BigInteger& integer) {
        if (sign * integer.sign >= 0)
            addSame(integer);
        else {
            sign *= -1;
            if ((*this < integer && sign == -1) || (*this >= integer && sign == 1))
                subSame(integer, true), sign *= -1;
            else
                subSame(integer, false);
        }
        return *this;
    }

    BigInteger& operator-=(const BigInteger& integer) {
        if (this == &integer) {
            clear();
            return *this;
        }
        sign *= -1;
        (*this) += integer;
        sign *= -1;
        return *this;
    }

    BigInteger& operator*=(const BigInteger& integer) {
        sign *= integer.sign;
        if (sign == 0) {
            clear();
            return *this;
        }

        if (size() + integer.size() < 200) {
            BigInteger ans;
            ans.sign = sign;
            ans.toSize(size() + integer.size() + 2);
            for (int i = 0; i < size(); ++i) {
                for (int j = 0; j < integer.size(); ++j) {
                    ans.digits[i + j] += digits[i] * integer.digits[j];
                    if (ans.digits[i + j] >= base) {
                        ans.digits[i + j + 1] += ans.digits[i + j] / base;
                        ans.digits[i + j] %= base;
                    }
                }
            }
            swap(ans);
            decreaseSize();
            return *this;
        }

        std::vector<cld> a;
        for (int i = 0; i < size(); ++i)
            a.push_back(digits[i]);
        std::vector<cld> b;
        for (int i = 0; i < integer.size(); ++i)
            b.push_back(integer.digits[i]);
        size_t n = 1;
        while (n <= a.size() + b.size())
            n *= 2;
        while (a.size() < n)
            a.push_back(0);
        while (b.size() < n)
            b.push_back(0);

        fft(a, 1);
        fft(b, 1);

        for (size_t i = 0; i < n; ++i)
            a[i] *= b[i];
        fft(a, -1);

        digits.clear();
        toSize(n * 2);
        for (int i = 0; i < size(); ++i) {
            if (i < (int) a.size())
                digits[i] += std::floor(a[i].real() / n + 0.5);
            if (digits[i] >= base) {
                digits[i + 1] += digits[i] / base;
                digits[i] %= base;
            }
        }
        decreaseSize();
        return *this;
    }

    BigInteger& operator/=(const BigInteger& integer) {
        division(integer, true);
        return *this;
    }

    BigInteger& operator%=(const BigInteger& integer) {
        division(integer, false);
        return *this;
    }

    BigInteger operator-() const {
        BigInteger temp = *this;
        temp.sign *= -1;
        return temp;
    }

    BigInteger& operator++() {
        (*this) += 1;
        return *this;
    }

    BigInteger& operator--() {
        (*this) -= 1;
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger temp = *this;
        ++(*this);
        return temp;
    }

    BigInteger operator--(int) {
        BigInteger temp = *this;
        --(*this);
        return temp;
    }

    int operator[](size_t id) const {
        return digits[id];
    }

    int getSign() const {
        return sign;
    }

    BigInteger& changeSign() {
        sign *= -1;
        return *this;
    }

    std::string toString() const {
        std::string ans;
        if (sign == 0) {
            ans += '0';
            return ans;
        }
        if (sign == -1)
            ans += '-';
        bool st = false;
        int pow = base / 10;
        int x = digits[size() - 1];
        for (int i = 0; i < numberOfDigits; ++i) {
            if (st || x / pow != 0) {
                st = true;
                ans += x / pow + '0';
                x %= pow;
            }
            pow /= 10;
        }
        for (int i = size() - 2; i >= 0; --i) {
            int x = digits[i];
            ans += outWithPrec(x);
        }
        return ans;
    }

    explicit operator bool() const {
        return sign != 0;
    }

    void clear() {
        digits.clear();
        digits.push_back(0);
        sign = 0;
    }
};

bool operator==(const BigInteger& integer1, const BigInteger& integer2) {
    if (integer1.getSign() != integer2.getSign() || integer1.size() != integer2.size())
        return false;
    for (int i = 0; i < integer1.size(); ++i)
        if (integer1[i] != integer2[i])
            return false;
    return true;
}

bool operator!=(const BigInteger& integer1, const BigInteger& integer2) {
    return !(integer1 == integer2);
}

bool operator<(const BigInteger& integer1, const BigInteger& integer2) {
    if (integer1.getSign() != integer2.getSign() || integer1.size() != integer2.size())
        return integer1.getSign() * integer1.size() < integer2.getSign() * integer2.size();
    for (int i = integer1.size() - 1; i >= 0; --i)
        if (integer1[i] != integer2[i])
            return ((integer1[i] < integer2[i]) == (integer1.getSign() > 0));
    return false;
}

bool operator<=(const BigInteger& integer1, const BigInteger& integer2) {
    return (integer1 < integer2) || (integer1 == integer2);
}

bool operator>(const BigInteger& integer1, const BigInteger& integer2) {
    return !(integer1 <= integer2);
}

bool operator>=(const BigInteger& integer1, const BigInteger& integer2) {
    return !(integer1 < integer2);
}

std::istream& operator>>(std::istream& in, BigInteger& integer) {
    integer.clear();
    std::string s;
    in >> s;
    integer = s;
    return in;
}

BigInteger operator"" _bi(unsigned long long x) {
    return BigInteger(x);
}

std::string outWithPrec(int x) {
    std::string ans;
    int b1 = BigInteger::base / 10;
    int dig = BigInteger::numberOfDigits;
    for (int i = dig; i > 0; --i) {
        ans += '0' + x / b1;
        x %= b1;
        b1 /= 10;
    }
    return ans;
}

BigInteger operator+(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c += b;
    return c;
}

BigInteger operator-(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c -= b;
    return c;
}

BigInteger operator*(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c *= b;
    return c;
}

BigInteger operator/(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c /= b;
    return c;
}

BigInteger operator%(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c %= b;
    return c;
}

BigInteger gcd(const BigInteger& a, const BigInteger& b) {
    if (a == 0)
        return b.getSign() >= 0 ? b : -b;
    return gcd(b % a, a);
}

BigInteger pow(BigInteger a, int n) {
    if (n == 0)
        return 1;
    if (n % 2 == 0) {
        BigInteger x = pow(a, n / 2);
        return x * x;
    }
    return a * pow(a, n - 1);
}


class Rational {
    friend std::istream& operator>>(std::istream& in, Rational& r) {
        int x;
        in >> x;
        r = x;
        return in;
    }

private:
    BigInteger numerator = 0;
    BigInteger denominator = 1;
    void normalise() {
        BigInteger common = gcd(numerator, denominator);
        numerator /= common;
        denominator /= common;
        if (denominator < 0) {
            numerator.changeSign();
            denominator.changeSign();
        }
    }
    void swap(const Rational& q) {
        numerator = q.numerator;
        denominator = q.denominator;
    }
    static const int precision = 30;

public:
    Rational(const Rational& q) {
        swap(q);
    }

    bool isZero() const {
        return (numerator.getSign() == 0);
    }

    Rational(const BigInteger& numerator1, const BigInteger& denominator1 = 1): numerator(numerator1), denominator(denominator1) {
        normalise();
    }

    Rational(long long n = 0): Rational(n, 1) {}

    Rational& operator=(const Rational& q) {
        numerator = q.numerator;
        denominator = q.denominator;
        return *this;
    }

    Rational& operator+=(const Rational& q) {
        if (this == &q) {
            numerator *= 2;
            normalise();
            return *this;
        }
        numerator = numerator * q.denominator + denominator * q.numerator;
        denominator *= q.denominator;
        normalise();
        return *this;
    }

    void changeSign() {
        numerator.changeSign();
    }

    Rational& operator-=(const Rational& q) {
        if (this == &q) {
            *this = 0;
            return *this;
        }
        numerator.changeSign();
        (*this) += q;
        numerator.changeSign();
        return *this;
    }

    Rational& operator*=(const Rational& q) {
        numerator *= q.numerator;
        denominator *= q.denominator;
        normalise();
        return *this;
    }

    Rational& operator/=(const Rational& q) {
        if (this == &q) {
            *this = 1;
            return *this;
        }
        numerator *= q.denominator;
        denominator *= q.numerator;
        normalise();
        return *this;
    }

    Rational operator-() const {
        Rational temp = *this;
        temp.numerator.changeSign();
        return temp;
    }

    std::string asDecimal(size_t precision = 0) const {
        std::string ans;
        if (numerator < 0)
            ans += '-';
        BigInteger n1 = (numerator > 0 ? numerator : -numerator);
        BigInteger temp = n1 / denominator;
        BigInteger remains = n1 % denominator;
        ans += temp.toString();
        if (precision > 0) {
            ans += '.';
            for (size_t i = 0; i < precision; ++i) {
                remains *= 10;
                BigInteger t = remains / denominator;
                ans += t.toString();
                remains %= denominator;
            }
        }
        return ans;
    }

    explicit operator double() const {
        std::stringstream buf;
        buf << asDecimal(precision);
        double ans;
        buf >> ans;
        return ans;
    }

    const BigInteger& getNumerator() const {
        return numerator;
    }

    const BigInteger& getDenominator() const {
        return denominator;
    }

    std::string toString() const {
        if (denominator == 1)
            return numerator.toString();
        return numerator.toString() + "/" + denominator.toString();
    }
};

std::ostream& operator<<(std::ostream& out, const Rational& r) {
    out << r.getNumerator() << "/" << r.getDenominator() << " ";
    return out;
}

bool operator==(const Rational& q1, const Rational& q2) {
    return (q1.getNumerator() == q2.getNumerator()) && (q1.getDenominator() == q2.getDenominator());
}

bool operator!=(const Rational& q1, const Rational& q2) {
    return !(q1 == q2);
}

bool operator<(const Rational& q1, const Rational& q2) {
    return q1.getNumerator() * q2.getDenominator() < q2.getNumerator() * q1.getDenominator();
}

bool operator<=(const Rational& q1, const Rational& q2) {
    return q1.getNumerator() * q2.getDenominator() <= q2.getNumerator() * q1.getDenominator();
}

bool operator>(const Rational& q1, const Rational& q2) {
    return !(q1 <= q2);
}

bool operator>=(const Rational& q1, const Rational& q2) {
    return !(q1 < q2);
}

Rational operator+(const Rational& q1, const Rational& q2) {
    Rational temp = q1;
    temp += q2;
    return temp;
}

Rational operator-(const Rational& q1, const Rational& q2) {
    Rational temp = q1;
    temp -= q2;
    return temp;
}

Rational operator*(const Rational& q1, const Rational& q2) {
    Rational temp = q1;
    temp *= q2;
    return temp;
}

Rational operator/(const Rational& q1, const Rational& q2) {
    Rational temp = q1;
    temp /= q2;
    return temp;
}
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

    bool isZero() const {
        return value == 0;
    }

    void changeSign() {
        if (value != 0)
            value = N - value;
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

    unsigned getVal() const {
        return value;
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

template<unsigned N>
bool operator==(const Residue<N>& a, const Residue<N>& b) {
    return a.getVal() == b.getVal();
}

template<unsigned N>
bool operator!=(const Residue<N>& a, const Residue<N>& b) {
    return a.getVal() != b.getVal();
}

template<unsigned M, unsigned N, typename Field = Rational>
class Matrix {
    std::vector<std::vector<Field> > matrix;
    static const Field zero;
public:
    Matrix<M, N, Field>(): matrix(M, std::vector<Field> (N, Field(0))) {}

    Matrix<M, N, Field>(const std::initializer_list<std::vector<int> >& lst): \
                                                                Matrix<M, N, Field>() {
        std::vector<std::vector<int> > vlst(M);
        std::copy(lst.begin(), lst.end(), vlst.begin());
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < N; ++j)
                matrix[i][j] = Field(vlst[i][j]);
    }

    Matrix<M, N, Field>& operator+=(const Matrix<M, N, Field>& m) {
        for (unsigned i = 0; i < M; ++i)
            for (unsigned j = 0; j < N; ++j)
                matrix[i][j] += m.matrix[i][j];
        return *this;
    }

    std::vector<Field>& operator[](size_t i) {
        return matrix[i];
    }

    const std::vector<Field>& operator[](size_t i) const {
        return matrix[i];
    }

    std::vector<std::vector<Field> > getMatrix() const {
        return matrix;
    }

    std::vector<Field> getRow(unsigned i) const {
        return matrix[i];
    }

    std::vector<Field> getColumn(unsigned i) const {
        std::vector<Field> ans;
        for (size_t j = 0; j < M; ++j)
            ans.push_back(matrix[j][i]);
        return ans;
    }

    Matrix<N, M, Field> transposed() const {
        Matrix<N, M, Field> ans;
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < N; ++j)
                ans[j][i] = matrix[i][j];
        return ans;
    }

    Matrix<M, N, Field>& operator-=(const Matrix<M, N, Field>& m) {
        for (unsigned i = 0; i < M; ++i)
            for (unsigned j = 0; j < N; ++j)
                matrix[i][j] -= m.matrix[i][j];
        return *this;
    }

    Matrix<M, N, Field>& operator*=(const Field& x) {
        for (unsigned i = 0; i < M; ++i)
            for (unsigned j = 0; j < N; ++j)
                matrix[i][j] *= x;
        return *this;
    }

    template<unsigned K>
    Matrix<M, K, Field>& operator*=(const Matrix<N, K, Field>& m);

    Field trace() const;

    const Field& at(size_t i, size_t j) const {
        if (i >= M || j >= N)
            return zero;
        return matrix[i][j];
    }

    template<unsigned I, unsigned J, unsigned L, unsigned L1 = L>
    Matrix<L, L1, Field> subMatrix() const {
        Matrix<L, L1, Field> ans;
        for (size_t i = I; i < I + L; ++i)
            for (size_t j = J; j < J + L1; ++j) {
                ans[i - I][j - J] = at(i, j);
            }
        return ans;
    }

    bool operator!=(const Matrix<M, N, Field>& m) const {
        return !((*this) == m);
    }

    Field det() const;

    unsigned rank() const {
        unsigned rg = N;
        std::vector<bool> used(M, false);
        std::vector<std::vector<Field> > a = matrix;
        for (size_t i = 0; i < N; ++i) {
            size_t id = 0;
            for (size_t j = 0; j < M; ++j) {
                if (!used[j] && a[j][i] != Field(0)) {
                    id = j;
                    break;
                }
            }

            if (used[id] || a[id][i] == Field(0)) {
                rg--;
                continue;
            }
            used[id] = true;

            for (size_t j = i + 1; j < N; ++j)
                a[id][j] /= a[id][i];

            for (size_t j = 0; j < M; ++j) {
                if (id == j || a[j][i] == Field(0))
                    continue;
                for (size_t k = i + 1; k < N; ++k)
                    a[j][k] -= a[id][k] * a[j][i];
            }
        }
        return rg;
    }

    void invert();

    Matrix<M, N, Field> inverted() const;
};

template<unsigned M, unsigned N, typename Field>
std::ostream& operator<<(std::ostream& out, const Matrix<M, N, Field>& m) {
    for (size_t i = 0; i < M; ++i) {
        for (size_t j = 0; j < N; ++j)
            out << m[i][j] << " ";
        out << "\n";
    }
    return out;
}

template<unsigned M, unsigned N, typename Field>
const Field Matrix<M, N, Field>::zero = Field(0);

template<unsigned M, unsigned N, unsigned K, typename Field = Rational>
Matrix<M, K, Field> mult(const Matrix<M, N, Field>& a, const Matrix<N, K, Field>& b) {
    Matrix<M, K, Field> c;
    for (unsigned i = 0; i < M; ++i)
        for (unsigned j = 0; j < N; ++j)
            for (unsigned k = 0; k < K; ++k)
                c[i][k] += a[i][j] * b[j][k];
    return c;
}

template<unsigned M, unsigned N, typename Field>
bool operator==(const Matrix<M, N, Field>& m1, const Matrix<M, N, Field>& m2) {
    for (size_t i = 0; i < M; ++i)
        for (size_t j = 0; j < N; ++j)
            if (m1[i][j] != m2[i][j])
                return false;
    return true;
}

template<unsigned M, unsigned N, typename Field>
bool operator!=(const Matrix<M, N, Field>& m1, const Matrix<M, N, Field>& m2) {
    return !(m1 == m2);
}

template<unsigned N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;

template<typename Field, unsigned M, unsigned N>
struct Square {};

template<typename Field>
struct Square<Field, 1, 1> {
    static Field trace(const Matrix<1, 1, Field>& m) {
        return m[0][0];
    }

    static Matrix<1, 1, Field> multSquare(const Matrix<1, 1, Field>& a, \
                                                const Matrix<1, 1, Field>& b) {
        Matrix<1, 1, Field> ans;
        ans[0][0] = a[0][0] * b[0][0];
        return ans;
    }

    static Field det(const Matrix<1, 1, Field>& m) {
        return m[0][0];
    }

    static void invert(const Matrix<1, 1, Field>& a) {
        a[0][0] = Field(1) / a[0][0];
    }
};

template<typename T>
void print(const std::vector<std::vector<T> >& a) {
    for (size_t i = 0; i < a.size(); ++i) {
        for (size_t j = 0; j < a[i].size(); ++j)
            std::cout << a[i][j] << " ";
        std::cout << "\n";
    }
    std::cout << "\n\n";
}

template<typename Field, unsigned M>
struct Square<Field, M, M> {
    static Field trace(const Matrix<M, M, Field>& m) {
        Field ans(0);
        for (size_t i = 0; i < M; ++i)
            ans += m[i][i];
        return ans;
    }

    static Matrix<M, M, Field> multSquare(const Matrix<M, M, Field>& a, const Matrix<M, M, Field>& b) {
        if (M < 64)
            return mult(a, b);
        std::vector<std::vector<Matrix<M / 2, M / 2, Field> > > A \
                                            (2, std::vector<Matrix<M / 2, M / 2, Field> > (2));
        std::vector<std::vector<Matrix<M / 2, M / 2, Field> > > B = A;
        std::vector<std::vector<Matrix<M / 2, M / 2, Field> > > C = A;
        std::vector<Matrix<M / 2, M / 2, Field> > P(7);

        A[0][0] = a.template subMatrix<0, 0, M / 2>();
        A[0][1] = a.template subMatrix<0, M / 2, M / 2>();
        A[1][0] = a.template subMatrix<M / 2, 0, M / 2>();
        A[1][1] = a.template subMatrix<M / 2, M / 2, M / 2>();

        B[0][0] = b.template subMatrix<0, 0, M / 2>();
        B[0][1] = b.template subMatrix<0, M / 2, M / 2>();
        B[1][0] = b.template subMatrix<M / 2, 0, M / 2>();
        B[1][1] = b.template subMatrix<M / 2, M / 2, M / 2>();

        P[0] = Square<Field, M / 2, M / 2>::multSquare(A[0][0] + A[1][1], B[0][0] + B[1][1]);
        P[1] = Square<Field, M / 2, M / 2>::multSquare(A[1][0] + A[1][1], B[0][0]);
        P[2] = Square<Field, M / 2, M / 2>::multSquare(A[0][0], B[0][1] - B[1][1]);
        P[3] = Square<Field, M / 2, M / 2>::multSquare(A[1][1], B[1][0] - B[0][0]);
        P[4] = Square<Field, M / 2, M / 2>::multSquare(A[0][0] + A[0][1], B[1][1]);
        P[5] = Square<Field, M / 2, M / 2>::multSquare(A[1][0] - A[0][0], B[0][0] + B[0][1]);
        P[6] = Square<Field, M / 2, M / 2>::multSquare(A[0][1] - A[1][1], B[1][0] + B[1][1]);

        C[0][0] = P[0] + P[3] - P[4] + P[6];
        C[0][1] = P[2] + P[4];
        C[1][0] = P[1] + P[3];
        C[1][1] = P[0] - P[1] + P[2] + P[5];

        Matrix<M, M, Field> ans;
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < M; ++j)
                ans[i][j] = C[i * 2 / M][j * 2 / M][i % (M / 2)][j % (M / 2)];
        return ans;
    }

    static Field det(const Matrix<M, M, Field>& m) {
        Field determinant(1);
        int countInvers = 0;
        std::vector<std::vector<Field> > a = m.getMatrix();
        for (size_t i = 0; i < M; ++i) {
            size_t id = i;
            for (size_t j = i; j < M; ++j) {
                if (!a[j][i].isZero()) {
                    id = j;
                    break;
                }
            }
            if (a[id][i].isZero())
                return Field(0);

            std::swap(a[i], a[id]);
            countInvers += (i != id);
            determinant *= a[i][i];

            for (size_t j = i + 1; j < M; ++j)
                a[i][j] /= a[i][i];
            a[i][i] = Field(1);

            for (size_t j = i + 1; j < M; ++j) {
                if (i == j || a[j][i].isZero())
                    continue;
                for (size_t k = i + 1; k < M; ++k)
                    a[j][k] -= a[i][k] * a[j][i] / a[i][i];
                a[j][i] = Field(0);
            }
        }
        if (countInvers & 1)
            determinant.changeSign();
        return determinant;
    }

    static void invert(Matrix<M, M, Field>& m) {
        Matrix<M, M * 2, Field> temp;
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < M; ++j)
                temp[i][j] = m[i][j];
        for (size_t j = M; j < M * 2; ++j)
                temp[j - M][j] = Field(1);
        for (size_t i = 0; i < M; ++i) {
            size_t id = i;
            for (size_t j = i; j < M; ++j)
                if (!temp[j][i].isZero()) {
                    id = j;
                    break;
                }

            if (temp[id][i].isZero())
                throw("Matrix with det=0!!!");
            std::swap(temp[i], temp[id]);
            for (size_t j = i + 1; j < M * 2; ++j)
                temp[i][j] /= temp[i][i];
            temp[i][i] = Field(1);
            for (size_t j = 0; j < M; ++j) {
                if (i == j || temp[j][i].isZero())
                    continue;
                for (size_t k = i + 1; k < M * 2; ++k)
                    temp[j][k] -= temp[j][i] * temp[i][k];
                temp[j][i] = Field(0);
            }
        }
        m = temp.template subMatrix<0, M, M>();
    }
};

template<unsigned M, unsigned N, typename Field>
void Matrix<M, N, Field>::invert() {
    Square<Field, M, N>::invert(*this);
}

template<unsigned M, unsigned N, typename Field>
Matrix<M, N, Field> Matrix<M, N, Field>::inverted() const {
    Matrix<M, N, Field> ans = (*this);
    ans.invert();
    return ans;
}

template<unsigned M, unsigned N, typename Field>
Field Matrix<M, N, Field>::det() const {
    return Square<Field, M, N>::det(*this);
}

template<unsigned M, unsigned N, typename Field>
Field Matrix<M, N, Field>::trace() const {
    return Square<Field, M, N>::trace(*this);
}

template<unsigned M, unsigned N, typename Field = Rational>
Matrix<M, N, Field> operator+(const Matrix<M, N, Field>& a, const Matrix<M, N, Field>& b) {
    Matrix<M, N, Field> c = a;
    c += b;
    return c;
}

template<unsigned M, unsigned N, typename Field = Rational>
Matrix<M, N, Field> operator-(const Matrix<M, N, Field>& a, const Matrix<M, N, Field>& b) {
    Matrix<M, N, Field> c = a;
    c -= b;
    return c;
}

template<unsigned M, unsigned N, typename Field = Rational>
Matrix<M, N, Field> operator*(const Matrix<M, N, Field>& a, const Field& x) {
    Matrix<M, N, Field> c = a;
    c *= x;
    return c;
}

template<unsigned M, unsigned N, typename Field = Rational>
Matrix<M, N, Field> operator*(const Field& x, const Matrix<M, N, Field>& a) {
    Matrix<M, N, Field> c = a;
    c *= x;
    return c;
}

template<unsigned M, unsigned N, typename Field>
template<unsigned K>
Matrix<M, K, Field>& Matrix<M, N, Field>::operator*=(const Matrix<N, K, Field>& m) {
    constexpr unsigned newSz = (1 << ((int)log2(std::max(M, std::max(N, K))) + 1));
    Matrix<newSz, newSz, Field> a = this->template subMatrix<0, 0, newSz>();
    Matrix<newSz, newSz, Field> b = m.template subMatrix<0, 0, newSz>();
    (*this) = (Square<Field, newSz, newSz>::multSquare(a, b)).template subMatrix<0, 0, M, K>();
    return *this;
}

template<unsigned M, unsigned N, unsigned K, typename Field = Rational>
Matrix<M, K, Field> operator*(const Matrix<M, N, Field>& m1, const Matrix<N, K, Field>& m2) {
    constexpr unsigned newSz = (1 << ((int)log2(std::max(M, std::max(N, K))) + 1));
    Matrix<newSz, newSz, Field> a = m1.template subMatrix<0, 0, newSz>();
    Matrix<newSz, newSz, Field> b = m2.template subMatrix<0, 0, newSz>();
    Matrix<M, K, Field> c = (Square<Field, newSz, newSz>::multSquare(a, b)).template subMatrix<0, 0, M, K>();
    return c;
}
