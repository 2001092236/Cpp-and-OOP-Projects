#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <complex>

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
    static const int base = 100;
    static const int numberOfDigits = 2;

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
        for (size_t i = 0; i < n; ++i)
            a[i] /= n;

        digits.clear();
        toSize(n * 2);
        for (int i = 0; i < size(); ++i) {
            if (i < (int) a.size())
                digits[i] += std::floor(a[i].real() + 0.5);
            if (digits[i] >= base) {
                digits[i + 1] += digits[i] / base;
                digits[i] %= base;
            }
        }
        decreaseSize();
        return *this;
    }

    BigInteger& operator/=(const BigInteger& integer) {
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
        ans.sign = sign * integer.sign;
        decreaseSize();
        swap(ans);
        return *this;
    }

    BigInteger& operator%=(const BigInteger& integer) {
        BigInteger temp = (*this);
        temp /= integer;
        temp *= integer;
        (*this) -= temp;
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

BigInteger gcd(BigInteger a, BigInteger b) {
    if (a == 0)
        return b > 0 ? b : b.changeSign();
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
