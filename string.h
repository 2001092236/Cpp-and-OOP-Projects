#include <iostream>
#include <cstring>

class String {
private:
    size_t realSize = 0;
    size_t bufferSize = 0;
    char* str = nullptr;
    static const size_t MinimalBufferSize;
    static const size_t ExpansionCoefficient = 2;

    bool isSame(const String& substring, size_t i) const {
        bool allow = false;
        for (size_t j = i; j <= i + substring.realSize - 1; ++j)
            if ((allow = (str[j] != substring[j - i])))
                break;
        return !allow;
    }

public:
    String() = default;

    String(size_t sz): realSize(sz) {
        bufferSize = std::max(sz, MinimalBufferSize);
        str = new char[bufferSize];
    }

    String(size_t sz, char c): String(sz) {
        memset(str, c, bufferSize);
    }

    String(const String& s): String(s.realSize) {
        memcpy(str, s.str, realSize);
    }

    String(const char* s): String(strlen(s)) {
        memcpy(str, s, realSize);
    }

    String(char c): String(1, c) {}

    void swap(String& s) {
        std::swap(str, s.str);
        std::swap(realSize, s.realSize);
        std::swap(bufferSize, s.bufferSize);
    }

    String& operator=(const String& s) {
        if (this == &s)
            return *this;
        String temp = s;
        swap(temp);
        return *this;
    }

    void push_back(char c) {
        if (realSize < bufferSize) {
            str[realSize++] = c;
            return;
        }
        String temp(bufferSize * ExpansionCoefficient);
        swap(temp);
        realSize = temp.realSize;
        memcpy(str, temp.str, realSize);
        str[realSize++] = c;
    }

    void pop_back() {
        if ((--realSize) * ExpansionCoefficient * ExpansionCoefficient < bufferSize) {
            String temp(bufferSize /= ExpansionCoefficient);
            swap(temp);
            memcpy(str, temp.str, realSize = temp.realSize);
        }
    }

    String& operator+=(const String& s) {
        size_t length = s.realSize;
        if (realSize * ExpansionCoefficient * ExpansionCoefficient < s.realSize) {
            String temp(realSize + s.realSize);
            memcpy(temp.str, str, realSize);
            memcpy(temp.str + realSize, s.str, s.realSize);
            swap(temp);
            return *this;
        }
        for (size_t i = 0; i < length; ++i)
            push_back(s.str[i]);
        return *this;
    }

    size_t find(const String& substring) const {
        for (size_t i = 0; i + substring.realSize < realSize + 1; ++i)
            if (isSame(substring, i))
                return i;
        return realSize;
    }

    size_t rfind(const String& substring) const {
        if (realSize < substring.realSize)
            return realSize;
        for (int i = realSize - substring.realSize; i >= 0; --i)
            if (isSame(substring, i))
                return i;
        return realSize;
    }

    String substr(size_t start, size_t count) const {
        String temp(count);
        memcpy(temp.str, str + start, count);
        return temp;
    }

    bool empty() const {
        return realSize == 0;
    }

    char& front() {
        return str[0];
    }

    const char& front() const {
        return str[0];
    }

    char& back() {
        return str[realSize - 1];
    }

    const char& back() const {
        return str[realSize - 1];
    }

    size_t length() const {
        return realSize;
    }

    char& operator[](size_t index) {
        return str[index];
    }

    const char& operator[](size_t index) const {
        return str[index];
    }

    void clear() {
        realSize = bufferSize = 0;
        if (str != nullptr)
            delete[] str;
        str = nullptr;
    }

    ~String() {
        if (str != nullptr)
            delete[] str;
    }
};

const size_t String::MinimalBufferSize = 4;

std::ostream &operator<<(std::ostream& out, const String& s) {
    for (size_t i = 0; i < s.length(); ++i)
        out << s[i];
    return out;
}

std::istream& operator>>(std::istream& in, String& s) {
    s.clear();
    char c;
    in >> c;
    while (!isspace(c) && c != EOF) {
        s.push_back(c);
        c = in.get();
    }
    return in;
}

bool operator==(const String& s1, const String& s2) {
    if (s1.length() != s2.length())
        return false;
    for (size_t i = 0; i < s1.length(); ++i)
        if (s1[i] != s2[i])
            return false;
    return true;
}

String operator+(const String& s1, const String& s2) {
    String temp = s1;
    temp += s2;
    return temp;
}
