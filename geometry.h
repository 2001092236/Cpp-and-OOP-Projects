#include <iostream>
#include <cmath>
#include <cassert>
#include <iomanip>
#include <vector>
#include <algorithm>

using ld = long double;

const ld eps = 1e-6;

const ld pi = acos(-1);

int sign(ld x) {
    if (x > 0)
        return 1;
    if (x < 0)
        return -1;
    return 0;
}

ld absl(ld x) {
    return x * sign(x);
}

bool equal(ld x, ld y) {
    return absl(x - y) < eps;
}

bool lessOrEqual(ld x, ld y) {
    return x + eps <= y;
}

ld sqr(ld x) {
    return x * x;
}

struct Point {
    ld x;
    ld y;
    Point(ld x = 0, ld y = 0): x(x), y(y) {}

    bool operator==(const Point& another) const {
        return equal(x, another.x) && equal(y, another.y);
    }

    bool operator!=(const Point& another) const {
        return !((*this) == another);
    }
};

struct Vector2D {
    ld x;
    ld y;

    Vector2D(ld x = 0, ld y = 0): x(x), y(y) {}

    Vector2D(const Point& p): Vector2D(p.x, p.y) {}

    Vector2D(const Point& p1, const Point& p2): Vector2D(p2.x - p1.x, p2.y - p1.y) {}

    explicit operator Point() const {
        return Point(x, y);
    }

    Vector2D& operator+=(const Vector2D& another) {
        x += another.x;
        y += another.y;
        return *this;
    }

    Vector2D& operator-=(const Vector2D& another) {
        x -= another.x;
        y -= another.y;
        return *this;
    }

    Vector2D& operator*=(ld a) {
        x *= a;
        y *= a;
        return *this;
    }
    Vector2D& operator/=(ld a) {
        x /= a;
        y /= a;
        return *this;
    }

    Vector2D operator-() const {
        Vector2D c = (*this);
        c.x = -c.x;
        c.y = -c.y;
        return c;
    }

    Vector2D operator*(ld a) const {
        Vector2D c = (*this);
        c *= a;
        return c;
    }


    Vector2D operator/(ld a) const {
        Vector2D c = (*this);
        c /= a;
        return c;
    }

    ld operator*(const Vector2D& another) const {
        return x * another.y - y * another.x;
    }

    ld operator%(const Vector2D& another) const {
        return x * another.x + y * another.y;
    }

    ld abs() const {
        return sqrt(sqr(x) + sqr(y));
    }

    Vector2D to_lengh(ld len) const {
        Vector2D v = (*this);
        v = v * len / v.abs();
        return v;
    }

    Vector2D rot90() const {
        Vector2D v = (*this);
        std::swap(v.x, v.y);
        v.x = -v.x;
        return v;
    }

    bool operator==(const Vector2D& another) const {
        return equal(x, another.x) && equal(y, another.y);
    }

    bool operator!=(const Vector2D& another) const {
        return !((*this) == another);
    }
};

Vector2D operator+(const Vector2D& a, const Vector2D& b) {
    Vector2D c = a;
    c += b;
    return c;
}

Vector2D operator-(const Vector2D& a, const Vector2D& b) {
    Vector2D c = a;
    c -= b;
    return c;
}

ld det2(ld a, ld b, ld c, ld d) {
    return a * d - b * c;
}

class Line {
    ld distFromPoint(const Point& p) const {
        return (p.x * A + p.y * B + C) / sqrt(sqr(A) + sqr(B));
    }
public:
    ld A;
    ld B;
    ld C;

    Line() = default;
    Line(ld A, ld B, ld C): A(A), B(B), C(C) {}
    Line(const Point& a, const Point& b) {
        A = a.y - b.y;
        B = b.x - a.x;
        C = -(A * a.x + B * a.y);
    }
    Line(ld k, ld b): Line(k, -1, b) {}
    Line(const Point& a, ld k): Line(k, a.y - k * a.x) {}

    Point reflex(const Point& p) const {
        ld d = distFromPoint(p);
        Vector2D v(A, B);
        v = v.to_lengh(d);
        Point ans(p - v * 2);
        return ans;
    }

    bool operator==(const Line& L) const {
        return equal(det2(A, B, L.A, L.B), 0) && equal(det2(B, C, L.B, L.C), 0) && equal(det2(C, A, L.C, L.A), 0);
    }

    bool operator!=(const Line& L) const {
        return !((*this) == L);
    }
};

Point rotate(Point p, Point O, ld ang) {
    Point p1(Vector2D(O, p));
    ang = ang * pi / 180;
    ld cosa = cos(ang);
    ld sina = sin(ang);
    p.x = O.x + p1.x * cosa - p1.y * sina;
    p.y = O.y + p1.x * sina + p1.y * cosa;
    return p;
}

Point reflex(Point p, Point O) {
    return Point(Vector2D(p, O) + Vector2D(O));
}

Point scale(Point p, Point O, ld coef) {
    Vector2D v(O, p);
    v *= coef;
    return Point(Vector2D(O) + v);
}

Point intersecLines(const Line& l1, const Line& l2) {
    ld x = (l1.B * l2.C - l2.B * l1.C) / (l1.A * l2.B - l2.A * l1.B);
    ld y = (l2.A * l1.C - l1.A * l2.C) / (l1.A * l2.B - l2.A * l1.B);
    return Point(x, y);
}

ld abs(Vector2D v) {
    return sqrt(sqr(v.x) + sqr(v.y));
}

ld sqrAbs(Vector2D v) {
    return sqr(v.x) + sqr(v.y);
}

ld sqrDist(Point a, Point b) {
    return sqrAbs(Vector2D(a) - Vector2D(b));
}

ld dist(Point a, Point b) {
    return sqrt(sqrDist(a, b));
}

class Shape {
public:
    virtual ld perimeter() const = 0;
    virtual ld area() const = 0;
    virtual bool operator==(const Shape& another) const = 0;
    virtual bool operator!=(const Shape& another) const {
        return !((*this) == another);
    }
    virtual bool isCongruentTo(const Shape& another) const = 0;
    virtual bool isSimilarTo(const Shape& another) const = 0;
    virtual bool containsPoint(const Point& point) const = 0;

    virtual void rotate(const Point& center, ld angle) = 0;
    virtual void reflex(const Point& center) = 0;
    virtual void reflex(const Line& axis) = 0;
    virtual void scale(const Point& center, ld coefficient) = 0;
    virtual ~Shape() {}
};

class Ellipse: public Shape {
protected:
    Point O;
    Point F;
    Point F1;
    ld a;
    ld b;
public:

    Ellipse(const Point& F, const Point& F1, ld a2): F(F), F1(F1), a(a2 / 2) {
        O = Point((Vector2D(F) + Vector2D(F1)) / 2);
        ld cc = sqrDist(O, F);
        b = sqrt(a * a - cc);
    }
    std::pair<Point, Point> focuses() const {
        return {F, F1};
    }
    std::pair<Line, Line> directrices() const {
        if (equal(a, b))
            throw std::logic_error("description");
        ld c = sqrt(sqr(a) - sqr(b));
        ld d = sqr(a) / c;
        Vector2D OF(O, F);
        Point D(OF.to_lengh(d) + Vector2D(O));
        Point D1(Vector2D(D) + OF.rot90());
        Line one(D, D1);
        OF = -OF;
        Point E(OF.to_lengh(d) + Vector2D(O));
        Point E1(Vector2D(E) + OF.rot90());
        Line two(E, E1);
        return {one, two};
    }
    ld eccentricity() const {
        return sqrt(sqr(a) - sqr(b)) / a;
    }
    Point center() const {
        return O;
    }

    ld perimeter() const override {
        return pi * (a + b) * (1 + (3 * sqr((a - b) / (a + b))) / (10 + sqrt(4 - 3 * sqr((a - b) / (a + b)))));
    }

    ld area() const override {
        return pi * a * b;
    }

    bool operator==(const Shape& another) const override {
        const Ellipse* ptr = dynamic_cast<const Ellipse*>(&another);
        if (ptr == nullptr)
            return false;
        Vector2D OF(O, F);
        Vector2D OFe(ptr->O, ptr->F);
        return (O == ptr->O) && ((F == ptr->F) || (F == ptr->F1)) && equal(a, ptr->a);
    }

    bool isCongruentTo(const Shape& another) const override {
        const Ellipse* ptr = dynamic_cast<const Ellipse*>(&another);
        if (ptr == nullptr)
            return false;
        return (ptr->O == O) && equal(a, ptr->a) && equal(b, ptr->b);
    }

    bool isSimilarTo(const Shape& another) const override {
        const Ellipse* ptr = dynamic_cast<const Ellipse*>(&another);
        if (ptr == nullptr)
            return false;
        return equal(b * ptr->a, a * ptr->b);
    }

    bool containsPoint(const Point& p) const override {
        bool ans =  lessOrEqual(dist(p, F) + dist(p, F1), a * 2);
        std::cerr << ans;
        return ans;
    }

    void rotate(const Point& center, ld ang) override {
        O = ::rotate(O, center, ang);
        F = ::rotate(F, center, ang);
        F1 = ::rotate(F1, center, ang);
    }

    void reflex(const Point& center) override {
        O = ::reflex(O, center);
        F = ::reflex(F, center);
        F1 = ::reflex(F1, center);
    }

    void reflex(const Line& axis) override {
        O = axis.reflex(O);
        F = axis.reflex(F);
        F1 = axis.reflex(F1);
    }

    void scale(const Point& center, ld coef) override {
        O = ::scale(O, center, coef);
        F = ::scale(F, center, coef);
        F1 = ::scale(F1, center, coef);
        a *= coef;
        b *= coef;
    }
};

class Circle: public Ellipse {
public:
    Circle(const Point& p, ld r): Ellipse(p, p, r * 2) {}
    ld radius() const {
        return a;
    }
};

bool containsPointAng(Point O, Point P1, Point P2, Point X) {
    Vector2D a(O, P1);
    Vector2D b(O, P2);
    Vector2D c(O, X);
    if (sign(a * c) * sign(a * b) >= 0 && sign(b * c) * sign(b * a) >= 0)
        return true;
    return false;
}

bool containsPointTriang(Point A, Point B, Point C, Point X) {
    return containsPointAng(A, C, B, X) && containsPointAng(C, A, B, X) && containsPointAng(A, B, C, X);
}

class Polygon: public Shape {
protected:
    std::vector<Point> Vertices;

    size_t getLeftBot(int alpha) const {
        size_t id = 0;
        Point p = Vertices[0];
        for (size_t i = 0; i < Vertices.size(); ++i)
            if (lessOrEqual(Vertices[i].x * alpha, p.x * alpha) || (equal(Vertices[i].x, p.x) && lessOrEqual(Vertices[i].y * alpha, p.y * alpha))) {
                p = Vertices[i];
                id = i;
            }
        return id;
    }

    void normalise() {
        size_t id = getLeftBot(1);
        for (size_t i = 0; i < id; i++)
            Vertices.push_back(Vertices[i]);
        std::reverse(Vertices.begin(), Vertices.end());
        for (size_t i = 0; i < id; ++i)
            Vertices.pop_back();
        std::reverse(Vertices.begin(), Vertices.end());
        if (getArea() < 0)
            std::reverse(++Vertices.begin(), Vertices.end());
    }

    ld getArea() const {
        ld ans = 0;
        size_t n = Vertices.size();
        for (size_t i = 2; i < n; ++i)
            ans += Vector2D(Vertices[0], Vertices[i - 1]) * Vector2D(Vertices[0], Vertices[i]);
        return ans / 2;
    }

public:

    Polygon() = default;
    Polygon(const std::vector<Point>& A): Vertices(A) {
        normalise();
    }

    Polygon(const std::initializer_list<Point>& lst) {
        size_t sz = lst.size();
        Vertices.resize(sz);
        std::copy(lst.begin(), lst.end(), Vertices.begin());
        normalise();
    }

    int verticesCount() const {
        return Vertices.size();
    }

    std::vector<Point> getVertices() const {
        return Vertices;
    }

    bool isConvex() const {
        int sig = 0;
        size_t n = Vertices.size();
        for (size_t i = 0; i < n; ++i) {
            Vector2D v1(Vertices[i], Vertices[(i + 1) % n]);
            Vector2D v2(Vertices[(i + 1) % n], Vertices[(i + 2) % n]);
            int rot = sign(v1 * v2);
            if (rot == 0)
                continue;
            if (rot * sig == -1)
                return false;
            sig = rot;
        }
        return true;
    }

    ld perimeter() const override {
        ld ans = 0;
        size_t n = verticesCount();
        for (size_t i = 0; i < n; ++i)
            ans += dist(Vertices[i], Vertices[(i + 1) % n]);
        return ans;
    }

    ld area() const override {
        return getArea();
    }

    bool operator==(const Shape& another) const override {
        const Polygon* s = dynamic_cast<const Polygon*>(&another);
        if (s == nullptr)
            return false;
        if (s->verticesCount() != verticesCount())
            return false;
        for (int i = 0; i < verticesCount(); ++i)
            if (Vertices[i] != s->Vertices[i])
                return false;
        return true;
    }

    bool isCongruentTo(const Shape& another) const override {
        const Polygon* s = dynamic_cast<const Polygon*>(&another);
        if (s == nullptr)
            return false;
        if (s->verticesCount() != verticesCount())
            return false;
        int n = verticesCount();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point O = Vertices[i];
                Point O1 = s->Vertices[j];
                Point P = Vertices[(i + 1) % n];
                Point P1 = s->Vertices[(j + 1) % n];
                Vector2D v(O, P);
                Vector2D v1(O1, P1);
                double ang = atan2(v * v1, v % v1) * 180 / pi;
                bool is = true;
                for (int k = 0; k < n; k++) {
                    P = Vertices[(i + k) % n];
                    P1 = s->Vertices[(j + k) % n];
                    if (Vector2D(O, ::rotate(P, O, ang)) != Vector2D(O1, P1)) {
                        is = false;
                        break;
                    }
                }
                if (is)
                    return true;
                P1 = s->Vertices[(j - 1 + n) % n];
                v = Vector2D(O, P);
                v1 = Vector2D(O1, P1);
                ang = atan2(v * v1, v % v1) * 180 / pi;
                is = true;
                for (int k = 0; k < n; k++) {
                    P = Vertices[(i + k) % n];
                    P1 = s->Vertices[(j - k + n) % n];
                    if (Vector2D(O, ::rotate(P, O, ang)) != Vector2D(O1, P1)) {
                        is = false;
                        break;
                    }
                }
                if (is)
                    return true;
            }
        }

        Polygon me = (*this);
        me.reflex(Line(Point(0, 0), Point(0, 1)));

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                Point O = me.Vertices[i];
                Point O1 = s->Vertices[j];
                Point P = me.Vertices[(i + 1) % n];
                Point P1 = s->Vertices[(j + 1) % n];
                Vector2D v(O, P);
                Vector2D v1(O1, P1);
                double ang = atan2(v * v1, v % v1) * 180 / pi;
                bool is = true;
                for (int k = 0; k < n; k++) {
                    P = me.Vertices[(i + k) % n];
                    P1 = s->Vertices[(j + k) % n];
                    if (Vector2D(O, ::rotate(P, O, ang)) != Vector2D(O1, P1)) {
                        is = false;
                        break;
                    }
                }
                if (is)
                    return true;
                P1 = s->Vertices[(j - 1 + n) % n];
                v = Vector2D(O, P);
                v1 = Vector2D(O1, P1);
                ang = atan2(v * v1, v % v1) * 180 / pi;
                is = true;
                for (int k = 0; k < n; k++) {
                    P = me.Vertices[(i + k) % n];
                    P1 = s->Vertices[(j - k + n) % n];
                    if (Vector2D(O, ::rotate(P, O, ang)) != Vector2D(O1, P1)) {
                        is = false;
                        break;
                    }
                }
                if (is)
                    return true;
            }
        }
        return false;
    }

    bool isSimilarTo(const Shape& another) const override {
        const Polygon* s = dynamic_cast<const Polygon*>(&another);
        if (s == nullptr)
            return false;
        if (s->verticesCount() != verticesCount())
            return false;
        ld coef = s->perimeter() / perimeter();
        Polygon t = (*this);
        t.scale(Point(0, 0), coef);
        return t.isCongruentTo(another);
    }

    bool containsPoint(const Point& p) const override {
        int n = verticesCount();
        int L = 1;
        int R = n - 1;
        if (!containsPointAng(Vertices[0], Vertices[L], Vertices[R], p))
        {
            return false;
        }
        while (R - L > 1) {
            int m = (L + R) / 2;
            if (containsPointAng(Vertices[0], Vertices[L], Vertices[m], p))
                R = m;
            else
                L = m;
        }
        bool ans = containsPointTriang(Vertices[0], Vertices[L], Vertices[R], p);
        return ans;
    }

    void rotate(const Point& center, ld ang) override {
        for (size_t i = 0; i < Vertices.size(); ++i)
            Vertices[i] = ::rotate(Vertices[i], center, ang);
        normalise();
    }

    void reflex(const Point& center) override {
        for (size_t i = 0; i < Vertices.size(); ++i)
            Vertices[i] = ::reflex(Vertices[i], center);
        normalise();
    }

    void reflex(const Line& axis) override {
        for (size_t i = 0; i < Vertices.size(); ++i)
            Vertices[i] = axis.reflex(Vertices[i]);
        normalise();
    }

    void scale(const Point& center, ld coef) override {
        for (size_t i = 0; i < Vertices.size(); ++i)
            Vertices[i] = ::scale(Vertices[i], center, coef);
        normalise();
    }
};

class Rectangle: public Polygon {
protected:
    ld getLength() const {
        return dist(Vertices[0], Vertices[1]);
    }
    ld getWidth() const {
        return dist(Vertices[1], Vertices[2]);
    }
public:
    Rectangle() = default;
    Rectangle(const Point& p1, const Point& p2, ld coef) {
        if (coef < 1)
            coef = 1 / coef;

        Vector2D v(p1, p2);
        v = v / (coef * coef + 1);
        Point H = Point(v + Point(p1));
        v *= coef;
        v = v.rot90();
        Point A(v + Vector2D(H));
        Point B(Vector2D(A, p1) + Vector2D(p2));
        Vertices.push_back(p1);
        Vertices.push_back(B);
        Vertices.push_back(p2);
        Vertices.push_back(A);
        normalise();
    }

    Point center() const {
        return Point((Vector2D(Vertices[0]) + Vector2D(Vertices[2])) / 2);
    }

    std::pair<Line, Line> diagonals() const {
        return {Line(Vertices[0], Vertices[2]), Line(Vertices[1], Vertices[3])};
    }

    ld perimeter() const override {
        return 2 * (getWidth() + getLength());
    }

    ld area() const override {
        return getWidth() * getLength();
    }
};

class Square: public Rectangle {
public:
    Square(const Point& p1, const Point& p2) : Rectangle(p1, p2, 1) {}

    Circle circumscribedCircle() const {
        return Circle(center(), getWidth() / sqrt(2));
    }

    Circle inscribedCircle() const {
        return Circle(center(), getWidth() / 2);
    }
};

class Triangle: public Polygon {
    long double geta() const {
        return dist(Vertices[1], Vertices[2]);
    }
    long double getb() const {
        return dist(Vertices[0], Vertices[2]);
    }
    long double getc() const {
        return dist(Vertices[1], Vertices[0]);
    }
public:
    Triangle(const std::vector<Point>& Vertices): Polygon(Vertices) {}

    Triangle(const Point& a, const Point& b, const Point& c) {
        Vertices.push_back(a);
        Vertices.push_back(b);
        Vertices.push_back(c);
        normalise();
    }

    Point centroid() const {
        Point A = Vertices[0];
        Point B = Vertices[1];
        Point C = Vertices[2];
        return Point((Vector2D(A) + Vector2D(B) + Vector2D(C)) / 3);
    }

    Circle inscribedCircle() const {
        Point A = Vertices[0];
        Point B = Vertices[1];
        Point C = Vertices[2];
        Point O((Vector2D(A) * geta() + Vector2D(B) * getb() + Vector2D(C) * getc()) / (geta() + getb() + getc()));
        return Circle(O, 2 * area() / perimeter());
    }

    Circle circumscribedCircle() const {
        ld S = area();
        Point A = Vertices[0];
        Point B = Vertices[1];
        Point C = Vertices[2];
        ld a = sqr(geta()) * (Vector2D(B, A) % Vector2D(C, A)) / 8 / sqr(S);
        ld b = sqr(getb()) * (Vector2D(A, B) % Vector2D(C, B)) / 8 / sqr(S);
        ld c = sqr(getc()) * (Vector2D(A, C) % Vector2D(B, C)) / 8 / sqr(S);
        return Circle(Point(Vector2D(A) * a + Vector2D(B) * b + Vector2D(C) * c), geta() * getb() * getc() / 4 / S);
    }

    Point orthocenter() const {
        Point A = Vertices[0];
        Point B = Vertices[1];
        Point C = Vertices[2];
        Point O = circumscribedCircle().center();
        return Point(Vector2D(O, A) + Vector2D(O, B) + Vector2D(O, C) + Vector2D(O));
    }

    Line EulerLine() const {
        Point O1 = orthocenter();
        Point M = centroid();
        Point D = circumscribedCircle().center();
        if (O1 != M)
            return Line(O1, M);
        return Line(O1, D);
    }

    Circle ninePointsCircle() const {
        Point O((Vector2D(orthocenter()) + Vector2D(circumscribedCircle().center())) / 2);
        return Circle(O, circumscribedCircle().radius() / 2);
    }
};
