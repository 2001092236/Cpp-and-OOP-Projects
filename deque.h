#include <iostream>
#include <vector>
#include <deque>
#include <random>
#include <time.h>
#include <cassert>

using std::cout;

template<typename T>
class Deque {
    static const int batchSize = 25;

    struct Batch {
        int sz;
        int number;
        int pointerFront;
        Batch* next = nullptr;
        Batch* prev = nullptr;
        std::vector<T> elements;

        Batch(int number, const T& t = T()): sz(batchSize), number(number), \
                                                pointerFront(0), next(nullptr), prev(nullptr) {
            elements.resize(batchSize);
            for (size_t i = 0; i < elements.size(); ++i)
                elements[i] = t;
        }

        static Batch* getBatch(int number, const T& t = T()) {
            Batch* b = new Batch(number, t);
            return b;
        }

        Batch* copy() const {
            Batch* b = getBatch(0);
            b->pointerFront = pointerFront;
            b->elements = elements;
            b->sz = sz;
            b->number = number;
            return b;
        }
    };

    std::vector<Batch*> batches;
    int minimalBatchNumber = 0;
    int maximalBatchNumber = -1;

    void swap(Deque& d) {
        std::swap(minimalBatchNumber, d.minimalBatchNumber);
        std::swap(maximalBatchNumber, d.maximalBatchNumber);
        std::swap(batches, d.batches);
    }

    int cntBatch() const {
        return maximalBatchNumber - minimalBatchNumber + 1;
    }

    void realloc() {
        int cnt = cntBatch();
        batches.resize(cnt * 3);
        for (int i = cnt * 2 - 1; i >= cnt; --i) {
            batches[i] = batches[i - cnt + minimalBatchNumber];
            batches[i]->number = i;
        }
        for (int i = 0; i < cnt; ++i) {
            batches[i] = Batch::getBatch(i);
            batches[i]->sz = 0;
        }

        for (int i = cnt * 2; i < cnt * 3; ++i) {
            batches[i] = Batch::getBatch(i);
            batches[i]->sz = 0;
        }

        for (int i = 1; i < cnt * 3; ++i) {
            batches[i - 1]->next = batches[i];
            batches[i]->prev = batches[i - 1];
        }
        minimalBatchNumber = cnt;
        maximalBatchNumber = cnt * 2 - 1;
    }

    void setNextAndPrev(int i) {
        if (i == 0)
            return;
        batches[i]->prev = batches[i - 1];
        batches[i - 1]->next = batches[i];
    }

public:
    Deque() = default;

    Deque(int cnt, const T& t = T()) {
        if (cnt < 0)
            throw(std::out_of_range("Negative Deque size!!!"));
        int cntBatch = (cnt + batchSize - 1) / batchSize;
        minimalBatchNumber = cntBatch;
        maximalBatchNumber = cntBatch * 2 - 1;
        batches.resize(cntBatch * 3);
        for (int i = 0; i < cntBatch * 3; ++i) {
            batches[i] = Batch::getBatch(i, t);
            if (i < cntBatch || i >= cntBatch * 2)
                batches[i]->sz = 0;
            setNextAndPrev(i);
        }
        batches[cntBatch * 2 - 1]->sz = (cnt % batchSize ? cnt % batchSize : batchSize);
    }

    Deque(const Deque& d): minimalBatchNumber(d.minimalBatchNumber), \
                                maximalBatchNumber(d.maximalBatchNumber) {
        batches.resize(d.batches.size());
        for (size_t i = 0; i < batches.size(); ++i) {
            batches[i] = d.batches[i]->copy();
            setNextAndPrev(i);
        }
    }

    Deque& operator=(const Deque& d) {
        Deque temp = d;
        swap(temp);
        return *this;
    }

    size_t size() const {
        if (cntBatch() == 0)
            return 0;
        if (cntBatch() == 1)
            return batches[minimalBatchNumber]->sz;
        return (cntBatch() - 2) * batchSize + batches[minimalBatchNumber]->sz \
                                                + batches[maximalBatchNumber]->sz;
    }

    T& operator[](int i) {
        if (i < batches[minimalBatchNumber]->sz)
            return batches[minimalBatchNumber]->elements \
                    [i + batches[minimalBatchNumber]->pointerFront];
        i -= batches[minimalBatchNumber]->sz;
        int numBatch = i / batchSize + minimalBatchNumber + 1;
        return batches[numBatch]->elements[i % batchSize + batches[numBatch]->pointerFront];
    }

    const T& operator[](int i) const {
        return const_cast<Deque&>(*this)[i];
    }

    T& at(int i) {
        if (i < 0 || i >= int(size()))
            throw(std::out_of_range("Segmentation fault at!!!"));
        return (*this)[i];
    }

    const T& at(int i) const {
        return const_cast<Deque&>(*this).at(i);
    }

    void push_back(const T& t) {
        if (cntBatch() == 0) {
            *this = Deque(1, t);
            return;
        }
        Batch* bptr = batches[maximalBatchNumber];
        if (bptr->pointerFront + bptr->sz < batchSize) {
            bptr->elements[bptr->pointerFront + bptr->sz] = t;
            ++bptr->sz;
            return;
        }
        if (maximalBatchNumber + 1 < int(batches.size())) {
            ++maximalBatchNumber;
            bptr = batches[maximalBatchNumber];
            bptr->elements[0] = t;
            ++bptr->sz;
            return;
        }
        realloc();
        push_back(t);
    }

    void push_front(const T& t) {
        if (cntBatch() == 0) {
            Deque d(1, t);
            (*this) = d;
            return;
        }
        Batch* bptr = batches[minimalBatchNumber];
        if (bptr->pointerFront > 0) {
            --(bptr->pointerFront);
            bptr->elements[bptr->pointerFront] = t;
            ++bptr->sz;
            return;
        }
        if (minimalBatchNumber > 0) {
            --minimalBatchNumber;
            bptr = batches[minimalBatchNumber];
            bptr->elements[batchSize - 1] = t;
            ++bptr->sz;
            bptr->pointerFront = batchSize - 1;
            return;
        }
        realloc();
        push_front(t);
    }

    void pop_back() {
        if (size() == 0)
            throw(std::out_of_range("Segmentation fault pop_back!!!"));
        --batches[maximalBatchNumber]->sz;
        if (batches[maximalBatchNumber]->sz == 0) {
            batches[maximalBatchNumber]->pointerFront = 0;
            --maximalBatchNumber;
        }
    }

    void pop_front() {
        if (size() == 0)
            throw(std::out_of_range("Segmentation fault pop_front!!!"));
        --batches[minimalBatchNumber]->sz;
        ++batches[minimalBatchNumber]->pointerFront;
        if (batches[minimalBatchNumber]->sz == 0) {
            batches[minimalBatchNumber]->pointerFront = 0;
            ++minimalBatchNumber;
        }
    }

    class const_iterator
    {
        int getNumberInDeque() const {
            return (batchPtr->number - parent->minimalBatchNumber) * parent->batchSize \
                        + elementPtr - (&(batchPtr->elements[0]));
        }

    protected:
        T* elementPtr = nullptr;
        Batch* batchPtr = nullptr;
        const Deque* parent = nullptr;

    public:
        const_iterator() = default;
        const_iterator(Batch* batchPtr, T* elementPtr, const Deque* parent): \
                        elementPtr(elementPtr), batchPtr(batchPtr), parent(parent) {}

        const_iterator(const const_iterator& iter): \
                    elementPtr(iter.elementPtr), batchPtr(iter.batchPtr), parent(iter.parent) {}

        const_iterator& operator++() {
            if (!batchPtr->next || elementPtr - &(batchPtr->elements[0]) < batchSize - 1)
                ++elementPtr;
            else {
                batchPtr = batchPtr->next;
                elementPtr = (&(batchPtr->elements[0]));
            }
            return *this;
        }

        const_iterator operator++(int) {
            const_iterator temp = (*this);
            ++(*this);
            return temp;
        }

        const_iterator& operator--() {
            if (elementPtr - (&(batchPtr->elements[0])) > 0)
                --elementPtr;
            else {
                batchPtr = batchPtr->prev;
                elementPtr = (&(batchPtr->elements[batchSize - 1]));
            }
            return *this;
        }

        const_iterator operator--(int) {
            const_iterator temp = (*this);
            --(*this);
            return temp;
        }

        bool operator==(const const_iterator& iter) const {
            return (elementPtr == iter.elementPtr) && (batchPtr == iter.batchPtr);
        }

        bool operator!=(const const_iterator& iter) const {
            return !(*this == iter);
        }

        bool operator<(const const_iterator& iter) const {
            if (batchPtr->number < iter.batchPtr->number)
                return true;
            if (batchPtr->number > iter.batchPtr->number)
                return false;
            return elementPtr < iter.elementPtr;
        }

        bool operator>(const const_iterator& iter) const {
            return iter < (*this);
        }

        bool operator>=(const const_iterator& iter) const {
            return !(*this < iter);
        }

        bool operator<=(const const_iterator& iter) const {
            return !(iter < *this);
        }

        int operator-(const const_iterator& it) {
            return getNumberInDeque() - it.getNumberInDeque();
        }

        const_iterator& operator+=(int x) {
            int elementId = elementPtr - &batchPtr->elements[0];
            if (x + elementId < parent->batchSize && x + elementId >= 0)
                elementPtr += x;
            else {
                    int cnt = parent->batchSize - elementId;
                    int sign = (x > 0 ? 1 : -1);
                    if (sign < 0) {
                        x *= -1;
                        cnt = parent->batchSize + 1 - cnt;
                    }
                    x -= cnt;
                    int numBatch = batchPtr->number + (x / parent->batchSize + 1) * sign;
                    int numInBatch = x % parent->batchSize;
                    if (sign < 0)
                        numInBatch = parent->batchSize - 1 - numInBatch;
                    batchPtr = parent->batches[numBatch];
                    elementPtr = &batchPtr->elements[numInBatch];
            }
            return *this;
        }

        const_iterator& operator-=(int x) {
            (*this) += (-x);
            return *this;
        }

        const_iterator operator+(int x) const {
            const_iterator temp = *this;
            temp += x;
            return temp;
        }

        friend const_iterator operator+(int x, const const_iterator& iter) {
            return iter + x;
        }

        const_iterator operator-(int x) const {
            const_iterator temp = *this;
            temp += (-x);
            return temp;
        }

        const T& operator*() const {
            return *elementPtr;
        }

        const T* operator->() const {
            return elementPtr;
        }
    };

    class iterator: public const_iterator {
    public:
        using const_iterator::const_iterator;
        using const_iterator::batchPtr;
        using const_iterator::elementPtr;
        using const_iterator::parent;
        using const_iterator::operator-;

        iterator& operator++() {
            ++static_cast<const_iterator&>(*this);
            return *this;
        }

        iterator operator++(int) {
            iterator temp = (*this);
            ++(*this);
            return temp;
        }

        iterator& operator--() {
            --static_cast<const_iterator&>(*this);
            return *this;
        }

        iterator operator--(int) {
            iterator temp = (*this);
            --(*this);
            return temp;
        }

        iterator& operator+=(int x) {
            static_cast<const_iterator&>(*this) += x;
            return *this;
        }

        iterator& operator-=(int x) {
            (*this) += (-x);
            return *this;
        }

        iterator operator+(int x) const {
            iterator temp = *this;
            temp += x;
            return temp;
        }

        friend iterator operator+(int x, const iterator& iter) {
            return iter + x;
        }

        iterator operator-(int x) const {
            iterator temp = *this;
            temp += (-x);
            return temp;
        }

        T& operator*() {
            return *elementPtr;
        }

        T* operator->() {
            return elementPtr;
        }
    };

    void insert(const_iterator it, const T& t) {
        push_back(T());
        iterator it1 = end();
        while (--it1 != it)
            *it1 = *(it1 - 1);
        *it1 = t;
    }

    void erase(const_iterator it) {
        iterator it1 = static_cast<iterator&>(it);
        while (++it1 != end())
            *(it1 - 1) = *it1;
        pop_back();
    }

    const_iterator cbegin() const {
        Batch* bptr = batches[minimalBatchNumber];
        const_iterator it(bptr, &(bptr->elements[bptr->pointerFront]), this);
        return it;
    }

    const_iterator begin() const {
        const_iterator it = cbegin();
        return it;
    }

    iterator begin() {
        const_iterator cb = cbegin();
        iterator it = static_cast<iterator&>(cb);
        return it;
    }

    const_iterator cend() const {
        Batch* bptr = batches[maximalBatchNumber];
        const_iterator it(bptr, &(bptr->elements[bptr->pointerFront + bptr->sz - 1]) + 1, this);
        return it;
    }

    const_iterator end() const {
        const_iterator it = cend();
        return it;
    }

    iterator end() {
        const_iterator ce = cend();
        iterator it = static_cast<iterator&>(ce);
        return it;
    }

    ~Deque() {
        for (size_t i = 0; i < batches.size(); ++i)
            delete batches[i];
    }
};
