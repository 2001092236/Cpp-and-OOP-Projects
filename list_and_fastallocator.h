#include <iostream>
#include <memory>
#include <vector>
#include <list>
#include <ctime>
#include <random>
#include <type_traits>


template<size_t chunkSize>
class FixedAllocator {
    std::vector<void*> free_ptr;
    FixedAllocator() = default;
    static const int nObj = 30;
public:
    static FixedAllocator<chunkSize>& get_alloc() {
        static FixedAllocator<chunkSize> alloc;
        return alloc;
    }

    void* allocate() {
        if (free_ptr.empty()) {
            char* ptr = static_cast<char*>(::operator new(chunkSize * nObj));
            for (int i = 0; i < nObj; ++i) {
                char* p = ptr + i * chunkSize;
                free_ptr.push_back(static_cast<void*>(p));
            }
        }
        void* ptr = free_ptr.back();
        free_ptr.pop_back();
        return ptr;
    }

    void deallocate(void* ptr) {
        free_ptr.push_back(ptr);
    }
};


template<typename T>
class FastAllocator {
public:
    FastAllocator() = default;

    using value_type = T;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using size_type = std::size_t;

    template<typename U>
    FastAllocator(const FastAllocator<U>&) {}

    T* allocate(size_t count) {
        size_t cnt = count * sizeof(T);
        if (cnt <= 4)
            return static_cast<T*>(FixedAllocator<4>::get_alloc().allocate());
        if (cnt <= 8)
            return static_cast<T*>(FixedAllocator<8>::get_alloc().allocate());
        if (cnt <= 16)
            return static_cast<T*>(FixedAllocator<16>::get_alloc().allocate());
        if (cnt <= 24)
            return static_cast<T*>(FixedAllocator<24>::get_alloc().allocate());
        if (cnt <= 32)
            return static_cast<T*>(FixedAllocator<32>::get_alloc().allocate());
        return static_cast<T*>(::operator new(cnt));
    }

    void deallocate(T* ptr, size_t count) noexcept {
        size_t cnt = count * sizeof(T);
        if (cnt <= 4) {
            FixedAllocator<4>::get_alloc().deallocate(ptr);
        } else if (cnt <= 8) {
            FixedAllocator<8>::get_alloc().deallocate(ptr);
        } else if (cnt <= 16) {
            FixedAllocator<16>::get_alloc().deallocate(ptr);
        } else if (cnt <= 24) {
            FixedAllocator<24>::get_alloc().deallocate(ptr);
        } else if (cnt <= 32) {
            FixedAllocator<32>::get_alloc().deallocate(ptr);
        } else {
            ::operator delete(ptr);
        }
    }
};

template<typename T, typename U>
bool operator==(const FastAllocator<T>&, const FastAllocator<U>&) {
    return true;
}

template<typename T, typename U>
bool operator!=(const FastAllocator<T>& a, const FastAllocator<U>& b) {
    return !(a == b);
}

template<typename T, typename Allocator = std::allocator<T>>
class List {
public:
    struct Node {
        T value;
        Node* next = nullptr;
        Node* prev = nullptr;
        Node() = default;
        Node(const T& value): value(value) {}
        Node(T&& value): value(std::move(value)) {}
    };
private:

    void cut(const Node* v) const {
        v->prev->next = v->next;
        v->next->prev = v->prev;
    }

    void linkAfter(Node* v, Node* nn) const {
        nn->next = v->next;
        nn->prev = v;
        v->next->prev = nn;
        v->next = nn;
    }

    Node* create_new_node(const T& t) {
        Node* nn = AllocTraits::allocate(allocator, 1);
        AllocTraits::construct(allocator, nn, t);
        return nn;
    }

    Node* create_new_node(T&& t) {
        Node* nn = AllocTraits::allocate(allocator, 1);
        AllocTraits::construct(allocator, nn, std::move(t));
        return nn;
    }

    void clear_for_move() {
        n_elem = 0;
        head = AllocTraits::allocate(allocator, 1);
        head->next = head;
        head->prev = head;
    }

public:


    void swap(List& lst) {
        std::swap(head, lst.head);
        std::swap(n_elem, lst.n_elem);
        if (AllocTraits::propagate_on_container_swap::value) {
            std::swap(t_allocator, lst.t_allocator);
            std::swap(allocator, lst.allocator);
        }
    }

    template<bool IsConst>
    class common_iterator {
    private:
        using T_ref = std::conditional_t<IsConst, const T&, T&>;
        using T_ptr = std::conditional_t<IsConst, const T*, T*>;
    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = std::conditional_t<IsConst, const T, T>;
        using pointer = T_ptr;
        using reference = T_ref;
        using difference_type = std::ptrdiff_t;


        std::conditional_t<IsConst, const Node*, Node*> ptr = nullptr;



        common_iterator() = default;

        common_iterator(const common_iterator<IsConst>& iter): ptr(iter.ptr) {}

        operator common_iterator<true> () {
            common_iterator<true> it(ptr);
            return it;
        }

        common_iterator(std::conditional_t<IsConst, const Node*, Node*> node_ptr): ptr(node_ptr) {}

        common_iterator& operator++() {
            ptr = ptr->next;
            return *this;
        }

        common_iterator operator++(int) {
            common_iterator temp = *this;
            ++(*this);
            return temp;
        }

        common_iterator& operator--() {
            ptr = ptr->prev;
            return *this;
        }

        common_iterator operator--(int) {
            common_iterator temp = *this;
            --(*this);
            return temp;
        }
        template<bool Y>
        bool operator==(const common_iterator<Y>& iter) const {
            return ptr == iter.ptr;
        }

        template<bool Y>
        bool operator!=(const common_iterator<Y>& iter) const {
            return ptr != iter.ptr;
        }

        reference operator*() const { ///MAY BE DELETE CONST IN THE END
            return ptr->value;
        }

        pointer operator->() const {
            return &(ptr->value); ///?????
        }
    };

    using TAllocTraits = std::allocator_traits<Allocator>;
    using nodeAlloc = typename TAllocTraits::template rebind_alloc<Node>;
    using AllocTraits = std::allocator_traits<nodeAlloc>;

    size_t n_elem;
    Allocator t_allocator;
    nodeAlloc allocator;

    Node* head;

    using iterator = common_iterator<false>;
    using const_iterator = common_iterator<true>;

    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    explicit List(const Allocator& alloc = Allocator()): n_elem(0), t_allocator(alloc), \
                                                                    head(AllocTraits::allocate(allocator, 1)) {
        head->next = head;
        head->prev = head;
    }

    explicit List(size_t count, const T& value, const Allocator& alloc = Allocator()): List(alloc) {
        for (size_t i = 0; i < count; ++i)
            push_back(value);
    }

    explicit List(size_t count, const Allocator& alloc = Allocator()): List(alloc) {
        n_elem = count;
        for (size_t i = 0; i < count; ++i) {
            Node* nn = AllocTraits::allocate(allocator, 1);
            AllocTraits::construct(allocator, nn);
            linkAfter(head->prev, nn);
        }
    }

    void clear() noexcept {
        while (n_elem != 0) {
            pop_back();
        }
        Node* nxt = head->next;
        for (size_t i = 0; i < n_elem; ++i) {
            Node* to = nxt->next;
            AllocTraits::destroy(allocator, nxt);
            AllocTraits::deallocate(allocator, nxt, 1);
            nxt = to;
        }
        n_elem = 0;
        head->prev = head;
        head->next = head;
    }

    void clear_all() {
        AllocTraits::deallocate(allocator, head, 1);
    }

    void create_empty_list() {
        head = AllocTraits::allocate(allocator, 1);
        head->next = head;
        head->prev = head;
    }

    List(List&& other): n_elem(other.n_elem), head(other.head) {
        other.clear_for_move();
        allocator = std::move(other.allocator);
        t_allocator = std::move(other.t_allocator);
    }

    List(const List& lst): n_elem(lst.n_elem), t_allocator(AllocTraits::select_on_container_copy_construction(lst.t_allocator)),\
                                    allocator(AllocTraits::select_on_container_copy_construction(lst.allocator)){
        head = AllocTraits::allocate(allocator, 1);
        head->next = head;
        head->prev = head;
        const Node* ptr = lst.head->next;
        for (size_t i = 0; i < n_elem; ++i) {
            Node* nn = create_new_node(ptr->value);
            linkAfter(head->prev, nn);
            ptr = ptr->next;
        }
    }

    List& operator=(const List& lst) {
        if (&lst == this)
            return *this;
        clear();
        if (AllocTraits::propagate_on_container_copy_assignment::value && t_allocator != lst.t_allocator) {
            clear_all();
            allocator = lst.allocator;
            t_allocator = lst.t_allocator;
            create_empty_list();
        }
        Node* ptr = lst.head->next;
        for (size_t i = 0; i < lst.size(); ++i) {
            push_back(ptr->value);
            ptr = ptr->next;
        }
        return *this;
    }

    List& operator=(List&& lst) noexcept {
        if (this == &lst)
            return *this;
        clear();
        if (AllocTraits::propagate_on_container_move_assignment::value && t_allocator != lst.t_allocator) {
            clear_all();
            t_allocator = lst.t_allocator;
            allocator = lst.allocator;
        }
        head = lst.head;
        n_elem = lst.n_elem;

        lst.create_empty_list();
        lst.n_elem = 0;
        return *this;
    }

    Allocator get_allocator() const {
        return t_allocator;
    }

    size_t size() const {
        return n_elem;
    }

    void push_back(const T& t) {
        Node* nn = create_new_node(t);
        ++n_elem;
        linkAfter(head->prev, nn);
    }

    void push_back(T&& t) {
        Node* nn = create_new_node(std::move(t));
        ++n_elem;
        linkAfter(head->prev, nn);
    }

    void push_front(const T& t) {
        Node* nn = create_new_node(t);
        ++n_elem;
        linkAfter(head, nn);
    }

    void push_front(T&& t) {
        Node* nn = create_new_node(std::move(t));
        ++n_elem;
        linkAfter(head, nn);
    }

    void pop_back() {
        --n_elem;
        Node* ptr = head->prev;
        cut(ptr);
        AllocTraits::destroy(allocator, ptr);
        AllocTraits::deallocate(allocator, ptr, 1);
    }

    void pop_front() {
        --n_elem;
        Node* ptr = head->next;
        cut(ptr);
        AllocTraits::destroy(allocator, ptr);
        AllocTraits::deallocate(allocator, ptr, 1);
    }

    template<typename ...Args>
    iterator emplace(const const_iterator& pos, Args&& ...args) {
        Node* nn = AllocTraits::allocate(allocator, 1);
        AllocTraits::construct(allocator, nn, std::forward<Args>(args)...);
        ++n_elem;
        linkAfter(pos.ptr->prev, nn);
        iterator ret(nn);
        return ret;
    }

    iterator insert(const const_iterator& it, const T& t) {
        Node* nn = create_new_node(t);
        ++n_elem;
        linkAfter(it.ptr->prev, nn);
        iterator ret(nn);
        return ret;
    }

    iterator insert(const const_iterator& it, T&& t) {
        Node* nn = create_new_node(std::move(t));
        ++n_elem;
        linkAfter(it.ptr->prev, nn);
        iterator ret(nn);
        return ret;
    }

    iterator erase(const const_iterator& it) {
        iterator nxt(const_cast<Node*>(it.ptr));
        ++nxt;
        --n_elem;
        cut(it.ptr);
        AllocTraits::destroy(allocator, it.ptr);
        AllocTraits::deallocate(allocator, const_cast<Node*>(it.ptr), 1);
        return nxt;
    }

    iterator erase(const_iterator first, const_iterator last) {
        while (first != last) {
            first = erase(first);
        }
        iterator ans(const_cast<Node*>(first.ptr));
        return ans;
    }

    iterator begin() {
        iterator it = head->next;
        return it;
    }

    iterator end() {
        iterator it = head;
        return it;
    }

    const_iterator begin() const {
        const_iterator it = head->next;
        return it;
    }

    const_iterator end() const {
        const_iterator it = head;
        return it;
    }

    const_iterator cbegin() const {
        const_iterator it = head->next;
        return it;
    }

    const_iterator cend() const {
        const_iterator it = head;
        return it;
    }

    reverse_iterator rbegin() {
        reverse_iterator it(head);
        return it;
    }

    reverse_iterator rend() {
        reverse_iterator it(head->next);
        return it;
    }

    const_reverse_iterator rbegin() const {
        const_reverse_iterator it(head);
        return it;
    }

    const_reverse_iterator rend() const {
        const_reverse_iterator it(head->next);
        return it;
    }

    const_reverse_iterator crbegin() const {
        const_reverse_iterator it(head);
        return it;
    }

    const_reverse_iterator crend() const {
        const_reverse_iterator it(head->next);
        return it;
    }

    ~List() {
        clear();
        clear_all();
    }

};
