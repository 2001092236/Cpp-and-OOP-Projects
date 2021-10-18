#include <cassert>
#include <memory>
#include <list>
#include <vector>
#include <iostream>
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

    void deallocate(void* ptr, size_t) {
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
            FixedAllocator<4>::get_alloc().deallocate(ptr, 4);
        } else if (cnt <= 8) {
            FixedAllocator<8>::get_alloc().deallocate(ptr, 8);
        } else if (cnt <= 16) {
            FixedAllocator<16>::get_alloc().deallocate(ptr, 16);
        } else if (cnt <= 24) {
            FixedAllocator<24>::get_alloc().deallocate(ptr, 24);
        } else if (cnt <= 32) {
            FixedAllocator<32>::get_alloc().deallocate(ptr, 32);
        } else {
            ::operator delete(ptr);
        }
    }

    template<class U>
    struct rebind {
        using other = FastAllocator<U>;
    };

};

template<typename T, typename U>
bool operator==(const FastAllocator<T>&, const FastAllocator<U>&) {
    return true;
}

template<typename T, typename U>
bool operator!=(const FastAllocator<T>&, const FastAllocator<U>&) {
    return false;
}

template<typename T, typename Allocator = std::allocator<T>>
class List {
public:
    struct Node {
        T value;
        Node* next = nullptr;
        Node* prev = nullptr;
        Node() {}
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

    void swap(List& elements) {
        std::swap(head, elements.head);
        std::swap(n_elem, elements.n_elem);
        if (AllocTraits::propagate_on_container_swap::value) {
            std::swap(t_allocator, elements.t_allocator);
            std::swap(allocator, elements.allocator);
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

        reference operator*() const {
            return ptr->value;
        }

        pointer operator->() const {
            return &(ptr->value);
        }
    };

    using TAllocTraits = std::allocator_traits<Allocator>;
    using nodeAlloc = typename Allocator::template rebind<Node>::other;
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

    List(const List& elements): n_elem(elements.n_elem), t_allocator(AllocTraits::select_on_container_copy_construction(elements.t_allocator)),\
                                    allocator(AllocTraits::select_on_container_copy_construction(elements.allocator)){
        head = AllocTraits::allocate(allocator, 1);
        head->next = head;
        head->prev = head;
        const Node* ptr = elements.head->next;
        for (size_t i = 0; i < n_elem; ++i) {
            Node* nn = create_new_node(ptr->value);
            linkAfter(head->prev, nn);
            ptr = ptr->next;
        }
    }

    List& operator=(const List& elements) {
        if (&elements == this)
            return *this;
        clear();
        if (AllocTraits::propagate_on_container_copy_assignment::value && t_allocator != elements.t_allocator) {
            clear_all();
            allocator = elements.allocator;
            t_allocator = elements.t_allocator;
            create_empty_list();
        }
        Node* ptr = elements.head->next;
        for (size_t i = 0; i < elements.size(); ++i) {
            push_back(ptr->value);
            ptr = ptr->next;
        }
        return *this;
    }

    List& operator=(List&& elements) noexcept {
        if (this == &elements)
            return *this;
        clear();
        if (AllocTraits::propagate_on_container_move_assignment::value && t_allocator != elements.t_allocator) {
            clear_all();
            t_allocator = elements.t_allocator;
            allocator = elements.allocator;
        }
        head = elements.head;
        n_elem = elements.n_elem;

        elements.create_empty_list();
        elements.n_elem = 0;
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


template<typename Key,
         typename Value,
         typename Hash = std::hash<Key>,
         typename Equal = std::equal_to<Key>,
         typename Alloc = std::allocator<std::pair<const Key, Value>>
>class UnorderedMap {
public:
    using NodeType = std::pair<const Key, Value>;

private:
    struct HashableNodeType {
        std::pair<const Key, Value> nodeValue;
        size_t hash = 0;

        const Key& first() const {
            return nodeValue.first;
        }

        Value& second() {
            return nodeValue.second;
        }

        const Value& second() const {
            return nodeValue.second;
        }

        template<typename ...Args>
        HashableNodeType(Args&&... args): nodeValue(std::forward<Args>(args)...) {}
    };

    using PtrAllocType = typename Alloc::template rebind<HashableNodeType*>::other;
    using LstIterator = typename List<HashableNodeType*, PtrAllocType>::iterator;
    using LstConstIterator = typename List<HashableNodeType*, PtrAllocType>::const_iterator;
    using NodeHashAlloc = typename Alloc::template rebind<HashableNodeType>::other;
    using AllocTraits = std::allocator_traits<NodeHashAlloc>;

    size_t getIdInHashTable(const Key& k) const {
        return hashFunc(k) % tableSize;
    }

    LstIterator linkToMap(HashableNodeType* ptr) {
        ptr->hash = hashFunc(ptr->first());
        size_t id = ptr->hash % tableSize;
        LstIterator it = hashTable[id];
        bool empty = (elements.end() == it);
        while (it != elements.end() && (*it)->hash % tableSize == id) {
            if (equalFunc((*it)->first(), ptr->first())) {
                (*it) = ptr;
                return it;
            }
            ++it;
        }
        LstIterator ins = elements.insert(it, ptr);

        if (empty)
            hashTable[id] = ins;
        return ins;
    }

    void clear_and_deallocate() {
        for (typename List<HashableNodeType*, PtrAllocType>::const_iterator it = elements.begin(); it != elements.end(); ++it) {
            AllocTraits::destroy(nodeAlloc, *it);
            AllocTraits::deallocate(nodeAlloc, *it, 1);
        }
        clear();
    }

    NodeHashAlloc nodeAlloc;
    List<HashableNodeType*, PtrAllocType> elements;

    Hash hashFunc;
    Equal equalFunc;

    size_t tableSize = 1;
    float maxLoadFactor = 0.5;
    std::vector<LstIterator> hashTable;

public:

    class Iterator: public LstIterator {
    public:
        Iterator(const LstIterator& it) {
            this->ptr = it.ptr;
        }
        NodeType& operator*() {
            return (*((this->ptr)->value)).nodeValue;
        }

        NodeType* operator->() {
            return &(((this->ptr)->value)->nodeValue);
        }
    };

    class ConstIterator: public List<HashableNodeType*, PtrAllocType>::const_iterator {
    public:
        ConstIterator(const LstConstIterator& it) {
            this->ptr = it.ptr;
        }

        ConstIterator(const LstIterator& it) {
            this->ptr = it.ptr;
        }
        ConstIterator(const Iterator& iter) {
            this->ptr = iter.ptr;
        }

        const NodeType& operator*() const {
            return (*((this->ptr)->value)).nodeValue;
        }

        const NodeType* operator->() const {
            return &(((this->ptr)->value)->nodeValue);
        }
    };

    explicit UnorderedMap(const Alloc& alloc = Alloc()): nodeAlloc(alloc) {
        hashTable.resize(tableSize, elements.end());
    }

    explicit UnorderedMap(size_t bucketCount, const Hash& hash = Hash(), const Equal& equal = Equal(), const Alloc& alloc = Alloc()):
                            nodeAlloc(alloc), hashFunc(hash), equalFunc(equal), tableSize(bucketCount) {
        hashTable.resize(tableSize, elements.end());
    }

    Alloc get_allocator() const noexcept {
        return Alloc(nodeAlloc);
    }

    Iterator find(const Key& key) {
        size_t id = getIdInHashTable(key);
        LstIterator it = hashTable[id];
        while (it != elements.end() && (*it)->hash % tableSize == id) {
            if (equalFunc((*it)->first(), key)) {
                return static_cast<Iterator>(it);
            }
            ++it;
        }
        return end();
    }

    ConstIterator find(const Key& key) const {
        return const_cast<UnorderedMap&>(*this).find(key);
    }

    Value& at(const Key& key) {
        Iterator it = find(key);
        if (it != end()) {
            return it->second;
        }
        throw std::out_of_range("This element isn't exist in unordered map!!!");
    }

    const Value& at(const Key& key) const {
        return const_cast<UnorderedMap&>(*this).at(key);
    }

    template<typename InputIt>
    void insert(InputIt first, InputIt last) {
        for (auto i = first; i != last; ++i) {
            insert(*i);
        }
    }

    size_t erase(const Key& key) {
        Iterator it = find(key);
        if (it == elements.end())
            return 0;
        erase(it);
        return 1;
    }


    template<typename ...Args>
    std::pair<Iterator, bool> emplace(Args&&... args) {
        HashableNodeType* ptr = AllocTraits::allocate(nodeAlloc, 1);
        using AllocTraitsForNodeType = std::allocator_traits<Alloc>;
        NodeType* ptr_p = &(ptr->nodeValue);
        Alloc allocNodeType(nodeAlloc);
        AllocTraitsForNodeType::construct(allocNodeType, ptr_p, std::forward<Args>(args)...);

        if (load_factor() > max_load_factor()) {
            rehash(tableSize * 2);
        }

        ptr->hash = hashFunc(ptr->first());
        size_t id = ptr->hash % tableSize;
        LstIterator it = hashTable[id];
        bool emp = (elements.end() == it);
        while (it != elements.end() && (*it)->hash % tableSize == id) {
            if (equalFunc((*it)->first(), ptr->first())) {
                AllocTraits::destroy(nodeAlloc, ptr);
                AllocTraits::deallocate(nodeAlloc, ptr, 1);
                return {static_cast<Iterator>(it), false};
            }
            ++it;
        }

        LstIterator ins = elements.insert(it, ptr);

        if (emp) {
            hashTable[id] = ins;
        }
        return {static_cast<Iterator>(ins), true};
    }

    std::pair<Iterator, bool> insert(const NodeType& node) {
        return emplace(node);
    }

    std::pair<Iterator, bool> insert(NodeType&& node) {
        return emplace(std::move(node));
    }

    template<class P>
    std::pair<Iterator, bool> insert(P&& value) {
        return emplace(std::forward<P>(value));
    }

    Value& operator[](const Key& key) {
        if (load_factor() > max_load_factor()) {
            rehash(tableSize * 2);
        }
        Iterator it = find(key);
        if (it != end()) {
            return it->second;
        }

        HashableNodeType* ptr = AllocTraits::allocate(nodeAlloc, 1);
        AllocTraits::construct(nodeAlloc, ptr, key, Value());
        linkToMap(ptr);
        return ptr->second();
    }

    Value& operator[](Key&& key) {
        if (load_factor() > max_load_factor()) {
            rehash(tableSize * 2);
        }
        Iterator it = find(key);
        if (it != end()) {
            return it->second;
        }

        HashableNodeType* ptr = AllocTraits::allocate(nodeAlloc, 1);
        AllocTraits::construct(nodeAlloc, ptr, std::move(key), Value());
        linkToMap(ptr);
        return ptr->second();
    }

    Iterator erase(ConstIterator pos) {
        size_t id = getIdInHashTable(pos->first);
        LstIterator it = elements.erase(pos);
        if (pos == hashTable[id]) {
            if (it == elements.end() || id != (*it)->hash)
                hashTable[id] = elements.end();
            else
                hashTable[id] = it;
        }
        return static_cast<Iterator>(it);
    }

    Iterator erase(ConstIterator first, ConstIterator last) {
        while (first != last) {
            first = erase(first);
        }
        return Iterator(const_cast<typename List<HashableNodeType*, PtrAllocType>::Node*>(first.ptr));
    }

    void clear() {
        elements.clear();
        for (size_t i = 0; i < hashTable.size(); ++i) {
            hashTable[i] = elements.end();
        }
    }

    void swap(UnorderedMap& m) {
        std::swap(tableSize, m.tableSize);
        std::swap(maxLoadFactor, m.maxLoadFactor);
        std::swap(hashFunc, m.hashFunc);
        std::swap(equalFunc, m.equalFunc);
        elements.swap(m.elements);
        std::swap(hashTable, m.hashTable);
        if (AllocTraits::propagate_on_container_swap::value)
            std::swap(nodeAlloc, m.nodeAlloc);
    }

    UnorderedMap& operator=(const UnorderedMap& other) {
        if (&other == this)
            return *this;
        hashFunc = other.hashFunc;
        equalFunc = other.equalFunc;
        tableSize = other.tableSize;
        maxLoadFactor = other.maxLoadFactor;
        hashTable.resize(tableSize);
        clear();
        if (AllocTraits::propagate_on_container_copy_assignment::value && nodeAlloc != other.nodeAlloc) {
            elements.clear_all();
            nodeAlloc = other.nodeAlloc;
            elements.allocator = other.elements.allocator;
            elements.t_allocator = other.elements.t_allocator;
            elements.create_empty_list();
        }
        for (auto it = other.begin(); it != other.end(); ++it) {
            insert(*it);
        }
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        if (&other == this)
            return *this;

        clear_and_deallocate();
        hashFunc = std::move(other.hashFunc);
        equalFunc = std::move(other.equalFunc);
        tableSize = std::move(other.tableSize);
        maxLoadFactor = std::move(other.maxLoadFactor);
        hashTable = std::move(other.hashTable);
        elements = std::move(other.elements);
        other.tableSize = 1;
        other.maxLoadFactor = 0.5;
        other.hashTable.clear();
        other.hashTable.push_back(other.elements.end());
        return *this;
    }


    UnorderedMap(const UnorderedMap& other): nodeAlloc(AllocTraits::select_on_container_copy_construction(other.nodeAlloc)),
                            elements(AllocTraits::select_on_container_copy_construction(other.elements.get_allocator())),
                                                    hashFunc(other.hashFunc), equalFunc(other.equalFunc),
                                                        tableSize(other.bucket_count()), maxLoadFactor(other.max_load_factor())
    {
        hashTable.resize(tableSize, elements.end());
        for (auto it = other.elements.begin(); it != other.elements.end(); ++it) {
            insert((**it).nodeValue);
        }
    }


    UnorderedMap(UnorderedMap&& other): nodeAlloc(std::move(AllocTraits::select_on_container_copy_construction(other.nodeAlloc))),elements(std::move(other.elements)),
                                            hashFunc(std::move(other.hashFunc)), equalFunc(std::move(other.equalFunc)),
                                                tableSize(std::move(other.bucket_count())), maxLoadFactor(std::move(other.max_load_factor())), hashTable(std::move(other.hashTable))
    {
        other.tableSize = 1;
        other.maxLoadFactor = 0.5;
        other.hashTable.clear();
        other.hashTable.push_back(other.elements.end());
    }


    void rehash(size_t n) {
        if (n * maxLoadFactor < size())
            n = size() / maxLoadFactor + 1;

        UnorderedMap temp(n, hashFunc, equalFunc, nodeAlloc);

        for (typename List<HashableNodeType*, PtrAllocType>::iterator it = elements.begin(); it != elements.end(); ++it) {
            temp.linkToMap(*it);
        }
        elements.clear();
        swap(temp);
    }

    void reserve(size_t count) {
        if (std::ceil(count / max_load_factor()) < tableSize)
            return;
        rehash(std::ceil(count / max_load_factor()));
    }

    void max_load_factor(float ml) {
        maxLoadFactor = ml;
    }

    float max_load_factor() const {
        return maxLoadFactor;
    }

    size_t size() const noexcept {
        return elements.size();
    }

    bool empty() const noexcept {
        return elements.size() != 0;
    }

    size_t bucket_count() const {
        return tableSize;
    }

    size_t max_size() const noexcept {
        return std::numeric_limits<int>::max();
    }

    Iterator begin() {
        return static_cast<Iterator>(elements.begin());
    }

    ConstIterator begin() const {
        return static_cast<ConstIterator>(elements.cbegin());
    }

    ConstIterator cbegin() const {
        return static_cast<ConstIterator>(elements.cbegin());
    }

    Iterator end() {
        return static_cast<Iterator>(elements.end());
    }

    ConstIterator end() const {
        return static_cast<ConstIterator>(elements.cend());
    }

    ConstIterator cend() const {
        return static_cast<ConstIterator>(elements.cend());
    }

    float load_factor() const {
        if (bucket_count() == 0)
            return 0;
        return size() * 1.0 / bucket_count();
    }

    ~UnorderedMap() {
        for (typename List<HashableNodeType*, PtrAllocType>::const_iterator it = elements.begin(); it != elements.end(); ++it) {
            AllocTraits::destroy(nodeAlloc, *it);
            AllocTraits::deallocate(nodeAlloc, *it, 1);
        }
    }
};

