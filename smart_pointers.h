#include <iostream>
#include <memory>
#include <optional>

struct AllocateSharedTag {};

struct BaseControlBlock {
    virtual void* get_ptr() = 0;
    virtual size_t& get_counter() = 0;
    virtual size_t& get_weak_counter() = 0;
    virtual void ask_deleter(void*) = 0;
    virtual void destroy_object() = 0;
    virtual void destroy() = 0;
    virtual void deallocate() = 0;
    virtual ~BaseControlBlock() {};
};

template<typename T, typename Allocator = std::allocator<T>, typename Deleter = std::default_delete<T>>
struct ControlBlock: public BaseControlBlock {
    size_t counter;
    size_t weak_counter = 0;
    std::optional<T> object;
    Allocator alloc;
    Deleter del;
    bool constructed_by_allocator = 0;

    using SelfType = ControlBlock<T, Allocator, Deleter>;
    using SelfAlloc = typename std::allocator_traits<Allocator>::template rebind_alloc<SelfType>;
    using SelfAllocTraits = std::allocator_traits<SelfAlloc>;

    ControlBlock(size_t counter): counter(counter) {}

    ControlBlock(size_t counter, const Deleter& del): counter(counter), del(del) {}
    ControlBlock(size_t counter, const Deleter& del, const Allocator& alloc): counter(counter), alloc(alloc), del(del) {}

    ControlBlock(size_t counter, const Allocator& alloc): counter(counter), alloc(alloc) {}

    ControlBlock(size_t counter, const T& object,
                         const Allocator& alloc = Allocator(), const Deleter& del = Deleter()):
                             counter(counter), object(object), alloc(alloc), del(del) {}

    ControlBlock(size_t counter, T&& object,
                         const Allocator& alloc = Allocator(), const Deleter& del = Deleter()):
                             counter(counter), object(std::move(object)), alloc(alloc), del(del) {}

    template<typename ...Args>
    ControlBlock(AllocateSharedTag, size_t counter, const Allocator& alloc, Args&&... args): counter(counter),
                                                object(std::in_place_t(), std::forward<Args>(args)...), alloc(alloc) {}

    void* get_ptr() override {
        if (!object.has_value()) {
            return nullptr;
        }
        return static_cast<void*>(&object.value());
    }

    size_t& get_counter() override {
        return counter;
    }

    size_t& get_weak_counter() override {
        return weak_counter;
    }

    void ask_deleter(void* ptr) override {
        del(static_cast<T*>(ptr));
    }

    void destroy_object() override {
        object.reset();
    }

    void deallocate() override {
        SelfAlloc a(alloc);
        SelfAllocTraits::deallocate(a, this, 1);
    }

    void destroy() override {
        SelfAlloc a(alloc);
        if (constructed_by_allocator)
            SelfAllocTraits::destroy(a, this);
        SelfAllocTraits::deallocate(a, this, 1);
    }

    ~ControlBlock() override = default;
};

template<typename T>
class WeakPtr;

template<typename T>
class SharedPtr {
private:
    struct make_shared_t {};

    template<typename Y, typename ...Args>
    friend SharedPtr<Y> makeShared(Args&&...);

    template<typename Y, typename Alloc, typename ...Args>
    friend SharedPtr<Y> allocateShared(const Alloc&, Args&&...);

    template<typename Y>
    friend class WeakPtr;

    template<typename Y>
    friend class SharedPtr;

    SharedPtr(make_shared_t, BaseControlBlock* cptr): cptr(cptr) {}

    template<typename Y>
    explicit SharedPtr(const WeakPtr<Y>& w): cptr(w.cptr), ptr(w.ptr) {
        if (cptr)
            ++(cptr->get_counter());
    }

    BaseControlBlock* cptr = nullptr;
    T* ptr = nullptr;

public:
    void swap(SharedPtr& sh) {
        std::swap(sh.cptr, cptr);
        std::swap(sh.ptr, ptr);
    }

    T* get() const {
        if (!cptr)
            return nullptr;
        if (!ptr)
            return static_cast<T*>(cptr->get_ptr());
        return ptr;
    }

    constexpr SharedPtr() noexcept = default;

    template<typename Y, typename Deleter, typename Alloc>
    SharedPtr(Y* ptr, Deleter d, Alloc alloc): ptr(ptr) {
        using BlockType = ControlBlock<Y, Alloc, Deleter>;
        using BlockAlloc = typename std::allocator_traits<Alloc>::template rebind_alloc<BlockType>;
        using AllocTraits = std::allocator_traits<BlockAlloc>;

        BlockAlloc block_allocator(alloc);
        auto storage = AllocTraits::allocate(block_allocator, 1);
        new(storage) BlockType(1, d, alloc);
        cptr = storage;
    }

    template<typename Y, typename Deleter>
    SharedPtr(Y* ptr, Deleter d): SharedPtr<Y>(ptr, d, std::allocator<Y>()) {}

    template<typename Y>
    explicit SharedPtr(Y* ptr): SharedPtr<Y> (ptr, std::default_delete<Y>()) {}

    SharedPtr(const SharedPtr& sh): cptr(sh.cptr), ptr(sh.ptr) {
        if (cptr)
            ++(cptr->get_counter());
    }

    template<typename Y>
    SharedPtr(const SharedPtr<Y>& sh): cptr(sh.cptr), ptr(sh.ptr) {
        if (cptr)
            ++(cptr->get_counter());
    }

    SharedPtr(SharedPtr&& sh): cptr(sh.cptr), ptr(sh.ptr) {
        sh.cptr = nullptr;
        sh.ptr = nullptr;
    }

    template<typename Y>
    SharedPtr(SharedPtr<Y>&& sh): cptr(sh.cptr), ptr(sh.ptr) {
        sh.cptr = nullptr;
        sh.ptr = nullptr;
    }

    SharedPtr& operator=(const SharedPtr& sh) {
        SharedPtr temp = sh;
        swap(temp);
        return *this;
    }

    template<typename Y>
    SharedPtr& operator=(const SharedPtr<Y>& sh) {
        SharedPtr temp = sh;
        swap(temp);
        return *this;
    }

    SharedPtr& operator=(SharedPtr&& sh) {
        SharedPtr temp = std::move(sh);
        swap(temp);
        return *this;
    }

    template<typename Y>
    SharedPtr& operator=(SharedPtr<Y>&& sh) {
        SharedPtr temp = std::move(sh);
        swap(temp);
        return *this;
    }

    T& operator*() const {
        if (ptr)
            return *ptr;
        return (static_cast<ControlBlock<T>*>(cptr)->object).value();
    }

    T* operator->() const {
        return &(**this);
    }

    size_t use_count() const {
        if (!cptr)
            return 0;
        return cptr->get_counter();
    }

    void reset() {
        SharedPtr<T>().swap(*this);
    }

    template<typename Y>
    void reset(Y* ptr) {
        SharedPtr<T>(ptr).swap(*this);
    }

    template<typename Y, typename Deleter>
    void reset(Y* ptr, Deleter del) {
        SharedPtr<T>(ptr, del).swap(*this);
    }

    template<typename Y, typename Deleter, typename Alloc>
    void reset(Y* ptr, Deleter del, Alloc alloc) {
        SharedPtr<T>(ptr, del, alloc).swap(*this);
    }

    ~SharedPtr() {
        if (!cptr)
            return;

        if (cptr->get_counter() > 1) {
            --cptr->get_counter();
            return;
        }

        if (ptr)
            cptr->ask_deleter(static_cast<void*>(ptr));
        else
            cptr->destroy_object();

        --(cptr->get_counter());

        if (cptr->get_weak_counter() == 0)
            cptr->destroy();
    }
};

template<typename T, typename Alloc, typename ...Args>
SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args) {
    using BlockType = ControlBlock<T, Alloc>;
    using ControlBlockAlloc = typename std::allocator_traits<Alloc>::template rebind_alloc<BlockType>;
    using BlockAllocTraits = std::allocator_traits<ControlBlockAlloc>;

    ControlBlockAlloc a(alloc);
    auto storage = BlockAllocTraits::allocate(a, 1);
    BlockAllocTraits::construct(a, storage, AllocateSharedTag(), 1, alloc, std::forward<Args>(args)...);
    storage->constructed_by_allocator = true;
    return SharedPtr<T>(typename SharedPtr<T>::make_shared_t(), storage);
}

template<typename T, typename ...Args>
SharedPtr<T> makeShared(Args&&... args) {
    return allocateShared<T>(std::allocator<T>(), std::forward<Args>(args)...);
}


template<typename T>
class WeakPtr {

    template<typename Y>
    friend class WeakPtr;

    template<typename Y>
    friend class SharedPtr;

private:
    BaseControlBlock* cptr = nullptr;
    T* ptr = nullptr;

public:
    void swap(WeakPtr& r) {
        std::swap(cptr, r.cptr);
        std::swap(ptr, r.ptr);
    }

    constexpr WeakPtr() noexcept = default;

    template<typename Y>
    WeakPtr(const SharedPtr<Y>& sh): cptr(sh.cptr), ptr(sh.ptr) {
        if (cptr)
            ++(cptr->get_weak_counter());
    }

    WeakPtr(const WeakPtr& r): cptr(r.cptr), ptr(r.ptr) {
        if (cptr)
            ++(cptr->get_weak_counter());
    }

    template<typename Y>
    WeakPtr(const WeakPtr<Y>& r): cptr(r.cptr), ptr(r.ptr) {
        if (cptr)
            ++(cptr->get_weak_counter());
    }

    WeakPtr(WeakPtr&& r): cptr(r.cptr), ptr(r.ptr) {
        r.cptr = nullptr;
        r.ptr = nullptr;
    }

    template<typename Y>
    WeakPtr(WeakPtr&& r): cptr(r.cptr), ptr(r.ptr) {
        r.cptr = nullptr;
        r.ptr = nullptr;
    }

    WeakPtr& operator=(const WeakPtr& r) {
        WeakPtr tmp = r;
        swap(tmp);
        return *this;
    }

    template<typename Y>
    WeakPtr& operator=(const WeakPtr<Y>& r) {
        WeakPtr tmp = r;
        swap(tmp);
        return *this;
    }

    template<typename Y>
    WeakPtr& operator=(const SharedPtr<Y>& r) {
        WeakPtr tmp = r;
        swap(tmp);
        return *this;
    }

    WeakPtr& operator=(WeakPtr&& r) {
        WeakPtr tmp = std::move(r);
        swap(tmp);
        return *this;
    }

    template<typename Y>
    WeakPtr& operator=(WeakPtr<Y>&& r) {
        WeakPtr tmp = std::move(r);
        swap(tmp);
        return *this;
    }

    long use_count() const {
        if (!cptr)
            return 0;
        return cptr->get_counter();
    }

    bool expired() const {
        return use_count() == 0;
    }

    SharedPtr<T> lock() const {
        return expired() ? SharedPtr<T>() : SharedPtr<T>(*this);
    }

    ~WeakPtr() {
        if (cptr == nullptr)
            return;
        --(cptr->get_weak_counter());
        if (cptr->get_counter() + cptr->get_weak_counter() == 0)
            cptr->destroy();
    }
};
