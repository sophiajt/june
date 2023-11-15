## Goal: Teachability, learnability, and readability

June was born of multiple years of experience teaching Rust. Rust, while being a powerful systems language, is also incredibly difficult to learn and a special challenge as a teacher to deliver complex concepts to classrooms of diverse technical backgrounds. In essence, it came from the idea: "what if we could do safe systems programming in a simpler way?"

From this vision came a language that used a different granularity for lifetimes, which allowed for easier inference and an easier lifetime mental model: "related things are allocated together".

The language itself draws from a variety of inspirations in addition to Rust: Go, Swift, embedded C, and others. The syntax has the goal of not requiring a lot of decorations or uncommon annotations. Instead, the syntax is built to be light and readable regardless of the languages the programmer is familiar with.

June is also built to be a teaching language that could be used to teach software engineering concepts. It is suitable to be used as someone's first programming language.

## Goal: Efficiency

One major benefit of systems programming, in addition to the flexibility of being usable on bare-metal, is the efficiency of the compiled code. June leverages these benefits in its design, and includes some uncommon language design choices that leverage decades of embedded and high-performance work.

Namely, June uses the idea of a custom allocator as one of its core implementation concepts. June developers will be invoking the default (and in the future custom) allocation strategies that allow groups of related allocations to efficiently sit beside each other in memory, even if those allocations are interleaved with allocations meant for other groups.

The end goal is that related allocations maintain cache locality as much as possible. This gives the added benefit that programmers need not restrict themselves to vectors of structs to maximise efficiency via cache locality. Instead, they can build data structures in a way that feels more natural and readable, while still benefitting from the underlying locality of the allocations used to build those data structures.

## Goal: Systems and application programming

June is a systems language, meaning it is intended to be able to be used to create low-level applications that can work directly on the metal. Use of the allocator will be tune-able for these systems, and is already a natural fit for systems that do not have access to OS-level memory management.

June is also geared towards application programming. Its direct OO approach coupled with its ease-of-use encourages application development using tradition OOP UI approaches as well as more innovative data-oriented styles. In essence, June allows the developer to pick the implementation that works best for the problem, and then encourages strong encapsulation for code reuse.

## Non-goal: Fine-grained memory reclamation

One characterist of June that you'll notice is that it prefers holding on to memory longer than other languages. You can think of this as a kind of memory "bloat" compared to more fine-grained allocation system. In one sense, the style is something more akin to garbage collection, though June does not use a garbage collector.

As it turns out, using intentional memory bloat is common. Rust and C++ developers, for example, use bloat to their advantage regularly. Take for example this Rust code:

```rust
fn main() {
    let mut v = vec![];

    println!("{}", v.capacity());

    for _ in 1..10000 {
        v.push(5);
    }

    println!("{}", v.capacity());

    for _ in 1..10000 {
        v.pop();
    }

    println!("{}", v.capacity());
}
```

Notice how capacity does not decrease, even as you empty the vector of its contents. The same is true in C++. This is an intentional design choice to allow you to begin pushing values again, and already have ample space.

In a way, vectors in C++ and Rust assume a "high-water mark" style of allocation, where they don't aggressively deallocate as the container shrinks its content size. Instead, they keep the highest allocation size as the working set.

In June, we use this concept for groups of related allocations. Much in the same way that a vector maintains a high-water allocation size, June encourages memory reuse rather than deletion and reallocation.

The goal of this is two-fold: it is more efficient to not free and reallocate memory from the system allocator, and it allows us to track the lifetime of related allocations together as a single entity. The former helps improve speed while the latter helps make the lifetime checking of a June program significantly simpler than borrow checking systems with fine-grained lifetimes.

sjt: do we want to call this "bloat" or give it a nicer name?

## Non-goal: RAII

RAII, or Resource Acquisition Is Initialisation, is a common pattern with C++ and Rust developers.

In June, however, it is not the goal of the language to support this pattern. There are a few reasons for this.

RAII assumes a focus on a single object's lifetime, and an immediate trigger of an event (often the destructor) when the object's lifetime has ended. This might be used to free memory or additional resources.

June's memory system instead tracks groups of related lifetimes together as one. This muddies the RAII concept, as there is no implied ordering of allocations among a group's allocations. Without this order, it is unclear as to which order destructors should run.

Equally important is June's ability to drop _all_ memory allocated to a group as a single drop. This is much more efficient then having to order destructors and waiting for them to run in sequence.

June does offer the `defer` command to defer the freeing of a resource, which can be attached to the lifetime of a group via one of its pointers. In effect, this can approximate some of the cleanup logic of RAII without enforcing the whole of the RAII pattern.
