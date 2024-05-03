# Language Basics

##  Data types

### Basic data types

* `i64`: `123`, `0`
* `f64`: `3.5`, `1.0`
* `bool`: `true`, `false`
* `void`

### C interop
* `c_string`: `c"hello"`
* `c_voidptr` 
* `c_char`: `c'b'`
* `c_int`

### Enums

```
enum State {
    None
    Some(i64)
    Struct { x: i64, y: i64 }
}

let x = State::Struct(x: 66, y: 77)

match x {
    State::None => println(c"None")
    State::Some(x) => println(x)
    State::Struct(x, y) => println(y)
}
```

### Structs

```
struct Box {
    width: i64

    fun grow_width(mut self, amount: i64) {
        .width += amount
    }
}
```

Methods live inside of struct definitions. You can also use a shorthand to refer to the instance variables. Rather than writing `self.width`, you can write `.width`.

###  Classes

You can use the `class` keyword interchangebly with `struct`. The only difference between the two is that classes make members and functions private by default. Note: This part of the design is currently in flux.

### Functions

```
fun foo(x: i64) -> i64 {
    return x
}

let bar = foo

println(bar(3))
```

### Raw buffers

Raw buffers represent a simple array with unchecked access. Because the access in unchecked, you need to index using an unsafe block.

```
mut buffer = raw[1, 2, 3]
unsafe {
    buffer[1] = 5 
    println(buffer[1])
}
```

You can also resize raw buffers (syntax definitely not final)

```
mut x = raw[1]

unsafe {
    resize x 2000 
    x[1999] = 100
    println(x[1999])
}
```

##  Variables

### Immutable-by-default

Variables, when created, are immutable by default. This binding controls the mutability of what is bound.

For example:

```
let x = 3
```

Creates the immutable variable `x`. Once created, its value can not be changed.

Likewise:

```
let foo = new Foo()
```

Creates an immutable `foo`. Reaching through `foo` gives you an immutable value, so it can not be used to updated the `Foo` object.

### Mutable bindings

You can create a mutable binding variable with `mut`, like so:

```
mut x = 3
x = 5
```

##  Control flow

### if..else

```
if true {
    println("hello")
} else {
    println("world")
}
```

### for..in

```
mut total = 0
for i in 1..10 {
    total += i
}

println(total)
```

### while

```
mut x = 1
while x < 10 {
    x = x + 1
}

println(x)
```

### return

```
fun foo() -> i64 {
    return 3
}
```

### break

```
while true {
    break
}
```

##  Defer :clock11: 

You can also defer work to be done when a pointer's group is being freed. To do this, use the `defer` keyword and pass in both the pointer to infer the group from and the function to run on the pointer right before the group is freed.

```
struct Person {
    name: c_string
}

fun greet(p: Person) {
    println(p.name)
}

fun main() {
    let person = new Person(name: c"Felicia");
    defer person greet
}
```

##  Generics

### Generic enum

```
enum Option<T> {
    None
    Some(T)
}
```

### Generic struct

```
struct Foo<T> {
    x: T
}
```

### Generic functions

```
fun id<T>(x: T) [x == return] -> T  {
    return x
}
```

## Lifetime annotations    

To describe the relationship between parameters and how they may be used, specifically how they may escape into other parameters or be used as a return value, we need to tell the compiler that this is allowed. To do so, we use a 'lifetime annotation' on the function.

Example lifetime annotations saying that two parameters have the same lifetime:
```
fun assign(mut person: Person, stats: Stats) [stats == person] {
    person.stats = stats
}
```

Example lifetime annotation saying the parameter can be used as a return value:
```
fun identity(x: Stats) [x == return] -> Stats {
    return x
}
```

##  Modules :pie:

Note: The module system is currently under active development and does not yet match the ultimately planned behavior.

#### Current State

Simple multi-file project support example:

main.june
```
use mod1;

mod1::foo()
```

mod1.june (in the future, this will likely require the `export` keyword)
```
fn foo() {}
```

file structure:
```
src/
    main.june mod1.june
```

### Future design
Public APIs of modules are defined using the `export` keyword. The module tree structure of a june application or library is defined by it's file structure. Modules can be either files in the same directory as their parent module, or they can be subdirectories, with an optional file matching the subdirectory name defining the submodule, with additional files being submodules of the submodule.


```
main/
    main.june
    mod1.june
    mod2.june
```

```
lib/
    lib.june
    mod1/
        mod1.june sub_mod1.june
    mod2.june
```



##  Top-level code

June source files may begin execution in an explicit `main` function. You may also use top-level code, instead.

For example, to print hello world, you can create a "hello.june":

```
println(c"hello world")
```

# Memory Safety

##  Lifetime checker :mag_right: 

Lifetimes are a fundamental concept of June. :muscle: 

In June, lifetimes are associated with groups of related allocations. For example, a linked list begins with a pointer pointing to the start of the list, and then follows with nodes pointing to each successive node. All nodes in this linked list have the same lifetime, denoted by the lifetime of the head pointer.

As groups of related allocations share a lifetime, the compiler will infer how long this group needs to live, and once the group has finished it will be automatically deleted and the memory will be reclaimed.

The lifetime checker also infers the lifetime of allocations in your code. This is a modular inference. Inference will infer the lifetime of an allocation to be in one of three groups:

* **Local**: this allocation does not escape this function
* **Param(name)**: This allocation escapes via the parameter called 'name'
* **Return**: This allocation escapes as the return of the function

## Constructing and inferring lifetimes

Constructing a new value using `new` will construct and infer the lifetime of the value being constructed.

```
let x = new Foo()
```

## Enforcing lifetimes

You can use the keyword `local` instead of `new` to enforce that the given allocation is only ever used locally and does not escape the current function.

For example:

```
let x = local Foo()
```

## Lifetime groups

Because we track the lifetime of groups of related allocations, we're also able to use a custom allocator to allocate these related items beside each other in memory, allowing for better cache locality.

## Owned pointers

Owned pointers can be created using the `owned` keyword. This will check that the abstraction being instantiated is fully-encapsulated. If it is, the compiler will create a single-owner pointer.

```
let x = new owned Foo()
```

## ABI

June uses a C ABI for laying out both struct and enum.

## Safe memory recycling (not yet implemented :construction:)

If an allocation is no longer needed but the group of allocations is still alive, you can opt to track the number of live aliases of a pointer and recycle that pointer if it is the sole alias of that memory.

##  C interop

There are a few features in June to aid with interop with C.

Here's an example of these features:

```
extern type FILE;

extern "C" fun fopen(filename: c_string, mode: c_string) -> FILE?
extern "C" fun fclose(file: FILE?) -> c_int;
extern "C" fun fgetc(file: FILE?) -> c_int;

unsafe {
    let file = fopen(c"tests/test_data/alphabet.txt", c"rb");
    if file != none {
        let x = fgetc(file)
        println(x as c_char)
    }
    fclose(file)
}

```
