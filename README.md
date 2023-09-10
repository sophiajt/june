# June, a safe, gradual systems language

## Pointer types

June has two fundamental pointer types: shared and owned.

A **shared** pointer type is one where multiple pointers can pointed to the same location. When assigned from one variable to another, a shared pointer *aliases*, creating a new pointer which points to the same location of the original pointer. Shared pointer types are a powerful pointer type, capable of creating arbitrary data structures quickly. Their nature of creating aliases means that they allow for encoding loops, back pointers, and more.

An **owned** pointer is one that has only a single owner at any one time. Owners may be a variable, a struct, or a parameter. Once assigned, an owned pointer will move from the original owner to the new owner. This helps to reason about what a pointer can do and when it can be safely freed.

|type|alias-able|lifetime as a field|safe delete|
|--|--|--|--|
|shared|yes|must match owner|no|
|owned|no|equal or greater than owner|yes|

### Deletion

Mutable, shared pointers can not be safely deleted as they may have aliases that exist elsewhere in memory. These would require an `unsafe { }` block to do the deletion.

This is why we'd recommend (and lint for) safe abstractions where mutable, shared pointers do not leak out of the abstractions. This helps the locus of reasoning of how these pointers are used. While we don't ban their use as they have obvious utility, we do lean programmers towards using them carefully.

Note that aliases can still be created inside of the safe abstraction, these should be easier to audit as a reviewer as you know that the reasoning around these aliases are local to the abstraction.

Safe pointers (owned pointers) could be freed, as could their members that are also owned, once the owned pointer is no longer in scope. This could be done without the need of an `unsafe` keyword, as we know there are no aliases in scope. Any fields that use shared pointers would not be safe to free automatically.
