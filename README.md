# June, a safe, gradual systems language

TODO:

- [x] Extend testing system to ensure proper allocation lifetimes are followed


NOTES:

Mutable, shared pointers can not be safely deleted as they may have aliases that exist elsewhere in memory. These would require an `unsafe { }` block to do the deletion.

This is why we'd recommend (and lint for) safe abstractions where mutable, shared pointers do not leak out of the abstractions. This helps the locus of reasoning of how these pointers are used. While we don't ban their use as they have obvious utility, we do lean programmers towards using them carefully.

Note that aliases can still be created inside of the safe abstraction, these should be easier to audit as a reviewer as you know that the reasoning around these aliases are local to the abstraction.

Safe pointers (owned pointers) could be freed, as could their members that are also owned, once the owned pointer is no longer in scope. This could be done without the need of an `unsafe` keyword, as we know there are no aliases in scope. Any fields that use shared pointers would not be safe to free automatically.

## Pointer types

|type|alias-able|lifetimes|safe delete|
|--|--|--|--|
|shared|yes|must match|no|
|owned|no|equal or greater|yes|
