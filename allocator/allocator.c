#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

const long DEFAULT_PAGE_SIZE = 1024;
const bool DEBUG_ZEROING = true;

struct ResourceCleanup
{
    void *resource;
    void (*cleanup_fun)(long, void *);

    struct ResourceCleanup *next;
};

struct AllocatorPage
{
    void *block;
    void *next_free;
    long amt_free;
};

struct AllocatorLevel
{
    int num_pages;
    struct AllocatorPage **pages;
    struct ResourceCleanup *cleanups;
};

struct Allocator
{
    int num_levels;
    struct AllocatorLevel **levels;
};

void print_allocator_level(struct AllocatorLevel *allocator_level)
{
    printf("  Resource cleanup\n");

    struct ResourceCleanup *current = allocator_level->cleanups;
    while (current != NULL)
    {
        printf("    Cleanup:\n");
        printf("      Pointer: %p\n", current->resource);
        printf("      Callback: %p\n", current->cleanup_fun);
    }

    printf("  Number of pages: %i\n", allocator_level->num_pages);

    for (int i = 0; i < allocator_level->num_pages; ++i)
    {
        printf("    Page: %i\n", i);
        printf("      Block: %p\n", allocator_level->pages[i]->block);
        printf("      Next free: %p\n", allocator_level->pages[i]->next_free);
        printf("      Remaining: %li\n", allocator_level->pages[i]->amt_free);
    }
}

void print_allocator(struct Allocator *allocator)
{
    long index = 0;
    long page = 0;

    printf("--[Allocator]--\n");

    for (int i = 0; i < allocator->num_levels; ++i)
    {
        if (allocator->levels[i] != NULL)
        {
            printf("Allocator level: %i\n", i);
            print_allocator_level(allocator->levels[i]);
        }
    }
}

struct Allocator *create_allocator(int num_default_levels)
{
    struct Allocator *allocator = (struct Allocator *)malloc(sizeof(struct Allocator));

    allocator->levels = (struct AllocatorLevel **)malloc(num_default_levels * sizeof(struct AllocatorLevel *));
    memset(allocator->levels, 0, num_default_levels * sizeof(struct AllocatorLevel *));
    allocator->num_levels = num_default_levels;

    return allocator;
}

void grow_allocator_levels(struct Allocator *allocator, int level)
{
    // keep levels at a multiple of 10 as we grow to cut down on the number
    // of times we do this, plus one because of zero-base
    level += (10 - level % 10) + 1;

    allocator->levels = (struct AllocatorLevel **)realloc(allocator->levels, level * sizeof(struct AllocatorLevel *));
    memset(allocator->levels + allocator->num_levels * sizeof(struct Allocator *), 0, (level - allocator->num_levels) * sizeof(struct AllocatorLevel *));
}

void create_allocator_level(struct Allocator *allocator, int level)
{
    if (allocator->num_levels <= level)
    {
        grow_allocator_levels(allocator, level);
    }

    // don't want to create a level where there already is one
    assert(allocator->levels[level] == NULL);

    allocator->levels[level] = (struct AllocatorLevel *)malloc(sizeof(struct AllocatorLevel));
    allocator->levels[level]->num_pages = 0;
    allocator->levels[level]->pages = NULL;
    allocator->levels[level]->cleanups = NULL;
}

void add_resource_cleanup(struct Allocator *allocator, int level, void *resource, void (*cleanup_fun)(long, void *))
{
    if (allocator->num_levels <= level)
    {
        grow_allocator_levels(allocator, level);
    }

    if (allocator->levels[level] == NULL)
    {
        create_allocator_level(allocator, level);
    }

    if (allocator->levels[level]->cleanups == NULL)
    {
        allocator->levels[level]->cleanups = (struct ResourceCleanup *)malloc(sizeof(struct ResourceCleanup));
        allocator->levels[level]->cleanups->cleanup_fun = cleanup_fun;
        allocator->levels[level]->cleanups->resource = resource;
        allocator->levels[level]->cleanups->next = NULL;
    }
    else
    {
        // struct ResourceCleanup *current = allocator->levels[level]->cleanups;

        // while (current->next != NULL)
        // {
        //     current = current->next;
        // }

        // current->next = (struct ResourceCleanup *)malloc(sizeof(struct ResourceCleanup));
        // current->next->cleanup_fun = cleanup_fun;
        // current->next->resource = resource;
        // current->next->next = NULL;

        struct ResourceCleanup *new_node = (struct ResourceCleanup *)malloc(sizeof(struct ResourceCleanup));
        new_node->cleanup_fun = cleanup_fun;
        new_node->resource = resource;
        new_node->next = allocator->levels[level]->cleanups;
        allocator->levels[level]->cleanups = new_node;
    }
}

struct AllocatorPage *create_allocator_page()
{
    struct AllocatorPage *output = (struct AllocatorPage *)malloc(sizeof(struct AllocatorPage));

    output->block = NULL;
    output->next_free = NULL;
    output->amt_free = 0;

    return output;
}

long calculate_default_size(long size)
{
    if (size < DEFAULT_PAGE_SIZE)
    {
        return DEFAULT_PAGE_SIZE;
    }
    else if (size > (4 * 1024 * 1024))
    {
        // large allocation, just fit the allocation instead of having extra
        return size;
    }
    else
    {
        // a note on alignment: in theory, since we're using structs/unions
        // I *hope* the C requirements will keep us memory aligned even
        // though we're doing our own allocations by requesting aligned sizes.
        return size + 1024;
    }
}

void *allocate_on_allocator_level(struct AllocatorLevel *allocator_level, long size)
{
    if (allocator_level->num_pages == 0)
    {
        allocator_level->pages = (struct AllocatorPage **)malloc(sizeof(struct AllocatorPage *));
        allocator_level->pages[0] = create_allocator_page();

        allocator_level->num_pages = 1;
    }

    for (int i = 0; i < allocator_level->num_pages; ++i)
    {
        if (allocator_level->pages[i]->block == NULL)
        {
            // We have a page that hasn't been used yet
            long default_size = calculate_default_size(size);
            allocator_level->pages[i]->block = malloc(default_size);
            void *output = allocator_level->pages[i]->block;
            allocator_level->pages[i]->next_free = allocator_level->pages[i]->block + size;
            allocator_level->pages[i]->amt_free = default_size - size;

            return output;
        }
        else if (allocator_level->pages[i]->amt_free >= size)
        {
            void *output = allocator_level->pages[i]->next_free;
            allocator_level->pages[i]->next_free += size;
            allocator_level->pages[i]->amt_free -= size;

            return output;
        }
    }

    // We can't find any pages that can hold what we have, so let's make a new one
    allocator_level->pages = realloc(allocator_level->pages, sizeof(struct AllocatorPage *) * (allocator_level->num_pages + 1));
    allocator_level->pages[allocator_level->num_pages] = create_allocator_page();

    long default_size = calculate_default_size(size);
    allocator_level->pages[allocator_level->num_pages]->block = malloc(default_size);
    void *output = allocator_level->pages[allocator_level->num_pages]->block;
    allocator_level->pages[allocator_level->num_pages]->next_free = allocator_level->pages[allocator_level->num_pages]->block + size;
    allocator_level->pages[allocator_level->num_pages]->amt_free = default_size - size;

    ++allocator_level->num_pages;

    return output;
}

void *allocate_resizeable_page_on_allocator_level(struct Allocator *allocator, int level, long size)
{
    if (level >= allocator->num_levels)
    {
        grow_allocator_levels(allocator, level);
    }

    if (allocator->levels[level] == NULL)
    {
        create_allocator_level(allocator, level);
    }

    struct AllocatorLevel *allocator_level = allocator->levels[level];
    // Each resizeable page is isolated, so let's make a new one
    allocator_level->pages = realloc(allocator_level->pages, sizeof(struct AllocatorPage *) * (allocator_level->num_pages + 1));
    allocator_level->pages[allocator_level->num_pages] = create_allocator_page();

    allocator_level->pages[allocator_level->num_pages]->block = malloc(size);
    void *output = allocator_level->pages[allocator_level->num_pages]->block;
    allocator_level->pages[allocator_level->num_pages]->next_free = allocator_level->pages[allocator_level->num_pages]->block + size;
    allocator_level->pages[allocator_level->num_pages]->amt_free = 0;

    ++allocator_level->num_pages;

    return output;
}

void *resize_page_on_allocator_level(struct Allocator *allocator, int level, void *origin, long new_size)
{
    struct AllocatorLevel *allocator_level = allocator->levels[level];

    for (int i = 0; i < allocator_level->num_pages; ++i)
    {
        if (allocator_level->pages[i]->block == origin)
        {
            allocator_level->pages[i]->block = realloc(allocator_level->pages[i]->block, new_size);
            allocator_level->pages[i]->next_free = allocator_level->pages[i]->block + new_size;

            return allocator_level->pages[i]->block;
        }
    }

    printf("INTERNAL ERROR: could not find origin: %p\n", origin);
    return NULL;
}

void free_allocator_level(struct Allocator *allocator, int level)
{
    if (allocator->levels[level] == NULL)
    {
        return;
    }

    struct AllocatorLevel *allocator_level = allocator->levels[level];

    // 1) Start with the resource cleanup

    while (allocator_level->cleanups != NULL)
    {
        allocator_level->cleanups->cleanup_fun(/*unused*/ 0, allocator_level->cleanups->resource);
        struct ResourceCleanup *old = allocator_level->cleanups;
        allocator_level->cleanups = allocator_level->cleanups->next;

        free(old);
    }

    // 2) Then, do memory cleanup

    if (allocator_level->num_pages == 0)
    {
        return;
    }

    // Only free the first page if it's too big
    if ((allocator_level->pages[0]->next_free - allocator_level->pages[0]->block + allocator_level->pages[0]->amt_free) > DEFAULT_PAGE_SIZE)
    {
        if (DEBUG_ZEROING)
        {
            memset(allocator_level->pages[0]->block, 0, allocator_level->pages[0]->next_free - allocator_level->pages[0]->block);
            allocator_level->pages[0]->amt_free += allocator_level->pages[0]->next_free - allocator_level->pages[0]->block;
            allocator_level->pages[0]->next_free = allocator_level->pages[0]->block;
        }
        else
        {
            free(allocator_level->pages[0]->block);
            allocator_level->pages[0]->block = NULL;
            allocator_level->pages[0]->next_free = NULL;
            allocator_level->pages[0]->amt_free = 0;
        }
    }
    else
    {
        // Reuse the previous block if it's small enough
        if (DEBUG_ZEROING)
        {
            memset(allocator_level->pages[0]->block, 0, allocator_level->pages[0]->next_free - allocator_level->pages[0]->block);
        }
        allocator_level->pages[0]->amt_free += (allocator_level->pages[0]->next_free - allocator_level->pages[0]->block);
        allocator_level->pages[0]->next_free = allocator_level->pages[0]->block;
    }

    for (int i = 1; i < allocator_level->num_pages; ++i)
    {
        if (DEBUG_ZEROING)
        {
            memset(allocator_level->pages[i]->block, 0, allocator_level->pages[i]->next_free - allocator_level->pages[i]->block);
            allocator_level->pages[i]->amt_free += allocator_level->pages[i]->next_free - allocator_level->pages[i]->block;
            allocator_level->pages[i]->next_free = allocator_level->pages[i]->block;
        }
        else
        {
            free(allocator_level->pages[i]->block);
            allocator_level->pages[i]->block = NULL;
            allocator_level->pages[i]->next_free = NULL;
            allocator_level->pages[i]->amt_free = 0;
        }
    }
}

void *allocate(struct Allocator *allocator, long size, int level)
{
    if (level >= allocator->num_levels)
    {
        grow_allocator_levels(allocator, level);
    }

    if (allocator->levels[level] == NULL)
    {
        create_allocator_level(allocator, level);
    }

    return allocate_on_allocator_level(allocator->levels[level], size);
}
