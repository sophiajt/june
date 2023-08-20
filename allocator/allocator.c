#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

const int DEFAULT_PAGE_SIZE = 1024;
#define max_of(x, y) (x) >= (y) ? (x) : (y)

struct Allocator
{
    void *contents;
    long capacity;
    struct Allocator *next;
};

void print_allocator(struct Allocator *allocator)
{
    long index = 0;
    long page = 0;

    do
    {
        long current_offset = 0;

        // FIXME: we should have the allocator capacity here?
        while (current_offset < allocator->capacity)
        {
            long id = *(long *)(allocator->contents + current_offset);
            long size = *(long *)(allocator->contents + current_offset + sizeof(id));
            void *contents = (allocator->contents + current_offset + sizeof(id) + sizeof(size));

            printf("-------\n");
            printf("page: %li\n", page);
            printf("index: %li\n", index);
            if (id == 0)
            {
                printf("FREE MEM\n");
            }
            else
            {
                printf("id: %li\n", id);
            }
            printf("size: %li\n", size);
            printf("contents: %p\n", contents);
            printf("-------\n");

            current_offset += sizeof(id) + sizeof(size) + size;
            ++index;
        }

        ++page;

        allocator = allocator->next;
    } while (allocator != NULL);
}

struct Allocator *create_allocator(long size)
{
    struct Allocator *allocator = (struct Allocator *)malloc(sizeof(struct Allocator));

    allocator->contents = malloc(size);
    allocator->capacity = size;

    // ID: 0 means the unused identifier
    *((long *)allocator->contents) = 0;
    *((long *)(allocator->contents + sizeof(long))) = size - sizeof(long) - sizeof(long);

    // printf("new allocator page of size: %li\n", size);

    allocator->next = NULL;

    return allocator;
}

void delete_allocator(struct Allocator *allocator)
{
    free(allocator->contents);
    free(allocator);
}

void *allocate(struct Allocator *allocator, long size, long id)
{
    if (size % 8 != 0)
    {
        // align to 64-bit
        size = size + (8 - (size % 8));
    }

    struct Allocator *current = allocator;

    while (1)
    {
        long offset = 0;

        while (offset < current->capacity)
        {
            long current_id = *(long *)(current->contents + offset);
            long current_size = *(long *)(current->contents + offset + sizeof(id));

            if ((current_id == 0) && (current_size >= size))
            {
                // We have found sufficient space
                void *output = current->contents + offset + sizeof(id) + sizeof(size);
                *(long *)(current->contents + offset) = id;
                *(long *)(current->contents + offset + sizeof(id)) = size;

                if (current_size >= size + sizeof(id) + sizeof(size))
                {
                    // Mark the remainder
                    *(long *)(current->contents + offset + size + sizeof(id) + sizeof(size)) = 0;
                    *(long *)(current->contents + offset + size + sizeof(id) + sizeof(size) + sizeof(id)) = current_size - (size + sizeof(id) + sizeof(size));
                }

                return output;
            }
            else
            {
                offset += *(long *)(current->contents + offset + sizeof(id)) + sizeof(id) + sizeof(size);
            }
        }

        if (current->next != NULL)
        {
            current = current->next;
        }
        else
        {
            break;
        }
    }

    // If we get here, we need to create a new page
    long page_size = max_of(DEFAULT_PAGE_SIZE, size + sizeof(id) + sizeof(size));
    struct Allocator *new_allocator = create_allocator(page_size);
    current->next = new_allocator;

    current = current->next;

    *(long *)(current->contents) = id;
    *(long *)(current->contents + sizeof(id)) = size;
    void *output = current->contents + sizeof(id) + sizeof(size);

    if (page_size == DEFAULT_PAGE_SIZE)
    {
        *(long *)(current->contents + sizeof(id) + sizeof(size) + size) = 0;
        *(long *)(current->contents + sizeof(id) + sizeof(size) + size + sizeof(id)) = DEFAULT_PAGE_SIZE - sizeof(id) - sizeof(size) - size - sizeof(id) - sizeof(size);
    }

    return (current->contents + sizeof(id) + sizeof(size));
}

void deallocate(struct Allocator *allocator, long id)
{
    struct Allocator *current = allocator;

    while (current != NULL)
    {
        long offset = 0;
        long prev_empty_offset = 0;

        while (offset < current->capacity)
        {
            long current_id = *(long *)(current->contents + offset);
            long current_size = *(long *)(current->contents + offset + sizeof(id));

            if (current_id == id)
            {
                *(long *)(current->contents + offset) = 0;
                // For debug purposes, totally clear the memory
                memset((current->contents + offset + sizeof(id) + sizeof(current_size)), 0, current_size);

                if (prev_empty_offset != offset)
                {
                    *(long *)(current->contents + prev_empty_offset + sizeof(id)) += current_size + sizeof(id) + sizeof(current_size);
                }
                offset += *(long *)(current->contents + offset + sizeof(id)) + sizeof(id) + sizeof(current_size);
            }
            else if (current_id == 0)
            {
                // Remember this, because we might compact the empty space
                if (prev_empty_offset != offset)
                {
                    *(long *)(current->contents + prev_empty_offset + sizeof(id)) += current_size + sizeof(id) + sizeof(current_size);
                }

                prev_empty_offset = offset;
                offset += *(long *)(current->contents + offset + sizeof(id)) + sizeof(id) + sizeof(current_size);
            }
            else
            {
                offset += *(long *)(current->contents + offset + sizeof(id)) + sizeof(id) + sizeof(current_size);
                prev_empty_offset = offset;
            }
        }

        current = current->next;
    }

    // Last compaction step, deallocate pages which are no longer needed
    current = allocator;
    struct Allocator *next = current->next;

    while (next != NULL)
    {
        long current_id = *(long *)(next->contents);
        long current_size = *(long *)(next->contents + sizeof(id));

        if ((current_id == 0) && (current_size == (next->capacity - sizeof(current_id) - sizeof(current_size))))
        {
            // The whole page is empty
            current->next = next->next;
            delete_allocator(next);
            next = current->next;
        }
        else
        {
            current = next;
            next = current->next;
        }
    }
}

// ===================
// ==== USER CODE ====
// ===================
