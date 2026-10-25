#ifndef OPEN64_TARGET_BUFFER_SERVICE_H
#define OPEN64_TARGET_BUFFER_SERVICE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define OPEN64_TARGET_BUFFER_SERVICE_V1 1

enum {
    OPEN64_TARGET_BUFFER_HOST = 1,
    OPEN64_TARGET_BUFFER_DEVICE = 2
};

enum {
    OPEN64_TARGET_NVIDIA = 1
};

typedef struct {
    void *address;
    size_t bytes;
    uint32_t tier;
    uint32_t device_id;
} OPEN64_TARGET_BUFFER_VIEW;

typedef struct {
    OPEN64_TARGET_BUFFER_VIEW view;
    uint32_t runtime_owned;
} OPEN64_TARGET_BUFFER_BINDING;

/* The provider operations are synchronous in V1. Completion must precede
 * finish_output or release; async execution requires a versioned extension. */
typedef struct {
    uint32_t abi_version;
    uint32_t target_id;
    uint32_t device_id;
    void *context;
    int (*allocate)(void *, size_t, size_t, OPEN64_TARGET_BUFFER_VIEW *);
    int (*transfer)(void *, const OPEN64_TARGET_BUFFER_VIEW *,
                    const OPEN64_TARGET_BUFFER_VIEW *, size_t);
    int (*validate_import)(void *, const OPEN64_TARGET_BUFFER_VIEW *,
                           size_t, size_t);
    int (*release)(void *, const OPEN64_TARGET_BUFFER_VIEW *);
} OPEN64_TARGET_BUFFER_SERVICE;

/* Bindings must be zero-initialized. A failed release leaves an owned
 * binding intact so the caller can retry after provider recovery. */
int open64_target_buffer_bind_input
    (const OPEN64_TARGET_BUFFER_SERVICE *, const OPEN64_TARGET_BUFFER_VIEW *,
     size_t, size_t, OPEN64_TARGET_BUFFER_BINDING *);
int open64_target_buffer_bind_output
    (const OPEN64_TARGET_BUFFER_SERVICE *, const OPEN64_TARGET_BUFFER_VIEW *,
     size_t, size_t, OPEN64_TARGET_BUFFER_BINDING *);
int open64_target_buffer_finish_output
    (const OPEN64_TARGET_BUFFER_SERVICE *, const OPEN64_TARGET_BUFFER_BINDING *,
     const OPEN64_TARGET_BUFFER_VIEW *, size_t);
int open64_target_buffer_release
    (const OPEN64_TARGET_BUFFER_SERVICE *, OPEN64_TARGET_BUFFER_BINDING *);

#ifdef __cplusplus
}
#endif

#endif
