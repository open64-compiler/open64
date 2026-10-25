#include "open64_target_buffer_service.h"

#include <stdint.h>
#include <string.h>

static int
valid_alignment(size_t alignment)
{
    return alignment != 0 && (alignment & (alignment - 1)) == 0;
}

static int
valid_view(const OPEN64_TARGET_BUFFER_VIEW *view, size_t bytes,
           size_t alignment)
{
    return view != NULL && view->address != NULL && view->bytes >= bytes &&
           (view->tier == OPEN64_TARGET_BUFFER_HOST ||
            view->tier == OPEN64_TARGET_BUFFER_DEVICE) &&
           ((uintptr_t)view->address & (alignment - 1)) == 0;
}

static int
valid_service(const OPEN64_TARGET_BUFFER_SERVICE *service)
{
    return service != NULL &&
           service->abi_version == OPEN64_TARGET_BUFFER_SERVICE_V1 &&
           service->target_id != 0 && service->allocate != NULL &&
           service->transfer != NULL && service->validate_import != NULL &&
           service->release != NULL;
}

static int
bind_buffer(const OPEN64_TARGET_BUFFER_SERVICE *service,
            const OPEN64_TARGET_BUFFER_VIEW *caller, size_t bytes,
            size_t alignment, int input,
            OPEN64_TARGET_BUFFER_BINDING *binding)
{
    if (!valid_service(service) || !valid_alignment(alignment) ||
        bytes == 0 || binding == NULL || binding->view.address != NULL ||
        !valid_view(caller, bytes, alignment))
        return -1;

    if (caller->tier == OPEN64_TARGET_BUFFER_DEVICE) {
        if (caller->device_id != service->device_id ||
            service->validate_import(service->context, caller, bytes,
                                     alignment) != 0)
            return -2;
        binding->view = *caller;
        binding->runtime_owned = 0;
        return 0;
    }

    OPEN64_TARGET_BUFFER_VIEW allocated = {NULL, 0, 0, 0};
    if (service->allocate(service->context, bytes, alignment,
                          &allocated) != 0) {
        if (allocated.address != NULL) {
            binding->view = allocated;
            binding->runtime_owned = 1;
            if (open64_target_buffer_release(service, binding) != 0)
                return -3;
        }
        return -2;
    }
    binding->view = allocated;
    binding->runtime_owned = 1;
    if (!valid_view(&binding->view, bytes, alignment) ||
        binding->view.tier != OPEN64_TARGET_BUFFER_DEVICE ||
        binding->view.device_id != service->device_id ||
        (input && service->transfer(service->context, &binding->view,
                                    caller, bytes) != 0)) {
        if (open64_target_buffer_release(service, binding) != 0)
            return -3;
        return -2;
    }
    return 0;
}

int
open64_target_buffer_bind_input
    (const OPEN64_TARGET_BUFFER_SERVICE *service,
     const OPEN64_TARGET_BUFFER_VIEW *source, size_t bytes,
     size_t alignment, OPEN64_TARGET_BUFFER_BINDING *binding)
{
    return bind_buffer(service, source, bytes, alignment, 1, binding);
}

int
open64_target_buffer_bind_output
    (const OPEN64_TARGET_BUFFER_SERVICE *service,
     const OPEN64_TARGET_BUFFER_VIEW *destination, size_t bytes,
     size_t alignment, OPEN64_TARGET_BUFFER_BINDING *binding)
{
    return bind_buffer(service, destination, bytes, alignment, 0, binding);
}

int
open64_target_buffer_finish_output
    (const OPEN64_TARGET_BUFFER_SERVICE *service,
     const OPEN64_TARGET_BUFFER_BINDING *binding,
     const OPEN64_TARGET_BUFFER_VIEW *destination, size_t bytes)
{
    if (!valid_service(service) || binding == NULL || bytes == 0 ||
        binding->view.tier != OPEN64_TARGET_BUFFER_DEVICE ||
        binding->view.device_id != service->device_id ||
        !valid_view(&binding->view, bytes, 1) ||
        !valid_view(destination, bytes, 1))
        return -1;
    if (destination->tier == OPEN64_TARGET_BUFFER_DEVICE)
        return destination->device_id == service->device_id &&
               binding->view.address == destination->address ? 0 : -1;
    return service->transfer(service->context, destination, &binding->view,
                             bytes) == 0 ? 0 : -2;
}

int
open64_target_buffer_release
    (const OPEN64_TARGET_BUFFER_SERVICE *service,
     OPEN64_TARGET_BUFFER_BINDING *binding)
{
    if (!valid_service(service) || binding == NULL)
        return -1;
    if (binding->view.address == NULL)
        return 0;
    if (binding->runtime_owned &&
        service->release(service->context, &binding->view) != 0)
        return -2;
    memset(binding, 0, sizeof(*binding));
    return 0;
}
