#include "open64_target_buffer_service.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char events[64];
    size_t count;
    int transfers;
    int fail_transfer;
    int fail_allocate;
    int fail_release;
    int allocations;
    int releases;
} MOCK_PROVIDER;

static void
record(MOCK_PROVIDER *mock, char event)
{
    assert(mock->count + 1 < sizeof(mock->events));
    mock->events[mock->count++] = event;
    mock->events[mock->count] = '\0';
}

static int
allocate(void *context, size_t bytes, size_t alignment,
         OPEN64_TARGET_BUFFER_VIEW *view)
{
    MOCK_PROVIDER *mock = (MOCK_PROVIDER *)context;
    void *address = malloc(bytes);
    assert(alignment <= 4);
    if (address == NULL)
        return 1;
    view->address = address;
    view->bytes = bytes;
    view->tier = OPEN64_TARGET_BUFFER_DEVICE;
    view->device_id = 7;
    ++mock->allocations;
    record(mock, 'A');
    return mock->allocations == mock->fail_allocate ? 1 : 0;
}

static int
transfer(void *context, const OPEN64_TARGET_BUFFER_VIEW *destination,
         const OPEN64_TARGET_BUFFER_VIEW *source, size_t bytes)
{
    MOCK_PROVIDER *mock = (MOCK_PROVIDER *)context;
    ++mock->transfers;
    record(mock, source->tier == OPEN64_TARGET_BUFFER_HOST ? 'H' : 'D');
    if (mock->transfers == mock->fail_transfer)
        return 1;
    memcpy(destination->address, source->address, bytes);
    return 0;
}

static int
validate_import(void *context, const OPEN64_TARGET_BUFFER_VIEW *view,
                size_t bytes, size_t alignment)
{
    MOCK_PROVIDER *mock = (MOCK_PROVIDER *)context;
    assert(view->bytes >= bytes && alignment == 4);
    record(mock, 'I');
    return 0;
}

static int
release(void *context, const OPEN64_TARGET_BUFFER_VIEW *view)
{
    MOCK_PROVIDER *mock = (MOCK_PROVIDER *)context;
    ++mock->releases;
    record(mock, 'R');
    if (mock->releases == mock->fail_release)
        return 1;
    free(view->address);
    return 0;
}

static OPEN64_TARGET_BUFFER_SERVICE
service_for(MOCK_PROVIDER *mock)
{
    OPEN64_TARGET_BUFFER_SERVICE service = {
        OPEN64_TARGET_BUFFER_SERVICE_V1, 1, 7, mock,
        allocate, transfer, validate_import, release
    };
    return service;
}

static OPEN64_TARGET_BUFFER_VIEW
view_for(void *address, size_t bytes, uint32_t tier, uint32_t device_id)
{
    OPEN64_TARGET_BUFFER_VIEW view = {address, bytes, tier, device_id};
    return view;
}

static void
test_host_staging(void)
{
    MOCK_PROVIDER mock = {{0}, 0, 0, 0, 0, 0, 0, 0};
    OPEN64_TARGET_BUFFER_SERVICE service = service_for(&mock);
    float a[4] = {1, 2, 3, 4};
    float b[4] = {5, 6, 7, 8};
    float c[4] = {0, 0, 0, 0};
    OPEN64_TARGET_BUFFER_VIEW av = view_for(a, sizeof(a),
                                             OPEN64_TARGET_BUFFER_HOST, 0);
    OPEN64_TARGET_BUFFER_VIEW bv = view_for(b, sizeof(b),
                                             OPEN64_TARGET_BUFFER_HOST, 0);
    OPEN64_TARGET_BUFFER_VIEW cv = view_for(c, sizeof(c),
                                             OPEN64_TARGET_BUFFER_HOST, 0);
    OPEN64_TARGET_BUFFER_BINDING da = {0}, db = {0}, dc = {0};
    int i;
    assert(open64_target_buffer_bind_input(&service, &av, sizeof(a), 4,
                                            &da) == 0);
    assert(open64_target_buffer_bind_input(&service, &bv, sizeof(b), 4,
                                            &db) == 0);
    assert(open64_target_buffer_bind_output(&service, &cv, sizeof(c), 4,
                                             &dc) == 0);
    for (i = 0; i < 4; ++i)
        ((float *)dc.view.address)[i] =
            ((float *)da.view.address)[i] + ((float *)db.view.address)[i];
    assert(open64_target_buffer_finish_output(&service, &dc, &cv,
                                                sizeof(c)) == 0);
    for (i = 0; i < 4; ++i)
        assert(c[i] == a[i] + b[i]);
    assert(open64_target_buffer_release(&service, &dc) == 0);
    assert(open64_target_buffer_release(&service, &db) == 0);
    assert(open64_target_buffer_release(&service, &da) == 0);
    assert(strcmp(mock.events, "AHAHADRRR") == 0);
    assert(mock.allocations == 3 && mock.transfers == 3 && mock.releases == 3);
    printf("host_binding events=%s allocations=%d transfers=%d releases=%d\n",
           mock.events, mock.allocations, mock.transfers, mock.releases);
}

static void
test_borrowed_device_buffers(void)
{
    MOCK_PROVIDER mock = {{0}, 0, 0, 0, 0, 0, 0, 0};
    OPEN64_TARGET_BUFFER_SERVICE service = service_for(&mock);
    float a[4] = {1, 2, 3, 4};
    float b[4] = {5, 6, 7, 8};
    float c[4] = {0, 0, 0, 0};
    OPEN64_TARGET_BUFFER_VIEW av = view_for(a, sizeof(a),
                                             OPEN64_TARGET_BUFFER_DEVICE, 7);
    OPEN64_TARGET_BUFFER_VIEW bv = view_for(b, sizeof(b),
                                             OPEN64_TARGET_BUFFER_DEVICE, 7);
    OPEN64_TARGET_BUFFER_VIEW cv = view_for(c, sizeof(c),
                                             OPEN64_TARGET_BUFFER_DEVICE, 7);
    OPEN64_TARGET_BUFFER_BINDING da = {0}, db = {0}, dc = {0};
    assert(open64_target_buffer_bind_input(&service, &av, sizeof(a), 4,
                                            &da) == 0);
    assert(open64_target_buffer_bind_input(&service, &bv, sizeof(b), 4,
                                            &db) == 0);
    assert(open64_target_buffer_bind_output(&service, &cv, sizeof(c), 4,
                                             &dc) == 0);
    assert(open64_target_buffer_finish_output(&service, &dc, &cv,
                                                sizeof(c)) == 0);
    assert(open64_target_buffer_release(&service, &dc) == 0);
    assert(open64_target_buffer_release(&service, &db) == 0);
    assert(open64_target_buffer_release(&service, &da) == 0);
    assert(strcmp(mock.events, "III") == 0);
    assert(mock.allocations == 0 && mock.transfers == 0 && mock.releases == 0);
    printf("borrowed_device events=%s allocations=%d transfers=%d releases=%d\n",
           mock.events, mock.allocations, mock.transfers, mock.releases);
}

static void
test_failure_cleanup_and_import_guard(void)
{
    MOCK_PROVIDER mock = {{0}, 0, 0, 1, 0, 0, 0, 0};
    OPEN64_TARGET_BUFFER_SERVICE service = service_for(&mock);
    float a[4] = {1, 2, 3, 4};
    OPEN64_TARGET_BUFFER_VIEW host = view_for(a, sizeof(a),
                                               OPEN64_TARGET_BUFFER_HOST, 0);
    OPEN64_TARGET_BUFFER_VIEW wrong_device = view_for(
        a, sizeof(a), OPEN64_TARGET_BUFFER_DEVICE, 8);
    OPEN64_TARGET_BUFFER_BINDING binding = {0};
    assert(open64_target_buffer_bind_input(&service, &host, sizeof(a), 4,
                                            &binding) != 0);
    assert(binding.view.address == NULL && mock.releases == 1);
    assert(strcmp(mock.events, "AHR") == 0);
    assert(open64_target_buffer_bind_input(&service, &wrong_device,
                                            sizeof(a), 4, &binding) != 0);
    assert(open64_target_buffer_bind_input(&service, &host,
                                            sizeof(a) + 1, 4, &binding) != 0);
    assert(strcmp(mock.events, "AHR") == 0);
    printf("transfer_failure events=%s allocations=%d transfers=%d releases=%d\n",
           mock.events, mock.allocations, mock.transfers, mock.releases);

    MOCK_PROVIDER partial = {{0}, 0, 0, 0, 1, 0, 0, 0};
    OPEN64_TARGET_BUFFER_SERVICE partial_service = service_for(&partial);
    assert(open64_target_buffer_bind_input(&partial_service, &host,
                                            sizeof(a), 4, &binding) != 0);
    assert(binding.view.address == NULL && partial.releases == 1);
    assert(strcmp(partial.events, "AR") == 0);
    printf("allocation_failure events=%s allocations=%d transfers=%d releases=%d\n",
           partial.events, partial.allocations, partial.transfers,
           partial.releases);

    MOCK_PROVIDER retry = {{0}, 0, 0, 0, 1, 1, 0, 0};
    OPEN64_TARGET_BUFFER_SERVICE retry_service = service_for(&retry);
    assert(open64_target_buffer_bind_input(&retry_service, &host,
                                            sizeof(a), 4, &binding) == -3);
    assert(binding.view.address != NULL && binding.runtime_owned == 1);
    assert(open64_target_buffer_release(&retry_service, &binding) == 0);
    assert(binding.view.address == NULL && strcmp(retry.events, "ARR") == 0);
    printf("release_retry events=%s allocations=%d transfers=%d releases=%d\n",
           retry.events, retry.allocations, retry.transfers, retry.releases);
}

int
main(void)
{
    test_host_staging();
    test_borrowed_device_buffers();
    test_failure_cleanup_and_import_guard();
    return 0;
}
