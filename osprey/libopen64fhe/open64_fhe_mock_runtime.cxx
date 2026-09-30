/*
 * Copyright (C) 2026 Open64 Project
 *
 * Deterministic provider used to certify the Open64 FHE runtime ABI.
 */

#include "open64_fhe_mock_test.h"
#include "open64_fhe_mock_sha256.h"

#include <array>
#include <mutex>
#include <new>
#include <set>
#include <vector>
#include <string.h>

enum OPEN64_FHE_MOCK_TOKEN_STATE {
  OPEN64_FHE_MOCK_TOKEN_LIVE = 1,
  OPEN64_FHE_MOCK_TOKEN_CLAIMED = 2,
  OPEN64_FHE_MOCK_TOKEN_CONSUMED = 3
};

struct open64_fhe_host_bootstrap_v1_s {
  uint64_t generation;
  uint32_t state;
  bool registry_sealed;
  uint8_t deployment_identity_sha256[32];
  std::set<std::array<uint8_t, 32> > trusted_envelopes;
};

struct open64_fhe_launcher_capability_v1_s {
  uint64_t generation;
  uint32_t state;
  open64_fhe_host_bootstrap_v1_t host;
};

struct open64_fhe_broker_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t child_count;
  uint8_t deployment_identity_sha256[32];
  std::set<std::array<uint8_t, 32> > trusted_envelopes;
};

struct open64_fhe_context_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t child_count;
  open64_fhe_broker_v1_t broker;
  uint8_t execution_profile_sha256[32];
  uint8_t provider_identity_sha256[32];
  uint8_t provider_manifest_sha256[32];
  uint8_t config_identity_sha256[32];
};

static uint64_t open64_fhe_mock_next_generation = 1;
static std::mutex open64_fhe_mock_mutex;
static std::set<open64_fhe_host_bootstrap_v1_t> open64_fhe_mock_hosts;
static std::set<open64_fhe_launcher_capability_v1_t>
    open64_fhe_mock_capabilities;
static std::set<open64_fhe_broker_v1_t> open64_fhe_mock_brokers;
static std::set<open64_fhe_context_v1_t> open64_fhe_mock_contexts;

static const uint8_t open64_fhe_mock_envelope_magic[8] = {
  0x4f, 0x36, 0x34, 0x46, 0x48, 0x45, 0x31, 0x00
};

struct OPEN64_FHE_MOCK_ENVELOPE_VIEW {
  uint32_t kind;
  uint32_t key_class_mask;
  const uint8_t *provider_identity_sha256;
  const uint8_t *config_identity_sha256;
  const uint8_t *payload;
  uint64_t payload_size;
  std::array<uint8_t, 32> envelope_sha256;
};

static uint32_t
open64_fhe_mock_load_le32(const uint8_t *bytes)
{
  return uint32_t(bytes[0]) | (uint32_t(bytes[1]) << 8) |
         (uint32_t(bytes[2]) << 16) | (uint32_t(bytes[3]) << 24);
}

static uint64_t
open64_fhe_mock_load_le64(const uint8_t *bytes)
{
  uint64_t value = 0;
  for (uint32_t i = 0; i < 8; ++i)
    value |= uint64_t(bytes[i]) << (i * 8);
  return value;
}

static void
open64_fhe_mock_store_le32(uint8_t *bytes, uint32_t value)
{
  for (uint32_t i = 0; i < 4; ++i)
    bytes[i] = uint8_t(value >> (i * 8));
}

static void
open64_fhe_mock_store_le64(uint8_t *bytes, uint64_t value)
{
  for (uint32_t i = 0; i < 8; ++i)
    bytes[i] = uint8_t(value >> (i * 8));
}

static std::array<uint8_t, 32>
open64_fhe_mock_digest(const void *bytes, size_t size)
{
  std::array<uint8_t, 32> digest;
  open64_fhe_mock_sha256(bytes, size, digest.data());
  return digest;
}

static bool
open64_fhe_mock_digest_equal(const uint8_t *left, const uint8_t *right)
{
  uint8_t difference = 0;
  for (uint32_t i = 0; i < 32; ++i)
    difference |= left[i] ^ right[i];
  return difference == 0;
}

static open64_fhe_status_v1
open64_fhe_mock_read_envelope(const void *envelope,
                              uint64_t envelope_size,
                              OPEN64_FHE_MOCK_ENVELOPE_VIEW *view)
{
  if (envelope == NULL || view == NULL ||
      envelope_size < OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 ||
      envelope_size > size_t(-1))
    return OPEN64_FHE_STATUS_ENVELOPE_INVALID;
  const uint8_t *bytes = static_cast<const uint8_t *>(envelope);
  if (memcmp(bytes, open64_fhe_mock_envelope_magic,
             sizeof(open64_fhe_mock_envelope_magic)) != 0 ||
      open64_fhe_mock_load_le32(bytes + 8) != 1 ||
      open64_fhe_mock_load_le32(bytes + 12) !=
          OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1)
    return OPEN64_FHE_STATUS_ENVELOPE_INVALID;
  uint64_t payload_size = open64_fhe_mock_load_le64(bytes + 88);
  if (payload_size != envelope_size - OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1)
    return OPEN64_FHE_STATUS_ENVELOPE_INVALID;
  std::array<uint8_t, 32> payload_digest = open64_fhe_mock_digest(
      bytes + OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1, size_t(payload_size));
  if (!open64_fhe_mock_digest_equal(payload_digest.data(), bytes + 96))
    return OPEN64_FHE_STATUS_INTEGRITY_ERROR;
  std::vector<uint8_t> authenticated(bytes, bytes + envelope_size);
  memset(authenticated.data() + 128, 0, 32);
  std::array<uint8_t, 32> envelope_digest = open64_fhe_mock_digest(
      authenticated.data(), authenticated.size());
  if (!open64_fhe_mock_digest_equal(envelope_digest.data(), bytes + 128))
    return OPEN64_FHE_STATUS_INTEGRITY_ERROR;
  view->kind = open64_fhe_mock_load_le32(bytes + 16);
  view->key_class_mask = open64_fhe_mock_load_le32(bytes + 20);
  view->provider_identity_sha256 = bytes + 24;
  view->config_identity_sha256 = bytes + 56;
  view->payload = bytes + OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1;
  view->payload_size = payload_size;
  view->envelope_sha256 = envelope_digest;
  return OPEN64_FHE_STATUS_OK;
}

static bool
open64_fhe_mock_digest_nonzero(const uint8_t digest[32])
{
  if (digest == NULL)
    return false;
  for (uint32_t i = 0; i < 32; ++i) {
    if (digest[i] != 0)
      return true;
  }
  return false;
}

static bool
open64_fhe_mock_host_live(open64_fhe_host_bootstrap_v1_t host)
{
  return host != NULL && open64_fhe_mock_hosts.count(host) == 1 &&
         host->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_capability_known(
    open64_fhe_launcher_capability_v1_t capability)
{
  return capability != NULL &&
         open64_fhe_mock_capabilities.count(capability) == 1;
}

static bool
open64_fhe_mock_broker_live(open64_fhe_broker_v1_t broker)
{
  return broker != NULL && open64_fhe_mock_brokers.count(broker) == 1 &&
         broker->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_context_live(open64_fhe_context_v1_t context)
{
  return context != NULL && open64_fhe_mock_contexts.count(context) == 1 &&
         context->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

extern "C" open64_fhe_status_v1
open64_fhe_mock_host_bootstrap_create_v1(
    const uint8_t deployment_identity_sha256[32],
    open64_fhe_host_bootstrap_v1_t *out_bootstrap)
{
  if (out_bootstrap == NULL || *out_bootstrap != NULL ||
      !open64_fhe_mock_digest_nonzero(deployment_identity_sha256))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    open64_fhe_host_bootstrap_v1_t host =
        new (std::nothrow) open64_fhe_host_bootstrap_v1_s;
    if (host == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    host->generation = open64_fhe_mock_next_generation++;
    host->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    host->registry_sealed = false;
    memcpy(host->deployment_identity_sha256,
           deployment_identity_sha256, 32);
    try {
      open64_fhe_mock_hosts.insert(host);
    } catch (...) {
      delete host;
      throw;
    }
    *out_bootstrap = host;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_mock_seal_envelope_v1(
    uint32_t kind,
    uint32_t key_class_mask,
    const uint8_t provider_identity_sha256[32],
    const uint8_t config_identity_sha256[32],
    const void *payload,
    uint64_t payload_size,
    void *envelope_buffer,
    uint64_t envelope_capacity,
    uint64_t *out_envelope_size)
{
  if (provider_identity_sha256 == NULL || config_identity_sha256 == NULL ||
      (payload == NULL && payload_size != 0) || out_envelope_size == NULL ||
      payload_size > uint64_t(size_t(-1)) - OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  uint64_t required = OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 + payload_size;
  *out_envelope_size = required;
  if (envelope_buffer == NULL || envelope_capacity < required)
    return OPEN64_FHE_STATUS_BUFFER_TOO_SMALL;
  uint8_t *bytes = static_cast<uint8_t *>(envelope_buffer);
  memset(bytes, 0, size_t(required));
  memcpy(bytes, open64_fhe_mock_envelope_magic,
         sizeof(open64_fhe_mock_envelope_magic));
  open64_fhe_mock_store_le32(bytes + 8, 1);
  open64_fhe_mock_store_le32(bytes + 12,
                             OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1);
  open64_fhe_mock_store_le32(bytes + 16, kind);
  open64_fhe_mock_store_le32(bytes + 20, key_class_mask);
  memcpy(bytes + 24, provider_identity_sha256, 32);
  memcpy(bytes + 56, config_identity_sha256, 32);
  open64_fhe_mock_store_le64(bytes + 88, payload_size);
  if (payload_size != 0)
    memcpy(bytes + OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1,
           payload, size_t(payload_size));
  open64_fhe_mock_sha256(
      bytes + OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1,
      size_t(payload_size), bytes + 96);
  open64_fhe_mock_sha256(bytes, size_t(required), bytes + 128);
  return OPEN64_FHE_STATUS_OK;
}

extern "C" open64_fhe_status_v1
open64_fhe_mock_host_trust_envelope_v1(
    open64_fhe_host_bootstrap_v1_t bootstrap,
    const void *envelope,
    uint64_t envelope_size)
{
  try {
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope(
        envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_host_live(bootstrap))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if (bootstrap->registry_sealed)
      return OPEN64_FHE_STATUS_BUSY;
    bootstrap->trusted_envelopes.insert(view.envelope_sha256);
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_mock_host_bootstrap_destroy_v1(
    open64_fhe_host_bootstrap_v1_t *bootstrap)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (bootstrap == NULL || !open64_fhe_mock_host_live(*bootstrap))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    (*bootstrap)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    memset((*bootstrap)->deployment_identity_sha256, 0, 32);
    (*bootstrap)->trusted_envelopes.clear();
    *bootstrap = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_launcher_capability_acquire_v1(
    open64_fhe_host_bootstrap_v1_t host_bootstrap,
    open64_fhe_launcher_capability_v1_t *out_capability)
{
  if (out_capability == NULL || *out_capability != NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_host_live(host_bootstrap))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    host_bootstrap->registry_sealed = true;
    open64_fhe_launcher_capability_v1_t capability =
        new (std::nothrow) open64_fhe_launcher_capability_v1_s;
    if (capability == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    capability->generation = open64_fhe_mock_next_generation++;
    capability->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    capability->host = host_bootstrap;
    try {
      open64_fhe_mock_capabilities.insert(capability);
    } catch (...) {
      delete capability;
      throw;
    }
    *out_capability = capability;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_launcher_capability_release_v1(
    open64_fhe_launcher_capability_v1_t *capability)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (capability == NULL || !open64_fhe_mock_capability_known(*capability))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*capability)->state == OPEN64_FHE_MOCK_TOKEN_CLAIMED)
      return OPEN64_FHE_STATUS_BUSY;
    if ((*capability)->state != OPEN64_FHE_MOCK_TOKEN_LIVE)
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    (*capability)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    (*capability)->host = NULL;
    *capability = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_broker_create_v1(
    open64_fhe_launcher_capability_v1_t *privileged_launcher,
    const open64_fhe_broker_desc_v1 *desc,
    open64_fhe_broker_v1_t *out_broker)
{
  if (desc == NULL || out_broker == NULL ||
      desc->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      desc->struct_size != sizeof(*desc) || desc->flags != 0 ||
      desc->reserved != 0)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  if (*out_broker != NULL) {
    if (privileged_launcher != NULL &&
        (void *)*out_broker == (void *)*privileged_launcher)
      return OPEN64_FHE_STATUS_ALIAS_FORBIDDEN;
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  }
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (privileged_launcher == NULL ||
        !open64_fhe_mock_capability_known(*privileged_launcher))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    open64_fhe_launcher_capability_v1_t capability = *privileged_launcher;
    if (capability->state == OPEN64_FHE_MOCK_TOKEN_CLAIMED)
      return OPEN64_FHE_STATUS_BUSY;
    if (capability->state != OPEN64_FHE_MOCK_TOKEN_LIVE)
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    open64_fhe_broker_v1_t broker =
        new (std::nothrow) open64_fhe_broker_v1_s;
    if (broker == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    broker->generation = open64_fhe_mock_next_generation++;
    broker->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    broker->child_count = 0;
    memcpy(broker->deployment_identity_sha256,
           capability->host->deployment_identity_sha256, 32);
    try {
      broker->trusted_envelopes = capability->host->trusted_envelopes;
      open64_fhe_mock_brokers.insert(broker);
    } catch (...) {
      delete broker;
      throw;
    }

    capability->state = OPEN64_FHE_MOCK_TOKEN_CLAIMED;
    *privileged_launcher = NULL;
    broker->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    capability->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    capability->host = NULL;
    *out_broker = broker;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_broker_destroy_v1(open64_fhe_broker_v1_t *broker)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (broker == NULL || !open64_fhe_mock_broker_live(*broker))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*broker)->child_count != 0)
      return OPEN64_FHE_STATUS_BUSY;
    (*broker)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    memset((*broker)->deployment_identity_sha256, 0, 32);
    (*broker)->trusted_envelopes.clear();
    *broker = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_context_import_v1(
    open64_fhe_broker_v1_t broker,
    const open64_fhe_context_desc_v1 *desc,
    const void *public_context_envelope,
    uint64_t envelope_size,
    open64_fhe_context_v1_t *out_context)
{
  if (desc == NULL || out_context == NULL || *out_context != NULL ||
      desc->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      desc->struct_size != sizeof(*desc) || desc->flags != 0 ||
      desc->reserved != 0 ||
      !open64_fhe_mock_digest_nonzero(desc->execution_profile_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->provider_identity_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->provider_manifest_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->config_identity_sha256))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_broker_live(broker))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope(
        public_context_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    if (broker->trusted_envelopes.count(view.envelope_sha256) != 1)
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    if (view.kind != OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT ||
        view.key_class_mask != 0)
      return OPEN64_FHE_STATUS_KIND_MISMATCH;
    if (!open64_fhe_mock_digest_equal(
            view.provider_identity_sha256,
            desc->provider_identity_sha256))
      return OPEN64_FHE_STATUS_PROVIDER_MISMATCH;
    if (!open64_fhe_mock_digest_equal(
            view.config_identity_sha256,
            desc->config_identity_sha256))
      return OPEN64_FHE_STATUS_CONFIG_MISMATCH;

    open64_fhe_context_v1_t context =
        new (std::nothrow) open64_fhe_context_v1_s;
    if (context == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    context->generation = open64_fhe_mock_next_generation++;
    context->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    context->child_count = 0;
    context->broker = broker;
    memcpy(context->execution_profile_sha256,
           desc->execution_profile_sha256, 32);
    memcpy(context->provider_identity_sha256,
           desc->provider_identity_sha256, 32);
    memcpy(context->provider_manifest_sha256,
           desc->provider_manifest_sha256, 32);
    memcpy(context->config_identity_sha256,
           desc->config_identity_sha256, 32);
    try {
      open64_fhe_mock_contexts.insert(context);
    } catch (...) {
      delete context;
      throw;
    }
    ++broker->child_count;
    *out_context = context;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_context_destroy_v1(open64_fhe_context_v1_t *context)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (context == NULL || !open64_fhe_mock_context_live(*context))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*context)->child_count != 0)
      return OPEN64_FHE_STATUS_BUSY;
    open64_fhe_broker_v1_t broker = (*context)->broker;
    (*context)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    (*context)->broker = NULL;
    memset((*context)->execution_profile_sha256, 0, 32);
    memset((*context)->provider_identity_sha256, 0, 32);
    memset((*context)->provider_manifest_sha256, 0, 32);
    memset((*context)->config_identity_sha256, 0, 32);
    --broker->child_count;
    *context = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}
