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

struct open64_fhe_keyset_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t key_class_mask;
  open64_fhe_context_v1_t context;
};

struct open64_fhe_model_package_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t dependent_count;
  open64_fhe_broker_v1_t broker;
  uint8_t model_identity_sha256[32];
  uint8_t config_identity_sha256[32];
};

struct open64_fhe_model_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t child_count;
  uint32_t asset_count;
  bool assets_sealed;
  bool inference_started;
  uint32_t sequence_cursor;
  open64_fhe_context_v1_t context;
  open64_fhe_keyset_v1_t keyset;
  open64_fhe_model_package_v1_t package;
  uint8_t model_identity_sha256[32];
  std::set<std::array<uint8_t, 32> > bound_assets;
};

struct open64_fhe_plain_tensor_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t reference_count;
  uint32_t model_reference_count;
  open64_fhe_model_v1_t model;
  uint8_t tensor_identity_sha256[32];
};

struct open64_fhe_ciphertext_v1_s {
  uint64_t generation;
  uint32_t state;
  uint32_t reference_count;
  open64_fhe_model_v1_t model;
  uint32_t level;
  int32_t scale_bits;
  uint32_t components;
  uint32_t active_slots;
  uint8_t value_identity_sha256[32];
  uint8_t tensor_identity_sha256[32];
  uint8_t layout_identity_sha256[32];
};

static uint64_t open64_fhe_mock_next_generation = 1;
static std::mutex open64_fhe_mock_mutex;
static std::set<open64_fhe_host_bootstrap_v1_t> open64_fhe_mock_hosts;
static std::set<open64_fhe_launcher_capability_v1_t>
    open64_fhe_mock_capabilities;
static std::set<open64_fhe_broker_v1_t> open64_fhe_mock_brokers;
static std::set<open64_fhe_context_v1_t> open64_fhe_mock_contexts;
static std::set<open64_fhe_keyset_v1_t> open64_fhe_mock_keysets;
static std::set<open64_fhe_model_package_v1_t> open64_fhe_mock_packages;
static std::set<open64_fhe_model_v1_t> open64_fhe_mock_models;
static std::set<open64_fhe_plain_tensor_v1_t> open64_fhe_mock_plain_tensors;
static std::set<open64_fhe_ciphertext_v1_t> open64_fhe_mock_ciphertexts;

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
open64_fhe_mock_read_envelope_header(
    const void *envelope,
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
  view->kind = open64_fhe_mock_load_le32(bytes + 16);
  view->key_class_mask = open64_fhe_mock_load_le32(bytes + 20);
  view->provider_identity_sha256 = bytes + 24;
  view->config_identity_sha256 = bytes + 56;
  view->payload = bytes + OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1;
  view->payload_size = payload_size;
  return OPEN64_FHE_STATUS_OK;
}

static open64_fhe_status_v1
open64_fhe_mock_verify_envelope_integrity(
    const void *envelope,
    uint64_t envelope_size,
    OPEN64_FHE_MOCK_ENVELOPE_VIEW *view)
{
  const uint8_t *bytes = static_cast<const uint8_t *>(envelope);
  std::array<uint8_t, 32> payload_digest = open64_fhe_mock_digest(
      view->payload, size_t(view->payload_size));
  if (!open64_fhe_mock_digest_equal(payload_digest.data(), bytes + 96))
    return OPEN64_FHE_STATUS_INTEGRITY_ERROR;
  std::vector<uint8_t> authenticated(bytes, bytes + envelope_size);
  memset(authenticated.data() + 128, 0, 32);
  std::array<uint8_t, 32> envelope_digest = open64_fhe_mock_digest(
      authenticated.data(), authenticated.size());
  if (!open64_fhe_mock_digest_equal(envelope_digest.data(), bytes + 128))
    return OPEN64_FHE_STATUS_INTEGRITY_ERROR;
  view->envelope_sha256 = envelope_digest;
  return OPEN64_FHE_STATUS_OK;
}

static open64_fhe_status_v1
open64_fhe_mock_read_envelope(const void *envelope,
                              uint64_t envelope_size,
                              OPEN64_FHE_MOCK_ENVELOPE_VIEW *view)
{
  open64_fhe_status_v1 status = open64_fhe_mock_read_envelope_header(
      envelope, envelope_size, view);
  if (status != OPEN64_FHE_STATUS_OK)
    return status;
  return open64_fhe_mock_verify_envelope_integrity(
      envelope, envelope_size, view);
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

static bool
open64_fhe_mock_keyset_live(open64_fhe_keyset_v1_t keyset)
{
  return keyset != NULL && open64_fhe_mock_keysets.count(keyset) == 1 &&
         keyset->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_package_live(open64_fhe_model_package_v1_t package)
{
  return package != NULL && open64_fhe_mock_packages.count(package) == 1 &&
         package->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_model_live(open64_fhe_model_v1_t model)
{
  return model != NULL && open64_fhe_mock_models.count(model) == 1 &&
         model->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_plain_live(open64_fhe_plain_tensor_v1_t plain_tensor)
{
  return plain_tensor != NULL &&
         open64_fhe_mock_plain_tensors.count(plain_tensor) == 1 &&
         plain_tensor->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
}

static bool
open64_fhe_mock_ciphertext_live(open64_fhe_ciphertext_v1_t ciphertext)
{
  return ciphertext != NULL &&
         open64_fhe_mock_ciphertexts.count(ciphertext) == 1 &&
         ciphertext->state == OPEN64_FHE_MOCK_TOKEN_LIVE;
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

extern "C" open64_fhe_status_v1
open64_fhe_keyset_import_v1(
    open64_fhe_context_v1_t context,
    const void *evaluation_keyset_envelope,
    uint64_t envelope_size,
    open64_fhe_keyset_v1_t *out_keyset)
{
  if (out_keyset == NULL || *out_keyset != NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_context_live(context))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope_header(
        evaluation_keyset_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    if ((view.key_class_mask & OPEN64_FHE_KEY_CLASS_SECRET) != 0)
      return OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN;
    status = open64_fhe_mock_verify_envelope_integrity(
        evaluation_keyset_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    open64_fhe_broker_v1_t broker = context->broker;
    if (broker->trusted_envelopes.count(view.envelope_sha256) != 1)
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    if (view.kind != OPEN64_FHE_ENVELOPE_KIND_KEYSET)
      return OPEN64_FHE_STATUS_KIND_MISMATCH;
    const uint32_t allowed_keys = OPEN64_FHE_KEY_CLASS_PUBLIC |
        OPEN64_FHE_KEY_CLASS_EVALUATION |
        OPEN64_FHE_KEY_CLASS_RELINEARIZATION |
        OPEN64_FHE_KEY_CLASS_ROTATION |
        OPEN64_FHE_KEY_CLASS_BOOTSTRAP;
    if (view.key_class_mask == 0 ||
        (view.key_class_mask & ~allowed_keys) != 0)
      return OPEN64_FHE_STATUS_KEY_CLASS_MISMATCH;
    if (!open64_fhe_mock_digest_equal(
            view.provider_identity_sha256,
            context->provider_identity_sha256))
      return OPEN64_FHE_STATUS_PROVIDER_MISMATCH;
    if (!open64_fhe_mock_digest_equal(
            view.config_identity_sha256,
            context->config_identity_sha256))
      return OPEN64_FHE_STATUS_CONFIG_MISMATCH;

    open64_fhe_keyset_v1_t keyset =
        new (std::nothrow) open64_fhe_keyset_v1_s;
    if (keyset == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    keyset->generation = open64_fhe_mock_next_generation++;
    keyset->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    keyset->key_class_mask = view.key_class_mask;
    keyset->context = context;
    try {
      open64_fhe_mock_keysets.insert(keyset);
    } catch (...) {
      delete keyset;
      throw;
    }
    ++context->child_count;
    *out_keyset = keyset;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_keyset_release_v1(open64_fhe_keyset_v1_t *keyset)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (keyset == NULL || !open64_fhe_mock_keyset_live(*keyset))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*keyset)->context == NULL || (*keyset)->context->state !=
        OPEN64_FHE_MOCK_TOKEN_LIVE)
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    for (std::set<open64_fhe_model_v1_t>::const_iterator it =
             open64_fhe_mock_models.begin();
         it != open64_fhe_mock_models.end(); ++it) {
      if ((*it)->state == OPEN64_FHE_MOCK_TOKEN_LIVE &&
          (*it)->keyset == *keyset)
        return OPEN64_FHE_STATUS_BUSY;
    }
    open64_fhe_context_v1_t context = (*keyset)->context;
    (*keyset)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    (*keyset)->key_class_mask = 0;
    (*keyset)->context = NULL;
    --context->child_count;
    *keyset = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_model_package_import_v1(
    open64_fhe_broker_v1_t broker,
    const void *model_package_envelope,
    uint64_t envelope_size,
    open64_fhe_model_package_v1_t *out_package)
{
  if (out_package == NULL || *out_package != NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_broker_live(broker))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope(
        model_package_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    if (broker->trusted_envelopes.count(view.envelope_sha256) != 1)
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    if (view.kind != OPEN64_FHE_ENVELOPE_KIND_MODEL_PACKAGE ||
        view.key_class_mask != 0)
      return OPEN64_FHE_STATUS_KIND_MISMATCH;
    uint8_t zero_provider[32] = {};
    if (!open64_fhe_mock_digest_equal(view.provider_identity_sha256,
                                      zero_provider))
      return OPEN64_FHE_STATUS_PROVIDER_MISMATCH;
    if (view.payload_size == 0)
      return OPEN64_FHE_STATUS_MODEL_PACKAGE_INVALID;
    open64_fhe_model_package_v1_t package =
        new (std::nothrow) open64_fhe_model_package_v1_s;
    if (package == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    package->generation = open64_fhe_mock_next_generation++;
    package->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    package->dependent_count = 0;
    package->broker = broker;
    memcpy(package->model_identity_sha256,
           view.envelope_sha256.data(), 32);
    memcpy(package->config_identity_sha256,
           view.config_identity_sha256, 32);
    try {
      open64_fhe_mock_packages.insert(package);
    } catch (...) {
      delete package;
      throw;
    }
    ++broker->child_count;
    *out_package = package;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_model_package_release_v1(
    open64_fhe_model_package_v1_t *package)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (package == NULL || !open64_fhe_mock_package_live(*package))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*package)->dependent_count != 0)
      return OPEN64_FHE_STATUS_BUSY;
    open64_fhe_broker_v1_t broker = (*package)->broker;
    (*package)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    (*package)->broker = NULL;
    memset((*package)->model_identity_sha256, 0, 32);
    memset((*package)->config_identity_sha256, 0, 32);
    --broker->child_count;
    *package = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_model_create_v1(
    open64_fhe_context_v1_t context,
    open64_fhe_keyset_v1_t keyset,
    open64_fhe_model_package_v1_t package,
    open64_fhe_model_v1_t *out_model)
{
  if (out_model == NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  if (*out_model != NULL) {
    if ((void *)*out_model == (void *)context ||
        (void *)*out_model == (void *)keyset ||
        (void *)*out_model == (void *)package)
      return OPEN64_FHE_STATUS_ALIAS_FORBIDDEN;
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  }
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_context_live(context) ||
        !open64_fhe_mock_keyset_live(keyset) ||
        !open64_fhe_mock_package_live(package))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if (keyset->context != context || package->broker != context->broker)
      return OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH;
    if (!open64_fhe_mock_digest_equal(package->config_identity_sha256,
                                      context->config_identity_sha256))
      return OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH;
    const uint32_t required_keys = OPEN64_FHE_KEY_CLASS_PUBLIC |
        OPEN64_FHE_KEY_CLASS_EVALUATION |
        OPEN64_FHE_KEY_CLASS_RELINEARIZATION |
        OPEN64_FHE_KEY_CLASS_ROTATION |
        OPEN64_FHE_KEY_CLASS_BOOTSTRAP;
    if ((keyset->key_class_mask & required_keys) != required_keys)
      return OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH;
    open64_fhe_model_v1_t model =
        new (std::nothrow) open64_fhe_model_v1_s;
    if (model == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    model->generation = open64_fhe_mock_next_generation++;
    model->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    model->child_count = 0;
    model->asset_count = 0;
    model->assets_sealed = false;
    model->inference_started = false;
    model->sequence_cursor = 0;
    model->context = context;
    model->keyset = keyset;
    model->package = package;
    memcpy(model->model_identity_sha256,
           package->model_identity_sha256, 32);
    try {
      open64_fhe_mock_models.insert(model);
    } catch (...) {
      delete model;
      throw;
    }
    ++context->child_count;
    ++package->dependent_count;
    *out_model = model;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_model_destroy_v1(open64_fhe_model_v1_t *model)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (model == NULL || !open64_fhe_mock_model_live(*model))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if ((*model)->child_count != 0)
      return OPEN64_FHE_STATUS_BUSY;
    for (std::set<open64_fhe_plain_tensor_v1_t>::const_iterator it =
             open64_fhe_mock_plain_tensors.begin();
         it != open64_fhe_mock_plain_tensors.end(); ++it) {
      if ((*it)->state == OPEN64_FHE_MOCK_TOKEN_LIVE &&
          (*it)->model == *model &&
          (*it)->reference_count > (*it)->model_reference_count)
        return OPEN64_FHE_STATUS_BUSY;
    }
    open64_fhe_context_v1_t context = (*model)->context;
    open64_fhe_model_package_v1_t package = (*model)->package;
    (*model)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    (*model)->context = NULL;
    (*model)->keyset = NULL;
    (*model)->package = NULL;
    (*model)->bound_assets.clear();
    for (std::set<open64_fhe_plain_tensor_v1_t>::const_iterator it =
             open64_fhe_mock_plain_tensors.begin();
         it != open64_fhe_mock_plain_tensors.end(); ++it) {
      open64_fhe_plain_tensor_v1_t plain = *it;
      if (plain->state != OPEN64_FHE_MOCK_TOKEN_LIVE ||
          plain->model != *model || plain->model_reference_count == 0)
        continue;
      plain->reference_count -= plain->model_reference_count;
      plain->model_reference_count = 0;
      if (plain->reference_count == 0) {
        plain->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
        plain->model = NULL;
      }
    }
    memset((*model)->model_identity_sha256, 0, 32);
    --context->child_count;
    --package->dependent_count;
    *model = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_plain_tensor_import_v1(
    open64_fhe_model_v1_t model,
    const void *plain_tensor_envelope,
    uint64_t envelope_size,
    open64_fhe_plain_tensor_v1_t *out_plain_tensor)
{
  if (out_plain_tensor == NULL || *out_plain_tensor != NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_model_live(model))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope(
        plain_tensor_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    if (model->context->broker->trusted_envelopes.count(
            view.envelope_sha256) != 1)
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    if (view.kind != OPEN64_FHE_ENVELOPE_KIND_PLAIN_TENSOR ||
        view.key_class_mask != 0)
      return OPEN64_FHE_STATUS_KIND_MISMATCH;
    uint8_t zero_provider[32] = {};
    if (!open64_fhe_mock_digest_equal(view.provider_identity_sha256,
                                      zero_provider))
      return OPEN64_FHE_STATUS_PROVIDER_MISMATCH;
    if (!open64_fhe_mock_digest_equal(view.config_identity_sha256,
                                      model->context->config_identity_sha256))
      return OPEN64_FHE_STATUS_CONFIG_MISMATCH;
    open64_fhe_plain_tensor_v1_t plain =
        new (std::nothrow) open64_fhe_plain_tensor_v1_s;
    if (plain == NULL)
      return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
    plain->generation = open64_fhe_mock_next_generation++;
    plain->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
    plain->reference_count = 1;
    plain->model_reference_count = 0;
    plain->model = model;
    memcpy(plain->tensor_identity_sha256,
           view.envelope_sha256.data(), 32);
    try {
      open64_fhe_mock_plain_tensors.insert(plain);
    } catch (...) {
      delete plain;
      throw;
    }
    *out_plain_tensor = plain;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_model_bind_asset_v1(open64_fhe_model_v1_t model,
                               open64_fhe_plain_tensor_v1_t asset)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_model_live(model) || !open64_fhe_mock_plain_live(asset))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if (asset->model != model)
      return OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH;
    if (model->assets_sealed)
      return OPEN64_FHE_STATUS_BUSY;
    std::array<uint8_t, 32> identity;
    memcpy(identity.data(), asset->tensor_identity_sha256, 32);
    if (model->bound_assets.count(identity) != 0)
      return OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH;
    try {
      model->bound_assets.insert(identity);
    } catch (...) {
      throw;
    }
    ++asset->reference_count;
    ++asset->model_reference_count;
    ++model->asset_count;
    return OPEN64_FHE_STATUS_OK;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_plain_tensor_retain_v1(open64_fhe_plain_tensor_v1_t plain_tensor)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_plain_live(plain_tensor))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    ++plain_tensor->reference_count;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_plain_tensor_release_v1(
    open64_fhe_plain_tensor_v1_t *plain_tensor)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (plain_tensor == NULL || !open64_fhe_mock_plain_live(*plain_tensor) ||
        (*plain_tensor)->reference_count <=
            (*plain_tensor)->model_reference_count)
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    --(*plain_tensor)->reference_count;
    if ((*plain_tensor)->reference_count == 0) {
      (*plain_tensor)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
      (*plain_tensor)->model = NULL;
    }
    *plain_tensor = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

static open64_fhe_status_v1
open64_fhe_mock_publish_ciphertext(
    open64_fhe_model_v1_t model,
    const uint8_t value_identity[32],
    const uint8_t tensor_identity[32],
    const uint8_t layout_identity[32],
    uint32_t level,
    int32_t scale_bits,
    uint32_t components,
    open64_fhe_ciphertext_v1_t *out_ciphertext)
{
  open64_fhe_ciphertext_v1_t ciphertext =
      new (std::nothrow) open64_fhe_ciphertext_v1_s;
  if (ciphertext == NULL)
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  ciphertext->generation = open64_fhe_mock_next_generation++;
  ciphertext->state = OPEN64_FHE_MOCK_TOKEN_LIVE;
  ciphertext->reference_count = 1;
  ciphertext->model = model;
  ciphertext->level = level;
  ciphertext->scale_bits = scale_bits;
  ciphertext->components = components;
  ciphertext->active_slots = 32768;
  memcpy(ciphertext->value_identity_sha256, value_identity, 32);
  memcpy(ciphertext->tensor_identity_sha256, tensor_identity, 32);
  memcpy(ciphertext->layout_identity_sha256, layout_identity, 32);
  try {
    open64_fhe_mock_ciphertexts.insert(ciphertext);
  } catch (...) {
    delete ciphertext;
    throw;
  }
  ++model->child_count;
  *out_ciphertext = ciphertext;
  return OPEN64_FHE_STATUS_OK;
}

extern "C" open64_fhe_status_v1
open64_fhe_ciphertext_import_v1(
    open64_fhe_model_v1_t model,
    const open64_fhe_import_binding_v1 *import_binding,
    const void *ciphertext_envelope,
    uint64_t envelope_size,
    open64_fhe_ciphertext_v1_t *out_ciphertext)
{
  if (import_binding == NULL || out_ciphertext == NULL ||
      *out_ciphertext != NULL ||
      import_binding->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      import_binding->struct_size != sizeof(*import_binding))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_model_live(model))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    open64_fhe_status_v1 status = open64_fhe_mock_read_envelope(
        ciphertext_envelope, envelope_size, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    if (view.kind != OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT ||
        view.key_class_mask != 0)
      return OPEN64_FHE_STATUS_KIND_MISMATCH;
    if (!open64_fhe_mock_digest_equal(view.provider_identity_sha256,
                                      model->context->provider_identity_sha256))
      return OPEN64_FHE_STATUS_PROVIDER_MISMATCH;
    if (!open64_fhe_mock_digest_equal(view.config_identity_sha256,
                                      model->context->config_identity_sha256))
      return OPEN64_FHE_STATUS_CONFIG_MISMATCH;
    if (!open64_fhe_mock_digest_equal(import_binding->expected_envelope_sha256,
                                      view.envelope_sha256.data()) ||
        !open64_fhe_mock_digest_equal(import_binding->model_identity_sha256,
                                      model->model_identity_sha256) ||
        !open64_fhe_mock_digest_nonzero(
            import_binding->authenticated_principal_sha256) ||
        !open64_fhe_mock_digest_nonzero(import_binding->session_identity_sha256) ||
        !open64_fhe_mock_digest_nonzero(import_binding->request_nonce) ||
        !open64_fhe_mock_digest_nonzero(import_binding->auth_key_id_sha256) ||
        !open64_fhe_mock_digest_nonzero(import_binding->auth_tag_hmac_sha256))
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    if (model->inference_started)
      return OPEN64_FHE_STATUS_BUSY;
    model->assets_sealed = true;
    status = open64_fhe_mock_publish_ciphertext(
        model, view.envelope_sha256.data(), view.envelope_sha256.data(),
        view.envelope_sha256.data(), 0, 56, 2, out_ciphertext);
    if (status == OPEN64_FHE_STATUS_OK) {
      model->inference_started = true;
      model->sequence_cursor = 0;
    }
    return status;
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_ciphertext_retain_v1(open64_fhe_ciphertext_v1_t ciphertext)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_ciphertext_live(ciphertext))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    ++ciphertext->reference_count;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_ciphertext_release_v1(open64_fhe_ciphertext_v1_t *ciphertext)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (ciphertext == NULL || !open64_fhe_mock_ciphertext_live(*ciphertext))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    open64_fhe_model_v1_t model = (*ciphertext)->model;
    if (--(*ciphertext)->reference_count == 0) {
      (*ciphertext)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
      (*ciphertext)->model = NULL;
      --model->child_count;
    }
    *ciphertext = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

static open64_fhe_status_v1
open64_fhe_mock_evaluate(
    uint32_t expected_kind,
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t input0,
    open64_fhe_ciphertext_v1_t input1,
    open64_fhe_plain_tensor_v1_t plain0,
    open64_fhe_plain_tensor_v1_t plain1,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  if (desc == NULL || out_result == NULL ||
      desc->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      desc->struct_size != sizeof(*desc) || desc->reserved != 0)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  if (*out_result != NULL) {
    if ((void *)*out_result == (void *)input0 ||
        (void *)*out_result == (void *)input1 ||
        (void *)*out_result == (void *)plain0 ||
        (void *)*out_result == (void *)plain1)
      return OPEN64_FHE_STATUS_ALIAS_FORBIDDEN;
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  }
  if (!open64_fhe_mock_model_live(model) ||
      !open64_fhe_mock_ciphertext_live(input0) ||
      (input1 != NULL && !open64_fhe_mock_ciphertext_live(input1)) ||
      (plain0 != NULL && !open64_fhe_mock_plain_live(plain0)) ||
      (plain1 != NULL && !open64_fhe_mock_plain_live(plain1)))
    return OPEN64_FHE_STATUS_INVALID_HANDLE;
  if (input0->model != model || (input1 != NULL && input1->model != model) ||
      (plain0 != NULL && plain0->model != model) ||
      (plain1 != NULL && plain1->model != model))
    return OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH;
  if (desc->operation_kind != expected_kind ||
      desc->sequence_index != model->sequence_cursor ||
      desc->input_count != (input1 == NULL ? 1u : 2u))
    return OPEN64_FHE_STATUS_CALL_ORDER_MISMATCH;
  if (!open64_fhe_mock_digest_equal(desc->config_identity_sha256,
                                    model->context->config_identity_sha256) ||
      !open64_fhe_mock_digest_equal(desc->input_value_identity_sha256[0],
                                    input0->value_identity_sha256) ||
      (input1 != NULL && !open64_fhe_mock_digest_equal(
           desc->input_value_identity_sha256[1],
           input1->value_identity_sha256)) ||
      !open64_fhe_mock_digest_equal(desc->input_tensor_identity_sha256[0],
                                    input0->tensor_identity_sha256) ||
      !open64_fhe_mock_digest_equal(desc->input_layout_identity_sha256[0],
                                    input0->layout_identity_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->semantic_event_id) ||
      !open64_fhe_mock_digest_nonzero(desc->descriptor_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->operation_identity_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->output_value_identity_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->output_tensor_identity_sha256) ||
      !open64_fhe_mock_digest_nonzero(desc->output_layout_identity_sha256))
    return OPEN64_FHE_STATUS_MODEL_REQUIREMENT_MISMATCH;
  if ((desc->payload == NULL && desc->payload_size != 0) ||
      desc->payload_size > size_t(-1))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  std::array<uint8_t, 32> payload_digest = open64_fhe_mock_digest(
      desc->payload, size_t(desc->payload_size));
  if (!open64_fhe_mock_digest_equal(payload_digest.data(),
                                    desc->payload_sha256))
    return OPEN64_FHE_STATUS_INTEGRITY_ERROR;
  uint32_t level = input0->level + 1;
  if (expected_kind == OPEN64_FHE_OP_BOOTSTRAP) {
    level = desc->payload_size >= sizeof(uint32_t)
        ? open64_fhe_mock_load_le32(
              static_cast<const uint8_t *>(desc->payload))
        : 15;
  }
  open64_fhe_status_v1 status = open64_fhe_mock_publish_ciphertext(
      model, desc->output_value_identity_sha256,
      desc->output_tensor_identity_sha256,
      desc->output_layout_identity_sha256, level, 56, 2, out_result);
  if (status == OPEN64_FHE_STATUS_OK)
    ++model->sequence_cursor;
  return status;
}

#define OPEN64_FHE_MOCK_UNARY(Name, Kind) \
extern "C" open64_fhe_status_v1 Name( \
    open64_fhe_model_v1_t model, open64_fhe_ciphertext_v1_t input, \
    const open64_fhe_operation_desc_v1 *desc, \
    open64_fhe_ciphertext_v1_t *out_result) \
{ \
  try { \
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex); \
    return open64_fhe_mock_evaluate(Kind, model, input, NULL, NULL, NULL, \
                                    desc, out_result); \
  } catch (const std::bad_alloc &) { \
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY; \
  } catch (...) { \
    return OPEN64_FHE_STATUS_INTERNAL_ERROR; \
  } \
}

OPEN64_FHE_MOCK_UNARY(open64_fhe_bootstrap_v1, OPEN64_FHE_OP_BOOTSTRAP)
OPEN64_FHE_MOCK_UNARY(open64_fhe_relu_normalize_v1,
                      OPEN64_FHE_OP_RELU_NORMALIZE)
OPEN64_FHE_MOCK_UNARY(open64_fhe_average_pool_v1,
                      OPEN64_FHE_OP_AVERAGE_POOL)
OPEN64_FHE_MOCK_UNARY(open64_fhe_layout_convert_v1,
                      OPEN64_FHE_OP_LAYOUT_CONVERT)

#undef OPEN64_FHE_MOCK_UNARY

extern "C" open64_fhe_status_v1
open64_fhe_residual_add_v1(
    open64_fhe_model_v1_t model, open64_fhe_ciphertext_v1_t main_path,
    open64_fhe_ciphertext_v1_t shortcut_path,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    return open64_fhe_mock_evaluate(
        OPEN64_FHE_OP_RESIDUAL_ADD, model, main_path, shortcut_path,
        NULL, NULL, desc, out_result);
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_relu_poly_stage_v1(
    open64_fhe_model_v1_t model, open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t coefficients,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    return open64_fhe_mock_evaluate(
        OPEN64_FHE_OP_RELU_POLY_STAGE, model, input, NULL,
        coefficients, NULL, desc, out_result);
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_conv2d_plain_v1(
    open64_fhe_model_v1_t model, open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t weight,
    open64_fhe_plain_tensor_v1_t optional_bias,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    return open64_fhe_mock_evaluate(
        OPEN64_FHE_OP_CONV2D_PLAIN, model, input, NULL,
        weight, optional_bias, desc, out_result);
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_linear_plain_v1(
    open64_fhe_model_v1_t model, open64_fhe_ciphertext_v1_t input,
    open64_fhe_plain_tensor_v1_t weight,
    open64_fhe_plain_tensor_v1_t optional_bias,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    return open64_fhe_mock_evaluate(
        OPEN64_FHE_OP_LINEAR_PLAIN, model, input, NULL,
        weight, optional_bias, desc, out_result);
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_relu_reconstruct_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t refreshed_input,
    open64_fhe_ciphertext_v1_t stage2_result,
    const open64_fhe_operation_desc_v1 *desc,
    open64_fhe_ciphertext_v1_t *out_result)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    return open64_fhe_mock_evaluate(
        OPEN64_FHE_OP_RELU_RECONSTRUCT, model, refreshed_input,
        stage2_result, NULL, NULL, desc, out_result);
  } catch (const std::bad_alloc &) {
    return OPEN64_FHE_STATUS_OUT_OF_MEMORY;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_ciphertext_export_v1(
    open64_fhe_model_v1_t model,
    open64_fhe_ciphertext_v1_t ciphertext,
    const open64_fhe_export_binding_v1 *export_binding,
    void *envelope_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written,
    open64_fhe_export_receipt_v1 *out_receipt)
{
  if (out_required_or_written == NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  *out_required_or_written = 0;
  if (export_binding == NULL || out_receipt == NULL ||
      export_binding->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      export_binding->struct_size != sizeof(*export_binding) ||
      out_receipt->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      out_receipt->struct_size != sizeof(*out_receipt))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  memset(out_receipt->actual_envelope_sha256, 0, 32);
  memset(out_receipt->auth_key_id_sha256, 0, 32);
  memset(out_receipt->receipt_hmac_sha256, 0, 32);
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_model_live(model) ||
        !open64_fhe_mock_ciphertext_live(ciphertext))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    if (ciphertext->model != model)
      return OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH;
    if (model->sequence_cursor != 147 ||
        !open64_fhe_mock_digest_equal(
            export_binding->model_identity_sha256,
            model->model_identity_sha256) ||
        !open64_fhe_mock_digest_equal(
            export_binding->final_output_identity_sha256,
            ciphertext->value_identity_sha256))
      return OPEN64_FHE_STATUS_CALL_ORDER_MISMATCH;
    if (!open64_fhe_mock_digest_nonzero(
            export_binding->authenticated_principal_sha256) ||
        !open64_fhe_mock_digest_nonzero(
            export_binding->session_identity_sha256) ||
        !open64_fhe_mock_digest_nonzero(export_binding->request_nonce) ||
        !open64_fhe_mock_digest_nonzero(export_binding->auth_key_id_sha256) ||
        !open64_fhe_mock_digest_nonzero(export_binding->auth_tag_hmac_sha256))
      return OPEN64_FHE_STATUS_TRUST_FAILURE;
    uint8_t payload[96];
    memset(payload, 0, sizeof(payload));
    memcpy(payload, ciphertext->value_identity_sha256, 32);
    memcpy(payload + 32, ciphertext->tensor_identity_sha256, 32);
    memcpy(payload + 64, ciphertext->layout_identity_sha256, 32);
    uint64_t required = OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 + sizeof(payload);
    *out_required_or_written = required;
    if (envelope_buffer == NULL || buffer_capacity < required)
      return OPEN64_FHE_STATUS_BUFFER_TOO_SMALL;
    uint64_t written = 0;
    open64_fhe_status_v1 status = open64_fhe_mock_seal_envelope_v1(
        OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT, 0,
        model->context->provider_identity_sha256,
        model->context->config_identity_sha256, payload, sizeof(payload),
        envelope_buffer, buffer_capacity, &written);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    OPEN64_FHE_MOCK_ENVELOPE_VIEW view;
    status = open64_fhe_mock_read_envelope(
        envelope_buffer, written, &view);
    if (status != OPEN64_FHE_STATUS_OK)
      return status;
    memcpy(out_receipt->actual_envelope_sha256,
           view.envelope_sha256.data(), 32);
    memcpy(out_receipt->auth_key_id_sha256,
           export_binding->auth_key_id_sha256, 32);
    uint8_t receipt_material[64];
    memcpy(receipt_material, export_binding->auth_tag_hmac_sha256, 32);
    memcpy(receipt_material + 32, view.envelope_sha256.data(), 32);
    open64_fhe_mock_sha256(receipt_material, sizeof(receipt_material),
                           out_receipt->receipt_hmac_sha256);
    *out_required_or_written = written;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_ciphertext_inspect_v1(
    open64_fhe_ciphertext_v1_t ciphertext,
    open64_fhe_ciphertext_info_v1 *out_info)
{
  if (out_info == NULL ||
      out_info->abi_version != OPEN64_FHE_ABI_VERSION_V1 ||
      out_info->struct_size != sizeof(*out_info))
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  memset(reinterpret_cast<uint8_t *>(out_info) + 8, 0,
         sizeof(*out_info) - 8);
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_ciphertext_live(ciphertext))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    out_info->level = ciphertext->level;
    out_info->scale_bits = ciphertext->scale_bits;
    out_info->components = ciphertext->components;
    out_info->active_slots = ciphertext->active_slots;
    out_info->size_bytes = 96;
    memcpy(out_info->tensor_identity_sha256,
           ciphertext->tensor_identity_sha256, 32);
    memcpy(out_info->layout_identity_sha256,
           ciphertext->layout_identity_sha256, 32);
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

static open64_fhe_status_v1
open64_fhe_mock_no_diagnostic(char *utf8_buffer,
                              uint64_t buffer_capacity,
                              uint64_t *out_required_or_written)
{
  (void)utf8_buffer;
  (void)buffer_capacity;
  if (out_required_or_written == NULL)
    return OPEN64_FHE_STATUS_INVALID_ARGUMENT;
  *out_required_or_written = 0;
  return OPEN64_FHE_STATUS_NO_DIAGNOSTIC;
}

extern "C" open64_fhe_status_v1
open64_fhe_get_last_diagnostic_v1(
    open64_fhe_context_v1_t context,
    char *utf8_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_context_live(context))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    return open64_fhe_mock_no_diagnostic(
        utf8_buffer, buffer_capacity, out_required_or_written);
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}

extern "C" open64_fhe_status_v1
open64_fhe_get_last_broker_diagnostic_v1(
    open64_fhe_broker_v1_t broker,
    char *utf8_buffer,
    uint64_t buffer_capacity,
    uint64_t *out_required_or_written)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (!open64_fhe_mock_broker_live(broker))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    return open64_fhe_mock_no_diagnostic(
        utf8_buffer, buffer_capacity, out_required_or_written);
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}
