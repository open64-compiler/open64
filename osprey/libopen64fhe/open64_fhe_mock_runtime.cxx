/*
 * Copyright (C) 2026 Open64 Project
 *
 * Deterministic provider used to certify the Open64 FHE runtime ABI.
 */

#include "open64_fhe_mock_test.h"

#include <mutex>
#include <new>
#include <set>
#include <string.h>

enum OPEN64_FHE_MOCK_TOKEN_STATE {
  OPEN64_FHE_MOCK_TOKEN_LIVE = 1,
  OPEN64_FHE_MOCK_TOKEN_CLAIMED = 2,
  OPEN64_FHE_MOCK_TOKEN_CONSUMED = 3
};

struct open64_fhe_host_bootstrap_v1_s {
  uint64_t generation;
  uint32_t state;
  uint8_t deployment_identity_sha256[32];
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
};

static uint64_t open64_fhe_mock_next_generation = 1;
static std::mutex open64_fhe_mock_mutex;
static std::set<open64_fhe_host_bootstrap_v1_t> open64_fhe_mock_hosts;
static std::set<open64_fhe_launcher_capability_v1_t>
    open64_fhe_mock_capabilities;
static std::set<open64_fhe_broker_v1_t> open64_fhe_mock_brokers;

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
open64_fhe_mock_host_bootstrap_destroy_v1(
    open64_fhe_host_bootstrap_v1_t *bootstrap)
{
  try {
    std::lock_guard<std::mutex> lock(open64_fhe_mock_mutex);
    if (bootstrap == NULL || !open64_fhe_mock_host_live(*bootstrap))
      return OPEN64_FHE_STATUS_INVALID_HANDLE;
    (*bootstrap)->state = OPEN64_FHE_MOCK_TOKEN_CONSUMED;
    memset((*bootstrap)->deployment_identity_sha256, 0, 32);
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
    *broker = NULL;
    return OPEN64_FHE_STATUS_OK;
  } catch (...) {
    return OPEN64_FHE_STATUS_INTERNAL_ERROR;
  }
}
