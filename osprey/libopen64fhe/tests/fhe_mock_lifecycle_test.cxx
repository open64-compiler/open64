#include "open64_fhe_mock_test.h"

#include <stdint.h>

static int
Require(open64_fhe_status_v1 actual, open64_fhe_status_v1 expected)
{
  return actual == expected ? 0 : 1;
}

int
main()
{
  uint8_t deployment[32] = {};
  deployment[0] = 1;
  open64_fhe_host_bootstrap_v1_t host = NULL;
  open64_fhe_launcher_capability_v1_t capability = NULL;
  open64_fhe_launcher_capability_v1_t consumed_capability = NULL;
  open64_fhe_broker_v1_t broker = NULL;
  open64_fhe_broker_desc_v1 desc = {
    OPEN64_FHE_ABI_VERSION_V1,
    sizeof(open64_fhe_broker_desc_v1),
    0,
    0
  };

  if (Require(open64_fhe_mock_host_bootstrap_create_v1(deployment, &host),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_launcher_capability_acquire_v1(host, &capability),
              OPEN64_FHE_STATUS_OK))
    return 1;

  open64_fhe_broker_desc_v1 invalid_desc = desc;
  invalid_desc.reserved = 1;
  if (Require(open64_fhe_broker_create_v1(
                  &capability, &invalid_desc, &broker),
              OPEN64_FHE_STATUS_INVALID_ARGUMENT) ||
      capability == NULL || broker != NULL)
    return 1;

  open64_fhe_broker_v1_t alias =
      reinterpret_cast<open64_fhe_broker_v1_t>(capability);
  if (Require(open64_fhe_broker_create_v1(&capability, &desc, &alias),
              OPEN64_FHE_STATUS_ALIAS_FORBIDDEN) ||
      capability == NULL ||
      alias != reinterpret_cast<open64_fhe_broker_v1_t>(capability))
    return 1;

  consumed_capability = capability;
  if (Require(open64_fhe_broker_create_v1(&capability, &desc, &broker),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_launcher_capability_release_v1(
                  &consumed_capability),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
      capability != NULL ||
      broker == NULL ||
      Require(open64_fhe_broker_destroy_v1(&broker),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_broker_destroy_v1(&broker),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
      Require(open64_fhe_mock_host_bootstrap_destroy_v1(&host),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_bootstrap_destroy_v1(&host),
              OPEN64_FHE_STATUS_INVALID_HANDLE))
    return 1;

  open64_fhe_launcher_capability_v1_t second = NULL;
  open64_fhe_launcher_capability_v1_t unknown =
      reinterpret_cast<open64_fhe_launcher_capability_v1_t>(uintptr_t(1));
  if (Require(open64_fhe_launcher_capability_acquire_v1(host, &second),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
      Require(open64_fhe_launcher_capability_release_v1(&unknown),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
      unknown == NULL)
    return 1;

  return 0;
}
