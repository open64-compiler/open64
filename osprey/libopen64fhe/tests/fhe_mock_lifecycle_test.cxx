#include "open64_fhe_mock_test.h"
#include "open64_fhe_mock_sha256.h"

#include <stdint.h>
#include <string.h>
#include <vector>

static int
Require(open64_fhe_status_v1 actual, open64_fhe_status_v1 expected)
{
  return actual == expected ? 0 : 1;
}

int
main()
{
  static const uint8_t abc_sha256[32] = {
    0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea,
    0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
    0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c,
    0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad
  };
  uint8_t observed_sha256[32];
  open64_fhe_mock_sha256("abc", 3, observed_sha256);
  if (memcmp(observed_sha256, abc_sha256, sizeof(abc_sha256)) != 0)
    return 1;

  uint8_t deployment[32] = {};
  uint8_t provider[32] = {};
  uint8_t config[32] = {};
  uint8_t profile[32] = {};
  uint8_t provider_manifest[32] = {};
  uint8_t other_provider[32] = {};
  uint8_t other_config[32] = {};
  deployment[0] = 1;
  provider[0] = 2;
  config[0] = 3;
  profile[0] = 4;
  provider_manifest[0] = 5;
  other_provider[0] = 6;
  other_config[0] = 7;
  const uint8_t context_payload[] = { 7, 8, 9, 10 };
  uint64_t context_envelope_size =
      OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 + sizeof(context_payload);
  std::vector<uint8_t> context_envelope(context_envelope_size);
  std::vector<uint8_t> wrong_kind_envelope(context_envelope_size);
  std::vector<uint8_t> wrong_provider_envelope(context_envelope_size);
  std::vector<uint8_t> wrong_config_envelope(context_envelope_size);
  std::vector<uint8_t> keyset_envelope(context_envelope_size);
  std::vector<uint8_t> secret_keyset_envelope(context_envelope_size);
  uint64_t actual_size = 0;
  open64_fhe_host_bootstrap_v1_t host = NULL;
  open64_fhe_launcher_capability_v1_t capability = NULL;
  open64_fhe_launcher_capability_v1_t consumed_capability = NULL;
  open64_fhe_broker_v1_t broker = NULL;
  open64_fhe_context_v1_t context = NULL;
  open64_fhe_broker_desc_v1 desc = {
    OPEN64_FHE_ABI_VERSION_V1,
    sizeof(open64_fhe_broker_desc_v1),
    0,
    0
  };

  if (Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, 0,
                  provider, config, context_payload, sizeof(context_payload),
                  context_envelope.data(), context_envelope.size(),
                  &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      actual_size != context_envelope_size ||
      Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT, 0,
                  provider, config, context_payload, sizeof(context_payload),
                  wrong_kind_envelope.data(), wrong_kind_envelope.size(),
                  &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, 0,
                  other_provider, config, context_payload,
                  sizeof(context_payload), wrong_provider_envelope.data(),
                  wrong_provider_envelope.size(), &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, 0,
                  provider, other_config, context_payload,
                  sizeof(context_payload), wrong_config_envelope.data(),
                  wrong_config_envelope.size(), &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_KEYSET,
                  OPEN64_FHE_KEY_CLASS_PUBLIC |
                      OPEN64_FHE_KEY_CLASS_EVALUATION |
                      OPEN64_FHE_KEY_CLASS_RELINEARIZATION |
                      OPEN64_FHE_KEY_CLASS_ROTATION |
                      OPEN64_FHE_KEY_CLASS_BOOTSTRAP,
                  provider, config, context_payload, sizeof(context_payload),
                  keyset_envelope.data(), keyset_envelope.size(),
                  &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_KEYSET,
                  OPEN64_FHE_KEY_CLASS_SECRET,
                  provider, config, context_payload, sizeof(context_payload),
                  secret_keyset_envelope.data(),
                  secret_keyset_envelope.size(), &actual_size),
              OPEN64_FHE_STATUS_OK))
    return 1;

  if (Require(open64_fhe_mock_host_bootstrap_create_v1(deployment, &host),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, context_envelope.data(), context_envelope.size()),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, wrong_kind_envelope.data(),
                  wrong_kind_envelope.size()),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, wrong_provider_envelope.data(),
                  wrong_provider_envelope.size()),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, wrong_config_envelope.data(),
                  wrong_config_envelope.size()),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, keyset_envelope.data(), keyset_envelope.size()),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_launcher_capability_acquire_v1(host, &capability),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_mock_host_trust_envelope_v1(
                  host, context_envelope.data(), context_envelope.size()),
              OPEN64_FHE_STATUS_BUSY))
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
      capability != NULL || broker == NULL)
    return 1;

  open64_fhe_context_desc_v1 context_desc = {};
  context_desc.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  context_desc.struct_size = sizeof(context_desc);
  memcpy(context_desc.execution_profile_sha256, profile, 32);
  memcpy(context_desc.provider_identity_sha256, provider, 32);
  memcpy(context_desc.provider_manifest_sha256, provider_manifest, 32);
  memcpy(context_desc.config_identity_sha256, config, 32);

  std::vector<uint8_t> corrupt = context_envelope;
  corrupt.back() ^= 1;
  if (Require(open64_fhe_context_import_v1(
                  broker, &context_desc, corrupt.data(), corrupt.size(),
                  &context),
              OPEN64_FHE_STATUS_INTEGRITY_ERROR) ||
      context != NULL)
    return 1;

  std::vector<uint8_t> untrusted(context_envelope_size);
  const uint8_t untrusted_payload[] = { 7, 8, 9, 11 };
  if (Require(open64_fhe_mock_seal_envelope_v1(
                  OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, 0,
                  provider, config, untrusted_payload,
                  sizeof(untrusted_payload), untrusted.data(),
                  untrusted.size(), &actual_size),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_context_import_v1(
                  broker, &context_desc, untrusted.data(), untrusted.size(),
                  &context),
              OPEN64_FHE_STATUS_TRUST_FAILURE) ||
      context != NULL)
    return 1;

  open64_fhe_keyset_v1_t keyset = NULL;
  if (Require(open64_fhe_context_import_v1(
                  broker, &context_desc, wrong_kind_envelope.data(),
                  wrong_kind_envelope.size(), &context),
              OPEN64_FHE_STATUS_KIND_MISMATCH) ||
      Require(open64_fhe_context_import_v1(
                  broker, &context_desc, wrong_provider_envelope.data(),
                  wrong_provider_envelope.size(), &context),
              OPEN64_FHE_STATUS_PROVIDER_MISMATCH) ||
      Require(open64_fhe_context_import_v1(
                  broker, &context_desc, wrong_config_envelope.data(),
                  wrong_config_envelope.size(), &context),
              OPEN64_FHE_STATUS_CONFIG_MISMATCH) ||
      context != NULL)
    return 1;

  if (Require(open64_fhe_context_import_v1(
                  broker, &context_desc, context_envelope.data(),
                  context_envelope.size(), &context),
              OPEN64_FHE_STATUS_OK) ||
      context == NULL ||
      Require(open64_fhe_broker_destroy_v1(&broker),
              OPEN64_FHE_STATUS_BUSY) ||
      broker == NULL ||
      Require(open64_fhe_keyset_import_v1(
                  context, secret_keyset_envelope.data(),
                  secret_keyset_envelope.size(), &keyset),
              OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN) ||
      keyset != NULL)
    return 1;

  secret_keyset_envelope.back() ^= 1;
  if (Require(open64_fhe_keyset_import_v1(
                  context, secret_keyset_envelope.data(),
                  secret_keyset_envelope.size(), &keyset),
              OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN) ||
      Require(open64_fhe_keyset_import_v1(
                  context, context_envelope.data(), context_envelope.size(),
                  &keyset),
              OPEN64_FHE_STATUS_KIND_MISMATCH) ||
      Require(open64_fhe_keyset_import_v1(
                  context, keyset_envelope.data(), keyset_envelope.size(),
                  &keyset),
              OPEN64_FHE_STATUS_OK) ||
      keyset == NULL ||
      Require(open64_fhe_context_destroy_v1(&context),
              OPEN64_FHE_STATUS_BUSY) ||
      context == NULL ||
      Require(open64_fhe_keyset_release_v1(&keyset),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_keyset_release_v1(&keyset),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
      Require(open64_fhe_context_destroy_v1(&context),
              OPEN64_FHE_STATUS_OK) ||
      Require(open64_fhe_context_destroy_v1(&context),
              OPEN64_FHE_STATUS_INVALID_HANDLE) ||
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
