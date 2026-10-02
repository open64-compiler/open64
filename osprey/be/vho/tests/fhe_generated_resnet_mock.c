/*
 * Link and execute the six-PU whirl2c ResNet against the standalone FHE mock.
 * The trace supplies execution order; the approved schedule is checked by the
 * test script. See doc/FHE-SYNC5-NATIVE-OWNERSHIP-AUDIT.md, S5-H, and
 * doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md. No ciphertext computation is claimed.
 */

#include "open64_fhe_mock_test.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "secure_resnet20.mid.w2c.c"

#define TRACE_COUNT 147
#define PLAIN_COUNT 47
#define ENVELOPE_CAPACITY 256

typedef struct {
  uint32_t ordinal;
  uint32_t kind;
  uintptr_t input0;
  uintptr_t input1;
  uintptr_t output;
} TRACE_EVENT;

typedef struct {
  uintptr_t token;
  uint8_t value[32];
  uint8_t tensor[32];
  uint8_t layout[32];
} TRACE_IDENTITY;

static TRACE_EVENT trace_events[TRACE_COUNT];
static TRACE_IDENTITY trace_identities[TRACE_COUNT + 1];
static uint32_t trace_identity_count;
static const uint8_t empty_payload_sha256[32] = {
  0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14,
  0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24,
  0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c,
  0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55
};

/* Keep one deterministic digest per token and identity dimension. */
static void
Fill_Digest(uint8_t digest[32], uint32_t sequence, uint8_t salt)
{
  uint32_t i;
  for (i = 0; i < 32; ++i)
    digest[i] = (uint8_t)(salt + sequence * 7U + i * 3U);
}

/* Find a prior result or the imported input without guessing graph topology. */
static const TRACE_IDENTITY *
Find_Identity(uintptr_t token)
{
  uint32_t i;
  for (i = 0; i < trace_identity_count; ++i)
    if (trace_identities[i].token == token)
      return &trace_identities[i];
  return NULL;
}

/* Parse the exact paired SELECT/EVAL records emitted by generated C. */
static int
Read_Trace(const char *path)
{
  FILE *file = fopen(path, "r");
  uint32_t i;
  char line[256];
  if (file == NULL)
    return 0;
  for (i = 0; i < TRACE_COUNT; ++i) {
    unsigned long selected_anchor, input0, input1, output;
    unsigned sequence, eval_sequence, ordinal, kind;
    if (fgets(line, sizeof(line), file) == NULL ||
        sscanf(line, "SELECT %u %u %u %lu", &sequence, &ordinal, &kind,
               &selected_anchor) != 4 ||
        fgets(line, sizeof(line), file) == NULL ||
        sscanf(line, "EVAL %u %lu %lu %lu", &eval_sequence, &input0,
               &input1, &output) != 4 ||
        sequence != i + 1 || eval_sequence != sequence ||
        selected_anchor != input0 || ordinal == 0 || kind == 0 ||
        kind > OPEN64_FHE_OP_LINEAR_PLAIN || output == 0) {
      fclose(file);
      return 0;
    }
    trace_events[i].ordinal = ordinal;
    trace_events[i].kind = kind;
    trace_events[i].input0 = (uintptr_t)input0;
    trace_events[i].input1 = (uintptr_t)input1;
    trace_events[i].output = (uintptr_t)output;
  }
  if (fgets(line, sizeof(line), file) == NULL ||
      strncmp(line, "SUMMARY selects=147 evals=147 output=set",
              strlen("SUMMARY selects=147 evals=147 output=set")) != 0 ||
      fgets(line, sizeof(line), file) != NULL) {
    fclose(file);
    return 0;
  }
  fclose(file);
  return 1;
}

/* Seal one typed test envelope without giving the generated code mock APIs. */
static int
Seal_Envelope(uint32_t kind, uint32_t key_mask,
              const uint8_t provider[32], const uint8_t config[32],
              const char *payload, uint8_t bytes[ENVELOPE_CAPACITY],
              uint64_t *size)
{
  return open64_fhe_mock_seal_envelope_v1
             (kind, key_mask, provider, config, payload, strlen(payload),
              bytes, ENVELOPE_CAPACITY, size) == OPEN64_FHE_STATUS_OK;
}

/* Materialize strict mock descriptors for the actual generated-C DAG edges. */
static int
Build_Descriptors(open64_fhe_operation_desc_v1 desc[TRACE_COUNT],
                  const uint8_t config[32], const uint8_t input_value[32])
{
  uint32_t i;
  trace_identity_count = 1;
  trace_identities[0].token = (uintptr_t)0x8000U;
  memcpy(trace_identities[0].value, input_value, 32);
  memcpy(trace_identities[0].tensor, input_value, 32);
  memcpy(trace_identities[0].layout, input_value, 32);
  for (i = 0; i < TRACE_COUNT; ++i) {
    const TRACE_EVENT *event = &trace_events[i];
    const TRACE_IDENTITY *first = Find_Identity(event->input0);
    const TRACE_IDENTITY *second = event->input1 == 0 ? NULL :
        Find_Identity(event->input1);
    TRACE_IDENTITY *result;
    if (first == NULL || (event->input1 != 0 && second == NULL) ||
        Find_Identity(event->output) != NULL)
      return 0;
    memset(&desc[i], 0, sizeof(desc[i]));
    desc[i].abi_version = OPEN64_FHE_ABI_VERSION_V1;
    desc[i].struct_size = sizeof(desc[i]);
    desc[i].sequence_index = i;
    desc[i].operation_kind = event->kind;
    desc[i].operation_ordinal = event->ordinal;
    desc[i].input_count = second == NULL ? 1 : 2;
    Fill_Digest(desc[i].semantic_event_id, i, 17);
    Fill_Digest(desc[i].descriptor_sha256, i, 31);
    Fill_Digest(desc[i].operation_identity_sha256, i, 47);
    memcpy(desc[i].config_identity_sha256, config, 32);
    memcpy(desc[i].input_value_identity_sha256[0], first->value, 32);
    memcpy(desc[i].input_tensor_identity_sha256[0], first->tensor, 32);
    memcpy(desc[i].input_layout_identity_sha256[0], first->layout, 32);
    if (second != NULL) {
      memcpy(desc[i].input_value_identity_sha256[1], second->value, 32);
      memcpy(desc[i].input_tensor_identity_sha256[1], second->tensor, 32);
      memcpy(desc[i].input_layout_identity_sha256[1], second->layout, 32);
    }
    result = &trace_identities[trace_identity_count++];
    result->token = event->output;
    Fill_Digest(result->value, i, 67);
    Fill_Digest(result->tensor, i, 83);
    Fill_Digest(result->layout, i, 101);
    memcpy(desc[i].output_value_identity_sha256, result->value, 32);
    memcpy(desc[i].output_tensor_identity_sha256, result->tensor, 32);
    memcpy(desc[i].output_layout_identity_sha256, result->layout, 32);
    memcpy(desc[i].payload_sha256, empty_payload_sha256, 32);
  }
  return trace_identity_count == TRACE_COUNT + 1;
}

/* Run the generated root after importing a complete typed mock environment. */
int
main(int argc, char **argv)
{
  uint8_t provider[32], config[32], profile[32], manifest[32];
  uint8_t deployment[32], zero[32] = {0};
  uint8_t context_bytes[ENVELOPE_CAPACITY], key_bytes[ENVELOPE_CAPACITY];
  uint8_t package_bytes[ENVELOPE_CAPACITY], input_bytes[ENVELOPE_CAPACITY];
  uint8_t plain_bytes[PLAIN_COUNT][ENVELOPE_CAPACITY];
  uint64_t context_size, key_size, package_size, input_size;
  uint64_t plain_size[PLAIN_COUNT];
  char plain_name[PLAIN_COUNT][32];
  const uint32_t key_mask = OPEN64_FHE_KEY_CLASS_PUBLIC |
      OPEN64_FHE_KEY_CLASS_EVALUATION |
      OPEN64_FHE_KEY_CLASS_RELINEARIZATION |
      OPEN64_FHE_KEY_CLASS_ROTATION |
      OPEN64_FHE_KEY_CLASS_BOOTSTRAP;
  open64_fhe_operation_desc_v1 descriptors[TRACE_COUNT];
  open64_fhe_host_bootstrap_v1_t host = NULL;
  open64_fhe_launcher_capability_v1_t capability = NULL;
  open64_fhe_broker_v1_t broker = NULL;
  open64_fhe_context_v1_t context = NULL;
  open64_fhe_keyset_v1_t keyset = NULL;
  open64_fhe_model_package_v1_t package = NULL;
  open64_fhe_model_v1_t model = NULL;
  open64_fhe_plain_tensor_v1_t plain[PLAIN_COUNT] = {0};
  open64_fhe_ciphertext_v1_t input = NULL, output = NULL;
  open64_fhe_broker_desc_v1 broker_desc;
  open64_fhe_context_desc_v1 context_desc;
  open64_fhe_import_binding_v1 binding;
  open64_fhe_ciphertext_info_v1 info;
  uint32_t i;

  if (argc < 2 || argc > 3 || !Read_Trace(argv[1]))
    return 2;
  memset(provider, 2, 32);
  memset(config, 3, 32);
  memset(profile, 4, 32);
  memset(manifest, 5, 32);
  memset(deployment, 1, 32);
  if (!Seal_Envelope(OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, 0,
                     provider, config, "context", context_bytes,
                     &context_size) ||
      !Seal_Envelope(OPEN64_FHE_ENVELOPE_KIND_KEYSET, key_mask,
                     provider, config, "keyset", key_bytes, &key_size) ||
      !Seal_Envelope(OPEN64_FHE_ENVELOPE_KIND_MODEL_PACKAGE, 0,
                     zero, config, "package", package_bytes,
                     &package_size) ||
      !Seal_Envelope(OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT, 0,
                     provider, config, "input", input_bytes, &input_size) ||
      !Build_Descriptors(descriptors, config, input_bytes + 128))
    return 3;
  if (argc == 3 && strcmp(argv[2], "wrong-ordinal") == 0)
    descriptors[73].operation_ordinal++;
  for (i = 0; i < PLAIN_COUNT; ++i) {
    snprintf(plain_name[i], sizeof(plain_name[i]), "plain-role-%u", i);
    if (!Seal_Envelope(OPEN64_FHE_ENVELOPE_KIND_PLAIN_TENSOR, 0,
                       zero, config, plain_name[i], plain_bytes[i],
                       &plain_size[i]))
      return 4;
  }
  if (open64_fhe_mock_host_bootstrap_create_v1(deployment, &host) != 0 ||
      open64_fhe_mock_host_trust_envelope_v1
          (host, context_bytes, context_size) != 0 ||
      open64_fhe_mock_host_trust_envelope_v1
          (host, key_bytes, key_size) != 0 ||
      open64_fhe_mock_host_trust_envelope_v1
          (host, package_bytes, package_size) != 0)
    return 5;
  for (i = 0; i < PLAIN_COUNT; ++i)
    if (open64_fhe_mock_host_trust_envelope_v1
            (host, plain_bytes[i], plain_size[i]) != 0)
      return 6;
  memset(&broker_desc, 0, sizeof(broker_desc));
  broker_desc.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  broker_desc.struct_size = sizeof(broker_desc);
  memset(&context_desc, 0, sizeof(context_desc));
  context_desc.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  context_desc.struct_size = sizeof(context_desc);
  memcpy(context_desc.execution_profile_sha256, profile, 32);
  memcpy(context_desc.provider_identity_sha256, provider, 32);
  memcpy(context_desc.provider_manifest_sha256, manifest, 32);
  memcpy(context_desc.config_identity_sha256, config, 32);
  if (open64_fhe_launcher_capability_acquire_v1(host, &capability) != 0 ||
      open64_fhe_broker_create_v1(&capability, &broker_desc, &broker) != 0 ||
      open64_fhe_context_import_v1
          (broker, &context_desc, context_bytes, context_size,
           &context) != 0 ||
      open64_fhe_keyset_import_v1
          (context, key_bytes, key_size, &keyset) != 0 ||
      open64_fhe_model_package_import_v1
          (broker, package_bytes, package_size, &package) != 0 ||
      open64_fhe_mock_model_package_set_schedule_v1
          (package, descriptors, TRACE_COUNT) != 0 ||
      open64_fhe_model_create_v1(context, keyset, package, &model) != 0)
    return 7;
  for (i = 0; i < PLAIN_COUNT; ++i)
    if (open64_fhe_plain_tensor_import_v1
            (model, plain_bytes[i], plain_size[i], &plain[i]) != 0)
      return 8;
  memset(&binding, 0, sizeof(binding));
  binding.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  binding.struct_size = sizeof(binding);
  Fill_Digest(binding.authenticated_principal_sha256, 0, 7);
  Fill_Digest(binding.session_identity_sha256, 0, 8);
  Fill_Digest(binding.request_nonce, 0, 9);
  memcpy(binding.expected_envelope_sha256, input_bytes + 128, 32);
  memcpy(binding.model_identity_sha256, package_bytes + 128, 32);
  Fill_Digest(binding.auth_key_id_sha256, 0, 10);
  Fill_Digest(binding.auth_tag_hmac_sha256, 0, 11);
  if (open64_fhe_ciphertext_import_v1
          (model, &binding, input_bytes, input_size, &input) != 0)
    return 9;
  if (argc == 3 && strcmp(argv[2], "missing-resource") == 0)
    plain[41] = NULL;
  if (argc == 3 && strcmp(argv[2], "missing-coefficient") == 0)
    plain[44] = NULL;
  if (argc == 3 && strcmp(argv[2], "wrong-handle") == 0)
    plain[41] = (open64_fhe_plain_tensor_v1_t)input;

  SecureResNet20(input, &output,
                 plain[0], plain[1], plain[2], plain[3],
                 plain[4], plain[5], plain[6], plain[7],
                 plain[8], plain[9], plain[10], plain[11],
                 plain[12], plain[13], plain[14], plain[15],
                 plain[16], plain[17], plain[18], plain[19],
                 plain[20], plain[21], plain[22], plain[23],
                 plain[24], plain[25], plain[26], plain[27],
                 plain[28], plain[29], plain[30], plain[31],
                 plain[32], plain[33], plain[34], plain[35],
                 plain[36], plain[37], plain[38], plain[39],
                 plain[40], plain[41], plain[42], plain[43],
                 model, plain[44], plain[45], plain[46]);
  if (argc == 3) {
    printf("MOCK negative=%s output=%s\n", argv[2],
           output == NULL ? "null" : "set");
    return output == NULL ? 0 : 1;
  }
  memset(&info, 0, sizeof(info));
  info.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  info.struct_size = sizeof(info);
  if (output == NULL || open64_fhe_ciphertext_inspect_v1(output, &info) != 0 ||
      info.level == 0 || info.scale_bits != 56 || info.components != 2)
    return 10;
  printf("MOCK positive=147 output=set level=%u scale=%d components=%u\n",
         info.level, info.scale_bits, info.components);
  if (open64_fhe_ciphertext_release_v1(&output) != 0 || output != NULL)
    return 11;
  return 0;
}
