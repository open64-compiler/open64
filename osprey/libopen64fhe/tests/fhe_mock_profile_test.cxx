#include "open64_fhe_mock_test.h"
#include "open64_fhe_mock_sha256.h"

#include <stdint.h>
#include <string.h>
#include <vector>

static bool
Ok(open64_fhe_status_v1 status)
{
  return status == OPEN64_FHE_STATUS_OK;
}

static void
Fill(uint8_t digest[32], uint8_t value)
{
  memset(digest, value, 32);
}

static std::vector<uint8_t>
Envelope(uint32_t kind, const uint8_t provider[32], const uint8_t config[32],
         const char *payload)
{
  uint64_t payload_size = strlen(payload);
  std::vector<uint8_t> bytes(
      OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 + payload_size);
  uint64_t written = 0;
  if (!Ok(open64_fhe_mock_seal_envelope_v1(
          kind, 0, provider, config, payload, payload_size,
          bytes.data(), bytes.size(), &written)) || written != bytes.size())
    bytes.clear();
  return bytes;
}

static std::vector<uint8_t>
KeysetEnvelope(const uint8_t provider[32], const uint8_t config[32])
{
  const uint32_t key_mask = OPEN64_FHE_KEY_CLASS_PUBLIC |
      OPEN64_FHE_KEY_CLASS_EVALUATION |
      OPEN64_FHE_KEY_CLASS_RELINEARIZATION |
      OPEN64_FHE_KEY_CLASS_ROTATION |
      OPEN64_FHE_KEY_CLASS_BOOTSTRAP;
  const char payload[] = "mock-keyset";
  std::vector<uint8_t> bytes(
      OPEN64_FHE_ENVELOPE_HEADER_SIZE_V1 + sizeof(payload) - 1);
  uint64_t written = 0;
  if (!Ok(open64_fhe_mock_seal_envelope_v1(
          OPEN64_FHE_ENVELOPE_KIND_KEYSET, key_mask, provider, config,
          payload, sizeof(payload) - 1, bytes.data(), bytes.size(),
          &written)) || written != bytes.size())
    bytes.clear();
  return bytes;
}

static void
InitializeDescriptor(open64_fhe_operation_desc_v1 *desc, uint32_t sequence,
                     uint32_t kind, const uint8_t config[32],
                     const uint8_t input_value[32],
                     const uint8_t input_tensor[32],
                     const uint8_t input_layout[32])
{
  memset(desc, 0, sizeof(*desc));
  desc->abi_version = OPEN64_FHE_ABI_VERSION_V1;
  desc->struct_size = sizeof(*desc);
  desc->sequence_index = sequence;
  desc->operation_kind = kind;
  desc->operation_ordinal = sequence;
  desc->visit_index = 0;
  desc->input_count = kind == OPEN64_FHE_OP_RESIDUAL_ADD ||
                      kind == OPEN64_FHE_OP_RELU_RECONSTRUCT ? 2 : 1;
  Fill(desc->semantic_event_id, uint8_t(1 + sequence % 251));
  Fill(desc->descriptor_sha256, uint8_t(2 + sequence % 251));
  Fill(desc->operation_identity_sha256, uint8_t(3 + sequence % 251));
  memcpy(desc->config_identity_sha256, config, 32);
  memcpy(desc->input_value_identity_sha256[0], input_value, 32);
  memcpy(desc->input_tensor_identity_sha256[0], input_tensor, 32);
  memcpy(desc->input_layout_identity_sha256[0], input_layout, 32);
  if (desc->input_count == 2) {
    memcpy(desc->input_value_identity_sha256[1], input_value, 32);
    memcpy(desc->input_tensor_identity_sha256[1], input_tensor, 32);
    memcpy(desc->input_layout_identity_sha256[1], input_layout, 32);
  }
  Fill(desc->output_value_identity_sha256,
       uint8_t(4 + sequence % 251));
  Fill(desc->output_tensor_identity_sha256,
       uint8_t(5 + sequence % 251));
  Fill(desc->output_layout_identity_sha256,
       uint8_t(6 + sequence % 251));
  open64_fhe_mock_sha256(NULL, 0, desc->payload_sha256);
}

static open64_fhe_status_v1
Evaluate(uint32_t kind, open64_fhe_model_v1_t model,
         open64_fhe_ciphertext_v1_t input,
         open64_fhe_plain_tensor_v1_t weight,
         open64_fhe_plain_tensor_v1_t bias,
         const open64_fhe_operation_desc_v1 *desc,
         open64_fhe_ciphertext_v1_t *output)
{
  switch (kind) {
  case OPEN64_FHE_OP_CONV2D_PLAIN:
    return open64_fhe_conv2d_plain_v1(
        model, input, weight, bias, desc, output);
  case OPEN64_FHE_OP_RESIDUAL_ADD:
    return open64_fhe_residual_add_v1(
        model, input, input, desc, output);
  case OPEN64_FHE_OP_BOOTSTRAP:
    return open64_fhe_bootstrap_v1(model, input, desc, output);
  case OPEN64_FHE_OP_RELU_NORMALIZE:
    return open64_fhe_relu_normalize_v1(model, input, desc, output);
  case OPEN64_FHE_OP_RELU_POLY_STAGE:
    return open64_fhe_relu_poly_stage_v1(
        model, input, weight, desc, output);
  case OPEN64_FHE_OP_RELU_RECONSTRUCT:
    return open64_fhe_relu_reconstruct_v1(
        model, input, input, desc, output);
  case OPEN64_FHE_OP_AVERAGE_POOL:
    return open64_fhe_average_pool_v1(model, input, desc, output);
  case OPEN64_FHE_OP_LAYOUT_CONVERT:
    return open64_fhe_layout_convert_v1(model, input, desc, output);
  case OPEN64_FHE_OP_LINEAR_PLAIN:
    return open64_fhe_linear_plain_v1(
        model, input, weight, bias, desc, output);
  default:
    return OPEN64_FHE_STATUS_UNSUPPORTED;
  }
}

int
main()
{
  uint8_t deployment[32];
  uint8_t provider[32];
  uint8_t config[32];
  uint8_t profile[32];
  uint8_t manifest[32];
  uint8_t zero[32] = {};
  Fill(deployment, 1);
  Fill(provider, 2);
  Fill(config, 3);
  Fill(profile, 4);
  Fill(manifest, 5);

  std::vector<uint8_t> context_envelope = Envelope(
      OPEN64_FHE_ENVELOPE_KIND_PUBLIC_CONTEXT, provider, config, "context");
  std::vector<uint8_t> keyset_envelope = KeysetEnvelope(provider, config);
  std::vector<uint8_t> package_envelope = Envelope(
      OPEN64_FHE_ENVELOPE_KIND_MODEL_PACKAGE, zero, config, "package");
  std::vector<uint8_t> weight_envelope = Envelope(
      OPEN64_FHE_ENVELOPE_KIND_PLAIN_TENSOR, zero, config, "weight");
  std::vector<uint8_t> bias_envelope = Envelope(
      OPEN64_FHE_ENVELOPE_KIND_PLAIN_TENSOR, zero, config, "bias");
  std::vector<uint8_t> input_envelope = Envelope(
      OPEN64_FHE_ENVELOPE_KIND_CIPHERTEXT, provider, config, "input");
  if (context_envelope.empty() || keyset_envelope.empty() ||
      package_envelope.empty() || weight_envelope.empty() ||
      bias_envelope.empty() || input_envelope.empty())
    return 1;

  open64_fhe_host_bootstrap_v1_t host = NULL;
  open64_fhe_launcher_capability_v1_t capability = NULL;
  open64_fhe_broker_v1_t broker = NULL;
  open64_fhe_context_v1_t context = NULL;
  open64_fhe_keyset_v1_t keyset = NULL;
  open64_fhe_model_package_v1_t package = NULL;
  open64_fhe_model_v1_t model = NULL;
  open64_fhe_plain_tensor_v1_t weight = NULL;
  open64_fhe_plain_tensor_v1_t bias = NULL;
  open64_fhe_ciphertext_v1_t current = NULL;

  if (!Ok(open64_fhe_mock_host_bootstrap_create_v1(deployment, &host)))
    return 1;
  const std::vector<uint8_t> *trusted[] = {
    &context_envelope, &keyset_envelope, &package_envelope,
    &weight_envelope, &bias_envelope
  };
  for (uint32_t i = 0; i < sizeof(trusted) / sizeof(trusted[0]); ++i) {
    if (!Ok(open64_fhe_mock_host_trust_envelope_v1(
            host, trusted[i]->data(), trusted[i]->size())))
      return 1;
  }
  open64_fhe_broker_desc_v1 broker_desc = {
    OPEN64_FHE_ABI_VERSION_V1, sizeof(broker_desc), 0, 0
  };
  if (!Ok(open64_fhe_launcher_capability_acquire_v1(host, &capability)) ||
      !Ok(open64_fhe_broker_create_v1(
          &capability, &broker_desc, &broker)))
    return 1;
  open64_fhe_context_desc_v1 context_desc = {};
  context_desc.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  context_desc.struct_size = sizeof(context_desc);
  memcpy(context_desc.execution_profile_sha256, profile, 32);
  memcpy(context_desc.provider_identity_sha256, provider, 32);
  memcpy(context_desc.provider_manifest_sha256, manifest, 32);
  memcpy(context_desc.config_identity_sha256, config, 32);
  if (!Ok(open64_fhe_context_import_v1(
          broker, &context_desc, context_envelope.data(),
          context_envelope.size(), &context)) ||
      !Ok(open64_fhe_keyset_import_v1(
          context, keyset_envelope.data(), keyset_envelope.size(), &keyset)) ||
      !Ok(open64_fhe_model_package_import_v1(
          broker, package_envelope.data(), package_envelope.size(), &package)) ||
      !Ok(open64_fhe_model_create_v1(context, keyset, package, &model)) ||
      !Ok(open64_fhe_plain_tensor_import_v1(
          model, weight_envelope.data(), weight_envelope.size(), &weight)) ||
      !Ok(open64_fhe_plain_tensor_import_v1(
          model, bias_envelope.data(), bias_envelope.size(), &bias)))
    return 1;

  open64_fhe_import_binding_v1 import_binding = {};
  import_binding.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  import_binding.struct_size = sizeof(import_binding);
  Fill(import_binding.authenticated_principal_sha256, 7);
  Fill(import_binding.session_identity_sha256, 8);
  Fill(import_binding.request_nonce, 9);
  memcpy(import_binding.expected_envelope_sha256,
         input_envelope.data() + 128, 32);
  memcpy(import_binding.model_identity_sha256,
         package_envelope.data() + 128, 32);
  Fill(import_binding.auth_key_id_sha256, 10);
  Fill(import_binding.auth_tag_hmac_sha256, 11);
  if (!Ok(open64_fhe_ciphertext_import_v1(
          model, &import_binding, input_envelope.data(), input_envelope.size(),
          &current)))
    return 1;

  uint32_t kinds[147];
  uint32_t count = 0;
  for (uint32_t i = 0; i < 21; ++i)
    kinds[count++] = OPEN64_FHE_OP_CONV2D_PLAIN;
  for (uint32_t i = 0; i < 9; ++i)
    kinds[count++] = OPEN64_FHE_OP_RESIDUAL_ADD;
  for (uint32_t i = 0; i < 19; ++i) {
    kinds[count++] = OPEN64_FHE_OP_BOOTSTRAP;
    kinds[count++] = OPEN64_FHE_OP_RELU_NORMALIZE;
    kinds[count++] = OPEN64_FHE_OP_RELU_POLY_STAGE;
    kinds[count++] = OPEN64_FHE_OP_RELU_POLY_STAGE;
    kinds[count++] = OPEN64_FHE_OP_RELU_POLY_STAGE;
    kinds[count++] = OPEN64_FHE_OP_RELU_RECONSTRUCT;
  }
  kinds[count++] = OPEN64_FHE_OP_AVERAGE_POOL;
  kinds[count++] = OPEN64_FHE_OP_LAYOUT_CONVERT;
  kinds[count++] = OPEN64_FHE_OP_LINEAR_PLAIN;
  if (count != 147)
    return 1;

  uint8_t value_identity[32];
  uint8_t tensor_identity[32];
  uint8_t layout_identity[32];
  memcpy(value_identity, input_envelope.data() + 128, 32);
  memcpy(tensor_identity, value_identity, 32);
  memcpy(layout_identity, value_identity, 32);
  for (uint32_t sequence = 0; sequence < count; ++sequence) {
    open64_fhe_operation_desc_v1 desc;
    InitializeDescriptor(&desc, sequence, kinds[sequence], config,
                         value_identity, tensor_identity, layout_identity);
    open64_fhe_ciphertext_v1_t output = NULL;
    if (sequence == 0) {
      ++desc.sequence_index;
      if (Evaluate(kinds[sequence], model, current, weight, bias,
                   &desc, &output) != OPEN64_FHE_STATUS_CALL_ORDER_MISMATCH ||
          output != NULL)
        return 1;
      --desc.sequence_index;
    }
    if (!Ok(Evaluate(kinds[sequence], model, current, weight, bias,
                     &desc, &output)) || output == NULL)
      return 1;
    memcpy(value_identity, desc.output_value_identity_sha256, 32);
    memcpy(tensor_identity, desc.output_tensor_identity_sha256, 32);
    memcpy(layout_identity, desc.output_layout_identity_sha256, 32);
    if (!Ok(open64_fhe_ciphertext_release_v1(&current)))
      return 1;
    current = output;
  }

  open64_fhe_export_binding_v1 export_binding = {};
  export_binding.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  export_binding.struct_size = sizeof(export_binding);
  Fill(export_binding.authenticated_principal_sha256, 7);
  Fill(export_binding.session_identity_sha256, 8);
  Fill(export_binding.request_nonce, 12);
  memcpy(export_binding.model_identity_sha256,
         package_envelope.data() + 128, 32);
  memcpy(export_binding.final_output_identity_sha256, value_identity, 32);
  Fill(export_binding.auth_key_id_sha256, 10);
  Fill(export_binding.auth_tag_hmac_sha256, 13);
  open64_fhe_export_receipt_v1 receipt = {};
  receipt.abi_version = OPEN64_FHE_ABI_VERSION_V1;
  receipt.struct_size = sizeof(receipt);
  uint64_t output_size = 0;
  if (open64_fhe_ciphertext_export_v1(
          model, current, &export_binding, NULL, 0, &output_size,
          &receipt) != OPEN64_FHE_STATUS_BUFFER_TOO_SMALL || output_size == 0)
    return 1;
  std::vector<uint8_t> output_envelope(output_size);
  if (!Ok(open64_fhe_ciphertext_export_v1(
          model, current, &export_binding, output_envelope.data(),
          output_envelope.size(), &output_size, &receipt)) ||
      !Ok(open64_fhe_ciphertext_release_v1(&current)) ||
      !Ok(open64_fhe_plain_tensor_release_v1(&bias)) ||
      !Ok(open64_fhe_plain_tensor_release_v1(&weight)) ||
      !Ok(open64_fhe_model_destroy_v1(&model)) ||
      !Ok(open64_fhe_model_package_release_v1(&package)) ||
      !Ok(open64_fhe_keyset_release_v1(&keyset)) ||
      !Ok(open64_fhe_context_destroy_v1(&context)) ||
      !Ok(open64_fhe_broker_destroy_v1(&broker)) ||
      !Ok(open64_fhe_mock_host_bootstrap_destroy_v1(&host)))
    return 1;
  return 0;
}
