#!/usr/bin/env python3
"""Machine guards for the normative FHE runtime ABI v1 documentation."""

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[4]
ABI_PATH = ROOT / "doc" / "FHE-RUNTIME-C-ABI-V1-CONTRACT.md"
DSL_PATH = ROOT / "doc" / "FHE-DSL-INTEGRATION-PLAN.md"
ACE_PATH = ROOT / "doc" / "FHE-ACE-RTLIB-RUNTIME-DECISION.md"


class FheRuntimeAbiContractTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.abi = ABI_PATH.read_text(encoding="utf-8")
        cls.dsl = DSL_PATH.read_text(encoding="utf-8")
        cls.ace = ACE_PATH.read_text(encoding="utf-8")

    def test_model_create_uses_imported_complete_package(self):
        self.assertRegex(
            self.abi,
            r"open64_fhe_model_package_import_v1\(\s*"
            r"open64_fhe_broker_v1_t broker,\s*const void \*model_package_envelope,"
            r"\s*uint64_t envelope_size,\s*"
            r"open64_fhe_model_package_v1_t \*out_package\);",
        )
        self.assertRegex(
            self.abi,
            r"open64_fhe_model_create_v1\(\s*open64_fhe_context_v1_t context,\s*"
            r"open64_fhe_keyset_v1_t keyset,\s*"
            r"open64_fhe_model_package_v1_t package,",
        )
        self.assertNotIn("const open64_fhe_model_desc_v1 *desc", self.abi)
        for entry in (
            "semantic-event-schedule-v1.json",
            "abi-evaluation-call-census-v1.json",
            "weight-manifest-v1.json",
            "operation-descriptor-directory-v1.json",
            "rotation-manifest-v1.json",
            "key-requirement-manifest-v1.json",
        ):
            self.assertIn(entry, self.abi)

    def test_package_directory_has_exactly_six_entries_and_one_config(self):
        directory = self.abi.split("Model admission never looks up", 1)[1]
        directory = directory.split("### Normative schemas", 1)[0]
        names = re.findall(r"`([a-z-]+-v1\.json)`", directory)
        self.assertEqual(
            [
                "semantic-event-schedule-v1.json",
                "abi-evaluation-call-census-v1.json",
                "weight-manifest-v1.json",
                "operation-descriptor-directory-v1.json",
                "rotation-manifest-v1.json",
                "key-requirement-manifest-v1.json",
            ],
            names,
        )
        self.assertIn("the name set has exactly these six members\nand no seventh entry", directory)
        self.assertIn("`config_identity_sha256` (`sha256`)", directory)
        self.assertIn(
            "equals the outer\nmodel-package envelope header, the imported context, and the identically named\nfield in every entry",
            directory,
        )

    def test_six_model_json_schemas_are_exhaustive_and_cross_linked(self):
        schema = self.abi.split("### Normative schemas for the six JSON entries", 1)[1]
        schema = schema.split("`model_create` consumes", 1)[0]
        self.assertIn("no unknown or duplicate keys", schema)
        self.assertIn("integers use shortest decimal notation", schema)
        self.assertIn("including their\nfinal LF", schema)
        exact_fragments = (
            "`sequence_index:u32`, `semantic_event_id:sha256`, `static_ordinal:u32`, `visit_index:u32`",
            "Exactly 147 events sorted by `sequence_index` 0--146",
            "`schedule_sha256:sha256`, `symbols` (array), `static_total:u32`, `dynamic_total:u32`",
            "`tensor_identity_sha256:sha256`, `role:id`, `envelope_sha256:sha256`",
            "`descriptor_sha256:sha256`, `semantic_event_id:sha256`, `sequence_index:u32`, `static_ordinal:u32`",
            "`payload_sha256:sha256`, `payload_base64:b64`",
            "`signed_rotations` (`s32[]`), `by_descriptor` (array)",
            "`required_key_class_mask:u32`, `requirements` (array)",
            "top-level mask is the OR of record masks and is exactly `0x0000001f`",
        )
        for fragment in exact_fragments:
            self.assertIn(fragment, schema)
        self.assertEqual(2, schema.count("Exactly 147 records"))
        self.assertIn("Exactly 147 descriptors", schema)
        self.assertNotIn("Exactly 87 records", schema)
        census_tuples = re.findall(
            r"`\(open64_fhe_[a-z0-9_]+,(\d+),(\d+),(\d+)\)`", schema
        )
        self.assertEqual(9, len(census_tuples))
        self.assertEqual(87, sum(int(row[1]) for row in census_tuples))
        self.assertEqual(147, sum(int(row[2]) for row in census_tuples))
        self.assertIn("every descriptor is referenced exactly once", schema)
        self.assertIn("The rotation top-level union covers all 147 records", schema)

    def test_expanded_schedule_fields_and_cursor_are_frozen(self):
        fields = (
            "sequence_index",
            "semantic_event_id",
            "static_ordinal",
            "visit_index",
            "invocation_identity_sha256",
            "source_context_sha256",
            "operation_kind",
            "abi_symbol",
            "descriptor_sha256",
            "input_value_identity_sha256",
            "output_value_identity_sha256",
        )
        schedule_rows = re.findall(
            r"^\| `semantic-event-schedule-v1\.json` \|.*$",
            self.abi,
            re.MULTILINE,
        )
        self.assertEqual(1, len(schedule_rows))
        schedule_section = schedule_rows[0]
        for field in fields:
            self.assertIn(f"`{field}", schedule_section)
        self.assertNotIn("Every entry has exactly `semantic_event_id`", self.abi)
        self.assertNotIn("`input_value_ids`", self.abi)
        self.assertNotIn("`output_value_id`", self.abi)
        self.assertIn("initializes its cursor to sequence index zero", self.abi)
        self.assertIn("Only publication of its distinct owning output advances", self.abi)
        self.assertRegex(
            self.abi,
            r"`CALL_ORDER_MISMATCH`, any other recoverable error, and a fatal\s+"
            r"provider event leave it unchanged",
        )

    def test_common_descriptor_rotation_and_key_joins_are_closed(self):
        common = self.abi.split(
            "Every one of the nine operation kinds uses one common", 1
        )[1].split("The value-input order", 1)[0]
        for field in (
            "input_value_identity_sha256:sha256[]",
            "output_value_identity_sha256:sha256",
            "input_tensor_identity_sha256s:sha256[]",
            "output_tensor_identity_sha256:sha256",
            "input_layout_identity_sha256s:sha256[]",
            "output_layout_identity_sha256:sha256",
            "weight_tensor_identity_sha256:sha256-or-null",
            "bias_tensor_identity_sha256:sha256-or-null",
            "asset_tensor_identity_sha256s:sha256[]",
            "signed_rotations:s32[]",
            "key_requirement_identity_sha256:sha256",
            "`input_states` (array)",
            "`output_state` (object)",
            "`parameters` (object)",
        ):
            self.assertIn(field, common)
        self.assertEqual(1, self.abi.count("Every one of the nine operation kinds uses one common"))
        operation_table = self.abi.split("| Operation kind |", 1)[1].split(
            "Unknown operation kinds", 1
        )[0]
        for kind in (
            "CONV2D_PLAIN",
            "RESIDUAL_ADD",
            "BOOTSTRAP",
            "RELU_NORMALIZE",
            "RELU_POLY_STAGE",
            "RELU_RECONSTRUCT",
            "AVERAGE_POOL",
            "LAYOUT_CONVERT",
            "LINEAR_PLAIN",
        ):
            self.assertEqual(1, operation_table.count(f"`{kind}`"))
        self.assertIn("a descriptor that uses no rotations has exactly `[]`", common)
        self.assertIn("Every dynamic descriptor identity selects exactly one rotation record and one\nkey-requirement record", self.abi)
        self.assertIn("`AVERAGE_POOL` is never empty", self.abi)
        self.assertIn("must\nall match it", self.abi)
        self.assertNotIn("`key_requirement_sha256`", self.abi)
        self.assertIn("weight and folded bias required", operation_table)
        self.assertIn("weight and classifier bias required", operation_table)
        self.assertIn("There is no implicit asset", self.abi)

    def test_value_weight_and_final_output_joins_are_closed_world(self):
        schedule = re.findall(
            r"^\| `semantic-event-schedule-v1\.json` \|.*$",
            self.abi,
            re.MULTILINE,
        )[0]
        for field in (
            "`value_declarations`",
            "`producer_kind:id`",
            "`producer_sequence_index:u32-or-null`",
        ):
            self.assertIn(field, schedule)
        self.assertIn("Exactly 148 declarations sorted by unique value identity", schedule)
        self.assertIn("one `model_input` with null producer index", schedule)
        self.assertIn("one `event` declaration for each sequence index 0--146", schedule)
        self.assertIn("A `sha256` value is 64 lowercase hexadecimal", self.abi)
        self.assertIn("graph is closed and acyclic", self.abi)
        self.assertIn("per-kind ABI order and arity above", self.abi)
        self.assertIn("No undeclared value, second\nproducer, forward edge, self-edge", self.abi)
        self.assertIn("unique value and tensor identities, and producer\nassignments are bijective", self.abi)
        self.assertIn("OPEN64-FHE-VALUE-TENSOR-V1", self.abi)
        self.assertIn("OPEN64-FHE-VALUE-V1", self.abi)
        self.assertIn("must match exactly one\n`weight-manifest-v1.json.tensors[].tensor_identity_sha256`", self.abi)
        self.assertIn("they are integrity subfields of that tensor record", self.abi)
        self.assertIn("`invocation_identity_sha256` equals `operation_identity_sha256`", self.abi)
        self.assertIn("must equal the export binding's\n`final_output_identity_sha256` byte for byte", self.abi)
        self.assertIn("`output_shape` is\nexactly two positive `u32` values", self.abi)
        self.assertIn("`flatten_start_dim` and `flatten_end_dim` are `u32` values 1 and 3", self.abi)

    def test_c_descriptor_maps_every_value_tensor_and_layout_edge(self):
        struct = self.abi.split("typedef struct open64_fhe_operation_desc_v1", 1)[1]
        struct = struct.split("} open64_fhe_operation_desc_v1;", 1)[0]
        for field in (
            "uint32_t sequence_index;",
            "uint32_t operation_ordinal;",
            "uint32_t visit_index;",
            "uint32_t input_count;",
            "uint8_t semantic_event_id[32];",
            "uint8_t descriptor_sha256[32];",
            "uint8_t input_value_identity_sha256[2][32];",
            "uint8_t output_value_identity_sha256[32];",
            "uint8_t input_tensor_identity_sha256[2][32];",
            "uint8_t output_tensor_identity_sha256[32];",
            "uint8_t input_layout_identity_sha256[2][32];",
            "uint8_t output_layout_identity_sha256[32];",
        ):
            self.assertIn(field, struct)
        self.assertIn("sizeof(open64_fhe_operation_desc_v1) == 496", self.abi)
        self.assertIn("offsetof(open64_fhe_operation_desc_v1, payload) == 480", self.abi)
        mapping = self.abi.split("fixed-field view of that\nsame directory record", 1)[1]
        mapping = mapping.split("For each evaluation call", 1)[0]
        self.assertIn("`operation_ordinal`, and `visit_index`\nequal JSON", mapping)
        self.assertIn("`static_ordinal`", mapping)
        self.assertIn("C `input_tensor_identity_sha256` equal JSON\n`input_tensor_identity_sha256s`", mapping)
        self.assertIn("C `input_layout_identity_sha256` equal\nJSON `input_layout_identity_sha256s`", mapping)
        self.assertIn("every byte in an unused\nsecond row is zero", mapping)
        self.assertIn("address exactly\nthe decoded `payload_base64` bytes", mapping)

    def test_actual_handles_and_model_bound_assets_are_checked(self):
        self.assertRegex(
            self.abi,
            r"open64_fhe_model_bind_asset_v1\(\s*"
            r"open64_fhe_model_v1_t model,\s*"
            r"open64_fhe_plain_tensor_v1_t asset\);",
        )
        self.assertIn("same checked metadata path as\n`ciphertext_inspect`", self.abi)
        self.assertIn("parameter position 0 or 1 must equal the corresponding\ndescriptor array row", self.abi)
        self.assertIn("Explicit\nplain `weight`, `optional_bias`, and `coefficients` handles", self.abi)
        self.assertIn("adds one model-owned reference in an immutable table", self.abi)
        self.assertIn("caller may then release its importing reference", self.abi)
        self.assertIn("after the higher-priority handle, trust-domain, envelope, integrity,\nauthentication, and identity checks", self.abi)
        self.assertIn("A missing required asset returns\n`MODEL_REQUIREMENT_MISMATCH`", self.abi)
        self.assertIn("leaves the table unchanged and unsealed; the host may bind the\nmissing asset and retry", self.abi)
        self.assertIn("otherwise successful first\nimport atomically publishes its ciphertext and changes the table from open to\nsealed", self.abi)
        self.assertIn("Every other failed first import also leaves it unsealed", self.abi)
        self.assertNotIn("normalization asset. Every\nEach of", self.abi)
        self.assertIn("Model destroy releases all model-owned asset\nreferences", self.abi)
        self.assertIn("there is no process-global or\nprovider-private implicit asset", self.abi)

    def test_relu_profile_and_evaluation_schedule_are_carried_in_six_entries(self):
        directory_rows = re.findall(
            r"^\| `operation-descriptor-directory-v1\.json` \|.*$",
            self.abi,
            re.MULTILINE,
        )
        self.assertEqual(1, len(directory_rows))
        row = directory_rows[0]
        for field in (
            "`relu_profiles`",
            "`profile_identity_sha256:sha256`",
            "`evaluation_schedule_identity_sha256:sha256`",
            "`bootstrap_input_state:state`",
            "`bootstrap_output_state:state`",
            "`normalize_output_state:state`",
            "`reconstruction_input_states`",
            "`reconstruction_output_state:state`",
        ):
            self.assertIn(field, row)
        self.assertIn("exactly three records sorted by\n`stage_index:u32` 0, 1, 2", self.abi)
        self.assertIn("degrees are respectively 7, 15, and 13", self.abi)
        self.assertIn("OPEN64-FHE-RELU-SCHEDULE-V1", self.abi)
        self.assertIn("OPEN64-FHE-RELU-PROFILE-V1", self.abi)
        poly_row = re.search(r"^\| `RELU_POLY_STAGE` \|.*$", self.abi, re.MULTILINE).group(0)
        self.assertIn("`profile_identity_sha256`", poly_row)
        self.assertIn("`evaluation_schedule_identity_sha256`", poly_row)
        self.assertIn("operation-descriptor-directory entry carries the canonical\nReLU profile", self.ace)

    def test_schedule_census_and_transcripts_are_distinct(self):
        for name in (
            "semantic-event-schedule-v1.json",
            "abi-evaluation-call-census-v1.json",
            "transport-lifecycle-transcript-v1.json",
            "failure-transcript-v1.json",
        ):
            self.assertIn(name, self.abi)
        self.assertIn("**Successful ABI evaluation-call total** | **87** | **147**", self.abi)
        static = 13 + 5 + (6 * 11) + 1 + 1 + 1
        dynamic = 21 + 9 + (6 * 19) + 1 + 1 + 1
        self.assertEqual((87, 147), (static, dynamic))
        self.assertIn("It excludes imports, exports", self.abi)
        self.assertIn("It is not included in successful-call counts", self.abi)

    def test_flatten_has_one_unconditional_abi_call(self):
        self.assertIn(
            "The MVP always emits exactly one `open64_fhe_layout_convert_v1` call",
            self.abi,
        )
        self.assertIn("| Layout conversion for flatten | 1 | 1 |", self.abi)
        self.assertNotIn("certified_no_call", self.abi + self.dsl + self.ace)

    def test_status_is_fixed_width_and_header_is_c_linkable(self):
        self.assertIn("typedef uint32_t open64_fhe_status_v1;", self.abi)
        self.assertNotRegex(self.abi, r"typedef\s+enum\s+open64_fhe_status")
        self.assertIn('#ifdef __cplusplus\nextern "C" {', self.abi)
        self.assertIn('} /* extern "C" */', self.abi)
        self.assertIn("_Static_assert(sizeof(open64_fhe_status_v1) == 4", self.abi)
        self.assertIn("sizeof(open64_fhe_import_binding_v1) == 232", self.abi)
        self.assertIn("sizeof(open64_fhe_export_binding_v1) == 232", self.abi)
        self.assertIn("sizeof(open64_fhe_export_receipt_v1) == 104", self.abi)
        self.assertIn("sizeof(open64_fhe_broker_desc_v1) == 16", self.abi)
        self.assertIn("sizeof(open64_fhe_operation_desc_v1) == 496", self.abi)

    def test_envelope_layout_kind_and_key_bits_are_exact(self):
        expected_offsets = {
            "magic": 0,
            "envelope_version": 8,
            "header_size": 12,
            "kind": 16,
            "key_class_mask": 20,
            "provider_identity_sha256": 24,
            "config_identity_sha256": 56,
            "payload_length": 88,
            "payload_sha256": 96,
            "envelope_sha256": 128,
        }
        for field, offset in expected_offsets.items():
            self.assertRegex(self.abi, rf"\| {offset} \| {field} \|")
        for name, value in (
            ("PUBLIC_CONTEXT", "1"),
            ("KEYSET", "2"),
            ("MODEL_PACKAGE", "3"),
            ("PLAIN_TENSOR", "4"),
            ("CIPHERTEXT", "5"),
        ):
            self.assertIn(
                f"#define OPEN64_FHE_ENVELOPE_KIND_{name} UINT32_C({value})",
                self.abi,
            )
        for name, value in (
            ("PUBLIC", "0x00000001"),
            ("EVALUATION", "0x00000002"),
            ("RELINEARIZATION", "0x00000004"),
            ("ROTATION", "0x00000008"),
            ("BOOTSTRAP", "0x00000010"),
            ("SECRET", "0x80000000"),
        ):
            self.assertIn(f"#define OPEN64_FHE_KEY_CLASS_{name} UINT32_C({value})", self.abi)

    def test_broker_trust_is_explicit_before_dispatch(self):
        self.assertRegex(
            self.abi,
            r"open64_fhe_broker_create_v1\(\s*"
            r"open64_fhe_launcher_capability_v1_t \*privileged_launcher,\s*"
            r"const open64_fhe_broker_desc_v1 \*desc,",
        )
        broker_struct = self.abi.split("typedef struct open64_fhe_broker_desc_v1", 1)[1]
        broker_struct = broker_struct.split("} open64_fhe_broker_desc_v1;", 1)[0]
        self.assertNotIn("trusted_registry", broker_struct)
        self.assertIn("uint32_t flags;", broker_struct)
        self.assertIn("uint32_t reserved;", broker_struct)
        self.assertNotIn("const void *trust_anchor;", self.abi)
        self.assertNotIn("registry signature/MAC is verified", self.abi)
        self.assertIn("administrator/root-owned\ndeployment resource", self.abi)
        self.assertIn("Generated C,\nserver request handlers, and untrusted plugins never receive it", self.abi)
        self.assertIn("self-reported digest is not authentication", self.abi)
        self.assertIn("match its object records one-for-one", self.abi)
        self.assertIn("ACE key blobs are opaque", self.abi)
        self.assertNotIn("provider-independent classifier", self.abi)
        self.assertIn(
            "`deployment_identity_sha256` (`sha256`), `auth_key_ids` (`sha256[]`), and\n`records` (array)",
            self.abi,
        )
        self.assertIn(
            "`envelope_sha256:sha256`, `kind:u32` (one envelope-kind constant),\n`provider_identity_sha256:sha256`, `config_identity_sha256:sha256`",
            self.abi,
        )
        self.assertIn("OPEN64_FHE_STATUS_TRUST_FAILURE", self.abi)
        self.assertIn("OPEN64_FHE_STATUS_SECRET_KEY_FORBIDDEN", self.abi)

    def test_launcher_capability_is_one_shot_and_broker_owned(self):
        self.assertRegex(
            self.abi,
            r"open64_fhe_launcher_capability_acquire_v1\(\s*"
            r"open64_fhe_host_bootstrap_v1_t host_bootstrap,\s*"
            r"open64_fhe_launcher_capability_v1_t \*out_capability\);",
        )
        self.assertRegex(
            self.abi,
            r"open64_fhe_launcher_capability_release_v1\(\s*"
            r"open64_fhe_launcher_capability_v1_t \*capability\);",
        )
        self.assertIn("publish one owning `MINTED` capability containing those\nresources", self.abi)
        self.assertIn("Capabilities are one-shot and have\nno retain operation", self.abi)
        self.assertIn("atomically changes `MINTED -> CLAIMED`", self.abi)
        self.assertIn("every\nsuccess or failure consumes it as `CLAIMED -> CONSUMED`", self.abi)
        self.assertIn("pre-claim failures: they\ndo not consume the capability", self.abi)
        self.assertIn("Any partial\nfailure releases the verifier if retained, unmaps/closes the registry", self.abi)
        self.assertIn("wipes the tentative generation, nonce, and token in reverse\nacquisition order", self.abi)
        self.assertIn("atomically changes `MINTED -> CONSUMED`, nulls the caller slot", self.abi)
        self.assertIn("unmaps/closes\nthe registry, releases and wipes the verifier reference", self.abi)
        self.assertIn("Release of null/stale/consumed returns\n`INVALID_HANDLE`; release while claimed returns `BUSY`", self.abi)
        self.assertIn("retains a separate broker-owned reference to the non-\nexportable HMAC verifier", self.abi)
        self.assertIn("Every post-claim outcome then unmaps/closes", self.abi)
        self.assertIn("Broker destroy wipes the immutable copy, nonce/replay state", self.abi)
        self.assertIn("output/ABI/struct validation has\npriority over capability validation", self.abi)

    def test_import_export_bindings_have_no_output_hash_cycle(self):
        import_struct = self.abi.split("typedef struct open64_fhe_import_binding_v1", 1)[1]
        import_struct = import_struct.split("} open64_fhe_import_binding_v1;", 1)[0]
        export_struct = self.abi.split("typedef struct open64_fhe_export_binding_v1", 1)[1]
        export_struct = export_struct.split("} open64_fhe_export_binding_v1;", 1)[0]
        self.assertIn("expected_envelope_sha256", import_struct)
        self.assertNotIn("expected_envelope_sha256", export_struct)
        self.assertIn("final_output_identity_sha256", export_struct)
        self.assertIn('`"O64FHE-IMPORT-V1\\0" || principal || session || nonce ||', self.abi)
        self.assertIn('`"O64FHE-EXPORT-V1\\0" || principal || session || nonce ||', self.abi)
        self.assertIn("receipt_hmac_sha256", self.abi)
        self.assertIn("RFC 2104 HMAC with FIPS 180-4 SHA-256", self.abi)
        self.assertIn("does not consume the nonce, write a\ntranscript/diagnostic", self.abi)
        self.assertIn("only a formal successful export fills all three", self.abi)
        self.assertRegex(
            self.abi,
            r"open64_fhe_ciphertext_export_v1\([\s\S]*?"
            r"const open64_fhe_export_binding_v1 \*export_binding,[\s\S]*?"
            r"open64_fhe_export_receipt_v1 \*out_receipt\);",
        )

    def test_all_tokens_have_a_broker_trust_domain(self):
        self.assertIn("Every broker/context/keyset/model-package/model/plaintext/ciphertext handle", self.abi)
        self.assertIn("owning broker identity and broker generation", self.abi)
        self.assertIn("#define OPEN64_FHE_STATUS_TRUST_DOMAIN_MISMATCH UINT32_C(26)", self.abi)
        self.assertIn("returns\n  `TRUST_DOMAIN_MISMATCH` before provider dispatch", self.abi)
        self.assertIn("including a\n  poisoned token--has been released", self.abi)

    def test_normal_recoverable_and_fatal_state_rules_are_separate(self):
        self.assertIn("normal `ACTIVE -> CLOSED` path", self.abi)
        self.assertIn("fatal `ACTIVE -> POISONED -> CLOSED` path", self.abi)
        self.assertIn("public-token\n  census, total public-reference census", self.abi)
        self.assertIn("provider-object census unchanged", self.abi)
        self.assertIn("marks the context and every live child public token `POISONED`", self.abi)
        self.assertIn("provider-object census is\n  zero", self.abi)
        self.assertNotIn("invalidates or poisons", self.abi + self.ace)

    def test_diagnostic_and_output_buffer_rules_are_total(self):
        self.assertIn("set to zero before any other fallible work", self.abi)
        self.assertIn("Every other failure leaves the buffer unchanged and the field\nzero", self.abi)
        self.assertRegex(
            self.abi,
            r"With no applicable record, either query returns `NO_DIAGNOSTIC`",
        )
        self.assertIn("never create,\nclear, or overwrite either record", self.abi)
        self.assertIn("recognized token belonging to a poisoned context returns `CONTEXT_POISONED`", self.abi)
        self.assertRegex(
            self.abi,
            r"open64_fhe_get_last_broker_diagnostic_v1\(\s*"
            r"open64_fhe_broker_v1_t broker,",
        )
        self.assertIn("Before `out_broker`\nis published, only status is available", self.abi)
        self.assertIn("Export preflight is the explicit no-record/no-transcript exception", self.abi)
        self.assertIn("updates only that context record", self.abi)
        self.assertIn("updates only that\nbroker record", self.abi)
        self.assertIn("Exactly one record is updated per failed call", self.abi)
        self.assertIn("no published\nbroker/context owner validates return only status", self.abi)

    def test_fatal_ownership_does_not_promise_provider_values(self):
        self.assertIn("Success and recoverable failure\n  leave their semantic values", self.abi)
        self.assertIn("Fatal worker termination\ndestroys its ACE objects", self.ace)
        self.assertIn("preserves only poisoned public token bits and\nreference counts for release", self.ace)

    def test_provider_docs_reference_but_do_not_redefine_public_abi(self):
        signature = re.compile(r"open64_fhe_status_v1\s+open64_fhe_[a-z0-9_]+\s*\(")
        self.assertGreater(len(signature.findall(self.abi)), 10)
        self.assertFalse(signature.search(self.dsl))
        self.assertFalse(signature.search(self.ace))
        for text in (self.dsl, self.ace):
            self.assertIn("doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md", text)
        self.assertIn("exactly one flatten/layout conversion", self.dsl)
        self.assertIn("one mandatory scheduled ABI call", self.ace)

    def test_ace_pin_remains_fail_closed_and_server_has_no_secret_helper(self):
        self.assertIn("SYNC-6 remains blocked", self.ace)
        self.assertIn("exact source and build hashes", self.ace)
        self.assertRegex(
            self.ace,
            r"`Prepare_input`, key-generation services, and decrypting `Handle_output`",
        )
        self.assertIn("not linked into, loaded by, or invoked from the", self.ace)


if __name__ == "__main__":
    unittest.main()
