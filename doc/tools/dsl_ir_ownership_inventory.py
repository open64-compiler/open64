"""Inventory DSL persistence and current owning services without modifying compiler code."""
import argparse
import collections
import csv
import io
import subprocess
import hashlib
import json
from pathlib import Path
import re

ROOT = None
STAGE = None
FILES = ['osprey/common/com/dsl_ir_image.h', 'osprey/common/ir_image/dsl_ir_image.cxx', 'osprey/be/com/dsl_ir_rewrite.cxx', 'osprey/be/vho/dsl_ckks_expand.cxx', 'osprey/common/ir_image/dsl_ir_print.cxx', 'osprey/be/com/dsl_ir_lower.cxx', 'osprey/be/com/dsl_ir_group_lower.cxx', 'osprey/be/com/dsl_ir_retype.cxx', 'osprey/be/com/dsl_runtime_interface.cxx', 'osprey/be/com/dsl_program_interface.cxx', 'osprey/common/ir_image/dsl_pu_specialize_internal.h', 'osprey/common/ir_image/dsl_image_checkpoint_internal.h', 'osprey/common/ir_image/dsl_lowered_relation.h', 'osprey/common/ir_image/dsl_image_service.h', 'osprey/common/util/dsl_strtab_checkpoint.h', 'osprey/be/com/dsl_external_tensor.h', 'osprey/be/com/dsl_ir_rewrite.h', 'osprey/be/com/dsl_ir_rewrite_queries_internal.h', 'osprey/be/com/dsl_ir_lower.h', 'osprey/be/com/dsl_ir_retype.h', 'osprey/be/com/dsl_runtime_interface.h', 'osprey/be/com/dsl_program_interface.h', 'osprey/be/com/dsl_pu_interface_verify.h', 'osprey/be/com/dsl_ir_lower_internal.h', 'osprey/be/com/dsl_ir_transaction_internal.h', 'osprey/be/com/dsl_runtime_interface_internal.h', 'osprey/be/com/dsl_program_interface_internal.h', 'osprey/be/vho/dsl_ckks_expand.h', 'osprey/be/vho/dsl_ckks_expand_internal.h']


def nodes(path):
    """Decode Clang's concatenated JSON roots without assuming one outer array."""
    text = path.read_text()
    decoder = json.JSONDecoder()
    offset = 0
    while offset < len(text):
        while offset < len(text) and text[offset].isspace():
            offset += 1
        if offset == len(text):
            break
        node, offset = decoder.raw_decode(text, offset)
        yield node


def location(value, fallback=None):
    """Prefer the macro spelling site while inheriting omitted expansion filenames."""
    if 'spellingLoc' in value:
        return location(value['spellingLoc'], fallback)
    result = dict(value, file=value.get('file', fallback))
    if ROOT is not None and result.get('file'):
        candidate = (ROOT / result['file']).resolve()
        if candidate.is_relative_to(ROOT):
            result['file'] = str(candidate.relative_to(ROOT))
    return result


def collect():
    """Deduplicate source occurrences and include explicit comparator methods."""
    result = {}
    for ast_name in sorted(path.name for path in STAGE.glob('*.ast.json')):
        for node in nodes(STAGE / ast_name):
            loc = location(node.get('loc', {}))
            path = loc.get('file')
            if path not in FILES or node.get('isImplicit'):
                continue
            kind = node['kind']
            name = node.get('name')
            if kind in ('CXXRecordDecl', 'EnumDecl') and not name:
                continue
            if kind not in ('TypedefDecl', 'CXXRecordDecl', 'EnumDecl', 'VarDecl', 'FunctionDecl', 'FunctionTemplateDecl'):
                continue
            text = (ROOT / path).read_text()
            begin = location(node['range']['begin'], path)
            end = location(node['range']['end'], path)
            if 'spellingLoc' in node.get('loc', {}):
                start = node['loc']['expansionLoc']['offset']
                finish = text.find('\n\n', start)
                if finish < 0:
                    finish = text.find('\n', start)
            else:
                start = begin['offset']
                finish = end['offset'] + end.get('tokLen', 1)
            snippet = text[start:finish]
            if kind == 'TypedefDecl':
                category = 'record' if snippet.startswith('typedef struct') else 'enum' if snippet.startswith('typedef enum') else 'alias'
            elif kind == 'CXXRecordDecl':
                category = 'record'
            elif kind == 'EnumDecl':
                category = 'enum'
            elif kind in ('FunctionDecl', 'FunctionTemplateDecl'):
                category = 'method'
            else:
                category = 'global'
            line = text.count('\n', 0, loc['offset']) + 1
            body = any(item['kind'] == 'CompoundStmt' for item in node.get('inner', [])) or kind == 'FunctionTemplateDecl'
            entry = dict(name=name, path=path, line=line,
                start_line=text.count('\n', 0, start) + 1,
                end_line=text.count('\n', 0, finish) + 1,
                category=category, role='definition' if body or category != 'method' else 'declaration',
                signature=node.get('type', {}).get('qualType', ''), source=snippet,
                storage_class=node.get('storageClass', ''),
                uses=sorted(set(re.findall(r'\bDSL_[A-Za-z0-9_]+\b', snippet)) - {name}))
            result[(path, line, name)] = entry
            if kind == 'CXXRecordDecl':
                for child in node.get('inner', []):
                    if child['kind'] != 'CXXMethodDecl' or child.get('isImplicit'):
                        continue
                    child_loc = location(child['loc'], path)
                    child_line = text.count('\n', 0, child_loc['offset']) + 1
                    child_begin = location(child['range']['begin'], path)['offset']
                    child_end = location(child['range']['end'], path)
                    child_finish = child_end['offset'] + child_end.get('tokLen', 1)
                    child_source = text[child_begin:child_finish]
                    result[(path, child_line, name + '::' + child['name'])] = dict(name=name + '::' + child['name'], path=path,
                        line=child_line, start_line=text.count('\n', 0, child_begin) + 1,
                        end_line=text.count('\n', 0, child_finish) + 1,
                        category='method', role='definition', signature=child['type']['qualType'],
                        source=child_source, storage_class='',
                        uses=sorted(set(re.findall(r'\bDSL_[A-Za-z0-9_]+\b', child_source))))
    return sorted(result.values(), key=lambda item: (FILES.index(item['path']), item['line'], item['name']))


PERSISTENT = {
    'core': 'DSL_IR_IMAGE_HEADER DSL_IR_OPCODE_DESCRIPTOR_RECORD DSL_IR_NODE_RECORD DSL_IR_ATTRIBUTE_RECORD DSL_IR_VALUE_RECORD DSL_IR_VALUE_REFERENCE_RECORD',
    'effects': 'DSL_EFFECT_IMAGE_HEADER DSL_STATE_OBJECT_RECORD DSL_STATE_EFFECT_RECORD',
    'calls': 'DSL_CALL_IMAGE_HEADER DSL_PU_SOURCE_IDENTITY_RECORD DSL_CALLSITE_METADATA_RECORD',
    'call_abi': 'DSL_CALL_ABI_IMAGE_HEADER DSL_CALL_ARGUMENT_RECORD',
    'pu_interface': 'DSL_PU_INTERFACE_IMAGE_HEADER DSL_PU_FORMAL_RECORD',
    'runtime_interface': 'DSL_RUNTIME_INTERFACE_IMAGE_HEADER DSL_RUNTIME_VALUE_PROJECTION_RECORD DSL_RUNTIME_CALL_PROJECTION_RECORD',
    'program_interface': 'DSL_PROGRAM_INTERFACE_IMAGE_HEADER DSL_RETIRED_FORMAL_RECORD DSL_RETIRED_CALL_ARGUMENT_RECORD DSL_RUNTIME_INPUT_RECORD DSL_RUNTIME_INPUT_BINDING_RECORD DSL_RUNTIME_INPUT_CALL_RECORD'
}

GROUPS = {
    'type_dependency': dict(owner='Foreign WN/PU/map type dependencies of nonpersistent service declarations',
        destination=['osprey/common/whirl_pu/dsl_pu_interface_verify.h', 'osprey/common/ir_image/dsl_call_association.h'], batch='I1-schema',
        reason='Forward declarations/aliases are not persistent DSL definitions. Remove these dependencies from the persistent header and include or forward-declare them only in the actual owning service interfaces; do not relocate the underlying Open64 core types.',
        lifetime='Borrowed core compiler type identity; no new .B encoding.',
        tests=['osprey/common/com/tests/dsl_operator_layout_test.sh']),
    'logical_print': dict(owner='Logical DSL persistent-image inspection',
        destination=['osprey/common/ir_image/dsl_ir_print.h', 'osprey/common/ir_image/dsl_ir_print.cxx'], batch='I2-image-support',
        reason='Printer declaration is a service, not image data. Existing dsl_ir_print.cxx owns its implementation; inventory destination moves inspection logic together, not only the prototype.',
        lifetime='Readable coherent image plus caller-owned output stream; printing creates no new image record.',
        tests=['osprey/common/com/tests/dsl_ir_tools_smoke_test.sh']),
    'persistent_schema': dict(owner='Native WHIRL persistent DSL schema', destination=['osprey/common/com/dsl_ir_image.h'], batch='I1-schema',
        reason='Actual image header/row or symbolic encoding of its fields; no transient compiler request is admitted.',
        lifetime='Binary artifact plus corresponding shared ST/TY/string/TCON tables; preserve every field/ID/version/layout.',
        tests=['osprey/common/com/tests/dsl_operator_layout_test.sh', 'osprey/common/com/tests/dsl_ir_tools_smoke_test.sh']),
    'image_store': dict(owner='Managed fixed-row WHIRL image storage and structural record services', destination=['osprey/common/ir_image/dsl_image_store.h', 'osprey/common/ir_image/dsl_image_store.cxx'], batch='I2-image-support',
        reason='Direct persistent-row operations and their live containers/support are not additional .B records. Proposed specific image-store owner outside common/com, not a generic transient bucket.',
        lifetime='Image lifetime; IDs/borrowed rows expire on reset/replacement, and append rollback is not arbitrary old-row recovery.',
        tests=['osprey/common/com/tests/dsl_operator_layout_test.sh', 'osprey/common/com/tests/dsl_ir_tools_smoke_test.sh']),
    'image_layout_checks': dict(owner='Persistent image layout verification', destination=['osprey/common/ir_image/tests/dsl_image_layout_test.cxx'], batch='I1-schema',
        reason='Typedef-char assertions are compile-time checks, not persisted structures; retain/rebuild their exact ABI assertions outside the schema directory.',
        lifetime='Compile time only; no runtime state or .B bytes.',
        tests=['osprey/common/com/tests/dsl_operator_layout_test.sh']),
    'image_io': dict(owner='WHIRL mapped-image parse/validate/commit logic', destination=['osprey/common/ir_image/dsl_image_io.h', 'osprey/common/ir_image/dsl_image_io.cxx'], batch='I2-image-support',
        reason='Borrowed section views and their methods are process-local even when the pointed-to row bytes are persistent. Keep parse/commit logic with the views outside common/com.',
        lifetime='Mapped buffer readable/aligned through validation/commit; live store copied/updated only at existing commit boundary; no pointers persisted.',
        tests=['osprey/common/com/tests/dsl_ir_tools_smoke_test.sh', 'osprey/common/com/tests/dsl_runtime_interface_contract_test.sh', 'osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'call_association': dict(owner='Live WN-call to persisted-callsite binding and offset finalization', destination=['osprey/common/ir_image/dsl_call_association.h', 'osprey/common/ir_image/dsl_call_association.cxx'], batch='I3-call-bindings',
        reason='Live WN pointers and active map associations are not .B rows. Mixed row/association APIs must separate association ownership from pure row checks/access before migration.',
        lifetime='Borrow owning PU tree/map; rebind after mapped reopen, invalidate before tree/map release; no PU activation authority.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/common/com/tests/dsl_ir_tools_smoke_test.sh']),
    'physical_interface': dict(owner='Backend-free active-PU physical CALL/FUNC_ENTRY ABI verification', destination=['osprey/common/whirl_pu/dsl_pu_interface_verify.h', 'osprey/common/whirl_pu/dsl_pu_interface_verify.cxx'], batch='I4-pu-services',
        reason='Reads physical WNs/local ST tables and sorts formals; readers/builders also call this logic. Proposed specific WHIRL-PU verifier owner outside common/com must remain independent of backend code generation.',
        lifetime='Matching driver-established active PU, symtab, tree and map; never switch to other PUs.',
        tests=['osprey/common/com/tests/dsl_runtime_interface_contract_test.sh', 'osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'runtime_projection': dict(owner='Active-PU runtime-handle ABI projection transaction', destination=['osprey/be/com/dsl_runtime_interface.h', 'osprey/be/com/dsl_runtime_interface.cxx'], batch='I4-pu-services',
        reason='Requests/plans/results drive physical handle/formal/call/return rewrites; persisted projection records remain schema, not these transient aggregates.',
        lifetime='Caller-owned request arrays; one active-PU application; lifetime and output/failure behavior must survive relocation.',
        tests=['osprey/common/com/tests/dsl_runtime_interface_contract_test.sh']),
    'program_projection': dict(owner='Program-plan admission and active-PU interface evolution mechanism', destination=['osprey/be/com/dsl_program_interface.h', 'osprey/be/com/dsl_program_interface.cxx'], batch='I4-pu-services',
        reason='Retirement/resource plans, results and commit fingerprints are transient. Cross-PU planning policy is an IPA scope gate, not authority supplied by this mechanism.',
        lifetime='Prepared plan and image/PU commit interval, matching active PU for local interpretation; preserve reset/commit fingerprint semantics.',
        tests=['osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'external_materialize': dict(owner='Generic active-PU external-tensor materialization', destination=['osprey/be/com/dsl_external_tensor.h', 'osprey/be/com/dsl_external_tensor.cxx'], batch='I5-external-values',
        reason='Preflight/create/rewrite requests and result aggregates are not rows. Materialization mutates ST/WN/image together and reuses backend-free tensor fact access; domain-neutral construction is not inherently FHE.',
        lifetime='Active source/target PU and borrowed shared strings; side payload facts persist through TCON/ST metadata, not the C++ view/request.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh']),
    'typed_external': dict(owner='Typed external-tensor source capture, generation-qualified proof and materialization', destination=['osprey/be/com/dsl_typed_external_tensor.h', 'osprey/be/com/dsl_typed_external_tensor.cxx'], batch='I5-external-values',
        reason='Generation/index handle and copied source snapshot are transient; individual lineage facts persist in existing node attributes and associated metadata, not a lineage DTO. Source and target activation remains caller-owned.',
        lifetime='Snapshot survives caller-controlled PU switches but expires on image/source reset; no source pointer is persisted and no IPA scope is granted.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/be/vho/tests/fhe_ckks_conv_assets_test.cxx']),
    'generated_external': dict(owner='Source-free external-tensor generation/provenance materialization', destination=['osprey/be/com/dsl_generated_external_tensor.h', 'osprey/be/com/dsl_generated_external_tensor.cxx'], batch='I5-external-values',
        reason='Generation request/result is transient even though flags, generation strings and ordinals are preserved in existing node attributes and associated metadata.',
        lifetime='One active-PU transaction; returned metadata strings borrowed until mutation/reset; preserve all-or-nothing reported rejection behavior.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/be/vho/tests/fhe_ckks_conv_assets_test.cxx']),
    'native_rewrite': dict(owner='Generic active-PU native/logical rewrite and redirect/retire transaction', destination=['osprey/be/com/dsl_native_value_rewrite.h', 'osprey/be/com/dsl_native_value_rewrite.cxx'], batch='I6-native-transactions',
        reason='Request arrays, WN templates and use scans are transient transaction logic. Move helpers with the requests; pure row mutation is a separate image-store operation.',
        lifetime='One authorized active PU; borrowed definitions/templates, preflight then commit, owner-safe dominance/use and REGION/call checks.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/common/com/tests/dsl_fhe_sync3_native_rewrite_test.sh']),
    'shared_tensor_verify': dict(owner='Backend-free external-tensor/lineage/provenance fact access and active-PU verification',
        destination=['osprey/common/whirl_pu/dsl_external_tensor_verify.h', 'osprey/common/whirl_pu/dsl_external_tensor_verify.cxx'], batch='I4-pu-services',
        reason='Readers and native builders call typed/generated validation, which needs borrowed tensor/lineage/provenance views and parsing helpers. Move this actual verifier logic and its DTOs together outside common/com without adding a frontend-to-backend dependency.',
        lifetime='Caller-established active PU when required; returned strings borrow image/ST/TY/TCON metadata. Persisted facts remain in existing node attributes/tables, not these DTOs.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/common/com/tests/dsl_ir_tools_smoke_test.sh']),
    'native_lower': dict(owner='Generic active-PU native-to-standard-WHIRL group/dead-source lowering', destination=['osprey/be/com/dsl_ir_lower.h', 'osprey/be/com/dsl_ir_lower.cxx'], batch='I6-native-transactions',
        reason='Lowering modes, relation/result aggregates and detached-block requests are not image rows; result lookup derives a transient summary from persistent relations.',
        lifetime='Active PU and detached-block ownership; source rows retained as lowered/dead provenance; no executable work from transient proof alone.',
        tests=['osprey/common/com/tests/dsl_ckks_complete_group_lowering_test.sh', 'osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'type_refine': dict(owner='Active-PU atomic tensor type refinement transaction', destination=['osprey/be/com/dsl_ir_retype.h', 'osprey/be/com/dsl_ir_retype.cxx'], batch='I6-native-transactions',
        reason='Type-refinement request/result counts are transient; selected ST/TY/value updates become persistent only through existing tables.',
        lifetime='One active PU; expected-old-type preflight, array-level commit/rollback without adding an image schema.',
        tests=['osprey/common/com/tests/dsl_builder_contract_test.cxx']),
    'ckks_expand': dict(owner='VHO-owned CKKS executable expansion transaction', destination=['osprey/be/vho/fhe_ckks_expand.h', 'osprey/be/vho/fhe_ckks_expand.cxx'], batch='I7-ckks',
        reason='CKKS step/call/use/operand scans, expansion policy, savepoint and fault injection belong with the scheme-specific executable expansion, not generic persistent IR.',
        lifetime='Owner-PU expansion interval; event and source rows may persist, but journal/physical WNs/fault stage do not. Preserve partial rollback limitations.',
        tests=['osprey/common/com/tests/dsl_ckks_expand_transaction_test.sh']),
    'pu_clone': dict(owner='Generic physical/logical PU clone transaction mechanism', destination=['osprey/be/com/dsl_pu_transaction.h', 'osprey/be/com/dsl_pu_transaction.cxx'], batch='I8-pu-clone',
        reason='Clone value-pair maps and count-only journals are transient; keep data with clone/save/restore logic. Current cross-PU driver use requires separate IPA ownership review before any move.',
        lifetime='Source and clone contexts supplied by the owning transaction; intact same-image prefixes; append-only restore does not restore strings/ST/WN/domain stores.',
        tests=['osprey/common/com/tests/dsl_builder_contract_test.cxx']),
    'mixed_reset': dict(owner='Split persistent table lifetime from phase-owned transient invalidation', destination=['osprey/common/ir_image/dsl_image_store.cxx', 'osprey/be/com/dsl_typed_external_tensor.cxx', 'osprey/be/com/dsl_program_interface.cxx'], batch='I2-image-support',
        reason='Current reset clears persisted tables plus external-source handles or prepared-plan/commit state. Driver/owner must coordinate callbacks explicitly; image support must not acquire backend references.',
        lifetime='Whole-image lifetime boundary, not PU rollback; transient invalidation must still precede any ID reuse.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh', 'osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'mixed_runtime_input': dict(owner='Split persistent runtime-input validation from active-source tensor proof', destination=['osprey/common/ir_image/dsl_image_validate.cxx', 'osprey/common/whirl_pu/dsl_external_tensor_verify.cxx'], batch='I4-pu-services',
        reason='Current row predicate conditionally invokes a borrowed external-tensor view when the source PU is active. Reader-only row legality must not depend on backend transient types.',
        lifetime='Shared image validation independent of PU plus separately supplied active-PU proof; preserve current context-sensitive acceptance until explicitly reviewed.',
        tests=['osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'mixed_lower_relation': dict(owner='Split persistent lowered-relation validation from transient result adapters and CKKS-specific joins', destination=['osprey/common/ir_image/dsl_lowered_relation_validate.cxx', 'osprey/common/fhe/dsl_ckks_event.cxx', 'osprey/be/com/dsl_ir_lower.cxx'], batch='I2-image-support',
        reason='Persistent cross-table integrity checks currently consume a transient lower-result and CKKS event knowledge. Keep pure row joins reader-linkable, transient result construction backend-owned, scheme-specific persisted joins FHE-owned.',
        lifetime='Live/proposed coherent image families, prevalidated core link ranges; result object is derived and never a .B row.',
        tests=['osprey/common/com/tests/dsl_program_interface_contract_test.sh', 'osprey/common/com/tests/dsl_ckks_expand_transaction_test.sh']),
    'mixed_node_rewrite': dict(owner='Split transient logical rewrite request from persistent row/link mutation', destination=['osprey/be/com/dsl_native_value_rewrite.cxx', 'osprey/common/ir_image/dsl_image_store.cxx'], batch='I6-native-transactions',
        reason='Current node rewrite API accepts borrowed arrays in a nonpersistent request; row-range rebuilding and persistent identity changes must remain a narrow store primitive, not expose phase request types.',
        lifetime='Request borrowed during commit; no physical-WN atomicity from this row helper alone.',
        tests=['osprey/common/com/tests/dsl_external_tensor_rewrite_test.sh']),
    'mixed_mapped_commit': dict(owner='Split paired mapped-image commit from transient program-plan commit marking', destination=['osprey/common/ir_image/dsl_image_io.cxx', 'osprey/be/com/dsl_program_interface.cxx'], batch='I2-image-support',
        reason='Mapped load/commit invokes a nonpersistent program commit-state marker. Preserve existing admission/reset behavior without linking mapped readers to backend plan logic.',
        lifetime='Both borrowed sections valid through parse/cross-check/commit; commit-marker state is process-only.',
        tests=['osprey/common/com/tests/dsl_program_interface_contract_test.sh']),
    'external_region': dict(owner='Existing REGION symbol-use/redirect mechanism used by owner-PU transactions', destination=['osprey/be/com/dsl_region_transaction.cxx'], batch='I6-native-transactions',
        reason='Local extern declarations refer to out-of-scope REGION implementations; callers manipulate live PU trees and REGION state. Record the dependency; audit companion implementation before relocation.',
        lifetime='Owner PU and live REGION store; no automatic widening to IPA.',
        tests=['osprey/common/com/tests/dsl_ckks_expand_transaction_test.sh'])
}

ASSIGNMENTS = {
    'image_store': 'DSL_PROGRAM_INTERFACE_COUNT_GETTER',
    'logical_print': 'DSL_IR_Image_Print',
    'type_dependency': 'WN pu_info PU_Info WN_MAP WN_MAP_TAB',
    'call_association': '''DSL_CALLSITE_RUNTIME_ASSOCIATION DSL_callsite_runtime_associations IPA_WN_MAP32_Get Current_Map_Tab
DSL_Call_Image_Call_Has_Association DSL_Call_Image_Association_Valid DSL_Call_Image_Validate
DSL_Call_Image_Add_Callsite DSL_Call_Image_Find_Callsite DSL_Call_Image_Get_Call_WN
DSL_Call_Image_Replace_Call_WN DSL_Call_Image_Retarget_Call_WN DSL_Call_Image_Finalize_PU
DSL_Call_Image_Load_PU DSL_Call_Image_Reset DSL_Call_Image_Load_Mapped
DSL_Call_ABI_Image_Add_Argument DSL_Call_ABI_Image_Find_Argument DSL_Call_ABI_Image_Update_Argument_Value''',
    'image_io': '''DSL_IR_IMAGE_VIEW DSL_RUNTIME_INTERFACE_MAPPED_VIEW DSL_PROGRAM_INTERFACE_MAPPED_VIEW
DSL_Runtime_Interface_Find_Value_In_View DSL_Runtime_Interface_View_Validate
DSL_Runtime_Interface_Mapped_View_Parse DSL_Runtime_Interface_Mapped_View_Commit DSL_Runtime_Interface_Image_Load_Mapped
DSL_Program_Interface_Find_Retired_Formal_In_View DSL_Program_Interface_Find_Retired_Call_In_View
DSL_Program_Interface_Find_Binding_In_View DSL_Program_Interface_Find_Projection_In_View
DSL_Program_Interface_Find_Call_Projection_In_View DSL_Program_Interface_Live_Formal_Count
DSL_Program_Interface_Binding_Less DSL_Program_Interface_View_Validate DSL_Program_Interface_Validate_Runtime_View
DSL_Program_Interface_Mapped_View_Parse DSL_PROGRAM_INTERFACE_TAKE DSL_IR_Image_Node_Acyclic DSL_IR_Image_View_Validate
DSL_IR_Image_Validate DSL_IR_Image_Load_Mapped''',
    'physical_interface': '''DSL_PU_FORMAL_ORDINAL_LESS DSL_PU_FORMAL_ORDINAL_LESS::operator()
DSL_Call_ABI_PU_Report DSL_Call_ABI_Value_Matches_ST DSL_Call_ABI_Image_Validate_PU
DSL_PU_Interface_Image_Validate_PU''',
    'runtime_projection': '''DSL_RUNTIME_VALUE_PROJECTION_REQUEST DSL_RUNTIME_CALL_PROJECTION_REQUEST
DSL_RUNTIME_INTERFACE_PLAN DSL_RUNTIME_INTERFACE_RESULT DSL_Runtime_Interface_Plan_Validate
DSL_Runtime_Interface_Result_Init DSL_Runtime_Interface_Apply_PU DSL_Runtime_Interface_Validate_PU''',
    'program_projection': '''DSL_RETIRED_FORMAL_REQUEST DSL_RETIRED_CALL_ARGUMENT_REQUEST DSL_RUNTIME_INPUT_REQUEST
DSL_RUNTIME_INPUT_BINDING_REQUEST DSL_RUNTIME_INPUT_CALL_REQUEST DSL_PROGRAM_INTERFACE_PLAN DSL_PROGRAM_INTERFACE_RESULT
DSL_Program_Interface_Plan_Validate DSL_Program_Interface_Result_Init DSL_Program_Interface_Apply_PU
DSL_Program_Interface_Validate_PU DSL_Program_Interface_Validate_Lowered_PU DSL_PROGRAM_INTERFACE_INVALID_REQUEST_INDEX''',
    'external_materialize': '''DSL_IR_MATERIALIZE_SOURCE_POLICY
DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_REQUEST DSL_IR_EXTERNAL_TENSOR_MATERIALIZATION_RESULT
DSL_IR_External_Tensor_Materialization_Request_Init
DSL_IR_Materialize_External_Tensor_Values DSL_IR_External_Tensor_Request_Valid
DSL_IR_Copy_Tensor_Metadata DSL_IR_PU_Value_Name_Exists''',
    'shared_tensor_verify': '''DSL_IR_EXTERNAL_TENSOR_REFERENCE DSL_IR_TYPED_EXTERNAL_TENSOR_LINEAGE
DSL_IR_GENERATED_EXTERNAL_TENSOR_PROVENANCE DSL_IR_Image_Get_External_Tensor_Reference
DSL_IR_Image_Get_Typed_External_Tensor_Lineage DSL_IR_Image_Get_Generated_External_Tensor_Provenance
DSL_IR_Typed_External_Tensor_Validate_PU DSL_IR_Generated_External_Tensor_Validate_PU
DSL_IR_Parse_Unsigned DSL_IR_Checksum_Valid DSL_IR_Tensor_Element_Size DSL_IR_Static_Tensor_Byte_Size
DSL_IR_Transformation_Name_Valid DSL_IR_Lowercase_SHA256_Valid''',
    'typed_external': '''DSL_IR_EXTERNAL_TENSOR_SOURCE_HANDLE DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_REQUEST
DSL_IR_TYPED_EXTERNAL_TENSOR_VALUE_RESULT DSL_IR_EXTERNAL_SOURCE_SNAPSHOT
DSL_ir_external_source_snapshots DSL_ir_external_source_generation DSL_IR_External_Tensor_Source_Reset
DSL_IR_Capture_External_Tensor_Source DSL_IR_External_Tensor_Source_Snapshot
DSL_IR_Typed_External_Tensor_Value_Request_Init DSL_IR_Materialize_Typed_External_Tensor_Values
DSL_IR_Typed_External_Tensor_Request_Valid''',
    'generated_external': '''DSL_IR_GENERATED_EXTERNAL_TENSOR_REQUEST DSL_IR_GENERATED_EXTERNAL_TENSOR_RESULT
DSL_IR_Generated_External_Tensor_Request_Init
DSL_IR_Materialize_Generated_External_Tensor_Values DSL_IR_Generated_External_Tensor_Request_Valid''',
    'native_rewrite': '''DSL_IR_NODE_REWRITE_REQUEST DSL_IR_NATIVE_VALUE_REWRITE_REQUEST DSL_IR_NATIVE_VALUE_RETIRE_REQUEST DSL_IR_RETIRE_USE_SCAN
DSL_IR_Image_Find_Definition_Value DSL_IR_Rewrite_Native_Value DSL_IR_Redirect_And_Retire_Native_Value
DSL_IR_Redirect_And_Retire_Native_View DSL_IR_Rewrite_Attributes_Match_Schema DSL_IR_Find_Containing_Block
DSL_IR_Value_From_Call_Actual DSL_IR_Retire_Scan_Tree DSL_IR_Definition_Precedes
DSL_IR_Retire_Has_Use_Outside_Block DSL_IR_Retire_Has_Symbol_Use DSL_IR_Redirect_And_Retire_Native_Value_Internal''',
    'native_lower': '''DSL_IR_NATIVE_VALUE_LOWER_MODE DSL_IR_LOWER_RELATION_KIND DSL_IR_LOWER_RELATION
DSL_IR_NATIVE_VALUE_LOWER_REQUEST DSL_IR_NATIVE_VALUE_LOWER_RESULT DSL_IR_DEAD_NATIVE_CONSTANT_REQUEST
DSL_IR_NATIVE_GROUP_MEMBER_REQUEST DSL_IR_NATIVE_GROUP_LOWER_REQUEST DSL_IR_NATIVE_GROUP_LOWER_RESULT
DSL_IR_Lower_Native_Values_To_Standard_Blocks DSL_IR_Elide_Dead_Native_Constants
DSL_IR_Lower_Native_Value_Groups_To_Standard_Blocks''',
    'type_refine': '''DSL_IR_VALUE_TYPE_REFINEMENT_REQUEST DSL_IR_VALUE_TYPE_REFINEMENT_RESULT DSL_IR_Refine_Native_Value_Types''',
    'ckks_expand': '''DSL_IR_Image_CKKS_Save DSL_IR_Image_CKKS_Restore DSL_IR_CKKS_CALL_USE
DSL_IR_CKKS_USE_SCAN DSL_IR_CKKS_PHYSICAL_OPERAND DSL_ckks_expand_test_fault_stage DSL_CKKS_Expand_Set_Test_Fault
DSL_IR_CKKS_Report DSL_IR_CKKS_Attributes_Match DSL_IR_CKKS_Payload DSL_IR_CKKS_Scan_Tree
DSL_IR_CKKS_Preflight_Calls DSL_IR_CKKS_Operand_Dominates DSL_IR_CKKS_Has_Handle_Address_Use
DSL_IR_CKKS_Resolve_Existing_Operand DSL_IR_CKKS_Preflight DSL_IR_Can_Expand_Native_Value_To_CKKS_Events
DSL_IR_CKKS_Add_Image_Step DSL_IR_Expand_Native_Value_To_CKKS_Events''',
    'pu_clone': '''DSL_IR_Image_Clone_PU_Values DSL_IR_Image_Clone_PU_Restore''',
    'mixed_reset': '''DSL_IR_Image_Reset DSL_Program_Interface_Image_Reset''',
    'mixed_runtime_input': '''DSL_Program_Interface_Runtime_Input_Contract_Valid''',
    'mixed_lower_relation': '''DSL_IR_Image_Resolve_Lowered_Relation DSL_IR_Image_Validate_Lowered_Relations DSL_IR_Lowered_Relation_Views_Validate''',
    'mixed_node_rewrite': '''DSL_IR_Image_Rewrite_Node''',
    'mixed_mapped_commit': '''DSL_Program_Interface_Mapped_View_Commit DSL_Program_Interface_Image_Load_Mapped
DSL_Program_Runtime_Interface_Images_Load_Mapped''',
    'external_region': '''DSL_Region_Symbol_Use_Count DSL_Region_Can_Redirect_Symbol DSL_Region_Redirect_Symbol'''
}


REVIEWED_IMAGE_METHODS = set('''
DSL_Call_ABI_Image_Argument_Count
DSL_Call_ABI_Image_Callee_Formal_Count
DSL_Call_ABI_Image_Find_Argument_By_Id
DSL_Call_ABI_Image_Get_Argument
DSL_Call_ABI_Image_Get_Callee_Formal_Argument
DSL_Call_ABI_Image_Get_Header
DSL_Call_ABI_Image_Has_Records
DSL_Call_ABI_Image_Load_Mapped
DSL_Call_ABI_Image_Report
DSL_Call_ABI_Image_Reset
DSL_Call_ABI_Image_Shift_Arguments
DSL_Call_ABI_Image_Validate
DSL_Call_Image_Add_PU_Identity
DSL_Call_Image_Callsite_Count
DSL_Call_Image_Find_PU_Identity
DSL_Call_Image_Get_Callsite
DSL_Call_Image_Get_Header
DSL_Call_Image_Get_PU_Identity
DSL_Call_Image_Has_Records
DSL_Call_Image_PU_Has_Calls
DSL_Call_Image_PU_Identity_Count
DSL_Call_Image_Report
DSL_Effect_Image_Add_State_Effect
DSL_Effect_Image_Add_State_Object
DSL_Effect_Image_Get_Header
DSL_Effect_Image_Get_State_Effect
DSL_Effect_Image_Get_State_Object
DSL_Effect_Image_Has_Records
DSL_Effect_Image_Load_Mapped
DSL_Effect_Image_Report
DSL_Effect_Image_Reset
DSL_Effect_Image_State_Effect_Count
DSL_Effect_Image_State_Object_Count
DSL_Effect_Image_Valid_Effect_Kind
DSL_Effect_Image_Valid_State_Kind
DSL_Effect_Image_Validate
DSL_IR_Attribute_Record_Init
DSL_IR_Image_Add_Attribute
DSL_IR_Image_Add_Node
DSL_IR_Image_Add_Opcode_Descriptor
DSL_IR_Image_Add_Section_Size
DSL_IR_Image_Add_Value
DSL_IR_Image_Add_Value_Reference
DSL_IR_Image_Attribute_Count
DSL_IR_Image_Ensure_Opcode_Descriptor
DSL_IR_Image_Executable_Node_Count
DSL_IR_Image_Find_Opcode_Descriptor
DSL_IR_Image_Find_PU_Value
DSL_IR_Image_Find_Value
DSL_IR_Image_Get_Attribute
DSL_IR_Image_Get_Header
DSL_IR_Image_Get_Node
DSL_IR_Image_Get_Opcode_Descriptor
DSL_IR_Image_Get_Value
DSL_IR_Image_Get_Value_Reference
DSL_IR_Image_Has_Records
DSL_IR_Image_Lowercase_SHA256_Valid
DSL_IR_Image_Mark_Value_Dead_Elided
DSL_IR_Image_Mark_Value_Lowered
DSL_IR_Image_Node_Count
DSL_IR_Image_Opcode_Descriptor_Count
DSL_IR_Image_Redirect_And_Lower_Value
DSL_IR_Image_Redirect_And_Retire_Value
DSL_IR_Image_Report
DSL_IR_Image_Retype_Value
DSL_IR_Image_Set_Node_Links
DSL_IR_Image_String_Id_Valid
DSL_IR_Image_Value_Belongs_To_PU
DSL_IR_Image_Value_Count
DSL_IR_Image_Value_Redirect_Target
DSL_IR_Image_Value_Reference_Count
DSL_IR_Node_Attribute
DSL_IR_Node_Record_Init
DSL_IR_Opcode_Descriptor_Record_Init
DSL_IR_Record_Init
DSL_IR_Table_Get
DSL_IR_Value_Record_Init
DSL_IR_Value_Reference_Record_Init
DSL_PU_Interface_Image_Add_Formal
DSL_PU_Interface_Image_Find_Formal
DSL_PU_Interface_Image_Formal_Count
DSL_PU_Interface_Image_Get_Formal
DSL_PU_Interface_Image_Get_Header
DSL_PU_Interface_Image_Has_Records
DSL_PU_Interface_Image_Load_Mapped
DSL_PU_Interface_Image_Report
DSL_PU_Interface_Image_Reset
DSL_PU_Interface_Image_Shift_Formals
DSL_PU_Interface_Image_Validate
DSL_PU_Interface_PU_Report
DSL_Program_Interface_Image_Add_Retired_Call
DSL_Program_Interface_Image_Add_Retired_Formal
DSL_Program_Interface_Image_Add_Runtime_Binding
DSL_Program_Interface_Image_Add_Runtime_Call
DSL_Program_Interface_Image_Add_Runtime_Input
DSL_Program_Interface_Image_Find_Retired_Call
DSL_Program_Interface_Image_Find_Retired_Formal
DSL_Program_Interface_Image_Find_Runtime_Binding
DSL_Program_Interface_Image_Get_Header
DSL_Program_Interface_Image_Get_Retired_Call
DSL_Program_Interface_Image_Get_Retired_Formal
DSL_Program_Interface_Image_Get_Runtime_Binding
DSL_Program_Interface_Image_Get_Runtime_Call
DSL_Program_Interface_Image_Get_Runtime_Input
DSL_Program_Interface_Image_Has_Records
DSL_Program_Interface_Image_Retired_Call_Count
DSL_Program_Interface_Image_Retired_Formal_Count
DSL_Program_Interface_Image_Runtime_Binding_Count
DSL_Program_Interface_Image_Runtime_Call_Count
DSL_Program_Interface_Image_Runtime_Input_Count
DSL_Program_Interface_Image_Validate
DSL_Program_Interface_PU_Valid
DSL_Program_Interface_Pointer_TY_Contract_Valid
DSL_Program_Interface_Report
DSL_Program_Interface_TCON_Contract_Valid
DSL_Program_Interface_TY_Contract_Valid
DSL_Program_Interface_Tensor_TY_Contract_Valid
DSL_Runtime_Interface_Image_Add_Call
DSL_Runtime_Interface_Image_Add_Value
DSL_Runtime_Interface_Image_Call_Count
DSL_Runtime_Interface_Image_Can_Retire_View
DSL_Runtime_Interface_Image_Find_Call
DSL_Runtime_Interface_Image_Find_Value
DSL_Runtime_Interface_Image_Get_Call
DSL_Runtime_Interface_Image_Get_Header
DSL_Runtime_Interface_Image_Get_Value
DSL_Runtime_Interface_Image_Has_Records
DSL_Runtime_Interface_Image_Redirect_Value
DSL_Runtime_Interface_Image_Reset
DSL_Runtime_Interface_Image_Retire_View
DSL_Runtime_Interface_Image_Validate
DSL_Runtime_Interface_Image_Value_Count
DSL_Runtime_Interface_Report
DSL_Runtime_Interface_Value_Record_Contract_Valid
DSL_State_Effect_Kind_Name
DSL_State_Effect_Record_Init
DSL_State_Kind_Name
DSL_State_Object_Record_Init
'''.split())


REVIEWED_SCHEMA_SYMBOLS = set('''
DSL_CALLSITE_METADATA_ID
DSL_CALLSITE_METADATA_INVALID_ID
DSL_CALLSITE_METADATA_RECORD_SIZE
DSL_CALL_ABI_IMAGE_HEADER_SIZE
DSL_CALL_ABI_IMAGE_MAGIC
DSL_CALL_ABI_IMAGE_VERSION
DSL_CALL_ARGUMENT_ID
DSL_CALL_ARGUMENT_INVALID_ID
DSL_CALL_ARGUMENT_INVALID_ORDINAL
DSL_CALL_ARGUMENT_RECORD_SIZE
DSL_CALL_IMAGE_HEADER_SIZE
DSL_CALL_IMAGE_MAGIC
DSL_CALL_IMAGE_VERSION
DSL_EFFECT_IMAGE_HEADER_SIZE
DSL_EFFECT_IMAGE_MAGIC
DSL_EFFECT_IMAGE_VERSION
DSL_INTERFACE_RETIREMENT_REASON
DSL_IR_ATTRIBUTE_ID
DSL_IR_ATTRIBUTE_INVALID_ID
DSL_IR_ATTRIBUTE_RECORD_SIZE
DSL_IR_ATTRIBUTE_VALUE_KIND
DSL_IR_IMAGE_CAPABILITY
DSL_IR_IMAGE_HEADER_SIZE
DSL_IR_IMAGE_RECORD_KIND
DSL_IR_IMAGE_VERSION
DSL_IR_NODE_FLAG
DSL_IR_NODE_ID
DSL_IR_NODE_INVALID_ID
DSL_IR_NODE_RECORD_SIZE
DSL_IR_NODE_REDIRECT_ORDINAL_MASK
DSL_IR_NODE_REDIRECT_ORDINAL_SHIFT
DSL_IR_OPCODE_DESCRIPTOR_ID
DSL_IR_OPCODE_DESCRIPTOR_INVALID_ID
DSL_IR_OPCODE_DESCRIPTOR_RECORD_SIZE
DSL_IR_VALUE_FLAG
DSL_IR_VALUE_ID
DSL_IR_VALUE_INVALID_ID
DSL_IR_VALUE_KIND
DSL_IR_VALUE_RECORD_SIZE
DSL_IR_VALUE_REFERENCE_ID
DSL_IR_VALUE_REFERENCE_INVALID_ID
DSL_IR_VALUE_REFERENCE_RECORD_SIZE
DSL_PROGRAM_INTERFACE_IMAGE_HEADER_SIZE
DSL_PROGRAM_INTERFACE_IMAGE_MAGIC
DSL_PROGRAM_INTERFACE_IMAGE_VERSION
DSL_PU_FORMAL_ID
DSL_PU_FORMAL_INVALID_ID
DSL_PU_FORMAL_INVALID_ORDINAL
DSL_PU_FORMAL_RECORD_SIZE
DSL_PU_INTERFACE_IMAGE_HEADER_SIZE
DSL_PU_INTERFACE_IMAGE_MAGIC
DSL_PU_INTERFACE_IMAGE_VERSION
DSL_PU_SOURCE_IDENTITY_ID
DSL_PU_SOURCE_IDENTITY_INVALID_ID
DSL_PU_SOURCE_IDENTITY_RECORD_SIZE
DSL_RETIRED_CALL_ARGUMENT_ID
DSL_RETIRED_CALL_ARGUMENT_INVALID_ID
DSL_RETIRED_CALL_ARGUMENT_RECORD_SIZE
DSL_RETIRED_FORMAL_ID
DSL_RETIRED_FORMAL_INVALID_ID
DSL_RETIRED_FORMAL_RECORD_SIZE
DSL_RUNTIME_BINDING_KIND
DSL_RUNTIME_CALL_DIRECTION
DSL_RUNTIME_CALL_PROJECTION_ID
DSL_RUNTIME_CALL_PROJECTION_INVALID_ID
DSL_RUNTIME_CALL_PROJECTION_RECORD_SIZE
DSL_RUNTIME_INPUT_BINDING_ID
DSL_RUNTIME_INPUT_BINDING_INVALID_ID
DSL_RUNTIME_INPUT_BINDING_KIND
DSL_RUNTIME_INPUT_BINDING_RECORD_SIZE
DSL_RUNTIME_INPUT_CALL_ID
DSL_RUNTIME_INPUT_CALL_INVALID_ID
DSL_RUNTIME_INPUT_CALL_RECORD_SIZE
DSL_RUNTIME_INPUT_ID
DSL_RUNTIME_INPUT_INVALID_ID
DSL_RUNTIME_INPUT_KIND
DSL_RUNTIME_INPUT_RECORD_SIZE
DSL_RUNTIME_INTERFACE_IMAGE_HEADER_SIZE
DSL_RUNTIME_INTERFACE_IMAGE_MAGIC
DSL_RUNTIME_INTERFACE_IMAGE_VERSION
DSL_RUNTIME_INTERFACE_INVALID_ORDINAL
DSL_RUNTIME_VALUE_PROJECTION_FLAG
DSL_RUNTIME_VALUE_PROJECTION_ID
DSL_RUNTIME_VALUE_PROJECTION_INVALID_ID
DSL_RUNTIME_VALUE_PROJECTION_RECORD_SIZE
DSL_STATE_EFFECT_ID
DSL_STATE_EFFECT_INVALID_ID
DSL_STATE_EFFECT_KIND
DSL_STATE_EFFECT_RECORD_SIZE
DSL_STATE_KIND
DSL_STATE_OBJECT_FLAG
DSL_STATE_OBJECT_ID
DSL_STATE_OBJECT_INVALID_ID
DSL_STATE_OBJECT_RECORD_SIZE
'''.split())


def classify(entries):
    """Fail closed on unknown nonpersistent structures and account for every method."""
    assigned = {}
    for group, words in ASSIGNMENTS.items():
        for name in words.split():
            assert name not in assigned, name
            assigned[name] = group
    records = {name: family for family, words in PERSISTENT.items() for name in words.split()}
    assert len(records) == 25
    for entry in entries:
        name = entry['name']
        category = entry['category']
        family = records.get(name)
        if family:
            group = 'persistent_schema'
            persistence = 'persistent_record'
        elif name in assigned:
            group = assigned[name]
            persistence = 'method_not_data' if category == 'method' else 'nonpersistent'
        elif category == 'alias' and name.endswith('_Size_Check'):
            group, persistence = 'image_layout_checks', 'compile_time_only'
        elif category == 'include_guard':
            group, persistence = ('persistent_schema' if entry['path'].endswith('/dsl_ir_image.h') else 'image_store'), 'preprocessor_scaffolding_not_data'
        elif category in ('alias', 'enum', 'macro') and entry['path'].endswith('dsl_ir_image.h'):
            assert name in REVIEWED_SCHEMA_SYMBOLS, 'New symbolic type/constant needs persistence evidence: ' + name
            group, persistence = 'persistent_schema', 'persistent_schema_symbol'
        elif category == 'method':
            assert name in REVIEWED_IMAGE_METHODS or entry['path'] in FILES, 'Unscoped method: ' + name
            group, persistence = 'image_store', 'method_not_data'
        elif category == 'alias' and name.endswith('_TABLE') or category == 'global' and name.endswith('_table'):
            group, persistence = 'image_store', 'nonpersistent_container_of_persistent_rows'
        else:
            assert entry['path'] in FILES and not entry['path'].endswith('/dsl_ir_image.h'), entry
            group, persistence = 'image_store', 'nonpersistent'
        entry.update(group=group, persistence=persistence, persistence_family=family,
            decision='retain_schema' if group == 'persistent_schema' else 'split_before_relocating' if group.startswith('mixed_') or group == 'call_association' and category == 'method' and name in {
                'DSL_Call_Image_Validate', 'DSL_Call_Image_Add_Callsite', 'DSL_Call_Image_Reset', 'DSL_Call_Image_Load_Mapped',
                'DSL_Call_Image_Retarget_Call_WN', 'DSL_Call_ABI_Image_Add_Argument', 'DSL_Call_ABI_Image_Update_Argument_Value'} else 'relocate_to_owning_logic',
            proposed_destinations=GROUPS[group]['destination'], batch=GROUPS[group]['batch'])
    for entry in entries:
        entry['decision'] = 'retain_schema' if entry['group'] == 'persistent_schema' else 'owned_service'
        entry['proposed_destinations'] = [entry['path']]
        entry['owning_directory'] = str(Path(entry['path']).parent)
    return entries


LEX = re.compile(r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|/\*[\s\S]*?\*/|//[^\n]*')


def source_tokens(source):
    """Ignore comments/spacing without losing literals or joining identifiers."""
    pattern = LEX.pattern + r'|[A-Za-z_]\w*|\d+|[^\s]'
    return [match.group() for match in re.finditer(pattern, source)
            if not match.group().startswith(('/*', '//'))]


SECTION_NAMES = {
    'core': ('.WHIRL.dsl', 'WN_write_dsl_ir_image', 'WN_get_dsl_ir_image', 'DSL_IR_Image_Load_Mapped'),
    'effects': ('.WHIRL.dsl_effects', 'WN_write_dsl_effect_image', 'WN_get_dsl_effect_image', 'DSL_Effect_Image_Load_Mapped'),
    'calls': ('.WHIRL.dsl_calls', 'WN_write_dsl_callsite_image', 'WN_get_dsl_callsite_image', 'DSL_Call_Image_Load_Mapped'),
    'call_abi': ('.WHIRL.dsl_call_abi', 'WN_write_dsl_call_abi_image', 'WN_get_dsl_call_abi_image', 'DSL_Call_ABI_Image_Load_Mapped'),
    'pu_interface': ('.WHIRL.dsl_pu_interface', 'WN_write_dsl_pu_interface_image', 'WN_get_dsl_pu_interface_image', 'DSL_PU_Interface_Image_Load_Mapped'),
    'runtime_interface': ('.WHIRL.dsl_runtime_interface', 'WN_write_dsl_runtime_interface_image', 'WN_get_dsl_runtime_interface_image', 'DSL_Runtime_Interface_Image_Load_Mapped'),
    'program_interface': ('.WHIRL.dsl_program_interface', 'WN_write_dsl_program_interface_image', 'WN_get_dsl_program_interface_image', 'DSL_Program_Interface_Image_Load_Mapped')
}


def sha_bytes(value):
    """Fingerprint exact source/evidence bytes, never compiler pointer identities."""
    return hashlib.sha256(value).hexdigest()


def masked(text):
    """Hide literals/comments without shifting offsets used for reference locations."""
    return LEX.sub(lambda match: ''.join('\n' if char == '\n' else ' ' for char in match.group()), text)


def supplements(entries):
    """Account for macros and foreign declarations excluded by the DSL AST filter."""
    result = list(entries)
    for name in FILES:
        text = (ROOT / name).read_text()
        clean = masked(text)
        for match in re.finditer(r'^#define\s+(\w+)[^\n]*', clean, re.M):
            symbol = match.group(1)
            start = match.start()
            finish = text.find('\n', start)
            while finish >= 0 and text[start:finish].rstrip().endswith('\\'):
                finish = text.find('\n', finish + 1)
            if finish < 0:
                finish = len(text)
            line = text.count('\n', 0, start) + 1
            result.append(dict(name=symbol, path=name, line=line, start_line=line,
                end_line=text.count('\n', 0, finish) + 1,
                category='include_guard' if symbol.endswith('_INCLUDED') else 'macro',
                role='definition', signature='', source=text[start:finish], storage_class='', uses=[]))
        for match in re.finditer(r'^(?:class WN;|struct pu_info;|typedef struct pu_info PU_Info;|typedef INT32 WN_MAP;|typedef struct wn_map_tab WN_MAP_TAB;)', clean, re.M):
            snippet = match.group()
            symbol = re.search(r'(\w+)\s*;', snippet).group(1)
            line = text.count('\n', 0, match.start()) + 1
            result.append(dict(name=symbol, path=name, line=line, start_line=line, end_line=line,
                category='foreign_type_dependency', role='declaration', signature='', source=snippet, storage_class='', uses=[]))
        for match in re.finditer(r'extern "C" INT32 IPA_WN_MAP32_Get\s*\([^;]+;', text):
            line = text.count('\n', 0, match.start()) + 1
            result.append(dict(name='IPA_WN_MAP32_Get', path=name, line=line, start_line=line,
                end_line=text.count('\n', 0, match.end()) + 1, category='method', role='declaration',
                signature='INT32 (WN_MAP_TAB *, WN_MAP, const WN *)', source=match.group(), storage_class='', uses=[]))
        for match in re.finditer(r'^extern WN_MAP_TAB \*Current_Map_Tab;', clean, re.M):
            line = text.count('\n', 0, match.start()) + 1
            result.append(dict(name='Current_Map_Tab', path=name, line=line, start_line=line, end_line=line,
                category='foreign_global_dependency', role='declaration', signature='WN_MAP_TAB *', source=match.group(), storage_class='', uses=[]))
    return sorted(result, key=lambda item: (FILES.index(item['path']), item['line'], item['name']))


def definition_after(clean, offset):
    """Recognize a function body after balanced arguments, not an ordinary call."""
    while offset < len(clean) and clean[offset].isspace():
        offset += 1
    if offset == len(clean) or clean[offset] != '(':
        return False
    depth = 0
    for end in range(offset, len(clean)):
        if clean[end] == '(':
            depth += 1
        elif clean[end] == ')':
            depth -= 1
            if depth == 0:
                suffix = clean[end + 1:].lstrip()
                if suffix.startswith('const'):
                    suffix = suffix[5:].lstrip()
                return suffix.startswith('{')
    return False


def references(entries):
    """Index lexical code uses and same-spelled definitions; do not claim a call graph."""
    names = {entry['name'] for entry in entries if entry['category'] not in ('include_guard', 'foreign_type_dependency') and '::' not in entry['name']}
    methods = {entry['name'] for entry in entries if entry['category'] == 'method'}
    pattern = STAGE / 'symbols.txt'
    pattern.write_text('\n'.join(sorted(names)) + '\n')
    found = subprocess.run(['rg', '-l', '-w', '-F', '-f', str(pattern), 'osprey',
        '--glob', '*.h', '--glob', '*.cxx', '--glob', '*.cpp', '--glob', '*.c'],
        cwd=ROOT, check=False, text=True, capture_output=True)
    assert found.returncode in (0, 1), found.stderr
    result = collections.defaultdict(list)
    definitions = collections.defaultdict(list)
    source_hashes = {}
    for path in sorted(found.stdout.splitlines()):
        data = (ROOT / path).read_bytes()
        clean = masked(data.decode('utf-8', errors='replace'))
        source_hashes[path] = sha_bytes(data)
        for match in re.finditer(r'\b[A-Za-z_]\w*\b', clean):
            name = match.group()
            if name not in names:
                continue
            line = clean.count('\n', 0, match.start()) + 1
            is_definition = name in methods and definition_after(clean, match.end())
            suffix = clean[match.end():].lstrip()
            role = 'definition_candidate' if is_definition else 'call_or_declaration_candidate' if name in methods and suffix.startswith('(') else 'code_reference'
            result[name].append(dict(path=path, line=line, role=role))
            if is_definition:
                definitions[name].append(dict(path=path, line=line))
    for entry in entries:
        if entry['category'] == 'method' and entry['role'] == 'definition':
            site = dict(path=entry['path'], line=entry['line'])
            if site not in definitions[entry['name']]:
                definitions[entry['name']].append(site)
    return result, definitions, source_hashes


def evidence_location(path, name):
    """Locate a named source declaration for reviewed writer/loader evidence."""
    text = (ROOT / path).read_text()
    match = re.search(r'\b' + re.escape(name) + r'\s*\(', text)
    assert match, (path, name)
    return dict(path=path, line=text.count('\n', 0, match.start()) + 1, name=name)


def sections(entries):
    """Require actual writer and loader source references for each persistent row."""
    result = {}
    for family, words in PERSISTENT.items():
        section, writer, reader, loader = SECTION_NAMES[family]
        writer_path = 'osprey/common/com/ir_bwrite.cxx'
        reader_path = 'osprey/common/com/ir_bread.cxx'
        write_text = (ROOT / writer_path).read_text()
        write_start = re.search(r'\b' + re.escape(writer) + r'\s*\(', write_text).start()
        next_writer = write_text.find('\nvoid\n', write_start)
        write_body = write_text[write_start:next_writer if next_writer >= 0 else len(write_text)]
        for record in words.split():
            assert re.search(r'\b' + re.escape(record) + r'\b', write_body), (writer, record)
        assert section in (ROOT / 'osprey/include/sys/elf_whirl.h').read_text()
        result[family] = dict(section=section, records=words.split(),
            writer=evidence_location(writer_path, writer), reader=evidence_location(reader_path, reader),
            loader=next(dict(path=entry['path'], line=entry['line'], name=loader) for entry in entries if entry['name'] == loader and entry['role'] == 'definition'),
            section_name_source='osprey/include/sys/elf_whirl.h')
    return result


def coverage(entries):
    """Cross-check explicit structures, header externs and five macro accessors."""
    details = []
    for path in FILES:
        clean = masked((ROOT / path).read_text())
        lexical = set(re.findall(r'typedef\s+struct\s*\{[\s\S]*?\}\s*(DSL_\w+)\s*;', clean))
        lexical.update(re.findall(r'\bstruct\s+(DSL_\w+)\s*\{', clean))
        structural = {entry['name'] for entry in entries if entry['path'] == path and entry['category'] == 'record'}
        assert lexical == structural, (path, lexical ^ structural)
        declarations = re.findall(r'^extern\b[^;]*?\b(DSL_\w+)\s*\(', clean, re.M)
        if path.endswith('.h'):
            methods = [entry for entry in entries if entry['path'] == path and entry['category'] == 'method']
            assert set(declarations) <= {entry['name'] for entry in methods}
        details.append(dict(path=path, source_sha256=sha_bytes((ROOT / path).read_bytes()),
            entities=len([entry for entry in entries if entry['path'] == path]),
            categories=dict(collections.Counter(entry['category'] for entry in entries if entry['path'] == path)),
            lexical_structures_equal_ast=True, local_conditional_branches='header guard only; no conditional DSL declarations in these three files'))
    accessors = [entry for entry in entries if entry['path'].endswith('dsl_ir_image.cxx') and entry['category'] == 'method'
        and re.fullmatch(r'DSL_Program_Interface_Image_(Retired_Formal|Retired_Call|Runtime_Input|Runtime_Binding|Runtime_Call)_Count', entry['name'])]
    assert len(accessors) == 5 and all(entry['role'] == 'definition' for entry in accessors)
    structures = sum(entry['category'] == 'record' for entry in entries)
    methods = sum(entry['category'] == 'method' for entry in entries)
    persistent = [entry for entry in entries if entry['persistence'] == 'persistent_record']
    assert len(persistent) == 25 and all(entry['path'] == FILES[0] for entry in persistent)
    assert not any(entry['category'] == 'method' or entry['persistence'] == 'nonpersistent'
                   for entry in entries if entry['path'] == FILES[0])
    return dict(files=details, ast_structures=structures, persistent_structures=25,
                nonpersistent_structures=structures - 25, method_occurrences=methods,
                generated_accessors=5, schema_has_no_service_declarations=True)



def self_tests():
    """Check extraction primitives independently before certifying source coverage."""
    assert masked('a /* b\nc */ d') == 'a     \n     d'
    assert 'DSL_fake' not in masked('"DSL_fake" // DSL_fake\nDSL_real')
    assert definition_after('(nested(call())) { body; }', 0)
    assert not definition_after('(arg));', 0)
    assert not definition_after('(arg);', 0)
    assert definition_after('(arg) const { return 0; }', 0)
    assert location(dict(spellingLoc=dict(file='owner', offset=2), expansionLoc=dict(offset=3)))['file'] == 'owner'
    assert len({name for words in PERSISTENT.values() for name in words.split()}) == 25
    assert source_tokens('name /* comment */ + value') == ['name', '+', 'value']
    assert source_tokens('"file://value/*keep*/"') == ['"file://value/*keep*/"']
    assert source_tokens('left right') != source_tokens('leftright')
    assert source_tokens("'/' // comment\n + 1") == ["'/'", '+', '1']
    return 12


def ctags_cross_check(entries, executable):
    """Reconcile independent lexical tags, documenting all six AST-only methods."""
    command = [executable, '--declarations', '-x', '--language=c++', *FILES]
    result = subprocess.run(command, cwd=ROOT, check=True, text=True, capture_output=True)
    (STAGE / 'ctags.txt').write_text(result.stdout)
    methods = {(entry['path'], entry['name'].split('::')[-1]) for entry in entries if entry['category'] == 'method'}
    other = {(entry['path'], entry['name']) for entry in entries if entry['category'] != 'method'}
    found = set()
    for line in result.stdout.splitlines():
        match = re.match(r'(\S+)\s+(\d+)\s+(\S+)\s+(.*)', line)
        if match is None:
            continue
        name, number, path, body = match.groups()
        if (path, name) in methods:
            found.add((path, name))
        if (path, name) not in methods and (path, name) not in other and '(' in body and (name.startswith('DSL_') or name == 'operator()'):
            raise AssertionError('Unreconciled lexical method candidate: ' + line)
    expected = {(FILES[1], 'DSL_Program_Interface_Image_' + suffix) for suffix in (
        'Get_Retired_Formal', 'Retired_Formal_Count', 'Retired_Call_Count', 'Runtime_Binding_Count', 'Runtime_Call_Count', 'Runtime_Input_Count')}
    assert methods - found == expected, methods - found
    return dict(command=command, unique_path_method_names=len(methods), recognized=len(found),
        ast_only=[dict(path=path, name=name) for path, name in sorted(expected)],
        explanation='Five macro-generated count accessors and one split-line getter are missed by GNU Emacs ctags but fully present in Clang AST; no unclassified lexical method candidates.')


def local_scratch(entries):
    """Account for named process-local STL/owner-private objects inside methods."""
    transient = {entry['name'] for entry in entries if entry['category'] in ('record', 'alias') and not entry['persistence'].startswith('persistent_')}
    result = {}
    definitions = {(entry['path'], entry['name']): entry for entry in entries if entry['category'] == 'method' and entry['role'] == 'definition'}

    def visit(node, method, path):
        """Collect explicit local objects by source identity, ignoring implicit copies."""
        if node.get('kind') == 'VarDecl' and not node.get('isImplicit'):
            kind = node.get('type', {}).get('qualType', '')
            tokens = set(re.findall(r'\b[A-Za-z_]\w*\b', kind))
            if re.search(r'\bstd::(vector|set|map|basic_string|string)\b', kind) or tokens & transient:
                loc = location(node['loc'], path)
                text = (ROOT / path).read_text()
                line = text.count('\n', 0, loc['offset']) + 1
                result[(path, line, node['name'])] = dict(path=path, line=line, name=node['name'], type=kind,
                    owning_method=method['name'], group=method['group'], proposed_destinations=method['proposed_destinations'],
                    persistence='nonpersistent_local_object', lifetime='lexical method/block; no independent .B object')
        for child in node.get('inner', []):
            visit(child, method, path)

    for ast_name in sorted(path.name for path in STAGE.glob('*.ast.json')):
        for node in nodes(STAGE / ast_name):
            path = location(node.get('loc', {})).get('file')
            key = (path, node.get('name'))
            if node['kind'] in ('FunctionDecl', 'FunctionTemplateDecl') and key in definitions:
                visit(node, definitions[key], path)
    return sorted(result.values(), key=lambda entry: (FILES.index(entry['path']), entry['line'], entry['name']))


def capture(extra_args, compiler):
    """Capture complete ASTs successfully; fail rather than accepting partial dumps."""
    commands = []
    for source in (path for path in FILES if path.endswith('.cxx')):
        key = Path(source).stem
        command = [compiler, '-std=gnu++98', '-DKEY', '-DTARG_X8664',
            '-Iosprey/linux/include', '-Iosprey/common/com', '-Iosprey/common/fhe',
            '-Iosprey/common/com/x8664', '-Iosprey/common/util', '-Iosprey/include',
            *extra_args, '-fsyntax-only', '-Xclang', '-ast-dump=json', '-Xclang', '-ast-dump-filter=DSL_', source]
        with (STAGE / (key + '.ast.json')).open('w') as output, (STAGE / (key + '.ast.log')).open('w') as log:
            subprocess.run(command, cwd=ROOT, stdout=output, stderr=log, check=True)
        commands.append(command)
    return commands


def write_reports(entries, commands, compiler, ctags):
    """Emit complete per-occurrence manifest and compact review CSV without source copies."""
    refs, definitions, reverse_source_hashes = references(entries)
    scope = coverage(entries)
    scope['ctags_cross_check'] = ctags_cross_check(entries, ctags)
    section_evidence = sections(entries)
    related_methods = collections.defaultdict(set)
    scoped_names = {entry['name'] for entry in entries}
    for entry in entries:
        identifiers = set(re.findall(r'\b[A-Za-z_]\w*\b', masked(entry['source'])))
        entry['uses'] = sorted({name for name in identifiers if name.startswith('DSL_') or name in scoped_names} - {entry['name']})
        if entry['category'] == 'method':
            for name in entry['uses']:
                related_methods[name].add(entry['name'])
    for entry in entries:
        source = entry.pop('source')
        entry['source_sha256'] = sha_bytes(source.encode())
        tokens = source_tokens(source)
        entry['token_sha256'] = sha_bytes(json.dumps(tokens).encode())
        if entry['category'] == 'method' and entry['role'] == 'definition':
            body = tokens[tokens.index('{'):] if '{' in tokens else tokens
            entry['body_token_sha256'] = sha_bytes(json.dumps(body).encode())
        entry['related_methods_in_scope'] = sorted(related_methods[entry['name']])
        entry['definition_sites'] = sorted(definitions.get(entry['name'], []), key=lambda site: (site['path'], site['line']))
        sites = refs.get(entry['name'], [])
        if entry.get('storage_class') == 'static':
            sites = [site for site in sites if site['path'] == entry['path']]
            entry['definition_sites'] = [site for site in entry['definition_sites'] if site['path'] == entry['path']]
        entry['reference_files'] = sorted({site['path'] for site in sites})
        entry['reference_index_key'] = entry['name']
        entry['review_status'] = 'located_current_owner_requires_compatibility_validation'
        if entry['persistence_family']:
            entry['persistence_evidence'] = entry['persistence_family']
        elif entry['persistence'] == 'persistent_schema_symbol':
            entry['persistence_evidence'] = 'Symbolic contract for fixed-row fields/header extents; not a separately emitted object. Enum members are retained in the declaration source hash.'
        else:
            entry['persistence_evidence'] = 'Not emitted as this object by the seven image writers. Group rationale distinguishes persistent pointed-to facts from process-local storage/views/methods.'
    for group, description in GROUPS.items():
        description['destination'] = sorted({entry['path'] for entry in entries if entry['group'] == group})
        description['status'] = 'current_source_owners_not_a_runtime_certificate'
    report = dict(schema='open64.dsl-ir-ownership-inventory.v1', status='current_pair_and_companion_owner_inventory',
        base_commit=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
        input_sources={path: sha_bytes((ROOT / path).read_bytes()) for path in FILES},
        evidence_sources={path: sha_bytes((ROOT / path).read_bytes()) for path in (
            'osprey/common/com/ir_bwrite.cxx', 'osprey/common/com/ir_bread.cxx', 'osprey/include/sys/elf_whirl.h')},
        generator_sha256=sha_bytes(Path(__file__).read_bytes()),
        compiler=subprocess.check_output([compiler, '--version'], text=True).splitlines()[0],
        ast_commands=commands, self_tests=self_tests(), coverage=scope,
        declaration_scope='Every explicit structure, enum/alias, macro, global store and method occurrence in the relocated pair and direct companion services; compiler-generated implicit members/instantiations and local variables are not independent APIs.',
        caller_index_scope='Lexical code references in osprey C/C++ sources; comments/literals excluded. Definition candidates checked by balanced argument/body syntax; same-spelled static functions in other TUs are not linkage proof. This is not an IPA/dynamic call graph.',
        sections=section_evidence, groups=GROUPS, entities=entries, local_transient_objects=local_scratch(entries),
        reference_index={name: [dict(path=path, lines=sorted({site['line'] for site in sites if site['path'] == path}))
            for path in sorted({site['path'] for site in sites})] for name, sites in sorted(refs.items())},
        reference_source_sha256=reverse_source_hashes,
        limits=['Inventory is source ownership evidence, not runtime or mapped-reopen certification.',
                'Only this pair and its direct companion services are scoped; unrelated common/com DSL infrastructure is not certified.',
                'No binary layout change is intended. WHIRL.pdf is absent; no new representation is designed here.'])
    (STAGE / 'dsl-ir-ownership-inventory.json').write_text(json.dumps(report, indent=2) + '\n')
    output = io.StringIO()
    writer = csv.writer(output, lineterminator='\n')
    writer.writerow(['path', 'line', 'start_line', 'end_line', 'name', 'category', 'role', 'persistence', 'owning_logic_group', 'decision', 'proposed_destinations', 'batch', 'related_methods_in_scope', 'definition_sites', 'reference_files'])
    for entry in entries:
        writer.writerow([entry['path'], entry['line'], entry['start_line'], entry['end_line'], entry['name'], entry['category'], entry['role'], entry['persistence'], entry['group'], entry['decision'],
            '; '.join(entry['proposed_destinations']), entry['batch'], '; '.join(entry['related_methods_in_scope']),
            '; '.join(site['path'] + ':' + str(site['line']) for site in entry['definition_sites']), '; '.join(entry['reference_files'])])
    (STAGE / 'dsl-ir-ownership-inventory.csv').write_text(output.getvalue())
    print(json.dumps(dict(coverage=scope, group_counts=dict(collections.Counter(entry['group'] for entry in entries)),
        entity_occurrences=len(entries), unique_names=len({entry['name'] for entry in entries}), referenced_files=len(reverse_source_hashes)), indent=2))


def main():
    """Generate into a fresh disposable directory, never overwrite repository sources."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--cxx', default='/usr/bin/clang++')
    parser.add_argument('--ctags', default='ctags')
    parser.add_argument('--extra-arg', action='append', default=[])
    args = parser.parse_args()
    global ROOT, STAGE
    ROOT = args.root.resolve()
    STAGE = args.output.resolve()
    assert not STAGE.exists(), 'Output must be fresh, not an existing receipt or source directory'
    STAGE.mkdir(parents=True)
    self_tests()
    commands = capture(args.extra_arg, args.cxx)
    entries = classify(supplements(collect()))
    write_reports(entries, commands, args.cxx, args.ctags)


if __name__ == '__main__':
    main()
