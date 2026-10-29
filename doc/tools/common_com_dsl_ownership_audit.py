"""Locate reviewed DSL persistence/ownership decisions without moving compiler code.

Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md. This combines an
explicit reviewed family policy, positive mapped-image witnesses, lexical type
extraction and an independently produced comment/function census. It is not an
AST, a linker call graph, a new persistence format or runtime certification.
"""
import argparse
import collections
import csv
import hashlib
import json
from pathlib import Path
import re
import subprocess

import dsl_ir_ownership_inventory as previous


SHARED = {
    'config.cxx': ('nonpersistent_options', 'osprey/common/util', 'DSL option-group inclusion/registration only; leave unrelated option groups outside this audit.'),
    'config_dsl.cxx': ('nonpersistent_options', 'osprey/common/util', 'Option storage and descriptor table; phase owns meaning. Preserve central registration and complete option propagation.'),
    'config_dsl.h': ('nonpersistent_options', 'osprey/common/util', 'Option declarations and trace bit are controls, not image fields.'),
    'ir_bcom.cxx': ('mapped_image_mechanism', 'osprey/common/ir_image', 'Actual tensor extension/metadata/KV and TCON/string image writer; these services are not row definitions.'),
    'ir_bread.cxx': ('mapped_image_mechanism', 'osprey/common/ir_image + osprey/be/com', 'Section mapping/loads and cache rebuild; physical PU checks stay backend-free and separately owned.'),
    'ir_bread.h': ('mapped_image_mechanism', 'osprey/common/ir_image', 'DSL section reader interfaces, not new image records.'),
    'ir_bwrite.cxx': ('mapped_image_mechanism', 'osprey/common/ir_image', 'Positive writer witnesses and gatekeeper invocation; preserve section order, versions and absent-section behavior.'),
    'ir_bwrite.h': ('mapped_image_mechanism', 'osprey/common/ir_image', 'DSL writer interfaces, not record objects.'),
    'ir_reader.cxx': ('logical_inspection', 'osprey/common/ir_image', 'DSL logical printing of persisted meaning, not a new source of analysis facts.'),
    'opcode_gen': ('mixed_encoded_vocabulary_and_generator', 'osprey/common/com + generator owner', 'Physical DSL slot is a persisted encoding; generation/check guidance is build logic. No wholesale legacy generator move proposed.'),
    'opcode_gen_core.cxx': ('mixed_encoded_vocabulary_and_generator', 'osprey/common/com + generator owner', 'Physical DSL operator table/generation additions; preserve every target slot and property.'),
    'opcode_gen_core.h': ('encoded_schema_vocabulary', 'osprey/common/com', 'Physical OPR_DSL spelling/slot and operator properties support the persisted WN encoding.'),
    'strtab.cxx': ('mixed_persistent_store_and_private_checkpoint', 'osprey/common/com + osprey/common/ir_image', 'String bytes persist; CKKS count save/restore is private transient store support, with declaration already in common/util. Do not expose raw tables or move unrelated legacy strings.'),
    'symtab.cxx': ('persistent_access_and_shared_type_canonicalization', 'osprey/common/com', 'Persistent tensor/ST access and shared tensor type canonicalization stay. Canonical hash/index/cache is transient service-private state, not a reason to move the service or a new .B row.'),
    'symtab.h': ('persistent_schema_and_shared_type_service_views', 'osprey/common/com', 'Three tensor/KV STORE rows persist. Canonical descriptor inputs, INFO, refinement and derived TENSOR_DESCRIPTOR_RECORD do not persist, but stay with shared tensor type construction/canonicalization services.'),
    'symtab_defs.h': ('encoded_schema_vocabulary', 'osprey/common/com', 'KIND_TENSOR and three SHDR tensor additions are binary encodings; SCOPE and generic table management are outside DSL-only relocation scope.'),
    'symtab_verify.cxx': ('persistent_structural_verification', 'osprey/common/com', 'Tensor-kind row integrity checks, not PU candidate discovery.'),
    'ttype.cxx': ('logical_inspection', 'osprey/common/ir_image', 'DSL/tensor logical type inspection; do not move unrelated historical type printers in this audit.'),
    'wn.cxx': ('shared_IR_construction_access_and_integrity', 'osprey/common/com', 'Native/marker constructors, accessors and generic unconsumed-DSL integrity checks stay. Their temporary scan counters are not persisted WN fields or optimization-policy objects.'),
    'wn.h': ('shared_IR_definitions_views_and_integrity', 'osprey/common/com', 'WN DSL encodings, constructors/accessors, decoded views and generic integrity-scan contracts stay with common WHIRL services, independently of helper-data durability.'),
    'wn_simp_ftable.h': ('shared_ingestion_simplifier_hook', 'osprey/common/com', 'DSL adapter entries stay with the existing common WHIRL canonicalization/folding rule engine, callable during ingestion. Do not move or duplicate the engine.'),
}

EXTRA_RECORDS = {
    'DSL_REGION_IMAGE_HEADER': 'region',
    'DSL_REGION_IMAGE_RECORD': 'region',
    'DSL_REGION_INTERFACE_RECORD': 'region',
    'DSL_CKKS_EVENT_IMAGE_HEADER': 'ckks-event',
    'DSL_CKKS_EVENT_RECORD': 'ckks-event',
    'DSL_TENSOR_TCON_ENVELOPE': 'tensor-carrier',
    'TY_TENSOR_EXTENSION_STORE': 'tensor-symtab',
    'ST_TENSOR_METADATA_STORE': 'tensor-symtab',
    'TY_DSL_KV': 'tensor-symtab',
}
ENCODED_OPCODE_ENUMS = {
    'DSL_OPERATOR', 'DSL_OPCODE_CATEGORY', 'DSL_OPCODE_LEVEL',
    'DSL_SHAPE_RULE', 'DSL_EFFECT_MODEL', 'DSL_LOWERING_MODEL',
}
SYMBOL_ACCESS = {
    'DSL_Tensor_Create_Result_Symbol', 'DSL_Tensor_Set_Unique_Ownership',
    'DSL_Tensor_Has_Unique_Ownership',
}


def sha(path):
    """Fingerprint complete source/evidence bytes, never normalized text."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def tracked(root):
    """Use Git's tracked directory inventory, excluding build/untracked copies."""
    return subprocess.check_output(
        ['git', 'ls-files', 'osprey/common/com'], cwd=root, text=True).splitlines()


def close_brace(text, start):
    """Find a balanced masked body; reject malformed input instead of guessing."""
    depth = 0
    for offset in range(start, len(text)):
        if text[offset] == '{':
            depth += 1
        elif text[offset] == '}':
            depth -= 1
            if depth == 0:
                return offset
    raise ValueError('Unbalanced type body')


def types(text):
    """Locate explicit record/enum definitions and typedefs in all textual branches.

    Literal/comment masking retains byte offsets. Nested bodies are balanced;
    aliases are separate occurrences, not extra persisted records. Implicit
    methods, macro expansions and preprocessor reachability require a later AST
    migration audit, not a fabricated compiler-completeness claim here.
    """
    clean = previous.masked(text)
    result = []
    covered = []
    pattern = r'\b(typedef\s+)?(struct|class|enum)\s*(\w+)?\s*(?::[^;{]+)?\{'
    for match in re.finditer(pattern, clean):
        if any(start <= match.start() < finish for start, finish in covered):
            continue
        end = close_brace(clean, clean.index('{', match.start())) + 1
        name = match.group(3)
        if match.group(1):
            alias = re.match(r'\s*(\w+)(?:\s*,\s*\*?\w+)*\s*;', clean[end:])
            if alias is None:
                raise ValueError('Unsupported typedef body suffix')
            name = alias.group(1)
            end += alias.end()
        if name is None:
            name = '<anonymous-enum@' + str(text.count('\n', 0, match.start()) + 1) + '>'
        result.append(dict(name=name, category='enum' if match.group(2) == 'enum' else 'record',
            line=text.count('\n', 0, match.start()) + 1, snippet=text[match.start():end]))
        covered.append((match.start(), end))
    for match in re.finditer(r'^typedef\s+[^;{}]+;', clean, re.M):
        if any(start <= match.start() < finish for start, finish in covered):
            continue
        declaration = match.group()
        callback = re.search(r'\(\s*\*\s*(\w+)\s*\)', declaration)
        if callback:
            name = callback.group(1)
        else:
            prefix = declaration.split('[', 1)[0]
            name = re.findall(r'\b[A-Za-z_]\w*\b', prefix)[-1]
        result.append(dict(name=name, category='callback' if callback else 'alias',
            line=text.count('\n', 0, match.start()) + 1,
            snippet=text[match.start():match.end()]))
    return sorted(result, key=lambda item: (item['line'], item['name']))


def macros(text):
    """Locate explicit macro definitions, preserving continued source spelling."""
    result = []
    clean = previous.masked(text)
    for match in re.finditer(r'^[ \t]*#\s*define\s+(\w+)[^\n]*', clean, re.M):
        end = text.find('\n', match.end())
        if end < 0:
            end = len(text)
        while text[match.start():end].rstrip().endswith('\\') and end < len(text):
            end = text.find('\n', end + 1)
            if end < 0:
                end = len(text)
        result.append(dict(name=match.group(1), category='macro',
            line=text.count('\n', 0, match.start()) + 1,
            snippet=text[match.start():end]))
    return result


def assignment(group, name, category):
    """Classify durability separately from shared-service or phase ownership.

    Ingestion-time canonicalization/folding and their transient descriptions stay
    with common IR services. Independent optimizer facts/plans do not inherit
    that ownership merely because they reference the same persistent WHIRL.
    """
    core = {name: family for family, names in previous.PERSISTENT.items()
            for name in names.split()}
    family = core.get(name) or EXTRA_RECORDS.get(name)
    if family and category == 'record':
        owner = 'osprey/common/fhe' if family == 'ckks-event' else 'osprey/common/com'
        return dict(persistence='persisted_record', persistence_family=family,
                    owner=owner, action='retain_schema' if family != 'ckks-event' else 'relocate_FHE_schema_unchanged')
    if name == 'DSL_TENSOR_TCON_RECORD':
        return dict(persistence='alias_of_persisted_record', owner='osprey/common/com', action='retain_alias')
    if name.endswith(('_INCLUDED', '_Size_Check', '_SIZE_ASSERT')):
        return dict(persistence='compile_time_scaffolding', owner=group['proposed_owner'], action='follow_owner_no_image_bytes')
    if group['id'] == 'persistent-core':
        assert category not in {'record', 'function'}, 'Core row/service requires boundary review: ' + name
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/com', action='retain')
    if group['id'] == 'persistent-builder':
        return dict(persistence='construction_interface_not_stored' if category == 'function' else 'transient_construction_description',
                    owner='osprey/common/com', action='retain_by_user_direction')
    if name in SYMBOL_ACCESS:
        return dict(persistence='persistent_symbol_construction_access_not_stored',
                    owner='osprey/common/com', action='retain')
    if group['id'] == 'opcode' and name in ENCODED_OPCODE_ENUMS:
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/com', action='retain_encoded_vocabulary')
    if group['id'] == 'region' and category == 'enum':
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/com', action='retain_encoding')
    if name == 'DSL_TENSOR_TCON_STORAGE_KIND':
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/com', action='retain_encoding')
    if category == 'macro' and (group['id'] in {'ckks-event', 'region'} or
            name in {'DSL_TENSOR_TCON_MAGIC', 'DSL_TENSOR_TCON_VERSION', 'DSL_TENSOR_TCON_ENVELOPE_SIZE'}):
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/fhe' if group['id'] == 'ckks-event' else 'osprey/common/com', action='retain_encoding_with_schema_owner')
    if name == 'TY_DSL_BIND_STATE':
        return dict(persistence='encoded_schema_symbol', owner='osprey/common/com', action='retain_encoding')
    if group['id'] in {'tensor-fold', 'simp', 'memory-behavior'}:
        return dict(persistence='service_not_stored' if category == 'function' else 'nonpersistent',
                    owner='osprey/common/com', action='retain_shared_ingestion_IR_service')
    if group['id'] in {'opcode', 'domain', 'gatekeeper', 'telemetry'}:
        decision = dict(persistence='service_not_stored' if category == 'function' else 'nonpersistent',
                        owner='osprey/common/com', action='retain_general_WHIRL_service')
        if group['id'] == 'telemetry':
            decision.update(intended_data_lifetime='beyond_compiler_session',
                current_storage='in_memory_profile_and_text_inspection_not_a_WHIRL_image_section')
        return decision
    if group['id'] == 'shared':
        if name.startswith('VHO_'):
            return dict(persistence='service_not_stored' if category == 'function' else 'nonpersistent',
                        owner='osprey/common/com', action='retain_shared_IR_integrity_scan')
        if category == 'function' and re.match(r'(?:DSL_WN_|WN_.*DSL|TY_|ST_|TENSOR_)', name):
            return dict(persistence='persistent_construction_access_not_stored',
                        owner='osprey/common/com', action='retain_shared_IR_interface_and_service_views')
        if name in {'DSL_OPCODE_ANNOTATION', 'DSL_WHIRL_NODE_RECORD', 'DSL_LOGICAL_OPCODE',
                'DSL_WHIRL_OPERAND_RECORD', 'DSL_OPCODE_CARRIER_KIND',
                'DSL_OPCODE_VERSION_DISPOSITION', 'DSL_OPCODE_OUTPUT_MODE'}:
            return dict(persistence='nonpersistent', owner='osprey/common/com', action='retain_shared_IR_service_view')
    owner = group['proposed_owner']
    if group['id'] == 'region':
        owner = 'osprey/common/ir_image' if name in {'DSL_Region_Write_PU', 'DSL_Region_Load_Mapped_PU', 'DSL_REGION_STORE', 'dsl_region_runtime', 'DSL_Region_Find_Store', 'DSL_Region_Get_Store', 'DSL_Region_Find_Owning_Store', 'DSL_Region_Reset', 'DSL_Region_Print_PU'} else 'osprey/be/com'
    elif group['id'] == 'shape':
        owner = 'osprey/be/vho' if 'Analyze_PU' in name or 'SOLVER' in name or '_Solver_' in name or 'REFINEMENT' in name else 'osprey/common/com'
    elif group['id'] == 'tensor-locality':
        owner = 'osprey/be/com' if 'CONTROL_' in name or 'control_snapshot' in name else 'osprey/be/vho'
    elif group['id'] == 'compute-realization' and name in {'DSL_COMPUTE_PROFILE_RECORD', 'DSL_compute_profile_get', 'DSL_compute_profile_find'}:
        owner = 'osprey/common/targ_info'
    return dict(persistence='service_not_stored' if category == 'function' else 'nonpersistent',
                owner=owner, action='retain_shared_IR_service' if owner == 'osprey/common/com' else 'relocate_with_related_logic')


def witness(root, item):
    """Locate a reviewed source token and bind it to complete-file identity."""
    path = root / item['path']
    text = path.read_text()
    assert item['token'] in text, item
    return dict(item, line=text.count('\n', 0, text.index(item['token'])) + 1,
                sha256=sha(path))


def persistence_witnesses(root):
    """Check each claimed stored family against actual writer/reader code.

    These are positive source-path witnesses, not new binary compatibility
    certification. A flat structure or a print function alone never qualifies.
    """
    result = {}
    writer = (root / 'osprey/common/com/ir_bwrite.cxx').read_text()
    sections = (root / 'osprey/include/sys/elf_whirl.h').read_text()
    for family, names in previous.PERSISTENT.items():
        section, producer, reader, loader = previous.SECTION_NAMES[family]
        start = writer.index(producer + ' (')
        end = writer.find('\nvoid\n', start)
        body = writer[start:end if end >= 0 else len(writer)]
        assert all(name in body for name in names.split()), family
        assert section in sections
        result[family] = dict(records=names.split(), section=section,
            writer=witness(root, dict(path='osprey/common/com/ir_bwrite.cxx', token=producer)),
            reader=witness(root, dict(path='osprey/common/com/ir_bread.cxx', token=reader)))
    special = {
        'ckks-event': ('.WHIRL.dsl_ckks_event', [
            ('ir_bwrite.cxx', 'WN_write_dsl_ckks_event_image'), ('ir_bread.cxx', 'WN_get_dsl_ckks_event_image')]),
        'region': ('WT_REGIONS PU subsection', [
            ('dsl_region.cxx', 'ir_b_save_buf(&header'), ('dsl_region.cxx', 'sizeof(store->interfaces[0])'),
            ('dsl_region.cxx', 'DSL_Region_Load_Mapped_PU')]),
        'tensor-carrier': ('SHDR_TCON plus SHDR_STR, not a new ELF section', [
            ('dsl_tensor_fold.cxx', 'memcpy(payload, &record, sizeof(record))'),
            ('dsl_tensor_fold.cxx', 'Host_To_Targ_String'), ('dsl_tensor_fold.cxx', 'DSL_Tensor_TCON_Decode_Carrier'),
            ('ir_bcom.cxx', 'TCON_strtab_buffer'), ('ir_bread.cxx', 'SHDR_STR')]),
        'tensor-symtab': ('SHDR_TY_TENSOR_EXT, SHDR_ST_TENSOR_METADATA, SHDR_TENSOR_DSL_KV', [
            ('ir_bcom.cxx', 'write_table (Ty_tensor_extensions'), ('ir_bcom.cxx', 'write_table (St_tensor_metadata'),
            ('ir_bcom.cxx', 'write_table (Tensor_dsl_kv_table'), ('ir_bread.cxx', 'SHDR_TY_TENSOR_EXT'),
            ('ir_bread.cxx', 'SHDR_ST_TENSOR_METADATA'), ('ir_bread.cxx', 'SHDR_TENSOR_DSL_KV')]),
    }
    for family, (section, locations) in special.items():
        result[family] = dict(section=section,
            records=[name for name, owner in EXTRA_RECORDS.items() if owner == family],
            witnesses=[witness(root, dict(path='osprey/common/com/' + path, token=token))
                       for path, token in locations])
    return result


def include_consumers(root, files):
    """Index explicit header inclusions and hashes, not a linker dependency graph."""
    names = {Path(item['path']).name for item in files if item['path'].endswith('.h')}
    command = ['rg', '-l', '-F']
    for name in sorted(names):
        command.extend(['-e', name])
    command.extend(['osprey', '--glob', '*.h', '--glob', '*.cxx', '--glob', '*.c', '--glob', '*.cpp'])
    found = subprocess.run(command, cwd=root, text=True, capture_output=True, check=False)
    assert found.returncode in (0, 1), found.stderr
    result = []
    for relative in found.stdout.splitlines():
        path = root / relative
        for number, line in enumerate(path.read_text(errors='replace').splitlines(), 1):
            match = re.match(r'\s*#\s*include\s*[<"]([^>"]+)[>"]', line)
            if match and Path(match.group(1)).name in names:
                result.append(dict(path=relative, line=number, header=Path(match.group(1)).name,
                                   spelling=match.group(1), sha256=sha(path)))
    return result


def check_policy(policy, dedicated):
    """Require exactly one reviewed assignment for every dedicated source path."""
    assigned = [path for group in policy['groups'] for path in group['files']]
    assert len(assigned) == len(set(assigned)), 'Duplicate source assignment'
    assert set(assigned) == set(dedicated), 'Missing or foreign source assignment'


def self_tests():
    """Exercise masking, nested/anonymous types, aliases and critical mixed roles."""
    assert types('/* struct Fake {}; */ "struct AlsoFake {};"') == []
    assert types('typedef struct { int nested[2]; } REAL;')[0]['name'] == 'REAL'
    assert types('struct REAL { void Member() { if (true) { return; } } };')[0]['name'] == 'REAL'
    assert types('typedef BOOL (*VISITOR)(const char *value);')[0]['category'] == 'callback'
    assert types('typedef char SIZE_CHECK[(sizeof(REAL) == 4) ? 1 : -1];')[0]['name'] == 'SIZE_CHECK'
    assert types('typedef struct FORWARD FORWARD;')[0]['category'] == 'alias'
    assert types('typedef struct LEGACY { int field; } LEGACY, *LEGACY_PTR;')[0]['name'] == 'LEGACY'
    try:
        close_brace('{', 0)
    except ValueError:
        pass
    else:
        raise AssertionError('Unbalanced body accepted')
    group = dict(id='persistent-builder', proposed_owner='unused')
    assert assignment(group, 'DSL_BUILDER_VALUE_INFO', 'record')['persistence'] == 'transient_construction_description'
    assert assignment(group, 'DSL_Builder_Create_Symbol', 'function')['owner'] == 'osprey/common/com'
    group['id'] = 'memory-behavior'
    assert assignment(group, 'DSL_Tensor_Has_Unique_Ownership', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_MEMORY_BEHAVIOR_CONTRACT', 'record')['persistence'] == 'nonpersistent'
    group['id'] = 'tensor-fold'
    assert assignment(group, 'DSL_TENSOR_TCON_ENVELOPE', 'record')['persistence'] == 'persisted_record'
    assert assignment(group, 'DSL_TENSOR_FOLD_CANDIDATE', 'record')['persistence'] == 'nonpersistent'
    assert assignment(group, 'DSL_TENSOR_FOLD_CANDIDATE', 'record')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_Tensor_TCON_Decode_Carrier', 'function')['owner'] == 'osprey/common/com'
    group['id'] = 'simp'
    assert assignment(group, 'DSL_Simp_Binary', 'function')['owner'] == 'osprey/common/com'
    group['id'] = 'shape'
    assert assignment(group, 'DSL_Shape_Check_Operator', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_Shape_Analyze_PU', 'function')['owner'] == 'osprey/be/vho'
    group['id'] = 'opcode'
    assert assignment(group, 'DSL_Operator_Get_Algebraic_Info', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_Opcode_Register', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_OPCODE_RECORD', 'record')['owner'] == 'osprey/common/com'
    group['id'] = 'domain'
    assert assignment(group, 'DSL_Domain_Register', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_DOMAIN_RECORD', 'record')['owner'] == 'osprey/common/com'
    group['id'] = 'gatekeeper'
    assert assignment(group, 'DSL_Gatekeeper_Verify_PU', 'function')['owner'] == 'osprey/common/com'
    assert assignment(group, 'DSL_GATEKEEPER_RESULT', 'record')['persistence'] == 'nonpersistent'
    group['id'] = 'telemetry'
    assert assignment(group, 'DSL_telemetry_profile_create', 'function')['owner'] == 'osprey/common/com'
    telemetry = assignment(group, 'DSL_TELEMETRY_RECORD', 'record')
    assert telemetry['owner'] == 'osprey/common/com' and telemetry['persistence'] == 'nonpersistent'
    assert telemetry['intended_data_lifetime'] == 'beyond_compiler_session'
    assert 'not_a_WHIRL_image_section' in telemetry['current_storage']
    group['id'] = 'shared'
    assert assignment(group, 'VHO_UNCONSUMED_DSL_SCAN', 'record')['owner'] == 'osprey/common/com'
    assert assignment(group, 'TY_DSL_BIND_STATE', 'enum')['persistence'] == 'encoded_schema_symbol'
    assert assignment(group, 'DSL_LOGICAL_OPCODE', 'record')['owner'] == 'osprey/common/com'
    assert macros('#define TEST \\ \n  continuation\n')[0]['name'] == 'TEST'
    assert assignment(group, 'dsl_example_INCLUDED', 'macro')['persistence'] == 'compile_time_scaffolding'
    for files in ([], ['one', 'one'], ['other']):
        try:
            check_policy(dict(groups=[dict(files=files)]), ['one'])
        except AssertionError:
            pass
        else:
            raise AssertionError('Invalid policy coverage accepted')
    group['id'] = 'persistent-core'
    try:
        assignment(group, 'UNKNOWN_ROW', 'record')
    except AssertionError:
        pass
    else:
        raise AssertionError('Unwitnessed core row accepted')
    return 39


def main():
    """Emit exhaustive family/file and located type/API inventories into fresh output.

    Authenticate function-census source hashes and reject uncovered source files,
    anchors or public declarations. A path/type-family decision is a reviewed
    relocation proposal, not permission to move private dependencies blindly.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--comment-audit', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    arguments = parser.parse_args()
    root = arguments.root.resolve()
    policy_path = root / 'doc/common-com-dsl-ownership-policy.json'
    policy = json.loads(policy_path.read_text())
    source_paths = tracked(root)
    dedicated = [path for path in source_paths if Path(path).parent == Path('osprey/common/com') and Path(path).name.startswith('dsl_')]
    check_policy(policy, dedicated)
    audit = json.loads(arguments.comment_audit.read_text())
    census = {item['path']: item for item in audit['scopes'][0]['files']}
    files = []
    entities = []
    excluded_functionlike_macros = []
    for group in policy['groups']:
        witnesses = [witness(root, item) for item in group['witnesses']]
        for relative in group['files']:
            path = root / relative
            assert sha(path) == census[relative]['sha256'], relative
            text = path.read_text()
            raw_functions = census[relative]['functions']
            excluded = [item for item in raw_functions if re.match(r'\s*#\s*define\b', item['signature'])]
            excluded_functionlike_macros.extend(dict(path=relative, name=item['name'], line=item['line']) for item in excluded)
            functions = [item for item in raw_functions if item not in excluded]
            declarations = set(re.findall(r'^extern\b[^;]*?\b(DSL_\w+)\s*\(', previous.masked(text), re.M)) if path.suffix == '.h' else set()
            assert declarations <= {item['name'] for item in functions}, (relative, declarations - {item['name'] for item in functions})
            located = types(text)
            located += macros(text)
            located += [dict(name=item['name'], line=item['line'], category='function', snippet=item['signature']) for item in functions]
            for item in located:
                entities.append(dict(path=relative, family=group['id'], **item,
                    **assignment(group, item['name'], item['category'])))
            files.append(dict(path=relative, sha256=sha(path), family=group['id'],
                classification=group['classification'], proposed_owner=group['proposed_owner'],
                action=group['action'], reason=group['reason'], witnesses=witnesses,
                types=sum(item['category'] in {'record', 'enum', 'alias', 'callback'} for item in located),
                macros=sum(item['category'] == 'macro' for item in located), function_sites=len(functions)))
    shared = []
    tests = []
    for relative in source_paths:
        if relative in dedicated:
            continue
        path = root / relative
        text = path.read_text(errors='replace')
        if '/tests/' in relative:
            if re.search('dsl|tensor', path.name, re.I) or re.search('dsl|tensor', text, re.I):
                tests.append(dict(path=relative, sha256=sha(path),
                    classification='test_not_persistent_schema',
                    owner='Follow tested implementation; split combined fixtures before relocation.'))
            continue
        if not re.search('dsl|tensor', text, re.I):
            continue
        assert path.name in SHARED, 'Unreviewed shared DSL fragment: ' + relative
        classification, owner, reason = SHARED[path.name]
        matches = [dict(line=number, text=line) for number, line in enumerate(text.splitlines(), 1)
                   if re.search('dsl|tensor', line, re.I)]
        shared.append(dict(path=relative, sha256=sha(path), classification=classification,
                           proposed_owner=owner, reason=reason, matches=matches))
        if path.suffix in ('.h', '.cxx'):
            for item in types(text):
                if not re.search('dsl|tensor', item['name'], re.I):
                    continue
                decision = assignment(dict(id='shared', proposed_owner=owner), item['name'], item['category'])
                entities.append(dict(path=relative, family='shared-fragment', **item, **decision))
    assert {Path(item['path']).name for item in shared} == set(SHARED)
    shared_sources = [item['path'] for item in shared if Path(item['path']).suffix in ('.h', '.cxx')]
    command = ['ctags', '--declarations', '-x', '--language=c++', *shared_sources]
    tags = subprocess.run(command, cwd=root, text=True, capture_output=True, check=True)
    shared_functions = []
    for line in tags.stdout.splitlines():
        match = re.match(r'(\S+)\s+(\d+)\s+(\S+)\s+(.*)', line)
        if not match:
            continue
        name, number, relative, body = match.groups()
        if not re.search('dsl|tensor', name, re.I) or re.match(r'\s*(?:#|typedef\b|struct\b|class\b|enum\b)', body):
            continue
        source_lines = (root / relative).read_text().splitlines(True)
        tail = ''.join(source_lines[int(number)-1:])
        token = re.search(r'\b' + re.escape(name.split('::')[-1]) + r'\b', source_lines[int(number)-1])
        if token is None or not tail[token.end():].lstrip().startswith('('):
            continue
        decision = assignment(dict(id='shared', proposed_owner=SHARED[Path(relative).name][1]), name, 'function')
        item = dict(path=relative, line=int(number), name=name, category='function',
                    family='shared-fragment', snippet=''.join(source_lines[int(number)-1:int(number)+3]), **decision)
        shared_functions.append(item)
        entities.append(item)
    shared_declarations = set()
    for relative in shared_sources:
        if not relative.endswith('.h'):
            continue
        clean = previous.masked((root / relative).read_text())
        for match in re.finditer(r'^extern\b[^;]*?\b(\w+)\s*\(', clean, re.M):
            if re.search('dsl|tensor', match.group(1), re.I):
                shared_declarations.add((relative, match.group(1)))
    assert shared_declarations <= {(item['path'], item['name']) for item in shared_functions}, 'Unreconciled shared public declaration'
    persisted = [item for item in entities if item['persistence'] == 'persisted_record']
    expected = {name for names in previous.PERSISTENT.values() for name in names.split()} | set(EXTRA_RECORDS)
    assert {item['name'] for item in persisted} == expected and len(persisted) == len(expected)
    output = arguments.output
    output.mkdir()
    report = dict(schema='open64.common-com-dsl-ownership-audit.v1',
        status='classification_only_no_code_moves',
        source_commit=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip(),
        policy_sha256=sha(policy_path), generator_sha256=sha(Path(__file__)),
        comment_audit_sha256=sha(arguments.comment_audit), self_tests=self_tests(),
        dedicated_files=len(files), shared_fragment_files=len(shared), test_files=len(tests),
        persisted_record_definitions=len(persisted),
        function_sites=sum(item['function_sites'] for item in files),
        excluded_functionlike_macros=excluded_functionlike_macros,
        shared_lexical_function_sites=len(shared_functions),
        category_counts=dict(collections.Counter(item['persistence'] for item in entities)),
        persistence_witnesses=persistence_witnesses(root),
        include_consumers=include_consumers(root, files),
        files=files, shared_fragments=shared, tests=tests, entities=entities,
        limits=['No code moves, schema changes, new sections or runtime certification.',
            'Explicit lexical types/functions, not implicit members, macro expansion or conditional AST reachability.',
            'Source occurrences and include consumers are not a resolved call graph or link proof.',
            'Function persistence labels describe role; no function machine code is part of .B.',
            'Per-family methods inherit ownership unless an explicit mixed-file override is recorded; exact helper cuts require migration review.',
            'Historical non-DSL records/functions in shared files are out of scope.'])
    (output / 'inventory.json').write_text(json.dumps(report, indent=2) + '\n')
    (output / 'shared-ctags.txt').write_text(tags.stdout)
    (output / 'shared-ctags.stderr').write_text(tags.stderr)
    for name, items, fields in (
            ('files.csv', files, ['path', 'family', 'classification', 'proposed_owner', 'action', 'reason']),
            ('entities.csv', entities, ['path', 'line', 'name', 'category', 'persistence', 'owner', 'action'])):
        with (output / name).open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=fields, extrasaction='ignore')
            writer.writeheader()
            writer.writerows(items)
    print(json.dumps({key: report[key] for key in ('dedicated_files', 'shared_fragment_files', 'test_files',
        'persisted_record_definitions', 'function_sites', 'category_counts', 'self_tests')}, indent=2))


if __name__ == '__main__':
    main()
