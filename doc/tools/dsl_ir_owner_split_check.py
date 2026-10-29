"""Prove schema and function-body preservation across the DSL owner relocation.

This is source equivalence evidence, not a replacement for linked fixtures,
mapped reopen, or manual contract review. See doc/WHIRL-DSL-INFRASTRUCTURE.md.
"""
import argparse
import collections
import hashlib
import json
from pathlib import Path
import re

import dsl_ir_ownership_inventory as inventory


def tokens(source):
    """Preserve literals and token boundaries while discarding only comments."""
    pattern = inventory.LEX.pattern + r'|[A-Za-z_]\w*|\d+|[^\s]'
    return [match.group() for match in re.finditer(pattern, source)
            if not match.group().startswith(('/*', '//'))]


def digest(value):
    """Hash canonical token sequences, not whitespace or source paths."""
    return hashlib.sha256(json.dumps(value).encode()).hexdigest()


def capture(root, paths, output, arguments):
    """Parse all owned implementations and account for each explicit definition."""
    inventory.ROOT = root.resolve()
    inventory.FILES = paths
    inventory.STAGE = output
    output.mkdir()
    commands = inventory.capture(arguments.extra_arg, arguments.cxx)
    entries = inventory.supplements(inventory.collect())
    bodies = collections.defaultdict(list)
    schemas = {}
    for entry in entries:
        sequence = tokens(entry['source'])
        if entry['category'] == 'method' and entry['role'] == 'definition':
            body = sequence[sequence.index('{'):] if '{' in sequence else sequence
            body = ['DSL_IR_NATIVE_VALUE_LOWER_RESULT'
                    if token == 'DSL_IR_LOWERED_RELATION_INFO' else token for token in body]
            bodies[entry['name']].append(digest(body))
        if entry['path'].endswith('/dsl_ir_image.h') and entry['category'] not in (
                'method', 'include_guard', 'foreign_type_dependency'):
            schemas[entry['name']] = digest(sequence)
    return dict(commands=commands, schemas=schemas,
                bodies={name: sorted(values) for name, values in sorted(bodies.items())},
                source_sha256={path: hashlib.sha256((root / path).read_bytes()).hexdigest()
                               for path in paths})


def self_test():
    """Reject accidental literal/comment confusion and joined-token equivalence."""
    assert tokens('name /* comment */ + value') == ['name', '+', 'value']
    assert tokens('"file://value/*keep*/"') == ['"file://value/*keep*/"']
    assert tokens('left right') != tokens('leftright')
    assert tokens("'/' // discarded\n + 1") == ["'/'", '+', '1']
    return 4


def main():
    """Retain complete before/after token receipts and fail on unexplained drift."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--before-root', type=Path, required=True)
    parser.add_argument('--after-root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--cxx', default='/usr/bin/clang++')
    parser.add_argument('--extra-arg', action='append', default=[])
    arguments = parser.parse_args()
    arguments.output.mkdir()
    tests = self_test()
    after_paths = list(inventory.FILES)
    before_paths = ['osprey/common/com/' + Path(path).name for path in after_paths
                    if (arguments.before_root / 'osprey/common/com' / Path(path).name).is_file()]
    before_paths = list(dict.fromkeys(before_paths))
    before = capture(arguments.before_root, before_paths, arguments.output / 'before', arguments)
    after = capture(arguments.after_root, after_paths, arguments.output / 'after', arguments)
    changed = {name: dict(before=before['bodies'].get(name), after=after['bodies'].get(name))
               for name in before['bodies'].keys() | after['bodies'].keys()
               if before['bodies'].get(name) != after['bodies'].get(name)}
    missing_schema = {name: value for name, value in after['schemas'].items()
                      if before['schemas'].get(name) != value}
    persistent = {name for words in inventory.PERSISTENT.values() for name in words.split()}
    assert persistent <= after['schemas'].keys()
    required_schema = persistent | inventory.REVIEWED_SCHEMA_SYMBOLS
    assert required_schema <= after['schemas'].keys()
    schema_text = (arguments.after_root / 'osprey/common/com/dsl_ir_image.h').read_text()
    schema_includes = set(re.findall(r'^#include "([^"]+)"', schema_text, re.M))
    assert schema_includes == {'defs.h', 'dsl_opcode.h', 'srcpos.h', 'symtab_idx.h', 'targ_const.h'}
    assert not re.search(r'\b(?:WN|PU_Info|WN_MAP)\b', inventory.masked(schema_text))
    historical = json.loads((arguments.before_root / 'doc/dsl-ir-ownership-inventory.json').read_text())
    current_names = {entry['name'] for entry in inventory.supplements(inventory.collect())}
    assert {entry['name'] for entry in historical['entities']} <= current_names
    assert all(not (arguments.after_root / path).exists() for path in before_paths
               if path != 'osprey/common/com/dsl_ir_image.h')
    report = dict(schema='open64.dsl-owner-relocation-equivalence.v1', self_tests=tests,
                  before=before, after=after, changed_function_bodies=changed,
                  changed_retained_schema=missing_schema,
                  persisted_records=len(persistent), retained_schema_contracts=len(after['schemas']),
                  compared_function_definitions=sum(len(values) for values in before['bodies'].values()),
                  schema_include_boundary='no transient or phase headers',
                  historical_inventory_names='all original names accounted for',
                  retired_source_paths='no forwarding files remain',
                  allowed_spelling_only_change='Image-owned DSL_IR_LOWERED_RELATION_INFO is an alias of the legacy result typedef.',
                  limits=['No ABI, mapped-file or runtime certification from this source proof alone.'])
    (arguments.output / 'equivalence.json').write_text(json.dumps(report, indent=2) + '\n')
    assert not changed, changed
    assert not missing_schema, missing_schema
    print('Unchanged function definitions:', report['compared_function_definitions'])
    print('Unchanged retained schema contracts:', report['retained_schema_contracts'])


if __name__ == '__main__':
    main()
