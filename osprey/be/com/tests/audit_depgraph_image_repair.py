"""Audit native x86-64 before/after graph images without rewriting either input.

Pin the current ABI with depgraph_image_test's census. Independently inspect
ELF/PU/symbol headers and retain every raw difference: only repaired absent
vertex offsets and unused ST index-zero carrier bytes may differ.
Design: doc/WHIRL-DEPENDENCE-IMAGE-REPAIR.md.
"""
import argparse
import difflib
import json
from pathlib import Path
import re
import struct


def inspect_image(path):
    """Follow current mapped headers, bound all test reads, and identify tables."""
    data = path.read_bytes()
    assert data[:6] == b'\x7fELF\x02\x01'
    header = struct.unpack_from('<16sHHIQQQIHHHHHH', data)
    assert header[1] == 0xff00 and header[11] == 64  # ET_IR, not a native executable
    sections = [struct.unpack_from('<IIQQQQIIQQ', data, header[6] + index * header[11])
                for index in range(header[12])]
    names = sections[header[13]]
    names = data[names[4]:names[4] + names[5]]
    sections = {names[row[0]:].split(b'\0', 1)[0].decode(): row for row in sections}

    def st_zero(base, limit):
        """Find the reserved ST index-zero row through actual symbol headers."""
        size, count = struct.unpack_from('<II', data, base)
        assert size == 8 + count * 24 and base + size <= limit
        for index in range(count):
            offset, length, stride, alignment, kind = struct.unpack_from('<QQIHH', data, base + 8 + index * 24)
            assert base + offset + length <= limit
            if kind == 2:
                assert stride == 64 and length >= stride and alignment == 8
                return list(range(base + offset, base + offset + stride))
        raise AssertionError('ST table missing')

    pu = sections['.WHIRL.pu_section']
    base, limit = pu[4], pu[4] + pu[5]
    position = base + struct.unpack_from('<I', data, base)[0]
    version, count = struct.unpack_from('<II', data, position)
    assert version == 1 and 0 < count <= 100000
    position += 8 + count * 20
    kinds = struct.unpack_from('<I', data, position)[0]
    assert kinds == 11
    position += 4
    rows = {}
    for expected in range(kinds):
        kind = struct.unpack_from('<I', data, position)[0]
        assert kind == expected
        position += 4
        rows[kind] = [struct.unpack_from('<II', data, position + index * 8) for index in range(count)]
        position += count * 8
    global_table = sections['.WHIRL.globals']
    reserved = st_zero(global_table[4], global_table[4] + global_table[5])
    graphs, trees = [], []
    for index in range(count):
        offset, size = rows[0][index]
        reserved += st_zero(base + offset, base + offset + size)
        tree_offset, tree_size = rows[1][index]
        trees.append(data[base + tree_offset:base + tree_offset + tree_size])
        offset, size = rows[2][index]
        if not size:
            continue
        start = base + offset
        vertices, edges = struct.unpack_from('<HH', data, start)
        assert 4 + vertices * 4 <= size
        offsets = list(struct.unpack_from('<' + 'I' * vertices, data, start + 4))
        assert all(value < tree_size for value in offsets)
        graphs.append(dict(pu=index, vertices=vertices, edge_high_water=edges,
                           offset=start, size=size, wn_offsets=offsets))
    return data, graphs, trees, set(reserved)


def main():
    """Require explicit native ABI evidence and retain exact raw/text diffs."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('before', 'after', 'unit-log', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    assert re.search(r'image_abi ST=64 symtab_header=24 local_header=128 word=4 vertex=2 edge=2',
                     args.unit_log.read_text())
    before, old_graphs, old_trees, old_reserved = inspect_image(args.before)
    after, new_graphs, new_trees, new_reserved = inspect_image(args.after)
    assert len(before) == len(after) and old_trees == new_trees
    assert len(old_graphs) == len(new_graphs)
    repaired, allowed = [], old_reserved & new_reserved
    for old, new in zip(old_graphs, new_graphs):
        for key in ('pu', 'vertices', 'edge_high_water', 'offset', 'size'):
            assert old[key] == new[key], key
        for index, (left, right) in enumerate(zip(old['wn_offsets'], new['wn_offsets'])):
            if left == 0:
                assert right != 0
                location = old['offset'] + 4 + index * 4
                allowed.update(range(location, location + 4))
                repaired.append(dict(pu=old['pu'], vertex=index + 1, old=left, new=right,
                                     image_offset=location))
            else:
                assert left == right
    changes = [dict(offset=index, before=left, after=right,
                    classification='RESERVED_ST_INDEX_ZERO' if index in old_reserved else 'REPAIRED_VERTEX_OFFSET')
               for index, (left, right) in enumerate(zip(before, after)) if left != right]
    assert {row['offset'] for row in changes} <= allowed, 'unexpected binary change'
    difference = ''.join(difflib.unified_diff(args.before.with_suffix('.T').read_text().splitlines(True),
                                            args.after.with_suffix('.T').read_text().splitlines(True)))
    args.output.with_suffix('.diff').write_text(difference)
    assert not difference, 'executable/symbol/source WHIRL changed'
    args.output.write_text(json.dumps(dict(status='PASS',graph_before=old_graphs,graph_after=new_graphs,
        repaired_vertices=repaired,tree_bytes_identical=True,raw_changes=changes,
        text_diff='empty',images_rewritten=False),indent=2)+'\n')
    print(json.dumps(dict(status='PASS',repaired_vertices=len(repaired),raw_changes=len(changes))))


if __name__ == '__main__':
    main()
