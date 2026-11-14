"""Classify raw image differences only at native-confirmed CG DEP_STRUCT padding.

No compiler image is rewritten. The Linux x86_64 layout is validated against
the producing native component's IMAGE_ABI trace. All meaningful bytes remain
subject to exact comparison. Design: AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md.
"""
import struct


def dependence_padding(data, abi):
    """Follow ELF/PU headers and the existing Depgraph_Write layout, failing
    closed on unsupported formats. Return only trailing DEP_STRUCT padding."""
    assert abi == dict(dep_bytes=4, must_offset=2, bool_bytes=1, vertex_bytes=2,
                       edge_bytes=2, word_bytes=4, pu_field_bytes=20), abi
    assert data[:6] == b'\x7fELF\x02\x01'
    header = struct.unpack_from('<16sHHIQQQIHHHHHH', data)
    section_offset, entry_size, count, names_index = header[6], header[11], header[12], header[13]
    sections = [struct.unpack_from('<IIQQQQIIQQ', data, section_offset + i * entry_size) for i in range(count)]
    names = sections[names_index]
    names = data[names[4]:names[4] + names[5]]
    pu = next(section for section in sections if names[section[0]:].split(b'\0', 1)[0] == b'.WHIRL.pu_section')
    base, end = pu[4], pu[4] + pu[5]
    position = base + struct.unpack_from('<I', data, base)[0]
    version, pus = struct.unpack_from('<II', data, position)
    assert version == 1 and 0 < pus <= 100000
    position += 8 + pus * abi['pu_field_bytes']
    kinds = struct.unpack_from('<I', data, position)[0]; position += 4
    assert kinds == 11
    padding = set()
    for expected in range(kinds):
        kind = struct.unpack_from('<I', data, position)[0]; position += 4
        assert kind == expected
        for _ in range(pus):
            offset, size = struct.unpack_from('<II', data, position); position += 8
            if kind != 2 or not size:
                continue
            cursor, graph_end = base + offset, base + offset + size
            assert base <= cursor < graph_end <= end
            vertices, edges = struct.unpack_from('<HH', data, cursor)
            cursor += 4 + vertices * 4
            seen = 0
            for _ in range(vertices):
                while True:
                    assert cursor + 2 <= graph_end
                    sink = struct.unpack_from('<H', data, cursor)[0]; cursor += 2
                    if not sink:
                        break
                    assert sink <= vertices
                    cursor = (cursor + 3) // 4 * 4
                    assert cursor + abi['dep_bytes'] <= graph_end
                    padding.update(range(cursor + abi['must_offset'] + abi['bool_bytes'], cursor + abi['dep_bytes']))
                    cursor += abi['dep_bytes']; seen += 1
            assert cursor == graph_end and seen <= edges
    assert position <= end
    return padding


def compare_images(first, second, abi):
    """Retain exact raw offsets/values; accept only identified padding bytes.
    Never mask arbitrary differences or change the input files."""
    left, right = first.read_bytes(), second.read_bytes()
    assert len(left) == len(right), (first, second, 'image size changed')
    differences = [i for i, values in enumerate(zip(left, right)) if values[0] != values[1]]
    if differences:
        allowed = dependence_padding(left, abi) & dependence_padding(right, abi)
        assert set(differences) <= allowed, (first.name, second.name, 'meaningful bytes changed',
                                           sorted(set(differences) - allowed)[:20])
    return dict(raw_byte_identical=not differences, meaningful_bytes_identical=True,
                classification='IDENTICAL' if not differences else 'EXISTING_DEP_STRUCT_PADDING_ONLY',
                raw_differences=[dict(offset=i, before=left[i], after=right[i]) for i in differences])
