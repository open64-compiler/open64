"""H7 fixed-artifact, device eligibility and reported-evidence contracts.

Package/server owned: no compiler, search, lowering, device probing or runtime
launch authority here. See doc/AI-COMPILER-H7-H20-QUALIFICATION-v1.0.md.
"""
import hashlib
import json
import math
from pathlib import Path
import re
import struct
from gpu_test_results import loads
from validate_gpu_results import Packet

ROLES = ('h20-gemm', 'h20-mla', 'h20-layernorm', 'h20-reduction')
PROFILES = {role: role + '.selected-f32.v1' for role in ROLES}
WALL = 'selected-operation.enqueue-stream-complete.v1'
DEVICE = 'selected-operation.cuda-events.separate-batch.v1'


def write(path, value):
    """Persist finite typed evidence, never coerce missing observations to zero."""
    Path(path).write_text(json.dumps(value, indent=2, allow_nan=False) + '\n')


def sha(path):
    """Bind the exact artifact bytes before reuse or publication."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def metadata(root, identity):
    """Read authenticated fixed launch/corpus metadata without importing it."""
    packet = Packet(Path(root), identity)
    value = loads(packet.read('h20.json'))
    if value.get('schema') != 'open64.h20-h7.packet.v1' or value.get('gpu_device') not in ('h20_96gb', 'h20_141gb'):
        raise ValueError('unsupported H20 target/schema')
    if set(value.get('profiles', {})) != set(ROLES) or value.get('search_invocations') != 0:
        raise ValueError('incomplete H20 matrix or search isolation')
    ids = set()
    for role in ROLES:
        cases = value['profiles'][role]
        if not cases:
            raise ValueError('empty H20 family')
        for case in cases:
            if not re.fullmatch('[a-z0-9_-]+', case['id']) or case['id'] in ids:
                raise ValueError('ambiguous H20 case')
            ids.add(case['id'])
            if case['gpu_device'] != value['gpu_device'] or not case['kernels'] or not case['variants']:
                raise ValueError('case target/launch/corpus missing')
            cuda=packet.read(case['source']).decode(); packet.read(case['native_trace'])
            binding=case['compiler_binding']
            if binding['gpu_device']!=value['gpu_device'] or hashlib.sha256(packet.read(binding['profile'])).hexdigest()!=binding['profile_sha256'] or hashlib.sha256(packet.read(binding['model'])).hexdigest()!=binding['model_sha256']:
                raise ValueError('compiler device/resource/model binding differs')
            if loads(packet.read(binding['profile'])).get('device')!=value['gpu_device']:
                raise ValueError('compiler profile targets another device')
            buffers={b['id']:b for b in case['buffers']}
            if len(buffers)!=len(case['buffers']) or any(not re.fullmatch('[0-9]{1,10}',key) or b['role'] not in ('input','output','scratch') for key,b in buffers.items()):
                raise ValueError('invalid buffer identity/role')
            for kernel in case['kernels']:
                if not kernel['arguments'] or len(kernel['arguments'])>256 or any(name not in buffers for name in kernel['arguments']):
                    raise ValueError('kernel argument/storage binding differs')
                if ('__global__ void '+kernel['name']+'(') not in cuda and not re.search(r'__global__\s+void\s+'+re.escape(kernel['name'])+r'\s*\(',cuda):
                    raise ValueError('selected source is not the declared native device kernel')
                if not re.fullmatch('[A-Za-z_][A-Za-z_0-9]*', kernel['name']):
                    raise ValueError('invalid kernel symbol')
                if len(kernel['grid'])!=3 or any(type(n) is not int or not 0 < n <= 2147483647 for n in kernel['grid']):
                    raise ValueError('invalid grid')
                if kernel['threads'] not in (32, 64, 128, 256, 512, 1024):
                    raise ValueError('invalid selected CTA')
                if type(kernel['shared_bytes']) is not int or not 0<=kernel['shared_bytes']<=1024**2:
                    raise ValueError('invalid shared reservation')
            if sum(b['bytes'] for b in case['buffers'])>16*1024**3 or any(type(b['bytes']) is not int or not 0<b['bytes']<=4*1024**3 for b in case['buffers']):
                raise ValueError('fixed buffer execution budget exceeded')
            variant_ids=set()
            for variant in case['variants']:
                if not re.fullmatch('[a-z0-9_-]{1,80}',variant['id']) or variant['id'] in variant_ids:
                    raise ValueError('unsafe/duplicate corpus variant')
                variant_ids.add(variant['id'])
                for direction,buffer_role in [('inputs','input'),('outputs','output')]:
                    if set(variant[direction])!={key for key,b in buffers.items() if b['role']==buffer_role}:
                        raise ValueError('variant buffer census differs')
                    for key,name in variant[direction].items():
                        factor=2 if buffer_role=='output' and case['numerical']['kind']=='f32-vs-f64' else 1
                        if len(packet.read(name))!=buffers[key]['bytes']*factor:raise ValueError('corpus extent differs from selected storage')
                for name in list(variant['inputs'].values()) + list(variant['outputs'].values()):
                    packet.read(name)
    return packet, value


def eligibility(target, observed):
    """Check explicit target plus actual identity/capacity; SM90 alone is insufficient.

    The broad memory intervals are packet admission policy, not SKU bandwidth
    facts. Partition identity/SMs and synthetic compiler profile remain explicit.
    """
    if target not in ('h20_96gb', 'h20_141gb'):
        raise ValueError('unknown selected device')
    if not re.fullmatch(r'(?:NVIDIA )?H20(?: [0-9]+GB)?', observed.get('name', '')):
        return False, 'observed GPU name is not an H20'
    if observed.get('compute_capability') != [9, 0] or not observed.get('uuid'):
        return False, 'SM90/UUID observation incomplete'
    memory = observed.get('total_memory_bytes')
    lo, hi = (80, 112) if target == 'h20_96gb' else (120, 160)
    if type(memory) is not int or not lo * 1024**3 <= memory <= hi * 1024**3:
        return False, 'observed capacity outside explicit variant admission interval'
    if observed.get('mig_enabled') is not False:
        return False, 'full-device/MIG observation missing or incompatible'
    return True, 'observed H20 name, UUID, SM90, full device and capacity interval match'


def compare(actual, expected, rule):
    """Independently check returned output bytes against server-owned goldens."""
    if rule['kind'] == 'f32-bits-nan-class':
        if len(actual) != len(expected) or len(actual) % 4:
            return False
        def same(a, b):
            return a == b or (a & 0x7fffffff) > 0x7f800000 and (b & 0x7fffffff) > 0x7f800000
        return all(same(a[0], b[0]) for a, b in zip(struct.iter_unpack('<I', actual), struct.iter_unpack('<I', expected)))
    if rule['kind'] != 'f32-vs-f64' or len(actual) * 2 != len(expected):
        return False
    for (a,), (b,) in zip(struct.iter_unpack('<f', actual), struct.iter_unpack('<d', expected)):
        if math.isnan(b):
            if not math.isnan(a): return False
        elif math.isinf(b):
            if a != b: return False
        elif not math.isfinite(a) or abs(a - b) > rule['atol'] + rule['rtol'] * abs(b):
            return False
    return True
