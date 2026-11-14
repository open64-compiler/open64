"""Portable Q7 authentication, numerical and sanitizer acceptance contracts.

Standard library only on the GPU host. No optimizer, search or kernel template.
Controlling specification: doc/AI-COMPILER-MLA-Q7-QUALIFICATION-v1.0.md.
"""
import hashlib
import math
from pathlib import Path
import re
import struct

FORMATS = {'float32': 'f', 'float64': 'd', 'int8': 'b', 'uint8': 'B', 'int64': 'q', 'bool': 'B'}


def authenticate(root, expected, allow_internal_symlinks=False):
    """Verify an externally identified manifest before loading any native code."""
    root = Path(root).resolve()
    manifest = (root / 'SHA256SUMS').read_bytes()
    if hashlib.sha256(manifest).hexdigest() != expected:
        raise ValueError('packet manifest identity differs')
    names = set()
    for row in manifest.decode().splitlines():
        digest, name = row.split('  ', 1)
        path = root / name
        if name in names or Path(name).is_absolute() or '..' in Path(name).parts or (path.is_symlink() and not allow_internal_symlinks) or root not in path.resolve().parents:
            raise ValueError('unsafe or duplicate packet path: ' + name)
        names.add(name)
        if hashlib.sha256(path.read_bytes()).hexdigest() != digest:
            raise ValueError('packet file differs: ' + name)
    return len(names)


def compare(actual, reference, dtype, reference_dtype, rtol, atol):
    """Check every element, finite tolerances and exact nonfinite classes/signs."""
    a = struct.Struct('<' + FORMATS[dtype]); b = struct.Struct('<' + FORMATS[reference_dtype])
    if len(actual) % a.size or len(reference) != len(actual) // a.size * b.size:
        raise ValueError('reference/output extent differs')
    maximum = relative = squares = 0.0
    count = 0
    for count, ((got,), (ref,)) in enumerate(zip(a.iter_unpack(actual), b.iter_unpack(reference)), 1):
        if dtype in ('int8', 'uint8', 'int64', 'bool'):
            good = got == ref
            error = abs(got - ref)
        elif not math.isfinite(ref):
            good = math.isnan(got) if math.isnan(ref) else got == ref
            error = 0.0
        else:
            good = math.isfinite(got) and math.isclose(got, ref, rel_tol=rtol, abs_tol=atol)
            error = abs(got - ref)
        if not good:
            raise ValueError('reference mismatch at element %d: %r vs %r' % (count - 1, got, ref))
        maximum = max(maximum, error)
        relative = max(relative, error / abs(ref) if ref and math.isfinite(ref) else 0.0)
        squares += error * error
    return dict(elements=count, max_absolute_error=maximum, max_relative_error=relative,
                rms_error=math.sqrt(squares / count) if count else 0.0)


def summarize(samples):
    """Retain raw finite samples; summarize a fixed workload without selecting it."""
    if not samples or any(not math.isfinite(x) or x < 0 for x in samples):
        raise ValueError('missing/invalid timing samples')
    ordered = sorted(samples)
    def percentile(fraction):
        position = (len(ordered) - 1) * fraction
        left = int(position); right = min(left + 1, len(ordered) - 1)
        return ordered[left] + (ordered[right] - ordered[left]) * (position - left)
    mean = sum(samples) / len(samples)
    return dict(samples=samples, count=len(samples), minimum=min(samples), median=percentile(.5),
                p95=percentile(.95), mean=mean, standard_deviation=math.sqrt(sum((x-mean)**2 for x in samples)/len(samples)))


def sanitizer_flags(tool, help_text):
    """Require bulk/proxy/shared coverage; older tools never silently pass."""
    required = ['check-bulk-copy', 'error-exitcode', 'report-api-errors']
    flags = ['--tool', tool, '--error-exitcode', '86', '--check-bulk-copy', 'yes', '--report-api-errors', 'no']
    if tool == 'racecheck':
        required += ['racecheck-fence-proxy-async', 'racecheck-memcpy-async', 'racecheck-detect-level']
        flags += ['--racecheck-fence-proxy-async', 'yes', '--racecheck-memcpy-async', 'yes',
                  '--racecheck-detect-level', 'info', '--racecheck-report', 'all']
    elif tool == 'initcheck':
        required += ['initcheck-address-space']
        flags += ['--initcheck-address-space', 'all']
    elif tool == 'memcheck':
        flags += ['--leak-check', 'full']
    elif tool != 'synccheck':
        raise ValueError('unsupported sanitizer')
    if any(name not in help_text for name in required):
        raise ValueError('sanitizer lacks required bulk/proxy/shared coverage: ' + tool)
    return flags


def check_sanitizer(tool, returncode, log, child):
    """A zero exit alone is insufficient: require clean summaries and real work."""
    if returncode or child.get('status') != 'PASS' or not child.get('gpu_execution') or not child.get('cases'):
        raise ValueError('sanitizer or its actual GPU child failed')
    if re.search(r'overflow|unsupported|not supported|not instrument|no attach|timed out', log, re.I):
        raise ValueError('sanitizer coverage is incomplete')
    errors = re.findall(r'ERROR SUMMARY:\s*(\d+) errors?', log)
    if any(int(x) for x in errors):
        raise ValueError('sanitizer reported errors')
    if any(int(x) for x in re.findall(r'(\d+) (?:errors?|warnings?)\b', log)):
        raise ValueError('sanitizer reported errors or warnings')
    if tool == 'racecheck':
        hazards = re.findall(r'RACECHECK SUMMARY:\s*(\d+) hazards? displayed', log)
        if not hazards or any(int(x) for x in hazards):
            raise ValueError('missing or nonzero race summary')
    elif not errors:
        raise ValueError('missing sanitizer error summary')
    return dict(status='PASS', tool=tool, clean_summaries=True, actual_GPU_child=True)
