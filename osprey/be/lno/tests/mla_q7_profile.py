"""Counter semantics shared by the fixed Q7 profiler and offline validator.

No CUDA, subprocess, compiler or search dependencies. Normalize only documented
base units and recompute derived values from raw counters. Contract:
doc/AI-COMPILER-MLA-Q7-PROFILING-v1.0.md.
"""
import csv
import io
import math
import re

SCHEMA = 'open64.mla-q7.profiling.v1'
# Query the installed tool before selecting any metric. Unavailable metrics
# remain unknown; a similarly named metric is never substituted heuristically.
COUNTERS = {
    'duration_ns': ('gpu__time_duration.sum', 'nsecond'),
    'issue_active_pct': ('smsp__issue_active.avg.pct_of_peak_sustained_active', '%'),
    'long_scoreboard_pct': ('smsp__warp_issue_stalled_long_scoreboard_per_warp_active.pct', '%'),
    'short_scoreboard_pct': ('smsp__warp_issue_stalled_short_scoreboard_per_warp_active.pct', '%'),
    'eligible_warps': ('smsp__warps_eligible.avg.per_cycle_active', 'warp'),
    'tensor_active_pct': ('sm__pipe_tensor_cycles_active.avg.pct_of_peak_sustained_elapsed', '%'),
    'fp32_active_pct': ('sm__pipe_fma_cycles_active.avg.pct_of_peak_sustained_elapsed', '%'),
    'hbm_read_bytes': ('dram__bytes_read.sum', 'byte'),
    'hbm_write_bytes': ('dram__bytes_write.sum', 'byte'),
    'hbm_utilization_pct': ('dram__throughput.avg.pct_of_peak_sustained_elapsed', '%'),
    'l2_bytes': ('lts__t_bytes.sum', 'byte'),
    'l2_utilization_pct': ('lts__throughput.avg.pct_of_peak_sustained_elapsed', '%'),
    'nvlink_tx_bytes': ('nvltx__bytes.sum', 'byte'),
    'nvlink_rx_bytes': ('nvlrx__bytes.sum', 'byte'),
    'pcie_read_bytes': ('pcie__read_bytes.sum', 'byte'),
    'pcie_write_bytes': ('pcie__write_bytes.sum', 'byte'),
}
NAMES = {name: key for key, (name, unit) in COUNTERS.items()}
GROUPS = ('compute_wait', 'data_paths', 'data_origin')


def number(value):
    if isinstance(value, bool) or not isinstance(value, (float, int)) or not math.isfinite(value) or value < 0:
        raise ValueError('counter must be finite and nonnegative')
    return value


def metric(value=None, unit='%', evidence='unknown', sources=(), denominator=None,
           scope='kernel', reason=None):
    """Unknowns always have null values; proxies never assert wall-time loss."""
    if value is not None:
        number(value)
    if evidence not in ('measured', 'derived', 'proxy', 'estimate', 'unknown'):
        raise ValueError('unknown evidence classification')
    if (value is None) != (evidence == 'unknown'):
        raise ValueError('unknown/value classification differs')
    return dict(value=value, unit=unit, evidence=evidence, sources=list(sources),
                denominator=denominator, scope=scope, reason=reason)


def available_metrics(text):
    tokens = set(re.findall(r'[a-z][a-z0-9_]*__[a-zA-Z0-9_.]+', text))
    return [name for name, unit in COUNTERS.values() if name in tokens]


def parse_ncu(text):
    """Read NCU raw CSV, rejecting ambiguity, duplicates, nonfinite and units.

    NCU may prepend diagnostics or repeat the header. An absent/unsupported
    value is retained as missing, never converted to zero. Device-wide metrics
    remain device-wide even when listed beside a kernel in a report.
    """
    header = None; launches = {}; notes = []
    for fields in csv.reader(io.StringIO(text)):
        if 'Metric Name' in fields and 'Metric Value' in fields:
            header = fields
            if len(set(header)) != len(header):
                raise ValueError('duplicate NCU CSV columns')
            continue
        if header and len(fields) != len(header) and any(x in NAMES for x in fields):
            raise ValueError('malformed NCU counter row')
        if not header or len(fields) != len(header):
            continue
        row = dict(zip(header, fields)); name = row['Metric Name']
        if name not in NAMES:
            continue
        ident = row.get('ID'); kernel = row.get('Kernel Name')
        if not ident or not kernel:
            raise ValueError('NCU launch identity is absent')
        if ident not in launches:
            launches[ident] = dict(id=ident, kernel=kernel, counters={})
        launch = launches[ident]
        if launch['kernel'] != kernel:
            raise ValueError('ambiguous NCU launch ID')
        key = NAMES[name]
        if key in launch['counters']:
            raise ValueError('duplicate NCU counter/launch')
        raw = row['Metric Value'].strip()
        if raw.lower() in ('n/a', 'nan', 'not available', 'not supported', ''):
            notes.append(dict(launch=ident, metric=name, reason='tool reports unavailable: '+raw))
            launch['counters'][key] = None
            continue
        # --print-units base --print-fp gives base-unit decimal values; grouping
        # commas are accepted only as valid thousands groups, not decimal commas.
        if not re.fullmatch(r'(?:\d+|\d{1,3}(?:,\d{3})+)(?:\.\d*)?(?:[eE][+-]?\d+)?', raw):
            raise ValueError('invalid/negative/nonfinite NCU value: '+raw)
        value = number(float(raw.replace(',', '')))
        unit = row.get('Metric Unit', '').strip()
        expected = COUNTERS[key][1]
        aliases = {'nsecond': ('nsecond', 'ns'), 'byte': ('byte', 'B'),
                   'warp': ('warp', 'warps'), '%': ('%', 'percent')}
        if unit not in aliases[expected]:
            raise ValueError('NCU unit differs for '+name+': '+unit)
        if expected == '%' and value > 100:
            raise ValueError('NCU percentage exceeds 100')
        launch['counters'][key] = dict(value=value, unit=expected, name=name)
    if header is None:
        raise ValueError('NCU raw CSV header is absent')
    return list(launches.values()), notes


def kernel_groups(launch):
    counters = launch['counters']
    def direct(key, evidence='measured', denominator=None, scope='kernel'):
        row = counters.get(key)
        return metric(row['value'], row['unit'], evidence, [row['name']], denominator, scope) if row else metric(
            unit=COUNTERS[key][1], reason='Counter unavailable on this GPU/tool or not collected.', scope=scope,
            denominator=denominator)
    compute = dict(
        issue_active_pct=direct('issue_active_pct', denominator='scheduler active cycles'),
        memory_dependency_stall_pct=direct('long_scoreboard_pct', 'proxy', 'active warp cycles; not elapsed time'),
        mio_dependency_stall_pct=direct('short_scoreboard_pct', 'proxy', 'active warp cycles; includes non-memory MIO'),
        eligible_warps=direct('eligible_warps', denominator='scheduler active cycles'),
        tensor_active_pct=direct('tensor_active_pct', denominator='elapsed cycles / tool sustained-peak convention'),
        fp32_active_pct=direct('fp32_active_pct', denominator='elapsed cycles / tool sustained-peak convention'),
        exposed_data_wait_pct=metric(reason='Aggregate stalls do not establish idle compute attributable to data.'),
        unknown_wait_pct=metric(reason='No mutually exclusive causal partition of compute waiting is available.'))
    active = counters.get('issue_active_pct')
    compute['issue_inactive_pct'] = metric(100-active['value'], evidence='derived', sources=[active['name']],
        denominator='scheduler active cycles', reason='Unused issue capacity; not data-wait or idle-SM percentage.') if active else metric(
        reason='Issue-active counter unavailable.', denominator='scheduler active cycles')
    paths = {key: direct(key, denominator='profiled kernel interval') for key in (
        'hbm_read_bytes', 'hbm_write_bytes', 'hbm_utilization_pct', 'l2_bytes', 'l2_utilization_pct')}
    for key in ('nvlink_tx_bytes', 'nvlink_rx_bytes', 'pcie_read_bytes', 'pcie_write_bytes'):
        paths[key] = direct(key, denominator='device counter collection interval', scope='device; not exclusive to this process')
    duration = counters.get('duration_ns')
    paths['profile_duration_ns'] = direct('duration_ns', denominator='instrumented/replayed kernel; not benchmark timing')
    for label, keys in (('hbm_bytes_per_second', ('hbm_read_bytes', 'hbm_write_bytes')), ('l2_bytes_per_second', ('l2_bytes',))):
        rows = [counters.get(key) for key in keys]
        paths[label] = metric(sum(x['value'] for x in rows)*1e9/duration['value'], 'byte/second', 'derived',
            [x['name'] for x in rows]+[duration['name']], 'instrumented kernel duration') if duration and duration['value']>0 and all(rows) else metric(
                unit='byte/second', reason='Missing byte count or positive profiled duration.')
    paths['time_bandwidth_saturated_pct'] = metric(reason='Average counters do not resolve time above a saturation threshold.')
    origins = {key: metric(reason='Requires buffer provenance and exposed critical-path dependency attribution.') for key in (
        'local_gpu_exposed_wait_pct', 'peer_gpu_exposed_wait_pct', 'remote_node_exposed_wait_pct')}
    return dict(compute_wait=compute, data_paths=paths, data_origin=origins)


def summarize_ncu(text):
    launches, notes = parse_ncu(text)
    return dict(kernels=[dict(id=x['id'], kernel=x['kernel'], groups=kernel_groups(x)) for x in launches], notes=notes)


def network_rates(before, after):
    """Host-interface totals are ambient traffic, never GPU-origin attribution."""
    start = number(before['monotonic_ns']); stop = number(after['monotonic_ns'])
    if stop <= start:
        raise ValueError('network snapshot interval is not positive')
    result = []
    for name in sorted(set(before['interfaces']) | set(after['interfaces'])):
        a = before['interfaces'].get(name); b = after['interfaces'].get(name)
        for direction in ('rx', 'tx'):
            key = direction+'_bytes'
            if a and b and key in a and key in b and number(b[key]) >= number(a[key]):
                value = metric((b[key]-a[key])*1e9/(stop-start), 'byte/second', 'derived',
                    ['network-before.json', 'network-after.json'], 'host monotonic snapshot interval',
                    'host interface '+name+'; all processes; not necessarily GPU traffic')
            else:
                value = metric(unit='byte/second', reason='Interface unavailable, changed or counter reset.', scope='host interface '+name)
            result.append(dict(interface=name, direction=direction, measurement=value))
    return result
