"""Explicitly synthetic H1b schema fixtures, never H20 hardware specifications.

Even the declared-measured variant is a parser-contract fixture, not a device
measurement. Production profiles must come from separately qualified inventory.
"""
RESOURCE_UNITS = dict(visible_sms='count', warp_lanes='count',
    physical_hbm_bytes='bytes', visible_hbm_bytes='bytes', l2_bytes='bytes',
    registers_per_sm='registers', shared_bytes_per_sm='bytes',
    shared_bytes_per_cta_default='bytes', shared_bytes_per_cta_optin='bytes',
    max_threads_per_sm='count', max_threads_per_cta='count', max_blocks_per_sm='count',
    register_quantum='registers', shared_quantum='bytes')
EXECUTION_UNITS = dict(hbm_read_rate='bytes/second', hbm_write_rate='bytes/second',
    hbm_mixed_rate='bytes/second', l2_rate='bytes/second', shared_rate_per_sm='bytes/second/SM',
    simt_rate_per_sm='operations/second/SM', memory_startup_ns='nanoseconds',
    add_dependent_ns='nanoseconds', mul_dependent_ns='nanoseconds',
    exp_dependent_ns='nanoseconds', div_dependent_ns='nanoseconds', launch_ns='nanoseconds',
    async_startup_ns='nanoseconds', dequant_rate_per_sm='operations/second/SM')
TOOLCHAIN = 'fixture-toolchain-v1'


def fact(value, unit, provenance='synthetic'):
    """Create a typed explicit unknown or exactly bounded synthetic quantity."""
    return dict(lower=value, value=value, upper=value, unit=unit,
                provenance=provenance if value is not None else 'unknown',
                evidence='fixture-only' if value is not None else '')


def profile(device='h20_96gb', mode='estimated'):
    """Return a small sentinel envelope whose quantities are deliberately synthetic."""
    memory = (1 if device == 'h20_96gb' else 2) << 30
    values = dict(zip(RESOURCE_UNITS, [2 if device == 'h20_96gb' else 3, 32,
        memory, memory, 1 << 20, 65536, 65536, 32768, 49152, 1024, 256, 8, 256, 256]))
    partition = 'fixture-mig-1' if mode == 'mig' else 'fixture-full-1'
    if mode == 'mig':
        values['visible_hbm_bytes'] = memory // 2
    if mode == 'partial':
        values['visible_sms'] = None
    if mode == 'unknown':
        values = dict.fromkeys(RESOURCE_UNITS)
    resources = dict(id='fixture-resource-v1', revision=1, device=device, architecture='sm_90',
        partition_kind='mig' if mode == 'mig' else 'full', partition_id=partition,
        inventory_id='fixture-inventory-only', observed_at='2026-11-12T00:00:00Z',
        values={key: fact(values[key], unit) for key, unit in RESOURCE_UNITS.items()})
    provenance = 'measured' if mode == 'declared-measured' else 'estimated'
    execution = dict(id='fixture-execution-v1', revision=1, device=device, architecture='sm_90',
        resource_id=resources['id'], resource_revision=1, partition_id=partition,
        toolchain=TOOLCHAIN, model='fixture-model-v1', observed_at='2026-11-12T00:00:00Z',
        conditions=dict(clock='fixture-clock', power='fixture-power', ecc='fixture-ecc',
                        cache='fixture-cache', workload='fixture-workload'),
        values={key: fact(None if mode == 'unknown' else (2.5 if unit == 'nanoseconds' else 1e10),
                          unit, provenance) for key, unit in EXECUTION_UNITS.items()})
    if mode == 'unknown':
        execution['conditions'] = dict.fromkeys(execution['conditions'])
        execution['observed_at'] = None
    if mode == 'resource-only':
        execution = None
    visible = values['visible_hbm_bytes']
    budget = dict(id='fixture-budget-v1', revision=1, resource_id=resources['id'],
        resource_revision=1, partition_id=partition,
        limit_bytes=fact(visible, 'bytes'), reserved_bytes=fact(4096 if visible else None, 'bytes'),
        usable_bytes=fact(visible - 4096 if visible else None, 'bytes'))
    return dict(schema='open64.gpu-device-profile.v1', device=device, architecture='sm_90',
                device_revision=1, contract='open64.gpu-profile.sm90.v1', toolchain=TOOLCHAIN,
                resources=resources, execution=execution, budget=budget)
