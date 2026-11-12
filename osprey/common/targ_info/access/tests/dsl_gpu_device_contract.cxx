/* Copyright (C) 2026 Open64 Project */
/* Common targ_info H1a exact device/ISA contract test. Exercise production
 * lookup, immutable caller results and old architecture rows without native
 * compiler initialization, hardware probing or optimizer policy.
 * Design: doc/AI-COMPILER-H20-MEMORY-HIERARCHY-IMPLEMENTATION-PLAN-v1.1.md. */
#include "dsl_memory_hierarchy.h"

static unsigned checks = 0;

/* Count an independent contract assertion and fail with its boundary name. */
static void check_contract(BOOL passed, const char *reason)
{
    ++checks;
    if (!passed) {
        fprintf(stderr, "gpu_device contract failed: %s\n", reason);
        exit(1);
    }
}

/* A refused request must preserve all caller bytes, including sentinel state;
 * this checks the production resolver, not a duplicate compatibility model. */
static void check_refusal(const char *arch, const char *device)
{
    DSL_HARDWARE_TARGET_SELECTION result;
    memset(&result, 0xa5, sizeof(result));
    unsigned char before[sizeof(result)];
    memcpy(before, &result, sizeof(result));
    check_contract(!DSL_hardware_target_resolve(arch, device, &result, NULL),
                   "unsupported request accepted");
    check_contract(memcmp(before, &result, sizeof(result)) == 0,
                   "refusal modified caller output");
}

/* Verify exact identities, H1a baseline admission and architecture-only parity.
 * No source/IR or resource tables are mutated; successful output is identity
 * only and is not a measured or device-specific performance description. */
int main()
{
    const char *names[] = { "h20_96gb", "h20_141gb" };
    const UINT32 ids[] = { DSL_HARDWARE_DEVICE_H20_96GB,
                           DSL_HARDWARE_DEVICE_H20_141GB };
    DSL_HARDWARE_DESCRIPTION_RECORD original;
    check_contract(DSL_hardware_description_find_arch("sm_90", &original),
                   "missing legacy row");
    for (unsigned i = 0; i < 2; ++i) {
        DSL_HARDWARE_DEVICE_RECORD device;
        check_contract(DSL_hardware_device_find(names[i], &device), "exact lookup");
        check_contract(device.device_id == ids[i] && device.revision == 1 &&
                       device.baseline_arch_id == DSL_HARDWARE_ARCH_SM_90 &&
                       strcmp(device.name, names[i]) == 0, "device identity");
        DSL_HARDWARE_TARGET_SELECTION implicit, explicit_arch;
        check_contract(DSL_hardware_target_resolve(NULL, names[i], &implicit, NULL),
                       "device-only baseline");
        check_contract(DSL_hardware_target_resolve("sm_90", names[i],
                       &explicit_arch, NULL), "explicit compatible ISA");
        check_contract(implicit.arch_id == DSL_HARDWARE_ARCH_SM_90 &&
                       strcmp(implicit.architecture, "sm_90") == 0 &&
                       implicit.device.device_id == ids[i] &&
                       explicit_arch.device.device_id == ids[i] &&
                       explicit_arch.arch_id == implicit.arch_id,
                       "device-only/explicit mismatch");
        check_refusal("sm_90a", names[i]);
        check_refusal("sm_100", names[i]);
        check_refusal("sm_100a", names[i]);
        check_refusal("m90", names[i]);
        check_refusal("sm_90typo", names[i]);
        check_refusal("", names[i]);
    }
    const char *bad[] = { "", "h20", "H20_96GB", "h20_96gb_extra",
                          " h20_96gb", "h200_141gb" };
    for (unsigned i = 0; i < sizeof(bad) / sizeof(bad[0]); ++i) {
        DSL_HARDWARE_DEVICE_RECORD result;
        memset(&result, 0x5a, sizeof(result));
        unsigned char before[sizeof(result)];
        memcpy(before, &result, sizeof(result));
        check_contract(!DSL_hardware_device_find(bad[i], &result),
                       "non-exact device lookup");
        check_contract(memcmp(before, &result, sizeof(result)) == 0,
                       "lookup failure changed output");
        check_refusal("sm_90", bad[i]);
    }
    check_contract(!DSL_hardware_device_find(NULL, NULL), "null lookup");
    check_contract(!DSL_hardware_device_find(names[0], NULL), "null output");
    check_contract(!DSL_hardware_target_resolve(NULL, names[0], NULL, NULL),
                   "null resolution output");
    const char *arches[] = { "sm_90", "sm_90a", "sm_100", "sm_100a" };
    for (unsigned i = 0; i < 4; ++i) {
        DSL_HARDWARE_TARGET_SELECTION selected;
        DSL_HARDWARE_DESCRIPTION_RECORD description;
        check_contract(DSL_hardware_target_resolve(arches[i], NULL, &selected, NULL),
                       "architecture-only admission changed");
        check_contract(DSL_hardware_description_find_arch(arches[i], &description) &&
                       selected.arch_id == description.arch_id &&
                       selected.device.device_id == DSL_HARDWARE_DEVICE_UNSPECIFIED &&
                       selected.device.revision == 0 &&
                       strcmp(selected.device.name, "unspecified") == 0,
                       "architecture-only identity");
    }
    DSL_HARDWARE_TARGET_SELECTION generic;
    check_contract(DSL_hardware_target_resolve(NULL, NULL, &generic, NULL) &&
                   generic.arch_id == DSL_HARDWARE_ARCH_SM_90 &&
                   generic.device.device_id == DSL_HARDWARE_DEVICE_UNSPECIFIED,
                   "legacy default");
    DSL_HARDWARE_DESCRIPTION_RECORD after;
    check_contract(DSL_hardware_description_find_arch("sm_90", &after) &&
                   memcmp(&original, &after, sizeof(after)) == 0,
                   "immutable architecture table");
    printf("gpu_device production contract passed: %u checks\n", checks);
    return 0;
}
