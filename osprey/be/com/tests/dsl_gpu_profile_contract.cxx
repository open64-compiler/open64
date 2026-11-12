/* Copyright (C) 2026 Open64 Project */
/* H1b focused import/ownership oracle against the production backend service.
 * No native compiler initialization or GPU; fixtures declare their provenance.
 * Design: doc/AI-COMPILER-H1B-GPU-PROFILE-IMPORT-v1.0.md. */
#include "dsl_gpu_profile.h"
#include <type_traits>
#include <utility>
#include <stdlib.h>
#include <string.h>

static_assert(!std::is_copy_constructible<DSL_GPU_DEVICE_CONTEXT>::value,
              "sealed context cannot expose mutable copies");
static_assert(std::is_same<decltype(std::declval<const DSL_GPU_DEVICE_CONTEXT &>().Resource(
              DSL_GPU_VISIBLE_SMS)), const DSL_GPU_RESOURCE_FACT *>::value,
              "facts must be borrowed const");

/* Fail at the precise production contract boundary; no assertion is elided. */
static void require_contract(bool condition, const char *reason)
{
    if (!condition) { fprintf(stderr, "H1b contract failed: %s\n", reason); exit(1); }
}

/* Exercise one supplied profile, preserving a prior context on all refusals.
 * Optional mutate mode proves the returned context has no live file/DOM views. */
int main(int argc, char **argv)
{
    require_contract(argc == 6 || argc == 7, "usage: FILE SHA DEVICE TOOLCHAIN pass|fail [mutate]");
    DSL_HARDWARE_TARGET_SELECTION target;
    require_contract(DSL_hardware_target_resolve(NULL, argv[3], &target, stderr), "target");
    DSL_GPU_DEVICE_CONTEXT_OWNER context;
    require_contract(DSL_gpu_profile_resolve(target, NULL, NULL, NULL, &context, stderr), "unknown context");
    const DSL_GPU_DEVICE_CONTEXT *unknown = context.Context();
    const std::string unknown_key = context.Context()->Context_sha256();
    require_contract(!context.Context()->Imported() && !context.Context()->Resource_known() &&
        !context.Context()->Execution_known() && !context.Context()->Budget_bounds_proved() &&
        !context.Context()->Resource(DSL_GPU_VISIBLE_HBM_BYTES)->known, "unknown is not zero/infinity");
    const bool loaded = DSL_gpu_profile_resolve(target, argv[1], argv[2], argv[4], &context, stderr);
    if (strcmp(argv[5], "fail") == 0) {
        require_contract(!loaded && context.Context() == unknown &&
            context.Context()->Context_sha256() == unknown_key, "failed import changed previous context");
        printf("PROFILE_REFUSED previous_context_preserved=1\n");
        return 0;
    }
    require_contract(strcmp(argv[5], "pass") == 0 && loaded, "expected import");
    require_contract(context.Context()->Imported() && context.Context()->Device() == argv[3] &&
        context.Context()->Toolchain() == argv[4] && context.Context()->Input_sha256() == argv[2] &&
        context.Context()->Resource_sha256().size() == 64 && context.Context()->Budget_sha256().size() == 64 &&
        context.Context()->Context_sha256().size() == 64, "owned identities");
    require_contract(context.Context()->Resource(DSL_GPU_RESOURCE_FIELD(-1)) == NULL &&
        context.Context()->Resource(DSL_GPU_RESOURCE_COUNT) == NULL &&
        context.Context()->Execution(DSL_GPU_EXECUTION_COUNT) == NULL &&
        context.Context()->Budget(DSL_GPU_BUDGET_COUNT) == NULL && context.Context()->Condition(5) == NULL, "bounded accessors");
    const DSL_GPU_DEVICE_CONTEXT *sealed = context.Context();
    const std::string sealed_key = context.Context()->Context_sha256();
    const std::string resource_key = context.Context()->Resource_sha256();
    const uint64_t sms = context.Context()->Resource(DSL_GPU_VISIBLE_SMS)->value;
    const std::string evidence = context.Context()->Resource(DSL_GPU_VISIBLE_SMS)->evidence;
    const char *bad_digest = "0000000000000000000000000000000000000000000000000000000000000000";
    require_contract(!DSL_gpu_profile_resolve(target, argv[1], bad_digest, argv[4], &context, NULL) &&
        context.Context() == sealed && context.Context()->Context_sha256() == sealed_key, "bad digest transaction");
    require_contract(!DSL_gpu_profile_resolve(target, argv[1], argv[2], "wrong-toolchain", &context, NULL) &&
        context.Context() == sealed, "wrong toolchain transaction");
    DSL_HARDWARE_TARGET_SELECTION other_target;
    const char *other = strcmp(argv[3], "h20_96gb") == 0 ? "h20_141gb" : "h20_96gb";
    require_contract(DSL_hardware_target_resolve(NULL, other, &other_target, NULL), "other target");
    require_contract(!DSL_gpu_profile_resolve(other_target, argv[1], argv[2], argv[4], &context, NULL) &&
        context.Context() == sealed, "cross-device transaction");
    require_contract(!DSL_gpu_profile_resolve(target, NULL, argv[2], NULL, &context, NULL) &&
        context.Context() == sealed, "orphan companion transaction");
    DSL_GPU_DEVICE_CONTEXT_OWNER independent;
    require_contract(DSL_gpu_profile_resolve(target, argv[1], argv[2], argv[4], &independent, NULL) &&
        independent.Context() != sealed && independent.Context()->Context_sha256() == sealed_key, "independent owned snapshot");
    if (argc == 7) {
        require_contract(strcmp(argv[6], "mutate") == 0, "mutation mode");
        FILE *file = fopen(argv[1], "wb");
        require_contract(file != NULL, "mutation file open");
        const char replacement[] = "not the imported JSON anymore\n";
        const bool written = fwrite(replacement, 1, sizeof(replacement)-1, file) == sizeof(replacement)-1;
        const bool closed = fclose(file) == 0;
        require_contract(written && closed, "mutation file write");
        require_contract(!DSL_gpu_profile_resolve(target, argv[1], argv[2], argv[4], &context, NULL) &&
            context.Context() == sealed && context.Context()->Context_sha256() == sealed_key, "post-file-change refusal");
    }
    context.Reset();
    require_contract(independent.Context()->Context_sha256() == sealed_key &&
        independent.Context()->Resource_sha256() == resource_key &&
        independent.Context()->Resource(DSL_GPU_VISIBLE_SMS)->value == sms &&
        independent.Context()->Resource(DSL_GPU_VISIBLE_SMS)->evidence == evidence,
        "snapshot lifetime independent of file/DOM/other context");
    printf("PROFILE_ACCEPT device=%s resource_known=%u execution_known=%u budget_proved=%u "
        "resource_sha256=%s execution_sha256=%s budget_sha256=%s context_sha256=%s "
        "visible_sms=%llu input_sha256=%s\n", independent.Context()->Device().c_str(), independent.Context()->Resource_known(),
        independent.Context()->Execution_known(), unsigned(independent.Context()->Budget_bounds_proved()),
        independent.Context()->Resource_sha256().c_str(), independent.Context()->Execution_sha256().c_str(),
        independent.Context()->Budget_sha256().c_str(), independent.Context()->Context_sha256().c_str(),
        (unsigned long long)sms, independent.Context()->Input_sha256().c_str());
    return 0;
}
