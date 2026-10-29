/*
 * Copyright (C) 2026 Open64 Project
 */

/*
 * Managed REGION store lifecycle and WT_REGIONS mapped-image I/O owned by
 * common/ir_image. PUs, trees and shared symbols/strings remain caller-owned;
 * only the three common/com schema records enter .B, with no new persistence.
 * Physical validation remains backend-free be/com direct-object support.
 * Design: doc/COMMON-COM-DSL-PERSISTENCE-OWNERSHIP-AUDIT.md and
 * doc/WHIRL-DSL-INFRASTRUCTURE.md.
 */

#ifndef DSL_REGION_IMAGE_INCLUDED
#define DSL_REGION_IMAGE_INCLUDED

#include "../com/dsl_region.h"
#include "pu_info.h"

typedef INT32 WN_MAP;

struct output_file;

/*
 * Delete every owned runtime wrapper and store and clear the registry,
 * invalidating all handles. Borrowed WNs, PUs and shared strings survive;
 * PU subsection pointers/states are deliberately not repaired here.
 */
extern void DSL_Region_Reset (void);

/*
 * Remove one registered PU store and delete its owned wrappers, preserving
 * borrowed tree nodes. When found, clear the regions pointer and mark the
 * subsection missing; no registered store is a no-op, including its PU state.
 */
extern void DSL_Region_Discard_PU_Store (PU_Info *pu);

/*
 * Stage independent runtime/interface vectors over an already cloned PU tree;
 * borrow PUs/WNs and copy rows, clearing stored offsets. Reject invalid or
 * already registered clones without writes; a source with no store succeeds
 * only when its subsection is missing. Correspondence/verification failure
 * discards the new clone store and marks it missing, not its original state.
 * Allocation exceptions are not caught or rolled back.
 */
extern BOOL DSL_Region_Clone_PU_Store (PU_Info *source, PU_Info *clone);

/*
 * Finalize existing WT_REGIONS rows into the caller's mapped-image output,
 * using its WN offset map while keeping runtime row offsets unchanged. No
 * store/empty regions succeeds without touching output. Invalid map/output
 * rejects before writes; a zero offset rejects after header/prior rows may
 * already be written, without output rollback or subsection publication.
 * Success publishes subsection offset/size and Written state. Caller owns
 * the output/map/tree and any outer file transaction; no verifier runs here.
 */
extern BOOL DSL_Region_Write_PU (PU_Info *pu, WN_MAP off_map,
                                 struct output_file *output);

/*
 * Copy header-checked WT_REGIONS rows and attach borrowed WNs at stored tree
 * offsets, appending to an existing or newly registered store. Caller keeps
 * aligned mapped bytes, relocated tree and shared strings/symbols readable.
 * Bad arguments/header/size return -1 with EINVAL before mutation; a bad later
 * offset also sets EINVAL but retains store creation and all prior appended
 * wrappers. After all appends, verification failure retains rows/InMem state
 * and does not explicitly set errno. No rollback, whole-WN bounds validation,
 * PU activation or mapped-tree ownership transfer is provided.
 */
extern INT DSL_Region_Load_Mapped_PU (PU_Info *pu, const void *tree_base,
                                      UINT64 tree_size, const void *section_base,
                                      UINT64 section_size);

#endif /* DSL_REGION_IMAGE_INCLUDED */
