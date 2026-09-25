/*
 * Copyright (C) 2026 Open64 Project
 */

#include <string.h>

#include "opt_cfg.h"
#include "opt_dsl_locality.h"
#include "dsl_ir_image.h"
#include "dsl_tensor_locality.h"
#include "pu_info.h"

static UINT32
WOPT_DSL_Control_Flags (const BB_NODE *bb)
{
  UINT32 flags = DSL_TENSOR_CONTROL_NONE;
  if (bb->Kind() == BB_LOGIF || bb->Kind() == BB_VARGOTO ||
      bb->Kind() == BB_IO)
    flags |= DSL_TENSOR_CONTROL_BRANCH;
  if (bb->Loopdepth() != 0)
    flags |= DSL_TENSOR_CONTROL_LOOP;
  if (bb->Rid_id() != 0 || bb->Kind() == BB_REGIONSTART ||
      bb->Kind() == BB_REGIONEXIT)
    flags |= DSL_TENSOR_CONTROL_REGION;
  if (bb->Hascall())
    flags |= DSL_TENSOR_CONTROL_EFFECT_BARRIER;
  return flags;
}

BOOL
WOPT_DSL_Populate_Tensor_Control_Snapshot
    (CFG *cfg, PU_Info *pu, DSL_TENSOR_CONTROL_SNAPSHOT *snapshot,
     FILE *diagnostic)
{
  if (cfg == NULL || pu == NULL || snapshot == NULL ||
      Current_PU_Info != pu)
    return FALSE;

  /*
   * Copy stable block relationships into common/com while this PU and CFG are
   * active. No BB_NODE, STMTREP, or other WOPT-local pointer crosses the API.
   */
  CFG_ITER iterator(cfg);
  BB_NODE *bb;
  FOR_ALL_ELEM (bb, iterator, Init()) {
    DSL_TENSOR_CONTROL_BLOCK block;
    memset(&block, 0, sizeof(block));
    block.block_id = bb->Id();
    block.reverse_postorder = bb->Rpo_id() + 1;
    block.immediate_dominator = bb->Idom() == NULL ? 0 : bb->Idom()->Id();
    block.immediate_postdominator =
        bb->Ipdom() == NULL ? 0 : bb->Ipdom()->Id();
    block.loop_depth = bb->Loopdepth();
    block.region_id = bb->Rid_id();
    block.flags = WOPT_DSL_Control_Flags(bb);
    if (!DSL_Tensor_Control_Snapshot_Add_Block
             (snapshot, &block, diagnostic))
      return FALSE;
  }

  CFG_ITER position_iterator(cfg);
  FOR_ALL_ELEM (bb, position_iterator, Init()) {
    UINT32 statement_order = 0;
    for (WN *statement = bb->Firststmt(); statement != NULL;
         statement = WN_next(statement)) {
      DSL_IR_VALUE_RECORD value;
      BOOL last_statement = statement == bb->Laststmt();
      ++statement_order;
      if (DSL_IR_Image_Find_Definition_Value
              (PU_Info_proc_sym(pu), statement, &value)) {
        DSL_TENSOR_CONTROL_POSITION position;
        memset(&position, 0, sizeof(position));
        position.node_id = value.producer_node_id;
        position.block_id = bb->Id();
        position.statement_order = statement_order;
        position.reverse_postorder = bb->Rpo_id() + 1;
        if (!DSL_Tensor_Control_Snapshot_Add_Position
                 (snapshot, &position, diagnostic))
          return FALSE;
      }
      if (last_statement)
        break;
    }
  }
  return DSL_Tensor_Control_Snapshot_Seal(snapshot, diagnostic);
}
