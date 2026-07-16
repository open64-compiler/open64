/*
 * Copyright (C) 2026 Open64 Project
 */

#ifndef dsl_memory_behavior_INCLUDED
#define dsl_memory_behavior_INCLUDED

#include "defs.h"
#include "dsl_opcode.h"

#define DSL_MEMORY_BEHAVIOR_MAX_OPERANDS 8

typedef enum {
    DSL_MEMORY_BEHAVIOR_NONE = 0,
    DSL_MEMORY_BEHAVIOR_READ = 0x0001,
    DSL_MEMORY_BEHAVIOR_MODIFY = 0x0002,
    DSL_MEMORY_BEHAVIOR_UNIQUE_OWNERSHIP = 0x0004,
    DSL_MEMORY_BEHAVIOR_SHARED_OWNERSHIP = 0x0008,
    DSL_MEMORY_BEHAVIOR_FRESH_RESULT = 0x0010,
    DSL_MEMORY_BEHAVIOR_VIEW = 0x0020,
    DSL_MEMORY_BEHAVIOR_MAY_ALIAS = 0x0040,
    DSL_MEMORY_BEHAVIOR_INPLACE_UPDATE = 0x0080,
    DSL_MEMORY_BEHAVIOR_CONSUMES = 0x0100
} DSL_MEMORY_BEHAVIOR_FLAG;

typedef struct {
    DSL_OPERATOR dsl_operator;
    UINT16 version;
    UINT16 operand_count;
    UINT32 operand_flags[DSL_MEMORY_BEHAVIOR_MAX_OPERANDS];
    UINT32 result_flags;
    INT16 related_operand;
    UINT16 reserved;
} DSL_MEMORY_BEHAVIOR_CONTRACT;

extern BOOL DSL_Memory_Behavior_Get_Contract
                                (DSL_OPERATOR dsl_operator,
                                 UINT16 version,
                                 DSL_MEMORY_BEHAVIOR_CONTRACT *contract);
extern const char *DSL_Memory_Behavior_Flag_Name (UINT32 flag);

#endif /* dsl_memory_behavior_INCLUDED */
