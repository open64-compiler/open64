/* Copyright (C) 2026 Open64 Project */
/* Native Q3 source-to-serving ABI description, over owned Q1 source records.
 * No pointers, engine probes, allocator, scheduling or native IR mutation.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#ifndef tensor_serving_schema_INCLUDED
#define tensor_serving_schema_INCLUDED
#include "tensor_workload.h"
struct VHO_TENSOR_SERVING_SCHEMA {
    VHO_TENSOR_WORKLOAD_GEOMETRY geometry;
    std::vector<UINT32> roots;
    UINT32 incoming[4],returned[4],table,tokens,slots;
    UINT64 page_tokens,pages,metadata_columns;
    std::string source_key,encoding;
    bool paged;
    /* Absent roots remain explicit; no ordinal/name heuristic supplies them. */
    VHO_TENSOR_SERVING_SCHEMA():table(~0U),tokens(~0U),slots(~0U),page_tokens(0),pages(0),metadata_columns(0),paged(false) {
        for(unsigned i=0;i<4;++i) incoming[i]=returned[i]=~0U;
    }
};
/* Recognize the actual page/scale/update DAG and contiguous Q1 buffer ABI.
 * Unsupported topology refuses before a serving pointer can be consumed. */
extern bool VHO_tensor_serving_schema(const VHO_TENSOR_PROGRAM &,VHO_TENSOR_SERVING_SCHEMA &,std::string &);
/* Source comments state the dynamic binding/lifetime gate, never invent it. */
extern std::string VHO_tensor_serving_explanation(const VHO_TENSOR_PROGRAM &);
#endif
