/* Copyright (C) 2026 Open64 Project */
/* Offline PU-local MLA chunk selection. This phase reuses the common hardware
 * identity registry and existing VHO occupancy arithmetic; no emitter policy,
 * device probing, new IR layout or be.so dependency. See MLA-AUTO-CHUNKS-v0.1. */
#include <algorithm>
#include <cctype>
#include <sstream>
#include <stdexcept>
#include "tensor_kernel_lower.h"
#include "dsl_memory_hierarchy.h"
#include "dsl_gemm_occupancy.h"

namespace {
/* Bounds make all coverage, footprint and rounded-capacity arithmetic finite. */
void require(bool ok,const char *why) {if(!ok) throw std::runtime_error(why);}
bool identifier(const std::string &s) {
    if(s.empty() || s.size()>96) return false;
    for(size_t i=0;i<s.size();++i)
        if(!std::isalnum((unsigned char)s[i]) && s[i]!='_' && s[i]!='-' && s[i]!='.') return false;
    return true;
}
/* Strictly compare achieved fractions, then minimize merge/fixed work.
 * Enumeration order favors direct attention for equivalent one-chunk plans. */
bool better(const VHO_TENSOR_CHUNK_CANDIDATE &a,const VHO_TENSOR_CHUNK_CANDIDATE &b) {
    UINT64 left=a.coverage*b.goal,right=b.coverage*a.goal;
    if(left!=right) return left>right;
    if(a.chunks!=b.chunks) return a.chunks<b.chunks;
    return a.width<b.width;
}
}

/* The schema deliberately labels this envelope estimated. Actual native
 * allocation and runtime occupancy still require backend/device evidence. */
BOOL VHO_tensor_gpu_profile_valid(const VHO_TENSOR_GPU_PROFILE &g,FILE *f) {
    try {
        DSL_HARDWARE_DESCRIPTION_RECORD arch;
        require(identifier(g.id) && (g.architecture=="sm_90" || g.architecture=="sm_90a") &&
            DSL_hardware_description_find_arch(g.architecture.c_str(),&arch),"unsupported MLA GPU profile identity/architecture");
        require(g.sms && g.sms<=1024 && g.registers_per_thread && g.registers_per_thread<=255 &&
            g.target_ctas_per_sm && g.target_ctas_per_sm<=32 && g.scratch_budget<=(UINT64(1)<<32),"invalid MLA profile policy bounds");
        const DSL_HARDWARE_RESOURCE_RECORD &r=g.resources;
        require(DSL_hardware_resources_valid(r),"incomplete MLA GPU resources");
        const UINT64 fields[]={r.max_blocks,r.max_warps,r.max_threads,r.max_threads_per_cta,
            r.registers_per_sm,r.max_registers_per_thread,r.register_quantum,r.register_warp_quantum,
            r.shared_capacity,r.shared_budget,r.shared_quantum,r.shared_reserved};
        for(UINT32 i=0;i<12;++i) require(fields[i]<=(UINT64(1)<<32),"oversized MLA resource field");
        require(r.max_blocks<=1024 && r.max_warps<=65536 && r.max_threads<=1048576 &&
            r.max_threads_per_cta<=r.max_threads && r.shared_reserved<r.shared_budget &&
            r.max_registers_per_thread<=255,"inconsistent MLA GPU resources");
        return TRUE;
    } catch(const std::exception &e) {if(f) fprintf(f,"MLA GPU profile: %s\n",e.what());return FALSE;}
}

/* Select from admitted native plans, before any WHIRL/ST/TY allocation.
 * Disabling auto on the private candidate prevents recursive auto selection. */
BOOL VHO_tensor_select_attention_chunks(const VHO_TENSOR_PROGRAM &input,
    const VHO_TENSOR_GPU_PROFILE &g,VHO_TENSOR_CHUNK_SELECTION *output,FILE *f) {
    try {
        require(output && VHO_tensor_gpu_profile_valid(g,f),"invalid automatic chunk output/profile");
        require(input.chunk_policy==1 || input.chunk_policy==2 || input.chunk_policy==3,"unknown automatic chunk policy");
        VHO_TENSOR_PROGRAM base=input;base.auto_chunks=false;
        require(base.register_latent && VHO_tensor_validate_program(base,f),"automatic chunks require admitted register attention");
        VHO_TENSOR_ATTENTION a=VHO_tensor_attention(base);require(a.enabled,"automatic chunks require absorbed attention");
        VHO_TENSOR_CHUNK_SELECTION result;UINT32 widths[]={0,32,64,128,256,512,1024,2048,4096};
        size_t winner=0;bool found=false;
        for(UINT32 w=0;w<sizeof(widths)/sizeof(widths[0]);++w) {
            UINT32 width=widths[w];
            if(input.chunk_policy==1 && width && width<128) continue;
            if(!width && a.length>4096) continue;
            if(width && width>=a.length) continue;
            VHO_TENSOR_CHUNK_CANDIDATE c;c.width=width;
            UINT64 span=width?width:a.length;c.chunks=(a.length+span-1)/span;
            c.ctas=a.batch*a.heads*c.chunks;
            c.threads=input.chunk_policy==3 && width && width<=64?width:128;
            c.local_shared=4*(((span+c.threads-1)/c.threads)*c.threads+8);
            c.merge_shared=c.chunks>1?4*(((c.chunks+127)/128)*128+8):0;
            c.scratch=c.chunks>1?4*a.batch*a.heads*c.chunks*(a.rank+2):0;
            c.reason="native_capacity";
            VHO_TENSOR_PROGRAM candidate=base;candidate.split_width=width;VHO_TENSOR_SCHEDULE plan;
            if(VHO_tensor_schedule(candidate,&plan,NULL)) {
                c.reason="scratch_budget";
                if(c.scratch<=g.scratch_budget) {
                    c.reason="resource_fit";bool fits=true;
                    for(size_t k=0;k<plan.kernels.size();++k) {
                        DSL_GEMM_OCCUPANCY_RESULT occupancy;
                        if(!DSL_gemm_occupancy(g.resources,32,g.sms,plan.kernels[k].threads,4*g.registers_per_thread,
                            VHO_tensor_shared_bytes(candidate,plan.kernels[k]),VHO_tensor_grid(candidate,plan.kernels[k]),&occupancy) ||
                            !occupancy.resident) {fits=false;break;}
                        if(plan.kernels[k].mapping==3) c.resident=occupancy.resident;
                    }
                    if(fits && c.resident) {
                        c.eligible=true;c.reason="eligible";
                        c.goal=g.sms*std::min(g.target_ctas_per_sm,c.resident);
                        c.coverage=std::min(c.ctas,c.goal);
                        if(!found || better(c,result.candidates[winner])) {winner=result.candidates.size();found=true;}
                    }
                }
            }
            result.candidates.push_back(c);
        }
        require(found,"no legal MLA chunk candidate fits GPU/scratch envelope");
        /* Avoid short-row split/merge overhead when the direct plan fits.
         * This threshold is part of the versioned policy, never an emitter rule. */
        if(a.length<=256)
            for(size_t i=0;i<result.candidates.size();++i)
                if(!result.candidates[i].width && result.candidates[i].eligible) {winner=i;result.short_direct=true;break;}
        result.width=result.candidates[winner].width;*output=result;return TRUE;
    } catch(const std::exception &e) {if(f) fprintf(f,"MLA chunk selection: %s\n",e.what());return FALSE;}
}

/* Policy identity is sealed independently of the unchanged v6 record shape.
 * Legacy recipes keep their original enumeration, metadata and fingerprint. */
const char *VHO_tensor_chunk_policy_name(const VHO_TENSOR_PROGRAM &p) {
    if(p.chunk_policy==1) return "coverage-v1";
    if(p.chunk_policy==2) return "coverage-v2";
    if(p.chunk_policy==3) return "coverage-v3";
    throw std::runtime_error("unknown automatic chunk policy");
}

/* Full profile and candidate ledger are sealed by the recipe fingerprint.
 * This concise identity is additionally persisted on each native kernel. */
std::string VHO_tensor_chunk_selection_identity(const VHO_TENSOR_PROGRAM &p) {
    std::ostringstream s;s<<VHO_tensor_chunk_policy_name(p)<<":"<<p.gpu.id<<":"<<p.gpu.architecture<<":sms="<<p.gpu.sms
        <<":regs_est="<<p.gpu.registers_per_thread<<":target="<<p.gpu.target_ctas_per_sm
        <<":scratch="<<p.gpu.scratch_budget<<":width="<<p.split_width;
    return s.str();
}
