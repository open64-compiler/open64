/* Copyright (C) 2026 Open64 Project */
/* Detached Q6 admission, byte partitioning and pipeline lifetime verification.
 * Separate selected-plan service; no runtime observation, candidate search
 * or mapped-WHIRL allocation. Owning contract:
 * doc/AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md. */
#include "tensor_quantized_gpu.h"
#include <sstream>
#include <stdexcept>
#include <algorithm>
#include <map>
namespace {
/* Fail closed before publication on an unsupported or overflowing layout. */
void Q6_need(bool ok,const char *why) {if(!ok) throw std::runtime_error(why);}
/* Resource and byte arithmetic must never turn overflow into apparent fit. */
UINT64 Q6_add(UINT64 a,UINT64 b) {Q6_need(a<=~UINT64(0)-b,"q6_add_overflow");return a+b;}
/* All multiplicands are unsigned, independent of the host pointer ABI. */
UINT64 Q6_mul(UINT64 a,UINT64 b) {Q6_need(!b || a<=~UINT64(0)/b,"q6_mul_overflow");return a*b;}
/* Every separately addressed raw stream starts on a 16-byte boundary. */
UINT64 Q6_align(UINT64 a) {return Q6_add(a,15)/16*16;}
/* Zero extents never represent a valid device worker. */
UINT64 Q6_ceil(UINT64 a,UINT64 b) {Q6_need(b!=0,"q6_zero_divisor");return a/b+(a%b!=0);}
/* Exact identities include delimiters and cannot alias through embedded text. */
void Q6_text(std::ostringstream &s,const std::string &v) {s<<v.size()<<':'<<v<<';';}
}
/* Not a signature: consumers must revalidate the actual device lease. */
std::string VHO_q6_mapping_key(const VHO_Q6_MAPPING &m) {
    std::ostringstream s;s<<"mla-q6-placement-v1;";
    const UINT64 values[]={m.batch,m.heads,m.queries,m.keys,m.latent,m.rotary,m.width,m.group,
        m.grid,m.threads,m.tiles,m.raw_stride,m.decoded_offset,m.decoded_bytes,m.barrier_offset,m.shared_bytes,m.slots,
        m.control.delivery,m.control.arch,m.control.ptx,m.control.toolkit,m.control.max_threads,m.control.shared_limit};
    for(unsigned i=0;i<sizeof(values)/sizeof(values[0]);++i) s<<values[i]<<';';
    for(unsigned i=0;i<4;++i) s<<m.raw_row[i]<<';'<<m.raw_offset[i]<<';';
    Q6_text(s,m.selected_key);Q6_text(s,m.plan_key);Q6_text(s,m.binding_key);return s.str();
}
/* Source recognition and canonical selection checks precede layout arithmetic.
 * Device resources are explicit; host page alignment never enables TMA. */
bool VHO_q6_mapping(const VHO_TENSOR_PROGRAM &p,const VHO_Q5_PLAN &plan,
    const std::string &selected_key,const VHO_Q6_CONTROL &c,VHO_Q6_MAPPING &out,std::string &why) {
    try {
        VHO_Q5_PLAN canonical;Q6_need(VHO_q5_plan_decode(VHO_q5_plan_encode(plan),canonical,why),"q6_invalid_selected_plan");
        Q6_need(plan.width && !plan.native_key.empty() && !selected_key.empty(),"q6_requires_selected_optimized_native_plan");
        VHO_Q5_LAYOUT layout;Q6_need(VHO_q5_layout(p,layout,why),"q6_unsupported_source_layout");
        Q6_need(plan.source_key==layout.schema.source_key,"q6_selected_source_mismatch");
        Q6_need((c.delivery==VHO_Q6_SYNCHRONOUS || c.delivery==VHO_Q6_BULK_TMA) &&
            c.arch==90 && c.max_threads && c.max_threads<=1024 && c.shared_limit && c.shared_limit<=48*1024,"q6_explicit_mapping_envelope");
        // Match the already reviewed MMA5-H conservative toolchain gate. This
        // is a supported implementation floor, not a claim about ISA minima.
        if(c.delivery==VHO_Q6_BULK_TMA) Q6_need(c.ptx>=806 && c.toolkit>=12080,"q6_bulk_toolchain_gate");
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=layout.schema.geometry;VHO_Q6_MAPPING m;m.control=c;
        m.batch=g.batch;m.heads=g.heads;m.queries=g.queries;m.keys=g.keys;m.latent=g.latent;m.rotary=g.rotary;
        Q6_need(m.batch && m.heads && m.queries && m.keys && m.latent && m.rotary,"q6_empty_geometry");
        m.width=plan.width;m.group=plan.group;m.tiles=Q6_ceil(m.keys,m.width);m.threads=Q6_mul(32,m.group);
        m.grid=Q6_mul(Q6_mul(m.batch,m.queries),Q6_ceil(m.heads,m.group));
        Q6_need(m.grid<=0xffffffffULL && m.threads<=c.max_threads,"q6_launch_extent");
        const bool u4=layout.schema.encoding=="u4-offset8-f32.v1";
        Q6_need(u4 || layout.schema.encoding=="s8-token-scaled-f32.v1","q6_encoding");
        if(u4) Q6_need(!(m.latent%2) && !(m.rotary%2),"q6_packed_row_extent");
        m.raw_row[0]=u4?m.latent/2:m.latent;m.raw_row[1]=4;
        m.raw_row[2]=u4?m.rotary/2:m.rotary;m.raw_row[3]=4;
        for(unsigned i=0;i<4;++i) {m.raw_offset[i]=m.raw_stride;m.raw_stride=Q6_add(m.raw_stride,Q6_align(Q6_mul(m.width,m.raw_row[i])));}
        m.slots=c.delivery==VHO_Q6_BULK_TMA?2:1;m.decoded_offset=Q6_mul(m.slots,m.raw_stride);
        m.decoded_bytes=Q6_add(Q6_align(Q6_mul(Q6_mul(m.width,m.latent),4)),Q6_align(Q6_mul(Q6_mul(m.width,m.rotary),4)));
        m.barrier_offset=Q6_add(m.decoded_offset,m.decoded_bytes);
        m.shared_bytes=Q6_align(Q6_add(m.barrier_offset,c.delivery==VHO_Q6_BULK_TMA?16:0));
        Q6_need(m.shared_bytes<=c.shared_limit,"q6_shared_resource_refusal");
        m.selected_key=selected_key;m.plan_key=VHO_q5_plan_key(plan);m.binding_key=plan.binding_key;
        std::ostringstream e;e<<"Q6 selected placement proposal: B="<<m.batch<<", H="<<m.heads<<", Q="<<m.queries<<", L="<<m.keys
            <<", W="<<m.width<<", G="<<m.group<<". One CTA owns a query/head-group and its complete ordered token folds. "
            <<m.slots<<" raw slot(s), each with distinct latent payload/scale and rotary payload/scale ranges; one decoded F32 tile is reused across heads. ";
        if(c.delivery==VHO_Q6_BULK_TMA) e<<"Wait current raw slot, issue next raw slot, dequantize current, publish to every consumer, compute current, release after every consumer finishes. Only aligned in-bounds contiguous fragments use bulk TMA; other fragments use synchronous copies. ";
        else e<<"Synchronous raw staging and dequantization; no asynchronous-overlap credit. ";
        e<<"S8 sign extension or even-low U4 unpack/offset-eight conversion precedes multiplication by the matching F32 scale. Three source-ordered max/denominator/weighted passes remain; no implicit online-softmax or split-reduction reassociation. "
            <<"This detached contract is not an emitted GPU kernel, device binding, launch lease or measured cost. Q3 source/buffer/page/cache/scale generations must be rebound by a device owner through completion and atomic publication. Q5 host bindings cannot authorize GPU execution. "
            <<"Search remains estimation-only; no candidate was transformed here. AOT selection remains deferred.";
        m.explanation=e.str();m.key=VHO_q6_mapping_key(m);out=m;why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Byte-exact partitioning of a single already bounded physical run. The caller
 * must split page discontinuities before this API; no gather is treated as a
 * bulk tensor-map load. Bulk total is the mbarrier expected-transaction count. */
bool VHO_q6_partition(UINT64 source,UINT64 destination,UINT64 bytes,UINT64 source_size,
    UINT64 destination_size,UINT32 source_base,UINT32 destination_base,bool tma,
    std::vector<VHO_Q6_COPY> &out,UINT64 &transaction_bytes,std::string &why) {
    try {
        Q6_need(bytes && source_base<16 && destination_base<16,"q6_run_shape");
        Q6_need(source<=source_size && bytes<=source_size-source && destination<=destination_size && bytes<=destination_size-destination,"q6_run_bounds");
        std::vector<VHO_Q6_COPY> parts;UINT64 bulk=0,prefix=bytes;
        UINT32 sm=(source%16+source_base)%16,dm=(destination%16+destination_base)%16;
        if(tma && sm==dm) {prefix=std::min(bytes,UINT64((16-sm)%16));bulk=(bytes-prefix)/16*16;}
        // Conservative one-barrier transaction-count envelope. Larger runs
        // retain a complete synchronous copy instead of truncating a count.
        if(bulk>0xffff0) {prefix=bytes;bulk=0;}
        if(prefix) parts.push_back(VHO_Q6_COPY(source,destination,prefix,false));
        if(bulk) parts.push_back(VHO_Q6_COPY(source+prefix,destination+prefix,bulk,true));
        UINT64 tail=bytes-prefix-bulk;if(tail) parts.push_back(VHO_Q6_COPY(source+prefix+bulk,destination+prefix+bulk,tail,false));
        out.swap(parts);transaction_bytes=bulk;why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* A common run list preserves latent/rotary/scale page joins. Destination
 * bytes are packed in logical token order within this exact raw slot. */
bool VHO_q6_transfers(const VHO_Q6_MAPPING &m,UINT64 tile,UINT32 slot,
    const std::vector<VHO_Q6_PHYSICAL_RUN> &runs,const UINT64 source_sizes[4],
    const UINT32 residues[4],std::vector<VHO_Q6_TRANSFER> &out,UINT64 &transaction_bytes,std::string &why) {
    try {
        Q6_need(m.key==VHO_q6_mapping_key(m) && tile<m.tiles && slot<m.slots && slot==tile%m.slots &&
            source_sizes && residues && !runs.empty(),"q6_transfer_binding");
        UINT64 begin=Q6_mul(tile,m.width);Q6_need(begin<m.keys,"q6_transfer_tile_bounds");
        UINT64 tokens=std::min(m.width,m.keys-begin),position=0,total=0;std::vector<VHO_Q6_TRANSFER> transfers;
        for(unsigned r=0;r<runs.size();++r) {
            const VHO_Q6_PHYSICAL_RUN &run=runs[r];Q6_need(run.tokens && run.tokens<=tokens-position,"q6_transfer_run_extent");
            for(unsigned stream=0;stream<4;++stream) {
                UINT64 source=Q6_mul(run.first_slot,m.raw_row[stream]),bytes=Q6_mul(run.tokens,m.raw_row[stream]);
                UINT64 dest=Q6_add(Q6_mul(slot,m.raw_stride),Q6_add(m.raw_offset[stream],Q6_mul(position,m.raw_row[stream])));
                std::vector<VHO_Q6_COPY> parts;UINT64 tx=0;
                Q6_need(VHO_q6_partition(source,dest,bytes,source_sizes[stream],m.decoded_offset,residues[stream],0,
                    m.control.delivery==VHO_Q6_BULK_TMA,parts,tx,why),"q6_transfer_run_refused");
                total=Q6_add(total,tx);for(unsigned i=0;i<parts.size();++i) transfers.push_back(VHO_Q6_TRANSFER(stream,parts[i]));
            }
            position+=run.tokens;
        }
        Q6_need(position==tokens && total<=0xffff0,"q6_transfer_complete_tile_or_tx_count");
        out.swap(transfers);transaction_bytes=total;why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Independent lifetime automaton. ISSUE never means ready; WAIT never means
 * dequantized; publication and last-consumer release are separate obligations. */
bool VHO_q6_verify_pipeline(const VHO_Q6_MAPPING &m,const std::vector<VHO_Q6_ACTION> &trace,std::string &why) {
    try {
        Q6_need(m.key==VHO_q6_mapping_key(m) && m.tiles && m.tiles<=1000000/21 &&
            (m.control.delivery==VHO_Q6_SYNCHRONOUS || m.control.delivery==VHO_Q6_BULK_TMA) &&
            m.slots==(m.control.delivery==VHO_Q6_BULK_TMA?2U:1U),"q6_pipeline_binding_or_extent");
        unsigned state[2]={0,0};UINT64 owner[2]={0,0},acquired=0,consumed=0;UINT32 pass=0;bool decoded=false;
        for(unsigned i=0;i<trace.size();++i) {
            const VHO_Q6_ACTION &a=trace[i];
            Q6_need(pass<3 && a.pass==pass && a.slot<m.slots && a.tile<m.tiles && a.slot==a.tile%m.slots,"q6_action_owner");
            unsigned &s=state[a.slot];
            if(a.kind==VHO_Q6_ACQUIRE) {
                Q6_need(!s && a.tile==acquired && a.tile<=consumed+m.slots-1,"q6_premature_slot_reuse");s=1;owner[a.slot]=a.tile;++acquired;
            } else {
                Q6_need(s && owner[a.slot]==a.tile,"q6_stale_slot_owner");
                switch(a.kind) {
                case VHO_Q6_ISSUE:Q6_need(s==1,"q6_duplicate_issue");s=2;break;
                case VHO_Q6_WAIT:Q6_need(s==2,"q6_wait_without_issue");s=3;break;
                case VHO_Q6_DEQUANTIZE:Q6_need(s==3 && !decoded && a.tile==consumed,"q6_dequant_before_ready_or_decoded_reuse");s=4;decoded=true;break;
                case VHO_Q6_PUBLISH:Q6_need(s==4 && decoded,"q6_missing_dequantization");s=5;break;
                case VHO_Q6_CONSUME:Q6_need(s==5 && decoded && a.tile==consumed,"q6_missing_consumer_publication");s=6;decoded=false;++consumed;break;
                case VHO_Q6_RELEASE:Q6_need(s==6,"q6_release_before_all_consumers");s=0;break;
                default:throw std::runtime_error("q6_unknown_action");
                }
            }
            if(consumed==m.tiles && !state[0] && !state[1]) {
                Q6_need(acquired==m.tiles && !decoded,"q6_incomplete_pass_drain");++pass;consumed=acquired=0;
            }
        }
        Q6_need(pass==3 && !decoded && !state[0] && !state[1],"q6_incomplete_pipeline_drain");why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Only the selected descriptor is expanded. This trace is review/verification
 * data, not WN or a candidate benchmark. Do not manufacture a device witness. */
bool VHO_q6_pipeline(const VHO_Q6_MAPPING &m,std::vector<VHO_Q6_ACTION> &out,std::string &why) {
    try {
        Q6_need(m.key==VHO_q6_mapping_key(m) && m.tiles && m.tiles<=1000000/21 && (m.slots==1 || m.slots==2),"q6_pipeline_binding_or_extent");
        std::vector<VHO_Q6_ACTION> trace;
        for(UINT32 pass=0;pass<3;++pass) {
            trace.push_back(VHO_Q6_ACTION(VHO_Q6_ACQUIRE,pass,0,0));trace.push_back(VHO_Q6_ACTION(VHO_Q6_ISSUE,pass,0,0));
            for(UINT64 tile=0;tile<m.tiles;++tile) {
                UINT32 slot=tile%m.slots;trace.push_back(VHO_Q6_ACTION(VHO_Q6_WAIT,pass,slot,tile));
                if(m.slots==2 && tile+1<m.tiles) {
                    trace.push_back(VHO_Q6_ACTION(VHO_Q6_ACQUIRE,pass,(tile+1)%2,tile+1));trace.push_back(VHO_Q6_ACTION(VHO_Q6_ISSUE,pass,(tile+1)%2,tile+1));
                }
                trace.push_back(VHO_Q6_ACTION(VHO_Q6_DEQUANTIZE,pass,slot,tile));trace.push_back(VHO_Q6_ACTION(VHO_Q6_PUBLISH,pass,slot,tile));
                trace.push_back(VHO_Q6_ACTION(VHO_Q6_CONSUME,pass,slot,tile));trace.push_back(VHO_Q6_ACTION(VHO_Q6_RELEASE,pass,slot,tile));
                if(m.slots==1 && tile+1<m.tiles) {
                    trace.push_back(VHO_Q6_ACTION(VHO_Q6_ACQUIRE,pass,0,tile+1));trace.push_back(VHO_Q6_ACTION(VHO_Q6_ISSUE,pass,0,tile+1));
                }
            }
        }
        Q6_need(VHO_q6_verify_pipeline(m,trace,why),"q6_generated_pipeline_invalid");out.swap(trace);why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}

/* Copy exact raw/decoded layouts and a bounded representative event prefix;
 * the existing Q6 verifier validates every generation/pass in the full trace.
 * Readiness of raw bytes never implies decoded readiness or readers-complete. */
bool VHO_q6_capture_movement(const VHO_Q6_MAPPING &m,const std::vector<VHO_Q6_ACTION> &actions,
    const DSL_MOVEMENT_BINDING &binding,DSL_MOVEMENT_FACTS &out,std::string &why) {
    out=DSL_MOVEMENT_FACTS();
    try {
        Q6_need(m.key==VHO_q6_mapping_key(m) && VHO_q6_verify_pipeline(m,actions,why),"movement_Q6_pipeline_unproved");
        Q6_need(binding.source_key==m.selected_key && binding.serving_key==m.binding_key && !binding.owner.empty(),"movement_Q6_source_or_Q3_binding");
        DSL_MOVEMENT_CAPTURE c;c.binding=binding;c.binding.family="MLA-Q6";
        std::ostringstream proof;proof<<m.key<<";actions="<<actions.size()<<';';
        for(size_t i=0;i<actions.size();++i) proof<<actions[i].kind<<';'<<actions[i].pass<<';'<<actions[i].slot<<';'<<actions[i].tile<<';';
        c.binding.proof_key=proof.str();c.binding.controls="existing-selected-Q6-raw/dequantize/publish/consume/release";
        c.work.owner="CTA/query/head-group";c.work.domain="selected cache staging only; three ordered passes";
        c.work.instances=m.grid;c.work.width=m.width;c.work.active_last=m.keys%m.width?m.keys%m.width:m.width;
        c.work.axes.push_back("CTA");c.work.extents.push_back(m.grid);c.work.axes.push_back("pass");c.work.extents.push_back(3);c.work.axes.push_back("token_tile");c.work.extents.push_back(m.tiles);c.work.axes.push_back("within_tile");c.work.extents.push_back(m.width);
        c.work.inactive_last=m.width-c.work.active_last;c.work.tail_policy="exact-active-tail; no padded global reads";
        c.work.numerical="source S8/U4 plus matching F32 scales; ordered three-pass folds unchanged";
        for(unsigned i=0;i<m.slots+1;++i) {
            bool decoded=i==m.slots;std::ostringstream name,version;name<<(decoded?"decoded":"raw/");if(!decoded) name<<i;
            version<<"p0/t"<<(decoded?0:i);std::vector<uint64_t> shape(1,decoded?m.decoded_bytes:m.raw_stride);
            DSL_MOVEMENT_STORAGE s=DSL_movement_dense_storage(name.str(),"Q6-shared-arena",version.str(),decoded?"decoded-F32-and-padding":"raw-payload-scale-slot",shape,1,true);
            s.offset=decoded?m.decoded_offset:i*m.raw_stride;s.capacity=m.shared_bytes;s.first_use="Q6-acquire";s.last_use="Q6-release";c.storages.push_back(s);
        }
        const UINT64 prefix=std::min(UINT64(3),m.tiles);std::map<UINT64,std::map<unsigned,unsigned> > events;
        for(size_t i=0;i<actions.size();++i) {
            const VHO_Q6_ACTION &a=actions[i];if(a.pass || a.tile>=prefix) continue;
            std::ostringstream raw,version;raw<<"raw/"<<a.slot;version<<"p0/t"<<a.tile;
            // Additional projections name completion/overwrite boundaries of
            // the existing actions; they never insert a wait or a barrier.
            unsigned kinds[2]={a.kind,0},count=1;
            if(a.kind==VHO_Q6_DEQUANTIZE) {kinds[0]=8;kinds[1]=a.kind;count=2;}
            if(a.kind==VHO_Q6_PUBLISH) {kinds[0]=9;kinds[1]=a.kind;count=2;}
            for(unsigned j=0;j<count;++j) {
                unsigned kind=kinds[j];DSL_MOVEMENT_EVENT e;std::ostringstream id;id<<"t"<<a.tile<<"/"<<kind;e.id=id.str();
                const char *names[]={"invalid","acquire","issue","raw-ready","raw-consumer","decoded-ready","consumer","readers-complete","decoded-overwrite-begin","raw-readers-complete"};e.kind=names[kind];
                bool decoded=kind==8 || kind==VHO_Q6_PUBLISH || kind==VHO_Q6_CONSUME || kind==VHO_Q6_RELEASE;
                e.storage=decoded?"decoded":raw.str();e.version=version.str();e.scope="CTA/all-head-consumers";e.evidence="existing-verified-Q6-action-contract";
                unsigned index=c.events.size();if(index) c.edges.push_back(DSL_MOVEMENT_EDGE(index-1,index));c.events.push_back(e);events[a.tile][kind]=index;
            }
        }
        for(UINT64 t=0;t<prefix;++t) {
            std::ostringstream raw,version;raw<<"raw/"<<t%m.slots;version<<"p0/t"<<t;
            DSL_MOVEMENT_LIFETIME r;r.storage=raw.str();r.version=version.str();r.raw_ready=r.decoded_ready=events[t][VHO_Q6_WAIT];
            r.consumer=events[t][VHO_Q6_DEQUANTIZE];r.readers_done=events[t][9];
            if(t+m.slots<prefix) {r.has_reuse=true;std::ostringstream next;next<<"p0/t"<<t+m.slots;r.next_version=next.str();r.reuse_ready=events[t+m.slots][VHO_Q6_ACQUIRE];}c.lifetimes.push_back(r);
            DSL_MOVEMENT_LIFETIME d;d.storage="decoded";d.raw_storage=raw.str();d.version=version.str();d.decoded=true;
            d.raw_ready=events[t][VHO_Q6_WAIT];d.decoded_ready=events[t][VHO_Q6_PUBLISH];d.consumer=events[t][VHO_Q6_CONSUME];d.readers_done=events[t][VHO_Q6_RELEASE];
            if(t+1<prefix) {d.has_reuse=true;std::ostringstream next;next<<"p0/t"<<t+1;d.next_version=next.str();d.reuse_ready=events[t+1][8];}c.lifetimes.push_back(d);
        }
        UINT64 row=0;for(unsigned i=0;i<4;++i) row=Q6_add(row,m.raw_row[i]);
        c.ledger.useful_reads=DSL_MOVEMENT_BYTES(Q6_mul(Q6_mul(m.batch,m.keys),row));
        c.ledger.requested_reads=DSL_MOVEMENT_BYTES(Q6_mul(Q6_mul(Q6_mul(m.grid,m.keys),row),3));
        c.ledger.scale=DSL_MOVEMENT_BYTES(Q6_mul(Q6_mul(Q6_mul(m.grid,m.keys),8),3));
        c.ledger.partial=DSL_MOVEMENT_BYTES(0);
        return DSL_MOVEMENT_FACTS::Create(c,out,why);
    } catch(const std::exception &e) {why=e.what();return false;}
}
