/* Copyright (C) 2026 Open64 Project */
/* Runtime-owned Q3 leases and atomic cache/scale publication. Host memory is
 * the default; explicit Q6d providers never become host-dereferenced pointers.
 * Built as an adapter library, never linked into be.so. Source descriptions are
 * shared with VHO; registry state is neither a compiler epoch nor search input.
 * Design: doc/AI-COMPILER-MLA-Q3-SERVING-v1.0.md. */
#include "tensor_serving.h"
#include "tensor_quantized_movement.h"
#include <atomic>
#include <mutex>
#include <set>
#include <cstring>
#include <cmath>
#include <limits>
#include <stdexcept>
namespace {
std::atomic<UINT64> owners(0),identities(0);
struct REGISTERED_SPAN {UINT64 base,bytes,owner,domain;};
std::mutex ownership_mutex;
std::map<UINT64,REGISTERED_SPAN> ownership;
/* All failures occur under registry ownership and publish no partial records. */
void need(bool b,const char *why) {if(!b) throw std::runtime_error(why);}
/* Saturate permanently at exhaustion. A refusal must never reset a counter and
 * make former allocation/ticket identities issuable again. */
UINT64 issue_counter(std::atomic<UINT64> &counter) {
    UINT64 previous=counter.load();
    do {need(previous!=std::numeric_limits<UINT64>::max(),"q3_generation_exhausted");}
    while(!counter.compare_exchange_weak(previous,previous+1));
    return previous+1;
}
/* Pointer arithmetic is checked before a host address is formed. */
UINT64 add(UINT64 a,UINT64 b) {need(a<=std::numeric_limits<UINT64>::max()-b,"q3_address_overflow");return a+b;}
/* Checked products keep maintenance byte estimates from wrapping before add. */
UINT64 mul(UINT64 a,UINT64 b) {need(!b || a<=std::numeric_limits<UINT64>::max()/b,"q4_extent_overflow");return a*b;}
/* Actual address alignment is advisory; it is not a device/TMA guarantee. */
UINT64 alignment(UINT64 a) {if(!a) return 0;UINT64 n=1;while(n<4096 && a%(n*2)==0) n*=2;return n;}
/* Q1 uses exact byte descriptors and explicit I64/F32 sizes. */
UINT64 scalar(const VHO_TENSOR_VALUE &v) {return v.dtype=="float32"?4:v.dtype=="int64"?8:1;}
/* Compare opaque handles including registry instance and allocation generation. */
bool equal(const VHO_SERVING_HANDLE &a,const VHO_SERVING_HANDLE &b) {return a.owner==b.owner && a.id==b.id && a.generation==b.generation;}
}
struct VHO_TENSOR_SERVING::IMPL {
    struct ALLOCATION {void *base;UINT64 bytes,generation,version,readers,writers,bundles,identity;};
    struct BUNDLE {std::vector<VHO_SERVING_VIEW> views;std::vector<UINT64> versions;UINT64 readers,pending;std::vector<UINT64> valid_lengths;};
    struct CALL {std::vector<VHO_SERVING_VIEW> views;VHO_SERVING_VIEW metadata;VHO_SERVING_FACTS facts;std::string controls;bool running;};
    VHO_SERVING_MEMORY memory;
    bool device;
    VHO_TENSOR_PROGRAM program;
    VHO_TENSOR_SERVING_SCHEMA schema;
    std::mutex mutex;
    UINT64 owner,next,tip;
    std::map<UINT64,ALLOCATION> allocations;
    std::map<UINT64,BUNDLE> bundles;
    std::map<UINT64,CALL> calls;
    std::map<UINT64,UINT64> readers;
    /* One instance is one source/cache timeline; generations never reset. */
    IMPL(const VHO_TENSOR_PROGRAM &p,const VHO_SERVING_MEMORY *m):device(m!=NULL),program(p),owner(issue_counter(owners)),next(0),tip(0) {
        memset(&memory,0,sizeof(memory));if(m) memory=*m;
    }
    /* Provider-owned uncertain transfers prohibit release after failed validation. */
    bool Idle() {return !device || memory.idle(memory.context);}
    /* Copy only observations/status through the memory service; never dereference
     * a device pointer as host memory. Device byte addresses use checked integers. */
    void Read(const void *address,void *out,UINT64 bytes) {
        if(device) {std::string why;need(memory.read(memory.context,address,out,bytes,why),why.empty()?"q6d_read_failed":why.c_str());}
        else memcpy(out,address,bytes);
    }
    /* Write is synchronous on success; an uncertain provider retains its storage. */
    void Store(void *address,const void *source,UINT64 bytes) {
        if(device) {std::string why;need(memory.write(memory.context,address,source,bytes,why),why.empty()?"q6d_write_failed":why.c_str());}
        else memcpy(address,source,bytes);
    }
    /* Owned control image stays live while workload facts are rebuilt. */
    std::vector<unsigned char> Copy(const VHO_SERVING_VIEW &v,UINT64 bytes,UINT64 align=1) {
        std::vector<unsigned char> out(bytes);Read(Address(v,bytes,align),&out[0],bytes);return out;
    }
    /* Issue non-reusable allocation, call, reader and bundle identities. */
    UINT64 Issue() {return issue_counter(identities);}
    /* Validate handles without dereferencing an expired pointer. */
    ALLOCATION &Allocation(const VHO_SERVING_HANDLE &h) {
        need(h.owner==owner && allocations.count(h.id),"q3_stale_allocation");ALLOCATION &a=allocations[h.id];
        need(h.generation==a.generation,"q3_allocation_generation");return a;
    }
    /* Exact ABI views are bounded before any control or scale load. */
    unsigned char *Address(const VHO_SERVING_VIEW &v,UINT64 required,UINT64 align=1) {
        ALLOCATION &a=Allocation(v.handle);
        if(device) {UINT64 id=0;std::string why;need(memory.validate(memory.context,a.base,a.bytes,id,why) && id==a.identity,"q6d_allocation_identity_changed");}
        need(v.bytes==required && v.offset<=a.bytes && required<=a.bytes-v.offset,"q3_buffer_extent");
        UINT64 address=add((UINT64)(uintptr_t)a.base,v.offset);need(address%align==0,"q3_scalar_alignment");return (unsigned char *)(uintptr_t)address;
    }
    /* Locate source root bindings without trusting argument names. */
    unsigned Root(UINT32 value) {
        for(unsigned i=0;i<schema.roots.size();++i) if(schema.roots[i]==program.values[value].storage) return i;
        need(false,"q3_missing_root");return 0;
    }
    /* Whole source ABI, status and metadata are disjoint, even for empty work. */
    void Views(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &meta) {
        need(v.size()==schema.roots.size()+1,"q3_abi_arity");std::vector<std::pair<UINT64,UINT64> > spans;
        for(unsigned i=0;i<v.size();++i) {
            UINT64 bytes=i==schema.roots.size()?4:VHO_tensor_elements(program.values[schema.roots[i]])*scalar(program.values[schema.roots[i]]);
            UINT64 align=i==schema.roots.size()?4:scalar(program.values[schema.roots[i]]);
            UINT64 first=(UINT64)(uintptr_t)Address(v[i],bytes,align);spans.push_back(std::make_pair(first,add(first,bytes)));
        }
        UINT64 bytes=schema.geometry.batch*schema.metadata_columns*8,first=(UINT64)(uintptr_t)Address(meta,bytes,8);
        spans.push_back(std::make_pair(first,add(first,bytes)));
        for(unsigned i=0;i<spans.size();++i) for(unsigned j=0;j<i;++j)
            need(spans[i].second<=spans[j].first || spans[j].second<=spans[i].first,"q3_abi_alias");
    }
    /* Loads use memcpy so host access never invents stronger alignment. */
    INT64 Integer(const unsigned char *p,UINT64 index) {INT64 v;memcpy(&v,p+index*8,8);return v;}
    /* Incoming and outgoing scales are checked only at source-read slots;
     * unused physical padding may contain poison. Masks never shrink this set. */
    void Scales(const std::vector<VHO_SERVING_VIEW> &v,const std::set<UINT64> &slots,bool output) {
        for(unsigned pair=1;pair<4;pair+=2) {
            UINT32 id=output?schema.returned[pair]:schema.incoming[pair];if(id==~0U) continue;
            const unsigned char *base=Address(v[Root(id)],VHO_tensor_elements(program.values[id])*4,4);
            for(std::set<UINT64>::const_iterator i=slots.begin();i!=slots.end();) {
                UINT64 first=*i,last=first;++i;while(i!=slots.end() && *i==last+1) {last=*i;++i;}
                need(last<VHO_tensor_elements(program.values[id]),"q3_scale_slot_bounds");std::vector<float> values(last-first+1);
                Read((const void *)(uintptr_t)add((UINT64)(uintptr_t)base,mul(first,4)),&values[0],mul(values.size(),4));
                for(unsigned j=0;j<values.size();++j) need(std::isfinite(values[j]) && values[j]>0,"q3_scale_domain");
            }
        }
    }
    /* Read current registered metadata/mask/page/slot contents. The exact copied
     * control image detects stale observed facts, not KV payload equality. */
    void Observe(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &meta,VHO_SERVING_FACTS &f,std::string &key,bool output_scales=false) {
        Views(v,meta);const VHO_TENSOR_WORKLOAD_GEOMETRY &g=schema.geometry;
        std::vector<unsigned char> metadata_copy=Copy(meta,g.batch*schema.metadata_columns*8,8);const unsigned char *m=&metadata_copy[0];key.assign((const char *)m,meta.bytes);
        const VHO_SERVING_VIEW &mask=v[Root(g.mask_value)];std::vector<unsigned char> mask_copy=Copy(mask,g.batch*g.heads*g.queries*g.keys);const unsigned char *bits=&mask_copy[0];
        key.append((const char *)bits,mask.bytes);VHO_TENSOR_WORKLOAD_INVOCATION in;in.version=1;in.source_key=schema.source_key;in.invocation_id=device?"registered-device-invocation":"registered-host-invocation";in.evidence="bound-serving-invocation-v1";
        const char *phases[]={"unknown","prefill","decode","speculative-verify"};const char *masks[]={"opaque","causal","causal-window"};const char *updates[]={"none","construction","append","indexed-unknown-order"};
        for(UINT64 b=0;b<g.batch;++b) {
            UINT64 offset=b*schema.metadata_columns;INT64 fields[9];for(unsigned k=0;k<9;++k) {fields[k]=Integer(m,offset+k);need(fields[k]>=0 && fields[k]<=16777216,"q3_metadata_domain");}
            need(fields[0]<4 && fields[5]<3 && fields[7]<4,"q3_metadata_enum");
            VHO_TENSOR_WORKLOAD_REQUEST r;r.request=b;r.phase=phases[fields[0]];r.queries=fields[1];r.history=fields[2];r.valid=fields[3];r.query_begin=fields[4];r.mask_policy=masks[fields[5]];r.window_left=fields[6];r.update=updates[fields[7]];r.append_tokens=fields[8];
            need(r.queries<=g.queries && r.valid<=g.keys && r.append_tokens<=(g.update_items_known?g.update_items:0),"q3_metadata_extent");
            UINT64 pos=offset+9;
            for(UINT64 j=0;j<g.queries;++j) {INT64 n=Integer(m,pos++);if(j<r.queries) {need(n>=0,"q3_position_domain");r.query_positions.push_back(n);}}
            for(UINT64 j=0;j<g.keys;++j) {INT64 n=Integer(m,pos++);if(j<r.valid) {need(n>=0,"q3_position_domain");r.key_positions.push_back(n);}}
            for(UINT64 j=0;j<(g.update_items_known?g.update_items:0);++j) {INT64 n=Integer(m,pos++);if(j<r.append_tokens) {need(n>=0,"q3_position_domain");r.update_positions.push_back(n);}}
            r.allowed.assign(bits+b*g.heads*g.queries*g.keys,bits+(b+1)*g.heads*g.queries*g.keys);in.requests.push_back(r);
        }
        std::string why;bool inferred=VHO_tensor_infer_workload(program,&in,f.workload,why);if(!inferred) throw std::runtime_error(why);
        std::set<UINT64> source_slots,history_slots,writes;
        if(schema.paged) {
            const VHO_SERVING_VIEW &tv=v[Root(schema.table)],&iv=v[Root(schema.tokens)];std::vector<unsigned char> table_copy=Copy(tv,tv.bytes,8),token_copy=Copy(iv,iv.bytes,8);const unsigned char *table=&table_copy[0],*tokens=&token_copy[0];
            key.append((const char *)table,tv.bytes);key.append((const char *)tokens,iv.bytes);
            UINT64 cols=(g.keys+schema.page_tokens-1)/schema.page_tokens;
            for(UINT64 t=0;t<g.keys;++t) need(Integer(tokens,t)==(INT64)t,"q3_token_enumeration");
            for(UINT64 b=0;b<g.batch;++b) for(UINT64 page=0;page<cols;++page) {
                INT64 physical=Integer(table,b*cols+page);need(physical>=0 && (UINT64)physical<schema.pages,"q3_page_bounds");
                VHO_SERVING_PAGE record;record.request=b;record.logical_page=page;record.physical_page=physical;
                record.capacity_tokens=std::min(schema.page_tokens,g.keys-page*schema.page_tokens);
                record.valid_tokens=in.requests[b].valid>page*schema.page_tokens?std::min(record.capacity_tokens,in.requests[b].valid-page*schema.page_tokens):0;
                for(unsigned i=0;i<4;++i) {
                    UINT32 root=schema.incoming[i];const VHO_SERVING_VIEW &view=v[Root(root)];
                    UINT64 row=program.values[root].shape.back()*scalar(program.values[root]),off=(UINT64)physical*schema.page_tokens*row;
                    unsigned char *base=Address(view,VHO_tensor_elements(program.values[root])*scalar(program.values[root]));
                    need(off<=view.bytes && record.capacity_tokens*row<=view.bytes-off,"q3_page_span");
                    record.byte_offsets[i]=off;record.row_bytes[i]=row;record.base_alignment[i]=alignment((UINT64)(uintptr_t)base);record.page_alignment[i]=alignment(add((UINT64)(uintptr_t)base,off));
                }
                f.pages.push_back(record);
                for(UINT64 t=0;t<record.capacity_tokens;++t) {UINT64 slot=(UINT64)physical*schema.page_tokens+t;source_slots.insert(slot);
                    if(page*schema.page_tokens+t<in.requests[b].history) history_slots.insert(slot);}
            }
            if(schema.slots!=~0U) {
                const VHO_SERVING_VIEW &sv=v[Root(schema.slots)];std::vector<unsigned char> slot_copy=Copy(sv,sv.bytes,8);const unsigned char *slots=&slot_copy[0];key.append((const char *)slots,sv.bytes);
                for(UINT64 b=0;b<g.batch;++b) {
                    const VHO_TENSOR_WORKLOAD_REQUEST &r=in.requests[b];need(r.update=="append","q3_unknown_append_order");
                    for(UINT64 j=0;j<r.append_tokens;++j) {
                        UINT64 token=r.history+j;INT64 physical=Integer(table,b*cols+token/schema.page_tokens),slot=Integer(slots,b*g.update_items+j);
                        need(slot>=0 && (UINT64)slot==(UINT64)physical*schema.page_tokens+token%schema.page_tokens,"q3_append_slot_mapping");
                        need(!history_slots.count(slot) && writes.insert(slot).second,"q3_shared_prefix_or_duplicate_write");
                    }
                }
            }
        } else for(UINT64 i=0;i<g.batch*g.keys;++i) source_slots.insert(i);
        if(!output_scales) for(std::set<UINT64>::const_iterator i=writes.begin();i!=writes.end();++i) source_slots.erase(*i);
        if(schema.paged || output_scales) Scales(v,source_slots,output_scales);
        f.bound=true;
    }
    /* Pin actual allocations once per view; shared input reads can overlap,
     * exclusive outputs/scratch/status never overlap another invocation. */
    void Pin(CALL &c) {
        for(unsigned i=0;i<c.views.size();++i) {ALLOCATION &a=Allocation(c.views[i].handle);bool input=i<schema.roots.size() && program.values[schema.roots[i]].role==1;
            if(input) ++a.readers;else ++a.writers;}
        ++Allocation(c.metadata.handle).readers;
    }
    /* Release after the synchronous callback; every writable content version
     * advances even on failure, so dirty scratch cannot masquerade as old data. */
    void Unpin(CALL &c,bool wrote) {
        for(unsigned i=0;i<c.views.size();++i) {ALLOCATION &a=Allocation(c.views[i].handle);bool input=i<schema.roots.size() && program.values[schema.roots[i]].role==1;
            if(input) --a.readers;else {--a.writers;if(wrote) a.version=Issue();}}
        --Allocation(c.metadata.handle).readers;if(c.facts.parent_bundle) --bundles[c.facts.parent_bundle].pending;
    }
    /* Bundle references pin generation, content version and mapping as one
     * immutable record. They are not equated by caller-supplied integers. */
    UINT64 Publish(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &meta,bool output,const VHO_SERVING_FACTS &f,CALL *call=NULL) {
        BUNDLE b;b.readers=b.pending=0;
        for(unsigned i=0;i<4;++i) b.views.push_back(v[Root(output?schema.returned[i]:schema.incoming[i])]);
        if(schema.paged) {b.views.push_back(v[Root(schema.table)]);b.views.push_back(v[Root(schema.tokens)]);}
        b.views.push_back(meta);b.versions.resize(b.views.size());
        for(unsigned i=0;i<f.workload.segments.size();++i) b.valid_lengths.push_back(output?f.workload.segments[i].valid:f.workload.segments[i].history);
        // Allocate all container state before the publication commit section.
        UINT64 id=Issue();bundles.emplace(id,std::move(b));BUNDLE &published=bundles[id];
        if(call) Unpin(*call,true);
        for(unsigned i=0;i<published.views.size();++i) {ALLOCATION &a=Allocation(published.views[i].handle);published.versions[i]=a.version;++a.bundles;}
        tip=id;return id;
    }
    /* Match actual incoming handles/ranges to the complete published tuple. */
    void Parent(const std::vector<VHO_SERVING_VIEW> &v,UINT64 parent,const VHO_SERVING_FACTS &facts) {
        if(!schema.paged) {need(!parent && !tip,"q3_unexpected_parent");return;}
        need(parent && parent==tip && bundles.count(parent),"q3_stale_bundle");BUNDLE &b=bundles[parent];
        std::vector<VHO_SERVING_VIEW> expected;for(unsigned i=0;i<4;++i) expected.push_back(v[Root(schema.incoming[i])]);
        expected.push_back(v[Root(schema.table)]);expected.push_back(v[Root(schema.tokens)]);
        need(b.valid_lengths.size()==facts.workload.segments.size(),"q3_history_join");
        for(unsigned i=0;i<b.valid_lengths.size();++i) need(b.valid_lengths[i]==(UINT64)facts.workload.segments[i].history,"q3_history_join");
        for(unsigned i=0;i<expected.size();++i) {
            need(equal(expected[i].handle,b.views[i].handle) && expected[i].offset==b.views[i].offset && expected[i].bytes==b.views[i].bytes,"q3_cache_scale_bundle_join");
            need(Allocation(expected[i].handle).version==b.versions[i],"q3_content_version");
        }
    }
};
/* Construction validates a detached source and engine legal page-size set. */
VHO_TENSOR_SERVING::VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &p,const std::vector<UINT64> &legal,std::string &why):VHO_TENSOR_SERVING(p,legal,NULL,why) {}
/* Source admission is identical; only the explicit memory domain differs. */
VHO_TENSOR_SERVING::VHO_TENSOR_SERVING(const VHO_TENSOR_PROGRAM &p,const std::vector<UINT64> &legal,const VHO_SERVING_MEMORY *memory,std::string &why):_impl(NULL) {
    try {need(!memory || (memory->version==1 && memory->bytes==sizeof(VHO_SERVING_MEMORY) && memory->domain && memory->validate && memory->read && memory->write && memory->idle),"q6d_memory_service");IMPL *candidate=new IMPL(p,memory);std::string reason;
        if(!VHO_tensor_serving_schema(p,candidate->schema,reason)) {delete candidate;throw std::runtime_error(reason);}
        if(candidate->schema.paged) {bool found=false;for(unsigned i=0;i<legal.size();++i) found|=legal[i]==candidate->schema.page_tokens;
            if(!found) {delete candidate;throw std::runtime_error("q3_allocator_page_size");}}
        _impl=candidate;why.clear();
    } catch(const std::exception &e) {why=e.what();}
}
/* Borrowed storage remains the owner's responsibility through final teardown. */
VHO_TENSOR_SERVING::~VHO_TENSOR_SERVING() {
    if(_impl) {std::lock_guard<std::mutex> lock(ownership_mutex);
        for(std::map<UINT64,REGISTERED_SPAN>::iterator i=ownership.begin();i!=ownership.end();) {
            if(i->second.owner==_impl->owner) i=ownership.erase(i);else ++i;
        }}
    delete _impl;
}
/* No operation is valid on a refused adapter. */
bool VHO_TENSOR_SERVING::Valid() const {return _impl!=NULL;}
/* The ABI wrapper uses this before destroying the borrowed-memory registry. */
bool VHO_TENSOR_SERVING::Quiescent() const {if(!_impl) return true;std::lock_guard<std::mutex> lock(_impl->mutex);return _impl->calls.empty() && _impl->readers.empty();}
/* Register an exact memory-domain span with an observed provider identity and
 * registry-issued generation; overlapping live registrations refuse. */
bool VHO_TENSOR_SERVING::Register(void *base,UINT64 bytes,const std::string &kind,VHO_SERVING_HANDLE &out,std::string &why) {
    out=VHO_SERVING_HANDLE();why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(kind==(_impl->device?"cuda-device-v1":"host-synchronous"),"q3_unsupported_memory_kind");need(base && bytes && bytes<=1073741824,"q3_allocation_extent");UINT64 first=(UINT64)(uintptr_t)base,last=add(first,bytes);
        std::lock_guard<std::mutex> global_lock(ownership_mutex);
        for(std::map<UINT64,REGISTERED_SPAN>::iterator i=ownership.begin();i!=ownership.end();++i)
            need(i->second.domain!=_impl->memory.domain || last<=i->second.base || first>=add(i->second.base,i->second.bytes),"q3_overlapping_allocation");
        UINT64 identity=0;if(_impl->device) need(_impl->memory.validate(_impl->memory.context,base,bytes,identity,why),"q6d_register_device_identity");
        UINT64 id=_impl->Issue();IMPL::ALLOCATION a={base,bytes,id,_impl->Issue(),0,0,0,identity};_impl->allocations.emplace(id,a);REGISTERED_SPAN span={first,bytes,_impl->owner,_impl->memory.domain};
        try {ownership.emplace(id,span);}catch(...) {_impl->allocations.erase(id);throw;}
        out.owner=_impl->owner;out.id=id;out.generation=id;return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* All cooperative mutation must pass the lease and publication barrier. */
bool VHO_TENSOR_SERVING::Write(const VHO_SERVING_HANDLE &h,UINT64 offset,const void *data,UINT64 bytes,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {IMPL::ALLOCATION &a=_impl->Allocation(h);need(!a.readers && !a.writers && !a.bundles,"q3_allocation_pinned");need(data && offset<=a.bytes && bytes<=a.bytes-offset,"q3_write_extent");VHO_SERVING_VIEW view;view.handle=h;view.bytes=a.bytes;void *base=_impl->Address(view,a.bytes);
        if(_impl->device) a.version=_impl->Issue();_impl->Store((void *)(uintptr_t)add((UINT64)(uintptr_t)base,offset),data,bytes);if(!_impl->device) a.version=_impl->Issue();return true;}
    catch(const std::exception &e) {why=e.what();return false;}
}
/* Read-only access is allowed with immutable readers, never active writers. */
bool VHO_TENSOR_SERVING::Read(const VHO_SERVING_HANDLE &h,UINT64 offset,void *data,UINT64 bytes,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {IMPL::ALLOCATION &a=_impl->Allocation(h);need(!a.writers,"q3_writer_active");need(data && offset<=a.bytes && bytes<=a.bytes-offset,"q3_read_extent");
        VHO_SERVING_VIEW view;view.handle=h;view.bytes=a.bytes;_impl->Read((void *)(uintptr_t)add((UINT64)(uintptr_t)_impl->Address(view,a.bytes),offset),data,bytes);return true;
    }catch(const std::exception &e) {why=e.what();return false;}
}
/* Retirement precedes freeing/reusing an address, preventing handle ABA. */
bool VHO_TENSOR_SERVING::Retire(const VHO_SERVING_HANDLE &h,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {IMPL::ALLOCATION &a=_impl->Allocation(h);need(!a.readers && !a.writers && !a.bundles,"q3_allocation_pinned");std::lock_guard<std::mutex> global_lock(ownership_mutex);ownership.erase(h.id);_impl->allocations.erase(h.id);return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Import requires an already-ready memory provider; no partial work is published. */
bool VHO_TENSOR_SERVING::Import(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &m,UINT64 &bundle,std::string &why) {
    bundle=0;why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->schema.paged && !_impl->tip,"q3_import_state");for(unsigned i=0;i<v.size();++i) {IMPL::ALLOCATION &a=_impl->Allocation(v[i].handle);need(!a.writers,"q3_writer_active");}
        VHO_SERVING_FACTS f;std::string key;_impl->Observe(v,m,f,key);bundle=_impl->Publish(v,m,false,f);return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Admission pins all real inputs and reserves the complete disjoint ABI. */
bool VHO_TENSOR_SERVING::Begin(const std::vector<VHO_SERVING_VIEW> &v,const VHO_SERVING_VIEW &m,UINT64 parent,VHO_SERVING_FACTS &out,std::string &why) {
    out=VHO_SERVING_FACTS();why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {_impl->Views(v,m);
        for(unsigned i=0;i<v.size();++i) {IMPL::ALLOCATION &a=_impl->Allocation(v[i].handle);bool input=i<_impl->schema.roots.size() && _impl->program.values[_impl->schema.roots[i]].role==1;
            need(!a.writers && (input || (!a.readers && !a.bundles)),"q3_writer_or_reader_conflict");}
        need(!_impl->Allocation(m.handle).writers,"q3_metadata_writer_active");IMPL::CALL c;c.views=v;c.metadata=m;c.running=false;
        _impl->Observe(v,m,c.facts,c.controls);_impl->Parent(v,parent,c.facts);c.facts.ticket=_impl->Issue();c.facts.parent_bundle=parent;
        for(unsigned i=0;i<v.size();++i) {IMPL::ALLOCATION &a=_impl->Allocation(v[i].handle);c.facts.allocation_ids.push_back(v[i].handle.id);c.facts.allocation_generations.push_back(a.generation);c.facts.content_versions.push_back(a.version);}
        IMPL::ALLOCATION &metadata_allocation=_impl->Allocation(m.handle);
        c.facts.allocation_ids.push_back(m.handle.id);c.facts.allocation_generations.push_back(metadata_allocation.generation);c.facts.content_versions.push_back(metadata_allocation.version);
        out=c.facts;_impl->calls.emplace(c.facts.ticket,c);_impl->Pin(_impl->calls[c.facts.ticket]);if(parent) ++_impl->bundles[parent].pending;return true;
    }catch(const std::exception &e) {out=VHO_SERVING_FACTS();why=e.what();return false;}
}
/* Freeze under the same mutex as Q3 publication. Re-read controls/scales and
 * parent, then copy generation facts. No borrowed pointer survives the call. */
bool VHO_TENSOR_SERVING::Freeze_costing(UINT64 ticket,VHO_Q4_SNAPSHOT &out,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}
    std::lock_guard<std::mutex> lock(_impl->mutex);
    try {
        need(_impl->calls.count(ticket) && !_impl->calls[ticket].running,"q4_stale_or_running_ticket");
        IMPL::CALL &c=_impl->calls[ticket];VHO_SERVING_FACTS fresh;std::string controls;
        _impl->Observe(c.views,c.metadata,fresh,controls);_impl->Parent(c.views,c.facts.parent_bundle,fresh);
        need(controls==c.controls,"q4_controls_changed");
        for(unsigned i=0;i<c.views.size()+1;++i) {
            IMPL::ALLOCATION &a=_impl->Allocation(i<c.views.size()?c.views[i].handle:c.metadata.handle);
            need(a.generation==c.facts.allocation_generations[i] && a.version==c.facts.content_versions[i],"q4_version_changed");
        }
        VHO_Q4_SNAPSHOT f;f.schema=_impl->schema;f.facts=c.facts;
        // Charge source cache maintenance once, irrespective of head grouping.
        // Unique operand sweeps are a model, not generated load instructions.
        std::set<UINT32> needed;
        for(unsigned i=0;i<4;++i) needed.insert(f.schema.returned[i]);
        const VHO_TENSOR_PROGRAM &p=_impl->program;
        for(unsigned rev=p.operations.size();rev;--rev) {
            const VHO_TENSOR_OPERATION &op=p.operations[rev-1];if(!needed.count(op.output)) continue;
            for(unsigned i=0;i<op.inputs.size();++i) needed.insert(op.inputs[i]);
            if(op.name=="common.reshape" || op.name=="common.transpose" || op.name=="common.unsqueeze") continue;
            UINT64 n=VHO_tensor_elements(p.values[op.output]);
            need(n<=UINT64(1073741824),"q4_source_extent");
            f.maintenance_ops=add(f.maintenance_ops,n);
            f.maintenance_bytes=add(f.maintenance_bytes,mul(n,scalar(p.values[op.output])));
            for(unsigned i=0;i<op.inputs.size();++i) {
                const VHO_TENSOR_VALUE &v=p.values[op.inputs[i]];
                f.maintenance_bytes=add(f.maintenance_bytes,mul(VHO_tensor_elements(v),scalar(v)));
            }
        }
        f.key=VHO_q4_snapshot_key(f);out=f;return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
/* Checking the exact rebuilt identity cannot revive an expired snapshot. */
bool VHO_TENSOR_SERVING::Revalidate_costing(UINT64 ticket,const std::string &key,std::string &why) {
    VHO_Q4_SNAPSHOT fresh;if(!Freeze_costing(ticket,fresh,why)) return false;
    if(fresh.key!=key) {why="q4_snapshot_changed";return false;}return true;
}
/* The callback is synchronous and runs outside the registry mutex. Publication
 * is a parent-version CAS after status/control/scale validation. */
bool VHO_TENSOR_SERVING::Run(UINT64 ticket,bool (*entry)(void *,void **,UINT32),void *context,UINT64 &bundle,std::string &why) {
    bundle=0;why.clear();if(!_impl) {why="q3_invalid_session";return false;}if(_impl->device) {why="q6d_device_requires_owned_completion";return false;}if(!entry) {std::string ignored;Cancel(ticket,ignored);why="q3_stale_or_running_ticket";return false;}std::vector<void *> args;bool armed=false;
    if(!Start_external(ticket,args,armed,why)) return false;
    bool completed=false;try {completed=entry(context,&args[0],args.size());}catch(...) {completed=false;}
    return Finish_external(ticket,completed,bundle,why);
}
/* Arm under the original Q3 reobservation/parent gate. Failures involving
 * uncertain device I/O preserve prepared/running leases for adapter drain. */
bool VHO_TENSOR_SERVING::Start_external(UINT64 ticket,std::vector<void *> &args,bool &armed,std::string &why) {
    armed=false;why.clear();if(!_impl) {why="q3_invalid_session";return false;}
    {
        std::lock_guard<std::mutex> lock(_impl->mutex);
        try {need(_impl->calls.count(ticket) && !_impl->calls[ticket].running ,"q3_stale_or_running_ticket");IMPL::CALL &c=_impl->calls[ticket];
            VHO_SERVING_FACTS f;std::string key;_impl->Observe(c.views,c.metadata,f,key);_impl->Parent(c.views,c.facts.parent_bundle,f);need(key==c.controls,"q3_controls_changed");
            for(unsigned i=0;i<c.views.size();++i) args.push_back(_impl->Address(c.views[i],c.views[i].bytes));
            c.running=true;armed=true;INT32 pending=-1;_impl->Store(args.back(),&pending,4);
        }catch(const std::exception &e) {why=e.what();if(_impl->calls.count(ticket) && !_impl->calls[ticket].running && _impl->Idle()) {_impl->Unpin(_impl->calls[ticket],false);_impl->calls.erase(ticket);}return false;}
    }
    return true;
}
/* Only a proven device drain or a returned synchronous callback reaches this
 * gate. Failed observation with uncertain I/O keeps the running ticket pinned. */
bool VHO_TENSOR_SERVING::Finish_external(UINT64 ticket,bool completed,UINT64 &bundle,std::string &why) {
    bundle=0;why.clear();if(!_impl) {why="q3_invalid_session";return false;}
    std::lock_guard<std::mutex> lock(_impl->mutex);need(_impl->calls.count(ticket) && _impl->calls[ticket].running,"q3_stale_or_unarmed_ticket");IMPL::CALL &c=_impl->calls[ticket];bool success=false;
    try {need(completed,"q3_callback_incomplete");INT32 status;_impl->Read(_impl->Address(c.views.back(),4),&status,4);need(status==0,"q3_source_status_refused");
        VHO_SERVING_FACTS f;std::string key;_impl->Observe(c.views,c.metadata,f,key,true);_impl->Parent(c.views,c.facts.parent_bundle,f);need(key==c.controls,"q3_controls_changed");
        bundle=_impl->Publish(c.views,c.metadata,true,f,&c);success=true;
    }catch(const std::exception &e) {why=e.what();if(!_impl->Idle()) return false;_impl->Unpin(c,true);}
    _impl->calls.erase(ticket);return success;
}
/* Prepared invocations can be abandoned, running callbacks cannot be reclaimed. */
bool VHO_TENSOR_SERVING::Cancel(UINT64 ticket,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->calls.count(ticket) && !_impl->calls[ticket].running,"q3_stale_or_running_ticket");_impl->Unpin(_impl->calls[ticket],false);_impl->calls.erase(ticket);return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Token-only lookup never touches former buffer addresses. */
bool VHO_TENSOR_SERVING::Is_live(UINT64 ticket) const {if(!_impl) return false;std::lock_guard<std::mutex> lock(_impl->mutex);return _impl->calls.count(ticket)!=0;}
/* A reader leases the whole bundle, including its exact mapping metadata. */
bool VHO_TENSOR_SERVING::Acquire_reader(UINT64 bundle,UINT64 &reader,std::string &why) {
    reader=0;why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->bundles.count(bundle),"q3_stale_bundle");++_impl->bundles[bundle].readers;reader=_impl->Issue();_impl->readers[reader]=bundle;return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Publication diagnostics retain exact component versions without exposing
 * borrowed pointers or extending the lifetime of the original publication. */
bool VHO_TENSOR_SERVING::Inspect_bundle(UINT64 id,VHO_SERVING_BUNDLE_FACTS &out,std::string &why) {
    out=VHO_SERVING_BUNDLE_FACTS();why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->bundles.count(id),"q3_stale_bundle");const IMPL::BUNDLE &b=_impl->bundles[id];VHO_SERVING_BUNDLE_FACTS f;f.bundle=id;f.valid_lengths=b.valid_lengths;
        for(unsigned i=0;i<b.views.size();++i) {f.allocation_ids.push_back(b.views[i].handle.id);f.allocation_generations.push_back(b.views[i].handle.generation);f.content_versions.push_back(b.versions[i]);}
        out=f;return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Resolve only under a live bundle reader, keeping the public facts ABI intact. */
bool VHO_TENSOR_SERVING::Bundle_views(UINT64 bundle,std::vector<VHO_SERVING_VIEW> &views,std::string &why) {
    if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->bundles.count(bundle) && _impl->bundles[bundle].readers,"q3_reader_required");views=_impl->bundles[bundle].views;return true;}
    catch(const std::exception &e) {why=e.what();return false;}
}
/* Completing the same reader twice or under another registry is a refusal. */
bool VHO_TENSOR_SERVING::Release_reader(UINT64 reader,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->readers.count(reader),"q3_stale_reader");--_impl->bundles[_impl->readers[reader]].readers;_impl->readers.erase(reader);return true;}catch(const std::exception &e) {why=e.what();return false;}
}
/* Retirement is explicit: old readers survive newer atomic publication. */
bool VHO_TENSOR_SERVING::Retire_bundle(UINT64 id,std::string &why) {
    why.clear();if(!_impl) {why="q3_invalid_session";return false;}std::lock_guard<std::mutex> lock(_impl->mutex);
    try {need(_impl->bundles.count(id),"q3_stale_bundle");IMPL::BUNDLE &b=_impl->bundles[id];need(!b.readers && !b.pending,"q3_bundle_in_use");
        for(unsigned i=0;i<b.views.size();++i) --_impl->Allocation(b.views[i].handle).bundles;
        _impl->bundles.erase(id);if(_impl->tip==id) _impl->tip=0;return true;}catch(const std::exception &e) {why=e.what();return false;}
}

#ifdef OPEN64_SERVING_TEST
/* Test-only local counter: prove permanent exhaustion without mutating any
 * live registry or adding a production fault-injection interface. */
bool VHO_tensor_serving_counter_test() {
    std::atomic<UINT64> counter(std::numeric_limits<UINT64>::max()-1);
    if(issue_counter(counter)!=std::numeric_limits<UINT64>::max()) return false;
    for(unsigned i=0;i<2;++i) {
        try {issue_counter(counter);return false;}
        catch(const std::runtime_error &e) {if(std::string(e.what())!="q3_generation_exhausted") return false;}
    }
    return counter.load()==std::numeric_limits<UINT64>::max();
}
#endif
