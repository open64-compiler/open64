/* Copyright (C) 2026 Open64 Project */
/* Pure H2 structural projection validation under backend ownership. Reuses the
 * existing SHA byte service; no discovery, legality policy, cost, selection,
 * native allocation, physical traffic inference or file/device observation.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "dsl_movement_facts.h"
#include "../vho/fhe_sha256.h"
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <limits>

/* Hash only owned bytes; this imports no cryptographic compiler semantics. */
std::string DSL_movement_digest(const std::string &bytes)
{
    return VHO_FHE_SHA256(reinterpret_cast<const unsigned char *>(bytes.data()),bytes.size());
}
/* Preserve the established unknown sentinel, leaving output unchanged on error. */
bool DSL_movement_add(uint64_t a,uint64_t b,uint64_t &out)
{
    if(a>=~uint64_t(0)-b) return false;out=a+b;return true;
}
/* Refuse sentinel/overflow even when a zero multiplier could hide an unknown. */
bool DSL_movement_mul(uint64_t a,uint64_t b,uint64_t &out)
{
    if(a==~uint64_t(0) || b==~uint64_t(0)) return false;
    if(b && a>(~uint64_t(0)-1)/b) return false;out=a*b;return true;
}
namespace {
/* Framed fields make owned identity unambiguous, including empty/unknown data. */
void DSL_movement_field(std::ostringstream &key,const std::string &value) {key<<value.size()<<':'<<value<<';';}
/* Structural refusals never grant an opportunity or retain a partial snapshot. */
void DSL_movement_need(bool condition,const char *why) {if(!condition) throw std::runtime_error(why);}
/* Serialize every binding field; shape/name equality is not storage equality. */
std::string DSL_movement_binding_key(const DSL_MOVEMENT_BINDING &b)
{
    std::ostringstream key;const std::string fields[]={b.family,b.owner,b.source_key,b.proof_key,b.controls,b.serving_key,b.device_key,b.device.Cache_key()};
    for(unsigned i=0;i<8;++i) DSL_movement_field(key,fields[i]);
    key<<b.epoch_known<<';'<<b.activation<<';'<<b.mutation<<';';return key.str();
}
/* Validate bounded DAG reachability and exact generation/participant joins.
 * Scope labels describe the existing proof; they do not create a new fence. */
std::string DSL_movement_content(const DSL_MOVEMENT_CAPTURE &c)
{
    const DSL_MOVEMENT_BINDING &b=c.binding;
    DSL_movement_need(!b.family.empty() && !b.owner.empty() && !b.source_key.empty() && !b.proof_key.empty(),"movement_missing_source_binding");
    DSL_movement_need(!b.epoch_known || b.activation,"movement_missing_activation");
    DSL_movement_need(b.device_key.empty() || b.device.Cache_key().empty() || b.device_key==b.device.Cache_key(),"movement_profile_binding");
    DSL_movement_need(c.storages.size() && c.storages.size()<=4096 && c.accesses.size()<=16384 && c.events.size()<=64,"movement_census_bounds");
    std::ostringstream key;key<<"owned-movement-v1;";DSL_movement_field(key,DSL_movement_binding_key(b));
    const DSL_MOVEMENT_WORK &w=c.work;DSL_movement_need(w.instances && w.width && w.active_last<=w.width && w.inactive_last<=w.width && w.active_last+w.inactive_last<=w.width,"movement_work_bounds");
    DSL_movement_field(key,w.owner);DSL_movement_field(key,w.domain);DSL_movement_field(key,w.tail_policy);DSL_movement_field(key,w.numerical);
    key<<w.instances<<';'<<w.width<<';'<<w.active_last<<';'<<w.inactive_last<<';';
    key<<"storage-count="<<c.storages.size()<<';';
    DSL_movement_need(!w.axes.empty() && w.axes.size()==w.extents.size() && w.axes.size()<=32,"movement_work_domain");
    key<<"axes="<<w.axes.size()<<';';
    for(size_t i=0;i<w.axes.size();++i) {DSL_movement_need(!w.axes[i].empty() && w.extents[i],"movement_work_extent");DSL_movement_field(key,w.axes[i]);key<<w.extents[i]<<';';}
    std::map<std::string,const DSL_MOVEMENT_STORAGE *> storage;
    for(size_t i=0;i<c.storages.size();++i) {
        const DSL_MOVEMENT_STORAGE &s=c.storages[i];
        DSL_movement_need(!s.id.empty() && !s.allocation.empty() && !s.version.empty() && s.bytes && s.offset<=s.capacity && s.bytes<=s.capacity-s.offset && storage.insert(std::make_pair(s.id,&s)).second,"movement_storage_range_or_identity");
        DSL_movement_need(!s.strides_known || (!s.shape.empty() && s.shape.size()==s.strides.size()),"movement_stride_proof");
        if(s.strides_known) {
            uint64_t extent=s.element_bytes;DSL_movement_need(extent && s.shape.size()<=32,"movement_element_or_rank");
            for(size_t j=0;j<s.shape.size();++j) {
                uint64_t span;DSL_movement_need(s.shape[j] && DSL_movement_mul(s.shape[j]-1,s.strides[j],span) && DSL_movement_add(extent,span,extent),"movement_stride_overflow");
            }
            DSL_movement_need(extent<=s.bytes,"movement_stride_range");
        }
        const std::string fields[]={s.id,s.allocation,s.version,s.encoding,s.role,s.layout,s.first_use,s.last_use};
        for(unsigned j=0;j<8;++j) DSL_movement_field(key,fields[j]);
        key<<s.offset<<';'<<s.bytes<<';'<<s.capacity<<';'<<s.element_bytes<<';'<<s.alignment<<';'<<s.alias_proved<<';'<<s.strides_known<<';'<<s.shape.size()<<';';
        for(size_t j=0;j<s.shape.size();++j) key<<s.shape[j]<<';';key<<s.strides.size()<<';';
        for(size_t j=0;j<s.strides.size();++j) key<<s.strides[j]<<';';
        for(size_t j=0;j<i;++j) if(c.storages[j].allocation==s.allocation)
            DSL_movement_need(c.storages[j].capacity==s.capacity,"movement_alias_capacity_disagreement");
    }
    key<<"access-count="<<c.accesses.size()<<';';
    std::set<std::string> access_ids,event_ids;
    for(size_t i=0;i<c.accesses.size();++i) {
        const DSL_MOVEMENT_ACCESS &a=c.accesses[i];
        DSL_movement_need(storage.count(a.storage) && access_ids.insert(a.id).second && !a.id.empty(),"movement_access_join");
        const DSL_MOVEMENT_STORAGE &s=*storage[a.storage];
        DSL_movement_need(a.version==s.version && a.offset>=s.offset && a.offset-s.offset<=s.bytes && a.extent<=s.bytes-(a.offset-s.offset) && a.multiplicity,"movement_access_range_or_version");
        DSL_movement_need(!a.reuse_proved || (a.counts_known && s.alias_proved && s.strides_known && !a.write_bytes && !a.reuse_scope.empty()),"movement_reuse_proof_missing");
        const std::string fields[]={a.id,a.storage,a.version,a.owner,a.relation,a.reuse_scope};
        for(unsigned j=0;j<6;++j) DSL_movement_field(key,fields[j]);
        key<<a.offset<<';'<<a.extent<<';'<<a.read_bytes<<';'<<a.write_bytes<<';'<<a.multiplicity<<';'<<a.reuse_proved<<';'<<a.counts_known<<';';
    }
    key<<"event-count="<<c.events.size()<<';';
    uint64_t reach[64]={};
    for(size_t i=0;i<c.events.size();++i) {
        const DSL_MOVEMENT_EVENT &e=c.events[i];
        DSL_movement_need(!e.id.empty() && !e.kind.empty() && !e.scope.empty() && !e.evidence.empty() && event_ids.insert(e.id).second,"movement_event_identity");
        DSL_movement_need(e.storage.empty() || (storage.count(e.storage) && !e.version.empty()),"movement_event_storage_join");
        const std::string fields[]={e.id,e.kind,e.storage,e.version,e.scope,e.evidence};for(unsigned j=0;j<6;++j) DSL_movement_field(key,fields[j]);
    }
    key<<"edge-count="<<c.edges.size()<<';';
    for(size_t i=0;i<c.edges.size();++i) {
        const DSL_MOVEMENT_EDGE &e=c.edges[i];DSL_movement_need(e.from<c.events.size() && e.to<c.events.size() && e.from!=e.to,"movement_event_edge");
        reach[e.from]|=uint64_t(1)<<e.to;key<<e.from<<'>'<<e.to<<';';
    }
    for(size_t k=0;k<c.events.size();++k) for(size_t i=0;i<c.events.size();++i) if(reach[i]&(uint64_t(1)<<k)) reach[i]|=reach[k];
    for(size_t i=0;i<c.events.size();++i) DSL_movement_need(!(reach[i]&(uint64_t(1)<<i)),"movement_event_cycle");
    key<<"lifetime-count="<<c.lifetimes.size()<<';';
    for(size_t i=0;i<c.lifetimes.size();++i) {
        const DSL_MOVEMENT_LIFETIME &l=c.lifetimes[i];
        unsigned nodes[]={l.raw_ready,l.decoded_ready,l.consumer,l.readers_done,l.reuse_ready};
        unsigned count=l.has_reuse?5:4;
        DSL_movement_need(storage.count(l.storage) && !l.version.empty() && (!l.has_reuse || (!l.next_version.empty() && l.next_version!=l.version)),"movement_lifetime_generation");
        for(unsigned j=0;j<count;++j) DSL_movement_need(nodes[j]<c.events.size() && c.events[nodes[j]].storage==((j==0 && l.decoded && !l.raw_storage.empty())?l.raw_storage:l.storage) && c.events[nodes[j]].version==(j==4?l.next_version:l.version),"movement_lifetime_join");
        for(unsigned j=1;j<count;++j) DSL_movement_need(c.events[nodes[j]].scope==c.events[nodes[0]].scope,"movement_participant_scope_changed");
        for(unsigned j=1;j<count;++j) DSL_movement_need((j==1 && !l.decoded && nodes[0]==nodes[1]) || (reach[nodes[j-1]]&(uint64_t(1)<<nodes[j])),"movement_readiness_or_reuse_order");
        if(l.decoded) DSL_movement_need(c.events[nodes[0]].kind=="raw-ready" && c.events[nodes[1]].kind=="decoded-ready","movement_raw_decoded_readiness");
        const std::string fields[]={l.storage,l.version,l.next_version,l.raw_storage};for(unsigned j=0;j<4;++j) DSL_movement_field(key,fields[j]);
        key<<l.decoded<<';'<<l.has_reuse<<';';for(unsigned j=0;j<5;++j) key<<nodes[j]<<';';
    }
    const DSL_MOVEMENT_LEDGER &v=c.ledger;
    DSL_movement_need(!v.transactions.known && !v.l2.known && !v.hbm.known && !v.spills.known,"movement_physical_not_source_fact");
    const DSL_MOVEMENT_BYTES values[]={v.useful_reads,v.useful_writes,v.source_reads,v.source_writes,v.requested_reads,v.requested_writes,v.transactions,v.l2,v.hbm,v.shared,v.spills,v.scale,v.page,v.index,v.partial};
    for(unsigned i=0;i<15;++i) {DSL_movement_need(!values[i].known || values[i].value!=~uint64_t(0),"movement_unknown_sentinel");key<<values[i].known<<';'<<(values[i].known?values[i].value:0)<<';';}
    return key.str();
}
}
/* Validation is all-or-nothing and preserves no stale successful body. */
bool DSL_MOVEMENT_FACTS::Create(const DSL_MOVEMENT_CAPTURE &capture,DSL_MOVEMENT_FACTS &out,std::string &reason)
{
    out=DSL_MOVEMENT_FACTS();
    try {DSL_MOVEMENT_FACTS result;result._content=DSL_movement_content(capture);result._capture=capture;
        result._key=DSL_movement_digest(result._content);result._valid=true;out=result;reason="owned";return true;
    } catch(const std::exception &error) {reason=error.what();return false;}
}
/* A closed/unknown epoch is deliberately unusable even when content survives. */
bool DSL_MOVEMENT_FACTS::Is_current(const DSL_MOVEMENT_BINDING &binding,bool epoch_live) const
{
    return _valid && epoch_live && _capture.binding.epoch_known && binding.epoch_known &&
        DSL_movement_binding_key(_capture.binding)==DSL_movement_binding_key(binding);
}
/* Compact summaries keep full source/proof identities owned but out of logs. */
void DSL_MOVEMENT_FACTS::Print(FILE *file,const char *stage) const
{
    if(!file || !_valid) return;
    const DSL_MOVEMENT_BINDING &b=_capture.binding;
    fprintf(file,"AI_MOVEMENT_FACTS 1 family=%s stage=%s key=%s source=%s device=%s storages=%lu accesses=%lu events=%lu lifetimes=%lu physical_traffic=unknown spills=unknown native_recapture=required search_mutation=none\n",
        b.family.c_str(),stage,_key.c_str(),DSL_movement_digest(b.source_key).c_str(),(b.device_key.empty()?b.device.Cache_key():b.device_key).c_str(),
        (unsigned long)_capture.storages.size(),(unsigned long)_capture.accesses.size(),(unsigned long)_capture.events.size(),(unsigned long)_capture.lifetimes.size());
    fprintf(file,"AI_MOVEMENT_WORK 1 family=%s instances=%llu width=%llu active_last=%llu inactive_last=%llu epoch_known=%u native_mutation=none\n",
        b.family.c_str(),(unsigned long long)_capture.work.instances,(unsigned long long)_capture.work.width,
        (unsigned long long)_capture.work.active_last,(unsigned long long)_capture.work.inactive_last,unsigned(b.epoch_known));
    const DSL_MOVEMENT_LEDGER &v=_capture.ledger;
    const DSL_MOVEMENT_BYTES values[]={v.useful_reads,v.useful_writes,v.source_reads,v.source_writes,v.requested_reads,v.requested_writes,v.transactions,v.l2,v.hbm,v.shared,v.spills,v.scale,v.page,v.index,v.partial};
    const char *names[]={"useful_reads","useful_writes","source_reads","source_writes","requested_reads","requested_writes","transactions","L2","HBM","shared_requests","spills","scale_payload","page_payload","index_payload","partial"};
    for(unsigned i=0;i<15;++i) fprintf(file,"AI_MOVEMENT_BYTES 1 family=%s term=%s known=%u bytes=%llu evidence=%s\n",
        b.family.c_str(),names[i],unsigned(values[i].known),(unsigned long long)(values[i].known?values[i].value:0),values[i].known?"source_or_bound_recipe":"unknown");
    for(size_t i=0;i<_capture.storages.size();++i) {
        const DSL_MOVEMENT_STORAGE &s=_capture.storages[i];
        fprintf(file,"AI_MOVEMENT_STORAGE 1 family=%s id=%s allocation=%s version=%s offset=%llu extent=%llu capacity=%llu alignment=%llu strides_known=%u alias_proved=%u\n",
            b.family.c_str(),s.id.c_str(),s.allocation.c_str(),s.version.c_str(),(unsigned long long)s.offset,(unsigned long long)s.bytes,
            (unsigned long long)s.capacity,(unsigned long long)s.alignment,unsigned(s.strides_known),unsigned(s.alias_proved));
    }
    for(size_t i=0;i<_capture.accesses.size();++i) {
        const DSL_MOVEMENT_ACCESS &a=_capture.accesses[i];
        fprintf(file,"AI_MOVEMENT_ACCESS 1 family=%s id=%s storage=%s version=%s counts_known=%u read_bytes=%llu write_bytes=%llu multiplicity=%llu reuse_proved=%u\n",
            b.family.c_str(),a.id.c_str(),a.storage.c_str(),a.version.c_str(),unsigned(a.counts_known),(unsigned long long)a.read_bytes,
            (unsigned long long)a.write_bytes,(unsigned long long)a.multiplicity,unsigned(a.reuse_proved));
    }
    for(size_t i=0;i<_capture.events.size();++i) {
        const DSL_MOVEMENT_EVENT &e=_capture.events[i];
        fprintf(file,"AI_MOVEMENT_EVENT 1 family=%s ordinal=%lu kind=%s storage=%s version=%s scope=%s\n",b.family.c_str(),(unsigned long)i,e.kind.c_str(),e.storage.c_str(),e.version.c_str(),e.scope.c_str());
    }
}

/* Dense projection uses source-validated extents, never tensor names as proof. */
DSL_MOVEMENT_STORAGE DSL_movement_dense_storage(const std::string &id,const std::string &allocation,
    const std::string &version,const std::string &role,const std::vector<uint64_t> &shape,uint64_t element_bytes,bool alias_proved)
{
    DSL_MOVEMENT_STORAGE s;s.id=id;s.allocation=allocation;s.version=version;s.role=role;
    s.encoding=element_bytes==4?"F32":element_bytes==8?"I64":"byte";s.layout="dense-row-major";
    s.element_bytes=element_bytes;s.shape=shape;s.strides.resize(shape.size());s.alias_proved=alias_proved;s.strides_known=true;
    uint64_t extent=element_bytes;DSL_movement_need(!shape.empty() && element_bytes,"movement_dense_shape");
    for(size_t i=shape.size();i;--i) {s.strides[i-1]=extent;DSL_movement_need(shape[i-1] && DSL_movement_mul(extent,shape[i-1],extent),"movement_dense_overflow");}
    s.bytes=s.capacity=extent;return s;
}
/* These symbolic source/conditional-contract boundaries grant no async engine,
 * barrier implementation, runtime lease or new cross-participant visibility. */
void DSL_movement_source_lifetime(DSL_MOVEMENT_CAPTURE &c,const std::string &storage,
    const std::string &version,const std::string &scope,const std::string &evidence)
{
    unsigned first=c.events.size();const char *kinds[]={"source-ready","source-consume","readers-complete"};
    for(unsigned i=0;i<3;++i) {
        DSL_MOVEMENT_EVENT e;std::ostringstream id;id<<storage<<"/source/"<<i;e.id=id.str();e.kind=kinds[i];
        e.storage=storage;e.version=version;e.scope=scope;e.evidence=evidence;c.events.push_back(e);
    }
    c.edges.push_back(DSL_MOVEMENT_EDGE(first,first+1));c.edges.push_back(DSL_MOVEMENT_EDGE(first+1,first+2));
    DSL_MOVEMENT_LIFETIME l;l.storage=storage;l.version=version;l.raw_ready=l.decoded_ready=first;
    l.consumer=first+1;l.readers_done=first+2;c.lifetimes.push_back(l);
}

/* Bound copied standard containers with slack for allocator/node overhead.
 * Every capacity is charged, including duplicate full-content/key strings. */
uint64_t DSL_MOVEMENT_FACTS::Storage_charge() const
{
    uint64_t bytes=sizeof(*this)+_content.capacity()+_key.capacity()+1024;
    // The complete framed serialization includes every variable field. Twice
    // its capacity plus per-record overhead conservatively covers the copied
    // capture, whose vectors are rebuilt from their actual element counts.
    uint64_t copy,overhead;
    if(!DSL_movement_mul(_content.size(),2,copy) ||
        !DSL_movement_mul(_capture.storages.size()+_capture.accesses.size()+_capture.events.size()+_capture.lifetimes.size()+_capture.edges.size()+64,1024,overhead) ||
        !DSL_movement_add(bytes,copy,bytes) || !DSL_movement_add(bytes,overhead,bytes)) return ~uint64_t(0);
    return bytes;
}
