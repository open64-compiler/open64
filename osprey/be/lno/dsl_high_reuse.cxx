/* Copyright (C) 2026 Open64 Project */
/* Pure MR1 analysis over copied High facts and the validated common algorithm.
 * No compiler-context services or new cost/selection policy. Source dense
 * requests and projected fused requests are distinct from GPU/HBM traffic.
 * See doc/AI-COMPILER-MLA-MR1-OWNED-REUSE-v1.0.md. */
#include "dsl_high_reuse.h"
#include "../vho/dsl_gemm_readiness.h"
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>

namespace {
/* Stable refusals never publish a partially proved reuse family. */
void need(bool ok,const char *why) { if(!ok) throw std::runtime_error(why); }
/* Canonical integer spelling and length framing avoid identity collisions. */
template<class T> std::string number(T value) { std::ostringstream s;s<<value;return s.str(); }
void field(std::string &key,const std::string &value) { key+=number(value.size())+":"+value; }
/* Reuse existing checked planning arithmetic; UINT64_MAX is never a value. */
uint64_t mul(uint64_t a,uint64_t b) { uint64_t n;need(DSL_readiness_mul(a,b,&n),"reuse_byte_count_overflow");return n; }
void add(uint64_t &a,uint64_t b) { uint64_t n;need(DSL_readiness_add(a,b,&n),"reuse_byte_count_overflow");a=n; }
/* Logical shapes in admitted recipes are positive, dense and statically bounded. */
uint64_t elements(const std::vector<UINT64> &shape) {
    uint64_t n=1;for(size_t i=0;i<shape.size();++i) n=mul(n,shape[i]);return n;
}
/* Address units are elements, not bytes, until an access's type is checked. */
std::vector<uint64_t> strides(const std::vector<UINT64> &shape) {
    std::vector<uint64_t> result(shape.size(),1);
    for(size_t i=shape.size();i>1;--i) result[i-2]=mul(result[i-1],shape[i-1]);
    return result;
}
/* Interpret only the same constant zero-based unit loops admitted by LNO. */
uint64_t trips(const DSL_HIGH_LOOP &loop) {
    need(loop.lower==0 && loop.step==1 && loop.upper>=0,"reuse_loop_bounds_unproved");
    uint64_t n=loop.upper;
    const std::string &c=loop.comparison;
    if(c.size()>=2 && c.substr(c.size()-2)=="LE") add(n,1);
    else need(c.size()>=2 && c.substr(c.size()-2)=="LT","reuse_loop_bounds_unproved");
    need(n && n<=16777216,"reuse_loop_bounds_unproved");return n;
}
/* Alias table pairs are symmetric. Access IDs, not base names, identify them. */
std::string pair_key(std::string a,std::string b) { if(b<a) std::swap(a,b);return a+";"+b; }

class ANALYSIS {
    const VHO_TENSOR_PROGRAM &p;
    const std::vector<DSL_HIGH_STORAGE> &native_storages;
    const std::vector<DSL_HIGH_ACCESS> &accesses;
    DSL_HIGH_REUSE_SUMMARY out;
    std::map<std::string,const DSL_HIGH_LOOP *> loops;
    std::map<std::string,unsigned> native_to_value;
    std::map<unsigned,size_t> result_storage;
    std::map<unsigned,unsigned> producers, family;
    std::map<unsigned,std::vector<const DSL_HIGH_ACCESS *> > writes;
    std::map<std::string,unsigned> group_operation;
    std::map<std::string,bool> alias;
    std::map<unsigned,std::string> versions;
    VHO_TENSOR_ATTENTION attention;
    /* Resolve loop identities from owned values only. */
    const DSL_HIGH_LOOP &loop(const std::string &id) const {
        std::map<std::string,const DSL_HIGH_LOOP *>::const_iterator i=loops.find(id);
        need(i!=loops.end(),"reuse_missing_loop");return *i->second;
    }
    /* Validate one dense scalar ARRAY projection, refusing unknown views. */
    void address(const DSL_HIGH_ACCESS &a,unsigned storage) const {
        need(a.direct_formal && !a.storage.empty(),"reuse_opaque_base");
        need(!a.conditional,"reuse_conditional_access");
        need(a.byte_offset==0 && a.dimensions.size()==1 && a.array_extent>0 &&
             (uint64_t)a.array_extent==elements(p.values[storage].shape),"reuse_strided_or_offset_view");
        const DSL_HIGH_DIM &d=a.dimensions[0];
        need(!d.nonconstant_loops && d.linear_symbols.empty() && d.constant>=0 &&
             d.loop_coefficients.size()==a.loop_ids.size(),"reuse_symbolic_access");
        need(a.element_bytes==(p.values[storage].dtype=="bool"?1:4),"reuse_element_encoding");
        need(a.element_encoding==(p.values[storage].dtype=="bool"?"U1":"F4"),"reuse_element_encoding");
        for(size_t i=0;i<a.loop_ids.size();++i) trips(loop(a.loop_ids[i]));
    }
    /* Prove that a producer writes every element once by matching output
     * strides and extents, without recognizing induction-variable names. */
    std::vector<std::string> output_axes(const DSL_HIGH_ACCESS &write,unsigned output) const {
        unsigned storage=p.values[output].storage;address(write,storage);
        need(write.dimensions[0].constant==0,"reuse_output_coverage_unproved");
        const std::vector<UINT64> &shape=p.values[output].shape;
        std::vector<uint64_t> step=strides(shape);std::vector<std::string> axes(shape.size());
        for(size_t i=0;i<write.loop_ids.size();++i) {
            uint64_t count=trips(loop(write.loop_ids[i]));if(count==1) continue;
            int64_t coefficient=write.dimensions[0].loop_coefficients[i];bool matched=false;
            for(size_t j=0;j<shape.size();++j)
                if(axes[j].empty() && shape[j]==count && coefficient>0 && (uint64_t)coefficient==step[j]) {
                    axes[j]=write.loop_ids[i];matched=true;break;
                }
            need(matched,"reuse_output_coverage_unproved");
        }
        for(size_t j=0;j<shape.size();++j) need(shape[j]==1 || !axes[j].empty(),"reuse_output_coverage_unproved");
        return axes;
    }
    /* Expected coordinates come from the admitted semantic contraction/view,
     * then must equal the native affine vectors, including the head dimension. */
    void read_map(const DSL_HIGH_ACCESS &read,unsigned operation,unsigned operand,
                  DSL_HIGH_REUSE_ACCESS &record) const {
        const VHO_TENSOR_OPERATION &op=p.operations[operation];
        const VHO_TENSOR_VALUE &input=p.values[op.inputs[operand]],&output=p.values[op.output];
        need(writes.count(output.storage) && writes.find(output.storage)->second.size()==1,"reuse_output_definition_unproved");
        const DSL_HIGH_ACCESS &write=*writes.find(output.storage)->second[0];
        need(write.execution_group==read.execution_group,"reuse_execution_scope_unproved");
        std::vector<std::string> axes=output_axes(write,op.output);
        std::vector<uint64_t> step=strides(input.shape);
        std::map<std::string,uint64_t> expected;
        for(size_t i=0;i<read.loop_ids.size();++i) expected[read.loop_ids[i]]=0;
        std::vector<std::string> input_axes(input.shape.size());
        if(op.name=="common.transpose") {
            need(read.dimensions[0].constant==0,"reuse_transpose_layout");
            need(operand==0,"reuse_transpose_operand");
            std::string text=op.attributes.find("attr.permutation")->second;
            const char *pos=text.c_str();
            for(size_t i=0;i<axes.size();++i) {
                char *end;unsigned axis=strtoul(pos,&end,10);
                need(axis<input_axes.size(),"reuse_transpose_layout");input_axes[axis]=axes[i];pos=*end?end+1:end;
            }
            record.purpose="materialized-cache-transpose";record.head_multiplicity=1;
        } else {
            need(op.name=="common.matmul" && operand==1 && input.shape.size()>=2,"reuse_cache_consumer_unproved");
            need(axes.size()>=input.shape.size() && axes.size()>=2,"reuse_cache_consumer_unproved");
            for(size_t i=0;i+2<input.shape.size();++i)
                if(input.shape[i]!=1) input_axes[i]=axes[axes.size()-input.shape.size()+i];
            input_axes.back()=axes.back();
            std::string reduction;
            for(size_t i=0;i<read.loop_ids.size();++i) {
                const std::string &id=read.loop_ids[i];
                if(trips(loop(id))>1 && std::find(axes.begin(),axes.end(),id)==axes.end()) {
                    need(reduction.empty(),"reuse_reduction_scope_unproved");reduction=id;
                }
            }
            uint64_t k=input.shape[input.shape.size()-2];
            record.reduction_extent=k;
            if(k>1 && reduction.empty()) {
                /* PREOPT may fully unroll a small reduction. Each native load
                 * then covers one fixed-coordinate slice; the later census
                 * must prove all slices exactly once before publishing reuse. */
                record.sliced_reduction=true;
                uint64_t offset=read.dimensions[0].constant,unit=step[step.size()-2];
                need(offset%unit==0 && offset/unit<k,"reuse_reduction_slice_unproved");
                record.reduction_slice=offset/unit;
            } else {
                need((k==1 && reduction.empty()) || (!reduction.empty() && trips(loop(reduction))==k),"reuse_reduction_scope_unproved");
                need(read.dimensions[0].constant==0,"reuse_strided_or_head_address");
            }
            input_axes[input_axes.size()-2]=reduction;
            record.head_loop=axes[1];record.head_multiplicity=attention.heads;
            if(!record.head_loop.empty()) {
                std::vector<std::string>::const_iterator h=std::find(read.loop_ids.begin(),read.loop_ids.end(),record.head_loop);
                need(h!=read.loop_ids.end(),"reuse_head_scope_unproved");
                need(read.dimensions[0].loop_coefficients[h-read.loop_ids.begin()]==0,"reuse_head_dependent_access");
            }
            record.purpose=operation==33?"weighted-latent":"score";
        }
        for(size_t i=0;i<input_axes.size();++i) if(input.shape[i]!=1 &&
            !(record.sliced_reduction && i==input_axes.size()-2)) {
            need(!input_axes[i].empty() && expected.count(input_axes[i]),"reuse_coordinate_unproved");
            add(expected[input_axes[i]],step[i]);
        }
        record.read_elements=1;
        for(size_t i=0;i<read.loop_ids.size();++i) {
            uint64_t count=trips(loop(read.loop_ids[i]));record.read_elements=mul(record.read_elements,count);
            if(count==1) continue;
            int64_t coefficient=read.dimensions[0].loop_coefficients[i];
            need(coefficient>=0 && (uint64_t)coefficient==expected[read.loop_ids[i]],"reuse_strided_or_head_address");
        }
        record.read_bytes=mul(record.read_elements,4);
        record.footprint_bytes=mul(elements(p.values[input.storage].shape),4);
        if(record.sliced_reduction) record.footprint_bytes/=record.reduction_extent;
        record.head_invariant=true;
    }
    /* Establish a complete write census and the top-level producer ordering.
     * Ambiguous scopes/multiple versions are refused instead of guessed. */
    void definitions() {
        for(size_t i=0;i<p.operations.size();++i) if(p.operations[i].name!="common.reshape")
            need(producers.insert(std::make_pair(p.values[p.operations[i].output].storage,(unsigned)i)).second,"reuse_multiple_definitions");
        for(size_t i=0;i<accesses.size();++i) {
            const DSL_HIGH_ACCESS &a=accesses[i];
            need(a.direct_formal && native_to_value.count(a.storage),"reuse_unrepresented_memory");
            unsigned storage=native_to_value[a.storage];
            if(a.write) {
                need(storage!=attention.cache && storage!=attention.rope_cache,"reuse_cache_storage_written");
                writes[storage].push_back(&a);
                need(producers.count(storage),"reuse_unmodeled_storage_write");
                unsigned operation=producers[storage];
                if(group_operation.count(a.execution_group))
                    need(group_operation[a.execution_group]==operation,"reuse_ambiguous_execution_group");
                group_operation[a.execution_group]=operation;
            }
        }
        for(size_t i=0;i<out.storages.size();++i) {
            DSL_HIGH_REUSE_STORAGE &s=out.storages[i];
            if(p.values[s.value].role==1) {
                versions[s.value]="entry/"+s.native_storage;s.producer_operation=UINT32_MAX;
            } else if(producers.count(s.value) && writes[s.value].size()==1) {
                const DSL_HIGH_ACCESS &w=*writes[s.value][0];
                versions[s.value]="definition/"+number(producers[s.value])+"/"+w.execution_group;
                s.producer_operation=producers[s.value];s.producer_group=w.execution_group;
            } else versions[s.value]="unproved";
            s.version=versions[s.value];
        }
    }
    /* A read's version is stable only if every possibly aliasing write is
     * excluded, except its unique complete dominating transpose definition. */
    void lifetime(const DSL_HIGH_ACCESS &read,unsigned storage) const {
        if(storage!=family.find(storage)->second) {
            need(writes.count(storage) && writes.find(storage)->second.size()==1,"reuse_multi_store_producer_unproved");
            const DSL_HIGH_ACCESS &producer=*writes.find(storage)->second[0];
            output_axes(producer,p.operations[producers.find(storage)->second].output);
            need(producer.execution_order<read.execution_order,"reuse_producer_not_dominating");
        }
        for(size_t j=0;j<accesses.size();++j) if(accesses[j].write) {
            const DSL_HIGH_ACCESS &w=accesses[j];
            unsigned written=native_to_value.find(w.storage)->second;
            if(written==storage) {
                need(storage!=family.find(storage)->second && writes.find(storage)->second.size()==1 &&
                    &w==writes.find(storage)->second[0],"reuse_cache_version_clobbered");
                continue;
            }
            std::map<std::string,bool>::const_iterator answer=alias.find(pair_key(read.id,w.id));
            need(answer!=alias.end(),"reuse_alias_coverage_missing");
            need(answer->second,"reuse_possible_alias_write");
        }
    }
public:
    /* Inputs are immutable owned records, never native handles. */
    ANALYSIS(const std::vector<DSL_HIGH_STORAGE> &s,const std::vector<DSL_HIGH_LOOP> &l,
             const std::vector<DSL_HIGH_ACCESS> &a,const std::vector<DSL_HIGH_ALIAS> &aliases,
             const VHO_TENSOR_PROGRAM &program) : p(program),native_storages(s),accesses(a) {
        for(size_t i=0;i<l.size();++i) need(loops.insert(std::make_pair(l[i].id,&l[i])).second,"reuse_duplicate_loop");
        for(size_t i=0;i<aliases.size();++i)
            need(alias.insert(std::make_pair(pair_key(aliases[i].first,aliases[i].second),aliases[i].disjoint)).second,"reuse_duplicate_alias_answer");
    }
    /* Prove closed MLA content lineage, full native accesses and read lifetimes. */
    DSL_HIGH_REUSE_SUMMARY Run() {
        need(p.opt_level==3 && !p.fuse && !p.cooperative && !p.fuse_attention && !p.split_width &&
            !p.auto_chunks && !p.search_selected && VHO_tensor_validate_program(p,NULL),"reuse_source_algorithm_unproved");
        VHO_TENSOR_PROGRAM descriptor=p;descriptor.fuse=descriptor.cooperative=descriptor.fuse_attention=true;
        descriptor.split_width=128;
        attention=VHO_tensor_attention(descriptor);need(attention.enabled,"reuse_attention_roles_unproved");
        unsigned formal=0;
        for(unsigned i=0;i<p.values.size();++i) if(p.values[i].storage==i) {
            need(formal<native_storages.size(),"reuse_storage_abi_mismatch");
            const DSL_HIGH_STORAGE &n=native_storages[formal];
            need(n.formal==formal && n.id=="F/"+number(formal) && n.pointer_formal &&
                n.element_bytes==(p.values[i].dtype=="bool"?1:4) && n.cache_trait==p.values[i].cache_trait,
                "reuse_storage_abi_mismatch");
            need(n.element_encoding==(p.values[i].dtype=="bool"?"U1":"F4"),"reuse_element_encoding");
            need(native_to_value.insert(std::make_pair(n.id,i)).second,"reuse_duplicate_storage");
            DSL_HIGH_REUSE_STORAGE record;record.value=i;record.root=UINT32_MAX;
            record.native_storage=n.id;record.native=n;
            record.shape.assign(p.values[i].shape.begin(),p.values[i].shape.end());
            record.strides=strides(p.values[i].shape);record.layout="dense-row-major";
            record.locality.object_bytes=mul(elements(p.values[i].shape),n.element_bytes);
            record.locality.size_state=DSL_TENSOR_SIZE_STATIC;
            for(unsigned j=0;j<p.values.size();++j) if(p.values[j].storage==i) record.alias_values.push_back(j);
            result_storage[i]=out.storages.size();out.storages.push_back(record);++formal;
        }
        need(formal==native_storages.size(),"reuse_storage_abi_mismatch");
        family[attention.cache]=attention.cache;family[attention.rope_cache]=attention.rope_cache;
        for(size_t i=0;i<p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=p.operations[i];
            unsigned source=p.values[op.inputs[0]].storage;
            if(op.name=="common.transpose" && family.count(source)) family[p.values[op.output].storage]=family[source];
        }
        definitions();
        for(std::map<unsigned,unsigned>::const_iterator i=family.begin();i!=family.end();++i) {
            DSL_HIGH_REUSE_STORAGE &s=out.storages[result_storage[i->first]];
            need(s.native.base_stable && s.native.uses_only_as_array_base && s.native.restrict_qualified,
                "reuse_storage_lifetime_unproved");
            s.root=i->second;s.origin_version=versions[s.root];
            s.locality.estimated_read_bytes=0;s.locality.estimated_write_bytes=0;
            /* No-alias writes prove this read version stable. Read/read
             * aliasing is not promoted to globally unique allocation state. */
            s.locality.alias_state=DSL_TENSOR_ALIAS_CONSERVATIVE;
            s.locality.lifetime_state=s.value==s.root?DSL_TENSOR_LIFETIME_LOOP:DSL_TENSOR_LIFETIME_DOMINATED;
            if(s.value!=s.root) need(s.version!="unproved","reuse_multi_store_producer_unproved");
        }
        for(size_t i=0;i<accesses.size();++i) {
            const DSL_HIGH_ACCESS &a=accesses[i];unsigned storage=native_to_value[a.storage];
            if(a.write || !family.count(storage)) continue;
            need(group_operation.count(a.execution_group),"reuse_consumer_scope_unproved");
            unsigned operation=group_operation[a.execution_group];
            const VHO_TENSOR_OPERATION &op=p.operations[operation];unsigned operand=UINT32_MAX;
            for(unsigned k=0;k<op.inputs.size();++k) if(p.values[op.inputs[k]].storage==storage) {
                need(operand==UINT32_MAX,"reuse_ambiguous_cache_operand");operand=k;
            }
            need(operand!=UINT32_MAX,"reuse_unmodeled_cache_read");
            address(a,storage);lifetime(a,storage);
            DSL_HIGH_REUSE_ACCESS record;record.id=a.id;record.storage=storage;record.root=family[storage];
            record.operation=operation;record.operand=op.inputs[operand];record.native=a;
            record.execution_group=a.execution_group;record.storage_version=versions[storage];
            read_map(a,operation,operand,record);out.accesses.push_back(record);
        }
        const unsigned roots[]={attention.cache,attention.rope_cache};
        for(unsigned i=0;i<2;++i) {
            DSL_HIGH_REUSE_CACHE cache;cache.root=roots[i];cache.heads=attention.heads;
            cache.logical_sweeps=i?1:2;cache.role=i?DSL_TENSOR_TRAIT_ROTARY_KEY_CACHE:DSL_TENSOR_TRAIT_LATENT_KV_CACHE;
            cache.role_provenance=p.values[cache.root].cache_trait.empty()?"closed-graph-derived":"declared-and-graph-verified";
            cache.version=versions[cache.root];
            DSL_HIGH_CACHE_TRAFFIC traffic;
            need(LNO_DSL_cache_traffic(attention.batch,attention.heads,attention.length,
                i?attention.rope:attention.rank,cache.logical_sweeps,traffic),"reuse_byte_count_overflow");
            cache.payload_bytes=traffic.payload_bytes;cache.fused_logical_read_bytes=traffic.fused_logical_read_bytes;
            unsigned first=UINT32_MAX,last=0;
            std::map<unsigned,std::vector<const DSL_HIGH_REUSE_ACCESS *> > consumers;
            for(size_t j=0;j<out.accesses.size();++j) if(out.accesses[j].root==cache.root) {
                const DSL_HIGH_REUSE_ACCESS &r=out.accesses[j];cache.reads.push_back(r.id);
                add(cache.source_read_bytes,r.read_bytes);consumers[r.operation].push_back(&r);
                if(r.native.execution_order<first) {first=r.native.execution_order;cache.first_use=r.execution_group;}
                if(r.native.execution_order>=last) {last=r.native.execution_order;cache.last_use=r.execution_group;}
            }
            unsigned head_consumers=0;
            for(std::map<unsigned,std::vector<const DSL_HIGH_REUSE_ACCESS *> >::const_iterator j=consumers.begin();j!=consumers.end();++j) {
                const DSL_HIGH_REUSE_ACCESS &r=*j->second[0];
                if(r.purpose!="materialized-cache-transpose") ++head_consumers;
                if(!r.sliced_reduction) need(j->second.size()==1,"reuse_duplicate_read_coverage");
                else {
                    std::set<uint64_t> slices;
                    for(size_t k=0;k<j->second.size();++k) {
                        const DSL_HIGH_REUSE_ACCESS &s=*j->second[k];
                        need(s.sliced_reduction && s.reduction_extent==r.reduction_extent &&
                            slices.insert(s.reduction_slice).second,"reuse_duplicate_reduction_slice");
                    }
                    need(slices.size()==r.reduction_extent,"reuse_reduction_slice_coverage_missing");
                }
            }
            need(head_consumers==cache.logical_sweeps && consumers.size()==cache.logical_sweeps+1,"reuse_cache_read_census_unproved");
            for(size_t j=0;j<out.storages.size();++j) if(out.storages[j].root==cache.root) {
                DSL_HIGH_REUSE_STORAGE &s=out.storages[j];
                s.first_use=s.value==s.root?"entry":s.producer_group;
                unsigned last_order=0;
                if(s.value!=cache.root) {
                    add(cache.materialized_copy_bytes,s.locality.object_bytes);
                    add(cache.source_write_bytes,s.locality.object_bytes);
                }
                for(size_t k=0;k<out.accesses.size();++k) if(out.accesses[k].storage==s.value) {
                    const DSL_HIGH_REUSE_ACCESS &r=out.accesses[k];
                    uint64_t total=s.locality.estimated_read_bytes;add(total,r.read_bytes);
                    s.locality.estimated_read_bytes=total;++s.locality.use_count;
                    if(r.native.execution_order>=last_order) {
                        last_order=r.native.execution_order;s.last_use=r.execution_group;
                    }
                }
                s.locality.last_use_path_position=last_order;
                if(s.value!=s.root) {
                    const DSL_HIGH_ACCESS &producer=*writes[s.value][0];
                    bool copied=false;
                    for(size_t k=0;k<out.accesses.size();++k) {
                        const DSL_HIGH_REUSE_ACCESS &r=out.accesses[k];
                        if(r.id==producer.copied_from_access && r.root==s.root &&
                            r.operation==s.producer_operation && r.purpose=="materialized-cache-transpose") copied=true;
                    }
                    need(copied,"reuse_transpose_value_flow_unproved");
                    s.locality.producer_path_position=producer.execution_order;
                }
                if(s.value!=cache.root) s.locality.estimated_write_bytes=s.locality.object_bytes;
            }
            out.caches.push_back(cache);
        }
        out.key="owned-mla-reuse-v1;";
        for(size_t i=0;i<out.storages.size();++i) {
            const DSL_HIGH_REUSE_STORAGE &s=out.storages[i];
            field(out.key,number(s.value)+"/"+number(s.root)+"/"+s.native_storage);
            field(out.key,s.version);field(out.key,s.origin_version);field(out.key,s.layout);
            field(out.key,s.first_use);field(out.key,s.last_use);
            for(size_t j=0;j<s.shape.size();++j) field(out.key,number(s.shape[j])+"/"+number(s.strides[j]));
            for(size_t j=0;j<s.alias_values.size();++j) field(out.key,number(s.alias_values[j]));
        }
        for(size_t i=0;i<out.accesses.size();++i) {
            const DSL_HIGH_REUSE_ACCESS &a=out.accesses[i];field(out.key,a.id);field(out.key,a.storage_version);
            field(out.key,a.head_loop);field(out.key,a.purpose);
            field(out.key,number(a.read_bytes)+"/"+number(a.footprint_bytes)+"/"+number(a.head_multiplicity));
            field(out.key,number(a.sliced_reduction)+"/"+number(a.reduction_extent)+"/"+number(a.reduction_slice));
        }
        for(size_t i=0;i<out.caches.size();++i) {
            const DSL_HIGH_REUSE_CACHE &c=out.caches[i];field(out.key,c.role);field(out.key,c.role_provenance);
            field(out.key,number(c.payload_bytes)+"/"+number(c.fused_logical_read_bytes)+"/"+
                number(c.source_read_bytes)+"/"+number(c.source_write_bytes));
        }
        return out;
    }
};
}

/* These are source-level logical requests, never an HBM byte estimate. */
bool LNO_DSL_cache_traffic(uint64_t b,uint64_t h,uint64_t l,uint64_t c,unsigned sweeps,DSL_HIGH_CACHE_TRAFFIC &out) {
    out=DSL_HIGH_CACHE_TRAFFIC();
    if(!b || !h || !l || !c || !sweeps) return false;
    try {
        DSL_HIGH_CACHE_TRAFFIC result;result.payload_bytes=mul(mul(mul(b,l),c),4);
        result.fused_logical_read_bytes=mul(mul(result.payload_bytes,h),sweeps);out=result;return true;
    } catch(const std::exception &) {return false;}
}
/* Reuse policy-free common locality vocabulary without fabricating DSL value IDs. */
DSL_HIGH_REUSE_STORAGE::DSL_HIGH_REUSE_STORAGE() : value(0),root(UINT32_MAX),producer_operation(UINT32_MAX) {
    memset(&locality,0,sizeof(locality));
    locality.reuse_distance_statements=DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    locality.working_set_bytes=DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    locality.estimated_read_bytes=DSL_TENSOR_LOCALITY_UNKNOWN_U64;
    locality.estimated_write_bytes=DSL_TENSOR_LOCALITY_UNKNOWN_U64;
}
/* Atomic publication of detached facts; unsupported forms retain no stale body. */
bool LNO_DSL_analyze_cache_reuse(const std::vector<DSL_HIGH_STORAGE> &s,const std::vector<DSL_HIGH_LOOP> &l,
    const std::vector<DSL_HIGH_ACCESS> &a,const std::vector<DSL_HIGH_ALIAS> &aliases,const VHO_TENSOR_PROGRAM &p,
    DSL_HIGH_REUSE_SUMMARY &out,std::string &reason) {
    out=DSL_HIGH_REUSE_SUMMARY();
    try {out=ANALYSIS(s,l,a,aliases,p).Run();reason="proved";return true;}
    catch(const std::exception &e) {out=DSL_HIGH_REUSE_SUMMARY();reason=e.what();return false;}
}
/* Native owner must validate the snapshot and producer ABI before this call. */
bool LNO_DSL_derive_cache_reuse(const DSL_HIGH_SNAPSHOT &snapshot,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_BINDINGS &bindings,const VHO_TENSOR_PROGRAM &program,DSL_HIGH_REUSE_FACTS &out,std::string &reason) {
    out=DSL_HIGH_REUSE_FACTS();
    if(!snapshot.Is_current(epoch,bindings)) return reason="reuse_stale_snapshot",false;
    if(snapshot.Producer_schema()!="common-high-v1" || snapshot.Buffer_contract()!="disjoint-storage-formals" ||
        program.source_file!=bindings.unit || snapshot.Producer_recipe()!=VHO_tensor_recipe_key(program))
        return reason="reuse_producer_binding_changed",false;
    DSL_HIGH_REUSE_FACTS result;
    if(!LNO_DSL_analyze_cache_reuse(snapshot.Storages(),snapshot.Loops(),snapshot.Accesses(),
        snapshot.Aliases(),program,result._summary,reason)) return false;
    result._stamp=snapshot.Stamp();result._bindings=bindings;result._source=snapshot.Source_key();
    result._facts=snapshot.Facts_key();result._memory=snapshot.Owned_memory_key();
    result._manifest=VHO_tensor_recipe_key(program);result._valid=true;out=result;return true;
}
/* Full context equality is necessary in addition to the owner's live recapture. */
bool DSL_HIGH_REUSE_FACTS::Is_current(const DSL_HIGH_SNAPSHOT &s,const DSL_HIGH_EPOCH &epoch,
    const DSL_HIGH_BINDINGS &b,const VHO_TENSOR_PROGRAM &p) const {
    return Is_live(epoch) && s.Is_current(epoch,b) && _bindings==b && _source==s.Source_key() &&
        _facts==s.Facts_key() && _memory==s.Owned_memory_key() && _manifest==VHO_tensor_recipe_key(p);
}
/* Retained diagnostic data stays readable after analyses die; it is not live authority. */
void DSL_HIGH_REUSE_FACTS::Print(FILE *f,const char *stage,bool live) const {
    if(!f || !_valid) return;
    fprintf(f,"AI_HIGH_REUSE 1 %s live=%s storages=%lu reads=%lu caches=%lu scope=invocation GPU_global_bytes=unknown transactions=unknown HBM_bytes=unknown L2_bytes=unknown SMEM_bytes=unknown spill_bytes=unknown partial_bytes=unknown grouped_materializer=false\n",
        stage,live?"true":"false",(unsigned long)_summary.storages.size(),(unsigned long)_summary.accesses.size(),(unsigned long)_summary.caches.size());
    if(std::string(stage)!="COLLECTED") return;
    fprintf(f,"AI_HIGH_REUSE 1 KEY %s\n",_summary.key.c_str());
    for(size_t i=0;i<_summary.storages.size();++i) {
        const DSL_HIGH_REUSE_STORAGE &s=_summary.storages[i];if(s.root==UINT32_MAX) continue;
        fprintf(f,"AI_HIGH_REUSE 1 STORAGE value=%u native=%s root=%u version=%s origin=%s layout=%s bytes=%llu aliases=%lu producer=%s first=%s last=%s lifetime=%u shape=",
            s.value,s.native_storage.c_str(),s.root,s.version.c_str(),s.origin_version.c_str(),s.layout.c_str(),
            (unsigned long long)s.locality.object_bytes,(unsigned long)s.alias_values.size(),s.producer_group.c_str(),
            s.first_use.c_str(),s.last_use.c_str(),s.locality.lifetime_state);
        for(size_t j=0;j<s.shape.size();++j) fprintf(f,"%s%llu",j?",":"",(unsigned long long)s.shape[j]);
        fprintf(f," strides=");
        for(size_t j=0;j<s.strides.size();++j) fprintf(f,"%s%llu",j?",":"",(unsigned long long)s.strides[j]);
        fprintf(f," alias_values=");
        for(size_t j=0;j<s.alias_values.size();++j) fprintf(f,"%s%u",j?",":"",s.alias_values[j]);
        fprintf(f," live_working_set_bytes=unknown reuse_distance=unknown\n");
    }
    for(size_t i=0;i<_summary.accesses.size();++i) {
        const DSL_HIGH_REUSE_ACCESS &a=_summary.accesses[i];
        fprintf(f,"AI_HIGH_REUSE 1 READ id=%s storage=%u root=%u op=%u operand=%u purpose=%s version=%s head_loop=%s head_invariant=true head_multiplicity=%llu elements=%llu source_bytes=%llu footprint_bytes=%llu sliced=%s reduction=%llu slice=%llu\n",
            a.id.c_str(),a.storage,a.root,a.operation,a.operand,a.purpose.c_str(),a.storage_version.c_str(),
            a.head_loop.empty()?"singleton-or-headless":a.head_loop.c_str(),(unsigned long long)a.head_multiplicity,
            (unsigned long long)a.read_elements,(unsigned long long)a.read_bytes,(unsigned long long)a.footprint_bytes,
            a.sliced_reduction?"true":"false",(unsigned long long)a.reduction_extent,(unsigned long long)a.reduction_slice);
    }
    for(size_t i=0;i<_summary.caches.size();++i) {
        const DSL_HIGH_REUSE_CACHE &c=_summary.caches[i];
        fprintf(f,"AI_HIGH_REUSE 1 CACHE root=%u role=%s provenance=%s version=%s heads=%llu payload_bytes=%llu logical_sweeps=%u fused_logical_request_bytes=%llu source_read_bytes=%llu source_write_bytes=%llu transpose_bytes=%llu first=%s last=%s\n",
            c.root,c.role.c_str(),c.role_provenance.c_str(),c.version.c_str(),(unsigned long long)c.heads,
            (unsigned long long)c.payload_bytes,c.logical_sweeps,(unsigned long long)c.fused_logical_read_bytes,
            (unsigned long long)c.source_read_bytes,(unsigned long long)c.source_write_bytes,
            (unsigned long long)c.materialized_copy_bytes,c.first_use.c_str(),c.last_use.c_str());
    }
}
