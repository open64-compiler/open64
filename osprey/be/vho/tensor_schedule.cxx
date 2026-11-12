/* Copyright (C) 2026 Open64 Project */
/* Detached tensor validation/scheduling reused by VHO tools and LNO's read-only
 * observer. No native compiler headers, contexts or allocation APIs. Design:
 * doc/AI-COMPILER-SEARCH-M2C-OBSERVER-v1.0.md. Planner behavior is moved unchanged. */
#include "tensor_kernel_lower.h"
#include <cctype>
#include <cstdlib>
#include <cerrno>
#include <set>
#include <sstream>
#include <stdexcept>
/* Contractions and reductions are materialization boundaries. Pure indexing
 * and elementwise producers can be recomputed without cross-thread effects. */
BOOL VHO_tensor_is_boundary(const VHO_TENSOR_OPERATION &op) {
    return op.name=="common.matmul" || op.name=="common.reduce_sum" || op.name=="common.reduce_max";
}

namespace {
/* Abort preparation before publishing a recipe or mutating WHIRL. */
void need(bool condition,const std::string &message) {
    if (!condition) throw std::runtime_error(message);
}
/* Parse canonical static shape/permutation numbers without executing tensors. */
std::vector<UINT64> parse_numbers(const std::string &text) {
    std::string s=text;
    if (s.size()>1 && s[0]=='[' && s[s.size()-1]==']') s=s.substr(1,s.size()-2);
    std::vector<UINT64> out; const char *p=s.c_str();
    for (;;) {
        need(std::isdigit((unsigned char)*p),"invalid shape/permutation");
        char *end; errno=0; UINT64 n=strtoull(p,&end,10);
        need(!errno && end!=p,"integer overflow"); out.push_back(n);
        if (!*end) break;
        need(*end==',' && end[1],"invalid integer delimiter"); p=end+1;
    }
    return out;
}
/* Print canonical attribute dimensions; bracket syntax belongs to TY only. */
std::string dimensions(const std::vector<UINT64> &shape) {
    std::ostringstream s;
    for (size_t i=0;i<shape.size();++i) {if(i) s<<",";s<<shape[i];}
    return s.str();
}
/* Persist small numeric facts as canonical metadata without new image rows. */
std::string decimal(UINT64 n) {std::ostringstream s;s<<n;return s.str();}
/* Return a right-aligned numpy broadcast shape, refusing incompatible axes. */
std::vector<UINT64> broadcast(const std::vector<UINT64> &a,const std::vector<UINT64> &b) {
    size_t rank=a.size()>b.size()?a.size():b.size();std::vector<UINT64> out(rank,1);
    for (size_t i=0;i<rank;++i) {
        UINT64 x=i<a.size()?a[a.size()-1-i]:1,y=i<b.size()?b[b.size()-1-i]:1;
        need(x==y || x==1 || y==1,"incompatible broadcast");out[rank-1-i]=x>y?x:y;
    }
    return out;
}
/* Admit source-bound schemas again in a fresh device process, with no TY creation. */
void validate_operation(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_OPERATION &op) {
    const VHO_TENSOR_VALUE &o=p.values[op.output],&a=p.values[op.inputs[0]];
    std::map<std::string,std::string> expected;
    std::vector<UINT64> shape=a.shape;std::string dtype="float32";
    UINT32 version=1,arity=1; bool float_inputs=true;
    if (op.name=="copy") {dtype=a.dtype;float_inputs=false;}
    else if (op.name=="common.reshape") {
        shape=o.shape; dtype=a.dtype;float_inputs=false;
        need(VHO_tensor_elements(a)==VHO_tensor_elements(o),"reshape element count");
        expected["attr.target_shape"]=dimensions(shape);
    } else if (op.name=="common.transpose") {
        need(op.attributes.count("attr.permutation"),"missing permutation");
        std::vector<UINT64> perm=parse_numbers(op.attributes.find("attr.permutation")->second);
        need(perm.size()==a.shape.size(),"permutation rank");std::set<UINT64> axes;
        for(size_t i=0;i<perm.size();++i) {
            need(perm[i]<perm.size() && axes.insert(perm[i]).second,"invalid permutation");shape[i]=a.shape[perm[i]];
        }
        dtype=a.dtype;float_inputs=false;expected["attr.permutation"]=dimensions(perm);
    } else if (op.name=="common.matmul") {
        version=3;arity=2;
        need(op.inputs.size()==2,"matmul arity");const VHO_TENSOR_VALUE &b=p.values[op.inputs[1]];
        need(a.shape.size()>=2 && b.shape.size()>=2 && a.shape.back()==b.shape[b.shape.size()-2],"matmul contraction");
        std::vector<UINT64> x(a.shape.begin(),a.shape.end()-2),y(b.shape.begin(),b.shape.end()-2);
        shape=broadcast(x,y);shape.push_back(a.shape[a.shape.size()-2]);shape.push_back(b.shape.back());
        expected["attr.transpose_kid0"]="false";expected["attr.transpose_kid1"]="false";
        expected["attr.batch_rule"]="numpy";expected["attr.accum_dtype"]="float32";
    } else if (op.name=="common.reduce_sum" || op.name=="common.reduce_max") {
        version=op.name=="common.reduce_sum"?3:1;
        std::ostringstream axis;axis<<a.shape.size()-1;expected["attr.axis"]=axis.str();
        need(op.attributes.count("attr.keepdim"),"missing keepdim");
        std::string keep=op.attributes.find("attr.keepdim")->second;
        need(keep=="true" || keep=="false","invalid keepdim");expected["attr.keepdim"]=keep;
        if(keep=="true") shape.back()=1;else shape.pop_back();
        expected["attr.accum_dtype"]="float32";expected["attr.reassociate"]="false";
    } else if (op.name=="common.fill_like") {
        need(op.attributes.count("attr.value_bits"),"missing fill bits");
        std::string bits=op.attributes.find("attr.value_bits")->second;
        need(bits.size()==8 && bits.find_first_not_of("0123456789abcdef")==std::string::npos,"invalid fill bits");
        UINT32 word=strtoul(bits.c_str(),NULL,16);
        need((word&0x7f800000)!=0x7f800000 || !(word&0x7fffff),"NaN fill is unsupported");
        expected["attr.value_bits"]=bits;
    } else if (op.name=="common.select") {
        arity=3;float_inputs=false;need(op.inputs.size()==3,"select arity");
        need(a.dtype=="bool" && p.values[op.inputs[1]].dtype=="float32" && p.values[op.inputs[2]].dtype=="float32","select types");
        shape=broadcast(broadcast(a.shape,p.values[op.inputs[1]].shape),p.values[op.inputs[2]].shape);
        expected["attr.broadcast_rule"]="numpy";
    } else if(op.name=="common.add" || op.name=="common.sub" || op.name=="common.mul" || op.name=="common.div" || op.name=="common.greater") {
        arity=2;need(op.inputs.size()==2,"binary arity");shape=broadcast(a.shape,p.values[op.inputs[1]].shape);
        expected["attr.broadcast_rule"]="numpy";if(op.name=="common.greater") dtype="bool";
    } else if(op.name=="common.exp" || op.name=="common.log") expected["attr.fp_semantics"]="source_unresolved";
    else if(op.name=="common.isfinite") dtype="bool";
    else if(op.name=="common.logical_not") {float_inputs=false;dtype="bool";need(a.dtype=="bool","logical-not input");}
    else need(false,"unsupported tensor operation: "+op.name);
    need(op.version==version && op.inputs.size()==arity,"unsupported version/arity: "+op.name);
    if(float_inputs) for(size_t i=0;i<op.inputs.size();++i) need(p.values[op.inputs[i]].dtype=="float32","float input required");
    need(shape==o.shape && dtype==o.dtype,"result shape/dtype mismatch: "+op.name);
    need(expected==op.attributes,"unsupported attributes: "+op.name);
}

/* Bound expression expansion before allocating device symbols or WN trees. */
void leaves(const VHO_TENSOR_PROGRAM &p,const std::map<UINT32,UINT32> &producer,
    UINT32 value,std::set<UINT32> &inputs,UINT32 &budget,const VHO_TENSOR_ATTENTION &a) {
    need(budget++<4096,"fusion expression expansion limit");
    if(a.enabled && (value==a.latent || value==a.lse)) {inputs.insert(a.bundle);return;}
    std::map<UINT32,UINT32>::const_iterator found=producer.find(value);
    if(found==producer.end() || VHO_tensor_is_boundary(p.operations[found->second])) {inputs.insert(value);return;}
    const VHO_TENSOR_OPERATION &op=p.operations[found->second];
    if(op.name!="common.fill_like") for(size_t i=0;i<op.inputs.size();++i) leaves(p,producer,op.inputs[i],inputs,budget,a);
}
/* Plan selection consumes the admitted source graph, not names or CUDA text.
 * Chunk ownership gives disjoint writes; the later launch orders all merges. */
VHO_TENSOR_SCHEDULE schedule(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_SCHEDULE plan;plan.bindings=p;plan.attention=VHO_tensor_attention(p);
    if(p.auto_chunks) need(VHO_tensor_select_attention_chunks(p,p.gpu,&plan.chunk_selection,NULL),"automatic source selection failed");
    const VHO_TENSOR_ATTENTION &a=plan.attention;UINT32 chunks=1;
    if(a.enabled) {
        chunks=p.split_width?(a.length+p.split_width-1)/p.split_width:1;
        VHO_TENSOR_VALUE packed;packed.name="attention_latent_lse";packed.dtype="float32";packed.role=0;packed.ordinal=0;
        UINT64 final_shape[]={a.batch,a.heads,a.rank+1};packed.shape.assign(final_shape,final_shape+3);packed.storage=a.bundle;
        need(VHO_tensor_elements(packed)<=16777216,"attention final allocation exceeds limit");plan.bindings.values.push_back(packed);
        if(chunks>1) {
            packed.name="attention_partial_max_sum_latent";UINT64 partial_shape[]={chunks,a.batch,a.heads,a.rank+2};packed.shape.assign(partial_shape,partial_shape+4);packed.storage=a.partial;
            need(VHO_tensor_elements(packed)<=16777216,"attention partial allocation exceeds limit");plan.bindings.values.push_back(packed);
        }
    }
    std::map<UINT32,UINT32> producer;
    for(UINT32 i=0;i<p.operations.size();++i) producer[p.operations[i].output]=i;
    for(UINT32 i=0;i<p.operations.size();++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[i];
        if(a.enabled && i!=1 && i!=33 && i!=37 && i!=46 && i!=47) continue;
        if(a.enabled && i==33) {
            VHO_TENSOR_KERNEL k;k.id=k.root=i;k.output=chunks>1?a.partial:a.bundle;k.stage=chunks>1?1:0;
            k.mapping=3;k.chunks=chunks;k.split_width=chunks>1?p.split_width:a.length;
            k.threads=p.chunk_policy>=3 && p.register_latent && k.stage==1 && (k.split_width==32 || k.split_width==64)?k.split_width:p.threads;
            k.register_statistics=p.chunk_policy>=4 && k.stage==1 && k.threads==32 && k.split_width==32;
            k.warp_owned_partials=p.chunk_policy==6 && p.register_latent && k.stage==1 && k.threads==64 && k.split_width==64;
            k.two_warp_statistics=p.chunk_policy==5 && p.register_latent && k.stage==1 && k.threads==64 && k.split_width==64;
            UINT32 ids[]={a.q,a.qr,a.cache,a.rope_cache,a.mask};k.inputs.assign(ids,ids+5);k.elements=VHO_tensor_elements(plan.bindings.values[k.output]);
            plan.kernels.push_back(k);
            if(chunks>1) {
                k.id=i+4096;k.mapping=4;k.stage=2;k.threads=p.threads;k.register_statistics=false;k.two_warp_statistics=false;k.warp_owned_partials=false;k.output=a.bundle;k.inputs.clear();k.inputs.push_back(a.partial);
                k.elements=VHO_tensor_elements(plan.bindings.values[k.output]);plan.kernels.push_back(k);
            }
            continue;
        }
        if(p.fuse ? (!VHO_tensor_is_boundary(op) && p.values[op.output].role!=2) : op.name=="common.reshape") continue;
        VHO_TENSOR_KERNEL k;k.threads=p.threads;k.id=k.root=i;k.output=op.output;k.stage=0;k.split_width=0;k.chunks=1;k.mapping=0;
        k.elements=VHO_tensor_elements(p.values[op.output]);
        if(p.fuse) {
            std::set<UINT32> inputs;UINT32 budget=0;
            for(size_t a=0;a<op.inputs.size();++a) leaves(p,producer,op.inputs[a],inputs,budget,plan.attention);
            /* Pin unused entry inputs on the first launch so producer inlining
             * cannot silently change the source program's packed input ABI. */
            if(plan.kernels.empty()) for(UINT32 v=0;v<p.values.size();++v) if(p.values[v].role==1) inputs.insert(v);
            k.inputs.assign(inputs.begin(),inputs.end());
        } else k.inputs=op.inputs;
        UINT64 width=VHO_tensor_is_boundary(op)?p.values[op.inputs[0]].shape.back():0;
        if(op.name=="common.matmul") width=p.values[op.inputs[0]].shape.back();
        if(p.cooperative && VHO_tensor_is_boundary(op)) k.mapping=op.name=="common.matmul"?(width>=32?2:0):1;
        if(!p.fuse_attention && p.split_width && width>p.split_width) {
            k.split_width=p.split_width;k.chunks=(width+p.split_width-1)/p.split_width;k.stage=1;
            need(k.elements<=16777216/k.chunks,"split scratch exceeds allocation limit");
            VHO_TENSOR_VALUE scratch=p.values[op.output];scratch.name="split_"+decimal(i);
            scratch.shape.clear();scratch.shape.push_back(k.chunks);scratch.shape.push_back(k.elements);
            scratch.role=0;scratch.ordinal=0;scratch.storage=plan.bindings.values.size();
            k.output=scratch.storage;k.elements*=k.chunks;plan.bindings.values.push_back(scratch);plan.kernels.push_back(k);
            k.id=i+4096;k.stage=2;k.inputs.clear();k.inputs.push_back(k.output);k.output=op.output;
            if(k.mapping) k.mapping=1;
            k.elements=VHO_tensor_elements(p.values[op.output]);
        }
        plan.kernels.push_back(k);
    }
    need(!plan.kernels.empty(),"no executable kernels");return plan;
}
}

#include "tensor_attention_pattern.inc"

/* Extents are bounded by validation, so the flat logical size cannot overflow. */
UINT64 VHO_tensor_elements(const VHO_TENSOR_VALUE &value) {
    UINT64 size=1;for(size_t i=0;i<value.shape.size();++i) size*=value.shape[i];return size;
}

/* Four complete warps per CTA; staged dots keep four adjacent columns together. */
UINT64 VHO_tensor_grid(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_KERNEL &k) {
    if(k.mapping>=3) {VHO_TENSOR_ATTENTION a=VHO_tensor_attention(p);return a.batch*a.heads*(k.mapping==3?k.chunks:1);}
    if(k.mapping==1) return (k.elements+3)/4;
    if(k.mapping==2) {
        const VHO_TENSOR_VALUE &out=p.values[p.operations[k.root].output];UINT64 n=out.shape.back();
        return (VHO_tensor_elements(out)/n)*((n+3)/4)*(k.stage==1?k.chunks:1);
    }
    return p.opt_level==3?(k.elements+p.threads-1)/p.threads:1;
}

/* Shared storage is selected explicitly and checked against mapped metadata. */
UINT64 VHO_tensor_shared_bytes(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_KERNEL &k) {
    if(k.warp_owned_partials) return 8*(VHO_tensor_attention(p).rank+2);
    if(k.register_statistics) return 0;
    if(k.two_warp_statistics) return 4*(64+4);
    if(k.mapping<3) return k.mapping==2?640:0;
    UINT64 count=k.mapping==3?k.split_width:k.chunks;
    UINT64 quantum=k.mapping==3?k.threads:128;
    return 4*(((count+quantum-1)/quantum)*quantum+8+(k.mapping==3 && !p.register_latent?128:0));
}

/* Recheck the detached handoff before allocating any device table entries. */
BOOL VHO_tensor_validate_program(const VHO_TENSOR_PROGRAM &p,FILE *diagnostic) {
    try {
        need(p.opt_level<=3 && p.threads==(p.opt_level==3?128:1),"invalid level/thread mapping");
        need((!p.fuse && !p.split_width) || p.opt_level==3,"optimization requires O3");
        need(!p.register_latent || p.fuse_attention,"register accumulation requires fused attention");
        need(!p.cooperative || (p.fuse && p.opt_level==3),"cooperative mapping requires fused O3");
        need(!p.split_width || (p.fuse && p.split_width>=2 && p.split_width<=4096),"invalid split width/fusion policy");
        need(!p.search_selected || (!p.auto_chunks && p.register_latent && p.cooperative && p.opt_level==3 &&
            !p.search_evidence.empty() && !p.search_identity.empty() && (p.chunk_policy==3 || p.chunk_policy==5 || p.chunk_policy==6)),"invalid fixed MLA selection");
        need(p.operations.size()<=4096,"too many operations");
        need(!p.source_pu.empty() && !p.source_file.empty() && !p.values.empty() && !p.operations.empty(),"empty tensor program/source mapping");
        std::set<UINT32> defined;UINT32 inputs=0,outputs=0;
        for(UINT32 i=0;i<p.values.size();++i) {
            const VHO_TENSOR_VALUE &v=p.values[i];need(v.dtype=="float32" || v.dtype=="bool","unsupported dtype");
            need(!v.shape.empty() && v.shape.size()<=16,"unsupported rank");UINT64 size=1;
            for(size_t j=0;j<v.shape.size();++j) {need(v.shape[j] && v.shape[j]<=16777216/size,"invalid/large extent");size*=v.shape[j];}
            need(v.role<=2 && v.storage<p.values.size(),"invalid role/storage");
            if(v.role) need(v.storage==i,"external value cannot alias a temporary");
            if(v.role==1) {need(v.ordinal==inputs++,"input ordinal");defined.insert(i);}
            if(v.role==2) need(v.ordinal==outputs++,"output ordinal");
        }
        need(inputs && outputs,"missing program interface");
        for(size_t i=0;i<p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=p.operations[i];need(op.output<p.values.size() && !defined.count(op.output),"redefinition/invalid output");
            need(op.source_line<=2147483647U,"invalid source line");
            need(!op.inputs.empty() && op.inputs.size()<=3,"invalid operand count");
            for(size_t j=0;j<op.inputs.size();++j) need(op.inputs[j]<p.values.size() && defined.count(op.inputs[j]) && p.values[op.inputs[j]].role!=2,"invalid operand use");
            validate_operation(p,op);const VHO_TENSOR_VALUE &out=p.values[op.output];
            need(out.storage==(op.name=="common.reshape"?p.values[op.inputs[0]].storage:op.output),"incorrect storage alias");defined.insert(op.output);
        }
        need(defined.size()==p.values.size(),"unused/undefined recipe values");
        if(p.fuse_attention) VHO_tensor_attention(p);
        if(p.auto_chunks) {
            need(p.register_latent,"automatic chunks require register attention");
            VHO_TENSOR_CHUNK_SELECTION choice;
            need(VHO_tensor_select_attention_chunks(p,p.gpu,&choice,diagnostic) && choice.width==p.split_width,"stale automatic chunk selection");
        }
        return TRUE;
    } catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor lowering: %s\n",e.what());return FALSE;}
}

/* Copy the selected plan only after all source and expansion checks succeed. */
BOOL VHO_tensor_schedule(const VHO_TENSOR_PROGRAM &p,VHO_TENSOR_SCHEDULE *out,FILE *diagnostic) {
    if(!out || !VHO_tensor_validate_program(p,diagnostic)) return FALSE;
    try {*out=schedule(p);return TRUE;}
    catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor schedule: %s\n",e.what());return FALSE;}
}

/* Frame every source recipe fact rather than relying on a lossy checksum or
 * object member order. The native High producer and observer share this key. */
std::string VHO_tensor_recipe_key(const VHO_TENSOR_PROGRAM &p) {
    std::ostringstream out;
    out << "tensor-semantic-v1/" << p.opt_level << '/' << p.threads << '/'
        << p.fuse << '/' << p.cooperative << '/' << p.split_width << '/' << p.fuse_attention << '/'
        << p.register_latent << '/' << p.auto_chunks << '/' << p.search_selected << '/' << p.chunk_policy << '/';
    out << p.source_pu.size() << ':' << p.source_pu << p.source_file.size() << ':' << p.source_file;
    out << "/values=" << p.values.size();
    for (size_t i = 0; i < p.values.size(); ++i) {
        const VHO_TENSOR_VALUE &v = p.values[i];
        out << '/' << v.name.size() << ':' << v.name << '/' << v.dtype.size() << ':' << v.dtype
            << '/' << v.role << '/' << v.ordinal << '/' << v.storage << '/' << v.shape.size();
        for (size_t j = 0; j < v.shape.size(); ++j) out << '/' << v.shape[j];
    }
    out << "/operations=" << p.operations.size();
    for (size_t i = 0; i < p.operations.size(); ++i) {
        const VHO_TENSOR_OPERATION &op = p.operations[i];
        out << '/' << op.name.size() << ':' << op.name << '/' << op.version << '/' << op.source_node << '/'
            << op.source_line << '/' << op.output << '/' << op.inputs.size();
        for (size_t j = 0; j < op.inputs.size(); ++j) out << '/' << op.inputs[j];
        out << '/' << op.attributes.size();
        for (std::map<std::string,std::string>::const_iterator j = op.attributes.begin(); j != op.attributes.end(); ++j)
            out << '/' << j->first.size() << ':' << j->first << j->second.size() << ':' << j->second;
    }
    return out.str();
}
