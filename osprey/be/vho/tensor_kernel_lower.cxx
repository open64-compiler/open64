/* Copyright (C) 2026 Open64 Project */
/* Ordinary loop/array WHIRL for native tensor expressions, owned by VHO.
 * Design: doc/AI-COMPILER-TENSOR-KERNEL-LOWERING-v0.2.md. Source admission
 * is read-only and PU-local; materialization mutates only an active empty
 * device PU. No model templates, mapped-layout changes or emitter decisions. */
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <set>
#include <sstream>
#include <stdexcept>
#include "tensor_kernel_lower.h"
#include "wn.h"
#include "wn_util.h"
#include "stab.h"
#include "pu_info.h"
#include "dsl_ir_image.h"
#include "dsl_gatekeeper.h"
#include "config.h"
#include "config_opt.h"
#include "ir_reader.h"
#include "targ_const.h"

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
/* Hash executable scalar structure, rejecting shared trees and native work.
 * This detects stale/mutated products; it is not a security signature. */
void fingerprint_node(const WN *n,UINT64 &hash,std::set<const WN *> &seen) {
    need(n && seen.insert(n).second && !DSL_WN_Is_Native(n),"invalid/shared/native device tree");
    std::vector<UINT64> words;OPCODE opc=WN_opcode(n);OPERATOR op=WN_operator(n);
    words.push_back(opc);words.push_back(WN_kid_count(n));
    if(OPCODE_has_sym(opc)) {
        const ST *s=WN_st(n);words.push_back(ST_st_idx(s));words.push_back(ST_class(s));words.push_back(ST_sclass(s));
        words.push_back(ST_device_symbol_kind(*s));
        if(ST_class(s)==CLASS_VAR) {
            TY_IDX ty=ST_type(s);words.push_back(ty);words.push_back(TY_kind(ty));words.push_back(TY_mtype(ty));words.push_back(TY_size(ty));
            words.push_back(TY_align(ty));words.push_back(TY_flags(ty));words.push_back(ST_is_value_parm(s));
            if(TY_kind(ty)==KIND_POINTER) {TY_IDX pointed=TY_pointed(ty);words.push_back(pointed);words.push_back(TY_mtype(pointed));words.push_back(TY_size(pointed));words.push_back(TY_flags(pointed));}
            if(TY_kind(ty)==KIND_STRUCT) {
                for(FLD_HANDLE f=TY_fld(ty);!f.Is_Null();f=FLD_next(f)) {
                    words.push_back(FLD_type(f));words.push_back(FLD_ofst(f));words.push_back(FLD_last_field(f));
                    const unsigned char *name=(const unsigned char *)FLD_name(f);while(*name) words.push_back(*name++);words.push_back(0);
                    if(FLD_last_field(f)) break;
                }
            }
        }
    }
    if(OPCODE_has_offset(opc)) words.push_back(WN_offset(n));
    if(OPCODE_has_1ty(opc) || OPCODE_has_2ty(opc)) words.push_back(WN_ty(n));
    if(OPCODE_has_2ty(opc)) words.push_back(WN_load_addr_ty(n));
    if(op==OPR_INTCONST) words.push_back(WN_const_val(n));
    if(op==OPR_CONST) {float f=(float)Targ_To_Host_Float(Tcon_Table[ST_tcon(WN_st(n))]);UINT32 bits;memcpy(&bits,&f,4);words.push_back(bits);}
    if(op==OPR_ARRAY) words.push_back(WN_element_size(n));
    if(op==OPR_LDID || op==OPR_STID || op==OPR_ILOAD || op==OPR_ISTORE) words.push_back(WN_field_id(n));
    if(op==OPR_PARM) words.push_back(WN_parm_flag(n));
    if(op==OPR_INTRINSIC_OP) words.push_back(WN_intrinsic(n));
    need(op!=OPR_CALL && op!=OPR_INTRINSIC_CALL && op!=OPR_ASM_STMT,"unexpected device effect");
    for(size_t i=0;i<words.size();++i) for(UINT32 b=0;b<8;++b) hash=(hash^((words[i]>>(8*b))&255))*UINT64(1099511628211ULL);
    if(op==OPR_BLOCK) for(WN *s=WN_first(n);s;s=WN_next(s)) fingerprint_node(s,hash,seen);
    else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) fingerprint_node(WN_kid(n,i),hash,seen);
}
/* Device identity excludes addresses and mutable compiler-process state. */
std::string fingerprint(WN *entry) {
    UINT64 hash=1469598103934665603ULL;std::set<const WN *> seen;fingerprint_node(entry,hash,seen);return decimal(hash);
}
/* Attach complete host binding facts from the canonical dense allocation. */
void bind_argument(ST_IDX st,UINT32 argument,const VHO_TENSOR_PROGRAM &p,UINT32 value) {
    std::string prefix="tensor.argument."+decimal(argument)+".";
    const VHO_TENSOR_VALUE &v=p.values[value],&storage=p.values[v.storage];
    ST_tensor_bind_metadata(st,(prefix+"value").c_str(),decimal(value).c_str());
    ST_tensor_bind_metadata(st,(prefix+"storage").c_str(),decimal(v.storage).c_str());
    ST_tensor_bind_metadata(st,(prefix+"role").c_str(),decimal(storage.role).c_str());
    ST_tensor_bind_metadata(st,(prefix+"ordinal").c_str(),decimal(storage.ordinal).c_str());
    ST_tensor_bind_metadata(st,(prefix+"elements").c_str(),decimal(VHO_tensor_elements(storage)).c_str());
    ST_tensor_bind_metadata(st,(prefix+"bytes").c_str(),decimal(VHO_tensor_elements(storage)*(storage.dtype=="bool"?1:4)).c_str());
    ST_tensor_bind_metadata(st,(prefix+"dtype").c_str(),storage.dtype.c_str());
    ST_tensor_bind_metadata(st,(prefix+"name").c_str(),storage.name.c_str());
}
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

class VHO_TENSOR_CAPTURE {
    PU_Info *_pu;
    VHO_TENSOR_PROGRAM _program;
    std::map<ST_IDX,UINT32> _symbols;
    std::set<UINT32> _defined;
    bool _returned;
public:
    /* Borrow active source scope; capture never creates WN/ST/TY rows. */
    VHO_TENSOR_CAPTURE(PU_Info *pu,UINT32 level):_pu(pu),_returned(false) {
        _program.opt_level=level;_program.threads=level==3?128:1;
        _program.source_pu=ST_name(PU_Info_proc_sym(pu));
    }
    /* Copy complete scalar descriptor facts, detaching the later device scope. */
    UINT32 Value(ST_IDX st,UINT32 role,UINT32 ordinal) {
        need(!_symbols.count(st),"duplicate tensor symbol");TY_IDX ty=ST_type(&St_Table[st]);
        need(TY_is_tensor_extension(ty),"non-tensor value");VHO_TENSOR_VALUE value;
        const TY_TENSOR_SCHEMA_KEY supported[]={TY_TENSOR_SCHEMA_KIND,TY_TENSOR_SCHEMA_DTYPE,TY_TENSOR_SCHEMA_RANK,
            TY_TENSOR_SCHEMA_SHAPE,TY_TENSOR_SCHEMA_LAYOUT,TY_TENSOR_SCHEMA_NO_ALIAS,TY_TENSOR_SCHEMA_CANONICAL,
            TY_TENSOR_SCHEMA_LINEAGE,TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME,TY_TENSOR_SCHEMA_LOWERING_HINT};
        for(UINT32 i=0;i<TY_tensor_attribute_count(ty);++i) {
            const char *key=NULL,*text=NULL;TY_DSL_BIND_STATE state;
            need(TY_tensor_attribute_at(ty,i,&key,&text,&state),"unreadable tensor descriptor field");bool known=false;
            for(UINT32 k=0;k<sizeof(supported)/sizeof(supported[0]);++k) if(!strcmp(key,TY_tensor_schema_key_name(supported[k]))) known=true;
            need(known,"unsupported tensor trait/representation field: "+std::string(key));
        }
        const char *kind=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_KIND);need(kind && !strcmp(kind,"tensor"),"unsupported tensor kind");
        const char *layout=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_LAYOUT);
        need(layout && (!strcmp(layout,"logical") || !strcmp(layout,"row_major")) &&
            !TY_tensor_attribute_is_bound(ty,"strides"),"unsupported physical tensor layout/strides");
        const char *dtype=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_DTYPE),*shape=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_SHAPE);
        need(dtype && shape,"incomplete tensor descriptor");value.dtype=dtype;value.shape=parse_numbers(shape);
        value.name=ST_name(st);value.role=role;value.ordinal=ordinal;value.storage=_program.values.size();
        UINT32 id=value.storage;_program.values.push_back(value);_symbols[st]=id;return id;
    }
    /* Admit only whole already-defined tensor operands, including bool masks. */
    UINT32 Operand(WN *wn) {
        need(WN_operator(wn)==OPR_LDID && !WN_offset(wn),"expected whole tensor operand");
        need(_symbols.count(WN_st_idx(wn)),"unknown operand");UINT32 id=_symbols[WN_st_idx(wn)];
        need(_defined.count(id) && _program.values[id].role!=2,"undefined operand/output read");return id;
    }
    /* Traverse source-order pure BLOCKs and retain every operation/result. */
    void Block(WN *block) {
        need(WN_operator(block)==OPR_BLOCK,"expected source BLOCK");
        for(WN *stmt=WN_first(block);stmt;stmt=WN_next(stmt)) {
            need(!_returned,"statement after RETURN");OPERATOR kind=WN_operator(stmt);
            if(kind==OPR_BLOCK) {Block(stmt);continue;}
            if(kind==OPR_RETURN) {_returned=true;continue;}
            need(kind==OPR_STID && !WN_offset(stmt),"unsupported source control/effect");
            ST_IDX st=WN_st_idx(stmt);UINT32 output=_symbols.count(st)?_symbols[st]:Value(st,0,0);
            need(!_defined.count(output) && _program.values[output].role!=1,"input mutation/redefinition");
            SRCPOS position=WN_Get_Linenum(stmt);
            VHO_TENSOR_OPERATION op;op.output=output;op.source_node=0;op.version=1;op.source_line=SRCPOS_linenum(position);
            if(_program.source_file.empty() && SRCPOS_filenum(position)) {
                const char *file=NULL,*directory=NULL;IR_Srcpos_Filename(position,&file,&directory);
                if(file) _program.source_file=file[0]=='/'?file:std::string(directory?directory:".")+"/"+file;
            }
            WN *rhs=WN_kid0(stmt);
            if(WN_operator(rhs)==OPR_LDID) {op.name="copy";op.inputs.push_back(Operand(rhs));}
            else {
                need(DSL_WN_Is_Native(rhs),"unsupported source expression");DSL_IR_VALUE_RECORD value;DSL_IR_NODE_RECORD node;DSL_IR_OPCODE_DESCRIPTOR_RECORD desc;
                need(DSL_IR_Image_Find_Definition_Value(_pu,stmt,&value) && DSL_IR_Image_Get_Node(value.producer_node_id,&node) &&
                    DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,&desc),"unresolved native definition");
                op.name=Index_To_Str(desc.stable_name);op.version=desc.version;op.source_node=node.id;
                for(INT i=0;i<WN_kid_count(rhs);++i) op.inputs.push_back(Operand(WN_kid(rhs,i)));
                for(UINT32 i=0;i<node.attribute_count;++i) {
                    DSL_IR_ATTRIBUTE_RECORD attr;need(DSL_IR_Image_Get_Attribute(node.first_attribute_id+i,&attr),"missing attribute");
                    need(op.attributes.insert(std::make_pair(Index_To_Str(attr.name),Index_To_Str(attr.value))).second,"duplicate attribute");
                }
            }
            if(op.name=="common.reshape") _program.values[output].storage=_program.values[op.inputs[0]].storage;
            _program.operations.push_back(op);_defined.insert(output);
        }
    }
    /* Return a complete admitted recipe only after the source final RETURN. */
    VHO_TENSOR_PROGRAM Capture() {
        WN *entry=PU_Info_tree_ptr(_pu);need(WN_operator(entry)==OPR_FUNC_ENTRY,"expected source entry");
        UINT32 inputs=0,outputs=0;
        for(INT i=0;i<WN_num_formals(entry);++i) {
            ST_IDX st=WN_st_idx(WN_formal(entry,i));const char *role=ST_tensor_metadata(st,"dsl.pu.result_role");
            need(!role || !strcmp(role,"tensor"),"state interface unsupported");
            UINT32 id=Value(st,role?2:1,role?outputs++:inputs++);if(!role) _defined.insert(id);
        }
        need(inputs && outputs,"missing source tensor interface");Block(WN_func_body(entry));need(_returned,"missing source RETURN");
        need(VHO_tensor_validate_program(_program,NULL),"invalid captured tensor recipe");return _program;
    }
};

/* Build one fresh integer expression; all generated trees have single owners. */
WN *integer(UINT64 n) {return WN_Intconst(MTYPE_U8,n);}
/* Load an ordinary scalar symbol in the driver's active device scope. */
WN *load(ST *s) {return WN_Ldid(TY_mtype(ST_type(s)),0,s,ST_type(s));}
/* Transfer ownership of an expression into one scalar assignment. */
WN *store(ST *s,WN *x) {return WN_Stid(TY_mtype(ST_type(s)),0,s,ST_type(s),x);}
/* Preserve binary32 literals, including infinity, through normal TCON creation. */
WN *floating(UINT32 bits) {float f;memcpy(&f,&bits,4);return WN_Floatconst(MTYPE_F4,f);}
/* Create a local with explicit source provenance; no symbols escape the PU. */
ST *local(const char *name,TYPE_ID mtype,SRCPOS source) {
    ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,MTYPE_To_TY(mtype));Set_ST_Srcpos(*s,source);return s;
}
/* Right-aligned broadcast coordinates are scalar integer WHIRL expressions. */
WN *broadcast_index(const VHO_TENSOR_VALUE &a,const VHO_TENSOR_VALUE &o,WN *flat,UINT32 trailing) {
    WN *index=integer(0);UINT64 in_stride=1,out_stride=1;
    for(INT axis=(INT)o.shape.size()-1-(INT)trailing;axis>=0;--axis) {
        INT other=axis-((INT)o.shape.size()-(INT)a.shape.size());
        if(other>=0) {
            if(a.shape[other]!=1) {
                WN *coord=WN_Binary(OPR_REM,MTYPE_U8,WN_Div(MTYPE_U8,WN_COPY_Tree(flat),integer(out_stride)),integer(o.shape[axis]));
                index=WN_Add(MTYPE_U8,index,WN_Mpy(MTYPE_U8,coord,integer(in_stride)));
            }
            in_stride*=a.shape[other];
        }
        out_stride*=o.shape[axis];
    }
    WN_DELETE_Tree(flat);return index;
}

/* Contractions and reductions are materialization boundaries. Pure indexing
 * and elementwise producers can be recomputed without cross-thread effects. */
bool boundary(const VHO_TENSOR_OPERATION &op) {
    return op.name=="common.matmul" || op.name=="common.reduce_sum" || op.name=="common.reduce_max";
}
/* Bound expression expansion before allocating device symbols or WN trees. */
void leaves(const VHO_TENSOR_PROGRAM &p,const std::map<UINT32,UINT32> &producer,
    UINT32 value,std::set<UINT32> &inputs,UINT32 &budget) {
    need(budget++<4096,"fusion expression expansion limit");
    std::map<UINT32,UINT32>::const_iterator found=producer.find(value);
    if(found==producer.end() || boundary(p.operations[found->second])) {inputs.insert(value);return;}
    const VHO_TENSOR_OPERATION &op=p.operations[found->second];
    if(op.name!="common.fill_like") for(size_t i=0;i<op.inputs.size();++i) leaves(p,producer,op.inputs[i],inputs,budget);
}
/* Plan selection consumes the admitted source graph, not names or CUDA text.
 * Chunk ownership gives disjoint writes; the later launch orders all merges. */
VHO_TENSOR_SCHEDULE schedule(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_SCHEDULE plan;plan.bindings=p;std::map<UINT32,UINT32> producer;
    for(UINT32 i=0;i<p.operations.size();++i) producer[p.operations[i].output]=i;
    for(UINT32 i=0;i<p.operations.size();++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[i];
        if(p.fuse ? (!boundary(op) && p.values[op.output].role!=2) : op.name=="common.reshape") continue;
        VHO_TENSOR_KERNEL k;k.id=k.root=i;k.output=op.output;k.stage=0;k.split_width=0;k.chunks=1;
        k.elements=VHO_tensor_elements(p.values[op.output]);
        if(p.fuse) {
            std::set<UINT32> inputs;UINT32 budget=0;
            for(size_t a=0;a<op.inputs.size();++a) leaves(p,producer,op.inputs[a],inputs,budget);
            /* Pin unused entry inputs on the first launch so producer inlining
             * cannot silently change the source program's packed input ABI. */
            if(plan.kernels.empty()) for(UINT32 v=0;v<p.values.size();++v) if(p.values[v].role==1) inputs.insert(v);
            k.inputs.assign(inputs.begin(),inputs.end());
        } else k.inputs=op.inputs;
        UINT64 width=boundary(op)?p.values[op.inputs[0]].shape.back():0;
        if(op.name=="common.matmul") width=p.values[op.inputs[0]].shape.back();
        if(p.split_width && width>p.split_width) {
            k.split_width=p.split_width;k.chunks=(width+p.split_width-1)/p.split_width;k.stage=1;
            need(k.elements<=16777216/k.chunks,"split scratch exceeds allocation limit");
            VHO_TENSOR_VALUE scratch=p.values[op.output];scratch.name="split_"+decimal(i);
            scratch.shape.clear();scratch.shape.push_back(k.chunks);scratch.shape.push_back(k.elements);
            scratch.role=0;scratch.ordinal=0;scratch.storage=plan.bindings.values.size();
            k.output=scratch.storage;k.elements*=k.chunks;plan.bindings.values.push_back(scratch);plan.kernels.push_back(k);
            k.id=i+4096;k.stage=2;k.inputs.clear();k.inputs.push_back(k.output);k.output=op.output;
            k.elements=VHO_tensor_elements(p.values[op.output]);
        }
        plan.kernels.push_back(k);
    }
    need(!plan.kernels.empty(),"no executable kernels");return plan;
}
/* Resolve an explicit driver-selected plan entry without switching PU scope. */
VHO_TENSOR_KERNEL selected(const VHO_TENSOR_SCHEDULE &plan,UINT32 id) {
    for(size_t i=0;i<plan.kernels.size();++i) if(plan.kernels[i].id==id) return plan.kernels[i];
    need(false,"kernel absent from selected schedule");return VHO_TENSOR_KERNEL();
}

class VHO_TENSOR_DEVICE {
    const VHO_TENSOR_PROGRAM &_program;
    const VHO_TENSOR_OPERATION &_operation;
    SRCPOS _source;
    VHO_TENSOR_KERNEL _kernel;
    std::map<UINT32,UINT32> _producer;
    std::vector<ST *> _formals;
    ST *_flat,*_physical,*_part,*_reduction,*_sum,*_next;
public:
    /* Bind only detached numeric facts and the active kernel's source position. */
    VHO_TENSOR_DEVICE(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_KERNEL &kernel,SRCPOS source):
        _program(p),_operation(p.operations[kernel.root]),_source(source),_kernel(kernel) {
        for(UINT32 i=0;i<p.operations.size();++i) _producer[p.operations[i].output]=i;
    }
    /* Return output facts; the preflight guarantees the table index is valid. */
    const VHO_TENSOR_VALUE &Output() const {return _program.values[_operation.output];}
    /* Return one input's facts without consulting the preserved source PU. */
    const VHO_TENSOR_VALUE &Input(UINT32 i) const {return _program.values[_operation.inputs[i]];}
    /* Construct an explicit 1-D OPR_ARRAY address with byte-sized bool storage. */
    WN *Address(UINT32 operand,WN *index) {
        const VHO_TENSOR_VALUE &v=_program.values[operand==_kernel.inputs.size()?_kernel.output:_kernel.inputs[operand]];
        WN *a=WN_Create(OPR_ARRAY,Pointer_Mtype,MTYPE_V,3);WN_element_size(a)=v.dtype=="bool"?1:4;
        WN_kid0(a)=load(_formals[operand]);WN_kid1(a)=integer(VHO_tensor_elements(v));WN_kid2(a)=index;return a;
    }
    /* Read from the explicit source element; promotion follows standard WHIRL. */
    WN *Read(UINT32 operand,WN *index) {
        return Evaluate(_operation.inputs[operand],index);
    }
    /* Inline only the pure indexing/elementwise region admitted by the plan.
     * Each returned expression is fresh; no WN subtree gains two parents. */
    WN *Evaluate(UINT32 value,WN *index) {
        for(UINT32 i=0;i<_kernel.inputs.size();++i) if(_kernel.inputs[i]==value) {
            TYPE_ID type=_program.values[value].dtype=="bool"?MTYPE_U1:MTYPE_F4;
            return WN_Iload(type,0,MTYPE_To_TY(type),Address(i,index));
        }
        need(_program.fuse && _producer.count(value),"unbound fused operand");
        const VHO_TENSOR_OPERATION &op=_program.operations[_producer[value]];
        const VHO_TENSOR_VALUE &out=_program.values[value];
        need(!boundary(op),"fusion crossed materialization boundary");
        if(op.name=="copy" || op.name=="common.reshape") return Evaluate(op.inputs[0],index);
        if(op.name=="common.fill_like") {WN_DELETE_Tree(index);return floating(strtoul(op.attributes.find("attr.value_bits")->second.c_str(),NULL,16));}
        if(op.name=="common.transpose") {
            std::vector<UINT64> perm=parse_numbers(op.attributes.find("attr.permutation")->second);
            const VHO_TENSOR_VALUE &in=_program.values[op.inputs[0]];WN *offset=integer(0);UINT64 stride=1;
            for(INT axis=(INT)perm.size()-1;axis>=0;--axis) {
                UINT64 source_stride=1;for(size_t j=perm[axis]+1;j<in.shape.size();++j) source_stride*=in.shape[j];
                WN *coord=WN_Binary(OPR_REM,MTYPE_U8,WN_Div(MTYPE_U8,WN_COPY_Tree(index),integer(stride)),integer(out.shape[axis]));
                offset=WN_Add(MTYPE_U8,offset,WN_Mpy(MTYPE_U8,coord,integer(source_stride)));stride*=out.shape[axis];
            }
            WN_DELETE_Tree(index);return Evaluate(op.inputs[0],offset);
        }
        std::vector<WN *> args;
        for(UINT32 i=0;i<op.inputs.size();++i) args.push_back(Evaluate(op.inputs[i],broadcast_index(_program.values[op.inputs[i]],out,WN_COPY_Tree(index),0)));
        WN_DELETE_Tree(index);const std::string &name=op.name;
        if(name=="common.exp" || name=="common.log") {
            WN *arg=WN_CreateParm(MTYPE_F4,args[0],MTYPE_To_TY(MTYPE_F4),WN_PARM_BY_VALUE);
            return WN_Create_Intrinsic(OPR_INTRINSIC_OP,MTYPE_F4,MTYPE_V,name=="common.exp"?INTRN_F4EXP:INTRN_F4LOG,1,&arg);
        }
        if(name=="common.isfinite") return WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,args[0]),floating(0x7f7fffff));
        if(name=="common.logical_not") return WN_EQ(MTYPE_U4,args[0],WN_Intconst(MTYPE_U4,0));
        if(name=="common.select") return WN_Select(MTYPE_F4,args[0],args[1],args[2]);
        if(name=="common.greater") return WN_GT(MTYPE_F4,args[0],args[1]);
        if(name=="common.add") return WN_Add(MTYPE_F4,args[0],args[1]);
        if(name=="common.sub") return WN_Sub(MTYPE_F4,args[0],args[1]);
        if(name=="common.mul") return WN_Mpy(MTYPE_F4,args[0],args[1]);
        need(name=="common.div","unsupported fused operation");return WN_Div(MTYPE_F4,args[0],args[1]);
    }
    /* Broadcast one input to the selected output element. */
    WN *Element(UINT32 operand) {return Read(operand,broadcast_index(Input(operand),Output(),load(_flat),0));}
    /* A canonical integer counted loop retains serial accumulation order. */
    WN *Loop(ST *iv,UINT64 extent,WN *body) {
        WN *begin=integer(0),*end=integer(extent);
        if(iv==_reduction && _kernel.stage==1) {
            WN_DELETE_Tree(begin);WN_DELETE_Tree(end);
            begin=WN_Mpy(MTYPE_U8,load(_part),integer(_kernel.split_width));
            WN *limit=WN_Add(MTYPE_U8,WN_COPY_Tree(begin),integer(_kernel.split_width));
            end=WN_Select(MTYPE_U8,WN_LT(MTYPE_U8,WN_COPY_Tree(limit),integer(extent)),limit,integer(extent));
        }
        return WN_CreateDO(WN_CreateIdname(0,iv),store(iv,begin),WN_LT(MTYPE_U8,load(iv),end),
            store(iv,WN_Add(MTYPE_U8,load(iv),integer(1))),body,NULL);
    }
    /* Materialize one scalar result, with explicit contraction/reduction loops. */
    WN *Computation(WN *body) {
        const std::string &name=_operation.name;
        if(_kernel.stage==2) {
            bool maximum=name=="common.reduce_max";WN *loop=WN_CreateBlock();
            WN_INSERT_BlockLast(body,store(_sum,floating(maximum?0xff800000:0)));
            WN *offset=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_reduction),integer(_kernel.elements)),load(_flat));
            WN_INSERT_BlockLast(loop,store(_next,Evaluate(_kernel.inputs[0],offset)));
            WN *value=maximum?WN_Select(MTYPE_F4,WN_LIOR(WN_NE(MTYPE_F4,load(_next),load(_next)),WN_GT(MTYPE_F4,load(_next),load(_sum))),load(_next),load(_sum)):
                WN_Add(MTYPE_F4,load(_sum),load(_next));
            WN_INSERT_BlockLast(loop,store(_sum,value));WN_INSERT_BlockLast(body,Loop(_reduction,_kernel.chunks,loop));return load(_sum);
        }
        if(name=="copy") return Read(0,load(_flat));
        if(name=="common.fill_like") return floating(strtoul(_operation.attributes.find("attr.value_bits")->second.c_str(),NULL,16));
        if(name=="common.transpose") {
            std::vector<UINT64> perm=parse_numbers(_operation.attributes.find("attr.permutation")->second);
            WN *index=integer(0);UINT64 stride=1;
            for(INT i=(INT)perm.size()-1;i>=0;--i) {
                UINT64 in_stride=1;for(size_t j=perm[i]+1;j<Input(0).shape.size();++j) in_stride*=Input(0).shape[j];
                WN *coord=WN_Binary(OPR_REM,MTYPE_U8,WN_Div(MTYPE_U8,load(_flat),integer(stride)),integer(Output().shape[i]));
                index=WN_Add(MTYPE_U8,index,WN_Mpy(MTYPE_U8,coord,integer(in_stride)));stride*=Output().shape[i];
            }
            return Read(0,index);
        }
        if(name=="common.matmul") {
            UINT64 m=Output().shape[Output().shape.size()-2],n=Output().shape.back(),k=Input(0).shape.back();
            WN *loop=WN_CreateBlock();WN_INSERT_BlockLast(body,store(_sum,floating(0)));
            WN *ai=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,broadcast_index(Input(0),Output(),WN_Div(MTYPE_U8,load(_flat),integer(m*n)),2),integer(m*k)),
                WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_Binary(OPR_REM,MTYPE_U8,WN_Div(MTYPE_U8,load(_flat),integer(n)),integer(m)),integer(k)),load(_reduction)));
            WN *bi=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,broadcast_index(Input(1),Output(),WN_Div(MTYPE_U8,load(_flat),integer(m*n)),2),integer(k*n)),
                WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_reduction),integer(n)),WN_Binary(OPR_REM,MTYPE_U8,load(_flat),integer(n))));
            WN_INSERT_BlockLast(loop,store(_sum,WN_Add(MTYPE_F4,load(_sum),WN_Mpy(MTYPE_F4,Read(0,ai),Read(1,bi)))));
            WN_INSERT_BlockLast(body,Loop(_reduction,k,loop));return load(_sum);
        }
        if(name=="common.reduce_sum" || name=="common.reduce_max") {
            bool maximum=name=="common.reduce_max";UINT64 width=Input(0).shape.back();WN *loop=WN_CreateBlock();
            WN_INSERT_BlockLast(body,store(_sum,floating(maximum?0xff800000:0)));
            WN_INSERT_BlockLast(loop,store(_next,Read(0,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(width)),load(_reduction)))));
            WN *value=maximum?WN_Select(MTYPE_F4,WN_LIOR(WN_NE(MTYPE_F4,load(_next),load(_next)),WN_GT(MTYPE_F4,load(_next),load(_sum))),load(_next),load(_sum)):
                WN_Add(MTYPE_F4,load(_sum),load(_next));
            WN_INSERT_BlockLast(loop,store(_sum,value));WN_INSERT_BlockLast(body,Loop(_reduction,width,loop));return load(_sum);
        }
        if(name=="common.exp" || name=="common.log") {
            WN *arg=WN_CreateParm(MTYPE_F4,Element(0),MTYPE_To_TY(MTYPE_F4),WN_PARM_BY_VALUE);
            return WN_Create_Intrinsic(OPR_INTRINSIC_OP,MTYPE_F4,MTYPE_V,name=="common.exp"?INTRN_F4EXP:INTRN_F4LOG,1,&arg);
        }
        if(name=="common.isfinite") return WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,Element(0)),floating(0x7f7fffff));
        if(name=="common.logical_not") return WN_EQ(MTYPE_U4,Element(0),WN_Intconst(MTYPE_U4,0));
        if(name=="common.select") return WN_Select(MTYPE_F4,Element(0),Element(1),Element(2));
        if(name=="common.greater") return WN_GT(MTYPE_F4,Element(0),Element(1));
        if(name=="common.add") return WN_Add(MTYPE_F4,Element(0),Element(1));
        if(name=="common.sub") return WN_Sub(MTYPE_F4,Element(0),Element(1));
        if(name=="common.mul") return WN_Mpy(MTYPE_F4,Element(0),Element(1));
        return WN_Div(MTYPE_F4,Element(0),Element(1));
    }
    /* Reuse a canonical CUDA coordinate declaration; earlier preflight owns collisions. */
    ST *Coordinate(const char *name) {
        const ST *old;ST_IDX idx;FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) if(!strcmp(ST_name(old),name)) return &St_Table[ST_st_idx(old)];
        TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,12,KIND_STRUCT,MTYPE_M,Save_Str("open64_tensor_dim3"));Set_TY_align(ty,4);
        const char *names[]={"x","y","z"};
        for(UINT32 i=0;i<3;++i) {FLD_HANDLE f=New_FLD();if(!i) Set_TY_fld(t,f);FLD_Init(f,Save_Str(names[i]),MTYPE_To_TY(MTYPE_U4),i*4);if(i==2) Set_FLD_last_field(f);}
        ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);return s;
    }
    /* Read the exact recorded x-field rather than relying on an emitter heuristic. */
    WN *Coord(ST *s) {return WN_Cvt(MTYPE_U4,MTYPE_U8,WN_Ldid(MTYPE_U4,0,ST_st_idx(s),ST_type(s),1));}
    /* Attach source positions to every generated statement and declaration. */
    void Position(WN *n) {
        if(OPCODE_has_next_prev(WN_opcode(n))) WN_Set_Linenum(n,_source);
        if(WN_operator(n)==OPR_BLOCK) for(WN *s=WN_first(n);s;s=WN_next(s)) Position(s);
        else for(INT i=0;i<WN_kid_count(n);++i) if(WN_kid(n,i)) Position(WN_kid(n,i));
    }
    /* Commit to one validated empty device PU; no recoverable failures remain. */
    void Materialize() {
        ST *function=&St_Table[PU_Info_proc_sym(Current_PU_Info)];
        TY_IDX type;TY &t=New_TY(type);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(type);
        TYLIST_IDX list;Set_TYLIST_type(New_TYLIST(list),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,list);
        for(UINT32 i=0;i<=_kernel.inputs.size();++i) {
            const VHO_TENSOR_VALUE &v=_program.values[i==_kernel.inputs.size()?_kernel.output:_kernel.inputs[i]];
            TY_IDX ptr=Make_Pointer_Type(MTYPE_To_TY(v.dtype=="bool"?MTYPE_U1:MTYPE_F4));
            std::ostringstream name;name<<"arg"<<i;ST *s=New_ST(CURRENT_SYMTAB);
            ST_Init(s,Save_Str(name.str().c_str()),CLASS_VAR,SCLASS_FORMAL,EXPORT_LOCAL,ptr);Set_ST_is_value_parm(s);Set_ST_Srcpos(*s,_source);
            bind_argument(ST_st_idx(function),i,_program,i==_kernel.inputs.size()?_kernel.output:_kernel.inputs[i]);
            _formals.push_back(s);Set_TYLIST_type(New_TYLIST(list),ptr);
        }
        Set_TYLIST_type(New_TYLIST(list),TY_IDX_ZERO);Set_PU_prototype(Pu_Table[ST_pu(function)],type);
        _flat=local("element",MTYPE_U8,_source);_reduction=local("reduction",MTYPE_U8,_source);
        _physical=local("work_item",MTYPE_U8,_source);_part=local("chunk",MTYPE_U8,_source);
        _sum=local("accumulator",MTYPE_F4,_source);_next=local("next_value",MTYPE_F4,_source);
        WN *entry=WN_CreateEntry(_formals.size(),function,WN_CreateBlock(),NULL,NULL);
        for(UINT32 i=0;i<_formals.size();++i) WN_kid(entry,i)=WN_CreateIdname(0,_formals[i]);
        WN *body=WN_CreateBlock(),*element=WN_CreateBlock();
        /* Chunk-major work keeps neighboring output coordinates together,
         * including contiguous latent-rank cache accesses in MLA matmul. */
        WN_INSERT_BlockLast(element,store(_flat,_kernel.stage==1?WN_Binary(OPR_REM,MTYPE_U8,load(_physical),integer(VHO_tensor_elements(Output()))):load(_physical)));
        if(_kernel.stage==1) WN_INSERT_BlockLast(element,store(_part,WN_Div(MTYPE_U8,load(_physical),integer(VHO_tensor_elements(Output())))));
        WN *value=Computation(element);
        TYPE_ID scalar=Output().dtype=="bool"?MTYPE_U1:MTYPE_F4;
        WN_INSERT_BlockLast(element,WN_Istore(scalar,0,ST_type(_formals.back()),Address(_kernel.inputs.size(),load(_physical)),value));
        if(_program.opt_level==3) {
            WN *index=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,Coord(Coordinate("blockIdx")),integer(_program.threads)),Coord(Coordinate("threadIdx")));
            WN_INSERT_BlockLast(body,store(_physical,index));
            WN_INSERT_BlockLast(body,WN_CreateIf(WN_LT(MTYPE_U8,load(_physical),integer(_kernel.elements)),element,WN_CreateBlock()));
        } else WN_INSERT_BlockLast(body,Loop(_physical,_kernel.elements,element));
        WN_INSERT_BlockLast(body,WN_CreateReturn());WN_func_body(entry)=body;Set_PU_Info_tree_ptr(Current_PU_Info,entry);Position(entry);
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_operator",_operation.name.c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_version",decimal(_operation.version).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_node",decimal(_operation.source_node).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_pu",_program.source_pu.c_str());
        std::ostringstream count;count<<_kernel.elements;ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.elements",count.str().c_str());
        std::ostringstream threads;threads<<_program.threads;ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.block_x",threads.str().c_str());
        std::ostringstream grid;grid<<(_program.opt_level==3?(_kernel.elements+_program.threads-1)/_program.threads:1);
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.grid_x",grid.str().c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.operation",decimal(_kernel.id).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.fused",_program.fuse?"true":"false");
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.split_stage",decimal(_kernel.stage).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.split_width",decimal(_kernel.split_width).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.opt_level",decimal(_program.opt_level).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.body_hash",fingerprint(entry).c_str());
    }
};
} // namespace

/* Extents are bounded by validation, so the flat logical size cannot overflow. */
UINT64 VHO_tensor_elements(const VHO_TENSOR_VALUE &value) {
    UINT64 size=1;for(size_t i=0;i<value.shape.size();++i) size*=value.shape[i];return size;
}

/* Recheck the detached handoff before allocating any device table entries. */
BOOL VHO_tensor_validate_program(const VHO_TENSOR_PROGRAM &p,FILE *diagnostic) {
    try {
        need(p.opt_level<=3 && p.threads==(p.opt_level==3?128:1),"invalid level/thread mapping");
        need((!p.fuse && !p.split_width) || p.opt_level==3,"optimization requires O3");
        need(!p.split_width || (p.fuse && p.split_width>=2 && p.split_width<=4096),"invalid split width/fusion policy");
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
        need(defined.size()==p.values.size(),"unused/undefined recipe values");return TRUE;
    } catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor lowering: %s\n",e.what());return FALSE;}
}

/* Copy the selected plan only after all source and expansion checks succeed. */
BOOL VHO_tensor_schedule(const VHO_TENSOR_PROGRAM &p,VHO_TENSOR_SCHEDULE *out,FILE *diagnostic) {
    if(!out || !VHO_tensor_validate_program(p,diagnostic)) return FALSE;
    try {*out=schedule(p);return TRUE;}
    catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor schedule: %s\n",e.what());return FALSE;}
}

/* Read-only source admission belongs to the driver's exact active source PU. */
BOOL VHO_tensor_prepare_program(PU_Info *pu,UINT32 level,VHO_TENSOR_PROGRAM *output,FILE *diagnostic) {
    if(!pu || pu!=Current_PU_Info || !output || level>3 || PU_Info_child(pu) ||
       !DSL_Gatekeeper_Verify_PU(pu,diagnostic,NULL)) return FALSE;
    try {VHO_TENSOR_PROGRAM result=VHO_TENSOR_CAPTURE(pu,level).Capture();*output=result;return TRUE;}
    catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor lowering: %s\n",e.what());return FALSE;}
}

/* All recovery checks precede the device-only allocation/commit section. */
BOOL VHO_tensor_materialize_kernel(const VHO_TENSOR_PROGRAM &p,UINT32 ordinal,SRCPOS source,FILE *diagnostic) {
    VHO_TENSOR_SCHEDULE plan;if(!VHO_tensor_schedule(p,&plan,diagnostic)) return FALSE;
    bool present=false;for(size_t i=0;i<plan.kernels.size();++i) if(plan.kernels[i].id==ordinal) present=true;
    if(!present || !source || !SRCPOS_filenum(source) || SRCPOS_linenum(source)<=0 ||
       !Current_PU_Info || CURRENT_SYMTAB<=GLOBAL_SYMTAB || Pointer_Size!=8 || Pointer_Mtype!=MTYPE_U8 ||
       OPT_Fast_Math || Roundoff_Level!=ROUNDOFF_NONE || IEEE_Arithmetic!=IEEE_ACCURATE) return FALSE;
    WN *entry=PU_Info_tree_ptr(Current_PU_Info);ST *function=&St_Table[PU_Info_proc_sym(Current_PU_Info)];
    if(!entry || WN_operator(entry)!=OPR_FUNC_ENTRY || WN_st_idx(entry)!=ST_st_idx(function) || WN_num_formals(entry) || WN_first(WN_func_body(entry)) ||
       ST_device_symbol_kind(*function)!=DEVICE_SYMBOL_KERNEL || !Scope_tab[CURRENT_SYMTAB].st ||
       ST_st_idx(Scope_tab[CURRENT_SYMTAB].st)!=ST_st_idx(function)) return FALSE;
    const ST *symbol;ST_IDX idx;
    FOREACH_SYMBOL(GLOBAL_SYMTAB,symbol,idx) if(!strcmp(ST_name(symbol),"threadIdx") || !strcmp(ST_name(symbol),"blockIdx")) {
        if(ST_class(symbol)!=CLASS_VAR || ST_sclass(symbol)!=SCLASS_EXTERN || TY_kind(ST_type(symbol))!=KIND_STRUCT || TY_size(ST_type(symbol))!=12) return FALSE;
        FLD_HANDLE f=TY_fld(ST_type(symbol));const char *names[]={"x","y","z"};
        for(UINT32 i=0;i<3;++i) {if(f.Is_Null() || strcmp(FLD_name(f),names[i]) || FLD_ofst(f)!=4*i || FLD_type(f)!=MTYPE_To_TY(MTYPE_U4) ||
            (i==2)!=!!FLD_last_field(f)) return FALSE;if(i!=2) f=FLD_next(f);}
    }
    VHO_TENSOR_DEVICE(plan.bindings,selected(plan,ordinal),source).Materialize();return TRUE;
}

/* Native reopening must preserve the exact device tree, prototype, launch and
 * storage bindings before a host adapter consumes any of those facts. */
BOOL VHO_tensor_verify_kernel(const VHO_TENSOR_PROGRAM &p,UINT32 ordinal,FILE *diagnostic) {
    VHO_TENSOR_SCHEDULE plan;if(!VHO_tensor_schedule(p,&plan,diagnostic) || !Current_PU_Info) return FALSE;
    try {
        VHO_TENSOR_KERNEL kernel=selected(plan,ordinal);const VHO_TENSOR_OPERATION &op=p.operations[kernel.root];
        WN *entry=PU_Info_tree_ptr(Current_PU_Info);ST_IDX st=PU_Info_proc_sym(Current_PU_Info);
        need(entry && ST_device_symbol_kind(St_Table[st])==DEVICE_SYMBOL_KERNEL && WN_num_formals(entry)==kernel.inputs.size()+1,"kernel interface mismatch");
        need(WN_operator(entry)==OPR_FUNC_ENTRY && WN_st_idx(entry)==st,"kernel entry symbol mismatch");
        TY_IDX prototype=ST_pu_type(&St_Table[st]);need(TY_kind(prototype)==KIND_FUNCTION && TY_has_prototype(prototype),"missing kernel prototype");
        TYLIST_IDX signature=TY_tylist(prototype);TY_IDX result=signature?TYLIST_type(Tylist_Table[signature]):TY_IDX_ZERO;
        /* A mapped reader owns different predefined-type caches from a fresh
         * builder. Inspect the mapped return type's meaning, not a host ID. */
        need(result && TY_mtype(result)==MTYPE_V,"kernel return type mismatch");
        for(UINT32 i=0;i<=kernel.inputs.size();++i) need(TYLIST_type(Tylist_Table[signature+1+i])==ST_type(WN_st(WN_formal(entry,i))),"kernel prototype argument mismatch");
        need(TYLIST_type(Tylist_Table[signature+kernel.inputs.size()+2])==TY_IDX_ZERO,"kernel prototype terminator mismatch");
        const char *keys[]={"tensor.kernel.source_operator","tensor.kernel.source_pu","tensor.kernel.elements","tensor.kernel.block_x","tensor.kernel.grid_x","tensor.kernel.operation","tensor.kernel.opt_level","tensor.kernel.body_hash"};
        std::string expected[]={op.name,p.source_pu,decimal(kernel.elements),decimal(p.threads),
            decimal(p.opt_level==3?(kernel.elements+p.threads-1)/p.threads:1),decimal(ordinal),decimal(p.opt_level),fingerprint(entry)};
        for(UINT32 k=0;k<8;++k) {const char *actual=ST_tensor_metadata(st,keys[k]);need(actual && expected[k]==actual,"stale/mutated device product");}
        const char *version=ST_tensor_metadata(st,"tensor.kernel.source_version"),*node=ST_tensor_metadata(st,"tensor.kernel.source_node");
        need(version && node && decimal(op.version)==version && decimal(op.source_node)==node,"source operation identity mismatch");
        const char *policy_keys[]={"tensor.kernel.fused","tensor.kernel.split_stage","tensor.kernel.split_width"};
        std::string policy_values[]={p.fuse?"true":"false",decimal(kernel.stage),decimal(kernel.split_width)};
        for(UINT32 i=0;i<3;++i) {const char *actual=ST_tensor_metadata(st,policy_keys[i]);need(actual && policy_values[i]==actual,"schedule policy mismatch");}
        for(UINT32 i=0;i<=kernel.inputs.size();++i) {
            UINT32 value=i==kernel.inputs.size()?kernel.output:kernel.inputs[i];const VHO_TENSOR_VALUE &v=plan.bindings.values[value],&storage=plan.bindings.values[v.storage];ST *formal=WN_st(WN_formal(entry,i));
            need(TY_kind(ST_type(formal))==KIND_POINTER && TY_mtype(TY_pointed(ST_type(formal)))==(v.dtype=="bool"?MTYPE_U1:MTYPE_F4),"kernel pointer dtype mismatch");
            const char *ak[]={"value","storage","role","ordinal","elements","bytes","dtype","name"};
            std::string av[]={decimal(value),decimal(v.storage),decimal(storage.role),decimal(storage.ordinal),decimal(VHO_tensor_elements(storage)),
                decimal(VHO_tensor_elements(storage)*(storage.dtype=="bool"?1:4)),storage.dtype,storage.name};
            for(UINT32 k=0;k<8;++k) {const char *actual=ST_tensor_metadata(st,(std::string("tensor.argument.")+decimal(i)+"."+ak[k]).c_str());need(actual && av[k]==actual,"kernel binding mismatch");}
        }
        return TRUE;
    } catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor kernel audit: %s\n",e.what());return FALSE;}
}
