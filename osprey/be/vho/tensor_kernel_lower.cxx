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
#include "tensor_kernel_explain.h"
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
/* Typed CUDA builtins are the existing ordinary CALL mechanism, not new IR
 * opcodes. Effects remain conservative; convergence belongs to this schedule. */
bool collective(const ST *s) {
    const char *name=ST_name(s);bool shuffle=!strcmp(name,"__shfl_down_sync"),warp=!strcmp(name,"__syncwarp");
    if(!shuffle && !warp && strcmp(name,"__syncthreads")) return false;
    if(ST_class(s)!=CLASS_FUNC || ST_sclass(s)!=SCLASS_EXTERN) return false;
    TY_IDX t=ST_pu_type(s);if(TY_kind(t)!=KIND_FUNCTION || !TY_has_prototype(t)) return false;
    TYLIST_IDX a=TY_tylist(t);TYPE_ID expected[]={shuffle?MTYPE_F4:MTYPE_V,MTYPE_U4,MTYPE_F4,MTYPE_U4,MTYPE_I4};
    UINT32 count=shuffle?5:warp?2:1;
    for(UINT32 i=0;i<count;++i) {TY_IDX p=TYLIST_type(Tylist_Table[a+i]);if(!p || TY_mtype(p)!=expected[i]) return false;}
    return TYLIST_type(Tylist_Table[a+count])==TY_IDX_ZERO;
}
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
            if(TY_kind(ty)==KIND_ARRAY) {
                TY_IDX element=TY_etype(ty);words.push_back(element);words.push_back(TY_mtype(element));words.push_back(TY_size(element));
                ARB_HANDLE arb=TY_arb(ty);UINT32 rank=ARB_dimension(arb);words.push_back(rank);
                for(UINT32 i=0;i<rank;++i) {
                    ARB_HANDLE bound=arb[i];
                    need(ARB_const_lbnd(bound) && ARB_const_ubnd(bound) && ARB_const_stride(bound),"dynamic shared bounds");
                    words.push_back(ARB_lbnd_val(bound));words.push_back(ARB_ubnd_val(bound));words.push_back(ARB_stride_val(bound));
                }
            }
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
    if(op==OPR_COMMENT) {
        const unsigned char *text=(const unsigned char *)Index_To_Str(WN_GetComment(n));
        while(*text) words.push_back(*text++);words.push_back(0);
    }
    if(op==OPR_INTRINSIC_OP) words.push_back(WN_intrinsic(n));
    if(op==OPR_CALL) {
        need(collective(WN_st(n)),"unexpected device call");words.push_back(WN_call_flag(n));
        const unsigned char *name=(const unsigned char *)ST_name(WN_st(n));while(*name) words.push_back(*name++);words.push_back(0);
    }
    need(op!=OPR_INTRINSIC_CALL && op!=OPR_ASM_STMT,"unexpected device effect");
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
    UINT32 value,std::set<UINT32> &inputs,UINT32 &budget,const VHO_TENSOR_ATTENTION &a) {
    need(budget++<4096,"fusion expression expansion limit");
    if(a.enabled && (value==a.latent || value==a.lse)) {inputs.insert(a.bundle);return;}
    std::map<UINT32,UINT32>::const_iterator found=producer.find(value);
    if(found==producer.end() || boundary(p.operations[found->second])) {inputs.insert(value);return;}
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
            k.threads=p.chunk_policy==3 && p.register_latent && k.stage==1 && (k.split_width==32 || k.split_width==64)?k.split_width:p.threads;
            UINT32 ids[]={a.q,a.qr,a.cache,a.rope_cache,a.mask};k.inputs.assign(ids,ids+5);k.elements=VHO_tensor_elements(plan.bindings.values[k.output]);
            plan.kernels.push_back(k);
            if(chunks>1) {
                k.id=i+4096;k.mapping=4;k.stage=2;k.threads=p.threads;k.output=a.bundle;k.inputs.clear();k.inputs.push_back(a.partial);
                k.elements=VHO_tensor_elements(plan.bindings.values[k.output]);plan.kernels.push_back(k);
            }
            continue;
        }
        if(p.fuse ? (!boundary(op) && p.values[op.output].role!=2) : op.name=="common.reshape") continue;
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
        UINT64 width=boundary(op)?p.values[op.inputs[0]].shape.back():0;
        if(op.name=="common.matmul") width=p.values[op.inputs[0]].shape.back();
        if(p.cooperative && boundary(op)) k.mapping=op.name=="common.matmul"?(width>=32?2:0):1;
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
    VHO_TENSOR_ATTENTION _attention;
    std::map<UINT32,UINT32> _producer;
    std::vector<ST *> _formals;
    ST *_flat,*_physical,*_part,*_reduction,*_sum,*_next;
    ST *_tid,*_lane,*_warp,*_valid,*_base,*_tile,*_begin,*_end,*_column,*_shared_a,*_shared_b;
public:
    /* Bind only detached numeric facts and the active kernel's source position. */
    VHO_TENSOR_DEVICE(const VHO_TENSOR_PROGRAM &p,const VHO_TENSOR_KERNEL &kernel,SRCPOS source):
        _program(p),_operation(p.operations[kernel.root]),_source(source),_kernel(kernel),_attention(VHO_tensor_attention(p)) {
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
        if(_attention.enabled && (value==_attention.latent || value==_attention.lse)) {
            WN *offset;
            if(value==_attention.lse) offset=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,index,integer(_attention.rank+1)),integer(_attention.rank));
            else offset=WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_Div(MTYPE_U8,WN_COPY_Tree(index),integer(_attention.rank)),integer(_attention.rank+1)),
                WN_Binary(OPR_REM,MTYPE_U8,index,integer(_attention.rank)));
            return Evaluate(_attention.bundle,offset);
        }
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
    /* Reuse the scalar lowering's exact batched matrix indices in new mappings. */
    WN *MatrixA(WN *flat,WN *k) {
        UINT64 m=Output().shape[Output().shape.size()-2],n=Output().shape.back(),width=Input(0).shape.back();
        WN *batch=broadcast_index(Input(0),Output(),WN_Div(MTYPE_U8,WN_COPY_Tree(flat),integer(m*n)),2);
        WN *row=WN_Binary(OPR_REM,MTYPE_U8,WN_Div(MTYPE_U8,flat,integer(n)),integer(m));
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,batch,integer(m*width)),WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,row,integer(width)),k));
    }
    /* The logical output coordinate supplies both broadcast batch and column. */
    WN *MatrixB(WN *flat,WN *k) {
        UINT64 m=Output().shape[Output().shape.size()-2],n=Output().shape.back(),width=Input(0).shape.back();
        WN *batch=broadcast_index(Input(1),Output(),WN_Div(MTYPE_U8,WN_COPY_Tree(flat),integer(m*n)),2);
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,batch,integer(width*n)),
            WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,k,integer(n)),WN_Binary(OPR_REM,MTYPE_U8,flat,integer(n))));
    }
    /* Construct a standard typed, effectful CUDA builtin declaration once. */
    ST *Builtin(bool shuffle,bool warp=false) {
        const char *name=shuffle?"__shfl_down_sync":warp?"__syncwarp":"__syncthreads";const ST *old;ST_IDX index;
        FOREACH_SYMBOL(GLOBAL_SYMTAB,old,index) if(!strcmp(ST_name(old),name)) {need(collective(old),"incompatible CUDA builtin");return &St_Table[ST_st_idx(old)];}
        TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(ty);
        TYLIST_IDX a;TYPE_ID types[]={shuffle?MTYPE_F4:MTYPE_V,MTYPE_U4,MTYPE_F4,MTYPE_U4,MTYPE_I4};
        UINT32 count=shuffle?5:warp?2:1;
        for(UINT32 i=0;i<count;++i) {Set_TYLIST_type(New_TYLIST(a),MTYPE_To_TY(types[i]));if(!i) Set_TY_tylist(t,a);}
        Set_TYLIST_type(New_TYLIST(a),TY_IDX_ZERO);PU_IDX pi;PU &pu=New_PU(pi);PU_Init(pu,ty,GLOBAL_SYMTAB+1);Set_PU_c_lang(pu);
        ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name),CLASS_FUNC,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);Set_ST_pu(s,pi);
        Set_ST_device_symbol_kind(*s,DEVICE_SYMBOL_FUNCTION);return s;
    }
    /* A full-warp shuffle CALL is immediately followed by its return-preg read. */
    void WarpTree(WN *body,bool maximum) {
        for(UINT32 offset=16;offset;offset/=2) {
            WN *call=WN_Call(MTYPE_F4,MTYPE_V,4,ST_st_idx(Builtin(true)));WN_Set_Call_Default_Flags(call);
            WN *args[]={WN_Intconst(MTYPE_U4,0xffffffffU),load(_sum),WN_Intconst(MTYPE_U4,offset),WN_Intconst(MTYPE_I4,32)};
            TYPE_ID types[]={MTYPE_U4,MTYPE_F4,MTYPE_U4,MTYPE_I4};
            for(UINT32 i=0;i<4;++i) WN_kid(call,i)=WN_CreateParm(types[i],args[i],MTYPE_To_TY(types[i]),WN_PARM_BY_VALUE);
            WN_INSERT_BlockLast(body,call);
            WN_INSERT_BlockLast(body,store(_next,WN_Ldid(MTYPE_F4,-1,Return_Val_Preg,MTYPE_To_TY(MTYPE_F4))));
            WN *combine=WN_CreateBlock();
            WN *value=maximum?WN_Select(MTYPE_F4,WN_LIOR(WN_NE(MTYPE_F4,load(_next),load(_next)),WN_GT(MTYPE_F4,load(_next),load(_sum))),load(_next),load(_sum)):WN_Add(MTYPE_F4,load(_sum),load(_next));
            WN_INSERT_BlockLast(combine,store(_sum,value));
            WN_INSERT_BlockLast(body,WN_CreateIf(WN_LT(MTYPE_U8,load(_lane),integer(32-offset)),combine,WN_CreateBlock()));
        }
    }
    /* Each synchronization point is outside all lane/column/tail predicates. */
    void Barrier(WN *body) {
        WN *call=WN_Call(MTYPE_V,MTYPE_V,0,ST_st_idx(Builtin(false)));WN_Set_Call_Default_Flags(call);WN_INSERT_BlockLast(body,call);
    }
    /* Full-mask warp fence publishes shared statistics under independent
     * thread scheduling; it is never placed inside a lane/tail predicate. */
    void WarpBarrier(WN *body) {
        WN *call=WN_Call(MTYPE_V,MTYPE_V,1,ST_st_idx(Builtin(false,true)));WN_Set_Call_Default_Flags(call);
        WN_kid0(call)=WN_CreateParm(MTYPE_U4,WN_Intconst(MTYPE_U4,0xffffffffU),MTYPE_To_TY(MTYPE_U4),WN_PARM_BY_VALUE);
        WN_INSERT_BlockLast(body,call);
    }
    /* Shared arrays use the established device-symbol attribute and ARRAY nodes. */
    ST *Shared(const char *name,UINT64 count) {
        TY_IDX ty=Make_Array_Type(MTYPE_F4,1,count);Set_TY_align(ty,16);Set_TY_is_user_align(ty);
        ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,ty);
        Set_ST_device_symbol_kind(*s,DEVICE_SYMBOL_SHARED);Set_ST_Srcpos(*s,_source);return s;
    }
    /* A shared allocation is CTA-local in CUDA, with exact selected extent. */
    WN *SharedAddress(ST *s,UINT64 count,WN *index) {
        WN *array=WN_Create(OPR_ARRAY,Pointer_Mtype,MTYPE_V,3);WN_element_size(array)=4;
        WN_kid0(array)=WN_Lda(Pointer_Mtype,0,s);WN_kid1(array)=integer(count);WN_kid2(array)=index;return array;
    }
    /* XOR column swizzling distributes each warp's strided K reads over all
     * 32 F32 banks, while each producer warp still writes distinct banks. */
    WN *SharedBIndex(WN *k,WN *column) {
        return WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_COPY_Tree(k),integer(4)),
            WN_Binary(OPR_BXOR,MTYPE_U8,column,WN_Div(MTYPE_U8,k,integer(8))));
    }
    /* Source masks and guards are evaluated before a staged value is published. */
    void SharedStore(WN *body,ST *s,UINT64 count,WN *index,WN *value) {
        WN_INSERT_BlockLast(body,WN_Istore(MTYPE_F4,0,Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4)),SharedAddress(s,count,index),value));
    }
    /* Read only after the CTA's producer barrier. */
    WN *SharedRead(ST *s,UINT64 count,WN *index) {return WN_Iload(MTYPE_F4,0,MTYPE_To_TY(MTYPE_F4),SharedAddress(s,count,index));}
    /* Cooperative row reductions and staged dot products share a complete warp
     * per result. No lane exits before a shuffle, including invalid outputs. */
    WN *CooperativeBody() {
        WN *body=WN_CreateBlock();UINT64 outputs=VHO_tensor_elements(Output());
        UINT64 width=_kernel.stage==2?_kernel.chunks:Input(0).shape.back();
        bool maximum=_operation.name=="common.reduce_max";
        WN_INSERT_BlockLast(body,store(_tid,Coord(Coordinate("threadIdx"))));
        WN_INSERT_BlockLast(body,store(_lane,WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(32))));
        WN_INSERT_BlockLast(body,store(_warp,WN_Div(MTYPE_U8,load(_tid),integer(32))));
        if(_kernel.mapping==1) {
            WN_INSERT_BlockLast(body,store(_physical,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,Coord(Coordinate("blockIdx")),integer(4)),load(_warp))));
            WN_INSERT_BlockLast(body,store(_flat,_kernel.stage==1?WN_Binary(OPR_REM,MTYPE_U8,load(_physical),integer(outputs)):load(_physical)));
            WN_INSERT_BlockLast(body,store(_part,_kernel.stage==1?WN_Div(MTYPE_U8,load(_physical),integer(outputs)):integer(0)));
            WN_INSERT_BlockLast(body,store(_valid,WN_LT(MTYPE_U8,load(_physical),integer(_kernel.elements))));
        } else {
            UINT64 n=Output().shape.back(),tiles=(outputs/n)*((n+3)/4);
            WN_INSERT_BlockLast(body,store(_part,_kernel.stage==1?WN_Div(MTYPE_U8,Coord(Coordinate("blockIdx")),integer(tiles)):integer(0)));
            WN_INSERT_BlockLast(body,store(_base,WN_Binary(OPR_REM,MTYPE_U8,Coord(Coordinate("blockIdx")),integer(tiles))));
            WN_INSERT_BlockLast(body,store(_base,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,WN_Div(MTYPE_U8,load(_base),integer((n+3)/4)),integer(n)),
                WN_Mpy(MTYPE_U8,WN_Binary(OPR_REM,MTYPE_U8,load(_base),integer((n+3)/4)),integer(4)))));
            WN_INSERT_BlockLast(body,store(_flat,WN_Add(MTYPE_U8,load(_base),load(_warp))));
            WN_INSERT_BlockLast(body,store(_physical,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_part),integer(outputs)),load(_flat))));
            WN_INSERT_BlockLast(body,store(_valid,WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,WN_Binary(OPR_REM,MTYPE_U8,load(_base),integer(n)),load(_warp)),integer(n))));
            _shared_a=Shared("staged_a",32);_shared_b=Shared("staged_b",128);
        }
        WN_INSERT_BlockLast(body,store(_begin,_kernel.stage==1?WN_Mpy(MTYPE_U8,load(_part),integer(_kernel.split_width)):integer(0)));
        WN *limit=WN_Add(MTYPE_U8,load(_begin),integer(_kernel.stage==1?_kernel.split_width:width));
        WN_INSERT_BlockLast(body,store(_end,WN_Select(MTYPE_U8,WN_LT(MTYPE_U8,WN_COPY_Tree(limit),integer(width)),limit,integer(width))));
        WN_INSERT_BlockLast(body,store(_sum,floating(maximum?0xff800000:0)));
        WN *loop=WN_CreateBlock();
        if(_kernel.mapping==1) {
            WN *value=_kernel.stage==2?Evaluate(_kernel.inputs[0],WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_reduction),integer(outputs)),load(_flat))):
                Read(0,WN_Add(MTYPE_U8,WN_Mpy(MTYPE_U8,load(_flat),integer(width)),load(_reduction)));
            WN_INSERT_BlockLast(loop,store(_next,value));
            WN_INSERT_BlockLast(loop,store(_sum,maximum?WN_Select(MTYPE_F4,WN_LIOR(WN_NE(MTYPE_F4,load(_next),load(_next)),WN_GT(MTYPE_F4,load(_next),load(_sum))),load(_next),load(_sum)):WN_Add(MTYPE_F4,load(_sum),load(_next))));
            WN_INSERT_BlockLast(body,WN_CreateDO(WN_CreateIdname(0,_reduction),store(_reduction,WN_Add(MTYPE_U8,load(_begin),load(_lane))),
                WN_LAND(load(_valid),WN_LT(MTYPE_U8,load(_reduction),load(_end))),store(_reduction,WN_Add(MTYPE_U8,load(_reduction),integer(32))),loop,NULL));
        } else {
            UINT64 n=Output().shape.back();WN *a=WN_CreateBlock(),*avalid=WN_CreateBlock(),*azero=WN_CreateBlock();
            WN_INSERT_BlockLast(loop,store(_reduction,WN_Add(MTYPE_U8,load(_tile),load(_lane))));
            SharedStore(avalid,_shared_a,32,load(_lane),Read(0,MatrixA(load(_base),load(_reduction))));SharedStore(azero,_shared_a,32,load(_lane),floating(0));
            WN_INSERT_BlockLast(a,WN_CreateIf(WN_LT(MTYPE_U8,load(_reduction),load(_end)),avalid,azero));
            WN_INSERT_BlockLast(loop,WN_CreateIf(WN_LT(MTYPE_U8,load(_tid),integer(32)),a,WN_CreateBlock()));
            WN_INSERT_BlockLast(loop,store(_column,WN_Add(MTYPE_U8,load(_base),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4)))));
            WN_INSERT_BlockLast(loop,store(_reduction,WN_Add(MTYPE_U8,load(_tile),WN_Div(MTYPE_U8,load(_tid),integer(4)))));
            WN *bvalid=WN_CreateBlock(),*bzero=WN_CreateBlock();
            SharedStore(bvalid,_shared_b,128,SharedBIndex(WN_Div(MTYPE_U8,load(_tid),integer(4)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4))),Read(1,MatrixB(load(_column),load(_reduction))));
            SharedStore(bzero,_shared_b,128,SharedBIndex(WN_Div(MTYPE_U8,load(_tid),integer(4)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4))),floating(0));
            WN *column_valid=WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,WN_Binary(OPR_REM,MTYPE_U8,load(_base),integer(n)),WN_Binary(OPR_REM,MTYPE_U8,load(_tid),integer(4))),integer(n));
            WN_INSERT_BlockLast(loop,WN_CreateIf(WN_LAND(column_valid,WN_LT(MTYPE_U8,load(_reduction),load(_end))),bvalid,bzero));
            Barrier(loop);WN *compute=WN_CreateBlock();
            WN_INSERT_BlockLast(compute,store(_sum,WN_Add(MTYPE_F4,load(_sum),WN_Mpy(MTYPE_F4,SharedRead(_shared_a,32,load(_lane)),
                SharedRead(_shared_b,128,SharedBIndex(load(_lane),load(_warp)))))));
            WN_INSERT_BlockLast(loop,WN_CreateIf(WN_LAND(load(_valid),WN_LT(MTYPE_U8,WN_Add(MTYPE_U8,load(_tile),load(_lane)),load(_end))),compute,WN_CreateBlock()));
            Barrier(loop);
            WN_INSERT_BlockLast(body,WN_CreateDO(WN_CreateIdname(0,_tile),store(_tile,load(_begin)),WN_LT(MTYPE_U8,load(_tile),load(_end)),
                store(_tile,WN_Add(MTYPE_U8,load(_tile),integer(32))),loop,NULL));
        }
        WarpTree(body,maximum);WN *write=WN_CreateBlock();
        WN_INSERT_BlockLast(write,WN_Istore(MTYPE_F4,0,ST_type(_formals.back()),Address(_kernel.inputs.size(),load(_physical)),load(_sum)));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_LAND(load(_valid),WN_EQ(MTYPE_U8,load(_lane),integer(0))),write,WN_CreateBlock()));
        return body;
    }
    #include "tensor_attention_device.inc"
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
        WN *body;
        if(_kernel.mapping) {
            _tid=local("cta_thread",MTYPE_U8,_source);_lane=local("warp_lane",MTYPE_U8,_source);
            _warp=local("cta_warp",MTYPE_U8,_source);_valid=local("valid_output",MTYPE_I4,_source);
            _base=local("column_tile",MTYPE_U8,_source);_tile=local("token_tile",MTYPE_U8,_source);
            _begin=local("token_begin",MTYPE_U8,_source);_end=local("token_end",MTYPE_U8,_source);
            _column=local("copy_column",MTYPE_U8,_source);body=_kernel.mapping>=3?AttentionBody():CooperativeBody();
        } else {
            body=WN_CreateBlock();WN *element=WN_CreateBlock();
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
        }
        WN_INSERT_BlockLast(body,WN_CreateReturn());WN_func_body(entry)=body;Set_PU_Info_tree_ptr(Current_PU_Info,entry);Position(entry);
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_operator",_operation.name.c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_version",decimal(_operation.version).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_node",decimal(_operation.source_node).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.source_pu",_program.source_pu.c_str());
        std::ostringstream count;count<<_kernel.elements;ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.elements",count.str().c_str());
        std::ostringstream threads;threads<<_kernel.threads;ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.block_x",threads.str().c_str());
        std::ostringstream grid;grid<<VHO_tensor_grid(_program,_kernel);
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.grid_x",grid.str().c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.operation",decimal(_kernel.id).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.mapping",decimal(_kernel.mapping).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.shared_bytes",decimal(VHO_tensor_shared_bytes(_program,_kernel)).c_str());
        if(_program.auto_chunks) ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.chunk_selection",VHO_tensor_chunk_selection_identity(_program).c_str());
        if(_program.register_latent) ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.attention_accumulation",_kernel.mapping>=3?"register_4_columns_per_warp":"none");
        if(_program.fuse_attention) ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.attention_layout",
            _kernel.mapping<3?"none":_kernel.stage==1?"maximum,denominator,normalized_latent":"latent,lse");
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.fused",_program.fuse?"true":"false");
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.split_stage",decimal(_kernel.stage).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.split_width",decimal(_kernel.split_width).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.opt_level",decimal(_program.opt_level).c_str());
        ST_tensor_bind_metadata(ST_st_idx(function),"tensor.kernel.body_hash",fingerprint(entry).c_str());
    }
};
} // namespace

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
    FOREACH_SYMBOL(GLOBAL_SYMTAB,symbol,idx) if(!strcmp(ST_name(symbol),"__shfl_down_sync") || !strcmp(ST_name(symbol),"__syncthreads") || !strcmp(ST_name(symbol),"__syncwarp"))
        if(!collective(symbol)) return FALSE;
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
        std::string expected[]={op.name,p.source_pu,decimal(kernel.elements),decimal(kernel.threads),
            decimal(VHO_tensor_grid(p,kernel)),decimal(ordinal),decimal(p.opt_level),fingerprint(entry)};
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
        if(p.fuse_attention) {
            const char *layout=ST_tensor_metadata(st,"tensor.kernel.attention_layout");
            std::string expected_layout=kernel.mapping<3?"none":kernel.stage==1?"maximum,denominator,normalized_latent":"latent,lse";
            need(layout && expected_layout==layout,"attention tuple layout mismatch");
        }
        if(p.auto_chunks) {
            const char *selection=ST_tensor_metadata(st,"tensor.kernel.chunk_selection");
            need(selection && VHO_tensor_chunk_selection_identity(p)==selection,"automatic selection metadata mismatch");
        }
        if(p.register_latent) {
            const char *accumulation=ST_tensor_metadata(st,"tensor.kernel.attention_accumulation");
            need(accumulation && !strcmp(accumulation,kernel.mapping>=3?"register_4_columns_per_warp":"none"),"attention accumulation mismatch");
        }
        if(p.cooperative) {
            const char *mapping=ST_tensor_metadata(st,"tensor.kernel.mapping"),*shared=ST_tensor_metadata(st,"tensor.kernel.shared_bytes");
            need(mapping && decimal(kernel.mapping)==mapping && shared && decimal(VHO_tensor_shared_bytes(p,kernel))==shared,"cooperative resource mapping mismatch");
        }
        const char *versioned_comments=ST_tensor_metadata(st,"tensor.kernel.explanations_version");
        if(versioned_comments) {
            need(!strcmp(versioned_comments,"1"),"unsupported explanation version");
            std::vector<VHO_TENSOR_EXPLANATION> comments=VHO_tensor_explanations(plan);
            WN *statement=WN_first(WN_func_body(entry));UINT32 count=0;
            for(size_t i=0;i<comments.size();++i) if(comments[i].kernel_id==ordinal) {
                need(statement && WN_operator(statement)==OPR_COMMENT && comments[i].text==Index_To_Str(WN_GetComment(statement)),"missing/changed WHIRL explanation");
                statement=WN_next(statement);++count;
            }
            const char *actual=ST_tensor_metadata(st,"tensor.kernel.explanations_count");
            need(actual && decimal(count)==actual,"explanation count mismatch");
        }
        return TRUE;
    } catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor kernel audit: %s\n",e.what());return FALSE;}
}

/* COMMENT is a nonexecuting VH--VL statement backed by CLASS_NAME, as in
 * WHIRL.pdf p23. Only the active PU is changed, after complete preflight. */
BOOL VHO_tensor_annotate_kernel(const VHO_TENSOR_PROGRAM &p,UINT32 ordinal,FILE *diagnostic) {
    if(!Current_PU_Info || !VHO_tensor_verify_kernel(p,ordinal,diagnostic)) return FALSE;
    ST_IDX function=PU_Info_proc_sym(Current_PU_Info);
    if(ST_tensor_metadata(function,"tensor.kernel.explanations_version")) return FALSE;
    VHO_TENSOR_SCHEDULE plan;if(!VHO_tensor_schedule(p,&plan,diagnostic)) return FALSE;
    std::vector<VHO_TENSOR_EXPLANATION> all=VHO_tensor_explanations(plan),comments;
    for(size_t i=0;i<all.size();++i) if(all[i].kernel_id==ordinal) {
        if(all[i].text.find("*/")!=std::string::npos || all[i].text.compare(0,7,"#pragma")==0) return FALSE;
        comments.push_back(all[i]);
    }
    WN *entry=PU_Info_tree_ptr(Current_PU_Info),*body=WN_func_body(entry),*first=WN_first(body);
    if(comments.empty() || !first) return FALSE;SRCPOS source=WN_Get_Linenum(first);
    if(!source || !SRCPOS_filenum(source) || SRCPOS_linenum(source)<=0) return FALSE;
    for(size_t i=0;i<comments.size();++i) {
        WN *comment=WN_CreateComment(comments[i].text.c_str());WN_Set_Linenum(comment,source);Set_ST_Srcpos(*WN_st(comment),source);
        WN_INSERT_BlockBefore(body,first,comment);
    }
    ST_tensor_bind_metadata(function,"tensor.kernel.explanations_version","1");
    ST_tensor_bind_metadata(function,"tensor.kernel.explanations_count",decimal(comments.size()).c_str());
    ST_tensor_bind_metadata(function,"tensor.kernel.body_hash",fingerprint(entry).c_str());
    return TRUE;
}
