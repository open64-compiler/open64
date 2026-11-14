/* Copyright (C) 2026 Open64 Project */
/* VHO source capture shared by the production driver and standalone tools.
 * Active PU only; all output is owned data, no native mutation or scope switch.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. */
#include <cctype>
#include <cstdlib>
#include <cerrno>
#include <set>
#include <sstream>
#include <stdexcept>
#include "tensor_kernel_lower.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "dsl_image_service.h"
#include "../com/dsl_ir_rewrite.h"
#include "dsl_gatekeeper.h"
#include "ir_reader.h"
namespace {
/* Source refusal occurs before any native allocation or output publication. */
void need(bool valid,const std::string &why) {if(!valid) throw std::runtime_error(why);}
/* Copy canonical dimensions using the established capture spelling. */
std::vector<UINT64> parse_numbers(const std::string &text) {
    std::string s=text;if(s.size()>1 && s[0]=='[' && s[s.size()-1]==']') s=s.substr(1,s.size()-2);
    std::vector<UINT64> out;const char *p=s.c_str();
    for(;;) {
        need(std::isdigit((unsigned char)*p),"invalid shape/permutation");char *end;errno=0;UINT64 n=strtoull(p,&end,10);
        need(!errno && end!=p,"integer overflow");out.push_back(n);if(!*end) break;
        need(*end==',' && end[1],"invalid integer delimiter");p=end+1;
    }return out;
}
class VHO_TENSOR_CAPTURE {
    PU_Info *_pu;
    VHO_TENSOR_PROGRAM _program;
    std::map<ST_IDX,UINT32> _symbols;
    std::set<UINT32> _defined;
    bool _returned, _quantized;
public:
    /* Borrow active source scope; capture never creates WN/ST/TY rows. */
    VHO_TENSOR_CAPTURE(PU_Info *pu,UINT32 level,bool quantized=false):_pu(pu),_returned(false),_quantized(quantized) {
        _program.opt_level=level;_program.threads=level==3?128:1;
        _program.source_pu=ST_name(PU_Info_proc_sym(pu));
    }
    /* Copy complete scalar descriptor facts, detaching the later device scope. */
    UINT32 Value(ST_IDX st,UINT32 role,UINT32 ordinal) {
        need(!_symbols.count(st),"duplicate tensor symbol");TY_IDX ty=ST_type(&St_Table[st]);
        need(TY_is_tensor_extension(ty),"non-tensor value");VHO_TENSOR_VALUE value;
        const TY_TENSOR_SCHEMA_KEY supported[]={TY_TENSOR_SCHEMA_KIND,TY_TENSOR_SCHEMA_DTYPE,TY_TENSOR_SCHEMA_RANK,
            TY_TENSOR_SCHEMA_SHAPE,TY_TENSOR_SCHEMA_LAYOUT,TY_TENSOR_SCHEMA_NO_ALIAS,TY_TENSOR_SCHEMA_CANONICAL,
            TY_TENSOR_SCHEMA_LINEAGE,TY_TENSOR_SCHEMA_SOURCE_LAYER_NAME,TY_TENSOR_SCHEMA_LOWERING_HINT,
            TY_TENSOR_SCHEMA_TRAITS};
        for(UINT32 i=0;i<TY_tensor_attribute_count(ty);++i) {
            const char *key=NULL,*text=NULL;TY_DSL_BIND_STATE state;
            need(TY_tensor_attribute_at(ty,i,&key,&text,&state),"unreadable tensor descriptor field");bool known=false;
            for(UINT32 k=0;k<sizeof(supported)/sizeof(supported[0]);++k) if(!strcmp(key,TY_tensor_schema_key_name(supported[k]))) known=true;
            if(_quantized && !strcmp(key,TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_QUANTIZATION))) known=true;
            need(known,"unsupported tensor trait/representation field: "+std::string(key));
            if(!strcmp(key,TY_tensor_schema_key_name(TY_TENSOR_SCHEMA_TRAITS)))
                need(state==TY_DSL_BIND_BOUND,"unbound tensor cache trait");
        }
        const char *kind=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_KIND);need(kind && !strcmp(kind,"tensor"),"unsupported tensor kind");
        const char *layout=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_LAYOUT);
        need(layout && (!strcmp(layout,"logical") || !strcmp(layout,"row_major")) &&
            !TY_tensor_attribute_is_bound(ty,"strides"),"unsupported physical tensor layout/strides");
        const char *dtype=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_DTYPE),*shape=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_SHAPE);
        need(dtype && shape,"incomplete tensor descriptor");value.dtype=dtype;value.shape=parse_numbers(shape);
        if(TY_tensor_attribute_is_bound(ty,TY_TENSOR_SCHEMA_TRAITS)) {
            const char *trait=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_TRAITS);
            need(DSL_tensor_cache_trait_known(trait),"unsupported tensor trait/representation field: traits");
            value.cache_trait=trait;
        }
        if(_quantized) {
            const char *encoding=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_QUANTIZATION);
            const char *scale=ST_tensor_metadata(st,"cache.inference.scale");
            const char *evidence=ST_tensor_metadata(st,"cache.inference.evidence");
            value.cache_encoding=encoding?encoding:"";
            value.cache_scale=scale?scale:"";value.cache_evidence=evidence?evidence:"";
        }
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
        need(_quantized?VHO_tensor_validate_quantized_high(_program,NULL):VHO_tensor_validate_program(_program,NULL),"invalid captured tensor recipe");return _program;
    }
};

}
/* Read-only source admission belongs to the driver's exact active source PU. */
BOOL VHO_tensor_prepare_program(PU_Info *pu,UINT32 level,VHO_TENSOR_PROGRAM *output,FILE *diagnostic) {
    if(!pu || pu!=Current_PU_Info || !output || level>3 || PU_Info_child(pu) ||
       !DSL_Gatekeeper_Verify_PU(pu,diagnostic,NULL)) return FALSE;
    try {VHO_TENSOR_PROGRAM result=VHO_TENSOR_CAPTURE(pu,level).Capture();*output=result;return TRUE;}
    catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"tensor lowering: %s\n",e.what());return FALSE;}
}

/* The driver selects the source PU. This opt-in source gate never admits a
 * packed cache to the old FP32 optimizer and never mutates native tables. */
BOOL VHO_tensor_prepare_quantized_program(PU_Info *pu,VHO_TENSOR_PROGRAM *output,FILE *diagnostic) {
    if(!pu || pu!=Current_PU_Info || !output || PU_Info_child(pu) ||
       !DSL_Gatekeeper_Verify_PU(pu,diagnostic,NULL)) return FALSE;
    try { VHO_TENSOR_PROGRAM result=VHO_TENSOR_CAPTURE(pu,0,true).Capture();*output=result;return TRUE; }
    catch(const std::exception &e) {if(diagnostic) fprintf(diagnostic,"Q1 High: %s\n",e.what());return FALSE;}
}
