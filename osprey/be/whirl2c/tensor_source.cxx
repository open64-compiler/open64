// Copyright (C) 2026. Distributed under the GNU GPL v2.
// Read-only mapped WHIRL source driver controlled by
// doc/AI-COMPILER-TENSOR-SOURCE-v0.1.md. No lowering or schedule selection.
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <cctype>
#include <unistd.h>
#include <fcntl.h>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <stdexcept>
#include <vector>
#include "defs.h"
#include "mempool.h"
#include "wn.h"
#include "stab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bread.h"
#include "errors.h"
#include "erglob.h"
#include "err_host.tab"
#include "dsl_ir_image.h"
#include "dsl_gatekeeper.h"

BOOL Run_vsaopt = FALSE;
INT8 Debug_Level = 0;
// Satisfy the common error infrastructure without compiler cleanup ownership.
void Signal_Cleanup(INT) {}
// This standalone reader does not provide target-specific error parameters.
const char *Host_Format_Parm(INT, MEM_PTR) { return "tensor-source"; }

namespace {
// Fail before any output is published; callers own diagnostics and exit status.
void require(bool ok, const std::string &why) {
  if (!ok) throw std::runtime_error(why);
}
// Parse a bounded comma-separated list of nonnegative decimal integers.
std::vector<unsigned long long> numbers(const char *text) {
  require(text && *text, "missing integer list");
  std::string list=text;
  if(list[0]=='[' && list[list.size()-1]==']') list=list.substr(1,list.size()-2);
  text=list.c_str();
  std::vector<unsigned long long> result;
  const char *p=text;
  for (;;) {
    require(std::isdigit(static_cast<unsigned char>(*p)), "invalid integer list");
    char *end=NULL; errno=0;
    unsigned long long n=strtoull(p,&end,10);
    require(!errno && end!=p, "integer overflow"); result.push_back(n);
    if (!*end) break;
    require(*end==',' && end[1], "invalid list delimiter"); p=end+1;
  }
  return result;
}
// Return a safe deterministic C++ identifier for a WHIRL symbol.
std::string identifier(const char *text) {
  std::string out="open64_reference_";
  for (const unsigned char *p=(const unsigned char *)text; *p; ++p)
    out+=std::isalnum(*p) ? char(*p) : '_';
  return out;
}
// Print symbol table identity rather than trusting frontend source spellings.
std::string value_name(ST_IDX st) {
  std::ostringstream out; out<<"v"<<st; return out.str();
}
// Read only positive static F32/bool logical tensor descriptors.
std::string tensor_type(TY_IDX ty) {
  require(TY_is_tensor_extension(ty), "non-tensor value");
  const char *dtype=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_DTYPE);
  require(dtype && (!strcmp(dtype,"float32") || !strcmp(dtype,"bool")), "unsupported dtype");
  std::vector<unsigned long long> shape=numbers(TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_SHAPE));
  require(shape.size()<=16, "unsupported rank");
  unsigned long long count=1;
  std::ostringstream out; out<<"TENSOR<"<<(!strcmp(dtype,"bool") ? "bool" : "float");
  for (size_t i=0;i<shape.size();++i) {
    require(shape[i] && shape[i]<=16777216/count, "reference tensor exceeds 16M elements");
    count*=shape[i]; out<<","<<shape[i];
  }
  out<<">"; return out.str();
}

class W2C_TENSOR_SOURCE {
  PU_Info *_pu;
  std::set<ST_IDX> _defined, _inputs, _outputs, _assigned;
  std::ostringstream _body;
  bool _returned;
  unsigned _operations;
public:
  // Bind the exact reader-selected PU; no symbol or IR table mutation occurs.
  explicit W2C_TENSOR_SOURCE(PU_Info *owner):_pu(owner),_returned(false),_operations(0) {}
  // Collect native typed attributes from the definition's stable image row.
  std::map<std::string,std::string> Attributes(WN *stmt, DSL_IR_NODE_RECORD &node,
                                             DSL_IR_OPCODE_DESCRIPTOR_RECORD &desc) {
    DSL_IR_VALUE_RECORD value;
    require(DSL_IR_Image_Find_Definition_Value(_pu,stmt,&value), "unresolved native definition");
    require(DSL_IR_Image_Get_Node(value.producer_node_id,&node) &&
            DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,&desc), "missing native descriptor");
    std::map<std::string,std::string> attrs;
    for (UINT32 i=0;i<node.attribute_count;++i) {
      DSL_IR_ATTRIBUTE_RECORD attr;
      require(DSL_IR_Image_Get_Attribute(node.first_attribute_id+i,&attr), "missing typed attribute");
      require(attrs.insert(std::make_pair(std::string(Index_To_Str(attr.name)),
                                         std::string(Index_To_Str(attr.value)))).second, "duplicate attribute");
    }
    return attrs;
  }
  // Resolve only initialized whole tensor operands; forbid offsets and memory effects.
  std::string Operand(WN *wn) {
    require(WN_operator(wn)==OPR_LDID && WN_offset(wn)==0, "expected whole tensor LDID");
    ST_IDX st=WN_st_idx(wn);
    require(_defined.count(st) && !_outputs.count(st), "undefined operand or output read");
    tensor_type(WN_ty(wn)); return value_name(st);
  }
  // Translate an admitted native definition, retaining operation boundaries.
  std::string Expression(WN *stmt) {
    WN *wn=WN_kid0(stmt);
    if (WN_operator(wn)==OPR_LDID) {
      require(tensor_type(WN_ty(stmt))==tensor_type(WN_ty(wn)), "copy type mismatch");
      return Operand(wn);
    }
    require(DSL_WN_Is_Native(wn), "unsupported non-native expression");
    DSL_LOGICAL_OPCODE logical;
    require(DSL_WN_Get_Logical_Opcode(wn,&logical,stderr), "invalid logical opcode");
    DSL_IR_NODE_RECORD node; DSL_IR_OPCODE_DESCRIPTOR_RECORD desc;
    std::map<std::string,std::string> attrs=Attributes(stmt,node,desc), expected;
    std::string name=Index_To_Str(desc.stable_name);
    std::vector<std::string> args;
    for (INT i=0;i<WN_kid_count(wn);++i) args.push_back(Operand(WN_kid(wn,i)));
    std::string type=tensor_type(WN_ty(stmt)), call;
    unsigned version=1, arity=1;
    if (name=="common.reshape") {
      call="reshape<"+type+">";
      std::string shape=TY_tensor_attribute(WN_ty(stmt),TY_TENSOR_SCHEMA_SHAPE);
      expected["attr.target_shape"]=shape.substr(1,shape.size()-2);
    } else if (name=="common.transpose") {
      std::vector<unsigned long long> p=numbers(attrs["attr.permutation"].c_str());
      std::ostringstream perm; perm<<"perm"<<node.id;
      _body<<"  const int "<<perm.str()<<"[] = {";
      for (size_t i=0;i<p.size();++i) {if(i) _body<<","; _body<<p[i];}
      _body<<"};\n";
      expected["attr.permutation"]=attrs["attr.permutation"];
      args.push_back(perm.str()); call="transpose<"+type+">";
    } else if (name=="common.matmul") {
      version=3; arity=2; call="matmul<"+type+">";
      expected["attr.transpose_kid0"]="false"; expected["attr.transpose_kid1"]="false";
      expected["attr.batch_rule"]="numpy"; expected["attr.accum_dtype"]="float32";
    } else if (name=="common.reduce_sum" || name=="common.reduce_max") {
      version=name=="common.reduce_sum" ? 3 : 1;
      std::vector<unsigned long long> dims=numbers(TY_tensor_attribute(WN_ty(WN_kid0(wn)),TY_TENSOR_SCHEMA_SHAPE));
      std::ostringstream axis; axis<<dims.size()-1;
      expected["attr.axis"]=axis.str(); expected["attr.keepdim"]=attrs["attr.keepdim"];
      require(expected["attr.keepdim"]=="true" || expected["attr.keepdim"]=="false", "invalid keepdim");
      expected["attr.accum_dtype"]="float32"; expected["attr.reassociate"]="false";
      call="reduce<"+type+","+(name=="common.reduce_max" ? "true" : "false")+">";
    } else if (name=="common.fill_like") {
      const std::string bits=attrs["attr.value_bits"];
      require(bits.size()==8 && bits.find_first_not_of("0123456789abcdef")==std::string::npos,"invalid binary32 literal");
      expected["attr.value_bits"]=bits; args.push_back("0x"+bits+"u"); call="fill_like<"+type+">";
    } else if (name=="common.select") {
      arity=3; expected["attr.broadcast_rule"]="numpy"; call="select<"+type+">";
    } else {
      const char *binary[]={"common.add","common.sub","common.mul","common.div","common.greater"};
      const char *unary[]={"common.exp","common.log","common.isfinite","common.logical_not"};
      for (unsigned i=0;i<5;++i) if(name==binary[i]) {
        arity=2; expected["attr.broadcast_rule"]="numpy";
        std::ostringstream op; op<<"binary<"<<type<<","<<i<<">"; call=op.str();
      }
      for (unsigned i=0;i<4;++i) if(name==unary[i]) {
        if(i<2) expected["attr.fp_semantics"]="source_unresolved";
        std::ostringstream op; op<<"unary<"<<type<<","<<i<<">"; call=op.str();
      }
    }
    require(!call.empty() && desc.version==version && logical.source_version==version,
            "unsupported operator/version: "+name);
    require(UINT32(WN_kid_count(wn))==arity && node.operand_count==arity,"operator arity mismatch");
    require(attrs==expected,"unsupported attributes: "+name);
    _body<<"  // native node "<<node.id<<": "<<name<<".v"<<version<<"\n";
    std::ostringstream result; result<<call<<"(";
    for(size_t i=0;i<args.size();++i) {if(i) result<<", "; result<<args[i];}
    result<<")"; ++_operations; return result.str();
  }
  // Visit pure straight-line tensor blocks, rejecting control flow and side effects.
  void Block(WN *wn) {
    require(WN_operator(wn)==OPR_BLOCK,"expected BLOCK");
    for(WN *s=WN_first(wn);s;s=WN_next(s)) {
      require(!_returned,"statement after RETURN");
      OPERATOR op=WN_operator(s);
      if(op==OPR_BLOCK) {Block(s); continue;}
      if(op==OPR_RETURN) {_returned=true; continue;}
      require(op==OPR_STID && WN_offset(s)==0,"unsupported statement or offset");
      ST_IDX st=WN_st_idx(s);
      require(!_inputs.count(st) && !_defined.count(st),"input mutation or repeated definition");
      std::string type=tensor_type(WN_ty(s));
      require(type==tensor_type(ST_type(WN_st(s))), "store symbol type mismatch");
      std::string rhs=Expression(s);
      _body<<"  "<<(_outputs.count(st) ? "" : type+" ")<<value_name(st)<<" = "<<rhs<<";\n";
      _defined.insert(st); if(_outputs.count(st)) _assigned.insert(st);
    }
  }
  // Emit a callable reference PU after validating its complete native tree.
  std::string Emit() {
    WN *entry=PU_Info_tree_ptr(_pu);
    require(WN_operator(entry)==OPR_FUNC_ENTRY,"expected FUNC_ENTRY");
    std::ostringstream signature;
    signature<<"extern \"C\" O64_HD void "<<identifier(ST_name(WN_st(entry)))<<"(\n";
    for(INT i=0;i<WN_num_formals(entry);++i) {
      WN *formal=WN_formal(entry,i); ST_IDX st=WN_st_idx(formal);
      const char *role=ST_tensor_metadata(st,"dsl.pu.result_role");
      bool output=role && !strcmp(role,"tensor");
      require(!role || output,"state result unsupported");
      if(output) _outputs.insert(st); else {_inputs.insert(st); _defined.insert(st);}
      signature<<"  "<<(output ? "" : "const ")<<tensor_type(ST_type(WN_st(formal)))<<" &"<<value_name(st);
      signature<<(i+1<WN_num_formals(entry) ? ",\n" : ") {\n");
    }
    require(!_inputs.empty() && !_outputs.empty(),"missing tensor interface");
    Block(WN_func_body(entry));
    require(_returned && _assigned==_outputs && _operations,"incomplete result definitions");
    signature<<_body.str()<<"}\n"; return signature.str();
  }
};

// Publish a fresh source file atomically, refusing overwrites and input aliases.
void publish(const std::string &path, const std::string &source) {
  std::string temp=path+".tmp.XXXXXX";
  std::vector<char> name(temp.begin(),temp.end()); name.push_back(0);
  int fd=mkstemp(&name[0]); require(fd>=0,"cannot create source staging file");
  size_t done=0; bool ok=true;
  while(done<source.size()) {
    ssize_t n=write(fd,source.data()+done,source.size()-done);
    if(n<0 && errno==EINTR) continue;
    if(n<=0) {ok=false;break;} done+=n;
  }
  if(ok) ok=fsync(fd)==0;
  if(close(fd)!=0) ok=false;
  if(ok) ok=link(&name[0],path.c_str())==0;
  unlink(&name[0]); require(ok,"cannot publish fresh source (destination may exist)");
}
} // namespace

// Read the mapped image, verify each PU, then publish CUDA without lowering.
int main(int argc,char **argv) {
  try {
    std::string input,output; bool cuda=false,reference=false;
    for(int i=1;i<argc;++i) {
      std::string a=argv[i];
      if(a=="-cuda" || a=="-CLIST:cuda=ON") cuda=true;
      else if(a=="-tensor-source") reference=true;
      else if(a.compare(0,17,"-CLIST:dotc_file=")==0) {
        require(output.empty() && a.size()>17,"duplicate or empty output option"); output=a.substr(17);
      }
      else if(a.compare(0,4,"-fB,")==0) {require(input.empty(),"duplicate input");input=a.substr(4);}
      else if(!a.empty() && a[0]!='-') {require(input.empty(),"duplicate input");input=a;}
      else if(a.compare(0,7,"-CLIST:")==0)
        throw std::runtime_error("unsupported tensor-source option: "+a);
      // Open64 phases receive the complete option set and ignore other owners.
    }
    require(cuda && reference && !input.empty(),"usage: whirl2c -cuda -tensor-source -CLIST:dotc_file=out.cu input.B");
    if(output.empty()) output=input+".reference.cu";
    MEM_Initialize(); Set_Error_Tables(Phases,host_errlist);
    Init_Error_Handler(10); Set_Error_File(NULL); Set_Error_Line(ERROR_LINE_UNKNOWN);
    WHIRL_Mldid_Mstid_On=TRUE;
    Open_Input_Info(const_cast<char *>(input.c_str()));
    Initialize_Symbol_Tables(FALSE); New_Scope(GLOBAL_SYMTAB,Malloc_Mem_Pool,FALSE);
    PU_Info *tree=Read_Global_Info(NULL); IR_reader_init();
    std::string source="// WHIRL-derived serial reference; no selected GPU mapping.\n#include \"open64_tensor_reference.cuh\"\nusing namespace open64_reference;\n\n";
    require(tree!=NULL,"empty image"); std::set<std::string> names;
    for(PU_Info *pu=tree;pu;pu=PU_Info_next(pu)) {
      require(!PU_Info_child(pu),"nested PU unsupported");
      MEM_POOL_Push(MEM_pu_nz_pool_ptr); MEM_POOL_Push(MEM_pu_pool_ptr);
      Read_Local_Info(MEM_pu_nz_pool_ptr,pu); Current_PU_Info=pu;
      require(names.insert(identifier(ST_name(PU_Info_proc_sym(pu)))).second,"C++ PU name collision");
      DSL_GATEKEEPER_RESULT result;
      require(DSL_Gatekeeper_Verify_PU(pu,stderr,&result),"native gatekeeper rejected PU");
      source+=W2C_TENSOR_SOURCE(pu).Emit()+"\n";
      Free_Local_Info(pu); MEM_POOL_Pop(MEM_pu_nz_pool_ptr); MEM_POOL_Pop(MEM_pu_pool_ptr);
    }
    Free_Input_Info(); publish(output,source); return 0;
  } catch(const std::exception &error) {
    fprintf(stderr,"whirl2c tensor-source: %s\n",error.what()); return 1;
  }
}
