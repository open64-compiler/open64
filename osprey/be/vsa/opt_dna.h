//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_dna.h
//
// ====================================================================
//


#ifndef opt_dna_INCLUDED
#define opt_dna_INCLUDED        "opt_dna.h"

#include "mempool_allocator.h"
#include "pu_info.h"       // for Current_PU_Info
#include "dwarf_DST_mem.h" // DST_TYPE
#include "be_symtab.h"     // for Be_scope_tab
#include "opt_htable.h"
#include "opt_util.h"      // for Vsa_check_sym_ignore
#include "opt_main.h"
#include <ext/hash_map>
#include <ext/hash_set>
using __gnu_cxx::hash_set;
#include <vector>
#include <stack>
#include "glob.h"
#include "whirl_file_ctx.h"
#include "whirl_file_mgr.h"
#include "symtab_access_global.h"
#include "opt_vsa_path.h"
#include "class_hierarchy.h"

#ifdef __STL_USE_NAMESPACES
using std::hash_map;
using std::vector;
#endif

#ifdef _KEEP_RCS_ID
static char *opt_dnarcs_id =         opt_dna_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

// forward declaration
extern WN_MAP Parent_Map;
class DNAGRAPH;
class DNA_NODE;
class EH_TABLE;
class INLCXT;
class IPSA;
class RNA_NODE;
class SRCPOS_HANDLE;
class VIRFUNC_INFO;
class VSA_ADDRESS_INFO;
class VSYM_TRACKER;
class HEAP_OBJ_REP;
class VSYM_FLD_REP;
class VSYM_OBJ_REP;
class GLOB_REF_MGR;
class GLOB_REF_INFO;
class CLASS_INFO;
class RBC_BASE;
typedef mempool_allocator<GLOB_REF_INFO *> GLOB_REF_ALLOCATOR;
typedef vector<GLOB_REF_INFO *, GLOB_REF_ALLOCATOR> GLOB_REF_LIST;
typedef uint64_t HO_ANNOT;

typedef enum {
  FOR_NONSPECIFIC = 0x00000000,
  FOR_ILOD_BASE   = 0x00000001,
  FOR_ISTOR_BASE  = 0x00000002,
  FOR_ILOD_REF    = 0x00000004,
  FOR_ISTOR_MOD   = 0x00000008,
  FOR_VSYM_BASE   = 0x00000010,
  FOR_PARM_BASE   = 0x00000020,
  FOR_DIVISOR     = 0x10000000,
  FOR_NPD         = 0x20000000,
  FOR_UIV         = 0x40000000,
  FOR_AOB         = 0x80000000,
} ILODSTORBASE;

enum _side_effect_attr {
  REF_NONE        = 0x00000000,
  REF_ILOAD       = 0x00000001,            // parameter used in ILOAD
  REF_ISTORE      = 0X00000002,            // parameter used in ISTORE
  REF_COPIED      = 0x00000004,            // parameter copied to another PTR
  REF_PTRPASSED   = 0x00000008,            // parameter of PTR type passed to a callee
  REF_RETURN      = 0x00000010,            // parameter as return value
  REF_MALLOCED    = 0x00000020,            // parameter of PTR type malloced
  REF_FREED       = 0x00000040,            // parameter of PTR type freed
  REF_BASE        = 0x00000080,            // parameter as base ptr
  REF_FLDNM       = 0x00000100,            // parameter as field name
  REF_FLDID       = 0x00000200,            // parameter as field id
  REF_GLOBREF     = 0x00000400,            // global is referenced
  REF_GLOBCHG     = 0x00000800,            // global's state changed

  ARG_LDA         = 0x00001000,            // argument with LDA
  ARG_LDA_PT_UDEF = 0x00002000,            // argument of LDA w/ its points-to uninitialized
  ARG_PTR         = 0x00004000,            // argument of pointer type
  ARG_PTR_UNIQ    = 0x00008000,            // argument of pointer type which points to unique mem
  ARG_PTR_PT_UDEF = 0x00010000,            // argument of ptr w/ its points-to uninitialized

  ARG_VALUE_IVAD  = 0x00100000,            // argument is value invalide addr
  ARG_VALUE_MDANG = 0x00200000,            // argument is value maydangling
  ARG_VALUE_UDEF  = 0x00400000,            // argument is undefined
  ARG_VALUE_MAYD  = 0x00800000,            // argument is maydef

  ARG_REF_MERGED  = 0x10000000,            // merged ARG and REF flags
  ARG_TAINTED     = 0x20000000,            // argument contains a tainted value
  LDA_VAL_REFED   = ARG_REF_MERGED|ARG_LDA|REF_ILOAD, // lda value get referenced in callee
  LDA_VAL_MODED   = ARG_REF_MERGED|ARG_LDA|REF_ISTORE,// lda value get modified in callee
  LDA_ADDR_SAVED  = ARG_REF_MERGED|ARG_LDA|REF_COPIED,// lda address get copied in callee
  PT2_VAL_REFED   = ARG_REF_MERGED|ARG_PTR|REF_ILOAD, // points-to value get referenced in callee
  PT2_VAL_MODED   = ARG_REF_MERGED|ARG_PTR|REF_ISTORE,// points-to value get modified in callee
  PTR_SAVED       = ARG_REF_MERGED|ARG_PTR|REF_COPIED,// pointer get copied in callee
};

#define TAG_ARRAY_INIT_SIZE 2

class TAG_NODE {
private:
  UINT64       _tag_arr[TAG_ARRAY_INIT_SIZE];  // store all tags, if set, bit on
  UINT64       _tag_init[TAG_ARRAY_INIT_SIZE];  // store each tag is setted on or off

public:
  TAG_NODE()
  {
    for (INT i = 0; i < TAG_ARRAY_INIT_SIZE; i++) {
      _tag_arr[i] = 0;
      _tag_init[i] = 0;
    }
  }
  void         Set_tag(IDTYPE tag_id, BOOL on)
  {
    Is_True(tag_id < TAG_ARRAY_INIT_SIZE * 64, ("Tag id out of length, id : %d", tag_id));
    _tag_arr[tag_id/64] = _tag_arr[tag_id/64] | (((UINT64) 0x01) << (tag_id % 64));
    if (on) {
      _tag_init[tag_id/64] = _tag_init[tag_id/64] | (((UINT64) 0x01) << (tag_id % 64));
    }
  }
  BOOL         Is_set_tag(IDTYPE tag_id)
  {
    Is_True(tag_id < TAG_ARRAY_INIT_SIZE * 64, ("Tag id out of length, id : %d", tag_id));
    return (BOOL) (_tag_arr[tag_id/64] & (((UINT64) 0x01) << (tag_id % 64)));
  }
  BOOL         Is_tag_on(IDTYPE tag_id)
  {
    Is_True(tag_id < TAG_ARRAY_INIT_SIZE * 64, ("Tag id out of length, id : %d", tag_id));
    return (BOOL) (_tag_init[tag_id/64] & (((UINT64) 0x01) << (tag_id % 64)));
  }
  TAG_NODE*    Merge(TAG_NODE *tag_node)
  {
    Is_True(tag_node, ("Merge tag node failed, tag node is NULL."));
    if (!tag_node)
      return NULL;
    for (INT i = 0; i < TAG_ARRAY_INIT_SIZE; i++) {
      _tag_arr[i] |= tag_node->_tag_arr[i];
      _tag_init[i] |= tag_node->_tag_init[i];
    }
    return this;
  }
  void         Collect_all_tags(std::vector<IDTYPE> &tag_id_vec)
  {
    for (IDTYPE i = 0; i < TAG_ARRAY_INIT_SIZE; i++) {
      if (_tag_arr[i] == 0)
        continue;
      for (IDTYPE j = 0; j < 64; j++) {
        IDTYPE index = i*64 + j;
        if (_tag_arr[index/64] & (((UINT64) 0x01) << (index % 64)))
          tag_id_vec.push_back(index);
      }
    }
  }
  BOOL         Is_set_any_tags()
  {
    for (INT i = 0; i < TAG_ARRAY_INIT_SIZE; i++) {
      if (_tag_arr[i] != 0) {
        return TRUE;
      }
    }
    return FALSE;
  }
  void         Print(FILE *fp=stderr)
  {
    for (INT i = TAG_ARRAY_INIT_SIZE - 1; i >= 0; i--) {
      fprintf(fp, "%16llx", _tag_arr[i]);
    }
    fprintf(fp, "\n");
  }
};

class VAR_NODE {
private:

  ST_IDX   _st_idx;                       // IDX in symtab; TODO: verify uniqueness w/i file
  UINT32   _flags;                        // attributes of this parameter
  CODEREP *_cr;                           // point to CR in the entry_chi list of the PU
                                          // it is not referenced if the _cr remains NULL
  union {
    HO_ANNOT _heap_annot;                 // heap_obj annotation for input formal(DNA)/actual before call(RNA)
    INT64    _value;                      // const value for in input formal(DNA)/actual before call(RNA)
  } _memsz_value_in;
  union {
    HO_ANNOT _heap_annot;                 // heap_obj annotation for output formal(DNA)/actual after call(RNA)
    INT64    _value;                      // const value for in output formal(DNA)/actual after call(RNA)
  } _memsz_value_out;

  TAG_NODE  _tag_node;
  
  VAR_NODE(void);                         // REQUIRED UNDEFINED UNWANTED methods
  VAR_NODE(const VAR_NODE&);              // REQUIRED UNDEFINED UNWANTED methods
  VAR_NODE& operator = (const VAR_NODE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  VAR_NODE(ST_IDX stid, CODEREP *cr): _st_idx(stid), _cr(cr)
  {
    _flags = REF_NONE;
    _memsz_value_in._value = 0;
    _memsz_value_out._value = 0;
  }
  ~VAR_NODE(void) { }

  ST_IDX   St_idx(void) const             { return _st_idx; }
  UINT32   Flags(void) const              { return _flags; }
  UINT32   Set_flag(UINT32 f)             { _flags |= f; return _flags; }
  CODEREP *Cr(void) const                 { return _cr; }
  void     Set_cr(CODEREP *cr)            { _cr = cr; }
  INT64    Value_in(void) const           { return _memsz_value_in._value; }
  void     Set_value_in(INT64 v)          { _memsz_value_in._value = v; }
  HO_ANNOT Hoa_in() const                 { return _memsz_value_in._heap_annot; }
  void     Set_hoa_in(HO_ANNOT hoa)       { _memsz_value_in._heap_annot = hoa;  }
  INT64    Value_out(void) const          { return _memsz_value_out._value; }
  void     Set_value_out(INT64 v)         { _memsz_value_out._value = v; }
  HO_ANNOT Hoa_out() const                { return _memsz_value_out._heap_annot; }
  void     Set_hoa_out(HO_ANNOT hoa)      { _memsz_value_out._heap_annot = hoa;  }
  void     Set_tag(IDTYPE tag_id, BOOL on){ _tag_node.Set_tag(tag_id, on); }
  BOOL     Is_set_tag(IDTYPE tag_id)      { return _tag_node.Is_set_tag(tag_id); }
  BOOL     Is_tag_on(IDTYPE tag_id)       { return _tag_node.Is_tag_on(tag_id); }
  TAG_NODE*  Get_tag_node()               { return &_tag_node; }
  TAG_NODE*  Merge_tag_node(TAG_NODE *tag_node) { _tag_node.Merge(tag_node); return &_tag_node; }
  CODEREP *Ipreg(void) const;
  void     Print(FILE *fp = TFile) const;

}; // class VAR_NODE 

enum _var_init_id { VAR_INIT_ID = 1 };

// =============================================================================
// 
// PDV: predicated value expression stores the side effect that is returned to
//      the caller.  Besides the return statement, the parameter of reference
//      kind could also serve as the mechanism to return value to the caller.
//      _flags tells what kind of return mechanism.
//
// =============================================================================
enum _return_value_attr {           // Return kind, encoded in _kind
  BY_RETURNSTMT = 0x00000001,
  BY_PARAMETER  = 0x00000002,
  BY_GLOBALVAR  = 0x00000004,
  BY_FREEIPARM  = 0x00000008,       // Track free heap memroy by iparm
  BY_FREEGLOBL  = 0x00000010,       // Track free heap memory by global
};

class AR {
private:

  INT32   _lb;
  INT32   _ub;
  UINT32  _stride;

  AR(void);                         // REQUIRED UNDEFINED UNWANTED methods
  AR(const AR&);                    // REQUIRED UNDEFINED UNWANTED methods
  AR& operator = (const AR&);       // REQUIRED UNDEFINED UNWANTED methods

public:
  AR(INT32 lb, INT32 ub, UINT32 stride): _lb(lb), _ub(ub), _stride(stride) { }
  ~AR(void) { }

}; // class AR

class PDV_NODE {
private:
  enum {
    PN_VALUE = BY_RETURNSTMT | BY_PARAMETER | BY_GLOBALVAR, // returns value
    PN_RESOURCE = BY_FREEIPARM | BY_FREEGLOBL,              // returns resource
  };

  UINT32   _flags;                        // attr defined in 'side_effect_attr'
  UINT16   _kind;                         // return value kind
  UINT16   _param;                        // which parameter is ref and assigned
  union {
    AR      *_range;                      // how big the range of ilod/istor
    HO_ANNOT _heap_annot;                 // heap_obj size if the PDV is associated with an heap obj on stmt Lhs
  } _u0;
  union {
    BB_NODE *_predicate;                  // NULL means unconditional for retval
  } _u1;
  STMTREP *_stmt;                         // statement to store the return value

  PDV_NODE(void);                         // REQUIRED UNDEFINED UNWANTED methods
  PDV_NODE(const PDV_NODE&);              // REQUIRED UNDEFINED UNWANTED methods
  PDV_NODE& operator = (const PDV_NODE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  PDV_NODE(BB_NODE *pd, STMTREP *s, UINT kind, UINT idx)
    : _flags(REF_NONE), _kind(kind), _param(idx), _stmt(s)
  {
    _u0._range = NULL;
    _u1._predicate = pd;
  }
  ~PDV_NODE(void) { }

  HO_ANNOT Hoa() const                    { return _u0._heap_annot;}
  void     Set_hoa(HO_ANNOT h)            { _u0._heap_annot = h; }
  BB_NODE *Predicate(void) const          { return _u1._predicate; }
  STMTREP *Stmt(void) const               { return _stmt; }
  void     Set_stmt(STMTREP *s)           { _stmt = s; }
  UINT32   Flags(void) const              { return _flags; }
  UINT32   Set_flag(UINT32 f)             { _flags |= f; return _flags; }
  void     Clear_flag(UINT32 f)           { _flags &= ~f; }
  UINT32   Kind() const                   { return _kind; }
  BOOL     Is_value() const               { return (_kind & PN_VALUE) != 0; }
  BOOL     Is_resource() const            { return (_kind & PN_RESOURCE) != 0; }
  UINT32   Oparam(void) const             { return _param; }
  void     Set_oparam(IDTYPE idx)         { _param = idx; }
  UINT32   Iparam(void) const             { return _param; }
  void     Set_iparam(IDTYPE idx)         { _param = idx; }
  UINT32   Update_flags(DNA_NODE* dna);
  BOOL     Copy_flag_from_cr(CODEREP* cr);
  BOOL     Copy_flag_to_cr(CODEREP* cr) const;
  void     Print(FILE *fp = stderr) const;

}; // class PDV_NODE 

enum _pdv_init_id { PDV_INIT_ID = 1 };
enum _pdv_predicate { PDV_PRED_TRUE = (UINT)NULL };

typedef mempool_allocator<PDV_NODE*> PNODE_ALLOCATOR;
typedef vector<PDV_NODE*, PNODE_ALLOCATOR> PNODE_VECTOR;
typedef mempool_allocator<RNA_NODE*> RNODE_ALLOCATOR;
typedef vector<RNA_NODE*, RNODE_ALLOCATOR> RNODE_VECTOR;
typedef mempool_allocator<VAR_NODE*> VNODE_ALLOCATOR;
typedef vector<VAR_NODE*, VNODE_ALLOCATOR> VNODE_VECTOR;
typedef mempool_allocator<STMTREP*>  STMTR_ALLOCATOR;
typedef vector<STMTREP*,  STMTR_ALLOCATOR> STMTR_VECTOR;
typedef enum { INVALID_VAR_IDX = 0 } VAR_IDX;
typedef vector< std::stack< STMTREP* > > STMTREP_STACK;
typedef std::stack <RNA_NODE*> CALL_STACK;
typedef mempool_allocator<UINT32> DNA_IDX_ALLOCATOR;
typedef hash_set<UINT32, __gnu_cxx::hash<UINT32>, __gnu_cxx::equal_to<UINT32>, DNA_IDX_ALLOCATOR> DNA_IDX_SET;

// The DNA contains requirements for Rule Based Checking
enum _dna_rbc_facts {
  DNA_RBC_NONE          = 0x00000000,  // not a RBC model
  DNA_RBC_ASSERT        = 0x00000001,  // contains an rbc_assert call
  DNA_RBC_MODEL         = 0x00000002,  // contains an model_decl call
  DNA_RBC_APL_RULE      = 0x00000004,  // contains an rbc_apply_rule call
  DNA_JNI_MODEL         = 0x00000008,  // contains JNI model
  DNA_FSM_MODELLED      = 0x00000010,  // contains FSM model
  DNA_RBC_RULE_CFG      = 0x00000020,  // contains an rbc rule related call, declare, disable, exception
  DNA_RBC_SE_EVAL       = 0x00000040,  // contains an evaluation rule
  DNA_RBC_TAG_CREATE    = 0x00000080,  // create tag, set tag on parm or ret
  DNA_RBC_TAG_OP        = 0x00000100,  // tag operation, tag or, tag copy
  DNA_THREAD_ENTRY      = 0x00000200,  // thread entry
  DNA_ISR_ENTRY         = 0x00000400,  // ISR (interrupt service routine) entry
  DNA_RBC_ASSERT_DNA    = 0x00000800,  // contains an rbc_assert on dna (different with
                                       // DNA_RBC_ASSET which is triggered by rna)
  DNA_RBC_FUNC_TAG      = 0x00001000,  // contains func tag
  DNA_RBC_GLOBAL_USED   = 0x00002000,  // function has global variable used model
  DNA_RBC_DEFINE_CALL   = 0x00004000,  // function has define call model
};

// The following summary or annotation of a function
enum _dna_attributes {
  DNA_SIDE_EFFECT_UNINIT  = 0X00000000, // nothing analyzed yet
  DNA_MEMORY_ALLOCATION   = 0x00000001, // Allocate memory and need caller to free
  DNA_MEMORY_DEALLOCATION = 0x00000002, // Free memory passed in from caller
  DNA_PARAM_NO_PASSTHRU   = 0x00000004, // Input parameter gets returned to caller
  DNA_SINGLE_RETURN       = 0x00000008, // only one return statement
  DNA_HAS_ASSUMPTION      = 0x00000010, // There's an rbc_assume in this function
  DNA_RT_PARM_UIV         = 0x00000020, // Report uiv for parm at cg root
  DNA_RT_DATA_NPD         = 0x00000040, // Report npd for global or static
  DNA_RT_DATA_MSF         = 0x00000080, // Report msf for global or static
  DNA_RT_MALLOC_MAYNPD    = 0x00000100, // Report may npd for malloc return value
  DNA_IN_RECURSION        = 0x00000200, // Inside the recursion circle
  DNA_RECURSION_ENTRY     = 0x00000400, // The entry of recursion circle in PRE ODER traversal
  DNA_ADDR_TAKEN          = 0x00000800, // The function address is saved or passed in somewhere
  DNA_SANITIZE_DATA       = 0x00001000, // function performs data sanitizing
  DNA_MAY_SLEEP           = 0x00002000, // function may sleep
  DNA_ATOMIC_REGION_BEGIN = 0x00004000, // function indicates the beginning of an atomic region
  DNA_ATOMIC_REGION_END   = 0x00008000, // function indicates the end of an atomic region
  DNA_ATOMIC_FUNC         = 0x00010000, // function itself is in atomic region
  DNA_IS_CONSTRUCTOR      = 0x00020000, // function is a constructor
  DNA_MEMORY_MAY_DEALLOC  = 0x00040000, // function may free memory passed from caller
  DNA_CHECK_ZERO          = 0x00080000, // function return true if param is zero, HACK for Jan 2020 release
  DNA_CHECK_NOT_ZERO      = 0x00100000, // function return true if param is not zero, HACK for Jan 2020 release
  DNA_NONE_FUNCTIONAL     = 0x00200000, // function that implements rbc model/assert
  DNA_CALLED_IN_LOOP      = 0x00400000, // function is called in loop
  DNA_ERRNO_SETTING       = 0x00800000, // function is errno-setting
  DNA_IN_MAIN_THREAD      = 0x01000000, // function runs in main thread
  DNA_IN_HELP_THREAD      = 0x02000000, // function runs in helper thread, which is only 1
  DNA_IN_WORK_THREAD      = 0x04000000, // function runs in worker threads
  DNA_IN_ISR              = 0x08000000, // function runs in ISR interrupt service routine
  DNA_RUNS_IN_MASK        = 0x0F000000, // mask for flags indicate where the function runs in
};

typedef enum _heapsize {
  HSZ_UNKNOWN = 0,
  HSZ_SINGLE  = 1,
  HSZ_MAX     = 2,
  HSZ_MIN     = 4,
  HSZ_EXACT   = 8,
  HSZ_MULTI   = HSZ_MAX|HSZ_MIN|HSZ_EXACT,
} HEAPSIZE;

typedef enum _heapstate {
  HS_NONE   = 0,
  HS_GOOD   = 1,
  HS_DANGLE = 2,
} HEAPSTATE;

typedef enum _heapinq {
  HINQ_NONE = 0,
  HINQ_STATE= 1,
  HINQ_HOR  = 2,
} HEAPINQ;

typedef enum _action {
  IN_NONE,
  IN_PARM,
  IN_ILOAD_BASE,
  IN_ISTORE_BASE,
} ACTION;

//==============================================================================
//
// STPATH class is used to annotate the WN/STMTREP/CODEREP with the original 
// st and its path before it is deleted or replaced by copy_prop
//
//==============================================================================
#define MAX_USE_CNT 63
#define MAX_PATH_SIZE 1023
class STPATH {
  typedef mempool_allocator<SRCPOS_NODE> SRCPOS_ALLOCATOR;
  typedef vector<SRCPOS_NODE, SRCPOS_ALLOCATOR> SRCPOS_NODE_VECTOR;
private:
  ST_IDX       _st_idx;
  UINT16       _field_id;
  UINT16       _use_cnt:6;    // MAX_USE_CNT 2^6 - 1
  const char  *_st_name;
  UINT16       _path_size:10;
  SRCPOS_NODE  _path[0];  // variable length member


  // private constructor only, use New_instance instead
  STPATH(ST_IDX idx, UINT16 field_id, const char *st_name, UINT16 size, MEM_POOL *pool) :
    _st_idx(idx),_field_id(field_id), _use_cnt(1), _st_name(st_name), _path_size(size)
  {
  }

  STPATH(void);                         // REQUIRED UNDEFINED UNWANTED methods
  STPATH(const STPATH&);                // REQUIRED UNDEFINED UNWANTED methods
  STPATH& operator = (const STPATH&);   // REQUIRED UNDEFINED UNWANTED methods
  
public:
  
  void          Set_idx(ST_IDX idx)          { _st_idx = idx; }
  void          Set_Field_id(UINT16 id)      { _field_id = id; }
  ST_IDX        St_idx() const               { return _st_idx; }
  const char   *St_name() const              { return _st_name; }
  UINT16        Field_id() const             { return _field_id; }
  UINT16        Path_size(void) const        { return _path_size; }
  SRCPOS_NODE&  Path(int i)                  { return _path[i]; }
  void          Add_path(SRCPOS_NODE vspos, UINT16 index)
  {
    _path[index] = vspos;
  }
  void          Inc_use_cnt(void)
  {
    Is_True(_use_cnt <= MAX_USE_CNT, ("use count is out of range"));
    _use_cnt++;
  }
  void          Set_use_cnt(UINT8 n)
  {
    Is_True(n <= MAX_USE_CNT, ("use count is out of range"));
    _use_cnt=n;
  }
  void          Dec_use_cnt(void)            { if(_use_cnt) _use_cnt--;}
  UINT8         Use_cnt(void) const          { return _use_cnt; }
  void          Update_sym(ST_IDX st_idx, UINT16 field_id, const char *st_name)
  {
    _st_idx = st_idx;
    _field_id = field_id;
    _st_name = st_name;
  }

  static
  const char   *Find_cr_stname(STRING_BUFFER *buf, OPT_STAB *opt_stab, CODEREP *cr, STMTREP *sr);
  static
  STPATH*       New_instance(ST_IDX idx, UINT16 field_id, UINT16 size, MEM_POOL* pool)
  {
    char *name = ST_name(idx);
    char *st_name = (char *) MEM_POOL_Alloc(pool, strlen(name) + 1);
    memcpy(st_name, name, strlen(name) + 1);
    return CXX_NEW_VARIANT(STPATH(idx, field_id, st_name, size, pool), size*sizeof(SRCPOS_NODE), pool);
  }

  static STPATH *New_instance(ST_IDX st_idx, UINT16 field_id, const char *st_name, UINT16 size, MEM_POOL *pool)
  {
    return CXX_NEW_VARIANT(STPATH(st_idx, field_id, st_name, size, pool), size*sizeof(SRCPOS_NODE), pool);
  }

  static
  STPATH*       New_instance(STPATH * sp, MEM_POOL* pool);
  void          Print(FILE *fp); 
}; // class STPATH

typedef uint64_t SRCR_ID;  //  (stmtrep_id  << 32 ) + coderep_id
typedef pair<SRCR_ID, STPATH*> SRCR2STPATH;
typedef mempool_allocator<SRCR2STPATH>  SRCR2STPATH_ALLOCATOR;
typedef hash_map<SRCR_ID, STPATH*, __gnu_cxx::hash<SRCR_ID>, std::equal_to<SRCR_ID>, SRCR2STPATH_ALLOCATOR> STPATH_MAP;

typedef pair<IDTYPE, ST_IDX>   ID_ST_PAIR;
typedef mempool_allocator<ID_ST_PAIR>   ID_ST_ALLOCATOR;
typedef hash_map<IDTYPE, ST_IDX, __gnu_cxx::hash<IDTYPE>, std::equal_to<IDTYPE>, ID_ST_ALLOCATOR> ID_ST_MAP;

// =============================================================================
//
// INLCXT_NODE:
// copy preopt's inline context from local pool to DNA's pool with INLCXT_NODE
//
// =============================================================================
class INLCXT_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( INLCXT_NODE )
private:
  INLCXT _inlcxt;

public:
  INLCXT_NODE(const INLCXT& inlcxt) : _inlcxt(inlcxt) { }
  INLCXT_NODE(INLCXT* parent, SRCPOS linenum, ST_IDX call_st)
   : _inlcxt(parent, linenum, call_st, TRUE) { }

  INLCXT* Inlcxt()                 { return &_inlcxt;       }
  BOOL    Equal(const INLCXT* rhs) { return _inlcxt == rhs; }
};

#define MAX_CALL_STACK_SIZE 1024

// =============================================================================
//
// DNA_NODE: it captures information that could potentially bring side effects
//
// =============================================================================
//class DNA_NODE : public SLIST_NODE {
class DNA_NODE {
  friend class CALLEE_ITER;
  friend class CALLER_ITER;
  enum _dna_file_idx { INVALID_DNA_FILE_IDX = 0 };

//  DECLARE_SLIST_NODE_CLASS ( DNA_NODE )

private:
  IDTYPE           _dna_idx;              // global index into dna table
  IDTYPE           _file_idx;             // index into file table
  IDTYPE           _pu_idx;               // index into procedure table
  IDTYPE           _fname_idx;            // function name index
  IDTYPE           _rbc_ref_idx;          // reference dna idx used in rbc nodes map
  MEM_POOL        *_mem_pool;             // global mem pool
  ST              *_st;                   // symtab entry for this function
                                          // st->Is_varargs_func() #imprecise sa
  COMP_UNIT       *_comp_unit;            // the main body of the PU
  CLASS_INFO      *_cls_info;             // the class info pointer
  WHIRL_PU_CONTEXT _context;              // context for this function
  RNODE_VECTOR     _call_list;            // list of rna/call_sites
  RNODE_VECTOR     _clby_list;            // list of rna/callers
  VNODE_VECTOR     _parm_list;            // list of parameters
  VNODE_VECTOR     _glob_list;            // list of global/static in this PU
  PNODE_VECTOR     _retv_list;            // list of value returned by return and output param
  STMTR_VECTOR     _rets_list;            // list of RETURN stmt w/o value
  PNODE_VECTOR     _rgvl_list;            // list of global/static value ar return point
  ID_ST_MAP        _vtbl_map;             // map cr to its vtable if the object is contructed here
  UINT32           _rbc_flags;            // attributes of RBC model
  UINT32           _flags;                // attributes of this function
  INT              _visit;                // visit token
  INT              _trav;                 // trav token
  STPATH_MAP       _stpath_map;           // map ST's PATH to WN/STMTREP/CODEREP so that when the
                                          // data flow from WN/STMTREP/CODEREP to ST is broken, the
                                          // ST can still be retrieved for warning reporting
  STPATH_MAP       _wn_stpath_map;        // map ST's PATH to wn
  INLCXT_NODE     *_inlcxt_list;          // saved INLCXT node which contains inline ctx for copy propagation
  UINT32           _rbc_api_parm_ofst;    // C/C++ API include "this" pointer, Java does not
                                          // set 1 for C/C++, 0 for Java
  mutable UINT32   _stack_size;           // stack size for current function
  STRING_VEC      *_fsm_list;             // FSMs this DNA_NODE is bound
  TAG_NODE         _ret_tag_node;         // return value's tag, one tag one bit

private:
  DNA_NODE(const DNA_NODE&);              // REQUIRED UNDEFINED UNWANTED methods
  DNA_NODE& operator = (const DNA_NODE&); // REQUIRED UNDEFINED UNWANTED methods

  IDTYPE        No_passthru(void)              { Set_flag(DNA_PARAM_NO_PASSTHRU); return INVALID_VAR_IDX; }
  BOOL          Tracing(void)                  { return Get_Trace(TP_WOPT2, VSA_DUMP_FLAG); }
  UINT32        Calc_stack_size() const;
  CODEREP      *Is_simple_expression(CODEREP *x, INT64 *adjuster, BOOL deeply) const;

public:
  DNA_NODE(void) : _file_idx(INVALID_DNA_FILE_IDX) {} // USE only for CONTEXT SWITCH
  DNA_NODE( IDTYPE dna_id, IDTYPE puid, ST *st, IDTYPE fname_idx, COMP_UNIT *cu, MEM_POOL* mp):
    _dna_idx(dna_id), _file_idx(INVALID_DNA_FILE_IDX),
    _pu_idx(puid), _fname_idx(fname_idx), _mem_pool(mp),
    _st(st), _comp_unit(cu), _cls_info(NULL),
    _call_list(VAR_INIT_ID, (RNA_NODE*)NULL, RNODE_VECTOR::allocator_type(mp)),
    _clby_list(VAR_INIT_ID, (RNA_NODE*)NULL, RNODE_VECTOR::allocator_type(mp)),
    _parm_list(VAR_INIT_ID, (VAR_NODE*)NULL, VNODE_VECTOR::allocator_type(mp)),
    _glob_list(VAR_INIT_ID, (VAR_NODE*)NULL, VNODE_VECTOR::allocator_type(mp)),
    _retv_list(PDV_INIT_ID, (PDV_NODE*)NULL, PNODE_VECTOR::allocator_type(mp)),
    _rets_list(STMTR_VECTOR::allocator_type(mp)),
    _rgvl_list(PDV_INIT_ID, (PDV_NODE*)NULL, PNODE_VECTOR::allocator_type(mp)),
    _vtbl_map(5, __gnu_cxx::hash<IDTYPE>(), std::equal_to<ST_IDX>(), ID_ST_ALLOCATOR(mp)),
    _stpath_map(5, __gnu_cxx::hash<SRCR_ID>(), std::equal_to<SRCR_ID>(), SRCR2STPATH_ALLOCATOR(mp)),
    _wn_stpath_map(5, __gnu_cxx::hash<SRCR_ID>(), std::equal_to<SRCR_ID>(), SRCR2STPATH_ALLOCATOR(mp))
  {
    _rbc_flags = DNA_RBC_NONE;
    _flags = DNA_SIDE_EFFECT_UNINIT;
    // Set_report_malloc_maynpd();            // turn on by default till RBC handles it
    //Collect_parm_list(cu->Input_tree());
    Save_context();
    _visit = 0;
    _trav = 0;
    _inlcxt_list = NULL;
    if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU()))
      _rbc_api_parm_ofst = 1;
    else
      _rbc_api_parm_ofst = 0;
    _stack_size = 0;
    _rbc_ref_idx = 0;
    _fsm_list = NULL;
    if(PU_is_constructor(Get_Current_PU())) {
      Set_flag(DNA_IS_CONSTRUCTOR);
    }
    if(PU_is_mainpu(Get_Current_PU())) {
      Set_flag(DNA_IN_MAIN_THREAD);
    }
    if(Is_in_rbc_file()) {
      Set_flag(DNA_NONE_FUNCTIONAL);
    }
  }
  ~DNA_NODE(void);

  // access functions
  IDTYPE        Dna_idx(void) const            { return _dna_idx; }

  IDTYPE        File_idx(void) const           { return _file_idx; }
  void          Set_file_idx(IDTYPE fidx)      { _file_idx = fidx; }

  IDTYPE        Pu_idx(void) const             { return _pu_idx; }
  PU           *Pu(void) const                 { return Pu_ptr(_file_idx, _pu_idx); }

  STRING        Fname(void) const              { return Str_ptr(_file_idx, _fname_idx); }

  void          Set_rbc_ref_idx(IDTYPE idx)    { _rbc_ref_idx = idx; }
  IDTYPE        Get_rbc_ref_idx(void) const    { return _rbc_ref_idx; }
  BOOL          Has_rbc_nodes(void) const      { return _rbc_ref_idx != 0; }

  MEM_POOL     *Mem_pool() const               { return _mem_pool; }

  ST           *St(void) const                 { return _st; }

  COMP_UNIT    *Comp_unit(void) const          { return _comp_unit; }
  void          Set_comp_unit(COMP_UNIT* cu)   { _comp_unit = cu; }

  CLASS_INFO   *Cls_info(void) const           { return _cls_info; }
  void          Set_cls_info(IPSA *ipsa);

  void          Save_context(void);
  void          Restore_context(void) const;

  RNODE_VECTOR *Call_list(void)                { return &_call_list; }
  RNODE_VECTOR *Clby_list(void)                { return &_clby_list; }
  VNODE_VECTOR *Parm_list(void)                { return &_parm_list; }
  VNODE_VECTOR *Glob_list(void)                { return &_glob_list; }
  PNODE_VECTOR *Retv_list(void)                { return &_retv_list; }
  STMTR_VECTOR *Rets_list(void)                { return &_rets_list; }
  PNODE_VECTOR *Rgvl_list(void)                { return &_rgvl_list; }
  ID_ST_MAP    *Vtbl_map(void)                 { return &_vtbl_map;  }

  RNA_NODE     *Callsite(UINT idx)             { return _call_list[idx]; }
  RNA_NODE     *Callby(UINT idx)               { return _clby_list[idx]; }
  UINT32        Call_site_cnt(void) const      { return _call_list.size(); }
  UINT32        Clby_list_cnt(void) const      { return _clby_list.size(); }
  INT           Callsite_cnt(void) const       { return _call_list.size() - 1; }
  INT           Caller_cnt(void) const         { return _clby_list.size() - 1; }

  BOOL          Is_root(void) const            { return Caller_cnt() == 0; }
  BOOL          Is_root_entry(void) const      { return Is_root(); }   // TODO: MORE LINK WORK
  void          Inc_ref_cnt(RNA_NODE *r)       { if(std::find(_clby_list.begin(), _clby_list.end(), r) == _clby_list.end()) {
                                                   _clby_list.push_back(r);
                                                 }
                                               }
  void          Dec_ref_cnt(RNA_NODE *r)       {  _clby_list.erase(std::find(_clby_list.begin(), _clby_list.end(), r));
                                               }
  void          Add_call(RNA_NODE *r)          { _call_list.push_back(r); }

  void          Set_visit(INT cnt)             { _visit = cnt; }
  INT           Get_visit(void) const          { return _visit; }

  void          Set_trav(INT cnt)              { _trav = cnt;  }
  INT           Get_trav(void) const           { return _trav; }


  UINT32        Rbc_parm_offset(void)          { return _rbc_api_parm_ofst; }

  UINT32        Stack_size(void) const         { if (_stack_size == 0) _stack_size = Calc_stack_size();
                                                 return _stack_size; }

  void          Add_fsm_list(STRING name)      { Is_True(Is_set_rbc_flag(DNA_FSM_MODELLED),
                                                         ("DNA_NODE::Add_fsm_list() Illegal call"));
                                                 if (_fsm_list == NULL) {
                                                   _fsm_list = CXX_NEW(STRING_VEC(STRING_VEC::allocator_type(Mem_pool())),
                                                                       Mem_pool());
                                                 }
                                                 for (INT i = 0; i < _fsm_list->size(); i++) {
                                                   STRING item = (*_fsm_list)[i];
                                                   if (strcmp(item, name) == 0)
                                                     return;
                                                 }
                                                 _fsm_list->push_back(name); }
  STRING_VEC   *Fsm_list(void)                 { return _fsm_list; }

  // flag related
  BOOL          Is_set_rbc_flag(UINT32 f) const{ return _rbc_flags & f; }
  UINT32        Rbc_flags(void) const          { return _rbc_flags; }
  UINT32        Set_rbc_flag(UINT32 f)         { _rbc_flags |= f; return _rbc_flags; }
  void          Clear_rbc_flag(UINT32 f)       { _rbc_flags &= ~f; }
  BOOL          Decl_FSM_only(void) const      { return Is_set_rbc_flag(DNA_RBC_MODEL) &&
                                                        Is_set_rbc_flag(DNA_FSM_MODELLED); }

  BOOL          Is_set(UINT32 f) const         { return _flags & f; }
  UINT32        Flags(void) const              { return _flags; }
  UINT32        Set_flag(UINT32 f)             { _flags |= f; return _flags; }
  void          Clear_flag(UINT32 f)           { _flags &= ~f; }

  BOOL          Deallocate(void) const         { return _flags & (DNA_MEMORY_DEALLOCATION |
                                                                  DNA_MEMORY_MAY_DEALLOC); }
  BOOL          May_deallocate(void) const     { return _flags & DNA_MEMORY_MAY_DEALLOC; }
  void          Set_deallocate(BOOL maybe)     { Set_flag(maybe ? DNA_MEMORY_MAY_DEALLOC : DNA_MEMORY_DEALLOCATION); }

  void          Set_non_functional(void)       { _flags |= DNA_NONE_FUNCTIONAL; }
  BOOL          Non_functional(void) const     { return Is_set(DNA_NONE_FUNCTIONAL); }

  void          Set_report_parm_uiv(void)      { _flags |= DNA_RT_PARM_UIV; }
  BOOL          Report_parm_uiv(void)const     { return Is_root() && (_flags & DNA_RT_PARM_UIV); }

  void          Set_report_data_msf(void)      { _flags |= DNA_RT_DATA_MSF; }
  BOOL          Report_data_msf(void)const     { return Is_root() && (_flags & DNA_RT_DATA_MSF); }

  void          Set_report_data_npd(void)      { _flags |= DNA_RT_DATA_NPD; }
  BOOL          Report_data_npd(void)const     { return (_flags & DNA_RT_DATA_NPD); }

  void          Set_report_malloc_maynpd(void) { _flags |= DNA_RT_MALLOC_MAYNPD; }
  BOOL          Report_malloc_maynpd(void)const{ return (_flags & DNA_RT_MALLOC_MAYNPD); }

  BOOL          Is_called_in_loop(void)        { return (_flags & DNA_CALLED_IN_LOOP); }

  UINT32        Thread_flags(void) const       { return (_flags & DNA_RUNS_IN_MASK); }
  // Exec_in_parallel:
  // 1. Exec in work thread or ISR, Thread_flags() >= DNA_IN_WORK_THREAD
  // 2. Exec in both main thread and help thread, Thread_flags() == (DNA_IN_MAIN_THREAD |  DNA_IN_HELP_THREAD)
  BOOL          Exec_in_parallel(void) const   { return Thread_flags() > DNA_IN_HELP_THREAD; }
  BOOL          Exec_in_thread(void) const     { return (_flags & DNA_IN_HELP_THREAD) ||
                                                        (_flags & DNA_IN_WORK_THREAD); }
  BOOL          Exec_in_isr(void) const        { return (_flags & DNA_IN_ISR); }

  // parm flags
  UINT32        Parm_flags(IDTYPE i) const                 { Is_True_Ret(i < _parm_list.size(), ("outof bound"), 0);
                                                             return _parm_list[i]->Flags(); }
  UINT32        Set_parm_flag(IDTYPE i,UINT32 f)           { Is_True_Ret(i < _parm_list.size(), ("outof bound"), f);
                                                             return _parm_list[i]->Set_flag(f); }
  BOOL          Is_parm_original(IDTYPE i)                 { Is_True_Ret(i < _parm_list.size(), ("outof bound"), FALSE);
                                                             return _parm_list[i]->Cr() != NULL; }
  BOOL          Is_set_parm_flag(IDTYPE i, UINT32 f)       { Is_True_Ret(i < _parm_list.size(), ("outof bound"), FALSE);
                                                             return _parm_list[i]->Flags()&f; }
  UINT32        Glob_flags(IDTYPE i) const                 { Is_True_Ret(i < _glob_list.size(), ("outof bound"), 0);
                                                             return _glob_list[i]->Flags(); }
  UINT32        Set_glob_flag(IDTYPE i, UINT32 f)          { Is_True_Ret(i < _glob_list.size(), ("outof bound"), f);
                                                             return _glob_list[i]->Set_flag(f); }
  BOOL          Is_set_glob_flag(IDTYPE i, UINT32 f)       { Is_True_Ret(i < _glob_list.size(), ("outof bound"), FALSE);
                                                             return _glob_list[i]->Flags()&f; }

  // tag
  void          Set_parm_tag(IDTYPE i, IDTYPE id, BOOL on) { Is_True_Ret(i < _parm_list.size(), ("outof bound"));
                                                             _parm_list[i]->Set_tag(id, on); }
  BOOL          Is_set_parm_tag(IDTYPE i, IDTYPE tag_id)   { Is_True_Ret(i < _parm_list.size(), ("outof bound"), FALSE);
                                                             return _parm_list[i]->Is_set_tag(tag_id); }
  BOOL          Is_parm_tag_on(IDTYPE i, IDTYPE tag_id)    { Is_True_Ret(i < _parm_list.size(), ("outof bound"), FALSE);
                                                             return _parm_list[i]->Is_tag_on(tag_id); }
  void          Set_ret_tag(IDTYPE tag_id, BOOL on)        { _ret_tag_node.Set_tag(tag_id, on); }
  BOOL          Is_set_ret_tag(IDTYPE tag_id)              { return _ret_tag_node.Is_set_tag(tag_id); }
  BOOL          Is_ret_tag_on(IDTYPE tag_id)               { return _ret_tag_node.Is_tag_on(tag_id); }
  TAG_NODE     *Get_ret_tag_node()                         { return &_ret_tag_node; }

  BOOL          Has_annots()                               { return _cls_info ? _cls_info->Has_annot() : FALSE; }
  BOOL          Get_annots(vector<STRING> &annots) const   { return _cls_info ? _cls_info->Get_meth_annots(Fname(), annots)
                                                                              : FALSE;
                                                           }

  BOOL          Is_in_rbc_file(void) const                 { FILE_INFO fi;
                                                             WHIRL_FILE_MANAGER* fmgr = WHIRL_FILE_MANAGER::Get();
                                                             if (fmgr)
                                                               fi = fmgr->Get_file(_file_idx).Whirl_file_info();
                                                             else
                                                               fi = File_info;
                                                             return FILE_INFO_is_rbc(fi);
                                                           }

  EH_TABLE     *EH_table(void) const                       { return Comp_unit()->EH_table(); }
  STR_IDX       Get_eh_throw_type(STMTREP* call, TY_IDX *throw_ty = NULL) const;

  void          Check_caller_arg_val_attr(IPSA *ipsa, IDTYPE which_arg, RNA_NODE *rna,
                                          CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                          ILODSTORBASE kind, VSYM_TRACKER *tracker = NULL);
  void          Check_coderep_attr(IPSA *ipsa, CODEREP *x, STMTREP *stmt,
                                   CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind);
  void          Check_global_val_attr(IPSA *ipsa, UINT32 file_idx, ST_IDX st_idx, RNA_NODE* rna,
                                      CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind);
  void          Check_callers_argument_for_uiv(IPSA *ipsa,CODEREP *x,CALL_STACK& cs,SRCPOS_HANDLE *sp_h,
                                               ILODSTORBASE kind, VSYM_TRACKER *tracker = NULL);
  void          Check_callers_global_for_uiv(IPSA *ipsa, UINT32 file_idx, ST_IDX st,
                                             CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind);
  void          Check_istore_for_uiv(IPSA *ipsa, BB_NODE *curbb, IDTYPE arg, CODEREP *chi_opnd, CALL_STACK &cs,
                                     SRCPOS_HANDLE *sp_h, ILODSTORBASE kind, hash_set<IDTYPE> &visited);
  void          Check_callee_side_effects_for_xfa(IPSA *ipsa, CODEREP *x,
                                                  CALL_STACK& cs, SRCPOS_HANDLE *sp_h, ILODSTORBASE kind);
  void          Collect_callee_return_value_for_xfa(IPSA *ipsa, CODEREP *x, RNA_NODE* rna,
                                                    vector<STMTREP*>& stmts);
  void          Collect_callee_global_value_for_xfa(IPSA *ipsa, CODEREP *x, RNA_NODE* rna,
                                                    vector<STMTREP*>& stmts);
  void          Check_caller_argument_for_aob(IPSA *ipsa, IDTYPE arg, RNA_NODE *rna, CALL_STACK& cs,
                                              SRCPOS_HANDLE *sp_h, VSA_ADDRESS_INFO *info,
                                              VSYM_TRACKER *tracker = NULL);
  void          Check_caller_global_for_aob(IPSA *ipsa, UINT32 file_idx, ST_IDX st_idx,
                                            INT32 ofst, RNA_NODE* rna,
                                            CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                            VSA_ADDRESS_INFO *info, VSYM_TRACKER *tracker = NULL);
  void          Check_callers_argument_for_aob(IPSA *ipsa, CODEREP *x, CALL_STACK& cs,
                                               SRCPOS_HANDLE *sp_h, VSA_ADDRESS_INFO *info,
                                               VSYM_TRACKER *tracker = NULL);
  void          Check_callers_global_for_aob(IPSA *ipsa, UINT32 file_idx, ST_IDX st, INT32 ofst,
                                             CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                             VSA_ADDRESS_INFO *info, VSYM_TRACKER *tracker = NULL);
  void          Check_callee_side_effects_for_aob(IPSA *ipsa, CODEREP *x,
                                                  CALL_STACK& cs, SRCPOS_HANDLE *sp_h,
                                                  VSA_ADDRESS_INFO *info, VSYM_TRACKER *tracker = NULL);
  void          Check_clinit_side_effects_for_aob(IPSA *ipsa, UINT32 file_idx,
                                                  ST_IDX st_idx, CALL_STACK& cs, 
                                                  SRCPOS_HANDLE *sp_h, 
                                                  VSA_ADDRESS_INFO *info);
  BOOL          Is_path_possible_in_callee(IPSA *ipsa, RNA_NODE* rna, IDTYPE param, CODEREP* cmp,
                                           CALL_STACK& cs, VSYM_TRACKER* tracker,
                                           const PATH_SELECTED& path);

  void          Collect_parm_list(WN *wn);// from IDNAME list under FUNC_ENTRY
  void          Create_arg_lists(IPSA *ipsa);
  void          Update_arg_crs();
  void          Propagate_arg_and_ret_tags();
  void          Merge_flags_from_callee_to_rna(IPSA *ipsa);
  void          Find_referenced_vars_at_entry(COMP_UNIT *cu, BB_NODE *bb);
  void          Create_refn_4_entrychi(COMP_UNIT *cu, STMTREP *entry_chi, MEM_POOL *pool);
  //void          Create_refn_4_exitmu(COMP_UNIT *cu, STMTREP *retstmt, MEM_POOL *pool);

  IDTYPE        Find_first_param_freed(void) {
                  for (INT i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
                    if ((_parm_list[i]->Flags() & REF_FREED) != 0)
                      return i;
                  } // parm_list iterations
                  return INVALID_VAR_IDX;
                }
  CODEREP      *Find_param_cr(AUX_ID aux) {
                  for (INT i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
                    CODEREP *cr = _parm_list[i]->Cr();
                    if (cr && cr->Aux_id() == aux)
                      return cr;
                  }
                  return NULL;
                }
  CODEREP      *Find_st_param(ST_IDX st) {
                  for (INT i = VAR_INIT_ID; i < _parm_list.size(); ++i) {
                    if (_parm_list[i]->St_idx() == st)
                      return _parm_list[i]->Cr();
                  }
                  return NULL;
                }
  CODEREP      *Get_param_cr(INT i) {
                  if (i >= VAR_INIT_ID && i < _parm_list.size()) {
                    return _parm_list[i]->Cr();
                  } else {
                    return NULL;
                  }
                }
  ST_IDX        Get_param_stidx(INT i) {
                  if (i >= VAR_INIT_ID && i < _parm_list.size()) {
                    return _parm_list[i]->St_idx();
                  } else {
                    return ST_IDX_ZERO;
                  }
                }
  // return cr for `this' parameter if exists
  CODEREP      *Get_this_param(TY_IDX *this_ty) {
                  // this must be called with the same context
                  if (_parm_list.size() > VAR_INIT_ID) {
                    VAR_NODE *parm = _parm_list[VAR_INIT_ID];
                    if (parm->St_idx() != ST_IDX_ZERO &&
                        ST_is_this_ptr(ST_ptr(parm->St_idx()))) {
                      if (this_ty) {
                        TY_IDX ptr_ty = ST_type(parm->St_idx());
                        Is_True_Ret(TY_kind(ptr_ty) == KIND_POINTER &&
                                    TY_kind(TY_pointed(ptr_ty)) == KIND_STRUCT,
                                    ("not ptr-to-struct type"), NULL);
                        *this_ty = TY_pointed(ptr_ty);
                      }
                      return parm->Cr();
                    }
                  }
                  return NULL;
                }
  // return object ty for `this' parameter if exists
  TY_IDX        Get_this_obj_ty(CODEREP *cr) {
                  Is_True(cr && cr->Kind() == CK_VAR, ("not var for this"));
                  TY *ptr_ty = Ty_ptr(_file_idx, cr->Lod_ty());
                  Is_True(TY_kind(*ptr_ty) == KIND_POINTER, ("not pointer type"));
                  TY_IDX obj_ty = TY_pointed(*ptr_ty);
                  Is_True(TY_kind(*Ty_ptr(_file_idx, obj_ty)) == KIND_STRUCT, ("not struct type"));
                  return obj_ty;
                }
  IDTYPE        Find_param_references(CODEREP *x, BOOL *is_ptr, ACTION act, IDTYPE_SET &visited_set, BOOL deeply = FALSE);
  IDTYPE        Find_first_glob_freed(void) {
                  for (INT i = VAR_INIT_ID; i < _glob_list.size(); ++i) {
                    if ((_glob_list[i]->Flags() & REF_FREED) != 0)
                      return i;
                  } // parm_list iterations
                  return INVALID_VAR_IDX;
                }
  IDTYPE        Find_glob_references(CODEREP *x, BOOL *is_ptr, ACTION act);
  RNA_NODE     *Get_callsite_rna(STMTREP *stmt);
  RNA_NODE     *Get_callsite_rna(CODEREP *call);
  BOOL          Symbolic_eval_predicate(RNA_NODE *rna, CODEREP *x, BB_NODE *exit_bb);
  IDTYPE        Eval_upward_value_attribute(RNA_NODE *rna, IDTYPE *oparam, INT64 *adjuster);
  void          Collect_global_ref_info(GLOB_REF_MGR *glob_mgr, MEM_POOL *pool);
  void          Collect_global_ref_info(BB_NODE *bb, GLOB_REF_MGR *glob_mgr, GLOB_REF_LIST *ref_list, INT *stack_size, MEM_POOL *pool);
  void          Collect_global_ref_info(STMTREP *sr, CODEREP *cr, GLOB_REF_LIST *ref_list, INT *stack_size, MEM_POOL *pool);
  PDV_NODE     *Get_global_retv(ST_IDX st_idx);

  HEAPSTATE     Return_heapobj(HEAPINQ chkhor, HEAP_OBJ_REP **hor, HEAPSIZE *hsz = NULL);

  void          Calculate_local_ho_annot(IPSA *ipsa);

  IDTYPE        Is_param(CODEREP *x) const;
  BOOL          Is_param_copyin(STMTREP *stmt) const;
  IDTYPE        Is_param_copyin(CODEREP *lhs, CODEREP *rhs);
  BOOL          Is_va_arg_last_param(ST* st) const;
  IDTYPE        Is_global(CODEREP *x) const;
  ST_IDX        Get_glob_stidx(IDTYPE i)  { return _glob_list[i]->St_idx(); }
  BOOL          Is_aux_global(AUX_ID aux) const { return Comp_unit()->Opt_stab()->Is_global(aux); }

  BOOL          Update_parm_flags(IPSA* ipsa);
  BOOL          Update_retv_flags(IPSA* ipsa);
  BOOL          Update_eh_types(IPSA* ipsa);
  CODEREP      *Is_func_of_iparm(CODEREP *x, BOOL var_only = FALSE);
  pair<STMTREP*, CODEREP*>
                Branch_stmt_4_bb(BB_NODE *bb, BOOL var_only = FALSE);

  ST           *St_ptr(ST_IDX st_idx) const
  {
    if (ST_IDX_level(st_idx) == GLOBAL_SYMTAB)
      return ::St_ptr(File_idx(), st_idx);
    else
      return &_context._st_tab->Entry(ST_IDX_index(st_idx));
  }

  void          Print(BOOL print_callsite_stmt, FILE *fp=stderr) const;
  void          Print(FILE *fp=stderr) const            { Print(TRUE, fp); }

  // functions for manipulate STPATH_MAP
  SRCR_ID       Gen_srcr_id(IDTYPE srid, IDTYPE crid) const
  {
    return ((UINT64)srid << 32) | ((UINT64)crid & 0xffffffff);
  }
  SRCR_ID       Gen_srcr_id(const STMTREP* sr, const CODEREP* cr) const
  {
    UINT32 srid = sr ? sr->Stmtrep_id() : 0;
    UINT32 crid = cr ? cr->Coderep_id() : 0;
    return Gen_srcr_id(srid, crid);
  }
  
  void          Map_st_node(STMTREP *sr, ST_IDX st);
  void          Map_st_node(WN *wn, ST_IDX st);
  void          Dump_all_stpath(FILE *fp);
  ST_IDX        Get_node_st(STMTREP *sr, CODEREP *cr) ; 
  void          Map_stpath(OPT_STAB *opt_stab, CODEREP *cr_old, STMTREP *sr, CODEREP *cr_new);
  BOOL          Update_stpath(STMTREP* sr, CODEREP* cr, WN* wn);
  BOOL          Update_stpath(WN* wn, STMTREP* sr, CODEREP* cr);
  STPATH       *Search_stpath(const STMTREP* sr, const CODEREP* cr) const;
  STPATH       *Get_stpath(const STMTREP* sr, const CODEREP* cr) const;
  STPATH       *Get_stpath(const WN*) const;
  STPATH       *Get_stpath(IDTYPE srid, IDTYPE crid) const;
  BOOL          Dup_stpath(const WN*, const WN*);
  void          Remove_dead_stpath(void);
  void          Prop_stpath(STMTREP* defstmt, STMTREP* stmt1, CODEREP * cr);
  void          Dump_stpath(const STMTREP *stmt, const CODEREP *cr, FILE*);
  INLCXT       *Search_inlcxt_node(const INLCXT* inlcxt)
  {
    INLCXT_NODE* node = _inlcxt_list;
    while (node != NULL) {
      if (node->Equal(inlcxt))
        return node->Inlcxt();
      node = node->Next();
    }
    return NULL;
  }

  INLCXT       *Copy_inlcxt_node(INLCXT* inlcxt)
  {
    if (inlcxt->Is_global())
      return inlcxt;
    INLCXT *node = Search_inlcxt_node(inlcxt);
    if (node != NULL)
      return node;
    INLCXT *parent = inlcxt->Parent();
    UINT64  line_num = inlcxt->Inlcxt_line_num();
    UINT32  call_st = inlcxt->Inlcxt_call_st();
    if (parent != NULL)
      parent = Copy_inlcxt_node(parent);
    INLCXT_NODE* cxt_node = CXX_NEW(INLCXT_NODE(parent, line_num, call_st), Mem_pool());
    cxt_node->Set_Next(_inlcxt_list);
    _inlcxt_list = cxt_node;
    return cxt_node->Inlcxt();
  }
}; // end of DNA_NODE


typedef pair<IDTYPE, GLOB_REF_LIST*> DNA_GLOB_PAIR;
typedef mempool_allocator<DNA_GLOB_PAIR> DNA_GLOB_ALLOCATOR;
typedef hash_map<IDTYPE, GLOB_REF_LIST *,
                 IDTYPE_HASHER, IDTYPE_EQUAL,
                 DNA_GLOB_ALLOCATOR> DNA_GLOB_REF_MAP;
typedef DNA_GLOB_REF_MAP::iterator DNA_GLOB_REF_ITER;

// =============================================================================
//
// GLOB_REF_INFO: store global reference info
//
// =============================================================================
class GLOB_REF_INFO
{
public:
  enum REF_TYPE {
    REF_BY_INVALID = 0x00000000,
    REF_BY_VAR_MU  = 0x00000001,
    REF_BY_VAR_CHI = 0x00000002,
    REF_BY_CALL    = 0x00000004,
    REF_BY_VAR     = REF_BY_VAR_MU | REF_BY_VAR_CHI,
  };
private:
  REF_TYPE    _type;      // global reference type
  ST_IDX      _st;        // reference symbol index
  STMTREP    *_ref_sr;    // reference statement
  DNA_NODE   *_ctx_dna;   // owner dna of reference stmt
  DNA_NODE   *_ref_dna;   // reference global define/initialize function

public:
  GLOB_REF_INFO(REF_TYPE type, ST_IDX st, STMTREP *sr,
                DNA_NODE *ctx_dna, DNA_NODE *dna) :
    _type(type), _st(st), _ref_sr(sr), _ctx_dna(ctx_dna), _ref_dna(dna) {
  }

  DNA_NODE   *Ref_dna(void) const        { return _ref_dna; }
  DNA_NODE   *Ctx_dna(void) const        { return _ctx_dna; }
  STMTREP    *Ref_sr(void) const         { return _ref_sr;  }
  ST_IDX      Ref_st(void) const         { return _st;      }
  REF_TYPE    Ref_type(void) const       { return _type;    }
  BOOL        Is_ref_by_var(void) const  { return _type & REF_BY_VAR; }
  BOOL        Is_ref_by_mu(void) const   { return _type & REF_BY_VAR_MU; }
  BOOL        Is_ref_by_chi(void) const  { return _type & REF_BY_VAR_CHI; }
  BOOL        Is_ref_by_call(void) const { return _type & REF_BY_CALL; }

  const char *Glob_name() const {
    if(Is_ref_by_call()) {
      Is_True_Ret(Ref_dna(), ("null ref dna"), "");
    }
    IDTYPE file_idx = Is_ref_by_var() ? _ctx_dna->File_idx() : _ref_dna->File_idx();
    return (_st == ST_IDX_ZERO ? "" : ST_name(file_idx, _st));
  }

  void Print(FILE *fp) const {
    fprintf(fp, "GLOB REF[%d]: st %s, dna %s\n",
            _type, Glob_name(), _ref_dna ? _ref_dna->Fname() : "null");
  }
};

// =============================================================================
//
// GLOB_REF_MGR: Manage the global reference map
//
// =============================================================================
class GLOB_REF_MGR
{
private:
  MEM_POOL         *_pool;        // pool to allocate memory
  DNA_GLOB_REF_MAP  _ref_map;     // map dna -> GLOB_REF_LIST

public:
  GLOB_REF_MGR(MEM_POOL *pool) : 
    _pool(pool), 
    _ref_map(32, IDTYPE_HASHER(), IDTYPE_EQUAL(), DNA_GLOB_ALLOCATOR(pool)) {
  }
  DNA_GLOB_REF_MAP *Ref_map() { return &_ref_map; }

  GLOB_REF_LIST *Get_ref_list(DNA_NODE *dna) {
    DNA_GLOB_REF_ITER it = _ref_map.find(dna->Dna_idx());
    if(it != _ref_map.end()) {
      return it->second;
    } else {
      return NULL;
    }
  }

  void Enter_global_info(DNA_NODE *dna, GLOB_REF_LIST *ref_list) {
    _ref_map[dna->Dna_idx()] = ref_list;
  }

  void Get_ref_chi_stmts(DNA_NODE *dna, ST_IDX global_st, vector<STMTREP*> &list);

  void Print(IPSA *ipsa, FILE *fp);
};

// =============================================================================
// implementation for inline function
// SRCPOS_NODE::File_idx() which uses DNA_NODE
// =============================================================================
inline UINT
SRCPOS_NODE::File_idx() const
{
  return _dna->File_idx();
}

// =============================================================================
//
// RNA_NODE: it captures call site specific information of a function.  We use
//           such information to bring forth call site specific side effects
//
// =============================================================================
typedef enum { INVALID_RNA_PU_IDX = 0 } RNA_PU_IDX;

#define MAX_RNA_FLAG      0xffffff  // 24 bit
#define MAX_RBC_OP        0xff      // 8 bit
enum RNA_FLAGS {
  RNA_BACK_EDGE           = 0x000001,  // This is a back edge in recusive call 
  RNA_ICALL_RESOLVED      = 0x000002,  // ICALL targets fully resolved
  RNA_CALLED_IN_LOOP      = 0x000004,  // RNA is called in loop
  RNA_HAS_FUNCTIONAL      = 0x000008,  // RNA has functional callee
  RNA_HAS_RBC_NODES       = 0x000010,  // RNA has rbc nodes
  // RNA symbolic flags propagated to caller rna
  RBC_SE_ASSERT           = 0x001000,  // RBC symbolic execution model assert call
  RBC_SE_MODEL            = 0x002000,  // RBC symbolic execution model model call
  RBC_SE_IMPLICIT_CALL    = 0x004000,  // RBC symbolic execution model eval call
  RBC_SE_IMPLICIT_ASSIGN  = 0x008000,  // RBC symbolic execution implicit assign
  RBC_SE_EXEC_PATH        = 0x010000,  // RBC symbolic execution on all exec path
  RBC_SE_TAG_OP           = 0x020000,  // RBC symbolic execution tag op
  RBC_SE_CONTAINER_OP     = 0x040000,  // RBC symbolic execution container op
  RBC_SE_ASSERT_DNA       = 0x100000,  // RBC symbolic execution model assert on dna node
  RBC_SE_GLOBAL_USED      = 0x200000,  // RBC symbolic execution annotate global var used
  RBC_SE_DEFINE_CALL      = 0x400000,  // RBC symbolic execution annotate function call
  RBC_SE_FLAG_MASK        = 0xfff000,  // RBC symbolic execution flag mask
};

#define DEF_RBC_OP(eval_op, init_ptr, eval_ptr, fname)  eval_op,
typedef enum rbc_opcode {
#include "rbc_eval_op.inc"
} RBC_OP;
typedef mempool_allocator<UINT32> RBC_OP_ALLOCATOR;
typedef hash_set<UINT32, __gnu_cxx::hash<UINT32>, __gnu_cxx::equal_to<UINT32>, RBC_OP_ALLOCATOR> RBC_OP_SET;

#define RBC_OP_CONTAINER_GET     { RBC_OP_SET_FUNC_COLL_BACK, RBC_OP_SET_FUNC_COLL_GET, \
                                   RBC_OP_SET_FUNC_MAP_GET, RBC_OP_SET_FUNC_STR_GET, \
                                   RBC_OP_SET_FUNC_COLL_BACK_REF, RBC_OP_SET_FUNC_COLL_GET_REF, \
                                   RBC_OP_SET_FUNC_MAP_GET_REF }
#define RBC_OP_CONTAINER_GET_REF { RBC_OP_SET_FUNC_COLL_BACK_REF, RBC_OP_SET_FUNC_COLL_GET_REF, \
                                   RBC_OP_SET_FUNC_MAP_GET_REF }
#define RBC_OP_CONTAINER_UPDATE  { RBC_OP_SET_FUNC_COLL_APPEND, RBC_OP_SET_FUNC_COLL_APPEND_REF, \
                                   RBC_OP_SET_FUNC_COLL_REMOVE, RBC_OP_SET_FUNC_MAP_PUT, \
                                   RBC_OP_SET_FUNC_MAP_PUT_REF }
#define RBC_OPS_CNT(ops_arr)     sizeof(ops_arr)/sizeof(RBC_OP)

#undef DEF_RBC_OP

typedef mempool_allocator<DNA_NODE*> DNODE_ALLOCATOR;
typedef vector<DNA_NODE*, DNODE_ALLOCATOR> DNODE_VECTOR;

enum CALLEE_FLAGS {
  CALL_TARGET         = 0x000000,     // Direct call target
  ICALL_VAR_TARGET    = 0x010000,     // Indirect call target resolved by var u-d
  ICALL_VSYM_TARGET   = 0x020000,     // Indirect call target resolved by vsym u-d
  ICALL_TYPE_TARGET   = 0x040000,     // Indirect call target resolved by type inference
  ICALL_DEVIRT_TARGET = 0x080000,     // Indirect call target resolved by class hierarchy
};

class CALLEE_INFO {
private:
  IDTYPE _callee;                 // callee DNA index
  UINT32 _flags;                  // callee flags

public:
  CALLEE_INFO(IDTYPE callee = INVALID_RNA_PU_IDX, UINT32 flag = CALL_TARGET)
   : _callee(callee), _flags(flag) { }

  IDTYPE Callee() const { return _callee; }
  UINT32 Flags() const { return _flags; }
  void   Update_flags(UINT32 flag) { _flags |= flag; }
  BOOL   Is_back_edge() const      { return (_flags & RNA_BACK_EDGE) != 0; }
};

typedef mempool_allocator<CALLEE_INFO> CALLEE_ALLOCATOR;
typedef vector<CALLEE_INFO, CALLEE_ALLOCATOR> CALLEE_VECTOR;

typedef std::vector< std::pair<UINT32, ST_IDX> > ICALL_TARGET_VECTOR;

//class RNA_NODE : public SLIST_NODE {
class RNA_NODE {
  friend class CALLEE_ITER;
  friend class CALLER_ITER;
  friend class RNA_CALLEE_ITER;

public:
  //DECLARE_SLIST_NODE_CLASS ( RNA_NODE )

private:
  IDTYPE        _rna_idx;                 // index into rna node table
  IDTYPE        _caller_idx;              // RNA caller id
  STMTREP      *_callstmt;                // statement where call is made
  CALLEE_VECTOR _callee_list;             // For indirect calls, all possible callee id
  VNODE_VECTOR  _arg_list;                // list of argumnt
  INT           _visit;                   // visit token
  INT           _trav;                    // trav token
  UINT32        _flags:24;                // _rbc_symbolic_exection_facts
  RBC_OP        _rbc_op:8;                // _rbc_symbolic_execution operator
  TAG_NODE      _ret_tag_node;            // return value's tag, one tag one bit

  RNA_NODE(void);                         // REQUIRED UNDEFINED UNWANTED methods
  RNA_NODE(const RNA_NODE&);              // REQUIRED UNDEFINED UNWANTED methods
  RNA_NODE& operator = (const RNA_NODE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  RNA_NODE( IDTYPE rnanode_id, IDTYPE caller, STMTREP *stmt, MEM_POOL *pool ):
    _rna_idx(rnanode_id), _caller_idx(caller), _callstmt(stmt),
    _arg_list(VAR_INIT_ID, (VAR_NODE*)(CXX_NEW(VAR_NODE(0, NULL), pool)), VNODE_VECTOR::allocator_type(pool)),
    _callee_list(CALLEE_VECTOR::allocator_type(pool))
  { _visit = 0; _trav = 0; _flags = 0; _rbc_op = RBC_OP_NONE; }
  ~RNA_NODE(void) { }

  VNODE_VECTOR *Arg_list(void)            { return &_arg_list; }
  INT       Arg_cnt(void) const           { return _arg_list.size() - 1; }
  CODEREP  *Get_arg(IDTYPE which_arg) const;
  IDTYPE    Rna_idx(void) const           { return _rna_idx; }
  IDTYPE    Caller_idx(void) const        { return _caller_idx; }
  STMTREP  *Callstmt(void) const          { return _callstmt; }
  MU_LIST  *Mu_list(void) const           { return _callstmt->Mu_list(); }
  CHI_LIST *Chi_list(void) const          { return _callstmt->Chi_list(); }
  SRCPOS    Linenum(void) const           { return _callstmt->Linenum(); }
  void      Add_callee(IDTYPE callee, UINT32 flag)
  {
    for (CALLEE_VECTOR::iterator iter = _callee_list.begin();
         iter != _callee_list.end(); ++ iter) {
      if (iter->Callee() == callee) {
        iter->Update_flags(flag);
        return;
      }
    }
    _callee_list.push_back(CALLEE_INFO(callee, flag));
  }
  BOOL      Has_callee(IDTYPE callee) const
  {
    CALLEE_VECTOR::const_iterator iter;
    for (iter = _callee_list.begin(); iter != _callee_list.end(); ++iter) {
      if (iter->Callee() == callee)
        return TRUE;
    }
    return FALSE;
  }
  const CALLEE_VECTOR& Callee_list() const{ return _callee_list; }
  CALLEE_INFO Callee_info(UINT idx) const { return _callee_list[idx]; }
  CALLEE_INFO* Callee_info(UINT idx)      { return &_callee_list[idx]; }
  UINT32    Callee_cnt() const            { return _callee_list.size(); }
  TY_IDX    Callee_ty() const
  {
    switch (_callstmt->Opr()) {
    case OPR_CALL:  return ST_type(_callstmt->St());
    case OPR_ICALL: return _callstmt->Ty();
    case OPR_INTRINSIC_CALL: return TY_IDX_ZERO;
    default:
      Is_True(FALSE, ("TODO: other call"));
      return TY_IDX_ZERO;
    }
  }
  ST*       Callee_st(void) const
  {
    return OPERATOR_has_sym(_callstmt->Opr()) ? _callstmt->St() : NULL;
  }
  IDTYPE    Uniq_callee(void) const
  {
    if (_callee_list.size() == 1)
      return _callee_list[0].Callee();
    else
      return INVALID_RNA_PU_IDX;
  }
  void      Set_callee_idx(IDTYPE caleidx)
  {
    Is_True(Callee_st() != NULL && _callee_list.size() == 0,
            ("wrong to call Set_callee_idx"));
    _callee_list.push_back(CALLEE_INFO(caleidx, CALL_TARGET));
  }
  void      Set_visit(INT cnt)            { _visit = cnt; }
  INT       Get_visit(void) const         { return _visit; }
  void      Set_trav(INT cnt)             { _trav = cnt;  }
  INT       Get_trav(void) const          { return _trav; }
  UINT32    Set_flag(UINT32 f)            { Is_True(f < MAX_RNA_FLAG, ("rna flag truncated"));
                                            _flags |= f; return _flags; }
  UINT32    Flag(void) const              { return _flags; }
  void      Clear_flag(UINT32 f)          { _flags &= ~f; }
  void      Prop_rbc_flags(IPSA *ipsa);
  void      Set_rbc_op(RBC_BASE *rbc, DNA_NODE *dna);
  void      Set_rbc_op(RBC_OP op)         { Is_True(op < MAX_RBC_OP, ("rbc op truncated"));
                                            _rbc_op = op; }
  RBC_OP    Rbc_op() const                { return _rbc_op; }
  BOOL      Is_rbc_op(RBC_OP op) const    { return _rbc_op == op; }
  BOOL      Is_flag_set(UINT32 f) const   { return (_flags & f) != 0; }
  BOOL      Is_container_op(void) const   { return (_flags & RBC_SE_CONTAINER_OP) != 0; }
  BOOL      Is_back_edge(void) const      { return (_flags & RNA_BACK_EDGE) != 0 ; }
  void      Set_is_back_edge()            { _flags |= RNA_BACK_EDGE; }
  BOOL      Is_indirect_call(void) const  { return _callstmt->Opr() == OPR_ICALL; }
  BOOL      Is_virtual_call(void) const   { return (_callstmt->Call_flags() & WN_CALL_IS_VIRTUAL) != 0; }
  BOOL      Is_interface_call(void) const { return (_callstmt->Call_flags() & WN_CALL_IS_INTERFACE) != 0; }
  BOOL      Is_resolved(void) const       { return (_callstmt->Opr() != OPR_ICALL) ||
                                                   (_flags & RNA_ICALL_RESOLVED) != 0; }
  
  void      Collect_arg_list(CODEREP *cr, IPSA *ipsa); // from the call expression

  void      Enter_arg_list(CODEREP *cr, IPSA *ipsa);
  void      Update_arg_crs(INT32 &arg_idx, CODEREP *cr);
  void      Propagate_from_caller(RNA_NODE *caller);
  BOOL      Update_retv_flags(DNA_NODE *caller, PDV_NODE* pdv);
  IDTYPE    Get_arg_with_aux(AUX_ID aux, OPT_STAB *stab); // find the arg and then return its flags
  IDTYPE    Get_arg_with_lda(ST_IDX stid); // find the arg and then return its flags
  IDTYPE    Get_arg_with_cr(CODEREP *cr);  // find the arg with the same cr
  pair<IDTYPE, BOOL> Get_arg(CODEREP* cr, const VSA *vsa); // first is arg index, second is vsa
  UINT32    Set_arg_flag(IDTYPE i, UINT32 f) { return _arg_list[i]->Set_flag(f); }
  UINT32    Get_arg_flags(IDTYPE i) const { return _arg_list[i]->Flags(); }
  ST_IDX    Get_arg_stidx(IDTYPE i) const { return _arg_list[i]->St_idx(); }
  BOOL      Is_set_arg_flag(IDTYPE i, UINT32 f) { return _arg_list[i]->Flags()&f; }
  CODEREP  *Get_arg_with_flag(UINT32 f) {
    for(IDTYPE i = INVALID_VAR_IDX; i <= Arg_cnt(); i++) {
      if(Is_set_arg_flag(i, f)) {
        return Get_arg(i);  // return first arg with flag f
      }
    }
    return NULL;
  }
  // return cr for `this' argument if exists
  CODEREP  *Get_this_arg() {
    if (_callstmt->Call_flags() & (WN_CALL_IS_VIRTUAL | WN_CALL_IS_INTERFACE)) {
      Is_True(Arg_cnt() >= VAR_INIT_ID, ("no this argument for virtual call"));
      return Get_arg(VAR_INIT_ID);
    }
    return NULL;
  }
  // return object ty for `this` argument if exists
  TY_IDX    Get_this_obj_ty(UINT32 file_idx, CODEREP *cr) {
    Is_True(cr != NULL, ("cr is null"));
    Is_True(file_idx == File_Index, ("TODO: file index mismatch"));
    TY_IDX ptr_ty = cr->object_ty();
    if (TY_kind(ptr_ty) != KIND_POINTER ||
        TY_kind(TY_pointed(ptr_ty)) != KIND_STRUCT) {
      ptr_ty = _callstmt->Rhs()->Opnd(0)->Ilod_ty();
    }
    Is_True(TY_kind(ptr_ty) == KIND_POINTER &&
            TY_kind(TY_pointed(ptr_ty)) == KIND_STRUCT, ("not ptr-to-struct type"));
    return TY_pointed(ptr_ty);
  }
  void      Set_arg_tag(IDTYPE i, IDTYPE tag_id, BOOL on)
  {
    Is_True(i < _arg_list.size(), ("Arg index out of bound, index : %d, bound : %d.", i, _arg_list.size()));
    if (i < _arg_list.size())
      _arg_list[i]->Set_tag(tag_id, on);
  }
  BOOL      Is_set_arg_tag(IDTYPE i, IDTYPE tag_id)
  {
    Is_True(i < _arg_list.size(), ("Arg index out of bound, index : %d, bound : %d.", i, _arg_list.size()));
    if (i < _arg_list.size())
      return _arg_list[i]->Is_set_tag(tag_id);
    return FALSE;
  }
  BOOL       Is_arg_tag_on(IDTYPE i, IDTYPE tag_id)
  {
    Is_True(i < _arg_list.size(), ("Arg index out of bound, index : %d, bound : %d.", i, _arg_list.size()));
    if (i < _arg_list.size())
      return _arg_list[i]->Is_tag_on(tag_id);
    return FALSE;
  }
  void       Set_ret_tag(IDTYPE tag_id, BOOL on)       { _ret_tag_node.Set_tag(tag_id, on); }
  BOOL       Is_set_ret_tag(IDTYPE tag_id)    { return _ret_tag_node.Is_set_tag(tag_id); }
  BOOL       Is_ret_tag_on(IDTYPE tag_id)     { return _ret_tag_node.Is_tag_on(tag_id); }
  TAG_NODE  *Merge_arg_tag_node(IDTYPE i, TAG_NODE *tag_node)
  {
    Is_True(i < _arg_list.size(), ("Arg index out of bound, index : %d, bound : %d.", i, _arg_list.size()));
    if (i < _arg_list.size())
      return _arg_list[i]->Merge_tag_node(tag_node);
    return NULL;
  }
  TAG_NODE  *Merge_ret_tag_node(TAG_NODE *tag_node)
  {
    return _ret_tag_node.Merge(tag_node);
  }

  void      Trim(IPSA *ipsa);

  void      Print(BOOL print_call_stmt, FILE *fp=stderr) const;
  void      Print(FILE *fp=stderr) const  { Print(TRUE, fp); }

}; // end of RNA_NODE

// =============================================================================
//
// CONTEXT_SWITCH wrap DNA_NODE::Save_context and DNA_NODE::Restore_context
//     to enable the IPSA call graph traversal recursively
//
// =============================================================================
class CONTEXT_SWITCH {
private:
  DNA_NODE _context;

  CONTEXT_SWITCH (const CONTEXT_SWITCH &);            // REQUIRED UNDEFINED UNWANTED methods
  CONTEXT_SWITCH& operator = (const CONTEXT_SWITCH&); // REQUIRED UNDEFINED UNWANTED methods
public:
  CONTEXT_SWITCH (const DNA_NODE *to_which_context)
  { _context.Save_context(); to_which_context->Restore_context();}
  ~CONTEXT_SWITCH (void) { _context.Restore_context(); }
};

// =============================================================================
//
// IPSA_ST: is the hash table that map the function st in a file to the index for a
//          DNA_NODE.  This enables the name binding during the link process.
//
// =============================================================================

#if 0
struct HASHER
{
  size_t operator() (const char *__s) const
  {
    unsigned long __h = 0;
    for ( ; *__s; ++__s)
      __h = 5 * __h + *__s;
    return size_t(__h);
  }
};

struct EQSTR
{
  bool operator()(const char* s1, const char* s2) const
  {
   return strcmp(s1, s2) == 0;
  }
};
#endif

//==============================================================================
// IPSA_ST
//  * Maintain the mapping from ST IDX to DNA_NODE IDX.
//  * Get DNA_NODE IDX from FILE IDX and ST IDX
// ST_MAP
//  * ST_MAP is a per-file hash map which maps the ST_IDX for function defined
//    in current file to DNA_NODE IDX
// FILE_ST_VECTOR
//  * FILE_ST_VECTOR is a vector of ST_MAP. Each element in FILE_ST_VECTOR
//    stores the ST_MAP for a input file
// Put() operation
//  * For the Put() operation, find the ST_MAP from FILE_ST_VECTOR by the FILE
//    IDX at first, then put the <ST IDX, DNA_NODE IDX> into the ST_MAP.
// Get() operation
//  * For the Get() operation, if ST is SCLASS_EXTERN, query linker resolution
//    result from WHIRL_FILE_MANAGER to get the definition FILE IDX and ST IDX
//    at first, find ST_MAP from FILE_ST_VECTOR by FILE IDX and get DNA_NODE IDX
//    by ST IDX from ST_MAP
//==============================================================================
class IPSA_ST {
private:

  typedef pair<ST_IDX, IDTYPE> HT_VALUE_TYPE;
  typedef mempool_allocator<HT_VALUE_TYPE> HT_ALLOCATOR;
  //typedef hash_map <ST_IDX, IDTYPE, HASHER, EQSTR, HT_ALLOCATOR> ST_MAP;
  typedef hash_map <ST_IDX, IDTYPE, __gnu_cxx::hash<ST_IDX>, std::equal_to<ST_IDX>, HT_ALLOCATOR> ST_MAP;
  typedef mempool_allocator<ST_MAP*> ST_ALLOCATOR;
  typedef vector<ST_MAP*, ST_ALLOCATOR> FILE_ST_VECTOR;

  FILE_ST_VECTOR _tab;
  MEM_POOL      *_mpool;

  IPSA_ST(void);                           // REQUIRED UNDEFINED UNWANTED methods
  IPSA_ST(const IPSA_ST&);                 // REQUIRED UNDEFINED UNWANTED methods
  IPSA_ST& operator = (const IPSA_ST&);    // REQUIRED UNDEFINED UNWANTED methods

public:

  typedef ST_MAP::value_type ST_MAPPING;
  IPSA_ST(UINT32 min_size, MEM_POOL *mem_pool);
  ~IPSA_ST();

  void Put(UINT32 file_idx, ST* st, IDTYPE dna_id);
  const IDTYPE Get (UINT32 file_idx, ST* st);
  const IDTYPE Get_xpreempt(UINT32 file_idx, ST_IDX st_idx, ST_SCLASS sclass);  // for resolved virtual functions. 
  const IDTYPE Cfa_get (UINT32 file_idx, ST* st);
}; // IPSA_ST

typedef pair<const char*, IDTYPE> STR_ID;
typedef mempool_allocator<STR_ID> SI_ALLCATOR;
struct streq
{
  BOOL operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};
typedef hash_map<const char*, IDTYPE, __gnu_cxx::hash<const char*>,
                     streq, SI_ALLCATOR> STR_MAP;

#define INVALID_STR_ID 0

//==============================================================================
// IPSA maintains the file level DNAGRAPH. 
//
// DNAGRAPH created for DNA/Dna Context Management for Static Analyzer.
// It contains info that is essential for optimizer but does not exist inside the
// wopt management data structure.
// TO perform static analysis across procedure boundary, we preserve CODEMAP
// which has all the SSA beyond single PU processing time.
// WHIRL does not give us the CFG and make it difficult to maintain SSA.
//
//==============================================================================

class CLASS_HIERARCHY;

extern void Rename_CODEMAP(COMP_UNIT *);

typedef enum {DNA_CREATION, DNA_INIT, SA_N_REPORT } IPSA_PHASES;
typedef pair<const STRING, IDTYPE> FLD_NAME_ID;
typedef mempool_allocator<FLD_NAME_ID> FLD_NAME_ALLCATOR;
typedef hash_map<const STRING, IDTYPE, __gnu_cxx::hash<const char*>,
                     streq, FLD_NAME_ALLCATOR> FLD_NAME_MAP;
#define FLD_ID_INVALID MAX_ID
#define FLD_OFST_ALL   INT32_MAX

// ==============================================================================
// FPTR_ASSGN_INFO
// Keep track on global function pointer assignment information
// ==============================================================================
class FPTR_ASSGN_INFO {
private:
  IDTYPE   _dna_idx;     // where the fptr is assigned
  IDTYPE   _offset;      // offset in aggregate
  STMTREP *_assign;      // assign stmt

public:
  // constructor
  FPTR_ASSGN_INFO() : _dna_idx(0), _offset(0), _assign(0) {}

  // get offset
  IDTYPE Offset() const { return _offset; }

  // get stmt
  STMTREP *Stmt() const { return _assign; }

public:
  // analyze stmt in dna, return INVALID_ST if it's not assign to global
  // function pointer
  ST *Analyze(IPSA *ipsa, DNA_NODE *dna, STMTREP *sr);

  // get unique call
  DNA_NODE *Callee(IPSA *ipsa) const;
};

typedef mempool_allocator<FPTR_ASSGN_INFO>        FPTR_ALLOCATOR;
typedef vector<FPTR_ASSGN_INFO, FPTR_ALLOCATOR>   FPTR_VECTOR;
typedef pair<uint64_t, FPTR_VECTOR*>              FST_FPTR_PAIR;
typedef mempool_allocator<FST_FPTR_PAIR>          FST_FPTR_ALLOCATOR;
typedef hash_map<uint64_t, FPTR_VECTOR*,
                 __gnu_cxx::hash<uint64_t>,
                 std::equal_to<uint64_t>,
                 FST_FPTR_ALLOCATOR>              FST_FPTR_MAP;

typedef vector<IDTYPE> SRC_VECTOR;
class ASSIGN_INFO
{
  private:
    struct ASSIGN_ITEM {
      IDTYPE         _tgt;    // implicit assign tgt
      SRC_VECTOR     _srcs;   // implicit assign src
      ASSIGN_ITEM(IDTYPE tgt, IDTYPE src) 
      {
        _tgt = tgt;
        _srcs.push_back(src);
      } 
      BOOL Exist_src(IDTYPE src) 
      {
        for(int src_idx = 0; src_idx < _srcs.size(); src_idx++) {
          if(src == _srcs[src_idx]) {
            return TRUE;
          }
        }
        return FALSE;
      }
    };

    vector<ASSIGN_ITEM *> _assign_info;           // {tgt1->src1,src2}
    MEM_POOL*             _pool;
    
    ASSIGN_INFO(const ASSIGN_INFO&);              // REQUIRED UNDEFINED UNWANTED methods
    ASSIGN_INFO& operator = (const ASSIGN_INFO&); // REQUIRED UNDEFINED UNWANTED methods

  public:
    ASSIGN_INFO(MEM_POOL* pool): _pool(pool) { _assign_info.clear(); }

    void        Enter_assign(IDTYPE tgt, IDTYPE src);
    SRC_VECTOR *Get_assign_src(IDTYPE tgt);
    void        Print(FILE *fp);
};

typedef ID_MAP<ASSIGN_INFO*, IDTYPE> RBC_ASIGN_MAP;
enum DNA_TRAVERSAL_ORDER
{
  DNA_TRAV_PRE_ORDER    = 0,
  DNA_TRAV_POST_ORDER   = 1,
  DNA_TRAV_TOP_DOWN_ORDER  = 2,
  DNA_TRAV_BOTTOM_UP_ORDER = 3,
  DNA_TRAV_LAST_ORDER   = 4
};

class RBC_BASE;

class IPSA {
  template<DNA_TRAVERSAL_ORDER _Order> friend class DNODE_ITER;
  friend class CALLEE_ITER;
  friend class CALLER_ITER;
  friend class RNA_CALLEE_ITER;
  friend class RBC_CONTEXT;
  friend class RBC_BASE;
  template<typename _CHECKER> friend class CHECKER_TRAVELER;
public:
  enum _ipsa_init_idx {DNA_INIT_IDX = 1, RNA_INIT_IDX = 1};
    
private:

  CXX_MEM_POOL  _mem_pool;             // Global pool used for a "this" object.
                                       // Must be constructed at first
  CXX_MEM_POOL  _loc_pool;             // Local pool used for individual checker phases
  IPSA_PHASES   _phase;                // NOTE: need further refinement 5/3/18
  IPSA_ST       _st;                   // symbol table for function resolution
  DNODE_VECTOR  _root_level_dnode;     // maintain the root level dna nodes
  DNODE_VECTOR  _dnaid_to_dnanode;     // file_dna_idx to DNA_NODE*
  RNODE_VECTOR  _rnaid_to_rnanode;     // rna_nd_idx to RNA_NODE*
  DNODE_VECTOR  _pre_order_dnode;      // a preorder iterator for DNA_NODE
  DNODE_VECTOR  _post_order_dnode;     // a postorder iterator for DNA_NODE
  DNODE_VECTOR  _top_order_dnode;      // topological order for DNA_NODE
  FST_FPTR_MAP  _fst_fptr_map;         // map for global function pointer
  DNA_IDX_SET   _shutdown_hooks;       // a list dna registered shutdown hook
  CHA          *_glob_cha;             // global class hirarchy 
  HEAP_OBJ_REP *_default_hor;          // represent (void *)Unknown
  VSYM_OBJ_REP *_default_vor;          // represent *(void *)Unknown
  HEAP_OBJ_REP *_null_hor;             // represent (void *)NULL
  VSYM_OBJ_REP *_null_vor;             // represent *(void *)NULL

  IDTYPE        _last_heap_obj_id;     // track the heap_obj at IPSA level
  IDTYPE        _last_vsym_obj_id;     // track the vsym_obj at IPSA level
  IDTYPE        _last_fld_name_id;     // track the vsym_fld_rep at IPSA level
  IDTYPE        _last_isu_grp_id;      // track the issue_group id at IPSA level

  BOOL          _trav_counter_is_set; // prevent recursive set of _trav_counter
  INT           _trav_counter;         // traversal cntl against recursion 
  INT           _pu_counter;           // use in -show
  INT           _real_pu_count;        // real pu count, exclude rbc functions
  RBC_BASE     *_rbc;                  // RBC instance pointer
  RBC_ASIGN_MAP _rbc_assign_infos;     // RBC implicit assgin info

  FLD_NAME_MAP  _fld_name_id_map;      // map fld_id -> fld name
  STR_MAP       _isu_grp_map;          // map issue_group string to an id
  

  IPSA(const IPSA&);                   // REQUIRED UNDEFINED UNWANTED methods
  IPSA& operator = (const IPSA&);      // REQUIRED UNDEFINED UNWANTED methods

  // DNA_CREATION Phase operations
  RNA_NODE     *New_rna(DNA_NODE *dna, STMTREP *stmt);
  void          Convert_icall_to_call(DNA_NODE *dna, STMTREP *stmt, ST* call_st);
  ST*           Find_or_create_fun_st(UINT32 file_idx, ST_IDX st_idx, UINT32 ref_file_idx, TY_IDX ty);
  TY_IDX        Find_icall_ty_from_ivar_mu(DNA_NODE *dna, CODEREP *opnd,
                                           AUX_ID aux, hash_set<IDTYPE> &visited_bb);
  void          Do_devirtualization(DNA_NODE *dna, STMTREP *stmt);
  void          Do_icall_promotion(DNA_NODE *dna, STMTREP *stmt);
  void          Find_calls_and_side_effects(DNA_NODE *dna, BB_NODE *bb, STMTREP_STACK *oparam);
  BOOL          Get_virtual_call_ty_and_ofst(CODEREP *callcr, TY_IDX *ty_idx,
                                             INT32 *offset, INT32 *vptr_ofst);
  // SA_N_REPORT Phase operations
  void          Connect_indirect_call(DNA_NODE*, RNA_NODE*, DNODE_VECTOR& addr_taken_funs);
  void          Connect_virtual_call(DNA_NODE*, RNA_NODE*);
  void          Connect_interface_call(DNA_NODE*, RNA_NODE*);
  //void          Resolved_indirect_and_virtual_call(DNA_NODE*, RNA_NODE*, DNODE_VECTOR&);
  void          Resolved_indirect_and_virtual_call(DNA_NODE *func, DNODE_VECTOR& addr_taken_funs);
  void          Resolved_register_call(DNA_NODE *func, DNODE_VECTOR& addr_taken_funs);
  void          Mark_thread_entry(UINT32 dna_idx, BOOL many);
  void          Mark_isr_entry(UINT32 dna_idx);
  BOOL          Verify_found_virtual(RNA_NODE* call_site, DNA_NODE* callee);
  void          Create_missing_dna(MEM_POOL *pool);
  void          Link(void);
  void          Analysis_driver(DNA_NODE *func, RNA_NODE *caller_instance);

  void          Enter_function_dna(UINT32 file_idx, ST* st, IDTYPE id);
  void          Enter_global_fptr_assign(DNA_NODE *dna, STMTREP *sr);

  void          Build_dfs_vector(BOOL set_fun_attr = FALSE);
  void          Build_top_vector(BOOL set_fun_attr = FALSE);
  void          Dfs_walk(INT ,mUINT8 *, BOOL);

  void          Collect_ty_kind(const STMTREP* call, vector<TY_KIND>& rna_ty);
  void          Collect_ty_kind(const VNODE_VECTOR* vec, vector<TY_KIND>& rna_ty);
  BOOL          Pu_match_type(DNA_NODE*, const vector<TY_KIND>&, TY_IDX);
  void          Build_vtable_name_set(MEM_POOL *pool);

  void          Print_dnode_vector(const DNODE_VECTOR& vec, FILE* fp) const;

  CODEREP      *Get_global(DNA_NODE *dna, UINT32 callee_fid, ST_IDX stidx, STMTREP *stmt); // find LDID w/ stidx from the mu_list

  void          Perform_constant_prop(DNA_NODE *dna, BOOL hva);     // perform inter-procedure constant propagation
  void          Perform_side_effect_prop(DNA_NODE *dna, BOOL hva);  // perform inter-procedure side-effect propagation

  HEAP_OBJ_REP *Create_special_hor(BOOL def_hor);
  VSYM_OBJ_REP *Create_special_vor(HEAP_OBJ_REP *base_hor, BOOL def_hor);
  STRING        Clone_string(STRING name) {
    STRING cloned_name = (STRING) CXX_NEW_ARRAY(BOOL, (strlen(name)+1), Mem_pool());
    strcpy(cloned_name, name);
    return cloned_name;
  }

public:

  IPSA();
  ~IPSA();

  IPSA_PHASES   Phase(void) const        { return _phase; }
  void          Transition_state(void)   { _phase = SA_N_REPORT; }

  void          Begin_trav_counter(void) {
                                            Is_True(!_trav_counter_is_set, ("recursive use of Begin_trav_counter()"));
                                            _trav_counter++;
                                            _trav_counter_is_set = TRUE;
                                         }
  void          End_trav_counter(void)   { _trav_counter_is_set = FALSE; }
  void          Set_trav(RNA_NODE *rn)   {
                                           INT tc = rn->Get_trav();
                                           rn->Set_trav(tc < _trav_counter ? _trav_counter : tc + 1);
                                         }
  BOOL          Traved(RNA_NODE *rn)     { return rn->Get_trav() > _trav_counter + 1; }

  MEM_POOL     *Mem_pool(void)           { return _mem_pool(); }
  MEM_POOL     *Loc_pool(void)           { return _loc_pool(); }
  IDTYPE        Last_heapobj_id() const  { return _last_heap_obj_id; }
  IDTYPE        Last_vsymobj_id() const  { return _last_vsym_obj_id; }
  IDTYPE        Last_fldname_id() const  { return _last_fld_name_id; }
  IDTYPE        Last_isugrp_id() const   { return _last_isu_grp_id; }
  IDTYPE        New_heapobj_id(void)     { return (_last_heap_obj_id == MAX_ID) ? MAX_ID : _last_heap_obj_id++;}
  IDTYPE        New_vsymobj_id(void)     { return (_last_vsym_obj_id == MAX_ID) ? MAX_ID : _last_vsym_obj_id++;}
  IDTYPE        New_fldname_id(void)     { return (_last_fld_name_id == MAX_ID) ? MAX_ID : _last_fld_name_id++;}
  IDTYPE        New_isugrp_id(void)      { return (_last_isu_grp_id == MAX_ID) ? MAX_ID : ++_last_isu_grp_id;}
  IDTYPE        Get_isugrp_id(const char *);
  IDTYPE        Put_isugrp_id(const char *);

  // class hierarchy
  CHA          *Glob_cha()               { return _glob_cha; }
  void          Build_and_merge_cha();
  void          Build_cha_begin();
  void          Build_cha_end();

  HEAP_OBJ_REP *Default_hor(void) const  { return _default_hor; }
  VSYM_OBJ_REP *Default_vor(void) const  { return _default_vor; }
  HEAP_OBJ_REP *Null_hor(void) const     { return _null_hor; }
  VSYM_OBJ_REP *Null_vor(void) const     { return _null_vor; }

  DNA_NODE     *Get_dna(IDTYPE dna_idx)  { return _dnaid_to_dnanode[dna_idx]; }
  RNA_NODE     *Get_rna(IDTYPE rna_idx)  { return _rnaid_to_rnanode[rna_idx]; }
  UINT32        Rna_count() const        { return _rnaid_to_rnanode.size(); }
  UINT32        Dna_count() const        { return _dnaid_to_dnanode.size(); }

  DNA_NODE     *New_dna(COMP_UNIT *cu);
  DNA_NODE     *Init_dna(COMP_UNIT *cu);
  DNA_NODE     *Get_dna(UINT32 file_idx, ST* sym)
  {
    IDTYPE id = _st.Get(file_idx, sym);
    if (id != INVALID_RNA_PU_IDX)
      return _dnaid_to_dnanode[_st.Get(file_idx, sym)];
    else
      return NULL;
  }
  DNA_NODE     *Get_dna(UINT32 file_idx, ST_IDX st_idx)
  {
    ST *sym = St_ptr(file_idx, st_idx);
    return sym ? Get_dna(file_idx, sym) : NULL;
  }
  DNA_NODE     *Cur_dna(void)
  {
    IDTYPE id = _dnaid_to_dnanode.size();
    return id > DNA_INIT_IDX ? _dnaid_to_dnanode[id-1] : NULL;
  }
  void          Rename_codemap(COMP_UNIT *cu)
  {
    if (cu->Dna()->Callsite_cnt() > 0) Rename_CODEMAP(cu);
  }

  CODEREP      *Callee_frees_heap_memory(DNA_NODE *dna, STMTREP *stmt, BOOL *maybe = NULL);
  HEAPSTATE     Callee_allocate_heap_memory(DNA_NODE *dna, STMTREP *stmt, BOOL chkhor);
  INT64         Callee_returns_heapobj_size(DNA_NODE *dna, STMTREP *stmt, HEAPSIZE *hsz = NULL);

  STMTREP      *Recover_call_stmt(DNA_NODE *dna, CODEREP *call);
  RNA_NODE     *Eval_callsite_arg_list(DNA_NODE *dna, STMTREP *stmt);
  CODEREP      *Eval_callee_side_effect_return_value(RNA_NODE *rna, IDTYPE *oparam, INT64 *adjuster);
  void          Analyze(void);
  void          Emit(char *);
  BOOL          Is_jni_call(RNA_NODE *rna);
  RBC_BASE     *Rbc(void);
  BOOL          Rna_has_rbc_parm_flag(RNA_NODE *rna, IDTYPE parm_idx, UINT32 flag);
  UINT32        Rna_get_rbc_parm_flag(RNA_NODE *rna, IDTYPE parm_idx);
  RBC_OP_SET *  Rna_get_rbc_ops(RNA_NODE *rna, RBC_OP_SET &op_set);
  BOOL          Rna_has_rbc_ops(RNA_NODE *rna, RBC_OP *ops, int ops_cnt);
  BOOL          Rna_has_rbc_op(RNA_NODE *rna, RBC_OP op);
  IDTYPE        Fld_name_2_id(STRING fld_rep);
  IDTYPE        Enter_fld_name(STRING fld_rep);
  const STRING  Fld_id_2_name(IDTYPE id);
  void          Identify_vsym_field_info(DNA_NODE *func);
  void          Enter_shutdown(IDTYPE idx) { _shutdown_hooks.insert(idx); }
  DNA_IDX_SET  &Shutdown_set() { return _shutdown_hooks; }
  ASSIGN_INFO  *Rna_2_assign_info(IDTYPE rna_idx) { return _rbc_assign_infos.Lookup(rna_idx); }
  void          Enter_rbc_assign(IDTYPE rna_idx, IDTYPE tgt, IDTYPE src);
  SRC_VECTOR   *Get_assign_src(RNA_NODE *rna, CODEREP *tgt);
  DNA_NODE     *Get_global_clinit_dna(UINT32 file_idx, ST_IDX stidx);

  void          Verify(void);
  void          Print(FILE *fp = stderr) const;
  template<DNA_TRAVERSAL_ORDER _Order>
  void          Print(FILE *fp = stderr);
  void          Print_fld_name_map(FILE *fp);
  void          Print_cr(DNA_NODE *dna, CODEREP *cr, FILE *fp);
  void          Print_sr(DNA_NODE *dna, STMTREP *sr, FILE *fp);
  void          Print_cfg(COMP_UNIT *cu, FILE *fp);
  void          Print_intrn_table(FILE *fp);

  void          Print_cg_vcg(const char *fname);
}; // class IPSA

//==============================================================================
//
// DNODE_ITER visit the DNA node in defined order
//
//==============================================================================
template<DNA_TRAVERSAL_ORDER _Order>
class DNODE_ITER {
private:
  DNODE_VECTOR* _v;
  DNODE_VECTOR::iterator _cur_iter;
 
  DNODE_ITER(void);                           // REQUIRED UNDEFINED UNWANTED methods
  DNODE_ITER(const DNODE_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
  DNODE_ITER& operator = (const DNODE_ITER&); // REQUIRED UNDEFINED UNWANTED methods

public:

  DNODE_ITER(IPSA *g) {
    switch (_Order) {
    case DNA_TRAV_POST_ORDER:
      _v = &g->_post_order_dnode;
      break;
    case DNA_TRAV_PRE_ORDER:
      _v = &g->_pre_order_dnode;
      break;
    case DNA_TRAV_TOP_DOWN_ORDER:
      _v = &g->_top_order_dnode;
      break;
    default:
      Is_True(FALSE, ("unsupported traverse order"));
    }
    Is_True(_v->size() == g->_dnaid_to_dnanode.size() - 1,
            ("wrong vector size"));
    Reset();
  }

  void Reset()                 { _cur_iter = _v->begin(); }
  void Next()                  { ++_cur_iter;  }
  DNA_NODE* Current()          { return *_cur_iter;}
  BOOL Is_end()                { return  _cur_iter == _v->end(); }
};

template<>
class DNODE_ITER<DNA_TRAV_BOTTOM_UP_ORDER> {
private:
  DNODE_VECTOR* _v;
  DNODE_VECTOR::reverse_iterator _cur_iter;
 
  DNODE_ITER(void);                           // REQUIRED UNDEFINED UNWANTED methods
  DNODE_ITER(const DNODE_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
  DNODE_ITER& operator = (const DNODE_ITER&); // REQUIRED UNDEFINED UNWANTED methods

public:

  DNODE_ITER(IPSA *g) {
    _v = &g->_top_order_dnode;
    Is_True(_v->size() == g->_dnaid_to_dnanode.size() - 1,
            ("wrong vector size"));
    Reset();
  }

  void Reset()                 { _cur_iter = _v->rbegin(); }
  void Next()                  { ++_cur_iter;  }
  DNA_NODE* Current()          { return *_cur_iter;}
  BOOL Is_end()                { return  _cur_iter == _v->rend(); }
};

//==============================================================================
//
// CALLEE_ITER visit the callee DNA node for all call site of a rountine
//
//==============================================================================
class CALLEE_ITER {
private:
  IPSA     *_g;
  DNA_NODE *_func;
  IDTYPE    _cur_call_site_idx;
  IDTYPE    _cur_indirect_idx;

  CALLEE_ITER(void);                            // REQUIRED UNDEFINED UNWANTED methods
  CALLEE_ITER(const CALLEE_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
  CALLEE_ITER& operator = (const CALLEE_ITER&); // REQUIRED UNDEFINED UNWANTED methods
public:
  CALLEE_ITER(IPSA* g, DNA_NODE *f):
    _g(g), _func(f),_cur_call_site_idx(IPSA::RNA_INIT_IDX),_cur_indirect_idx(0)
  {
    Reset();
  }
  BOOL Is_end(void)
  {
    return _cur_call_site_idx >= _func->Call_site_cnt();
  }
  void Reset(void)
  {
    _cur_call_site_idx = IPSA::RNA_INIT_IDX;
    _cur_indirect_idx = 0;
    RNA_NODE* call_site;
    while(_cur_call_site_idx < _func->Call_site_cnt() &&
          (call_site = _func->Callsite(_cur_call_site_idx)) &&
          call_site->Callee_cnt() == 0)
      // filter out indirect call which has no callee candidate
      _cur_call_site_idx++;
  }
  
  RNA_NODE* Current_callsite(void)
  {
    Is_True(_cur_call_site_idx < _func->Call_site_cnt(), ("CALLEE_ITER Out of Range"));
    return _func->Callsite(_cur_call_site_idx);
  }

  CALLEE_INFO* Current_callee(void)
  {
    RNA_NODE* call_site = Current_callsite();
    Is_True(_cur_indirect_idx < call_site->Callee_cnt(), ("CALLEE_ITER Out of Range"));
    return call_site->Callee_info(_cur_indirect_idx);
  }

  DNA_NODE* Current(void)
  {
    Is_True(_cur_call_site_idx < _func->Call_site_cnt(), ("CALLEE_ITER Out of Range"));
    RNA_NODE* call_site = _func->Callsite(_cur_call_site_idx);
    Is_True(_cur_indirect_idx < call_site->Callee_cnt(), ("CALLEE_ITER Out of Range"));
    IDTYPE callee = call_site->Callee_info(_cur_indirect_idx)->Callee();
    Is_True(callee >= IPSA::DNA_INIT_IDX, ("invalid callee idx"));
    return _g->Get_dna(callee);
  }
  
  void Next(void);
};

//==============================================================================
//
// CALLER_ITER visit the caller DNA node for all call site of a rountine
//
//==============================================================================
class CALLER_ITER {
private:
  IPSA     *_g;
  DNA_NODE *_func;
  IDTYPE    _cur_call_site_idx;

  CALLER_ITER(void);                            // REQUIRED UNDEFINED UNWANTED methods
  CALLER_ITER(const CALLER_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
  CALLER_ITER& operator = (const CALLER_ITER&); // REQUIRED UNDEFINED UNWANTED methods
public:
  CALLER_ITER(IPSA *g, DNA_NODE *f):
    _g(g), _func(f), 
    _cur_call_site_idx(IPSA::RNA_INIT_IDX)       { }
  BOOL Is_end(void)                              { return _cur_call_site_idx >= _func->Clby_list_cnt(); }
  void Reset(void)                               { _cur_call_site_idx = IPSA::RNA_INIT_IDX; }
  RNA_NODE *Current_callsite(void)               
  { 
    Is_True(!Is_end(), ("CALLER_ITER Out of Range")); 
    return _func->Callby(_cur_call_site_idx);
  }
  DNA_NODE *Current(void)
  {
    Is_True(!Is_end(), ("CALLER_ITER Out of Range")); 
    RNA_NODE *callsite =  _func->Callby(_cur_call_site_idx);
    return _g->Get_dna(callsite->Caller_idx());
  }
  void Next(void)
  {
    Is_True(!Is_end(), ("CALLER_ITER Out of Range"));
    _cur_call_site_idx++;
  }
};

//==============================================================================
//
// RNA_CALLEE_ITER visit the callee DNA node for an RNA_NODE
//
//==============================================================================
class RNA_CALLEE_ITER {
private:
  IPSA     *_g;
  RNA_NODE *_callsite;
  IDTYPE    _cur_callee_idx;

  RNA_CALLEE_ITER(void);                                // REQUIRED UNDEFINED UNWANTED methods
  RNA_CALLEE_ITER(const RNA_CALLEE_ITER&);              // REQUIRED UNDEFINED UNWANTED methods
  RNA_CALLEE_ITER& operator = (const RNA_CALLEE_ITER&); // REQUIRED UNDEFINED UNWANTED methods
public:
  RNA_CALLEE_ITER(IPSA *g, RNA_NODE *callsite):
    _g(g), _callsite(callsite), _cur_callee_idx(0) {}
  BOOL Is_end(void)
  { 
    return _cur_callee_idx >= _callsite->Callee_cnt();
  }
  void Reset(void) { _cur_callee_idx = 0; }
  DNA_NODE *Current(void)
  {
    Is_True(!Is_end(), ("RNA_CALLEE_ITER Out of Rang"));
    IDTYPE callee = _callsite->Callee_info(_cur_callee_idx)->Callee();
    Is_True(callee >= IPSA::DNA_INIT_IDX, ("invalid callee idx"));
    return _g->Get_dna(callee);
  }
};

extern void Dump_stpath(const WN*,  FILE*);


#endif  // opt_dna_INCLUDED
