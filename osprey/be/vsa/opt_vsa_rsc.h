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
// Module: opt_vsa_rsc.h
//
// ====================================================================
//

#ifndef opt_vsa_rsc_INCLUDED
#define opt_vsa_rsc_INCLUDED        "opt_vsa_rsc.h"
#include <bitset>
#include "vsa_defs.h"
#include "opt_dna.h"
#include "rbc_base.h"

class BB_LIST;
class BB_NODE;
class CODEREP;
class FSM_OBJ_REP;
class HEAP_OBJ_REP;
class TAG_OBJ_REP;
class OPT_STAB;
class STMTREP_LIST;
class VSYM_OBJ_REP;
class HOR_LIST;
class TAG_OBJ;
class TOR_LIST_OLD;
class TOR_LIST;

// =============================================================================
//
// RSC_OBJ models a system resource, such as a object allocated through malloc,
// a file descriptor created by file open, and so on. 
//
// =============================================================================
typedef enum {
  RSC_KIND_NONE,
  RSC_KIND_IPARM,                 // pointer passed from caller
  RSC_KIND_ALLOC,                 // malloc equivalence creation
  RSC_KIND_AUTO,                  // local pointer assigned from other source
  RSC_KIND_VSYM,                  // virtual symbol
  RSC_KIND_LOCK,                  // resource kind for lock/unlock
  RSC_KIND_FSM,                   // resource kind for finite state machine
  RSC_KIND_LDA,                   // resource kind for &variable
  RSC_KIND_ALLOCA,                // _alloca equivalence creation
  RSC_KIND_TAG,                   // resource kind for tag used in RULE editor
} RSC_KIND;

typedef enum {
  RSC_SUB_NONE = 0,               // not a valid sub kind
} SUB_KIND;

typedef enum {
  RSC_FLAG_NONE         = 0,              // not a valid RSC flag
  RSC_ADDR_PASSED_OUT   = 0x1,            // RSC's address been passed out through call
  RSC_ADDR_PASSED_IN    = 0x2,            // RSC's address been passed in through call
  RSC_TAG_USED          = 0x4,            // Tag RSC is been used in user code
} RSC_FLAG;

typedef enum {
  FLD_INVALID,                    // VSYM FLD invalid
  FLD_K_ID,                       // VSYM FLD specified by field id/array constant offset
  FLD_K_ANY,                      // VSYM FLD specified by any element
  FLD_K_FLD_UNIQ,                 // VSYM FLD specified by uniq fld name
  FLD_K_FLD_MULTI,                // VSYM FLD specified by several candidates
  FLD_K_CLS_FLD_UNIQ,             // VSYM FLD specified by uniq class and fld, can be compared with FLD_K_ID
  FLD_K_CLS_FLD_MULTI,            // VSYM FLD specified by multiple class and fld
  FLD_K_UNKNOWN,                  // VSYM FLD specified by unkown class and fld
  FLD_K_LAST = FLD_K_UNKNOWN,
} VS_FLD_KIND;

typedef enum {
  VS_NOT_MATCH = 0,           // not the same vsym obj
  VS_EXACT_MATCH,         // the same vsym obj
  VS_MAY_MATCH,           // maybe the same vsym
} VS_MTATCH_KIND;

typedef enum {
  FT_IF       = 0,
  FT_RETURN   = 1,
  FT_CALL     = 2,
} FSM_TRANSITION_TYPE;

typedef enum {
  TS_NOT_MATCH         = 0,   // transition not match
  TS_EXACT_MATCH       = 1,   // transition exact match
  TS_CAND_MATCH        = 2,   // transition match by class hierarchy
  TS_TAG_MATCH         = 3,   // transition match by function tag
} FSM_MATCH_KIND;

typedef enum {
  INVALID_ID = 0
} SYM_ID;

// =============================================================================
//
// RSC_IN : the name and ID of a Resource
//
// =============================================================================
template <class NAME>
class RSC_IN : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( RSC_IN )

private:
  RSC_KIND   _kind : 8;                // source of this rsc_obj
  SUB_KIND   _sub_kind : 8;            // sub kind of RSC_KIND
  mUINT16    _flags;                   // flags
  IDTYPE     _id;                      // identifier
  NAME       _symid_fldid_name;        // which symbol points to this rsc_obj
                                       // HEAP_OBJ: CODEREP
                                       //           LDA d for &d
                                       //           LDID p for p = malloc()
                                       //           IVAR for vor based hor
                                       // VSYM_OBJ: VSYM_FLD_REP
                                       // FSM_OBJ:  STRING
                                       // TAG_OBJ:  STRING

  RSC_IN(void);                        // REQUIRED UNDEFINED UNWANTED methods
  RSC_IN(const RSC_IN&);               // REQUIRED UNDEFINED UNWANTED methods
  RSC_IN& operator = (const RSC_IN&);  // REQUIRED UNDEFINED UNWANTED methods

public:
  
  RSC_IN( IDTYPE id, NAME aux_id ):_kind(RSC_KIND_NONE),_sub_kind(RSC_SUB_NONE),
                                   _flags(RSC_FLAG_NONE),_id(id),
                                   _symid_fldid_name(aux_id) { }
  RSC_IN( RSC_KIND k, IDTYPE id, NAME name ):_kind(k),_sub_kind(RSC_SUB_NONE),
                                   _flags(RSC_FLAG_NONE),_id(id),
                                   _symid_fldid_name(name) { }
  ~RSC_IN(void) { }

  RSC_KIND   Kind(void) const          { return _kind;      }
  void       Set_kind(RSC_KIND k)      { _kind = k;         }
  SUB_KIND   Sub_kind(void) const      { return _sub_kind;  }
  void       Set_sub_kind(SUB_KIND s)  { _sub_kind = s;     }
  IDTYPE     Id(void) const            { return _id; }
  NAME       Tag_name(void) const      { return _symid_fldid_name; }
  void       Set_tag_name(NAME name)   { _symid_fldid_name = name; }
  NAME       Ho_cr() const             { return _symid_fldid_name; }
  void       Set_ho_cr(NAME cr)        { _symid_fldid_name = cr; }
  NAME       Fld_rep(void) const       { return _symid_fldid_name; }
  NAME      *Fld_rep_ptr(void) const   { return (NAME*)&_symid_fldid_name; }
  void       Set_fld_rep(NAME fldid)   { _symid_fldid_name = fldid; }
  NAME       Fsm_name(void) const      { return _symid_fldid_name;  }
  void       Set_fsm_name(NAME name)   { _symid_fldid_name = name; }
  mUINT16    Flag(void) const          { return _flags; }
  BOOL       Is_set_flag(RSC_FLAG f)   { return _flags & f; }
  void       Set_flag(RSC_FLAG f)      { _flags |= f;   }
  void       Clear_flag(RSC_FLAG f)    { _flags &= ~f;  }
  void       Print(FILE *fp) const;
};

// =============================================================================
//
// RSC_BS : the basic information for Resource Modeling, it is common to all
//          resource we model in our system
//
// =============================================================================
template <class NAME, class BS>
class RSC_BS : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( RSC_BS )
  typedef RSC_IN<NAME> RSC_IDNM;

private:
  RSC_IDNM   _id_name;                 // ID and Name
  BS         _base_or_size;            // base for vysm or size in byte in allocation

  RSC_BS(void);                        // REQUIRED UNDEFINED UNWANTED methods
  RSC_BS(const RSC_BS&);               // REQUIRED UNDEFINED UNWANTED methods
  RSC_BS& operator = (const RSC_BS&);  // REQUIRED UNDEFINED UNWANTED methods

public:
  
  RSC_BS( IDTYPE id, NAME aux_id ):_id_name(id, aux_id)
  {
    Is_True(id != MAX_ID, ("RSC_BS::RSC_BS: Failed to allocate new RSC_BS"));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, ">>>>>>>> RSC_BS::RSC_BS _id=%d, _symid_fldid=%ld\n", id, (uint64_t)aux_id));
    _base_or_size = NULL;
  }
  
  ~RSC_BS(void) { }

  RSC_KIND   Kind(void) const          { return _id_name.Kind();      }
  void       Set_kind(RSC_KIND k)      { _id_name.Set_kind(k);        }
  SUB_KIND   Sub_kind(void) const      { return _id_name.Sub_kind();  }
  void       Set_sub_kind(SUB_KIND s)  { _id_name.Set_sub_kind(s);    }
  mUINT16    Flag(void) const          { return _id_name.Flag();      }
  void       Set_flag(RSC_FLAG f)      { _id_name.Set_flag(f);        }
  void       Clear_flag(RSC_FLAG f)    { _id_name.Clear_flag(f);      }
  IDTYPE     Id(void) const            { return _id_name.Id(); }
  NAME       Ho_cr(void) const         { return _id_name.Ho_cr();    }
  void       Set_ho_cr(NAME cr)        { _id_name.Set_ho_cr(cr);     }
  NAME       Fld_rep(void) const       { Is_True(Kind() == RSC_KIND_VSYM, ("Must be vsym"));
                                         return _id_name.Fld_rep();  }
  NAME      *Fld_rep_ptr(void) const   { Is_True(Kind() == RSC_KIND_VSYM, ("Must be vsym"));
                                         return _id_name.Fld_rep_ptr();  }
  void       Set_fld_rep(NAME fldid)   { Is_True(Kind() == RSC_KIND_VSYM, ("Must be vsym"));
                                         _id_name.Set_fld_rep(fldid); }
  NAME       Fsm_name(void) const      { Is_True(Kind() == RSC_KIND_FSM, ("Must be FSM"));
                                         return _id_name.Fsm_name();  }
  void       Set_fsm_name(NAME fldid)  { Is_True(Kind() == RSC_KIND_FSM, ("Must be FSM"));
                                         _id_name.Set_fsm_name( fldid ); }
  BS         Byte_size(void) const     { return _base_or_size; }
  void       Set_byte_size(BS sz)      { _base_or_size = sz;   }
  PHI_NODE  *Base_phi(void) const      { return (PHI_NODE *)_base_or_size; }
  void       Set_base_phi(PHI_NODE *p) { _base_or_size = (BS)p;            }
  CODEREP   *Ref_cr(void) const        { return (CODEREP *)_base_or_size;  }
  void       Set_ref_cr(CODEREP *cr)   { _base_or_size = (BS)cr;           }
  BS         Base_hor(void) const      { Is_True(Kind() == RSC_KIND_VSYM, ("Must be vsym"));
                                         return _base_or_size; }
  void       Set_base_hor(BS hor)      { Is_True(Kind() == RSC_KIND_VSYM, ("Must be vsym"));
                                         _base_or_size = hor;   }
  BS         Fsm(void) const           { Is_True(Kind() == RSC_KIND_FSM, ("Must be fsm"));
                                         return _base_or_size; }
  void       Set_fsm(BS fsm)           { Is_True(Kind() == RSC_KIND_FSM, ("Must be fsm"));
                                         _base_or_size = fsm; }
  void       Print(FILE *fp) const;
};


// =============================================================================
//
// RSC_VER : the utility to manage the resource versioning, including the latest
//           version number of the resource.  Versioning here is the gneral term
//           for numbering a specific resource instance in the context of a
//           procedure.  This class also contain the stack for rename algorithm.
//
// =============================================================================
template <class REPT>
class RSC_VR {
public:

  typedef std::pair< REPT, IDTYPE> TPAIR;

private:
  BB_LIST       *_def_bbs;             // list of BBs defining this rsc_obj
  IDTYPE         _last_version;        // last version it has
  REPT           _entry_chi;           // return this if stack Is_empty
  STACK<TPAIR>  *_stack;               // used in rename

  RSC_VR(void);                        // REQUIRED UNDEFINED UNWANTED methods
  RSC_VR(const RSC_VR&);               // REQUIRED UNDEFINED UNWANTED methods
  RSC_VR& operator = (const RSC_VR&);  // REQUIRED UNDEFINED UNWANTED methods

public:
  RSC_VR( BB_LIST *defbbs, IDTYPE lv, STACK<TPAIR> *stk, REPT chi ):
    _def_bbs(defbbs), _last_version(lv), _entry_chi(chi), _stack(stk) { }

  ~RSC_VR(void) { }

  BB_LIST   *Def_bbs(void) const       { return _def_bbs; }
  void       Prepend_def_bbs(BB_NODE *bb, MEM_POOL *p)
                                       {
                                         if(_def_bbs == NULL) {
                                           _def_bbs = (BB_LIST*)CXX_NEW( BB_LIST(bb), p );
                                           _def_bbs->Set_Next(NULL);
                                         } else if(!_def_bbs->Contains(bb)) {
                                           _def_bbs = _def_bbs->Prepend(bb,p);
                                         }
                                       }
  INT32      Gen_version(void)         { return ++_last_version; }
  IDTYPE     Last_version(void) const  { return _last_version; }
  void       Set_entry_chi(REPT sor)   { _entry_chi = sor;}
  BOOL       Is_entry_chi(REPT sor)    { return _entry_chi == sor; }
  REPT       Entry_chi(void) const     { return _entry_chi; }
  void       Set_stack(STACK<TPAIR> *s){ _stack = s; }
  STACK<TPAIR>  *Stack(void) const     { return _stack; }
  REPT       Top_of_stack(void)
  {
    if (_stack->Is_Empty()) return _entry_chi;
    return _stack->Top().first;
  }
  BOOL       Top_match_sr(STMTREP* sr)
  {
    return _stack->Top().second == sr->Stmtrep_id();
  }
  void       Push(REPT t, STMTREP* sr) {
    IDTYPE id = sr? sr->Stmtrep_id(): 0;
    // Only keep 1 version in top of stack
    // for the same stmt
    if(sr && _stack->Top().second == id)
    {
      if(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
        fprintf(TFile, "RSC_OBJ::Pop " );
        _stack->Top().first->Print(TFile);
        fprintf(TFile, "Replace with ");
        t->Print(TFile);
        fprintf(TFile, ", stmtrep_id = %d \n", _stack->Top().second);
      }
      _stack->Pop();
    }

    if(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
      fprintf(TFile, "RSC_OBJ::Push " );
      t->Print(TFile);
      fprintf(TFile, ", stmtrep_id = %d \n", id);
    }
    TPAIR tp(t, id);
    _stack->Push(tp);
  }

  REPT       Pop() {
    if(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG)) {
      fprintf(TFile, "RSC_OBJ::Pop " );
      _stack->Top().first->Print(TFile);
      fprintf(TFile, ", stmtrep_id = %d\n", _stack->Top().second);
    }
    return _stack->Pop().first;
  }
};

template <class NAME, class BS, class REPT>
class RSC_OBJ : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( RSC_OBJ )
public:

  typedef RSC_BS<NAME, BS> RSC_BASE;
  typedef RSC_VR<REPT> RSC_VER;
  typedef std::pair< REPT, IDTYPE> TPAIR;

private:
  RSC_BASE  *_rsc_base;                // the base info
  RSC_OBJ   *_basedon;                 // q = p infers 'q based-on p'
  RSC_VER    _vermgr;                  // manage the versioning business

  RSC_OBJ(void);                       // REQUIRED UNDEFINED UNWANTED methods
  RSC_OBJ(const RSC_OBJ&);             // REQUIRED UNDEFINED UNWANTED methods
  RSC_OBJ& operator = (const RSC_OBJ&);// REQUIRED UNDEFINED UNWANTED methods

public:

  RSC_OBJ( IDTYPE id, NAME aux_id, MEM_POOL *pool ):
    _vermgr(NULL,0, NULL,NULL)
  {
    Is_True(id != MAX_ID, ("RSC_OBJ::RSC_OBJ: Failed to allocate new RSC_OBJ"));
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, ">>>>>>>> RSC_OBJ::RSC_OBJ _id=%d, _symid_fldid=%ld\n", id, (uint64_t)aux_id));
    _rsc_base = CXX_NEW(RSC_BASE(id, aux_id), pool);
    _basedon = NULL;
  }

  RSC_OBJ( RSC_BASE *rsc_base ): _rsc_base(rsc_base), _basedon(NULL),
    _vermgr(NULL,0, NULL,NULL) { }

  ~RSC_OBJ(void) { }

  RSC_KIND   Kind(void) const          { return _rsc_base->Kind();     }
  void       Set_kind(RSC_KIND k)      { _rsc_base->Set_kind( k );     }
  SUB_KIND   Sub_kind(void) const      { return _rsc_base->Sub_kind(); }
  void       Set_sub_kind(SUB_KIND s)  { _rsc_base->Set_sub_kind(s);   }
  mUINT16    Flag(void) const          { return _rsc_base->Flag(); }
  void       Set_flag(RSC_FLAG f)      { _rsc_base->Set_flag(f);   }
  void       Clear_flag(RSC_FLAG f)    { _rsc_base->Clear_flag(f);  }
  IDTYPE     Id(void) const            { return _rsc_base->Id();    }
  IDTYPE     Sym_id(void) const;
  NAME       Ho_cr(void) const         { return _rsc_base->Ho_cr();    }
  void       Set_ho_cr(NAME cr)        { Is_True((Ho_cr() == NULL ||
                                                  Kind() != RSC_KIND_LDA),
                                                 ("Change Sym_id for LDA"));
                                                 _rsc_base->Set_ho_cr(cr); }
  NAME       Fld_rep(void) const       { return _rsc_base->Fld_rep();  }
  NAME      *Fld_rep_ptr(void) const   { return _rsc_base->Fld_rep_ptr(); }
  void       Set_fld_rep(NAME fldid)   { _rsc_base->Set_fld_rep(fldid); }
  NAME       Fsm_name(void) const      { return _rsc_base->Fsm_name();  }
  void       Set_fsm_name(NAME fldid)  { _rsc_base->Set_fsm_name(fldid); }
  BS         Byte_size(void) const     { return _rsc_base->Byte_size();  }
  void       Set_byte_size(BS sz)      { _rsc_base->Set_byte_size(sz);   }
  PHI_NODE  *Base_phi(void) const      { return _rsc_base->Base_phi();   }
  void       Set_base_phi(PHI_NODE *p) { _rsc_base->Set_base_phi(p);     }
  CODEREP   *Ref_cr(void) const        { return _rsc_base->Ref_cr();     }
  void       Set_ref_cr(CODEREP *cr)   { _rsc_base->Set_ref_cr(cr);      }
  BS         Base_hor(void) const      { return _rsc_base->Base_hor();   }
  void       Set_base_hor(BS hor)      { _rsc_base->Set_base_hor(hor);   }
  BS         Fsm(void) const           { return _rsc_base->Fsm(); }
  void       Set_fsm(BS fsm)           { _rsc_base->Set_fsm(fsm); }
  void       Print(FILE *fp) const;
  // Based_on/Get_based_on/Set_based_on is useless in new HO/VO
  RSC_OBJ   *Based_on(void) const      { return _basedon;  }
  RSC_OBJ   *Get_based_on(void)        { return (Based_on() == NULL)? this :
                                                 Based_on()->Get_based_on(); }
  void       Set_based_on(RSC_OBJ *so) { Is_True(this != so->Get_based_on(), ("based on itself"));
                                         if (Sym_id() != so->Sym_id() )
                                           _basedon = so; }

  BB_LIST   *Def_bbs(void) const       { return _vermgr.Def_bbs(); }
  void       Prepend_def_bbs(BB_NODE *bb, MEM_POOL *p) { _vermgr.Prepend_def_bbs(bb, p); }
  INT32      Gen_version(void)         { return _vermgr.Gen_version(); }
  IDTYPE     Last_version(void) const  { return _vermgr.Last_version(); }
  void       Set_entry_chi(REPT sor)   { _vermgr.Set_entry_chi(sor);}
  BOOL       Is_entry_chi(REPT sor)    { return _vermgr.Is_entry_chi(sor); }
  REPT       Entry_chi(void) const     { return _vermgr.Entry_chi(); }
  void       Set_stack(STACK<TPAIR> *s){ _vermgr.Set_stack(s); }
  STACK<TPAIR>  *Stack(void) const     { return _vermgr.Stack(); }
  REPT       Top_of_stack(void)        { return _vermgr.Top_of_stack(); }
  BOOL       Top_match_sr(STMTREP* sr) { return _vermgr.Top_match_sr(sr); }
  void       Push(REPT t, STMTREP* sr) { _vermgr.Push(t, sr); }
  REPT       Pop()                     { return _vermgr.Pop(); }
  
  void       Add_rept(REPT sor) {
    REPT entry = Entry_chi();
    Is_True(entry != NULL && sor != NULL &&
            sor->Next() == NULL, ("bad list"));
    sor->Set_Next(entry->Next());
    entry->Set_Next(sor);
  }
}; // end of RSC_OBJ


enum RSC_OBJ_VERSION {
  ROR_VERSION_NOT_SET   = 0,
  ROR_VERSION_ENTRY_CHI = -1,
};

typedef enum {
  ROR_DEF_BY_NONE   = 0,
  ROR_DEF_BY_PHI    = 1,
  ROR_DEF_BY_CHI    = 2,
  ROR_DEF_BY_ISTORE = 3,
  ROR_DEF_BY_IPARM  = 4,               // passed from caller
  ROR_DEF_BY_AUTO   = 5,               // local pointer assigned from other src
  ROR_DEF_BY_ALLOC  = 6,               // malloc equivalence creation
  ROR_DEF_BY_FREE   = 7,               // free triggers a new heap_obj_rep
  ROR_DEF_BY_COPY   = 8,               // copied from rhs of an asgn stmt 
  ROR_DEF_BY_LDA    = 9,               // address of a variable
  ROR_DEF_BY_NULL   = 10,              // this NULL object represent NULL ptr
  ROR_DEF_BY_TRANSIT= 11,              // FSM object created by state transition
  ROR_DEF_BY_DANGLE = 12,              // variation of FREE, updated by callee
  ROR_DEF_BY_ALLOCA = 13,              // model pointer created by opr_alloca
  ROR_DEF_BY_VARPHI = 14,              // heap object defined by variable phi
  ROR_DEF_BY_VORPHI = 15,              // heap object defined by VO phi
  ROR_DEF_BY_HORPHI = 16,              // vsym object defined by HO phi
  ROR_DEF_BY_LAST   = ROR_DEF_BY_HORPHI,
  ROR_DEF_ATTRS     = 0xff,            // mask for DEF attributes
  ROR_ASGN_TO_NONE  = 0,               // assigned not set
  ROR_ASGN_TO_GLOBAL= 0x100,           // assigned to global pointer
  ROR_ASGN_TO_RETREG= 0x200,           // assigned to return register
  ROR_ASGN_TO_LOCAL = 0x400,           // assigned to local or preg
  ROR_ASGN_TO_OPARM = 0x800,           // assigned to output param
  ROR_ASGN_ATTRS    = 0xf00,           // mask for ASGN attributes
  TAG_ASGN_TO_VAR   = 0x100,           // used by TAG_OBJ_REP
  TAG_ASGN_TO_VOR   = 0x200,           // used by TAG_OBJ_REP
  TAG_ASGN_TO_HOR   = 0x400,           // used by TAG_OBJ_REP
  TAG_ASGN_ATTRS    = 0x700,           // used by TAG_OBJ_REP
  ROR_VSYM_TAINTED  = 0x1000,          // for VSYM_OBJ_REP, tainted vsym
  ROR_VSYM_MAY_DEF  = 0x2000,          // for VSYM_OBJ_REP, vsym may be defined(caused by alias)
  ROR_VSYM_ATTRS    = 0xf000,          // mask for VSYM attributes
  ROR_HEAP_INJURED  = 0x10000,         // for HEAP_OBJ_REP, injured hor in loop like "while(...) { p = p->next; }"
  ROR_HEAP_ATTRS    = 0xf0000,         // mask for HEAP attributes
} ROR_ATTR;


// =============================================================================
//
// VSYM_FLD_REP class is used to specify the fld info for VSYM_OBJ to represent
// cases when field id is unkown.
// IN JNI code field id is often speicified by function GetFieldId with a given
// field name. Before create VSYM_OBJ, will try to find the field name by U-D.
// If found a constant string the fld name will be set,
// otherwise will generate "UNK" for it.
// Different vsym will be created based on different fld kind.
// Field name is composed by field names and coderep_id (if name is unkown),
// IPSA::_fld_name_id_map stores the ID and string mapping.
//
// =============================================================================
class VSYM_FLD_REP {
  private:
    VS_FLD_KIND   _kind    : 8;           // kind
    IDTYPE        _id_name : 24;          // field id or field string id
    mINT32        _ofst;                  // offset for array and array in struct

    VSYM_FLD_REP (void);                  // REQUIRED UNDEFINED UNWANTED methods

  public:
    VSYM_FLD_REP(VS_FLD_KIND kind, IDTYPE id, mINT32 ofst)
      : _kind(kind), _id_name(id), _ofst(ofst) { }

    operator uint64_t() const { return *(uint64_t*)this; }

    bool Find(const VSYM_FLD_REP* o) const
    {
      if(_kind != o->Kind()) {
        return FALSE;
      } else {
        return (_ofst != 0 || o->Ofst() != 0)
                 ? _ofst == o->Ofst()
                 : _id_name == o->Fld_id_name();
      }
    }

    BOOL Is_uniq_id() const
    {
      if(_kind == FLD_K_ID || _kind == FLD_K_CLS_FLD_UNIQ) {
        return TRUE;
      }
      return FALSE;
    }

    BOOL Is_any() const
    {
      return (_kind == FLD_K_ANY);
    }

    BOOL Aliased(VSYM_FLD_REP o) const
    {
      if (_kind == FLD_K_ANY) {
        // `this' is aggregate and o is in this aggregate
        if (o.Kind() == FLD_K_ID &&
            (_id_name == 0 ||
             _id_name == o.Fld_id_name()))
          return TRUE;
        // both are aggregates. 1 is whole, the other is part
        if (o.Kind() == FLD_K_ANY &&
            _id_name != o.Fld_id_name() &&
            (_id_name == 0 ||
             o.Fld_id_name() == 0))
          return TRUE;
      }
      else if (_kind == FLD_K_ID) {
        // `this' is in aggregate of `o'
        if (o.Kind() == FLD_K_ANY &&
            (o.Fld_id_name() == _id_name ||
             o.Fld_id_name() == 0))
          return TRUE;
      }
      return FALSE;
    }

    VS_MTATCH_KIND Match(const VSYM_FLD_REP* o) const
    {
      if(_kind == FLD_K_UNKNOWN || o->Kind() == FLD_K_UNKNOWN) {
        return VS_NOT_MATCH;
      } else if(_kind == o->Kind() || (Is_uniq_id() && o->Is_uniq_id())) {
        return (_ofst != 0 || o->Ofst() != 0)
                 ? (_ofst == o->Ofst() ? VS_EXACT_MATCH : VS_NOT_MATCH)
                 : (_id_name == o->Fld_id_name() ? VS_EXACT_MATCH : VS_NOT_MATCH);
      } else if(Is_any() && o->Is_any() && _id_name == o->Fld_id_name()) {
        return VS_EXACT_MATCH;
      } else if (Is_any() || o->Is_any()) {
        return VS_NOT_MATCH;
      } else if(Is_uniq_id() || o->Is_uniq_id()) {
        // one of them is uniq
        return VS_MAY_MATCH;
      } else {
        return VS_NOT_MATCH;
      }
    }

    INT Compare(VSYM_FLD_REP vfr) const
    {
      uint64_t lhs = *(uint64_t*)this;
      uint64_t rhs = *(uint64_t*)&vfr;
      return lhs < rhs ? -1
                       : lhs > rhs ? 1 : 0;
    }

    void Get_fld_names(IPSA *ipsa, vector<char *>& names) const {
      Is_True(Kind() != FLD_K_ID && Kind() != FLD_K_CLS_FLD_UNIQ, ("invalid kind"));
      const char *fld_name = ipsa->Fld_id_2_name(Fld_name_idx());
      if(fld_name != NULL) {
        const char *p = fld_name;
        char *new_name = (char *) malloc(strlen(fld_name) + 1);
        char *start = new_name;
        while(*p != '\0') {
          if(*p == '|' || *p == ':') {
            *new_name = '\0';
            names.push_back(start);
            new_name = (char *) malloc(strlen(fld_name) + 1);
            start = new_name;
          } else {
            *new_name = *p;
            new_name++;
          }
          p++;
        }
        *new_name = '\0';
        if(strlen(start) != 0) {
          names.push_back(start);
        }
      }
    }
    VS_FLD_KIND Kind(void) const         { return _kind;  }
    void        Set_kind(VS_FLD_KIND k)  { _kind = k; }
    IDTYPE      Fld_id_name() const      { return _id_name; }
    IDTYPE      Fld_id(void) const       { Is_True(Is_uniq_id(), ("Must be fld_id"));
                                           return _id_name;  }
    mINT32      Ofst(void) const         { return _ofst; }
    void        Set_fld_id(IDTYPE id)    { Is_True(Is_uniq_id(), ("Must be fld_id"));
                                          _id_name = id;  }
    IDTYPE      Fld_name_idx(void) const { Is_True(!Is_uniq_id(), ("Must be fld name"));
                                           return _id_name; }
    void        Set_fld_name(IDTYPE nm)  { Is_True(!Is_uniq_id(), ("Must be fld name"));
                                           _id_name = nm; }
    void        Print(FILE *fp) const
    {
      switch(Kind()) {
        case FLD_K_ID:
        case FLD_K_CLS_FLD_UNIQ:
          fprintf(fp, "id:%d ofst:%d", Fld_id(), Ofst());
          break;
        case FLD_K_ANY:
          fprintf(fp, "any id:%d ofst:%d", Fld_id_name(), Ofst());
          break;
        case FLD_K_FLD_UNIQ:
        case FLD_K_FLD_MULTI:
        case FLD_K_CLS_FLD_MULTI:
        case FLD_K_UNKNOWN:
          fprintf(fp, "fnm: %d", Fld_name_idx());
          break;
        default:
          Is_True(false, ("unexpected FLD kind"));
          return;
      }
    }
    void        Print(IPSA *ipsa, FILE *fp) const
    {
      switch(Kind()) {
        case FLD_K_ID:
        case FLD_K_CLS_FLD_UNIQ:
          fprintf(fp, "id:%d ofst:%d", Fld_id(), Ofst());
          break;
        case FLD_K_ANY:
          fprintf(fp, "any id:%d ofst:%d", Fld_id_name(), Ofst());
          break;
        case FLD_K_FLD_UNIQ:
        case FLD_K_FLD_MULTI:
        case FLD_K_CLS_FLD_MULTI:
        case FLD_K_UNKNOWN:
        {
          IDTYPE id = Fld_name_idx();
          fprintf(fp, "fnm:%d->[%s]", id, ipsa->Fld_id_2_name(id));
          break;
        }
        default:
          Is_True(false, ("unexpected FLD kind"));
          return;
      }
    }
};

// =============================================================================
//
// HEAP_OBJ is specialized version of RSC_OBJ, for heap object management. 
//
// HEAP_OBJ_REP represents a sepcific version of HEAP_OBJ, it is in SSA form to
//              keep track of ISTORE/ILOD
//
// HO_LIST defines the list of heap_obj. This list is maintained in Class VSA.
// HO_LIST_ITER simplifies the traversal of the HO_LIST
//
// =============================================================================

typedef RSC_OBJ<CODEREP*, CODEREP*, HEAP_OBJ_REP*> HEAP_OBJ;
typedef RSC_OBJ<VSYM_FLD_REP, HEAP_OBJ_REP*, VSYM_OBJ_REP*> VSYM_OBJ;

// HEAP_OBJ::Sym_id
template<> inline IDTYPE
RSC_OBJ<CODEREP*, CODEREP*, HEAP_OBJ_REP*>::Sym_id() const {
  CODEREP *cr = Ho_cr();
  if (cr != NULL) {
    return cr->Kind() == CK_LDA ? cr->Lda_aux_id() :
             cr->Kind() == CK_VAR ? cr->Aux_id() :
               (cr->Kind() == CK_IVAR && cr->Ivar_mu_node()) ?
                 cr->Ivar_mu_node()->Aux_id() : INVALID_ID;
  }
  else {
    return INVALID_ID;
  }
}
   
// HEAP_OBJ::Print
template<> inline void
RSC_OBJ<CODEREP*, CODEREP*, HEAP_OBJ_REP*>::Print(FILE *fp) const {
  fprintf(fp, "Heap object #%d, last_version: %d, cr: ",
              Id(), Last_version());
  if (Ho_cr()) {
    fprintf(fp, "\n");
    Ho_cr()->Print(0, fp);
  }
  else {
    fprintf(fp, "<null>\n");
  }
}

// VSYM_OBJ::Print
template<> inline void
RSC_OBJ<VSYM_FLD_REP, HEAP_OBJ_REP*, VSYM_OBJ_REP*>::Print(FILE *fp) const {
  fprintf(fp, "Vsym object #%d, ", Id());
  Fld_rep().Print(fp);
  fprintf(fp, ", last_version:%d\n", Last_version());
}

//  HO_LIST is a internal linked list
//
class HO_LIST : public SLIST {
  DECLARE_SLIST_CLASS (HO_LIST, HEAP_OBJ)
private:
  HO_LIST(const HO_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  HO_LIST& operator = (const HO_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~HO_LIST(void)                       {}
  HEAP_OBJ  *Find(IDTYPE symid, BOOL is_lda) ;
  HEAP_OBJ  *Find(VSA *vsa, AUX_STAB_ENTRY *sym, BOOL is_lda) ;
  INT        Count(void);
  void       Print(FILE *fp=stderr) ;
};

class HO_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (HO_LIST_ITER, HEAP_OBJ, HO_LIST)
  HO_LIST_ITER(const HO_LIST_ITER&);   // REQUIRED UNDEFINED UNWANTED methods
  HO_LIST_ITER& operator = (const HO_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~HO_LIST_ITER(void)                       {}
  HEAP_OBJ   *First_elem(void)         { return First(); }
  HEAP_OBJ   *Next_elem(void)          { return Next();  }
};

class FIELD_OBJ_REP : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( FIELD_OBJ_REP )

private:
  HEAP_OBJ_REP *_field_hor;
  VSYM_FLD_REP  _field_rep;

  FIELD_OBJ_REP(const FIELD_OBJ_REP&);            // disable copy ctor
  FIELD_OBJ_REP& operator=(const FIELD_OBJ_REP&); // disable assignment

public:
  FIELD_OBJ_REP(HEAP_OBJ_REP* hor, VS_FLD_KIND kind, IDTYPE id, mINT32 ofst)
    : _field_hor(hor), _field_rep(kind, id, ofst) { }

  FIELD_OBJ_REP(HEAP_OBJ_REP* hor, VSYM_FLD_REP vfr)
    : _field_hor(hor), _field_rep(vfr) { }

  HEAP_OBJ_REP *Hor() const      { return _field_hor; }
  void          Set_hor(HEAP_OBJ_REP* hor) { _field_hor = hor; }
  const VSYM_FLD_REP *Fld_rep() const      { return &_field_rep; }
  VS_FLD_KIND   Fld_kind() const { return _field_rep.Kind(); }

  void          Print(FILE* fp) const;
};

class HEAP_OBJ_REP : public SLIST_NODE {

private:
  HEAP_OBJ      *_heap_obj;        // which heap_obj it belongs to
  union {
    PHI_NODE    *_phi_def;         // the phi node that define this
    STMTREP     *_stmt_def;        // the stmtrep that defines this
  };
  SRCPOS_NODE    _srcpos_node;     // where this version is created
  
  IDTYPE         _version;         // version #, for tracing purpose
  ROR_ATTR       _attr : 24;       // must check ROR_ATTR element count
  BOOL           _escaped : 1;
  BOOL           _fld_any : 1;     // has FLD_K_ANY vor
  BOOL           _uniq_vo : 1;     // has uniq vo
  HOR_LIST      *_unified_list;    // result from one pointer points to muliple heap_obj
  FIELD_OBJ_REP *_field_list;      // p->field = q, add q's hor to p hor's _field_list
  HEAP_OBJ_REP  *_prev_ver;        // the previous hor of the same heap_obj
  TOR_LIST_OLD  *_tor_list;        // binded tag obj list
  VSYM_OBJ      *_vsym_obj;        // first vsym obj on this hor
  VSYM_OBJ      *_owner_vo;        // which vo it belongs to (only set if uniq)

  HEAP_OBJ_REP(void);                  // REQUIRED UNDEFINED UNWANTED methods
  HEAP_OBJ_REP(const HEAP_OBJ_REP&);   // REQUIRED UNDEFINED UNWANTED methods
  HEAP_OBJ_REP& operator = (const HEAP_OBJ_REP&); // REQUIRED UNDEFINED UNWANTED methods

public:
  HEAP_OBJ_REP( HEAP_OBJ *ho, HEAP_OBJ_REP *prev )
  : _heap_obj(ho), _prev_ver(prev),
    _escaped(FALSE), _fld_any(FALSE), _tor_list(NULL), _vsym_obj(NULL),
    _owner_vo(NULL), _uniq_vo(TRUE)
  {
    VSA_STATS_inc(hor);
    _phi_def = NULL;
    _version = ROR_VERSION_NOT_SET;
    _attr = ROR_DEF_BY_NONE;
    _unified_list = NULL;
    _field_list = NULL;
    if (ho->Entry_chi())
      ho->Add_rept(this);
  }

  HEAP_OBJ_REP  *Next(void) const              { return (HEAP_OBJ_REP *)SLIST_NODE::Next(); }
  void           Set_Next(HEAP_OBJ_REP *v)     { SLIST_NODE::Set_Next(v); }

  VSYM_OBJ      *Vsym_obj() const              { return _vsym_obj; }
  void           Set_vsym_obj(VSYM_OBJ *vo)    { _vsym_obj = vo; }
  VSYM_OBJ      *Find(VSYM_FLD_REP *vfr) const
  {
    VSYM_OBJ *vo = _vsym_obj;
    Is_True(vo == NULL || vo->Base_hor() == this, ("invalid base hor"));
    if (vo == NULL || vo->Base_hor() != this)
      return NULL;
    do {
      if (vo->Fld_rep().Find(vfr))
        return vo;
      vo = vo->Next();
      if (vo == NULL || vo->Base_hor() != this)
        return NULL;
    } while (TRUE);
    return NULL;
  }

  VSYM_OBJ      *Owner_vo(void) const          { return _uniq_vo ? _owner_vo : NULL ; }
  void           Set_owner_vo(VSYM_OBJ *vo)    { if (_owner_vo && _owner_vo != vo)
                                                   _uniq_vo = FALSE;
                                                 else
                                                   _owner_vo = vo;
                                               }

  HEAP_OBJ      *Heap_obj(void) const          { return _heap_obj; }
  HEAP_OBJ      *Rsc_obj(void) const           { return _heap_obj; }
  void           Set_heap_obj(HEAP_OBJ *ho)    { _heap_obj = ho; }
  IDTYPE         Heap_obj_id(void) const       { return _heap_obj ? _heap_obj->Id() : -1; }
  IDTYPE         Version(void) const           { return _version; }
  void           Set_version(IDTYPE v)         { _version = v; }
  void           Set_version_entry_chi(void)   { _version = ROR_VERSION_ENTRY_CHI; }
  BOOL           Version_entry_chi(void) const { return _version == ROR_VERSION_ENTRY_CHI; }
  BOOL           Version_not_set(void) const   { return _version == ROR_VERSION_NOT_SET; }
  SRCPOS_NODE    Srcpos_node(void) const       { return _srcpos_node; }
  SRCPOS_NODE   *Srcpos_nodep(void)            { return &_srcpos_node; }
  void           Set_srcpos_node(STMTREP* stmt,
                                 DNA_NODE* dna,
                                 PATH_INFO info)
                                               { _srcpos_node.Set_info(info);
                                                 _srcpos_node.Set_dna(dna);
                                                 _srcpos_node.Set_stmt(stmt); }
  void           Set_srcpos_node(BB_NODE* bb,
                                 DNA_NODE* dna,
                                 PATH_INFO info)
                                               { _srcpos_node.Set_info(info);
                                                 _srcpos_node.Set_dna(dna);
                                                 _srcpos_node.Set_bb(bb); }
  PHI_NODE      *Phi_def(void) const           { Is_True(Attr() == ROR_DEF_BY_PHI ||
                                                         Attr() == ROR_DEF_BY_VARPHI ||
                                                         Attr() == ROR_DEF_BY_VORPHI,
                                                         ("not def by phi"));
                                                 return _phi_def; }
  void           Set_phi_def(PHI_NODE *phi)    { Is_True(Attr() == ROR_DEF_BY_PHI ||
                                                         Attr() == ROR_DEF_BY_VARPHI ||
                                                         Attr() == ROR_DEF_BY_VORPHI,
                                                         ("not def by phi"));
                                                 _phi_def = phi; }
  BOOL          Is_phi(void) const             { return (Attr() == ROR_DEF_BY_PHI    ||
                                                         Attr() == ROR_DEF_BY_VARPHI ||
                                                         Attr() == ROR_DEF_BY_VORPHI); }
  STMTREP       *Stmt_def(void) const          { Is_True(Has_defstmt(), ("not def by stmt"));
                                                 return _stmt_def; }
  STMTREP       *Defstmt(void) const           { return Stmt_def(); }
  BOOL           Has_defstmt() const           { return Attr() == ROR_DEF_BY_CHI ||
                                                         Attr() == ROR_DEF_BY_ISTORE ||
                                                         Attr() == ROR_DEF_BY_COPY ||
                                                         Attr() == ROR_DEF_BY_ALLOC ||
                                                         Attr() == ROR_DEF_BY_ALLOCA ||
                                                         Attr() == ROR_DEF_BY_FREE ||
                                                         Attr() == ROR_DEF_BY_DANGLE ||
                                                         Attr() == ROR_DEF_BY_LDA; }
  void           Set_stmt_def(STMTREP *stmt,
                              DNA_NODE *dna)   { Is_True(Attr() == ROR_DEF_BY_CHI ||
                                                         Attr() == ROR_DEF_BY_ISTORE ||
                                                         Attr() == ROR_DEF_BY_COPY ||
                                                         Attr() == ROR_DEF_BY_ALLOC ||
                                                         Attr() == ROR_DEF_BY_ALLOCA ||
                                                         Attr() == ROR_DEF_BY_FREE ||
                                                         Attr() == ROR_DEF_BY_LDA,
                                                         ("not def by stmt"));
                                                 Is_True(_stmt_def == NULL || _stmt_def == stmt,
                                                         ("def stmt already set"));
                                                 _stmt_def = stmt;
                                                 if(Attr() == ROR_DEF_BY_COPY)
                                                   _srcpos_node.Set_info(PATHINFO_COPY);
                                                 else if(Attr() == ROR_DEF_BY_CHI)
                                                   _srcpos_node.Set_info(PATHINFO_CHI);
                                                 else if(Attr() == ROR_DEF_BY_ISTORE)
                                                   _srcpos_node.Set_info(PATHINFO_ISTORE);
                                                 else if(Attr() == ROR_DEF_BY_ALLOC)
                                                   _srcpos_node.Set_info(PATHINFO_ALLOC);
                                                 else if(Attr() == ROR_DEF_BY_FREE)
                                                   _srcpos_node.Set_info(PATHINFO_FREE);
                                                 else if(Attr() == ROR_DEF_BY_LDA)
                                                   _srcpos_node.Set_info(PATHINFO_LDA);
                                                 _srcpos_node.Set_stmt(stmt);
                                                 _srcpos_node.Set_dna(dna);
                                               }
  BOOL           Is_entry_chi(void)            { return _heap_obj->Is_entry_chi(this); }
  BOOL           Is_phi_identical(void) const;

  IDTYPE         Gen_name(STMTREP*);

  HOR_LIST      *Ulist() const                 { return _unified_list; }
  void           Set_ulist(HOR_LIST *ul)       { _unified_list = ul; }

  FIELD_OBJ_REP *Flist() const                 { return _field_list; }
  void           Set_flist(FIELD_OBJ_REP *fl)  { _field_list = fl; }
  void           Prepend_fl(FIELD_OBJ_REP *fl) { fl->Set_Next(Flist()); Set_flist(fl); }
  FIELD_OBJ_REP *Find_fld(const VSYM_FLD_REP* vfr) const;

  HEAP_OBJ_REP  *Prev_ver() const              { return _prev_ver; }
  void           Set_prev_ver(HEAP_OBJ_REP *p) { Is_True(p && p != this,
                                                    ("Hor previous same as hor, hor attr : %d, ho%dv%d", 
                                                    p->Attr(), p->Heap_obj()->Id(), p->Version()));
                                                 _prev_ver = p; }
  ROR_ATTR       Attr(void) const              { return ROR_ATTR(_attr&ROR_DEF_ATTRS); }
  ROR_ATTR       Asgn_attr(void) const         { return ROR_ATTR(_attr&ROR_ASGN_ATTRS); }
  ROR_ATTR       Vsym_attr(void) const         { return ROR_ATTR(_attr&ROR_VSYM_ATTRS); }
  ROR_ATTR       Heap_attr(void) const         { return ROR_ATTR(_attr&ROR_HEAP_ATTRS); }
  void           Set_asgn_attr(ROR_ATTR k)     { _attr = ROR_ATTR(_attr | k); }
  void           Set_vsym_attr(ROR_ATTR k)     { _attr = ROR_ATTR(_attr | k); }
  void           Set_heap_attr(ROR_ATTR k)     { _attr = ROR_ATTR(_attr | k); }
  void           Set_attr(ROR_ATTR k)          { _attr = k; if (k == ROR_DEF_BY_IPARM)
                                                              _heap_obj->Set_kind(RSC_KIND_IPARM);
                                                            else if (k == ROR_DEF_BY_AUTO) 
                                                              _heap_obj->Set_kind(RSC_KIND_AUTO);
                                                            else if (k == ROR_DEF_BY_LDA)
                                                              _heap_obj->Set_kind(RSC_KIND_LDA);
                                                            else if (k == ROR_DEF_BY_ALLOCA)
                                                              _heap_obj->Set_kind(RSC_KIND_ALLOCA);
                                                            else if (k == ROR_DEF_BY_ALLOC)
                                                              _heap_obj->Set_kind(RSC_KIND_ALLOC); }
  void           Set_injured()                 { Set_heap_attr(ROR_HEAP_INJURED); }
  BOOL           Injured() const               { return (_attr & ROR_HEAP_INJURED) == ROR_HEAP_INJURED; }
  void           Set_escaped(BOOL escaped)     { _escaped = escaped; }
  BOOL           Escaped() const               { return _escaped;    }
  void           Set_field_any(BOOL any)       { _fld_any = any;     }
  BOOL           Field_any() const             { return _fld_any;    }

  void           Set_tor_list(TOR_LIST_OLD *tor_list) { _tor_list = tor_list; }
  TOR_LIST_OLD  *Tor_list() const              { return _tor_list; }
  void           Print(FILE *fp) const         { fprintf(fp, "ho%dv%d", Heap_obj_id(), Version()); }
  void           Print() const;
  void           Print_detail(FILE *fp) const;
}; // end of HEAP_OBJ_REP

// iterator to traverse all vo based on the same hor
class HOR_VO_LIST_ITER {
private:
  HEAP_OBJ_REP *_hor;     // heap obj on which the vsym is based
  VSYM_OBJ     *_iter;    // current vsym obj on this heap obj

public:
  // constructor
  HOR_VO_LIST_ITER(HEAP_OBJ_REP *hor) : _hor(hor), _iter(hor->Vsym_obj()) {}

  void      Init() {}

  VSYM_OBJ *First()
  {
    return _iter;
  }

  BOOL      Is_Empty() const
  {
    return _iter == NULL || _iter->Base_hor() != _hor;
  }

  VSYM_OBJ *Next()
  {
    if (_iter == NULL)
      return NULL;
    // move to next
    _iter = _iter->Next();
    // check if next is based on same hor
    if (_iter && _iter->Base_hor() != _hor)
      _iter = NULL;
    return _iter;
  }
}; // HOR_VO_LIST_ITER

// HOR_NODE is a linked list node
// HEAP_OBJ_REP may be appended to multiple lists. So use HEAP_OBJ_REP::_next
// is not enough
class HOR_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(HOR_NODE);

private:
  HEAP_OBJ_REP *_hor;

public:
  HOR_NODE(HEAP_OBJ_REP *hor) : _hor(hor) {}

  // return HEAP_OBJ_REP
  HEAP_OBJ_REP *Hor() const { return _hor; }
};

//  HOR_LIST is a internal linked list
//
class HOR_LIST : public SLIST {
  //DECLARE_SLIST_CLASS (HOR_LIST, HOR_NODE)
private:
  HOR_LIST(const HOR_LIST&);              // REQUIRED UNDEFINED UNWANTED methods
  HOR_LIST& operator = (const HOR_LIST&); // REQUIRED UNDEFINED UNWANTED methods

public:
  HOR_LIST(void) : SLIST()                {}
  ~HOR_LIST(void)                         {}
  HOR_NODE *Head()                        { return (HOR_NODE *)SLIST::Head(); }
  void Append(HOR_NODE *nd)               { SLIST::Append(nd); }
  BOOL Append(HOR_NODE *nd, HOR_NODE *od) { return SLIST::Append(nd, od); }

  HOR_NODE *Remove(HOR_NODE *prev, HOR_NODE *cur) {
    return (HOR_NODE *)SLIST::Remove(prev, cur);
  }

  HEAP_OBJ_REP  *Find(HEAP_OBJ_REP *hor) ;
  INT            Count(void);
  void           Print(FILE *fp=stderr) ;
};

class HOR_LIST_ITER : public SLIST_ITER {
private:
  //DECLARE_SLIST_ITER_CLASS (HOR_LIST_ITER, HOR_NODE, HOR_LIST)
  HOR_LIST_ITER(const HOR_LIST_ITER&);             // REQUIRED UNDEFINED UNWANTED methods
  HOR_LIST_ITER& operator = (const HOR_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  HOR_LIST_ITER(void)       { SLIST_ITER::Init(); }
  ~HOR_LIST_ITER(void)      {}

  HEAP_OBJ_REP *First(void) {
    HOR_NODE *n = (HOR_NODE *)SLIST_ITER::First();
    return n ? n->Hor() : NULL;
  }
  HEAP_OBJ_REP *Next(void) {
    HOR_NODE *n = (HOR_NODE *)SLIST_ITER::Next();
    return n ? n->Hor() : NULL;
  }

  HEAP_OBJ_REP *First_elem(void) { return First(); }
  HEAP_OBJ_REP *Next_elem(void)  { return Next();  }
};

typedef vector<HEAP_OBJ_REP*, mempool_allocator<HEAP_OBJ_REP*> > HOR_ARRAY;
extern void Id_map_fprint(FILE *fp, HOR_ARRAY* obj);

// =============================================================================
//
// VSYM_OBJ is specialized version of RSC_OBJ, for vsym management.  HOR is the
//          primary key and the ifld_id is the secondary key.
//          fld_id enables the field sensitivity which is critical to Java, which
//          gurantees no alias across field in a class.
//
// VSYM_OBJ_REP represents a sepcific version of VSYM_OBJ, it is in SSA form to
//              keep track of ISTORE/ILOD
//
// VO_LIST defines the list of vsym_obj. This list is maintained in Class VSA.
// VO_LIST_ITER simplifies the traversal of the VO_LIST
//
// =============================================================================

//  VO_LIST is a internal linked list
//
class VO_LIST : public SLIST {
  DECLARE_SLIST_CLASS (VO_LIST, VSYM_OBJ)
private:
  VO_LIST(const VO_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  VO_LIST& operator = (const VO_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~VO_LIST(void)                       {}
  VSYM_OBJ *Find(HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr) ;
  INT       Count(void);
  void      Print(const VSA *vsa, FILE *fp=stderr) ;
};

class VO_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (VO_LIST_ITER, VSYM_OBJ, VO_LIST)
  VO_LIST_ITER(const VO_LIST_ITER&);   // REQUIRED UNDEFINED UNWANTED methods
  VO_LIST_ITER& operator = (const VO_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~VO_LIST_ITER(void)                       {}
  VSYM_OBJ  *First_elem(void)         { return First(); }
  VSYM_OBJ  *Next_elem(void)          { return Next();  }
};

class VSYM_OBJ_REP : public SLIST_NODE {

private:
  VSYM_OBJ          *_vsym_obj;        // which vsym_obj it belongs to
  union {
    PHI_NODE        *_phi_def;         // the phi node that define this
    STMTREP         *_stmt_def;        // the stmtrep that defines this
  };
  HEAP_OBJ_REP      *_hor;             // the HOR for rhs of *p=malloc()
  mUINT32            _use_chain;       // use chain for this vor
  mUINT16            _version;         // version #, for tracing purpose
  mUINT16            _attr;
  SRCPOS_NODE        _srcpos_node;     // where this version is created
  TOR_LIST          *_tor_list;        // binded tag objects
  V_ANNOT            _vsa_annot;       // vsa annotation

  VSYM_OBJ_REP(void);                  // REQUIRED UNDEFINED UNWANTED methods
  VSYM_OBJ_REP(const VSYM_OBJ_REP&);   // REQUIRED UNDEFINED UNWANTED methods
  VSYM_OBJ_REP& operator = (const VSYM_OBJ_REP&); // REQUIRED UNDEFINED UNWANTED methods

public:

  VSYM_OBJ_REP( VSYM_OBJ *vo )
    : _vsym_obj(vo), _srcpos_node(), _use_chain(0), _tor_list(NULL), _vsa_annot(0)
  {
    VSA_STATS_inc(vor);
    _phi_def = NULL;
    _hor = NULL;
    _version = ROR_VERSION_NOT_SET;
    _attr = ROR_DEF_BY_NONE;
    if (vo->Entry_chi())
      vo->Add_rept(this);
  }

  VSYM_OBJ_REP *Next()                    { return (VSYM_OBJ_REP *)SLIST_NODE::Next(); }
  void          Set_Next(VSYM_OBJ_REP *v) { SLIST_NODE::Set_Next(v); }

  VSYM_OBJ *Vsym_obj(void) const          { return _vsym_obj; }
  VSYM_OBJ *Rsc_obj(void) const           { return _vsym_obj; }
  IDTYPE    Vsym_obj_id(void) const       { return _vsym_obj ? _vsym_obj->Id() : -1; }
  void      Set_vsym_obj(VSYM_OBJ *ho)    { _vsym_obj = ho; }
  HEAP_OBJ_REP *Hor(void) const           { return _hor; }
  void      Set_hor(HEAP_OBJ_REP *hor)    { _hor = hor;
                                            if (hor && hor->Tor_list())
                                              Set_tor_list(hor->Tor_list());
                                            mUINT16 base_flag = Vsym_obj()->Base_hor()->Heap_obj()->Flag();
                                            mUINT16 addr_flag = base_flag & (RSC_ADDR_PASSED_IN|RSC_ADDR_PASSED_OUT);
                                            _hor->Heap_obj()->Set_flag((RSC_FLAG)addr_flag);
                                          }
  IDTYPE    Version(void) const           { return _version; }
  void      Set_version(IDTYPE v)         { _version = v; }
  void      Set_version_entry_chi(void)   { _version = ROR_VERSION_ENTRY_CHI; }
  BOOL      Version_entry_chi(void) const { return _version == ROR_VERSION_ENTRY_CHI; }
  BOOL      Version_not_set(void) const   { return _version == ROR_VERSION_NOT_SET; }
  SRCPOS_NODE  Srcpos_node(void) const    { return _srcpos_node; }
  SRCPOS_NODE *Srcpos_nodep(void)         { return &_srcpos_node; }
  void      Set_srcpos_node(STMTREP* stmt, DNA_NODE* dna, PATH_INFO info)
  {
    _srcpos_node.Set_info(info);
    _srcpos_node.Set_stmt(stmt);
    _srcpos_node.Set_dna(dna);
  }
  void      Set_srcpos_node(BB_NODE* bb, DNA_NODE* dna, PATH_INFO info)
  {
    _srcpos_node.Set_info(info);
    _srcpos_node.Set_bb(bb);
    _srcpos_node.Set_dna(dna);
  }
  PHI_NODE *Phi_def(void) const           { Is_True(Attr() == ROR_DEF_BY_PHI ||
                                                    Attr() == ROR_DEF_BY_HORPHI,
                                                    ("not def by phi"));
                                            return _phi_def; }
  void      Set_phi_def(PHI_NODE *phi)    { Is_True(Attr() == ROR_DEF_BY_PHI ||
                                                    Attr() == ROR_DEF_BY_HORPHI,
                                                    ("not def by phi"));
                                            _phi_def = phi; }
  BOOL      Is_phi() const                { return Attr() == ROR_DEF_BY_PHI ||
                                                   Attr() == ROR_DEF_BY_HORPHI;
                                          }
  STMTREP  *Stmt_def(void) const          { Is_True(Attr() == ROR_DEF_BY_ISTORE ||
                                                    Attr() == ROR_DEF_BY_COPY ||
                                                    Attr() == ROR_DEF_BY_CHI,
                                                    ("not def by stmt"));
                                            return _stmt_def; }
  STMTREP  *Defstmt(void) const           { return Stmt_def(); }
  STMTREP  *Find_1st_istore(hash_set<IDTYPE> &visited_bb) const;

  void      Set_stmt_def(STMTREP *stmt,
                         DNA_NODE *dna)   { Is_True(Attr() == ROR_DEF_BY_ISTORE ||
                                                    Attr() == ROR_DEF_BY_COPY ||
                                                    Attr() == ROR_DEF_BY_CHI,
                                                    ("not def by stmt"));
                                            Is_True(_stmt_def == NULL || _stmt_def == stmt,
                                                    ("def stmt already set"));
                                            _stmt_def = stmt;
                                            if(Attr() == ROR_DEF_BY_CHI)
                                              Set_srcpos_node(stmt,
                                                              dna,
                                                              PATHINFO_CHI);
                                            else if(Attr() == ROR_DEF_BY_COPY)
                                              Set_srcpos_node(stmt,
                                                              dna,
                                                              PATHINFO_COPY);
                                            else
                                              Set_srcpos_node(stmt,
                                                              dna,
                                                              PATHINFO_ISTORE);
                                          }
  BOOL      Is_entry_chi(void)            { return _vsym_obj->Is_entry_chi(this); }

  IDTYPE    Gen_name(STMTREP*);
  void      Set_attr(ROR_ATTR k)          { _attr = k; }
  ROR_ATTR  Attr(void) const              { return ROR_ATTR(_attr & ROR_DEF_ATTRS); }
  void      Set_vsym_attr(ROR_ATTR k)     { _attr = ROR_ATTR(_attr | (k & ROR_VSYM_ATTRS)); }
  ROR_ATTR  Vsym_attr(void) const         { return ROR_ATTR(_attr & ROR_VSYM_ATTRS); }
  template <typename T>
  void      Set_tor_list(T *tor_list)     { _tor_list = (TOR_LIST*) tor_list; }
  template <typename T> 
  T        *Tor_list() const              { return (T*)_tor_list; }
  V_ANNOT   Vsa_annot() const             { return _vsa_annot; }
  void      Set_vsa_annot(V_ANNOT v)      { _vsa_annot = v; }
  UINT32    Use_chain() const             { return _use_chain; }
  void      Set_use_chain(V_ANNOT v)      { _use_chain = v; }
  void      Print(FILE *fp) const         { Print_detail(fp); }
  void      Print() const;
  void      Print_detail(FILE *fp) const;
}; // end of VSYM_OBJ_REP

#include <utility>
typedef std::pair<HEAP_OBJ_REP *, CODEREP *> CHOR;
typedef std::pair<VSYM_OBJ_REP *, CODEREP *> CVOR;

// =============================================================================
//
// TRANSIT: defines a transition entry for a Finite Statement Machine
//          all elements in the transition entry are character string to make
//          the system extensible without VSA changes.  All these strings are
//          local copy for the time being.
//
// =============================================================================
class TRANSIT {
private:
  IDTYPE      _state;
  STRING      _action;
  CODEREP    *_key;
  CODEREP    *_cond;
  IDTYPE      _next_state;
  STRING_VEC *_errcode;
  INT32       _msg_id;
  BOOL        _default;

  TRANSIT(void);                       // REQUIRED UNDEFINED UNWANTED methods
  TRANSIT(const TRANSIT&);             // REQUIRED UNDEFINED UNWANTED methods
  TRANSIT& operator = (const TRANSIT&);// REQUIRED UNDEFINED UNWANTED methods

public:
  TRANSIT(IDTYPE state, STRING action, CODEREP *key, CODEREP *cond,
          IDTYPE nextstate, STRING_VEC *errcode, INT32 msg_id, BOOL deft) :
    _state(state), _action(action), _key(key), _cond(cond),
    _next_state(nextstate), _errcode(errcode), _msg_id(msg_id), _default(deft)
  {
  };

  IDTYPE      State(void) const           { return _state;  }
  STRING      Action(void) const          { return _action; }
  CODEREP    *Key(void) const             { return _key;    }
  CODEREP    *Cond(void) const            { return _cond;   }
  IDTYPE      Nstate(void) const          { return _next_state; }
  STRING_VEC *Errcode(void) const         { return _errcode;}
  INT32       Msg_id(void) const          { return _msg_id; }
  BOOL        Is_default(void) const      { return _default;}

}; // class TRANSIT


// =============================================================================
//
// FSM: Finite State Machine, it contains a vector of state transition entries.
//      For convenience, custom Finite State Machine by external developer makes
//      use of character string to define the state transition.
//      We create a local copy of all strings and therefore the caller is
//      responsible for freeing up the memory for the string.
//      It maintains two states, start_state and final_state internally.
//
// =============================================================================
enum FSM_STATE {
  FSM_STATE_INVALID = -1,
  FSM_STATE_UNKNOWN =  0,
};

enum FSM_ATTR {
  FSM_ATTR_NONE        = 0x00000000, // nothing
  FSM_ATTR_RET_TRANSIT = 0x00000001, // fsm contains a transit on return statement
  FSM_ATTR_IF_TRANSIT  = 0x00000002, // fsm contains a transit on if statement
};

class FSM {
  typedef mempool_allocator<TRANSIT*> TS_ALLOCATOR;
  typedef vector<TRANSIT*, TS_ALLOCATOR> TRANSIT_VEC;

private:
  TRANSIT_VEC *_ts_vec;          // actual content of SCC data
  STRING_VEC  *_state_vec;       // state id index into this vector
  BOOL         _tracing;         // depends on VSA_DUMP_FLAG
  IDTYPE       _start_state;     // the start state of this FSM
  IDTYPE       _final_state;     // the final state of this FSM
  UINT32       _flags;           // attributes of this fsm
  DNA_NODE    *_fsm_dna;         // fsm dna node
  MEM_POOL    *_mem_pool;

  FSM(void);                     // REQUIRED UNDEFINED UNWANTED methods
  FSM(const FSM&);               // REQUIRED UNDEFINED UNWANTED methods
  FSM& operator = (const FSM&);  // REQUIRED UNDEFINED UNWANTED methods

  IDTYPE Find_state(STRING state);

public:
  FSM(MEM_POOL *mem_pool):_mem_pool(mem_pool) {
    _ts_vec = CXX_NEW(TRANSIT_VEC(TRANSIT_VEC::allocator_type(mem_pool)), mem_pool);
    _state_vec = CXX_NEW(STRING_VEC(STRING_VEC::allocator_type(mem_pool)), mem_pool);
    _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG);

    // The _start_state serves as the state of the compilation from ASCII to binary
    Get_state((char*)"unknown");     // take up the (*_state_vec)[0] for FSM_STATE_UNKNOWN
    _start_state = _final_state = (IDTYPE) FSM_STATE_INVALID;
    _fsm_dna = NULL;
    _flags = FSM_ATTR_NONE;
  }

  ~FSM(void) { }

  BOOL         Is_set(UINT32 f) const  { return _flags & f; }
  UINT32       Flags(void) const       { return _flags; }
  UINT32       Set_flag(UINT32 f)      { _flags |= f; return _flags; }
  void         Clear_flag(UINT32 f)    { _flags &= ~f; }

  TRANSIT_VEC *Ts_vec(void)            { return _ts_vec; }
  STRING_VEC  *State_vec(void)         { return _state_vec; }
  MEM_POOL    *Mem_pool(void)          { return _mem_pool; }
  DNA_NODE    *Dna(void)               { return _fsm_dna; }
  BOOL         Tracing(void) const     { return _tracing; }
  void         Print(FILE *);
  IDTYPE       Get_state(STRING state);
  IDTYPE       Add_state(STRING state) { return Get_state(state);  }
  void         Set_start(STRING n)     { _start_state = Get_state(n); }
  void         Set_final(STRING n)     { _final_state = Get_state(n); }
  IDTYPE       Start_state(void) const { Is_True(_start_state != FSM_STATE_INVALID,
                                                 ("FSM::Start_state not initialized"));
                                         return _start_state; }
  IDTYPE       Final_state(void) const { Is_True(_final_state != FSM_STATE_INVALID,
                                                 ("FSM::Final_state not initialized"));
                                         return _final_state; }
  void         Set_dna(DNA_NODE *dna)  { _fsm_dna = dna; }
  UINT32       State_num(void)         { return _state_vec->size() - 1; }

  // We build FSM by the following two functions
  void         Add_transition(IDTYPE state, STRING action, CODEREP *key, CODEREP *cond,
                              IDTYPE nstate, STRING_VEC *errcode, INT32 msg_id);
  void         Set_default_action(STRING state, STRING_VEC *errcode, INT32 msg_id);
  TRANSIT     *Get_default_action(IDTYPE state, INT *idx);

  // The FSM is not valid if any of the state transition is not defined
  BOOL         Verify(void) const;
};


// =============================================================================
//
// FSM_BASE is specialized version of RSC_BS, for fsm base management at the
//          whole program level
//
// FB_LIST defines the list of fsm_base. This list is maintained in Class VSA.
// FB_LIST_ITER simplifies the traversal of the FB_LIST
//
// =============================================================================
typedef RSC_BS<STRING, FSM*> FSM_BASE;

// FSM_BASE::Print
template<> inline void
RSC_BS<STRING, FSM*>::Print(FILE *fp) const {
  fprintf(fp, "Fsm object #%d, FSM name:%s", Id(), (char*)(INTPTR)Fsm_name());
}

//  FB_LIST is a internal linked list
//
class FB_LIST : public SLIST {
  DECLARE_SLIST_CLASS (FB_LIST, FSM_BASE)
private:
  FB_LIST(const FB_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  FB_LIST& operator = (const FB_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~FB_LIST(void)                       {}
  FSM_BASE  *Find(STRING fsmname) ;
  INT        Count(void);
  void       Print(FILE *fp=stderr) ;
};

class FB_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (FB_LIST_ITER, FSM_BASE, FB_LIST)
  FB_LIST_ITER(const FB_LIST_ITER&);             // REQUIRED UNDEFINED UNWANTED methods
  FB_LIST_ITER& operator = (const FB_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~FB_LIST_ITER(void)                  {}
  FSM_BASE   *First_elem(void)         { return First(); }
  FSM_BASE   *Next_elem(void)          { return Next();  }
};
   

// =============================================================================
//
// FSM_OBJ is specialized version of RSC_OBJ, for fsm object management. 
//
// FSM_OBJ_REP represents a sepcific version of FSM_OBJ, it keeps track of the
//             state of the FSM
//
// FO_LIST defines the list of fsm_obj. This list is maintained in Class VSA.
// FO_LIST_ITER simplifies the traversal of the FO_LIST
//
// =============================================================================
typedef RSC_OBJ<STRING, FSM*, FSM_OBJ_REP*> FSM_OBJ;

// FSM_OBJ::Print
template<> inline void
RSC_OBJ<STRING, FSM*, FSM_OBJ_REP*>::Print(FILE *fp) const {
  fprintf(fp, "FSM Object #%d, name: %s\n", Id(), Fsm_name());
}

//  FO_LIST is a internal linked list
//
class FO_LIST : public SLIST {
  DECLARE_SLIST_CLASS (FO_LIST, FSM_OBJ)
private:
  FO_LIST(const FO_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  FO_LIST& operator = (const FO_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~FO_LIST(void)                       {}
  FSM_OBJ    *Find(STRING fsmname) ;
  INT         Count(void);
  void        Print(FILE *fp=stderr) ;
};

class FO_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (FO_LIST_ITER, FSM_OBJ, FO_LIST)
  FO_LIST_ITER(const FO_LIST_ITER&);             // REQUIRED UNDEFINED UNWANTED methods
  FO_LIST_ITER& operator = (const FO_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~FO_LIST_ITER(void)                     {}
  FSM_OBJ    *First_elem(void)            { return First(); }
  FSM_OBJ    *Next_elem(void)             { return Next();  }
};
   
/*
The following are for FSM_OBJ instances:
start final; DELETE THE FSM INSTANCE
*/

class FSM_OBJ_REP : public SLIST_NODE {
private:

  FSM_OBJ    *_fsm_obj;                   // which fsm_obj it belongs to
  union {
    PHI_NODE *_phi_def;                   // the phi node that define this
    STMTREP  *_stmt_def;                  // the stmtrep that defines this
  };
  IDTYPE      _version;                   // version #, for tracing purpose
  IDTYPE      _state;                     // state #, for FSM evaluation
  SRCPOS_NODE _srcpos_node;               // where this state is transitioned
  TRANSIT    *_transit;                   // FSM machine's transition
  CODEREP    *_key;                       // which symbol is bound to this REP
                                          // FSM_OBJ_REP is bound to statement actually,
                                          // key indicates which var in the statement is related
  ROR_ATTR    _attr : 31;                 // must check ROR_ATTR element count
  BOOL        _visited : 1;               // visited

  FSM_OBJ_REP(void);                      // REQUIRED UNDEFINED UNWANTED methods
  FSM_OBJ_REP(const FSM_OBJ_REP&);        // REQUIRED UNDEFINED UNWANTED methods
  FSM_OBJ_REP& operator = (const FSM_OBJ_REP&); // REQUIRED UNDEFINED UNWANTED methods

public:
  FSM_OBJ_REP( FSM_OBJ *fo, TRANSIT *tr, MEM_POOL *pool ):
    _fsm_obj(fo), _transit(tr), _phi_def(NULL), _key(NULL), _visited(FALSE)
  { _version = ROR_VERSION_NOT_SET; _state = FSM_STATE_INVALID; _attr = ROR_DEF_BY_NONE;
  }

  FSM_OBJ     *Fsm_obj(void) const        { return _fsm_obj; }
  FSM_OBJ     *Rsc_obj(void) const        { return _fsm_obj; }
  void         Set_fsm_obj(FSM_OBJ *fo)   { _fsm_obj = fo; }

  FSM         *Fsm(void) const            { return _fsm_obj->Fsm(); }
  void         Set_initial_state(void)    { _state = Fsm()->Start_state(); }

  CODEREP     *Key(void) const            { return _key; }
  void         Set_key(CODEREP *v)        { _key = v; }

  IDTYPE       Version(void) const        { return _version; }
  void         Set_version(IDTYPE v)      { _version = v; }
  void         Set_version_entry_chi(void){ _version = ROR_VERSION_ENTRY_CHI; }
  BOOL         Version_entry_chi(void)const{ return _version == ROR_VERSION_ENTRY_CHI; }
  BOOL         Version_not_set(void) const{ return _version == ROR_VERSION_NOT_SET; }

  IDTYPE       State(void) const          { return _state; }
  void         Set_state(IDTYPE s)        { _state = s; }
  void         Set_state_unknown(void)    { _state = Fsm()->Get_state((char*)"unknown"); }
  void         Reset(void)                { _state = Fsm()->Start_state(); }
  BOOL         State_entry_chi(void) const{ return _state == Fsm()->Start_state(); }
  BOOL         State_not_set(void) const  { return _state == FSM_STATE_INVALID; }

  SRCPOS_NODE  Srcpos_node(void) const    { return _srcpos_node; }
  SRCPOS_NODE *Srcpos_nodep(void)         { return &_srcpos_node; }
  void         Set_srcpos_node(STMTREP* stmt,
                               DNA_NODE* dna,
                               PATH_INFO info)
                                          { _srcpos_node.Set_info(info);
                                            _srcpos_node.Set_dna(dna);
                                            _srcpos_node.Set_stmt(stmt); }
  void         Set_srcpos_node(BB_NODE* bb,
                               DNA_NODE* dna,
                               PATH_INFO info)
                                          { _srcpos_node.Set_info(info);
                                            _srcpos_node.Set_dna(dna);
                                            _srcpos_node.Set_bb(bb); }

  PHI_NODE    *Phi_def(void) const        { Is_True(Attr() == ROR_DEF_BY_PHI,
                                                    ("not def by phi"));
                                            return _phi_def; }
  void         Set_phi_def(PHI_NODE *phi) { Is_True(Attr() == ROR_DEF_BY_PHI,
                                                    ("not def by phi"));
                                            _phi_def = phi; }
  STMTREP     *Stmt_def(void) const       { Is_True(Attr() == ROR_DEF_BY_CHI ||
                                                    Attr() == ROR_DEF_BY_ISTORE ||
                                                    Attr() == ROR_DEF_BY_COPY ||
                                                    Attr() == ROR_DEF_BY_TRANSIT,
                                                    ("not def by stmt"));
                                            return _stmt_def; }
  STMTREP     *Defstmt(void) const        { return Stmt_def(); }
  void         Set_stmt_def(STMTREP *stmt,
                            DNA_NODE *dna){ Is_True(Attr() == ROR_DEF_BY_CHI ||
                                                    Attr() == ROR_DEF_BY_ISTORE ||
                                                    Attr() == ROR_DEF_BY_COPY ||
                                                    Attr() == ROR_DEF_BY_TRANSIT,
                                                    ("not def by stmt"));
                                            Is_True(_stmt_def == NULL || _stmt_def == stmt,
                                                    ("def stmt already set"));
                                            _stmt_def = stmt;
                                            if(Attr() == ROR_DEF_BY_COPY)
                                              _srcpos_node.Set_info(PATHINFO_COPY);
                                            else if(Attr() == ROR_DEF_BY_CHI)
                                              _srcpos_node.Set_info(PATHINFO_CHI);
                                            else if(Attr() == ROR_DEF_BY_ISTORE)
                                              _srcpos_node.Set_info(PATHINFO_ISTORE);
                                            else if(Attr() == ROR_DEF_BY_TRANSIT)
                                              _srcpos_node.Set_info(PATHINFO_TRANSIT);
                                            _srcpos_node.Set_stmt(stmt);
                                            _srcpos_node.Set_dna(dna);
                                          }
  BOOL         Is_entry_chi(void)         { return _fsm_obj->Is_entry_chi(this); }

  IDTYPE       Gen_name(STMTREP*);

  TRANSIT     *Transit() const            { return _transit; }
  void         Set_transit(TRANSIT *tr)   { _transit = tr; }
  ROR_ATTR     Attr(void) const           { return _attr; }
  void         Set_attr(ROR_ATTR k)       { _attr = k; if (k == ROR_DEF_BY_TRANSIT) 
                                                         _fsm_obj->Set_kind(RSC_KIND_FSM); }
  BOOL         Visited(void) const        { return _visited; }
  void         Set_visited(void)          { _visited = TRUE; }
  void         Print(FILE *fp) const      { fprintf(fp, "fo%d(%s)t(%s:%d->%d)k(cr%d)", _fsm_obj->Id(),
                                                    Fsm_obj()->Fsm_name(),
                                                    _transit == NULL ? "null" : _transit->Action(),
                                                    _transit == NULL ? -1 : _transit->State(),
                                                    _transit == NULL ? -1 : _transit->Nstate(),
                                                    _key == NULL ? -1 : _key->Coderep_id()); }
};

typedef std::pair<STRING, FSM_OBJ*> STR_FSM;
typedef mempool_allocator<STR_FSM> STR_FSM_ALLOCATOR;
typedef vector<STR_FSM*, STR_FSM_ALLOCATOR> STR_FSM_VECTOR;

typedef vector<FSM_OBJ_REP*, mempool_allocator<FSM_OBJ_REP*> > FOR_ARRAY;
extern void Id_map_fprint(FILE *fp, FOR_ARRAY *obj);

// =============================================================================
//
// TAG_BASE is specialized version of RSC_IN, for TAG base management at the
//          whole program level
//
// TB_LIST defines the list of tag_base. This list is maintained in Class VSA.
// TB_LIST_ITER simplifies the traversal of the TB_LIST
//
// =============================================================================
typedef enum _tag_obj_kind {
  TAG_KIND_INVALID      = 0x0,
  TAG_KIND_VAR          = 0x1,
  TAG_KIND_HOR          = 0x2,
  TAG_KIND_VOR          = 0x3,
} TAGOKIND;
typedef RSC_IN<STRING> TAG_BASE;

// TAG_BASE::Print
template<> inline void
RSC_IN<STRING>::Print(FILE *fp) const {
  fprintf(fp, "Tag object #%d, TAG name:%s", Id(), (char*)(INTPTR)Tag_name());
}

//  TB_LIST is a internal linked list
//
class TB_LIST : public SLIST {
  DECLARE_SLIST_CLASS (TB_LIST, TAG_BASE)
private:
  TB_LIST(const TB_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  TB_LIST& operator = (const TB_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~TB_LIST(void)                       {}
  TAG_BASE  *Find(STRING tag_name) ;
  INT        Count(void);
  INT        Real_use_cnt(void);
  void       Print(FILE *fp=stderr) ;
};

class TB_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (TB_LIST_ITER, TAG_BASE, TB_LIST)
  TB_LIST_ITER(const TB_LIST_ITER&);             // REQUIRED UNDEFINED UNWANTED methods
  TB_LIST_ITER& operator = (const TB_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~TB_LIST_ITER(void)                  {}
  TAG_BASE   *First_elem(void)         { return First(); }
  TAG_BASE   *Next_elem(void)          { return Next();  }
};


// =============================================================================
//
// TAG_OBJ is the light version of Resource Modeling that carries the tag/attr
//         for.  TAG must be single state.  Therefore, it can attach to CODEREP,
//         VSYM_OBJ_REP or HEAP_OBJ_REP.
//
//   NOTE: If an attribute carries multiple state, it should be managed by FSM.
//
//         The lightweight version keeps ID only, comparing to RSC_OBJ
//         An user defined type is identified by is name and the ID is assigned
//         internally by RBC.  The type of ID is managed by RBC.
//         Preserving the interface enables logic sharing thru template. -Shin
//
//
// TO_LIST defines the list of tag_obj. This list is maintained in Class VSA.
//
// TO_LIST_ITER simplifies the traversal of the TO_LIST
//
// =============================================================================
typedef RSC_IN<STRING> TAG_BASE;

typedef enum _tor_def_attr {
  TO_DEF_BY_DEFAULT     = 0,
  TO_DEF_BY_ENTRY_CHI   = 1,
  TO_DEF_BY_CHI         = 2,
  TO_DEF_BY_SE          = 3,  // def by symbolic eval
  TO_DEF_BY_COPY        = 4,
  TO_DEF_BY_PHI         = 5,
  TO_DEF_BY_CREATE      = 6,
  TO_DEF_BY_TAG_ATTR    = 7,
  // below attr are for rbc merge, will not be saved on TOR
  TO_DEF_BY_OR          = 8,  // OR is different with PHI, OR result is calculated
                              // by operand priority
} TOR_DEF_ATTR;

#if 0
class TAG_OBJ : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS ( TAG_OBJ )

private:
  TAG_BASE        *_tag_base;           // TAG identifier and name
  TAGOKIND         _obj_kind:4;         // the kind of object it belongs to
  TOR_DEF_ATTR     _flags:12;           // the attribute of tag obj
  union {
    CODEREP       *_var;                // tag attached to a CK_VAR CODEREP
    HEAP_OBJ_REP  *_hor;                // tag attached to a HEAP_OBJ_REP
    VSYM_OBJ_REP  *_vor;                // tag attached to a VSYM_OBJ_REP
  };
  STMTREP         *_defstmt;            // the def stmt for tag obj
  TAG_OBJ         *_deftag;             // the def tag

  TAG_OBJ(void);                       // REQUIRED UNDEFINED UNWANTED methods
  TAG_OBJ(const TAG_OBJ&);             // REQUIRED UNDEFINED UNWANTED methods
  TAG_OBJ& operator = (const TAG_OBJ&);// REQUIRED UNDEFINED UNWANTED methods

public:

  TAG_OBJ(TAG_BASE *tag_base, CODEREP *var):
    _tag_base(tag_base), _var(var),  _defstmt(NULL), _deftag(NULL),
    _flags(TO_DEF_BY_DEFAULT) { _obj_kind = TAG_KIND_VAR; }
  TAG_OBJ(TAG_BASE *tag_base, HEAP_OBJ_REP *hor):
    _tag_base(tag_base), _hor(hor), _defstmt(NULL), _deftag(NULL),
    _flags(TO_DEF_BY_DEFAULT) { _obj_kind = TAG_KIND_HOR; }
  TAG_OBJ(TAG_BASE *tag_base, VSYM_OBJ_REP *vor):
    _tag_base(tag_base), _vor(vor),  _defstmt(NULL), _deftag(NULL),
    _flags(TO_DEF_BY_DEFAULT) { _obj_kind = TAG_KIND_VOR; }

  ~TAG_OBJ(void) { }

  IDTYPE        Id(void) const              { return _tag_base->Id(); }
  STRING        Tag_name(void) const        { return _tag_base->Tag_name(); }
  TAG_BASE     *Tag_base() const            { return _tag_base; }
  TAGOKIND      Tag_kind() const            { return _obj_kind; }
  CODEREP      *Coderep(void) const         { return _var; }
  HEAP_OBJ_REP *Heapobjrep(void)const       { return _hor; }
  void          Set_hor(HEAP_OBJ_REP *hor)  { Is_True_Ret(_obj_kind == TAG_KIND_HOR, ("Obj kind is not hor.")); _hor = hor; }
  VSYM_OBJ_REP *Vsymobjrep(void)const       { return _vor; }
  void          Reset_flag(TOR_DEF_ATTR f)  { _flags = (TOR_DEF_ATTR)(_flags & ~f); }
  void          Set_flag(TOR_DEF_ATTR f)    { _flags = (TOR_DEF_ATTR)(_flags | f); }
  BOOL          Is_flag_set(TOR_DEF_ATTR f) { return _flags & f; }
  TOR_DEF_ATTR  Flags(void) const           { return _flags; }
  void          Copy_flags(TOR_DEF_ATTR f)  { _flags = f; }
  void          Set_defstmt(STMTREP *sr)    { _defstmt = sr;}
  void          Set_deftag(TAG_OBJ *tag)    { _deftag = tag; _flags = TO_DEF_BY_COPY; }
  STMTREP      *Defstmt(void) const         { return _defstmt; }
  TAG_OBJ      *Deftag(void) const          { return _deftag; }

  void          Print(FILE *fp) const       { fprintf(fp, "to%d", Id()); }
}; // TAG_OBJ

//  TO_LIST is a internal linked list
//
class TO_LIST : public SLIST {
  DECLARE_SLIST_CLASS (TO_LIST, TAG_OBJ)
private:
  TO_LIST(const TO_LIST&);             // REQUIRED UNDEFINED UNWANTED methods
  TO_LIST& operator = (const TO_LIST&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~TO_LIST(void)                       {}
  TAG_OBJ    *Find(void *tagobjrp) ;
  INT         Count(void);
  void        Print(FILE *fp=stderr) ;
};

class TO_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (TO_LIST_ITER, TAG_OBJ, TO_LIST)
  TO_LIST_ITER(const TO_LIST_ITER&);             // REQUIRED UNDEFINED UNWANTED methods
  TO_LIST_ITER& operator = (const TO_LIST_ITER&);// REQUIRED UNDEFINED UNWANTED methods

public:
  ~TO_LIST_ITER(void)                  {}
  TAG_OBJ    *First_elem(void)         { return First(); }
  TAG_OBJ    *Next_elem(void)          { return Next();  }
};
#endif

#define TAG_ATTR_DEFAULT_NUM 128
#define TAG_INVALID_ID 0
#define TAG_START_ID 1

typedef std::vector<TAG_OBJ_REP*, mempool_allocator<TAG_OBJ_REP*> > TOR_VEC;
typedef std::bitset<TAG_ATTR_DEFAULT_NUM> TAG_ATTRS;
class TAG_OBJ_REP : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS (TAG_OBJ_REP)
private:
  TAG_BASE          *_tag_base;           // TAG identifier and name
  UINT32             _id;                 // the id of current TAG_OBJ_REP
  TAGOKIND           _obj_kind:2;         // the kind of object it belongs to
  TOR_DEF_ATTR       _def_attr:4;         // the define attribute of tag obj
  UINT32             _def_attr_id:26;     // the def attr id only for TO_DEF_BY_TAG_ATTR
  union {
    CODEREP         *_cr;                 // tag attached to a CK_VAR CODEREP
    HEAP_OBJ_REP    *_hor;                // tag attached to a HEAP_OBJ_REP
    VSYM_OBJ_REP    *_vor;                // tag attached to a VSYM_OBJ_REP
  };
  CODEREP           *_hor_cr;             // hor attached to a cr
  STMTREP           *_defstmt;            // the def stmt for tag obj
  // union {
    TAG_OBJ_REP     *_deftor;             // the def tag obj rep
    TOR_VEC         *_phi_list;           // the phi node list of tab_obj_rep
  // };
  TAG_ATTRS          _tag_attrs;          // the user defined attributes for tor

  TAG_OBJ_REP(const TAG_OBJ_REP&);
  TAG_OBJ_REP& operator=(const TAG_OBJ_REP&);

public:
  TAG_OBJ_REP(TAG_BASE *tag_base, UINT32 id, CODEREP *cr,  STMTREP *defstmt):
    _tag_base(tag_base), _id(id), _cr(cr), _obj_kind(TAG_KIND_VAR), _hor_cr(NULL),
    _defstmt(defstmt), _deftor(NULL), _phi_list(NULL), _def_attr(TO_DEF_BY_DEFAULT),
    _def_attr_id(TAG_INVALID_ID) {}
  TAG_OBJ_REP(TAG_BASE *tag_base, UINT32 id, HEAP_OBJ_REP *hor, STMTREP *defstmt):
    _tag_base(tag_base),  _id(id), _hor(hor), _obj_kind(TAG_KIND_HOR), _hor_cr(NULL),
    _defstmt(defstmt), _deftor(NULL), _phi_list(NULL), _def_attr(TO_DEF_BY_DEFAULT),
    _def_attr_id(TAG_INVALID_ID) {}
  TAG_OBJ_REP(TAG_BASE *tag_base, UINT32 id, VSYM_OBJ_REP *vor, STMTREP *defstmt):
    _tag_base(tag_base), _id(id), _vor(vor), _obj_kind(TAG_KIND_VOR), _hor_cr(NULL),
    _defstmt(defstmt), _deftor(NULL), _phi_list(NULL), _def_attr(TO_DEF_BY_DEFAULT),
    _def_attr_id(TAG_INVALID_ID) {}

  TAG_BASE      *Tag_base() const             { return _tag_base; }
  UINT32         Id() const                   { return _id;       }
  TAGOKIND       Obj_kind() const             { return _obj_kind; }
  CODEREP       *Coderep(void) const          { return _cr; }
  HEAP_OBJ_REP  *Heapobjrep(void) const       { return _hor; }
  VSYM_OBJ_REP  *Vsymobjrep(void) const       { return _vor; }
  void           Set_hor_cr(CODEREP *cr)      { _hor_cr = cr; }
  CODEREP*       Hor_cr() const               { return _hor_cr; }
  TOR_DEF_ATTR   Def_attr() const             { return _def_attr; }
  void           Set_def_attr(TOR_DEF_ATTR attr)  { _def_attr = attr; }
  STMTREP       *Defstmt() const              { return _defstmt; }
  void           Set_defstmt(STMTREP *stmt)   { _defstmt = stmt; }
  void           Set_deftor(TAG_OBJ_REP *tor) { _deftor = tor; }
  TAG_OBJ_REP   *Deftor() const               { Is_True_Ret(_def_attr == TO_DEF_BY_COPY ||
                                                  _def_attr == TO_DEF_BY_TAG_ATTR,
                                                  ("Is not defined by copy or tag_attr, def attr : %d", _def_attr), NULL);
                                                return _deftor;
                                              }
  BOOL           Has_deftor() const           { return _def_attr == TO_DEF_BY_COPY ||
                                                       _def_attr == TO_DEF_BY_TAG_ATTR;
                                              }
  void           Set_tag_attr(IDTYPE idx)     { Is_True_Ret(idx < TAG_ATTR_DEFAULT_NUM,
                                                            ("Index exceed the default length."));
                                                _tag_attrs.set(idx);
                                                _def_attr_id = idx; }
  BOOL           Is_set_tag_attr(IDTYPE idx)  { Is_True_Ret(idx < TAG_ATTR_DEFAULT_NUM,
                                                            ("Index exceed the default length."),FALSE);
                                                return _tag_attrs[idx]; }

  TAG_ATTRS&     Get_tag_attrs()              { return _tag_attrs; }
  void           Set_tag_attrs()              { _tag_attrs.set(); }
  void           Clone_tag_attrs(TAG_ATTRS &tag_attrs)
                                              {
                                                if (tag_attrs.any()) {
                                                  for (int i = 0; i < tag_attrs.size(); i++) {
                                                    if (tag_attrs[i])
                                                      _tag_attrs.set(i);
                                                  }
                                                }
                                              }

  UINT32         Def_attr_id()                { Is_True_Ret(Def_attr() == TO_DEF_BY_TAG_ATTR,
                                                            ("Invalid tag attr"), TAG_INVALID_ID);
                                                return _def_attr_id;
                                              }
  BOOL           Tag_on()                     { return !_tag_attrs.all(); } // if one bit not set, return true

  TOR_VEC       *Phi_list() const             { Is_True_Ret(_def_attr == TO_DEF_BY_PHI, ("not def by phi"), NULL);
                                                return _phi_list;
                                              }
  PHI_NODE      *Def_phi() const              { if (Defstmt() != NULL) return NULL;
                                                Is_True_Ret(_def_attr == TO_DEF_BY_PHI, ("not def by phi"), NULL);
                                                switch(Obj_kind()) {
                                                  case TAG_KIND_VAR:
                                                    if (Coderep()->Is_flag_set(CF_DEF_BY_PHI)) {
                                                      return Coderep()->Defphi();
                                                    }
                                                  break;
                                                  case TAG_KIND_HOR:
                                                    if (Heapobjrep()->Is_phi()) {
                                                      return Heapobjrep()->Phi_def();
                                                    }
                                                  break;
                                                  case TAG_KIND_VOR:
                                                    if (Vsymobjrep()->Is_phi()) {
                                                      return Vsymobjrep()->Phi_def();
                                                    }
                                                  break;
                                                  default:
                                                    Is_True_Ret(FALSE, ("invalid tor obj_kind"), NULL);
                                                }
                                                return NULL;
                                              }
  void           Set_const_side_effect(RBC_ENGINE::TAG_DEF_VAL tag_def_val);
  BOOL           Push_phi(TAG_OBJ_REP *tor, MEM_POOL *pool);
  const char*    Def_attr_str() const;
  void           Print(FILE *fp=stderr, BOOL detail = TRUE) const;
};

class TOR_LIST_OLD : public SLIST {
  DECLARE_SLIST_CLASS (TOR_LIST_OLD, TAG_OBJ_REP)
private:
  TOR_LIST_OLD(const TOR_LIST_OLD&);
  TOR_LIST_OLD& operator=(const TOR_LIST_OLD&);

public:
  ~TOR_LIST_OLD(void)                   {}
  INT                Count(void);
  TAG_OBJ_REP       *Find(TAG_OBJ_REP *tor);
  TAG_OBJ_REP       *Find(IDTYPE tag_id);
  void               Print(FILE *fp=stderr, BOOL detail = TRUE);
};


class TOR_LIST_OLD_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (TOR_LIST_OLD_ITER, TAG_OBJ_REP, TOR_LIST_OLD)
  TOR_LIST_OLD_ITER(const TOR_LIST_OLD_ITER&);
  TOR_LIST_OLD_ITER& operator=(const TOR_LIST_OLD_ITER&);

public:
  ~TOR_LIST_OLD_ITER(void)             {}
  TAG_OBJ_REP       *First_elem(void)  { return First(); }
  TAG_OBJ_REP       *Next_elem(void)   { return Next(); }
};

class TOR_LIST {
private:
  TOR_VEC _tor_vec;
public:
  TOR_LIST(MEM_POOL *pool) : _tor_vec(pool) {}
  BOOL         Empty()                      { return _tor_vec.empty(); }
  INT32        Size()                       { return _tor_vec.size();  }
  void         Push_back(TAG_OBJ_REP *tor)
  {
    Is_True_Ret(!Find(tor), ("tor with same tag id already added"));
    _tor_vec.push_back(tor);
  }

  TAG_OBJ_REP *operator[] (INT32 idx)
  {
    Is_True_Ret(idx >=0 && idx < Size(), ("idx outof bound"), NULL);
    return _tor_vec[idx];
  }
  INT32 Find_pos(TAG_OBJ_REP *tor)
  {
    for (INT32 idx = 0; idx < Size(); idx++) {
      TAG_OBJ_REP *cur_tor = _tor_vec[idx];
      if (cur_tor == tor) {
        return idx;
      } else if (tor->Tag_base()->Id() == cur_tor->Tag_base()->Id()) {
        return idx;
      }
    }
    return -1;
  }
  TAG_OBJ_REP *Find(TAG_OBJ_REP *tor)
  {
    for (INT32 idx = 0; idx < Size(); idx++) {
      TAG_OBJ_REP *cur_tor = _tor_vec[idx];
      if (cur_tor == tor) {
        return cur_tor;
      } else if (tor->Tag_base()->Id() == cur_tor->Tag_base()->Id()) {
        return cur_tor;
      }
    }
    return NULL;
  }
  TAG_OBJ_REP *Find(IDTYPE id)
  {
    for (INT32 idx = 0; idx < Size(); idx++) {
      TAG_OBJ_REP *cur_tor = _tor_vec[idx];
      if (id == cur_tor->Tag_base()->Id()) {
        return cur_tor;
      }
    }
    return NULL;
  }
  void Remove(INT32 idx)
  {
    Is_True_Ret(idx >=0 && idx < Size(), ("idx outof bound"));
    TOR_VEC::iterator iter = _tor_vec.begin() + idx;
    _tor_vec.erase(iter);
  }

  void Print(FILE *fp, BOOL detail = TRUE)
  {
    INT32 idx = 0;
    for (; idx < Size(); idx++) {
      if (idx == 0) {
        fprintf(fp, "[");
      } else {
        fprintf(fp, "|");
      }
      TAG_OBJ_REP *cur_tor = _tor_vec[idx];
      cur_tor->Print(fp, detail);
    }
    if (idx != 0)
      fprintf(fp, "]");
  }
};

#endif  // opt_vsa_rsc_INCLUD
