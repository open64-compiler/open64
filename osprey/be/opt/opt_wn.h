/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_wn.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_wn.h,v $
//
// Revision history:
//  15-SEP-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:  WN interface for Optimizer
//
// ====================================================================
// ====================================================================


#ifndef opt_wn_INCLUDED
#define opt_wn_INCLUDED	"opt_wn.h"
#ifdef _KEEP_RCS_ID
static char *opt_wnrcs_id = 	opt_wn_INCLUDED"$Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#ifndef optimizer_INCLUDED
#include "optimizer.h"
#endif
#include "defs.h"
#include "opt_defs.h"
#include "wn.h"
#include "opt_alias_interface.h"   // for READ_WRITE
#include "region_util.h" // for REGION_LEVEL
#include "opt_base.h"
#include <map>

// Forward declaration
class ALIAS_MANAGER;
class AUX_STAB_ENTRY;
class CHI_LIST;
class CODEREP;
class MU_LIST;
class OPT_STAB;
class POINTS_TO;
class VAR_LIST;

typedef struct pf_pointer PF_POINTER;
typedef struct region_id RID;

// STMT_CONTAINER is used to create or modify the WN list
class STMT_CONTAINER {
private:
  WN      *head;      // the first stmt
  WN      *tail;      // the last stmt

  STMT_CONTAINER(const STMT_CONTAINER&);
  STMT_CONTAINER& operator = (const STMT_CONTAINER&);

public:
  STMT_CONTAINER(void)         { head=NULL; tail=NULL; }
  STMT_CONTAINER(WN *f);
  STMT_CONTAINER(WN *f, WN *l) { head=f; tail=l; }

  ~STMT_CONTAINER(void) {}

  WN  *Head(void)     { return head; }
  WN  *Tail(void)     { return tail; }

  void Insert_before(WN *me, WN *wn); // set head to wn if me is head
  void Insert_after (WN *me, WN *wn); // set tail to wn if me is tail
  void Insert_lst_before(WN *,
			 WN *, WN *); // set head to wn_f if me is head
  void Insert_lst_after (WN *,
			 WN *, WN *); // set tail to wn_l if me is tail
  void Prepend(WN *wn)                // prepend to the head
    { if (head) Insert_before(head, wn);
      else    head = tail = wn;
    }
  void Append(WN *wn)                 // append to the tail
    { if (tail) Insert_after(tail, wn);
      else    head = tail = wn;
    }
  void Append_list(WN *f, WN *l)      // append the list f-l to tail
    { if (tail) { Insert_lst_after(tail, f, l); }
      else {	head = f; tail = l; }    
    }
  void Remove(WN *me);                // reset head/tail if me's head/tail
                                      // set cur to NULL if me is cur

  void Set_head(WN *wn)               { head = wn; }
  void Set_tail(WN *wn)               { tail = wn; }

  void Print(FILE *fp=stderr);
};


//   STMT_ITER is used to iterate through the statement list of a BB.
//    In the WN statement list, the linked-list data structure is also
//    WN data structure, therefore First & First_elem are equivalent.
//
// Because the optimizer operate on the IR read in by IR_reader and build
// data structure around it, the original WN chain is retained.  The head
// and tail for any particular BB will chain up with the rest of the code.
// To hide this fact and provide a better abstraction, we create a contain.


class STMT_ITER {
private:
  WN      *head;      // the first stmt
  WN      *tail;      // the last stmt
  WN      *cur;       // the current stmt in use

  STMT_ITER(const STMT_ITER&);
  STMT_ITER& operator = (const STMT_ITER&);

public:
  STMT_ITER(void)         { head=NULL; tail=NULL; cur=NULL; }
  STMT_ITER(WN *f);
  STMT_ITER(WN *f, WN *l) { head=f; tail=l; cur=f; }

  ~STMT_ITER(void) {}
  
  void  Init(WN *h, WN *t) { head=h; tail=t; cur=h; }
  WN   *First(void)        { return (cur = head); }
  WN   *Last(void)         { return (cur = tail); }
  WN   *Next(void)         { return (cur=(cur==NULL||cur==tail)?NULL:WN_next(cur)); }
  WN   *Prev(void)         { return (cur=(cur==NULL||cur==head)?NULL:WN_prev(cur)); }
  WN   *Peek_next(void)    { return (cur==NULL||cur==tail)?NULL:WN_next(cur); } 
  WN   *Peek_prev(void)    { return (cur==NULL||cur==head)?NULL:WN_prev(cur); } 
  WN   *Cur(void)          { return cur; }
  WN   *First_elem(void)   { return (cur = head); }
  WN   *Last_elem(void)    { return (cur = tail); }
  WN   *Next_elem(void)    { return (cur=(cur==NULL||cur==tail)?NULL:WN_next(cur)); }
  WN   *Prev_elem(void)    { return (cur=(cur==NULL||cur==head)?NULL:WN_prev(cur)); }
  BOOL  Is_Empty(void)     { return cur == NULL; }
  BOOL  Is_Empty_Reverse(void)  { return cur == NULL; }
  BOOL No_stmt(void)  { return head == NULL; }
  BOOL Validate(void) { return head != NULL && tail != NULL; }
  void Print(FILE *fp=stderr);
};


// flags for the debugging _wn_flag_map
enum WN_FLAG_TYPE {
  WN_ST_IS_SYM = 0,
  WN_ST_IS_AUX = 1,
  WN_ST_IS_VER = 2,
  WN_FLAG_ST_TYPE = 0x3,   // ST_IS_SYM | ST_IS_AUX | ST_IS_VER
  WN_FLAG_DO_LOOP = 0x8,
  WN_FLAG_PROMOTED_DO_LOOP = 0x16   // do-loop promoted from while-do
};

// these routines create/delete the wn flags map 
extern void  WN_init_flags(MEM_POOL *pool);   
extern void  WN_fini_flags(void);             

// Set and read the wn flgs
extern void         Set_wn_flags(WN *wn, INT32 flag);
extern INT32        Wn_flags(const WN *wn);


// WN associates with a CHI list 
// (WN is either STID, STORE, CALL, ICALL, IO)
//
extern BOOL OPCODE_has_chi( OPCODE );
extern BOOL OPERATOR_has_chi( OPERATOR );
extern BOOL WN_has_chi(const WN *, REGION_LEVEL );

// WN associates with a MU list
// (WN is either a LDID, LOAD, CALL, ICALL, IO).
//
extern BOOL OPCODE_has_mu( OPCODE );
extern BOOL OPERATOR_has_mu( OPERATOR );
extern BOOL WN_has_mu( const WN *, REGION_LEVEL );

// The ST field of the WN node contains an index into the VER_STAB after
// SSA Renaming.
//
extern BOOL WN_has_ver(const WN *wn);

// The ST field of the WN node need to be or has been converted to
// OPT_STAB index.
extern BOOL WN_has_aux(const WN *wn);

// The WN node with this operator need to be or has been converted to
// OPT_STAB index.
extern BOOL OPERATOR_has_aux(const OPERATOR opr);

// similar to OPERATOR_has_aux
extern BOOL OPCODE_has_aux(const OPCODE opc);

// To obtain the ST *
extern ST      *WN_sym(const WN *wn);

// To obtain the index into the OPT_STAB from the ST field.
extern AUX_ID   WN_aux(const WN *wn);

// To obtain the index into the VER_STAB from the ST field.
extern VER_ID   WN_ver(const WN *wn);

// Assign an index of the OPT_STAB into the ST field.
extern void     WN_set_aux(WN *wn, AUX_ID s);

// Assign an index of the VER_STAB into the ST field.
extern void     WN_set_ver(WN *wn, VER_ID v);

// Determine the mtype-class of the WN node
extern INT32    Get_mtype_class(MTYPE);

// Determine the opcode to use to load a value of the given mtype
extern OPCODE Ldid_from_mtype( MTYPE mtype );

// Get an MTYPE given an mtype class and size.
extern MTYPE Mtype_from_mtype_class_and_size( INT mtype_class, INT bytes );

// Determine the opcode to use to load a value of the given mtype 
// class and size in bytes.
extern OPCODE Ldid_from_mtype_class_and_size(INT mtype_class,INT bytes);

// Determine the opcode to use to store a value of the given mtype 
// class and size in bytes.
extern OPCODE Stid_from_mtype_class_and_size(INT mtype_class,INT bytes);

// Prepare the emit LNO info from mainopt
extern void Init_lno_info_for_main_emitter(void);

// Get the vertex id from the LNO dep graph
extern INT32 WN_get_dep_graph_vertex(WN *);

// Detach the WN node from the LNO dep graph
extern void  WN_detach_wn_from_dep_graph(INT32);

// Add the WN node to the LNO dep graph
extern void WN_add_lno_info(WN *, CODEREP *);

// Duplicate the WN's node dep_graph
extern void WN_dup_dep_vertex(WN *oldwn, WN *newwn);

// Print out the dep graph
extern void Print_dep_graph(FILE *);

// Obtain the PF-pointer of the WN
extern PF_POINTER *WN_get_pf_pointer(WN *wn);

// Print out the PF-pointer 
extern void Print_pf_pointer(FILE *, PF_POINTER *);

// Check if a ILOAD/ISTORE is volatile
extern BOOL Ilod_TY_is_volatile(TY_IDX);

// Check if a LDID/STID is volatile
extern BOOL Lod_TY_is_volatile(TY_IDX);

// Obtain the constant value from the WHIRL and present it in INT64
extern INT64 WN_get_const_val(WN *);

enum { SIGN_UNKNOWN = 0x0,
       SIGN_1_EXTD  = 0x1,
       SIGN_0_EXTD  = 0x2,
     };

extern UINT   Actual_data_size(CODEREP *cr, OPT_STAB *opt_stab, INT &signess);
extern BOOL  No_truncation_by_value_size(MTYPE to_mtype, BOOL sign_extd, CODEREP *rhs, OPT_STAB *opt_stab, BOOL trace_phi = TRUE);

// the following four functions are used for modeling the machine
// behavior on handling the load with zero extension and sign
// extension when two loads are CSEed.  We need to insert convertion
// when the ILOAD is both sign extended and zero extended ILOAD when
// we generate the load from PREG.  The information for this convert
// generation due to CSE by PRE is kept in the EXPREP of the tree.
// However, these utility routines are solely base on the result type
// and the descriptor type of an ILOAD.

// When the result type size is greater than the descriptor type size
// and the result type size is 4 bytes, or I8
extern BOOL Is_hi_sign_extended(MTYPE result_ty, MTYPE desc_ty);

// When the result type size is greater than the descriptor type size
// and the descriptor type is signed
extern BOOL Is_lo_sign_extended(MTYPE result_ty, MTYPE desc_ty);

// When we save to a preg for the expression, we want to load with
// sign extended value.  This function gives us the type of the load.
extern MTYPE  Type_for_saved_load(BOOL hi_sign_ext, BOOL lo_sign_ext,
                                  MTYPE lod_type);

// Create an assignment of the form "i = i"
extern WN *Create_identity_assignment(AUX_STAB_ENTRY *sym, AUX_ID aux_id, TY_IDX ty);

// Find the type to use for Create_identity_assignment()
// Returns null if a reasonable type for assignment can't be found
extern TY_IDX Identity_assignment_type( AUX_STAB_ENTRY *sym, OPT_PHASE phase );

//  Obtain the mod-ref information from the uplevel procedure variable list
extern READ_WRITE Get_MP_modref(const WN *pragma_list, const POINTS_TO *pt,
				const ALIAS_RULE *rule);

// Obtain the accessed id list for the nested procedure
extern WN *Get_MP_accessed_id_list(const ST *st);

// Check if we allow this opcode to be copy-propagated
extern BOOL Op_can_be_propagated(OPCODE op, OPT_PHASE phase);

// walk the pragma block of a region, look for the given pragma id
extern BOOL Is_region_with_pragma(WN *wn, WN_PRAGMA_ID pragma_id);

extern void Connect_cr_wn(WN_MAP *cr_wn_map, CODEREP *cr, WN *wn);

extern MTYPE Rebuild_rtype(MTYPE rtype, INT bits);

extern BOOL OPERATOR_is_fake(OPERATOR oper);
extern BOOL OPCODE_is_fake(OPCODE opc);
extern BOOL OPERATOR_is_volatile(OPERATOR oper);
extern BOOL OPCODE_is_volatile(OPCODE opc);
extern std::pair<bool, int> WN_get_val(WN *, std::map<WN *, WN *> &);
extern BOOL WN_has_disjoint_val_range(WN *, WN *, std::map<WN *, WN *> &, std::map<WN *, WN *> &, std::map<AUX_ID, WN *> &);
extern BOOL WN_has_indir_load(WN *);
extern void Collect_operands(WN *, STACK<WN *> *, STACK<WN *> *);
extern WN * WN_get_deriv(WN *, std::map<AUX_ID, WN *> &map);

  WN * WN_copy(WN *wn);  // copy a WN node
  WN * WN_copy_with_map (WN *wn);
  MTYPE Mtype_from_class_size(MTYPE t1, MTYPE t2);
  void fdump_tree(FILE *f, WN *wn);
  void fdump_wn(FILE *f,WN *wn);
  void fdump_wn_no_st(FILE *fp,WN *wn);
  void fdump_tree_no_st(FILE *fp,WN *wn);
  void WN_copy_stmap(WN *src, WN *dst);

  // The following should be called only from the debugger.
  // void dump_tree(WN *wn);
  // void dump_wn(WN *wn);
  // void dump_wn_no_st(WN *wn);
  // void dump_tree_no_st(WN *wn);

#define WN_Pragma_is_Parallel(pragma) \
  (pragma == WN_PRAGMA_PARALLEL_BEGIN ||\
   pragma == WN_PRAGMA_PFOR_BEGIN ||\
   pragma == WN_PRAGMA_PDO_BEGIN ||\
   pragma == WN_PRAGMA_PARALLEL_DO ||\
   pragma == WN_PRAGMA_DOACROSS ||\
   pragma == WN_PRAGMA_SINGLE_PROCESS_BEGIN)

#endif  // opt_wn_INCLUDED


