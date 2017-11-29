/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//-*-C++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: lego_util.h
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 *  Description:
 * 
 *    Utility routines for lego.
 *
 *  Exported types and functions:
 *
 *      DISTR_ARRAY *Lookup_DACT(ST *array_st);
 *         
 *          Find the DACT for the given array_st in the lego hash table.
 *
 * 
 *     typedef enum { var_local, var_global, var_formal, var_common } VAR_KIND;
 *          
 *          Classification of variable storage classes for lego purposes.
 *          
 *      VAR_KIND ST_Var_Kind (ST*);
 *
 *          Return the classification of the variable.
 *    
 *
 *      WN *AWN_Binary(OPERATOR opr, TYPE_ID rtype, WN *kid0, WN *kid1);
 *          
 *          Return a WN* for the binary operator opr with opcode of type rtype.
 *          The following macros use this as a common base:
 *              AWN_Mpy(TYPE_ID rtype, WN *l, WN *r)
 *              AWN_Mod(TYPE_ID rtype, WN *l, WN *r) 
 *              AWN_Rem(TYPE_ID rtype, WN *l, WN *r)         
 *              AWN_Sub(TYPE_ID rtype, WN *l, WN *r)         
 *              AWN_Add(TYPE_ID rtype, WN *l, WN *r)         
 *              AWN_Div(TYPE_ID rtype, WN *l, WN *r)         
 *              AWN_Max(TYPE_ID rtype, WN *l, WN *r)         
 *              AWN_Min(TYPE_ID rtype, WN *l, WN *r)         
 *
 *      WN *AWN_LdidSym(SYMBOL *var);
 *          
 *          Return the an LDID of the SYMBOL var.
 *
 *      WN *AWN_StidIntoSym(SYMBOL *var, WN *val);
 *
 *          Return the an STID of val into the SYMBOL var.
 * 
 *      WN *Create_Positive_Divceil(TYPE_ID type, WN *kid0, WN *kid1,
 *                  BOOL can_speculate);
 * 
 *          Divceil(a,b), a,b > 0 is equivalent to (a + b - 1) / b.  
 *          Assuming that kid0,kid1 > 0, return the (kid0 + kid1 - 1) / kid1.
 *          Mark the div as safe to speculate if so requested.
 *
 *      void Set_Array_Dim(WN *array_expr, INT dim, WN *bound, WN *index);
 *         
 *          Set the index and bound of dimension dim of array_expr to 
 *          the given whirl trees index and bound, respectively
 *         
 *      void Replace_WN(WN *old_wn, WN *new_wn);
 *       
 *           Replace old_wn with new_wn in old_wn's parent,
 *           do NOT delete old_wn. 
 *
 *      extern WN* Get_Numthreads_Ldid (LEGO_INFO* li);
 *
 *          Given a LEGO_INFO describing the affinity for a 
 *          dimension of a distributed array, return an LDID
 *          of the numprocs variable for that distributed dimension.
 *
 *      extern WN* Get_Runtime_Numthreads_Ldid ();
 *
 *          Return an LDID of the runtime variable, __mp_sug_numthreads.
 *
 *      extern WN* Get_Runtime_Cur_Numthreads_Ldid ();
 *
 *          Return an LDID of the runtime variable, __mp_cur_numthreads.
 * 
 *	extern void Freeze_Numthreads_Ldid(WN* wn_loop);
 * 
 * 	    Freeze the number of threads before the loop 'wn_loop'. 
 * 
 * 	extern void Freeze_Cur_Numthreads_Func(WN* wn_loop); 
 * 
 *	    Freeze the current number of threads before the loop 'wn_loop'.
 * 
 * 	extern WN* Get_Frozen_Numthreads_Ldid(WN* wn_loop);
 * 
 * 	    Get the frozen value of the number of threads for the loop 
 * 	    'wn_loop'. 
 *
 * 	extern WN* Get_Runtime_Cur_Numthreads_Func(WN* wn_loop) 
 * 
 * 	    Get the frozen value of the number of threads as a function
 * 	    for the PDO 'wn_loop'.
 *  
 *      extern void Set_Runtime_Call_Side_Effects (WN* call_wn);
 *          Set the side-effect bits on the WHIRL call node to a libMP
 *          routine.
 * 
 *      extern WN* Lego_Find_Node(SYMBOL sym, WN* wn_tree)
 * 
 *          Find the first node the the tree rooted at 'wn_tree' with
 * 	    symbol 'sym' and return it.
 * 
 *      extern void Lego_Find_Nodes(OPERATOR opr, SYMBOL sym, WN* wn_tree,
 *          STACK<WN*>* stack)
 *
 *	    Find all of the nodes in the tree rooted at 'wn_tree' with the
 *	    symbol 'sym' and OPERATOR type 'opr' and push their addresses 
 *	    on the 'stack'.
 *
 *      extern INT Get_New_Lego_Mp_Tile_Key(void) 
 *
 * 	    Returns a unique key which can be used to index groups of
 * 	    loops produced by lego or MP tiling.
 *
 *
 *
 *  extern SYMBOL* Create_Local_Symbol (char* name, TYPE_ID mtype);
 *      Create a local ST of the given mtype and name, return a SYMBOL.
 *
 *  extern WN* Get_MP_Region (WN* wn);
 *      Find the closest enclosing MP region (including wn), if any.
 *      Return NULL otherwise.
 *
 *  extern BOOL Fixed_Size_Array_Is_Stride_One(ST* array_st);
 *      Assert if the bounds and strides of an array are not constants.
 *      Then return true if it is stride-one.
 *
 *
 *  extern WN_MAP Safe_Spec_Map;
 *      This map applies only to dev/rem/mod operator nodes.
 *      If a node has a non-NULL value for this map, then it is safe
 *      to speculate this particular node.
 *
 *  
 * ====================================================================
 * ====================================================================
 */


#ifndef _LEGO_UTIL_INCLUDED_
#define _LEGO_UTIL_INCLUDED_

#include "defs.h"
#include "stab.h"
#include "wn.h"
#include "stab.h"
#include "access_vector.h"

/***********************************************************************
 *
 * General global declaration.
 *
 ***********************************************************************/

/* Globally visible ST entries for functions in the runtime library. */
#define HT_Push                  0
#define HT_Pop                   1
#define HT_Top                   2
#define HT_Check                 3
#define HT_Replace               4
#define Initialize_Dart          5
#define Allocate_Dart            6
#define Alloc_Reshape            7
#define Dealloc_Reshape          8
#define Migrate_Array            9
#define Unmigrate_Array         10
#define Migrate_Pages           11
#define Proc_Pool_Push          12
#define Proc_Pool_Pop           13
#define Cyclic_Bounds           14
#define Simple_Bounds           15
#define Processor_Layout        16
#define Processor_Coordinates   17
#define Dynamic_Affinity_Bounds 18
#define mp_sug_numthreads       19
#define mp_cur_numthreads       20
#define mp_my_threadnum         21
#define mp_numthreads_fn        22
#define Deallocate_Dart         23
#define Compare_Darts           24
#define ECHT_Push               25
#define ECHT_Pop                26
#define ECHT_Check              27
#define ECHT_Compare            28
#ifndef KEY // Pathscale does not have this
#define mp_cur_numthreads_func  29
#endif
#define DST_MAX                 30 


extern ST* distr_st_entries[DST_MAX];

/* Globally visible TY entries. */
#define RT_dim_struct   0
#define RT_dim_ptr      1
#define RT_struct       2
#define RT_ptr          3
#define VOID_ptr        4
#define DTY_MAX         5
extern TY_IDX distr_ty_entries[DTY_MAX];

/* Globally visible offsets into a dart. Only place that needs it
 * outside of lego_gen is lego_pragma, for cyclic(0) processing.
 */
#define dart_offset_num_dim           0
#define dart_offset_element_size      8
#define dart_offset_flags            16 
#define dart_offset_distr_n          24
#define dart_offset_distr_p          32
#define dart_offset_distr_k          40
#define dart_offset_distr_lb         48

/* number of doubles in dart base and in dart distr */
#define dart_base_size                3 
#define dart_distr_size               4

extern "C" {
  void Mp_File_Init (void);
}

/***********************************************************************
 *
 * Looking up DACTs 
 *
 ***********************************************************************/

class DISTR_ARRAY;
extern DISTR_ARRAY *Lookup_DACT(ST *array_st);
extern TY_IDX DART_ptr_TY; // global variable, storing TY of dart-ptr

/***********************************************************************
 *
 * Classifying variables
 *
 ***********************************************************************/


typedef enum { var_local, var_global, var_formal, var_common } VAR_KIND;

extern VAR_KIND ST_Var_Kind (ST*);

#define ST_isGlobal(st) (ST_Var_Kind(st) == var_global)
#define ST_isLocal(st) (ST_Var_Kind(st) == var_local)
#define ST_isFormal(st) (ST_Var_Kind(st) == var_formal)
#define ST_isCommon(st) (ST_Var_Kind(st) == var_common)

extern TY_IDX Lego_Get_Original_Type(ST* st);
extern TY_IDX Lego_Get_Array_Type(ST* st);

/***********************************************************************
 *
 * Creating and manipulating code
 *
 ***********************************************************************/

extern WN *AWN_Binary   (OPERATOR opr, TYPE_ID rtype, WN *kid0, WN *kid1,
                         BOOL can_speculate = FALSE);
extern WN* AWN_LdidSym  (SYMBOL *var);
extern WN *AWN_StidIntoSym(SYMBOL *var, WN *val);
extern WN *Create_Positive_Divceil(TYPE_ID type, WN *kid0, WN *kid1,
                                   BOOL can_speculate);
extern void Set_Array_Dim(WN *array_expr, INT dim, WN *bound, WN *index);
extern void Replace_WN  (WN *old_wn, WN *new_wn);

class LEGO_INFO;
extern WN* Get_Numthreads_Ldid (LEGO_INFO* li);
extern WN* Get_Runtime_Numthreads_Ldid ();
extern WN* Get_Runtime_Cur_Numthreads_Ldid ();
extern void Set_Runtime_Call_Side_Effects (WN* call_wn);
extern void Freeze_Numthreads_Ldid(WN* wn_loop);
extern void Freeze_Cur_Numthreads_Func(WN* wn_loop);
extern WN* Get_Frozen_Numthreads_Ldid(WN* wn_loop); 
extern WN* Get_Runtime_Cur_Numthreads_Func(WN* wn_loop);
extern WN* Loop_Step(WN* wn_loop);

extern SYMBOL* Create_Local_Symbol (char* name, TYPE_ID mtype);
extern WN* Get_MP_Region (WN* wn);
extern BOOL Fixed_Size_Array_Is_Stride_One(ST* array_st);

extern ST *Find_Return_Registers(TYPE_ID type,PREG_NUM *rreg1,PREG_NUM *rreg2);

extern BOOL PU_has_reshaped_commons;
extern WN_MAP Safe_Spec_Map;

#define AWN_Mpy(rtype, l, r)           AWN_Binary(OPR_MPY, rtype, l, r, FALSE)
#define AWN_Mod(rtype, l, r)           AWN_Binary(OPR_MOD, rtype, l, r, FALSE)
#define AWN_Rem(rtype, l, r)           AWN_Binary(OPR_REM, rtype, l, r, FALSE)
#define AWN_Sub(rtype, l, r)           AWN_Binary(OPR_SUB, rtype, l, r, FALSE)
#define AWN_Add(rtype, l, r)           AWN_Binary(OPR_ADD, rtype, l, r, FALSE)
#define AWN_Div(rtype, l, r)           AWN_Binary(OPR_DIV, rtype, l, r, FALSE)
#define AWN_Max(rtype, l, r)           AWN_Binary(OPR_MAX, rtype, l, r, FALSE)
#define AWN_Min(rtype, l, r)           AWN_Binary(OPR_MIN, rtype, l, r, FALSE)

#define AWN_Rem_Safe(rtype, l, r)      AWN_Binary(OPR_REM, rtype, l, r, TRUE)
#define AWN_Div_Safe(rtype, l, r)      AWN_Binary(OPR_DIV, rtype, l, r, TRUE)
#define AWN_Mod_Safe(rtype, l, r)      AWN_Binary(OPR_MOD, rtype, l, r, TRUE)

extern WN* Lego_Find_Node(SYMBOL sym, WN* wn_tree);

extern void Lego_Find_Nodes(OPERATOR opr, SYMBOL sym, WN* wn_tree,
                            STACK<WN*>* stack);

extern mBOOL Single_Loop_Coeff(ACCESS_VECTOR *av, INT64 *stride, INT64 *offset,
                               mINT32 *depth);

extern INT Get_New_Lego_Mp_Tile_Key(void); 

enum MP_SCHED_TYPE {MP_SCHED_UNKNOWN,
                    MP_SCHED_SIMPLE,
                    MP_SCHED_DYNAMIC,
                    MP_SCHED_GSS,
                    MP_SCHED_INTERLEAVE,
                    MP_SCHED_RUNTIME,
                    MP_SCHED_PSEUDOLOWERED};
class MP_INFO {
  MP_SCHED_TYPE _sched_type;
  SYMBOL* _pid_sym0;
  SYMBOL* _pid_sym1;
  INT _nest_index;
  INT _nest_total;
  SYMBOL* _nest_layout;
  BOOL _is_pdo; 
  BOOL _plower_disabled;
  SYMBOL* _sym_frozen;  
public:
  MP_INFO(MP_SCHED_TYPE sched_type, BOOL is_pdo);
  MP_INFO(WN* wn_pragmas);
  MP_INFO(MP_INFO* mp_info);
  MP_SCHED_TYPE Sched_Type() const { return _sched_type; }
  SYMBOL* Pid_Sym0() const { return _pid_sym0; }
  SYMBOL* Pid_Sym1() const { return _pid_sym1; }
  void Set_Pid0(SYMBOL *pid_sym) { _pid_sym0 = pid_sym; }
  void Set_Pid1(SYMBOL *pid_sym) { _pid_sym1 = pid_sym; }
  INT Nest_Index() { return _nest_index; }
  INT Nest_Total() { return _nest_total; }
  void Set_Nest_Total(INT nest_total) { _nest_total = nest_total; }
  SYMBOL* Nest_Layout() { return _nest_layout; }
  void Set_Nest_Layout(SYMBOL* sym_layout)
    { _nest_layout = sym_layout; }
  BOOL Is_Pdo() { return _is_pdo; }
  BOOL Plower_Disabled() const { return _plower_disabled; }
  void Disable_Plowering () { _plower_disabled = TRUE; }
  SYMBOL* Sym_Frozen() { return _sym_frozen; }
  void Set_Sym_Frozen(SYMBOL* sym_frozen) { _sym_frozen = sym_frozen; }
  void Print(FILE* fp);
};

/***********************************************************************
 *
 * Debugging information
 *
 ***********************************************************************/


extern BOOL Debug_Lego;
extern BOOL Verbose_Lego;
#ifdef  Is_True_On
#define DB_PRINT(x) if (Debug_Lego) { x; }
#else
#define DB_PRINT(x)
#endif

#ifdef Is_True_On
#define VB_PRINT(x) if (Verbose_Lego) { x; }
#else
#define VB_PRINT(x)
#endif

#endif // _LEGO_UTIL_INCLUDED_
