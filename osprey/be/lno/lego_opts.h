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


//-*-c++-*-
//
// ====================================================================
// ====================================================================
//
// Module: lego_opts.h
// $Revision$
// $Date$
// $Author$
// $Source$
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
//    
//    Data structures and routines for lego-specific code transformations.
// 
// Exported types and functions:
//
//     LEGO_INFO
//
//         Specifies lego-specific code transformations. Contains the
//         affinity function used for tiling/scheduling the code and 
//         front and back peeling factors.
//
//     WN *Generate_Bounds(const WN *doloop, 
//			   const SYMBOL *pid, 
//			   SYMBOL **new_lb, SYMBOL **new_ub, 
//			   SYMBOL **new_step, INT bound);         
//         Generate new loop bounds and step for doloop to
//         partition the iteration space across processors, according to
//         lego_info. The new bounds and step are functions of pid, the 
//         processor id. The code to generate the new bounds and step
//         is placed into a OPC_BLOCK and returned.  The new lower bound,
//         upper bound and step are assigned into new_lb, new_ub, and
//         new_step, respectively. For 2-level loop tiling, bound == 0,
//	   the default.  For 3-level loop tiling, bound == 0 => implies 
//         generates the bounds for the middle loop in the tile, while 
//         bound == 1 generates the bounds for the inner loop in the tile. 
//
//	BOOL Loop_Bounds_Simple(const WN *doloop)
//
//	   Returns TRUE if loop bounds are simple enough for us to 
//    	   generate scheduling code for lego. 
//
//	WN* Interleaved_Pragma_Chunksize(const WN* wn_loop, DU_MANAGER* du)
//
//	   Returns a WN* to the chunksize with which the 'wn_loop' is
//	   to be interleave scheduled.  Asserts if 'wn_loop' is not an
//         interleave scheduled do_across.
//
//  extern WN_MAP RR_Map;
//      This map applies only to array nodes that reference reshaped arrays.
//      The value of this MAP (if any) carries information about the local/
//      remote nature of the reference for each dimension of the array.
//
// ====================================================================
// ====================================================================

#ifndef lego_opts_INCLUDED
#define lego_opts_INCLUDED "lego_opts.h"

#ifdef _KEEP_RCS_ID
static char *lego_opts_rcs_id = access_vector_INCLUDED "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "access_vector.h"
#include "stab.h"
#include "wn.h"
#include "lego_affinity.h"

class LEGO_INFO {
  BOOL  _dynamic_affinity;
  SYMBOL *_array;
  INT32 _dim_num;
  INT32 _stride;
  INT32 _offset;
  INT32 _front_peel;
  INT32 _back_peel;
  INT32 _min_offset;
  INT32 _max_offset;
  BOOL  _too_messy;

  SYMBOL *_pid_sym0;                  // process id (first tile) 
  SYMBOL *_pid_sym1;                  // process id (second tile) 
  SYMBOL *_local_index_sym;           // current index value on curr proc

  SYMBOL *_runtime_lb_sym;            // symbol for lb if runtime bounds generated
  SYMBOL *_runtime_ub_sym;            // symbol for ub if runtime bounds generated
  SYMBOL *_runtime_step_sym;          // symbol for step if runtime bounds generated

  DYN_ARRAY<WN*> _local_index_wn;    // All defs of the local_index

public:
  LEGO_INFO(SYMBOL *array);
  LEGO_INFO(SYMBOL *array, INT32 dim, INT32 stride, INT32 offset);
  LEGO_INFO(SYMBOL *array, INT32 dim, INT32 stride, INT32 offset,
	    INT32 front_peel, INT32 back_peel);
  LEGO_INFO(LEGO_INFO* lego_info, MEM_POOL *pool); 
  ~LEGO_INFO(); 
  void Init(SYMBOL *array, INT32 dim, INT32 stride, INT32 offset,
	    INT32 front_peel, INT32 back_peel);

  BOOL Dynamic_Affinity () const { return _dynamic_affinity; }
  void Set_Dynamic_Affinity () { _dynamic_affinity = TRUE; }
  SYMBOL *Array()  const { return _array; }
  INT32 Dim_Num() const { return _dim_num; }
  INT32 Stride() const { return _stride; }
  INT32 Offset() const { return _offset; }
  INT32 Front_Peel() const { return _front_peel; }
  INT32 Back_Peel() const { return _back_peel; }
  INT32 Min_Offset() const { return _min_offset; }
  INT32 Max_Offset() const { return _max_offset; }
  BOOL Is_Too_Messy() const { return _too_messy; }

  SYMBOL *Pid_Sym0() const { return _pid_sym0; }
  SYMBOL *Pid_Sym1() const { return _pid_sym1; }

  SYMBOL *Runtime_Lb_Sym() const { return _runtime_lb_sym; }
  SYMBOL *Runtime_Ub_Sym() const { return _runtime_ub_sym; }
  SYMBOL *Runtime_Step_Sym() const { return _runtime_step_sym; }
  void Set_Runtime_Lb_Sym(SYMBOL *lb_sym) { _runtime_lb_sym = lb_sym; }
  void Set_Runtime_Ub_Sym(SYMBOL *ub_sym) { _runtime_ub_sym = ub_sym; }
  void Set_Runtime_Step_Sym(SYMBOL *step_sym) { _runtime_step_sym = step_sym; }

  SYMBOL *Local_Index_Sym() const { return _local_index_sym; }
  WN *Local_Index_WN(INT i) const { return _local_index_wn[i]; }
  BOOL Has_Local_Index() const { return (_local_index_sym != NULL); }
  INT64 Get_Local_Step(WN *doloop);

  WN *Pid0(WN* curr_wn);  // returns ldid of _pid_sym
  WN *Pid1(WN* curr_wn);  // returns ldid of _pid_sym
  WN *Local_Index();      // returns ldid of _local_index_sym

  void Set_Front_Peel(INT32 front_peel) { _front_peel = front_peel; }
  void Set_Back_Peel(INT32 back_peel) { _back_peel = back_peel; }
  void Set_Min_Offset(INT32 min_offset) { _min_offset = min_offset; }
  void Set_Max_Offset(INT32 max_offset) { _max_offset = max_offset; }
  void Set_Too_Messy() { _too_messy = TRUE; }
  void Set_Pid0(SYMBOL *pid_sym);
  void Set_Pid1(SYMBOL *pid_sym);
  void Create_Local_Index(WN *doloop);

  void Print(FILE* fp);
};

extern BOOL Loop_Bounds_Simple(const WN *doloop);
extern WN *Generate_Bounds(const WN *doloop, 
			   SYMBOL **new_lb, SYMBOL **new_ub,
			   SYMBOL **new_step, INT bound = 0);

extern BOOL disable_divmod_opts;
extern BOOL disable_rr_maps;
extern WN_MAP RR_Map;
  
class RR_DIM {
  INT _offset;  // 0 means conforming. Non-zero means do div/mod
  WN* _do_wn;   // pointer to the processor tile-loop
  INT _index;   // index into Lego_LB_Symbol array of processor tile-loop
  RR_DIM (const RR_DIM&);
  RR_DIM* operator= (const RR_DIM&);
public:
  RR_DIM (void) {}
  void Init (INT offset, WN* do_wn, INT index) {
    _offset = offset;
    _do_wn = do_wn;
    _index = index;
  }
  void Init (RR_DIM* rrdim) {
    _offset = rrdim->_offset;
    _do_wn = rrdim->_do_wn;
    _index = rrdim->_index;
  }
  void Remotize () { _do_wn = NULL; }
  WN* Do_Loop () const { return _do_wn; }
  INT Index ()   const { return _index; }
  void Print (FILE* fp) {
    fprintf (fp, "offset %d proc_wn 0x%p index %d\n", _offset, _do_wn, _index);
  }
};

class RR_INFO {
  INT     _ndims;
  RR_DIM* _rrdim;
  RR_INFO (void);
  RR_INFO (const RR_INFO&);
  RR_INFO* operator= (const RR_INFO&);
public:
  RR_INFO (INT ndims);
  RR_INFO (RR_INFO* rri);
  ~RR_INFO ();
  RR_DIM* Dim(INT i) const { return &(_rrdim[i]); }
  void Print (FILE* fp) {
    fprintf (fp, "RR_Map\n");
    for (INT i=0; i<_ndims; i++) {
      fprintf (fp, "\t%d: ", i);
      _rrdim[i].Print(fp);
    }
  }
};

extern RR_INFO* Get_RR_Map (WN* wn);
extern void Set_RR_Map (WN* wn, RR_INFO* rri);
extern void Pre_Peel_RR_Map_Update  (WN* do_wn, WN* prev_wn, BOOL create_loop);
extern void Post_Peel_RR_Map_Update (WN* do_wn, WN* next_wn, BOOL create_loop);
extern WN* Interleaved_Pragma_Chunksize(const WN* wn_loop, DU_MANAGER* du);

#endif 




