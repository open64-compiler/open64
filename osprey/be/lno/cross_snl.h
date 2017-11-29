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


// This is definitely -*- C++ -*-
//
// ===================================================================
// .NAME ARRAY_SNL_INFO - Keeps information about an SNL
// .INCLUDE cross_snl.h
// .FILE cross_snl.h
//
// .SECTION Description
//
//  This class contains information about array usage in an SNL
//       1) Array sections only read
//       2) Array sections only written
//       3) Array scetions both read and written
//       4) Which loops of the SNL access which dimensions
//
//
// Exported Type :
// 
//   ARRAY_SNL_INFO 
//  
// ====================================================================

#ifndef _cross_snl_INCLUDED
#define _cross_snl_INCLUDED


/*
 * Stores information about individual array references
 */
class ARA_REF_INFO {
  ARA_REF       * _ref;               // the original reference
  ARA_REF       * _proj_ref;          // the projected reference
  BOOL            _is_messy;
  INT32           _dim;

  public :
  ARA_REF_INFO(ARA_REF* ref, ARA_LOOP_INFO* leaf);
  ARA_REF *Get_Ref() { return _ref; }
  ARA_REF *Get_Proj_Ref() { return _proj_ref; }
  INT32   Dim(void) {return _dim; }
  void Set_Messy(BOOL flag) { _is_messy = flag; }
  BOOL Is_Messy(void) { return _is_messy; }
  void Print(FILE *file);

};

typedef STACK<ARA_REF_INFO*> ARA_REF_INFO_ST;

/*
 * Stores information about all references in an SNL
 */
class ARRAY_SNL_INFO {
  WN              * _snl_root;        // the root do-loop of the SNL
  WN              * _snl_leaf;        // th einner most loop of the snl
  INT               _depth;           // how many loops are there in this SNL
  ARA_LOOP_INFO   * _ara_root;        // root of array region analysis structure
  ARA_LOOP_INFO   * _ara_leaf;        // info correspoding to inner most loop
  ARA_REF_INFO_ST   _rd_ref_list;     // list of array references in this SNL
  ARA_REF_INFO_ST   _wr_ref_list;

  BOOL             _is_messy;           // Is this messy or not?

  void Add_Reference(ARA_REF_INFO_ST * ref_list, ARA_REF *ref);
  void Add_Write_Reference(ARA_REF *ref);
  void Add_Read_Reference(ARA_REF *ref);

public: 
  ARRAY_SNL_INFO(WN* wn_outer, INT nloops, ARA_LOOP_INFO *ara_root);

  INT Get_Depth(void) { return _depth;}
  WN* Get_SNL_Root(void) { return _snl_root; }
  ARA_REF_INFO_ST& Read_Refs(void)  { return _rd_ref_list; }
  ARA_REF_INFO_ST& Write_Refs(void) { return _wr_ref_list; }
  BOOL Is_Messy(void) { return _is_messy; }
  void  Set_Messy(BOOL flag) { _is_messy = flag; }
  void Walk_SNL(void);
  void Print(FILE* file);

};


/*
 * Stores information about an SNL used in cross loop cache analysis
 */
typedef STACK<PARALLEL_INFO *> PARALLEL_INFO_ST;

class CROSS_SNL_INFO {
  WN                 *_snl_root;         // root of the SNL
  INT32               _depth;            // depth of the SNL
  ARRAY_SNL_INFO     *_asi;              // references in this SNL
  double              _seq_mch_cost;     // cost of the sequental version of the SNL
  double              _seq_cache_cost;   // cost of sequential cache cost 
  PARALLEL_INFO_ST    _pist;             // a list of parallel info structures
  
public: 
  CROSS_SNL_INFO(WN* parent, INT32 nloops);
  ARRAY_SNL_INFO *Get_Array_References(void) { return _asi; }
  void Set_Array_References(ARRAY_SNL_INFO *asi) { _asi = asi; }
  WN * SNL_Root(void) { return _snl_root; }
  INT32 SNL_Depth(void) { return _depth; }
  INT32 Num_Parallel_Options(void) { return _pist.Elements(); }
  PARALLEL_INFO_ST *Parallel_Options(void) { return &_pist; }
  PARALLEL_INFO * Get_Parallel_Option(INT32 i) { return _pist.Bottom_nth(i); }
  void Print(FILE *f);

  void Set_Seq_Machine_Cost(double cost) {  _seq_mch_cost = cost; }
  void Set_Seq_Cache_Cost(double cost) { _seq_cache_cost = cost; }
  double Get_Seq_Machine_Cost(void) { return _seq_mch_cost; }
  double Get_Seq_Cache_Cost(void) { return _seq_cache_cost; }

  void Sort_Parallel_Options(void);
  void Weed_Out_Minimum(INT nmin);
  void Weed_Out_Inner(void);

};

// to keep a list of SNLs
// we will be working on all the SNLs in this list
typedef STACK<CROSS_SNL_INFO *> CROSS_SNL_INFO_ST;

// The group of SNLs we will be working with
class SNL_STREAM {
  CROSS_SNL_INFO_ST   _st;                    // list of snls in this stream
  WN                 *_parent;               // parent of the sibling SNLs
  ARA_LOOP_INFO      *_ara_info;             // ara_info corresponding to parent

  // set of variables to return a stream of parallelization options
  BOOL                _parallel_only;   // parallel options only?
  INT32*              _options;         // the options : one for each SNL
  INT32*              _min_path;        // the minimum cost path
  double              _min_cost;        // the minimum cost
  BOOL                _finished;        // last one seen?

public:
  SNL_STREAM(WN *parent);
  ~SNL_STREAM(void);
  WN * Get_Parent(void) { return _parent; }
  ARA_LOOP_INFO * Get_Ali(void) { return _ara_info; }
  INT32 Num_SNL(void) { return _st.Elements(); }
  void Add_SNL(CROSS_SNL_INFO *csi) { _st.Push(csi); }
  CROSS_SNL_INFO *Get_SNL(INT32 i) { return _st.Bottom_nth(i); }
  void Cleanup(void);
  void Print(FILE *f);

  void Stream_Init(BOOL parallel_only);
  void Stream_Next(void);
  INT32 *Stream_Curr(void);
  BOOL Stream_Over(void);
  void Set_Min_Path(double cost);
};

typedef STACK<SNL_STREAM *> SNL_STREAM_ST;


/* --------------------------------------------------------------------- */
void SNL_Parallelization_Costs(WN* wn_outer, INT nloops, PARALLEL_INFO_ST  *pist,
			       double *mscc, double *msmc);

extern void Cross_Loop_Cache_Analysis(PU_Info* current_pu, WN* func_nd);

#endif










