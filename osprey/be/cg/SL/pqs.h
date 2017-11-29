/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#ifndef PQS_H_INCLUDED
#define PQS_H_INCLUDED

#ifndef PQSTEST
#define PQS_USE_MEMPOOLS
#endif

// Tracing variable
extern BOOL PQS_Tracing;

#ifdef PQS_USE_MEMPOOLS
// Mempool variable
extern MEM_POOL PQS_mem_pool;
extern void PQS_Init_Memory(void);
#endif
 
/*================================================================
//================================================================

Vectors of PQS_NODEs

//================================================================
*/

#ifdef PQS_USE_MEMPOOLS
typedef mempool_allocator<PQS_NODE_IDX> PQS_NODE_IDX_ALLOCATOR;
#else
typedef alloc PQS_NODE_IDX_ALLOCATOR;
#endif

typedef vector<PQS_NODE_IDX,PQS_NODE_IDX_ALLOCATOR> PQS_NODE_IDX_VECTOR;


/*================================================================
//================================================================
//================================================================
*/

// Pairs of index and TN.
typedef pair<PQS_NODE_IDX,PQS_TN> PQS_TNI;
#define PQS_TNI_TN(x) ((x).second)
#define PQS_TNI_IDX(x) ((x).first)

struct lt_tni {
   inline bool operator()(const PQS_TNI &t1, const PQS_TNI &t2) const {
      if (PQS_TNI_TN(t1) == PQS_TNI_TN(t2)) {
	 return PQS_TNI_IDX(t1) < PQS_TNI_IDX(t2);
      } else {
	 return TN_number(PQS_TNI_TN(t1)) < TN_number(PQS_TNI_TN(t2));
      }
   }
};

typedef PQS_SET<PQS_TNI,lt_tni> PQS_TNI_SET;
typedef PQS_SET<PQS_TNI,lt_tni>::set_type PQS_TNI_SET_TYPE;

//================================================================
//================================================================
//================================================================
/* This is the definition of the NODE class 
 */

class PQS_NODE {
friend class PQS_MANAGER;
private:
// Data members
   PQS_OP    _inst;                    // pointer to actual instruction setting things
   PQS_ITYPE _itype;                   // Style of the instruction (i.e. UNC, OR,  AND, etc.)
   PQS_TN       out_pred1,out_pred2;   // Points to the TN's for the predicates set by the instruction
   PQS_NODE_IDX in_pred1,in_pred2;     // Nodes for the instructions setting previous instances
   PQS_NODE_IDX qual_pred;             // Node for the qualifying predicate
   PQS_TN       qual_tn;               // TN of the qualifying predicate
   PQS_NODE_IDX other_instruction;     // Pointer to another instruction having the same condition
   PQS_NODE_IDX_VECTOR use1,use2;
   PQS_NODE_FLAGS flags;               // Miscellaneous flags

   PQS_MARKER_TYPE _marker1,_marker2;        // For tree walks
   void Init(void);

public:
   PQS_NODE()
#ifdef PQS_USE_MEMPOOLS
      : use1(PQS_NODE_IDX_VECTOR::allocator_type(&PQS_mem_pool)),
	use2(PQS_NODE_IDX_VECTOR::allocator_type(&PQS_mem_pool)) 
#endif
   {
      Init();
   }
   PQS_NODE(PQS_ITYPE itype, PQS_OP inst)
#ifdef PQS_USE_MEMPOOLS
      : use1(PQS_NODE_IDX_VECTOR::allocator_type(&PQS_mem_pool)),
	use2(PQS_NODE_IDX_VECTOR::allocator_type(&PQS_mem_pool)) 
#endif
   {
      Init();
      _inst = inst;
      _itype = itype;
   }

   ~PQS_NODE(){};

   void add_use(PQS_TN tn, PQS_NODE_IDX idx);
   inline INT32 num_use1() { return use1.size(); }
   inline INT32 num_use2() { return use2.size(); }
   inline PQS_NODE_IDX get_use1(INT32 i) { Is_True((i>=0 && i<num_use1()),("Bad i")); return use1[i]; }
   inline PQS_NODE_IDX get_use2(INT32 i) { Is_True((i>=0 && i<num_use2()),("Bad i")); return use2[i]; }
   
   inline PQS_MARKER_TYPE Get_Marker1(void) {return _marker1;}
   inline PQS_MARKER_TYPE Get_Marker2(void) {return _marker2;}
   inline void Set_Marker1(PQS_MARKER_TYPE m) {_marker1 = m;}
   inline void Set_Marker2(PQS_MARKER_TYPE m) {_marker2 = m;}
   inline void Set_Flags(PQS_NODE_FLAGS f) {flags = f;}
   inline PQS_NODE_FLAGS Get_Flags(void) {return flags;}

   void Print(FILE *f=stdout);
};

#ifdef PQS_USE_MEMPOOLS
typedef mempool_allocator<PQS_NODE> PQS_NODE_ALLOCATOR;
#else
typedef alloc PQS_NODE_ALLOCATOR;
#endif
typedef vector <PQS_NODE,PQS_NODE_ALLOCATOR> PQS_NODE_VECTOR;

class PQS_MANAGER {
private:
   PQS_NODE_VECTOR _data;
   PQS_MARKER_TYPE _mark_number;

   BOOL PQS_is_disjoint_helper(PQS_NODE_IDX tni2, PQS_TN tn2);
   PQS_TRUTH never_true_together(PQS_TN t1, PQS_TN t2, PQS_NODE_IDX tni);
   BOOL may_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL may_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL always_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL always_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL never_set_TRUE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL never_set_FALSE(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL may_set_TRUE(INT32 truth);
   BOOL may_set_FALSE(INT32 truth);
   BOOL never_set_TRUE(INT32 truth);
   BOOL never_set_FALSE(INT32 truth);
   BOOL always_set_TRUE(INT32 truth);
   BOOL always_set_FALSE(INT32 truth);
   BOOL qual_always_true(INT32 truth);
   INT32  get_truth_info(PQS_NODE_IDX tni, PQS_TN tn);
   BOOL PQS_is_subset_of(PQS_NODE_IDX tni1, PQS_TN tn1, PQS_NODE_IDX tni2, PQS_TN tn2);
   BOOL PQS_is_subset_of(PQS_NODE_IDX tni1, PQS_TN tn1, PQS_TN_SET &tns2);
   BOOL PQS_is_disjoint (PQS_NODE_IDX tni1, PQS_NODE_IDX tni2, PQS_TN tn1, PQS_TN tn2);
   BOOL PQS_is_disjoint_h (PQS_NODE_IDX tni1, PQS_NODE_IDX tni2, PQS_TN tn1, PQS_TN tn2);
   void PQS_Mark_TN_Parents_TRUE(PQS_NODE_IDX tni, PQS_TN tn);
   void PQS_Mark_TN_Parents_TRUE(PQS_TN tn);
   PQS_TN_SET Simplify_TN_Set (const PQS_TN_SET &tn_in);
   void Simplify_TNI_Set (PQS_TNI_SET &tni_in);
   BOOL Simplify_In_Set(PQS_NODE_IDX tni, PQS_TN tn, PQS_TNI_SET &tnis);
   void Init_TN_OP_Info(void);

public:

   PQS_MANAGER()
#ifdef PQS_USE_MEMPOOLS
      : _data(&PQS_mem_pool)
#endif
   {
      PQS_NODE dummy;
      _mark_number=0;
      _data.push_back(dummy);
      Init_TN_OP_Info();
   }

   ~PQS_MANAGER();

#ifndef PQSTEST
   TN_MAP PQS_tn_map;
   OP_MAP PQS_op_map;
#endif   
   
   inline PQS_MARKER_TYPE Current_Marker() {return _mark_number;}
   inline void Update_Marker(void) {++_mark_number;}
   inline void Set_Manager_Marker(PQS_MARKER_TYPE mark) {_mark_number = mark;}

   inline PQS_NODE_IDX New_pqs_idx(PQS_ITYPE itype, PQS_OP inst); 

   // Accessors
   inline void PQS_NODE_set_out_pred1(PQS_NODE_IDX i, PQS_TN p1) {_data[i].out_pred1 = p1;}
   inline void PQS_NODE_set_itype(PQS_NODE_IDX i, PQS_ITYPE itype) {_data[i]._itype = itype;}
   inline void PQS_NODE_set_out_pred2(PQS_NODE_IDX i, PQS_TN p2) {_data[i].out_pred2 = p2;}
   inline void PQS_NODE_set_in_pred1(PQS_NODE_IDX i, PQS_NODE_IDX p1) {_data[i].in_pred1 = p1;}
   inline void PQS_NODE_set_in_pred2(PQS_NODE_IDX i, PQS_NODE_IDX p2) {_data[i].in_pred2 = p2;}
   inline void PQS_NODE_set_qual_pred(PQS_NODE_IDX i, PQS_NODE_IDX p1) {_data[i].qual_pred = p1;}
   inline void PQS_NODE_set_qual_tn(PQS_NODE_IDX i, PQS_TN t) {_data[i].qual_tn = t;}
   inline void PQS_NODE_add_use(PQS_NODE_IDX i, PQS_TN p, PQS_NODE_IDX use) {
      if (PQS_Is_Real_Idx(i)) _data[i].add_use(p,use);
   }
   inline void PQS_NODE_Mark(PQS_NODE_IDX i) {
      _data[i].Set_Marker1(_mark_number);
      _data[i].Set_Marker2(_mark_number);
   }
   inline void PQS_NODE_Mark(PQS_NODE_IDX i,PQS_TN t) {
      // set the marker for the specific TN t
      if (_data[i].out_pred1 == t) _data[i].Set_Marker1(_mark_number);
      if (_data[i].out_pred2 == t) _data[i].Set_Marker2(_mark_number);
   }
   inline void PQS_NODE_set_flags(PQS_NODE_IDX i,PQS_NODE_FLAGS f) {_data[i].flags=f;}
   inline void PQS_NODE_set_condition_true(PQS_NODE_IDX i) {_data[i].flags |= PQS_FLAG_CONDITION_TRUE;}
   inline void PQS_NODE_set_condition_false(PQS_NODE_IDX i) {_data[i].flags |= PQS_FLAG_CONDITION_FALSE;}
   
   // getters

   inline PQS_OP PQS_NODE_get_op(PQS_NODE_IDX i) {      return _data[i]._inst;   }
   inline PQS_ITYPE PQS_NODE_get_itype(PQS_NODE_IDX i) {      return _data[i]._itype;   }
   inline PQS_TN PQS_NODE_get_out_pred1(PQS_NODE_IDX i) {      return _data[i].out_pred1;   }
   inline PQS_TN PQS_NODE_get_out_pred2(PQS_NODE_IDX i) {      return _data[i].out_pred2;   }
   inline PQS_NODE_IDX PQS_NODE_get_in_pred1(PQS_NODE_IDX i) {      return _data[i].in_pred1;   }
   inline PQS_NODE_IDX PQS_NODE_get_in_pred2(PQS_NODE_IDX i) {      return _data[i].in_pred2;   }
   inline PQS_TN PQS_NODE_get_qual_tn(PQS_NODE_IDX i) {      return _data[i].qual_tn;   }
   inline PQS_NODE_IDX PQS_NODE_get_qual_pred(PQS_NODE_IDX i) {      return _data[i].qual_pred;   }
   inline INT32 PQS_NODE_num_use1(PQS_NODE_IDX i) {      return _data[i].num_use1();   }
   inline INT32 PQS_NODE_num_use2(PQS_NODE_IDX i) {     return _data[i].num_use2();   }
   inline PQS_NODE_IDX PQS_NODE_get_use1(PQS_NODE_IDX i,INT32 num) {      return _data[i].get_use1(num);   }
   inline PQS_NODE_IDX PQS_NODE_get_use2(PQS_NODE_IDX i,INT32 num) {      return _data[i].get_use2(num);   }
   inline BOOL Is_Marked(PQS_NODE_IDX i) {return (_mark_number == _data[i]._marker1 || _mark_number == _data[i]._marker2);
   }

   inline BOOL Is_Marked1(PQS_NODE_IDX i) {return (_mark_number == _data[i]._marker1);}
   inline BOOL Is_Marked2(PQS_NODE_IDX i) {return (_mark_number == _data[i]._marker2);}

   inline PQS_MARKER_TYPE PQS_NODE_get_marker(PQS_NODE_IDX i, INT32 num) { 
      if (num==1) return _data[i].Get_Marker1();
      else return _data[i].Get_Marker2();
   }
   inline PQS_NODE_FLAGS PQS_NODE_get_flags(PQS_NODE_IDX i) {return _data[i].flags;}
   inline BOOL PQS_NODE_condition_true(PQS_NODE_IDX i) {return (_data[i].flags & PQS_FLAG_CONDITION_TRUE) != 0;}
   inline BOOL PQS_NODE_condition_false(PQS_NODE_IDX i) {return (_data[i].flags & PQS_FLAG_CONDITION_FALSE) != 0;}


   // Utility
   
   // given an index and a TN, return the index of the previous instance
   inline PQS_NODE_IDX PQS_NODE_get_up_idx(PQS_NODE_IDX i, PQS_TN t)
   {
      if (PQS_NODE_get_out_pred1(i) == t) {
	 return PQS_NODE_get_in_pred1(i);
      } else if (PQS_NODE_get_out_pred2(i) == t) {
	 return PQS_NODE_get_in_pred2(i);
      } else {
	 FmtAssert(0,("get_up_idx: malformed idx %d\n",i));
	 return PQS_IDX_INVALID;
      }
   }
   
  
   // given an index and a TN, return whether this is the TN in the 1 slot or the TN in the 2 slot
   inline INT32 PQS_NODE_get_1_2(PQS_NODE_IDX i, PQS_TN t) {
      if (PQS_NODE_get_out_pred1(i) == t) {
	 return 1;
      } else if (PQS_NODE_get_out_pred2(i) == t) {
	 return 2;
      } 
      FmtAssert(0,("get_1_2: malformed idx %d\n",i));
      return 0;
   }

   // Get the other TN in an index
   inline PQS_TN PQS_NODE_get_other_tn(PQS_NODE_IDX i, PQS_TN t) {
      if (PQS_NODE_get_out_pred1(i) == t) {
	 return PQS_NODE_get_out_pred2(i);
      } else if (PQS_NODE_get_out_pred2(i) == t) {
	 return PQS_NODE_get_out_pred1(i);
      }
      FmtAssert(0,("get_other: malformed idx %d\n",i));
      return 0;
   }


   // Global functions
  
   void Print_all(FILE *f=stdout);
   void Print_idx(PQS_NODE_IDX idx, FILE *f=stdout);

   BOOL PQS_is_disjoint(PQS_TN tn1, PQS_TN tn2);
   BOOL PQS_is_disjoint(PQS_TN_SET &tns1, PQS_TN_SET &tns2);
   
   BOOL PQS_is_subset_of (PQS_TN tn1, PQS_TN tn2);
   BOOL PQS_is_subset_of (PQS_TN tn1, PQS_TN_SET &tns2);
   BOOL PQS_is_subset_of (PQS_TN_SET &tns1, PQS_TN_SET &tns2);

   PQS_NODE_IDX PQS_Add_Instruction (PQS_OP inst);

   // The dedicated P0 TN
   PQS_TN PQS_TN_P0;

};

#endif

// Local Variables:
// mode:C++
// End:
