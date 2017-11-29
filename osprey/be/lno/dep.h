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



//                      Dependence Vector List
//                     -----------------------
//
// Description:
//
//     A list of dependence vectors (see dvector.h in be/com for
//     dependence vectors).
//
//
// Exported types and functions:
//
//      DEPV_ARRAY
//
//		An array of DEPV.  Each DEPV must be the same size.
//		To save space this is implemented as a class with one
//		field for the the number of DEPV, one field for the
//		the number of dimensions and a one-element array for
//		the actual DEPVs.  In reality, this one-elment array
//		is actually larger than one element (it grows off the
//		bottom).  Therefore, the user should never call a constructor
//	        for DEPV_ARRAY.  Instead, Create_DEPV_ARRAY routines are
//		provided.
//
//	    mUINT8	Num_Vec() const
//
//		How many vectors in the array
//
//	    mUINT8	Num_Dim() const
//
//		How many dimensions in each DEPV
//
//	    mUINT8      Num_Unused_Dim() const
//
//		How many outer loops for which there is no component in
//		the vector
//
//	    void	Set_Num_Unused_Dim(mUINT8 unused_dim)
//
//		Set the number of unused dims to unused_dim
//
//	    DEPV	*Depv(mUINT8 i) const
//
//		Return vector 'i' (0 being the first)
//
//	    DEPV_ARRAY *Create_DEPV_ARRAY(mUINT8 num_vec, mUINT8 num_dim,
//				mUINT8 num_unused_dim,MEM_POOL *pool)
//
//		Create an array with num_vec vectors and num_dim dimensions.
//		You must use this routine to create a DEPV_ARRAY.  DO NOT
//		USE CXX_NEW (DEPV_ARRAY is implemented as a variable sized
//		structure, growing past the bottom).
//
//	    DEPV_ARRAY *Create_DEPV_ARRAY(const DEPV_ARRAY *orig,
//				MEM_POOL *pool)
//
//	    DEPV_ARRAY *Create_DEPV_ARRAY(DEPV_LIST *depv_list,MEM_POOL *pool)
//
//		Create an array to match the depv_list
//
//	   DEPV_ARRAY *Shorten(UINT num_dim, MEM_POOL *pool)
//
//		Chop the inner dimensions leaving num_dim dimensions
//
//	   DEP *Shorten_To_Dep(MEM_POOL *pool)
//
//		Convert the DEPV_ARRAY into a one-dimensional DEP.
//		For example, given (0,+),(0,=) return +=
//		Given (2,...) return NULL (since one-dimensional implies
//		that all the outer loop variables are not changing)
//
//	   DEPV *Union(MEM_POOL *pool)
//
//		Union all the DEPV in the array into a single DEPV
//
//	    void Print(FILE *fp) const
//
//		Print the array
//
//	    void Delete_DEPV_ARRAY(DEPV_ARRAY *array,MEM_POOL *pool)
//
//		Delete the array.
//
//	    INT Max_Level() const
//
//		What is the maximum level of the dependences in the array
//
//          BOOL Equal_Through_Depth (INT depth);
//              
//              Return true if all the dependences upto (and including) depth
//              are EQ, false otherwise.
//
//          BOOL One_Equal_Through_Depth (INT depth);
//              
//              Return true if at least one the dependences upto (and  
//              including) depth are EQ, false otherwise.
//
//         INT Loop_Carrying_Dependence() 
//
//	        Returns the depth of the loop carrying the dependence for
//              this DEPV_ARRAY, -1 if no loop carries the dependence. 
//	
//	   BOOL Is_Blockable(INT start_depth, INT stop_depth)
//
//		Returns TRUE if the dependence does not prevent the loops 
//		from depth 'start_depth' to 'stop_depth' inclusive from 
//		being blocked.  Returns FALSE otherwise. 
//
//      DEPV_LIST:public SLIST
//
//	  	A list of DEPV.  This is only used by the dependence computing
//		routines.  Inside the graph we store arrays of DEPV
//
//	   DEPV_LIST(mUINT8 dv_dim, mUINT8 num_unused_dim, MEM_POOL *pool)
//
//		Create a new, empty, list of dv_dim dimensional vectors
//		given that there are num_unused_dim outer, unused loops
//
//	    DEPV_LIST(DEPV_ARRAY *array, MEM_POOL *pool)
//
//		Copy a DEPV_ARRAY into a list
//
//
//	    DEPV_LIST(WN *ref1, WN *ref2,mUINT8 common_nest,mUINT8 dv_dim, 
//	       BOOL use_bounds,MEM_POOL *pool, const DOLOOP_STACK *s1,
//	       const DOLOOP_STACK *s2)
//
//		Create a new DEPV_LIST to represent the dependences from
//		reference 1 to reference 2.
//		They must be nested in common_nest (> 0)  common DO LOOPS
//		Compute dv_dim-dimensional dependence vectors
//		dv_dim must be <= common_nest
//		If dv_dim < the common DO loops, only compute dependence vectors
//		for the inner common dv_dim loops
//		    i.e. given
//		    do i
//		      do j
//		        do k
//		          ref1
//		        do k2
//		          ref2
//		    If dv_dim=1, only compute dependence vectors for loop j
//
//		If the references are independent, DEPV_LIST will be empty
//		Bounds information is used iff use_bounds = TRUE
//		s1 and s2 are stacks of all the surrounding do loop nodes
//		(outermost do loop in position 0)
//		
//
//	    ~DEPV_LIST()
//
// 	    void Append(DEPV *depv,INT num_bad_outer);
//
//		depv is a depv computed ignoring the "num_bad_outer"
//		dimensions (i.e. treating them as =).  
//		Put these dimensions in.  For example given (3) with
//		num_bad_outer = 1, append to the list (= 3), (+ *) and (- 3)
//
//	    void Remove_Duplicates()
//
//		Remove all duplicate depvs from the list
//
//	    void Normalize_Step(INT64 *step)
//
//		Divide each dep by the step size. Remove deps that
//	    aren't multiples of the step size.
//
//
// 	    void Print(FILE *fp) const
//		
//		Print out the list of vectors
//
//	    mUINT8 Num_Dim() const
//
//		How many dimensions in each DEPV on the list
//
//	    mUINT8      Num_Unused_Dim() const
//
//		How many outer loops for which there is no component in
//		the vector
//
//	    void Lex_Pos_Decompose(MEM_POOL *pool,DEPV_LIST *pos, 
//		DEPV_LIST *neg, BOOL keep_pos_equals,BOOL keep_neg_equals) 
//
//		Compute the lexicographically positive decomposition
//		of this and of -this.  Put lexpos(this) into pos.
//		Put lexpos(-this) into neg.
//	        The original list will not be changed.  All lists
//		must use the same pool.  If (0,...,0) is on the original
//		list it will be placed in pos iff keep_pos_equals==TRUE,
//		it will be place in neg iff keep_neg_equals==TRUE
//
//         void Blockable_Part(MEM_POOL *pool, DEPV_LIST* dl_block,
//              mUINT8 num_unused_dim, mUINT8 dv_dim, INT start_depth,
//              INT stop_depth)
//
//		Compute the portion of the dependences of 'this' which 
//		are blockable (i.e. fully permutable) from loop depths 
//		'start_depth' to 'stop_depth' inclusive, and append them 
//		to 'dl_block'.  We assume that the dependences have 
//		'num_unused_dim' number of unused dimensions and 
//		'dv_dim' used dimensions. 
//
//	   DEP Convert_To_Dep()
//
//		Given that each element on the list is one dimensional,
//		unite all the elements on the list into a single DEP
//
//	    extern DEPV_LIST *Lex_Pos_Compose(MEM_POOL *pool,
//			DEPV_LIST *pos,DEPV_LIST *neg)
//
//		Reverse Lex_Pos_Decompose.  This gets the union
//		of pos and the negation of neg.  This destroys pos
//		and neg
//
//	    BOOL Is_Inner_Single_Distance()
//
//		Is the inner dependence of every dep in the list the same
//		distance?  Returns FALSE if list is empty.
//
//	    BOOL Is_Inner_Single_Non_Zero_Distance()
//
//		Is the inner dependence of every dep in the list the same
//		non-zero distance?  Returns FALSE if list is empty.
//
//	    BOOL Is_Lexpos() const
//
//		Are all the components guranteed to be lexically positive
//
//	    BOOL Contains_All_Equals() const
//
//		Does this contain an all equals dependence
//
//	    void Eliminate_Inner_Carried()
//
//		Eliminate all dependences of the form (=,=,...,!=)
//		Note this doesn't get rid of (+=,=,...,!=)
//
//	    void Eliminate_Inner_Carried_Or_All_Equals()
//
//		Eliminate all dependences of the form (=,=,...,*)
//
//	    void Eliminate_Non_Distance_Carried_By(INT i)
//
//		Eliminate all non-constant distances carried by loop i.
//		
//
//	DEPV_NODE
//
//		One element of a DEPV_LIST
//
//	    DEPV *Depv
//
//		The dependence vector 
//
// 	    void Print(FILE *fp, mUINT8 num_dim) const
//
//		Print out the node.  num_dim is the number of dimensions
//		in the vector
//
//	    DEPV_NODE(DEPV *depv) 
//	    ~DEPV_NODE() 
//
//		Create and destroy a node
//
//	    void Lex_Pos_Decompose(MEM_POOL *pool,DEPV_LIST *pos, 
//		DEPV_LIST *neg, mUINT8 dv_dim, INT first_dim,
//		BOOL keep_pos_equals,BOOL keep_neg_equals) 
//
//		Compute the lexicographically positive decomposition
//		of this and of -this.  Put lexpos(this) into pos.
//		Put lexpos(-this) into neg.  It is assumed that
//		this[0..first_dim-1] = DIR_EQ.
//	        The original list will not be changed.  All lists
//		must use the same pool.
//
//         void Blockable_Part(MEM_POOL *pool, DEPV_LIST* dl_block,
//              mUINT8 num_unused_dim, mUINT8 dv_dim, INT start_depth,
//              INT stop_depth)
//
//		Compute the portion of the dependence of 'this' which 
//		is blockable (i.e. fully permutable) from loop depths 
//		'start_depth' to 'stop_depth' inclusive, and append them 
//		to 'dl_block'.  We assume that the dependences have 
//		'num_unused_dim' number of unused dimensions and 
//		'dv_dim' used dimensions. 
//
//	    BOOL Equal(DEPV_NODE *node2, mUINT8 num_dim)
//		Is this node equivalent to node2
//
//	DEPV_ITER
//
//		Iteratre through a list
//
//	static DEP_RESULT DEPV_COMPUT::Base_Test(const WN *wn1, 
//          ARA_REF *ara_ref1, const WN *wn2, ARA_REF *ara_ref2);
//
//		Are the two bases of the array load/stores DEP_INDEPENDENT, 
//	        DEP_DEPENDENT or DEP_CONTINUE
//
//


/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef dep_INCLUDED
#define dep_INCLUDED "dep.h"

#ifdef _KEEP_RCS_ID
static char *dep_rcs_id = dep_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef dvector_INCLUDED
#include "dvector.h"
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif


enum DEP_RESULT { DEP_INDEPENDENT, DEP_DEPENDENT, DEP_CONTINUE};
typedef STACK<const SYMBOL *> SYMBOL_STACK;
class REGION_UN;

class DEPV_NODE: public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS( DEPV_NODE);
public:
  DEPV *Depv;
  DEPV_NODE(DEPV *depv) { Depv = depv; };
  ~DEPV_NODE() { DEPV_Free(Default_Mem_Pool,Depv);};
  void Normalize_Step(INT dim, INT64 step, BOOL *is_indep);
  void Print(FILE *fp, mUINT8 num_dim) const;
  void Lex_Pos_Decompose(MEM_POOL *pool,class DEPV_LIST *pos, 
	class DEPV_LIST *neg,mUINT8 dv_dim, INT first_dim,
	BOOL keep_pos_equals, BOOL keep_neg_equals);
  void Blockable_Part(MEM_POOL *pool, class DEPV_LIST* dl_block, 
        mUINT8 num_unused_dim, mUINT8 dv_dim, INT start_depth, INT stop_depth);
  BOOL Equal(DEPV_NODE *node2, mUINT8 num_dim);
};

class ARA_REF;

// represent coupling in array summaries (_coeff from array_region.h)
class DEPV_COEFF
{
public:
  INT32 *_coeff;
  INT32 _first_var; // column position of first variable
  INT32 _num_dim;
  DEPV_COEFF(INT32 *coeff, INT32 first_var, INT32 num_dim) {
    _coeff = coeff;
    _first_var = first_var;
    _num_dim = num_dim;
  }
};


// Class used to compute DEPV_LIST
// We use this class as a convenient holding place for all the semi-global
// (global throughout the computation of a DEPV_LIST) variables
class DEPV_COMPUTE
{
  friend class DEPV_LIST;
  DEPV_COMPUTE(const DEPV_COMPUTE&);
  DEPV_COMPUTE& operator= (const DEPV_COMPUTE&);
private:
  MEM_POOL *_pool;
  DEPV_COMPUTE() {;}
  ~DEPV_COMPUTE() {;}
  INT64 *_step1;
  INT64 *_step2;
  void Set_Step(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2);
  void Print(FILE *fp) const;
  void Compute(DEPV_LIST *result,WN *ref1, ARA_REF *array_ref1,
	       WN *ref2, ARA_REF *array_ref2,
	       UINT8 common_nest,mUINT8 dv_dim, 
	       BOOL use_bounds, MEM_POOL *pool,
  	       const DOLOOP_STACK *s1, const DOLOOP_STACK *s2);
  BOOL Same_Permutation(WN *ref1, WN *ref2, ACCESS_ARRAY **perm1,
	ACCESS_ARRAY **perm2, WN *outer_loop);

  enum { MAX_ROWS=99, MAX_COLS=30};
  DEP_RESULT Trivial_Test(const ACCESS_ARRAY *a1, const ACCESS_ARRAY *a2,
			  mUINT8 dv_dim, mUINT16 common_nest,
			  BOOL *non_trivial_dim, BOOL *seen_non_trivial) const;
  BOOL Same_Monotonic(WN *array1_index, WN *array2_index, 
	const ACCESS_VECTOR *av1, const ACCESS_VECTOR *av2, 
	mUINT16 common_nest, mUINT16 dv_dim,
	const DOLOOP_STACK *s1) const;
  BOOL Same_Monotonic(WN *array1, WN *array2, 
	const ACCESS_ARRAY *a1, const ACCESS_ARRAY *a2, 
	mUINT16 common_nest, mUINT16 dv_dim,
	const DOLOOP_STACK *s1) const;
  const WN *Find_First_Ldid_For_Symbol(const WN *wn,const SYMBOL *symb)const;
  BOOL Simple_Gcd_Indep(ACCESS_ARRAY *a1, ACCESS_ARRAY *a2, 
					INT common_nest, INT dv_dim) const;
  BOOL Simple_Gcd_Indep(ACCESS_VECTOR *av1, ACCESS_VECTOR *av2) const;
  mUINT16 Common_Nest(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2) const;
  static BOOL Equiv_Dims(const WN *array1, const WN *array2);
  static BOOL Equiv_Dim(const WN *exp1, const WN *exp2) ;
  static BOOL Equiv_Dims(const WN* wn_array, ARA_REF* ara_ref);
  static BOOL Equiv_Dims(ARA_REF* ara_ref, const WN* wn_array);
  static BOOL Equiv_Dims(ARA_REF* ara_ref1, ARA_REF* ara_ref2);

  static mINT32 _work_eq[MAX_ROWS][MAX_COLS];
  static mINT32 _work_le[MAX_ROWS][MAX_COLS];
  static mINT32 _tmp[MAX_COLS];
  static INT64 _work_eq_const[MAX_ROWS];
  static INT64 _work_le_const[MAX_ROWS];
  INT _work_eq_rows;
  INT _work_le_rows;
  INT _work_cols;
  BOOL Copy_Equals_To_Work(const ACCESS_ARRAY *a1, const ACCESS_ARRAY *a2,
        SYMBOL_STACK *symbol_stack, const BOOL *non_trivial_dim);
  BOOL Copy_Equal_To_Work(ACCESS_VECTOR *av1, ACCESS_VECTOR *av2,
        			SYMBOL_STACK *symbol_stack);
  BOOL Copy_Equal_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff,
        		SYMBOL_STACK *symbol_stack,BOOL is_first_ref);
  BOOL Copy_Le_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff,
        	SYMBOL_STACK *symbol_stack, BOOL is_first_ref, BOOL negate_av);
  BOOL Create_Dummy_Vars(INT number, SYMBOL_STACK *symbol_stack, INT& result);
  BOOL Copy_Stride_To_Work(ACCESS_VECTOR *av, DEPV_COEFF *coeff,
		INT stride, SYMBOL_STACK *symbol_stack, BOOL is_first_ref);
  BOOL Copy_Call_Ref_To_Work(ACCESS_ARRAY *aa, DEPV_COEFF *coeff,
			SYMBOL_STACK *symbol_stack, BOOL is_first_ref);
  BOOL Copy_Call_To_Work(REGION_UN &image1, SYMBOL_STACK *symbol_stack,
  			DEPV_COEFF *coeff, BOOL is_first_ref);
  BOOL Copy_Bounds_To_Work(const DOLOOP_STACK *s1, const DOLOOP_STACK *s2,
        SYMBOL_STACK *symbol_stack);
  BOOL Copy_Bound_To_Work(INT var_num,ACCESS_VECTOR *bound,
        SYMBOL_STACK *symbol_stack,BOOL is_ref1);
  DEP_RESULT Find_Init_Distance_Used(DEPV *init_distance, BOOL *is_used, 
					INT dv_dim) const;
  void Bounds_Set_Is_Used(BOOL *is_used, BOOL *bounds_used, 
		INT *num_bounds_used) const;
  void Set_Map_Used(const BOOL *is_used,INT *num_used,INT *map_used) const;
  void Copy_To_Soe(const BOOL *is_used, const BOOL *bounds_used,
		   const INT *map_used, class SYSTEM_OF_EQUATIONS *soe) const;
  void Compute_Dep_Vectors(class SYSTEM_OF_EQUATIONS *soe, const INT *is_used,
     const INT *map_used,DEPV *input_dependence,
     DEPV_LIST *result, BOOL same_references, INT dv_dim,INT num_bad_outer) ;
  INT First_Star(const DEPV *vector, const INT *is_used) const;
  void Add_Direction(class SYSTEM_OF_EQUATIONS *soe, INT i, 
		 const INT *map_used, DIRECTION direction);

  INT _nd1,_nd2;  // How deep is each loop nested
  // mapping of column positions
  INT _first_dv1;
  INT _first_non_com1;
  INT _first_dv2;
  INT _first_non_com2;
  INT _first_symbol;
  void Print_Work(FILE *fp);
public:
  static DEP_RESULT Base_Test(const WN *wn1, ARA_REF *ara_ref1,
				const WN *wn2,ARA_REF *ara_ref2);
  static WN * Find_Def(WN *wn1);

};


class DEPV_LIST: public SLIST {
  DECLARE_SLIST_CLASS( DEPV_LIST, DEPV_NODE )
  mUINT8 _dv_dim;
  mUINT8 _num_unused_dim;
  MEM_POOL *_pool;

public:
  void Print(FILE *fp) const;
  DEPV_LIST(class DEPV_ARRAY *array, MEM_POOL *pool);
  DEPV_LIST(WN *ref1, WN *ref2, mUINT8 common_nest, mUINT8 dv_dim,
	    BOOL use_bounds, MEM_POOL *pool, const DOLOOP_STACK *s1,
	    const DOLOOP_STACK *s2); 
  DEPV_LIST(mUINT8 dv_dim, mUINT8 num_unused_dim, MEM_POOL *pool) { 
    _dv_dim = dv_dim; 
    _num_unused_dim = num_unused_dim;
    _pool = pool;
  }
  void Append(DEPV *depv,INT num_bad_outer);
  void Normalize_Step(INT64 *step);
  BOOL Is_Lexpos() const;
  BOOL Contains_All_Equals() const;
  mUINT8 Num_Dim() const { return _dv_dim; }
  mUINT8 Num_Unused_Dim() const { return _num_unused_dim; }
  void Remove_Unused_Dim(INT count) {_num_unused_dim -= count; } 
  void Lex_Pos_Decompose(MEM_POOL *pool,DEPV_LIST *pos, DEPV_LIST *neg,
	BOOL keep_pos_equals, BOOL keep_neg_equals);
  void Blockable_Part(MEM_POOL *pool, class DEPV_LIST* dl_block, 
        mUINT8 num_unused_dim, mUINT8 dv_dim, INT start_depth, INT stop_depth);
  DEP Convert_To_Dep();
  BOOL Is_Inner_Single_Distance();
  BOOL Is_Inner_Non_Zero_Single_Distance();
  void Eliminate_Inner_Carried();
  void Eliminate_Inner_Carried_Or_All_Equals();
  void Eliminate_Non_Distance_Carried_By(INT i);
   void Remove_Duplicates();
  ~DEPV_LIST();
};



class DEPV_ITER: public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( DEPV_ITER, DEPV_NODE ,DEPV_LIST)
public:
  ~DEPV_ITER() {}
};

class DEPV_CONST_ITER: public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS(DEPV_CONST_ITER, DEPV_NODE ,DEPV_LIST)
public:
  ~DEPV_CONST_ITER() {}
};

class DEPV_ARRAY {
private:
  mUINT8 _num_vec; 
  mUINT8 _dim; // bits 0..3 used for used dims, others for unused
  DEPV _data[1];  // in reality this will be larger
  DEPV_ARRAY() { FmtAssert(0,("Constructor of DEPV_ARRAY called")); }
  friend DEPV_ARRAY *Create_DEPV_ARRAY(mUINT8 num_vec,mUINT8 num_dim,
					mUINT8 num_unused_dim,MEM_POOL*);
  friend DEPV_ARRAY *Create_DEPV_ARRAY(DEPV_LIST *depv_list, MEM_POOL *pool);
  friend void Delete_DEPV_ARRAY(DEPV_ARRAY *array, MEM_POOL *pool);
public:
  mUINT8 Num_Vec() const { return _num_vec; }
  mUINT8 Num_Dim() const { return _dim&15; }
  mUINT8 Num_Unused_Dim() const { return (_dim&240) >> 4; }
  void	Set_Num_Unused_Dim(mUINT8 unused_dim) { _dim = (unused_dim << 4) | Num_Dim(); };
  void Remove_Unused_Dim(INT count) {_dim -= (count << 4); } 
  DEPV_ARRAY *Shorten(UINT num_dim, MEM_POOL *pool);
  DEP *Shorten_To_Dep(MEM_POOL *pool);
  DEPV *Union(MEM_POOL *pool);
  DEPV *Depv(mUINT8 i) { return &_data[i*Num_Dim()]; }
  const DEPV *Depv(mUINT8 i) const { return &_data[i*Num_Dim()]; }
  void Print(FILE *fp) const;
  DEPV_ARRAY(const DEPV_ARRAY&);
  DEPV_ARRAY& operator= (const DEPV_ARRAY&);
  INT Max_Level() const;
  BOOL Equal_Through_Depth (INT depth);
  BOOL One_Equal_Through_Depth (INT depth);
  INT Loop_Carrying_Dependence(); 
  BOOL Is_Blockable(INT start_depth, INT stop_depth); 
};

extern DEPV_ARRAY *Create_DEPV_ARRAY(mUINT8 num_vec, 
			mUINT8 num_dim, mUINT8 nun_unused_dim,MEM_POOL *pool);
extern DEPV_ARRAY *Create_DEPV_ARRAY(DEPV_LIST *depv_list,MEM_POOL *pool);
extern DEPV_ARRAY *Create_DEPV_ARRAY(const DEPV_ARRAY *,MEM_POOL *);
extern void Delete_DEPV_ARRAY(DEPV_ARRAY *array, MEM_POOL *pool);
extern DEPV_LIST *Lex_Pos_Compose(MEM_POOL *pool,DEPV_LIST *pos,DEPV_LIST *neg);
   


#endif  // dep_INCLUDED
