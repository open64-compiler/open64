/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vn_expr_taxonomy.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vn_expr_taxonomy.h,v $
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
// INCLUSION DEPENDENCIES:
//
//   Before including this file, you must also include the following
//   files directly or indirectly:
//
//      common/com/defs.h
//      common/util/errors.h
//      common/com/targ_const.h
//
//
// INTERFACE DESCRIPTION:
//
//   This interface should be hidden to all, except to those source files
//   that implement the VN_EXPR class, such as opt_vn_expr.cxx.  This
//   class defines subclasses of VN_EXPR.
//
//   The reason we separated out these declarations from opt_vn_expr.cxx,
//   is such that we can split the opt_vn_expr.cxx file into several
//   source files should the implementation grow too complex to warrant
//   keeping it all in one source file.
//
//
// IMPLEMENTATION DETAILS:
//
//   We maintain a free list for each KIND of VN_EXPR, where the free-list
//   is a static member of the subclass corresponding to the KIND.  This
//   free-list is consulted when new VN_EXPR objects are created.
//
//   We perform value numbering by means of hashing, and to allow two
//   expressions that are equal due to algebraic rules such as 
//   commutativity to hash to the same value number, we perform simple
//   canonicalization (canonicalize).  Canonicalization changes a VN_EXPR
//   in-place, and the VN_EXPR::KIND cannot not change due to 
//   canonicalization.
//
//   We enable more value numbering matches between VN_EXPRs by means
//   means of simplification (simplify()).  Simplification is more
//   aggressive than canonicalization, and it may change the VN_EXPR::KIND.
//   When the KIND changes, a new VN_EXPR::PTR is returned, and the old
//   one can be free()'ed by the client.  Because of this and to allow
//   value numbering without aggressive simplification, we leave it to
//   the client to simplify() a created VN_EXPR.
//
//   Hence, canonicalization happens as a side-effect of calls to the
//   hash() method, while simplification should be done explicitly
//   by the client of VN_EXPR as the first thing after a 
//   Create_<Kind> call.
//
// POSSIBLE IMPROVEMENTS (TODO):
//
//   More simplification!
//
// CONSTRAINTS:
//
//   Same as for opt_vn_Expr.h.
//
// SEE ALSO:
//
//   Same as for opt_vn_Expr.h.
//
// ====================================================================
// ====================================================================


#ifndef opt_vn_expr_taxonomy_INCLUDED
#define opt_vn_expr_taxonomy_INCLUDED "opt_vn_expr_taxonomy.h"

#include "opt_vn_expr.h"
#include "intrn_info.h"
#include "wutil.h"

#ifdef __STL_USE_NAMESPACES
using std::pair;
#endif

typedef pair<VN_VALNUM,VN_VALNUM> VN_VALNUM_PAIR;


// The classes defined in this file.
//
class VN_LITERAL_EXPR;
class VN_UNARY_EXPR;
class VN_BINARY_EXPR;
class VN_TERNARY_EXPR;
class VN_INTR_OP_EXPR;
class VN_PHI_EXPR;
class VN_LDA_ADDR_EXPR;
class VN_ARRAY_ADDR_EXPR;
class VN_MEMLOC_EXPR;

inline void
Switch_Vn_Opnd(VN_VALNUM &vn1, VN_VALNUM &vn2)
{
   VN_VALNUM tmp = vn1;
   vn1 = vn2;
   vn2 = tmp;
}

//------------------ Handling of free-lists -------------------
//--------------------------------------------------------------

class FREE_STACK
{
private:

   typedef SEGMENTED_ARRAY<VN_EXPR::PTR, VN_BUFFER_CHUNKSIZE> ARRAY_OF_EXPR;

   ARRAY_OF_EXPR _stack;
   INT32         _top;
   MEM_POOL     *_mpool;

public:

   FREE_STACK(MEM_POOL *mpool = Malloc_Mem_Pool): 
      _stack(mpool), _top(-1), _mpool(mpool) {}
   
   ~FREE_STACK()
   {
      for (INT32 i = 0; i <= _top; i++)
      {
	 // Assumes expressions are allocated from same mpool as free-list
	 //
	 CXX_DELETE(_stack[i], _mpool);
      }
      _stack.Delete_last(_stack.Size());
      _top = -1;
   }
   
   void push(VN_EXPR::PTR expr)
   {
      UINT new_top = _top + 1;
      if (new_top == _stack.Size())
	 _stack.New_entry(new_top) = expr;
      else
	 _stack[new_top] = expr;
      _top = new_top;
   }
   
   VN_EXPR::PTR pop()
   {
      VN_EXPR::PTR expr;
      
      if (_top >= 0)
      {
	 expr = _stack[_top];
	 _top -= 1;
      }
      else 
	 expr = NULL;
      return expr;
   }
}; // FREE_STACK


//--------------- Interface for VN_LITERAL_EXPR ----------------
//--------------------------------------------------------------

class VN_LITERAL_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   // If the liternal is a vector, the <_vect_ty> indicates the vector 
   // type, and the _tcon hold the elements value. All elements should
   // hold same value.
   // 
   // If the liternal is a scalar, _vect_ty should be set to MTYPE_UNKNOWN.
   //
   TCON _tcon;
   TYPE_ID _vect_ty;

   VN_LITERAL_EXPR(const TCON &tcon) : _tcon(tcon) {} 
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}
  
   static VN_LITERAL_EXPR *Create(const TCON &tcon, TYPE_ID vect_ty) 
   {
      VN_LITERAL_EXPR *expr = (VN_LITERAL_EXPR *)_Free->pop();
      if (expr == NULL)
	 expr = CXX_NEW(VN_LITERAL_EXPR(tcon), _Mpool);
      else
	 expr->_tcon = tcon;

      expr->_vect_ty = vect_ty;
      Is_True (vect_ty == MTYPE_UNKNOWN || MTYPE_is_vector (vect_ty),
               ("vect_ty is invalid"));

      return expr;
   }

   BOOL is_const_vect (void) const {
     if (_vect_ty != MTYPE_UNKNOWN) {
       Is_True (MTYPE_is_vector (_vect_ty), ("isn't vector type"));
       return TRUE;
     }
     return FALSE;
   }

   TYPE_ID get_vect_type (void) const { return _vect_ty; }

   void free() 
   {
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return LITERAL;
   }

   UINT32 get_num_opnds() const 
   {
      return 0;
   }

   BOOL has_top_opnd() const 
   {
      return FALSE;
   }

   BOOL has_bottom_opnd() const 
   {
      return FALSE;
   }

   const TCON &get_tcon() const 
   {
      return _tcon;
   }
   
   PTR simplify(VN *)
   {
      return this; // A literal is as simple as it can be!
   }
   
   size_t hash()
   {
      INT64 hval;  // Use value bits, without regard for type, for ints!
      if (!Targ_Is_Integral(_tcon, &hval))
	 hval = Hash_TCON(&_tcon, INT32_MAX);
      return hval;
   }
   
   BOOL is_equal_to(CONST_PTR expr) const;

   // Gee, why make dump function inlinable?
   //
   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%s_const", 
              _vect_ty == MTYPE_UNKNOWN ? 
              MTYPE_name(TCON_ty(_tcon)) : MTYPE_name(_vect_ty));
      fprintf(fp, "(%s)", Targ_Print(NULL, _tcon)); // Using default formatting
   }

}; // VN_LITERAL_EXPR


//---------------- Interface for VN_UNARY_EXPR -----------------
//--------------------------------------------------------------

class VN_UNARY_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   OPCODE    _opc;
   VN_VALNUM _vn;

   VN_UNARY_EXPR(OPCODE opc, const VN_VALNUM &vn):
      _opc(opc), _vn(vn) {}
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_UNARY_EXPR *Create(OPCODE opc, const VN_VALNUM &vn);
  
   void free() 
   {
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return UNARY;
   }

   UINT32 get_num_opnds() const 
   {
      return 1;
   }

   BOOL has_top_opnd() const 
   {
      return _vn.is_top();
   }

   BOOL has_bottom_opnd() const 
   {
      return _vn.is_bottom();
   }

   VN_VALNUM get_opnd(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_UNARY_EXPR::opnd access"));
      return _vn;
   }

   OPCODE get_opc() const 
   {
      return _opc;
   }

   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_UNARY_EXPR::opnd access"));
      _vn = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      OPERATOR opr = OPCODE_operator(_opc);
      Is_True(opr != OPR_PARM && !has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      return _vn.ordinal() + (_opc << 4);
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      return (expr != NULL              &&
	      expr->get_kind() == UNARY && 
	      expr->get_opc() == _opc   &&
	      expr->get_opnd(0) == _vn);
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%s(", OPCODE_name(_opc));
      _vn.print(fp);
      fputs(")", fp);
   }
   
}; // VN_UNARY_EXPR


//---------------- Interface for VN_BINARY_EXPR -----------------
//--------------------------------------------------------------

class VN_BINARY_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   OPCODE    _opc;
   VN_VALNUM _vn[2];

   void _canonicalize();
   PTR _fold_2literals(OPERATOR  opr1, // ADD or SUB
		       CONST_PTR literal1,
		       OPERATOR  opr2, // ADD or SUB
		       CONST_PTR literal2);
   PTR  _simplify_2literals(OPERATOR  opr1, // ADD or SUB
			    CONST_PTR literal1,
			    OPERATOR  opr2, // ADD or SUB
			    CONST_PTR literal2,
			    OPERATOR  opr3, // ADD or SUB
			    VN_VALNUM vn3,
			    VN       *v);
   PTR  _simplify_3adds(OPERATOR  opr0, // ADD or SUB
			VN_VALNUM vn0,
			OPERATOR  opr1, // ADD or SUB
			VN_VALNUM vn1,
			OPERATOR  opr2, // ADD or SUB
			VN_VALNUM vn2,
			VN       *v);
   PTR _simplify_4adds(OPERATOR  opr0, // ADD or SUB
		       VN_VALNUM vn0,
		       OPERATOR  opr1, // ADD or SUB
		       VN_VALNUM vn1,
		       OPERATOR  opr2, // ADD or SUB
		       VN_VALNUM vn2,
		       OPERATOR  opr3, // ADD or SUB
		       VN_VALNUM vn3,
		       VN       *v);
   PTR  _simplify_add(CONST_PTR opnd1, CONST_PTR opnd2, VN *v);
   PTR  _simplify_sub(CONST_PTR opnd1, CONST_PTR opnd2, VN *v);
      
   VN_BINARY_EXPR(OPCODE opc, const VN_VALNUM &vn1, const VN_VALNUM &vn2):
      _opc(opc) 
   {
      _vn[0] = vn1; _vn[1] = vn2;
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_BINARY_EXPR *Create(OPCODE           opc, 
				 const VN_VALNUM &vn1, 
				 const VN_VALNUM &vn2);
  
   void free() 
   {
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return BINARY;
   }

   UINT32 get_num_opnds() const 
   {
      return 2;
   }

   BOOL has_top_opnd() const 
   {
      return _vn[0].is_top() || _vn[1].is_top();
   }

   BOOL has_bottom_opnd() const 
   {
      return _vn[0].is_bottom() || _vn[1].is_bottom();
   }

   VN_VALNUM get_opnd(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_BINARY_EXPR::opnd access"));
      return _vn[i];
   }

   OPCODE get_opc() const 
   {
      return _opc;
   }

   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_BINARY_EXPR::opnd access"));
      _vn[i] = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      _canonicalize();
      return _opc + (_vn[0].ordinal() << 4) + (_vn[1].ordinal() << 8);
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      return (expr != NULL                &&
	      expr->get_kind() == BINARY  &&
	      expr->get_opc() == _opc     && 
	      expr->get_opnd(0) == _vn[0] &&
	      expr->get_opnd(1) == _vn[1]);
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%s(", OPCODE_name(_opc));
      _vn[0].print(fp);
      fputs(", ", fp);
      _vn[1].print(fp);
      fputs(")", fp);
   }
   
}; // VN_BINARY_EXPR


//---------------- Interface for VN_TERTIARY_EXPR -----------------
//--------------------------------------------------------------

class VN_TERNARY_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   OPCODE    _opc;
   VN_VALNUM _vn[3];
   
   VN_TERNARY_EXPR(OPCODE           opc, 
		   const VN_VALNUM &vn1, 
		   const VN_VALNUM &vn2, 
		   const VN_VALNUM &vn3):
      _opc(opc)
   {
      _vn[0] = vn1; _vn[1] = vn2; _vn[2] = vn3;
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_TERNARY_EXPR *Create(OPCODE           opc, 
				  const VN_VALNUM &vn1, 
				  const VN_VALNUM &vn2, 
				  const VN_VALNUM &vn3);
  
   void free() 
   {
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return TERNARY;
   }

   UINT32 get_num_opnds() const 
   {
      return 3;
   }

   VN_VALNUM get_opnd(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_TERNARY_EXPR::opnd access"));
      return _vn[i];
   }

   BOOL has_top_opnd() const 
   {
      return _vn[0].is_top() || _vn[1].is_top() || _vn[2].is_top();
   }

   BOOL has_bottom_opnd() const 
   {
      return _vn[0].is_bottom() || _vn[1].is_bottom() || _vn[2].is_bottom();
   }

   OPCODE get_opc() const 
   {
      return _opc;
   }

   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_TERNARY_EXPR::opnd access"));
      _vn[i] = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      return 
	 _opc + 
	 (_vn[0].ordinal() << 4) + 
	 (_vn[1].ordinal() << 6) + 
	 (_vn[2].ordinal() << 8);
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      return (expr != NULL                &&
	      expr->get_kind() == TERNARY &&
	      expr->get_opc() == _opc     &&
	      expr->get_opnd(0) == _vn[0] &&
	      expr->get_opnd(1) == _vn[1] &&
	      expr->get_opnd(2) == _vn[2]);
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%s(", OPCODE_name(_opc));
      _vn[0].print(fp);
      fputs(", ", fp);
      _vn[1].print(fp);
      fputs(", ", fp);
      _vn[2].print(fp);
      fputs(")", fp);
   }
   
}; // VN_TERNARY_EXPR


//-------------- Interface for VN_INTR_OP_EXPR -----------------
//--------------------------------------------------------------

class VN_INTR_OP_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   INTRINSIC       _intr_opc;
   UINT32          _num_opnds;
   VN_VALNUM_PAIR  _opnd[3];    // (first == PARAM, second == MU)!
   VN_VALNUM_PAIR *_opnd_array; // For larger numbers of arguments
   
   VN_INTR_OP_EXPR(INTRINSIC intr_opc, 
		   UINT32    num_opnds):
      _intr_opc(intr_opc), _num_opnds(num_opnds), _opnd_array(NULL)
   {
      if (num_opnds > 3)
	 _opnd_array = CXX_NEW_ARRAY(VN_VALNUM_PAIR, num_opnds, _Mpool);
      for (INT i = 0; i < num_opnds; i++)
      {
	 set_opnd(i, VN_VALNUM::Bottom());
	 set_opnd_vsym(i, VN_VALNUM::Bottom());
      }
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_INTR_OP_EXPR *Create(INTRINSIC intr_opc, 
				  UINT32    num_opnds);
  
   void free() 
   {
      if (_opnd_array != NULL)
      {
	 CXX_DELETE_ARRAY(_opnd_array, _Mpool);
	 _opnd_array = NULL;
      }
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return INTR_OP;
   }

   UINT32 get_num_opnds() const 
   {
      return _num_opnds;
   }

   VN_VALNUM get_opnd(UINT i) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_INTR_OP_EXPR::opnd access"));
      return (_opnd_array == NULL? 
	      _opnd[i].first : _opnd_array[i].first);
   }

   VN_VALNUM get_vsym(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_INTR_OP_EXPR::vsym access"));
      return (_opnd_array == NULL? 
	      _opnd[i].second : _opnd_array[i].second);
   }

   BOOL has_top_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Top()) i++;
      return (i < get_num_opnds());
   }

   BOOL has_bottom_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Bottom()) i++;
      return (i < get_num_opnds());
   }

   INTRINSIC get_intr_opc() const 
   {
      return _intr_opc;
   }

   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_INTR_OP_EXPR::opnd access"));
      if (_opnd_array == NULL)
	 _opnd[i].first = vn;
      else
	 _opnd_array[i].first = vn;
   }

   void set_opnd_vsym(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_INTR_OP_EXPR::vsym access"));
      if (_opnd_array == NULL)
	 _opnd[i].second = vn;
      else
	 _opnd_array[i].second = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      size_t hashval = _intr_opc;
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 hashval += (get_opnd(i).ordinal() << 4);
	 hashval += (get_vsym(i).ordinal() << 4);
      }
      return hashval;
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      BOOL truth = (expr != NULL                        &&
		    expr->get_kind() == INTR_OP         &&
		    expr->get_num_opnds() == _num_opnds &&
		    expr->get_intr_opc() == _intr_opc);

      for (INT i = 0; truth && i < get_num_opnds(); i++)
	 truth = (expr->get_opnd(i) == get_opnd(i) &&
		  expr->get_vsym(i) == get_vsym(i));
      return truth;
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%s(", INTRINSIC_name(_intr_opc));
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 if (i > 0)
	    fputs(", ", fp);
	 fputs("<", fp);
	 get_opnd(i).print(fp);
	 fputs(",", fp);
	 get_vsym(i).print(fp);
	 fputs(">", fp);
      }
      fputs(")", fp);
   }
   
}; // VN_INTR_OP_EXPR


//---------------- Interface for VN_PHI_EXPR -----------------
//--------------------------------------------------------------

class VN_PHI_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   IDTYPE     _block_id;
   PHI_TAG    _phi_tag;
   UINT32     _num_opnds;
   VN_VALNUM  _vn[3];    // The common case!
   VN_VALNUM *_vn_array; // For larger numbers of arguments
   
   VN_PHI_EXPR(UINT32 num_opnds, IDTYPE block_id, PHI_TAG phi_tag):
      _num_opnds(num_opnds), 
      _block_id(block_id), 
      _phi_tag(phi_tag),
      _vn_array(NULL)
   {
      if (num_opnds > 3)
	 _vn_array = CXX_NEW_ARRAY(VN_VALNUM, num_opnds, _Mpool);
      for (INT i = 0; i < num_opnds; i++) set_opnd(i, VN_VALNUM::Bottom());
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_PHI_EXPR *Create(UINT32  num_opnds,
			      IDTYPE  block_id,
			      PHI_TAG phi_tag);
  
   void free() 
   {
      if (_vn_array != NULL)
      {
	 CXX_DELETE_ARRAY(_vn_array, _Mpool);
	 _vn_array = NULL;
      }
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return PHI;
   }

   UINT32 get_num_opnds() const 
   {
      return _num_opnds;
   }

   VN_VALNUM get_opnd(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_PHI_EXPR::opnd access"));
      return (_vn_array == NULL? _vn[i] : _vn_array[i]);
   }

   BOOL has_top_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Top()) i++;
      return (i < get_num_opnds());
   }

   BOOL has_bottom_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Bottom()) i++;
      return (i < get_num_opnds());
   }

   PHI_TAG get_phi_tag() const 
   {
      return _phi_tag;
   }

   IDTYPE get_block_id() const 
   {
      return _block_id;
   }
   
   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_PHI_EXPR::opnd access"));
      if (_vn_array == NULL)
	 _vn[i] = vn;
      else
	 _vn_array[i] = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      size_t hashval = _block_id;
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      for (INT i = 0; i < get_num_opnds(); i++)
	 hashval += (get_opnd(i).ordinal() << 4);
      return hashval;
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      BOOL truth = (expr != NULL                        &&
		    expr->get_kind() == PHI             &&
		    expr->get_num_opnds() == _num_opnds &&
		    expr->get_block_id() == _block_id   &&
		    expr->get_phi_tag() == _phi_tag);

      for (INT i = 0; truth && i < get_num_opnds(); i++)
	 truth = (expr->get_opnd(i) == get_opnd(i));
      return truth;
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "Phi[blck=%d](", (INT32)_block_id);
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 if (i > 0)
	    fputs(", ", fp);
	 get_opnd(i).print(fp);
      }
      fputs(")", fp);
   }
   
}; // VN_PHI_EXPR
   

//--------------- Interface for VN_LDA_ADDR_EXPR ---------------
//--------------------------------------------------------------

class VN_LDA_ADDR_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   INT32 _lda_cr_id;
   
   VN_LDA_ADDR_EXPR(INT32 lda_cr_id): _lda_cr_id(lda_cr_id) {}
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_LDA_ADDR_EXPR *Create(INT32 lda_cr_id);
  
   void   free()                              {_Free->push(this);}
   KIND   get_kind()                    const {return LDA_ADDR;}
   UINT32 get_num_opnds()               const {return 0;}
   BOOL   has_top_opnd()                const {return FALSE;}
   BOOL   has_bottom_opnd()             const {return FALSE;}
   IDTYPE get_lda_cr_id_id()            const {return _lda_cr_id;}
   PTR    simplify(VN *)                      {return this;}
   size_t hash()                              {return (size_t)_lda_cr_id;}
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      return (expr != NULL                        &&
	      expr->get_kind() == LDA_ADDR        &&
	      expr->get_lda_cr_id() == _lda_cr_id);
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "LDA cr_id(%d)", (INT32)_lda_cr_id);
   }
   
}; // VN_LDA_ADDR_EXPR


//-------------- Interface for VN_ARRAY_ADDR_EXPR --------------
//--------------------------------------------------------------

class VN_ARRAY_ADDR_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   WN_ESIZE   _esize;     // Array element size
   INT32      _num_opnds; // 1 + 2*Number of array dimensions
   VN_VALNUM  _vn[5];     // The common case, for up to 2 dimensional arrays!
   VN_VALNUM *_vn_array;  // For larger numbers of arguments
   
   VN_ARRAY_ADDR_EXPR(WN_ESIZE esize, INT32 num_dims):
      _esize(esize)
   {
      _num_opnds = 1 + 2*num_dims;
      if (_num_opnds > 5)
	 _vn_array = CXX_NEW_ARRAY(VN_VALNUM, _num_opnds, _Mpool);
      else
	 _vn_array = NULL;

      for (INT i = 0; i < _num_opnds; i++) set_opnd(i, VN_VALNUM::Bottom());
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_ARRAY_ADDR_EXPR *Create(WN_ESIZE esize, INT32 num_dims);
  
   void free() 
   {
      if (_vn_array != NULL)
      {
	 CXX_DELETE_ARRAY(_vn_array, _Mpool);
	 _vn_array = NULL;
      }
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return ARRAY_ADDR;
   }

   UINT32 get_num_opnds() const 
   {
      return _num_opnds;
   }

   VN_VALNUM get_opnd(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), 
	      ("Erroneous VN_ARRAY_ADDR_EXPR::opnd access"));
      return (_vn_array == NULL? _vn[i] : _vn_array[i]);
   }

   BOOL has_top_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Top()) i++;
      return (i < get_num_opnds());
   }

   BOOL has_bottom_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Bottom()) i++;
      return (i < get_num_opnds());
   }

   INT32 get_num_dims() const
   {
      return (_num_opnds - 1)/2;
   }
   
   WN_ESIZE get_esize() const
   {
      return _esize;
   }
   
   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), 
	      ("Erroneous VN_ARRAY_ADDR_EXPR::opnd access"));
      if (_vn_array == NULL)
	 _vn[i] = vn;
      else
	 _vn_array[i] = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      size_t hashval = get_opnd(0).ordinal(); // Value number for base address
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      for (INT i = 1; i < get_num_opnds(); i++)
	 hashval += (get_opnd(i).ordinal() << 4);
      return hashval;
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      BOOL truth = (expr != NULL                    &&
		    expr->get_kind() == ARRAY_ADDR  &&
		    expr->get_esize() == _esize     &&
		    expr->get_num_opnds() == _num_opnds);

      for (INT i = 0; truth && i < get_num_opnds(); i++)
	 truth = (expr->get_opnd(i) == get_opnd(i));
      return truth;
   }
   
   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "ARRAY[esize=%d](", (INT32)_esize);
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 if (i > 0)
	    fputs(",", fp);
	 get_opnd(i).print(fp);
      }
      fputs(")", fp);
   }

}; // VN_ARRAY_ADDR_EXPR


//-------------- Interface for VN_MEMLOC_EXPR --------------
//--------------------------------------------------------------

class VN_MEMLOC_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   MTYPE      _dsctype : 8;
   VN_VALNUM  _bytesize;
   VN_VALNUM  _offset;
   VN_VALNUM  _base_addr;   // Address = _base_addr + _offset
   VN_VALNUM  _vsym_valnum; // Really the coderep_id for the virtual var
   
   VN_MEMLOC_EXPR(MTYPE            dsctype,
		  const VN_VALNUM &bytesize,
		  const VN_VALNUM &offset,
		  const VN_VALNUM &base_addr,
		  const VN_VALNUM &vsym_valnum):
      _dsctype(dsctype),
      _bytesize(bytesize), 
      _offset(offset), 
      _base_addr(base_addr), 
      _vsym_valnum(vsym_valnum)
   {}

   void _canonicalize()
   {
      if (_base_addr < _offset)
	 Switch_Vn_Opnd(_base_addr, _offset); // Apply commutativity
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_MEMLOC_EXPR *Create(MTYPE            dsctype, 
				 const VN_VALNUM &bytesize,
				 const VN_VALNUM &offset,
				 const VN_VALNUM &base_addr,
				 const VN_VALNUM &vsym_valnum);
  
   void free() 
   {
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return MEMLOC;
   }

   UINT32 get_num_opnds() const 
   {
      return 0;
   }

   BOOL has_top_opnd() const 
   {
      return (_bytesize == VN_VALNUM::Top()  ||
	      _offset == VN_VALNUM::Top()    ||
	      _base_addr == VN_VALNUM::Top() ||
	      _vsym_valnum == VN_VALNUM::Top());
   }

   BOOL has_bottom_opnd() const 
   {
      return (_bytesize == VN_VALNUM::Bottom()  ||
	      _offset == VN_VALNUM::Bottom()    ||
	      _base_addr == VN_VALNUM::Bottom() ||
	      _vsym_valnum == VN_VALNUM::Bottom());
   }

   MTYPE get_dsctype() const
   {
      return _dsctype;
   }
   
   VN_VALNUM get_bytesize() const
   {
      return _bytesize;
   }

   VN_VALNUM get_offset() const
   {
      return _offset;
   }
   
   VN_VALNUM get_base_addr() const
   {
      return _base_addr;
   }
   
   VN_VALNUM get_vsym(UINT i=0) const
   {
      return _vsym_valnum;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      _canonicalize();
      return (_vsym_valnum.ordinal()      + 
	      (_base_addr.ordinal() << 4) + 
	      (_offset.ordinal() << 4)    + 
	      (_bytesize.ordinal() << 4)  +
	      (_dsctype << 8));
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      // TODO: Be more lenient about the size and dsctype from a 
      // sign-extension point-of-view.
      //
      BOOL truth = (expr != NULL                        &&
		    expr->get_kind() == MEMLOC          &&
		    expr->get_dsctype() == _dsctype     &&
		    expr->get_bytesize() == _bytesize   &&
		    expr->get_offset() == _offset       &&
		    expr->get_base_addr() == _base_addr &&
		    expr->get_vsym() == _vsym_valnum);
      return truth;
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "MEMLOC[mty=%s, bytesz=", MTYPE_name(_dsctype));
      _bytesize.print(fp);
      fputs(", ofst=", fp);
      _offset.print(fp);
      fputs(", base=", fp);
      _base_addr.print(fp);
      fputs(", vsym=", fp);
      _vsym_valnum.print(fp);
      fputs("]", fp);
   }
   
}; // VN_MEMLOC_EXPR


#ifdef KEY
//-------------- Interface for VN_CALL_OP_EXPR -----------------
//--------------------------------------------------------------

class VN_CALL_OP_EXPR : public VN_EXPR
{
private:
   
   static FREE_STACK *_Free;

   ST_IDX          _aux_id;
   UINT32          _num_opnds;
   VN_VALNUM_PAIR  _opnd[3];    // (first == PARAM, second == MU)!
   VN_VALNUM_PAIR *_opnd_array; // For larger numbers of arguments
   
   VN_CALL_OP_EXPR(ST_IDX aux_id, 
		   UINT32    num_opnds):
      _aux_id(aux_id), _num_opnds(num_opnds), _opnd_array(NULL)
   {
      if (num_opnds > 3)
	 _opnd_array = CXX_NEW_ARRAY(VN_VALNUM_PAIR, num_opnds, _Mpool);
      for (INT i = 0; i < num_opnds; i++)
      {
	 set_opnd(i, VN_VALNUM::Bottom());
	 set_opnd_vsym(i, VN_VALNUM::Bottom());
      }
   }
   
public:

   static void Init_Free_List() {_Free = CXX_NEW(FREE_STACK(_Mpool), _Mpool);}
   static void Reclaim_Free_List() {CXX_DELETE(_Free, _Mpool); _Free = NULL;}

   static VN_CALL_OP_EXPR *Create(ST_IDX aux_id, 
				  UINT32    num_opnds);
  
   void free() 
   {
      if (_opnd_array != NULL)
      {
	 CXX_DELETE_ARRAY(_opnd_array, _Mpool);
	 _opnd_array = NULL;
      }
      _Free->push(this);
   }
   
   KIND get_kind() const 
   {
      return CALL_OP;
   }

   UINT32 get_num_opnds() const 
   {
      return _num_opnds;
   }

   VN_VALNUM get_opnd(UINT i) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_CALL_OP_EXPR::opnd access"));
      return (_opnd_array == NULL? 
	      _opnd[i].first : _opnd_array[i].first);
   }

   VN_VALNUM get_vsym(UINT i=0) const 
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_CALL_OP_EXPR::vsym access"));
      return (_opnd_array == NULL? 
	      _opnd[i].second : _opnd_array[i].second);
   }

   BOOL has_top_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Top()) i++;
      return (i < get_num_opnds());
   }

   BOOL has_bottom_opnd() const 
   {
      INT i = 0;
      while(i < get_num_opnds() && get_opnd(i) != VN_VALNUM::Bottom()) i++;
      return (i < get_num_opnds());
   }

   ST_IDX get_aux_id() const 
   {
      return _aux_id;
   }

   void set_opnd(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_CALL_OP_EXPR::opnd access"));
      if (_opnd_array == NULL)
	 _opnd[i].first = vn;
      else
	 _opnd_array[i].first = vn;
   }

   void set_opnd_vsym(UINT32 i, VN_VALNUM vn)
   {
      Is_True(i < get_num_opnds(), ("Erroneous VN_CALL_OP_EXPR::vsym access"));
      if (_opnd_array == NULL)
	 _opnd[i].second = vn;
      else
	 _opnd_array[i].second = vn;
   }
   
   PTR simplify(VN *v);
   
   size_t hash()
   {
      size_t hashval = _aux_id;
      Is_True(!has_bottom_opnd() && !has_top_opnd(),
	     ("Cannot hash() with Top() or Bottom() operand!"));
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 hashval += (get_opnd(i).ordinal() << 4);
	 hashval += (get_vsym(i).ordinal() << 4);
      }
      return hashval;
   }
   
   BOOL is_equal_to(CONST_PTR expr) const
   {
      BOOL truth = (expr != NULL                        &&
		    expr->get_kind() == CALL_OP         &&
		    expr->get_num_opnds() == _num_opnds &&
		    expr->get_aux_id() == _aux_id);

      for (INT i = 0; truth && i < get_num_opnds(); i++)
	 truth = (expr->get_opnd(i) == get_opnd(i) &&
		  expr->get_vsym(i) == get_vsym(i));
      return truth;
   }

   void print(FILE *fp = stderr) const
   {
      fprintf(fp, "%d", _aux_id);
      for (INT i = 0; i < get_num_opnds(); i++)
      {
	 if (i > 0)
	    fputs(", ", fp);
	 fputs("<", fp);
	 get_opnd(i).print(fp);
	 fputs(",", fp);
	 get_vsym(i).print(fp);
	 fputs(">", fp);
      }
      fputs(")", fp);
   }
   
}; // VN_CALL_OP_EXPR
#endif

#endif // opt_vn_expr_taxonomyINCLUDED
