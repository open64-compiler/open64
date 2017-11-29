/* -*- c++ -*- */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


// -*-C++-*-

/**
*** Description:
***
***	SYSTEM_OF_EQUATIONS represents an integer linear system of equations
***	of the form (Ale)x <= (ble) and (Aeq)x = (beq).  The entries of
***	the matrix (upper case) are 32 bit integers, and the entries of
***	the vector (lower case) are 64 bit integers.  Each row represents
***	one equation.  During the computation process, 32 bit overflow
***     is checked, but 64 bit overflow is not.
***
***	The actual system is represented by two IMAT and two arrays of mINT64.
***	Extra space is typically allocated to the matrices (as specified in
***	the constructor) so that the system can grow.  If the system grows
***	too much, more space is automatically allocated. 
***
***	In addition, the system contains a large static work array.
***	The work array only contains le constraints.  The equality 
***	constraints from Aeq are subbed into the work array when needed.
***
***	The memory pool supplied with the constructor is where all space
***	(other than the global work array) needed for this object is allocated.
***
***	There are no symbol names, variables etc associated with the colums.
***	While each column represents a variable, *which* variable a column
***	represents must be indicated elsewhere.
***
***
*** Exported Types:
***
***	SYSTEM_OF_EQUATIONS
***
*** Exported Functions:
***
***	SYSTEM_OF_EQUATIONS(
***	    INT32 eqns_le,
***	    INT32 eqns_eq,
***	    INT32 vars,
***	    MEM_POOL*
***	);
***
***		
***	    Create a SYSTEM_OF_EQUATIONS, space is reserved for
***	    eqns_le rows in Ale, eqns_eq rows in Aeq and vars columns.
***	    Num_Eq_Constraints() and Num_Le_Constraints() 
***         are initialized to 0. As more equations are needed, they are 
***         added automatically in chunks of 32.
***	    Num_Vars() is initialized to vars
***
***
***	SYSTEM_OF_EQUATIONS(const SYSTEM_OF_EQUATIONS*, MEM_POOL*)
***
***	    The method used to copy one system of equations to another.
***
***	~SYSTEM_OF_EQUATIONS()
***
***	    As you would expect.
***
***     INT32 Num_Eq_Constraints() const;
***
***         How many valid equality constraints are in this system of equations.
***
***     INT32 Num_Le_Constraints() const;
***
***         How many valid le constraints are in this system of equations.
***
***     INT32 Num_Vars() const;
***
***         How many valid variables are in this system.
***
***     void Add_Vars(const INT32 num_vars)
***
***	    Add num_vars valid columns to the matrices and vectors.
***	    If there is space, this basically zeroes the columns of
***	    the matrices.  Otherwise, this may also involve resizing.
***
***     void Remove_Last_Vars(INT num_vars)
***
***	    Get rid of the last num_vars variables.  All these columns
***	    must be zero on entry.
***
***	void Add_Eq(const mINT32 row[], INT64 b);
***	void Add_Le(const mINT32 row[], INT64 b);
***
***	    Add an equation to the current system, either as an equals or
***	    le than condition.  
***
***     BOOL Add_Le_Non_Redundant(const mINT32 row[], INT64 b);
***
***	    Like Add_Le, but only adds it if adding it is not redundant.
***	    Return true if added.
***
***	void Add_Eq(INT num_rows);
***	void Add_Le(INT num_rows);
***	    Add num_rows, uninitialized, to the current system.
***
***     void Add_Soe(const SYSTEM_OF_EQUATIONS* soe);
***
***         Add the passed in system of equations to the current system.
***
***     void Zero_Row_Le(INT32 r);
***
***         Zero row r in Ale,ble 
***
***     void Complement_Le(INT32 r);
***
***	    Makes the row have the complementary equation, which is NOT
***	    -Ale() <= -b, but rather -Ale() <= -b-1
***
***     INT32 Leftmost_Non_Zero_Le(INT32 r) const;
***
***         Returns first c such that Ale()(r,c) is non-zero.  Returns
***         Num_Vars() if entire row is zero.
***
***     void Sort_Le(INT* sort_criteria, BOOL descending = FALSE);
***
***         Move rows so that the rows are ordered by sort_criteria.
***	    Sort the sort_criteria as well.  That way, 
***	    sort_criteria[i] < sort_criteria[j] if and only if i<j.
***
***     void Remove_Last_Eq(INT how_many = 1);
***
***	    Remove the last how_many equality consttrains from the system
***
***     void Remove_Last_Le(INT how_many = 1);
***
***	    Remove the last how_many inequality constraints from the system
***
***     void Remove_Eq_Number(INT which);
***
***	    Remove the equation (zero is the first equation).
***
***     void Remove_Le_Number(INT which);
***
***	    Remove the equation (zero is the first equation).
***
***	BOOL Is_Consistent()
***
***	    Returns FALSE if the system of equations is proved inconsistent.
***
***	void Mark_Redundant(BOOL *is_redundant)
***
***	    Mark redundant constraints. On output, is_redundant[i] is
***         TRUE iff "le" constraint 'i' is redundant.
***	    The intial system is assume to be consistent 
***
***     BOOL Copy_To_Work()
***
***	    Copy the inequality constraints into the work array.
***	    Return FALSE if fails due to lack of space (extremely unlikely)
***
***     void Add_Work_Le()
***
***	    Add all the work le constraints into Ale
***	    If work has more columns than Ale, add the extra ones to Ale (zero)
***	    If work has fewer columns than Ale, add the extra zero ones to work
***
***     void Add_Work_Le_Non_Simple_Redundant()
***
***	    As Add_Work_Le(), but only add those equations that are not simple
***	    redundant equations.  It does not check that either the original
***	    system or the work array are not redundant.
***
***     BOOL Sub_In_Equal(BOOL *proved_inconsistent)
***
***	    Substitute the equality constraints from Aeq into the inequality
***         constraints in the work array.
***	    Returns FALSE on failure.
***
***     static BOOL Project(const INT32 i, BOOL *proved_inconsistent, 
***		     const INT32 min_var=0)
***
***         Project the sytem of equations in the 'i' dimension.
***         i.e. eliminate variable 'i' from the system.  Column
***         'i' will be zeroed in the process.
***         Note that Project works on the work array.
***         It does not affect the A or b arrays.
***         You must call Copy_To_Work() before calling Project.
***         Note also that Project ignores the equality constraint.
***         You must first call Sub_In_Equal(...) (but after Copy_To_Work())i
***         if you want to use the equality constraints.
***	    Project is the only public routine that works explicitly on
***	    the work array.  All the others manage the copying themselves.
***         Proved_inconsistent is set to TRUE iff in the course
***         of the projection, the system is proved inconsistent.
***         If the system is inconsistent, the state of the work
***         array is undefined. This routine returns FALSE if it 
***         fails (lack of work space or overflow).
***         Work(i,j) is assumed to = 0 for any j < min_var.  This allows
***         a more efficient projection when one projects away the variables
***         in order
***
***     void Take_Gcds()
***
***         Compact the system of equations.  Divide through equations
***                a1x1 + ... + anxn <= b
***         by gcd(a1,...,an), lhs becomes floor(b/gcd(a1,...,an)).
***         Likewise, for the equalities, divide through by
***         gcd(a1,...,an,b).  (If b is not divisible by gcd(a1,...,an)
***         then the system is inconsistent, but let's just do it this way.
***         Nothing changes.
***
***     IMAT& Aeq(), const IMAT& Aeq() const;
***
***         return Aeq
***
***     IMAT& Ale(), const IMAT& Ale() const;
***
***         return Ale
***
***     INT64 *Beq() 
***
***         return Beq
***
***     INT64 *Ble()
***
***         return Ble
***
***	INT32 Work(const INT32 i, const INT32 j) const
***
***	    return elment (i,j) of the work array
***
***	INT64 Work_Const(const INT32 i) const
***
***	    return the constant term of the i'th work constraint
***
***	INT32 Work_Constraints() const
***
***	    return the number of constraints in the work array
***
***	void Reset_To(INT32 nrows_le, INT32 nrows_eq, INT32 ncols)
***
***	    Make the system of equations smaller be reducing the number of
***	    rows in Ale, and/or reducing the number of rows in Aeq, and/or
***	    reducing the number of columns.
***
***	void Print(FILE *fp) const
***
***	    Print out the sytem of constraints.
***
***	static void Print_Work(FILE *fp)
***
***	    Print out the work array.
***
***	MEM_POOL *Pool() const
***
***	    Return the mempool being used.
***
***	SYSTEM_OF_EQUATIONS&	operator = (const SYSTEM_OF_EQUATIONS&);
***	SYSTEM_OF_EQUATIONS(const SYSTEM_OF_EQUATIONS&);
***	SYSTEM_OF_EQUATIONS();
***
***	    These are not to be used.
***
***
**/

#ifndef soe_INCLUDED
#define soe_INCLUDED "soe.h"

#ifdef _KEEP_RCS_ID
static char *soe_rcs_id = soe_INCLUDED "$Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#ifndef _defs_INCLUDED
#include "defs.h"
#endif
#ifndef _defs_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef mat_INCLUDED
#include "lnopt_main.h"
#include "mat.h"
#endif
#ifndef INFIN_DECLARE
#include "infin.h"
#endif



#ifdef LNO
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif
#endif


enum SVPC_RESULT { SVPC_CONSISTENT,SVPC_INCONSISTENT,SVPC_INAPPLICABLE };
enum ACY_RESULT { ACY_CONSISTENT,ACY_INCONSISTENT,ACY_INAPPLICABLE };


class SYSTEM_OF_EQUATIONS {
public:

  SYSTEM_OF_EQUATIONS(INT32 eqns_le,INT32 eqns_eq,INT32 vars, MEM_POOL *pool) 
			: _Ale(eqns_le,vars,pool), _Aeq(eqns_eq,vars,pool) {
    _pool = pool;
    _ble = CXX_NEW_ARRAY(mINT64,eqns_le,pool);
    _beq = CXX_NEW_ARRAY(mINT64,eqns_eq,pool);
    _eqns_le = _eqns_eq = 0;
    _eqns_le_stored = eqns_le;
    _eqns_eq_stored = eqns_eq;
    _vars = vars;
    _work_rows = _work_rows_eq = 0;
  }
  SYSTEM_OF_EQUATIONS(const SYSTEM_OF_EQUATIONS *soe, MEM_POOL *pool); 
  ~SYSTEM_OF_EQUATIONS() { 
    CXX_DELETE_ARRAY(_ble,_pool); 
    CXX_DELETE_ARRAY(_beq,_pool);
  }

  INT32 Num_Eq_Constraints() const { return(_eqns_eq); }
  INT32 Num_Le_Constraints() const { return(_eqns_le); }
  INT32 Num_Vars() const { return(_vars); }
  void Add_Eq(const mINT32 row[], INT64 b);
  void Add_Eq(INT num_rows);
  void Add_Le(const mINT32 row[], INT64 b);
  BOOL Add_Le_Non_Redundant(const mINT32 row[], INT64 b);
  void Add_Le(INT num_rows);
  void Add_Vars(const INT32 num_vars);
  void Add_Soe(const SYSTEM_OF_EQUATIONS* soe);
  void Zero_Row_Le(INT32 r);
  void Complement_Le(INT32 r);
  INT32 Leftmost_Non_Zero_Le(INT32 r) const;
  void Sort_Le(INT* sort_criteria, BOOL descending = FALSE);

  void Remove_Last_Eq(INT how_many=1) { _eqns_eq-=how_many; }
  void Remove_Last_Le(INT how_many=1) { _eqns_le-=how_many; }
  void Remove_Eq_Number(INT which);
  void Remove_Le_Number(INT which);
  void Remove_Last_Vars(INT num_vars);
  void Reset_To(INT32 nrows_le, INT32 nrows_eq, INT32 ncols);

  BOOL Is_Consistent();
  void Mark_Redundant (BOOL *is_redundant);
  INT16 Mark_Simple_Redundant(BOOL *is_redundant);
  INT16 Mark_New_Redundant(BOOL *is_redundant);
  BOOL Copy_To_Work();
  void Add_Work_Le();
  void Add_Work_Le_Non_Simple_Redundant();
  BOOL Sub_In_Equal(BOOL *proved_inconsistent);
  static BOOL Project(const INT32 i,BOOL *proved_inconsistent,
		      const INT32 min_var=0);
  INT Change_Base(const INT32 dim, const INT32 pos, MEM_POOL *pool);
  void Take_Gcds();
  const IMAT& Aeq() const { return(_Aeq); }
  IMAT& Aeq() { return(_Aeq); }
  const IMAT& Ale() const { return(_Ale); }
  IMAT& Ale() { return(_Ale); }
  mINT64 *Beq() { return(_beq); }
  const mINT64 *Beq() const { return(_beq); }
  mINT64 *Ble() { return(_ble); }
  const mINT64 *Ble() const { return(_ble); }
  mINT32 Work(const INT32 i, const INT32 j) const { return _work[i][j]; }
  mINT64 Work_Const(const INT32 i) const { return _work_const[i]; }
  mINT32 Work_Constraints() const { return _work_rows; } 
  INT32 Simple_Redundant(const INT32 r1, const INT32 r2)
    { return Simple_Redundant(&_work[r1][0],&_work[r2][0],
			      _work_const[r1], _work_const[r2],
			      0, _work_cols);
    }
  static void Elim_Simple_Redundant(const INT32 min_var=0);
  BOOL Prove_Redundant(const INT set, const INT dim);
  BOOL Is_Consistent_Work();

  MEM_POOL *Pool() const {return _pool;}
  void Print(FILE *fp) const;
  static void Print_Work(FILE *fp) ;

#ifdef KEY
  // External hooks to SVPC and related stuff for implementing loop unswitching
  BOOL SVPC_Applicable() { return SVPC() != SVPC_INAPPLICABLE; }
  INT32_INFIN Lower_Bound(INT col) { return _lower_bound[col]; }
  INT32_INFIN Upper_Bound(INT col) { return _upper_bound[col]; }
  mINT32 Work_Cols() { return _work_cols; }
#endif

 private:

  // these are private and undefined

  INT32 ROW_INCR() const { return(32); }

  SYSTEM_OF_EQUATIONS&	operator = (const SYSTEM_OF_EQUATIONS&);
  SYSTEM_OF_EQUATIONS(const SYSTEM_OF_EQUATIONS&);
  SYSTEM_OF_EQUATIONS();

  IMAT		_Ale;
  IMAT		_Aeq;
  mINT64*	_ble;
  mINT64*	_beq;

  MEM_POOL*	_pool;
  mINT32	_eqns_le;
  mINT32	_eqns_le_stored; // how many rows actually stored
  mINT32	_eqns_eq;
  mINT32	_eqns_eq_stored; // how many rows actually stored
  mINT32	_vars;  // number of valid variables

  // A static work array
#define SOE_MAX_WORK_ROWS 1000
#define SOE_MAX_WORK_COLS 30
  static mINT32 _work[SOE_MAX_WORK_ROWS][SOE_MAX_WORK_COLS];
  static mINT64 _work_const[SOE_MAX_WORK_ROWS];
  static mINT32 _work_rows;
  static mINT32 _work_cols;
  static BOOL _is_redundant[SOE_MAX_WORK_ROWS];
  static BOOL _is_svpc[SOE_MAX_WORK_ROWS];


  // work array for subbing in equality
#define SOE_MAX_WORK_ROWS_EQ 100
  static mINT32 _work_eq[SOE_MAX_WORK_ROWS_EQ][SOE_MAX_WORK_COLS];
  static mINT64 _work_const_eq[SOE_MAX_WORK_ROWS_EQ];
  static mINT32 _work_rows_eq;

  // helper routines
  static BOOL Elim_One(const INT32 i,const INT32 plus, const INT32 minus, 
		BOOL *proved_inconsistent, const INT32 min_var);
  static INT32 Is_Simple_Redundant(const INT32* row1, const INT32* row2, 
				   INT64 row1c, INT64 row2c,
				   INT min_var, INT cols);
  static INT32 Simple_Redundant(const INT32* row1, const INT32* row2, 
				INT64 row1c, INT64 row2c,
				INT min_var, INT cols);
  BOOL Copy_To_Work(const INT32 from, const INT32 to);
  BOOL Copy_Inverse_To_Work(const INT32 i);

  // SVPC routines
  static INT32_INFIN _lower_bound[SOE_MAX_WORK_COLS];
  static INT32_INFIN _upper_bound[SOE_MAX_WORK_COLS];
  BOOL One_Var_Consistent(INT32 var,INT32 from, INT32 to);
  void SVPC_Set_Bound(INT32 i, INT32 var);
  SVPC_RESULT SVPC();
  ACY_RESULT Acyclic_Test();
  INT Var_Leaf(INT i, INT *sign);
  void Acy_Set_Var(INT i, INT32_INFIN val, BOOL *inconsistent);

  // Sub_In_Equal helper routines
  BOOL Copy_To_Work_Eq();
  BOOL Sub_Last_Equal(BOOL *proved_inconsistent);
  BOOL Sub_Last_Equal_Unary(const INT32 i, const INT32 minvar);
  INT32 Normalize_Eq_and_Find_Smallest(INT32, INT32, BOOL *);
  INT32 Mod_Hat(INT32 a,INT32 b);
  BOOL Add_Work_Var();

  void Gcd_Normalize();
};

#endif



