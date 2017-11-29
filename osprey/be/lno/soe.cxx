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


//-*-c++-*-
//                     Systems of Equations
//                     --------------
//
//
// Description:
//
//     SYSTEM_OF_EQUATIONS represents an integer linear system of equations
//     of the form (Ale)x <= (ble) and (Aeq)x = (beq).  The entries of
//     the matrix (upper case) are 32 bit integers, and the entries of
//     the vector (lower case) are 64 bit integers.  Each row represents
//     one equation.
//
//     The actual system is represented by two IMAT and two arrays of mINT64.
//     Extra space is typically allocated to the matrices (as specified in
//     the constructor) so that the system can grow.  If the system grows
//     too much, more space is automatically allocated.
//
//     In addition, the system contains a large static work array.
//
//     The memory pool supplied with the constructor is where all space
//     (other than the global work array) needed for this object is allocated.
//
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: soe.c  
 * $Revision: 1.7 $
 * $Date: 04/12/21 14:57:16-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Linear system of equalities and inequalities
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include <sys/types.h>
//#include "frac.h"
#include "soe.h"
//#include "lnopt_main.h"
#include "tracing.h"
#define  TT_LNO_DEP2			0x00000002  // from lnopt_main.cxx

#ifdef LNO
#include "ipa_lno_util.h"
#endif 

#ifdef KEY
#include "config.h"                                  // for Allow_wrap_around_opt
#endif

//-----------------------------------------------------------------------
// greatest common divisor
//-----------------------------------------------------------------------

#ifndef LNO
inline mINT32 myabs(mINT32 i)
{
  return i < 0 ? -i : i;
}

inline mINT64 myabs(mINT64 i)
{
  return i < 0 ? -i : i;
}

// Greatest common divisor.
static
INT32 Gcd(mINT32 i, mINT32 j)
{
  i = myabs(i);
  j = myabs(j);

  if (i == 0)
    return j;
  if (j == 0)
    return i;

  if (i > j) {
    INT32 t = i;
    i = j;
    j = t;
  }

  do {
    INT32 t = i;
    i = j%i;
    j = t;
  } while (i);

  return j;
}

static
INT64 Gcd(mINT64 i, mINT64 j)
{
  i = myabs(i);
  j = myabs(j);

  if (i == 0)
    return j;
  if (j == 0)
    return i;

  if (i > j) {
    INT64 t = i;
    i = j;
    j = t;
  }

  do {
    INT64 t = i;
    i = j%i;
    j = t;
  } while (i);

  return j;
}

#endif

SYSTEM_OF_EQUATIONS::SYSTEM_OF_EQUATIONS(const SYSTEM_OF_EQUATIONS *soe, 
		MEM_POOL *pool) : _Ale(soe->_Ale,pool), _Aeq(soe->_Aeq,pool)
{
  _pool = pool;
  // ble and beq should actually have storage allocated for
  // eqns_le_stored, and not just _eqns_le, and _eqns_eq
  // respectively 
  _ble = CXX_NEW_ARRAY(mINT64,soe->_eqns_le_stored,pool);
  _beq = CXX_NEW_ARRAY(mINT64,soe->_eqns_eq_stored,pool);
  INT32 i;
  for (i=0; i<soe->_eqns_le; i++) {
    _ble[i] = soe->_ble[i];
  }
  for (i=0; i<soe->_eqns_eq; i++) {
    _beq[i] = soe->_beq[i];
  }
  _eqns_eq = soe->_eqns_eq;
  _eqns_eq_stored = soe->_eqns_eq_stored;
  _eqns_le = soe->_eqns_le;
  _eqns_le_stored = soe->_eqns_le_stored;
  _vars = soe->_vars;
}

void SYSTEM_OF_EQUATIONS::Add_Vars(const INT32 num_vars) 
{
  if (_Ale.Cols() < (_vars+num_vars)) _Ale.D_Add_Cols(num_vars);
  if (_Aeq.Cols() < (_vars+num_vars)) _Aeq.D_Add_Cols(num_vars);

  // zero the new columns
  INT32 i;
  for (i=0; i<_eqns_le; i++) {
    for (INT32 j=_vars; j<_vars+num_vars; j++) {
      _Ale(i,j) = 0;
    }
  }
  for (i=0; i<_eqns_eq; i++) {
    for (INT32 j=_vars; j<_vars+num_vars; j++) {
      _Aeq(i,j) = 0;
    }
  }
  _vars += num_vars;
}

void SYSTEM_OF_EQUATIONS::Remove_Last_Vars(INT num_vars) 
{
  FmtAssert(num_vars <= _vars,("Removed more variables than exist"));
  // Make sure the columns are zero

  INT32 i;
  for (i=0; i<_eqns_le; i++) {
    for (INT32 j=_vars-num_vars; j<_vars; j++) {
      FmtAssert(_Ale(i,j) == 0,("Removed a used variable"));
      _Ale(i,j) = 0;
    }
  }
  for (i=0; i<_eqns_eq; i++) {
    for (INT32 j=_vars-num_vars; j<_vars; j++) {
      FmtAssert(_Aeq(i,j) == 0,("Removed a used variable"));
      _Aeq(i,j) = 0;
    }
  }
  _vars -= num_vars;
}

void SYSTEM_OF_EQUATIONS::Add_Eq(const mINT32 row[], INT64 b)
{
  if (_eqns_eq + 1 > _eqns_eq_stored) {  // make space
    _Aeq.D_Add_Rows(ROW_INCR());
    _eqns_eq_stored += ROW_INCR();
    INT64 *tmp = CXX_NEW_ARRAY(mINT64,_eqns_eq_stored,_pool);
    for (INT32 i=0; i<_eqns_eq; i++) {
      tmp[i] = _beq[i];
    }
    CXX_DELETE_ARRAY(_beq,_pool);
    _beq = tmp;
  }
  for (INT32 i=0; i<_vars; i++) {
    _Aeq(_eqns_eq,i) = row[i];
  }
  _beq[_eqns_eq++] = b;
}

void SYSTEM_OF_EQUATIONS::Add_Eq(INT num_rows)
{
  if (_eqns_eq + num_rows > _eqns_eq_stored) {  // make space
    _Aeq.D_Add_Rows(MAX(_eqns_eq+num_rows-_eqns_eq_stored,ROW_INCR()));
    _eqns_eq_stored += MAX(_eqns_eq+num_rows-_eqns_eq_stored,ROW_INCR());
    INT64 *tmp = CXX_NEW_ARRAY(mINT64,_eqns_eq_stored,_pool);
    for (INT32 i=0; i<_eqns_eq; i++) {
      tmp[i] = _beq[i];
    }
    CXX_DELETE_ARRAY(_beq,_pool);
    _beq = tmp;
  }
  _eqns_eq += num_rows;
}

void SYSTEM_OF_EQUATIONS::Add_Le(const mINT32 row[], INT64 b)
{
  if (_eqns_le >= _eqns_le_stored) {  // make space
    _Ale.D_Add_Rows(ROW_INCR());
    _eqns_le_stored += ROW_INCR();
    INT64 *tmp = CXX_NEW_ARRAY(mINT64,_eqns_le_stored,_pool);
    for (INT32 i=0; i<_eqns_le; i++) {
      tmp[i] = _ble[i];
    }
    CXX_DELETE_ARRAY(_ble,_pool);
    _ble = tmp;
  }
  for (INT32 i=0; i<_vars; i++) {
    _Ale(_eqns_le,i) = row[i];
  }
  // The following is really a bug - you always need to allocate
  // more storage for the right hand sides.. This is not the 
  // greatest solution.
  _ble[_eqns_le++] = b;
}

BOOL SYSTEM_OF_EQUATIONS::Add_Le_Non_Redundant(const mINT32 row[], INT64 b)
{
  BOOL rv;
  INT  newrow = Num_Le_Constraints();

  Add_Le(row, b);
  Complement_Le(newrow);
  if (Is_Consistent()) {
    rv = TRUE;
    Complement_Le(newrow);
  }
  else {
    rv = FALSE;
    Remove_Last_Le();
  }

  return rv;
}

void SYSTEM_OF_EQUATIONS::Add_Le(INT num_rows)
{
  if (_eqns_le + num_rows > _eqns_le_stored) {  // make space
    _Ale.D_Add_Rows(MAX(_eqns_le+num_rows-_eqns_le_stored,ROW_INCR()));
    _eqns_le_stored += MAX(_eqns_le+num_rows-_eqns_le_stored,ROW_INCR());
    INT64 *tmp = CXX_NEW_ARRAY(mINT64,_eqns_le_stored,_pool);
    for (INT32 i=0; i<_eqns_le; i++) {
      tmp[i] = _ble[i];
    }
    CXX_DELETE_ARRAY(_ble,_pool);
    _ble = tmp;
  }
  _eqns_le += num_rows;
}

void SYSTEM_OF_EQUATIONS::Add_Soe(const SYSTEM_OF_EQUATIONS* soe)
{
  INT32 r;
  FmtAssert(Num_Vars() == soe->Num_Vars(), ("Incompatable soe's in Add_Soe"));

  for (r = 0; r < soe->Num_Le_Constraints(); r++)
    Add_Le(&soe->_Ale(r,0), soe->_ble[r]);
  for (r = 0; r < soe->Num_Eq_Constraints(); r++)
    Add_Eq(&soe->_Aeq(r,0), soe->_beq[r]);
}


void SYSTEM_OF_EQUATIONS::Zero_Row_Le(INT32 r)
{
  for (INT32 c = 0; c < Num_Vars(); c++)
    _Ale(r,c) = 0;
    _ble[r] = 0;
}

void SYSTEM_OF_EQUATIONS::Complement_Le(INT32 r)
{
  for (INT32 c = 0; c < Num_Vars(); c++)
    _Ale(r,c) = -_Ale(r,c);
  _ble[r] = -_ble[r] - 1;
}

INT32 SYSTEM_OF_EQUATIONS::Leftmost_Non_Zero_Le(INT32 r) const
{
  INT32 c;
  for (c = 0; c < Num_Vars(); c++)
    if (_Ale(r,c))
      break;
  return c;
}

void SYSTEM_OF_EQUATIONS::Sort_Le(INT* sort_criteria, BOOL descending)
{
  // Insertion sort

  INT rows = Num_Le_Constraints();
  INT cols = Num_Vars();

  for (INT i = 0; i < rows-1; i++) {

    // select smallest (largest) for i

    INT mx = i;
    for (INT j = i+1; j < rows; j++) {
      if (!descending) {
	if (sort_criteria[j] < sort_criteria[mx])
	  mx = j;
      }
      else {
	if (sort_criteria[j] > sort_criteria[mx])
	  mx = j;
      }
    }

    // swap if necessary

    if (mx != i) {
      INT ts = sort_criteria[mx];
      sort_criteria[mx] = sort_criteria[i];
      sort_criteria[i] = ts;

      INT64 tmp = _ble[i];
      _ble[i] = _ble[mx];
      _ble[mx] = tmp;

      for (INT32 c = 0; c < cols; c++) {
	INT32 tmp = _Ale(i,c);
	_Ale(i,c) = _Ale(mx,c);
	_Ale(mx,c) = tmp;
      }
    }
  }
}


void SYSTEM_OF_EQUATIONS::Print(FILE *fp) const
{
  fprintf(fp,"\n\tAle, ble is \n");
  INT32 i;
  for (i=0; i<_eqns_le; i++) {
    fprintf(fp, "\t");
    for (INT32 j=0; j<_vars; j++) {
      fprintf(fp," %d ",_Ale(i,j));
    }
    fprintf(fp,"    %lld \n",_ble[i]);
  }
  fprintf(fp,"\n");

  if (_eqns_eq == 0) return;
  fprintf(fp,"\tAeq,beq is \n");
  for (i=0; i<_eqns_eq; i++) {
    fprintf(fp, "\t");
    for (INT32 j=0; j<_vars; j++) {
      fprintf(fp," %d ",_Aeq(i,j));
    }
    fprintf(fp,"    %lld \n",_beq[i]);
  }
  // fprintf(fp,"\n");
}

void SYSTEM_OF_EQUATIONS::Print_Work(FILE *fp) 
{
  fprintf(fp,"work_le,const_le is \n");
  INT32 i;
  for (i=0; i<_work_rows; i++) {
    for (INT32 j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_const[i]);
  }
  fprintf(fp,"\n");
  if (_work_rows_eq ==0) return;

  fprintf(fp,"work_eq, const_eq is \n");
  for (i=0; i<_work_rows_eq; i++) {
    for (INT32 j=0; j<_work_cols; j++) {
      fprintf(fp," %d ",_work_eq[i][j]);
    }
    fprintf(fp,"    %lld \n",_work_const_eq[i]);
  }
  fprintf(fp,"\n");
}

// Copy the system of equations to the work space
BOOL SYSTEM_OF_EQUATIONS::Copy_To_Work()
{
  if ( _vars > SOE_MAX_WORK_COLS) return(FALSE);
  _work_cols = _vars;
  _work_rows = 0;
  return(Copy_To_Work(0,_eqns_le-1));
}

// Copy work le to the end of Ale, zero excess columns if any.
void SYSTEM_OF_EQUATIONS::Add_Work_Le()
{
  if (_work_cols > _vars) {
    Add_Vars(_work_cols - _vars +1);
  }

  if (_work_cols < _vars) {
    for (INT r = 0; r < _work_rows; r++)
      for (INT j = _work_cols; j < _vars; j++)
	_work[r][j] = 0;
  }

  for (INT r = 0; r < _work_rows; r++) {
    Add_Le(&_work[r][0], _work_const[r]);
  }
}


// Copy work le to the end of Ale, zero excess columns if any.
// Before adding an equation, make sure it's not simply redundant with
// anything in this.

void SYSTEM_OF_EQUATIONS::Add_Work_Le_Non_Simple_Redundant()
{
  if (_work_cols > _vars) {
    Add_Vars(_work_cols - _vars +1);
  }

  if (_work_cols < _vars) {
    for (INT r = 0; r < _work_rows; r++)
      for (INT j = _work_cols; j < _vars; j++)
	_work[r][j] = 0;
  }

  INT eqns = _eqns_le;
  INT r;
  for (r = 0; r < _work_rows; r++) {
    INT e;
    for (e = 0; e < eqns; e++) {
      INT rd = Is_Simple_Redundant(&_work[r][0], &_Ale(e,0),
				   _work_const[r], _ble[e],
				   0, _vars);
      if (rd == 1) {
	break;
      }
      else if (rd == 2) {
	for (INT c = 0; c < _vars; c++)
	  _Ale(e,c) = _work[r][c];
	_ble[e] = _work_const[r];
	break;
      }
    }
    if (e == eqns)
      Add_Le(&_work[r][0], _work_const[r]);
  }
}



// Can you prove the system of equations inconsistent
// Can always conservatively return TRUE
BOOL SYSTEM_OF_EQUATIONS::Is_Consistent()
{
  BOOL proved_inconsistent;
  if (!Copy_To_Work()) return(TRUE);  
  if (!Sub_In_Equal(&proved_inconsistent)) return(TRUE);
  if (proved_inconsistent) return(FALSE);
  return(Is_Consistent_Work());
}

// substitute all the equality constraints into the inequality constraints
// inside the work array
// Use Pugh's algorithm from the original Omega Test paper
// Set proved_inconsistent to true, if in the process the system is 
// proved inconsistent
// Return false on uncorrectable failure (oveflow)
BOOL SYSTEM_OF_EQUATIONS::Sub_In_Equal(BOOL *proved_inconsistent) 
{
  *proved_inconsistent = FALSE;
  if (_eqns_eq == 0) return(TRUE);
  if (!Copy_To_Work_Eq()) return(TRUE);  // copy to work space 
					 //return if no space
  while (_work_rows_eq > 0) {
     if (!Sub_Last_Equal(proved_inconsistent)) return(FALSE);
     if (*proved_inconsistent) return(TRUE);
  }
  return(TRUE);
}

// The heart of Fourier-Motzkin elimination
// This routine eliminates variable 'i' from the work array
// This partitions each constraint, c, based on the sign of _work[c][i]
// Each constraint in the positive partition is used vrs each constraint in
//  the negative partition to eliminate the variable
// This returns FALSE on failure (lack of space or overflow)
// _work[i][j] is assumed to = 0 for any j < min_var
// checks for overflow on the 32 bit coefficients but not the 64 bit constants

BOOL SYSTEM_OF_EQUATIONS::Project(const INT32 i,BOOL *proved_inconsistent,
					const INT32 min_var)
{
  Is_True(i < _work_cols,
	  ("Bad i for SYSTEM_OF_EQUATIONS::Project"));
  *proved_inconsistent = FALSE;

  // do the work
  INT32 last_undone = _work_rows-1;  // the last constraint not looked at 
  INT32 plus=0;

  while (plus <= last_undone) {  // each plus
    if (_work[plus][i] > 0) {
      // each minus
      for (INT32 minus=0; minus <= last_undone; minus++) {  
        if (_work[minus][i] < 0) {
	  if (!Elim_One(i,plus,minus,proved_inconsistent,min_var)) {
	    return(FALSE);
	  } else if (*proved_inconsistent) {
	    return(TRUE);
	  }
	}  
      } 

      // we're done with this plus so we can erase it by copying over it
      if (plus != _work_rows-1) {
	_work_const[plus] = _work_const[_work_rows-1];
	for (INT32 col=min_var; col < _work_cols; col++) {
	  _work[plus][col] = _work[_work_rows-1][col];
	}
	if (last_undone == _work_rows-1) {  // no new constraints created
	  last_undone--;  // last_undone has been moved to plus
	} else {  // a new constraint has been moved to plus
	  Is_True(_work[plus][i] <= 0,("Error in Project "));
	  plus++; // new constraint cannot be a plus (new ones never are)
	}
      } else {
	if (last_undone == _work_rows-1) {  // no new constraints created
	  last_undone--;  // last_undone has been moved to plus
        }
      }
      _work_rows--;
      if (_work_rows ==0) {
	return(TRUE);  // no constraints left
      }
    } else {
      plus++;
    }
  } // we're done with all the pluses

  // erase all the negatives by copying/compacting over them
  for (INT32 minus=0; minus <_work_rows; minus++) {
    if (_work[minus][i] < 0) {
      while ((minus < _work_rows-1) && _work[_work_rows-1][i] < 0) _work_rows--;
      if (minus < _work_rows-1) {
	_work_const[minus] = _work_const[_work_rows-1];
	for (INT32 col=min_var; col < _work_cols; col++) {
	  _work[minus][col] = _work[_work_rows-1][col];
	}
      }
      _work_rows--;
    }
  }
  Elim_Simple_Redundant(min_var);
  return(TRUE);
}


// Mark redundant constrains
// To check if constraint[i] is redundant
// Copy the non-redundant of constraints[0..i-1,i+1...] to the work array
// Copy the inverse of constraint [i] into the work array
// If the system is inconsistent then constraint[i] is redundant
// This may conservatively not find some redundancies
void SYSTEM_OF_EQUATIONS::Mark_Redundant(BOOL *is_redundant)
{
  BOOL proved_inconsistent;

  if (_eqns_le  < 1) return;
  INT32 i;
  for (i=0; i<_eqns_le; i++) is_redundant[i] = FALSE;

  for (i=0; i<_eqns_le; i++) {
    _work_rows = 0;
    // copy non-redundant constraints 0..i-1 into the work array
    INT32 j;
    for (j=0; j<= i-1; j++) {
      if (!is_redundant[j]) {
        if (!Copy_To_Work(j,j)) return;
      }
    }
    // copy constraints i.._eqns_le-1 into the work array (can't be redundant)
    for (j =i+1; j<_eqns_le; j++) {
        if (!Copy_To_Work(j,j)) return;
    }

    if (!Copy_Inverse_To_Work(i)) return;
    if (!Sub_In_Equal(&proved_inconsistent)) return;
    if (proved_inconsistent || !Is_Consistent_Work()) {
      is_redundant[i] = TRUE;
    }
  }
  
}


//
// PRIVATE ROUTINES
//

// The heart of the heart of Fourier-Motzkin
// Combine one plus vrs one negative constraint to eliminate variable i
// Put the result in row _work_rows
// This is called by the project routine
// This returns FALSE on failure (lack of space or overflow)
BOOL SYSTEM_OF_EQUATIONS::Elim_One(const INT32 i, const INT32 plus, 
				   const INT32 minus,BOOL *proved_inconsistent,
				   const INT32 min_var)
{
  if (_work_rows+1 >= SOE_MAX_WORK_ROWS) {
    return(FALSE);
  }

  INT64 tmp; // used to check overflow
  INT32 g = Gcd(abs(_work[plus][i]),abs(_work[minus][i]));
  INT64 plus_mult = _work[plus][i] / g;
  INT64 minus_mult = -_work[minus][i] / g;

  _work_const[_work_rows] = minus_mult*_work_const[plus] + 
		            plus_mult*_work_const[minus];

  BOOL has_vars = FALSE;  // does the new constraint have any variables
  INT32 col;
  for (col=0; col < min_var; col++) _work[_work_rows][col] = 0;

  if (minus_mult == 1 && plus_mult == 1) { // common case, avoid the multiply
    for (col=min_var; col < _work_cols; col++) {
      tmp = (INT64) _work[plus][col] + (INT64) _work[minus][col];
      if (abs(tmp) > INT32_MAX) return(FALSE);  // overflow
      _work[_work_rows][col] = (INT32) tmp;
      if (_work[_work_rows][col] != 0) has_vars = TRUE;
    }
  } else { 
    for (col=min_var; col < _work_cols; col++) {
      tmp = minus_mult*_work[plus][col] + plus_mult*_work[minus][col];
      if (abs(tmp) > INT32_MAX) return(FALSE);  // overflow
      _work[_work_rows][col] = (INT32) tmp;
      if (_work[_work_rows][col] != 0) has_vars = TRUE;
    }
  }

  if (!has_vars) {  // the new constraint is null
    if (_work_const[_work_rows] < 0) {
      *proved_inconsistent = TRUE;
      return(TRUE);
    }
  } else {
    _work_rows++;
  }
  return(TRUE);
}


// Eliminate simple redundant constraints from the work array
// Simple tests to improve efficiency.
// We consider the two constraints redundant
// if one constraint (ignoring the constant term) is a multiple of the other
// the redundant constraint is the one that is less tight
// All cols < min_var assumed to be 0

void SYSTEM_OF_EQUATIONS::Elim_Simple_Redundant(const INT32 min_var)
{

  static INT16 last_non_zero[SOE_MAX_WORK_ROWS];
  // set last_non_zero to the column position of the last  non-zero elements
  // this is an efficiency aid, redundant constraints must have the same
  // value for this
  INT i;
  for (i=0; i<_work_rows; i++) {
    _is_redundant[i] = FALSE;
    INT j = _work_cols - 1;
    while ((j >= min_var) && (_work[i][j] == 0)) j--;
    last_non_zero[i] = j;
  }

  // find the redundant constraints
  for (i=0; i<_work_rows; i++) {
    if (!_is_redundant[i]) {
      for (INT32 j=i+1; j<_work_rows; j++) {
	if (!_is_redundant[j] && (last_non_zero[i] == last_non_zero[j])) {
	  INT redun=Is_Simple_Redundant(&_work[i][0], &_work[j][0],
					_work_const[i], _work_const[j],
					min_var, _work_cols);
	  if (redun==1) {
	    _is_redundant[i] = TRUE;
	  } else if (redun==2) {
	    _is_redundant[j] = TRUE;
	  }


	}
      }
    }
  }

  // compact the redundant constraints 
  for (i=0; i<_work_rows; i++) {
    if (_is_redundant[i]) {
      while ((i <_work_rows-1) && _is_redundant[_work_rows-1]) _work_rows--;
      if (i < _work_rows -1) {
	_work_const[i] = _work_const[_work_rows-1];
	for (INT32 col=min_var; col<_work_cols; col++) {
	  _work[i][col] = _work[_work_rows-1][col];
	}
      }
      _work_rows--;
    }
  }
}

// is first constraint or second redundant
// if first is redundant return 1
// if second is redundant return 2
// otherwise return 0
// All cols < min_var assumed to be 0

INT SYSTEM_OF_EQUATIONS::Is_Simple_Redundant(const mINT32* row1,
                                             const mINT32* row2,
                                             INT64 row1c,
                                             INT64 row2c,
                                             INT min_var,
                                             INT cols)
{
  BOOL has_multiple = FALSE;
  INT32 mult_n=1, mult_d=1;

  for (INT col = min_var; col < cols; col++) {
    if (row1[col] == 0) {
      if(row2[col] != 0) return(0);
    }
    else if (row2[col] == 0) {
      if(row1[col] != 0) return(0);
    }
    else if (has_multiple) {
      if ((INT64) mult_n * row2[col] != (INT64) mult_d*row1[col]) return(0);
    }
    else {
      has_multiple = TRUE;
      mult_n = row1[col];
      mult_d = row2[col];
      if ((mult_n < 0) != (mult_d < 0)) return(0);
    }
  }

  if (abs(row1c) > INT32_MAX) return(0);  // overflow
  if (abs(row2c) > INT32_MAX) return(0);  // overflow


  // it is redundant find out which constraint is tighter
  if (mult_n == mult_d) {
    if (row1c <= row2c) {
      return 2; // 1 is tighter
    } else {
      return 1;
    }
  } else {
    if (mult_d > 0 && mult_d*row1c <= mult_n*row2c 
	|| mult_d < 0 && mult_d*row1c >= mult_n*row2c) {
      return 2; // 1 is tighter
    } else {
      return 1;
    }
  }
}

// is first constraint or second redundant
// if first is redundant return 1
// if second is redundant return 2
// if tie, return 3
// otherwise return 0
// All cols < min_var assumed to be 0

INT SYSTEM_OF_EQUATIONS::Simple_Redundant(const mINT32* row1,
					  const mINT32* row2,
					  INT64 row1c,
					  INT64 row2c,
					  INT min_var,
					  INT cols)
{
  BOOL has_multiple = FALSE;
  INT32 mult_n=1, mult_d=1;

  for (INT col = min_var; col < cols; col++) {
    if (row1[col] == 0) {
      if(row2[col] != 0) return(0);
    }
    else if (row2[col] == 0) {
      if(row1[col] != 0) return(0);
    }
    else if (has_multiple) {
      if ((INT64) mult_n * row2[col] != (INT64) mult_d*row1[col]) return(0);
    }
    else {
      has_multiple = TRUE;
      mult_n = row1[col];
      mult_d = row2[col];
      if ((mult_n < 0) != (mult_d < 0)) return(0);
    }
  }

  if (abs(row1c) > INT32_MAX) return(0);  // overflow
  if (abs(row2c) > INT32_MAX) return(0);  // overflow

  // it is redundant find out which constraint is tighter
  if (mult_n == mult_d) {
    if (row1c < row2c) {
      return 2; // 1 is tighter
    } else if (row2c < row1c) {
      return 1;
    } else 
      return 3;
    
  } else {
    if (mult_d*row1c < mult_n*row2c) {
      return 2; // 1 is tighter
    } else if (mult_d*row1c > mult_n*row2c) {
      return 1;
    } else
      return 3;

  }
}

// Copy all the 'le' constraints from 'from' to 'to' inclusive 
// into the work space
// Return FALSE if not enough space
BOOL SYSTEM_OF_EQUATIONS::Copy_To_Work(const INT32 from, const INT32 to)
{
  if (_work_rows + (to-from+1) > SOE_MAX_WORK_ROWS) return(FALSE);
  if (_work_rows ==0) {
    if (_vars > SOE_MAX_WORK_COLS) return(FALSE);
    _work_cols = _vars;
  } else {
    FmtAssert(_vars==_work_cols,("Inconsistency in Copy_To_Work"));
  }
  INT32 initial_work_rows = _work_rows;

  INT32 i;
  for (i=from; i<= to; i++ ) {
    for (INT32 j=0; j<_work_cols; j++) {
      _work[_work_rows][j] = _Ale(i,j);
    }
    _work_rows++;
  }
  _work_rows = initial_work_rows;
  for (i=from; i <= to; i++) {
    _work_const[_work_rows++] = _ble[i];
  }
  return(TRUE);
} 

// Copy the inverse of constraint i into the work space
// Return FALSE if not enough space
BOOL SYSTEM_OF_EQUATIONS::Copy_Inverse_To_Work(const INT32 i)
{
  if (_work_rows + 1 > SOE_MAX_WORK_ROWS) return(FALSE);
  if (_work_rows ==0) {
    if (_vars > SOE_MAX_WORK_COLS) return(FALSE);
    _work_cols = _vars;
  } else {
    FmtAssert(_vars==_work_cols,("Inconsistency in Copy_Inverse_To_Work"));
  }

  for (INT32 j=0; j<_vars; j++) {
      _work[_work_rows][j] = -_Ale(i,j);
  }
  _work_const[_work_rows] = -_ble[i]-1;
  _work_rows++;
  return(TRUE);
} 

// Copy all the 'eq' constraints into the equal work space
// Return FALSE if not enough space
BOOL SYSTEM_OF_EQUATIONS::Copy_To_Work_Eq()
{
  if (_eqns_eq > SOE_MAX_WORK_ROWS_EQ) return(FALSE);
  if (_work_rows ==0) {
    if (_vars > SOE_MAX_WORK_COLS) return(FALSE);
    _work_cols = _vars;
  } else {
    FmtAssert(_vars==_work_cols,("Inconsistency in Copy_Inverse_To_Work"));
  }
  _work_rows_eq = _eqns_eq;

  for (INT32 i=0; i< _eqns_eq; i++ ) {
    _work_const_eq[i] = _beq[i];
    for (INT32 j=0; j<_work_cols; j++) {
      _work_eq[i][j] = _Aeq(i,j);
    }
  }
  return(TRUE);
} 

// Is the systm of inequalities in the work array consistent
// Can always conservatively return TRUE
// Use the SVPC test followed by Acyclic test followed by Fourier Motzkin
BOOL SYSTEM_OF_EQUATIONS::Is_Consistent_Work()
{
  INT debug = 0;
  if (Get_Trace(TP_LNOPT,TT_LNO_DEP2)) {
    debug = 2;
  }

  SVPC_RESULT result = SVPC();
  if (result == SVPC_CONSISTENT) {
    if (debug >= 2) fprintf(TFile,"SVPC returns consistent \n");
    return (TRUE);
  } else if (result == SVPC_INCONSISTENT) {
    if (debug >= 2) fprintf(TFile,"SVPC returns inconsistent \n");
    return(FALSE);
  }

  ACY_RESULT acy_result = Acyclic_Test();
  if (acy_result == ACY_CONSISTENT) {
    if (debug >= 2) fprintf(TFile,"acyclic returns consistent \n");
    return (TRUE);
  } else if (acy_result == ACY_INCONSISTENT) {
    if (debug >= 2) fprintf(TFile,"acyclic returns inconsistent \n");
    return(FALSE);
  }

  Gcd_Normalize();

  BOOL proved_inconsistent;
  for (INT32 i=0; i<_work_cols-1; i++) {
    if (!Project(i,&proved_inconsistent,i)) {
      if (debug >= 2) fprintf(TFile,"fourier returns consistent \n");
      return(TRUE);  // Project failed
    } else if (proved_inconsistent) {
      if (debug >= 2) fprintf(TFile,"fourier returns inconsistent \n");
      return(FALSE); // Project proved the system inconsistent
    }
  }
  BOOL four_result = One_Var_Consistent(_work_cols-1,0,_work_rows-1);
  if (debug >= 2) {
    if (four_result) fprintf(TFile,"fourier returns consistent \n");
    else fprintf(TFile,"fourier returns inconsistent \n");
  }
  return four_result;
}

// Normalize all the le constraints by their gcd, tighten the constant
void SYSTEM_OF_EQUATIONS::Gcd_Normalize()
{

  INT32 g = 0;
  for (INT i=0; i<_work_rows; i++) {
    INT j=0;
    g = 0;

    while (j<_work_cols && !_work[i][j]) j++;
    if (j<_work_cols) g = abs(_work[i][j]);

    for (j=j+1; j<_work_cols; j++) {
      g = Gcd(g,abs(_work[i][j]));
    }

    if (g && g != 1) {
      for (INT j=0; j<_work_cols; j++) {
        _work[i][j] = _work[i][j] / g;
      }
      if (_work_const[i] > 0 || ((_work_const[i] % g) == 0)) {
	_work_const[i] = _work_const[i] / g;
      } else {
	_work_const[i] = _work_const[i] / g -1;
      }
    }
  }
}


// Perform the SVPC (Single Variable per Constraint) test
// Also set _lower and _upper_bound to be used by the Acyclic test
SVPC_RESULT SYSTEM_OF_EQUATIONS::SVPC()
{
  BOOL applicable = TRUE;
  // initialize the bounds
  INT32 i;
  for (i=0; i<_work_cols; i++) {
    _lower_bound[i].Set_Neg_Infinite();
    _upper_bound[i].Set_Infinite();
  }

  for (i=0; i<_work_rows; i++) {
    INT32 num_vars = 0, var_pos=0;
    BOOL applicable_i = TRUE; // is constraint 'i' an SVPC constraint
    for (INT32 j=0; j<_work_cols; j++) {
      if (_work[i][j] != 0) {
	if (num_vars) {
	  applicable = FALSE;
	  applicable_i = FALSE;
	}  
	num_vars = 1;
	var_pos = j;
      }
    }
    if (num_vars == 0) {
      if (_work_const[i] < 0) {
	return(SVPC_INCONSISTENT);
      }
    } else if (applicable_i && !One_Var_Consistent(var_pos,i,i)) {
      return(SVPC_INCONSISTENT);
    }
    _is_svpc[i] = applicable_i;
  }
  if (!applicable) {
    return(SVPC_INAPPLICABLE);  
  } else {
    return(SVPC_CONSISTENT);
  }
}

// Constraints from to to only refer to variable var
// Are they inconsistent (with themselves or with the current bounds)
// This routine assumes that _lower_bound and _upper_bound are valid on entry
// This routine changes _lower_bound and _upper_bound
BOOL SYSTEM_OF_EQUATIONS::One_Var_Consistent(INT32 var, INT32 from, INT32 to)
{
#ifdef KEY 
  // Bug 2927 - SVPC does not handle overflow correctly. When wrap-around
  // optimization is off, turn off SVPC by always returning consistent 
  // loop bounds.
  if (! Allow_wrap_around_opt ) return (TRUE);
#endif
  for (INT32 i=from; i<=to; i++) {
    if ((_work_const[i] >= (INT32_MAX-1)) || (_work_const[i]<=(INT32_MIN+1))) {
      return(TRUE);  // Overflow
    }
    SVPC_Set_Bound(i,var);
    if (_lower_bound[var] > _upper_bound[var]) {
      return(FALSE);
    }
  }
  return(TRUE);
}

// Update the lower or upper bound given constraint i, which only uses
// variable var
// assumes the constant fits in 32 bits
void SYSTEM_OF_EQUATIONS::SVPC_Set_Bound(INT32 i, INT32 var)
{
  INT32 tmp;
  if (_work[i][var] > 0) { // upper bound = _work_const[i]/_work[i][var]
			   // rounded towards negative infinite

    if ((_work_const[i] >= 0) || (_work_const[i] % _work[i][var] == 0)) {
      tmp = _work_const[i] / _work[i][var];
    } else {
      tmp  = -(-_work_const[i] / _work[i][var]) -1;
    }
    if (tmp < _upper_bound[var]) _upper_bound[var] = tmp;

  } else if (_work[i][var] < 0) {//lower bound = -_work_const[i]/_work[i][var] 
				 // rounded towards positive infinite
    if ((_work_const[i] >= 0) || (_work_const[i] % _work[i][var] == 0)) {
      tmp = -(_work_const[i] / (-_work[i][var]));
    } else {
      tmp = (-_work_const[i] / -_work[i][var]) + 1;
    }
    if (tmp > _lower_bound[var]) _lower_bound[var] = tmp;
  } else {  // _work[i][var] == 0, NULL constraint
      if (_work_const[i] < 0) { // inconsistent so set LB > UB
	_lower_bound[var] = 1;
	_upper_bound[var] = 0;
      }
  }
}

// The Acyclic Test (see Maydan's thesis for explanation)
// We go through the constraints looking for leaves to substitute
// If we make it a full cycle without finding anything, 
//	we return ACY_INAPPLICABLE
// If we make it a full cycle with everything substituted, we're done
ACY_RESULT SYSTEM_OF_EQUATIONS::Acyclic_Test()
{
  INT sign=0;
  BOOL inconsistent;

  BOOL *var_substituted = CXX_NEW_ARRAY(BOOL,_work_cols,_pool);

  for (INT i=0; i<_work_cols; i++) {
    var_substituted[i] = FALSE;
  }

  BOOL subbed_one = TRUE;
  BOOL failed_one = TRUE;
  while (subbed_one && failed_one) { // look for a variable to substitute
    subbed_one = FALSE;              // keep going while we're making progress
    failed_one = FALSE;
    for (INT i=0; i<_work_cols; i++) {  // check variable i
      if (!var_substituted[i]) {  // has it already been subbed away
        if (Var_Leaf(i,&sign)) { // is it subbable
	  var_substituted[i] = TRUE;
          if (sign > 0) {
	    subbed_one = TRUE;
            Acy_Set_Var(i,_lower_bound[i],&inconsistent);
            if (inconsistent) {
	      CXX_DELETE_ARRAY(var_substituted,_pool);
              return(ACY_INCONSISTENT);
            }
          } else if (sign < 0) {
	    subbed_one = TRUE;
            Acy_Set_Var(i,_upper_bound[i],&inconsistent);
            if (inconsistent) {
	      CXX_DELETE_ARRAY(var_substituted,_pool);
              return(ACY_INCONSISTENT);
            }
          }
        } else {
	  failed_one = TRUE;
        }
      }
    }
  }
  if (failed_one) {
    return(ACY_INAPPLICABLE);
  }
  return (ACY_CONSISTENT);  // didn't fail any, so subbed everything
}

// Is var 'i' a leaf, i.e. looking at the non-svpc constraints, is it
// only constrained in one direction.  Return 1 if it is, 0 otherwise.
// if it is, set *sign to 1 if we're 
// multiplying the variable by a positive coefficient, -1 if by
// a negative coefficient, 0 if the variable is never used
INT SYSTEM_OF_EQUATIONS::Var_Leaf(INT i, INT *sign)
{
  INT s=0;
  for (INT row=0; row<_work_rows; row++) {
    if (!_is_svpc[row]) {
      if (_work[row][i] > 0) {
        if (s == -1) return 0;  // not a leaf
        s = 1;
      } else if (_work[row][i] < 0) {
        if (s == 1) return 0;  // not a leaf
        s = -1;
      }
    }
  }
  *sign = s;
  return(1);
}

// Set variable 'i' to val
// If a constraint becomes an SVPC constraint, update _lower_bound or
// _upper_bound and set _is_svpc
// set *inconsistent if the system has been proven inconsistent
void SYSTEM_OF_EQUATIONS::Acy_Set_Var(INT i,INT32_INFIN val,BOOL *inconsistent)
{
  BOOL is_infin = (val.Is_Infinite() || val.Is_Neg_Infinite());
  INT v;
  if (!is_infin) {
    v = val.Value();
  } 

  for (INT row=0; row<_work_rows; row++) {
    if (!_is_svpc[row]) {
      if (_work[row][i]) {
        if (is_infin) { // zero the row, this can always be satisfied
	  _work_const[row] = 0;
	  for (INT j=0; j<_work_cols; j++) {
	    _work[row][j] = 0;
          }
	  _is_svpc[row] = TRUE;
        } else { // set the row
	  if (_work[row][i] == 1) {
	    _work_const[row] -= v;
	  } else if (_work[row][i] == -1) {
	    _work_const[row] += v;
          } else {
	    _work_const[row] -= _work[row][i]*v;
          }
	  _work[row][i] = 0;

	  // has this become a SVPC constraint
	  INT32 num_vars=0, var_pos=0;
	  for (INT j=0; j<_work_cols && (num_vars <= 1); j++) {
	    if (_work[row][j] != 0) {
	      num_vars++;
	      var_pos = j;
            }
          }
	  if (num_vars == 1) {
	    if (!One_Var_Consistent(var_pos,row,row)) {
	      *inconsistent = TRUE;
	      return;
            }
	    _is_svpc[row] = TRUE;
          }
        }
      }
    }
  }
  *inconsistent = FALSE;
}

// Routines to help with subbing in equality constraints

// Substitute in the last equality constraint in the work equality array
// return FALSE on failure (32 bit overflow)
BOOL SYSTEM_OF_EQUATIONS::Sub_Last_Equal(BOOL *proved_inconsistent)
{
  INT32 eqnum = _work_rows_eq - 1;

  // find the first non-zero coefficient
  INT32 i;
  for (i=0; i<_work_cols && _work_eq[eqnum][i] == 0; i++);
  if (i == _work_cols) { // no variables constraint
    if (_work_const_eq[eqnum] != 0) {
      *proved_inconsistent = TRUE;
    }
    _work_rows_eq--;
    return(TRUE);
  }

  // first check if the first non-zero coefficient is +- 1
  // substitution is trivial
  if (abs(_work_eq[eqnum][i]) == 1) {  // common, easy case
    if (!Sub_Last_Equal_Unary(i,i)) return(FALSE);;
    _work_rows_eq--;
    return(TRUE);
  }

  // Next normalize and check if min coefficient is +- 1
  INT32 smallest_var = Normalize_Eq_and_Find_Smallest
				(eqnum,i,proved_inconsistent);
  if (*proved_inconsistent) return(TRUE);
  if (abs(_work_eq[eqnum][smallest_var]) == 1) {  
    if (!Sub_Last_Equal_Unary(smallest_var,i)) return(FALSE);
    _work_rows_eq--;
    return(TRUE);
  }

  // add in Pugh's extra variable and constraint
  Add_Work_Var();
  INT32 m = abs(_work_eq[eqnum][smallest_var]) + 1;
  _work_rows_eq++; eqnum++;

  _work_eq[eqnum][_work_cols-1] = m;
  for (i=0; i<_work_cols-1; i++) {
    _work_eq[eqnum][i] = -Mod_Hat(_work_eq[eqnum-1][i],m);
  }
  _work_const_eq[eqnum] = -Mod_Hat(_work_const_eq[eqnum-1],m);
  return(TRUE);
}

// Sub in the last equality constraint
// Given that _work_eq[_work_rows_eq-1][i] = +- 1
// Given that _work_eq[_work_rows_eq-1][0..min_var-1] = 0
// Return FALSE on failure (overflow of 32 bits)
BOOL SYSTEM_OF_EQUATIONS::Sub_Last_Equal_Unary(const INT32 i,const INT32 minvar)
{
  INT32 eqnum = _work_rows_eq - 1;
  if (_work_eq[eqnum][i] == -1) {  // multiply both sides by -1
    for (INT32 j=minvar; j<_work_cols; j++) {
      _work_eq[eqnum][j] = -_work_eq[eqnum][j];
    }
    _work_const_eq[eqnum] = -_work_const_eq[eqnum];
  }

  // sub in to the other equality constraints
  for (INT32 eq=0; eq<eqnum; eq++) {
    INT64 mult = (INT64) _work_eq[eq][i];
    // _work_eq[eq][...] += -mult*_work_eq[eqnum][...]
    if (mult == 0) {  // avoid the multiplication in the simple case
    } else if (mult == 1) { 
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work_eq[eq][j] - (INT64) _work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work_eq[eq][j] = (INT32) tmp;
      }
      _work_const_eq[eq] -= _work_const_eq[eqnum];
    } else if (mult == -1) {
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work_eq[eq][j] + (INT64) _work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work_eq[eq][j] = (INT32) tmp;
      }
      _work_const_eq[eq] += _work_const_eq[eqnum];
    } else {
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work_eq[eq][j] - mult*_work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work_eq[eq][j] = (INT32) tmp;
      }
      _work_const_eq[eq] -= mult*_work_const_eq[eqnum];
    }
    _work_eq[eq][i] = 0;
  }

  // sub into the inequality constraints
  for (INT32 le=0; le<_work_rows; le++) {
    INT32 mult = _work[le][i];
    if (mult == 0) {
    } else if (mult == 1) {  // avoid the multiplication in the simple case
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work[le][j] - (INT64) _work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work[le][j] = (INT32) tmp;
      }
      _work_const[le] -= _work_const_eq[eqnum];
    } else if (mult == -1) {
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work[le][j] + (INT64) _work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work[le][j] = (INT32) tmp;
      }
      _work_const[le] += _work_const_eq[eqnum];
    } else {
      for (INT32 j=minvar; j<_work_cols; j++) {
	INT64 tmp = (INT64) _work[le][j] - mult*_work_eq[eqnum][j];
	if (abs(tmp) > INT32_MAX) return(FALSE);
	_work[le][j] = (INT32) tmp;
      }
      _work_const[le] -= mult*_work_const_eq[eqnum];
    }
    _work[le][i] = 0;
  }
  return(TRUE);
}

// Normalize the euqation (divide by the gcd of coefficients)
// Return the position of the coefficient with the minimum absolute value
// Set inconsistent if the constant is not a multiple of the gcd
// Given that _work_eq[eqnum][0..minvar-1] = 0 and _work_eq[eqnum][minvar] != 0
INT32 SYSTEM_OF_EQUATIONS::Normalize_Eq_and_Find_Smallest
			(INT32 eqnum,INT32 minvar,BOOL *proved_inconsistent)
{
  INT32 g = abs(_work_eq[eqnum][minvar]);
  INT32 smallest = g;
  INT32 smallest_pos = minvar;

  Is_True(g != 0,
   ("Bad minvar for SYSTEM_OF_EQUATIONS::Normalize_Eq_and_Find_Smallest"));

  // find the gcd and the smallest guy
  INT32 i;
  for (i=minvar+1; i<_work_cols; i++) {
    INT32 tmp = abs(_work_eq[eqnum][i]);
    if (tmp != 0) {
      g = Gcd(g,tmp);
      if (tmp < smallest) {
        smallest = tmp;
        smallest_pos = i;
      }
    }
  }

  if (abs(_work_const_eq[eqnum]) % g != 0) {
    *proved_inconsistent = TRUE;
    return(-1);
  }

  // divid through by gcd
  _work_const_eq[eqnum] /= g;
  for (i=minvar; i<_work_cols; i++) {
    _work_eq[eqnum][i] /= g;
  }
  return(smallest_pos);
}

void SYSTEM_OF_EQUATIONS::Take_Gcds()
{
  INT i;
  for (i = 0; i < _eqns_le; i++) {
    INT64 g = _Ale(i,0);
    INT j;
    for (j = 1; j < _vars; j++)
      g = Gcd(g, (INT64)_Ale(i,j));
    if (g == 0)
      continue; 
    FmtAssert(g>0, ("Take_Gcds(): impossible gcd %lld", g));
    for (j = 0; j < _vars; j++)
      _Ale(i,j) /= (INT32)g;
    if (_ble[i] >= 0)
      _ble[i] = _ble[i]/(INT32)g;
    else
      _ble[i] = -((-_ble[i] + g - 1)/(INT32)g);
  }

  for (i = 0; i < _eqns_eq; i++) {
    INT64 g = _beq[i];
    INT j;
    for (j = 0; j < _vars; j++)
      g = Gcd(g, (INT64)_Aeq(i,j));
    if (g == 0)
      continue; 
    FmtAssert(g>0, ("Take_Gcds(): impossible gcd %lld", g));
    for (j = 0; j < _vars; j++)
      _Aeq(i,j) /= (INT32)g;
    _beq[i] /= (INT32)g;
  }
}

// Return a-b*floor(a/b+1/2)
INT32 SYSTEM_OF_EQUATIONS::Mod_Hat(INT32 a, INT32 b)
{
  INT32 num = 2*a+b;
  INT32 den = 2*b;
  INT32 floor;
  if (((num >= 0) &&  (den >0)) || ((num <= 0) &&  (den <0))) {
    floor = num/den;
  } else if ((num % den) == 0) {
    floor = num/den;
  } else {
    floor = num/den-1;
  }
  return(a-b*floor);
}

// add an extra zero column to the work arrays
// return FALSE on failure (lack of space)
BOOL SYSTEM_OF_EQUATIONS::Add_Work_Var()
{
  if (_work_cols >= SOE_MAX_WORK_COLS) return(FALSE);

  INT32 i;
  for (i=0; i<_work_rows; i++) {
    _work[i][_work_cols] = 0;
  }
  for (i=0; i<_work_rows_eq; i++) {
    _work_eq[i][_work_cols] = 0;
  }
  _work_cols++;
  return(TRUE);
}

void SYSTEM_OF_EQUATIONS::Reset_To(INT32 nrows_le, INT32 nrows_eq, INT32 ncols)
{
  INT32 nle = Num_Le_Constraints() - nrows_le;
  INT32 neq = Num_Eq_Constraints() - nrows_eq;
  INT32 nvr = Num_Vars() - ncols;

  if (nle > 0)	Remove_Last_Le(nle);
  else		Is_True(nle == 0, ("Reset_To can't add Ale rows"));

  if (neq > 0)	Remove_Last_Eq(neq);
  else		Is_True(neq == 0, ("Reset_To can't add Aeq rows"));

  if (nvr > 0)	Remove_Last_Vars(nvr);
  else		Is_True(nvr == 0, ("Reset_To can't add variables"));
}

// get space for the statics
mINT32 SYSTEM_OF_EQUATIONS::_work[SOE_MAX_WORK_ROWS][SOE_MAX_WORK_COLS];
mINT64 SYSTEM_OF_EQUATIONS::_work_const[SOE_MAX_WORK_ROWS];
mINT32 SYSTEM_OF_EQUATIONS::_work_rows;
mINT32 SYSTEM_OF_EQUATIONS::_work_cols;
BOOL SYSTEM_OF_EQUATIONS::_is_redundant[SOE_MAX_WORK_ROWS];
BOOL SYSTEM_OF_EQUATIONS::_is_svpc[SOE_MAX_WORK_ROWS];
INT32_INFIN SYSTEM_OF_EQUATIONS::_lower_bound[SOE_MAX_WORK_COLS];
INT32_INFIN SYSTEM_OF_EQUATIONS::_upper_bound[SOE_MAX_WORK_COLS];
mINT32 
 SYSTEM_OF_EQUATIONS::_work_eq[SOE_MAX_WORK_ROWS_EQ][SOE_MAX_WORK_COLS];
mINT64 SYSTEM_OF_EQUATIONS::_work_const_eq[SOE_MAX_WORK_ROWS_EQ];
mINT32 SYSTEM_OF_EQUATIONS::_work_rows_eq;

void SYSTEM_OF_EQUATIONS::Remove_Eq_Number(INT k)
{
  for (INT i = k+1; i < _eqns_eq; i++) {
    for (INT j = 0; j < _vars; j++)
      Aeq()(i-1,j) = Aeq()(i,j);
    Beq()[i-1] = Beq()[i];
  }
  _eqns_eq--;
}

void SYSTEM_OF_EQUATIONS::Remove_Le_Number(INT k)
{
  for (INT i = k+1; i < _eqns_le; i++) {
    for (INT j = 0; j < _vars; j++)
      Ale()(i-1,j) = Ale()(i,j);
    Ble()[i-1] = Ble()[i];
  }
  _eqns_eq--;
}

//=============================================================================
// Move index variable at Coeff(i) out of the base by switching one of the
// axle from [0:dim-1] into the base.
// After the base change, Coeff(i) will become zero for all the rows in the
// equalities and inequalities.
// This will only handle the case where Coeff(i) can has non-zero values only 
// in the equality constraints. The more general cases need HNF.
// Return TRUE is successful and FALSE if the change of base resulting in
// lose of information.
//=============================================================================
INT
SYSTEM_OF_EQUATIONS::Change_Base(const INT32 dim, const INT32 pos, MEM_POOL *pool)
{

  INT pivot_row = -1;
  INT pivot_column = dim+pos;

  INT32 i;
  for (i = 0; i < _eqns_eq; ++i) {
    if (_Aeq(i,pivot_column) != 0) {
      if (pivot_row < 0)
	pivot_row = i;
      else if (abs(_Aeq(i,pivot_column)) < abs(_Aeq(pivot_row,pivot_column))) 
	pivot_row = i;
    }
  }

  // For now, just return.  We will have to look into the
  // inequalities for the more complicated cases.
  if (pivot_row < 0) return pivot_row;

  INT64 *work = CXX_NEW_ARRAY(INT64, _vars, pool);
  INT64 pivot_value = _Aeq(pivot_row,pivot_column);

  // Change the base with the pivot_row
  for (i = 0; i < _eqns_eq; ++i) {
    if (i!=pivot_row && _Aeq(i,pivot_column)!=0) {
      INT32 k;
      for (k = 0; k <_vars; ++k) {
	work[k] = - pivot_value*_Aeq(i,k) 
	  + _Aeq(i,pivot_column)*_Aeq(pivot_row,k);
      }

      _beq[i] = - pivot_value*_beq[i] +	_Aeq(i,pivot_column)*_beq[pivot_row];

      if (abs(_beq[i]) > INT32_MAX) {
	CXX_DELETE(work, pool);
	return -1;
      }

      INT32 g = abs(_beq[i]);

      for (k = 0; k <_vars; ++k) {
	if (abs(work[k] <= INT32_MAX)) {
	  _Aeq(i,k) = work[k];
	  g = Gcd(g,abs(work[k]));
	} else {
	  CXX_DELETE(work, pool);
	  return -1;
	}
      }
      if (g>1) {
	for (INT32 k = 0; k < _vars; ++k)
	  _Aeq(i,k) = _Aeq(i,k)/g;
	_beq[i] = _beq[i]/g;
      }
    }
  }
  
  // Update the inequalities
  for ( i = 0; i < _eqns_le; ++i) {
    if (_Ale(i,pivot_column)!=0) {
      INT32 k;
      for (k = 0; k <_vars; ++k) {
	if (pivot_value < 0) {
	  work[k] = - pivot_value*_Ale(i,k)
	    + _Ale(i,pivot_column)*_Aeq(pivot_row,k);
	} else {
	  work[k] = pivot_value*_Ale(i,k)
	    - _Ale(i,pivot_column)*_Aeq(pivot_row,k);
	}
      }
      if (pivot_value < 0) {
	_ble[i] = - pivot_value*_ble[i] + _Ale(i,pivot_column)*_beq[pivot_row];
      } else {
	_ble[i] =   pivot_value*_ble[i] - _Ale(i,pivot_column)*_beq[pivot_row];
      }

      if (abs(_ble[i]) > INT32_MAX) {
	CXX_DELETE(work, pool);
	return FALSE;
      }

      INT32 g = abs(_ble[i]);
      
      for (k = 0; k < _vars; ++k) {
	if (abs(work[k] <= INT32_MAX)) {
	  _Ale(i,k) = work[k];
	  g = Gcd(g, abs(work[k]));
	} else {
	  CXX_DELETE(work, pool);
	  return -1;
	}
      }
      if (g>1) {
	for (INT32 k = 0; k < _vars; ++k)
	  _Ale(i,k) = _Ale(i,k)/g;
	_ble[i] = _ble[i]/g;
      }
    }
  }

  CXX_DELETE(work,pool);
  return pivot_row;
  
}

// This is basically the first half of Elim_Simple_Redundant.
// Mark the simple redundant constraints from the work array
// Simple tests to improve efficiency.
// We consider the two constraints redundant
// if one constraint (ignoring the constant term) is a multiple of the other
// the redundant constraint is the one that is less tight
// All cols < min_var assumed to be 0
INT SYSTEM_OF_EQUATIONS::Mark_Simple_Redundant(BOOL *is_redundant)
{
  INT counter=0;
  static INT16 last_non_zero[SOE_MAX_WORK_ROWS];
  // set last_non_zero to the column position of the last  non-zero elements
  // this is an efficiency aid, redundant constraints must have the same
  // value for this
  INT i;
  for (i=0; i<_work_rows; i++) {
    is_redundant[i] = FALSE;
    INT j = _work_cols - 1;
    while ((j >= 0) && (_work[i][j] == 0)) j--;
    last_non_zero[i] = j;
  }

  // find the redundant constraints
  for (i=0; i<_work_rows; i++) {
    if (!is_redundant[i]) {
      for (INT32 j=i+1; j<_work_rows; j++) {
	if (!is_redundant[j] && (last_non_zero[i] == last_non_zero[j])) {
	  INT redun=Is_Simple_Redundant(&_work[i][0], &_work[j][0],
					_work_const[i], _work_const[j],
					0, _work_cols);
	  if (redun==1) {
	    is_redundant[i] = TRUE;
	    counter++;
	  } else if (redun==2) {
	    is_redundant[j] = TRUE;
	    counter++;
	  }
	}
      }
    }
  }

  return counter++;
}

// Mark redundant new constrains that are already not set in 'is_redundant'.
// To check if constraint[i] is redundant
// Copy the non-redundant of constraints[0..i-1,i+1...] to the work array
// Copy the inverse of constraint [i] into the work array
// If the system is inconsistent then constraint[i] is redundant
// This may conservatively not find some redundancies
INT SYSTEM_OF_EQUATIONS::Mark_New_Redundant(BOOL *is_redundant)
{
  BOOL proved_inconsistent;
  INT counter = 0;

  if (_eqns_le  < 1) return 0;

  for (INT32 i=0; i<_eqns_le; i++) {
    _work_rows = 0;
    // copy non-redundant constraints 0..i-1 into the work array
    INT32 j;
    for (j=0; j<= i-1; j++) {
      if (!is_redundant[j]) {
        if (!Copy_To_Work(j,j)) return counter;
      }
    }
    // copy constraints i.._eqns_le-1 into the work array (can't be redundant)
    for (j =i+1; j<_eqns_le; j++) {
      if (!is_redundant[j]) {
        if (!Copy_To_Work(j,j)) return counter;
      }
    }

    if (!Copy_Inverse_To_Work(i)) return counter;
    if (!Sub_In_Equal(&proved_inconsistent)) return counter;
    if (proved_inconsistent || !Is_Consistent_Work()) {
      is_redundant[i] = TRUE;
      counter++;
    }
  }
  
  return counter;
}

//===================================================================
// Given two sets of bounds that are already interleaved in the SOE:
//    A1  <= 0  lower bound of A's axle 1
//    A1' <= 0  upper bound of A's axle 1
//    B1  <= 0  lower bound of B's axle 1
//    B1' <= 0  upper bound of B's axle 1
//    A2  <= 0  lower bound of A's axle 2
//    A2' <= 0  upper bound of A's axle 2
//    B2  <= 0  lower bound of B's axle 2
//    B2' <= 0  upper bound of B's axle 2
//    ....
// Prove if all the bounds of A (or B) are redundant
//================================================================== 
BOOL SYSTEM_OF_EQUATIONS::Prove_Redundant(const INT set, const INT dim)
{

  Is_True(_eqns_le == 4*dim, ("The number of constraints does not match that of dimensions"));
  INT set1= (set) ? 0 : 1;

  for (INT32 i = 0; i<dim; ++i) {
    _work_rows=0;
    
    // Copy the non-redundant constraints to the work array
    INT32 j;
    for (j = 0; j<dim; ++j) {
      if (!Copy_To_Work(4*j+2*set1,4*j+2*set1)||
	  !Copy_To_Work(4*j+2*set1+1,4*j+2*set1+1))
	return FALSE;
    }

    // lower bound is redundant
    if (!Copy_Inverse_To_Work(4*i+2*set)) return FALSE;
    if (Is_Consistent_Work()) return FALSE;

    _work_rows=0;
    
    // Copy the non-redundant constraints to the work array
    for (j = 0; j<dim; ++j) {
      if (!Copy_To_Work(4*j+2*set1,4*j+2*set1)||
	  !Copy_To_Work(4*j+2*set1+1,4*j+2*set1+1))
	return FALSE;
    }
    
    // upper bound is redundant
    if (!Copy_Inverse_To_Work(4*i+2*set+1)) return FALSE;
    if (Is_Consistent_Work()) return FALSE;
  }

  return TRUE;

}
