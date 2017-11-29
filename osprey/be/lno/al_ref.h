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
 * Module: al_ref.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description:
 * 
 *    Representation of references to reshaped arrays.
 *
 *  Exported Types:
 *
 *      ARRAY_LOWER_REF 
 *        
 *        Represents a reference to a reshaped array.  The reference can either be
 *        an array access or the entire array can be passed as a parameter.
 *
 *
 *  Exported Functions:
 *
 *      ARRAY_LOWER_REF(WN *array_ref, DISTR_ARRAY *dact, 
 *		        ARRAY_LOWER_LOOP *parent_loop, mBOOL is_param = FALSE)
 *          
 *        Constructor for ARRAY_LOWER_REF.  All fields are passed in.
 *
 *     ~ARRAY_LOWER_REF(void)
 *
 *        Destructor for ARRAY_LOWER_REF.
 *
 *      WN *Array_Ref(void) const
 *    
 *        Returns the array reference.
 *
 *     DISTR_ARRAY *Dact(void) const 
 * 
 *        Returns the dact for the array referenced. 
 *
 *     ARRAY_LOWER_LOOP *Parent_Loop(void) const
 * 
 *        Returns the loop enclosing this array reference.
 *
 *     mBOOL Is_Param(void) const 
 *
 *        Returns TRUE if the reference is a parameter, FALSE if the reference is
 *        an array access.
 * 
 *     void Lower_Ref(void)
 *
 *        Lower the array reference. 
 * 
 * ====================================================================
 * ====================================================================
 */
#ifndef al_ref_INCLUDED
#define al_ref_INCLUDED

#ifdef _KEEP_RCS_ID
static char *al_ref_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "wn.h"
#include "lego_pragma.h"

class ARRAY_LOWER_LOOP; // Defined in al_loop.h

class ARRAY_LOWER_REF {
private:
  WN *_array_ref;                   // pointer to whirl node for array expr
  DISTR_ARRAY *_dact;               // pointer to DACT for this array expr
  ARRAY_LOWER_LOOP *_parent_loop;   // back-pointer to loop containing this ref
  mBOOL _is_param;                  // is the array reference a parameter?

  ARRAY_LOWER_REF(void);
  ARRAY_LOWER_REF(const ARRAY_LOWER_REF&);
  ARRAY_LOWER_REF& operator=(const ARRAY_LOWER_REF&);

  ARRAY_LOWER_LOOP *Get_Ref_Loop(INT dim,INT64 *ref_stride,INT64 *ref_offset);

  void Lower_Array_Expr(void);
  void Lower_Array_Param(void);

  mBOOL Is_Block_Dim_Local(INT dim);
  void Create_Block_Dim(INT dim, WN *distr_addr, WN *local_addr, 
			INT *curr_distr, INT *curr_local);
  void Create_Remote_Block_Dim(INT dim, WN *distr_addr, WN *local_addr,
			       INT *curr_distr, INT *curr_local);
  void Create_Local_Block_Dim(INT dim, WN *distr_addr, WN *local_addr,
			      INT *curr_distr, INT *curr_local);

  mBOOL Is_Cyclic_Dim_Local(INT dim);
  void Create_Cyclic_Dim(INT dim, WN *distr_addr, WN *local_addr,
			 INT *curr_distr, INT *curr_local);
  void Create_Remote_Cyclic_Dim(INT dim, WN *distr_addr, WN *local_addr,
				INT *curr_distr, INT *curr_local);
  void Create_Local_Cyclic_Dim(INT dim, WN *distr_addr, WN *local_addr,
			       INT *curr_distr, INT *curr_local);

  mBOOL Is_Blkcyc_Dim_Local(INT dim);
  void Create_Blkcyc_Dim(INT dim, WN *distr_addr, WN *local_addr,
			 INT *curr_distr, INT *curr_local, WN *chunk_nd);
  void Create_Remote_Blkcyc_Dim(INT dim, WN *distr_addr, WN *local_addr,
				INT *curr_distr, INT *curr_local, WN *chunk_nd);
  void Create_Local_Blkcyc_Dim(INT dim, WN *distr_addr, WN *local_addr,
			       INT *curr_distr, INT *curr_local, WN *chunk_nd);

  void Create_Star_Dim(INT dim, WN *local_addr, INT *curr_local);

public: 
  ARRAY_LOWER_REF(WN *array_ref, DISTR_ARRAY *dact, 
		  ARRAY_LOWER_LOOP *parent_loop, mBOOL is_param = FALSE);
  ~ARRAY_LOWER_REF(void);

  WN *Array_Ref(void) const { return _array_ref; }
  DISTR_ARRAY *Dact(void) const { return _dact; }
  ARRAY_LOWER_LOOP *Parent_Loop(void) const { return _parent_loop; }
  mBOOL Is_Param(void) const {return _is_param; }
 
  void Lower_Ref(void);
};

#endif /* al_ref_INCLUDED */

