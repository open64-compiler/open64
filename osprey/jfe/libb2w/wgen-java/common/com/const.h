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


#ifndef const_INCLUDED
#define const_INCLUDED


#ifndef targ_const_INCLUDED
#include "targ_const.h"
#endif /* targ_const_INCLUDED */

class WN;

extern ST *
New_Const_Sym (TCON_IDX tcon, TY_IDX ty);


extern ST *
Gen_String_Sym (TCON *val, TY_IDX ty, BOOL unique);


extern TCON Const_Val (WN *n);

extern WN *
Make_Const (TCON c);

static const INT32 MAX_SYMBOLIC_CONST_NAME_LEN = 1024;

#ifndef MONGOOSE_BE

extern WN *
Make_Zerocon ( TYPE_ID mtype );

extern WN *
Make_Comparison_Result_Const ( INT16 val );

extern WN *
Make_Integer_Const ( INT16 mtype, TARG_INT val );

#endif /* MONGOOSE_BE */

extern WN *Make_Reduction_Identity ( INT32 opr, TYPE_ID mtype );

#endif /* const_INCLUDED */
