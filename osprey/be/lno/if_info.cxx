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
/* ====================================================================
 * ====================================================================
 *
 * Module: if_info.cxx
 * $Revision: 
 * $Date: 
 * $Author:
 * $Source:
 *
 * Revision history:
 *  20-Nov-96 - Original Version 
 *
 * Description:
 *
 * Basic if information required to analyze loops
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "if_info.h"

void IF_INFO::Print(FILE *fp)
{
  if (Contains_Do_Loops) fprintf(fp,"It has dos \n");
  if (Contains_Regions) fprintf(fp,"It has regions \n");
  if (Condition_On_Then) {
    fprintf(fp,"The access array (on the then) is "); 
    Condition->Print(fp,TRUE);
  } else {
    fprintf(fp,"The access array (on the else) is "); 
    Condition->Print(fp,TRUE);
  }
  fprintf(fp,"\n");
  if (Freq_True>=0.0) 
    fprintf(fp,"True branch taken probability is %f\n", Freq_True);
  if (Freq_False>=0.0)
    fprintf(fp,"False branch taken probablity is %f\n", Freq_False);
}
