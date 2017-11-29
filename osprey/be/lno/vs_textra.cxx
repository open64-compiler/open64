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
*** Vector space member functions and friends for which template functions are
*** not adequate.  Thus 'textra' => 'template extra'.
**/

/** $Revision: 1.6 $
*** $Date: 04/12/21 14:57:16-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.vs_textra.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define vs_textra_CXX      "vs_textra.cxx"
const static char *rcs_id =   vs_textra_CXX "$Revision: 1.6 $";


#include <sys/types.h>
#include "lnopt_main.h"
#include "vs.h"
#include "lnoutils.h"
#include "frac.h"
#include "ipa_lno_util.h"

const char* _Skip_Whitespace(const char* cp)
{
  INT c;

  while ((c = *cp) != '\0' && c != ' ' && c != '\n' && c != '\t')
    cp++;

  return cp;
}

template<>
void VECTOR_SPACE<double>::Print_Element(FILE* f, double e)
{
  fprintf(f, "%g", e);
}

template<>
void VECTOR_SPACE<FRAC>::Print_Element(FILE* f, FRAC e)
{
  e.Print(f);
}

template<>
void VECTOR_SPACE<double>::Reduce_Row(double*, INT)
{
  return;
}

template<>
void VECTOR_SPACE<FRAC>::Reduce_Row(FRAC* row, INT elts)
{
  INT j;

  // make all entries integral
  FRAC_ETY	lcm_d = 1;
  for (j = 0; j < elts; j++)
    lcm_d = Lcm(lcm_d, row[j].D());
  if (lcm_d != 1) {
    for (j = 0; j < elts; j++) {
      // like row[j] *= lcm_d, but with less chance of overflow
      row[j] = FRAC((lcm_d/row[j].D()) * row[j].N());
    }
  }

  // make smallest integral
  FRAC_ETY	gcd_n = 1;
  for (j = 0; j < elts; j++)
    gcd_n = Gcd(gcd_n, row[j].N());
  if (gcd_n != 1) {
    for (j = 0; j < elts; j++) {
      // like row[j] /= gcd_n, but with less chance of overflow
      row[j] = FRAC(row[j].N() / gcd_n);
    }
  }
}
