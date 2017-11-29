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

/**
*** Module: cxx_graph.cxx
*** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:35-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_graph.cxx $
*** 
*** Revision history:
*** 
***     21-OCT-94 dkchen - Original Version
***     10-NOV-94 dkchen - Make DIRECTED_GRAPH16 a template class and move
***                        it to graph_template.h.
*** 
*** Description:
*** 
*** This file contains definitions for fuctions in the VERTEX16 and EDGE16
*** classes.
*** 
**/

#include "cxx_graph.h"
#include "cxx_memory.h"
#include "errors.h"

const VINDEX16 INVALID_VINDEX16=0xffff;
VINDEX16 GRAPH16_CAPACITY = 0xfffe;

VERTEX16&
VERTEX16::operator=(const VERTEX16& v) {

  // copy evverything

  _from = v._from;
  _to = v._to;

  return *this;
}

EDGE16&
EDGE16::operator=(const EDGE16& e) {

  // copy evverything

  _from = e._from;
  _to = e._to;
  _nfrom = e._nfrom;
  _nto = e._nto;

  return *this;
}

