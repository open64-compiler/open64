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
/**
*** Module: cxx_graph.h
*** $Revision: 1.6 $
*** $Date: 05/12/05 08:59:12-08:00 $
*** $Author: bos@eng-24.pathscale.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_graph.h $
*** 
*** Revision history:
***
***     21-OCT-94 dkchen - Original Version
***     10-NOV-94 dkchen - Make DIRECTED_GRAPH16 a template class and move
***                        it to graph_template.h.
***
*** Description:
*** 
***     This interface describes vertex and edge classes used in a directed
***     graph class. See "graph_template.h" for details on the directed
***     graph class, DIRECTED_GRAPH16, which assumes the edges and
***     vertices to be derived classes from EDGE16 and VERTEX16 described
***     here.
*** 
*** Exported Types and Functions:
***
***     VINDEX16
***     EINDEX16
***
***         Type of a index to a vertex/edge in the graph
*** 
***     VERTEX16
*** 
***         Type of a vertex in the graph.
*** 
***   		VERTEX16()
*** 
*** 	        Construct a vertex and initialize it.
*** 
*** 		~VERTEX16()
*** 
*** 	        Destruct a vertex.
*** 
***         VERTEX16& operator=(const VERTEX16& v)
*** 
*** 		Vertex assignment operation. In/out edges are copied.
*** 
***         EINDEX16 	Get_In_Edge() const
***         EINDEX16 	Get_Out_Edge() const
*** 
*** 		Get the first in/out edge of this vertex.
*** 
*** 
***     EDGE16
*** 
***         Type of an edge in the graph.
*** 
*** 
***   		EDGE16()
*** 
*** 	        Construc an edge and initialize it.
*** 
*** 		~EDGE16()
*** 
*** 	        Destruct an edge.
*** 
***         EDGE16& operator=(const EDGE16& e)
*** 
*** 		Edge assignment operation. Source and sink vertices
***             are copied.
*** 
***         VINDEX16 	Get_Source() const
***         VINDEX16 	Get_Sink() const
*** 
*** 		Get the source/sink vertex of this edge.
*** 
**/

#ifndef cxx_graph_INCLUDED
#define cxx_graph_INCLUDED "cxx_graph.h"

#ifdef _KEEP_RCS_ID
static char *cxx_graph_rcs_id = cxx_graph_INCLUDED "$Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif

typedef mUINT16 VINDEX16;
typedef mUINT16 EINDEX16; // increase graph capacity. see bug 13018

extern const VINDEX16 INVALID_VINDEX16;
extern VINDEX16 GRAPH16_CAPACITY;

#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif

/* To up the graph capacity, here's what all one needs to do:
 * 1. change the typedefs below to (say) mUINT32.
 * 2. change the typedefs in graph_template.h
 * 3. increase INVALID_VINDEX16 and GRAPH16_CAPACITY in cxx_graph.cxx.
 * 4. change 
 *       ARRAY_DIRECTED_GRAPH16(mUINT16 num_v, mUINT16 num_e, WN_MAP map, 
 *    to
 *       ARRAY_DIRECTED_GRAPH16(mUINT32 num_v, mUINT32 num_e, WN_MAP map, 
 *    in dep_graph.h
 * 5. increase LNO_Graph_Capacity in lnodriver.c.
 */


class VERTEX16 {
  friend 	class DIRECTED_GRAPH16<class EDGE16, class VERTEX16>;
  friend  class DIRECTED_GRAPH16<class ARRAY_EDGE16,class ARRAY_VERTEX16>;  
  friend  class DIRECTED_GRAPH16<class LAT_EDGE16,class LAT_VERTEX16>;  
  friend  class DIRECTED_GRAPH16<class TEDGE,class TVERTEX>;  
  friend  class DIRECTED_GRAPH16<class FB_EDGE, class FB_NODE>;
private:
  EINDEX16	_from;		// link to in edges
				// also used to link free vertices
  EINDEX16	_to;		// link to out edges
				// also used to mark free vertices if
				// _to == INVALID_VINDEX16

		VERTEX16(const VERTEX16&);

protected:
  void 		Set_Next_Free_Vertex(VINDEX16 i){ _from = i; }
  void 		Set_To_Free() 			{ _to = INVALID_VINDEX16; }
  BOOL 		Is_Free() const		{ return _to == INVALID_VINDEX16; }
  VINDEX16 	Get_Next_Free_Vertex() const	{ return _from; }
  void 		Set_Out_Edge(EINDEX16 i) 	{ _from = i; }
  void 		Set_In_Edge(EINDEX16 i) 	{ _to = i; }

public:
  		VERTEX16() 			{ _from = 0; _to = 0; }
		~VERTEX16()			{};

  VERTEX16&	operator = (const VERTEX16&);

  EINDEX16 	Get_In_Edge() const 		{ return _to; }
				// Get the first in-edge of the vertex
  EINDEX16 	Get_Out_Edge() const		{ return _from; }
				// Get the first out-edge of the vertex
};

class EDGE16 {
  friend	class DIRECTED_GRAPH16<class EDGE16,class VERTEX16>;
  friend  class DIRECTED_GRAPH16<class ARRAY_EDGE16,class ARRAY_VERTEX16>;  
  friend  class DIRECTED_GRAPH16<class LAT_EDGE16,class LAT_VERTEX16>;  
  friend  class DIRECTED_GRAPH16<class TEDGE,class TVERTEX>;  
  friend  class DIRECTED_GRAPH16<class FB_EDGE, class FB_NODE>;
  friend	class SCC_DIRECTED_GRAPH16;
private:

  VINDEX16	_from;		// the source of the edge
  VINDEX16	_to;		// the sink of the edge
  EINDEX16	_nfrom;		// next edge of same source
  EINDEX16	_nto;		// next edge of same sink

		EDGE16(const EDGE16&);

protected:
  void 		Set_Source(VINDEX16 i)  	{ _from = i; }
  void 		Set_Sink(VINDEX16 i) 		{ _to = i; }
  void 		Set_Next_Out_Edge(EINDEX16 i) 	{ _nfrom = i; }
  void 		Set_Next_In_Edge(EINDEX16 i) 	{ _nto = i; }
  void 		Set_Next_Free_Edge(EINDEX16 i) 	{ _from = i; }
  void 		Set_To_Free() 			{ _to = INVALID_VINDEX16; }
  BOOL 		Is_Free() const		{ return _to == INVALID_VINDEX16; }
  VINDEX16 	Get_Next_Free_Edge() const	{ return _from; }
  EINDEX16 	Get_Next_Out_Edge() const	{ return _nfrom; }
  EINDEX16 	Get_Next_In_Edge() const	{ return _nto; }
public:
  		EDGE16() 			{ _from=_to=_nfrom=_nto=0; }
		~EDGE16()			{}

  EDGE16&	operator = (const EDGE16&);

  VINDEX16 	Get_Source() const		{ return _from; }
  VINDEX16 	Get_Sink() const		{ return _to; }
};

#endif		// cxx_graph_INCLUDED

