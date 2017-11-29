/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipl_array_bread_write_INCLUDED
#define ipl_array_bread_write_INCLUDED

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"       // DYN_ARRAY
#endif
#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"           // HASH_TABLE
#endif
#ifndef ipa_section_INCLUDED
#include "ipa_section.h"        // Classes for array sections
#endif

// forward class declaration to minimize header file inclusion
class SUMMARY_PROCEDURE;
class SUMMARY_FILE_HEADER;

// a hash table that stores terms that can be re-used
// the key is a 64 bit element, the hash table stores the
// id into the term array
typedef DYN_ARRAY<INT> INTEGER_ARRAY;
typedef HASH_TABLE<UINT64, INTEGER_ARRAY*> TERM_HASH_TABLE;

// external interface to the array section summary information
extern void
Init_write_asections(MEM_POOL*);

// map the array sections per pu into the output file, so the
// memory can then all be freed
extern void 
Map_asections(ARRAY_SUMMARY *summary, SUMMARY_PROCEDURE *p);

//------------------------------------------------------------------------
// write out the summary information 
// for flow array section analysis
//------------------------------------------------------------------------
class ARRAY_SUMMARY_OUTPUT 
{
private:

  // memory pool that is persistent
  MEM_POOL *_m; 
  // contains the terms of linear expressions
  TERM_ARRAY *_terms;
  // contains all the nodes that have been projected
  PROJECTED_ARRAY *_project_nodes;
  // contain all the projected regions
  PROJECTED_REGION_ARRAY *_projected_regions;
  // contain index and count of each set of projected
  // regions
  ARRAY_OF_REGION_ARRAYS *_region_arrays;
  // contain the nodes attached to various points
  // in the control flow structure
  CFG_NODE_INFO_ARRAY *_cfg_nodes;
  IVAR_ARRAY *_ivars;
  LOOPINFO_ARRAY *_loopinfo_nodes;
  INT_ARRAY *_scalar_items;
  TERM_HASH_TABLE *_term_hash_table;
  INT offset_term, offset_projected_node, offset_scalars;
  INT offset_projected_region, offset_region, offset_cfg_node;
  INT offset_ivar, offset_loop_info;
  INT table_size;
  
public:
  ARRAY_SUMMARY_OUTPUT(MEM_POOL *m)
    {
      _m = m;
      _terms = CXX_NEW(TERM_ARRAY(m), m);
      _project_nodes = CXX_NEW(PROJECTED_ARRAY(m), m);
      _projected_regions = CXX_NEW(PROJECTED_REGION_ARRAY(m),
					 m);
      _region_arrays = CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
      _cfg_nodes = CXX_NEW(CFG_NODE_INFO_ARRAY(m), m);
      _ivars = CXX_NEW(IVAR_ARRAY(m), m);
      _loopinfo_nodes = CXX_NEW(LOOPINFO_ARRAY(m), m);
      _scalar_items = CXX_NEW(INT_ARRAY(m), m);
      _term_hash_table = CXX_NEW(TERM_HASH_TABLE(200, m),m);

      offset_term =  offset_projected_node = offset_scalars = 0;
      offset_projected_region = offset_region = offset_cfg_node = 0;
      offset_ivar = offset_loop_info = 0;
      
    };
  
  TERM_ARRAY* Get_term_array()  { return _terms;};
  PROJECTED_ARRAY* Get_projected_array() 
    { return _project_nodes; };
  PROJECTED_REGION_ARRAY* Get_projected_region_array() 
    { return _projected_regions;};
  ARRAY_OF_REGION_ARRAYS* Get_region_array() 
    { return _region_arrays; };
  CFG_NODE_INFO_ARRAY *Get_cfg_node_array() 
    { return _cfg_nodes;};
  IVAR_ARRAY *Get_ivar_array() 
    { return _ivars;};
  INT_ARRAY *Get_scalar_array()
    { return _scalar_items;};

  INT Get_term_offset() const { return offset_term;};

  TERM *Get_term(INT i) {return &(*_terms)[i];};
  PROJECTED_NODE *Get_projected_node(INT i) 
    {return &(*_project_nodes)[i];};
  PROJECTED_REGION *Get_projected_region(INT i)
    {return &(*_projected_regions)[i]; };
  REGION_ARRAYS* Get_region_array (INT i) 
    {return &(*_region_arrays)[i]; };
  CFG_NODE_INFO* Get_cfg_node(INT i)
    { return &(*_cfg_nodes)[i]; };
  IVAR* Get_ivar(INT i) { return &(*_ivars)[i]; };
  LOOPINFO *Get_loopinfo(INT i)
    { return &(*_loopinfo_nodes)[i]; };
  SCALAR_INFO *Get_scalars(INT i) { return &(*_scalar_items)[i]; };

  TERM_HASH_TABLE* Get_term_hash_table() { return _term_hash_table;};

  INT Get_term_count()  { return _terms->Lastidx(); };
  INT Get_projected_node_count()  { return
   _project_nodes->Lastidx();};
  INT Get_projected_region_count()  { return
    _projected_regions->Lastidx(); };
  INT Get_region_count() { return _region_arrays->Lastidx(); };
  INT Get_cfg_node_count() { return _cfg_nodes->Lastidx(); };
  INT Get_ivar_count() { return _ivars->Lastidx(); };
  INT Get_loopinfo_count() { return _loopinfo_nodes->Lastidx();};
  INT Get_scalars_count() { return _scalar_items->Lastidx();};

  void Write_summary(struct output_file *fl, 
		     INT cur_sec_disp);
  void Trace(FILE *f, const void *sbase);
  void Trace(FILE *f);

  void Print_ivar_array(FILE *f, INT size, IVAR *node);
  void Print_cfg_node_array(FILE *f, INT size, CFG_NODE_INFO* node);
  void Print_regions_array(FILE *f, INT size, REGION_ARRAYS* node);
  void Print_projected_region_array(FILE *f, INT size,PROJECTED_REGION *node);
  void Print_projected_array(FILE *f, INT size, PROJECTED_NODE* node);
  void Print_term_array(FILE *f, INT size, TERM *term, IVAR *ivar);
  void Print_loopinfo_array(FILE *f, INT size, LOOPINFO *loop_info);
  void Print_scalar_array(FILE *f, INT size, SCALAR_INFO* scalar);
  void Map_summary_info(ARRAY_SUMMARY *summary);

  // return the starting offset into the array 
  INT Map_region_arrays(REGION_ARRAYS* r);

  INT Map_projected_region(PROJECTED_REGION *proj_region);
  INT Map_proj_array(PROJECTED_ARRAY *p);
  INT Map_loop_info(LOOPINFO *l);
  void Map_term(TERM *t_in, TERM* t_out);
  void Map_ivar_array(IVAR_ARRAY* ivar);

  void Update_array_sect_header(SUMMARY_FILE_HEADER *header_addr);

  // utilities for merging term entries
  // returns an index into the term array.
  // If not found then return -1
  INT Search_for_terms(LINEX *l);
  void Insert_terms(TERM* t,INT idx, INT count);
  UINT64 Get_key(TERM* t, INT num_terms);
  // update the pointers in the projected node to indices
};

extern ARRAY_SUMMARY_OUTPUT *Array_Summary_Output;

#endif

