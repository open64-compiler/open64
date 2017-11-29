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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef cxx_ipa_section_prop_INCLUDED
#define cxx_ipa_section_prop_INCLUDED

extern MEM_POOL IPA_array_prop_pool;
extern BOOL Trace_IPA_Sections;

//------------------------------------------------------------
// DESCR:  perform scalar EUSE and KILL analysis
//------------------------------------------------------------
class IPA_SCALAR_MUST_DF_FLOW : public IPA_DATA_FLOW
{
protected:
  virtual void  InitializeNode(void* n);
  virtual void* Meet(void* in, void* vertex, INT *change);
  virtual void* Trans(void* in, void* out, void* vertex, INT *change);
  virtual void  PostProcessIO(void* n);

public:
  IPA_SCALAR_MUST_DF_FLOW(IPA_CALL_GRAPH *cg, DF_DIRECTION ddf, MEM_POOL *m);
  virtual void Print_entry(FILE* fp, void*, void* n);
};

//------------------------------------------------------------
// DESCR: perform array MOD/REF analysis
//------------------------------------------------------------
class IPA_ARRAY_DF_FLOW: public IPA_DATA_FLOW
{
protected:
  virtual void  InitializeNode(void* n);
  virtual void* Meet(void* in, void* vertex, INT* change);
  virtual void* Trans(void* in, void* out, void* vertex, INT* change);
  virtual void  PostProcessIO(void* n);

public:
  IPA_ARRAY_DF_FLOW(IPA_CALL_GRAPH* cg, DF_DIRECTION ddf, MEM_POOL* m);
  virtual void Print_entry(FILE* fp, void*, void* n);
};

//--------------------------------------------------------------------
// Propagate formals that are used as symbolic terms in array sections
//--------------------------------------------------------------------
class IPA_FORMALS_IN_ARRAY_SECTION_DF: public IPA_DATA_FLOW
{
protected:
  virtual void* Meet(void* in, void* vertex, INT* change);
  virtual void* Trans(void* in, void* out, void* vertex, INT* change);

public:
  IPA_FORMALS_IN_ARRAY_SECTION_DF(IPA_CALL_GRAPH* cg, 
                                  DF_DIRECTION ddf, 
                                  MEM_POOL* m);
  virtual void Print_entry(FILE* fp, void* out, void* vertex);
};

#endif // cxx_ipa_section_INCLUDED



