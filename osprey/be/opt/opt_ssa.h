//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_ssa.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ssa.h,v $
//
// Revision history:
//  4-OCT-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// ====================================================================
// ====================================================================


#ifndef opt_ssa_INCLUDED
#define opt_ssa_INCLUDED	"opt_ssa.h"

#include "defs.h"
#include "opt_defs.h"
#include "cxx_memory.h"
#include "opt_sym.h"

//  Forward declaration
class CFG;
class OPT_STAB;
class BB_NODE;
class BB_LIST;
class STMTREP;
class COLOR;
class PHI_LIST;
class COPYPROP;
class EXC;
class EXC_SCOPE;

class SSA {
private:
  MEM_POOL *mem_pool;
  MEM_POOL *loc_pool;  
  CFG      *_cfg;
  CODEMAP  *_htable;
  OPT_STAB *_opt_stab;

            SSA(void);
            SSA(const SSA&);
            SSA& operator = (const SSA&);

  OPT_STAB *Opt_stab(void)    { return _opt_stab; }
  CFG      *Cfg(void)         { return _cfg; }
  CODEMAP  *Htable(void)      { return _htable; }

  void      Collect_defs_bb(MEM_POOL *);
  void      Place_phi_node(MEM_POOL *);
  BB_NODE  *Insert_identity_assignment_4_loopexit(BB_NODE *,AUX_ID,MEM_POOL *);
  void      Insert_identity_assignment_4_entry(CFG *cfg, OPT_STAB *opt_stab);
  void      Rename(CFG *cfg, OPT_STAB *opt_stab, BB_NODE *bb);
  void      Rename_rhs(OPT_STAB *opt_stab, WN *wn);
  void      Gen_name(OPT_STAB *opt_stab, IDTYPE var);
  void      Value_number(CODEMAP *htable, OPT_STAB *opt_stab, BB_NODE *bb, 
			 COPYPROP *copyprop, EXC *exc);
  CODEREP  *Du2cr( CODEMAP *htable, OPT_STAB *opt_stab, VER_ID du, STMTREP *stmt);
  CODEREP  *Revive_phi_chi_opnd(VER_ID du);
  void	    Make_live_phi_chi_opnd(VER_ID du);
#ifdef KEY
  void      Print_ssa_ver_for_wn(WN* wn, INT indent);
#endif
public:
            SSA(MEM_POOL *gpool,
		MEM_POOL *lpool){mem_pool=gpool; loc_pool=lpool; }
           ~SSA(void) 	  {};
  MEM_POOL *Mem_pool(void)    { return mem_pool; }

  void      Construct(CODEMAP *htable,
		      CFG *cfg, OPT_STAB *opt_stab);  //  Construct SSA form.
  
  CODEREP  *Get_zero_version_CR(AUX_ID aux_id, OPT_STAB *, VER_ID);
  void      Pointer_Alias_Analysis(void);         
  void      Dead_store_elim(CFG *, OPT_STAB *, EXC *);
  void	    Find_zero_versions();
  void      Create_CODEMAP(void);
  void      Value_number_mu_list(MU_LIST *mu_list);
  void      Value_number_mu_node(MU_NODE *mu_node);
  void	    Resurrect_phi(PHI_NODE *phi);
  void	    Resurrect_chi(CHI_NODE *chi);
  void      Print();
};

class CODEREP;


class PHI_KEY {
  IDTYPE _bb_id;
  AUX_ID _aux_id;
public:
  // A constructor is not allowed because 
  // PHI_KEY is a union member in ID_MAP!
  void Init(IDTYPE bb_id, AUX_ID aux_id) {
    _bb_id = bb_id;
    _aux_id = aux_id;
  }
  friend BOOL operator==(const PHI_KEY x, const PHI_KEY y) 
    {
      return (x._bb_id == y._bb_id && x._aux_id == y._aux_id);
    }
  friend BOOL operator!=(const PHI_KEY x, const PHI_KEY y) 
    {
      return (x._bb_id != y._bb_id || x._aux_id != y._aux_id);
    }
};


// Phi functions
class PHI_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(PHI_NODE)
private:

  enum PHI_NODE_FLAGS {
    PNF_NONE		   = 0x00,
    PNF_LIVE		   = 0x01,
    PNF_DSE_DEAD	   = 0x02,
    PNF_DCE_DEAD	   = 0x04,// phi res is dead after DCE
    PNF_RES_IS_CR	   = 0x08,// phi res is CODEREP, not VER_STAB index
    PNF_OPND_IV_UPDATE	   = 0x10,// an opnd is defined by an IV update
    PNF_OPND_NOT_IV_UPDATE = 0x20,// an opnd is not defined by an IV update
    PNF_VISITED            = 0x40, // vistied bit.
    PNF_NULL_SSU_PROCESSED = 0x80, // prevent infinite loop in setting 
				   // NULL_SSU_VERSION flag in SPRE step 2
    PNF_FIND_DEF_PROCESSED = 0x100, // prevent infinite loop in looking for 
			    	    // real stores in SPRE step 6 
    PNF_INCOMPLETE         = 0x200, // when this bit is set, its dominator
                                    // frontier might not have a phi 
                                    // this bit is set by phi generated by LPRE/EPRE.
    PNF_SIZE_VISITED       = 0x400, // No_trucation_by_value_size visited
    PNF_FIND_CORR_PROCESSED = 0x800, // prevent infinite loop in looking for 
			    	    // corresponding preg in SPRE step 6 
  };

  AUX_ID      _aux_id;
  mUINT32     _flags;
  mINT16      size;
  mINT16      count;
  BB_NODE     *_bb;
  union PHI_ELEM {
  friend class PHI_NODE;
  private:
    AUX_ID    version;
    CODEREP   *cr;
  };

  // vec[0] is the result
  // vec[1..] are the operands
  PHI_ELEM *vec;

  PHI_NODE(void);
  PHI_NODE(const PHI_NODE&);
  PHI_NODE& operator = (const PHI_NODE&);

public:
         PHI_NODE(mINT16 in_degree, MEM_POOL *pool, BB_NODE *bb)
	   { vec = (PHI_ELEM*) CXX_NEW_ARRAY(PHI_ELEM, (in_degree + 1), pool);
	     size = in_degree; count = 0; _bb = bb; 
	     _flags = PNF_NONE; }
        ~PHI_NODE(void)                  { /*CXX_DELETE_ARRAY(vec, Opt_default_pool);*/ }
  
  AUX_ID   Opnd(INT32 i) const           { return vec[i+1].version; }
  AUX_ID   Result(void) const            { return vec[0].version; }
  CODEREP *OPND(INT32 i) const           { return vec[i+1].cr;  }
  CODEREP *RESULT(void) const            { return vec[0].cr; }
  mINT16   Size(void) const              { return size; }
  mINT16   Count(void) const             { return count; }
  IDTYPE   Aux_id(void) const            { return _aux_id; }
  BB_NODE *Bb(void) const                { return _bb; }
  void     Set_bb(BB_NODE *bb)           { _bb = bb; }

  PHI_KEY  Key(void) const               { PHI_KEY key; key.Init(_bb->Id(), _aux_id); return key; }
  
  mUINT8   Flags(void) const             { return _flags; }
  void     Set_flags(mUINT8 i)           { _flags = i; }
  void     Set_opnd(const INT32 i, AUX_ID cr)
                                         { vec[i+1].version = cr;}
  void     Set_result(AUX_ID cr)         { vec[0].version = cr; }
  void     Set_opnd(const INT32 i, CODEREP *c)
                                         { vec[i+1].cr = c; }
  void     Set_result(CODEREP *c)        { vec[0].cr = c; }
  void     Set_count(mINT16 c)           { count = c; }
  void     Set_aux_id(AUX_ID v)          { _aux_id = v; }
  void     Set_invalid(void)             { vec[0].cr = NULL; }
  void     Reset_OPND(INT32 i)           { vec[i+1].cr = NULL; }
  PHI_ELEM *Vec()                        { return vec; }

  // Remove the i'th operand from the phi-node (0 is first opnd)
  void     Remove_opnd(INT32 i);

  // flags field accessors
  BOOL	   Live(void) const		{ return _flags & PNF_LIVE; }
  void	   Set_live(void)		{ _flags |= PNF_LIVE; }
  void	   Reset_live(void)		{ _flags &= ~PNF_LIVE; }
  BOOL	   Dse_dead(void) const		{ return _flags & PNF_DSE_DEAD; }
  void	   Set_dse_dead(void)		{ _flags |= PNF_DSE_DEAD; }
  void	   Reset_dse_dead(void)		{ _flags &= ~PNF_DSE_DEAD; }
  BOOL	   Dce_dead(void) const         { return _flags & PNF_DCE_DEAD; }
  void	   Reset_dce_dead(void)		{ _flags &= ~PNF_DCE_DEAD; }
  void	   Set_dce_dead(void)		{ _flags |= PNF_DCE_DEAD; }
  BOOL	   Res_is_cr(void) const        { return _flags & PNF_RES_IS_CR; }
  void	   Set_res_is_cr(void)		{ _flags |= PNF_RES_IS_CR; }
  // is one of the operands defined by an IV update statement
  BOOL	   Opnd_iv_update(void) const   { return _flags & PNF_OPND_IV_UPDATE; }
  void	   Set_opnd_iv_update(void)	{ _flags |= PNF_OPND_IV_UPDATE; }
  // none of the operands defined by an IV update statement
  BOOL	   Opnd_not_iv_update(void) const{ return _flags & PNF_OPND_NOT_IV_UPDATE; }
  void	   Set_opnd_not_iv_update(void)	{ _flags |= PNF_OPND_NOT_IV_UPDATE; }
  BOOL	   Visited(void) const		{ return _flags & PNF_VISITED; }
  void	   Set_visited(void)		{ _flags |= PNF_VISITED; }
  void	   Reset_visited(void)		{ _flags &= ~PNF_VISITED; }
  BOOL	   Null_ssu_processed(void) const   { return _flags & PNF_NULL_SSU_PROCESSED; }
  void	   Set_null_ssu_processed(void)	{ _flags |= PNF_NULL_SSU_PROCESSED; }
  BOOL	   Find_def_processed(void) const   { return _flags & PNF_FIND_DEF_PROCESSED; }
  void	   Set_find_def_processed(void)	{ _flags |= PNF_FIND_DEF_PROCESSED; }
  void	   Reset_find_def_processed(void) { _flags &= ~PNF_FIND_DEF_PROCESSED; }
  BOOL	   Find_corr_processed(void) const { return _flags & PNF_FIND_CORR_PROCESSED; }
  void	   Set_find_corr_processed(void)	{ _flags |= PNF_FIND_CORR_PROCESSED; }
  BOOL	   Incomplete(void) const	{ return _flags & PNF_INCOMPLETE; }
  void	   Set_incomplete(void)		{ _flags |= PNF_INCOMPLETE; }
  void	   Reset_incomplete(void)	{ _flags &= ~PNF_INCOMPLETE; }

  BOOL	   Is_size_visited(void) const	{ return _flags & PNF_SIZE_VISITED; }
  void	   Set_size_visited(void)	{ _flags |= PNF_SIZE_VISITED; }
  void	   Reset_size_visited(void)	{ _flags &= ~PNF_SIZE_VISITED; }

  //  Print functions
  void     Print(INT32 in_degree,
		 FILE *fp=stderr) const;
  void     PRINT(INT32 in_degree,
		 FILE *fp=stderr) const;
  void     Print(FILE *fp=stderr) const;
};


class PHI_LIST : public SLIST {
  DECLARE_SLIST_CLASS (PHI_LIST, PHI_NODE)

private:
  INT32   in_degree;          

  PHI_LIST(const PHI_LIST&);
  PHI_LIST& operator = (const PHI_LIST&);

public:
  PHI_LIST(BB_NODE *bb);
  ~PHI_LIST(void)			{}

  PHI_NODE *New_phi_node(IDTYPE var, MEM_POOL *pool, BB_NODE *bb)
    { PHI_NODE *p = (PHI_NODE *) CXX_NEW (PHI_NODE(in_degree, pool, bb), pool); 
      for (INT32 i = 0; i < in_degree; i++)
	p->Set_opnd(i, (AUX_ID)0);
      p->Set_result((AUX_ID)0);
      p->Set_aux_id(var);
      Append(p);
      return p;
    }

  PHI_LIST *Dup_phi_node(MEM_POOL *pool, BB_NODE *bb, INT pos);
  PHI_LIST *Dup_phi_node(MEM_POOL *pool, BB_NODE *bb);
  // Remove the i'th operand from the phi-nodes (0 is first opnd)
  void     Remove_opnd(INT32 i);
	
  INT32 In_degree(void) const           { return in_degree; }
  void  Set_in_degree( INT32 n )	{ in_degree = n; }

  void  Print(FILE *fp=stderr);    
  void  PRINT(FILE *fp=stderr);    
};


class PHI_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (PHI_LIST_ITER, PHI_NODE, PHI_LIST)

  PHI_LIST_ITER(const PHI_LIST_ITER&);
  PHI_LIST_ITER& operator = (const PHI_LIST_ITER&);

public:
  ~PHI_LIST_ITER(void)		{}

  PHI_NODE *First_elem(void)	{ return First(); }
  PHI_NODE *Next_elem(void)	{ return Next();  }
};

class PHI_OPND_ITER {
private:
  PHI_NODE *_phi;
  INT       _curidx;

  PHI_OPND_ITER(void);
  PHI_OPND_ITER(const PHI_OPND_ITER&);
  PHI_OPND_ITER& operator = (const PHI_OPND_ITER&);

public:
  PHI_OPND_ITER(PHI_NODE *phi)   { _phi = phi; }
  ~PHI_OPND_ITER(void)           {}
  void     Init(PHI_NODE *phi)   { _phi = phi; }
  void     Init(void)            {}
  CODEREP *First_elem(void)      { _curidx = 0; return _phi->OPND(_curidx); }
  CODEREP *Next_elem(void)       { _curidx++;
				   return (Is_Empty())? NULL:_phi->OPND(_curidx); }
  BOOL     Is_Empty(void) const  { return _curidx >= _phi->Size(); }
  INT	   Curidx(void) const	 { return _curidx; }
};

#endif  // opt_ssa_INCLUDED
