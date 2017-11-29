//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_bdce.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_bdce.h,v $
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
// ====================================================================


#ifndef opt_bdce_INCLUDED
#define opt_bdce_INCLUDED	"opt_bdce.h"

#ifdef _KEEP_RCS_ID
static char *opt_bdcercs_id = 	opt_bdce_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */


class BITWISE_DCE {
private:
  OPT_STAB   *_opt_stab;        // the optimizer symtab
  CFG        *_cfg;             // the control flow graph
  CODEMAP    *_htable;          // the hash table
  MEM_POOL   *_loc_pool;        // the permanent pool for new nodes
  BOOL        _copy_propagate;  // whether or not to perform copy prop

  BOOL	      _tracing; 
  UINT64     *_livebits;        // array of bit mask for each coderep node;
                                // tells which of the 64 integer bits are live
  UINT       _livebits_size;
  UINT8      *_usecnt;          // array of integers for each coderep node;
                                // counts number of times variable appears;
  BB_NODE_SET *_cd_bbs;		// set of bbs for which a live statement is 
				// found to be control-dependent on
  BB_NODE_SET *_live_bbs;	// set of bbs in which some statement is found
				// live

  OPT_STAB *Opt_stab(void)const { return _opt_stab; }
  CFG      *Cfg(void) const     { return _cfg; }
  CODEMAP  *Htable(void) const 	{ return _htable; }
  MEM_POOL *Loc_pool(void)const { return _loc_pool; }
  BOOL	    Tracing(void) const { return _tracing; }
  UINT64    Livebits(CODEREP *cr) const {
                              if (cr->Coderep_id() >= _livebits_size)
                                return UINT64_MAX;
                              return _livebits[cr->Coderep_id()]; }

  BOOL	    More_bits_live(CODEREP *cr, UINT64 live_bits) const
		    { return (live_bits & ~_livebits[cr->Coderep_id()]) != 0; }
  void	    Union_livebits(CODEREP *cr, UINT64 live_bits)
				{ _livebits[cr->Coderep_id()] |= live_bits; }
  UINT8     Usecnt(CODEREP *cr) const {
                              Is_True(cr->Coderep_id() < _livebits_size,
                                ("BITWISE_DCE::Usecnt: index out of range"));
                              return _usecnt[cr->Coderep_id()]; }

  void      IncUsecnt(CODEREP *cr) const
                                { if (_usecnt[cr->Coderep_id()] < 2)
				    _usecnt[cr->Coderep_id()]++; }
  BB_NODE_SET *Cd_bbs(void) const { return _cd_bbs; }
  BB_NODE_SET *Live_bbs(void) const { return _live_bbs; }

  BITWISE_DCE(void);               // REQUIRED UNDEFINED UNWANTED methods
  BITWISE_DCE(const BITWISE_DCE&); // REQUIRED UNDEFINED UNWANTED methods
  BITWISE_DCE& operator = (const BITWISE_DCE&); // REQUIRED UNDEFINED UNWANTED methods

  void Initialize_stmts_dead(void);
  BOOL Operators_without_dependency(OPERATOR opr);
  UINT64 Bitmask_of_size(UINT64);
  UINT64 Fill_lower_bits(UINT64);
  UINT64 Bits_in_type(MTYPE);
  UINT64 Bits_in_coderep_result(CODEREP *);
  UINT64 Bits_in_var(CODEREP *);
  void Mark_stmt_live(STMTREP *);
  void Mark_entire_var_live(CODEREP *, BOOL);
  void Mark_var_bits_live(CODEREP *, UINT64, BOOL);
  void Mark_tree_bits_live(CODEREP *, UINT64, BOOL);
  void Find_and_mark_cd_branch_live(BB_NODE *);
  void Make_bb_live(BB_NODE *);
  void Find_and_mark_return_live(BB_NODE *);
  BOOL Redundant_cvtl(BOOL, INT32, INT32, CODEREP *);
  void Mark_willnotexit_stmts_live(BB_NODE *);
  CODEREP *Copy_propagate(CODEREP *, STMTREP *);
  CODEREP *Delete_cvtls(CODEREP *, STMTREP *);
  void Delete_dead_nodes(void);
  void Print_nodes_with_dead_bits(FILE *);
  void Print_node_usecnts(FILE *);
  void Print_livebits(INT32);		// for use in dbx
#if defined(TARG_SL)
  void Repair_Injured_AuxIntrnOP (void);
#endif

public:
  BITWISE_DCE( CODEMAP *htable, OPT_STAB *opt_stab, CFG *cfg, MEM_POOL *lpool,
	       BOOL copy_propagate ):
    _opt_stab(opt_stab), _htable(htable), _cfg(cfg), _loc_pool(lpool),
    _copy_propagate(copy_propagate)
    {
      _livebits = (UINT64 *) CXX_NEW_ARRAY(UINT64, _htable->Coderep_id_cnt(),
				      _loc_pool);
      _livebits_size = _htable->Coderep_id_cnt();
      _usecnt = (UINT8 *) CXX_NEW_ARRAY(UINT8, _htable->Coderep_id_cnt(),
				      _loc_pool);
      BZERO(_livebits, _htable->Coderep_id_cnt() * sizeof(UINT64));
      BZERO(_usecnt, _htable->Coderep_id_cnt() * sizeof(UINT8));

      _cd_bbs = (BB_NODE_SET *) CXX_NEW(BB_NODE_SET(_cfg->Last_bb_id()+1, _cfg,
					_loc_pool, BBNS_EMPTY), _loc_pool);
      _live_bbs = (BB_NODE_SET *)CXX_NEW(BB_NODE_SET(_cfg->Last_bb_id()+1, _cfg,
					_loc_pool, BBNS_EMPTY), _loc_pool);

      _tracing = Get_Trace(TP_GLOBOPT, DCE_DUMP_FLAG);
    }
  ~BITWISE_DCE(void) { /*CXX_DELETE_ARRAY(_livebits, _loc_pool);*/
                       /*CXX_DELETE_ARRAY(_usecnt, _loc_pool);*/ }

  void Bitwise_dce(void);
}; // end of class BITWISE_DCE

#endif  // opt_bdce_INCLUDED
