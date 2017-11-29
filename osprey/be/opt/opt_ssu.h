//-*-c++-*-

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_ssu.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ssu.h,v $
//
// Revision history:
//  11-DEC-96 ptu - Original Version
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
//   IPHI_HASH_ENTRY:the individual node that carries the iphi node
//                   in the iphi-hash table.
//            the body of the class contains:
//
//     _bb:          which BB this iphi function is associated with
//     _iphi_result: this is actually a flag, tells whether this iphi
//                   node is a variable or an expression by checking
//                   the Kind().
//     _node:        contains the real iphi node.  TODO: This field
//                   can be deleted if we do not find use of it.
//
// ====================================================================
// ====================================================================


#ifndef opt_ssu_INCLUDED
#define opt_ssu_INCLUDED	"opt_ssu.h"

#include "defs.h"
#ifndef errors_INCLUDE
#include "errors.h"
#endif
#include "opt_defs.h"
#include "cxx_memory.h"
#include "opt_sym.h"

//  Forward declaration
class CFG;
class OPT_STAB;
class BB_NODE;
class BB_LIST;
class STMTREP;
class IPHI_LIST;
class COPYPROP;
class EXP_OCCURS;
class ETABLE;

class SSU {
private:
  MEM_POOL *_mem_pool;
  MEM_POOL *_loc_pool;  
  CFG      *_cfg;
  CODEMAP  *_htable;
  OPT_STAB *_opt_stab;
  ETABLE   *_etable;
  BOOL      _tracing;
  IDX_32_SET *_e_num_set;
  IDX_32_SET **_make_diff_ssu_version_called_in_bb; // array indexed
  		// by BB id of set of aux_id's processed already by 
		// Make_diff_ssu_version

            SSU(const SSU&);
            SSU& operator = (const SSU&);

  OPT_STAB *Opt_stab(void) const { return _opt_stab; }
  CFG      *Cfg(void)      const { return _cfg; }
  CODEMAP  *Htable(void)   const { return _htable; }
  ETABLE   *Etable(void)   const { return _etable; }
  BOOL      Tracing(void)  const { return _tracing; }

  EXP_WORKLST *SPRE_candidate(CODEREP *cr);
  void	    Insert_iphis_recursive(EXP_WORKLST *, BB_NODE *);
  void	    Make_non_postdominated_iphi_opnd_null(BB_NODE *iphibb, 
						  EXP_PHI *iphi);
  BOOL	    Find_intervening_iphi(EXP_WORKLST *wk, CODEREP *v, BB_NODE *usebb);
  void	    Make_diff_ssu_version_at_phi(EXP_WORKLST *wk,
					 BB_NODE *defbb,
					 PHI_NODE *phi);
  void	    Check_iphi_presence(EXP_WORKLST *wk,
  				BB_NODE *iphibb);
  void	    Make_null_ssu_version_in_iphi_for_e_num_set(
					  BB_NODE *iphibb,
					  BB_NODE *usebb);
  void	    Make_diff_ssu_version(EXP_WORKLST *wk, 
				  CODEREP *v, 
				  BB_NODE *usebb,
				  BOOL only_itself);
  void      Traverse_mu_read(MU_LIST *, BB_NODE *);
  void      Traverse_cr_rw(CODEREP *, BB_NODE *, BOOL is_store);
  void      Iphi_insertion(void);
  inline void Reset_tos_downsafe(void);
  void	    Propagate_occurrences(EXP_OCCURS *iphi_occ, CODEREP *cr);
  void      Rename(BB_NODE *bb);

public:
            SSU(void);
            SSU(CODEMAP  *htable,
		CFG      *cfg, 
		OPT_STAB *opt_stab, 
		ETABLE   *etable,
		MEM_POOL *gpool,
		MEM_POOL *lpool,
                BOOL      tracing) 
	                         { _htable   = htable;
				   _cfg      = cfg;
				   _opt_stab = opt_stab;
				   _etable   = etable;
				   _mem_pool = gpool; 
				   _loc_pool = lpool;
                                   _tracing  = tracing;
				   _e_num_set = CXX_NEW(IDX_32_SET(etable->Exp_worklst()->Len(), 
				   				   lpool, 
								   OPTS_DONT_CARE), 
						       lpool);
				  _make_diff_ssu_version_called_in_bb =
				    (IDX_32_SET **) CXX_NEW_ARRAY(IDX_32_SET *,
						cfg->Total_bb_count(), lpool);
				  for (INT32 i = 0; i < cfg->Total_bb_count();
				       i++)
				    _make_diff_ssu_version_called_in_bb[i] = 
				   	CXX_NEW(IDX_32_SET(opt_stab->Lastidx(), 
							   lpool, 
							   OPTS_FALSE),
						lpool); 
                                 }
           ~SSU(void) 	  { /*CXX_DELETE_ARRAY(_make_diff_ssu_version_called_in_bb, lpool);*/ }

  void      Construct(void);
  MEM_POOL *Mem_pool(void) const { return _mem_pool; }
  MEM_POOL *Loc_pool(void) const { return _loc_pool; }
};

#endif  // opt_ssu_INCLUDED
