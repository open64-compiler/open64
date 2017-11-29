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


/* ====================================================================
* ====================================================================
*
* Module: opt_alias_mgr.h
* $Revision: 1.2 $
* $Date: 02/11/07 23:41:38-00:00 $
* $Author: fchow@keyresearch.com $
* $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_alias_mgr.h $
*
* Revision history:
*  07-APR-95 lo - spilt from opt_alias.h
*
* Description:
*
* ====================================================================
* ====================================================================
*/

#ifndef opt_alias_mgr_INCLUDED
#define opt_alias_mgr_INCLUDED	"opt_alias_mgr.h"
#ifdef _KEEP_RCS_ID
static char *opt_alias_mgrrcs_id = 	opt_alias_mgr_INCLUDED"$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

/***********************************************************************
*
*  ALIAS MANAGER IMPLEMENTATION
*
*  See opt_alias_interface.h also.
*
* 
*  The manager also consists of a alias mapping.  It maps WN nodes
*  to alias_ids.  During WHIRL emit time, each WN memory operation is 
*  allocated an alias_id.  Identical memory operation should have the same 
*  alias_id.  The alias_id is used to index into a dynamic array of
*  POINTS_TO * to obtain the alias information of the WN node.
* 
*  In order to conserve space, an optimization is done for PREGs and 
*  scalars that have no aliases except itself.  As usual, they are
*  assigned alias_ids, but given a NULL POINTS_TO information because
*  their alias information are easily derived from the WN node.
*  
************************************************************************
*/

#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#include "opt_alias_interface.h"
#include "opt_points_to.h"
#include "mempool.h"
#include "tracing.h"			// for TFile

class ALIAS_CLASSIFICATION;
class AliasAnalyzer;

class ALIAS_MANAGER {

private:
  MEM_POOL              _mem_pool;
  BOOL                  _trace;
  WN_MAP                _map;
  WN_MAP                _homing_map;
  IDTYPE                _last_alias_id;
  DYN_ARRAY<POINTS_TO*> *_vec;
  static const IDTYPE   _preg_id = 1;
  IDTYPE                _no_alias_info_id;
  ALIAS_CONTEXT         _pu_context;      // bitset of accepted alias rule
  ALIAS_RULE            *_rule;

  ALIAS_MANAGER(const ALIAS_MANAGER&);
  ALIAS_MANAGER& operator = (const ALIAS_MANAGER&);

  DYN_ARRAY<POINTS_TO*> *Vec(void) const { return _vec; }
  vector<IDTYPE, mempool_allocator<IDTYPE> > *_invalid_ip_alias_classes;

public:
  ALIAS_MANAGER(WN *entryWN);
  ~ALIAS_MANAGER(void);

  IDTYPE     Id(const WN *wn) const;
  IDTYPE     Preg_id(void) const             { return _preg_id; }
  IDTYPE     No_alias_info_id(void) const    { return _no_alias_info_id; }
  POINTS_TO  *Pt(IDTYPE id) const            { return (*_vec)[id]; }
  ALIAS_CONTEXT Pu_context(void) const       { return _pu_context; }
  ALIAS_RULE *Rule(void)   const             { return _rule; }
  WN_MAP     Map(void) const		     { return _map; }
  WN_MAP     Homing_map(void) const	     { return _homing_map; }
  inline void       Set_id(WN *wn, IDTYPE id) const;
  inline void       Set_pu_context(ALIAS_CONTEXT ct){ _pu_context = ct; }
  inline void       Set_context(ALIAS_CONTEXT c);
  inline IDTYPE     New_alias_id(void);   
  inline POINTS_TO *New_points_to(WN *wn);  // also assign new id

  // Cross-DSO out-of-line versions of member functions required by wopt.so.
  IDTYPE     Cross_dso_new_alias_id(void);   
  void       Cross_dso_set_id(WN *wn, IDTYPE id) const;

  // setup alias information for EMIT
  void              Gen_alias_id(WN *, POINTS_TO *);
  void              Gen_alias_id_list(WN *, POINTS_TO_LIST *);
  void              Gen_black_box_alias(WN *);
  // duplicate the alias id for an entire WN tree for EMIT
  void              Dup_tree_alias_id( const WN *old_wn, WN *new_wn );
  void              Print( const WN *wn, FILE *fp = TFile ) const;

  void              Forget_alias_class_info(void);
  void              Transfer_alias_class_to_alias_manager(const ALIAS_CLASSIFICATION &,
							        WN                   *);

  // Support to fix 707179
  inline BOOL May_refer_to_alloca_mem(const WN *) const;
  BOOL              Safe_to_speculate(const WN *) const;

  void Note_invalid_ip_alias_class(const WN *);
  void Erase_ip_alias_class_if_invalid(WN *);

  // support homing
  BOOL Homing_load( const WN *load_wn ) const
		{ return WN_MAP32_Get(Homing_map(),load_wn); }
  void Set_homing_load( WN *load_wn, BOOL b ) const
		{ WN_MAP32_Set(Homing_map(),load_wn,b); }

  BOOL Homing_store( const WN *store_wn ) const
		{ return WN_MAP32_Get(Homing_map(),store_wn); }
  void Set_homing_store( WN *store_wn, BOOL b ) const
		{ WN_MAP32_Set(Homing_map(),store_wn,b); }

  ALIAS_RESULT Aliased(WN *wn, const POINTS_TO *pt,
                        BOOL ignore_loop_carried = FALSE);
  ALIAS_RESULT Aliased(const POINTS_TO *pt, WN *wn,
                        BOOL ignore_loop_carried = FALSE);
  ALIAS_RESULT Aliased(const POINTS_TO *pt1, const POINTS_TO *pt2,
                        BOOL ignore_loop_carried = FALSE);
};

#endif /* opt_alias_mgr.h include */
