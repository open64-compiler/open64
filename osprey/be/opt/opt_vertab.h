//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_vertab.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vertab.h,v $
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


#include "id_map.h"

#include "opt_etable.h"
#include "opt_htable.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "tracing.h"

#include "opt_config.h"		// WOPT_Enable_Output_Copy


class E_VER_INFO {
private:
  EXP_OCCURS  *_avail_def;

#if Is_True_On
  UINT32       _occ_count;
  BOOL         _redefined;
#endif

public:
  E_VER_INFO(void)
    {
      _avail_def = NULL;

#if Is_True_On
      _occ_count = 0;
      _redefined = FALSE;
#endif
    }

  ~E_VER_INFO(void) { }

  void        Set_avail_def(EXP_OCCURS *const avail_def)
    { _avail_def = avail_def; }

  EXP_OCCURS *Avail_def(void) const { return _avail_def; }

#if Is_True_On
  void        Set_occ_count(const UINT32 occ_count)
    { _occ_count = occ_count; }

  UINT32      Inc_occ_count(const UINT32 increment = 1)
    { return _occ_count = _occ_count + increment; }

  UINT32      Occ_count(void) const { return _occ_count; }

  void        Set_redefined(void)
    { _redefined = TRUE; }

  BOOL        Redefined(void) const { return _redefined; }
#endif
};


class E_VER_TAB {
private:
  const BOOL        _tracing;
        E_VER_INFO *_e_ver_tab;
        MEM_POOL   *_pool;

#if Is_True_On
  const UINT32      _n_versions;
#endif

public:
  E_VER_TAB(      MEM_POOL *pool,
	    const UINT32    n_versions,
	    const BOOL      tracing) :
#if Is_True_On
    _n_versions(n_versions),
#endif
    _tracing(tracing), _pool(pool)
      {
	_e_ver_tab = CXX_NEW_ARRAY(E_VER_INFO, n_versions, _pool);
      }

  ~E_VER_TAB(void) { CXX_DELETE_ARRAY(_e_ver_tab, _pool); }

  void Set_avail_def(const IDTYPE            e_version,
		           EXP_OCCURS *const avail_def)
    {
      Is_True(e_version < _n_versions, ("e_version out of range"));
      _e_ver_tab[e_version].Set_avail_def(avail_def);
    }

  EXP_OCCURS *Avail_def(const IDTYPE e_version)
    {
      Is_True(e_version < _n_versions, ("e_version out of range"));
      return _e_ver_tab[e_version].Avail_def();
    }

  /* ARGSUSED */
  void Note_version_use(const IDTYPE e_version,
			const UINT32 increment = 1)
    {
#if Is_True_On
      Inc_occ_count(e_version, increment);
#endif
      FmtAssert(Avail_def(e_version) != NULL,
		("E_VER_TAB: E-version %d has no available definition",
		 e_version));
      Is_True((Occ_count(e_version) > 1) ||
	      (Avail_def(e_version)->Occ_kind() ==
	       EXP_OCCURS::OCC_PHI_OCCUR),
	      ("E_VER_TAB: E-version %d: inconsistent occurrence count",
	       e_version));

      // There will be a reload of this version, so the available
      // definition for this version must be saved.
      if (Avail_def(e_version)->Occ_kind() ==
	  EXP_OCCURS::OCC_REAL_OCCUR) {
	Avail_def(e_version)->Set_save_to_temp();
      }
    }

  void Set_real_avail_def(const IDTYPE            e_version,
			        EXP_OCCURS *const avail_def)
    {
#if Is_True_On
      if (Avail_def(e_version) != NULL) {
	Set_redefined(e_version);
      }
      Set_occ_count(e_version, 1);
#endif
      Set_avail_def(e_version, avail_def);
    }

  BOOL Tracing() const
    { return _tracing; }

#if Is_True_On
  void Set_occ_count(const IDTYPE e_version,
		     const UINT32 occ_count)
    {
      Is_True(e_version < _n_versions, ("e_version out of range"));
      _e_ver_tab[e_version].Set_occ_count(occ_count);
    }

  UINT32 Inc_occ_count(const IDTYPE e_version,
		       const UINT32 increment = 1)
    {
      Is_True(e_version < _n_versions, ("e_version out of range"));
      return _e_ver_tab[e_version].Inc_occ_count(increment);
    }

  UINT32 Occ_count(const IDTYPE e_version) const
    {
      Is_True(e_version < _n_versions, ("e_version out of range"));
      return _e_ver_tab[e_version].Occ_count();
    }

  void Set_redefined(const IDTYPE e_version)
    {
      _e_ver_tab[e_version].Set_redefined();
    }

  BOOL Redefined(const IDTYPE e_version) const
    {
      return _e_ver_tab[e_version].Redefined();
    }

  UINT32 N_versions(void) const
    { return _n_versions; }
#endif
};
