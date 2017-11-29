//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_efinalize.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_efinalize.h,v $
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


class REHASH_INFO {
private:
  typedef enum {
    RI_NONE              = 0x0,
    RI_CODEREP_UNOWNABLE = 0x1
  } RI_FLAGS;

  UINT32            _flags;
  EXP_OCCURS *      _owning_t_ver;
  UINT32            _max_rehash_cost;
  CODEREP    *const _coderep;

public:
  REHASH_INFO(CODEREP *coderep) : _coderep(coderep)
    {
      _owning_t_ver    = NULL;
      _max_rehash_cost = 0;
      _flags           = 0;
    }

  CODEREP    *Coderep() const             { return _coderep; }

  void        Set_coderep_unownable(void)
    { _flags |= RI_CODEREP_UNOWNABLE; }

  BOOL        Coderep_unownable(void) const
    { return _flags & RI_CODEREP_UNOWNABLE; }

  void        Set_max_rehash_cost(UINT32 cost)
    { _max_rehash_cost = cost; }

  UINT32      Max_rehash_cost(void) const { return _max_rehash_cost; }

  void        Set_owning_t_ver(EXP_OCCURS *occ)
    { _owning_t_ver = occ; }

  EXP_OCCURS *Owning_t_ver(void) const    { return _owning_t_ver; }

  void        Print(FILE *fp) const
    {
      if (_owning_t_ver != NULL) {
	_owning_t_ver->Print(fp);
      }
      else {
	fprintf(fp, "  unowned");
      }
      if (Coderep_unownable()) {
	fprintf(fp, " / unownable");
      }
      fprintf(fp, "\n");
    }
};
