//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_mu_chi.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_mu_chi.h,v $
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


#ifndef opt_mu_chi_INCLUDED
#define opt_mu_chi_INCLUDED "opt_mu_chi.h"
#ifdef _KEEP_RCS_ID
static char *opt_mu_chircs_id = opt_mu_chi_INCLUDED"$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "opcode.h"
#include "opt_sym.h"

typedef struct pf_pointer PF_POINTER;


//  PF_NODE is a linked list of PF_POINTER
//
class PF_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS (PF_NODE)
private:
  PF_POINTER *_pf_pointer;
public:
  PF_NODE(void) {};
  PF_NODE(const PF_NODE&);
  PF_NODE& operator = (const PF_NODE&);
  PF_POINTER *Pf_pointer(void) const { return _pf_pointer; }
  void Set_pf_pointer(PF_POINTER *p) { _pf_pointer = p; }
};


//  PF_LIST is a internal linked list
//
class PF_LIST : public SLIST {
  DECLARE_SLIST_CLASS (PF_LIST, PF_NODE)
private:
  PF_LIST(const PF_LIST&);
  PF_LIST& operator = (const PF_LIST&);
public:
  PF_NODE *New_pf_node(PF_POINTER *pf, MEM_POOL *pool) 
    { PF_NODE *p = (PF_NODE *) CXX_NEW (PF_NODE , pool);
      p->Set_pf_pointer(pf);
      Append(p);
      return p;
    }
  void  Print(FILE *fp=stderr);
};


class PF_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (PF_LIST_ITER, PF_NODE, PF_LIST)
  PF_LIST_ITER(const PF_LIST_ITER&);
  PF_LIST_ITER& operator = (const PF_LIST_ITER&);
public:
  ~PF_LIST_ITER(void)		{}
  PF_POINTER *First_elem(void)	{ return First()->Pf_pointer(); }
  PF_POINTER *Next_elem(void)	{ return Next()->Pf_pointer();  }
};


//  MU_NODE:  contain one opnd.
//
class MU_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(MU_NODE)
private:
  AUX_ID     _aux_id;
  union {
    VER_ID   _opnd;
    CODEREP  *_cr;
  } _u;
public:
  MU_NODE(void) {};
  MU_NODE(const MU_NODE& mn)          { Set_aux_id(mn._aux_id);
					_u = mn._u; }
  MU_NODE& operator = (const MU_NODE&);
  AUX_ID   Aux_id(void) const         { return _aux_id; }
  VER_ID   Opnd(void) const           { return _u._opnd; }
  CODEREP *OPND(void) const           { return _u._cr; }
  BOOL	   Is_Valid(void) const       { return _u._cr != NULL; }
  void     Set_aux_id(AUX_ID var)     { _aux_id = var; }
  void     Set_opnd(VER_ID opnd)      { _u._opnd = opnd; }
  void     Set_OPND(CODEREP *cr, BOOL set_dont_prop = TRUE);
  void     Init(AUX_ID var)           { Set_aux_id(var); Set_opnd(var); }
  void     Clone(MU_NODE *mu)         { Set_aux_id(mu->Aux_id()); Set_OPND(mu->OPND()); }
  void     Print(FILE *fp=stderr) const;
  void     Print_Deref(FILE *fp=stderr) const;
};

//  MU_LIST is a internal linked list
//
class MU_LIST : public SLIST {
  DECLARE_SLIST_CLASS (MU_LIST, MU_NODE)
private:
  MU_LIST(const MU_LIST&);
  MU_LIST& operator = (const MU_LIST&);

  BOOL     Contains(AUX_ID var);
public:
  // add a new mu node
  MU_NODE *New_mu_node(AUX_ID var, MEM_POOL *pool) {
    MU_NODE *p = (MU_NODE *) CXX_NEW (MU_NODE , pool);
    p->Set_aux_id(var);
    p->Set_opnd(var);
    Append(p);
    return p;
  }

  //  Check the mu list for a duplicate before adding the mu-node
  MU_NODE *New_mu_node_no_dups(AUX_ID var, MEM_POOL *pool) {
    if (MU_NODE *munode = Search_mu_node( var ))
      return munode;
    return New_mu_node( var, pool );
  }

  MU_NODE *Clone_mu_node(MU_NODE *mu, MEM_POOL *pool) {
    MU_NODE *p = (MU_NODE *) CXX_NEW (MU_NODE , pool);
    p->Clone(mu);
    Append(p);
    return p;
  }

  inline void Clone_mu_list(MU_LIST *mu_list, MEM_POOL *pool);

  MU_NODE *Search_mu_node(AUX_ID var);
  MU_NODE *New_mu_node_w_cur_vse(AUX_ID var, VER_ID vse, MEM_POOL *pool);
  void 	   Delete_def_at_entry_mus(OPT_STAB *opt_stab);
  void  Print(FILE *fp=stderr);
};


class MU_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (MU_LIST_ITER, MU_NODE, MU_LIST)
  MU_LIST_ITER(const MU_LIST_ITER&);
  MU_LIST_ITER& operator = (const MU_LIST_ITER&);
public:
  ~MU_LIST_ITER(void)		{}
  MU_NODE *First_elem(void)	{ return First(); }
  MU_NODE *Next_elem(void)	{ return Next();  }
};


inline void 
MU_LIST::Clone_mu_list(MU_LIST *mu_list, MEM_POOL *pool) 
{
  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;
  FOR_ALL_NODE( mnode, mu_iter, Init(mu_list) ) {
    Clone_mu_node(mnode, pool);
  }
}


//  CHI_NODE:  contain one result and one opnd field.
//
class CHI_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(CHI_NODE)
private:
  struct {             // overloading _aux_id for the Live flag
    UINT32 _live:1;    // WATCH OUT: LITTLE ENDIANess porting
    UINT32 _dse_dead:1; // this chi result is found dead by DSE
    UINT32 _ssu_processed:1; // flag used in SSU to marked processed
    AUX_ID _aux_id:29;
  } _id;
  union {
    VER_ID   _ver;
    CODEREP  *_cr;
  } _result;
  union {
    VER_ID   _ver;
    CODEREP  *_cr;
  } _opnd;

public:
  CHI_NODE(void) {};
  CHI_NODE(const CHI_NODE&);
  CHI_NODE& operator = (const CHI_NODE&);

  AUX_ID   Aux_id(void) const         { return _id._aux_id; }
  BOOL     Live(void) const           { return _id._live; }
  BOOL     Dse_dead(void) const       { return _id._dse_dead; }
  BOOL     Ssu_processed(void) const  { return _id._ssu_processed; }
  VER_ID   Opnd(void) const           { return _opnd._ver; }
  CODEREP *OPND(void) const           { return _opnd._cr; }
  VER_ID   Result(void) const         { return _result._ver; }
  CODEREP *RESULT(void) const         { return _result._cr; }
  void     Set_aux_id(AUX_ID var)     { _id._aux_id = var; }
  void     Set_live(BOOL b)           { _id._live = b; }
  void     Set_dse_dead(BOOL b)       { _id._dse_dead = b; }
  void     Set_ssu_processed(BOOL b)  { _id._ssu_processed = b; }
  void     Set_opnd(VER_ID opnd)      { _opnd._ver = opnd; }
  void     Set_OPND(CODEREP *cr, BOOL set_dont_prop = TRUE);
  void     Set_result(VER_ID ver)     { _result._ver = ver; }
  void     Set_RESULT(CODEREP *cr)    { _result._cr = cr; }
  void     Clone(CHI_NODE *chi) {
    Set_live(chi->Live());
    Set_dse_dead(chi->Dse_dead());
    Set_ssu_processed(chi->Ssu_processed());
    Set_aux_id(chi->Aux_id());
    Set_OPND(chi->OPND(), FALSE);
    Set_RESULT(chi->RESULT());
  }
  CHI_NODE *Copy_chi_node(MEM_POOL *pool) {
    CHI_NODE *p = (CHI_NODE *) CXX_NEW (CHI_NODE, pool);
    p->Clone(this);
    return p;
  }
  void     Print(FILE *fp=stderr) const;
  void     Print_Deref(FILE *fp=stderr) const;
};


class CHI_LIST : public SLIST {
  DECLARE_SLIST_CLASS (CHI_LIST, CHI_NODE)
private:
  CHI_LIST(const CHI_LIST&);
  CHI_LIST& operator = (const CHI_LIST&);
public:
  // add a new chi node
  CHI_NODE *New_chi_node(AUX_ID var, MEM_POOL *pool) {
    CHI_NODE *p = (CHI_NODE *) CXX_NEW (CHI_NODE , pool);
    p->Set_live(FALSE);
    p->Set_dse_dead(FALSE);
    p->Set_ssu_processed(FALSE);
    p->Set_aux_id(var);
    p->Set_opnd(var);
    p->Set_result(var);
    Append(p);
    return p;
  }

  //  Check the chi list for a duplicate before adding the chi-node
  CHI_NODE *New_chi_node_no_dups(AUX_ID var, MEM_POOL *pool) {
    if (CHI_NODE *chinode = Search_chi_node( var ))
      return chinode;
    return New_chi_node( var, pool );
  }

  CHI_NODE *Clone_chi_node(CHI_NODE *chi, MEM_POOL *pool) {
    CHI_NODE *p = (CHI_NODE *) CXX_NEW (CHI_NODE , pool);
    p->Clone(chi);
    Append(p);
    return p;
  }

  inline void Clone_chi_list(CHI_LIST *chi_list, MEM_POOL *pool);

  CHI_NODE *Search_chi_node(AUX_ID var);
  void  Print(FILE *fp=stderr);
};


class CHI_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (CHI_LIST_ITER, CHI_NODE, CHI_LIST)
  CHI_LIST_ITER(const CHI_LIST_ITER&);
  CHI_LIST_ITER& operator = (const CHI_LIST_ITER&);
public:
  ~CHI_LIST_ITER(void)		{}
  CHI_NODE *First_elem(void)	{ return First(); }
  CHI_NODE *Next_elem(void)	{ return Next();  }
};


inline void 
CHI_LIST::Clone_chi_list(CHI_LIST *chi_list, MEM_POOL *pool) 
{
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  FOR_ALL_NODE( cnode, chi_iter, Init(chi_list) ) {
    Clone_chi_node(cnode, pool);
  }
}


//  one entry for each load/store.
//
class OCC_TAB_ENTRY {

  AUX_ID            _aux_id;  // index into aux_stab
  WN                *_wn;
  union {
    // for ILOAD, MLOAD, ISTORE, MSTORE
    struct {
      union {
	MU_NODE     *_mu_node;
	CHI_LIST    *_chi_list;
      } _u2;
      INT32         _lno_dep_vertex_load;  // vertex index for LNO ILOAD
      INT32         _lno_dep_vertex_store; // vertex index for LNO ISTORE
      PF_POINTER    *_pf_pointer;     // Pointer to prefetch info
    } _is_mem;

    // for CALL, RETURN, PREFETCH
    struct {
      PF_LIST       *_pf_list;
      MU_LIST       *_mu_list;
      CHI_LIST      *_chi_list;
      POINTS_TO_LIST *_pt_list;
    } _is_stmt;  
  } _u1;
  // _points_to is only valid for _is_mem.
  // It need to be outside the union because of C++ constructor requirements.
  POINTS_TO     _points_to;    

public:

  OCC_TAB_ENTRY(const OCC_TAB_ENTRY &occ)
  {
    _aux_id = occ._aux_id;
    _wn     = occ._wn;
    _u1     = occ._u1;
    (&_points_to)->Copy_fully(&(occ._points_to));
  }

  OCC_TAB_ENTRY(void) { }

  AUX_ID        Aux_id(void) const             { return _aux_id; }
  BOOL          Is_stmt(void) const            { return _aux_id == 0; }
  BOOL          Is_mem(void) const             { return _aux_id != 0; }
  BOOL          Is_load(void) const            { return Is_mem() && (WN_operator(_wn) == OPR_PARM || OPCODE_is_load(WN_opcode(_wn))); }
  BOOL          Is_store(void) const           { return Is_mem() && (WN_operator(_wn) == OPR_OPT_CHI || OPCODE_is_store(WN_opcode(_wn))); }
  WN            *Wn(void) const                { return _wn; }
  INT32         Lno_dep_vertex_load(void) const  { return _u1._is_mem._lno_dep_vertex_load; }
  INT32         Lno_dep_vertex_store(void) const { return _u1._is_mem._lno_dep_vertex_store; }
  PF_POINTER    *Pf_pointer(void) const          { return _u1._is_mem._pf_pointer; }
  
  POINTS_TO     *Points_to(void) {
    Is_True(WN_operator(_wn) == OPR_PARM || OPCODE_is_load(WN_opcode(_wn)) || OPCODE_is_store(WN_opcode(_wn)), 
	    ("wn is not load/store."));
    return &_points_to;
  }
  MU_NODE       *Mem_mu_node(void) const {
    Is_True(Is_load(), ("occ tab entry not a load."));
    return _u1._is_mem._u2._mu_node;
  }
  MU_LIST       *Stmt_mu_list(void) const {
    Is_True(Is_stmt(), ("occ tab entry not a stmt."));
    return _u1._is_stmt._mu_list;
  }
  CHI_LIST      *Mem_chi_list(void) const {
    Is_True(Is_store(), ("occ tab entry not a store."));
    return _u1._is_mem._u2._chi_list;
  }
  CHI_LIST      *Stmt_chi_list(void) const {
    Is_True(Is_stmt(), ("occ tab entry not a stmt."));
    return _u1._is_stmt._chi_list;
  }
  PF_LIST       *Pf_list(void) const {
    Is_True(Is_stmt(), ("occ tab entry not a stmt."));
    return _u1._is_stmt._pf_list;
  }
  POINTS_TO_LIST *Pt_list(void) const {
    Is_True(Is_stmt(), ("occ tab entry not a stmt."));
    return _u1._is_stmt._pt_list;
  }

  void  Set_aux_id(AUX_ID var)        { _aux_id = var; }
  void  Set_wn(WN *wn)                { _wn = wn; }
  void  Set_lno_dep_vertex_load(INT32 idx)  { _u1._is_mem._lno_dep_vertex_load = idx; }
  void  Set_lno_dep_vertex_store(INT32 idx) { _u1._is_mem._lno_dep_vertex_store = idx; }
  void  Set_pf_pointer(PF_POINTER *pf){ _u1._is_mem._pf_pointer = pf; }
  void  Set_mem_mu_node(MU_NODE *mu_node)
    {  Is_True(Is_load(), ("occ tab entry not a load."));
       _u1._is_mem._u2._mu_node = mu_node;
     }
  void  Set_stmt_mu_list(MU_LIST *mu_list)
    {  Is_True(Is_stmt(), ("occ tab entry not a stmt."));
       _u1._is_stmt._mu_list = mu_list;
     }
  void  Set_mem_chi_list(CHI_LIST *chi_list)
    {  Is_True(Is_store(), ("occ tab entry not a store."));
       _u1._is_mem._u2._chi_list = chi_list;
     }
  void  Set_stmt_chi_list(CHI_LIST *chi_list)
    {  Is_True(Is_stmt(), ("occ tab entry not a stmt."));
       _u1._is_stmt._chi_list = chi_list;
     }
  void  Set_pf_list(PF_LIST *pf_list)
    {  Is_True(Is_stmt(), ("occ tab entry not a stmt."));
       _u1._is_stmt._pf_list = pf_list; 
     }
  void  Set_pt_list(POINTS_TO_LIST *pt_list)
    {  Is_True(Is_stmt(), ("occ tab entry not a stmt."));
       _u1._is_stmt._pt_list = pt_list; 
     }

  void  New_mem_mu_node(AUX_ID var, MEM_POOL *pool) 
    {  MU_NODE *mnode = CXX_NEW(MU_NODE, pool);
       mnode->Init(var);
       Set_mem_mu_node(mnode);
    }
  
  void  Clone(OCC_TAB_ENTRY *, MEM_POOL *);

  void  Print(FILE *fp=stderr);
      
};


#endif  // opt_mu_chi_INCLUDED
