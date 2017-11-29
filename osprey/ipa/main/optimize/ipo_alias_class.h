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


//-*-c++-*-
//
// This code implements:
// Steensgaard, B.  "Points-to analysis in almost linear time."  Proceedings 
// of 23rd Annual ACM SIGACT-SIGPLAN Symposium on Principles of Programming
// Languages.  Held: St. Petersburg Beach, FL, USA, 21-24 Jan. 1996.
// (USA:  ACM, 1996. p. 32-41)  (Conference paper - English)
//
// Issues:
//   o) How to handle varargs functions. Answer: Make the assumption
//   that function calls don't look at or do anything nefarious with
//   arguments that aren't passed to them. This seems reasonable. But
//   there is still an issue with how to handle the compilation of
//   varargs function definitions.
//
// ======================================================================
// Interface to the outside:
//   IP_ALIAS_CLASSIFICATION::IP_ALIAS_CLASSIFICATION
//     This constructor does only constant-time stuff, since the
//     IP_ALIAS_CLASSIFICATION instance gets constructed whether or
//     not alias classification will be performed. This unconditional
//     construction greatly simplifies the transfer of information
//     between IP_ALIAS_CLASSIFICATION and the rest of alias analysis.
//
//   void IP_ALIAS_CLASSIFICATION::Classify_memops(WN *)
//     This routine takes as a parameter a pointer to the FUNC_ENTRY
//     node for the compilation unit and gets the job of alias
//     classification done. The output from the routine is accessible
//     through the member function
//     IP_ALIAS_CLASSIFICATION::Alias_class(const WN *). Until and unless
//     this routine is called, the only other legal member calls are
//     to IP_ALIAS_CLASSIFICATION::Release_resources(void) and to the
//     IP_ALIAS_CLASSIFICATION destructor.
//
//   IDTYPE Alias_class(const WN *) const
//     This routine returns an identifier corresponding to the alias
//     equivalence class of the specified WHIRL node. The WHIRL node
//     must correspond to a memory operation (load, store, or
//     parameter). If two WN's have different alias classes, they
//     provably do not alias.
//
//   void Print(FILE *) const;
//     This routine prints ASCII information about the
//     IP_ALIAS_CLASSIFICATION instance on the specified file.
//
//   void Release_resources(void);
//     This routine discards all the resources associated with the
//     IP_ALIAS_CLASSIFICATION instance, in preparation to destruct the
//     instance. This routine is separate from the destructor because
//     we need a place to destroy the parts that get created only if
//     alias classification is actually done.
// ======================================================================

#ifndef opt_alias_class_INCLUDED
#define opt_alias_class_INCLUDED "opt_alias_class.h"

#include "cxx_memory.h"
#include "cxx_template.h"
#include "errors.h"
#include "tracing.h"
#include "opt_defs.h"
#include "opt_union_find.h"
#include "opt_wn.h"
#include "id_map.h"
#ifndef opt_points_to_INCLUDED
#include "opt_points_to.h"
#endif

extern "C" {
#include "bitset.h"
}

#ifdef KEY
using idmap::ID_MAP;
#endif

class IP_ALIAS_CLASS_REP;
class IP_ALIAS_CLASSIFICATION;
extern IP_ALIAS_CLASSIFICATION *Ip_alias_class;

class IP_ALIAS_CLASS_MEMBER : public U_F_ELEMENT<IP_ALIAS_CLASS_MEMBER> {
  // All data members of this class (aside from the base class) are
  // for debugging. None of them is supposed to have any effect on
  // what happens.
private:
  typedef enum {
    ACM_NONE,
    ACM_BASE,
    ACM_WN
  } ACM_KIND;

  ACM_KIND  _kind;
  union {
    IDTYPE    _base_id;
    // const WN *_wn;	// Not valid from one PU to the next
  };

  void    Set_kind(ACM_KIND kind)   { _kind = kind; }

public:
  IP_ALIAS_CLASS_MEMBER(IDTYPE base_id) :
    _kind(ACM_BASE), _base_id(base_id)
      { }

  /* ARGSUSED */
  IP_ALIAS_CLASS_MEMBER(const WN *wn) :
    _kind(ACM_WN)  // , _wn(wn)
      { }

  IP_ALIAS_CLASS_MEMBER(IDTYPE base_id, IP_ALIAS_CLASS_REP *acr) :
    _kind(ACM_BASE), _base_id(base_id)
      {	Put_in_set((U_F_REP<IP_ALIAS_CLASS_MEMBER> *) acr); }

  IP_ALIAS_CLASS_MEMBER(void) :
    _kind(ACM_NONE), _base_id(0)
      { }

  IDTYPE  Base_id(void) const
    {
      Is_True(_kind == ACM_BASE,
	      ("IP_ALIAS_CLASS_MEMBER::Base_id: ACM must be BASE kind"));
      return _base_id;
    }

  void    Set_base_id(IDTYPE base_id)
    {
      Is_True(_kind == ACM_NONE,
	      ("IP_ALIAS_CLASS_MEMBER::Set_base_id: ACM kind must not be set"));
      Set_kind(ACM_BASE);
      _base_id = base_id;
    }

#if DELETE_ME
  const WN *Wn(void) const
    {
      Is_True(_kind == ACM_WN,
	      ("IP_ALIAS_CLASS_MEMBER::Wn: ACM must be WN kind"));
      return _wn;
    }

  void    Set_wn(WN *wn)
    {
      Is_True(_kind == ACM_NONE,
	      ("IP_ALIAS_CLASS_MEMBER::Set_wn: ACM kind must not be set"));
      Set_kind(ACM_WN);
      _wn = wn;
    }
#endif

  void    Print(FILE *) const;

  // Do we need to define operators to cast to and from the base
  // class? What about copies of the base class member functions for
  // the derived class?

  // The following is non-const because find entails path
  // compression...
  IP_ALIAS_CLASS_REP *Alias_class(void)
    { return (IP_ALIAS_CLASS_REP *) Find(); }
};

struct IP_AC_VALUE_TYPE_REP {
  IP_ALIAS_CLASS_MEMBER *_data_member, *_code_member;

  IP_AC_VALUE_TYPE_REP(IP_ALIAS_CLASS_MEMBER *data_member,
		       IP_ALIAS_CLASS_MEMBER *code_member) :
    _data_member(data_member), _code_member(code_member)
      { }

  IP_AC_VALUE_TYPE_REP(void) :
    _data_member(NULL), _code_member(NULL)
      { }

  void                   Init(IP_ALIAS_CLASS_MEMBER *data_member,
			      IP_ALIAS_CLASS_MEMBER *code_member)
    {
      _data_member = data_member;
      _code_member = code_member;
    }

  IP_ALIAS_CLASS_MEMBER *const &Data_member(void) const { return _data_member; }
  IP_ALIAS_CLASS_MEMBER *const &Code_member(void) const { return _code_member; }

  IP_ALIAS_CLASS_MEMBER *&Data_member(void) { return _data_member; }
  IP_ALIAS_CLASS_MEMBER *&Code_member(void) { return _code_member; }
};

// Just a wrapper around IP_AC_VALUE_TYPE_REP, to keep our declared types
// strictly straight.
struct IP_AC_REF_TYPE_REP {
  IP_AC_VALUE_TYPE_REP _value;

  IP_AC_REF_TYPE_REP(IP_ALIAS_CLASS_MEMBER *data_member,
		     IP_ALIAS_CLASS_MEMBER *code_member) :
    _value(data_member, code_member)
      { }

  IP_ALIAS_CLASS_MEMBER *const &Data_member(void) const
    { return _value.Data_member(); }

  IP_ALIAS_CLASS_MEMBER *const &Code_member(void) const
    { return _value.Code_member(); }

  IP_ALIAS_CLASS_MEMBER *&Data_member(void)
    { return _value.Data_member(); }

  IP_ALIAS_CLASS_MEMBER *&Code_member(void)
    { return _value.Code_member(); }
};

struct IP_AC_LAMBDA_TYPE_REP {
private:
  // For now we glom all function returns together. I doubt we'll lose
  // much precision this way because most functions have only one
  // return anyway. This data member is hidden from outside because we
  // want to check that it is never accessed for a descriptor that we
  // know pertains exclusively to a memory allocation routine.
  IP_ALIAS_CLASS_MEMBER *returns;

public:
  BOOL                   frees_memory;
  BOOL                   returns_new_memory;
  BOOL                   arity_established;
  // Alias classification info for arguments is given in the same
  // order as the argument lists for the function(s) described by this
  // descriptor.
  vector<IP_ALIAS_CLASS_MEMBER *,
         mempool_allocator<IP_ALIAS_CLASS_MEMBER *> > fixed_args;
  IP_ALIAS_CLASS_MEMBER *remaining_args;

  IP_AC_LAMBDA_TYPE_REP(MEM_POOL *pool) :
    arity_established(FALSE), returns_new_memory(FALSE), frees_memory(FALSE),
    fixed_args(pool), returns(NULL), remaining_args(NULL)
      { }

  void            Set_arities(UINT, UINT, const IP_ALIAS_CLASSIFICATION *);

  IP_ALIAS_CLASS_MEMBER *Argument_member(UINT i) const
    {
      Is_True(arity_established,
	      ("IP_AC_LAMBDA_TYPE_REP: arity must be established"));
      IP_ALIAS_CLASS_MEMBER *arg_class_representative;
      if (i < fixed_args.size()) {
	arg_class_representative = fixed_args[i];
      }
      else {
	arg_class_representative = remaining_args;
      }
      return arg_class_representative;
    }

  IP_ALIAS_CLASS_MEMBER *&Returns(void)
    {
      Is_True(!returns_new_memory,
	      ("IP_AC_LAMBDA_TYPE_REP: should not use static "
	       "returns class for memory allocator"));
      return returns;
    }

  IP_ALIAS_CLASS_MEMBER *const &Returns(void) const
    {
      Is_True(!returns_new_memory,
	      ("IP_AC_LAMBDA_TYPE_REP: should not use static "
	       "returns class for memory allocator"));
      return returns;
    }

#ifdef KEY
  IP_ALIAS_CLASS_MEMBER *Return_class_member(void) const
    {
      if (returns_new_memory)
        return Memory_allocator_return_class_member (Ip_alias_class);
      else
        return returns;
    }
#endif

  IP_ALIAS_CLASS_MEMBER
  *Memory_allocator_return_class_member(IP_ALIAS_CLASSIFICATION *ac) const;

  /* ARGSUSED */
  IP_ALIAS_CLASS_REP *Return_class(UINT i = 0) const
    {
      Is_True(arity_established,
	      ("IP_AC_LAMBDA_TYPE_REP: arity must be established"));
      Is_True(!returns_new_memory,
	      ("IP_AC_LAMBDA_TYPE_REP: wrong member for memory allocator"));
      return returns->Alias_class();
    }

  void Union_func_args(IP_AC_LAMBDA_TYPE_REP &, MEM_POOL *, BOOL);
};

typedef enum {
  IP_ACR_BOTTOM_TYPE,
  IP_ACR_VALUE_TYPE,	// Will this ever be used?
  IP_ACR_REF_TYPE,
  IP_ACR_LAMBDA_TYPE
} IP_ACR_TYPE_SORT;

class IP_ACR_SLIST;

// The C++ class representing an alias class
class IP_ALIAS_CLASS_REP : public U_F_REP<IP_ALIAS_CLASS_MEMBER> {
  friend class IP_ALIAS_CLASSIFICATION;
  friend class IP_ACR_SLIST;
#if Is_True_On
  friend void  print_table(void);
#endif

  struct IP_AC_TYPE_INFO {
    IP_ACR_TYPE_SORT       _sort;

    // The following should be in a union.
    IP_AC_LAMBDA_TYPE_REP *_signature;
    IP_AC_REF_TYPE_REP     _ref;

    IP_AC_TYPE_INFO(void) :
      _sort(IP_ACR_BOTTOM_TYPE), _signature(NULL), _ref(NULL, NULL)
        { }
  };

public:
  typedef IP_ALIAS_CLASS_MEMBER        *PENDING_SET_ITEM;
  typedef slist<PENDING_SET_ITEM,
                mempool_allocator<PENDING_SET_ITEM> > PENDING_SET;

private:
  static IDTYPE          _last_id_used;
  static IP_ACR_SLIST    _free_list;
  static UINT32          _recycled_acr_nodes;

  union {
    IDTYPE               _id;
    IP_ALIAS_CLASS_REP  *_next_free;
  };

  PENDING_SET            _pending;

  IP_AC_TYPE_INFO        _type_info;

public:
  IP_ALIAS_CLASS_REP(MEM_POOL *pool) :
    _id(++_last_id_used),
    _pending(pool)
      { }

  ~IP_ALIAS_CLASS_REP(void)
    {
      if (Sort() == IP_ACR_LAMBDA_TYPE) {
	// TODO: Destruct the signature and free its memory
      }
    }

  void Free_acr(void);

  IDTYPE            Id(void) const { return _id; }

  const IP_AC_REF_TYPE_REP &Ref(void) const
    {
      Is_True(_type_info._sort == IP_ACR_REF_TYPE,
	      ("Illegal REF access"));
      return _type_info._ref;
    }

  IP_AC_REF_TYPE_REP &Ref(void)
    {
      Is_True(_type_info._sort == IP_ACR_REF_TYPE,
	      ("Illegal REF access"));
      return _type_info._ref;
    }

  const IP_AC_LAMBDA_TYPE_REP &Signature(void) const
    {
      Is_True(_type_info._sort == IP_ACR_LAMBDA_TYPE,
	      ("Illegal LAMBDA access"));
      return *_type_info._signature;
    }

  IP_AC_LAMBDA_TYPE_REP &Signature(void)
    {
      Is_True(_type_info._sort == IP_ACR_LAMBDA_TYPE,
	      ("Illegal LAMBDA access"));
      return *_type_info._signature;
    }

  IP_ACR_TYPE_SORT  Sort(void) const
    {
      Is_True((_type_info._sort != IP_ACR_BOTTOM_TYPE) ||
	      ((_type_info._ref.Data_member() == NULL) &&
	       (_type_info._ref.Code_member() == NULL) &&
	       (_type_info._signature == NULL)),
	      ("BOTTOM-typed ACR must not have ref or signature fields"));
      Is_True((_type_info._sort != IP_ACR_REF_TYPE) ||
	      ((Ref().Data_member() != NULL) &&
	       (Ref().Code_member() != NULL) &&
	       (_type_info._signature == NULL)),
	      ("REF-typed ACR must have ref fields set up and no signature"));
      Is_True((_type_info._sort != IP_ACR_LAMBDA_TYPE) ||
	      ((_type_info._ref.Data_member() == NULL) &&
	       (_type_info._ref.Code_member() == NULL) &&
	       (_type_info._signature != NULL)),
	      ("LAMBDA-typed ACR must have signature set up and no "
	       "ref fields"));
      return _type_info._sort;
    }

  void              Add_pending(IP_ALIAS_CLASS_REP *, BOOL);
  void              Merge_pending(IP_ALIAS_CLASS_REP &, BOOL);
  void              Process_pending(IP_ALIAS_CLASS_MEMBER *,
				    MEM_POOL *, BOOL);
  PENDING_SET      &Pending(void) { return _pending; }

  void              Join(IP_ALIAS_CLASS_REP &, MEM_POOL *, BOOL);

  IP_ALIAS_CLASS_REP  *Data_class_pointed_to(void) const
    {
      Is_True(Sort() == IP_ACR_REF_TYPE,
	      ("IP_ALIAS_CLASS_REP::Data_class_pointed_to: must be "
	       "REF type"));
      return Ref().Data_member()->Alias_class();
    }

  IP_ALIAS_CLASS_REP *Func_class_pointed_to(void) const
    {
      Is_True(Sort() == IP_ACR_REF_TYPE,
	      ("IP_ALIAS_CLASS_REP::Func_class_pointed_to: must be "
	       "REF type"));
      return Ref().Code_member()->Alias_class();
    }

  void Settype(const IP_AC_REF_TYPE_REP &, MEM_POOL *, BOOL);

  // Note that only representations outside the free list can be printed.
  void              Print(FILE *,
			  IP_ALIAS_CLASS_REP *global_class = NULL) const;
};

struct IP_ACR_SLIST {
  IP_ALIAS_CLASS_REP *head;

public:
  void push_front(IP_ALIAS_CLASS_REP *acr)
    {
      acr->_next_free = head;
      head = acr;
    }

  IP_ALIAS_CLASS_REP *front(void) const { return head; }

  IP_ALIAS_CLASS_REP *pop_front(void)
    {
      IP_ALIAS_CLASS_REP *retval = head;
      head = head->_next_free;
      return retval;
    }
};

struct AC_ST_IDENTIFIER {
  ST_IDX pu_st_idx;		// ST_IDX of the PU that declares this
				// ST; must be in the global symtab.
  ST_IDX base_st_idx;		// ST_IDX of the base ST when it is in
				// scope.
  void Set_pu_st_idx(ST_IDX pu)
    {
      Is_True(ST_IDX_level(pu) == GLOBAL_SYMTAB ||
	      pu == (ST_IDX) 0,
	      ("IP_ALIAS_CLASSIFICATION: PU must be global symbol"));
      pu_st_idx = pu;
    }

  void Set_base_st_idx(ST_IDX base) { base_st_idx = base; }
};

struct AC_PREG_IDENTIFIER {
  ST_IDX   pu_st_idx;		// ST_IDX of the PU using this PREG;
				// must be in the global symtab.
  mINT32  preg_num;

  void Set_pu_st_idx(ST_IDX pu)
    {
      Is_True(ST_IDX_level(pu) == GLOBAL_SYMTAB,
              ("IP_ALIAS_CLASSIFICATION: PU must be global symbol"));
      pu_st_idx = pu;
    }

  void Set_preg_num(INT64 preg)
    {
      preg_num = (mINT32) preg;
      Is_True((INT64) preg_num == preg,
	      ("preg number overflows 32 bits"));
    }
};

static bool operator==(AC_ST_IDENTIFIER x, AC_ST_IDENTIFIER y)
{
  return (x.pu_st_idx   == y.pu_st_idx &&
	  x.base_st_idx == y.base_st_idx);
}

static bool operator==(AC_PREG_IDENTIFIER x, AC_PREG_IDENTIFIER y)
{
  return (x.pu_st_idx == y.pu_st_idx &&
	  x.preg_num  == y.preg_num);
}

class IP_BASE_ID_MAP_ENTRY;	// defined in ipo_alias_class.cxx.

class IP_ALIAS_CLASSIFICATION {
  friend class IP_ALIAS_CLASS_REP;
  friend class IP_AC_LAMBDA_TYPE_REP;
  friend class MERGE_NEST_REF_CLASSES;
  friend class CLASSIFY_INITIALIZED_DATA;

private:
  ID_MAP<IDTYPE, AC_ST_IDENTIFIER>    _st_id_to_base_id_map;

  // The following is a dynamic array of pointers rather than a
  // dynamic array of IP_BASE_ID_MAP_ENTRYs because it makes
  // construction cleaner when we allocate a new entry. The DYN_ARRAY
  // template doesn't provide a way to pass constructor arguments when
  // the dynamic array is enlarged.
  DYN_ARRAY<IP_BASE_ID_MAP_ENTRY *>   _base_id_map;

  ID_MAP<IDTYPE, AC_PREG_IDENTIFIER>  _preg_id_to_base_id_map;

  WN_MAP                              _memop_classification_map;
  WN_MAP                              _indir_classification_map;
  BOOL                                _memops_classified;
  BOOL                                _maps_initialized;
  MEM_POOL                           *_pool;
  BOOL                                _mem_pool_valid;
  IP_ALIAS_CLASS_MEMBER              *_member_of_global_data_class;
  IP_ALIAS_CLASS_MEMBER              *_member_of_global_func_class;
  IP_ALIAS_CLASS_MEMBER              *_member_of_misused_code_class;
  IP_ALIAS_CLASS_MEMBER              *_member_of_const_addr_class;

  BOOL                                _tracing;
  const BOOL                          _verbose;

  // Member functions for internal use
  IP_ALIAS_CLASS_REP *Class_of_base_id(IDTYPE) const;

  BOOL                Can_be_referred_to_by_code(ST_IDX);
  void                Merge_initial_value_classes(IDTYPE,
						  INITV_IDX);
  void                Classify_inito(const INITO *);
  WN                 *Classify_wn_and_kids(WN *);

  IP_ALIAS_CLASS_MEMBER *Classify_lhs_of_store(WN *);

  void                Classify_deref_of_expr(IP_ALIAS_CLASS_MEMBER *,
					     WN *, BOOL);

  IP_ALIAS_CLASS_MEMBER *Incorporate_call_and_parm_flags(      IP_ALIAS_CLASS_MEMBER *,
							 const WN                    *);

  void                Handle_function_definition(WN *);
  WN                 *Handle_assignment(WN *);
  WN                 *Handle_return_val(WN *);              
  WN                 *Handle_call(WN *);

  BOOL                Callee_saves_no_parms(const WN *);
  BOOL                Callee_returns_new_memory(const WN *);
  BOOL                Callee_returns_new_memory(const ST *);
  BOOL                Callee_frees_memory(const WN *);
  BOOL                Callee_frees_memory(const ST *);
  BOOL                Stmt_stores_return_value_from_callee(const WN *);
  BOOL                Stmt_stores_return_value_to_caller(const WN *);
  BOOL                Uses_no_return_value(const WN *);

  WN_MAP              Indir_classification_map(void) const
    { return _indir_classification_map; }

  void                Finalize_ac_map(WN *);
  void                Finalize_ac_map_wn(WN *);

  IP_ALIAS_CLASS_REP    *New_alias_class(IP_ACR_TYPE_SORT,
					 IP_ALIAS_CLASS_MEMBER &) const;

  IP_ALIAS_CLASS_MEMBER *New_alias_class_member(void) const;
#if DELETE_ME
  IP_ALIAS_CLASS_MEMBER *New_alias_class_member(IDTYPE) const;
#endif // DELETE_ME
  IP_ALIAS_CLASS_MEMBER *New_alias_class_member(const WN *) const;

  void Find_declared_base_and_offset(ST_IDX, ST_IDX &, INT64 &);

  IDTYPE              New_base_id(ST_IDX, ST_IDX);
  IDTYPE              ST_base_id(ST_IDX, ST_IDX);
  IDTYPE              Base_id(const ST *, INT64);
  IDTYPE              WN_base_id(const WN *);
  ID_MAP<IDTYPE, AC_PREG_IDENTIFIER> &Preg_id_to_base_id_map(void)
    { return _preg_id_to_base_id_map; }

  MEM_POOL           *Pool(void) const { return _pool; }

  void                Conditional_join(IP_ALIAS_CLASS_REP *,
				       IP_ALIAS_CLASS_REP *);

  IP_ALIAS_CLASS_REP *Global_data_class(void) const
    { return _member_of_global_data_class->Alias_class(); }

  IP_ALIAS_CLASS_REP *Global_func_class(void) const
    { return _member_of_global_func_class->Alias_class(); }

  IP_ALIAS_CLASS_REP *Const_addr_class(void) const
    { return _member_of_const_addr_class->Alias_class(); }

  IP_ALIAS_CLASS_REP *Class_of_code_misused_as_data(void) const
    { return _member_of_misused_code_class->Alias_class(); }

  IDTYPE              Alias_class(IDTYPE) const;

  BOOL                Tracing(void) const { return _tracing; }

  WN_MAP              Memop_classification_map(void) const
    { return _memop_classification_map; }

public:
  IP_ALIAS_CLASSIFICATION(MEM_POOL *pool) :
    _pool(pool), _base_id_map(pool), _verbose(TRUE),
    _memop_classification_map(WN_MAP_ALIAS_CLASS),
    _indir_classification_map(WN_MAP_AC_INTERNAL),
    _preg_id_to_base_id_map(128, (IDTYPE) 0, pool,
			    Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)),
    _st_id_to_base_id_map(256, (IDTYPE) 0, pool,
			  Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)),
    _memops_classified(FALSE), _maps_initialized(FALSE),
    _mem_pool_valid(TRUE)
      {
	// Assert that structures used as ID_MAP hash keys have no
	// padding. Such padding can have garbage in it that makes two
	// component-wise equal structs hash to different locations.
	{
	  AC_ST_IDENTIFIER dummy;
	  FmtAssert(sizeof(dummy) ==
		    (sizeof(dummy.pu_st_idx) +
		     sizeof(dummy.base_st_idx)),
		    ("AC_ST_IDENTIFIER must not have padding"));
	}
	{
	  AC_PREG_IDENTIFIER dummy;
	  FmtAssert(sizeof(dummy) ==
		    (sizeof(dummy.pu_st_idx) +
		     sizeof(dummy.preg_num)),
		    ("AC_PREG_IDENTIFIER must not have padding"));
	}

	MEM_POOL_Push(_pool);
	if (Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG)) {
	  _tracing = TRUE;
	}
	else {
	  _tracing = FALSE;
	}
	IP_ALIAS_CLASS_REP::_last_id_used = PESSIMISTIC_AC_ID;
	IP_ALIAS_CLASS_REP::_free_list.head = NULL;
	IP_ALIAS_CLASS_REP::_recycled_acr_nodes = 0;

	// Set up the _member_of_global_data_class:
	_member_of_global_data_class = New_alias_class_member();
	IP_ALIAS_CLASS_REP *dummy_acr =
	  New_alias_class(IP_ACR_REF_TYPE,
			  *_member_of_global_data_class);
	dummy_acr->Data_class_pointed_to()->Join(*dummy_acr, pool, _tracing);

	// Set up the _const_addr_class:
	_member_of_const_addr_class = New_alias_class_member();
	dummy_acr =
	  New_alias_class(IP_ACR_REF_TYPE, *_member_of_const_addr_class);
	dummy_acr->Data_class_pointed_to()->Join(*Global_data_class(),
						 pool, _tracing);

	// Set up the _global_func_class:
	_member_of_global_func_class = New_alias_class_member();
	dummy_acr = New_alias_class(IP_ACR_LAMBDA_TYPE,
				    *_member_of_global_func_class);
	dummy_acr->Signature().remaining_args = _member_of_global_data_class;
	dummy_acr->Signature().Returns()      = _member_of_global_data_class;
        dummy_acr->Signature().arity_established = TRUE;
	_member_of_global_data_class->Alias_class()->
	  Func_class_pointed_to()->Join(*dummy_acr, pool, _tracing);

	_member_of_misused_code_class = New_alias_class_member();
	(void) New_alias_class(IP_ACR_REF_TYPE,
			       *_member_of_misused_code_class);

	// Skip base ID zero
	_base_id_map.Initidx(0);
      }

  void                Init_maps(void);

  void                Release_resources(void);

  void                Classify_initialized_data(INITO_TAB *);

  void                Classify_memops(WN *);

  void                Finalize_memops(WN *);

  IDTYPE              Alias_class(const WN *) const;

  void                Print(FILE *) const;
};

inline IP_ALIAS_CLASS_MEMBER *
IP_AC_LAMBDA_TYPE_REP::Memory_allocator_return_class_member(IP_ALIAS_CLASSIFICATION *ac) const
{
  Is_True(returns_new_memory,
	  ("IP_AC_LAMBDA_TYPE_REP: must be memory allocator"));
  IP_ALIAS_CLASS_REP *return_value_class =
    ac->New_alias_class(IP_ACR_REF_TYPE, *ac->New_alias_class_member());
  IP_ALIAS_CLASS_REP *return_deref_class =
    ac->New_alias_class(IP_ACR_REF_TYPE, *ac->New_alias_class_member());
  return_value_class->Data_class_pointed_to()->Join(*return_deref_class,
						    ac->Pool(),
						    ac->Tracing());
  return return_value_class->Representative();
}

extern vector<char *> Ip_alias_class_files;
#endif
