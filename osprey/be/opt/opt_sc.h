/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
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

// ===============================================================================================
//
// Description:
//
//                   Structured  Component Nodes
//                   --------------------------
//      This is the structured component node data structure that represents if-region, loop-region
//      and other structured control flow components in the source program. 
//
// Reserved prefix:
// ----------------
//
//     SC         for structured component node
//
// Exported types:
// ---------------
//     SC_NODE
//
//          A structured component node.  Every structured component node contains the
//          following fields.
//
//              IDTYPE     _id
//                    A unique identifier for a structured component node.
//
//              SC_TYPE    type
//                    The type of a SC_NODE.  See SC_TYPE.
//
//              MEM_POOL * pool
//                    Allocation memory pool.
//
//
//              BB_NODE * bb_rep
//                    A pointer to representing BB_NODE.  
//
//              BB_LIST * bbs
//                    A list of BB_NODEs contained in this SC_NODE.
//
//              SC_NODE * parent
//                    A pointer to the parent node.
//
//              SC_LIST * kids
//                    A pointer to the list of child nodes.  The list is ordered
//                    according to order of occurrence in the procedure.      
//
//              IDTYPE _class_id
//                    ID of the loop class this SC_NODE belongs.  Scratch field. 
//                    Loops are classified according to their path symmetricities in the SC tree.
//
//              int _depth
//                    Depth of this SC_NODE in the SC tree. Scratch field.
//
//              int flag
//                    Flag of this SC_NODE.  see SC_NODE_FLAG.
//
//              SC_NODE * next
//                    A pointer to the next node in a lost.  Scratch field.
//
//
//     SC_LIST :SLIST_NODE
//
//          A singly linked list of SC_NODEs.  We use this class to build the children node list.
//
//     SC_LIST_CONTAINER : SLIST
//     SC_LIST_ITER : SLIST_ITER
//
//          The standard container and iterator for singly linked list.
//     
//
// ==============================================================================================

#ifndef opt_sc_INCLUDED
#define opt_sc_INCLUDED "opt_sc.h"
#include <iterator>

#ifndef opt_bb_INCLUDED
#include "opt_bb.h"
#endif
#ifndef opt_base_INCLUDED
#include "opt_base.h"
#endif
#ifndef opt_main_INCLUDED
#include "opt_main.h"
#endif
#ifndef opt_alias_class_INCLUDED
#include "opt_alias_class.h"
#endif
#ifndef wn_simp_INCLUDED
#include "wn_simp.h"
#endif

extern "C" {
#include "bitset.h"
}

// Type of SC nodes
enum SC_TYPE {
  SC_NONE = 0,
  SC_IF,     // if-region
  SC_THEN,   // then-path of a if-region
  SC_ELSE,   // else-path of a if-region
  SC_LOOP,   // loop region
  SC_BLOCK,  // blocks of straight-line codes
  SC_FUNC,   // func entry
  SC_LP_START, // H-WHIRL loop start
  SC_LP_COND,  // H-WHIRL loop cond
  SC_LP_STEP,  // H-WHIRL loop step
  SC_LP_BACKEDGE,  // H-WHIRL loop backedge
  SC_LP_BODY,  // loop body
  SC_COMPGOTO, // COMPGOTO
  SC_OTHER   // other structured control flows
};

extern BOOL SC_type_has_rep(SC_TYPE type);
extern BOOL SC_type_has_bbs(SC_TYPE type);
extern BOOL SC_type_is_marker(SC_TYPE type);

static const char * sc_type_name[] =
  {"NONE", "IF", "THEN", "ELSE", "LOOP", "BLOCK", "FUNC",
   "LP_START", "LP_COND", "LP_STEP", "LP_BACKEDGE", "LP_BODY", "COMPGOTO", "OTHER"};

static const char * sc_type_name_abbr[] =
  {"", "^", "", "", "o", "-", "",
   "", "", "", "", "", "", ""};

// bit mask.
enum SC_NODE_FLAG
{
  HAS_SYMM_LOOP = 0x1
};

// bit mask for transformations at the extended transformation (EXT) phase.
enum EXT_TRANS_KIND {
    EXT_TRANS_NONE = 0,    // Do not do EXT.
    EXT_TRANS_FUSION = 1,  // Do loop fusions at the EXT phase.
    EXT_TRANS_TRAVERSE = 2     // Do traversal transformations at the EXT phase.
};

// Structure component nodes.
class SC_NODE {
private:
  SC_TYPE type;
  IDTYPE _id;
  MEM_POOL * pool;
  union {
    BB_NODE * bb_rep;  // Pointer to this SC_NODE's representing BB_NODE. Valid for SC_IF.
    BB_LIST * bbs;     // A list of BB_NODEs. Valid for SC_BLOCK only.
  } u1;
  SC_NODE * parent;
  SC_LIST * kids;
  IDTYPE _class_id;
  int _depth;
  int _flag;
  SC_NODE * next;

private:
  BOOL Is_member(BB_NODE *);

public:
  IDTYPE       Id(void)          const  { return _id; }
  void         Set_id(IDTYPE i )        { _id = i; }
  IDTYPE       Class_id(void)    const  { return _class_id; }
  void         Set_class_id(IDTYPE i)   { _class_id = i; }
  int          Depth(void)       const  { return _depth; }
  void         Set_depth(int i)         { _depth = i; }
  int          Flag()            const  { return _flag; }
  void         Set_flag(int i)          { _flag = i; }
  SC_NODE *    Next()            const { return next; }
  SC_NODE *    Last();
  void         Set_next(SC_NODE * node) { next = node; }
  void         Remove_flag(int i);
  BOOL         Has_flag(int i)          { return ((_flag & i) != 0); }
  void         Add_flag(int i)          { if (!Has_flag(i)) {_flag += i; } }
  SC_TYPE      Type(void)        const { return type; }
  void         Set_type(SC_TYPE i)     { type = i; }
  const char * Type_name(void) const   { return sc_type_name[type]; }
  const char * Type_name_abbr(void)  const   { return sc_type_name_abbr[type]; }
  BB_NODE *    Get_bb_rep()    const   { return (SC_type_has_rep(type) ? u1.bb_rep : NULL); }
  void         Set_bb_rep(BB_NODE * i) 
  { 
    FmtAssert(SC_type_has_rep(type), ("Unexpected SC_NODE"));
    u1.bb_rep = i; 
  }

  BB_LIST *       Get_bbs()   const { return (SC_type_has_bbs(type) ? u1.bbs : NULL); }
  void            Append_bbs(BB_NODE *i)
  {
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    if (u1.bbs == NULL)
      u1.bbs = CXX_NEW(BB_LIST(i), pool);
    else
      u1.bbs = u1.bbs->Append(i, pool);
  }

  void            Prepend_bbs(BB_NODE *i)
  {
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    if (u1.bbs == NULL)
      u1.bbs = CXX_NEW(BB_LIST(i), pool);
    else
      u1.bbs = u1.bbs->Prepend(i, pool);
  }

  void           Set_bbs(BB_LIST * i)
  { 
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    u1.bbs = i; 
  }

  MEM_POOL * Get_pool()                { return pool; }
  void       Set_pool(MEM_POOL * i)    { pool = i; }

  SC_NODE *  Parent(void)      const   { return parent; }
  void       Set_parent(SC_NODE * i)   { parent = i; }
  void       Set_kids(SC_LIST * i)     { kids = i; }
  SC_LIST *  Kids(void)        const   { return kids; }
  void       Clear(void);
  BOOL       Is_empty_block();
  BOOL       Is_empty();
  BOOL       Is_same(SC_NODE *);
  SC_NODE(void)          { Clear(); }
  SC_NODE(const SC_NODE&);
  ~SC_NODE(void)         {}

  void Print(FILE *fp=stderr, BOOL = TRUE) const;

  void Append_kid(SC_NODE * sc);
  void Prepend_kid(SC_NODE * sc);
  void Remove_kid(SC_NODE * sc);
  void Insert_before(SC_NODE * sc);
  void Insert_after(SC_NODE * sc);
  SC_NODE * Last_kid();
  SC_NODE * Last_non_empty_kid();
  SC_NODE * First_non_empty_kid();
  SC_NODE * Last_kid_of_type(SC_TYPE);
  SC_NODE * First_kid();
  SC_NODE * First_executable_kid();
  SC_NODE * Next_sibling();
  SC_NODE * Prev_sibling();
  SC_NODE * Prev_sibling_of_type(SC_TYPE);
  SC_NODE * Next_sibling_of_type(SC_TYPE);
  SC_NODE * Next_in_tree();
  SC_NODE * Next_executable_sibling();
  SC_NODE * Get_nesting_if(SC_NODE *);
  SC_NODE * Get_nesting_if(INT);
  std::pair<SC_NODE *, bool> Get_nesting_if();
  std::pair<SC_NODE *, int> Get_outermost_nesting_if();
  std::pair<SC_NODE *, int> Get_outermost_nesting_loop();
  SC_NODE * First_kid_of_type(SC_TYPE);
  BOOL Contains(BB_NODE *);
  BB_NODE * Then();
  BB_NODE * Else();
  BB_NODE * Merge();
  void      Set_merge(BB_NODE *);
  BB_NODE * Head();
  BB_NODE * Then_end();
  BB_NODE * Else_end();
  BB_NODE * Exit();
  BB_LOOP * Loopinfo();
  WN *      Index();
  SC_NODE * Find_kid_of_type(SC_TYPE);
  SC_NODE * Find_parent_of_type(SC_TYPE);
  SC_NODE * Find_offspring_of_type(SC_TYPE);
  void Unlink();
  void Convert(SC_TYPE);
  void Delete();
  BOOL Is_well_behaved();
  BOOL Is_sese();
  BOOL Has_same_loop_struct(SC_NODE *);
  BOOL Has_symmetric_path(SC_NODE *, BOOL);
  UINT32 Encode_path(SC_NODE *);
  SC_NODE * Find_lcp(SC_NODE *);
  BB_NODE * First_bb();
  BB_NODE * Last_bb();
  // Find first executable statement in this SC_NODE.
  WN * First_executable_stmt();
  // Find first executable statement's containing block in this SC_NODE.
  BB_NODE * First_executable_blk();
  BOOL Is_pred_in_tree(SC_NODE *);
  int Num_of_loops(SC_NODE *, BOOL, BOOL);
  int Executable_stmt_count();
  BOOL Has_loop();
  SC_NODE * Get_real_parent();
  SC_NODE * Get_real_parent(int);
  SC_NODE * Get_node_at_dist(SC_NODE *, int dist);
  WN * Get_cond();
  BOOL Is_ctrl_equiv(SC_NODE *);
  BOOL Get_bounds(WN **, WN **, WN **);
  BOOL Compare_Trees(SC_NODE *);
  BOOL All_kids_clonable(SC_NODE *);
  BOOL Clonable(BOOL);
};

class SC_LIST : public SLIST_NODE {
private:
  SC_NODE * node;
  SC_LIST(const SC_LIST &);
  SC_LIST &operator = (const SC_LIST&);

public:
  SC_LIST(void)         { Clear(); }
  SC_LIST(SC_NODE * nd) { Clear(); node = nd;}
  ~SC_LIST(void)        {};
  
  DECLARE_SLIST_NODE_CLASS( SC_LIST )
  SC_LIST *Append (SC_NODE *bb, MEM_POOL *pool);  

  SC_LIST *Prepend(SC_NODE *bb, MEM_POOL *pool)
  {
    SC_LIST * new_sclst = (SC_LIST*) CXX_NEW(SC_LIST(bb), pool);
    new_sclst->Set_Next(this);
    return new_sclst;
  }

  SC_LIST *Remove(SC_NODE *sc, MEM_POOL *pool);
  BOOL Contains(SC_NODE *sc) const;
  void Print (FILE *fp = stderr) const;
  SC_NODE * Last_elem();
  SC_NODE * First_elem();
  
  void Init(SC_NODE *nd)      { node = nd; }
  void Clear(void)            { node = NULL; }

  SC_NODE *Node(void)  const { return node;}
  void Set_node(SC_NODE *sc)  { node = sc; }
};

class SC_LIST_CONTAINER : public SLIST {
private:
  DECLARE_SLIST_CLASS( SC_LIST_CONTAINER, SC_LIST )  

  SC_LIST_CONTAINER(const SC_LIST_CONTAINER&);
  SC_LIST_CONTAINER& operator = (const SC_LIST_CONTAINER&);

public:
  ~SC_LIST_CONTAINER(void) {};

  void Append (SC_NODE *sc, MEM_POOL *pool);
  void Prepend (SC_NODE *sc, MEM_POOL *pool);
  void Remove(SC_NODE *sc, MEM_POOL *pool);
  SC_NODE *Remove_head(MEM_POOL *pool);
  BOOL Contains(SC_NODE *sc) const;
};

class SC_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS( SC_LIST_ITER, SC_LIST, SC_LIST_CONTAINER )
public:
  void     Init(void)       {}
  void Validate_unique(FILE *fp=stderr);
  SC_NODE *First_sc(void)   { return (First()) ? Cur()->Node():NULL; }
  SC_NODE *Next_sc(void)    { return (Next())  ? Cur()->Node():NULL; }
  SC_NODE *Cur_sc(void)     { return (Cur())   ? Cur()->Node():NULL; }
  SC_NODE *First_elem(void) { return (First()) ? Cur()->Node():NULL; }
  SC_NODE *Next_elem(void)  { return (Next())  ? Cur()->Node():NULL; }
};

#endif /*opt_sc_INCLUDED*/
/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
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

// ===============================================================================================
//
// Description:
//
//                   Structured  Component Nodes
//                   --------------------------
//      This is the structured component node data structure that represents if-region, loop-region
//      and other structured control flow components in the source program. 
//
// Reserved prefix:
// ----------------
//
//     SC         for structured component node
//
// Exported types:
// ---------------
//     SC_NODE
//
//          A structured component node.  Every structured component node contains the
//          following fields.
//
//              IDTYPE     _id
//                    A unique identifier for a structured component node.
//
//              SC_TYPE    type
//                    The type of a SC_NODE.  See SC_TYPE.
//
//              MEM_POOL * pool
//                    Allocation memory pool.
//
//
//              BB_NODE * bb_rep
//                    A pointer to representing BB_NODE.  
//
//              BB_LIST * bbs
//                    A list of BB_NODEs contained in this SC_NODE.
//
//              SC_NODE * parent
//                    A pointer to the parent node.
//
//              SC_LIST * kids
//                    A pointer to the list of child nodes.  The list is ordered
//                    according to order of occurrence in the procedure.      
//
//              IDTYPE _class_id
//                    ID of the loop class this SC_NODE belongs.  Scratch field. 
//                    Loops are classified according to their path symmetricities in the SC tree.
//
//              int _depth
//                    Depth of this SC_NODE in the SC tree. Scratch field.
//
//              int flag
//                    Flag of this SC_NODE.  see SC_NODE_FLAG.
//
//              SC_NODE * next
//                    A pointer to the next node in a lost.  Scratch field.
//
//
//     SC_LIST :SLIST_NODE
//
//          A singly linked list of SC_NODEs.  We use this class to build the children node list.
//
//     SC_LIST_CONTAINER : SLIST
//     SC_LIST_ITER : SLIST_ITER
//
//          The standard container and iterator for singly linked list.
//     
//
// ==============================================================================================

#ifndef opt_sc_INCLUDED
#define opt_sc_INCLUDED "opt_sc.h"
#include <iterator>

#ifndef opt_bb_INCLUDED
#include "opt_bb.h"
#endif
#ifndef opt_base_INCLUDED
#include "opt_base.h"
#endif
#ifndef opt_main_INCLUDED
#include "opt_main.h"
#endif
#ifndef opt_alias_class_INCLUDED
#include "opt_alias_class.h"
#endif
#ifndef wn_simp_INCLUDED
#include "wn_simp.h"
#endif

extern "C" {
#include "bitset.h"
}

// Type of SC nodes
enum SC_TYPE {
  SC_NONE = 0,
  SC_IF,     // if-region
  SC_THEN,   // then-path of a if-region
  SC_ELSE,   // else-path of a if-region
  SC_LOOP,   // loop region
  SC_BLOCK,  // blocks of straight-line codes
  SC_FUNC,   // func entry
  SC_LP_START, // H-WHIRL loop start
  SC_LP_COND,  // H-WHIRL loop cond
  SC_LP_STEP,  // H-WHIRL loop step
  SC_LP_BACKEDGE,  // H-WHIRL loop backedge
  SC_LP_BODY,  // loop body
  SC_COMPGOTO, // COMPGOTO
  SC_OTHER   // other structured control flows
};

extern BOOL SC_type_has_rep(SC_TYPE type);
extern BOOL SC_type_has_bbs(SC_TYPE type);
extern BOOL SC_type_is_marker(SC_TYPE type);

static const char * sc_type_name[] =
  {"NONE", "IF", "THEN", "ELSE", "LOOP", "BLOCK", "FUNC",
   "LP_START", "LP_COND", "LP_STEP", "LP_BACKEDGE", "LP_BODY", "COMPGOTO", "OTHER"};

static const char * sc_type_name_abbr[] =
  {"", "^", "", "", "o", "-", "",
   "", "", "", "", "", "", ""};

// bit mask.
enum SC_NODE_FLAG
{
  HAS_SYMM_LOOP = 0x1
};

// bit mask for transformations at the extended transformation (EXT) phase.
enum EXT_TRANS_KIND {
    EXT_TRANS_NONE = 0,    // Do not do EXT.
    EXT_TRANS_FUSION = 1,  // Do loop fusions at the EXT phase.
    EXT_TRANS_TRAVERSE = 2     // Do traversal transformations at the EXT phase.
};

// Structure component nodes.
class SC_NODE {
private:
  SC_TYPE type;
  IDTYPE _id;
  MEM_POOL * pool;
  union {
    BB_NODE * bb_rep;  // Pointer to this SC_NODE's representing BB_NODE. Valid for SC_IF.
    BB_LIST * bbs;     // A list of BB_NODEs. Valid for SC_BLOCK only.
  } u1;
  SC_NODE * parent;
  SC_LIST * kids;
  IDTYPE _class_id;
  int _depth;
  int _flag;
  SC_NODE * next;

private:
  BOOL Is_member(BB_NODE *);

public:
  IDTYPE       Id(void)          const  { return _id; }
  void         Set_id(IDTYPE i )        { _id = i; }
  IDTYPE       Class_id(void)    const  { return _class_id; }
  void         Set_class_id(IDTYPE i)   { _class_id = i; }
  int          Depth(void)       const  { return _depth; }
  void         Set_depth(int i)         { _depth = i; }
  int          Flag()            const  { return _flag; }
  void         Set_flag(int i)          { _flag = i; }
  SC_NODE *    Next()            const { return next; }
  SC_NODE *    Last();
  void         Set_next(SC_NODE * node) { next = node; }
  void         Remove_flag(int i);
  BOOL         Has_flag(int i)          { return ((_flag & i) != 0); }
  void         Add_flag(int i)          { if (!Has_flag(i)) {_flag += i; } }
  SC_TYPE      Type(void)        const { return type; }
  void         Set_type(SC_TYPE i)     { type = i; }
  const char * Type_name(void) const   { return sc_type_name[type]; }
  const char * Type_name_abbr(void)  const   { return sc_type_name_abbr[type]; }
  BB_NODE *    Get_bb_rep()    const   { return (SC_type_has_rep(type) ? u1.bb_rep : NULL); }
  void         Set_bb_rep(BB_NODE * i) 
  { 
    FmtAssert(SC_type_has_rep(type), ("Unexpected SC_NODE"));
    u1.bb_rep = i; 
  }

  BB_LIST *       Get_bbs()   const { return (SC_type_has_bbs(type) ? u1.bbs : NULL); }
  void            Append_bbs(BB_NODE *i)
  {
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    if (u1.bbs == NULL)
      u1.bbs = CXX_NEW(BB_LIST(i), pool);
    else
      u1.bbs = u1.bbs->Append(i, pool);
  }

  void            Prepend_bbs(BB_NODE *i)
  {
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    if (u1.bbs == NULL)
      u1.bbs = CXX_NEW(BB_LIST(i), pool);
    else
      u1.bbs = u1.bbs->Prepend(i, pool);
  }

  void           Set_bbs(BB_LIST * i)
  { 
    FmtAssert(SC_type_has_bbs(type), ("Unexpected SC_NODE"));
    u1.bbs = i; 
  }

  MEM_POOL * Get_pool()                { return pool; }
  void       Set_pool(MEM_POOL * i)    { pool = i; }

  SC_NODE *  Parent(void)      const   { return parent; }
  void       Set_parent(SC_NODE * i)   { parent = i; }
  void       Set_kids(SC_LIST * i)     { kids = i; }
  SC_LIST *  Kids(void)        const   { return kids; }
  void       Clear(void);
  BOOL       Is_empty_block();
  BOOL       Is_empty();
  BOOL       Is_same(SC_NODE *);
  SC_NODE(void)          { Clear(); }
  SC_NODE(const SC_NODE&);
  ~SC_NODE(void)         {}

  void Print(FILE *fp=stderr, BOOL = TRUE) const;

  void Append_kid(SC_NODE * sc);
  void Prepend_kid(SC_NODE * sc);
  void Remove_kid(SC_NODE * sc);
  void Insert_before(SC_NODE * sc);
  void Insert_after(SC_NODE * sc);
  SC_NODE * Last_kid();
  SC_NODE * Last_non_empty_kid();
  SC_NODE * First_non_empty_kid();
  SC_NODE * Last_kid_of_type(SC_TYPE);
  SC_NODE * First_kid();
  SC_NODE * First_executable_kid();
  SC_NODE * Next_sibling();
  SC_NODE * Prev_sibling();
  SC_NODE * Prev_sibling_of_type(SC_TYPE);
  SC_NODE * Next_sibling_of_type(SC_TYPE);
  SC_NODE * Next_in_tree();
  SC_NODE * Next_executable_sibling();
  SC_NODE * Get_nesting_if(SC_NODE *);
  SC_NODE * Get_nesting_if(INT);
  std::pair<SC_NODE *, bool> Get_nesting_if();
  std::pair<SC_NODE *, int> Get_outermost_nesting_if();
  std::pair<SC_NODE *, int> Get_outermost_nesting_loop();
  SC_NODE * First_kid_of_type(SC_TYPE);
  BOOL Contains(BB_NODE *);
  BB_NODE * Then();
  BB_NODE * Else();
  BB_NODE * Merge();
  void      Set_merge(BB_NODE *);
  BB_NODE * Head();
  BB_NODE * Then_end();
  BB_NODE * Else_end();
  BB_NODE * Exit();
  BB_LOOP * Loopinfo();
  WN *      Index();
  SC_NODE * Find_kid_of_type(SC_TYPE);
  SC_NODE * Find_parent_of_type(SC_TYPE);
  SC_NODE * Find_offspring_of_type(SC_TYPE);
  void Unlink();
  void Convert(SC_TYPE);
  void Delete();
  BOOL Is_well_behaved();
  BOOL Is_sese();
  BOOL Has_same_loop_struct(SC_NODE *);
  BOOL Has_symmetric_path(SC_NODE *, BOOL);
  UINT32 Encode_path(SC_NODE *);
  SC_NODE * Find_lcp(SC_NODE *);
  BB_NODE * First_bb();
  BB_NODE * Last_bb();
  // Find first executable statement in this SC_NODE.
  WN * First_executable_stmt();
  // Find first executable statement's containing block in this SC_NODE.
  BB_NODE * First_executable_blk();
  BOOL Is_pred_in_tree(SC_NODE *);
  int Num_of_loops(SC_NODE *, BOOL, BOOL);
  int Executable_stmt_count();
  BOOL Has_loop();
  SC_NODE * Get_real_parent();
  SC_NODE * Get_real_parent(int);
  SC_NODE * Get_node_at_dist(SC_NODE *, int dist);
  WN * Get_cond();
  BOOL Is_ctrl_equiv(SC_NODE *);
  BOOL Get_bounds(WN **, WN **, WN **);
  BOOL Compare_Trees(SC_NODE *);
  BOOL All_kids_clonable(SC_NODE *);
  BOOL Clonable(BOOL);
};

class SC_LIST : public SLIST_NODE {
private:
  SC_NODE * node;
  SC_LIST(const SC_LIST &);
  SC_LIST &operator = (const SC_LIST&);

public:
  SC_LIST(void)         { Clear(); }
  SC_LIST(SC_NODE * nd) { Clear(); node = nd;}
  ~SC_LIST(void)        {};
  
  DECLARE_SLIST_NODE_CLASS( SC_LIST )
  SC_LIST *Append (SC_NODE *bb, MEM_POOL *pool);  

  SC_LIST *Prepend(SC_NODE *bb, MEM_POOL *pool)
  {
    SC_LIST * new_sclst = (SC_LIST*) CXX_NEW(SC_LIST(bb), pool);
    new_sclst->Set_Next(this);
    return new_sclst;
  }

  SC_LIST *Remove(SC_NODE *sc, MEM_POOL *pool);
  BOOL Contains(SC_NODE *sc) const;
  void Print (FILE *fp = stderr) const;
  SC_NODE * Last_elem();
  SC_NODE * First_elem();
  
  void Init(SC_NODE *nd)      { node = nd; }
  void Clear(void)            { node = NULL; }

  SC_NODE *Node(void)  const { return node;}
  void Set_node(SC_NODE *sc)  { node = sc; }
};

class SC_LIST_CONTAINER : public SLIST {
private:
  DECLARE_SLIST_CLASS( SC_LIST_CONTAINER, SC_LIST )  

  SC_LIST_CONTAINER(const SC_LIST_CONTAINER&);
  SC_LIST_CONTAINER& operator = (const SC_LIST_CONTAINER&);

public:
  ~SC_LIST_CONTAINER(void) {};

  void Append (SC_NODE *sc, MEM_POOL *pool);
  void Prepend (SC_NODE *sc, MEM_POOL *pool);
  void Remove(SC_NODE *sc, MEM_POOL *pool);
  SC_NODE *Remove_head(MEM_POOL *pool);
  BOOL Contains(SC_NODE *sc) const;
};

class SC_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS( SC_LIST_ITER, SC_LIST, SC_LIST_CONTAINER )
public:
  void     Init(void)       {}
  void Validate_unique(FILE *fp=stderr);
  SC_NODE *First_sc(void)   { return (First()) ? Cur()->Node():NULL; }
  SC_NODE *Next_sc(void)    { return (Next())  ? Cur()->Node():NULL; }
  SC_NODE *Cur_sc(void)     { return (Cur())   ? Cur()->Node():NULL; }
  SC_NODE *First_elem(void) { return (First()) ? Cur()->Node():NULL; }
  SC_NODE *Next_elem(void)  { return (Next())  ? Cur()->Node():NULL; }
};

#endif /*opt_sc_INCLUDED*/
