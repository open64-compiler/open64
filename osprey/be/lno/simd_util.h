/* 
  Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.

  Open64 is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.
*/

#ifndef simd_util_INCLUDED
#define simd_util_INCLUDED

#include <list>

// Forward declaration
//
class SIMD_EXPR;
class SIMD_EXPR_MGR;
class SIMD_VECTOR_CONF_BASE;
class SIMD_VECTOR_CONF;

/////////////////////////////////////////////////////////////////////////////////
//
//   Arch specific stuff are encapsulated by SIMD_VECTOR_CONF_BASE and 
//  SIMD_VECTOR_CONF.
//
//   TODO: it would be better to place these stuff in a separate header file
//
/////////////////////////////////////////////////////////////////////////////////
//
class SIMD_VECTOR_CONF_BASE {
public:
    // Does H.W support vectorization
    BOOL Arch_Has_Vect (void) const { return FALSE; } 

    // About SSE 
    //
    BOOL Is_SSE_Family (void)   const { return FALSE; }
    BOOL Is_MMX (void)   const { return FALSE; }
    BOOL Is_SSE (void)   const { return FALSE; }
    BOOL Is_SSE2 (void)  const { return FALSE; }
    BOOL Is_SSE3 (void)  const { return FALSE; }
    BOOL Is_SSE4a (void) const { return FALSE; }
    BOOL Is_SSSE3 (void) const { return FALSE; }
    BOOL Is_SSE41 (void) const { return FALSE; }
    BOOL Is_SSE42 (void) const { return FALSE; }

    INT Get_Vect_Byte_Size (void) const { return -1; }
    INT Get_Vect_Len_Given_Elem_Ty (TYPE_ID) const { -1; }
};

#ifdef TARG_X8664

class SIMD_VECTOR_CONF : public SIMD_VECTOR_CONF_BASE {
public:
    BOOL Arch_Has_Vect (void) const { return TRUE; }

    BOOL Is_MMX (void)   const { return Is_Target_MMX (); }
    BOOL Is_SSE (void)   const { return Is_Target_SSE (); }
    BOOL Is_SSE2 (void)  const { return Is_Target_SSE2 (); }
    BOOL Is_SSE3 (void)  const { return Is_Target_SSE3 (); }
    BOOL Is_SSE4a (void) const { return Is_Target_SSE4a (); }
    BOOL Is_SSSE3 (void) const { return Is_Target_SSSE3 (); }
    BOOL Is_SSE41 (void) const { return Is_Target_SSE41 (); }
    BOOL Is_SSE42 (void) const { return Is_Target_SSE42 (); }
    BOOL Is_SSE_Family (void) const {
        return Is_SSE () || Is_SSE2 () || Is_SSE3 () || 
               Is_SSE4a () || Is_SSSE3 () || Is_SSE41 () ||
               Is_SSE42 ();
    }

    INT Get_Vect_Byte_Size (void) const { return 16; }
    INT Get_Vect_Len_Given_Elem_Ty (TYPE_ID t) const 
        { return 16/MTYPE_byte_size(t);}
};

#else 

class SIMD_VECTOR_CONF : public SIMD_VECTOR_CONF_BASE;

#endif

extern SIMD_VECTOR_CONF Simd_vect_conf;

/////////////////////////////////////////////////////////////////////////////////
//
//   First of all, SIMD_EXPR is a container hosting vectorization related 
// informations. Among all these information, some can be derived directly from 
// the given WN expression itself; some need context. For instance, in 
// the following snippet, the vectorizable expression "(x * (INT32)sa2[i])" doesn't
// need to have 32 significant bits. However, the expression per se cannot reveal 
// this info, but the "contex" will help.
//
//    INT16 sa1[], sa2[]; INT32 x;
//    for (i = 0; i < N; i++) { sa1[i] = (INT16)(x * (INT32)sa2[i])
//
//   Since a SIMD_EXPR is not aware of the "context" it is in, it has to "derive" 
// information blindly, and imprecisely. The objects who have better knowledge 
// of the context should correct them properly.
//
//   Second, SIMD_EXPR is responsible for physically converting its corresponding
// scalar expression into vectorized form.
//
//////////////////////////////////////////////////////////////////////////////////
// 
class SIMD_EXPR {
public:
    friend class SIMD_EXPR_MGR;

    INT32 Get_Misalignment (void) { Is_True (FALSE, ("TBD")); return -1; } 

    INT32 Get_Vect_Len (void) const { return _vect_len; }
    INT32 Get_Vect_Elem_Byte_Sz (void) const { return _elem_sz; }

    BOOL Is_Invar (void) const { return _is_invar; }
    WN* Get_Wn (void) const { return _expr; } 

private:
    SIMD_EXPR (WN* expr);

    void Set_Elem_Sz (INT sz);

    WN* _expr;

    INT16 _vect_len;
    INT16 _elem_sz;
    INT16 _mis_align;

    BOOL _is_invar;
};

typedef mempool_allocator<SIMD_EXPR*> SIMD_EXPR_ALLOC;
typedef std::list<SIMD_EXPR*, SIMD_EXPR_ALLOC> SIMD_EXPR_LIST;


//////////////////////////////////////////////////////////////////////////////
//
//   SIMD_EXPR_MGR is to manage all SIMD_EXPRs of the loop being vectorized.
// Its duty includes:
//
//   - identify vectorizable expressions.
//   - allocate/free a SIMD_EXPR.
//   - collect statistical information of the SIMD_EXPRs under management
//
/////////////////////////////////////////////////////////////////////////////
//
class SIMD_EXPR_MGR {
public:
    SIMD_EXPR_MGR (WN* loop, MEM_POOL*);
    const SIMD_EXPR_LIST& Get_Expr_List (void) const { return _exprs; }

    // This func is provided for the time being.
    //
    void Convert_From_Lagacy_Expr_List (SCALAR_REF_STACK*); 

    inline UINT Get_Max_Vect_Len (void) const;
    inline UINT Get_Min_Vect_Len (void) const; 

private:
    MEM_POOL* _mp;
    WN* _loop;
    SIMD_EXPR_LIST _exprs;
    
    UINT16 _min_vect_len;
    UINT16 _max_vect_len;
};


//////////////////////////////////////////////////////////////////////////////
//     
//          Inline functions are defined here 
//
//////////////////////////////////////////////////////////////////////////////
//
inline UINT
SIMD_EXPR_MGR::Get_Max_Vect_Len (void) const {
    Is_True (_max_vect_len != 0, ("_max_vect_len isn't set properly"));
    return _max_vect_len; 
}

inline UINT
SIMD_EXPR_MGR::Get_Min_Vect_Len (void) const {
    Is_True (_min_vect_len != 0, ("_min_vect_len isn't set properly"));
    return _min_vect_len; 
}

#endif /* simd_util_INCLUDED */
