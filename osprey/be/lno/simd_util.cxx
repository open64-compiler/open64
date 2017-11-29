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

#include "defs.h"
#include "glob.h"
#include "wn.h"
#include "cxx_memory.h"
#include "lwn_util.h"
#include "ff_utils.h"
#include "simd_util.h"

/////////////////////////////////////////////////////////////////////////////
//
//          Implementation of SIMD_EXPR
//
/////////////////////////////////////////////////////////////////////////////
//
SIMD_EXPR::SIMD_EXPR (WN* expr) {
    _expr= expr;

    _elem_sz = MTYPE_byte_size (WN_rtype (expr));
    _vect_len = Simd_vect_conf.Get_Vect_Len_Given_Elem_Ty (WN_rtype(expr));

    _mis_align = -1;
    _is_invar = FALSE;
}

/////////////////////////////////////////////////////////////////////////////
//
//          Implementation of SIMD_EXPR_MGR 
//
/////////////////////////////////////////////////////////////////////////////
//
SIMD_EXPR_MGR::SIMD_EXPR_MGR (WN* loop, MEM_POOL* mp):
    _loop(loop), _mp(mp), _exprs(mp) {

    _min_vect_len = _max_vect_len = 0;
}

void
SIMD_EXPR_MGR::Convert_From_Lagacy_Expr_List (SCALAR_REF_STACK* simd_ops) {

    Is_True (_exprs.empty (), ("expr is not empty"));
    
    _min_vect_len = Simd_vect_conf.Get_Vect_Byte_Size ();
    _max_vect_len = 0;

    for (INT i=0, elem_cnt = simd_ops->Elements(); i<elem_cnt; i++) {
        WN* wn_expr = simd_ops->Top_nth(i).Wn;
        SIMD_EXPR* expr = CXX_NEW (SIMD_EXPR (wn_expr), _mp);
        
        _exprs.push_back (expr);
        INT vec_len = expr->Get_Vect_Len ();
        _min_vect_len = MIN(vec_len, _min_vect_len);
        _max_vect_len = MAX(vec_len, _max_vect_len); 
    }
}
