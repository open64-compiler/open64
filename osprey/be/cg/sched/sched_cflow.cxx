//-*-c++-*-
/*
 *  Copyright (C) 2000-2002, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *  
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer. 
 *  
 *  Redistributions in binary form must reproduce the above copyright notice, this list
 *  of conditions and the following disclaimer in the documentation and/or other materials
 *  provided with the distribution. 
 *
 *  Neither the name of the owner nor the names of its contributors may be used to endorse or
 *  promote products derived from this software without specific prior written permission. 
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 *  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


#include "defs.h"
#include "ipfec_defs.h"
#include "ipfec_options.h"

#include "tracing.h"
#include "errors.h"


#include "op.h"
#include "bb.h"
#include "be_util.h"

#include "mtypes.h"
#include "targ_sim.h"

#include "region.h"

#include "sched_util.h"
#include "sched_path.h"
#include "sched_cflow.h"
#include "scheduler.h" /* for prototype of SCHEDULER::Clean_Up */

    /* =====================================================
     * =====================================================
     *
     *     EXEC_PATH implementation starts from here
     *
     * =====================================================
     * =====================================================
     */

    /* constructor & destructor 
     */
EXEC_PATH :: EXEC_PATH (EXEC_PATH_ID id,MEM_POOL *mp) : 
    _mp(mp), _path_node_seq(mp) {

    _id = id;
    _has_call = _has_nested_rgn = FALSE ;
    _bb_num   = _nested_rgn_num = 0;
}

EXEC_PATH :: EXEC_PATH (const EXEC_PATH& ep, MEM_POOL *mp) :
    _mp(mp), 
    _path_node_seq (mp) {
    
    _path_node_seq = ep._path_node_seq ;
    Setup_Hash () ;

    _has_call       = ep.Path_Has_Call ();
    _has_nested_rgn = ep.Path_Has_Nested_Rgn ();

    _bb_num         = BB_Node_In_Total  ();
    _nested_rgn_num = Rgn_Node_In_Total ();
}


EXEC_PATH&
EXEC_PATH :: operator = (const EXEC_PATH& ep) {

    FmtAssert (FALSE,("Has yet implemented!"));
    return *this ; /* to make compiler happy */
}

    /* ====================================================
     *
     *  Append_Path_Segment
     *
     *  add a segment of to this path. a segment is of pair
     *  of node as well as its incoming edge. the first 
     *  segment can be viewed as the pair of first node 
     *  with NULL incoming edge.
     *
     * ====================================================
     */
void
EXEC_PATH :: Append_Path_Segment 
    (REGIONAL_CFG_EDGE * in_edge, REGIONAL_CFG_NODE *node) {

    REGIONAL_CFG_NODE* n = in_edge ? in_edge->Dest () : node ;
    float reach_prob ;
    
    if (_path_node_seq.size ()) {
    
        Is_True (node == n && node, ("incoming_edge->Dest () != n"));

        reach_prob = 
            _path_node_seq.back().Prob_From_Root () * in_edge->Prob();

    } else {

        Is_True (!in_edge, ("in_edge != NULL"));
        reach_prob = 1.0f;
    }

    if (n->Is_Region ()) {
        Set_Has_Nested_Rgn (); 
        Inc_Path_Nested_Rgn_Num ();
    } else {
        Inc_Path_BB_Num ();
        if (BB_call(n->BB_Node())) { 
            Set_Has_Call ();
        }
    }
    
    _path_node_seq.push_back (PATH_NODE_INFO(n,in_edge,reach_prob));
    Add_Hash (n, _path_node_seq.size() - 1);
}


    /* =====================================================
     * =====================================================
     *
     *   class EXEC_PATH_SET starts from here
     *
     * =====================================================
     * =====================================================
     */
EXEC_PATH_SET :: EXEC_PATH_SET (MEM_POOL *mp, mINT32 size) {
    _mp       = mp ;
    
    Is_True (size >= 0, ("size should greater than zero!"));
    _size = size;

    _bv_words_num = 
        (UINT32(_size + BV_WORD_SIZE - 1)) >> BV_WORD_SIZE_LOG_2;

        /* setup BV_WORD vector */
    if (_bv_words_num <= sizeof(_bv_words)/sizeof(_bv_words[0])) {
        _bv = &_bv_words[0];
    } else {
        _bv = TYPE_MEM_POOL_ALLOC_N (BV_WORD,_mp,_bv_words_num);
    }

    for (INT i = _bv_words_num - 1; i >= 0; i--) { _bv[i] = 0; }
    
        /* init most-significant-word's bit-mask 
         */
    _msw_mask = Pattern_Word (
                    (_size % BV_WORD_SIZE) ? 
                     _size % BV_WORD_SIZE : BV_WORD_SIZE, 
                      0/* 0 0-bits */);
}

    /* change set's capacity 
     */
void
EXEC_PATH_SET :: Resize (INT32 new_size) {
    
    Is_True (new_size >= 0, ("negative size(%d)", new_size));
    
    mINT32 wc = (new_size + BV_WORD_SIZE - 1) / BV_WORD_SIZE; 
    _size = new_size;

    if (wc > _bv_words_num) {

        BV_WORD * p = TYPE_MEM_POOL_ALLOC_N (BV_WORD,_mp, wc);
        for (mINT32 i = wc - 1; i >= 0; i--) {
            p[i] = 0 ;
        }

        for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
            p[i] = _bv[i];
        }

        _bv = p;

    } 

    _bv_words_num = wc ;
    
    _msw_mask = Pattern_Word (
                    (_size % BV_WORD_SIZE) ? 
                     _size % BV_WORD_SIZE : BV_WORD_SIZE, 
                      0/* 0 0-bits */);

    _bv[_bv_words_num - 1] &= _msw_mask ;
}
     
     /*  operator =  */
EXEC_PATH_SET&
EXEC_PATH_SET :: operator = (const EXEC_PATH_SET& ps) {
    
    /*NOTE: _mp remain unchanged */

    _size = ps._size ;

    _bv_words_num = ps._bv_words_num ;
    _bv = (_bv_words_num <= sizeof(_bv_words)/sizeof(_bv_words[0])) ?
          &_bv_words[0] : 
          TYPE_MEM_POOL_ALLOC_N (BV_WORD, _mp, _bv_words_num);
    
    for (INT i = _bv_words_num - 1 ; i >= 0; i--) {
        _bv[i] = ps._bv[i];
    }

    return *this ;
}

    /* =====================================================
     *
     * Is_Member 
     * 
     * check to see whether <path> is a member of this set
     *
     * ====================================================
     */
BOOL
EXEC_PATH_SET :: Is_Member (EXEC_PATH_ID path) {
    
    if (Path_Id_Is_Valid (path, FALSE)) return FALSE ;

}

    /* ====================================================
     *
     * Add_Path_Id/Del_Path_Id
     *
     * Add/Remove one EXEC_PATH_ID to/from this set 
     *
     * ===================================================
     */
void
EXEC_PATH_SET :: Add_Path_Id (const EXEC_PATH_ID path_id) {

    if (!Path_Id_Is_Valid (path_id)) return ;

    INT32 word_idx, bit_pos;
    WordIdx_BitPos (path_id, word_idx, bit_pos); 

    BV_WORD_Set_Bit (_bv[word_idx], bit_pos);
}

void
EXEC_PATH_SET :: Del_Path_Id  (const EXEC_PATH_ID path_id) {

    if (!Path_Id_Is_Valid (path_id)) return ;

    INT32 word_idx, bit_pos;
    WordIdx_BitPos (path_id, word_idx, bit_pos); 

    BV_WORD_Clear_Bit (_bv[word_idx], bit_pos);
}

     /* bitwise and(&) operation */

EXEC_PATH_SET&
EXEC_PATH_SET :: operator &  (const EXEC_PATH_SET& eps) {

    Equal_Size (eps, TRUE/*abort exec if !equ*/);

    EXEC_PATH_SET *p = CXX_NEW(EXEC_PATH_SET(_mp,_size), _mp);

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        p->_bv[i] &= eps._bv[i];
    }

    return *p ; 
}

void
EXEC_PATH_SET :: operator &= (const EXEC_PATH_SET& eps) {

    Equal_Size (eps, TRUE/*abort exec if !equ*/);

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        _bv[i] &= eps._bv[i];
    }
}

EXEC_PATH_SET&
EXEC_PATH_SET :: operator ~ (void) {
    
    EXEC_PATH_SET * p = CXX_NEW(EXEC_PATH_SET(_mp,_size), _mp);

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        p->_bv[i] = ~_bv[i];
    }

    return * p;
}

    /* =================================================
     * Bitwise_Not :
     * 
     * Toggle each (bit)-element of this set. 
     *
     * ================================================
     */
void
EXEC_PATH_SET :: Bitwise_Not (void) {
    
    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        _bv[i] = ~_bv[i];    
    }
    
    _bv[_bv_words_num-1] &= _msw_mask ;
}

    /* ====================================================
     *
     *  shift-left operation
     *
     *  shift left operation. set is deemed as an integer.
     *
     * ===================================================
     */
EXEC_PATH_SET&
EXEC_PATH_SET :: operator << (const UINT16 shift_l_by) {
    
    UINT16 d = shift_l_by >> BV_WORD_SIZE_LOG_2;
    UINT16 r = shift_l_by % BV_WORD_SIZE;

    EXEC_PATH_SET * p = CXX_NEW(EXEC_PATH_SET(_mp,_size), _mp);
    *p = *this ;

    if (r > 0) {
        for (mINT32 i = _bv_words_num - 1; i >= 1 ; i--) {
            p->_bv[i] = 
                (_bv[i] << r) | (_bv[i-1] >> (BV_WORD_SIZE - r));
        }

        p->_bv[0] = _bv[0] << r;
        p->_bv[_bv_words_num - 1] = 
           _bv[_bv_words_num - 1] & _msw_mask ;
    }

    if (d == 0) { return * p; }
    
    if (d >= _bv_words_num) {
        for (mINT32 i = _bv_words_num - 1; i >= d; i--) {
            p->_bv[i] = 0;
        }
    } else {

        for (mINT32 i = _bv_words_num - 1; i >= d; i--) {
            p->_bv[i] = _bv[i-d];    
        }

        p->_bv[_bv_words_num - 1] &= _msw_mask ;
    }

    return *p;
}

    /* =======================================================
     *
     *  shift-left, assignment
     *
     * ======================================================
     */
void
EXEC_PATH_SET :: operator <<= (const UINT16 shift_l_by) {
    
    UINT16 d = shift_l_by >> BV_WORD_SIZE_LOG_2;
    UINT16 r = shift_l_by % BV_WORD_SIZE;

    if (r > 0) {
        for (mINT32 i = _bv_words_num - 1; i >= 1 ; i--) {
            _bv[i] = BV_WORD_SHIFT_RIGHT(_bv[i],r) | 
                     BV_WORD_SHIFT_LEFT (_bv[i-1],BV_WORD_SIZE - r);
        }

        _bv[0] = BV_WORD_SHIFT_LEFT (_bv[0],r);
        _bv[_bv_words_num - 1] &= _msw_mask ;
    }

    if (d == 0) { return ; }
    
    if (d >= _bv_words_num) {
        for (mINT32 i = _bv_words_num - 1; i >= d; i--) {
            _bv[i] = 0;
        }
    } else {
        for (mINT32 i = _bv_words_num - 1; i >= d; i--) {
            _bv[i] = _bv[i-d];    
        }
        _bv[_bv_words_num - 1] &= _msw_mask ;
    }

}

    /* =================================================
     *
     *  shift right operation 
     *
     * ================================================
     */
EXEC_PATH_SET&
EXEC_PATH_SET :: operator >>  (const UINT16 shift_r_by) {
    
    FmtAssert (FALSE,("Has not yet implemented!"));
    return *this; /* to make compiler happy */
}

void
EXEC_PATH_SET :: operator >>= (const UINT16 shift_r_by) {
    FmtAssert (FALSE,("Has not yet implemented!"));
}

    /* ==================================================
     * 
     *  Bitwise union operation 
     * 
     * ==================================================
     */
EXEC_PATH_SET&
EXEC_PATH_SET :: operator | (const EXEC_PATH_SET& eps) {
   
    Equal_Size (eps, TRUE/*abort exec if !equ-size */);
    
    EXEC_PATH_SET * p = CXX_NEW (EXEC_PATH_SET(_mp,_size), _mp);

    for (mINT32 i = _bv_words_num - 1; i >= 0 ; i--) {
       p->_bv[i] = _bv[i] | eps._bv[i];
    }
    
    return *p;
}

void
EXEC_PATH_SET :: operator |= (const EXEC_PATH_SET& eps) {
    
    Equal_Size (eps, TRUE/*abort exec if !equ-size */);

    for (mINT32 i = _bv_words_num - 1; i >= 0 ; i--) {
       _bv[i] = _bv[i] | eps._bv[i];
    }
}

    /* return the of 0 through <_size> - 1 inclusively 
     */
EXEC_PATH_SET&
EXEC_PATH_SET :: Universe (void) {
    
    EXEC_PATH_SET * p = CXX_NEW(EXEC_PATH_SET(_mp,_size), _mp);

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        p->_bv[i] = (BV_WORD)(-1);
    }
    
    p->_bv[0] &= _msw_mask ;
    
    return *p ;
}

void
EXEC_PATH_SET :: Set_To_Be_Universe (void) {

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        _bv[i] = (BV_WORD)(-1);     
    }
    _bv[_bv_words_num - 1] &= _msw_mask ;
}

    /* ===================================================
     *
     *  Subset
     * 
     *  return the intersection of <this> and [from - to]
     *
     * ===================================================
     */
EXEC_PATH_SET&
EXEC_PATH_SET :: Subset (EXEC_PATH_ID from, EXEC_PATH_ID to) {

    Is_True (from <= to, 
        ("Low-boudary(%d) > up-boundary(%d)", from, to));
    
    from = min(from,_size-1);
    to   = max(0, to);
    
        /* remove EXEC_PATH_ID less than <from> 
         */
    EXEC_PATH_SET * p = CXX_NEW(EXEC_PATH_SET(_mp,_size),_mp);

    for (mINT32 i = from/BV_WORD_SIZE - 1; i >= 0 ; i--) {
        p->_bv[i] = 0;
    }
    p->_bv[0] &= Pattern_Word (_size % BV_WORD_SIZE,0);
    
        /* remove EXEC_PATH_ID greater than <to>
         */
    for (mINT32 i = to/BV_WORD_SIZE; i <= _bv_words_num - 1; i++) {
        p->_bv[i] = 0 ;    
    }
    p->_bv[_bv_words_num-1] &= 
        Pattern_Word (_size % BV_WORD_SIZE, 0);

    return * p;
}

    /* ====================================================
     *
     * Is_Subset_Of 
     *
     * return TRUE iff <*this> is the subset of <eps>
     *
     * ====================================================
     */
BOOL
EXEC_PATH_SET :: Is_Subset_Of (EXEC_PATH_SET* eps) {

    Equal_Size (*eps, TRUE/*abort exec if !equ-size*/);

    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) { 
        if ((_bv[i] & eps->_bv[i]) != _bv[i]) {
            return FALSE;
        }
    }

    return TRUE;
}

    /* ====================================================
     *
     * Intersection_Is_Empty 
     *
     * return TRUE iff the intersection of <*this> and 
     * <eps> is not empty set.
     *
     * ====================================================
     */
BOOL
EXEC_PATH_SET :: Intersection_Is_Empty (EXEC_PATH_SET* eps) {

    Equal_Size (*eps, TRUE/*abort exec if !equ-size*/);
    
    for (mINT32 i = _bv_words_num - 1; i >= 0; i--) {
        if (_bv[i] & eps->_bv[i]) {
            return FALSE;    
        }
    }
    
    return TRUE;
}


    /* =========================================================
     *
     *  First_Path_Id 
     * 
     *  return the mininum EXEC_PATH_ID for a non-empty set and
     *  INVALID_EXEC_PATH_ID for an empty one.
     *
     * ========================================================
     */
EXEC_PATH_ID
EXEC_PATH_SET :: First_Path_Id (void) {
    
    mINT32 idx = -1;
    for (mINT32 i = 0 ; i < _bv_words_num ; i++) {
        if (_bv[i] != 0) { idx = i; break; }
    }

    if (idx == _bv_words_num) return INVALID_EXEC_PATH_ID;
    
    BV_WORD w = _bv[idx];
    EXEC_PATH_ID pos = idx * BV_WORD_SIZE;

    while (!(w&1)) { w >>= 1;  ++pos; }
    return pos; 
}

    /* =======================================================
     *
     *  Next_Path_Id 
     *
     *  return the minimum EXEC_PATH_ID which are greater
     *  than <path> if <path> is the largest path in this 
     *  set this routine return INVALID_PATH_ID.
     *  
     *
     *  if <check_membershit> is turned on, Next_Path_Id will 
     *  trigger assertion when it encounts an non-member <path>.
     * 
     * ========================================================
     */
EXEC_PATH_ID
EXEC_PATH_SET :: Next_Path_Id 
    (EXEC_PATH_ID path, BOOL check_membership) {
    
    if (!Path_Id_Is_Valid (path, check_membership)) {
        return INVALID_EXEC_PATH_ID;
    }

    INT32 wd_idx, bp;
    WordIdx_BitPos (path, wd_idx,bp);

        /* check membership if necessary 
         */
    if (check_membership && 
        !BV_WORD_Test_Bit (_bv[wd_idx], bp)) {

        FmtAssert (FALSE, ("path(%d) is not a member of this set", path));

    }

    BV_WORD w = BV_WORD_SHIFT_RIGHT(_bv[wd_idx], bp + 1);

    if (w) {
        bp ++ ;
    } else { 
            /* next EXEC_PATH_ID is *NOT* in the same BV_WORD 
             * as <path> does.
             */
        if (_bv_words_num == wd_idx) {
                /* obviously, <path> is last/greatest 
                 * EXEC_PATH_ID in this set.
                 */
            return INVALID_EXEC_PATH_ID;
        }

        bp = 0; 
        for (wd_idx ++; wd_idx < _bv_words_num ; wd_idx ++) {

            w = _bv[wd_idx];
            if (w) break ; 
        }

        if (wd_idx >= _bv_words_num) {
            return INVALID_EXEC_PATH_ID;
        }
    }
        
        /* skipping the 0-bits
         */
    while (!(w&1)) { w >>= 1; ++ bp ; }

    return wd_idx * BV_WORD_SIZE + bp;
}

    /* =================================================
     *
     *  Last_Path_Id 
     * 
     *  Has not yet implemented
     * 
     * ================================================
     */
EXEC_PATH_ID
EXEC_PATH_SET :: Last_Path_Id (void) {

    FmtAssert (FALSE,("Sorry, this routines has yet been implemented"));

    return 0;
}

    /* =================================================
     *
     * Prev_Path_Id 
     *
     * Has not yet implemented.
     *
     * =================================================
     */
EXEC_PATH_ID
EXEC_PATH_SET :: Prev_Path_Id 
    (EXEC_PATH_ID path, BOOL check_membership) {

    FmtAssert (FALSE,("Sorry, this routines has yet been implemented"));

    return 0;

}

    /* =======================================================
     *
     *  There_Are_Continguous_Path_Id 
     *
     * check to see whether there are <length> number of 
     * continguous path-id begins with <begin_path_id> 
     * (including <begin_path_id> itself.
     * 
     * =======================================================
     */
BOOL
EXEC_PATH_SET::There_Are_Continguous_Path_Id 
        (EXEC_PATH_ID begin_path_id, INT32 length) {

    Is_True (length > 0, 
             ("length(%d) should be no less than 1", length));

    mINT32 start_wd_idx, start_bp;
    mINT32 end_wd_idx,   end_bp;

    WordIdx_BitPos (begin_path_id, start_wd_idx, start_bp);
    WordIdx_BitPos (begin_path_id + length - 1, end_wd_idx, end_bp);

    BV_WORD w;
    if (start_wd_idx == end_wd_idx) {
       w =  Pattern_Word (length, start_bp); 
       return (_bv[end_wd_idx] & w) == w;
    }

    w = Pattern_Word (BV_WORD_SIZE - start_bp, start_bp);
    if ((_bv[start_wd_idx] & w) != w) {
        return FALSE ;
    }

    for (mINT32 i = start_wd_idx + 1; i < end_wd_idx ; i++) {
        if (_bv[i] != (BV_WORD)-1) return FALSE;
    }

    w = Pattern_Word (end_bp + 1, 0);
    
    return (_bv[end_wd_idx] & w) == w ;
}

    /* ========================================================
     *
     *  Union_Range_Inclusively 
     *
     *  Add EXEC_PATH_ID from <from> through <to> inclusively 
     *  to the set.
     *
     * =======================================================
     */
void
EXEC_PATH_SET :: Union_Range_Inclusively 
    (EXEC_PATH_ID from, EXEC_PATH_ID to) {
    
        /* check the input parameters' validity
         */
    Path_Id_Is_Valid (from, TRUE);
    Path_Id_Is_Valid (to,   TRUE);
    Is_True (from <= to, ("from (%d) > to(%d)", from, to));

    INT32 from_word_idx, to_word_idx, from_bitpos, to_bitpos;

    WordIdx_BitPos (from, from_word_idx, from_bitpos);
    WordIdx_BitPos (to,   to_word_idx,  to_bitpos);

    if (from_word_idx == to_word_idx) {

        _bv[from_word_idx] |= 
            Pattern_Word (to_bitpos - from_bitpos + 1, from_bitpos);

        return ;
    }

    _bv[from_word_idx] |= 
        Pattern_Word (BV_WORD_SIZE - from_bitpos, from_bitpos);

    for (mINT32 i = from_word_idx + 1 ; i < to_word_idx; i ++) {
        _bv[i] = (BV_WORD)(-1);
    }

    _bv[to_word_idx] |= Pattern_Word (to_bitpos + 1, 0);  
}

    /* ==================================================
     *
     * Difference operation 
     *
     * ==================================================
     */
EXEC_PATH_SET&
EXEC_PATH_SET:: operator - (const EXEC_PATH_SET& eps) {
   
    Equal_Size (eps, TRUE/*abort exec if !equ-size*/);
    
    EXEC_PATH_SET * p = CXX_NEW(EXEC_PATH_SET(_mp,_size),_mp);
    * p = *this ;

    p->Bitwise_Not ();
    *p &= *this;

    return *p;
}

void
EXEC_PATH_SET :: operator -= (const EXEC_PATH_SET& eps) {
    
    Equal_Size (eps, TRUE);

    for (mINT32 i = _bv_words_num - 1; i >= 0 ; i--) {
        _bv[i] &= ~eps._bv[i]; 
    }
}

    /* ========================================================
     *
     *   Union_Partioned_Path_Set 
     *
     * Partion each contiguous <pred_path_num> '1' of eps into 
     * <pred_path_num - pred_path_num - base> of '0' followed
     * by <succ_path_num>s '1' and then followed by <base> of
     * '0'. 
     *
     * ========================================================
     */ 
void
EXEC_PATH_SET :: Union_Partioned_Path_Set 
    (EXEC_PATH_SET& eps,  INT32 pred_path_num,
     INT32 succ_path_num, INT32 base) {

    EXEC_PATH_SET p = eps ;
    INT32 path_id = eps.First_Path_Id ();

    while (!EXEC_PATH_ID_IS_INVALID(path_id)) {

        #ifdef Is_True_On
        if (!eps.There_Are_Continguous_Path_Id (path_id, pred_path_num)) {
            Is_True (FALSE, 
                ("There should be %d number of continous '1' bits", 
                  pred_path_num));
        }
        #endif /* Is_True_On */

        Union_Range_Inclusively 
            (path_id + base, path_id + base + succ_path_num - 1);

        path_id += (pred_path_num - 1); 
        path_id = eps.Next_Path_Id (path_id, FALSE);
    }
}

    /* ====================================================
     * ====================================================
     * 
     *      EXEC_PATH_MGR implementation starts from here 
     *
     * ====================================================
     * ====================================================
     */
EXEC_PATH_MGR :: EXEC_PATH_MGR (MEM_POOL *mp) :
    _mp(mp),  _pathv(mp), 
    _ep_node_info (mp) {

    _path_info_invalid = TRUE;
    _total_paths = 0;
}


EXEC_PATH_MGR&
EXEC_PATH_MGR :: operator = (EXEC_PATH_MGR& epm) {
    
        /* copy a manager is not allowed 
         */
    FmtAssert (FALSE,("Copying a 'manager' is not allowed"));
    
    return *this ; /* to make compiler happy */
}


    /* ===========================================================
     * 
     *  Calc_Subgraph_Path_Num 
     *
     *  calculate the path through each "x-subgraph". It is a 
     *  supporting routine to Acquire_Path_Info.
     *  x-subgraph is such termed:
     *
     *      a) x-subgraph is a subgraph of <_reigon> (data member 
     *        of this class, of type REGION). and,
     *
     *      b) x-subgraph is a DAG(obviously, since <_region> 
     *         itself is DAG). and
     *
     *      c) node x is the *ONLY* root of x-graph. and
     *     
     *      d) x-subgraph is maximum subgraph which satisfies 
     *         constraint a), b) and c)
     *
     *   put in other word, x-subgraph is the maximum SEME(single 
     *   entry multiple exits) subgraph of <_region>(of type REGION)
     *   with node x being its only root node.
     *
     * ============================================================
     */
INT32
EXEC_PATH_MGR :: Calc_Subgraph_Path_Num (void) {
    
    REGIONAL_CFG_NODE * n  = NULL ;
    EP_NODE_INFO      * ni = NULL ;

    for (REVERSE_TOPO_REGIONAL_CFG_ITER riter(_region->Regional_Cfg());
         riter != 0 ;
         ++riter) {
        n  = *riter ;
        ni = (EP_NODE_INFO *)_ep_node_info.Get_Map (n);
        
        ni->subgraph_path_num = 0;

        for (CFG_SUCC_NODE_ITER succs(n) ; succs != 0 ; ++succs) {
            REGIONAL_CFG_NODE * s = *succs ;
            EP_NODE_INFO * si = (EP_NODE_INFO*)_ep_node_info.Get_Map (s);
            ni->subgraph_path_num += si->subgraph_path_num;
        }

        mINT32 tmp = ni->subgraph_path_num;
        if (!tmp) {
            /* <n> is leaf node of <_region>
             */
            ni->subgraph_path_num = 1;
        } else if (tmp > MAX_PATH_NUM) {
            _path_info_invalid = TRUE; 
            return -1;
        }
    }

        /* ni is now associated with the root node 
         */
    Is_True (ni, ("we got an empty REGION(%d)", _region->Id()));
    return ni->subgraph_path_num;
}


    /* =====================================================
     *
     * Acquire_Path_Info 
     *
     * acquire execution path information of entire REGION
     *
     * =====================================================
     */
INT32
EXEC_PATH_MGR :: Acquire_Path_Info (REGION *r) {

    _region = r ;
    if (!r) { return 0 ; }

        /* step 1: initialize the map between each node
         *         and EP_NODE_INFO structure where we 
         *         need keep some info associated with
         *         node.
         */
    _ep_node_info.Initialize_Map (_region);
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_region->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        
        REGIONAL_CFG_NODE * n = *iter ; 
        EP_NODE_INFO* t=CXX_NEW(EP_NODE_INFO(n,_mp),_mp);
        _ep_node_info.Set_Map (n,t);

    }

        /* step 2: calculate how many path through each node
         */
    mINT32 total_path_num = Calc_Subgraph_Path_Num ();
    if (total_path_num >= 0) {
        _total_paths = total_path_num;
    } else {
        _path_info_invalid = TRUE;
        return -1;
    } 

        /* step 3: (1) Initialize each node's path-set. including:
         *            - path-set's size(==total_path_num)
         *            - set root-node's path-set to be 
         *              {Px | 0 <= x < total_path_num}
         *
         *         (2) Initialize EXEC_PATH for each execution-path. 
         */
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_region->Regional_Cfg()) ; 
         iter != 0; ++iter) {
         EP_NODE_INFO * ni = (EP_NODE_INFO*) _ep_node_info.Get_Map (*iter);
         ni->eps.Resize (total_path_num);
    }
    
        /* handle the *unique* root node 
         */
    NODE_VECTOR rnv = _region->Entries () ;
    Is_True (rnv.size() == 1, 
            ("_region(id:%d) has more than one entries!"));
    REGIONAL_CFG_NODE * unique_root = rnv[0];

    ((EP_NODE_INFO*) _ep_node_info.Get_Map (unique_root))
        -> eps.Set_To_Be_Universe ();
      

        /* initialize EXEC_PATH for each individual exec path
         */ 
    _pathv.resize (total_path_num);
    for (INT i = 0 ; i < total_path_num; i ++) {
        _pathv[i] = CXX_NEW(EXEC_PATH(EXEC_PATH_ID(i),_mp),_mp);
        _pathv[i] -> Append_Path_Segment (NULL, unique_root);
    }

        /* step 4: calculate path-set flow through each node, and
         *         at the same time collect the information for 
         *         each individual path.
         */
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_region->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        
        INT32 alloc_base = 0;
        EP_NODE_INFO *  nd_info = 
            (EP_NODE_INFO*) _ep_node_info.Get_Map (*iter);
        EXEC_PATH_SET * nd_eps  = &nd_info->eps;

        for (REGIONAL_CFG_EDGE * e = (*iter)->First_Succ () ;
             NULL != e; 
             e = e->Next_Succ ()) {
    
            REGIONAL_CFG_NODE * succ = e->Dest ();
            EP_NODE_INFO * succ_info = 
                (EP_NODE_INFO*) _ep_node_info.Get_Map (e->Dest ());
            EXEC_PATH_SET * succ_eps = &succ_info->eps;

                /* ref comment of <Union_Partioned_Path_Set>
                 * for details.
                 */
            succ_eps->Union_Partioned_Path_Set (
                         nd_info   -> eps, 
                         nd_info   -> subgraph_path_num,
                         succ_info -> subgraph_path_num,
                         alloc_base);

                /* The following commented out statements are used to 
                 * collect info of individual paths, however, currently 
                 * these info does not actually needed during code motion.
                 */
            /*
            for (EXEC_PATH_ID pid = succ_eps->First_Path_Id () ; 
                 !EXEC_PATH_ID_IS_INVALID(pid); 
                 pid = succ_eps->Next_Path_Id (pid)) {
                
                Is_True (pid < Path_In_Total (), 
                         ("path id(%d) is out of range(<%d)", 
                          pid, 
                          Path_In_Total ()));

                (_pathv[pid])->Append_Path_Segment (e,e->Dest());
            }*/

            alloc_base += succ_info -> subgraph_path_num;
        }
    }
    
    _path_info_invalid = FALSE;

    return total_path_num;
}

    /* ====================================================
     *
     * Get_Path_Flow_Thru 
     *
     * return all path (within <bb>/<r>'s home region) flow 
     * through <bb>/<r>.
     *
     * ====================================================
     */
EXEC_PATH_SET* 
EXEC_PATH_MGR :: Get_Path_Flow_Thru (BB* bb) {

    Is_True (!Path_Info_Is_Invalid (), 
             ("execution path information does not collect properly"));

    EP_NODE_INFO* t=(EP_NODE_INFO*) _ep_node_info.Get_Map (bb);
    return t ? &t->eps : NULL;

}

EXEC_PATH_SET*
EXEC_PATH_MGR :: Get_Path_Flow_Thru (REGION* r) {

    Is_True (!Path_Info_Is_Invalid (), 
             ("execution path information does not collect properly"));

    EP_NODE_INFO* t=(EP_NODE_INFO*) _ep_node_info.Get_Map (r);
    return t ? &t->eps : NULL;
}



    /* ====================================================
     * ====================================================
     *
     *    class SUB_SEME_EXEC_PATH_MGR's definition
     *
     * ====================================================
     * ====================================================
     */
SUB_SEME_EXEC_PATH_MGR :: SUB_SEME_EXEC_PATH_MGR (MEM_POOL * mp) :
    _mp(mp), _paths(mp) {

    _sub_SEME_root = NULL;
    _base_graph    = NULL; 
}

SUB_SEME_EXEC_PATH_MGR :: ~SUB_SEME_EXEC_PATH_MGR (void) {
    /* do nothing */
}

    /* =====================================================
     *
     * Derive_Exec_Path_Info 
     *
     * derive sub-SEME-graph(control flow)'s execution path 
     * information from current schedule-scope(REGION,SEME)'s 
     * exec-path info which is managed by EXEC_PATH_MGR.
     *
     * =====================================================
     */
void
SUB_SEME_EXEC_PATH_MGR :: Derive_Exec_Path_Info 
    (EXEC_PATH_MGR * exec_path_mgr, 
     REGIONAL_CFG_NODE* sub_SEME_root) {

    
}
    /* =====================================================
     * =====================================================
     * 
     *   RGN_CFLOW_MGR implementation starts from here
     *  
     * =====================================================
     * =====================================================
     */

char * RGN_CFLOW_MGR::_invalid_prompt_msg = 
                "control flow is invalid!";

    /* ====================================================
     *
     *  _init_data_member 
     * 
     * Initialize the data member of RGN_CFLOW_MGR, called 
     * only by Init({REGION* | BB*})
     *
     * ====================================================
     */
void
RGN_CFLOW_MGR::_init_data_member (void) {

    _cflow_info_valid = FALSE;
    _bb_num  = _rgn_num = 0;

    _max_bb_id = _min_bb_id = _max_rgn_id = _min_rgn_id = 0;

    _scope = NULL ;
    _bb_scope = NULL;

    _bb_id_2_map_idx_vect    = NULL;
    _rgn_id_2_map_idx_vect   = NULL;
    _map_idx_2_bb_vect       = NULL;
    _map_idx_2_rgn_vect      = NULL; 

}

    /* =====================================================
     *
     *  Init 
     *   
     * acquire the control flow into the <rgn> 
     *
     * ====================================================
     */
void
RGN_CFLOW_MGR::Init (REGION *rgn) {
    _init_data_member ();
    
    _scope = rgn ;

    _cflow_info_valid = TRUE;
    _acquire_cflow_info () ;
}

    /* ========================================================
     *
     * Init 
     * 
     * Setup the class properly when schedule scope is confined 
     * whinin <bb>. this routine are provided to make other 
     * routines uniformly handle the two cases : regional cfg 
     * and single-BB.
     *
     * ========================================================
     */
void
RGN_CFLOW_MGR::Init (BB * bb) {
    _init_data_member () ;

    _scope = NULL;
    _bb_scope = bb ;

    _max_bb_id = _min_bb_id = BB_id(bb);
    _bb_num = 1;

    _cflow_info_valid = TRUE;
}

   /* =============================================================
    *
    * _acquire_basic_cflow_info 
    * 
    * acquire "basic" information of CFG. 
    * e.g how many BB and nested region in current schedule scope
    *   
    * =============================================================
    */
void
RGN_CFLOW_MGR::_acquire_basic_cflow_info (void) {

    _bb_num     = _rgn_num      = 0 ;
    _max_bb_id  = _min_bb_id    = 0 ;
    _max_rgn_id = _min_rgn_id   = 0 ;

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        UINT32   node_id ; 

        if ((*iter)->Is_Region()) {
            ++ _rgn_num ; 
            node_id     = (UINT32) (*iter)->Region_Node()->Id() ;   
            _max_rgn_id = max (_max_rgn_id, node_id) ;
            _min_rgn_id = min (_min_rgn_id, node_id) ;
        } else {
            ++ _bb_num ;
            node_id     = (UINT32) BB_id((*iter)->BB_Node()) ;
            _max_bb_id  = max (_max_bb_id, node_id) ;
            _min_bb_id  = min (_min_bb_id, node_id) ;
        }
    }
}

    /* ===============================================================
     *
     *  _setup_map_array 
     * 
     *  allocate space for mapping, these mapping including 
     *
     *      1. map the discrete BB id to contiguous range of integer, 
     *      2. map BB's-contiguous-id to BB*
     *      3. map the discrete REGION id to contigous range of integer.
     *      4. map REGION's-contiguous-id to REGION *
     *
     * ===============================================================
     */

void
RGN_CFLOW_MGR::_setup_map_array (void) {

    _bb_id_2_map_idx_vect = 
        _bb_num ? TYPE_MEM_POOL_ALLOC_N (UINT32, &_mem_pool, 
                                         1 + _max_bb_id) :
                  NULL;

    _rgn_id_2_map_idx_vect = 
        _rgn_num ? TYPE_MEM_POOL_ALLOC_N (UINT32, &_mem_pool,
                                          1 + _max_rgn_id) :
                   NULL;

    _map_idx_2_bb_vect = 
        _bb_num ? TYPE_MEM_POOL_ALLOC_N (BB* , &_mem_pool, 
                                         1 + _max_bb_map_idx()) : 
                  NULL; 

    _map_idx_2_rgn_vect = 
        _rgn_num ? TYPE_MEM_POOL_ALLOC_N (REGION *, &_mem_pool,
                                          1 + _max_rgn_map_idx()) :
                   NULL;            
}

    /* ========================================================
     *
     *  _setup_node_cflow_info_array 
     * 
     *  create NODE_CFLOW_INFO array 
     * 
     * ========================================================
     */
void
RGN_CFLOW_MGR::_setup_node_cflow_info_array (void) {

    INT32 elem_num = _bb_num ? _max_bb_map_idx() + 1 : 0;
    if (elem_num) {
        _bb_node_cflow_info.resize (elem_num);
    } else {
        _bb_node_cflow_info.clear ();
    }

    elem_num = _rgn_num ? _max_rgn_map_idx() + 1 : 0 ; 
    if (elem_num) {
        _rgn_node_cflow_info.resize (elem_num);
    } else {
        _rgn_node_cflow_info.clear ();
    }
}

    /* =======================================================
     *
     *  _node_cflow_info (BB*) 
     * 
     * get the _NODE_CFLOW_INFO associated with <bb>
     *
     * =======================================================
     */
inline RGN_CFLOW_MGR::_NODE_CFLOW_INFO&
RGN_CFLOW_MGR::_node_cflow_info (BB *bb) {

    if (_bb_2_map_idx(bb) >= _bb_node_cflow_info.size()) {
        FmtAssert (FALSE, 
                   ("BB:%d's node info has not set up properly",
                   BB_id(bb)));
    }

    return _bb_node_cflow_info[_bb_2_map_idx(bb)];

}

    /* ======================================================
     * 
     * _node_cflow_info 
     *  
     * get the _NODE_CFLOW_INFO associated with <rgn>
     *
     * ======================================================
     */
inline RGN_CFLOW_MGR::_NODE_CFLOW_INFO&
RGN_CFLOW_MGR::_node_cflow_info (REGION *rgn) {

    if (_rgn_2_map_idx(rgn) >= _rgn_node_cflow_info.size ()) {
        FmtAssert (FALSE,
                   ("REGION:%d's node info has not set up properly",
                   rgn->Id ()));
    }

    return _rgn_node_cflow_info[_rgn_2_map_idx(rgn)]; 
}

    /* ======================================================
     * 
     * _node_cflow_info 
     *  
     * get the _NODE_CFLOW_INFO associated with <node>
     *
     * ======================================================
     */
inline RGN_CFLOW_MGR::_NODE_CFLOW_INFO&
RGN_CFLOW_MGR::_node_cflow_info (REGIONAL_CFG_NODE * node) {

    return node->Is_Region () ? 
        _node_cflow_info (node->Region_Node()) :
        _node_cflow_info (node->BB_Node()) ;
}


  /* ============================================================
   * 
   *   Access to _NODE_CFLOW_INFO's each fields
   *
   * ============================================================
   */

INT32
RGN_CFLOW_MGR::Max_Level (REGIONAL_CFG_NODE * node) {
    Is_True (Valid (), (_invalid_prompt_msg));

    return (_node_cflow_info (node)).max_level ;
}

INT32
RGN_CFLOW_MGR::Max_Level (BB * bb) {

    Is_True (Valid (), (_invalid_prompt_msg));

    if (!_bb_scope) {
        return (_node_cflow_info (bb)).max_level ;
    } else {
        Is_True (bb == _bb_scope, ("not in this scope!"));
        return 1;
    }
}

INT32
RGN_CFLOW_MGR:: Min_Level (REGIONAL_CFG_NODE * node) {

    Is_True (Valid (), (_invalid_prompt_msg));

    return (_node_cflow_info (node)).min_level ;
}

INT32
RGN_CFLOW_MGR::Min_Level (BB * bb) {

    Is_True (Valid (), (_invalid_prompt_msg));

    if (!_bb_scope) {
        return (_node_cflow_info (bb)).min_level ;
    } else {
        Is_True (bb == _bb_scope, ("not in this scope!"));
        return 1 ;
    }
}

    /* ====================================================
     *
     * BB1_Reachable_From_BB2 
     * 
     *  return TRUE iff <bb1> is reachable from <bb2> in 
     *  current acyclic schedule scope; FALSE otherwise.
     *
     * ====================================================
     */
BOOL
RGN_CFLOW_MGR::BB1_Reachable_From_BB2 (BB * bb1, BB* bb2) {
    if (bb1 == bb2) return TRUE;

    BS * reach_info = _reach_info_vect (bb2) ;
    Is_True (reach_info, ("bb's reachable info is not available!")) ;

    return BS_MemberP (reach_info, _bb_2_map_idx(bb1)); 
}


    /* ======================================================
     *
     * BB_Reachable_From_RGN  
     * 
     * return TRUE iff <bb> is reachable from nested <rgn> 
     * in current *ACYCLIC* schedule scope. FALSE otherwise.
     *
     * ======================================================
     */
BOOL
RGN_CFLOW_MGR::BB_Reachable_From_RGN  (BB * bb, REGION * rgn) {
    BS * reach_info = _reach_info_vect (rgn) ;
    Is_True (reach_info, ("rgn's reachable info is not available!"));

    return BS_MemberP (reach_info, _bb_2_map_idx(bb));
}


   /* =======================================================
    *
    * _reach_info_vect (BB*)
    * 
    * return all BBs that are reachable from <bb> in current
    * schedule scope.
    * 
    * =======================================================
    */
inline BS *
RGN_CFLOW_MGR::_reach_info_vect (BB *bb) {

    return _node_cflow_info (bb).reach_bb ;
}

   /* ========================================================
    *
    * _reach_info_vect (REGION *)
    *
    * return all BBs that are reachable from nested <rgn> in
    * current schedule scope.
    *
    * =======================================================
    */
inline BS *
RGN_CFLOW_MGR::_reach_info_vect (REGION *rgn) {

    return (_node_cflow_info (rgn)).reach_bb ;
}

   /* ===========================================================
    *
    * _reach_info_vect (REGIONAL_CFG_NODE *)
    *
    * return all BBs that are reachable from regional-cfg-<node> 
    * in current schedule scope.
    *
    * ===========================================================
    */
inline BS *
RGN_CFLOW_MGR::_reach_info_vect (REGIONAL_CFG_NODE *node) {

    return (_node_cflow_info (node)).reach_bb ;
}

inline RGN_CFLOW_MGR::REACH_PROB_VECT *
RGN_CFLOW_MGR::_reach_prob_vect (BB *bb) {

    return &((_node_cflow_info (bb)).reach_prob) ;
}


inline RGN_CFLOW_MGR::REACH_PROB_VECT *
RGN_CFLOW_MGR::_reach_prob_vect (REGION *rgn) {

    return &((_node_cflow_info (rgn)).reach_prob);
}

inline RGN_CFLOW_MGR::REACH_PROB_VECT *
RGN_CFLOW_MGR::_reach_prob_vect (REGIONAL_CFG_NODE * node) {

    return &((_node_cflow_info (node)).reach_prob);
}



/* ============================================================
 * 
 *      reachable-{info|prob}-vector util 
 *
 * ==========================================================*/
inline  BS *
RGN_CFLOW_MGR::_create_empty_reach_bb_vect (void) {
    return BS_Create_Empty (_max_bb_map_idx()+1 , &_mem_pool);
}

inline BS * 
RGN_CFLOW_MGR::_add_reachable_bb (BB *from, BB* to) {
    BS * reach_info = _reach_info_vect (from) ;
    Is_True (reach_info, ("reachable info is not available!"));
    
    return BS_Union1D (reach_info, _bb_2_map_idx (to), &_mem_pool) ;
}

inline BS * 
RGN_CFLOW_MGR::_add_reachable_bb (REGION* rgn, BB* to) {
    BS * reach_info = _reach_info_vect (rgn) ;
    Is_True (reach_info, ("reachable info is not available!"));

    return BS_Union1D (reach_info, _bb_2_map_idx (to), &_mem_pool) ;
}

inline  BS *
RGN_CFLOW_MGR::_add_reachable_bb (BS * vect, BB* bb, MEM_POOL *mp) {
    return BS_Union1D (vect, _bb_2_map_idx(bb), mp) ;
}

inline  BS *  
RGN_CFLOW_MGR::_add_reachable_bbs (BB *from, BS * reach_bbs) {
    BS * reach_info = _reach_info_vect (from) ;
    Is_True (reach_info, ("reachable info is not available!"));
   
    return BS_UnionD (reach_info, reach_bbs, &_mem_pool) ;
}

inline  BS *  
RGN_CFLOW_MGR::_add_reachable_bbs (REGION* rgn, BS * reach_bbs) {
    BS * reach_info = _reach_info_vect (rgn) ;
    Is_True (reach_info, ("reachable info is not available!"));

    return BS_UnionD (reach_info, reach_bbs, &_mem_pool) ;
}

inline  BS * 
RGN_CFLOW_MGR::_add_reachable_bbs (
    REGIONAL_CFG_NODE * node, BS * reach_bbs) {

    return node->Is_Region () ? 
            _add_reachable_bbs (node->Region_Node(), reach_bbs) :
            _add_reachable_bbs (node->BB_Node () , reach_bbs);
}

inline BS *
RGN_CFLOW_MGR::_set_bb_is_reachable (BS* reach_vect, BB * bb,MEM_POOL* p) {

    return BS_Union1D (reach_vect, _bb_2_map_idx(bb),p);
}

inline BOOL
RGN_CFLOW_MGR::_is_bb_reachable (BS *reach_vect, BB *bb) {

   return BS_MemberP (reach_vect, _bb_2_map_idx(bb));
}


inline void
RGN_CFLOW_MGR::_set_bb_reach_prob (REACH_PROB_VECT* prob_vect, 
                                   BB* src_bb, REACH_PROB prob) {

    INT32 map_idx = _bb_2_map_idx(src_bb);
    Is_True (map_idx < prob_vect->elem_num, 
             ("map idx out of range"));
    prob_vect->reach_prob_vect[map_idx] = prob ;

}

inline void
RGN_CFLOW_MGR::_set_bb_reach_prob (BB *from, BB* to,REACH_PROB prob) {

    REACH_PROB_VECT * vect = _reach_prob_vect (from);
    Is_True (vect, ("reachable prob is NULL")) ;

    INT32 map_idx = _bb_2_map_idx(to) ;
    Is_True (map_idx < vect->elem_num, ("map idx out of range"));
    vect->reach_prob_vect[map_idx] = prob ;
}


REACH_PROB 
RGN_CFLOW_MGR::_bb_reach_prob (REGION * from,BB *to) {
    REACH_PROB_VECT * vect = _reach_prob_vect(from);
    Is_True (vect, ("reachable prob is NULL")) ;

    INT32 map_idx = _bb_2_map_idx(to) ;
    Is_True (map_idx < vect->elem_num, ("map idx out of range"));

    return  vect->reach_prob_vect[map_idx];
}

REACH_PROB
RGN_CFLOW_MGR::_bb_reach_prob (BB * from,BB *to) {
    REACH_PROB_VECT * vect = _reach_prob_vect(from); 
    Is_True (vect, ("reachable prob is NULL"));

    INT32 map_idx = _bb_2_map_idx(to) ;
    Is_True (map_idx < vect->elem_num, ("map idx out of range"));

    return  vect->reach_prob_vect[map_idx];
}

REACH_PROB
RGN_CFLOW_MGR::_bb_reach_prob (REGIONAL_CFG_NODE * node,BB *to) {
    return node->Is_Region () ? 
           _bb_reach_prob (node->Region_Node(), to) :
           _bb_reach_prob (node->BB_Node () , to);
}


void
RGN_CFLOW_MGR::_fused_mult_add (REACH_PROB_VECT * dest, REACH_PROB_VECT *src, 
            float mult_by) {

    Is_True (dest->elem_num == src->elem_num, ("mismatch vector!"));

    for (INT32 i = dest->elem_num - 1 ; i >= 1 ; i --) {
        dest->reach_prob_vect[i] =  
            INT32(dest->reach_prob_vect[i] + src->reach_prob_vect[i] * mult_by) ;
    }
}

 /* ===========================================================
  * 
  *         miscellaneous 
  * 
  * =========================================================*/


void
RGN_CFLOW_MGR::_acquire_cflow_info (void) {

    _acquire_basic_cflow_info ();

    _setup_map_array () ; /* setup mapping arrays */
    _setup_node_cflow_info_array () ;

    /*  forward pass :
     *  o. BB_id(bb) => bb-map-idx & REGION->Id() => rgn-map-idx 
     *  o. bb-map-idx => BB*  & rgn-map-idx => REGION *
     *  o. calc nodes' "min_level"
     */

    UINT32 next_bb_map_idx  = ID_MAP_BASE ;
    UINT32 next_rgn_map_idx = ID_MAP_BASE ;

    INT32  max_leaf_node_level = 0 ;

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter (_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {

        BB * b ; REGION * r;
        
        if (!(*iter)->Is_Region()) {
            b = (*iter)->BB_Node () ;
            _bb_id_2_map_idx_vect [BB_id(b)] = next_bb_map_idx ; 
            _map_idx_2_bb_vect [next_bb_map_idx++] = b ; 

            _node_cflow_info(b).node.bb = b;

        } else {
            r = (*iter)->Region_Node() ;
            _rgn_id_2_map_idx_vect [r->Id()] = next_rgn_map_idx ;
            _map_idx_2_rgn_vect [next_rgn_map_idx++] = r ;

            _node_cflow_info(r).node.rgn = r ;
        }

        UINT32 level = Min_Level (*iter) ;
        for (CFG_PRED_NODE_ITER preds(*iter) ; preds != 0 ; ++preds) {
            UINT32  l = Min_Level (*preds) ;
            level = max (level,l+1) ; 
        }

        _NODE_CFLOW_INFO& info = _node_cflow_info (*iter);
        info.min_level = level ;
        info.max_level = _bb_num + _rgn_num + 1;

        if (!(*iter)->Succ_Num()) {
            max_leaf_node_level = max(max_leaf_node_level, level);
        }
    }

        /* backward pass :
         * o. get "max_level" for each node 
         */
    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter (_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {

        if (!(*iter)->Succ_Num()) {
            _node_cflow_info(*iter).max_level = max_leaf_node_level ;
        }

        UINT32 level = _node_cflow_info(*iter).max_level ; 

        for (CFG_PRED_NODE_ITER preds(*iter) ; preds != 0 ; ++preds) {
            UINT32 l = Max_Level (*preds) ;
            _node_cflow_info (*preds).max_level = min(l, level-1);
        }
    }
    
    _acquire_reachable_info () ;
    _acquire_reach_prob_info () ;

    _exec_path_mgr.Acquire_Path_Info (Scope ());
}

void
RGN_CFLOW_MGR::_acquire_reachable_info (void) {

    if (_bb_num) {
        Is_True (!_bb_node_cflow_info.empty (), 
            ("_bb_node_cflow_info has not built up!")) ;

        for (INT32 i = ID_MAP_BASE; i <= _bb_num ; i++) {
            _bb_node_cflow_info[i].reach_bb = 
                _create_empty_reach_bb_vect ();
        }
    }

    if (_rgn_num) {
        Is_True (!_rgn_node_cflow_info.empty (),
                 ("_rgn_node_cflow_info has not built up!")) ;
        for (INT32 i = ID_MAP_BASE ; i <= _rgn_num ; i ++) {
            _rgn_node_cflow_info[i].reach_bb = 
                _create_empty_reach_bb_vect ();
        }
    }

    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter(_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        REACH_INFO_VECT * reach_bb = (_node_cflow_info(*iter)).reach_bb;

        for (CFG_SUCC_NODE_ITER succs(*iter) ; succs != 0 ; ++succs) {
            REACH_INFO_VECT * succ_reach_bb = 
                                    _node_cflow_info (*succs).reach_bb;
            Is_True (succ_reach_bb, ("succs reachable vector has not built up!"));

            reach_bb = _add_reachable_bbs (*iter,succ_reach_bb);
            if ((*succs)->Is_Region()) continue ;
            reach_bb = _set_bb_is_reachable (reach_bb, (*succs)->BB_Node(),&_mem_pool);
        }

        _node_cflow_info (*iter).reach_bb = reach_bb ; 
    }
}

void
RGN_CFLOW_MGR::_acquire_reach_prob_info (void) {
    if (_bb_num == 0) return ;

    if (_bb_num) {

        for (INT32 i = ID_MAP_BASE ; 
             i <= _bb_num + ID_MAP_BASE - 1 ; i++) {

            _bb_node_cflow_info[i].reach_prob.elem_num = 
                    _max_bb_map_idx() + 1;
            _bb_node_cflow_info[i].reach_prob.vect_size = 
                    _max_bb_map_idx() + 1;
            _bb_node_cflow_info[i].reach_prob.reach_prob_vect = 
                    TYPE_MEM_POOL_ALLOC_N (REACH_PROB, 
                                           &_mem_pool,_max_bb_map_idx() + 1);
        } /* end of for */
    } /* end of if */

    if (_rgn_num) {

        for (INT32 i = ID_MAP_BASE; 
             i <= _rgn_num + ID_MAP_BASE - 1; i++) {

            _rgn_node_cflow_info[i].reach_prob.elem_num = 
                    _max_bb_map_idx() + 1;

            _rgn_node_cflow_info[i].reach_prob.vect_size = 
                    _max_bb_map_idx() + 1 ;

            _rgn_node_cflow_info[i].reach_prob.reach_prob_vect = 
                    TYPE_MEM_POOL_ALLOC_N (REACH_PROB, 
                                           &_mem_pool,_max_bb_map_idx() + 1);
        }
    }

    REGIONAL_CFG * cfg = _scope -> Regional_Cfg();

    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter (cfg); iter != 0 ; ++iter) {
        if (!(*iter)->Is_Region()) {
            BB *bb = (*iter)->BB_Node() ;
            _set_bb_reach_prob (bb,bb,(UINT32)(1.0 * REACH_PROB_SCALE)) ;
        }
        
        REACH_PROB_VECT * dest = _reach_prob_vect (*iter) ;
        
        for (REGIONAL_CFG_EDGE * edges = (*iter)->First_Succ() ;
            edges != NULL ; edges = edges->Next_Succ()) {
            REACH_PROB_VECT * src = _reach_prob_vect (edges->Dest()) ;
            _fused_mult_add (dest, src, cfg->Edge_Prob(edges)) ; 
        }
    }
}


UINT16
RGN_CFLOW_MGR::bb_node_succ_num (REGIONAL_CFG_NODE * node) {
    REGIONAL_CFG_EDGE * edge ;
    INT icount = 0 ; 
    for (edge = node->First_Succ() ; edge ; edge = edge->Next_Succ()) 
       if (!edge->Dest()->Is_Region()) ++ icount ;
    return icount ;
}

UINT16
RGN_CFLOW_MGR::bb_node_pred_num (REGIONAL_CFG_NODE * node) {
    REGIONAL_CFG_EDGE * edge ;
    INT icount = 0 ;
    for (edge = node->First_Pred() ; edge ; edge = edge->Next_Pred()) 
       if (!edge->Dest()->Is_Region()) ++ icount ;
    return icount ;
}

BOOL
RGN_CFLOW_MGR::Has_Scheduled_Preds (BB* bb) {
  BBLIST *pred ;
  FOR_ALL_BB_PREDS (bb,pred) 
    if (BB_scheduled (pred->item)) return TRUE;

  return FALSE ;
}

BOOL
RGN_CFLOW_MGR::Critical_Edge_Present (REGION *rgn) {

    REGIONAL_CFG *cfg = rgn->Regional_Cfg() ;

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(cfg); iter!=0;++iter){

        REGIONAL_CFG_NODE *node = *iter;
        if(node->Is_Region()) continue;
        if(node->Succ_Num()<=1) continue;

        INT succ_bb_num , pred_bb_num ;

        succ_bb_num = bb_node_succ_num (node) ;
        if (succ_bb_num <= 1) continue ;

        for(REGIONAL_CFG_EDGE *edge = node->First_Succ(); edge!=NULL; edge = edge->Next_Succ()){
            REGIONAL_CFG_NODE *succ_node = edge->Dest();
            if(succ_node->Is_Region()) continue;
            if (bb_node_pred_num (succ_node) > 1) 
               return TRUE ;
        }
    }

    return FALSE ;

}


void
RGN_CFLOW_MGR::_compute_node_level () {

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {

        _NODE_CFLOW_INFO& cflow_info = _node_cflow_info (*iter) ;
        cflow_info.min_level = 1 ;
        cflow_info.max_level = 1 ;

    }


    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {
        INT32 level ;

        _NODE_CFLOW_INFO& cflow_info = _node_cflow_info (*iter);
        level = cflow_info.min_level - 1;

        for (CFG_PRED_NODE_ITER preds(*iter) ; preds != 0 ; ++preds) {
            INT32 L = Min_Level (*preds);
            level = (level > L) ? level : L ;
        }
    
        cflow_info.min_level = cflow_info.max_level = level + 1 ; 
    }


    for (REVERSE_TOPO_REGIONAL_CFG_ITER iter (_scope->Regional_Cfg()) ; 
        iter != 0 ; ++iter) {

        INT32 level ;
        
        _NODE_CFLOW_INFO& cflow_info = _node_cflow_info (*iter);
        level = cflow_info.max_level + 1;

        for (CFG_SUCC_NODE_ITER succs(*iter) ; succs != 0 ; ++succs) {
            INT32 L = Max_Level(*succs);
            level = (level > L) ? level : L ; 
        }

        cflow_info.max_level = level - 1 ; 
    }
}

INT32 
RGN_CFLOW_MGR::Across_Node_Num (BB * from , BB * to) {

    if (from == to) return 0 ;

    BB * ance , * desc ;
    if (BB1_Reachable_From_BB2 (to, from)) {
        ance = from ; desc = to ; 
    } else {
        ance = to ; desc = from ; 
    }

    if (!BB1_Reachable_From_BB2(desc, ance)) return 0 ;

    INT32 d1 = Max_Level (desc) - Max_Level (ance);

    INT32 d2 = Min_Level (desc) - Min_Level (ance);

    return d1 < d2 ? d1 : d2 ;
}


    /* ===============================================================
     * ===============================================================
     *
     *      PU entry/exit block splitting & merging starts from here
     * 
     * ==============================================================
     * ==============================================================
     */
    
 /* keep track of the splitted (PU) entry- exit-blocks 
  */
static BB_VECTOR _splitted_pu_boundary_bbs ;  


 /*==============================================================
  *
  *  BB_Has_Already_Been_Splitted 
  * 
  *  return TRUE iff (PU) entry- or exit <bb> has already been 
  *  carved into two.
  *
  *  <bb> should be a PU's entry- or exit- block, otherwize 
  *  assertion is triggered.
  *
  * ============================================================
  */
static BOOL
BB_Has_Already_Been_Splitted (BB * bb) {

    Is_True (BB_entry(bb) || BB_exit(bb), 
            ("BB:%d is neither entry- nor exit-BB", BB_id(bb)));

    for (BB_VECTOR_ITER iter =  _splitted_pu_boundary_bbs.begin() ;
         iter != _splitted_pu_boundary_bbs.end () ; iter ++) {
        
        if (*iter == bb) return TRUE;
    }

    return FALSE ;
}

  /* =============================================================
   *
   *  Init_Split_PU_Entry_Or_Exit_BB 
   *  Add_Splitted_Entry_Exit_BB 
   *  Remove_Splitted_Entry_Exit_BB 
   *  
   *  keep track of splitted PU's entry- exit- block
   * 
   * ============================================================
   */
void
Init_Split_PU_Entry_Or_Exit_BB (void) {
    _splitted_pu_boundary_bbs.clear () ;
  
    /* Currently, scheduler has hard time in handling a block both are 
     * PU entry and exit. Split it into two to make to circumvent the 
     * problem.
     */
    if (!BB_exit (REGION_First_BB) || 
        !IPFEC_Glos_Split_Entry_BB || !IPFEC_Glos_Split_Exit_BB) {
        return ;
    }

    /* From this point through the end of this function, we are splitting 
     * the block into a sequence of 3 blocks: pure entry block, sandwiched 
     * middled block, and pure exit block.
     */

    /* find the split point from which through the end of REGION_First_BB are 
     * put into pure-exit block. 
     */ 
    OP* exit_boundary_op = NULL, *entry_boundary_op = NULL, *op;
    FOR_ALL_BB_OPs_FWD (REGION_First_BB, op) {
        if (OP_no_move_before_gra (op) && OP_Is_Copy_From_Save_TN (op)) {
            exit_boundary_op = op; break;
        }
    }
    while (exit_boundary_op && 
           (exit_boundary_op = OP_prev(exit_boundary_op)) &&
           OP_no_move_before_gra (exit_boundary_op)) {
    }

    /* find the split point from the through the begining of REGION_First_BB
     * are put into pre-entry block. 
     */
    FOR_ALL_BB_OPs_REV  (REGION_First_BB, op) {
        if (OP_no_move_before_gra (op) && OP_Is_Copy_To_Save_TN (op)) {
            entry_boundary_op = op; 
            break;
        }
    }
    while (entry_boundary_op && 
           (entry_boundary_op = OP_next(entry_boundary_op)) &&
           OP_no_move_before_gra (entry_boundary_op)) {
    }

    if (!entry_boundary_op || !exit_boundary_op) {
        return;
    }

    /* count the number of "useful" instruction which are going to be put 
     * in the sandwiched block.
     */
    INT insn_cnt = 0;
    OP* t = entry_boundary_op;
    for (; t && t != exit_boundary_op; t = OP_next(t)) {
        insn_cnt ++;
    }
    if (t != exit_boundary_op) {
        /* it is possible for empty PU */
        return;
    }

    if (insn_cnt < 4) {
        /* there are few "useful" instruction, don't need to perform prepass global sched, 
         * <t> and <exit_boundary_op> may not meet in case of empty PU.
         */
       return;
    }
         
    RGN_Divide_BB (REGION_First_BB, exit_boundary_op);
    RGN_Divide_BB (REGION_First_BB, OP_prev(entry_boundary_op));
}

  /* =============================================================
   *
   *  Add_Splitted_Entry_Exit_BB 
   *  see the comment of Init_Split_PU_Entry_Or_Exit_BB 
   *
   * =============================================================
   */ 
static void
Add_Splitted_Entry_Exit_BB (BB *bb) {
    
    Is_True (BB_entry (bb) || BB_exit (bb), 
            ("BB:%d is neither entry- nor exit-BB",
             BB_id(bb)));
    Is_True (!BB_Has_Already_Been_Splitted (bb),
            ("BB:%d has already splitted", BB_id(bb)));

    _splitted_pu_boundary_bbs.push_back (bb); 
}

  /* =============================================================
   *
   *  Remove_Splitted_Entry_Exit_BB 
   *  see the comment of Init_Split_PU_Entry_Or_Exit_BB 
   *
   * =============================================================
   */ 
void
Remove_Splitted_Entry_Exit_BB (BB * bb) {
    
    for (BB_VECTOR_ITER iter = _splitted_pu_boundary_bbs.begin () ;
         iter != _splitted_pu_boundary_bbs.end () ; iter ++) {
        
        if (*iter == bb) {
            _splitted_pu_boundary_bbs.erase (iter) ;
            return ;
        }
    }
}

  /* =============================================================
   *
   * Copy_Entry_BB_Annotation 
   * 
   * COPY (rather than move) some essential Annotation from entry
   * block to the splitted temp block 
   *
   * ============================================================
   */
static void
Copy_Entry_BB_Annotation (BB* entry_bb, BB* splitted_bb) {

    for (ANNOTATION * annot = BB_annotations (entry_bb) ; annot ; 
         annot = ANNOT_next (annot)) {

        switch (ANNOT_kind(annot)) {

        case ANNOT_LABEL:
        case ANNOT_ENTRYINFO :
        case ANNOT_EXITINFO :
            break ;

        case ANNOT_PRAGMA : 
        case ANNOT_CALLINFO : 
        case ANNOT_NOTE :
        case ANNOT_LOOPINFO :
        case ANNOT_SWITCH : 
        case ANNOT_ROTATING_KERNEL :
#if defined(TARG_SL)
        case ANNOT_ASMINFO:
#endif
            BB_Add_Annotation (splitted_bb,ANNOT_kind (annot),ANNOT_info(annot));
            break ;

        default :
            Is_True (FALSE , ("unknow annotation \n")) ;
        }
    }
}

    /* ============================================================
     *
     *  Split_PU_Entry_BB 
     * 
     *  split PU's entry block speicified by <entry_bb> into two and 
     *  return the new entry block.
     * 
     * ===========================================================
     */
BB *
Split_PU_Entry_BB (BB * entry_bb) {

    Is_True (BB_entry (entry_bb), 
             ("BB:%d is not entry BB", BB_id(entry_bb)));
    Is_True (!BB_Has_Already_Been_Splitted (entry_bb),
             ("BB:%d has already been splitted", BB_id(entry_bb)));

    OP * last_op, *boundary_op, * sp_adj;
    sp_adj = BB_entry_sp_adj_op (entry_bb);

    for (boundary_op = BB_last_op (entry_bb); 
         boundary_op && boundary_op != sp_adj ; 
         boundary_op = OP_prev(boundary_op)) {

        if (OP_glue(boundary_op)                || 
            OP_no_move_before_gra(boundary_op)  ||
            OP_access_reg_bank(boundary_op)) {
            break ; 
        }
    }

    Is_True (boundary_op , ("boundary_op can't be null\n"));
    if (boundary_op == BB_last_op(entry_bb)) {
        return NULL;
    }


    /* split entry-bb and update {regional|global}-cfg properly 
     */

    REGION *       home_rgn = Home_Region (entry_bb);
    REGIONAL_CFG * rgn_cfg  = home_rgn->Regional_Cfg();

    BB * split_bb = 
        RGN_Gen_And_Insert_BB_After (entry_bb, home_rgn->Regional_Cfg()) ;
    BB_freq (split_bb) = BB_freq (entry_bb);

    BBLIST * nxt, * succ;
    for (succ = BB_succs (entry_bb); succ; succ = nxt) {

        BB* bb_succ = BBLIST_item(succ);
        nxt = BBLIST_next(succ);
        RGN_Link_Pred_Succ_With_Prob(split_bb, bb_succ, BBLIST_prob(succ));
        RGN_Unlink_Pred_Succ(entry_bb, bb_succ);

    }

    RGN_Link_Pred_Succ_With_Prob (entry_bb, split_bb, 1.0f);

    /* move all ops that follows boundary_op to the newly allocated bb 
     */
    while (OP_next(boundary_op)) {

        BB_Move_Op_To_End (split_bb, entry_bb, OP_next(boundary_op)) ;

    }

    /* liveness analysis : share same liveness structure , 
     * inaccurate yet suffice.
     */
    BB_bbregs (split_bb) = BB_bbregs (entry_bb);

    /* set & reset flags for entry- and splitted-bb
     */
    BB_flag (split_bb) = BB_flag (entry_bb) ;
    Reset_BB_entry (split_bb) ;

    Copy_Entry_BB_Annotation (entry_bb,split_bb);

    if (entry_bb) { Add_Splitted_Entry_Exit_BB (entry_bb) ; } 

    return entry_bb ;
}

    /* ==============================================================
     *
     * Split_PU_Entry_BB 
     * 
     * split PU's entry block in <rgn> if any and return new entry 
     * block if splitting is actualy performed. 
     * 
     * ==============================================================
     */
BB *
Split_PU_Entry_BB (REGION * rgn) {

    NODE_VECTOR rgn_entry = rgn->Entries () ;

    Is_True (rgn_entry.size() == 1 , 
        ("Split_PU_Entry_BB() region has more than one entries\n"));
    
    REGIONAL_CFG_NODE * cfg_node = *(rgn_entry.begin());

    BB * entry_bb ;
    
    if (cfg_node->Is_Region() || 
        (entry_bb = cfg_node->BB_Node()) && !BB_entry(entry_bb)) {
        return NULL;
    }

    return !BB_exit(entry_bb) ? Split_PU_Entry_BB (entry_bb) : NULL;
}



    /* =============================================================
     *
     *  Sink_Return_Val_OP 
     * 
     * ============================================================
     */
static void
Sink_Return_Val_OP (BB * split, BB * exit) {
    
    Is_True (BB_exit(exit) && 
             BB_Unique_Predecessor(exit) == split &&
             !BB_xfer_op(split),
             ("BB:%d is not splited from BB:%d",
               BB_id(split), BB_id(exit)));

    if (BB_length(split) <= 0) return ;

    CG_DEP_Compute_Graph (
            split,
            INCLUDE_ASSIGNED_REG_DEPS,
            NON_CYCLIC,
            INCLUDE_MEMREAD_ARCS,
            INCLUDE_MEMIN_ARCS,
            NO_CONTROL_ARCS,
            NULL);

    Reset_BB_scheduled (split) ;
    Reset_BB_scheduled (exit)  ;

    #define OP_Should_Sink(o)       OP_Scheduled((o)) 
    #define Set_OP_Should_Sink(o)   Set_OP_Scheduled((o)) 
    #define Reset_OP_Should_Sink(o) Reset_OP_Scheduled((o))

    OP * op ;
    FOR_ALL_BB_OPs (split, op) {
        Reset_OP_Should_Sink(op);
    }

    FOR_ALL_BB_OPs (split, op) {     
        if (OP_def_return_value(op)) {
            Set_OP_Should_Sink(op);
        }

        if (OP_Should_Sink(op)) {
            for (ARC_LIST * arcs = OP_succs(op) ; 
                 arcs != NULL;
                 arcs = ARC_LIST_rest(arcs)) {

                ARC * arc = ARC_LIST_first(arcs);
                OP  * succ = ARC_succ(arc);
                
                if (OP_bb(succ) == split) {
                    Set_OP_Should_Sink (succ);
                }
            }
        }
    }

    OP * prev_op ;
    for (op = BB_last_op(split); op ; op = prev_op) {

        prev_op = OP_prev(op);

        if (OP_Should_Sink(op)) {
           BB_Move_Op_To_Start (exit, split, op);
            Reset_OP_Should_Sink(op);
        }
    }

    CG_DEP_Delete_Graph (split);

    #undef OP_Should_Sink
    #undef Set_OP_Should_Sink
    #undef Reset_OP_Should_Sink

}

    /* copy annotation from splitted block to exit_bb 
     */
static void
Copy_Exit_BB_Annot (BB * exit_bb, BB * splitted_exit) {

    for (ANNOTATION* ant = BB_annotations (exit_bb) ; 
         ant ; 
         ant = ANNOT_next (ant)) {

        BOOL add_annot ;
        add_annot = TRUE;

        switch (ANNOT_kind(ant)) {
        case ANNOT_LABEL:
            Set_BB_has_label (splitted_exit); break;

        case ANNOT_PRAGMA:
            Set_BB_has_pragma (splitted_exit); break;

        case ANNOT_ENTRYINFO: 
        case ANNOT_EXITINFO:
            add_annot = FALSE; break ;

        case ANNOT_NOTE:
            Set_BB_has_note(splitted_exit); break;

        case ANNOT_LOOPINFO:
            FmtAssert (FALSE, 
                ("Exit block BB:%d should not has loop-info annotation", 
                 BB_id(exit_bb)));
            add_annot = FALSE; break ;

        case ANNOT_CALLINFO:
            Is_True (BB_tail_call(exit_bb), ("it should be a tail call"));
            add_annot = FALSE;
            break;

        case ANNOT_ASMINFO:
            Set_BB_asm(exit_bb); break;

        case ANNOT_SWITCH:

            FmtAssert (FALSE,
                ("Exit block BB:%d should not has switch annotation", 
                  BB_id(exit_bb)));
            add_annot = FALSE; break ;

        case ANNOT_ROTATING_KERNEL:
            FmtAssert (FALSE,
                ("Exit block BB:%d cannot be a rotating kernel", 
                  BB_id(exit_bb)));
            add_annot = FALSE; break ;

        default:
            FmtAssert(FALSE, 
                ("unexpected annotation kind: %d", ANNOT_kind(ant)));
        } /* end of switch */


        if (add_annot) {
            BB_Add_Annotation 
                (splitted_exit, ANNOT_kind(ant), ANNOT_info(ant));
        }
    } /* end of for */
}

    /* ==============================================================
     *
     *  Split_PU_Exit_BB 
     * 
     *  split PU's exit block specified by <epi> into two; and 
     *  return the new exit block if splitting actualy performed, 
     *  NULL otherwise.
     *
     * ==============================================================
     */

BB *
Split_PU_Exit_BB  (BB * epi) {

    Is_True (BB_exit(epi), ("BB:%d is not exit BB", BB_id(epi)));

    Is_True (!BB_Has_Already_Been_Splitted (epi),
             ("BB:%d has already been splitted", BB_id(epi)));

    if (BB_entry(epi)) {
        /* we do not split PU's entry/exit block when such situation
         * occurs: PU has only one block, so exit share common block 
         * with entry
         */
        return NULL; 
    }

    OP * sp_adj = BB_exit_sp_adj_op (epi);
    OP * op_tmp, * op_boundary = NULL;

    for (op_tmp = BB_first_op (epi) ; 
         op_tmp && op_tmp != sp_adj ;
         op_tmp = OP_next(op_tmp)) {

        if (OP_glue(op_tmp)                ||
            OP_no_move_before_gra(op_tmp)  ||
            OP_access_reg_bank(op_tmp)) {

            op_boundary = OP_prev(op_tmp);
            break ;

        }
    }

    if (!op_boundary) { return NULL; }

    /* split entry-bb and update {regional|global}-cfg properly 
     */
    REGION *       home_rgn = Home_Region (epi);
    REGIONAL_CFG * rgn_cfg  = home_rgn->Regional_Cfg();

    BB * split_bb = 
        RGN_Gen_And_Insert_BB_Before (epi, home_rgn->Regional_Cfg());
    BB_freq (split_bb) = BB_freq (epi);

    /* modify the CFG accordingly 
     */
    while (BBLIST * pred_item = BB_preds(epi)) {

        BB* pred_bb = BBLIST_item (pred_item);

        /* We cannot obtain the prob by BBLIST_prob(pred_bb) since 
         * BBLIST_prob is only valid for succ edges
         */
        float prob  = BBLIST_prob (BB_Find_Succ(pred_bb,epi));

        RGN_Link_Pred_Succ_With_Prob(pred_bb, split_bb, prob);
        RGN_Unlink_Pred_Succ(pred_bb,epi);

    } /* end of while(BBLIST *...) */

    RGN_Link_Pred_Succ_With_Prob (split_bb, epi, 1.0f);

    /* move all ops that before <op_boundary> includingly to the 
     * newly allocated bb 
     */

    for (; op_boundary ; op_boundary = op_tmp) {
        op_tmp = OP_prev(op_boundary);
        BB_Move_Op_To_Start (split_bb, epi, op_boundary) ;
    }

    /* Liveness analysis : share common liveness structure , 
     * Inaccurate yet suffice.
     */
    BB_bbregs (split_bb) = BB_bbregs (epi);

    /* set & reset flags for entry- and splitted-bb
     */
    BB_flag (split_bb) = BB_flag (epi) ;
    Reset_BB_exit (split_bb) ;
    Reset_BB_call (split_bb);

    Add_Splitted_Entry_Exit_BB (epi) ; 

    Copy_Exit_BB_Annot (epi, split_bb);

    /* workaround :
     * TODO : finish this workaround comment 
     */
    Sink_Return_Val_OP (split_bb, epi);

    return epi;
}

    /* ============================================================
     *
     * Split_PU_Exit_BB  
     * 
     * if <rgn> contains PU's exit-blocks, split them into two by 
     * invoking Split_PU_Exit_BB(BB*).
     *
     * ref the comment of Split_PU_Exit_BB(BB* epi) for the purpose
     * and approch of splitting & merging PU's entry/exit block.
     *
     * ===========================================================
     */
void
Split_PU_Exit_BB  (REGION * rgn) {

    NODE_VECTOR rgn_exits = rgn->Exits () ;

    for (NODE_VECTOR_ITER iter = rgn_exits.begin () ; 
         iter != rgn_exits.end () ;
         iter ++) {
    
        REGIONAL_CFG_NODE * cfg_node = *iter;
        if (cfg_node->Is_Region()) continue ;

        BB * epi = cfg_node->BB_Node () ;
        if (BB_exit (epi)) {
            epi = Split_PU_Exit_BB (epi); 
        }
    } /* end of for statement */

}

    /* =============================================================
     *
     *  Merge_Splitted_PU_Entry_BB 
     * 
     *  merge the splitted part of entry-BB with entry-BB, whichby, 
     *  obtain the larger BB.  
     *
     * ============================================================
     */
void
Merge_Splitted_PU_Entry_BB (BB * splitted_entry) {

    Is_True (BB_entry (splitted_entry), 
            ("BB:%d is not entry-BB", BB_id(splitted_entry)));

    BB* split = BB_Unique_Successor (splitted_entry) ;
    Is_True (split && !BB_xfer_op(splitted_entry), 
             ("BB:%d is not splitted entry BB %d\n", 
             BB_id(splitted_entry)));


    BBLIST * nxt, *succ;
    for (succ = BB_succs(split); succ; succ = nxt) {

        BB* bb_succ = BBLIST_item(succ);
        nxt = BBLIST_next(succ);
        RGN_Link_Pred_Succ_With_Prob (splitted_entry, bb_succ, 
                                      BBLIST_prob(succ));
        RGN_Unlink_Pred_Succ(split, bb_succ);

    }

    RGN_Unlink_Pred_Succ (splitted_entry, split);

    /* mv ops */
    BB_Append_All (splitted_entry, split);
    BB_bbregs (split) = NULL ;
          
    if (BB_call (splitted_entry)) {

        OP * op = BB_xfer_op (splitted_entry) ;

        if (!op || !OP_call (op)) {

            Reset_BB_call (splitted_entry) ;

            ANNOTATION * bb_annot = BB_annotations (splitted_entry) ;
            ANNOTATION * call_annot ; 

            if (bb_annot && 
                (call_annot = ANNOT_Get (bb_annot,ANNOT_CALLINFO))) 
                ANNOT_Unlink (bb_annot,call_annot) ;

        }
    }

    SCHEDULER::Clean_Up (splitted_entry);
    RGN_Remove_BB_And_Edges (split);

    Remove_Splitted_Entry_Exit_BB (splitted_entry);
}

    /* ============================================================
     *
     * Merge_Splitted_PU_Exit_BB  
     *
     * merge the splitted PU exit BB into one
     * 
     * ============================================================
     */
void
Merge_Splitted_PU_Exit_BB  (BB * exit_block) {

    Is_True (BB_exit (exit_block), 
            ("BB:%d is not exit-block", BB_id(exit_block)));

    BB* split = BB_Unique_Predecessor (exit_block);
    Is_True (split && !BB_xfer_op(split), 
             ("BB:%d is not splitted exit BB %d\n", BB_id(exit_block)));

    while (BBLIST * pred_item = BB_preds(split)) {

        BB* p = BBLIST_item(pred_item);
        float prob = BBLIST_prob (BB_Find_Succ(p,split));

        RGN_Link_Pred_Succ_With_Prob (p, exit_block, prob);
        RGN_Unlink_Pred_Succ(p,split);
    }

    RGN_Unlink_Pred_Succ (split, exit_block);

    /* mv ops */
    BB_Prepend_All (exit_block, split);
    BB_bbregs (split) = NULL ;
          
    SCHEDULER::Clean_Up (exit_block);

    RGN_Remove_BB_And_Edges (split);

    Remove_Splitted_Entry_Exit_BB (exit_block);
}


    /* ==============================================================
     *
     *   Merge_All_Splitted_Entry_and_Exit_BB 
     * 
     *  merge all splitted entry- exit- BB back to where it orginally
     *  be carved from. 
     *
     * ===============================================================
     */
void
Merge_All_Splitted_Entry_and_Exit_BB (void) {
    
    while (!_splitted_pu_boundary_bbs.empty ()) {

        /* get the last element 
         */
        BB * bb = _splitted_pu_boundary_bbs.back () ;
        if (BB_entry(bb)) {
            Merge_Splitted_PU_Entry_BB (bb) ;
        } else if (BB_exit(bb)) {
            Merge_Splitted_PU_Exit_BB (bb);
        } else {
            FmtAssert (FALSE,
                ("BB:%d is neither entry- nor exit-BB\n", BB_id(bb)));
        }
        
        Is_True (_splitted_pu_boundary_bbs.empty () ||
                 bb != _splitted_pu_boundary_bbs.back (),

                 ("Merge_Splitted_PU_{Entry|Exit}_BB has not erase"
                  " BB:%d from _splitted_pu_boundary_bbs", 
                  BB_id (bb))) ;
    }
}



    /* =================================================================
     * =================================================================
     *
     *          Dominator and Postdominator info 
     *
     * =================================================================
     * =================================================================
     */
void 
Calculate_Dominator_Info (REGION_TREE *rgn_tree) {

    Calculate_Dominators () ;

    BS* empty_BB_set = BS_Create_Empty (PU_BB_Count + 2, 
                                         &MEM_phase_pool);

    extern BOOL Is_Abnormal_Loop (REGION* region) ;

    typedef std::queue<REGION * >  _RGN_QUEUE;
    _RGN_QUEUE  rgn_queue ;

    #define SET_RGN_IN_ABNORMAL_LOOP(x) { x = (REGION*)((INTPTR)(x) | 1); }
    #define GET_RGN(x)                  ((REGION*)(((INTPTR)(x)) & ~1))
    #define RGN_IN_ABNORMAL_LOOP(x)     ((INTPTR)(x) & 1)

    REGION  * rgn = rgn_tree->Root();
    if (Is_Abnormal_Loop (rgn)) {
        SET_RGN_IN_ABNORMAL_LOOP(rgn);
    }

    rgn_queue.push (rgn);
    

    while (!rgn_queue.empty ()) {

        REGION * tmp = rgn_queue.front();
        rgn_queue.pop () ;

        if (RGN_IN_ABNORMAL_LOOP(tmp)) {

            for (TOPOLOGICAL_REGIONAL_CFG_ITER 
                 iter ((GET_RGN(tmp))->Regional_Cfg()); 
                 iter != 0; ++ iter) {
                
                if ((*iter)->Is_Region ()) continue ;

                BB * b = (*iter)->BB_Node ();

                    /* dominate-set is correct even in abnormal loop
                     */
                //Set_BB_dom_set  (b, empty_BB_set); 
                Set_BB_pdom_set (b, empty_BB_set);
            }
        }

        for (REGION_KID_ITER kids_iter(GET_RGN(tmp)) ; 
             kids_iter != 0 ; ++kids_iter) {
            
            REGION * nested_rgn = *kids_iter ;
            if (Is_Abnormal_Loop (nested_rgn)) {
                SET_RGN_IN_ABNORMAL_LOOP(nested_rgn);
            }
            
            rgn_queue.push (nested_rgn);
        }
    } /* end of while */
}

BOOL
BB1_Postdominate_BB2 (BB * bb1, BB* bb2) {
    BOOL b = BB_SET_MemberP (BB_pdom_set(bb2), bb1);
    if (!b) {
        if (bb1 == bb2) return TRUE;     
        for (BB* bb = BB_Unique_Successor(bb2); 
             bb != NULL; 
             bb = BB_Unique_Successor (bb)) {
            if (bb == bb1) { return TRUE; }
        }
    }

    return b;
}

    /* ============================================================
     *
     * BB_Pos_Analysis 
     *
     * analysis (control flow) relationship between <cutting_set>
     * and <bb>.
     *
     * ============================================================ 
     */
BB_POS 
BB_Pos_Analysis 
    (BB* bb, BB_VECTOR* cutting_set, RGN_CFLOW_MGR * cflow_info) {

    for (BB_VECTOR_CONST_ITER iter = cutting_set->begin () ;
         iter != cutting_set->end() ; 
         iter++) {
        
        BB * tmp = *iter ;
        if (tmp == bb) return IN_SISS ;
        
        if (cflow_info->BB1_Reachable_From_BB2 (tmp,bb)) {
            return ABOVE_SISS ;
        }

        if (cflow_info->BB1_Reachable_From_BB2 (bb,tmp)) {
            return BELOW_SISS;
        }
    }
    
    return OTHER;
}

 /* ============================================================
  *
  *  Dump
  *
  *  Dump RGN_CFLOW_MGR class's status 
  *
  * ============================================================
  */
void
RGN_CFLOW_MGR::Dump (FILE *f, BOOL verbose) {

    if (verbose) {
        fprintf (f, "%s\t\tRGN_CFLOW_MGR status\n%s\n", DBar, DBar); 
    }

    if (!Valid ()) {
        if (verbose) { fputs ("Invalid!\n",f); }
        return ;
    }

    char * prompt = "BBS REACHABLE FROM " ;
    INT num = 0, prompt_len = strlen(prompt);

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_scope->Regional_Cfg());
        iter != 0 ; ++iter) {
        
        BS * reach_info = _reach_info_vect (*iter);
        Is_True (reach_info,("fail to get reachable-BB"));

        fprintf (f, "%s\t\t%s:%3d min-level %3d max-leve %3d\n", 
                    SBar, 
                    (*iter)->Is_Region () ? "RGN" : "BB",
                    (*iter)->Is_Region () ? (*iter)->Region_Node()->Id() :
                                             BB_id((*iter)->BB_Node()),
                    Min_Level(*iter), 
                    Max_Level(*iter)) ;

        if (!(*iter)->Succ_Num()) { 
            fputc ('\n', f);
        } else {
            fputs ("reachable nodes:", f);
        }

        BS_ELT  elt = BS_Choose(reach_info);
        if (elt == BS_CHOOSE_FAILURE ) {
            continue ;
        }

        INT elem_count = -1; 
        do {
            ++ elem_count ; if (!(elem_count % 5)) fprintf (f,"\n");

            BB * bb = _map_idx_2_bb ((INT32)elt);
            mBB_NUM	bb_id = BB_id (bb);

            fprintf (f, "BB:%3d(p:%4.2f) ", (INT32)(BS_ELT)bb_id, 
                    (float)(_bb_reach_prob 
                        (*iter,bb)/(float)REACH_PROB_SCALE)); 
                                
        } while ((elt = BS_Choose_Next(reach_info,elt)) != BS_CHOOSE_FAILURE); 

        fputc ('\n', f);
    }
}

#ifdef Is_True_On 
void
RGN_CFLOW_MGR :: gdb_dump (void) {
    Dump (stderr); fflush (stderr);    
}
#endif /* Is_True_On */
    /* =============================================================
     *
     * Workaround_Dom_Info_For_In_Abnormal_Loop_Rgn 
     *
     * ref the header file for details
     *
     * =============================================================
     */
void
Workaround_Dom_Info_For_In_Abnormal_Loop_Rgn (REGION * r) {

    BS* empty_BB_set = BS_Create_Empty (1, &MEM_phase_pool);

    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter (r->Regional_Cfg ()) ; 
        iter != 0 ; ++iter) {

        if ((*iter)->Is_Region ()) { 
            continue ;
        }

        BB* b = (*iter)->BB_Node () ;
        Set_BB_dom_set  (b,empty_BB_set);
        Set_BB_pdom_set (b, empty_BB_set);
    }
}
