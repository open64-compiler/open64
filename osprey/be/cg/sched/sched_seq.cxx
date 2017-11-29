/*
 *  Copyright (C) 2000-2003, Intel Corporation
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

 /* =========================================================================
  * =========================================================================
  * 
  * Module: sched_seq.cxx
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:22 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_seq.cxx,v $
  *
  * Revision comments:
  *
  *     28-July-2002 - initial version
  *
  * Description:
  * ============
  *
  * ==========================================================================
  * ==========================================================================
  */

#include "sched_seq.h"


SCHED_SEQ :: SCHED_SEQ (REGION* r, MEM_POOL* mp) : 
    _candidates (mp) {
    _rgn = r ;
    _mp  = mp;
    _cur = NULL;
}

BOOL
SCHED_SEQ :: Qualified (REGIONAL_CFG_NODE *nd) {
    if (nd->Is_Region()) return FALSE;

    BB* bb = nd->BB_Node () ;

    if (BB_Is_Isolated_From_Sched (bb)) {
        return FALSE;
    }

    return !BB_entry(bb) && !BB_exit(bb);
}

BOOL
SCHED_SEQ :: Node1_Is_Sparser 
    (REGIONAL_CFG_NODE *nd1, REGIONAL_CFG_NODE *nd2) {

        /* Favor BB with more bubbles over less one, we assume that 
         * BB with more OPs is sparer than less one.
         */
    INT32 density1 = nd1->Is_Region () ? 0 : BB_length(nd1->BB_Node());
    INT32 density2 = nd2->Is_Region () ? 0 : BB_length(nd2->BB_Node());
            
    return density1 > density2 ;
}

    /* ========================================================
     * ========================================================
     *
     *      Implementation of TOPDOWN_SCHED_SEQ 
     *
     * ========================================================
     * ========================================================
     */


REGIONAL_CFG_NODE* 
TOPDOWN_SCHED_SEQ::next_node (void) {

    float freq = -1.0f ;
    REGIONAL_CFG_NODE * best = NULL;
    INT32  index = -1;

    INT32 root_num = _candidates.size();
    for (INT32 idx = root_num - 1 ; idx >= 0 ; idx--) {

        REGIONAL_CFG_NODE* cand = _candidates[idx] ;
        float node_freq = cand->Home_Region ()-> Regional_Cfg ()-> 
                          Node_Freq (cand);
        float deviation = node_freq/200.0 ;

        if (!best || (node_freq - deviation) > freq || 
            node_freq + deviation > freq && 
            Node1_Is_Sparser (cand,best)) {
            best = cand ; 
            freq = node_freq ;
            index = idx ;
        }
    }

    if (!best) return NULL;

    if (index + 1 != root_num) {
        _candidates[index] = _candidates[root_num - 1];
    }

    _candidates.resize(--root_num);

    for (CFG_SUCC_NODE_ITER succs(best) ; succs != 0 ; ++succs) {
        if (! --_node_info_map[*succs]._n_pred) {
            _candidates.push_back(*succs); 
        }
    }

    return best ; 
}

TOPDOWN_SCHED_SEQ::TOPDOWN_SCHED_SEQ (REGION *rgn, MEM_POOL *mp) :
    SCHED_SEQ (rgn, mp) {

        /* initialize the pred num 
         */
    for (TOPOLOGICAL_REGIONAL_CFG_ITER iter(_rgn->Regional_Cfg());
        iter != 0; ++iter) {
        _node_info_map[*iter]._n_pred = (*iter)->Pred_Num();
    }
}

BB*
TOPDOWN_SCHED_SEQ :: First (void) {

    _candidates.clear () ;
    _cur = NULL ;
    _candidates.push_back (_rgn->Entries()[0]);

    return Next () ;
}

BB*
TOPDOWN_SCHED_SEQ :: Next (void) {

    while (REGIONAL_CFG_NODE * nd = next_node ()) {
       if (Qualified(nd)) return _cur = nd->BB_Node();
    }

    return _cur = NULL;
}

    /* =============================================================
     * =============================================================
     *
     *          Implementation of DEEPDOWN_SCHED_SEQ
     *
     * ============================================================
     * ============================================================
     */
DEEPDOWN_SCHED_SEQ :: DEEPDOWN_SCHED_SEQ (REGION* r, MEM_POOL* mp) : 
    SCHED_SEQ (r, mp) {
}

BB*
DEEPDOWN_SCHED_SEQ :: First (void) {

    _candidates.clear () ;
    _cur = NULL ;
    _candidates.push_back (_rgn->Entries()[0]);

    return Next () ;
}

BB*
DEEPDOWN_SCHED_SEQ :: Next (void) {

    INT nd_idx;

    float most_freq = -1.0f;
    REGIONAL_CFG_NODE* n, *most_freq_n;
    INT   idx = -1;

    for (nd_idx = _candidates.size () - 1; 
         nd_idx >= 0 ; 
         nd_idx --) {

        n = _candidates[nd_idx];

        float node_freq = n->Home_Region ()-> Regional_Cfg ()-> 
                          Node_Freq (n);

        Is_True (node_freq >= 0.0f, ("Negative frequency"));

        #define FREQ_DEVATION (0.1f)
        float u = node_freq * (1 + FREQ_DEVATION);
        float l = node_freq * (1 - FREQ_DEVATION);
        #undef FREQ_DEVATION

        if (u < most_freq) { continue; } 
        if (l > most_freq || 
            Node1_Is_Sparser (_candidates[nd_idx], _candidates[idx])) {
            idx = nd_idx; 
            most_freq = node_freq;
        }
    }

    if (idx == -1) return NULL;
    most_freq_n = _candidates[idx];

        /* delete this node 
         */
    _candidates[idx] = _candidates[_candidates.size () - 1];
    _candidates.resize (_candidates.size () - 1);

        /* push the immediate succs 
         */

    for (CFG_SUCC_NODE_ITER iter(most_freq_n); iter != 0; ++iter) {
        if (find(_candidates.begin (), _candidates.end (), *iter) ==
             _candidates.end ()) {

           _candidates.push_back (*iter);
        }
    }

    if (n->Is_Region () || !Qualified(most_freq_n)) {
        return Next ();        
    }

    return _cur = most_freq_n->BB_Node ();
}


