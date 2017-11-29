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
  * Module: sched_seq.h
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:23 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_seq.h,v $
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

#ifndef sched_seq_INCLUDED
#define sched_seq_INCLUDED

#include "sched_util.h"


    /* ========================================================
     * ========================================================
     *
     *  Declairation of SCHED_SEQ (base class 
     *
     * ========================================================
     * ========================================================
     */
class SCHED_SEQ {

protected:

    BOOL Qualified (REGIONAL_CFG_NODE *) ;

    NODE_VECTOR _candidates; 
    REGION* _rgn ;
    BB*     _cur ;
    
    MEM_POOL* _mp;

    BOOL Node1_Is_Sparser (REGIONAL_CFG_NODE*, REGIONAL_CFG_NODE *); 

public: 

    SCHED_SEQ (REGION *, MEM_POOL*) ;
    ~SCHED_SEQ (void) {} ;

    BB* First (void) { return NULL; }
    BB* Next (void)  { return NULL; }
    BB* Cur (void) { Is_True (_cur, ("no CURRENT BB node!")) ;  return _cur ; }
};


    /* ==========================================================
     * ==========================================================
     *
     *         declaition of TOPDOWN_SCHED_SEQ 
     *
     * ==========================================================
     * ==========================================================
     */
class TOPDOWN_SCHED_SEQ : public SCHED_SEQ {
private:

    class NODE_INFO { public: INT32 _n_pred; NODE_INFO() {_n_pred = 0;} };
    typedef mempool_allocator<pair<REGIONAL_CFG_NODE*,NODE_INFO> > 
  		    NODE_INFO_ALLOC;
    template <class _Ptr_Tp>
    struct ptr_hash {
        size_t operator()(_Ptr_Tp __x) const { return UINT(__x); }
    };

#if (__GNUC__ == 3)
    typedef __gnu_cxx::hash_map<REGIONAL_CFG_NODE*, NODE_INFO,
#else
    typedef hash_map<REGIONAL_CFG_NODE*, NODE_INFO,
#endif // __GNUC__ == 3
                 ptr_hash<REGIONAL_CFG_NODE*>,
                 std::equal_to<REGIONAL_CFG_NODE*>, NODE_INFO_ALLOC>  
				 NODE_INFO_MAP;

    REGIONAL_CFG_NODE * next_node (void) ;

        /* _node_info_map:  f: rgn_node -> <has-not-sched-pred-num> 
         */
    NODE_INFO_MAP  _node_info_map;  

public: 

    TOPDOWN_SCHED_SEQ (REGION *, MEM_POOL*) ;
    ~TOPDOWN_SCHED_SEQ (void) {} ;

    BB* First (void) ;
    BB* Next (void) ;
};

    /* =============================================================
     * =============================================================
     *
     *      Declaition of DEEPDOWN_SCHED_SEQ 
     *
     * ============================================================
     * ===========================================================
     */
class DEEPDOWN_SCHED_SEQ : public SCHED_SEQ {
private:
    void Delete_And_Push_Succ (REGIONAL_CFG_NODE*);

public:
    DEEPDOWN_SCHED_SEQ (REGION* , MEM_POOL*);
    ~DEEPDOWN_SCHED_SEQ (void) {} ;

    BB* First (void);
    BB* Next  (void);
};

 #endif /* sched_seq_INCLUDED */
