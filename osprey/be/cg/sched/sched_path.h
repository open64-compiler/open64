/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

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
  * Module: sched_path.h
  * $Revision: 1.1 $ 
  * $Date: 2005/12/30 01:50:23 $
  * $Author: weitang $ 
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_path.h,v $
  *
  * Description:
  * ===========
  *
  *     this module is dedicated to acquire maintain and query execution 
  *     path.
  *
  * =========================================================================
  * =========================================================================
  */

#ifndef sched_path_INCLUDED
#define sched_path_INCLUDED

#include <list> 
#include <vector> 
using std::vector;

#include "ipfec_defs.h"

#include "tracing.h"
#include "bb.h"
#include "region.h"

#include "sched_util.h"


/* EXEC_PATH_ID stuff */
typedef INT32 EXEC_PATH_ID;
#define EXEC_PATH_MAX_ID (0x7fffffff)
#define INVALID_EXEC_PATH_ID        (EXEC_PATH_ID(-1))
#define EXEC_PATH_ID_IS_INVALID(x)  ((x) < 0 || (x) > EXEC_PATH_MAX_ID)
#define ASSIGN_INVALID_EXEC_PATH_ID(x)  ((x) = -1)

    /* =====================================================
     * =====================================================
     *
     *      class EXEC_PATH definition
     *
     * ====================================================
     * ====================================================
     */

    /* little class to describe each node in path, this data structure
     * is used only by EXEC_PATH
     */
class PATH_NODE_INFO {
private:
    REGIONAL_CFG_NODE * _node ; 
    BB     * _bb_node ; 
    REGION * _rgn_node ;

    REGIONAL_CFG_EDGE * _incoming_edge;
    float  _incoming_edge_prob ;

    float  _prob ; /* from root node toward this node's exec prob */

public:
    REGIONAL_CFG_NODE* Node (void) const { return _node ; }
    REGIONAL_CFG_EDGE* Incoming_Edge (void) const {
                            return _incoming_edge;
                       }
    float Incoming_Edge_prob (void) const {
                return _incoming_edge ? 
                       _incoming_edge -> Prob () : 0.0f;
          }

    BOOL Associated_With_This_Node (BB* b) const {
            return _bb_node == b;
         }

    BOOL Associated_With_This_Node (REGION * r) const {
            return _rgn_node == r;
         }

    BOOL Associated_With_This_Node (REGIONAL_CFG_NODE *n) const {
            return n == _node ;
         }

    float Prob_From_Root (void) const { return _prob ; }

    PATH_NODE_INFO (REGIONAL_CFG_NODE *n, 
                    REGIONAL_CFG_EDGE * incoming_edge, 
                    float prob) {

        _node = n ;
        _incoming_edge = incoming_edge;

        _prob = prob;

        if (n->Is_Region ()) {
            _rgn_node = n->Region_Node ();
            _bb_node  = NULL;
        } else {
            _bb_node  = n->BB_Node ();
            _rgn_node = NULL;
        }
            
    }

    ~PATH_NODE_INFO (void) { /* do nothing */ }

}; /* end of tagPATH_NODE_INFO */

typedef mempool_allocator<PATH_NODE_INFO>      PATH_NODE_ALLOC; 
typedef vector<PATH_NODE_INFO,PATH_NODE_ALLOC> PATH_NODE_INFO_VECT;
typedef PATH_NODE_INFO_VECT::iterator          PATH_NODE_INFO_VECT_ITER;
typedef PATH_NODE_INFO_VECT::const_iterator   
        PATH_NODE_INFO_VECT_CONST_ITER;


class EXEC_PATH {

friend class EXEC_PATH_MGR ;

private:

        /* Each node of this path are stored in <_path_node_info> 
         * in order  
         */
    PATH_NODE_INFO_VECT _path_node_seq;

        /* We use the hash table to speed query " given a node, 
         * which PATH_NODE_INFO_VECT (in <_path_node_seq>) is 
         * associated with it?". 
         */
    enum { EP_HASH_BUCKET_NUM = 23,}; /* make sure it is a prime */
    enum { INVALID_NODE_INFO_IDX = -1,};

    struct EP_HASH_BUCKET {
        INT32 node_idx ; /* the index to  _path_node_seq */
        struct EP_HASH_BUCKET * next ;
        EP_HASH_BUCKET (void) {
            node_idx = INVALID_NODE_INFO_IDX ; next = NULL; 
        }
    };

    EXEC_PATH::EP_HASH_BUCKET  _hash_tab[EP_HASH_BUCKET_NUM] ;

    inline INT32 Hash_Key (BB * b) { return BB_id(b) % EP_HASH_BUCKET_NUM ; } 
    inline INT32 Hash_Key (REGION *r) { return r->Id() % EP_HASH_BUCKET_NUM;}
    inline INT32 Hash_Key (REGIONAL_CFG_NODE* n) {
                                return n->Is_Region () ? 
                                       Hash_Key (n->Region_Node ()) :
                                       Hash_Key (n->BB_Node ());
                          } 
    inline void  Add_Hash (REGIONAL_CFG_NODE *n, INT32 idx_2_node_info) {

                    INT32 hk = Hash_Key (n);
                    if (_hash_tab[hk].node_idx == INVALID_NODE_INFO_IDX) {
                        _hash_tab[hk].node_idx = idx_2_node_info;
                        _hash_tab[hk].next     = NULL;
                    } else {
                        EXEC_PATH::EP_HASH_BUCKET * p = &_hash_tab[hk];
                        while (p->next) { p = p->next ; }
                        p -> next = CXX_NEW (EXEC_PATH::EP_HASH_BUCKET, _mp);
                        p -> next -> node_idx = idx_2_node_info ;
                        p -> next -> next     = NULL;
                    }
                 }

    inline INT32 Get_Path_Node_Info_Idx (REGIONAL_CFG_NODE *n) {

           INT32 hk = Hash_Key (n); 
           INT32 idx;
           while ((idx = _hash_tab[hk].node_idx) != 
                   INVALID_NODE_INFO_IDX) {

              if (_path_node_seq[idx].Associated_With_This_Node(n)) {
                 return idx;
              }
           }/* while */
           return INVALID_NODE_INFO_IDX;
       }
    
    inline void Setup_Hash (void) {
                for (INT i = _path_node_seq.size() - 1 ; i >= 0; i--) {
                    Add_Hash (_path_node_seq[i].Node (), i);
                }
            }

    MEM_POOL *      _mp ;
    EXEC_PATH_ID    _id ;

    mBOOL  _has_call; /* there are call along the path */
    mBOOL  _has_nested_rgn; /* there are nested rgn along the path */

    INT16  _bb_num;   /* this many BB node along the path */
    INT16  _nested_rgn_num; /* this many nested rgn along the path */

    void  Append_Path_Segment 
        (REGIONAL_CFG_EDGE *incoming_edge, REGIONAL_CFG_NODE *node);

    void  Set_Has_Call       (void) { _has_call = TRUE ; }
    void  Set_Has_Nested_Rgn (void) { _has_nested_rgn = TRUE; }

    INT16 Inc_Path_BB_Num         (void) { return ++_bb_num ; }
    INT16 Inc_Path_Nested_Rgn_Num (void) { return ++_nested_rgn_num; }

public:
    
        /* constructor & destructor 
         */
    EXEC_PATH  (EXEC_PATH_ID id, MEM_POOL* mp) ;
    EXEC_PATH  (const EXEC_PATH& ep, MEM_POOL *mp) ;

    ~EXEC_PATH (void) { /* do nothing */ }
    EXEC_PATH& operator = (const EXEC_PATH& ep);

        /* query internal state 
         */ 
    EXEC_PATH_ID Id (void) const { return _id ; }
    INT32  Path_Len (void) const { return _path_node_seq.size() ; }

    INT16 BB_Node_In_Total  (void) const { return _bb_num ;         }
    INT16 Rgn_Node_In_Total (void) const { return _nested_rgn_num ; }

    BOOL Path_Has_Call       (void) const { return _has_call ;      }
    BOOL Path_Has_Nested_Rgn (void) const { return _has_nested_rgn; }

    INT32 Node_Index (REGIONAL_CFG_NODE *n) {
                return Get_Path_Node_Info_Idx (n);
          }

           /* check to see whether node is in this path
            */
    BOOL Node_In_Path (BB *b) {
            for (PATH_NODE_INFO_VECT_ITER iter = _path_node_seq.begin ();
                 iter != _path_node_seq.end (); iter++) {

                if ((*iter).Associated_With_This_Node(b)) {
                    return TRUE;
                }
            }
            return FALSE ;
         }

    BOOL Node_In_Path (REGION *r) {
            for (PATH_NODE_INFO_VECT_ITER iter = _path_node_seq.begin ();
                 iter != _path_node_seq.end (); iter++) {

                if ((*iter).Associated_With_This_Node(r)) {
                    return TRUE;
                }
            }
            return FALSE ;
         }


        /* debug and tracing 
         */
    void Dump (FILE *f=stderr);
    #ifdef Is_True_On
    void gdb_dump (void);
    #endif 
};

    /* EXEC_PATH vector as well as its iterator 
     */
typedef mempool_allocator<EXEC_PATH*>       EXEC_PATH_ALLOC;
typedef vector<EXEC_PATH*,EXEC_PATH_ALLOC>  EXEC_PATH_VECTOR;
typedef EXEC_PATH_VECTOR::iterator          EXEC_PATH_VECT_ITER;
typedef EXEC_PATH_VECTOR::const_iterator    EXEC_PATH_VECT_CONST_ITER;

    /* =====================================================
     * =====================================================
     *
     * class EXEC_PATH definition
     *
     * ====================================================
     * ====================================================
     */
class EXEC_PATH_SET {

#ifdef UINT64
    typedef UINT64 BV_WORD;
    enum { BV_WORD_SIZE = 64,     }; 
    enum { BV_WORD_SIZE_LOG_2 = 6,};
#else 
    typedef UINT32 BV_WORD;
    enum { BV_WORD_SIZE = 32,     };
    enum { BV_WORD_SIZE_LOG_2 = 5,};
#endif

        /* The following two routines are used to wordaround 
         * gcc (v2.95.2)'s bug:
         *
         *     assume x is 32-bit non-zero variable (rather than 
         *     constant) y is integer whose value >= 32. x >> y 
         *     still equal to x.
         */
    BV_WORD  BV_WORD_SHIFT_LEFT (BV_WORD w, UINT32 shift_l_by) {
                return  (shift_l_by < BV_WORD_SIZE) ? 
                             (w << shift_l_by) : 0;
             }

    BV_WORD  BV_WORD_SHIFT_RIGHT (BV_WORD w, UINT32 shift_r_by) {
                return (shift_r_by < BV_WORD_SIZE) ? 
                        (w >> shift_r_by) : 0 ;
             }

        /* BV_WORD bitwise operations
         */
    BOOL    BV_WORD_Test_Bit (BV_WORD w, UINT32 pos) {
                return (pos < BV_WORD_SIZE) ? 
                        (w & (1 << pos)) : FALSE;
            }
    
    BV_WORD BV_WORD_Set_Bit (BV_WORD& w, UINT32 pos) {
                return pos < BV_WORD_SIZE ? (w |= (1 << pos)) : w;
            }

    BV_WORD BV_WORD_Xor_Bit (BV_WORD& w, UINT pos) {
                return pos < BV_WORD_SIZE ? (w ^= (1 << pos)) : w; 
            }

    BV_WORD BV_WORD_Clear_Bit (BV_WORD& w, UINT pos) {
                return pos < BV_WORD_SIZE ? (w &= ~(1 << pos)) : w ;
            }

private:

    BV_WORD * _bv;   /* bit-vector. each bit represent a EXEC_PATH. */
    BV_WORD _bv_words[4]; /* we have never encounted a region 
                       * which has more than 32 * 4 paths, so we 
                       * preallocate 32(or 64) *4 paths to obviate
                       * the need of allocating little block from
                       * <_mp> for most REGIONs. And, because 
                       * <_bv> and <_bv_words> are <_bv_words> 
                       * so close, that they are more likely 
                       * to be put in same cache line. 
                       */
    BV_WORD _msw_mask; /* most-significant-word's bit-mask */
    mINT32 _bv_words_num; /* <_bv> has this many BV_WORD */
    mINT32 _size;     /* this set handle this many EXEC_PATH_IDs */

    MEM_POOL * _mp ; 

        /* Check EXEC_PATH_ID's validity, return FALSE if 
         * path_id is invalid, TRUE, otherwise. if 
         * <abort...invalid> is set, abort program's execution
         * if we encount an invalid path_id.
         */
    BOOL Path_Id_Is_Valid (EXEC_PATH_ID path_id, 
                           BOOL abort_exec_if_invalid = TRUE) {
                                    
                if (EXEC_PATH_ID_IS_INVALID(path_id) || 
                    path_id >= _size) {

                    if (abort_exec_if_invalid) {
                        Fail_FmtAssertion ("Invalid path_id %d", path_id);
                    }
                
                    return FALSE ;
                }

                return TRUE;
            }

    BOOL Equal_Size (const EXEC_PATH_SET& eps,
                     BOOL  abort_exec_if_invalid = TRUE) { 
                
                if (_size != eps.Size ()) {
                    if (abort_exec_if_invalid) {
                        Fail_FmtAssertion 
                            ("two sets' size do not match(%d vs. %d)",
                             _size, eps.Size ());
                    }
                    return FALSE;
                }

                return TRUE;
            }
           
           /* Determine which bit represent <path_id>. 
            * Upon returning <bitpos>th significant bit of <_bv>[word_idx]
            * is exactly the bit corresponding to <path_id>.
            */
    void WordIdx_BitPos (EXEC_PATH_ID path_id, 
                         INT32& word_idx, INT32& bitpos) {
                word_idx = path_id >> BV_WORD_SIZE_LOG_2;
                bitpos   = path_id % BV_WORD_SIZE ;
            }

        /* bitwise & operation : from interger perspective 
         */
    EXEC_PATH_SET& operator &  (const EXEC_PATH_SET& eps);
    void           operator &= (const EXEC_PATH_SET& eps);

        /* bitwise shift-left/right operation: from integer perspective
         */
    EXEC_PATH_SET& operator <<  (const UINT16 shift_l_by);
    void           operator <<= (const UINT16 shift_l_by);
    EXEC_PATH_SET& operator >>  (const UINT16 shift_r_by);
    void           operator >>= (const UINT16 shift_r_by);

        /* bitwise not(~) operation : from interger perspective 
         */
    EXEC_PATH_SET& operator ~ (void) ;
    void Bitwise_Not (void) ;/* (*this-set) = ~(*this-set) */

        /* bitwise union operation 
         */ 
    EXEC_PATH_SET& operator |  (const EXEC_PATH_SET& eps) ;
    void operator |= (const EXEC_PATH_SET& eps) ;

        /* check to see whether there are <length> number of 
         * continguous path-id begins with <begin_path_id> (
         * including <begin_path_id> itself.
         */
    BOOL There_Are_Continguous_Path_Id 
        (EXEC_PATH_ID begin_path_id, INT32 length);

        /* return a BV_WORD with contiguous 
         *  (BV_WORD_SIZE - middle_one_num - low_zero_num) number of 0-bit,
         *  followed by <middle_one_num> number of 1-bit and then followed
         *  by <low_zero_num> number of 0-bits.
         */  
    BV_WORD Pattern_Word (UINT32 middle_one_num, UINT32 low_zero_num) {

                Is_True (low_zero_num + middle_one_num <= BV_WORD_SIZE,
                         ("low_zero_num(%d) + middle_one_num(%d) >"
                          " BV_WORD_SIZE", 
                          low_zero_num, middle_one_num));

                return (middle_one_num != BV_WORD_SIZE) ? 
                        (((1 << middle_one_num) - 1) << low_zero_num) : 
                        BV_WORD(-1); 
            }
public:

    EXEC_PATH_SET (MEM_POOL *mp, mINT32 size=128);
    EXEC_PATH_SET& operator = (const EXEC_PATH_SET& eps);
    EXEC_PATH_SET (EXEC_PATH_SET& ps, MEM_POOL *mp) {
                    _mp = mp ; *this = ps ;
                  }
    void Clear (void) { for (mINT32 i = _bv_words_num - 1; i >= 0 ;i--) {
                            _bv[i] = 0; 
                        }
                      }

    BOOL Is_Empty (void) const { for (mINT32 i=_bv_words_num - 1; i >= 0; i--) {
                                    if (_bv[i]) return FALSE;
                                 }
                                 return TRUE;
                               }

        /* Size () :   query this set can handle how many path.
         * Resize () : change the capacity of this set.
         */
    INT32 Size (void) const         { return _size ; }
    void Resize (INT32 new_size) ;  /* change the size */ 


        /* check to see whether <path> is a member of this set
         */
    BOOL  Is_Member (EXEC_PATH_ID path) ;

        /* add/del one EXEC_PATH_ID into/from set 
         */
    void Add_Path_Id (const EXEC_PATH_ID id) ;
    void Del_Path_Id (const EXEC_PATH_ID id) ;

        /* union operation 
         */
    EXEC_PATH_SET& operator + (const EXEC_PATH_SET& eps) {
                              return (*this) | eps ;
                           }

    void operator += (const EXEC_PATH_SET& eps) { (*this) |= eps ; }
    void Union_Range_Inclusively (EXEC_PATH_ID from, EXEC_PATH_ID to);
    void Union_Partioned_Path_Set 
            (EXEC_PATH_SET& eps, INT32 pred_path_num, 
             INT32 succ_path_num,INT32 base);

        /* diff operation 
         */
    EXEC_PATH_SET& operator -  (const EXEC_PATH_SET& eps) ;
    void           operator -= (const EXEC_PATH_SET& eps) ;
    

        /* universe set 
         */
    EXEC_PATH_SET& Universe  (void);
    void  Set_To_Be_Universe (void);


        /* Intersection operation
         */
    BOOL Intersection_Is_Empty (EXEC_PATH_SET* eps);

        /* Subset operation
         */

        /* return the intersection of <this> and exec path whose id 
         * ranges from <from> thru <to> inclusively.
         */
    EXEC_PATH_SET& Subset (EXEC_PATH_ID from, EXEC_PATH_ID to);

        /* ret true iff eps is subset of <*this> */
    BOOL Is_Subset_Of (EXEC_PATH_SET* eps);




        /* looping over each EXEC_PATH_ID. if <check_membershit>
         * is turned on, {Next|Prev}_Path_Id will trigger assertion 
         * when it encounts an non-member <path>.
         */
    EXEC_PATH_ID  First_Path_Id (void) ;
    EXEC_PATH_ID  Next_Path_Id  
                    (EXEC_PATH_ID path, BOOL check_membership=TRUE) ;
    EXEC_PATH_ID  Last_Path_Id  (void) ;
    EXEC_PATH_ID  Prev_Path_Id 
                    (EXEC_PATH_ID path, BOOL check_membership=TRUE) ;


        /* tracing & dumping 
         */ 
    void Dump (FILE * f=stderr);

    #ifdef Is_True_On 
    void gdb_dump (void);
    #endif
};


    /* =========================================================
     * =========================================================
     * 
     *  class EXEC_PATH_MGR 
     *
     * =========================================================
     * =========================================================
     */ 
class EXEC_PATH_MGR {
private:

    REGION * _region;       /* we collect exec-path info confined
                             * within this region */
    
    EXEC_PATH_VECTOR _pathv ;  /* EXEC_PATH vector, indiced by its Id() */
    MEM_POOL * _mp;         /* underlying memory pool */

            /* information associated with each node*/
    typedef struct tagEP_NODE_INFO {

        REGIONAL_CFG_NODE *n;      /* associated node */
        MEM_POOL * mp;             /* underlying memory pool */ 
        mINT32 subgraph_path_num ; /* this many paths in the subgraph
                                    * leading from <n> */
        mINT32 path_num;           /* this many paths flow through
                                    * entire <_region> */
        EXEC_PATH_SET eps;         /* all exec-paths */

        tagEP_NODE_INFO (REGIONAL_CFG_NODE *node,MEM_POOL* memp) :
            mp(memp), eps(memp), n(node) {
            subgraph_path_num = 0;
            path_num = 0;
        }

        ~tagEP_NODE_INFO (void) { /* do nothing */ } ;

    } EP_NODE_INFO;

    CFG_NODE_MAP    _ep_node_info ; /* f : node -> EP_NODE_INFO */
    BOOL   _path_info_invalid ;
    mINT32 _total_paths;

    enum { MAX_PATH_NUM = 128, };

        /* Calc_Subgraph_Path_Num 
         * 
         * Supporting routines for Acquire_Path_Info.
         */
    INT32 Calc_Subgraph_Path_Num (void);


public:

    EXEC_PATH_MGR   (MEM_POOL * mp);
    ~EXEC_PATH_MGR  (void) { /* do nothing */ } 

    BOOL Path_Info_Is_Invalid (void) const {return _path_info_invalid;}
    
        /* copy a "manager" are not allowed. this routine actually
         * resort to Fail_FmtAssertion to abort execution.
         */
    EXEC_PATH_MGR& operator = (EXEC_PATH_MGR& epm);
    
        /* query internal state */
    EXEC_PATH * Path (EXEC_PATH_ID pid) const {
                    return (_pathv.size() > pid) ? _pathv[pid] : NULL;
                }
    REGION * Scope (void) { return _region ; }

    INT32  Path_In_Total (void) const { return _total_paths; } 

        /* acquire execution path information. return exec-path in total 
         */
    INT32  Acquire_Path_Info (REGION *r) ;

    EXEC_PATH_SET* Get_Path_Flow_Thru (BB* bb);
    EXEC_PATH_SET* Get_Path_Flow_Thru (REGION* r);
    EXEC_PATH_SET* Get_Path_Flow_Thru (REGIONAL_CFG_NODE* r) {
                        return r->Is_Region () ? 
                               Get_Path_Flow_Thru (r->Region_Node ()) :
                               Get_Path_Flow_Thru (r->BB_Node ());
                   }

        /* tracing & dumping 
         */
    void  Dump (FILE *f=stderr);
    
    #ifdef Is_True_On 
    void  gdb_dump (void);
    #endif 

}; /* end of class EXEC_PATH_MGR */

    /* ===========================================================
     * ===========================================================
     *
     *  class SUB_SEME_EXEC_PATH_MGR
     *
     *  manager of execution path information of sub SEME control 
     *  flow graph. (The whole graph's execution path is managed 
     *  by EXEC_PATH_MGR). 
     *
     *  We do not collect the sub-SEME's execution path info from
     *  scrath, rather, derive from whole-graph-exec-path-info's
     *  manager(EXEC_PATH_MGR).
     *
     * ===========================================================
     * ===========================================================
     */
class SUB_SEME_EXEC_PATH_MGR {
private:

    REGION *    _base_graph;
    REGIONAL_CFG_NODE * _sub_SEME_root;
    MEM_POOL *  _mp;
    
    EXEC_PATH_VECTOR  _paths; 
    
public:

    SUB_SEME_EXEC_PATH_MGR  (MEM_POOL *mp);
    ~SUB_SEME_EXEC_PATH_MGR (void);

    void Derive_Exec_Path_Info (EXEC_PATH_MGR * exec_path_mgr, 
                                REGIONAL_CFG_NODE* sub_SEME_root) ;
    
    INT32  Path_In_Total (void) const { return _paths.size(); }

    EXEC_PATH * First_Path (void) const { 
                    return Path_In_Total () ? _paths[0] : NULL; 
                }

    EXEC_PATH * Last_Path (void) const {
                    return Path_In_Total () ? 
                            _paths[Path_In_Total () - 1]:
                            NULL;
                }

    EXEC_PATH * Next_Path (EXEC_PATH * ep) const {
                    EXEC_PATH_ID pid = ep->Id ();
                    return (pid+ 1 >= Path_In_Total ()) ? 
                            _paths[pid + 1] : 
                            NULL;
                }
};

#endif /* sched_path_INCLUDED */

