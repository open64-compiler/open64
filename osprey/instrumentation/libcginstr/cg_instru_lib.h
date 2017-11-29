/*
  Copyright (c) 2001, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer.

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution.

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef cg_instru_lib_INCLUDED
#define cg_instru_lib_INCLUDED

#ifndef EDGE_PROFILE
#define EDGE_PROFILE
#endif

#include "profile_com.h"
#include "fb_info.h"

#define ERROR_VALUE -1

#ifndef MAP_FILE              // 44BSD defines this & requires it to mmap files 
#define MAP_FILE 0            // to compile under systems other than 44BSD  
#endif

#define EDGE_PROFILE_ALLOC   0x0001
#define VALUE_PROFILE_ALLOC  0x0002
#define STRIDE_PROFILE_ALLOC  0x0004

#ifdef TARG_LOONGSON
#define CACHE_PROFILE_ALLOC  0x0008
#define CACHE_TAG(cp, addr)	((addr) >> (cp)->tag_shift)
#define CACHE_SET(cp, addr)	(((addr) >> (cp)->set_shift) & (cp)->set_mask)
#define CACHE_BLK(cp, addr)	((addr) & (cp)->blk_mask)
#define CACHE_TAGSET(cp, addr)	((addr) & (cp)->tagset_mask)
#define CACHE_BINDEX(cp, blks, i)					\
	((struct cache_blk_t *)(((char *)(blks)) +				\
	(i)*(sizeof(struct cache_blk_t))))

#endif
struct _FREQ
{
    FB_FREQ_TYPE  _type;
    UINT64 _value;
};


#ifdef TARG_LOONGSON
/* cache block (or line) definition */
struct cache_blk_t
{
    struct cache_blk_t *way_next;	/* next block in the ordered way chain, used
                                           to order blocks for replacement */
    struct cache_blk_t *way_prev;	/* previous block in the order way chain */
    UINT64 tag;				/* data block tag value */
};

struct cache_set_t
{
    struct cache_blk_t *way_head;	/* head of way list */
    struct cache_blk_t *way_tail;	/* tail pf way list */
    struct cache_blk_t *blks;		/* cache blocks, allocated sequentially, so
	                                   this pointer can also be used for random									  access to cache blocks */
};

struct  cache_t
{
    int nsets;				/* number of sets */
    int bsize;				/* block size in bytes */
    int assoc;				/* cache associativity */
    unsigned int hit_latency;		/* cache hit latency */
    UINT64 blk_mask;
    int set_shift;
    UINT64 set_mask;			/* use *after* shift */
    int tag_shift;
    UINT64 tag_mask;			/* use *after* shift */
    UINT64 tagset_mask;			/* used for fast hit detection */
    unsigned char *data;  		/* pointer to data blocks allocation */
    /* last block to hit, used to optimize cache hit processing */
    UINT64 last_tagset;			/* tag of last line accessed */
    struct cache_blk_t *last_blk;	/* cache block last accessed */
    struct cache_set_t sets[1];		/* each entry is a set */
};
#endif

struct PU_PROFILE_INFO
{
    // edge profile info
    INT32  _edge_sum;
    INT32  _has_alloc;
    _FREQ* _counter;

    // value profile info
    UINT32 _instr_count; // how many instrutions has been profiled in this pu. E
    // qual to the item number in <_val_prof_tnv_table>
    UINT64 _sum_count;   // the sum of each profiled instruction executes. Used as
    // Checksum.
    UINT32 _ld_count;    // for prefetch how many ld struction has been profile in this pu
#ifdef TARG_LOONGSON
    UINT32 _mem_count;   // for cache profiling
    FB_CACHE* _cache_prof_table;
#endif
    FB_TNV* _val_prof_tnv_table;
    FB_TNV* _srd_prof_tnv_table;

    PU_PROFILE_INFO(): _has_alloc(0),
            _edge_sum(0),
            _counter(NULL),
            _sum_count(0),
            _instr_count(0),
            _ld_count(0),
            _val_prof_tnv_table(NULL),
            _srd_prof_tnv_table(NULL) {}

    PU_PROFILE_INFO(INT32 sum, _FREQ* counter)
    {
        _edge_sum = sum;
        _has_alloc = EDGE_PROFILE_ALLOC;
        _counter = counter;
        _sum_count = 0;
        _instr_count = 0;
        _ld_count = 0;
        _val_prof_tnv_table = NULL;
        _srd_prof_tnv_table = NULL;
    }

    PU_PROFILE_INFO(UINT32 instr_count, UINT32 ld_count, UINT32 sum_count, FB_TNV* tnv_table, FB_TNV* srd_tnv_table)
    {
        _edge_sum = 0;
        _has_alloc = 0;
        _counter = NULL;
        _sum_count = sum_count;
        _instr_count = instr_count;
        _ld_count = ld_count;
        _val_prof_tnv_table = tnv_table;
        _srd_prof_tnv_table = srd_tnv_table;
    }
};

// ------------------------------------------------------------------
// List of functions in instrumentation lib
// ------------------------------------------------------------------
extern "C"
{

    void __profile_init(char* output_file,
    PROFILE_PHASE phasenum);
    char* __profile_pu_init(char* _srcfile_pu_name, INT32 checksum);

    void __profile_edge(char* srcfile_pu_name, UINT32 id);

    PU_PROFILE_INFO *  __value_profile_pu_init(char * outputfile,
            char* srcfile_pu_name,  PROFILE_PHASE phase, UINT32 instr_count);
    void __value_profile_invoke(PU_PROFILE_INFO * pu_hdr,
                                UINT32 instr_id, UINT64 value);
    PU_PROFILE_INFO *  __stride_profile_pu_init(char * outputfile,
            char* srcfile_pu_name,  PROFILE_PHASE phase, UINT32 instr_count);
    void __stride_profile_invoke(PU_PROFILE_INFO * pu_hdr,
                                 UINT32 instr_id, UINT64 value);
#ifdef TARG_LOONGSON
    PU_PROFILE_INFO * __cache_profile_pu_init(char * outputfile,
            char* srcfile_pu_name, PROFILE_PHASE phase, UINT32 instr_count);
    void __cache_profile_invoke(PU_PROFILE_INFO *pu_hdr,
                                UINT32 instr_id, UINT64 addr);
#endif

}

#endif

