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

#include <stack>

#include <vector>
extern "C"
{
#include "unistd.h"
#include "sys/types.h"
#include "sys/stat.h"
#include "fcntl.h"
#include "sys/mman.h"         // for  mmap() 
#include "stdio.h"
};

#include <string>
#include <ext/hash_map>
#include <map>
#include<_G_config.h>
using namespace std;
extern "C"
{
#include "stdlib.h"
#include "time.h"
};
#include "cg_instru_lib.h"

typedef PU_PROFILE_INFO * PU_PROFILE;

namespace
{
using __gnu_cxx::hash_map;
struct ptrhash
{
    size_t operator()(void *p)const
    {
        return reinterpret_cast<size_t>(p);
    }
};
}

typedef hash_map<char*, PU_PROFILE, ptrhash> HASH_MAP;
typedef stack<PU_PROFILE> STACK_PROFILE;
typedef stack<char *> STACK_NAME;


static HASH_MAP PU_PROFILE_INFO_TABLE;

static STACK_PROFILE PU_PROFILE_STACK;
static STACK_NAME PU_NAME;

static const INT32   Pu_Hdr_size = sizeof(Pu_Hdr);
static const INT32   Fb_Hdr_size = sizeof(Fb_Hdr);
static const INT32   FB_FREQ_size = sizeof(FB_FREQ);
static const INT32   _FREQ_size = sizeof(_FREQ);
static const INT32   PU_PROFILE_INFO_size = sizeof(PU_PROFILE_INFO);
static const INT32   TNV_item_size = sizeof(FB_TNV);
#ifdef TARG_LOONGSON
static const INT32   FBCACHE_item_size = sizeof(FB_CACHE);
#endif

static INT32   fd = -1;                     // output file
static INT32   pu_num = 0;                  // the program's PU sum
static INT32   pu_counter = 0;
static INT32   profile_offset = 0;          // the feedback file header's offset
static INT32   file_header_offset = 0;

static PU_PROFILE_INFO* edge_counter = NULL;
static PROFILE_PHASE prof_phase;

static INT32   PU_header_offset = 0;        // the Pus' header offset.
static INT32   str_table_offset = 0;        // the string table offset.
static INT32   Pu_file_offset = 0;          // the Pu data offset.
static INT32   str_table_size = 0;          // the string table size
static INT32   current_PU_header_offset = 0;
static INT32   current_str_table_offset = 0;
static INT32   current_Pu_file_offset = 0;
static INT32   current_offset = 0 ;
static INT32   output_file_size = 0;
static char*   output_file_name = NULL;
static char*   map_addr = NULL;
static char*   src_fname = NULL;
static char*   current_pu_name = NULL;
static BOOL    have_open_output_file = FALSE;
static BOOL    have_finish = FALSE;

static char* current_srcfile_pu_name = NULL;
static char* prev_srcfile_pu_name = NULL;
static PU_PROFILE_INFO * current_pu_profile_info = NULL ;
static PU_PROFILE_INFO * prev_pu_profile_info = NULL;
#ifdef TARG_LOONGSON
static struct cache_t * cp, * tlb;
/* where to insert a block onto the ordered way chain */
enum list_loc_t { Head, Tail };
#endif
static PU_PROFILE cur_pu_profile;

#ifdef VALUE_PROFILE_VERIFY
static FILE * fout;
#endif

// ------------------------------------------------------------------
// List of functions that is used to write feedback data into output
// file in instrumentation lib
// ------------------------------------------------------------------
static void _write_File_Header(void);
static void _write_Pu_Header(void);
static void _write_Str_Header(void);
static void _write_pu_data(void);
static void _write_edge_profile(PU_PROFILE_INFO* pu_info);

static void _write_TNV_items_profile(PU_PROFILE_INFO* pu_info);
static void _write_srd_TNV_items_profile(PU_PROFILE_INFO* pu_info);
#ifdef TARG_LOONGSON
static void _write_FBCACHE_items_profile(PU_PROFILE_INFO* pu_info);
#endif

// __profile_finalize will be invoked in crt, it write feedback data into output file
static void __profile_finalize(void);

// Free_Space free the allocated memory space
static void Free_Space(void);

// ------------------------------------------------------------------
// function in instrumentation lib when errors occur
// ------------------------------------------------------------------
static void instru_lib_error(const char *fmt, const char* msg = "");

// -----------------------------------------------------------------------
// See "cg_instru_lib.h" for interface.
// -----------------------------------------------------------------------
#ifdef TARG_LOONGSON
/* return log of a number to the base 2 */
int
log_base2(int n)
{
    int power = 0;

    if (n <= 0 || (n & (n - 1)) != 0)
        instru_lib_error("log2() only works for positive power of two values");

    while (n >>= 1)
        power++;

    return power;
}

void __cache_init()
{
    struct cache_blk_t* blk;
    int nsets = 128; 	// Mhd.L[0].Size/(Mhd.L[0].Associativity*Mhd.L[0].Line_Size)
    int bsize = 32;	// Mhd.L[0].Line_Size
    int assoc = 2;	// Mhd.L[0].Associativity
    int i, j, bindex;

    /* allocate the cache structure */
    cp = (struct cache_t *)
         calloc(1, sizeof(struct cache_t) + (nsets - 1) * sizeof(struct cache_set_t));
    if (!cp)
        instru_lib_error("out of virtual memory");

    cp->nsets = nsets;
    cp->bsize = bsize;
    cp->assoc = assoc;
    cp->hit_latency = 1;

    /* compute derived parameters */
    cp->blk_mask = bsize - 1;
    cp->set_shift = log_base2(bsize);
    cp->set_mask = nsets - 1;
    cp->tag_shift = cp->set_shift + log_base2(nsets);
    cp->tag_mask = (1 << (32 - cp->tag_shift)) - 1;
    cp->tagset_mask = ~cp->blk_mask;
    cp->last_tagset = 0;
    cp->last_blk = NULL;
    /* allocate data blocks */
    cp->data = (unsigned char *)calloc(nsets * assoc,
                                       sizeof(struct cache_blk_t));

    /* slice up the data blocks */
    for (bindex = 0, i = 0; i < nsets; i++)
    {
        cp->sets[i].way_head = NULL;
        cp->sets[i].way_tail = NULL;

        /* NOTE: all the blocks in a set *must* be allocated contiguously,
        otherwise, block accesses through SET->BLKS will fail (used
        during random replacement selection) */
        cp->sets[i].blks = CACHE_BINDEX(cp, cp->data, bindex);

        /* link the data blocks into ordered way chain and hash table bucket
           chains, if hash table exists */
        for (j = 0; j < assoc; j++)
        {
            /* locate next cache block */
            blk = CACHE_BINDEX(cp, cp->data, bindex);
            bindex++;

            /* invalidate new cache block */
            blk->tag = 0;

            /* insert into head of way list, order is arbitrary at this point */
            blk->way_next = cp->sets[i].way_head;
            blk->way_prev = NULL;
            if (cp->sets[i].way_head)
                cp->sets[i].way_head->way_prev = blk;
            cp->sets[i].way_head = blk;
            if (!cp->sets[i].way_tail)
                cp->sets[i].way_tail = blk;
        }
    }

    int nsets2 = 1;
    int bsize2 = 4096;	// Mhd.L[0].Page_Size
    int assoc2 = 96;	// Mhd.L[0].TLB_Entries
    int hit_latency2 = 1;

    tlb = (struct cache_t *)
          calloc(1, sizeof(struct cache_t) + (nsets2 - 1) * sizeof(struct cache_set_t));
    if (!tlb)
        instru_lib_error("out of virtual memory");
    tlb->nsets = nsets2;
    tlb->bsize = bsize2;
    tlb->assoc = assoc2;
    tlb->hit_latency = hit_latency2;

    /* compute derived parameters */
    tlb->blk_mask = bsize2 - 1;
    tlb->set_shift = log_base2(bsize2);
    tlb->set_mask = nsets2 - 1;
    tlb->tag_shift = tlb->set_shift + log_base2(nsets2);
    tlb->tag_mask = (1 << (32 - tlb->tag_shift)) - 1;
    tlb->tagset_mask = ~tlb->blk_mask;
    tlb->last_tagset = 0;
    tlb->last_blk = NULL;
    /* allocate data blocks */
    tlb->data = (unsigned char *)calloc(nsets2 * assoc2,
                                        sizeof(struct cache_blk_t));

    /* slice up the data blocks */
    for (bindex = 0, i = 0; i < nsets2; i++)
    {
        tlb->sets[i].way_head = NULL;
        tlb->sets[i].way_tail = NULL;

        /* NOTE: all the blocks in a set *must* be allocated contiguously,
        otherwise, block accesses through SET->BLKS will fail (used
        during random replacement selection) */
        tlb->sets[i].blks = CACHE_BINDEX(tlb, tlb->data, bindex);

        /* link the data blocks into ordered way chain and hash table bucket
           chains, if hash table exists */
        for (j = 0; j < assoc2; j++)
        {
            /* locate next cache block */
            blk = CACHE_BINDEX(tlb, tlb->data, bindex);
            bindex++;

            /* invalidate new cache block */
            blk->tag = 0;

            /* insert into head of way list, order is arbitrary at this point */
            blk->way_next = tlb->sets[i].way_head;
            blk->way_prev = NULL;
            if (tlb->sets[i].way_head)
                tlb->sets[i].way_head->way_prev = blk;
            tlb->sets[i].way_head = blk;
            if (!tlb->sets[i].way_tail)
                tlb->sets[i].way_tail = blk;
        }
    }

}
#endif
void __profile_init(char* output_file,
                    PROFILE_PHASE phasenum)
{
    if (!have_open_output_file && !have_finish)
    {
        char time_str[30];
        time_t lt;
        lt = time(NULL);
        sprintf(time_str, ".%d", (INT)lt);

        INT32 str_len = strlen(time_str);
        INT32 slen = strlen(output_file);
        output_file_name = (char *) malloc(sizeof(char) *
                                           (slen + str_len + 1));
        if (output_file_name == NULL)
            instru_lib_error("Unable to malloc space");
        strcpy(output_file_name, output_file);
        strcpy(&(output_file_name[slen]), time_str);
        unlink(output_file_name);
        if ((fd = open64(output_file_name, O_RDWR | O_CREAT | O_TRUNC, 00777)) < 0)
            instru_lib_error("Unable to open file: %s", output_file_name);
        have_open_output_file = TRUE;
        prof_phase = phasenum;
#ifdef TARG_LOONGSON
        /* Initialize the cache and tlb */
        __cache_init();
#endif
        atexit(__profile_finalize);
    }
}

char* __profile_pu_init(char* srcfile_pu_name, INT check_sum)
{
    if (!have_finish)
    {
        PU_PROFILE & current_pu_profile = PU_PROFILE_INFO_TABLE[srcfile_pu_name];
        current_srcfile_pu_name = srcfile_pu_name;

        if (current_pu_profile == NULL)
        {
            current_pu_profile = new PU_PROFILE_INFO(check_sum,
                    (_FREQ*) calloc(check_sum, _FREQ_size));
        }
        else
        {
            if (!((current_pu_profile->_has_alloc) & EDGE_PROFILE_ALLOC))
            {
                current_pu_profile->_counter = (_FREQ *)calloc(check_sum, _FREQ_size);
                current_pu_profile->_has_alloc |= EDGE_PROFILE_ALLOC;
                current_pu_profile->_edge_sum = check_sum;
            }
        }
        cur_pu_profile = current_pu_profile;
    }
    return srcfile_pu_name;
}

void __profile_edge(char* srcfile_pu_name, UINT32 id)
{
    if (!have_finish)
    {
        if (current_srcfile_pu_name != srcfile_pu_name)
        {
            cur_pu_profile = PU_PROFILE_INFO_TABLE[srcfile_pu_name];
            current_srcfile_pu_name = srcfile_pu_name;
        }

        if (id < cur_pu_profile -> _edge_sum)
            cur_pu_profile->_counter[id]._value++;
    }
}

static void __profile_finalize()
{
#ifdef VALUE_PROFILE_VERIFY
    fclose(fout);
    have_finish = TRUE;
    return;
#endif
    hash_map<char* , PU_PROFILE_INFO*, ptrhash >::iterator i;
    PU_PROFILE_INFO* current_PU_profile_info;
    profile_offset = 0;
    pu_counter = PU_PROFILE_INFO_TABLE.size();
    file_header_offset = 0;
    PU_header_offset = Fb_Hdr_size + file_header_offset;
    str_table_offset = pu_counter * Pu_Hdr_size + Fb_Hdr_size;
    Pu_file_offset = str_table_offset;
    current_PU_header_offset = PU_header_offset;
    current_str_table_offset = str_table_offset;


    output_file_size = Fb_Hdr_size + Pu_Hdr_size * pu_counter;
    str_table_size = 0;
    for (i = PU_PROFILE_INFO_TABLE.begin();
            i != PU_PROFILE_INFO_TABLE.end(); i++)
    {
        PU_PROFILE_INFO * pu_info = (*i).second;

        output_file_size += pu_info->_edge_sum * FB_FREQ_size;
        output_file_size += pu_info->_instr_count * TNV_item_size;
        output_file_size += pu_info->_ld_count * TNV_item_size;
#ifdef TARG_LOONGSON
        output_file_size += pu_info->_mem_count * FBCACHE_item_size;
#endif
        INT size = strlen((*i).first) + strlen(" ");
        output_file_size += size;
        str_table_size += size;
        Pu_file_offset += size;
    }
    current_Pu_file_offset = Pu_file_offset;
    if (lseek(fd, output_file_size, SEEK_SET) < 0)
        instru_lib_error("Unable to seek file:%s", output_file_name);

    if (write(fd, "", 1) != 1)
        instru_lib_error("Unable to write file:%s", output_file_name);

    map_addr = (char *) mmap(0, output_file_size, PROT_READ | PROT_WRITE,
                             MAP_FILE | MAP_SHARED, fd, 0);

    if (map_addr == MAP_FAILED)
    {
        close(fd);
        instru_lib_error("Unable to mmap");
    }

    _write_File_Header();
    _write_Pu_Header();
    _write_Str_Header();
    _write_pu_data();

    munmap(map_addr, output_file_size);
    close(fd);
    Free_Space();
    have_finish = TRUE;
}

static void  _write_File_Header()
{
    Fb_Hdr file_header;
    char fb_ident[16] = "0123456789abcde";
    current_offset = 0;
    int i;
    for (i = 0; i < 16; i++)
        file_header.fb_ident[i] = fb_ident[i];
    file_header.fb_version = INSTR_CURRENT;
    file_header.fb_profile_offset = 0;      // current_Pu_file_offset;
    file_header.fb_pu_hdr_offset = current_PU_header_offset;
    file_header.fb_pu_hdr_ent_size = Pu_Hdr_size;
    file_header.fb_pu_hdr_num = pu_counter;
    file_header.fb_str_table_offset = current_str_table_offset;
    file_header.fb_str_table_size = str_table_size;
    file_header.phase_num = prof_phase;
    memcpy(map_addr + current_offset, &file_header, Fb_Hdr_size);
    current_offset += Fb_Hdr_size;
}

static void  _write_Str_Header()
{
    hash_map<char* , PU_PROFILE_INFO*, ptrhash >::iterator i;
    for (i = PU_PROFILE_INFO_TABLE.begin();
            i != PU_PROFILE_INFO_TABLE.end(); i++)
    {
        memcpy(map_addr + current_offset, (*i).first,
               strlen((*i).first) + 1);
        current_offset += strlen((*i).first) + 1;
    }
}

static void  _write_Pu_Header()
{

    Pu_Hdr pu_header;
    INT current_pu_name_index = 0;
    hash_map<char* , PU_PROFILE_INFO*, ptrhash >::iterator i;
    for (i = PU_PROFILE_INFO_TABLE.begin();
            i != PU_PROFILE_INFO_TABLE.end(); i++)
    {
        PU_PROFILE_INFO* info = (*i).second;
        {
            pu_header.pu_num_edge_entries = info->_edge_sum;
            pu_header.pu_edge_offset = current_Pu_file_offset;
            pu_header.pu_value_offset = current_Pu_file_offset +
                                        info->_edge_sum * FB_FREQ_size;
            pu_header.pu_stride_offset = pu_header.pu_value_offset + info->_instr_count * TNV_item_size;
#ifdef TARG_LOONGSON
            pu_header.pu_cache_offset = pu_header.pu_stride_offset + info->_ld_count * TNV_item_size;
#endif
            pu_header.pu_checksum = info->_edge_sum;
            pu_header.pu_name_index = current_pu_name_index;

            pu_header.pu_instr_count = info->_instr_count;
            pu_header.pu_instr_exec_count = info->_sum_count;
            pu_header.pu_ld_count = info->_ld_count;
#ifdef TARG_LOONGSON
            pu_header.pu_mem_count = info->_mem_count;
#endif

            current_pu_name_index += strlen((*i).first) + strlen(" ");
            pu_header.pu_file_offset = 0;
            pu_header.pu_inv_offset = current_Pu_file_offset;
            pu_header.pu_num_inv_entries = 0;
            pu_header.pu_br_offset = current_Pu_file_offset;
            pu_header.pu_num_br_entries = 0;
            pu_header.pu_switch_offset = current_Pu_file_offset;
            pu_header.pu_switch_target_offset = current_Pu_file_offset;
            pu_header.pu_num_switch_entries = 0;
            pu_header.pu_cgoto_offset = current_Pu_file_offset;
            pu_header.pu_cgoto_target_offset = current_Pu_file_offset;
            pu_header.pu_num_cgoto_entries = 0;
            pu_header.pu_loop_offset = current_Pu_file_offset;
            pu_header.pu_num_loop_entries = 0;
            pu_header.pu_scircuit_offset = current_Pu_file_offset;
            pu_header.pu_num_scircuit_entries = 0;
            pu_header.pu_call_offset = current_Pu_file_offset;
            pu_header.pu_num_call_entries = 0;
            memcpy(map_addr + current_offset, &pu_header, Pu_Hdr_size);
            current_offset += Pu_Hdr_size;
            current_Pu_file_offset += info->_edge_sum * FB_FREQ_size;
            current_Pu_file_offset += info->_instr_count * TNV_item_size;
            current_Pu_file_offset += info->_ld_count * TNV_item_size;
#ifdef TARG_LOONGSON
            current_Pu_file_offset += info->_mem_count * FBCACHE_item_size;
#endif
        }
    }
}

static void  _write_pu_data()
{
    hash_map<char* , PU_PROFILE_INFO*, ptrhash >::iterator i;
    for (i = PU_PROFILE_INFO_TABLE.begin();
            i != PU_PROFILE_INFO_TABLE.end(); i++)
    {
        PU_PROFILE_INFO* info = (*i).second;
        _write_edge_profile(info);
        _write_TNV_items_profile(info);
        _write_srd_TNV_items_profile(info);
#ifdef TARG_LOONGSON
        _write_FBCACHE_items_profile(info);
#endif
    }
}

static void  _write_edge_profile(PU_PROFILE_INFO* pu_info)
{
    INT pu_num_edge_entries = pu_info->_edge_sum;
    FB_FREQ fb_freq;
    for (INT j = 0; j < pu_num_edge_entries; j++)
    {
        fb_freq._value = (float) pu_info->_counter[j]._value;
        fb_freq._type = FB_FREQ_TYPE_EXACT;
        memcpy(map_addr + current_offset, &fb_freq, FB_FREQ_size);
        current_offset += FB_FREQ_size;
    }
}

void Free_Space()
{
    hash_map<char* , PU_PROFILE_INFO*, ptrhash >::iterator i;
    for (i = PU_PROFILE_INFO_TABLE.begin();
            i != PU_PROFILE_INFO_TABLE.end(); i++)
        free((*i).second);
}

void instru_lib_error(const char *fmt, const char* msg)
{
    printf(fmt, msg);
    exit(-1);
}

////////////////////////////////////////////////////////////////
//// value profile part
////////////////////////////////////////////////////////////////
PU_PROFILE_INFO * __value_profile_pu_init(char * outputfile,
        char* srcfile_pu_name, PROFILE_PHASE phase, UINT32 instr_count)
{
#ifdef VALUE_PROFILE_VERIFY
    // new code here: this is for offline test, write all data into one txt file.
    if (!have_open_output_file)
    {
        if ((fout = fopen(outputfile, "wb")) == NULL)
            instru_lib_error("Unable to open file: %s", outputfile);
        have_open_output_file = TRUE;
        void __profile_finalize();
        atexit(__profile_finalize);
    }
    return;
#endif
    __profile_init(outputfile, phase);

    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    pPU_PROFILE_INFO = PU_PROFILE_INFO_TABLE[srcfile_pu_name];
    if (pPU_PROFILE_INFO != NULL)
    {
        if (!((pPU_PROFILE_INFO->_has_alloc) & VALUE_PROFILE_ALLOC))
        {
            pPU_PROFILE_INFO->_val_prof_tnv_table = (FB_TNV *)calloc(instr_count, TNV_item_size);
            if (!(pPU_PROFILE_INFO->_val_prof_tnv_table))
            {
                instru_lib_error("__value_profile_pu_init : Not enough memory.\n");
            }
            pPU_PROFILE_INFO->_instr_count = instr_count;
            pPU_PROFILE_INFO->_has_alloc |= VALUE_PROFILE_ALLOC;
        }
    }
    else
    {
        FB_TNV * tnv_table = (FB_TNV*) calloc(instr_count, TNV_item_size);
        if (!tnv_table)
        {
            instru_lib_error("__value_profile_pu_init : Not enough memory.\n");
        }
        pPU_PROFILE_INFO = new PU_PROFILE_INFO(instr_count, 0, 0, tnv_table, NULL);
        pPU_PROFILE_INFO->_has_alloc |= VALUE_PROFILE_ALLOC;
        PU_PROFILE_INFO_TABLE[srcfile_pu_name] = pPU_PROFILE_INFO;
    }
    return pPU_PROFILE_INFO;
}

void __value_profile_invoke(PU_PROFILE_INFO * pu_hdr, UINT32 instr_id, UINT64 value)
{
#ifdef VALUE_PROFILE_VERIFY
    fprintf(fout, "[%s%c :%u,%llu]", srcfile_pu_name, '\0', instr_id, value);
    return;
#endif
    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    pPU_PROFILE_INFO = pu_hdr;
    pPU_PROFILE_INFO->_sum_count++;
    pPU_PROFILE_INFO->_val_prof_tnv_table[instr_id]._id = instr_id;  // actually this is no use.
    pPU_PROFILE_INFO->_val_prof_tnv_table[instr_id]._exec_counter++; // execution counter.
    pPU_PROFILE_INFO->_val_prof_tnv_table[instr_id]._flag = 0;
    pPU_PROFILE_INFO->_val_prof_tnv_table[instr_id]._clear_counter++;
    // now the tnv table info update.
    // We use the first 6 items as "steady part", the last 4 items as "clear part".
    // clear_interval is the sum of _exec_counter of the middle two in the steady part.
    FB_TNV * ptnv = &(pPU_PROFILE_INFO->_val_prof_tnv_table[instr_id]);
    INT i, j;
    UINT64 clear_interval = ptnv->_counters[3] + ptnv->_counters[4];
    if (ptnv->_clear_counter >= clear_interval)
    {
        ptnv->_clear_counter = 0;
        // resort tnv
        UINT64 tmpvalues[10], tmpcounters[10];
        for (i = 0; i < 10; i++)
        {
            tmpvalues[i] = ptnv->_values[i];
            tmpcounters[i] = ptnv->_counters[i];
        }
        INT a, b;
        a = 0;
        b = 6;
        i = 0;
        while (a < 6 && b < 10)
        {
            while (a < 6 && tmpcounters[a] >= tmpcounters[b])
            {
                ptnv->_values[i] = tmpvalues[a];
                ptnv->_counters[i] = tmpcounters[a];
                i++;
                a++;
            }
            while (b < 10 && tmpcounters[b] >= tmpcounters[a])
            {
                ptnv->_values[i] = tmpvalues[b];
                ptnv->_counters[i] = tmpcounters[b];
                i++;
                b++;
            }
        }
        while (a < 6)
        {
            ptnv->_values[i] = tmpvalues[a];
            ptnv->_counters[i] = tmpcounters[a];
            i++;
            a++;
        }
        while (b < 10)
        {
            ptnv->_values[i] = tmpvalues[b];
            ptnv->_counters[i] = tmpcounters[b];
            i++;
            b++;
        }
        // clear the clear_part
        for (i = 6; i < 10; i++)
        {
            ptnv->_values[i] = 0;
            ptnv->_counters[i] = 0;
        }
    }

    // see if the value can be put into first 6 values (steady part)
    for (i = 0; i < 6; i++)
    {
        if (value == ptnv->_values[i] && ptnv->_counters[i] > 0)
        {
            ptnv->_counters[i]++;
            j = i;
            while (j > 0 && ptnv->_counters[j-1] < ptnv->_counters[j])
            {
                UINT64 tmp;
                tmp = ptnv->_values[j-1];
                ptnv->_values[j-1] = ptnv->_values[j];
                ptnv->_values[j] = tmp;

                tmp = ptnv->_counters[j-1];
                ptnv->_counters[j-1] = ptnv->_counters[j];
                ptnv->_counters[j] = tmp;
            }
            break;
        }
        else if (ptnv->_counters[i] == 0)
        {
            ptnv->_values[i] = value;
            ptnv->_counters[i] = 1;
            break;
        }
    }

    // if the value can be put in first 6 values (steady part)
    // then it is ok.
    if (i < 6)
        return;
    // put the value into last 4 values (clear part)
    for (i = 6; i < 10; i++)
    {
        if (value == ptnv->_values[i] && ptnv->_counters[i] > 0)
        {
            ptnv->_counters[i]++;
            j = i;
            while (j > 6 && ptnv->_counters[j-1] < ptnv->_counters[j])
            {
                UINT64 tmp;
                tmp = ptnv->_values[j-1];
                ptnv->_values[j-1] = ptnv->_values[j];
                ptnv->_values[j] = tmp;

                tmp = ptnv->_counters[j-1];
                ptnv->_counters[j-1] = ptnv->_counters[j];
                ptnv->_counters[j] = tmp;
            }
            break;
        }
        else if (ptnv->_counters[i] == 0)
        {
            ptnv->_values[i] = value;
            ptnv->_counters[i] = 1;
            break;
        }
    }
}

static void  _write_TNV_items_profile(PU_PROFILE_INFO* pu_info)
{
    INT pu_num_TNV_items = pu_info->_instr_count;
    FB_TNV tnv_item;
    for (INT j = 0; j < pu_num_TNV_items; j++)
    {
        tnv_item = pu_info->_val_prof_tnv_table[j];
        memcpy(map_addr + current_offset, &tnv_item, TNV_item_size);
        current_offset += TNV_item_size;
    }
}

////////////////////////////////////////////////////////////////
//// end of value profile part
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//// stride profile part
////////////////////////////////////////////////////////////////

PU_PROFILE_INFO * __stride_profile_pu_init(char * outputfile,
        char* srcfile_pu_name, PROFILE_PHASE phase, UINT32 instr_count)
{
#ifdef VALUE_PROFILE_VERIFY
    // new code here: this is for offline test, write all data into one txt file.
    if (!have_open_output_file)
    {
        if ((fout = fopen(outputfile, "wb")) == NULL)
            instru_lib_error("Unable to open file: %s", outputfile);
        have_open_output_file = TRUE;
        void __profile_finalize();
        atexit(__profile_finalize);
    }
    return;
#endif
    __profile_init(outputfile, phase);

    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    pPU_PROFILE_INFO = PU_PROFILE_INFO_TABLE[srcfile_pu_name];
    current_srcfile_pu_name = srcfile_pu_name;
    if (pPU_PROFILE_INFO != NULL)
    {
        if (!((pPU_PROFILE_INFO->_has_alloc) & STRIDE_PROFILE_ALLOC))
        {
            pPU_PROFILE_INFO->_srd_prof_tnv_table = (FB_TNV *)calloc(instr_count, TNV_item_size);
            if (!(pPU_PROFILE_INFO->_srd_prof_tnv_table))
            {
                instru_lib_error("__stride_profile_pu_init : Not enough memory.\n");
            }
            pPU_PROFILE_INFO->_ld_count = instr_count;
            pPU_PROFILE_INFO->_has_alloc |= STRIDE_PROFILE_ALLOC;
        }
    }
    else
    {
        FB_TNV * tnv_table = (FB_TNV*) calloc(instr_count, TNV_item_size);
        if (!tnv_table)
        {
            instru_lib_error("__value_profile_pu_init : Not enough memory.\n");
        }
        pPU_PROFILE_INFO = new PU_PROFILE_INFO(0, instr_count, 0, NULL, tnv_table);
        pPU_PROFILE_INFO->_has_alloc |= STRIDE_PROFILE_ALLOC;
        PU_PROFILE_INFO_TABLE[srcfile_pu_name] = pPU_PROFILE_INFO;
    }
    return pPU_PROFILE_INFO;
}

void __stride_profile_invoke(PU_PROFILE_INFO * pu_hdr, UINT32 instr_id, UINT64 value)
{
#ifdef VALUE_PROFILE_VERIFY
    fprintf(fout, "[%s%c :%u,%llu]", srcfile_pu_name, '\0', instr_id, value);
    return;
#endif

    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    pPU_PROFILE_INFO = pu_hdr;
    instr_id--;
    pPU_PROFILE_INFO->_srd_prof_tnv_table[instr_id]._id = instr_id; //actually this is no use.
    pPU_PROFILE_INFO->_srd_prof_tnv_table[instr_id]._flag = 0;
    pPU_PROFILE_INFO->_srd_prof_tnv_table[instr_id]._exec_counter++;
    pPU_PROFILE_INFO->_srd_prof_tnv_table[instr_id]._clear_counter++;//execution counter.

    // now the tnv table info update.
    // We use the first 6 items as "steady part", the last 4 items as "clear part".
    // clear_interval is the sum of _exec_counter of the middle two in the steady part.
    FB_TNV * ptnv = &(pPU_PROFILE_INFO->_srd_prof_tnv_table[instr_id]);
    INT i, j;
    UINT64 clear_interval = ptnv->_counters[3] + ptnv->_counters[4];
    if (ptnv->_clear_counter >= clear_interval)
    {
        ptnv->_clear_counter = 0;
        // resort tnv
        UINT64 tmpvalues[10], tmpcounters[10];
        for (i = 0; i < 10; i++)
        {
            tmpvalues[i] = ptnv->_values[i];
            tmpcounters[i] = ptnv->_counters[i];
        }
        INT a, b;
        a = 0;
        b = 6;
        i =  0;
        while (a < 6 && b < 10)
        {
            while (a < 6 && tmpcounters[a] >= tmpcounters[b])
            {
                ptnv->_values[i] = tmpvalues[a];
                ptnv->_counters[i] = tmpcounters[a];
                i++;
                a++;
            }
            while (b < 10 && tmpcounters[b] >= tmpcounters[a])
            {
                ptnv->_values[i] = tmpvalues[b];
                ptnv->_counters[i] = tmpcounters[b];
                i++;
                b++;
            }
        }
        while (a < 6)
        {
            ptnv->_values[i] = tmpvalues[a];
            ptnv->_counters[i] = tmpcounters[a];
            i++;
            a++;
        }
        while (b < 10)
        {
            ptnv->_values[i] = tmpvalues[b];
            ptnv->_counters[i] = tmpcounters[b];
            i++;
            b++;
        }
        // clear the clear_part
        for (i = 6; i < 10; i++)
        {
            ptnv->_values[i] = 0;
            ptnv->_counters[i] = 0;
        }
    }
    UINT64 temp_address = value;
    value = value - ptnv->_address;
    if (value == ptnv->_stride_steps)
    {
        ptnv->_zero_std_counter++;
    }
    ptnv->_stride_steps = value;
    ptnv->_address = temp_address;
    // see if the value can be put into first 6 values (steady part)
    for (i = 0; i < 6; i++)
    {
        if (value == ptnv->_values[i] && ptnv->_counters[i] > 0)
        {
            ptnv->_counters[i]++;
            j = i;
            while (j > 0 && ptnv->_counters[j-1] < ptnv->_counters[j])
            {
                UINT64 tmp;
                tmp = ptnv->_values[j-1];
                ptnv->_values[j-1] = ptnv->_values[j];
                ptnv->_values[j] = tmp;

                tmp = ptnv->_counters[j-1];
                ptnv->_counters[j-1] = ptnv->_counters[j];
                ptnv->_counters[j] = tmp;
            }
            break;
        }
        else if (ptnv->_counters[i] == 0)
        {
            ptnv->_values[i] = value;
            ptnv->_counters[i] = 1;
            break;
        }
    }


    // if the value can be put in first 6 values (steady part)
    // then it is ok.
    if (i < 6)
        return;

    // put the value into last 4 values (clear part)
    for (i = 6; i < 10; i++)
    {
        if (value == ptnv->_values[i] && ptnv->_counters[i] > 0)
        {
            ptnv->_counters[i]++;
            j = i;
            while (j > 6 && ptnv->_counters[j-1] < ptnv->_counters[j])
            {
                UINT64 tmp;
                tmp = ptnv->_values[j-1];
                ptnv->_values[j-1] = ptnv->_values[j];
                ptnv->_values[j] = tmp;

                tmp = ptnv->_counters[j-1];
                ptnv->_counters[j-1] = ptnv->_counters[j];
                ptnv->_counters[j] = tmp;
            }
            break;
        }
        else if (ptnv->_counters[i] == 0)
        {
            ptnv->_values[i] = value;
            ptnv->_counters[i] = 1;
            break;
        }
    }

}

static void  _write_srd_TNV_items_profile(PU_PROFILE_INFO* pu_info)
{
    INT pu_num_TNV_items = pu_info->_ld_count;
    FB_TNV tnv_item;
    for (INT j = 0; j < pu_num_TNV_items; j++)
    {
        tnv_item = pu_info->_srd_prof_tnv_table[j];
        memcpy(map_addr + current_offset, &tnv_item, TNV_item_size);
        current_offset += TNV_item_size;
    }
}

#ifdef TARG_LOONGSON
/* insert BLK into the order way chain in SET at location WHERE */
static void
update_way_list(struct cache_set_t *set,	/* set contained way chain */
                struct cache_blk_t *blk,	/* block to insert */
                enum list_loc_t where)		/* insert location */
{
    /* unlink entry from the way list */
    if (!blk->way_prev && !blk->way_next)
    {
        /* only one entry in list (direct-mapped), no action */
        Is_True(set->way_head == blk && set->way_tail == blk, " blk should be way_head or way_tail");
        /* Head/Tail order already */
        return;
    }
    /* else, more than one element in the list */
    else if (!blk->way_prev)
    {
        Is_True(set->way_head == blk && set->way_tail != blk, "blk should be way_head");
        if (where == Head)
        {
            /* already there */
            return;
        }
        /* else, move to tail */
        set->way_head = blk->way_next;
        blk->way_next->way_prev = NULL;
    }
    else if (!blk->way_next)
    {
        /* end of list (and not front of list) */
        Is_True(set->way_head != blk && set->way_tail == blk, "blk should be way_tail");
        if (where == Tail)
        {
            /* already there */
            return;
        }
        set->way_tail = blk->way_prev;
        blk->way_prev->way_next = NULL;
    }
    else
    {
        /* middle of list (and not front or end of list) */
        Is_True(set->way_head != blk && set->way_tail != blk, "blk should not be way_head and way_tail");
        blk->way_prev->way_next = blk->way_next;
        blk->way_next->way_prev = blk->way_prev;
    }

    /* link BLK back into the list */
    if (where == Head)
    {
        /* link to the head of the way list */
        blk->way_next = set->way_head;
        blk->way_prev = NULL;
        set->way_head->way_prev = blk;
        set->way_head = blk;
    }
    else if (where == Tail)
    {
        /* link to the tail of the way list */
        blk->way_prev = set->way_tail;
        blk->way_next = NULL;
        set->way_tail->way_next = blk;
        set->way_tail = blk;
    }
    else
    {
        printf("bogus WHERE designator");
        return;
    }
}

PU_PROFILE_INFO * __cache_profile_pu_init(char * outputfile,
        char* srcfile_pu_name, PROFILE_PHASE phase, UINT32 instr_count)
{
#ifdef VALUE_PROFILE_VERIFY
    // this is for offline test, write all data into one txt file.
    if (!have_open_output_file)
    {
        if ((fout = fopen(outputfile, "w")) == NULL)
            instru_lib_error("Unable to open file: %s", outputfile);
        have_open_output_file = TRUE;
        atexit(__profile_finalize);
    }
    return NULL;
#endif

    __profile_init(outputfile, phase);
    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    pPU_PROFILE_INFO = PU_PROFILE_INFO_TABLE[srcfile_pu_name];
    if (pPU_PROFILE_INFO != NULL)
    {
        if (!((pPU_PROFILE_INFO->_has_alloc) & CACHE_PROFILE_ALLOC))
        {
            pPU_PROFILE_INFO->_cache_prof_table = (FB_CACHE *)calloc(instr_count, FBCACHE_item_size);
            if (!(pPU_PROFILE_INFO->_cache_prof_table))
            {
                instru_lib_error("__cache_profile_pu_init : Not enough memory.\n");
            }
            pPU_PROFILE_INFO->_mem_count = instr_count;
            pPU_PROFILE_INFO->_has_alloc |= CACHE_PROFILE_ALLOC;
        }
    }
    else
    {
        FB_CACHE * fbcache_table = (FB_CACHE*) calloc(instr_count, FBCACHE_item_size);
        if (!fbcache_table)
        {
            instru_lib_error("__cache_profile_pu_init : Not enough memory.\n");
        }
        pPU_PROFILE_INFO = (PU_PROFILE_INFO*)calloc(1, sizeof(PU_PROFILE_INFO));
        pPU_PROFILE_INFO->_mem_count = instr_count;
        pPU_PROFILE_INFO->_cache_prof_table = fbcache_table;
        pPU_PROFILE_INFO->_has_alloc |= CACHE_PROFILE_ALLOC;
        PU_PROFILE_INFO_TABLE[srcfile_pu_name] = pPU_PROFILE_INFO;
    }
    return pPU_PROFILE_INFO;
}


void __cache_profile_invoke(PU_PROFILE_INFO *pu_hdr, UINT32 instr_id, UINT64 addr)
{
#ifdef VALUE_PROFILE_VERIFY
    fprintf(fout, "[%u,%llu]\n", instr_id, addr);
    return;
#endif

    PU_PROFILE_INFO * pPU_PROFILE_INFO;
    UINT64 tag = CACHE_TAG(tlb, addr);
    UINT64 set = CACHE_SET(tlb, addr);
    UINT64 bofs = CACHE_BLK(tlb, addr);
    struct cache_blk_t *blk, *repl;

    pPU_PROFILE_INFO = pu_hdr;
    instr_id--;
    pPU_PROFILE_INFO->_cache_prof_table[instr_id]._id = instr_id;
    pPU_PROFILE_INFO->_cache_prof_table[instr_id]._exec_counter++;
    pPU_PROFILE_INFO->_cache_prof_table[instr_id]._flag = 0;

    if (CACHE_TAGSET(tlb, addr) == tlb->last_tagset)
    {
        /* hit in the same block */
        blk = tlb->last_blk;
        pPU_PROFILE_INFO->_cache_prof_table[instr_id]._tlbhit_counter++;
        tlb->last_tagset = CACHE_TAGSET(tlb, addr);
        tlb->last_blk = blk;
        goto cache;
    }

    /* low-associativity cache, linear search the way list */
    for (blk = tlb->sets[set].way_head;
            blk;
            blk = blk->way_next)
    {
        if (blk->tag == tag)
        {
            pPU_PROFILE_INFO->_cache_prof_table[instr_id]._tlbhit_counter++;
            if (blk->way_prev)
                update_way_list(&tlb->sets[set], blk, Head);
            tlb->last_tagset = CACHE_TAGSET(tlb, addr);
            tlb->last_blk = blk;
            goto cache;
        }
    }

    pPU_PROFILE_INFO->_cache_prof_table[instr_id]._tlbmiss_counter++;
    repl = tlb->sets[set].way_tail;
    update_way_list(&tlb->sets[set], repl, Head);
    /* blow away the last block to hit */
    tlb->last_tagset = 0;
    tlb->last_blk = NULL;
    /* update block tags */
    repl->tag = tag;

cache:
    UINT64 tag2 = CACHE_TAG(cp, addr);
    UINT64 set2 = CACHE_SET(cp, addr);
    UINT64 bofs2 = CACHE_BLK(cp, addr);

    if (CACHE_TAGSET(cp, addr) == cp->last_tagset)
    {
        /* hit in the same block */
        blk = cp->last_blk;
        pPU_PROFILE_INFO->_cache_prof_table[instr_id]._l1hit_counter++;
        cp->last_tagset = CACHE_TAGSET(cp, addr);
        cp->last_blk = blk;
        return;
    }
    /* low-associativity cache, linear search the way list */
    for (blk = cp->sets[set2].way_head;
            blk;
            blk = blk->way_next)
    {
        if (blk->tag == tag2)
        {
            pPU_PROFILE_INFO->_cache_prof_table[instr_id]._l1hit_counter++;
            if (blk->way_prev)
                update_way_list(&cp->sets[set2], blk, Head);
            cp->last_tagset = CACHE_TAGSET(cp, addr);
            cp->last_blk = blk;
            return;
        }
    }
    pPU_PROFILE_INFO->_cache_prof_table[instr_id]._l1miss_counter++;
    repl = cp->sets[set2].way_tail;
    update_way_list(&cp->sets[set2], repl, Head);
    /* blow away the last block to hit */
    cp->last_tagset = 0;
    cp->last_blk = NULL;
    /* update block tags */
    repl->tag = tag2;

}

static void  _write_FBCACHE_items_profile(PU_PROFILE_INFO* pu_info)
{
    INT pu_num_fbcache_items = pu_info->_mem_count;
    FB_CACHE fbcache_item;
    for (INT j = 0; j < pu_num_fbcache_items; j++)
    {
        fbcache_item = pu_info->_cache_prof_table[j];
        memcpy(map_addr + current_offset, &fbcache_item, FBCACHE_item_size);
        current_offset += FBCACHE_item_size;
    }
}
#endif
