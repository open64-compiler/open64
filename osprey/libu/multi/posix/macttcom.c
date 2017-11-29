/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libu/multi/posix/macttcom.c	92.1	06/25/99 14:35:10"

/*
 *
 * TASK COMMON Support.
 *
 */

#include "macdefs.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <thread.h>
#include "mactdefs.h"


/*
 * Types defining the task common description table.
 */
typedef struct
{
    unsigned int            tbd_blength : 32;   /* Length of block (bytes)   */
    unsigned int            tbd_address : 32;   /* Address of block          */
    unsigned int            tbd_init    :  1;   /* 0: no init, 1: init       */
    unsigned int                        : 23;   /* <unused>                  */
    unsigned int            tbd_namelen :  8;   /* Length of name (bytes)    */
    unsigned int            tbd_nameoff : 32;   /* Offset of name (bytes)    */
    unsigned int            tbd_init_1  : 32;   /* First init value word     */
    unsigned int            tbd_init_2  : 32;   /* Second init value word    */
}                               tcom_blk_desc;

typedef struct
{
    unsigned int            td_version  :  8;   /* Version                   */
    unsigned int                        :  8;   /* <unused>                  */
    unsigned int            td_numblks  : 16;   /* Number of task commons    */
    unsigned int            td_length   : 32;   /* Len of all blocks (bytes) */
    tcom_blk_desc           td_blocks[1];       /* Block descriptors         */
                                                /* Space for block names     */
}                               taskcom_description;


/*
 * Declarations of local functions.
 */
static void                     destroy_taskcom_desc
                                    (void *tc_desc_addr);
static taskcom_description     *get_taskcom_desc_addr
                                    (void);
static void                     put_taskcom_desc_addr
                                    (taskcom_description *tc_desc_addr);
static taskcom_description     *build_taskcom_desc
                                    (void);

/*
 * These are used for maintaining the thread-specific task common
 * descriptor addresses.  See {get,put}_taskcom_desc_addr().
 *
 * These routines use thread-specific data rather than macrotask
 * IDs or other such, since they also have to work when they're
 * called by an autotask, for example.  Thread-specific data is
 * unique across the whole program.
 */
static thread_key_t             tcda_key;
static mutex_t                  tcda_key_lock;
static int                      tcda_key_initialized = 0;


/*
 * This is the global _taskcom, with the proper type.
 */
#define _TASKCOM                ((taskcom_description *) _taskcom)


#if defined(SHORT_TERM_TASKCOM)
    /*
     * The size of a task common description table with a given
     * number of block descriptors in it.
     */
#   define TCDESC_SIZE(blks)    (  sizeof(taskcom_description)              \
                                 + (((blks) - 1) * sizeof(tcom_blk_desc))   \
                                )

    /*
     * An entry in a task common cross reference hash list.
     */
    typedef struct Hashlist_Entry
    {
        void               *hle_address;        /* Static address          */
        unsigned int        hle_size;           /* Size, in bytes          */
        unsigned int        hle_number;         /* Assigned number         */
        struct Hashlist_Entry
                           *hle_next;           /* Next list entry         */
        struct Hashlist_Entry
                           *hle_onext;          /* Next ordered list entry */
    }                           hashlist_entry;

    /*
     * The hash table for the task common cross reference.  The table
     * size is a prime.  The hashing function can shift the address
     * given to it right 3 bits before doing the MOD, because common
     * blocks are always aligned on doubleword boundaries (thus the
     * low 3 bits of the address must be zero).
     */
    #define TCCR_HASH_SIZE      109             /* It's a prime */
    #define TCCR_HASH(addr)     (((addr) >> 3) % TCCR_HASH_SIZE)

    static hashlist_entry      *tccr_hash_table[TCCR_HASH_SIZE]
                                = { NULL };

    /*
     * The next task common block number.
     */
    static unsigned int         next_hle_number = 1;

    /*
     * Total size of all known task commons.
     */
    static unsigned int         total_tcom_size = 0;

    /*
     * The head and tail of the ordered list of all cross reference
     * entries.
     */
    static hashlist_entry      *olist_head = NULL,
                               *olist_tail = NULL;

    /*
     * A semaphore to single thread access to the cross reference
     * structure.
     */
    static int                  tccr_lock = 0;

#endif


#if defined(DEBUG)
    /*
     * Print the contents of a task common description table.
     */
    static void print_tcom_desc(taskcom_description *tc_desc);
    static void print_tcom_desc(taskcom_description *tc_desc)
    {
        unsigned int            tdb_idx;
        tcom_blk_desc          *tdb_ptr
                                = tc_desc->td_blocks;

        printf("task_description at %#x:\n", tc_desc);
        printf("  ->td_version  %10u\n", tc_desc->td_version);
        printf("  ->td_numblks  %10u\n", tc_desc->td_numblks);
        printf("  ->td_length   %10u\n", tc_desc->td_length );
        for (tdb_idx = 0; tdb_idx < tc_desc->td_numblks; tdb_idx++, tdb_ptr++)
        {
            printf("  ->td_block[%2d]", tdb_idx);
            if (tdb_ptr->tbd_nameoff != 0 && tdb_ptr->tbd_namelen != 0)
                printf(" (%*.*s)", tdb_ptr->tbd_namelen, tdb_ptr->tbd_namelen,
                       (((char *) tc_desc) + tdb_ptr->tbd_nameoff)
                      );
            printf(":\n");

            printf("    .tbd_blength  %10u\n",  tdb_ptr->tbd_blength);
            printf("    .tbd_address  %#10x\n", tdb_ptr->tbd_address);
            printf("    .tbd_init     %10u\n",  tdb_ptr->tbd_init   );
            printf("    .tbd_namelen  %10u\n",  tdb_ptr->tbd_namelen);
            printf("    .tbd_nameoff  %10u\n",  tdb_ptr->tbd_nameoff);
            printf("    .tbd_init_1   %#10x\n", tdb_ptr->tbd_init_1 );
            printf("    .tbd_init_2   %#10x\n", tdb_ptr->tbd_init_2 );
        }
    }


    /*
     * Print the contents of the task common cross reference data
     * structure.
     */
    static void print_tcom_cref(void);
    static void print_tcom_cref(void)
    {
        unsigned int            tccr_hidx;
        hashlist_entry         *hle_ptr;

        printf("Task common CREF:\n");

        printf("  next_hle_number  %10d\n",  next_hle_number);
        printf("  total_tcom_size  %10d\n",  total_tcom_size);
        printf("  olist_head       %#10x\n", olist_head);
        printf("  olist_tail       %#10x\n", olist_tail);

        for (tccr_hidx = 0; tccr_hidx < TCCR_HASH_SIZE; tccr_hidx++)
        {
            if ((hle_ptr = tccr_hash_table[tccr_hidx]) == NULL)
                continue;
            printf("  tccr_hash_table[%3d]:\n", tccr_hidx);
            do
            {
                printf("    hashlist_entry at %#x:\n", hle_ptr);
                printf("      ->hle_address  %#10x\n", hle_ptr->hle_address);
                printf("      ->hle_size     %10d\n",  hle_ptr->hle_size   );
                printf("      ->hle_number   %10d\n",  hle_ptr->hle_number );
                printf("      ->hle_next     %#10x\n", hle_ptr->hle_next   );
                printf("      ->hle_onext    %#10x\n", hle_ptr->hle_onext  );
                hle_ptr = hle_ptr->hle_next;
            } while (hle_ptr != NULL);
        }
    }
#endif


/*========================================
 * destroy_taskcom_desc
 *
 * Free a task common descriptor.  This is the destructor
 * function for thr_keycreate(3T).
 */
static void
destroy_taskcom_desc(void *tc_desc_addr)
{
    free(tc_desc_addr);
    return;
}
 
 
/*========================================
 * get_taskcom_desc_addr
 *
 * Return the caller's task common descriptor address.
 */
static taskcom_description *
get_taskcom_desc_addr(void)
{
    /*
     * This routine uses thread-specific data rather than macrotask
     * IDs or other such, since it also has to work when it's called
     * by an autotask, for example.  So we have to use something that
     * is unique across the whole program.
     */
    taskcom_description        *tc_desc_addr;

    /*
     * If we haven't created the thread-specific data yet, then
     * do so now.
     */
    if (!tcda_key_initialized) {
        (void) mutex_lock(&tcda_key_lock);
        if (!tcda_key_initialized) {
            (void) thr_keycreate(&tcda_key, destroy_taskcom_desc);
            tcda_key_initialized = 1;
        }
        (void) mutex_unlock(&tcda_key_lock);
    }

    /*
     * Get this thread's task common descriptor address, and return
     * it.
     */
    (void) thr_getspecific(tcda_key, ((void **) &tc_desc_addr));

    return(tc_desc_addr);
}
 
 
/*========================================
 * put_taskcom_desc_addr
 *
 * Return the caller's task common descriptor address.
 */
static void
put_taskcom_desc_addr(taskcom_description *tc_desc_addr)
{

    /*
     * If we haven't created the thread-specific data yet, then
     * do so now.
     */
    if (!tcda_key_initialized) {
        (void) mutex_lock(&tcda_key_lock);
        if (!tcda_key_initialized) {
            (void) thr_keycreate(&tcda_key, destroy_taskcom_desc);
            tcda_key_initialized = 1;
        }
        (void) mutex_unlock(&tcda_key_lock);
    }

    /*
     * Set this thread's task common descriptor address.
     */
    (void) thr_setspecific(tcda_key, tc_desc_addr);

    /*
     * Done.
     */
    return;
}


/*========================================
 * build_taskcom_desc
 *
 * Build the task common description table for a new task and
 * allocate space for its task common blocks, using what is
 * known so far about the program's task common blocks.  The
 * description table may be extended later, if there are one
 * or more task common blocks we don't yet know about.
 */
static taskcom_description *
build_taskcom_desc(void)
{

#   if defined(SHORT_TERM_TASKCOM)

        unsigned int            blk_count;      /* Private block count */
        unsigned int            blk_total_size; /* Private block count */
        taskcom_description    *tc_desc;        /* Task common desc table */
        void                   *com_blocks;     /* Space for task commons */
        unsigned int            tdb_idx;        /* Tc_desc->td_blocks[] idx */
        tcom_blk_desc          *tdb_ptr;        /* Tc_desc->td_blocks[] ptr */
        hashlist_entry         *hle_ptr;        /* Pointer into hash list */

#       if defined(DEBUG)
            printf("==================== build_taskcom_desc()\n");
#       endif

        /*
         * Gather the currently known information.
         */
        _mact_psem(&tccr_lock);                                        /*CRIT*/
        {                                                              /*CRIT*/
                                                                       /*CRIT*/
#           if defined(DEBUG)                                          /*CRIT*/
                print_tcom_cref();                                     /*CRIT*/
#           endif                                                      /*CRIT*/
                                                                       /*CRIT*/
            blk_count      = next_hle_number - 1;                      /*CRIT*/
            blk_total_size = total_tcom_size;                          /*CRIT*/
        }                                                              /*CRIT*/
        _mact_vsem(&tccr_lock);                                        /*CRIT*/

        /*
         * Get space for the description table, and for the common
         * blocks themselves.
         */
        tc_desc    = (taskcom_description *) malloc(TCDESC_SIZE(blk_count));
        com_blocks = malloc(blk_total_size);

        /*
         * Fill in the description table, and while we're at it,
         * initialize the common blocks from the initial values
         * in the real blocks.
         */
        tc_desc->td_version = 2;
        tc_desc->td_numblks = blk_count;
        tc_desc->td_length  = blk_total_size;
        tdb_ptr = tc_desc->td_blocks;
        hle_ptr = olist_head;
        for (tdb_idx = 0;
             tdb_idx < blk_count;
             tdb_idx++, tdb_ptr++, hle_ptr = hle_ptr->hle_onext
            )
        {
            tdb_ptr->tbd_blength = hle_ptr->hle_size;
            tdb_ptr->tbd_address = (unsigned int) com_blocks;
            com_blocks           = (  ((char *) com_blocks)
                                    + tdb_ptr->tbd_blength
                                   );
            memcpy(((void *) tdb_ptr->tbd_address),
                   hle_ptr->hle_address,
                   tdb_ptr->tbd_blength);
            tdb_ptr->tbd_init    = 0;
            tdb_ptr->tbd_namelen = 0;
            tdb_ptr->tbd_nameoff = 0;
            tdb_ptr->tbd_init_1  = 0;
            tdb_ptr->tbd_init_2  = 0;
        }

#   else

        unsigned int            blk_count;      /* Private block count */
        unsigned int            blk_total_size; /* Private block count */
        unsigned int            name_length;    /* Length of names (bytes) */
        taskcom_description    *tc_desc;        /* Task common desc table */
        void                   *com_blocks;     /* Space for task commons */
        unsigned int            tdb_idx;        /* Tc_desc->td_blocks[] idx */
        tcom_blk_desc          *tdb_ptr;        /* Tc_desc->td_blocks[] ptr */
        tcom_blk_desc          *tctdb_ptr;      /* _TASKCOM->td_blocks[] ptr */

#       if defined(DEBUG)
            printf("==================== build_taskcom_desc()\n");
#       endif

        /*
         * Get the number of task common blocks and their total size,
         * as well as the requirements for name space.
         */
        blk_count      = _TASKCOM->td_numblks;
        blk_total_size = _TASKCOM->td_length;
        name_length    = 0;
        for (tdb_idx = 0; tdb_idx < blk_count; tdb_idx++)
            name_length += (_TASKCOM->td_blocks[tdb_idx].namelen + 3) & ~3;

        /*
         * Get space for the description table, and for the common
         * blocks themselves.  If there are no task common blocks,
         * then there's no need to malloc the description table;
         * we can use a static one instead.
         */
        if (blk_count == 0 && name_length == 0)
        {
            tc_desc    = &_TASKCOM;
            com_blocks = NULL;
        }
        else
        {
            tc_desc    = (taskcom_description *)
                         malloc(TCDESC_SIZE(blk_count) + name_length);
            com_blocks = malloc(blk_total_size);

            /*
             * Fill in the description table.
             */
            tc_desc->td_version = _TASKVER;
            tc_desc->td_numblks = blk_count;
            tc_desc->td_length  = blk_total_size;
            if (blk_count > 0)
            {
                tctdb_ptr = _TASKCOM->td_blocks;
                tdb_ptr   =  tc_desc->td_blocks;
                for (tdb_idx = 0;
                     tdb_idx < blk_count;
                     tdb_idx++, tdb_ptr++, tctdb_ptr++
                    )
                {
                    tdb_ptr->tbd_blength = tctdb_ptr->tbd_blength;
                    tdb_ptr->tbd_address = (unsigned int) com_blocks;
                    com_blocks           = (  ((char *) com_blocks)
                                            + tdb_ptr->tbd_blength
                                           );
                    memcpy(((void *) tdb_ptr->tbd_address),
                           ((void *) tctdb_ptr->tbd_address),
                           tdb_ptr->tbd_blength);
                    tdb_ptr->tbd_init    = tctdb_ptr->tbd_init;
                    tdb_ptr->tbd_namelen = tctdb_ptr->tbd_namelen;
                    tdb_ptr->tbd_nameoff = tctdb_ptr->tbd_nameoff;
                    tdb_ptr->tbd_init_1  = tctdb_ptr->tbd_init_1;
                    tdb_ptr->tbd_init_2  = tctdb_ptr->tbd_init_2;
                }

                if (name_length > 0)
                {
                    memcpy(tctdb_ptr, tdb_ptr, name_length);
                }
            }
        }

#   endif


#   if defined(DEBUG)
        print_tcom_desc(tc_desc);
        printf("====================\n");
#   endif

    /*
     * Tell the caller where the task common description table is.
     */
    return(tc_desc);

}


/*
 * _taskcom_address
 *
 * Convert a static common block address to the address of the
 * corresponding task common block for the calling task.
 */
void *
#if defined(SHORT_TERM_TASKCOM)
_taskcom_address(void *tblk_addr, unsigned int tblk_size)
#else
_taskcom_address(unsigned int tblk_num)
#endif
{
    taskcom_description        *tc_desc;        /* Task common desc table    */

#   if defined(SHORT_TERM_TASKCOM)

        unsigned int            tccr_hidx;      /* Tccr_hash_table index     */
        hashlist_entry         *hle_ptr;        /* Pointer into hash list    */
        unsigned int            blk_num;        /* Task common block number  */
        unsigned int            tdb_idx;        /* Tc_desc->td_blocks[] idx  */
        tcom_blk_desc          *blk_ptr;        /* Pointer to new block desc */

#       if defined(DEBUG)
            printf("==================== _taskcom_address(%#x, %d)\n",
                   tblk_addr, tblk_size
                  );
#       endif

        /*
         * Pad the block size to the next doubleword boundary.
         */
        tblk_size = (tblk_size + 7) & ~((unsigned int) 7);

#       if defined(DEBUG)
            printf("New tblk_size %d\n", tblk_size);
            printf("Before lookup: ");
            print_tcom_cref();
#       endif

        /*
         * Find this block in the hash table.  If it's not there,
         * then put it there.
         */
        _mact_psem(&tccr_lock);                                        /*CRIT*/
        {                                                              /*CRIT*/
            tccr_hidx = TCCR_HASH((unsigned int) tblk_addr);           /*CRIT*/
            for (hle_ptr = tccr_hash_table[tccr_hidx];                 /*CRIT*/
                    hle_ptr != NULL                                    /*CRIT*/
                 && hle_ptr->hle_address != tblk_addr;                 /*CRIT*/
                 hle_ptr = hle_ptr->hle_next                           /*CRIT*/
                )                                                      /*CRIT*/
                ;                                                      /*CRIT*/
                                                                       /*CRIT*/
            if (hle_ptr == NULL)                                       /*CRIT*/
            {                                                          /*CRIT*/
                                                                       /*CRIT*/
#           if defined(DEBUG)                                          /*CRIT*/
                printf("Block address %#x not in CREF\n", tblk_addr);  /*CRIT*/
#           endif                                                      /*CRIT*/
                                                                       /*CRIT*/
                hle_ptr = (hashlist_entry *)                           /*CRIT*/
                          malloc(sizeof(hashlist_entry));              /*CRIT*/
                hle_ptr->hle_address       = tblk_addr;                /*CRIT*/
                hle_ptr->hle_size          = tblk_size;                /*CRIT*/
                hle_ptr->hle_number        = next_hle_number;          /*CRIT*/
                hle_ptr->hle_next          = tccr_hash_table           /*CRIT*/
                                            [tccr_hidx];               /*CRIT*/
                hle_ptr->hle_onext         = NULL;                     /*CRIT*/
                tccr_hash_table[tccr_hidx] = hle_ptr;                  /*CRIT*/
                if (olist_tail == NULL)                                /*CRIT*/
                    olist_head             = hle_ptr;                  /*CRIT*/
                else                                                   /*CRIT*/
                    olist_tail->hle_onext  = hle_ptr;                  /*CRIT*/
                olist_tail                 = hle_ptr;                  /*CRIT*/
                                                                       /*CRIT*/
                next_hle_number += 1;                                  /*CRIT*/
                total_tcom_size += tblk_size;                          /*CRIT*/
                                                                       /*CRIT*/
#           if defined(DEBUG)                                          /*CRIT*/
                printf("After add: ");                                 /*CRIT*/
                print_tcom_cref();                                     /*CRIT*/
#           endif                                                      /*CRIT*/
                                                                       /*CRIT*/
            }                                                          /*CRIT*/
        }                                                              /*CRIT*/
        _mact_vsem(&tccr_lock);                                        /*CRIT*/

        /*
         * Get this block's descriptor in the calling task's
         * task common description table.  If the table doesn't
         * have enough space to describe this block, extend it.
         * If the table shows the block as not yet allocated,
         * then allocate it.
         */
        tc_desc = get_taskcom_desc_addr();
        if (tc_desc == NULL)
        {
            tc_desc = build_taskcom_desc();
            put_taskcom_desc_addr(tc_desc);

#           if defined(DEBUG)
                printf("Task had no task common, added: ");
                print_tcom_desc(tc_desc);
#           endif

        }
        blk_num = hle_ptr->hle_number;

        if (tc_desc->td_numblks < blk_num)
        {
            tc_desc = (taskcom_description *)
                      realloc(tc_desc, TCDESC_SIZE(blk_num));
            put_taskcom_desc_addr(tc_desc);

            for (tdb_idx = tc_desc->td_numblks; tdb_idx < blk_num; tdb_idx++)
            {
                tc_desc->td_blocks[tdb_idx].tbd_blength = 0;
                tc_desc->td_blocks[tdb_idx].tbd_address = 0;
                tc_desc->td_blocks[tdb_idx].tbd_init    = 0;
                tc_desc->td_blocks[tdb_idx].tbd_namelen = 0;
                tc_desc->td_blocks[tdb_idx].tbd_nameoff = 0;
                tc_desc->td_blocks[tdb_idx].tbd_init_1  = 0;
                tc_desc->td_blocks[tdb_idx].tbd_init_2  = 0;
            }

            tc_desc->td_numblks = blk_num;

#           if defined(DEBUG)
                printf("Task had no descriptor for this block: ");
                print_tcom_desc(tc_desc);
#           endif
        }

        blk_ptr = &tc_desc->td_blocks[blk_num - 1];
        if (blk_ptr->tbd_address == 0)
        {
            blk_ptr->tbd_blength  = tblk_size;
            blk_ptr->tbd_address  = (unsigned int) malloc(tblk_size);
            memcpy(((void *) blk_ptr->tbd_address),
                   tblk_addr,
                   blk_ptr->tbd_blength);
            tc_desc->td_length   += tblk_size;
        }

#       if defined(DEBUG)
            printf("Done: ");
            print_tcom_desc(tc_desc);
            printf("====================\n");
#       endif

        /*
         * Tell the caller where the private task common block is.
         */
        return((void *) blk_ptr->tbd_address);

#   else

        /*
         * Tell the caller where the private task common block is,
         * building it if it doesn't exist.
         */
        tc_desc = get_taskcom_desc_addr();
        if (tc_desc == NULL)
        {
            tc_desc = build_taskcom_desc();
            put_taskcom_desc_addr(tc_desc);
        }

#       if defined(DEBUG)
            printf("==================== _taskcom_address(%d)\n", tblk_num);
            print_tcom_desc(tc_desc);
            printf("====================\n");
#       endif

        return((void *) (tc_desc->td_blocks[tblk_num - 1].tbd_address));

#   endif
}
