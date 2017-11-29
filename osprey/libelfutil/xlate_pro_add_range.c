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


/*
   xlate_pro_add_range.c
   $Revision: 1.1.1.1 $

   'range' is the same meaning as 'length' and is length in bytes
   of instructions. (not numbers of instructions)





*/

#include "xlateincl.h"

#ifdef _LIBELF_XTND_EXPANDED_DATA
#pragma weak xlate_pro_add_range_xtnd = _xlate_pro_add_range_xtnd
#elif defined(BUILD_OS_DARWIN)
#else
#pragma weak xlate_pro_add_range = _xlate_pro_add_range
#endif

int xlate_pro_add_range(xlate_table_pro     table,
	Elf64_Addr  new_address,
	Elf64_Xword new_range,
	Elf64_Addr  old_address,
	Elf64_Xword old_range)
{
   int retstatus  = XLATE_TB_STATUS_NO_ERROR;
   if(table->tb_magic != PRO_MAGIC_VAL) {
        return XLATE_TB_STATUS_INVALID_TABLE;
   }
   if(table->tb_no_new_entries_allowed ) {
        return XLATE_TB_STATUS_ADD_TOO_LATE;
   }

   if(table->tb_con_table) {
	/* we have a merge */
	retstatus = table->tb_debug_add_range(table,
   		&table->tb_debug_block_head,
    		&table->tb_debug_block_tail,
		new_address,
		new_range,
		old_address,
		old_range,
		&table->tb_debug_highwater);
	if(retstatus == XLATE_TB_STATUS_NO_ERROR) {
		retstatus = _xlate_merge_range(table,
		    	new_address,
                	new_range,
                	old_address,
                	old_range);
	}
   } else {
	/* no merge: initial range table */
	retstatus = table->tb_std_add_range(table,
   		&table->tb_std_block_head,
    		&table->tb_std_block_tail,
		new_address,
		new_range,
		old_address,
		old_range,
		&table->tb_std_highwater);
   }
   return retstatus;
}

/* Here  we do the actual merging
   
   The old-address here is matched with the new_address of
   the consumer table.

   Since this is fairly complicated in detail we present first
   the possible scenarios.

   Here n1, o1 are the new and old addresses given by the caller of
   _xlate_merge. nr1 or1 are the ranges (lengths)
   of n1, o1 respectively.  n2, o2 are the new and old addresses from the
   input consumer table. Between these we do the composition.

   Complexity arises from the fact that nr1 and or1 need not be
   the same, nor need  nr2 or2 be the same.

   Recall that nr1 > or1 means that the last instruction
   address in the old range maps into  multiple instructions
   in the new range.  And vice versa if nr1 < or1

   In the following table we are following the merge logic in that
   we iterate thru the possible or1 addresses and treat each
   such address as a case to handle and create from that case
   a tentative_new_range.  

   When we '_xlate_add_tentative_new_range' we decide if the new stuff
   is really new or a proper extension of a range we are constructing.
   This is how we avoid having an unnecessary explosion of ranges.
   _xlate_add_tentative_new_range() actually creates ranges as necessary.

   Anyway, here is an attempt to illustrate the cases

   Shorthand:
   1-1 means that nr? == or? or we are NOT at the end of 
       one of nr? or or?  In this 1-1 case there is exactly
	one old address for each new address and vice-versa.
 
   nr2>or2 means we are at the end of or2 and there are multiple
       new addresses mapping to the one old addr
   nr2<or2 means we are at the end of nr2 and there are multiple
       old addresses mapping to the new old addr
	So we deal with all the o2 addresses implied at once

   nr1>or1 means we are at the end of or1 and there are multiple
       new addresses mapping to the one old addr
	So we deal with all the new addresses at once.
   nr1<or1 means we are at the end of nr1 and there are multiple
       old addresses mapping to the new old addr


   I call the result column (in the table just below)
   tentative because it specifies
   what is passed to  _xlate_add_tentative_new_range, which
   merges ranges (where that is possible) rather than writing
   every one  out immediately.  This sometimes saves significant
   space in the output translate table (though there is no
   difference in the resulting translations that the output
   table specifies).


   new translation        input from                  result
   input                  consumer xlate table        tentative entries
   n1  nr1   o1 or1       n2 nr2  o2  or2        ==>  n3  nr3  o3 or3
   ----------------       ---------------             ---------------

      1-1                   1-1                       n1   4   o2   4

      1-1                   nr2>or2                   n1   4   o2   4
      1-1                   nr2<or2                   n1   4   o2  length
								  of o2 
								  to end

      nr1<or1                1-1                      n1 4     o2 4

      nr1<or1                nr2>or2                  n1 4     o2 4
      nr1<or1                nr2<or2                  n1 4     o2  length
								  of o2 
								  to end

      nr1>or1                1-1                      n1  length o2  4
                                                          of n1 
                                                          to end
      nr1>or1                nr2>or2                  n1  length o2  4
                                                          of n1 
                                                          to end
      nr1>or1                nr2<or2                  n1  length o2  length
                                                          of n1      of o2 
                                                          to end     to end

      I actually drew boxes like
         n,r=3*4   o,r=5*4
	+--+     +--+
        |  |     |  |
        +--+     |  |
                 |  |
                 +--+
      to signify that o was longer than n to understand this better.
	In this case, o[0-3] is 1-1  o[4-7] is 1-1, o[8-11] is
	  a case where o is longer so the lenght of o to end is
		3*4 or 12 bytes.

      Hope this makes sense.

      If one just willy-nilly added ranges based on the o3 etc there
      would be too many ranges. We don't do that. We merge
      the tentative ranges into real ranges.


      Note the special cases: a merged POxPO==>PO 
      table can be any combo (Theoretically).
      A merged PSxPS==>PS table will be all 1-1.

      Anything else (==> GE) can be any combo input.
*/



void
_xlate_pro_reset_saved_range(xlate_table_pro tab)
{
	tab->tb_non_empty_range = 0;
}

static int
_xlate_finish_saved_range(xlate_table_pro     table)
{
   int         retstatus;
#ifdef DEBUG
#endif
 
   retstatus = table->tb_std_add_range(table,
   		&table->tb_std_block_head,
    		&table->tb_std_block_tail,
		table->tb_save_new_address,
		table->tb_save_new_range,
		table->tb_save_old_address,
		table->tb_save_old_range,
		&table->tb_std_highwater);
   return retstatus;
}

/* All zeros args (except table of course) mean
   write out current working range.

   Once the decision is made (per the long comment above)
   what xlate entries must be created those entries
   are passed in here.   This uses those to build the
   actual xlate entries to be written out eventually.
   If sequences of xlate entries passed in can be merged into
   a single xlate entry that is done.

   Any input which cannot be merged with the
   previous entry (which itself might be a merge of
   several calls' worth) results in  emitting a
   real xlate entry and using the input to save (but
   not yet write) a new one.
*/
int
_xlate_add_tentative_new_range(xlate_table_pro     table,
	Elf64_Addr  new_address,
	Elf64_Xword new_range,
	Elf64_Addr  old_address,
	Elf64_Xword old_range)
{
   int         retstatus  = XLATE_TB_STATUS_NO_ERROR;
   if(table->tb_non_empty_range) {
    if(new_range == 0) {
	/* simply a signal to write out current range */
	retstatus = _xlate_finish_saved_range(table);
        table->tb_non_empty_range = 0;
    } else {
	/* Here we attempt to add the new range onto the existing range:
	** This is the place we attempt to prevent  too many ranges
	** from being created 
	*/
        Elf64_Addr highnew = table->tb_save_new_address + table->tb_save_new_range;
	if (new_address == highnew){
          Elf64_Addr highold = table->tb_save_old_address + table->tb_save_old_range;
	   /* is extending existing new range */
	  if(old_address == highold) {
	     /* is also extending old address. */
	     if(table->tb_save_old_range == table->tb_save_new_range) {
		/*currently even, so ok to extend jointly */
		table->tb_save_old_range += old_range;
		table->tb_save_new_range += new_range;
	     } else {
	       retstatus = _xlate_finish_saved_range(table);
	       table->tb_save_new_address = new_address;
	       table->tb_save_old_address = old_address;
	       table->tb_save_new_range = new_range;
               table->tb_save_old_range = old_range;
               table->tb_non_empty_range = 1;
	     }
	  } else if(old_address == (highold - INSTRUCTION_SIZE)) {
		/* adding to existing old */
	       table->tb_save_new_range += new_range;
		/* do not count matched instruction in old range addition 
		*/
	       table->tb_save_old_range += (old_range - INSTRUCTION_SIZE);
	  } else {
	     retstatus = _xlate_finish_saved_range(table);
	     table->tb_save_new_address = new_address;
	     table->tb_save_old_address = old_address;
	     table->tb_save_new_range = new_range;
             table->tb_save_old_range = old_range;
             table->tb_non_empty_range = 1;
	  }
	} else if(new_address ==  (highnew - INSTRUCTION_SIZE)) {
	  /* Is adding to end of existing new 
	  ** Also means we have the same new instruction address
	  ** as a previous one, which is an ERROR and
	  ** Caught as an input error.
	     So the following code can never be executed.
	  */
          Elf64_Addr highold = table->tb_save_old_address + table->tb_save_old_range;
	  if(old_address == highold) {
		table->tb_save_old_range += old_range;
		table->tb_save_new_range += (new_range - INSTRUCTION_SIZE);
	  } else if(old_address == (highold - INSTRUCTION_SIZE)) {
		/* This is an input error: adding to existing
		** old *and* new. Cannot happen if well formed input.
		*/
		table->tb_save_old_range += old_range- INSTRUCTION_SIZE;
		table->tb_save_new_range += new_range- INSTRUCTION_SIZE;
	  } else {
		/* this is impossible I think!
		*/
	  	retstatus = _xlate_finish_saved_range(table);
	  	table->tb_save_new_address = new_address;
	  	table->tb_save_old_address = old_address;
	  	table->tb_save_new_range = new_range;
          	table->tb_save_old_range = old_range;
          	table->tb_non_empty_range = 1;
	  }
        } else {
	  /* this means there is a gap in the new instruction addresses
	  ** input.
	  ** Not allowed: caught during input.
	  */
	  retstatus = _xlate_finish_saved_range(table);
	  table->tb_save_new_address = new_address;
	  table->tb_save_old_address = old_address;
	  table->tb_save_new_range = new_range;
          table->tb_save_old_range = old_range;
          table->tb_non_empty_range = 1;
	}
    }
   } else {
    /* empty range: we have no existing range data */
    if(new_range == 0) {
	/* nothing to finish up: done.
	*/
    } else {
	table->tb_save_new_address = new_address;
	table->tb_save_old_address = old_address;
	table->tb_save_new_range = new_range;
        table->tb_save_old_range = old_range;
        table->tb_non_empty_range = 1;
    }
   }

   return retstatus;
}
int
_xlate_merge_range(xlate_table_pro     table,
	Elf64_Addr  new_address,
	Elf64_Xword new_range,
	Elf64_Addr  old_address,
	Elf64_Xword old_range)
{
   int         retstatus  = XLATE_TB_STATUS_NO_ERROR;
   Elf64_Addr  tr_address; 
   xlate_block consumer_range;
   Elf64_Xword nindex = 0;
   Elf64_Xword oindex = 0;
   Elf64_Xword consumer_tab_new_address;
   Elf64_Xword last_valid_nindex = new_range - INSTRUCTION_SIZE;
   Elf64_Xword last_valid_oindex = old_range - INSTRUCTION_SIZE;
   Elf64_Xword tentative_new_range;
   Elf64_Xword tentative_old_range;

   while(  oindex < old_range  ) {
      consumer_tab_new_address = old_address + oindex;
      if(oindex == last_valid_oindex &&
		last_valid_nindex > last_valid_oindex) {
	  /* case nr1 > or1 
	  */
	  tentative_new_range = last_valid_nindex - last_valid_oindex
			+ INSTRUCTION_SIZE;
      } else {
	  tentative_new_range = INSTRUCTION_SIZE;
      }

      retstatus = xlate_address(table->tb_con_table,
	 XLATE_ADDRESS_INPUT_NEW_ADDRESS,
	 consumer_tab_new_address,
	 &tr_address,
	 &consumer_range);
      if ( retstatus != XLATE_TB_STATUS_NO_ERROR) {
	return retstatus;
      } else {
        tentative_old_range = INSTRUCTION_SIZE;
	/* last new instr at end of returned consumer range? */
        if(consumer_tab_new_address == (consumer_range.xe_new_address+
			consumer_range.xe_new_range- INSTRUCTION_SIZE)) {
	  /* yes.*/
	  /* is consumer old range > consumer new range? */
	  if(consumer_range.xe_old_range > consumer_range.xe_new_range) {
	    /* case  nr2 < or2
	    */
	    tentative_old_range = 
		consumer_range.xe_old_range - consumer_range.xe_new_range
		+ INSTRUCTION_SIZE;
	  }
	}
	retstatus = _xlate_add_tentative_new_range(table,
		new_address + nindex,
		tentative_new_range,
		tr_address,
		tentative_old_range);
	if(retstatus != XLATE_TB_STATUS_NO_ERROR) {
		return retstatus;
	}
	nindex += tentative_new_range;
	oindex += tentative_old_range;
      }
   }
   return retstatus;
}
