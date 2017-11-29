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

	EVENTS.C

	Utility routines for reading events sections.

*/

#include <elf_abi.h>
#include <elf_mips.h>
#include <sys/types.h>
#include <sys/syssgi.h>
#include <stdio.h>
#include <leb128.h>
#include "events.h"

		/*
		 * Strings for messages about the kind names.
		 */
static char *event_string0[16] = {
    /* 0x0 -> 0x0f */
    "EK_NULL", 
    "EK_ADDR_RESET", 
    "EK_INCR_LOC_EXT", 
    "EK_ENTRY", 
    "EK_IF_ENTRY", 
    "EK_EXIT", 
    "EK_PEND", 
    "EK_SWITCH_32", 
    "EK_SWITCH_64", 
    "EK_DUMMY", 
    "EK_BB_START", 
    "EK_INCR_LOC_UNALIGNED", 
    "EK_GP_PROLOG_HI", 
    "EK_GP_PROLOG_LO", 
    "EK_GOT_PAGE", 
    "EK_GOT_OFST"
};

static char *event_string1[16] = {
    /* 0x10 -> 0x1f */
    "EK_HI", 
    "EK_LO", 
    "EK_64_HIGHEST", 
    "EK_64_HIGHER", 
    "EK_64_HIGH", 
    "EK_64_LOW", 
    "EK_GPREL", 
    "EK_DEF", 
    "EK_FCALL_LOCAL", 
    "EK_FCALL_EXTERN", 
    "EK_FCALL_EXTERN_BIG", 
    "EK_FCALL_MULT", 
    "EK_FCALL_MULT_PARTIAL", 
    "EK_LTR_FCALL", 
    "EK_PCREL_GOT0", 
    "0x1f"
};

static char *event_string3[16] = {
    /* 0x30 -> 0x3f */
    "CK_DEFAULT", 
    "CK_ALIGN", 
    "CK_INSTR", 
    "CK_DATA", 
    "CK_SADDR_32", 
    "CK_GADDR_32", 
    "CK_CADDR_32", 
    "CK_SADDR_64", 
    "CK_GADDR_64", 
    "CK_CADDR_64", 
    "CK_NO_XFORM", 
    "CK_NO_REORDER", 
    "CK_GP_GROUP", 
    "CK_STUBS", 
    "0x3e", 
    "0x3f"
};

static char *event_string5[16] = {
    /* 0x50 -> 0x5f */
    "EK_CK_UNUSED_NONE_0", 
    "EK_CK_UNUSED_NONE_1", 
    "EK_CK_UNUSED_NONE_2", 
    "EK_CK_UNUSED_NONE_3", 
    "EK_CK_UNUSED_NONE_4", 
    "EK_CK_UNUSED_16BIT_0", 
    "EK_CK_UNUSED_16BIT_1", 
    "EK_CK_UNUSED_16BIT_2", 
    "EK_CK_UNUSED_16BIT_3", 
    "EK_CK_UNUSED_16BIT_4", 
    "EK_CK_UNUSED_32BIT_0", 
    "EK_CK_UNUSED_32BIT_1", 
    "EK_CK_UNUSED_32BIT_2", 
    "EK_CK_UNUSED_64BIT_0", 
    "EK_CK_UNUSED_64BIT_1", 
    "EK_CK_UNUSED_64BIT_2"
};

static char *event_string6[16] = {
    /* 0x60 -> 0x6f */
    "EK_CK_UNUSED_64BIT_3", 
    "EK_CK_UNUSED_64BIT_4", 
    "EK_CK_UNUSED_ULEB128_0", 
    "EK_CK_UNUSED_ULEB128_1", 
    "EK_CK_UNUSED_ULEB128_2", 
    "EK_CK_UNUSED_ULEB128_3", 
    "EK_CK_UNUSED_ULEB128_4", 
    "EK_CK_UNUSED_ULEB128_5", 
    "EK_CK_UNUSED_ULEB128_6", 
    "EK_CK_UNUSED_ULEB128_7", 
    "EK_CK_UNUSED_ULEB128_8", 
    "EK_CK_UNUSED_ULEB128_9", 
    "0x6c", 
    "0x6d", 
    "0x6e", 
    "0x6f"
};

static char event_buf[20];


	/*******************************************************
		Function: event_kind_string

		Return the name of the the kind based on the 
		kind number. If I don't reccognize the the kind
		return the hex string for the number.

	 *******************************************************/
char *event_kind_string(__uint32_t kind)
{
__uint32_t range = kind / 16;
__uint32_t index = kind & 0xf;

    switch (range) {
	case 0:
	    return(event_string0[index]);
	case 1:
	    return(event_string1[index]);
	case 3:
	    return(event_string3[index]);
	case 5:
	    return(event_string5[index]);
	case 6:
	    return(event_string6[index]);

	default:
	    sprintf(event_buf, "0x%x", kind);
	    return(event_buf);
    }
}


	/*******************************************************
		Function: event_get_word64

		Put the next 8 bytes of data into an unsigned
		64 bit double word. This only works for big endian.

	 *******************************************************/
__uint64_t
event_get_word64(char *p)
{
__uint64_t i;
union {
    __uint64_t u;
    char a[8];
}x;

    for (i = 0; i < 8; i++)
	x.a[i] = p[i];
	
    return(x.u);
}


	/*******************************************************
		Function: event_get_word32

		Put the next 4 bytes of data into an unsigned
		32 bit word. This only works for big endian.

	 *******************************************************/
__uint32_t
event_get_word32(char *p)
{
__uint32_t i;
union {
    __uint32_t u;
    char a[4];
}x;

    for (i = 0; i<4; i++)
	x.a[i] = p[i];
	
    return(x.u);
}

	/*******************************************************
		Function: event_get_word16

		Put the next 2 bytes of data into an unsigned
		16 bit word. This only works for big endian.

	 *******************************************************/
unsigned short
event_get_word16(char *p)
{
__uint32_t i;
union {
    unsigned short u;
    char a[2];
}x;

    for (i = 0; i<2; i++)
	x.a[i] = p[i];
	
    return(x.u);
}

	/*******************************************************
		Function: event_get_next_rec

		Get information from the events section record
		pointed to by p_event and return a pointer to the
		next record.

	 *******************************************************/
char *
event_get_next_rec(
	char *p_event,		    /* pointer into section */
	__uint32_t offset,		    /* current offset into text section */
	Full_Events *p_full) /* information from record */
{
int len;
int temp_offset;     /* used for EK_FCALL_LOCAL */
unsigned char type, ubyte;
unsigned short ushort;
short sshort;
__uint32_t uword;

    p_full->fevnt_arg1 = 0;
    p_full->fevnt_arg2 = 0;
    p_full->fevnt_arg3 = 0;
    p_full->fevnt_pre_arg1 = 0;
    p_full->fevnt_pre_arg2 = 0;
    p_full->fevnt_pre_arg3 = 0;

    type = *p_event++;
    
		/*
		 * Increment the current offset by the lower 7 bits
		 * of the first byte multiplied by 4.
		 */
    if (type & EK_INCR_LOC) {
	p_full->fevnt_pre_arg1 = (type & 0x7f);
	p_full->fevnt_arg1 = (type & 0x7f) << 2;
	offset += (type & 0x7f) << 2;
	p_full->fevnt_type = EK_INCR_LOC;
	p_full->fevnt_offset = offset;
	return (p_event);
    }

    
    switch((int)type) {

		/*
		 * Resets the offset. The first argument is
		 * a 32 bit offset from the beginning of the
		 * text section this events section is representing.
		 * The second argument is a 16 bit offset to where
		 * the next EK_ADDR_RESET record is.
		 */
	case EK_ADDR_RESET:

	    offset = event_get_word32(p_event);
	    p_full->fevnt_arg1 = offset;
	    p_full->fevnt_pre_arg1 = offset;
	    p_event += 4;

	    ushort = event_get_word16(p_event);
	    p_full->fevnt_arg2 = ushort;
	    p_full->fevnt_pre_arg2 = ushort;
	    p_event += 2;
	    break;

		/*
		 * Increment the current offset. The increment is
		 * an unsigned LEB128 value and is multiplied by 4
		 * after decompression.
		   In 7.3 n32 (at least in that case) for 
		   .MIPS.contents.sdata it has been observed that
		   the uword is really big (effectively negative).
		   Which is erroneous, but happens.
		 */
	case EK_INCR_LOC_EXT:

	    
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg1 = uword<<2;
	    offset += uword<<2;
	    p_full->fevnt_pre_arg1 = uword;
	    break;

		/*
		 * Increment the current offset. The increment is
		 * an unsigned LEB128 value.
		 */
	case EK_INCR_LOC_UNALIGNED: 
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event  += len;
	    p_full->fevnt_arg1 = uword;
	    offset += uword;
	    p_full->fevnt_pre_arg1 = uword;
	    break;


		/*
		 * 16 bit argument is the lower half of the
		 * displacement from gp for a 2 part gp prolog.
		 */
	case EK_GP_PROLOG_HI:	/* fall through */

		/*
		 * 16 bit argument is the upper half of the
		 * displacement from gp for a 2 part gp prolog.
		 */
	case EK_GP_PROLOG_LO:	/* fall through */

		/*
		 * 16 bit argument is the offset from the
		 * base page for a 2 part gp page + offset.
		 */
	case EK_GOT_PAGE:   /* fall through */

		/*
		 * 16 bit argument is the displacement from the
		 * gp for a got page entry for a 2 part gp page + offset.
		 */
	case EK_GOT_OFST:

		/*
		 * 16 bit argument is the LO part of the 32bit address.
		 */
	case EK_HI:	/* fall through */

		/*
		 * 16 bit argument is the HI part of the 32bit address.
		 */
	case EK_LO:	/* fall through */

		/*
		 * 16 bit argument is the pcrel HI, implied total is 
		 * entry zero of GOT 0.
		 */
	case EK_PCREL_GOT0:	/* fall through */

	case EK_CK_UNUSED_16BIT_0:
	case EK_CK_UNUSED_16BIT_1:
	case EK_CK_UNUSED_16BIT_2:
	case EK_CK_UNUSED_16BIT_3:
	case EK_CK_UNUSED_16BIT_4:

	    sshort = (short)event_get_word16(p_event);
	    p_full->fevnt_arg1 = sshort;
	    p_full->fevnt_pre_arg1 = sshort;
	    p_event += 2;
	    break;

		/*
		 * For each of the following 4 cases, the argument is 
		 * the full 64bit address.
		 */
	case EK_64_HIGHEST:
	case EK_64_HIGHER:
	case EK_64_HIGH:
	case EK_64_LOW:
	case EK_CK_UNUSED_64BIT_0:
	case EK_CK_UNUSED_64BIT_1:
	case EK_CK_UNUSED_64BIT_2:
	case EK_CK_UNUSED_64BIT_3:
	case EK_CK_UNUSED_64BIT_4:

	    p_full->fevnt_arg1 = event_get_word64(p_event);
	    p_full->fevnt_pre_arg1 = p_full->fevnt_arg1;
	    p_event += 8;
	    break;

		/* 
		 * argument1 - 1 byte flag
		 * argument2 - 32bit address
		 * argument2 - ULEB128 value
		 */
	case EK_SWITCH_32:
	    p_full->fevnt_arg1 = *p_event++;
	    p_full->fevnt_arg2 = event_get_word32 (p_event);
	    p_event += 4;
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg3 = uword;
	    break;

		/* 
		 * argument1 - 1 byte flag
		 * argument2 - 64bit address
		 * argument2 - ULEB128 value
		 */
	case EK_SWITCH_64:
	    p_full->fevnt_arg1 = *p_event++;
	    p_full->fevnt_arg2 = event_get_word64 (p_event);
	    p_event += 8;
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg3 = uword;
	    break;

		/*
		 * Specifies the alignment for the current location.
		 * The length is an unsigned LEB128 value.
		 * The alignment is a one byte field with a number in the
		 * range of 0 to 63.
		 */
	case CK_ALIGN:
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg1 = uword;
	    p_full->fevnt_pre_arg1 = uword;
	    ubyte = *p_event++;
	    p_full->fevnt_arg2 = ubyte;
	    break;

	case CK_DEFAULT:	/* Default dat type for section. */
	    ubyte = *p_event++;
	    p_full->fevnt_arg2 = ubyte;
	    break;

	case CK_INSTR:		/* Executable instructions. */
	case CK_DATA:		/* Non-address data. */
	case CK_SADDR_32:	/* Simple 32-bit address data. */
	case CK_GADDR_32:	/* GP-relative 32-bit address data. */
	case CK_CADDR_32:	/* Complex 32-bit address data. */
	case CK_SADDR_64:	/* Simple 64-bit address data. */
	case CK_GADDR_64:	/* relative 64-bit address data. */
	case CK_CADDR_64:	/* Complex 64-bit address data. */
	case CK_NO_XFORM:	/* No transformations of instructions allowed. */
	case CK_NO_REORDER:	/* No reordering of instructions allowed. */
	case CK_STUBS:		/* Marks rld stub code */
	case EK_FCALL_MULT:
	case EK_FCALL_MULT_PARTIAL:
	case EK_CK_UNUSED_ULEB128_0:
	case EK_CK_UNUSED_ULEB128_1:
	case EK_CK_UNUSED_ULEB128_2:
	case EK_CK_UNUSED_ULEB128_3:
	case EK_CK_UNUSED_ULEB128_4:
	case EK_CK_UNUSED_ULEB128_5:
	case EK_CK_UNUSED_ULEB128_6:
	case EK_CK_UNUSED_ULEB128_7:
	case EK_CK_UNUSED_ULEB128_8:
	case EK_CK_UNUSED_ULEB128_9:
	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg1 = uword;
	    p_full->fevnt_pre_arg1 = uword;
	    break;

	case EK_BB_START: /* hard to find basic block start */
	case EK_NULL:	/* padding record */
	case EK_ENTRY:	/* Marks an entry point into the subprogram. */
	case EK_EXIT:	/* Marks final exit points of a subprogram. */
	case EK_PEND:	/* Marks the last instruction of a subprogram. */
	case EK_GPREL:  /* Marks a GP-relative reference. */
	case EK_CK_UNUSED_NONE_0:
	case EK_CK_UNUSED_NONE_1:
	case EK_CK_UNUSED_NONE_2:
	case EK_CK_UNUSED_NONE_3:
	case EK_CK_UNUSED_NONE_4:
	    break;

		/*
		 * argument1 - ULEB128 value
		 * argument1 - 4 byte unsigned
		 */
	case CK_GP_GROUP:

	    len = _leb128_unsigned_decode32(p_event,&uword);
	    p_event += len;
	    p_full->fevnt_arg1 = uword;
	    p_full->fevnt_pre_arg1 = uword;

	    p_full->fevnt_arg2 = event_get_word32 (p_event);
	    p_event += 4;

	    break;


		/*
		 * argument1 - 4 byte unsigned
		 */
	case EK_IF_ENTRY:
	case EK_LTR_FCALL:
	case EK_FCALL_LOCAL:
	case EK_CK_UNUSED_32BIT_0:
	case EK_CK_UNUSED_32BIT_1:
	case EK_CK_UNUSED_32BIT_2:

	    temp_offset = event_get_word32(p_event);
	    p_full->fevnt_arg1 = temp_offset;
	    p_full->fevnt_pre_arg1 = temp_offset;
	    p_event += 4;
	    break;

	case EK_FCALL_EXTERN_BIG:
	    ushort = event_get_word16(p_event);
	    p_full->fevnt_arg1 = ushort;
	    p_full->fevnt_pre_arg1 = ushort;
	    p_event += 2;
	    ushort = event_get_word16(p_event);
	    p_full->fevnt_arg2 = ushort;
	    p_full->fevnt_pre_arg2 = ushort;
	    p_event += 2;
	    break;

		/*
		 * argument1 - 2 byte unsigned
		 */
	case EK_FCALL_EXTERN:
	    ushort = event_get_word16(p_event);
	    p_full->fevnt_arg1 = ushort;
	    p_full->fevnt_pre_arg1 = ushort;
	    p_event += 2;
	    break;

	default:
	    /* TODO: Print an error message here. */
	    break;
    }

    p_full->fevnt_offset = offset;
    p_full->fevnt_type = type;

    return(p_event);
}

	/*******************************************************
		Function: event_find_record

		starting at the given point in the section 
		pointed to by p_event, find the next events
		section record of the given type. 

		It is assumed that the current offset comming 
		into this routine will be stored in p_full.
		
		Returns EK_NULL if type not found.

	 *******************************************************/
__uint32_t 
event_find_record(
	char *p_event,		    /* pointer into the events section */
	Full_Events *p_full,	    /* structure for events record info */
	__uint32_t type, 	    /* event section type */
	char *p_end)		    /* end of the current events section */
{

    while (p_event < p_end) {
	p_event = event_get_next_rec(p_event, p_full->fevnt_offset, p_full);
	if (p_full->fevnt_type == type)
	    return((__uint32_t)type);
	 
    }
    
    return (EK_NULL);
}

