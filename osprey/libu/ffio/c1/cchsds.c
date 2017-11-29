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


static char USMID[] = "@(#) libu/ffio/c1/cchsds.c	92.0	10/08/98 14:57:41";

#include <ffio.h>
#include <errno.h>
#include <liberrno.h>
#include "../cchio.h"
#include <cray/nassert.h>

/*
 * _mem_fr_sds moves data into user memory from a secondary data segment (SDS).
 *
 * Returns 0 on normal return, or else -1 with error code in errno.
 */
_mem_fr_sds(
bitptr  ubuf,		/* user buffer to receive data */
bitptr  sdsaddr,	/* SDS bit address of data */
int     nbits		/* number of bits to move */
)
{
	long sds_bit_offset;
	char *ucaddr;
	long *uwaddr;
	int  ret;
	
	if (nbits & (BITPBLOCK - 1)) {
		errno = FDC_ERR_GRAN;	/* must be block multiple */
		return -1;
	}

	ucaddr	 = BPTR2CP(ubuf);
	uwaddr	 = BPTR2WP(ubuf);
	if (ucaddr != (char*)uwaddr) {
		errno = FDC_ERR_SDSWB;	/* must be word-aligned */
		return -1;
	}

	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	if (sds_bit_offset & (BITPBLOCK - 1)) {
		errno = FDC_ERR_GRAN;	/* must be block multiple */
		return -1;
	}

	ret = ssread(uwaddr, BITS2BLOCKS(sds_bit_offset), BITS2BLOCKS(nbits));
	if (ret == -1)
		errno = FDC_ERR_SDSIO;
	return ret;
}

/*
 * _sds_fr_mem moves data into SDS from user memory.
 *
 * Returns 0 on normal return, or else -1 with error code in errno.
 */
_sds_fr_mem(
bitptr  sdsaddr,	/* SDS bit address where data will be received */
bitptr  ubuf,		/* user buffer containing data */
int     nbits		/* number of bits to move */
)
{
	long sds_bit_offset;
	char *ucaddr;
	long *uwaddr;
	int  ret;
	
	if (nbits & (BITPBLOCK - 1)) {
		errno = FDC_ERR_GRAN;	/* must be block multiple */
		return -1;
	}

	ucaddr	 = BPTR2CP(ubuf);
	uwaddr	 = BPTR2WP(ubuf);
	if (ucaddr != (char*)uwaddr) {
		errno = FDC_ERR_SDSWB;	/* must be word-aligned */
		return -1;
	}

	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	if (sds_bit_offset & (BITPBLOCK - 1)) {
		errno = FDC_ERR_GRAN;	/* must be block multiple */
		return -1;
	}

	ret = sswrite(uwaddr, BITS2BLOCKS(sds_bit_offset), BITS2BLOCKS(nbits));
	if (ret == -1)
		errno = FDC_ERR_SDSIO;
	return ret;
}
/*
 * _sdsset sets all bytes in an SDS data segment to a specified value.
 *
 * Both byte_offset and nbytes must be 512-word block multiples.
 *
 * Returns byte_offset on normal return.  If an error is encountered -1 is 
 * returned with the error code in errno.
 */
_sdsset(
int byte_offset,	/* Byte offset into SDS area */
int value,		/* Value to which all bytes should be set */
int nbytes)		/* Number of bytes to set */
{
#define _BUFFER_BLOCKS	10

	char bytebuf[_BUFFER_BLOCKS * BYTPBLOCK]; /* must be word-aligned */
	int  nblocks;
	int  blk_offset;
	int  ret;
	int  this_chunk;
	int  left;
	
	/* check that byte_offset and nbytes are on block boundaries */
	if ((byte_offset | nbytes) & (BYTPBLOCK - 1)) {
		errno = FDC_ERR_GRAN;
		return -1;
	}

	nblocks    = BYTES2BLOCKS(nbytes);
	blk_offset = BYTES2BLOCKS(byte_offset);
	
	(void)memset(bytebuf, value, MIN(nblocks, _BUFFER_BLOCKS) * BYTPBLOCK);

	left  = nblocks;
	while (left > 0) {
		this_chunk = MIN(left, _BUFFER_BLOCKS);

		ret = sswrite((long)bytebuf, blk_offset, this_chunk); 
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}

		left       -= this_chunk;
		blk_offset += this_chunk;
	}
	return byte_offset;
}
/*
 * _sdsset_any sets all bytes in an SDS data segment to a specified value.
 *
 * Unlink _sdsset, byte_offset and nbytes can have any value.
 *
 * Returns byte_offset on normal return.  If an error is encountered -1 is 
 * returned with the error code in errno.
 */

_sdsset_any(
int byte_offset,	/* Byte offset into SDS area */
int value,		/* Value to which all bytes should be set */
int nbytes)		/* Number of bytes to set */
{
#define _BUFFER_BLOCKS	10

	char bytebuf[_BUFFER_BLOCKS * BYTPBLOCK]; /* must be word-aligned */
	int  nblocks;
	int  blk_offset;
	int  ret;
	int  this_chunk;
	int  left;
	int  lblk_offset;	
	int  headbytes,tailbytes;
	int  orig_byte_offset;

	orig_byte_offset = byte_offset;
	nblocks    = BYTES2BLOCKS(nbytes);
	blk_offset = BYTES2BLOCKS(byte_offset);

	/* if byte_offset is not on a block boundary */
	if ((byte_offset) & (BYTPBLOCK - 1)) {

		headbytes = BYTPBLOCK - (byte_offset & (BYTPBLOCK -1 ));
			
		if (headbytes > nbytes)
			headbytes = nbytes;
		ret = ssread(bytebuf, blk_offset, 1);
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}
		memset(bytebuf + (byte_offset & (BYTPBLOCK-1)), value, headbytes);
		ret = sswrite(bytebuf, blk_offset, 1);	
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}
		nbytes -= headbytes;
		nblocks = BYTES2BLOCKS(nbytes);
		byte_offset += headbytes;
		blk_offset = BYTES2BLOCKS(byte_offset);
	}
	if (nbytes & (BYTPBLOCK - 1)){
		/* these will be the bytes left over at the end */
		tailbytes = nbytes & (BYTPBLOCK - 1);
		lblk_offset = BYTES2BLOCKS(byte_offset + nbytes -1);
		ret = ssread(bytebuf, lblk_offset, 1);
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}
		memset(bytebuf, value, tailbytes);
		ret = sswrite(bytebuf, lblk_offset, 1);
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}
		nbytes -= tailbytes;
	}
	if (nbytes & (BYTPBLOCK -1 )){
		abort();
	}
	
	(void)memset(bytebuf, value, MIN(nblocks, _BUFFER_BLOCKS) * BYTPBLOCK);

	left  = nblocks;
	while (left > 0) {
		this_chunk = MIN(left, _BUFFER_BLOCKS);

		ret = sswrite((long)bytebuf, blk_offset, this_chunk); 
		if (ret == -1) {
			errno = FDC_ERR_SDSIO;
			return -1;
		}

		left       -= this_chunk;
		blk_offset += this_chunk;
	}
	return orig_byte_offset;
}
/*
 * _any_mem_fr_sds moves data into user memory from a secondary data segment (SDS).
 *
 * unlike _mem_fr_sds, _any_mem_fr_sds handles moving a number of bits that
 * may not be a multiple of 512.
 *
 * Returns 0 on normal return, or else -1 with error code in errno.
 */
_any_mem_fr_sds(
bitptr  ubuf,		/* user buffer to receive data */
bitptr  sdsaddr,	/* SDS bit address of data */
int     nbits		/* number of bits to move */
)
{

	int sds_bit_offset;	
	int sds_bit_offset_blk;	
	int rbits;
	char localbuf[BYTPBLOCK];
	bitptr locptr;
	long *uwaddr;
	char *ucaddr;

	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	if (sds_bit_offset & (BITPBLOCK -1)) {
		/* The sds address is not on a block boundary. */
		/* Read data from sds to a local buffer. Copy the */
		/* appropriate part of the local buffer to user's memory. */
		sds_bit_offset_blk = (sds_bit_offset & ~(BITPBLOCK - 1));
		if(ssread((int *)localbuf, BITS2BLOCKS(sds_bit_offset_blk), 1) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
		rbits = MIN(nbits, BITPBLOCK - (sds_bit_offset -
			sds_bit_offset_blk));
		locptr = CPTR2BP(localbuf);
		SET_BPTR(locptr, INC_BPTR(locptr, sds_bit_offset - sds_bit_offset_blk));
		MOV_BITS(ubuf, locptr, rbits);
		SET_BPTR(ubuf, INC_BPTR(ubuf, rbits));
		nbits -= rbits;
		SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, rbits));
		if (nbits == 0)
			return(0);
		
		/* Verify that our sds address is now on a block boundary */
		assert (((SUBT_BPTR(sdsaddr, WPTR2BP(0))) & (BITPBLOCK -1)) == 0);
	}
	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	uwaddr	 = BPTR2WP(ubuf);
	ucaddr	 = BPTR2CP(ubuf);
	if ((nbits & (BITPBLOCK-1)) || (ucaddr != (char *)uwaddr)){
		int  left;

		/* Either we are not reading in a multiple of blocks or */
		/* the user's address is not word-aligned. */
		/* Round nbits down to a block boundary and */
		/* move those to user's memory. */

		locptr = CPTR2BP(localbuf);	
		rbits = nbits & ~(BITPBLOCK-1);
		if (rbits) {
			if (ucaddr != (char*)uwaddr) {
				/* ubuf is not word aligned. */
				/* Read the data from sds into a local */
				/* buffer and copy to the user's memory */
				left = rbits;
				sds_bit_offset_blk = BITS2BLOCKS(sds_bit_offset);
				while (left > 0) {
				   if (ssread((int *)localbuf,
				       sds_bit_offset_blk, 1) == -1) {
					errno = FDC_ERR_SDSIO;
					return(-1);
				   }
				   MOV_BITS(ubuf, locptr, BITPBLOCK);	
				   SET_BPTR(ubuf, INC_BPTR(ubuf, BITPBLOCK));
				   SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, BITPBLOCK));
				   sds_bit_offset_blk++;
				   left-= BITPBLOCK;
				}
			}
			else {
				if (ssread(uwaddr, BITS2BLOCKS(sds_bit_offset),
				   BITS2BLOCKS(rbits)) == -1) {
					errno = FDC_ERR_SDSIO;
					return(-1);
				}
				SET_BPTR(ubuf, INC_BPTR(ubuf, rbits));
				SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, rbits));
			}
			sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
		}
		/* get last block into local memory and */
		/* transfer to 	user's memory */
		if (ssread((int *)localbuf, BITS2BLOCKS(sds_bit_offset), 1) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
		assert((nbits - rbits) < BITPBLOCK);
		MOV_BITS(ubuf, locptr, nbits - rbits);	
	}
	else {
		if(ssread(uwaddr, BITS2BLOCKS(sds_bit_offset), BITS2BLOCKS(nbits)) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
	}
	return(0);
}
/*
 * _any_sds_fr_mem moves data into a secondary data segment (SDS) from
 *	user memory
 *
 * unlike _sds_fr_mem, _any_sds_fr_mem handles moving a number of bits that
 * may not be a multiple of 512.
 *
 * Returns 0 on normal return, or else -1 with error code in errno.
 */
_any_sds_fr_mem(
bitptr  sdsaddr,	/* SDS bit address of data */
bitptr  ubuf,		/* user buffer to receive data */
int     nbits		/* number of bits to move */
)
{

	int sds_bit_offset;	
	int sds_bit_offset_blk;	
	int rbits;
	char localbuf[BYTPBLOCK];
	bitptr locptr;
	long *uwaddr;
	char *ucaddr;

	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	if (sds_bit_offset & (BITPBLOCK -1)) {
		/* The sds address is not on a block boundary. */
		/* Read data from sds to a local buffer. Copy the */
		/* user's memory to the appropriate part of the local */
		/* buffer, and write it back out to sds. */
		sds_bit_offset_blk = (sds_bit_offset & ~(BITPBLOCK - 1));
		if (ssread((int *)localbuf, BITS2BLOCKS(sds_bit_offset_blk), 1) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
		rbits = MIN(nbits, BITPBLOCK - (sds_bit_offset -
			sds_bit_offset_blk));
		locptr = CPTR2BP(localbuf);
		SET_BPTR(locptr, INC_BPTR(locptr, sds_bit_offset - sds_bit_offset_blk));
		MOV_BITS(locptr, ubuf, rbits);
		SET_BPTR(ubuf, INC_BPTR(ubuf, rbits));
		nbits -= rbits;
		if(sswrite((int *)localbuf, BITS2BLOCKS(sds_bit_offset_blk), 1) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
		SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, rbits));
		if (nbits == 0)
			return(0);
		
		assert(((SUBT_BPTR(sdsaddr, WPTR2BP(0))) & (BITPBLOCK -1)) == 0);
	}
	sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
	uwaddr	 = BPTR2WP(ubuf);
	ucaddr	 = BPTR2CP(ubuf);
	if ((nbits & (BITPBLOCK-1)) || (ucaddr != (char *)uwaddr)){
		int left;

		locptr = CPTR2BP(localbuf);	

		/* round down nbits to a block boundary */
		rbits = nbits & ~(BITPBLOCK-1);
		if (rbits) {
			if (ucaddr != (char*)uwaddr) {
				/* ubuf is not word aligned. */
				left = rbits;
				sds_bit_offset_blk = BITS2BLOCKS(sds_bit_offset);
				while (left > 0) {
				   if( ssread((int *)localbuf,
				      sds_bit_offset_blk, 1) == -1) {
				      errno = FDC_ERR_SDSIO;
				      return(-1);
				   }
				   MOV_BITS(locptr, ubuf, BITPBLOCK);	
				   SET_BPTR(ubuf, INC_BPTR(ubuf, BITPBLOCK));

				   if( sswrite((int *)localbuf,
				      sds_bit_offset_blk, 1) == -1) {
				      errno = FDC_ERR_SDSIO;
				      return(-1);
				   }
				   SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, BITPBLOCK));
				   sds_bit_offset_blk++;
				   left-= BITPBLOCK;
				
				}
			}
			else {
				if (_sds_fr_mem(sdsaddr, ubuf, rbits) == -1) {
					return(-1);
				}
				SET_BPTR(ubuf, INC_BPTR(ubuf, rbits));
				SET_BPTR(sdsaddr, INC_BPTR(sdsaddr, rbits));
			}
                        sds_bit_offset = SUBT_BPTR(sdsaddr, WPTR2BP(0));
		}
		/* Get last block into local memory. Merge in user's memory */
		/* and write it back out to sds. */
	        if( ssread((int *)localbuf, BITS2BLOCKS(sds_bit_offset), 1) == -1) {
		      errno = FDC_ERR_SDSIO;
		      return(-1);
		}
		MOV_BITS(locptr, ubuf, nbits - rbits);	
	        if( sswrite((int *)localbuf, BITS2BLOCKS(sds_bit_offset), 1) == -1) {
		        errno = FDC_ERR_SDSIO;
			return(-1);
		}
	}
	else {
		if(sswrite(uwaddr, BITS2BLOCKS(sds_bit_offset), BITS2BLOCKS(nbits)) == -1) {
			errno = FDC_ERR_SDSIO;
			return(-1);
		}
	}
	return(0);
}
