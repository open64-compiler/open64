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


#pragma ident "@(#) libu/ffio/cmplz.c	92.2	06/29/99 13:16:47"


/*
 * File compression ala IEEE Computer, June 1984.
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 *	 Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <utime.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "cmpio.h"

#define IBUFL 		32768
#define BITS		16
#define HSIZE		69001		/* 95% occupancy */
#define INIT_BITS 	9		/* Initial number of bits/code */
#define CHECK_GAP 	10000		/* Ratio check interval */

#define ARGVAL() 	(*++(*argv) || (--argc && *++argv))
#define MAXCODE(n_bits)	((1 << (n_bits)) - 1)

/*
 * The next two codes should not be changed lightly, as they
 * must not lie within the contiguous general code space.
 */ 
#define FIRST		257		/* First free entry */
#define	CLEAR		256		/* Table clear output code */

/*  
 * Define third byte of header. Masks 0x40 and 0x20 are free.
 * I think 0x20 should mean that there is a fourth header byte 
 * (for expansion).
 */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80

/*  
 * Fixed size stack used by decompress. System V.4 version 
 * overlays part of htab; this uses another array instead.  
 * But the size of this array is larger than that in Sys 
 * V.4, so we should be safe. 
 */
#define de_stack	((int_64 *) &coutbuf)
#define de_stackend	((int_64 *) &(coutbuf[(2*IBUFL)-1]))

/*
 * A code_int must be able to hold 2**BITS values of
 * type int_64, and also -1.
 */
typedef int_64		code_int;
typedef int_64		count_int;
typedef	unsigned char	char_type;

/*
 * Private globals variables used by the compression and
 * decompression routines .
 */
static char_type magic_header[] = { "\037\235" };  /* 1F 9D */
static int_64 	 n_bits;			   /* Number of bits/code */
static int_64 	 maxbits;		   	   /* User settable max # bits/code */
static code_int  maxcode;			   /* Maximum code, given n_bits */
static code_int  maxmaxcode;	   		   /* Should NEVER generate this code */
static count_int htab [HSIZE];			   /* Hash table */
static code_int  hsize = HSIZE;			   /* Used for dynamic table sizing */

static int_64 	 nmask = MAXCODE(INIT_BITS);	   /* Used for decompression */
static int_64 	 obuf[2*IBUFL/4];		   /* Compressed word buffer */
static int_64 	 cbuf[2*IBUFL];			   /* Decompress: compressed buffer */
						   /* Compress: input buffer */
static int_64 	 coutbuf[2*IBUFL];		   /* Compress: temp file between 
						    * _lz_output() and _lz_pack_output() 
						    * Decompress: de_stack
						    */

static code_int  free_ent = 0;			   /* First unused entry */
static int_64 	 r_off_global;
static int_64 	 b_global;

/*
 * Block compression parameters -- after all codes are 
 * used up, and compression rate changes, start over.
 */
static int	 dsize;				   /* Used for decompression */
static int_64 	 ratio = 0;
static int 	 clear_flg = 0;
static int_64 	 in_count = 1;			   /* Length of input */
static int 	 bytes_out;			   /* Length of compressed output */
static int	 offset;
static int_64 	 block_compress;
static int	 io_output;			   /* Used for compression */
static count_int checkpoint;

/*
 * These variables are used by the compression routine.
 * They contain information about the compressed buffer
 * which is then returned back to the user.
 */
static char *mybuf;			   /* Compressed output buffer */
static int   mybuf_size;		   /* Maximum output buffer size */
static int   mybuf_wrtn;		   /* Write count for output buffer */

/*
 * Function prototypes used in this file.
 */

static int _lz_cl_block();
static void _lz_cl_hash();
static int_64 _lz_pack_output(int n);
static int _lz_output(code_int code);
static int _lz_write_block(int end, int io);
static int _lz_write_buf(char *buf, int num);
static code_int _lz_getcode(cmp_lyr *cinfo, char *inbuf, int *index, int nread);

/**************** Compresssion Routines ****************/

/*
 * Algorithm:  use open addressing double hashing (no chaining) on the prefix
 * code / next character combination.  We do a variant of Knuth's algorithm
 * D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime secondary probe.  
 * Here, the modular division first probe is gives way to a faster exclusive-or 
 * manipulation.  Also do block compression with an adaptive reset, whereby the 
 * code table is cleared when the compression ratio decreases, but after the 
 * table fills.  The variable-length output codes are re-sized at this point, 
 * and a special CLEAR code is generated for the decompressor. Late addition:  
 * construct the table according to file size for noticeable speed improvement 
 * on small files.  
 */
int
_lz_compress(cinfo, buf, nread, outbuf, out_size)
	cmp_lyr	 *cinfo;
	char 	 *buf; 
	int 	  nread; 
	char 	**outbuf; 
	int 	 *out_size;
{
	int	  ret = 0;
	int_64 	 *iptr, a;
	char 	  tmp, inbuf[IBUFL];
	char	 *rtnName = "_lz_compress";

	register int_64   disp, c, fcode;
	register code_int ent, i = 0;
	register code_int hsize_local = hsize;
	register int 	  num_read, hshift;

	TRC_ENTER;

	bytes_out = 0;
	offset = 0;

	/*
	 * Allocate space for the compressed output buffer. The size
	 * will be less or equal to the size of the uncompressed buffer.
	 */
	if ((mybuf = calloc (nread, sizeof(char))) == NULL) {
		ret = FDC_ERR_LZ_BUFALLOC;
		goto done;
	}

	mybuf_wrtn = 0;
	maxbits = BITS;
	mybuf_size = nread;
	block_compress = BLOCK_MASK;

	if (ret = _lz_write_buf((char *) &magic_header[0], 1)) {
		ret = FDC_ERR_LZ_WRITEHDR;
		goto done;
	}
	if (ret = _lz_write_buf((char *) &magic_header[1], 1)) {
		ret = FDC_ERR_LZ_WRITEHDR;
		goto done;
	}
	tmp = maxbits | block_compress;

	if (ret = _lz_write_buf (&tmp, 1)) {
		ret = FDC_ERR_LZ_WRITEMAX;
		goto done;
	}
	
	bytes_out += 3;	/* Account for the 3-byte header magic */

	in_count 	= 1;
	checkpoint 	= CHECK_GAP;
	clear_flg 	= 0;
	ratio 		= 0;
	b_global 	= 0;
	r_off_global 	= 0;
	io_output	= 0;
	maxmaxcode 	= 1 << BITS;
	maxcode 	= MAXCODE(n_bits = INIT_BITS);
	free_ent 	= ((block_compress) ? FIRST : 256 );

	ent = buf[0];
	buf++;
	nread--;

	hshift = 0;
	for (fcode = (int_64) hsize_local; fcode < 65536L; fcode *= 2L)
		hshift++;
	hshift = 8 - hshift; /* Set hash code range bound */

	hsize_local = hsize;
	_lz_cl_hash(); /* Clear hash table */

	while (nread > 0) {
		num_read = (nread > sizeof(inbuf)) ? sizeof(inbuf) : nread;
		memcpy (inbuf, buf, num_read);
		buf += num_read;
		nread -= num_read;

		if (_unpack (inbuf, (int_64 *) cbuf, num_read, -1) < 0) {
			ret = FDC_ERR_LZ_UNPACK;
			goto done;
		}

		iptr = cbuf;

		while (num_read--) {
			c = *iptr++;

			if (c > 255) {
				ret = FDC_ERR_LZ_BADCHAR;
				goto done;
			}	

			in_count++;
			fcode = (int_64) (((int_64) c << maxbits) + ent);
			i = ((c << hshift) ^ ent); /* XOR hashing */

			a = htab[i];

			if (((uint_64)a >> 32) == fcode) {
				ent = a & 0xffffffff;
				continue;
			} else if (a < 0) /* Empty slot */
				goto nomatch;

			disp = hsize_local - i;	/* Secondary hash */

			if (i == 0) {
				disp = 1;
			}

		probe:
			if ((i -= disp) < 0 )
				i += hsize_local;

			a = htab[i];

			if (((uint_64)a >> 32) == fcode ) {
				ent = a & 0xffffffff;
				continue;
			}

			if ( a > 0 ) {
				goto probe;
			}

		nomatch:
			if (ret = _lz_output((code_int) ent)) {
				goto done;
			}
			ent = c;

			if (free_ent < maxmaxcode ) {
				htab[i] = (fcode << 32) | free_ent++;
			} else if (in_count >= checkpoint && block_compress ) {
				if (ret = _lz_cl_block()) {
					goto done;
				}
			}
		}

		if (ret = _lz_output((code_int) -2)) {
			goto done;
		}
	}

	/*
	 * Put out the final code.
	 */
	if (ret = _lz_output((code_int) ent)) {
		goto done;
	}

	if (ret = _lz_output((code_int) -1)) {
		goto done;
	}

	/* 
 	 * Check if we achieve any compression at all.
	 */
	if (bytes_out >= (int) in_count) {
		ret = FDC_ERR_LZ_NOCOMP;
		goto done;
	}

	*outbuf = mybuf;
	*out_size = mybuf_wrtn;

done:
	if (ret != 0) {
		if (mybuf != NULL) {
			free (mybuf);
			mybuf = NULL;
		}

		*outbuf = NULL;
		*out_size = 0;
	}

	TRC_LOG("mybuf_wrtn = %d, ret = %d", mybuf_wrtn, ret);
	TRC_LEAVE;

	return (ret);
}

/*
 * This routine maintains a BITS character long buffer (so that 8 codes
 * will fit in exactly). This uses the VAX insv instruction approach to 
 * insert each code in turn. When the buffer fills up we empty it and 
 * start over.
 *
 * The input is an n_bits-bit integer 'code' argument. If 'code' is -1 
 * then we know we've reached the end of buffer. This assumes that 
 *
 * 	n_bits =< (long)wordsize - 1. 
 *
 * If 'code' is -2, then simply call _lz_write_block() with 0.
 *
 * The code is then written to the compressed output buffer. We also 
 * assume that chars are 8 bits long.
 */
static int
_lz_output(code)
	code_int code;
{
	int ret = 0;

	/* 
	 * We must count 'bytes_out' exactly like the original
	 * compress does. The variable 'offset' is used in this 
	 * computation. 
	 */
	if (code == -1) {
		if (io_output > 0) {
			if (ret = _lz_write_block(1, io_output)) {
				goto done;
			}
			io_output = 0;
		}

		if (offset) {
			/* 
			 * Account for extra bytes up to word boundary.
			 */
			bytes_out += (offset + 7) / 8;
			offset = 0;
		}
	} else if (code == -2) {
		if (io_output > 0) {
			if (ret = _lz_write_block(0, io_output)) {
				goto done;
			}
			io_output = 0;
		}
		/* 
		 * All bytes have been accounted for.
		 */
	} else {
		/* 
		 * Add code to output buffer.
		 */
		coutbuf[io_output] = (int) code;
		io_output++;
		offset += n_bits;	/* Increment for this code */

		if (offset == (n_bits << 3)) {
			/* 
			 * Buffer "full", so original compress would now
			 * do I/O.
			 */
			bytes_out += n_bits;
			offset = 0;
		}

		/*
		 * If the next entry is going to be too big for the 
		 * code size, then increase it, if possible.
		 */
		if (free_ent > maxcode) {
			/*
			 * Write the whole buffer, because the input side
			 * won't discover the size increase until after
			 * it has read it.
			 */
			if (offset) {
				/* 
				 * Original compress outputs entire 
				 * buffer here. 
				 */
				bytes_out += n_bits;
				io_output += 8 - (offset / n_bits);
				offset = 0;
			}

			if (ret = _lz_write_block(0, io_output)) {
				goto done;
			}

			io_output = 0;
			n_bits++;

			maxcode = (n_bits == maxbits) ? 
					maxmaxcode : MAXCODE(n_bits);
		}

		if ((code == CLEAR) && (io_output > 0)) {
			if (offset) {
				/* 
				 * Original compress outputs entire 
				 * buffer here. 
				 */
				bytes_out += n_bits;
				io_output += 8 - (offset / n_bits);
				offset = 0;
			}

			if (ret = _lz_write_block(0, io_output)) {
				goto done;
			}

			io_output = 0;
			maxcode = MAXCODE (n_bits = INIT_BITS);
		}
	}

done:
	return (ret);
}

/*
 * Table clear for block compress.
 */
static int
_lz_cl_block ()
{
	register int_64 rat, ret = 0;

	checkpoint = in_count + CHECK_GAP;

	/*
	 * Shift will overflow.
	 */
	if(in_count > 0x007fffff) {
		rat = (unsigned long int) bytes_out >> 8;
		rat = (rat == 0) ? 0x7fffffff : in_count / rat;
	} else {
		/* 8 fractional bits */
		rat = (in_count << 8) / bytes_out; 
	}

	if ( rat > ratio ) {
		ratio = rat;
	} else {
		ratio = 0;
		 _lz_cl_hash();
		free_ent = FIRST;
		clear_flg = 1;

		ret = _lz_output((code_int) CLEAR);
	}

	return (ret);
}

/*
 * This routine resets the code table.
 */
static void
_lz_cl_hash()
{
	int i;

	for (i = 0; i < hsize; i++) {
		htab[i] = -1;
	}
}

/*
 *  This merges 4 code words into a register before putting it into
 *  one or two words.  This depends on the code word being less than
 *  or equal to 16-bits and the machine word being 64-bits. The loop
 *  pre-reads 4 code words to allow overlap on the reads.
 */
static int_64
_lz_pack_output(n)
	int 	 n;
{
	int_64 b = b_global;
	int_64 r = r_off_global;
	int_64 nb = n_bits;
	int_64 c0 = coutbuf[0];
	int_64 c1, c2, c3, io = 0;
	int    i = 0;

	if (n >= 4) {
		c1 = coutbuf[1];
		c2 = coutbuf[2];
		c3 = coutbuf[3];

#pragma align
		for (; i < (n-3); i += 4) {
			int c;
			c = c0 | (c1 << nb) | (c2 << 2*nb) | (c3 << 3*nb);

			c0 = coutbuf[i+4];
			c1 = coutbuf[i+5];
			c2 = coutbuf[i+6];
			c3 = coutbuf[i+7];

			b |= c << r;

			if ((r += 4*nb) >= 8*sizeof(int_64)) {
				obuf[io] = b;
				io++;
				r -= 8*sizeof(int_64);

				b = ((4*nb - r) == 8*sizeof(int_64)) ?
					0 : (uint_64) c >> (4*nb - r);

			}
		}
	}

	for (; i < n; i++) {
		b |= c0 << r;
		if ((r += nb) >= 8*sizeof(int_64)) {
			obuf[io] = b;
			io++;
			r -= 8*sizeof(int_64);
			b = (uint_64)c0 >> (nb - r);
		}
		c0 = coutbuf[i+1];
	}

	r_off_global = r;
	b_global = b;

	return (io);
}

/*
 * Write characters to compressed output buffer.
 */
static int
_lz_write_buf(buf, num)
	char 	*buf; 
	int 	 num;
{
	int	ret = 0;

	if ((mybuf_wrtn + num) <= mybuf_size) {
		memcpy((mybuf + mybuf_wrtn), buf, num);
		mybuf_wrtn += num;
		goto done;
	}

	ret = FDC_ERR_LZ_NOCOMP;

done:
	return (ret);
}

/*
 * 'end' is 1 if this is the end of buffer.
 * 'io' is the number of output entries in the buffer.
 */
static int
_lz_write_block(end, io)
	int 	 end; 
	int 	 io;
{
	int 	i, nc;
	int_64 	n, ret;

	n = _lz_pack_output(io);    /* pack output data */
	nc = n*sizeof(int_64);

	if (end > 0 && r_off_global) {
		obuf[n] = b_global;
		n++;
		nc += (r_off_global + 7)/8;
		r_off_global = 0;
	}

	/* Reverse bytes */

	for (i = 0; i < n; i++) {
		int a = obuf[i];
		a = ((unsigned int)(a & 0xff00ff00ff00ff00)>>8) |
		    ((a << 8) & 0xff00ff00ff00ff00);
		a = ((unsigned int)(a & 0xffff0000ffff0000)>>16) |
		    ((a << 16) & 0xffff0000ffff0000);
		a = ((unsigned int)(a & 0xffffffff00000000)>>32) |
		    ((a << 32) & 0xffffffff00000000);
		obuf[i] = a;
	}

	ret = _lz_write_buf((char *)obuf, nc);

	return (ret);
}

/**************** DeCompression Routines ****************/

/*
 * Decompress stdin to stdout.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.  The tables used herein are shared
 * with those of the compress() routine.  See the definitions above.
 */
int
_lz_decompress(cinfo, inbuf, nread, outsize, outbuf)
	cmp_lyr	*cinfo;
	char	*inbuf;
	int	 nread;
	int	 outsize;
	char	**outbuf;
{
	register int_64   *stackp;
	register code_int  code, oldcode, incode;

	char	c;
	int_64  finchar;
	int_64	a, *p = cbuf;
	int 	ret = 0, index = 0;
	char	*rtnName = "_lz_decompress";

	TRC_ENTER;

	TRC_LOG("cmp size %d, uncmp size %d", nread, outsize);

	dsize = 0;
	offset = 0;
	clear_flg = 0;
	*outbuf = NULL;

	if ((inbuf[0] != (magic_header[0] & 0xFF)) ||
	    (inbuf[1] != (magic_header[1] & 0xFF))) {
		ret = FDC_ERR_LZ_BADHEADER;
		goto done;
	}

	maxbits = inbuf[2];        
	block_compress = maxbits & BLOCK_MASK;
	maxbits &= BIT_MASK;
	maxmaxcode = 1 << maxbits;
	if(maxbits > BITS) {
		ret = FDC_ERR_LZ_BADMAXBITS;
		goto done;
	}
	inbuf += 3;
	nread -= 3;

	/*
	 * Initialize the first 256 entries in the hash table.
	 */
	maxcode = MAXCODE(n_bits = INIT_BITS);
	nmask = MAXCODE(INIT_BITS);

	for (code = 255; code >= 0; code--) {
		htab[code] = (char_type) code;
	}

	free_ent = ((block_compress) ? FIRST : 256 );
	finchar = oldcode = _lz_getcode(cinfo, inbuf, &index, nread);

	if(oldcode == -1) {	/* End of buffer already? */
		goto done;	/* Get out of here */
	}

	if ((mybuf = calloc (outsize, sizeof(char))) == NULL) {
		ret = FDC_ERR_LZ_BUFALLOC;
		goto done;
	}
	mybuf_size = outsize;
	mybuf_wrtn = 0;

	c = (char) finchar;
	if (_lz_write_buf((char *) &c, 1)) {
		ret = FDC_ERR_LZ_WRITEMAX;
		goto done;
	}

	stackp = de_stack;

	while ((code = _lz_getcode(cinfo, 
				inbuf, &index, nread)) > -1 ) {

		if ((code == CLEAR) && block_compress) {
			for (code = 255; code >= 0; code--)
				htab[code] &= 0xff;

			clear_flg = 1;
			free_ent = FIRST - 1;

			/*
			 * If code is zero we reached an untimely death.
			 */
			if ((code = _lz_getcode(cinfo, 
				inbuf, &index, nread)) == -1)
				break;
		}

		incode = code;

		/* Special case for KwKwK string. */
		if (code >= free_ent) {
			*stackp++ = finchar;
			code = oldcode;
		}

		/*
		 * Generate output characters in reverse order
		 */
		a = htab[code];
		while (code >= 256) {
			if (stackp > de_stackend) {
				ret = FDC_ERR_LZ_BADINPUT;
				goto done;
			}
			*stackp++ = a & 0xff;
			code = (uint_64)a >> 32;
			a = htab[code];
		}
		a = finchar = a & 0xff;

		/* And put them out in forward order */
		do {
			*p++ = a;
			a = *--stackp;
		} while (stackp >= de_stack);
		stackp = de_stack;

		if (p > (&cbuf[IBUFL])) {
			(void) _pack((int_64 *) cbuf, 
				     (char *) obuf, (p - cbuf), -1);
			if (ret = _lz_write_buf((char *)obuf, 
					  sizeof(char) * (p - cbuf))) {
				goto done;
			}
			p = cbuf;
		}

		/* Generate the new entry. */
		if ((code=free_ent) < maxmaxcode) {
			htab[code] = finchar | (oldcode << 32);
			free_ent = code+1;
		} 

		/* Remember previous code. */
		oldcode = incode;
	}

	if (p > cbuf) {
		(void) _pack((int_64 *) cbuf, (char *) obuf, (p - cbuf), -1);
		if (ret = _lz_write_buf((char *)obuf, 
					sizeof(char) * (p - cbuf))) {
			goto done;
		}
	}

	*outbuf = mybuf;

	/*
	 * Verify that the number of bytes written is the same
	 * as that given for the size of the uncompressed buffer.
	 */
	if (mybuf_wrtn != outsize) {
		ret = FDC_ERR_LZ_UNCMPLEN;
	}
done:
	if (ret != 0) {
		*outbuf = NULL;

		if (mybuf != NULL) {
			free (mybuf);
			mybuf = NULL;
		}
	}

	TRC_LOG("mybuf_wrtn %d, mybuf_size %d", 
		mybuf_wrtn, mybuf_size);
	TRC_LEAVE;

	return (ret);
}

/*
 * Read one code from the standard input. If we reach the 
 * end of the input buffer this routine returns -1.
 */
static code_int
_lz_getcode(cinfo, inbuf, index, nread)
	cmp_lyr	*cinfo;
	char	*inbuf;
	int	*index;
	int	 nread;
{
	register code_int 	code;
	static uint_64 		tbuf[64];
	register int_64 	r_off;
	int 			n, i;

	if (clear_flg > 0 || offset >= dsize || free_ent > maxcode) {
		/*
		 * If the next entry will be too big for the current code
		 * size, then we must increase the size.  This implies 
		 * reading a new buffer full, too.
		 */
		if (free_ent > maxcode) {
			n_bits++;
			nmask = MAXCODE(n_bits);

			maxcode = (n_bits == maxbits) ? 
				maxmaxcode : MAXCODE(n_bits);
		}

		if (clear_flg > 0) {
			maxcode = MAXCODE (n_bits = INIT_BITS);
			nmask = maxcode;
			clear_flg = 0;
		}

		dsize = n_bits;
		if ((*index + n_bits) > nread) {
			if (*index >= nread) {
				code = -1; /* End of buffer */
				goto done;
			}
			dsize = nread - *index;
		}
	
		memcpy((void *)tbuf, (void *)(inbuf + *index), dsize);
		*index += dsize;

		/* Reverse bytes */
		n = (dsize - 1)/sizeof(int_64) + 1;

		for (i = 0; i < n; i++) {
			int a = tbuf[i];
			a = ((uint_64)(a & 0xff00ff00ff00ff00)>>8) |
			    ((a << 8) & 0xff00ff00ff00ff00);
			a = ((uint_64)(a & 0xffff0000ffff0000)>>16) |
			    ((a << 16) & 0xffff0000ffff0000);
			a = ((uint_64)(a & 0xffffffff00000000)>>32) |
			    ((a << 32) & 0xffffffff00000000);
			tbuf[i] = a;
		}
		offset = 0;

		/* Round size down to integral number of codes */
		dsize = (dsize << 3) - (n_bits - 1);
	}

	i = offset >> 6;
	r_off = offset & 0x3f;
	code = (tbuf[i] >> r_off) & nmask;

	if ((r_off += n_bits) > 8*sizeof(int_64)) {
		r_off -= 8*sizeof(int_64);
		code |= ((tbuf[i+1] << (n_bits - r_off)) & nmask);
	}

	offset += n_bits;

done:
	return code;
}
