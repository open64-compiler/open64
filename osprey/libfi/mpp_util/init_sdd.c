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


#pragma ident "@(#) libfi/mpp_util/init_sdd.c	92.1	07/13/99 10:45:02"

#include "f90_macros.h"

/**************************************************************************
 *
 * _init_sdd() - This routine initializes an sdd to distribute
 *               each dimension of the result array as specified
 *               by the caller.
 *
 * Input:
 *      sdd     - A pointer to an shared data descriptor
 *      base    - A pointer to the base address of the array
 *                that is being distributed.
 *      ndim    - The number of dimensions of the array that
 *                is being distributed.
 *      weights - A list of processor weight ratios for each
 *                dimension. Usually, set each entry to 2 except
 *                for the dimensions that you would like to be
 *                distributed degenerate; set these to 0.
 *      blknum  - A list containing the number of blocks that
 *                will be used to lay out each dimension. If a
 *                dimension is BLOCK(N), then this will be
 *                extent/N. If a dimension is BLOCK, then this
 *                will just be extent.
 *      extents - A list containing the extents for each dimension.
 *
 *************************************************************************/

void
_init_sdd(void *sdd, void *base, long ndim, long *weights, long *blknum,
          long *extents)
{
        long tmp1, tmp2, i, j;
        long pe_bcnts[7], blksize[7];
        long cyc_ebp[7], blk_ebp[7];

        /*
         * Set the base address field in the sdd to the base pointer.
         */

        _sdd_write_base(sdd,base);

        /*
         * Call the routine that will figure out an appropriate
         * distribution for each dimension base on the information
         * in arrays weights and blknum. On return, pe_bcnts will
         * contain the log2 of the number of processors to assign
         * to each dimension.
         */

        _MPLAPD(_N_PES, ndim, weights, blknum, pe_bcnts);

        /*
         * Compute the log2 of the block size for each dimension by:
         *      blksize = LOG2(extents/2**pe_bcnts)
         */

#pragma CRI shortloop
        for (i=0; i < ndim; i++) {
            blksize[i] = 1;
            for (j=0; j < pe_bcnts[i]; j++) {
                blksize[i] *= 2;
            }
            blksize[i] = LOG2(extents[i]/blksize[i]);
        }

        /*
         * Compute the cycle end bit position (cyc_ebp) and the block
         * end bit position (blk_ebp) for each dimension.
         */

        cyc_ebp[0] = LOG2(extents[0]);
        blk_ebp[0] = blksize[0];
#pragma CRI shortloop
        for (i=1; i < ndim; i++) {
            cyc_ebp[i] = LOG2(extents[i]) + cyc_ebp[i-1];
            blk_ebp[i] = blksize[i] + cyc_ebp[i-1];
        }

        /*
         * Fill in the cycle end bit position, the PE bit count, and
         * the block end bit position for each dimension into the
         * shared data descriptor.
         */

        _sdd_write_cyc_ebp(sdd,1,cyc_ebp[0]);
        _sdd_write_pe_bcnt(sdd,1,pe_bcnts[0]);
        _sdd_write_blk_ebp(sdd,1,blk_ebp[0]);
        if (ndim > 1) {
            _sdd_write_cyc_ebp(sdd,2,cyc_ebp[1]);
            _sdd_write_pe_bcnt(sdd,2,pe_bcnts[1]);
            _sdd_write_blk_ebp(sdd,2,blk_ebp[1]);
            if (ndim > 2) {
                _sdd_write_cyc_ebp(sdd,3,cyc_ebp[2]);
                _sdd_write_pe_bcnt(sdd,3,pe_bcnts[2]);
                _sdd_write_blk_ebp(sdd,3,blk_ebp[2]);
                if (ndim > 3) {
                    _sdd_write_cyc_ebp(sdd,4,cyc_ebp[3]);
                    _sdd_write_pe_bcnt(sdd,4,pe_bcnts[3]);
                    _sdd_write_blk_ebp(sdd,4,blk_ebp[3]);
                    if (ndim > 4) {
                        _sdd_write_cyc_ebp(sdd,5,cyc_ebp[4]);
                        _sdd_write_pe_bcnt(sdd,5,pe_bcnts[4]);
                        _sdd_write_blk_ebp(sdd,5,blk_ebp[4]);
                        if (ndim > 5) {
                            _sdd_write_cyc_ebp(sdd,6,cyc_ebp[5]);
                            _sdd_write_pe_bcnt(sdd,6,pe_bcnts[5]);
                            _sdd_write_blk_ebp(sdd,6,blk_ebp[5]);
                            if (ndim > 6) {
                                _sdd_write_cyc_ebp(sdd,7,cyc_ebp[6]);
                                _sdd_write_pe_bcnt(sdd,7,pe_bcnts[6]);
                                _sdd_write_blk_ebp(sdd,7,blk_ebp[6]);
                            }
                        }
                    }
                }
            }
        }

}
