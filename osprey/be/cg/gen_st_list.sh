#!/bin/sh
### ======================================================================
### ======================================================================
###
### Module: gen_st_list.sh
### $Revision: 1.1.1.1 $
### $Date: 2005/06/23 02:15:36 $
### $Author: sxyang $
### $Source: /u/cvs/Pathscale.MIPS/be/cg/gen_st_list.sh,v $
### Revision history:
###   27-Feb-92 - Original version
###
### Usage:      gen_st_list MTP_BIN
###
###     Generate the st_list.[ch] module.  The argument is the MTP_BIN
###     directory.  We do this in a file so the make rule can depend on
###     and it can be rebuilt when the procedure changes
###
### ======================================================================
### ======================================================================



sh $1/gen_x_list.sh    'ST*'                                      \
                            'ST'                                       \
                            'defs.h'                                   \
			    'errors.h'				       \
                            'mempool.h'                                 \
                            'cgir.h'                                   \
                            'st_list.h'
