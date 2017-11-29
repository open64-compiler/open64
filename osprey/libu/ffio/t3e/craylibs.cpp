#
#
#  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of version 2.1 of the GNU Lesser General Public License 
#  as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it would be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
#  Further, this software is distributed without any warranty that it is
#  free of the rightful claim of any third person regarding infringement 
#  or the like.  Any license provided herein, whether implied or 
#  otherwise, applies only to this software file.  Patent licenses, if
#  any, provided herein do not apply to combinations of this program with 
#  other software, or any other product whatsoever.  
#
#  You should have received a copy of the GNU Lesser General Public 
#  License along with this program; if not, write the Free Software 
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
#  USA.
#
#  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
#  Mountain View, CA 94043, or:
#
#  http://www.sgi.com
#
#  For further information regarding this notice, see:
#
#  http://oss.sgi.com/projects/GenInfo/NoticeExplan
#
#

* USMID @(#) libu/ffio/t3e/craylibs.cpp	92.0	10/08/98 14:57:41 
******************************************************************************
* CrayLibs default CLD directives file for CRAY T3E systems.
******************************************************************************

* _DATA_STREAMS_OFF is a data symbol ("extern int") which may be referenced
* and stored to by C modules which desire to toggle the data streams default
* to "off"

streamunsafe=_DATA_STREAMS_OFF


* DATA_STREAMS_OFF@ is a common block which may be defined in Fortran modules 
* which desire to toggle the data streams default to "off"

streamunsafe=DATA_STREAMS_OFF@


* Functions which may access memory in streams-incoherent manner.
* Their use requires the user to observe the T3E streams coherency
* guidelines outlined in the streams_guide(5) man page.

streamunsafe=SHMEM_INT8_AND_TO_ALL
streamunsafe=shmem_int_and_to_all
streamunsafe=SHMEM_INT4_AND_TO_ALL
streamunsafe=shmem_short_and_to_all
streamunsafe=SHMEM_REAL8_MAX_TO_ALL
streamunsafe=shmem_double_max_to_all
streamunsafe=SHMEM_REAL4_MAX_TO_ALL
streamunsafe=shmem_float_max_to_all
streamunsafe=SHMEM_INT8_MAX_TO_ALL
streamunsafe=shmem_int_max_to_all
streamunsafe=SHMEM_INT4_MAX_TO_ALL
streamunsafe=shmem_short_max_to_all
streamunsafe=SHMEM_REAL8_MIN_TO_ALL
streamunsafe=shmem_double_min_to_all
streamunsafe=SHMEM_REAL4_MIN_TO_ALL
streamunsafe=shmem_float_min_to_all
streamunsafe=SHMEM_INT8_MIN_TO_ALL
streamunsafe=shmem_int_min_to_all
streamunsafe=SHMEM_INT4_MIN_TO_ALL
streamunsafe=shmem_short_min_to_all
streamunsafe=SHMEM_INT8_OR_TO_ALL
streamunsafe=shmem_int_or_to_all
streamunsafe=SHMEM_INT4_OR_TO_ALL
streamunsafe=shmem_short_or_to_all
streamunsafe=SHMEM_COMP8_PROD_TO_ALL
streamunsafe=shmem_complexd_prod_to_all
streamunsafe=SHMEM_COMP4_PROD_TO_ALL
streamunsafe=shmem_complexf_prod_to_all
streamunsafe=SHMEM_REAL8_PROD_TO_ALL
streamunsafe=shmem_double_prod_to_all
streamunsafe=SHMEM_REAL4_PROD_TO_ALL
streamunsafe=shmem_float_prod_to_all
streamunsafe=SHMEM_INT8_PROD_TO_ALL
streamunsafe=shmem_int_prod_to_all
streamunsafe=SHMEM_INT4_PROD_TO_ALL
streamunsafe=shmem_short_prod_to_all
streamunsafe=SHMEM_BROADCAST
streamunsafe=shmem_broadcast
streamunsafe=SHMEM_BROADCAST4
streamunsafe=shmem_broadcast32
streamunsafe=SHMEM_BROADCAST8
streamunsafe=shmem_broadcast64
streamunsafe=SHMEM_CLEAR_LOCK
streamunsafe=shmem_clear_lock
streamunsafe=SHMEM_COLLECT
streamunsafe=shmem_collect
streamunsafe=SHMEM_COLLECT4
streamunsafe=shmem_collect32
streamunsafe=SHMEM_COLLECT8
streamunsafe=shmem_collect64
streamunsafe=SHMEM_FCOLLECT
streamunsafe=shmem_fcollect
streamunsafe=SHMEM_CLEAR_EVENT
streamunsafe=SHMEM_SET_EVENT
streamunsafe=SHMEM_TEST_EVENT
streamunsafe=SHMEM_WAIT_EVENT
streamunsafe=shmem_clear_event
streamunsafe=shmem_set_event
streamunsafe=shmem_test_event
streamunsafe=shmem_wait_event
streamunsafe=SHMEM_FCOLLECT4
streamunsafe=shmem_fcollect32
streamunsafe=SHMEM_FCOLLECT8
streamunsafe=shmem_fcollect64
streamunsafe=SHMEM_FENCE
streamunsafe=shmem_fence
streamunsafe=shmem_double_g
streamunsafe=shmem_float_g
streamunsafe=shmem_int_g
streamunsafe=shmem_long_g
streamunsafe=shmem_short_g
streamunsafe=SHMEM_MY_PE
streamunsafe=shmem_my_pe
streamunsafe=SHMEM_N_PES
streamunsafe=shmem_n_pes
streamunsafe=shmem_double_p
streamunsafe=shmem_float_p
streamunsafe=shmem_int_p
streamunsafe=shmem_long_p
streamunsafe=shmem_short_p
streamunsafe=SHMEM_QUIET
streamunsafe=shmem_quiet
streamunsafe=SHMEM_SET_LOCK
streamunsafe=shmem_set_lock
streamunsafe=SHMEM_STACK
streamunsafe=shmem_stack
streamunsafe=SHMEM_TEST_LOCK
streamunsafe=shmem_test_lock
streamunsafe=SHMEM_WAIT
streamunsafe=shmem_wait
streamunsafe=SHMEM_WAIT_UNTIL
streamunsafe=shmem_wait_until
streamunsafe=SHMEM_COMP8_SUM_TO_ALL
streamunsafe=shmem_complexd_sum_to_all
streamunsafe=SHMEM_COMP4_SUM_TO_ALL
streamunsafe=shmem_complexf_sum_to_all
streamunsafe=SHMEM_REAL8_SUM_TO_ALL
streamunsafe=shmem_double_sum_to_all
streamunsafe=SHMEM_REAL4_SUM_TO_ALL
streamunsafe=shmem_float_sum_to_all
streamunsafe=SHMEM_INT8_SUM_TO_ALL
streamunsafe=shmem_int_sum_to_all
streamunsafe=SHMEM_INT4_SUM_TO_ALL
streamunsafe=shmem_short_sum_to_all
streamunsafe=SHMEM_INT8_XOR_TO_ALL
streamunsafe=shmem_int_xor_to_all
streamunsafe=SHMEM_INT4_XOR_TO_ALL
streamunsafe=shmem_short_xor_to_all
streamunsafe=SHMEM_INT8_CSWAP
streamunsafe=shmem_int_cswap
streamunsafe=shmem_long_cswap
streamunsafe=SHMEM_INT4_CSWAP
streamunsafe=shmem_short_cswap
streamunsafe=SHMEM_INT4_ADD
streamunsafe=SHMEM_INT4_FADD
streamunsafe=shmem_short_add
streamunsafe=shmem_short_fadd
streamunsafe=FAST_SHMEM_INT4_FINC
streamunsafe=FAST_SHMEM_INT4_INC
streamunsafe=SHMEM_INT4_FINC
streamunsafe=SHMEM_INT4_INC
streamunsafe=fast_shmem_short_finc
streamunsafe=fast_shmem_short_inc
streamunsafe=shmem_short_finc
streamunsafe=shmem_short_inc
streamunsafe=SHMEM_INT8_MSWAP
streamunsafe=shmem_int_mswap
streamunsafe=shmem_long_mswap
streamunsafe=SHMEM_INT4_MSWAP
streamunsafe=shmem_short_mswap
streamunsafe=SHMEM_IGET
streamunsafe=shmem_iget
streamunsafe=SHMEM_IGET4
streamunsafe=shmem_iget32
streamunsafe=SHMEM_IGET8
streamunsafe=shmem_iget64
streamunsafe=SHMEM_IPUT
streamunsafe=shmem_iput
streamunsafe=SHMEM_IPUT4
streamunsafe=shmem_iput32
streamunsafe=SHMEM_IPUT8
streamunsafe=shmem_iput64
streamunsafe=SHMEM_IXGET
streamunsafe=shmem_ixget
streamunsafe=SHMEM_IXGET4
streamunsafe=shmem_ixget32
streamunsafe=SHMEM_IXGET8
streamunsafe=shmem_ixget64
streamunsafe=SHMEM_IXPUT
streamunsafe=shmem_ixput
streamunsafe=SHMEM_IXPUT4
streamunsafe=shmem_ixput32
streamunsafe=SHMEM_IXPUT8
streamunsafe=shmem_ixput64
streamunsafe=SHMEM_GET
streamunsafe=_F_shmem_get
streamunsafe=shmem_get
streamunsafe=shmem_getmem
streamunsafe=SHMEM_GETMEM
streamunsafe=SHMEM_GET4
streamunsafe=SHMEM_GET32
streamunsafe=shmem_get32
streamunsafe=SHMEM_GET8
streamunsafe=SHMEM_GET64
streamunsafe=shmem_get64
streamunsafe=SHMEM_PUT
streamunsafe=_F_shmem_put
streamunsafe=shmem_put
streamunsafe=shmem_putmem
streamunsafe=SHMEM_PUTMEM
streamunsafe=SHMEM_PUT4
streamunsafe=SHMEM_PUT32
streamunsafe=shmem_put32
streamunsafe=SHMEM_PUT8
streamunsafe=SHMEM_PUT64
streamunsafe=shmem_put64
streamunsafe=SHMEM_REAL8_SWAP
streamunsafe=shmem_double_swap
streamunsafe=SHMEM_REAL4_SWAP
streamunsafe=shmem_float_swap
streamunsafe=SHMEM_INT8_SWAP
streamunsafe=SHMEM_SWAP
streamunsafe=shmem_int_swap
streamunsafe=shmem_long_swap
streamunsafe=shmem_swap
streamunsafe=SHMEM_INT4_SWAP
