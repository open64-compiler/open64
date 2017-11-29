/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

/* ====================================================================
 * ====================================================================
 *
 * Module: float_rf.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/ia64/float_rf.h,v $
 *
 * 82-bit register format floating point utilities interface.
 *
 * ====================================================================
 * ==================================================================== */

#ifndef float_rf_INCLUDED
#define float_rf_INCLUDED

#ifdef _MIPSEL
#define TARGETEL 1
#endif

#ifdef _MIPSEB
#define TARGETEB 1
#endif

#ifdef linux
#else
#include <inttypes.h>
#endif

/*************************** structs.h ***************************/

#ifndef _STRUCTS_H_
#define _STRUCTS_H_

#include <stdint.h>

typedef	struct {
  uint32_t unused : 14;
  uint32_t sign   : 1;
  uint32_t exp    : 17;
  uint64_t frac   : 64;
} float_rf;


typedef	union {
  uint64_t i;
  struct {
#if TARGETEL
    uint64_t trapsDisabled : 6;
    uint64_t controls	   : 7;
    uint64_t flags	   : 6;
    uint64_t ISR_fpa	   : 1;
    uint64_t ISR_ebc	   : 1;
    uint64_t reserved	   : 43;
#endif
#if TARGETEB
    uint64_t reserved	   : 43;
    uint64_t ISR_ebc	   : 1;
    uint64_t ISR_fpa	   : 1;
    uint64_t flags	   : 6;
    uint64_t controls	   : 7;
    uint64_t trapsDisabled : 6;
#endif
  } c;
  struct {
#if TARGETEL
    uint64_t invalid_trap_disabled    : 1;
    uint64_t den_unn_op_trap_disabled : 1;
    uint64_t zero_div_trap_disabled   : 1;
    uint64_t overflow_trap_disabled   : 1;
    uint64_t underflow_trap_disabled  : 1;
    uint64_t inexact_trap_disabled    : 1;
    uint64_t ftz		      : 1;
    uint64_t wre		      : 1;
    uint64_t pc			      : 2;
    uint64_t rc			      : 2;
    uint64_t td			      : 1;
    uint64_t invalid_flag	      : 1;
    uint64_t den_unn_op_flag	      : 1;
    uint64_t zero_div_flag	      : 1;
    uint64_t overflow_flag	      : 1;
    uint64_t underflow_flag	      : 1;
    uint64_t inexact_flag	      : 1;
    uint64_t ISR_fpa		      : 1;
    uint64_t ISR_ebc		      : 1;
    uint64_t reserved		      : 43;
#endif
#if TARGETEB
    uint64_t reserved		      : 43;
    uint64_t ISR_ebc		      : 1;
    uint64_t ISR_fpa		      : 1;
    uint64_t inexact_flag	      : 1;
    uint64_t underflow_flag	      : 1;
    uint64_t overflow_flag	      : 1;
    uint64_t zero_div_flag	      : 1;
    uint64_t den_unn_op_flag	      : 1;
    uint64_t invalid_flag	      : 1;
    uint64_t td			      : 1;
    uint64_t rc			      : 2;
    uint64_t pc			      : 2;
    uint64_t wre		      : 1;
    uint64_t ftz		      : 1;
    uint64_t inexact_trap_disabled    : 1;
    uint64_t underflow_trap_disabled  : 1;
    uint64_t overflow_trap_disabled   : 1;
    uint64_t zero_div_trap_disabled   : 1;
    uint64_t den_unn_op_trap_disabled : 1;
    uint64_t invalid_trap_disabled    : 1;
#endif
  } b;
} status_field;

#endif

/*************************** externs.h ***************************/

#ifndef _STRUCTS_H_
#endif

#if defined(linux)
#endif

extern "C" {
    uint64_t mul64(uint64_t, uint64_t, uint64_t *);
}

extern	float_rf	Quiet_nan_rf;
extern	float_rf	NaTVal_rf;
extern	float_rf	Infinity_rf;
extern	float_rf	Zero_rf;

extern	float_rf	round(uint32_t, int32_t, uint64_t, uint64_t, status_field* );

extern	float_rf	round_l(uint32_t, int64_t, uint64_t, status_field* );

extern	int32_t		renorm( uint64_t * );

extern	float_rf	soft_div( uint32_t, int32_t, uint64_t,
				  uint32_t, int32_t, uint64_t,
				  status_field * );

extern
float_rf
recip_sqrt( uint32_t, int32_t, uint64_t );

extern
float_rf
fma( const float_rf *, const float_rf *, const float_rf *, status_field * );

extern
float_rf
fnorm( const float_rf *, const float_rf *, const float_rf *, status_field * );

extern
float_rf
fms( const float_rf *, const float_rf *, const float_rf *, status_field * );

extern
float_rf
fnma( const float_rf *, const float_rf *, const float_rf *, status_field * );

extern
float_rf
fmin( const float_rf *, const float_rf *, status_field * );

extern
float_rf
fmax( const float_rf *, const float_rf *, status_field * );

extern
float_rf
famin( const float_rf *, const float_rf *, status_field * );

extern
float_rf
famax( const float_rf *, const float_rf *, status_field * );

extern
float_rf
frcpa( int32_t *, const float_rf *, const float_rf *, status_field * );

extern
float_rf
frsqrta( int32_t *, const float_rf *, status_field * );

extern
float_rf
fcvtfx( const float_rf *, status_field * );

extern
float_rf
fcvtfxtrunc( const float_rf *, status_field * );

extern
float_rf
fcvtfxu( const float_rf *, status_field * );

extern
float_rf
fcvtfxutrunc( const float_rf *, status_field * );

extern
float_rf
fcvtxf( const float_rf * );

extern
int32_t
fcmp_eq( const float_rf *, const float_rf *, status_field * );

extern
int32_t
fcmp_lt( const float_rf *, const float_rf *, status_field * );

extern
int32_t
fcmp_le( const float_rf *, const float_rf *, status_field * );

extern
int32_t
fcmp_unord( const float_rf *, const float_rf *, status_field * );

extern
float_rf
madd( uint32_t, int32_t, uint64_t, uint32_t, int32_t, uint64_t,
	uint32_t, int32_t, uint64_t, status_field * );

extern
void
breakout(const float_rf *, uint32_t *, int32_t *, uint64_t *, status_field * );

extern
int32_t
breakout_and_test2(const float_rf *, uint32_t *, int32_t *, uint64_t *,
		   const float_rf *, uint32_t *, int32_t *, uint64_t *,
		   status_field * );

extern
int32_t
breakout_and_test3(const float_rf *, uint32_t *, int32_t *, uint64_t *,
		   const float_rf *, uint32_t *, int32_t *, uint64_t *,
		   const float_rf *, uint32_t *, int32_t *, uint64_t *,
		   status_field * );

extern
int32_t
breakout_and_test( const float_rf *, uint32_t *, int32_t *, uint64_t *, 
		   status_field * );



/******************************************************************/

#endif
