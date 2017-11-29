/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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



/********************************************************
This is a table indexed by Fortran type which contains 
the INTRINSIC numbers for all of the intrinsics. 

The table is indexed by type and intrinsic, with 
the types and intrinsics defined using the special enumerated types 
(to do the mappings properly)

********************************************************/

typedef enum {
	t_I1=0,
	t_I2=1,
	t_I4=2,
	t_I8=3,
	t_F4=4,
	t_F8=5,
	t_FQ=6,
	t_C4=7,
	t_C8=8,
	t_CQ=9,
	t_LAST=10,
	t_BAD = t_LAST
} t_enum;


typedef enum {
i_abs,
i_acos,
i_ashift,
i_asin,
i_atan,
i_atan2,
i_complex,
i_conjg,
i_cos,
i_cosh,
i_exp,
i_exponent,
i_fraction,
i_ieee_int,
i_ibits,
i_imag,
i_ishftc,
i_log,
i_log10,
i_mod,
i_modulo,
i_near,
i_nextafter,
i_pos_diff,
i_round,
i_rrspace,
i_scale,
i_set_exponent,
i_sign_xfer,
i_sin,
i_sinh,
i_space,
i_sqrt,
i_tan,
i_tanh,
i_trunc,
i_acosd,
i_asind,
i_atand,
i_atan2d,
i_cosd,
i_sind,
i_tand,
i_LAST
} i_enum;


typedef struct {
   INTRINSIC I;
   OPCODE    O;
} intopc;

/* Accessor macros */
#define WOP(x) {INTRINSIC_NONE,OPC_##x}
#define IOP(x) {INTRN_##x,OPCODE_UNKNOWN}
#define NOWI   {INTRINSIC_NONE,OPCODE_UNKNOWN}
#define GET_WOP(x) ((x).O)
#define GET_IOP(x) ((x).I)

/* Predefined stuff */
#define NOINT NOWI,NOWI,NOWI,NOWI
#define NOREAL NOWI,NOWI,NOWI
#define NOCMPX NOWI,NOWI,NOWI
#define NOTIMP NOINT,NOREAL,NOCMPX

#define IREALS(x) IOP(F4##x),IOP(F8##x),IOP(FQ##x)
#define ICMPXS(x) IOP(C4##x),IOP(C8##x),IOP(CQ##x)
#define IINTS(x)  IOP(I1##x),IOP(I2##x),IOP(I4##x),IOP(I8##x)
#define WREALS(x) WOP(F4##x),WOP(F8##x),WOP(FQ##x)
#define WCMPXS(x) WOP(C4##x),WOP(C8##x),WOP(CQ##x)
#define WINTS(x)  WOP(I4##x),WOP(I4##x),WOP(I4##x),WOP(I8##x)


/* Intrinsic table */

typedef struct {
	const char *iname;
	intopc ops[t_LAST];
} itab_entry;


#define GET_ITAB_WOP(intr,ty) GET_WOP(intrinsic_lookup[intr].ops[t_from_mtype(ty)])
#define GET_ITAB_IOP(intr,ty) GET_IOP(intrinsic_lookup[intr].ops[t_from_mtype(ty)])

itab_entry intrinsic_lookup[i_LAST] = {

{"abs",{ WINTS(ABS),WREALS(ABS),IOP(F4C4ABS),IOP(F8C8ABS),IOP(FQCQABS)}},
{"acos",{ NOINT,IREALS(ACOS),NOCMPX }},
{"ashift",{ WINTS(ASHR),NOREAL, NOCMPX }},  /* See if this is true (7/1/96) */
{"asin",{ NOINT,IREALS(ASIN),NOCMPX }},
{"atan",{ NOINT,IREALS(ATAN),NOCMPX }},
{"atan2", { NOINT,IREALS(ATAN2),NOCMPX }},
{"complex",{ NOINT, NOREAL, WCMPXS(COMPLEX) }}, 
{"conjg",{ NOINT,NOREAL,ICMPXS(CONJG) }},
{"cos",{ NOINT,IREALS(COS),ICMPXS(COS) }},
{"cosh",{ NOINT,IREALS(COSH),NOCMPX }},
{"exp",{ NOINT, IREALS(EXP),ICMPXS(EXP) }},
{"exponent",{ NOINT, IREALS(EXPONENT), NOCMPX }}, /* Special */
{"fraction",{ NOINT, IREALS(FRACTION), NOCMPX }}, 
{"ieee_int",{ IINTS(IEEE_INT), IREALS(IEEE_INT), NOCMPX }},
{"ibits",{ IINTS(BITS), NOREAL, NOCMPX }},
{"imag",{ NOINT, WREALS(IMAGPART), NOCMPX }},
{"ishftc",{ IOP(I4SHFTC),IOP(I4SHFTC),IOP(I4SHFTC),IOP(I8SHFTC), NOREAL, NOCMPX }},
{"log",{ NOINT, IREALS(LOG), ICMPXS(LOG) }},
{"log10",{ NOINT, IREALS(LOG10), NOCMPX }},
{"mod",{ WINTS(REM), IREALS(MOD), NOCMPX }},
{"modulo",{ WINTS(MOD), IREALS(MODULO), NOCMPX }},
{"near",{ NOINT, IREALS(NEAREST), NOCMPX }},
{"nextafter",{ NOINT, IREALS(NEXTAFTER), NOCMPX }},
{"pos_diff",{ IINTS(DIM), IREALS(DIM), NOCMPX }},
{"round",{ NOINT, IREALS(ANINT),NOCMPX }},
{"rrspace",{ NOINT, IREALS(RRSPACING), NOCMPX }},
{"scale",{ NOINT, IREALS(SCALE), NOCMPX }},
{"set_exponent",{ NOINT, IREALS(SET_EXPONENT), NOCMPX }},
{"sign_xfer",{ IINTS(SIGN),IREALS(SIGN),NOCMPX }},
{"sin",{ NOINT, IREALS(SIN), ICMPXS(SIN) }},
{"sinh",{ NOINT, IREALS(SINH), NOCMPX }},
{"space",{ NOINT, IREALS(SPACING), NOCMPX }},
{"sqrt",{ NOINT, WREALS(SQRT), WCMPXS(SQRT) }},
{"tan",{ NOINT, IREALS(TAN), NOCMPX }},
{"tanh",{ NOINT, IREALS(TANH), NOCMPX }},
{"trunc",{ NOINT, IREALS(AINT),NOCMPX }},
{"acosd",{ NOINT,IREALS(ACOSD),NOCMPX }},
{"asind",{ NOINT,IREALS(ASIND),NOCMPX }},
{"atand",{ NOINT,IREALS(ATAND),NOCMPX }},
{"atan2d", { NOINT,IREALS(ATAN2D),NOCMPX }},
{"cosd",{ NOINT,IREALS(COSD), NOCMPX }},
{"sind",{ NOINT, IREALS(SIND), NOCMPX }},
{"tand",{ NOINT, IREALS(TAND), NOCMPX }},
};


/* method of argumemt passing */

enum pb_method { p_VALUE, p_REF } ;

static WN * cwh_intrin_build(WN **k, INTRINSIC intr,TYPE_ID bt, INT numargs) ;

