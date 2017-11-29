/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifndef irbdata_INCLUDED
#define irbdata_INCLUDED

/* ====================================================================
 * Module: irbdata.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/irbdata.h,v $
 *
 * Revision history:
 *  21-Mar-93 - Original Version
 *
 * Description:
 * declaration of the initialized data structures
 * ==================================================================== */


#ifndef symtab_INCLUDED
#include "symtab.h"			// for Scope_tab
#endif

#ifndef irbdata_defs_INCLUDED
#include "irbdata_defs.h"
#endif

// access functions for INITO

inline ST_IDX
INITO_st_idx (const INITO& inito)	{ return inito.st_idx; }
inline void
Set_INITO_st_idx (INITO& inito, ST_IDX st) { inito.st_idx = st; }
inline void
Set_INITO_st_idx (INITO_IDX idx, ST_IDX st) 
    { Set_INITO_st_idx(Inito_Table[idx], st); }
inline ST*
INITO_st (const INITO& ino)		{ return &St_Table[ino.st_idx]; }
inline ST*
INITO_st (const INITO* ino)		{ return &St_Table[ino->st_idx]; }
inline ST*
INITO_st (const INITO_IDX idx)		{ return &St_Table[Inito_Table[idx].st_idx]; }

inline INITV_IDX
INITO_val (const INITO& inito)		{ return inito.val; }
inline INITV_IDX
INITO_val (INITO_IDX idx)		{ return INITO_val (Inito_Table[idx]);}
inline void
Set_INITO_val (INITO& ino, INITV_IDX inv) { ino.val = inv; }
inline void
Set_INITO_val (INITO_IDX idx, INITV_IDX inv)
    { Set_INITO_val(Inito_Table[idx], inv); }


// access functions for INITV

// read access for INITV

inline INITV_IDX
INITV_next (const INITV& initv)		{ return initv.next; }
inline INITV_IDX
INITV_next (const INITV_IDX idx)	{ return Initv_Table[idx].next; }

inline void
Set_INITV_next(const INITV_IDX idx, const INITV_IDX nidx) 
{
  Initv_Table[idx].next = nidx; 
}

/*ARGSUSED*/ 
inline void
INITV_read_check (BOOL condition)
{
    Is_True (condition, ("INITV read access error"));
}

inline INITVKIND
INITV_kind (const INITV& initv)		{ return initv.kind; }
inline INITVKIND
INITV_kind (const INITV_IDX idx)	{ return Initv_Table[idx].kind; }

inline UINT16
INITV_repeat1 (const INITV& initv) {
#ifdef Is_True_On
    switch (initv.kind) {
    case INITVKIND_ZERO:
    case INITVKIND_ONE:
    case INITVKIND_VAL:
      INITV_read_check (FALSE);
      break;
    default:
      break;
    }
#endif	/* Is_True_On */

    return initv.repeat1;
}

inline UINT32
INITV_repeat2 (const INITV& initv) {
#ifdef Is_True_On
    switch (initv.kind) {
    case INITVKIND_ZERO:
    case INITVKIND_ONE:
    case INITVKIND_VAL:
      break;
    default:
      INITV_read_check (FALSE);
      break;
    }
#endif

    return initv.Repeat2 ();
}

inline UINT32
INITV_repeat (const INITV_IDX inv)
{
	switch (Initv_Table[inv].kind) {
	case INITVKIND_VAL:
	case INITVKIND_ZERO:
	case INITVKIND_ONE:
		return Initv_Table[inv].u.tcval.repeat2;
	default:
		return Initv_Table[inv].repeat1;
	}
}

inline ST_IDX
INITV_st (const INITV& initv) {
#ifdef TARG_IA64
    INITV_read_check ((initv.kind == INITVKIND_SYMOFF) ||
		      (initv.kind == INITVKIND_SYMIPLT));
#else
    INITV_read_check (initv.kind == INITVKIND_SYMOFF);
#endif
    return initv.St ();
}
inline ST_IDX
INITV_st (const INITV_IDX initv) {
#ifdef TARG_IA64
    INITV_read_check ((Initv_Table[initv].kind == INITVKIND_SYMOFF) ||
		      (Initv_Table[initv].kind == INITVKIND_SYMIPLT));
#else
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_SYMOFF);
#endif
    return Initv_Table[initv].St ();
}
inline void
Set_INITV_st (INITV_IDX inv, ST_IDX st)
{
#ifdef TARG_IA64
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMOFF ||
		      Initv_Table[inv].kind == INITVKIND_SYMIPLT);
#else
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMOFF);
#endif
    Initv_Table[inv].u.sto.st = st;
}
	
inline INT32
INITV_ofst (const INITV& initv) {
#ifdef TARG_IA64
    INITV_read_check (initv.kind == INITVKIND_SYMOFF || 
		      initv.kind == INITVKIND_SYMIPLT);
#else
    INITV_read_check (initv.kind == INITVKIND_SYMOFF);
#endif
    return initv.Ofst ();
}
inline INT32
INITV_ofst (const INITV_IDX initv) {
#ifdef TARG_IA64
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_SYMOFF ||
		      Initv_Table[initv].kind == INITVKIND_SYMIPLT);
#else
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_SYMOFF);
#endif
    return Initv_Table[initv].Ofst ();
}
inline void
Set_INITV_ofst (INITV_IDX inv, INT32 ofst)
{
#ifdef TARG_IA64
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMOFF ||
		      Initv_Table[inv].kind == INITVKIND_SYMIPLT);
#else
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMOFF);
#endif
    Initv_Table[inv].u.sto.ofst = ofst;
}

inline LABEL_IDX
INITV_lab (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_LABEL);
    return initv.Lab ();
}
inline LABEL_IDX
INITV_lab (const INITV_IDX initv) {
    return INITV_lab (Initv_Table[initv]);
}
inline INT16
INITV_lab_flags (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_LABEL);
    return initv.Lab_flags ();
}
inline INT16
INITV_lab_flags (const INITV_IDX initv) {
    return INITV_lab_flags (Initv_Table[initv]);
}
inline mTYPE_ID
INITV_lab_mtype (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_LABEL);
    return initv.Lab_mtype();
}
inline mTYPE_ID
INITV_lab_mtype (const INITV_IDX initv) {
    return INITV_lab_mtype (Initv_Table[initv]);
}
inline void
Set_INITV_lab (INITV& inv, LABEL_IDX lab) { 
	inv.u.lab.lab = lab;
	inv.u.lab.flags = INITVLABELFLAGS_UNUSED;
        inv.u.lab.mtype = MTYPE_UNKNOWN;
}
inline void
Set_INITV_lab (INITV_IDX inv, LABEL_IDX lab) { 
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_LABEL);
    Set_INITV_lab(Initv_Table[inv], lab); 
}

inline LABEL_IDX
INITV_lab1 (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_SYMDIFF ||
		      initv.kind == INITVKIND_SYMDIFF16);
    return initv.Lab1 ();
}
inline LABEL_IDX
INITV_lab1 (const INITV_IDX initv) {
    return INITV_lab1 (Initv_Table[initv]);
}
inline void
Set_INITV_lab1 (INITV& inv, LABEL_IDX lab1) { 
	inv.u.stdiff.lab1 = lab1;
}
inline void
Set_INITV_lab1 (INITV_IDX inv, LABEL_IDX lab1) { 
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMDIFF ||
		      Initv_Table[inv].kind == INITVKIND_SYMDIFF16);
    Set_INITV_lab1(Initv_Table[inv], lab1); 
}

inline ST_IDX
INITV_st2 (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_SYMDIFF ||
		      initv.kind == INITVKIND_SYMDIFF16);
    return initv.St2 ();
}
inline ST_IDX
INITV_st2 (const INITV_IDX initv) {
    return INITV_st2 (Initv_Table[initv]);
}
inline void
Set_INITV_st2 (INITV_IDX inv, ST_IDX st2) { 
    INITV_read_check (Initv_Table[inv].kind == INITVKIND_SYMDIFF ||
		      Initv_Table[inv].kind == INITVKIND_SYMDIFF16);
    Initv_Table[inv].u.stdiff.st2 = st2;
}

inline TCON_IDX
INITV_tc (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_VAL);
    return initv.Tc ();
}
inline TCON&
INITV_tc_val (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_VAL);
    return Tcon_Table[initv.Tc ()];
}
inline TCON&
INITV_tc_val (const INITV_IDX initv) {
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_VAL);
    return Tcon_Table[Initv_Table[initv].Tc ()];
}

inline TYPE_ID
INITV_mtype (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_ZERO ||
		      initv.kind == INITVKIND_ONE);
    return initv.Mtype ();
}
inline TYPE_ID
INITV_mtype (const INITV_IDX initv) {
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_ZERO ||
		      Initv_Table[initv].kind == INITVKIND_ONE);
    return Initv_Table[initv].Mtype ();
}

#ifdef KEY
inline mINT32
INITV_flags (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_BLOCK);
    return initv.u.blk.flags;
}

inline void
Set_INITV_flags (INITV& initv, mINT32 flags) {
    INITV_read_check (initv.kind == INITVKIND_BLOCK);
    initv.u.blk.flags = flags;
}

inline void
Set_INITV_flags (INITV_IDX initv, mINT32 flags) {
    Set_INITV_flags (Initv_Table[initv], flags);
}
#endif // KEY

inline INITV_IDX
INITV_blk (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_BLOCK);
    return initv.Blk ();
}
inline INITV_IDX
INITV_blk (const INITV_IDX initv) {
	return INITV_blk(Initv_Table[initv]);
}
inline void
Set_INITV_blk (INITV& inv, INITV_IDX blk) { 
	inv.u.blk.blk = blk;
#ifdef KEY
	inv.u.blk.flags = 0;
#else
	inv.u.blk.unused = 0;
#endif // KEY
}
inline void
Set_INITV_blk (INITV_IDX inv, INITV_IDX blk) { 
	Set_INITV_blk(Initv_Table[inv], blk); 
}

inline INT32
INITV_pad (const INITV& initv) {
    INITV_read_check (initv.kind == INITVKIND_PAD);
    return initv.Pad ();
}
inline INT32
INITV_pad (const INITV_IDX initv) {
    INITV_read_check (Initv_Table[initv].kind == INITVKIND_PAD);
    return Initv_Table[initv].Pad ();
}
inline void
Set_INITV_pad (INITV& initv, UINT32 pad_bytes) {
   initv.u.pad.pad = pad_bytes;
}


// utility functions

extern INITO_IDX
New_INITO (ST_IDX st, INITV_IDX val = 0);
inline INITO_IDX
New_INITO (const ST *st, INITV_IDX val = 0) {
    return New_INITO (ST_st_idx (st), val);
}

extern INITV_IDX New_INITV (void);

extern INITV_IDX
Copy_INITV (INITV_IDX parent_inv, INITO_IDX ino, INITV_IDX inv);

extern void
INITV_Init_Integer (INITV_IDX inv, 
	TYPE_ID mtype, INT64 val, UINT16 repeat = 1);

extern void
INITV_Init_Float (INITV_IDX inv, TYPE_ID mtype, double val, UINT16 repeat = 1);

extern void
INITV_Init_String (INITV_IDX inv, char *str, INT size, UINT16 repeat = 1);

extern void
INITV_Init_Symoff (INITV_IDX inv, ST *st, INT64 ofst, UINT16 repeat = 1);

#ifdef TARG_IA64
extern void
INITV_Init_Symiplt (INITV_IDX inv, ST *st, INT64 ofst, UINT16 repeat = 1);
#endif
extern void
INITV_Init_Label (INITV_IDX inv, LABEL_IDX lab, UINT16 repeat = 1, INT16 flags = INITVLABELFLAGS_UNUSED, mTYPE_ID mtype = MTYPE_UNKNOWN);

extern void
INITV_Init_Symdiff (INITV_IDX inv, 
	LABEL_IDX lab1, ST *st2, BOOL halfword, UINT16 repeat = 1);

extern void
#ifdef KEY
INITV_Init_Block (INITV_IDX inv, INITV_IDX bval, UINT16 repeat = 1, mINT32 flags = 0);
#else
INITV_Init_Block (INITV_IDX inv, INITV_IDX bval, UINT16 repeat = 1);
#endif // KEY

extern void
INITV_Init_Pad (INITV_IDX inv, UINT32 pad_bytes);

// append inv either as child of ino or as next sibling of prev_inv
inline INITV_IDX
Append_INITV (INITV_IDX inv, INITO_IDX ino, INITV_IDX prev_inv)
{
  if (prev_inv == INITV_IDX_ZERO)
      Set_INITO_val (ino, inv);
  else
      Set_INITV_next (prev_inv, inv);
  return inv;
}

// return the next to-be-allocated INITV_IDX
inline INITV_IDX
INITV_Next_Idx(void) { return Initv_Table.Size(); }

// return total size of initvs under inito
extern UINT
Get_INITO_Size (INITO_IDX ino);

extern INITO_IDX
Find_INITO_For_Symbol (const ST *st);

extern void
Print_INITV (const INITV& initv);

extern void
Print_INITV_idx (const INITV_IDX inv_idx);

extern void
Print_INITVs (INITV_IDX initv);

extern void
Print_INITO (const INITO& ino);
inline void
Print_INITO (INITO_IDX idx)	{ Print_INITO(Inito_Table[idx]); }

extern void
Print_Inits (UINT level);


// internal inline routines for writing INITV

inline void
INITV_Set_SYMOFF (INITV& initv, mUINT16 rp1, ST_IDX st, INT32 ofst) {
    initv.next = 0;
    initv.kind = INITVKIND_SYMOFF;
    initv.repeat1 = rp1;
    initv.u.sto.st = st;
    initv.u.sto.ofst = ofst;
}

#ifdef TARG_IA64
inline void
INITV_Set_SYMIPLT (INITV& initv, mUINT16 rp1, ST_IDX st, INT32 ofst) {
    initv.next = 0;
    initv.kind = INITVKIND_SYMIPLT;
    initv.repeat1 = rp1;
    initv.u.sto.st = st;
    initv.u.sto.ofst = ofst;
}
#endif

inline void
INITV_Set_LABEL (INITV& initv, mUINT16 rp1, LABEL_IDX lab, INT16 flags = INITVLABELFLAGS_UNUSED, mTYPE_ID mtype = MTYPE_UNKNOWN) {
    initv.next = 0;
    initv.kind = INITVKIND_LABEL;
    initv.repeat1 = rp1;
    initv.u.lab.lab = lab;
    initv.u.lab.flags = flags;
    initv.u.lab.mtype = mtype;
}

inline void
INITV_Set_int (INITV& initv, INITVKIND kind, TYPE_ID m, UINT32 rp2) {
    initv.next = 0;
    initv.kind = kind;
    initv.repeat1 = 0;
    initv.u.tcval.u.mtype = m;
    initv.u.tcval.repeat2 = rp2;
}
inline void
INITV_Set_ZERO (INITV& initv, TYPE_ID m, UINT32 rp2) {
    INITV_Set_int (initv, INITVKIND_ZERO, m, rp2);
}
inline void
INITV_Set_ONE (INITV& initv, TYPE_ID m, UINT32 rp2) {
    INITV_Set_int (initv, INITVKIND_ONE, m, rp2);
}

inline void
INITV_Set_VAL (INITV& initv, TCON_IDX t, mUINT32 rp2) {
    initv.next = 0;
    initv.kind = INITVKIND_VAL;
    initv.repeat1 = 0;
    initv.u.tcval.u.tc = t;
    initv.u.tcval.repeat2 = rp2;
}

inline void
#ifndef KEY
INITV_Set_BLOCK (INITV& initv, mUINT16 rp1, INITV_IDX b) {
#else
INITV_Set_BLOCK (INITV& initv, mUINT16 rp1, INITV_IDX b, mINT32 flags=0) {
#endif
    initv.next = 0;
    initv.kind = INITVKIND_BLOCK;
    initv.repeat1 = rp1;
    initv.u.blk.blk = b;
#ifndef KEY
    initv.u.blk.unused = 0;
#else
    initv.u.blk.flags = flags;
#endif // !KEY
}

inline void
INITV_Set_PAD (INITV& initv, UINT32 pad_bytes) {
    initv.next = 0;
    initv.kind = INITVKIND_PAD;
    initv.repeat1 = 1;
    initv.u.pad.pad = pad_bytes;
    initv.u.pad.unused = 0;
}

inline void
INITV_Set_SYMDIFF (INITV& initv, mUINT16 rp1, LABEL_IDX s1, ST_IDX s2,
		   BOOL halfword) 
{ 
    initv.next = 0;
    initv.kind = halfword ? INITVKIND_SYMDIFF16 : INITVKIND_SYMDIFF;
    initv.repeat1 = rp1;
    initv.u.stdiff.lab1 = s1;
    initv.u.stdiff.st2 = s2;
}
    


// old routines here for compatibility; should eventually remove

extern INITV_IDX
Irb_Init_Symoff (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, ST *st,
		 INT64 ofst);
		 
extern INITV_IDX
Irb_Init_Label (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, LABEL_IDX lab);

extern INITV_IDX
Irb_Init_Symdiff (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, LABEL_IDX lab1,
		  ST *st2, BOOL halfword);

extern INITV_IDX
Irb_Init_Val (INITO_IDX ino, INITV_IDX inv, UINT32 repeat, TCON_IDX tc);

extern INITV_IDX
Irb_Init_Pad (INITO_IDX ino, INITV_IDX inv, UINT32 pad_bytes);

extern INITV_IDX
Irb_Init_Block (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat);

extern INITV_IDX
Irb_Init_Integer (INT size, INT64 value, INT32 repeat, INITO_IDX ino,
		  INITV_IDX inv); 
		 
extern INITV_IDX
Irb_Init_Integer_Of_Type (TYPE_ID mtype, INT64 value, INT32 repeat,
			  INITO_IDX ino, INITV_IDX inv); 	 
		 
extern INITV_IDX
Irb_Init_String (INT size, char *str, INT32 repeat, INITO_IDX ino,
		 INITV_IDX inv);

#ifndef MONGOOSE_BE

extern INITV_IDX
Irb_Init_Float (INT size, double value, INT32 repeat, INITO_IDX ino,
		INITV_IDX inv);

extern INITV_IDX
Irb_Init_Float_4 (INT size, float value, INT32 repeat, INITO_IDX ino,
		  INITV_IDX inv);

extern INITV_IDX
Irb_Init_Quad (INT size, QUAD_TYPE value, INT32 repeat, INITO_IDX ino,
	       INITV_IDX inv);

extern INITV_IDX
Irb_Init_Complex (INT size, double real, double imag, INT32 repeat,
		  INITO_IDX ino, INITV_IDX inv);

extern INITV_IDX
Irb_Init_Complex_4 (INT size, float real, float imag, INT32 repeat,
		    INITO_IDX ino, INITV_IDX inv);

extern INITV_IDX
Irb_Init_Complex_Quad (INT size, QUAD_TYPE real, QUAD_TYPE imag,
		       INT32 repeat, INITO_IDX ino, INITV_IDX inv);

#endif /* MONGOOSE_BE */

#endif /* irbdata_INCLUDED */
