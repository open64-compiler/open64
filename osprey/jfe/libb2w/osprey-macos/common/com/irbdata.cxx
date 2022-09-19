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


/* ====================================================================
 * ====================================================================
 *
 * Module: irbdata.c
 *
 * Revision history:
 *  21-Mar-93 - Original Version
 *
 * Description:
 *
 *	declaration of the initialized data structures
 *
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#include "defs.h"
#include "tracing.h"			// for TFile
#include "irbdata.h"


INITO_IDX
New_INITO (ST_IDX st, INITV_IDX val)
{
    INITO_IDX idx;
    INITO &inito = Scope_tab[ST_IDX_level (st)].inito_tab->New_entry (idx);

    Set_INITO_st_idx (inito, st);
    Set_INITO_val (inito, val);

    return make_INITO_IDX (idx, ST_IDX_level (st));
} 

INITV_IDX
New_INITV (void)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);
    return idx;
}

inline void
add_initv (INITV_IDX ninv, INITO_IDX ino, INITV_IDX inv)
{
    if (inv != 0)
	Initv_Table[inv].next = ninv;
    else if (ino != 0)
	Set_INITO_val (Inito_Table[ino], ninv);
} // add_initv

INITV_IDX
Copy_INITV(INITV_IDX parent_inv, INITO_IDX ino, INITV_IDX inv)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, parent_inv);

    memcpy(&initv, &Initv_Table[inv], sizeof(INITV));
   
    return idx;
}

void
INITV_Init_Integer (INITV_IDX inv, TYPE_ID mtype, INT64 val, UINT16 repeat)
{
    if (val == 0)
	INITV_Set_ZERO (Initv_Table[inv], mtype, repeat);
    else if (val == 1)
	INITV_Set_ONE (Initv_Table[inv], mtype, repeat);
    else {
    	TCON tc  = Host_To_Targ (mtype, val);
    	INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), repeat);
    }
}

void
INITV_Init_Float (INITV_IDX inv, TYPE_ID mtype, double val, UINT16 repeat)
{
    TCON tc = Host_To_Targ_Float (mtype, val);
    INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), repeat);
}

void
INITV_Init_String (INITV_IDX inv, char *str, INT size, UINT16 repeat)
{
    // note that it is assumed that the size will include space 
    // for an ending null if needed.
    TCON tc = Host_To_Targ_String (MTYPE_STR, str, size);
    INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), repeat);
}

void
INITV_Init_Symoff (INITV_IDX inv, ST *st, INT64 ofst, UINT16 repeat)
{
    INITV_Set_SYMOFF (Initv_Table[inv], repeat, ST_st_idx(st), ofst);
}

#ifdef TARG_IA64
void
INITV_Init_Symiplt (INITV_IDX inv, ST *st, INT64 ofst, UINT16 repeat)
{
    INITV_Set_SYMIPLT (Initv_Table[inv], repeat, ST_st_idx(st), ofst);
}
#endif

void
INITV_Init_Label (INITV_IDX inv, LABEL_IDX lab, UINT16 repeat, INT16 flags, mTYPE_ID mtype)
{
    INITV_Set_LABEL (Initv_Table[inv], repeat, lab, flags, mtype);
}

void
INITV_Init_Symdiff (INITV_IDX inv,  
	LABEL_IDX lab1, ST *st2, BOOL halfword, UINT16 repeat)
{
    INITV_Set_SYMDIFF (Initv_Table[inv], repeat, 
		lab1, ST_st_idx (st2), halfword);
}

void
INITV_Init_Pad (INITV_IDX inv, UINT32 pad_bytes) 
{
    INITV_Set_PAD (Initv_Table[inv], pad_bytes);
}

#ifdef KEY
void
INITV_Init_Block (INITV_IDX inv, INITV_IDX bval, UINT16 repeat, mINT32 flags) 
{
    INITV_Set_BLOCK (Initv_Table[inv], repeat, bval, flags);
}
#else
void
INITV_Init_Block (INITV_IDX inv, INITV_IDX bval, UINT16 repeat) 
{
    INITV_Set_BLOCK (Initv_Table[inv], repeat, bval);
}
#endif // KEY


INITV_IDX
Irb_Init_Symoff (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, ST *st,
		 INT64 ofst)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);

#ifdef FRONT_END
    // st could be zero for mutually referencing structures
    // one such example is in CONFORM_ANSI c65.c
    INITV_Set_SYMOFF (initv, repeat, st ? ST_st_idx (st) : 0, ofst);
#else
    INITV_Set_SYMOFF (initv, repeat, ST_st_idx (st), ofst);
#endif /* FRONT_END */

    return idx;
} // Irb_Init_Symoff

#ifdef TARG_IA64
INITV_IDX
Irb_Init_Symiplt (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, ST *st,
		 INT64 ofst)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);

#ifdef FRONT_END
    // st could be zero for mutually referencing structures
    // one such example is in CONFORM_ANSI c65.c
    INITV_Set_SYMIPLT (initv, repeat, st ? ST_st_idx (st) : 0, ofst);
#else
    INITV_Set_SYMIPLT (initv, repeat, ST_st_idx (st), ofst);
#endif /* FRONT_END */

    return idx;
} // Irb_Init_Symiplt
#endif

INITV_IDX
Irb_Init_Label (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, LABEL_IDX lab)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    INITV_Set_LABEL (initv, repeat, lab);
    return idx;
}

INITV_IDX
Irb_Init_Symdiff (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat, LABEL_IDX lab1,
		  ST *st2, BOOL halfword)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    INITV_Set_SYMDIFF (initv, repeat, lab1, ST_st_idx (st2),
		       halfword);
    return idx;
}


INITV_IDX
Irb_Init_Val (INITO_IDX ino, INITV_IDX inv, UINT32 repeat, TCON_IDX tc)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    INITV_Set_VAL (initv, tc, repeat);
    return idx;
}


INITV_IDX
Irb_Init_Pad (INITO_IDX ino, INITV_IDX inv, UINT32 pad_bytes)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    INITV_Set_PAD (initv, pad_bytes);
    return idx;
}


INITV_IDX
Irb_Init_Block (INITO_IDX ino, INITV_IDX inv, mUINT16 repeat)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    INITV_Set_BLOCK (initv, repeat, 0);
    return idx;
}


static INITV_IDX
Irb_Init_predefined_integer (INITO_IDX ino, INITV_IDX inv, INT32 repeat,
			     TYPE_ID mtype, INT32 val)
{
    INITV_IDX idx;
    INITV& initv = Initv_Table.New_entry (idx);

    add_initv (idx, ino, inv);
    if (val == 0)
	INITV_Set_ZERO (initv, mtype, repeat);
    else
	INITV_Set_ONE (initv, mtype, repeat);
	
    return idx;
}


INITV_IDX
Irb_Init_Integer_Of_Type (TYPE_ID mtype, INT64 value, INT32 repeat,
			  INITO_IDX ino, INITV_IDX inv) 
{
    if (value == 0 || value == 1)
	return Irb_Init_predefined_integer (ino, inv, repeat, mtype, value);

    TCON tc  = Host_To_Targ(mtype, value);

    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


INITV_IDX
Irb_Init_Integer (INT size, INT64 value, INT32 repeat, INITO_IDX ino,
		  INITV_IDX inv) 
{
    TYPE_ID mtype;
    
    switch (size) {
    case 1:
	mtype = MTYPE_I1;
	break;
    case 2:
	mtype = MTYPE_I2;
	break;
    case 4:
	mtype = MTYPE_I4;
	break;
    case 8:
	mtype = MTYPE_I8;
	break;
    }

    return Irb_Init_Integer_Of_Type (mtype, value, repeat, ino, inv);
}


INITV_IDX
Irb_Init_String (INT size, char *str, INT32 repeat, INITO_IDX ino,
		 INITV_IDX inv) 
{
    // note that it is assumed that the size will include space 
    // for an ending null if needed.
    TCON tc = Host_To_Targ_String (MTYPE_STR, str, size);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


#ifndef MONGOOSE_BE

INITV_IDX
Irb_Init_Float (INT size, double value, INT32 repeat, INITO_IDX ino,
		INITV_IDX inv)
{
    TCON tc = Host_To_Targ_Float (size == 4 ? MTYPE_F4 : MTYPE_F8, value);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}

INITV_IDX
Irb_Init_Float_4 (INT size, float value, INT32 repeat, INITO_IDX ino,
		  INITV_IDX inv)
{
    TCON tc = Host_To_Targ_Float_4 (size == 4 ? MTYPE_F4 : MTYPE_F8, value);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


INITV_IDX
Irb_Init_Quad (INT size, QUAD_TYPE value, INT32 repeat, INITO_IDX ino,
	       INITV_IDX inv)
{
    TCON tc = Host_To_Targ_Quad (value);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


INITV_IDX
Irb_Init_Complex (INT size, double real, double imag, INT32 repeat,
		  INITO_IDX ino, INITV_IDX inv) 
{
    TCON tc = Host_To_Targ_Complex (size == 8 ? MTYPE_C4 : MTYPE_C8, real,
				    imag);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


INITV_IDX
Irb_Init_Complex_4 (INT size, float real, float imag, INT32 repeat,
		    INITO_IDX ino, INITV_IDX inv) 
{
    TCON tc = Host_To_Targ_Complex_4 (size == 8 ? MTYPE_C4 : MTYPE_C8, real,
				      imag); 
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}


INITV_IDX
Irb_Init_Complex_Quad (INT size, QUAD_TYPE real, QUAD_TYPE imag,
		       INT32 repeat, INITO_IDX ino, INITV_IDX inv) 
{
    TCON tc = Host_To_Targ_Complex_Quad (real, imag);
    return Irb_Init_Val (ino, inv, repeat, Enter_tcon (tc));
}
#endif /* MONGOOSE_BE */


struct find_inito_predicate
{
    ST_IDX st;

    find_inito_predicate (const ST *s) : st (ST_st_idx (s)) {}

    BOOL operator () (UINT, const INITO *inito) const {
	return INITO_st_idx (*inito) == st;
    }
};


INITO_IDX
Find_INITO_For_Symbol (const ST *st)
{
    ST_IDX idx = ST_st_idx (st);

    return For_all_until (Inito_Table, ST_IDX_level (idx),
			  find_inito_predicate (st));

} // Find_INITO_For_Symbol


template <class OP>
void
#ifdef __GNU_BUG_WORKAROUND
// Work around an obnoxious compiler bug
For_all_initv (INITV_IDX idx, const OP op)
#else
For_all_initv (INITV_IDX idx, const OP& op)
#endif
{
    while (idx) {
	const INITV& initv = Initv_Table[idx];
	op (initv);
	idx = INITV_next (initv);
    }
}


void
Print_INITV (const INITV& initv)
{
    INT repeat = 0;
    switch (INITV_kind (initv)) {

    case INITVKIND_ZERO:
	repeat = INITV_repeat2 (initv);
	fprintf (TFile, " VAL: 0");
	break;

    case INITVKIND_ONE:
	repeat = INITV_repeat2 (initv);
	fprintf (TFile, " VAL: 1");
	break;

    case INITVKIND_VAL:
	repeat = INITV_repeat2 (initv);
	fprintf (TFile," VAL: %s", 
		 Targ_Print (NULL, Tcon_Table[INITV_tc (initv)])); 
	break;
	
    case INITVKIND_SYMOFF:
	repeat = INITV_repeat1 (initv);
	fprintf (TFile," SYMOFF: %s(0x%x)+%d(0x%x)",
		ST_class(INITV_st(initv)) == CLASS_CONST ?
		"<constant>" : ST_name(INITV_st(initv)),
		 INITV_st (initv),
		 INITV_ofst (initv), INITV_ofst (initv)); 
	break;
#ifdef TARG_IA64
    case INITVKIND_SYMIPLT:
	repeat = INITV_repeat1 (initv);
	fprintf (TFile," SYMIPLT: %s(0x%x)+%d(0x%x)",
		ST_class(INITV_st(initv)) == CLASS_CONST ?
		"<constant>" : ST_name(INITV_st(initv)),
		 INITV_st (initv),
		 INITV_ofst (initv), INITV_ofst (initv)); 
	break;
#endif
    case INITVKIND_LABEL:
	repeat = INITV_repeat1 (initv);
	fprintf (TFile," LABEL: %s (%d) flags=%d mtype=%d", LABEL_name (INITV_lab (initv)),
		 INITV_lab (initv), INITV_lab_flags(initv), INITV_lab_mtype(initv));
	break;
	
    case INITVKIND_SYMDIFF:
    case INITVKIND_SYMDIFF16:
	repeat = INITV_repeat1 (initv);
	if (INITV_kind (initv) == INITVKIND_SYMDIFF16)
	    fputs (" SYMDIFF16: ", TFile);
	else
	    fputs (" SYMDIFF: ", TFile);
	fprintf (TFile," %s-%s(0x%x)", LABEL_name (INITV_lab1 (initv)), 
		ST_name (INITV_st2 (initv)), INITV_st2 (initv));
	break;
	
    case INITVKIND_BLOCK:
	repeat = INITV_repeat1 (initv);
	fprintf (TFile," BLOCK: \n");
	Print_INITVs (INITV_blk (initv));
	fprintf (TFile, " ENDBLOCK");
	break;
	
    case INITVKIND_PAD:
	repeat = INITV_repeat1 (initv);
	fprintf (TFile," PAD: %d", INITV_pad (initv));
	break;
	
    default:
	fprintf(TFile,"  bad initv kind %d", INITV_kind (initv));
    }
    if (repeat > 1) 
	fprintf (TFile, " (repeat %d)", repeat);
    fprintf (TFile, "\n");
} // Print_INITV

void 
Print_INITV_idx (const INITV_IDX inv)
{
  Print_INITV (Initv_Table[inv]);
}

void
Print_INITVs (INITV_IDX idx)
{
    For_all_initv (idx, Print_INITV);
}

void
Print_INITVs (FILE *f, INITV_IDX idx)
{
   FILE *save_file = Get_Trace_File();
   Set_Trace_File_internal(f);
   Print_INITVs(idx);
   Set_Trace_File_internal(save_file);
}


void
INITO::Print (FILE *f) const
{
    if (st_idx != 0)
	fprintf (f, "%s (0x%x):\n", ST_name (st_idx), st_idx);
    else
	fputs ("<noname>:\n", f);

    Print_INITVs (f,val);
}

void
Print_INITO (const INITO& ino)
{
    ino.Print (TFile);
}


void
Print_Inits (UINT level)
{
    UINT size = Scope_tab[level].inito_tab->Size ();

    for (UINT i = 1; i < size; ++i)
	Print_INITO (i);
}

extern void dump_INITO_idx(INITO_IDX idx)
{
   FILE *temp;
   
   temp = Get_Trace_File();
   Set_Trace_File_internal(stdout);
   Print_INITO(Inito_Table[idx]);
   Set_Trace_File_internal(temp);
}

extern void dump_INITV_idx(INITV_IDX idx)
{
   FILE *temp;
   
   temp = Get_Trace_File();
   Set_Trace_File_internal(stdout);
   Print_INITV(Initv_Table[idx]);
   Set_Trace_File_internal(temp);
}


static UINT
Get_INITV_Size (INITV_IDX inv)
{
	INITV_IDX temp_inv = inv;
	UINT size;
	switch (INITV_kind(inv)) {
	case INITVKIND_SYMOFF:
	case INITVKIND_SYMDIFF:
	case INITVKIND_LABEL:
		size = Pointer_Size;
		break;
#ifdef TARG_IA64
	case INITVKIND_SYMIPLT:
		size = Pointer_Size << 1;
		break;
#endif
	case INITVKIND_VAL:
		if (TCON_ty(INITV_tc_val(inv)) == MTYPE_STR)
			size = TCON_str_len(INITV_tc_val(inv));
		else
			size = MTYPE_byte_size(TCON_ty(INITV_tc_val(inv)));
		break;
	case INITVKIND_ZERO:
	case INITVKIND_ONE:
		size = MTYPE_byte_size(INITV_mtype(inv));
		break;
	case INITVKIND_PAD:
		size = INITV_pad(inv);
		break;
	case INITVKIND_BLOCK:
		size = 0;
		inv = INITV_blk(inv);
		while (inv != 0) {
			size += Get_INITV_Size (inv);
			inv = INITV_next(inv);
		}
		break;
	default:
		FmtAssert(FALSE, ("Get_INITV_Size unexpected kind"));
		size = 0;
		break;
	}
	return size * INITV_repeat(temp_inv);
}

/* add up size of all the initv under the inito */
extern UINT
Get_INITO_Size (INITO_IDX ino)
{
	INITV_IDX inv = INITO_val(ino);
	UINT sum = 0;
	while (inv != 0) {
		sum += Get_INITV_Size (inv);
		inv = INITV_next(inv);
	}
	return sum;
}

