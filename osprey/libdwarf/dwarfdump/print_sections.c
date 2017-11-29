/* 
  Copyright (C) 2000,2003,2004 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1500 Crittenden Lane,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan



$Header: /proj/osprey/CVS/open64/osprey1.0/libdwarf/dwarfdump/print_sections.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include "globals.h"
#include "dwarf_names.h"

#ifdef __MINGW32__
#include "translatetypes.h"
#endif /* __MINGW32__ */

/*
 * Print line number information:
 * 	filename
 *	new basic-block
 *	[line] [address] <new statement>
 */

static Dwarf_Unsigned local_dwarf_decode_u_leb128(unsigned char *leb128,
						  unsigned int
						  *leb128_length);



static char *regnames[] = {
    "cfa", "r1/at", "r2/v0", "r3/v1",
    "r4/a0", "r5/a1", "r6/a2", "r7/a3",
    "r8/t0", "r9/t1", "r10/t2", "r11/t3",
    "r12/t4", "r13/t5", "r14/t6", "r15/t7",
    "r16/s0", "r17/s1", "r18/s2", "r19/s3",
    "r20/s4", "r21/s5", "r22/s6", "r23/s7",
    "r24/t8", "r25/t9", "r26/k0", "r27/k1",
    "r28/gp", "r29/sp", "r30/s8", "r31",

    "$f0", "$f1",
    "$f2", "$f3",
    "$f4", "$f5",
    "$f6", "$f7",
    "$f8", "$f9",
    "$f10", "$f11",
    "$f12", "$f13",
    "$f14", "$f15",
    "$f16", "$f17",
    "$f18", "$f19",
    "$f20", "$f21",
    "$f22", "$f23",
    "$f24", "$f25",
    "$f26", "$f27",
    "$f28", "$f29",
    "$f30", "$f31",
    "ra", "slk",

};
static void printreg(Dwarf_Signed);

/* referred in dwarfdump.c */
Dwarf_Die current_cu_die_for_print_frames;

extern void
print_line_numbers_this_cu(Dwarf_Debug dbg, Dwarf_Die cu_die)
{
    Dwarf_Signed linecount;
    Dwarf_Line *linebuf;
    Dwarf_Signed i;
    Dwarf_Addr pc;
    Dwarf_Unsigned lineno;
    Dwarf_Signed column;
    string filename;
    Dwarf_Bool newstatement;
    Dwarf_Bool lineendsequence;
    Dwarf_Bool new_basic_block;
    int lres;
    int sres;
    int ares;
    int lires;
    int cores;

    printf("\nline number info\n");
    if (verbose > 1) {
	lres = _dwarf_print_lines(cu_die, &err);
	if (lres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_srclines details", lres, err);
	}
	return;
    }
    lres = dwarf_srclines(cu_die, &linebuf, &linecount, &err);
    if (lres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_srclines", lres, err);
    } else if (lres == DW_DLV_NO_ENTRY) {
	/* no line information is included */
    } else {
	printf
	    ("<source>\t[row,column]\t<pc>\t//<new statement or basic block\n");

	for (i = 0; i < linecount; i++) {
	    Dwarf_Line line = linebuf[i];
	    int nsres;

	    sres = dwarf_linesrc(line, &filename, &err);
	    ares = dwarf_lineaddr(line, &pc, &err);
	    if (sres == DW_DLV_ERROR) {
		print_error(dbg, "dwarf_linesrc", sres, err);
	    }
	    if (sres == DW_DLV_NO_ENTRY) {
		filename = "<unknown>";
	    }
	    if (ares == DW_DLV_ERROR) {
		print_error(dbg, "dwarf_lineaddr", ares, err);
	    }
	    if (ares == DW_DLV_NO_ENTRY) {
		pc = 0;
	    }
	    lires = dwarf_lineno(line, &lineno, &err);
	    if (lires == DW_DLV_ERROR) {
		print_error(dbg, "dwarf_lineno", lires, err);
	    }
	    if (lires == DW_DLV_NO_ENTRY) {
		lineno = -1LL;
	    }
	    cores = dwarf_lineoff(line, &column, &err);
	    if (cores == DW_DLV_ERROR) {
		print_error(dbg, "dwarf_lineoff", cores, err);
	    }
	    if (cores == DW_DLV_NO_ENTRY) {
		column = -1LL;
	    }
	    printf("%s:\t[%3llu,%2lld]\t%#llx", filename, lineno,
		   column, pc);
	    if (sres == DW_DLV_OK)
		dwarf_dealloc(dbg, filename, DW_DLA_STRING);

	    nsres = dwarf_linebeginstatement(line, &newstatement, &err);
	    if (nsres == DW_DLV_OK) {
		if (newstatement) {
		    printf("\t// new statement");
		}
	    } else if (nsres == DW_DLV_ERROR) {
		print_error(dbg, "linebeginstatment failed", nsres,
			    err);
	    }
	    nsres = dwarf_lineblock(line, &new_basic_block, &err);
	    if (nsres == DW_DLV_OK) {
		if (new_basic_block) {
		    printf("\t// new basic block");
		}
	    } else if (nsres == DW_DLV_ERROR) {
		print_error(dbg, "lineblock failed", nsres, err);
	    }
	    nsres = dwarf_lineendsequence(line, &lineendsequence, &err);
	    if (nsres == DW_DLV_OK) {
		if (lineendsequence) {
		    printf("\t// end of text sequence");
		}
	    } else if (nsres == DW_DLV_ERROR) {
		print_error(dbg, "lineblock failed", nsres, err);
	    }
	    printf("\n");

	    dwarf_dealloc(dbg, line, DW_DLA_LINE);
	}
	dwarf_dealloc(dbg, linebuf, DW_DLA_LIST);
    }
}

/*

A strcpy which ensures NUL terminated string
and never overruns the output.

*/
static void
safe_strcpy(char *out, int outlen, char *in, int inlen)
{
    if (inlen >= (outlen - 1)) {
	strncpy(out, in, outlen - 1);
	out[outlen - 1] = 0;
    } else {
	strcpy(out, in);
    }
}

/*
	Returns 1 if a proc with this low_pc found.
	Else returns 0.


*/
static int
get_proc_name(Dwarf_Debug dbg, Dwarf_Die die, Dwarf_Addr low_pc,
	      char *proc_name_buf, int proc_name_buf_len)
{
    Dwarf_Signed atcnt, i;
    Dwarf_Attribute *atlist;
    Dwarf_Addr low_pc_die = 0;
    int atres;
    int funcres = 1;
    int funcpcfound = 0;
    int funcnamefound = 1;

    proc_name_buf[0] = 0;	/* always set to something */
    atres = dwarf_attrlist(die, &atlist, &atcnt, &err);
    if (atres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_attrlist", atres, err);
	return 0;
    }
    if (atres == DW_DLV_NO_ENTRY) {
	return 0;
    }
    for (i = 0; i < atcnt; i++) {
	Dwarf_Half attr;
	int ares;
	string temps;
	int sres;
	int dres;

	if (funcnamefound == 1 && funcpcfound == 1) {
	    /* stop as soon as both found */
	    break;
	}
	ares = dwarf_whatattr(atlist[i], &attr, &err);
	if (ares == DW_DLV_ERROR) {
	    print_error(dbg, "get_proc_name whatattr error", ares, err);
	} else if (ares == DW_DLV_OK) {
	    switch (attr) {
	    case DW_AT_name:
		sres = dwarf_formstring(atlist[i], &temps, &err);
		if (sres == DW_DLV_ERROR) {
		    print_error(dbg,
				"formstring in get_proc_name failed",
				sres, err);
		    safe_strcpy(proc_name_buf, proc_name_buf_len, "ERROR in dwarf_formstring!", 50	/* safe 
													   wrong 
													   length 
													   since 
													   is 
													   big 
													 */ );
		} else if (sres == DW_DLV_NO_ENTRY) {
		    safe_strcpy(proc_name_buf, proc_name_buf_len, "NO ENTRY on dwarf_formstring?!", 50	/* safe 
													   wrong 
													   length 
													   since 
													   is 
													   big 
													 */ );
		} else {
		    int len = strlen(temps);

		    safe_strcpy(proc_name_buf, proc_name_buf_len, temps,
				len);
		    dwarf_dealloc(dbg, temps, DW_DLA_STRING);
		}
		funcnamefound = 1;	/* FOUND THE NAME */
		break;
	    case DW_AT_low_pc:
		dres = dwarf_formaddr(atlist[i], &low_pc_die, &err);
		if (dres == DW_DLV_ERROR) {
		    print_error(dbg, "formaddr in get_proc_name failed",
				dres, err);
		    low_pc_die = ~low_pc;
		    /* ensure no match */
		}
		funcpcfound = 1;

		break;
	    default:
		break;
	    }
	}
    }
    for (i = 0; i < atcnt; i++) {
	dwarf_dealloc(dbg, atlist[i], DW_DLA_ATTR);
    }
    dwarf_dealloc(dbg, atlist, DW_DLA_LIST);
    if (funcnamefound == 0 || funcpcfound == 0 || low_pc != low_pc_die) {
	funcres = 0;
    }
    return (funcres);
}

/*
	Modified Depth First Search looking for the procedure:
	a) only looks for children of subprogram.
	b) With subprogram looks at current die *before* looking
	   for a child.
	
	Needed since some languages, including MP Fortran,
	have nested functions.
	Return 0 on failure, 1 on success.
*/
static int
get_nested_proc_name(Dwarf_Debug dbg, Dwarf_Die die, Dwarf_Addr low_pc,
		     char *ret_name_buf, int ret_name_buf_len)
{
    char name_buf[BUFSIZ];
    Dwarf_Die curdie = die;
    int die_locally_gotten = 0;
    Dwarf_Die prev_child = 0;
    Dwarf_Die newchild = 0;
    Dwarf_Die newsibling = 0;
    Dwarf_Half tag;
    Dwarf_Error err = 0;
    int chres = DW_DLV_OK;

    ret_name_buf[0] = 0;
    while (chres == DW_DLV_OK) {
	int tres;

	tres = dwarf_tag(curdie, &tag, &err);
	newchild = 0;
	err = 0;
	if (tres == DW_DLV_OK) {
	    int lchres;

	    if (tag == DW_TAG_subprogram) {
		int proc_name_v;

		proc_name_v = get_proc_name(dbg, curdie, low_pc,
					    name_buf, BUFSIZ);
		if (proc_name_v) {
		    /* this is it */
		    safe_strcpy(ret_name_buf, ret_name_buf_len,
				name_buf, strlen(name_buf));
		    if (die_locally_gotten) {
			/* If we got this die from the parent, we don
			   not want to dealloc here! */
			dwarf_dealloc(dbg, curdie, DW_DLA_DIE);
		    }
		    return 1;
		}
		/* check children of subprograms recursively should
		   this really be check children of anything? */

		lchres = dwarf_child(curdie, &newchild, &err);
		if (lchres == DW_DLV_OK) {
		    /* look for inner subprogram */
		    int newprog =
			get_nested_proc_name(dbg, newchild, low_pc,
					     name_buf, BUFSIZ);
		    dwarf_dealloc(dbg, newchild, DW_DLA_DIE);
		    if (newprog) {
			/* Found it.  We could just take this name or
			   we could concatenate names together For now, 
			   just take name */
			safe_strcpy(ret_name_buf, ret_name_buf_len,
				    name_buf, strlen(name_buf));
			return 1;
		    }
		} else if (lchres == DW_DLV_NO_ENTRY) {
		    /* nothing to do */
		} else {
		    print_error(dbg,
				"get_nested_proc_name dwarf_child() failed ",
				chres, err);
		    if (die_locally_gotten) {
			/* If we got this die from the parent, we don
			   not want to dealloc here! */
			dwarf_dealloc(dbg, curdie, DW_DLA_DIE);
		    }
		    return 0;
		}
	    }			/* end if TAG_subprogram */
	} else {
	    print_error(dbg, "no tag on child read ", tres, err);
	    if (die_locally_gotten) {
		/* If we got this die from the parent, we don not want
		   to dealloc here! */
		dwarf_dealloc(dbg, curdie, DW_DLA_DIE);
	    }
	    return 0;
	}
	/* try next sibling */
	prev_child = curdie;
	chres = dwarf_siblingof(dbg, curdie, &newsibling, &err);
	if (chres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_cu_header On Child read ", chres,
			err);
	    if (die_locally_gotten) {
		/* If we got this die from the parent, we don not want
		   to dealloc here! */
		dwarf_dealloc(dbg, curdie, DW_DLA_DIE);
	    }
	    return 0;
	} else if (chres == DW_DLV_NO_ENTRY) {
	    return 0;		/* proc name not at this level */
	} else {		/* DW_DLV_OK */
	    curdie = newsibling;
	    if (die_locally_gotten) {
		/* If we got this die from the parent, we don not want
		   to dealloc here! */
		dwarf_dealloc(dbg, prev_child, DW_DLA_DIE);
	    }
	    prev_child = 0;
	    die_locally_gotten = 1;
	}

    }
    if (die_locally_gotten) {
	/* If we got this die from the parent, we don not want to
	   dealloc here! */
	dwarf_dealloc(dbg, curdie, DW_DLA_DIE);
    }
    return 0;
}

/*
  For MP Fortran and possibly other languages, functions 
  nest!  As a result, we must dig thru all functions, 
  not just the top level.


  This remembers the CU die and restarts each search at the start
  of  the current cu.

  No  mechanism exists for dwarf_dealloc of the last 
  current_cu_die_for_print_frames other than dwarf_finish().

*/
string
get_fde_proc_name(Dwarf_Debug dbg, Dwarf_Addr low_pc)
{
    static char proc_name[BUFSIZ];
    Dwarf_Unsigned cu_header_length;
    Dwarf_Unsigned abbrev_offset;
    Dwarf_Half version_stamp;
    Dwarf_Half address_size;
    Dwarf_Unsigned next_cu_offset = 0;
    int cures = DW_DLV_OK;
    int dres = DW_DLV_OK;
    int chres = DW_DLV_OK;
    int looping = 0;

    if (current_cu_die_for_print_frames == NULL) {
	cures
	    = dwarf_next_cu_header(dbg, &cu_header_length,
				   &version_stamp, &abbrev_offset,
				   &address_size, &next_cu_offset,
				   &err);
	if (cures == DW_DLV_ERROR) {
	    return NULL;
	} else if (cures == DW_DLV_NO_ENTRY) {
	    /* loop thru the list again */
	    current_cu_die_for_print_frames = 0;
	    ++looping;
	} else {		/* DW_DLV_OK */
	    dres = dwarf_siblingof(dbg, NULL,
				   &current_cu_die_for_print_frames,
				   &err);
	    if (dres == DW_DLV_ERROR) {
		return NULL;
	    }
	}
    }
    if (dres == DW_DLV_OK) {
	Dwarf_Die child = 0;

	if (current_cu_die_for_print_frames == 0) {
	    /* no information. Possibly a stripped file */
	    return NULL;
	}
	chres =
	    dwarf_child(current_cu_die_for_print_frames, &child, &err);
	if (chres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_cu_header on child read ", chres,
			err);
	} else if (chres == DW_DLV_NO_ENTRY) {
	} else {		/* DW_DLV_OK */
	    int gotname =
		get_nested_proc_name(dbg, child, low_pc, proc_name,
				     BUFSIZ);
	    if (gotname) {
		return (proc_name);
	    }
	    dwarf_dealloc(dbg, child, DW_DLA_DIE);
	    child = 0;
	}
    }
    for (;;) {
	Dwarf_Die ldie;

	cures = dwarf_next_cu_header(dbg, &cu_header_length,
				     &version_stamp, &abbrev_offset,
				     &address_size, &next_cu_offset,
				     &err);

	if (cures != DW_DLV_OK) {
	    break;
	}


	dres = dwarf_siblingof(dbg, NULL, &ldie, &err);

	if (current_cu_die_for_print_frames) {
	    dwarf_dealloc(dbg, current_cu_die_for_print_frames,
			  DW_DLA_DIE);
	}
	current_cu_die_for_print_frames = 0;
	if (dres == DW_DLV_ERROR) {
	    print_error(dbg,
			"dwarf_cu_header Child Read finding proc name for .debug_frame",
			chres, err);
	    continue;
	} else if (dres == DW_DLV_NO_ENTRY) {
	    ++looping;
	    if (looping > 1) {
		print_error(dbg, "looping  on cu headers!", dres, err);
		return NULL;
	    }
	    continue;
	}
	/* DW_DLV_OK */
	current_cu_die_for_print_frames = ldie;
	{
	    int chres;
	    Dwarf_Die child;

	    chres =
		dwarf_child(current_cu_die_for_print_frames, &child,
			    &err);
	    if (chres == DW_DLV_ERROR) {
		print_error(dbg, "dwarf Child Read ", chres, err);
	    } else if (chres == DW_DLV_NO_ENTRY) {

		;		/* do nothing, loop on cu */
	    } else {		/* DW_DLV_OK) */

		int gotname =
		    get_nested_proc_name(dbg, child, low_pc, proc_name,
					 BUFSIZ);
		if (gotname) {
		    return (proc_name);
		}
		dwarf_dealloc(dbg, child, DW_DLA_DIE);
	    }
	}
    }
    return (NULL);
}

 /*ARGSUSED*/ static void
print_frame_inst_bytes(Dwarf_Debug dbg,
		       Dwarf_Ptr cie_init_inst, Dwarf_Signed len,
		       Dwarf_Signed data_alignment_factor,
		       int code_alignment_factor, Dwarf_Half addr_size)
{
    unsigned char *instp = (unsigned char *) cie_init_inst;
    Dwarf_Unsigned uval;
    Dwarf_Unsigned uval2;
    unsigned int uleblen;
    unsigned int off = 0;
    unsigned int loff = 0;
    unsigned short u16;
    unsigned int u32;
    unsigned long long u64;

    for (; len > 0;) {
	unsigned char ibyte = *instp;
	int top = ibyte & 0xc0;
	int bottom = ibyte & 0x3f;
	int delta;
	int reg;

	switch (top) {
	case DW_CFA_advance_loc:
	    delta = ibyte & 0x3f;
	    printf("\t%2u DW_CFA_advance_loc %d", off,
		   (int) (delta * code_alignment_factor));
	    if (verbose) {
		printf("  (%d * %d)", (int) delta,
		       (int) code_alignment_factor);
	    }
	    printf("\n");
	    break;
	case DW_CFA_offset:
	    loff = off;
	    reg = ibyte & 0x3f;
	    uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
	    instp += uleblen;
	    len -= uleblen;
	    off += uleblen;
	    printf("\t%2u DW_CFA_offset ", loff);
	    printreg(reg);
	    printf(" %lld", (signed long long)
		   (((Dwarf_Signed) uval) * data_alignment_factor));
	    if (verbose) {
		printf("  (%llu * %d)", (unsigned long long) uval,
		       (int) data_alignment_factor);
	    }
	    printf("\n");
	    break;

	case DW_CFA_restore:
	    reg = ibyte & 0x3f;
	    printf("\t%2u DW_CFA_restore \n", off);
	    printreg(reg);
	    printf("\n");
	    break;

	default:
	    loff = off;
	    switch (bottom) {
	    case DW_CFA_set_loc:
		/* operand is address, so need address size */
		/* which will be 4 or 8. */
		switch (addr_size) {
		case 4:
		    {
			__uint32_t v32;

			memcpy(&v32, instp + 1, addr_size);
			uval = v32;
		    }
		    break;
		case 8:
		    {
			__uint64_t v64;

			memcpy(&v64, instp + 1, addr_size);
			uval = v64;
		    }
		default:
		    printf
			("Error: Unexpected address size %d in DW_CFA_set_loc!\n",
			 addr_size);
		    uval = 0;
		}

		instp += addr_size;
		len -= (Dwarf_Signed) addr_size;
		off += addr_size;
		printf("\t%2u DW_CFA_set_loc %llu\n",
		       loff, (unsigned long long) uval);
		break;
	    case DW_CFA_advance_loc1:
		delta = (unsigned char) *(instp + 1);
		uval2 = delta;
		instp += 1;
		len -= 1;
		off += 1;
		printf("\t%2u DW_CFA_advance_loc1 %llu\n",
		       loff, (unsigned long long) uval2);
		break;
	    case DW_CFA_advance_loc2:
		memcpy(&u16, instp + 1, 2);
		uval2 = u16;
		instp += 2;
		len -= 2;
		off += 2;
		printf("\t%2u DW_CFA_advance_loc2 %llu\n",
		       loff, (unsigned long long) uval2);
		break;
	    case DW_CFA_advance_loc4:
		memcpy(&u32, instp + 1, 4);
		uval2 = u32;
		instp += 4;
		len -= 4;
		off += 4;
		printf("\t%2u DW_CFA_advance_loc4 %llu\n",
		       loff, (unsigned long long) uval2);
		break;
	    case DW_CFA_MIPS_advance_loc8:
		memcpy(&u64, instp + 1, 8);
		uval2 = u64;
		instp += 8;
		len -= 8;
		off += 8;
		printf("\t%2u DW_CFA_MIPS_advance_loc8 %llu\n",
		       loff, (unsigned long long) uval2);
		break;
	    case DW_CFA_offset_extended:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		uval2 =
		    local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_offset_extended ", loff);
		printreg(uval);
		printf(" %lld", (signed long long)
		       (((Dwarf_Signed) uval2) *
			data_alignment_factor));
		if (verbose) {
		    printf("  (%llu * %d)", (unsigned long long) uval2,
			   (int) data_alignment_factor);
		}
		printf("\n");
		break;

	    case DW_CFA_restore_extended:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_restore_extended ", loff);
		printreg(uval);
		printf("\n");
		break;
	    case DW_CFA_undefined:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_undefined ", loff);
		printreg(uval);
		printf("\n");
		break;
	    case DW_CFA_same_value:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_same_value ", loff);
		printreg(uval);
		printf("\n");
		break;
	    case DW_CFA_register:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		uval2 =
		    local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_register ", loff);
		printreg(uval);
		printf(" = ");
		printreg(uval2);
		printf("\n");
		break;
	    case DW_CFA_remember_state:
		printf("\t%2u DW_CFA_remember_state\n", loff);
		break;
	    case DW_CFA_restore_state:
		printf("\t%2u DW_CFA_restore_state\n", loff);
		break;
	    case DW_CFA_def_cfa:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		uval2 =
		    local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_def_cfa ", loff);
		printreg(uval);
		printf(" %llu", (unsigned long long) uval2);
		printf("\n");
		break;
	    case DW_CFA_def_cfa_register:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_def_cfa_register ", loff);
		printreg(uval);
		printf("\n");
		break;
	    case DW_CFA_def_cfa_offset:
		uval = local_dwarf_decode_u_leb128(instp + 1, &uleblen);
		instp += uleblen;
		len -= uleblen;
		off += uleblen;
		printf("\t%2u DW_CFA_def_cfa_offset %llu\n",
		       loff, (unsigned long long) uval);
		break;

	    case DW_CFA_nop:
		printf("\t%2u DW_CFA_nop\n", loff);
		break;

	    case DW_CFA_def_cfa_expression : /* DWARF3 */
		break;
	    case DW_CFA_expression       : /* DWARF3 */
		break;
#ifdef KEY
            case DW_CFA_offset_extended_sf : /* DWARF3 */
#else
            case DW_CFA_cfa_offset_extended_sf : /* DWARF3 */
#endif
		break;
            case DW_CFA_def_cfa_sf       :        /* DWARF3 */
		break;
            case DW_CFA_def_cfa_offset_sf : /* DWARF3 */
		break;

#ifdef DW_CFA_GNU_window_save
	    case DW_CFA_GNU_window_save:{
		    /* no information: this just tells unwinder to
		       restore the window registers from the previous
		       frame's window save area */
		    printf("\t%2u DW_CFA_GNU_window_save \n", loff);
		    break;
		}
#endif
#ifdef  DW_CFA_GNU_args_size
		/* single uleb128 is the current arg area size in
		   bytes. no register exists yet to save this in */
	    case DW_CFA_GNU_args_size:{
		    Dwarf_Unsigned lreg;

		    lreg =
			local_dwarf_decode_u_leb128(instp + 1,
						    &uleblen);
		    printf
			("\t%2u DW_CFA_GNU_args_size arg size: %llu\n",
			 loff, (unsigned long long) lreg);
		    instp += uleblen;
		    len -= uleblen;
		    off += uleblen;

		    break;
		}
#endif

	    default:
		printf
		    ("\t%u Unexpected op 0x%x: unable to print more\n",
		     loff, (unsigned int) bottom);
		len = 0;
		break;
	    }
	}
	instp++;
	len--;
	off++;
    }
}

/* get all the data in .debug_frame */
extern void
print_frames(Dwarf_Debug dbg)
{
    Dwarf_Cie *cie_data;
    Dwarf_Signed cie_element_count;
    Dwarf_Fde *fde_data;
    Dwarf_Signed fde_element_count;
    Dwarf_Signed i, j, k;
    Dwarf_Addr low_pc;
    Dwarf_Unsigned func_length;
    Dwarf_Ptr fde_bytes;
    Dwarf_Unsigned fde_bytes_length;
    Dwarf_Off cie_offset;
    Dwarf_Signed cie_index;
    Dwarf_Off fde_offset;
    Dwarf_Signed reg;
    Dwarf_Signed offset;
    Dwarf_Signed eh_table_offset;
    Dwarf_Addr row_pc;
    int fres;
    Dwarf_Half address_size;
    int offres;
    int framed = 0;

    fres = dwarf_get_address_size(dbg, &address_size, &err);
    if (fres != DW_DLV_OK) {
	print_error(dbg, "dwarf_get_address_size", fres, err);
    }
    for (framed = 0; framed < 2; ++framed) {
	char *framename = 0;
	int silent_if_missing = 0;

	if (framed == 0) {
	    framename = ".debug_frame";
	    /* 
	     * Big question here is how to print all the info?
	     * Can print the logical matrix, but that is huge,
	     * though could skip lines that don't change.
	     * Either that, or print the instruction statement program
	     * that describes the changes.
	     */
	    fres =
		dwarf_get_fde_list(dbg, &cie_data, &cie_element_count,
				   &fde_data, &fde_element_count, &err);
	} else {
	    /* This is gnu g++ exceptions in a .eh_frame section. Which 
	       is just like .debug_frame except that the empty, or
	       'special' CIE_id is 0, not -1 (to distinguish fde from
	       cie). And the augmentation is "eh". As of egcs-1.1.2
	       anyway. A non-zero cie_id is in a fde and is the
	       difference between the fde address and the beginning of
	       the cie it belongs to. This makes sense as this is
	       intended to be referenced at run time, and is part of
	       the running image. */

	    /* 
	     * Big question here is how to print all the info?
	     * Can print the logical matrix, but that is huge,
	     * though could skip lines that don't change.
	     * Either that, or print the instruction statement program
	     * that describes the changes.
	     */
	    silent_if_missing = 1;
	    framename = ".eh_frame";
	    fres =
		dwarf_get_fde_list_eh(dbg, &cie_data,
				      &cie_element_count, &fde_data,
				      &fde_element_count, &err);
	}
	if (fres == DW_DLV_ERROR) {
	    printf("\n%s\n", framename);
	    print_error(dbg, "dwarf_get_fde_list", fres, err);
	} else if (fres == DW_DLV_NO_ENTRY) {
	    if (!silent_if_missing)
		printf("\n%s\n", framename);
	    /* no frame information */
	} else {		/* DW_DLV_OK */

	    printf("\n%s\n", framename);
	    printf("\nfde:\n");

	    for (i = 0; i < fde_element_count; i++) {
		string temps = 0;
		int fres;

		fres = dwarf_get_fde_range(fde_data[i],
					   &low_pc, &func_length,
					   &fde_bytes,
					   &fde_bytes_length,
					   &cie_offset, &cie_index,
					   &fde_offset, &err);
		if (fres == DW_DLV_ERROR) {
		    print_error(dbg, "dwarf_get_fde_range", fres, err);
		}
		if (fres == DW_DLV_NO_ENTRY) {
		    continue;
		}
		if (cu_name_flag &&
		    fde_offset_for_cu_low != DW_DLV_BADOFFSET &&
		    (fde_offset < fde_offset_for_cu_low ||
		     fde_offset > fde_offset_for_cu_high)) {
		    continue;
		}
		fres = dwarf_get_fde_exception_info(fde_data[i],
						    &eh_table_offset,
						    &err);
		if (fres == DW_DLV_ERROR) {
		    print_error(dbg, "dwarf_get_fde_exception_info",
				fres, err);
		}
		temps = get_fde_proc_name(dbg, low_pc);
		printf
		    ("<%3lld><%#llx:%#llx><%s><fde offset 0x%llx length: 0x%llx>",
		     cie_index, low_pc, (low_pc + func_length),
		     temps ? temps : "", fde_offset, fde_bytes_length);

		if (eh_table_offset == DW_DLX_NO_EH_OFFSET) {
		    printf("<eh offset %s>\n", "none");
		} else if (eh_table_offset ==
			   DW_DLX_EH_OFFSET_UNAVAILABLE) {
		    printf("<eh offset %s>\n", "unknown");
		} else {
		    printf("<eh offset 0x%llx>\n", eh_table_offset);
		}
		/* call dwarf_get_fde_info_for_reg() to get whole
		   matrix */
		for (j = low_pc; j < low_pc + func_length; j++) {
		    for (k = 0; k < DW_FRAME_LAST_REG_NUM; k++) {
			Dwarf_Signed offset_relevant;
			int fires;

			fires = dwarf_get_fde_info_for_reg(fde_data[i],
							   (Dwarf_Half)
							   k,
							   (Dwarf_Addr)
							   j,
							   &offset_relevant,
							   &reg,
							   &offset,
							   &row_pc,
							   &err);
			if (fires == DW_DLV_ERROR) {
			    print_error(dbg,
					"dwarf_get_fde_info_for_reg",
					fires, err);
			}
			if (fires == DW_DLV_NO_ENTRY) {
			    continue;
			}
			if (row_pc != j) {
			    /* duplicate row */
			    break;
			}
			if (k == 0)
			    printf("    %08llx:\t", j);
			switch (reg) {
			case DW_FRAME_UNDEFINED_VAL:
			    printreg(k);
			    printf("=u ");
			    break;
			case DW_FRAME_SAME_VAL:
			    break;
			default:
			    printreg(k);
			    printf("=");
			    if (offset_relevant == 0) {
				printreg(reg);
				printf(" ");
			    } else {
				printf("%02lld", offset);
				printf("(");
				printreg(reg);
				printf(") ");
			    }
			    break;
			}
			if (k == DW_FRAME_LAST_REG_NUM - 1) {
			    printf("\n");
			}
		    }
		}
		if (verbose > 1) {
		    Dwarf_Off fde_off;
		    Dwarf_Off cie_off;

		    /* get the fde instructions and print them in raw
		       form, just like cie instructions */
		    Dwarf_Ptr instrs;
		    Dwarf_Unsigned ilen;
		    int res;

		    res = dwarf_get_fde_instr_bytes(fde_data[i],
						    &instrs, &ilen,
						    &err);
		    offres =
			_dwarf_fde_section_offset(dbg, fde_data[i],
						  &fde_off, &cie_off,
						  &err);
		    if (offres == DW_DLV_OK) {
			printf("\tfde sec. offset %llu 0x%llx"
			       " cie offset for fde: %llu 0x%llx\n",
			       (unsigned long long) fde_off,
			       (unsigned long long) fde_off,
			       (unsigned long long) cie_off,
			       (unsigned long long) cie_off);

		    }


		    if (res == DW_DLV_OK) {
			int cires;
			Dwarf_Unsigned cie_length;
			Dwarf_Small version;
			string augmenter;
			Dwarf_Unsigned code_alignment_factor;
			Dwarf_Signed data_alignment_factor;
			Dwarf_Half return_address_register_rule;
			Dwarf_Ptr initial_instructions;
			Dwarf_Unsigned initial_instructions_length;

			cires = dwarf_get_cie_info(cie_data[cie_index],
						   &cie_length,
						   &version,
						   &augmenter,
						   &code_alignment_factor,
						   &data_alignment_factor,
						   &return_address_register_rule,
						   &initial_instructions,
						   &initial_instructions_length,
						   &err);
			if (cires == DW_DLV_ERROR) {
			    printf
				("Bad cie index %lld with fde index %lld!\n",
				 (long long) cie_index, (long long) i);
			    print_error(dbg, "dwarf_get_cie_info",
					cires, err);
			}
			if (cires == DW_DLV_NO_ENTRY) {
			    ;	/* ? */
			} else {


			    print_frame_inst_bytes(dbg, instrs, ilen,
						   data_alignment_factor,
						   code_alignment_factor,
						   address_size);
			}
		    } else if (res == DW_DLV_NO_ENTRY) {
			printf
			    ("Impossible: no instr bytes for fde index %d?\n",
			     (int) i);
		    } else {
			/* DW_DLV_ERROR */
			printf
			    ("Error: on gettinginstr bytes for fde index %d?\n",
			     (int) i);
			print_error(dbg, "dwarf_get_fde_instr_bytes",
				    res, err);
		    }

		}
	    }
	    /* 
	       Print the cie. */
	    if (verbose) {
		printf("\ncie:\n");
		for (i = 0; i < cie_element_count; i++) {
		    int cires;
		    Dwarf_Unsigned cie_length;
		    Dwarf_Small version;
		    string augmenter;
		    Dwarf_Unsigned code_alignment_factor;
		    Dwarf_Signed data_alignment_factor;
		    Dwarf_Half return_address_register_rule;
		    Dwarf_Ptr initial_instructions;
		    Dwarf_Unsigned initial_instructions_length;
		    Dwarf_Off cie_off;

		    cires = dwarf_get_cie_info(cie_data[i],
					       &cie_length,
					       &version,
					       &augmenter,
					       &code_alignment_factor,
					       &data_alignment_factor,
					       &return_address_register_rule,
					       &initial_instructions,
					       &initial_instructions_length,
					       &err);
		    if (cires == DW_DLV_ERROR) {
			print_error(dbg, "dwarf_get_cie_info", cires,
				    err);
		    }
		    if (cires == DW_DLV_NO_ENTRY) {
			;	/* ? */
		    }
		    if (verbose) {
			printf("<%3lld>\tversion\t\t\t\t%d\n", i,
			       version);
			cires =
			    _dwarf_cie_section_offset(dbg, cie_data[i],
						      &cie_off, &err);
			if (cires == DW_DLV_OK) {
			    printf("\tcie sec. offset %llu 0x%llx\n",
				   (unsigned long long) cie_off,
				   (unsigned long long) cie_off);

			}

			printf("\taugmentation\t\t\t%s\n", augmenter);
			printf("\tcode_alignment_factor\t\t%llu\n",
			       code_alignment_factor);
			printf("\tdata_alignment_factor\t\t%lld\n",
			       data_alignment_factor);
			printf("\treturn_address_register\t\t%d\n",
			       return_address_register_rule);
			printf
			    ("\tbytes of initial instructions:\t%lld\n",
			     (long long) initial_instructions_length);
			printf("\tcie length :\t\t\t%lld\n",
			       (long long) cie_length);
			print_frame_inst_bytes(dbg,
					       initial_instructions,
					       initial_instructions_length,
					       data_alignment_factor,
					       code_alignment_factor,
					       address_size);
		    }
		}
	    }
	    dwarf_dealloc(dbg, cie_data, DW_DLA_LIST);
	    dwarf_dealloc(dbg, fde_data, DW_DLA_LIST);
	}
    }
}

/* get all the data in .debug_pubnames */
extern void
print_pubnames(Dwarf_Debug dbg)
{
    Dwarf_Global *globbuf;
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Die die;
    Dwarf_Off die_off;
    Dwarf_Off die_CU_off;
    Dwarf_Die cu_die;
    Dwarf_Off cu_off;
    char *name;
    int res;

    printf("\n.debug_pubnames\n");
    res = dwarf_get_globals(dbg, &globbuf, &count, &err);
    if (res == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_globals", res, err);
    } else if (res == DW_DLV_NO_ENTRY) {
	/* (err == 0 && count == DW_DLV_NOCOUNT) means there are no
	   pubnames.  */
    } else {
	for (i = 0; i < count; i++) {
	    int nres;
	    int dres;
	    int ddres;
	    int cudres;

	    nres = dwarf_global_name_offsets(globbuf[i],
					     &name, &die_off, &cu_off,
					     &err);
	    if (nres != DW_DLV_OK) {
		print_error(dbg, "dwarf_global_name_offsets",
			    nres, err);
	    }

	    /* get die at die_off */
	    dres = dwarf_offdie(dbg, die_off, &die, &err);
	    if (dres != DW_DLV_OK)
		print_error(dbg, "dwarf_offdie", dres, err);

	    /* get offset of die from its cu-header */
	    ddres = dwarf_die_CU_offset(die, &die_CU_off, &err);
	    if (ddres != DW_DLV_OK) {
		print_error(dbg, "dwarf_die_CU_offset", ddres, err);
	    }

	    /* get die at offset cu_off */
	    cudres = dwarf_offdie(dbg, cu_off, &cu_die, &err);
	    if (cudres != DW_DLV_OK) {
		print_error(dbg, "dwarf_offdie", cudres, err);
	    }
	    printf("global %-15s die %lld, cu-die %lld,"
		   " off-in-cu %lld, cu %lld",
		   name, (long long) die_off, (long long) cu_off,
		   /* the cu die offset */
		   (long long) die_CU_off,
		   /* following is absolute offset of the ** beginning
		      of the cu */
		   (long long) (die_off - die_CU_off));
	    {
		/* get the offset of the cu header itself in the
		   section */
		Dwarf_Off off = 0;
		int cures3 = dwarf_global_cu_offset(globbuf[i],
						    &off, &err);

		if (cures3 != DW_DLV_OK) {
		    print_error(dbg, "dwarf_global_cu_offset", cudres,
				err);
		}
		if ((die_off - die_CU_off) != off) {
		    printf(" error: real cuhdr %llu", off);
		    exit(1);
		}

	    }
	    printf("\n");
	    dwarf_dealloc(dbg, name, DW_DLA_STRING);
	    /* print associated die too? */

	    if (check_pubname_attr) {
		Dwarf_Bool has_attr;
		int ares;

		ares =
		    dwarf_hasattr(die, DW_AT_external, &has_attr, &err);
		if (ares == DW_DLV_ERROR) {
		    print_error(dbg, "hassattr on DW_AT_external", ares,
				err);
		}
		pubname_attr_result.checks++;
		if (ares == DW_DLV_OK && has_attr) {
		    /* Should the value of flag be examined? */
		} else {
		    pubname_attr_result.errors++;
		    DWARF_CHECK_ERROR2(name,
				       "pubname does not have DW_AT_external")
		}
	    }

	    dwarf_dealloc(dbg, globbuf[i], DW_DLA_GLOBAL);
	}
	dwarf_dealloc(dbg, globbuf, DW_DLA_LIST);
    }
}


struct macro_counts_s {
	long mc_start_file;
	long mc_end_file;
	long mc_define;
	long mc_undef;
	long mc_extension;
	long mc_code_zero;
	long mc_unknown;
};

static void 
print_one_macro_entry_detail(long i, 
		char *type,
		struct Dwarf_Macro_Details_s *mdp)
{
    /*"DW_MACINFO_*: section-offset file-index [line] string\n" */
    if(mdp->dmd_macro) {
        printf( "%3ld %s: %6llu %2lld [%4lld] \"%s\" \n",
                        i,
                        type,
                        mdp->dmd_offset,
                        mdp->dmd_fileindex,
                        mdp->dmd_lineno,
                          mdp->dmd_macro);
    }else {
        printf( "%3ld %s: %6llu %2lld [%4lld] 0\n",
                        i,
                        type,
                        mdp->dmd_offset,
                        mdp->dmd_fileindex,
                        mdp->dmd_lineno );
    }

}

static void 
print_one_macro_entry(long i, 
		struct Dwarf_Macro_Details_s *mdp,
		struct macro_counts_s*counts)
{

   switch (mdp->dmd_type) {
   case  0:
	counts->mc_code_zero++;
        print_one_macro_entry_detail(i,"DW_MACINFO_type-code-0",mdp);
	break;

   case  DW_MACINFO_start_file :
	counts->mc_start_file++;
        print_one_macro_entry_detail(i,"DW_MACINFO_start_file",mdp);
	break;

   case  DW_MACINFO_end_file  :
	counts->mc_end_file++;
        print_one_macro_entry_detail(i,"DW_MACINFO_end_file  ",mdp);
	break;

   case  DW_MACINFO_vendor_ext :
	counts->mc_extension++;
        print_one_macro_entry_detail(i,"DW_MACINFO_vendor_ext",mdp);
	break;

   case DW_MACINFO_define:
	counts->mc_define++;
        print_one_macro_entry_detail(i,"DW_MACINFO_define    ",mdp);
	break;

   case DW_MACINFO_undef:
	counts->mc_undef++;
        print_one_macro_entry_detail(i,"DW_MACINFO_undef     ",mdp);
	break;

   default:
	{
        char  create_type[50]; /* More than large enough. */
	counts->mc_unknown++;
	snprintf(create_type,sizeof(create_type),
			"DW_MACINFO_0x%x", mdp->dmd_type);	
        print_one_macro_entry_detail(i,create_type,mdp);
	}
	break;
   }
}

/* print data in .debug_macinfo */
/* FIXME: should print name of file whose index is in macro data
   here  --  somewhere.
*/
/*ARGSUSED*/ extern void
print_macinfo(Dwarf_Debug dbg)
{
    Dwarf_Off offset = 0;
    Dwarf_Unsigned max = 0;
    Dwarf_Signed count;
    long group = 0;
    Dwarf_Macro_Details *maclist;
    int lres;

    printf("\n.debug_macinfo\n");

    while ((lres = dwarf_get_macro_details(dbg, offset,
					   max, &count, &maclist,
					   &err)) == DW_DLV_OK) {
	long i;
	struct macro_counts_s  counts;


	memset(&counts, 0,sizeof(counts));

        printf("\n");
        printf("compilation-unit .debug_macinfo # %ld\n",group);
        printf("num name section-offset file-index [line] \"string\"\n");
	for (i = 0; i < count; i++) {
	    struct Dwarf_Macro_Details_s *mdp = &
			maclist[i];
	    print_one_macro_entry(i,mdp,&counts);
	}

	if(counts.mc_start_file ==  0) {
	  printf("DW_MACINFO file count of zero is invalid DWARF2/3\n");
	}
	if(counts.mc_start_file != counts.mc_end_file) {
	  printf("Counts of DW_MACINFO file (%ld) end_file (%ld) "
		"do not match!.\n",
		counts.mc_start_file,counts.mc_end_file);
	}
	if(counts.mc_code_zero < 1) {
	  printf("Count of zeros in macro group should be non-zero "
		"(1 preferred), count is %ld\n",
		counts.mc_code_zero );
	}
	printf("Macro counts: start file %ld, "
		"end file %ld, "
		"define %ld, "
		"undef %ld "
		"ext %ld, "
		"code-zero %ld, "
		"unknown %ld\n",
		counts.mc_start_file,
		counts.mc_end_file,
		counts.mc_define,
		counts.mc_undef,
		counts.mc_extension,
		counts.mc_code_zero,
		counts.mc_unknown);


	/* int type=  maclist[count - 1].dmd_type; */
	/* ASSERT: type is zero */
        
	offset = maclist[count - 1].dmd_offset + 1;
	dwarf_dealloc(dbg, maclist, DW_DLA_STRING);
	++group;
    }
    if (lres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_macro_details", lres, err);
    }
}

/* print data in .debug_loc */
extern void
print_locs(Dwarf_Debug dbg)
{
    Dwarf_Unsigned offset = 0;
    Dwarf_Addr hipc_offset;
    Dwarf_Addr lopc_offset;
    Dwarf_Ptr data;
    Dwarf_Unsigned entry_len;
    Dwarf_Unsigned next_entry;
    int lres;

    printf("\n.debug_loc format <o b e l> means "
	   "section-offset begin-addr end-addr length-of-block-entry\n");
    while ((lres = dwarf_get_loclist_entry(dbg, offset,
					   &hipc_offset, &lopc_offset,
					   &data, &entry_len,
					   &next_entry,
					   &err)) == DW_DLV_OK) {
	printf("\t <obel> 0x%08llx 0x%09llx " "0x%08llx " "%8lld\n",
	       (long long) offset, (long long) lopc_offset,
	       (long long) hipc_offset, (long long) entry_len);
	offset = next_entry;
    }
    if (lres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_loclist_entry", lres, err);
    }
}

/* print data in .debug_abbrev */
extern void
print_abbrevs(Dwarf_Debug dbg)
{
    Dwarf_Abbrev ab;
    Dwarf_Unsigned offset = 0;
    Dwarf_Unsigned length;
    Dwarf_Unsigned attr_count;
    Dwarf_Half tag;
    Dwarf_Half attr;
    Dwarf_Signed form;
    Dwarf_Off off;
    Dwarf_Unsigned i;
    string child_name;
    Dwarf_Unsigned abbrev_num = 1;
    Dwarf_Signed child_flag;
    int abres;
    int tres;
    int acres;
    Dwarf_Unsigned abbrev_code = 0;

    printf("\n.debug_abbrev\n");
    while ((abres = dwarf_get_abbrev(dbg, offset, &ab,
				     &length, &attr_count,
				     &err)) == DW_DLV_OK) {

	if (attr_count == 0) {
	    /* Simple innocuous zero : null abbrev entry */
	    if (dense) {
		printf("<%lld><%lld><%lld><%s>\n",
		       abbrev_num,
		       offset, (signed long long) /* abbrev_code */ 0,
		       "null .debug_abbrev entry");
	    } else {
		printf("<%4lld><%5lld><code: %2lld> %-20s\n",
		       abbrev_num,
		       offset, (signed long long) /* abbrev_code */ 0,
		       "null .debug_abbrev entry");
	    }

	    offset += length;
	    ++abbrev_num;
	    continue;
	}
	tres = dwarf_get_abbrev_tag(ab, &tag, &err);
	if (tres != DW_DLV_OK) {
	    print_error(dbg, "dwarf_get_abbrev_tag", tres, err);
	}
	tres = dwarf_get_abbrev_code(ab, &abbrev_code, &err);
	if (tres != DW_DLV_OK) {
	    print_error(dbg, "dwarf_get_abbrev_code", tres, err);
	}
	if (dense)
	    printf("<%lld><%lld><%lld><%s>", abbrev_num,
		   offset, abbrev_code, get_TAG_name(dbg, tag));
	else
	    printf("<%4lld><%5lld><code: %2lld> %-20s", abbrev_num,
		   offset, abbrev_code, get_TAG_name(dbg, tag));
	++abbrev_num;
	acres = dwarf_get_abbrev_children_flag(ab, &child_flag, &err);
	if (acres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_get_abbrev_children_flag", acres,
			err);
	}
	if (acres == DW_DLV_NO_ENTRY) {
	    child_flag = 0;
	}
	child_name = get_children_name(dbg, child_flag);
	if (dense)
	    printf(" %s", child_name);
	else
	    printf("%s\n", child_name);
	/* Abbrev just contains the format of a die, which debug_info
	   then points to with the real data. So here we just print the 
	   given format. */
	for (i = 0; i < attr_count; i++) {
	    int aeres;

	    aeres =
		dwarf_get_abbrev_entry(ab, i, &attr, &form, &off, &err);
	    if (aeres == DW_DLV_ERROR) {
		print_error(dbg, "dwarf_get_abbrev_entry", aeres, err);
	    }
	    if (aeres == DW_DLV_NO_ENTRY) {
		attr = -1LL;
		form = -1LL;
	    }
	    if (dense)
		printf(" <%ld>%s<%s>", (unsigned long) off,
		       get_AT_name(dbg, attr),
		       get_FORM_name(dbg, form));
	    else
		printf("      <%5ld>\t%-28s%s\n",
		       (unsigned long) off, get_AT_name(dbg, attr),
		       get_FORM_name(dbg, form));
	}
	offset += length;
	if (dense)
	    printf("\n");
    }
    if (abres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_abbrev", abres, err);
    }
}

/* print data in .debug_string */
extern void
print_strings(Dwarf_Debug dbg)
{
    Dwarf_Signed length;
    string name;
    Dwarf_Off offset = 0;
    int sres;

    printf("\n.debug_string\n");
    while ((sres = dwarf_get_str(dbg, offset, &name, &length, &err))
	   == DW_DLV_OK) {
	printf("name at offset %lld, length %lld is %s\n",
	       offset, length, name);
	offset += length + 1;
    }
    if (sres == DW_DLV_ERROR) {
	print_error(dbg, "get_str failure", sres, err);
    }
}

/* get all the data in .debug_aranges */
extern void
print_aranges(Dwarf_Debug dbg)
{
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Arange *arange_buf;
    Dwarf_Addr start;
    Dwarf_Unsigned length;
    Dwarf_Off cu_die_offset;
    Dwarf_Die cu_die;
    int ares;
    int aires;

    printf("\n.debug_aranges\n");
    ares = dwarf_get_aranges(dbg, &arange_buf, &count, &err);
    if (ares == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_aranges", ares, err);
    } else if (ares == DW_DLV_NO_ENTRY) {
	/* no arange is included */
    } else {
	for (i = 0; i < count; i++) {
	    aires = dwarf_get_arange_info(arange_buf[i],
					  &start, &length,
					  &cu_die_offset, &err);
	    if (aires != DW_DLV_OK) {
		print_error(dbg, "dwarf_get_arange_info", aires, err);
	    } else {
		int dres;

		dres = dwarf_offdie(dbg, cu_die_offset, &cu_die, &err);
		if (dres != DW_DLV_OK) {
		    print_error(dbg, "dwarf_offdie", dres, err);
		} else {
		    if (cu_name_flag) {
			Dwarf_Half tag;
			Dwarf_Attribute attrib;
			Dwarf_Half theform;
			int tres;
			int dares;
			int fres;

			tres = dwarf_tag(cu_die, &tag, &err);
			if (tres != DW_DLV_OK) {
			    print_error(dbg, "dwarf_tag in aranges",
					tres, err);
			}
			dares =
			    dwarf_attr(cu_die, DW_AT_name, &attrib,
				       &err);
			if (dares != DW_DLV_OK) {
			    print_error(dbg, "dwarf_attr arange"
					" derived die has no name",
					dres, err);
			}
			fres = dwarf_whatform(attrib, &theform, &err);
			if (fres == DW_DLV_OK) {
			    if (theform == DW_FORM_string
				|| theform == DW_FORM_strp) {
				string temps;
				int sres;

				sres =
				    dwarf_formstring(attrib, &temps,
						     &err);
				if (sres == DW_DLV_OK) {
				    string p = temps;

				    if (cu_name[0] != '/') {
					p = strrchr(temps, '/');
					if (p == NULL) {
					    p = temps;
					} else {
					    p++;
					}
				    }
				    if (!strcmp(cu_name, p)) {
					dwarf_dealloc(dbg, temps,
						      DW_DLA_STRING);
				    } else {
					dwarf_dealloc(dbg, temps,
						      DW_DLA_STRING);
					continue;
				    }
				} else {
				    print_error(dbg,
						"arange: string missing",
						sres, err);
				}
			    }
			} else {
			    print_error(dbg,
					"dwarf_whatform unexpected value",
					fres, err);
			}
			dwarf_dealloc(dbg, attrib, DW_DLA_ATTR);
		    }
		    printf("\narange starts at %llx, "
			   "length of %lld, cu_die_offset = %lld",
			   start, length, cu_die_offset);
		    /* get the offset of the cu header itself in the
		       section */
		    {
			Dwarf_Off off = 0;
			int cures3 =
			    dwarf_get_arange_cu_header_offset(arange_buf
							      [i],
							      &off,
							      &err);
			if (cures3 != DW_DLV_OK) {
			    print_error(dbg, "dwarf_get_cu_hdr_offset",
					cures3, err);
			}
			if (verbose)
			    printf(" cuhdr %llu", off);
		    }
		    printf("\n");
		    print_one_die(dbg, cu_die, (boolean) TRUE,
				  /* srcfiles= */ 0,
				  /* cnt= */ 0);

		    dwarf_dealloc(dbg, cu_die, DW_DLA_DIE);
		}
	    }
	    /* print associated die too? */
	    dwarf_dealloc(dbg, arange_buf[i], DW_DLA_ARANGE);
	}
	dwarf_dealloc(dbg, arange_buf, DW_DLA_LIST);
    }
}

/* get all the data in .debug_static_funcs */
extern void
print_static_funcs(Dwarf_Debug dbg)
{
    Dwarf_Func *funcbuf;
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Die die;
    Dwarf_Off die_off;
    Dwarf_Off die_CU_off;
    Dwarf_Die cu_die;
    Dwarf_Off cu_off;
    char *name;
    int gfres;

    printf("\n.debug_static_func\n");
    gfres = dwarf_get_funcs(dbg, &funcbuf, &count, &err);
    if (gfres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_funcs", gfres, err);
    } else if (gfres == DW_DLV_NO_ENTRY) {
	/* no static funcs */
    } else {
	for (i = 0; i < count; i++) {
	    int fnres;
	    int ores;
	    int cures;
	    int dores;

	    fnres =
		dwarf_func_name_offsets(funcbuf[i], &name, &die_off,
					&cu_off, &err);
	    if (fnres != DW_DLV_OK) {
		print_error(dbg, "dwarf_func_name_offsets", fnres, err);
	    }
	    ores = dwarf_offdie(dbg, die_off, &die, &err);
	    if (ores != DW_DLV_OK)
		print_error(dbg, "dwarf_offdie on statics", ores, err);
	    cures = dwarf_die_CU_offset(die, &die_CU_off, &err);
	    if (cures != DW_DLV_OK) {
		print_error(dbg, "dwarf_die_CU_offset", cures, err);
	    }
	    dores = dwarf_offdie(dbg, cu_off, &cu_die, &err);
	    if (dores != DW_DLV_OK) {
		print_error(dbg, "dwarf_offdie2", dores, err);
	    }
	    printf("static-func %-15s die %lld, cu-die %lld,"
		   " off-in-cu %lld, cu %lld",
		   name, (long long) die_off, (long long) cu_off,
		   /* the cu die offset */
		   (long long) die_CU_off,
		   /* following is absolute offset of the ** beginning
		      of the cu */
		   (long long) (die_off - die_CU_off));
	    if (verbose) {
		Dwarf_Off off = 0;
		int cures3 = dwarf_func_cu_offset(funcbuf[i],
						  &off, &err);

		if (cures3 != DW_DLV_OK) {
		    print_error(dbg, "dwarf_arange_cu_offset",
				cures3, err);
		}
		if (((die_off - die_CU_off)) != off) {
		    printf(" error: real cuhdr %llu", off);
		    exit(1);
		}
	    }
	    printf("\n");
	    dwarf_dealloc(dbg, name, DW_DLA_STRING);
	    /* print associated die too? */
	    dwarf_dealloc(dbg, funcbuf[i], DW_DLA_FUNC);
	}
	dwarf_dealloc(dbg, funcbuf, DW_DLA_LIST);
    }
}

/* get all the data in .debug_static_vars */
extern void
print_static_vars(Dwarf_Debug dbg)
{
    Dwarf_Var *varbuf;
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Die die;
    Dwarf_Off die_off;
    Dwarf_Off die_CU_off;
    Dwarf_Die cu_die;
    Dwarf_Off cu_off;
    char *name;
    int gvres;

    printf("\n.debug_static_vars\n");
    gvres = dwarf_get_vars(dbg, &varbuf, &count, &err);
    if (gvres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_vars", gvres, err);
    } else if (gvres == DW_DLV_NO_ENTRY) {
	/* no static vars */
    } else {
	for (i = 0; i < count; i++) {
	    int vnres;
	    int ores;
	    int cures;
	    int dores;

	    vnres =
		dwarf_var_name_offsets(varbuf[i], &name, &die_off,
				       &cu_off, &err);
	    if (vnres != DW_DLV_OK) {
		print_error(dbg, "dwarf_var_name_offsets", vnres, err);
	    }
	    ores = dwarf_offdie(dbg, die_off, &die, &err);
	    if (ores != DW_DLV_OK)
		print_error(dbg, "dwarf_offdie", ores, err);
	    cures = dwarf_die_CU_offset(die, &die_CU_off, &err);
	    if (cures != DW_DLV_OK) {
		print_error(dbg, "dwarf_die_CU_offset", cures, err);
	    }
	    dores = dwarf_offdie(dbg, cu_off, &cu_die, &err);
	    if (dores != DW_DLV_OK) {
		print_error(dbg, "dwarf_offdie2", dores, err);
	    }
	    printf("static-var %-15s die %lld, cu-die %lld,"
		   " off-in-cu %lld, cu %lld",
		   name, (long long) die_off, (long long) cu_off,
		   /* the cu die offset */
		   (long long) die_CU_off,
		   /* following is absolute offset of the ** beginning
		      of the cu */
		   (long long) (die_off - die_CU_off));
	    {
		Dwarf_Off off = 0;
		int cures3 = dwarf_var_cu_offset(varbuf[i],
						 &off, &err);

		if (cures3 != DW_DLV_OK) {
		    print_error(dbg, "dwarf_arange_cu_offset",
				cures3, err);
		}
		if ((die_off - die_CU_off) != off) {
		    printf(" error: real cuhdr %llu", off);
		    exit(1);
		}
		if (verbose) {
		    printf(" cuhdr %llu", off);
		}
	    }
	    printf("\n");


	    dwarf_dealloc(dbg, name, DW_DLA_STRING);
	    /* print associated die too? */
	    dwarf_dealloc(dbg, varbuf[i], DW_DLA_VAR);
	}
	dwarf_dealloc(dbg, varbuf, DW_DLA_LIST);
    }
}

/* get all the data in .debug_types */
extern void
print_types(Dwarf_Debug dbg)
{
    Dwarf_Type *typebuf;
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Die die;
    Dwarf_Off die_off;
    Dwarf_Off die_CU_off;
    Dwarf_Die cu_die;
    Dwarf_Off cu_off;
    char *name;
    int gtres;

    printf("\n.debug_types\n");
    gtres = dwarf_get_types(dbg, &typebuf, &count, &err);
    if (gtres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_types", gtres, err);
    } else if (gtres == DW_DLV_NO_ENTRY) {
	/* no types */
    } else {
	for (i = 0; i < count; i++) {
	    int tnres;
	    int ores;
	    int cures;
	    int dores;

	    tnres = dwarf_type_name_offsets(typebuf[i], &name,
					    &die_off, &cu_off, &err);
	    if (tnres != DW_DLV_OK) {
		print_error(dbg, "dwarf_type_name_offsets", tnres, err);
	    }
	    ores = dwarf_offdie(dbg, die_off, &die, &err);
	    if (ores != DW_DLV_OK)
		print_error(dbg, "dwarf_offdie", ores, err);
	    cures = dwarf_die_CU_offset(die, &die_CU_off, &err);
	    if (cures != DW_DLV_OK) {
		print_error(dbg, "dwarf_die_CU_offset", cures, err);
	    }
	    dores = dwarf_offdie(dbg, cu_off, &cu_die, &err);
	    if (dores != DW_DLV_OK) {
		print_error(dbg, "dwarf_offdie2", dores, err);
	    }
	    printf("type %-15s die %lld, cu-die %lld,"
		   " off-in-cu %lld, cu %lld",
		   name, (long long) die_off, (long long) cu_off,
		   /* the cu die offset */
		   (long long) die_CU_off,
		   /* following is absolute offset of the ** beginning
		      of the cu */
		   (long long) (die_off - die_CU_off));
	    {
		Dwarf_Off off = 0;
		int cures3 = dwarf_type_cu_offset(typebuf[i],
						  &off, &err);

		if (cures3 != DW_DLV_OK) {
		    print_error(dbg, "dwarf_arange_cu_offset",
				cures3, err);
		}
		if ((die_off - die_CU_off) != off) {
		    printf(" error: real cuhdr %llu", off);
		    exit(1);
		}
		if (verbose)
		    printf(" cuhdr %llu", off);

	    }
	    printf("\n");

	    dwarf_dealloc(dbg, name, DW_DLA_STRING);
	    /* print associated die too? */
	    dwarf_dealloc(dbg, typebuf[i], DW_DLA_TYPENAME);
	}
	dwarf_dealloc(dbg, typebuf, DW_DLA_LIST);
    }
}

/* get all the data in .debug_weaknames */
extern void
print_weaknames(Dwarf_Debug dbg)
{
    Dwarf_Weak *weaknamebuf;
    Dwarf_Signed count;
    Dwarf_Signed i;
    Dwarf_Die die;
    Dwarf_Off die_off;
    Dwarf_Off die_CU_off;
    Dwarf_Die cu_die;
    Dwarf_Off cu_off;
    char *name;
    int wkres;

    printf("\n.debug_weaknames\n");
    wkres = dwarf_get_weaks(dbg, &weaknamebuf, &count, &err);
    if (wkres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_get_weaks", wkres, err);
    } else if (wkres == DW_DLV_NO_ENTRY) {
	/* no weaknames */
    } else {
	for (i = 0; i < count; i++) {
	    int tnres;
	    int ores;
	    int cures;
	    int dores;

	    tnres = dwarf_weak_name_offsets(weaknamebuf[i],
					    &name, &die_off, &cu_off,
					    &err);
	    if (tnres != DW_DLV_OK) {
		print_error(dbg, "dwarf_weak_name_offsets", tnres, err);
	    }
	    ores = dwarf_offdie(dbg, die_off, &die, &err);
	    if (ores != DW_DLV_OK)
		print_error(dbg, "dwarf_offdie", ores, err);
	    cures = dwarf_die_CU_offset(die, &die_CU_off, &err);
	    if (cures != DW_DLV_OK) {
		print_error(dbg, "dwarf_die_CU_offset", cures, err);
	    }
	    dores = dwarf_offdie(dbg, cu_off, &cu_die, &err);
	    if (dores != DW_DLV_OK) {
		print_error(dbg, "dwarf_offdie2", dores, err);
	    }
	    printf("weakname %-15s die %lld, cu-die %lld,"
		   " off-in-cu %lld, cu %lld",
		   name, (long long) die_off, (long long) cu_off,
		   /* the cu die offset */
		   (long long) die_CU_off,
		   /* following is absolute offset of the ** beginning
		      of the cu */
		   (long long) (die_off - die_CU_off));

	    {
		Dwarf_Off off = 0;
		int cures3 = dwarf_weak_cu_offset(weaknamebuf[i],
						  &off, &err);

		if (cures3 != DW_DLV_OK) {
		    print_error(dbg, "dwarf_arange_cu_offset",
				cures3, err);
		}
		if ((die_off - die_CU_off) != off) {
		    printf(" error: real cuhdr %llu", off);
		    exit(1);
		}
		if (verbose)
		    printf(" cuhdr %llu", off);

	    }
	    printf("\n");


	    dwarf_dealloc(dbg, name, DW_DLA_STRING);
	    /* print associated die too? */
	    dwarf_dealloc(dbg, weaknamebuf[i], DW_DLA_WEAK);
	}
	dwarf_dealloc(dbg, weaknamebuf, DW_DLA_LIST);
    }
}

static void
printreg(Dwarf_Signed reg)
{
    printf("%s", regnames[reg]);
}


/*
    decode ULEB
*/
static Dwarf_Unsigned
local_dwarf_decode_u_leb128(unsigned char *leb128,
			    unsigned int *leb128_length)
{
    unsigned char byte;
    Dwarf_Unsigned number;
    unsigned int shift;
    unsigned int byte_length;

    number = 0;
    shift = 0;
    byte_length = 1;
    byte = *leb128;
    for (;;) {
	number |= (byte & 0x7f) << shift;
	shift += 7;

	if ((byte & 0x80) == 0) {
	    if (leb128_length != NULL)
		*leb128_length = byte_length;
	    return (number);
	}

	byte_length++;
	byte = *(++leb128);
    }
}
