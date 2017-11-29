/* 
  Copyright (C) 2000,2004 Silicon Graphics, Inc.  All Rights Reserved.

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


$Header: /proj/osprey/CVS/open64/osprey1.0/libdwarf/dwarfdump/print_die.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include "globals.h"
#include "dwarf_names.h"

static void get_attr_value(Dwarf_Debug dbg, Dwarf_Half tag,
			   Dwarf_Attribute attrib, char **srcfiles,
			   Dwarf_Signed cnt);
static void print_attribute(Dwarf_Debug dbg, Dwarf_Die die,
			    Dwarf_Half attr,
			    Dwarf_Attribute actual_addr,
			    boolean print_information, char **srcfiles,
			    Dwarf_Signed cnt);
static void get_location_list(Dwarf_Debug dbg, Dwarf_Die die,
			      Dwarf_Attribute attr);
static int tag_attr_combination(Dwarf_Half tag, Dwarf_Half attr);

static int indent_level = 0;
static boolean local_symbols_already_began = FALSE;

Dwarf_Off fde_offset_for_cu_low = DW_DLV_BADOFFSET;
Dwarf_Off fde_offset_for_cu_high = DW_DLV_BADOFFSET;

/* Dwarf_Half list_of_attrs[] */
/*#include "at_list.i" unreferenced */

#define DIE_STACK_SIZE 50
static Dwarf_Die die_stack[DIE_STACK_SIZE];

/* a buffer to hold attribute values - initialized in print_infos() */
string attrib_buf;
int attrib_bufsiz;

#define PUSH_DIE_STACK(x) { die_stack[indent_level] = x; }
#define POP_DIE_STACK { die_stack[indent_level] = 0; }

#include "_tag_tree_table.c"

/*
   Look only at valid table entries
   The check here must match the building-logic in
   tag_tree.c
   And must match the tags defined in dwarf.h
*/
#define MAX_CHECKED_TAG_ID 0x35
static int
tag_tree_combination(Dwarf_Half tag_parent, Dwarf_Half tag_child)
{
    if (tag_parent > 0 && tag_parent <= MAX_CHECKED_TAG_ID
	&& tag_child > 0 && tag_child <= MAX_CHECKED_TAG_ID) {
	return ((tag_tree_combination_table[tag_parent]
		 [tag_child / 0x20]
		 & (1 << (tag_child % 0x20))) > 0 ? TRUE : FALSE);
    } else
	return (FALSE);
}

/* recursively follow the die tree */
extern void
print_die_and_children(Dwarf_Debug dbg, Dwarf_Die in_die_in,
		       char **srcfiles, Dwarf_Signed cnt)
{
    Dwarf_Die child;
    Dwarf_Die sibling;
    Dwarf_Error err;
    int tres;
    int cdres;
    Dwarf_Die in_die = in_die_in;

    for (;;) {
	PUSH_DIE_STACK(in_die);

	if (check_tag_tree) {
	    tag_tree_result.checks++;
	    if (indent_level == 0) {
		Dwarf_Half tag;

		tres = dwarf_tag(in_die, &tag, &err);
		if (tres != DW_DLV_OK) {
		    tag_tree_result.errors++;
		    DWARF_CHECK_ERROR
			("Tag-tree root is not DW_TAG_compile_unit")
		} else if (tag == DW_TAG_compile_unit) {
		    /* OK */
		} else {
		    tag_tree_result.errors++;
		    DWARF_CHECK_ERROR
			("tag-tree root is not DW_TAG_compile_unit")
		}
	    } else {
		Dwarf_Half tag_parent, tag_child;
		int pres;
		int cres;

		pres =
		    dwarf_tag(die_stack[indent_level - 1], &tag_parent,
			      &err);
		cres = dwarf_tag(in_die, &tag_child, &err);
		if (pres != DW_DLV_OK)
		    tag_parent = 0;
		if (cres != DW_DLV_OK)
		    tag_child = 0;
		if (pres != cres) {
		    if (cres == DW_DLV_OK) {
			DWARF_CHECK_ERROR2(get_TAG_name(dbg, tag_child),
					   "Tag-tree relation is not valid.")
		    } else {
			DWARF_CHECK_ERROR2("<child has no name>",
					   "Tag-tree relation is not valid..")
		    }
		} else if (pres != DW_DLV_OK) {
		    if (cres == DW_DLV_OK) {
			DWARF_CHECK_ERROR2(get_TAG_name(dbg, tag_child),
					   "Tag-tree Relation is not valid...")
		    } else {
			DWARF_CHECK_ERROR2("<child has no name>",
					   "Tag-tree relation is not valid....")
		    }
		} else if (tag_tree_combination(tag_parent, tag_child)) {
		    /* OK */
		} else {
		    DWARF_CHECK_ERROR2(get_TAG_name(dbg, tag_child),
				       "tag-tree relation is not valid")
		}
	    }
	}

	/* here to pre-descent processing of the die */
	print_one_die(dbg, in_die, info_flag, srcfiles, cnt);

	cdres = dwarf_child(in_die, &child, &err);
	/* child first: we are doing depth-first walk */
	if (cdres == DW_DLV_OK) {
	    indent_level++;
	    print_die_and_children(dbg, child, srcfiles, cnt);
	    indent_level--;
	    if (indent_level == 0)
		local_symbols_already_began = FALSE;
	    dwarf_dealloc(dbg, child, DW_DLA_DIE);
	} else if (cdres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_child", cdres, err);
	}

	cdres = dwarf_siblingof(dbg, in_die, &sibling, &err);
	if (cdres == DW_DLV_OK) {
	    /* print_die_and_children(dbg, sibling, srcfiles, cnt); We 
	       loop around to actually print this, rather than
	       recursing. Recursing is horribly wasteful of stack
	       space. */
	} else if (cdres == DW_DLV_ERROR) {
	    print_error(dbg, "dwarf_siblingof", cdres, err);
	}

	/* Here do any post-descent (ie post-dwarf_child) processing
	   of the in_die. */

	POP_DIE_STACK;
	if (in_die != in_die_in) {
	    /* Dealloc our in_die, but not the argument die, it belongs 
	       to our caller. Whether the siblingof call worked or not. 
	     */
	    dwarf_dealloc(dbg, in_die, DW_DLA_DIE);
	}
	if (cdres == DW_DLV_OK) {
	    /* Set to process the sibling, loop again. */
	    in_die = sibling;
	} else {
	    /* We are done, no more siblings at this level. */

	    break;
	}
    }				/* end for loop on siblings */
    return;
}

#define SPACE(x) { register int i; for (i=0;i<x;i++) putchar(' '); }


/* print info about die */
void
print_one_die(Dwarf_Debug dbg, Dwarf_Die die, boolean print_information,
	      char **srcfiles, Dwarf_Signed cnt)
{
    Dwarf_Signed i;
    Dwarf_Off offset, overall_offset;
    string tagname;
    Dwarf_Half tag;
    Dwarf_Signed atcnt;
    Dwarf_Attribute *atlist;
    int tres;
    int ores;
    int atres;

    tres = dwarf_tag(die, &tag, &err);
    if (tres != DW_DLV_OK) {
	print_error(dbg, "accessing tag of die!", tres, err);
    }
    tagname = get_TAG_name(dbg, tag);
    ores = dwarf_dieoffset(die, &overall_offset, &err);
    if (ores != DW_DLV_OK) {
	print_error(dbg, "dwarf_dieoffset", ores, err);
    }
    ores = dwarf_die_CU_offset(die, &offset, &err);
    if (ores != DW_DLV_OK) {
	print_error(dbg, "dwarf_die_CU_offset", ores, err);
    }

    if (!dst_format && print_information) {
	if (indent_level == 0) {
	    if (dense)
		printf("\n");
	    else {
		printf
		    ("\nCOMPILE_UNIT<header overall offset = %llu>:\n",
		     overall_offset - offset);
	    }
	} else if (local_symbols_already_began == FALSE &&
		   indent_level == 1 && !dense) {
	    printf("\nLOCAL_SYMBOLS:\n");
	    local_symbols_already_began = TRUE;
	}
	if (dense) {
	    SPACE(2 * indent_level);
	    if (indent_level == 0) {
		printf("<%d><%llu+%llu><%s>", indent_level,
		       overall_offset - offset, offset, tagname);
	    } else {
		printf("<%d><%llu><%s>", indent_level, offset, tagname);
	    }
	} else {
	    printf("<%d><%5llu>\t%s\n", indent_level, offset, tagname);
	}
    }

    atres = dwarf_attrlist(die, &atlist, &atcnt, &err);
    if (atres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_attrlist", atres, err);
    } else if (atres == DW_DLV_NO_ENTRY) {
	/* indicates there are no attrs.  It is not an error. */
	atcnt = 0;
    }


    for (i = 0; i < atcnt; i++) {
	Dwarf_Half attr;
	int ares;

	ares = dwarf_whatattr(atlist[i], &attr, &err);
	if (ares == DW_DLV_OK) {
	    print_attribute(dbg, die, attr,
			    atlist[i],
			    print_information, srcfiles, cnt);
	} else {
	    print_error(dbg, "dwarf_whatattr entry missing", ares, err);
	}
    }

    for (i = 0; i < atcnt; i++) {
	dwarf_dealloc(dbg, atlist[i], DW_DLA_ATTR);
    }
    if (atres == DW_DLV_OK) {
	dwarf_dealloc(dbg, atlist, DW_DLA_LIST);
    }

    if (dense && print_information) {
	printf("\n\n");
    }
    return;
}


static void
print_attribute(Dwarf_Debug dbg, Dwarf_Die die, Dwarf_Half attr,
		Dwarf_Attribute attr_in,
		boolean print_information,
		char **srcfiles, Dwarf_Signed cnt)
{
    Dwarf_Attribute attrib;
    Dwarf_Signed sval;
    string atname;
    string valname;
    int tres;
    int vres;
    Dwarf_Half tag;

    atname = get_AT_name(dbg, attr);

    /* the following gets the real attribute, even in the ** face of an 
       incorrect doubling, or worse, of attributes */
    attrib = attr_in;
    /* do not get attr via dwarf_attr: if there are (erroneously) **
       multiple of an attr in a DIE, dwarf_attr will ** not get the
       second, erroneous one and dwarfdump ** will print the first one
       multiple times. Oops. */

    tres = dwarf_tag(die, &tag, &err);
    if (tres == DW_DLV_ERROR) {
	tag = 0;
    } else if (tres == DW_DLV_NO_ENTRY) {
	tag = 0;
    } else {
	/* ok */
    }
    if (check_attr_tag) {
	attr_tag_result.checks++;
	if (tres == DW_DLV_ERROR) {
	    attr_tag_result.errors++;
	    DWARF_CHECK_ERROR2(get_AT_name(dbg, attr),
			       "make sure of the tag-attr combination..")
	} else if (tres == DW_DLV_NO_ENTRY) {
	    attr_tag_result.errors++;
	    DWARF_CHECK_ERROR2(get_AT_name(dbg, attr),
			       "make sure of the tag-attr combination..")
	} else if (tag_attr_combination(tag, attr)) {
	    /* OK */
	} else {
	    attr_tag_result.errors++;
	    DWARF_CHECK_ERROR2(get_AT_name(dbg, attr),
			       "make sure of the tag-attr combination")
	}
    }

    switch (attr) {
    case DW_AT_language:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_LANG_name(dbg, sval);
	break;
    case DW_AT_accessibility:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_ACCESS_name(dbg, sval);
	break;
    case DW_AT_visibility:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_VIS_name(dbg, sval);
	break;
    case DW_AT_virtuality:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_VIRTUALITY_name(dbg, sval);
	break;
    case DW_AT_identifier_case:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_ID_name(dbg, sval);
	break;
    case DW_AT_inline:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_INL_name(dbg, sval);
	break;
    case DW_AT_encoding:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_ATE_name(dbg, sval);
	break;
    case DW_AT_ordering:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_ORD_name(dbg, sval);
	break;
    case DW_AT_calling_convention:
	vres = dwarf_formsdata(attrib, &sval, &err);
	if (vres != DW_DLV_OK) {
	    sval = -1;
	}
	valname = get_CC_name(dbg, sval);
	break;
    case DW_AT_location:
    case DW_AT_data_member_location:
    case DW_AT_vtable_elem_location:
    case DW_AT_string_length:
    case DW_AT_return_addr:
    case DW_AT_use_location:
    case DW_AT_static_link:
    case DW_AT_frame_base:
	/* value is a location description or location list */
	get_location_list(dbg, die, attrib);
	valname = attrib_buf;
	break;
    default:
	get_attr_value(dbg, tag, attrib, srcfiles, cnt);
	valname = attrib_buf;
	break;
    }
    if (print_information) {
	if (dense)
	    printf(" %s<%s>", atname, valname);
	else
	    printf("\t\t%-28s%s\n", atname, valname);
    }
}



static int
_dwarf_print_one_locdesc(Dwarf_Debug dbg,
			 Dwarf_Locdesc * llbuf,
			 char *start_buf, char **out_buf)
{
    char *p = start_buf;
    Dwarf_Locdesc *locd;
    Dwarf_Half no_of_ops = 0;
    string op_name;
    int i;

    if (verbose || llbuf->ld_from_loclist) {
	sprintf(p, "<lowpc=0x%llx>",
		(unsigned long long) llbuf->ld_lopc);
	p += strlen(p);
	sprintf(p, "<highpc=0x%llx>",
		(unsigned long long) llbuf->ld_hipc);
	p += strlen(p);
	if (verbose) {
	    sprintf(p, "<from %s offset 0x%llx>",
		    llbuf->
		    ld_from_loclist ? ".debug_loc" : ".debug_info",
		    (unsigned long long) llbuf->ld_section_offset);
	    p += strlen(p);
	}
    }


    locd = llbuf;
    no_of_ops = llbuf->ld_cents;
    for (i = 0; i < no_of_ops; i++) {
	Dwarf_Small op;
	Dwarf_Unsigned opd1, opd2;

	if (i > 0)
	    *p++ = ' ';

	op = locd->ld_s[i].lr_atom;
	if (op > DW_OP_nop) {
	    print_error(dbg, "dwarf_op unexpected value", DW_DLV_OK,
			err);
	    return DW_DLV_ERROR;
	}
	op_name = get_OP_name(dbg, op);
	strcpy(p, op_name);
	p += strlen(op_name);
	opd1 = locd->ld_s[i].lr_number;
	if (op >= DW_OP_breg0 && op <= DW_OP_breg31) {
	    sprintf(p, "%+lld", (Dwarf_Signed) opd1);
	    p += strlen(p);
	} else {
	    switch (op) {
	    case DW_OP_addr:
		sprintf(p, " %#llx", opd1);
		p += strlen(p);
		break;
	    case DW_OP_const1s:
	    case DW_OP_const2s:
	    case DW_OP_const4s:
	    case DW_OP_const8s:
	    case DW_OP_consts:
	    case DW_OP_skip:
	    case DW_OP_bra:
	    case DW_OP_fbreg:
		sprintf(p, " %lld", (Dwarf_Signed) opd1);
		p += strlen(p);
		break;
	    case DW_OP_const1u:
	    case DW_OP_const2u:
	    case DW_OP_const4u:
	    case DW_OP_const8u:
	    case DW_OP_constu:
	    case DW_OP_pick:
	    case DW_OP_plus_uconst:
	    case DW_OP_regx:
	    case DW_OP_piece:
	    case DW_OP_deref_size:
	    case DW_OP_xderef_size:
		sprintf(p, " %llu", opd1);
		p += strlen(p);
		break;
	    case DW_OP_bregx:
		sprintf(p, "%llu", opd1);
		p += strlen(p);
		opd2 = locd->ld_s[i].lr_number2;
		sprintf(p, "%+lld", (Dwarf_Signed) opd2);
		p += strlen(p);
		break;
	    default:
		break;
	    }
	}
    }

    *out_buf = p;
    return DW_DLV_OK;
}

/* Fill buffer with location lists 
   This is not very sensible with potentially long expressions
   or many loclist entries! Buffer overrun is possible :-( and
   not checked.  */
 /*ARGSUSED*/ static void
get_location_list(Dwarf_Debug dbg, Dwarf_Die die, Dwarf_Attribute attr)
{
    Dwarf_Locdesc *llbuf = 0;
    Dwarf_Locdesc **llbufarray = 0;
    Dwarf_Signed no_of_elements;
    Dwarf_Error err;
    int i;
    string p;
    int lres;
    int llent = 0;

    if (use_old_dwarf_loclist) {
	char *buf_out;

	lres = dwarf_loclist(attr, &llbuf, &no_of_elements, &err);
	if (lres == DW_DLV_ERROR)
	    print_error(dbg, "dwarf_loclist", lres, err);
	if (lres == DW_DLV_NO_ENTRY)
	    return;

	_dwarf_print_one_locdesc(dbg, llbuf, attrib_buf, &buf_out);
	dwarf_dealloc(dbg, llbuf->ld_s, DW_DLA_LOC_BLOCK);
	dwarf_dealloc(dbg, llbuf, DW_DLA_LOCDESC);
	return;
    }

    /* FIX: need to deal with location list here, not just 1 element
       For now we just do element 0, since there should be just 1.
       Temporarily. Till we do loclists for real (.debug_loc). */
    lres = dwarf_loclist_n(attr, &llbufarray, &no_of_elements, &err);
    if (lres == DW_DLV_ERROR)
	print_error(dbg, "dwarf_loclist", lres, err);
    if (lres == DW_DLV_NO_ENTRY)
	return;
    p = attrib_buf;

    for (llent = 0; llent < no_of_elements; ++llent) {
	char *buf_out;

	llbuf = llbufarray[llent];
	if (!dense && llbuf->ld_from_loclist) {
	    sprintf(p, "[%2d]", llent);
	    p += strlen(p);
	}
	lres = _dwarf_print_one_locdesc(dbg, llbuf, p, &buf_out);
	if (lres == DW_DLV_ERROR) {
	    return;
	}
	if (lres == DW_DLV_OK) {
	    p = buf_out;	/* So we add follow-on at end, else is
				   NO_ENTRY and nothing meaningful to
				   add. */
	}
	if ((llent + 1) < no_of_elements) {
	    /* Only add newline if sparse and there are more to print.
	       Final newline added by our caller. */
	    if (!dense) {
		sprintf(p, "\n\t\t\t\t\t");
		p += strlen(p);
	    }
	}
    }
    for (i = 0; i < no_of_elements; ++i) {
	dwarf_dealloc(dbg, llbufarray[i]->ld_s, DW_DLA_LOC_BLOCK);
	dwarf_dealloc(dbg, llbufarray[i], DW_DLA_LOCDESC);
    }
    dwarf_dealloc(dbg, llbufarray, DW_DLA_LIST);
}

/* fill buffer with attribute value 
   We pass in tag so we can try to do the right thing with
   broken compiler DW_TAG_enumerator */
static void
get_attr_value(Dwarf_Debug dbg, Dwarf_Half tag, Dwarf_Attribute attrib,
	       char **srcfiles, Dwarf_Signed cnt)
{
    Dwarf_Half theform;
    string temps;
    Dwarf_Block *tempb;
    Dwarf_Signed tempsd = 0;
    Dwarf_Unsigned tempud = 0;
    int i;
    string p;
    Dwarf_Half attr;
    Dwarf_Off off;
    Dwarf_Die die_for_check;
    Dwarf_Half tag_for_check;
    Dwarf_Bool tempbool;
    Dwarf_Addr addr = 0;
    int fres;
    int bres;
    int wres;
    int dres;
    Dwarf_Half direct_form = 0;

    fres = dwarf_whatform(attrib, &theform, &err);
    /* depending on the form and the attribute, process the form */
    if (fres == DW_DLV_ERROR) {
	print_error(dbg, "dwarf_whatform cannot find attr form", fres,
		    err);
    } else if (fres == DW_DLV_NO_ENTRY) {
	return;
    }

    dwarf_whatform_direct(attrib, &direct_form, &err);
    /* ignore errors in dwarf_whatform_direct() */


    switch (theform) {
    case DW_FORM_addr:
	bres = dwarf_formaddr(attrib, &addr, &err);
	if (bres == DW_DLV_OK) {
	    sprintf(attrib_buf, "%#llx", (unsigned long long) addr);
	} else {
	    print_error(dbg, "addr formwith no addr?!", bres, err);
	}
	break;
    case DW_FORM_ref_addr:
	/* DW_FORM_ref_addr is not accessed thru formref: ** it is an
	   address (global section offset) in ** the .debug_info
	   section. */
	bres = dwarf_global_formref(attrib, &off, &err);
	if (bres == DW_DLV_OK) {
	    sprintf(attrib_buf, "<global die offset %llu>",
		    (unsigned long long) off);
	} else {
	    print_error(dbg,
			"DW_FORM_ref_addr form with no reference?!",
			bres, err);
	}
	break;
    case DW_FORM_ref1:
    case DW_FORM_ref2:
    case DW_FORM_ref4:
    case DW_FORM_ref8:
    case DW_FORM_ref_udata:
	bres = dwarf_formref(attrib, &off, &err);
	if (bres != DW_DLV_OK) {
	    print_error(dbg, "ref formwith no ref?!", bres, err);
	}
	/* do references inside <> to distinguish them ** from
	   constants. In dense form this results in <<>>. Ugly for
	   dense form, but better than ambiguous. davea 9/94 */
	sprintf(attrib_buf, "<%llu>", off);
	if (check_type_offset) {
	    wres = dwarf_whatattr(attrib, &attr, &err);
	    if (wres == DW_DLV_ERROR) {

	    } else if (wres == DW_DLV_NO_ENTRY) {
	    }
	    if (attr == DW_AT_type) {
		dres = dwarf_offdie(dbg, cu_offset + off,
				    &die_for_check, &err);
		type_offset_result.checks++;
		if (dres != DW_DLV_OK) {
		    type_offset_result.errors++;
		    DWARF_CHECK_ERROR
			("DW_AT_type offset does not point to type info")
		} else {
		    int tres2;

		    tres2 =
			dwarf_tag(die_for_check, &tag_for_check, &err);
		    if (tres2 == DW_DLV_OK) {
			switch (tag_for_check) {
			case DW_TAG_array_type:
			case DW_TAG_class_type:
			case DW_TAG_enumeration_type:
			case DW_TAG_pointer_type:
			case DW_TAG_reference_type:
			case DW_TAG_string_type:
			case DW_TAG_structure_type:
			case DW_TAG_subroutine_type:
			case DW_TAG_typedef:
			case DW_TAG_union_type:
			case DW_TAG_ptr_to_member_type:
			case DW_TAG_set_type:
			case DW_TAG_subrange_type:
			case DW_TAG_base_type:
			case DW_TAG_const_type:
			case DW_TAG_file_type:
			case DW_TAG_packed_type:
			case DW_TAG_thrown_type:
			case DW_TAG_volatile_type:
			    /* OK */
			    break;
			default:
			    type_offset_result.errors++;
			    DWARF_CHECK_ERROR
				("DW_AT_type offset does not point to type info")
				break;
			}
			dwarf_dealloc(dbg, die_for_check, DW_DLA_DIE);
		    } else {
			type_offset_result.errors++;
			DWARF_CHECK_ERROR
			    ("DW_AT_type offset does not exist")
		    }
		}
	    }
	}
	break;
    case DW_FORM_block:
    case DW_FORM_block1:
    case DW_FORM_block2:
    case DW_FORM_block4:
	fres = dwarf_formblock(attrib, &tempb, &err);
	if (fres == DW_DLV_OK) {
	    if (tempb->bl_len >= attrib_bufsiz) {
		attrib_buf =
		    (string) realloc(attrib_buf, tempb->bl_len);
		if (attrib_buf == NULL) {
		    fprintf(stderr,
			    "ERROR: out of memory for attribute buffer\n");
		    exit(1);
		}
		attrib_bufsiz = tempb->bl_len;
	    }
	    for (i = 0, p = attrib_buf; i < tempb->bl_len; i++, p += 2) {
		sprintf(p, "%02x",
			*(i + (unsigned char *) tempb->bl_data));
	    }
	    dwarf_dealloc(dbg, tempb, DW_DLA_BLOCK);
	} else {
	    print_error(dbg, "DW_FORM_blockn cannot get block\n", fres,
			err);
	}
	break;
    case DW_FORM_data1:
    case DW_FORM_data2:
    case DW_FORM_data4:
    case DW_FORM_data8:
	fres = dwarf_whatattr(attrib, &attr, &err);
	if (fres == DW_DLV_ERROR) {
	    print_error(dbg, "FORM_datan cannot get attr", fres, err);
	} else if (fres == DW_DLV_NO_ENTRY) {
	    print_error(dbg, "FORM_datan cannot get attr", fres, err);
	} else {
	    switch (attr) {
	    case DW_AT_ordering:
	    case DW_AT_byte_size:
	    case DW_AT_bit_offset:
	    case DW_AT_bit_size:
	    case DW_AT_inline:
	    case DW_AT_language:
	    case DW_AT_visibility:
	    case DW_AT_virtuality:
	    case DW_AT_accessibility:
	    case DW_AT_address_class:
	    case DW_AT_calling_convention:
	    case DW_AT_encoding:
	    case DW_AT_identifier_case:
	    case DW_AT_MIPS_loop_unroll_factor:
	    case DW_AT_MIPS_software_pipeline_depth:
	    case DW_AT_decl_column:
	    case DW_AT_decl_file:
	    case DW_AT_decl_line:
	    case DW_AT_start_scope:
	    case DW_AT_stride_size:
	    case DW_AT_count:
	    case DW_AT_stmt_list:
	    case DW_AT_MIPS_fde:
		wres = dwarf_formudata(attrib, &tempud, &err);
		if (wres == DW_DLV_OK) {
		    /* attrib_buf is large compared to %llu output, so
		       sprintf is safe */
		    sprintf(attrib_buf, "%llu", tempud);
		    if (attr == DW_AT_decl_file) {
			if (srcfiles && tempud > 0 && tempud <= cnt) {
			    /* added by user request */
			    /* srcfiles is indexed starting at 0, but
			       DW_AT_decl_file defines that 0 means no
			       file, so tempud 1 means the 0th entry in
			       srcfiles, thus tempud-1 is the correct
			       index into srcfiles.  */
			    char *fname = srcfiles[tempud - 1];
			    size_t used_so_far = strlen(attrib_buf);
			    char *targ = attrib_buf + used_so_far;
			    size_t bytes_left = attrib_bufsiz - (used_so_far + 1	/* for 
											   NUL 
											   byte 
											   of 
											   string 
											 */
								 + 1	/* for 
									   the 
									   blank 
									   we 
									   want 
									   to 
									   add 
									 */ );
			    size_t namelen = strlen(fname) + 1;

			    if (namelen > bytes_left) {
				/* Trim off front of fname. Prefix with 
				   ... 10 is arbitrary. Small compared
				   to size of attrib_buf. Prevents
				   overruning attrib_buf 3 would be
				   enough, for ... */

				fname += (namelen - bytes_left) + 10;
				strcpy(targ, " ");
				++targ;
				strcat(targ, "...");
				strcat(targ, fname);

			    } else {
				/* ok, fits as is */
				strcpy(targ, " ");
				++targ;
				strcpy(targ, fname);
			    }
			}
		    }
		} else if (wres == DW_DLV_NO_ENTRY) {
		    /* nothing? */
		} else {
		    print_error(dbg, "Cannot get formudata..", wres,
				err);
		}
		break;
	    case DW_AT_const_value:
		wres = dwarf_formudata(attrib, &tempud, &err);
		if (wres == DW_DLV_OK) {
		    if (tag == DW_TAG_enumerator) {
			/* See bug 583450. ** we are recording
			   enumerators ** as unsigned. Wrong. Thru
			   cmplrs 7.2.1
			   ------------------------------------- **
			   Using temp int i due to compiler ** bug
			   584010 (cast to int ignored ** if no
			   variable assigned to). */
			int i = (int) tempud;

			tempsd = i;
			sprintf(attrib_buf, "%lld", tempsd);
		    } else {
			sprintf(attrib_buf, "%llu", tempud);
		    }
		} else if (wres == DW_DLV_NO_ENTRY) {
		    /* nothing? */
		} else {
		    if (tag == DW_TAG_enumerator) {
			wres = dwarf_formsdata(attrib, &tempsd, &err);
			if (wres == DW_DLV_OK) {
			    /* See bug 583450. ** we are recording
			       enumerators ** as unsigned. Wrong. Thru
			       cmplrs 7.2.1 */
			    sprintf(attrib_buf, "%lld", tempsd);
			} else if (wres == DW_DLV_NO_ENTRY) {
			    /* nothing? */
			} else {
			    print_error(dbg,
					"Cannot get formudata or formsdata..",
					wres, err);
			}
		    } else {
			print_error(dbg, "Cannot get formudata..", wres,
				    err);
		    }
		}
		break;
	    case DW_AT_upper_bound:
	    case DW_AT_lower_bound:
	    default:
		wres = dwarf_formsdata(attrib, &tempsd, &err);
		if (wres == DW_DLV_OK) {
		    sprintf(attrib_buf, "%lld", tempsd);
		} else if (wres == DW_DLV_NO_ENTRY) {
		    /* nothing? */
		} else {
		    print_error(dbg, "Cannot get formsdata..", wres,
				err);
		}
		break;
	    }
	}
	if (cu_name_flag) {
	    if (attr == DW_AT_MIPS_fde) {
		if (fde_offset_for_cu_low == DW_DLV_BADOFFSET) {
		    fde_offset_for_cu_low
			= fde_offset_for_cu_high = tempud;
		} else if (tempud < fde_offset_for_cu_low) {
		    fde_offset_for_cu_low = tempud;
		} else if (tempud > fde_offset_for_cu_high) {
		    fde_offset_for_cu_high = tempud;
		}
	    }
	}
	break;
    case DW_FORM_sdata:
	wres = dwarf_formsdata(attrib, &tempsd, &err);
	if (wres == DW_DLV_OK) {
	    sprintf(attrib_buf, "%lld", tempsd);
	} else if (wres == DW_DLV_NO_ENTRY) {
	    /* nothing? */
	} else {
	    print_error(dbg, "Cannot get formsdata..", wres, err);
	}
	break;
    case DW_FORM_udata:
	wres = dwarf_formudata(attrib, &tempud, &err);
	if (wres == DW_DLV_OK) {
	    sprintf(attrib_buf, "%llu", tempud);
	} else if (wres == DW_DLV_NO_ENTRY) {
	    /* nothing? */
	} else {
	    print_error(dbg, "Cannot get formudata....", wres, err);
	}
	break;
    case DW_FORM_string:
    case DW_FORM_strp:
	wres = dwarf_formstring(attrib, &temps, &err);
	if (wres == DW_DLV_OK) {
	    if (strlen(temps) >= attrib_bufsiz) {
		attrib_buf =
		    (string) realloc(attrib_buf, strlen(temps) + 1);
		if (attrib_buf == NULL) {
		    fprintf(stderr,
			    "ERROR: out of memory for attribute buffer\n");
		    exit(1);
		}
		attrib_bufsiz = strlen(temps) + 1;
	    }
	    sprintf(attrib_buf, "%s", temps);
	    dwarf_dealloc(dbg, temps, DW_DLA_STRING);
	} else if (wres == DW_DLV_NO_ENTRY) {
	    /* nothing? */
	} else {
	    print_error(dbg, "Cannot get formstr/p....", wres, err);
	}

	break;
    case DW_FORM_flag:
	wres = dwarf_formflag(attrib, &tempbool, &err);
	if (wres == DW_DLV_OK) {
	    if (tempbool) {
		sprintf(attrib_buf, "yes(%d)", tempbool);
	    } else {
		sprintf(attrib_buf, "no");
	    }
	} else if (wres == DW_DLV_NO_ENTRY) {
	    /* nothing? */
	} else {
	    print_error(dbg, "Cannot get formflag/p....", wres, err);
	}
	break;
    case DW_FORM_indirect:
	/* We should not ever get here, since the true form was
	   determied and direct_form has the DW_FORM_indirect if it is
	   used here in this attr. */
	sprintf(attrib_buf, get_FORM_name(dbg, theform));
	break;
    default:
	print_error(dbg, "dwarf_whatform unexpected value", DW_DLV_OK,
		    err);
    }
    if (verbose && direct_form && direct_form == DW_FORM_indirect) {
	strcat(attrib_buf, " (used DW_FORM_indirect) ");
    }
}

#include "_tag_attr_table.c"

static int
tag_attr_combination(Dwarf_Half tag, Dwarf_Half attr)
{
    if (attr > 0 && attr < 0x60) {
	return ((tag_attr_combination_table[tag][attr / 0x20]
		 & (1 << (attr % 0x20))) > 0 ? TRUE : FALSE);
    } else if (attr == DW_AT_MIPS_fde) {
	/* no check now */
	return (TRUE);
    } else
	return (FALSE);
}
