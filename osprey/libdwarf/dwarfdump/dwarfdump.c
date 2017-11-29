/* 
  Copyright (C) 2000,2002,2004 Silicon Graphics, Inc.  All Rights Reserved.

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


$Header: /proj/osprey/CVS/open64/osprey1.0/libdwarf/dwarfdump/dwarfdump.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include "globals.h"

/* for 'open' */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

extern char *optarg;

#define OKAY 0
#define FAILED 1
#define BYTES_PER_INSTRUCTION 4

#if ! defined(BUILD_OS_DARWIN)
static string process_args(int argc, char *argv[]);
static void print_infos(Dwarf_Debug dbg);

static string program_name;
int check_error = 0;

/* defined in print_die.c, buffer to hold attribute values */
extern string attrib_buf;
extern int attrib_bufsiz;

/* defined in print_sections.c, die for the current compile unit, 
   used in get_fde_proc_name() */
extern Dwarf_Die current_cu_die_for_print_frames;


boolean info_flag = FALSE;
boolean use_old_dwarf_loclist = FALSE;	/* This so both dwarf_loclist() 
					   and dwarf_loclist_n() can be
					   tested. Defaults to new
					   dwarf_loclist_n() */

static boolean line_flag = FALSE;
static boolean abbrev_flag = FALSE;
static boolean frame_flag = FALSE;
static boolean exceptions_flag = FALSE;
static boolean pubnames_flag = FALSE;
static boolean macinfo_flag = FALSE;
static boolean loc_flag = FALSE;
static boolean aranges_flag = FALSE;
static boolean string_flag = FALSE;
static boolean reloc_flag = FALSE;
static boolean static_func_flag = FALSE;
static boolean static_var_flag = FALSE;
static boolean type_flag = FALSE;
static boolean weakname_flag = FALSE;

int verbose = 0;
boolean dense = FALSE;
boolean ellipsis = FALSE;
boolean dst_format = FALSE;

boolean check_abbrev_code = FALSE;
boolean check_pubname_attr = FALSE;
boolean check_reloc_offset = FALSE;
boolean check_attr_tag = FALSE;
boolean check_tag_tree = FALSE;
boolean check_type_offset = FALSE;

static boolean dwarf_check = FALSE;

char cu_name[BUFSIZ];
boolean cu_name_flag = FALSE;
Dwarf_Unsigned cu_offset = 0;

Dwarf_Check_Result abbrev_code_result;
Dwarf_Check_Result pubname_attr_result;
Dwarf_Check_Result reloc_offset_result;
Dwarf_Check_Result attr_tag_result;
Dwarf_Check_Result tag_tree_result;
Dwarf_Check_Result type_offset_result;

Dwarf_Error err;

#define PRINT_CHECK_RESULT(str,result)  {\
    fprintf(stderr, "%-24s%8d%8d\n", str, result.checks, result.errors); \
}

static int process_one_file(Elf * elf, string file_name, int archive);

/*
 * Iterate through dwarf and print all info.
 */
int
main(int argc, char *argv[])
{
    string file_name;
    int f;
    Elf_Cmd cmd;
    Elf *arf, *elf;
    int archive = 0;

    (void) elf_version(EV_NONE);
    if (elf_version(EV_CURRENT) == EV_NONE) {
	(void) fprintf(stderr, "dwarfdump: libelf.a out of date.\n");
	exit(1);
    }

    file_name = process_args(argc, argv);
#ifdef __CYGWIN__
    f = open(file_name, O_RDONLY | O_BINARY);
#else
    f = open(file_name, O_RDONLY);
#endif
    if (f == -1) {
	fprintf(stderr, "%s ERROR:  can't open %s\n", program_name,
		file_name);
	return (FAILED);
    }

    cmd = ELF_C_READ;
    arf = elf_begin(f, cmd, (Elf *) 0);
    if (elf_kind(arf) == ELF_K_AR) {
	archive = 1;
    }
    while ((elf = elf_begin(f, cmd, arf)) != 0) {
	Elf32_Ehdr *eh32;

#ifdef HAVE_ELF64_GETEHDR
	Elf64_Ehdr *eh64;
#endif /* HAVE_ELF64_GETEHDR */
	eh32 = elf32_getehdr(elf);
	if (!eh32) {
#ifdef HAVE_ELF64_GETEHDR
	    /* not a 32-bit obj */
	    eh64 = elf64_getehdr(elf);
	    if (!eh64) {
		/* not a 64-bit obj either! */
		/* dwarfdump is quiet when not an object */
	    } else {
		process_one_file(elf, file_name, archive);
	    }
#endif /* HAVE_ELF64_GETEHDR */
	} else {
	    process_one_file(elf, file_name, archive);
	}
	cmd = elf_next(elf);
	elf_end(elf);
    }
    elf_end(arf);
    if (check_error)
	return FAILED;
    else
	return OKAY;
}

/*
  Given a file which we know is an elf file, process
  the dwarf data.

*/
static int
process_one_file(Elf * elf, string file_name, int archive)
{
    Dwarf_Debug dbg;
    int dres;

    dres = dwarf_elf_init(elf, DW_DLC_READ, NULL, NULL, &dbg, &err);
    if (dres == DW_DLV_NO_ENTRY) {
	printf("No DWARF information present in %s\n", file_name);
	return 0;
    }
    if (dres != DW_DLV_OK) {
	print_error(dbg, "dwarf_elf_init", dres, err);
    }
    /* allocate buffer for attribute value */
    attrib_buf = (string) malloc(ATTRIB_BUFSIZ);
    if (attrib_buf == NULL) {
	fprintf(stderr, "%s ERROR: mallocing %d bytes for buffer\n",
		program_name, ATTRIB_BUFSIZ);
	exit(FAILED);
    }
    attrib_bufsiz = ATTRIB_BUFSIZ;


    if (archive) {
	Elf_Arhdr *mem_header = elf_getarhdr(elf);

	printf("\narchive member \t%s\n",
	       mem_header ? mem_header->ar_name : "");
    }

    if (info_flag || line_flag || cu_name_flag)
	print_infos(dbg);
    if (pubnames_flag)
	print_pubnames(dbg);
    if (macinfo_flag)
	print_macinfo(dbg);
    if (loc_flag)
	print_locs(dbg);
    if (abbrev_flag)
	print_abbrevs(dbg);
    if (string_flag)
	print_strings(dbg);
    if (aranges_flag)
	print_aranges(dbg);
    if (frame_flag) {
	current_cu_die_for_print_frames = 0;
	print_frames(dbg);
    }
    if (static_func_flag)
	print_static_funcs(dbg);
    if (static_var_flag)
	print_static_vars(dbg);
    if (type_flag)
	print_types(dbg);
    if (weakname_flag)
	print_weaknames(dbg);
    if (reloc_flag)
	print_relocinfo(dbg);
    if (dwarf_check) {
	fprintf(stderr, "DWARF CHECK RESULT\n");
	fprintf(stderr, "<item>                  <checks><errors>\n");
    }
    if (check_pubname_attr)
	PRINT_CHECK_RESULT("pubname_attr", pubname_attr_result)
	    if (check_attr_tag)
	    PRINT_CHECK_RESULT("attr_tag", attr_tag_result)
		if (check_tag_tree)
		PRINT_CHECK_RESULT("tag_tree", tag_tree_result)
		    if (check_type_offset)
		    PRINT_CHECK_RESULT("type_offset",
				       type_offset_result)

			dres = dwarf_finish(dbg, &err);
    if (dres != DW_DLV_OK) {
	print_error(dbg, "dwarf_finish", dres, err);
    }
    /* free attrib_buf */
    if (attrib_buf != NULL)
	free(attrib_buf);


    return 0;

}

/* process arguments and return object filename */
static string
process_args(int argc, char *argv[])
{
    extern int optind;
    int c;
    boolean usage_error = FALSE;
    int oarg;

    program_name = argv[0];

    while ((c =
	    getopt(argc, argv, "ailfghbprmcsvdeok:gu:t:ywz")) != EOF) {
	switch (c) {
	case 'g':
	    use_old_dwarf_loclist = TRUE;
	case 'i':
	    info_flag = TRUE;
	    break;
	case 'l':
	    line_flag = TRUE;
	    break;
	case 'f':
	    frame_flag = TRUE;
	    break;
	case 'b':
	    abbrev_flag = TRUE;
	    break;
	case 'p':
	    pubnames_flag = TRUE;
	    break;
	case 'r':
	    aranges_flag = TRUE;
	    break;
	case 'm':
	    macinfo_flag = TRUE;
	    break;
	case 'c':
	    loc_flag = TRUE;
	    break;
	case 's':
	    string_flag = TRUE;
	    break;
	case 'a':
	    info_flag = line_flag = frame_flag = abbrev_flag = TRUE;
	    pubnames_flag = aranges_flag = macinfo_flag = TRUE;
	    loc_flag = string_flag = TRUE;
	    reloc_flag = TRUE;
	    static_func_flag = static_var_flag = TRUE;
	    type_flag = weakname_flag = TRUE;
	    break;
	case 'v':
	    verbose++;
	    break;
	case 'd':
	    dense = TRUE;
	    break;
	case 'e':
	    ellipsis = TRUE;
	    break;
	case 'o':
	    reloc_flag = TRUE;
	    break;
	case 'k':
	    dwarf_check = TRUE;
	    oarg = optarg[0];
	    switch (oarg) {
	    case 'a':
		check_pubname_attr = TRUE;
		check_attr_tag = TRUE;
		check_tag_tree = check_type_offset = TRUE;
		pubnames_flag = info_flag = TRUE;
		break;
	    case 'e':
		check_pubname_attr = TRUE;
		pubnames_flag = TRUE;
		break;
	    case 'r':
		check_attr_tag = TRUE;
		info_flag = TRUE;
		break;
	    case 't':
		check_tag_tree = TRUE;
		info_flag = TRUE;
		break;
	    case 'y':
		check_type_offset = TRUE;
		info_flag = TRUE;
		break;
	    default:
		usage_error = TRUE;
		break;
	    }
	    break;
	case 'u':		/* compile unit */
	    cu_name_flag = TRUE;
	    strcpy(cu_name, optarg);
	    break;
	case 't':
	    oarg = optarg[0];
	    switch (oarg) {
	    case 'a':
		/* all */
		static_func_flag = static_var_flag = TRUE;
		break;
	    case 'f':
		/* .debug_static_func */
		static_func_flag = TRUE;
		break;
	    case 'v':
		/* .debug_static_var */
		static_var_flag = TRUE;
		break;
	    default:
		usage_error = TRUE;
		break;
	    }
	    break;
	case 'y':		/* .debug_types */
	    type_flag = TRUE;
	    break;
	case 'w':		/* .debug_weaknames */
	    weakname_flag = TRUE;
	    break;
	case 'z':
	    fprintf(stderr, "-z is no longer supported:ignored\n");
	    break;
	default:
	    usage_error = TRUE;
	    break;
	}
    }
    if (usage_error || (optind != (argc - 1))) {
	fprintf(stderr, "Usage:  %s <options> <object file>\n",
		program_name);
	fprintf(stderr, "options:\t-a\tprint all sections\n");
	fprintf(stderr, "\t\t-b\tprint abbrev section\n");
	fprintf(stderr, "\t\t-c\tprint loc section\n");
	fprintf(stderr,
		"\t\t-d\tdense: one line per entry (info section only)\n");
	fprintf(stderr,
		"\t\t-e\tellipsis: short names for tags, attrs etc.\n");
	fprintf(stderr, "\t\t-f\tprint frame section\n");
	fprintf(stderr, "\t\t-g\t(use incomplete loclist support)\n");
	fprintf(stderr, "\t\t-h\tprint exception tables\n");
	fprintf(stderr, "\t\t-i\tprint info section\n");
	fprintf(stderr, "\t\t-k[aerty] check dwarf information\n");
	fprintf(stderr, "\t\t   a\tdo all checks\n");
	fprintf(stderr, "\t\t   e\texamine attributes of pubnames\n");
	fprintf(stderr, "\t\t   r\texamine attr-tag relation\n");
	fprintf(stderr, "\t\t   t\texamine tag trees\n");
	fprintf(stderr, "\t\t   y\texamine type info\n");
	fprintf(stderr, "\t\t-l\tprint line section\n");
	fprintf(stderr, "\t\t-m\tprint macinfo section\n");
	fprintf(stderr, "\t\t-o\tprint relocation info\n");
	fprintf(stderr, "\t\t-p\tprint pubnames section\n");
	fprintf(stderr, "\t\t-r\tprint aranges section\n");
	fprintf(stderr, "\t\t-s\tprint string section\n");
	fprintf(stderr, "\t\t-t[afv] static: \n");
	fprintf(stderr, "\t\t   a\tprint both sections\n");
	fprintf(stderr, "\t\t   f\tprint static func section\n");
	fprintf(stderr, "\t\t   v\tprint static var section\n");
	fprintf(stderr,
		"\t\t-u<file> print sections only for specified file\n");
	fprintf(stderr, "\t\t-v\tverbose: show more information\n");
	fprintf(stderr, "\t\t-w\tprint weakname section\n");
	fprintf(stderr, "\t\t-y\tprint type section\n");
	exit(FAILED);
    }
    return argv[optind];
}

/* process each compilation unit in .debug_info */
static void
print_infos(Dwarf_Debug dbg)
{
    Dwarf_Unsigned cu_header_length;
    Dwarf_Unsigned abbrev_offset;
    Dwarf_Half version_stamp;
    Dwarf_Half address_size;
    Dwarf_Die cu_die;
    Dwarf_Unsigned next_cu_offset;
    int nres;

    if (info_flag)
	printf("\n.debug_info\n");

    /* loop until it returns 0 */
    while ((nres =
	    dwarf_next_cu_header(dbg, &cu_header_length, &version_stamp,
				 &abbrev_offset, &address_size,
				 &next_cu_offset, &err))
	   == DW_DLV_OK) {
	int sres;

	if (cu_name_flag) {
	    int tres;
	    Dwarf_Half tag;
	    Dwarf_Attribute attrib;
	    Dwarf_Half theform;
	    int fres;
	    int ares;

	    sres = dwarf_siblingof(dbg, NULL, &cu_die, &err);
	    if (sres != DW_DLV_OK) {
		print_error(dbg, "siblingof cu header", sres, err);
	    }
	    tres = dwarf_tag(cu_die, &tag, &err);
	    if (tres != DW_DLV_OK) {
		print_error(dbg, "tag of cu die", tres, err);
	    }
	    ares = dwarf_attr(cu_die, DW_AT_name, &attrib, &err);
	    if (ares != DW_DLV_OK) {
		print_error(dbg, "dwarf DW_AT_name ", ares, err);
	    }
	    fres = dwarf_whatform(attrib, &theform, &err);
	    if (fres != DW_DLV_OK) {
		print_error(dbg, "dwarf_whatform problem ", fres, err);
	    } else if (theform == DW_FORM_string
		       || theform == DW_FORM_strp) {
		string temps;
		int strres;
		string p;

		strres = dwarf_formstring(attrib, &temps, &err);
		p = temps;
		if (strres != DW_DLV_OK) {
		    print_error(dbg,
				"formstring failed unexpectedly",
				strres, err);
		}
		if (cu_name[0] != '/') {
		    p = strrchr(temps, '/');
		    if (p == NULL) {
			p = temps;
		    } else {
			p++;
		    }
		}
		if (strcmp(cu_name, p)) {
		    dwarf_dealloc(dbg, temps, DW_DLA_STRING);
		    continue;
		}
		dwarf_dealloc(dbg, temps, DW_DLA_STRING);
	    } else {
		print_error(dbg,
			    "dwarf_whatform unexpected value",
			    fres, err);
	    }
	    dwarf_dealloc(dbg, attrib, DW_DLA_ATTR);
	    dwarf_dealloc(dbg, cu_die, DW_DLA_DIE);
	}
	if (verbose && info_flag) {
	    if (dense) {
		printf("<%s>", "cu_header");
		printf(" %s<%llu>", "cu_header_length",
		       cu_header_length);
		printf(" %s<%d>", "version_stamp", version_stamp);
		printf(" %s<%llu>", "abbrev_offset", abbrev_offset);
		printf(" %s<%d>\n", "address_size", address_size);

	    } else {
		printf("\nCU_HEADER:\n");
		printf("\t\t%-28s%llu\n", "cu_header_length",
		       cu_header_length);
		printf("\t\t%-28s%d\n", "version_stamp", version_stamp);
		printf("\t\t%-28s%llu\n", "abbrev_offset",
		       abbrev_offset);
		printf("\t\t%-28s%d", "address_size", address_size);
	    }
	}

	/* process a single compilation unit in .debug_info. */
	sres = dwarf_siblingof(dbg, NULL, &cu_die, &err);
	if (sres == DW_DLV_OK) {
	    if (info_flag || cu_name_flag) {
		Dwarf_Signed cnt = 0;
		char **srcfiles = 0;
		int srcf = dwarf_srcfiles(cu_die,
					  &srcfiles, &cnt, &err);

		if (srcf != DW_DLV_OK) {
		    srcfiles = 0;
		    cnt = 0;
		}

		print_die_and_children(dbg, cu_die, srcfiles, cnt);
		if (srcf == DW_DLV_OK) {
		    int si;

		    for (si = 0; si < cnt; ++si) {
			dwarf_dealloc(dbg, srcfiles[si], DW_DLA_STRING);
		    }
		    dwarf_dealloc(dbg, srcfiles, DW_DLA_LIST);
		}
	    }
	    if (line_flag)
		print_line_numbers_this_cu(dbg, cu_die);
	    dwarf_dealloc(dbg, cu_die, DW_DLA_DIE);
	} else if (sres == DW_DLV_NO_ENTRY) {
	    /* do nothing I guess. */
	} else {
	    print_error(dbg, "Regetting cu_die", sres, err);
	}
	cu_offset = next_cu_offset;
    }
    if (nres == DW_DLV_ERROR) {
	string errmsg = dwarf_errmsg(err);
	long long myerr = dwarf_errno(err);

	fprintf(stderr, "%s ERROR:  %s:  %s (%lld)\n",
		program_name, "attempting to print .debug_info",
		errmsg, myerr);
	fprintf(stderr, "attempting to continue.\n");
    }
}

#else /* ! defined(BUILD_OS_DARWIN) */
static string program_name = "";
boolean ellipsis = FALSE;
int main() { fprintf(stderr, "Not yet supported on Mac OS\n"); return 1; }
#endif /* ! defined(BUILD_OS_DARWIN) */

/* ARGSUSED */
void
print_error(Dwarf_Debug dbg, string msg, int dwarf_code,
	    Dwarf_Error err)
{
    if (dwarf_code == DW_DLV_ERROR) {
	string errmsg = dwarf_errmsg(err);
	long long myerr = dwarf_errno(err);

	fprintf(stderr, "%s ERROR:  %s:  %s (%lld)\n",
		program_name, msg, errmsg, myerr);
    } else if (dwarf_code == DW_DLV_NO_ENTRY) {
	fprintf(stderr, "%s NO ENTRY:  %s: \n", program_name, msg);
    } else if (dwarf_code == DW_DLV_OK) {
	fprintf(stderr, "%s:  %s \n", program_name, msg);
    } else {
	fprintf(stderr, "%s InternalError:  %s:  code %d\n",
		program_name, msg, dwarf_code);
    }
    exit(FAILED);

}
