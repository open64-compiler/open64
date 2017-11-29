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


#ifndef __IPA_OPTION_H__
#define __IPA_OPTION_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * These are for passing option information from the
 * static linker to ipa without involving the linkers
 * internal option table. Any enumerated types defined
 * here can be added to,  but not altered in any other
 * way. This allows the linker to change without affecting
 * ipa.
 */

typedef enum{
    LD_IPA_SHARABLE, 
    LD_IPA_DEMANGLE, 
    LD_IPA_SHOW, 
    LD_IPA_HIDES, 
    LD_IPA_TARGOS, 
    LD_IPA_VERBOSE, 
    LD_IPA_KEEP_TEMPS, 
    LD_IPA_ISA,
    LD_IPA_XXXX, 
#ifdef TARG_SL
    LD_IPA_IPISR,
#endif
    MAX_LD_IPA
}ld_ipa_option_enum;

typedef struct ld_ipa_option {
    ld_ipa_option_enum opt_ndx;
    unsigned    flag		: 4;    /*  */
    unsigned     set		: 4;    /*  */
} LD_IPA_OPTION;

extern LD_IPA_OPTION ld_ipa_opt[MAX_LD_IPA];

#define HS_DEFAULT 0
#define HS_HIDES 1
#define HS_EXPORTS 2
#define HS_IGNORE 3

    /*
     * The following struct is used for storing lists
     * of names. Most often these lists are gathered
     * from the commandline, but not always.
     */
typedef struct {    /* all symbols specified in command line */
    char **sym;     /* are collected here, one type per entry */
    int    num;
    int	   max;
} OPTION_SYM;

                 /* these are set to bit fields for */
                 /* easy table initialization */
#define          F_RELOCATABLE       1
#define          F_NON_SHARED        2
#define          F_CALL_SHARED       4
#define          F_MAKE_SHARABLE     8
#define          F_STATIC    (F_NON_SHARED | F_RELOCATABLE)
#define          F_DYNAMIC   (~(F_STATIC))
#define          F_MAIN      (F_NON_SHARED | F_CALL_SHARED)
#define		 F_EXEC	     (~F_RELOCATABLE)
#define          F_ALL       (F_STATIC | F_DYNAMIC)
#define          F_CALL_SHARED_RELOC (F_RELOCATABLE | F_CALL_SHARED)

typedef enum {
	TOS_MIPS_O32,

	TOS_MIPS_R32,
	TOS_MIPS_N32,
	TOS_MIPS_64,
	TOS_IA64_64,
	TOS_IA64_32, 
#ifdef TARG_LOONGSON
	/* Add support for loongson2e , loongson2f and loongson3*/
	TOS_LOONGSON_2e,
	TOS_LOONGSON_2f,
	TOS_LOONGSON_3,
#endif
	TOS_MAX
}targos_enum;

extern string tos_string[TOS_MAX];

#ifdef _SUPPORT_IPA
#define verboseflag ld_ipa_opt[LD_IPA_VERBOSE].flag
#define keep	    ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag
#endif

extern char *outfilename;
extern string ipacom_flags;
extern string WB_flags;
extern string Y_flags;
extern int ipa_argc;
extern string *ipa_argv;
extern int ipa_use_intrinsic;

extern void ld_to_ipa_option_update(void);
extern void ipa_to_ld_option_update(void);

#ifdef __cplusplus
}
#endif

#endif /* __IPA_OPTION_H__ */
