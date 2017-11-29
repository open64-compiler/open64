/*

  Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.

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

  Contact information:  PathScale, Inc., 2071 Stierlin Court, Suite 200,
  Mountain View CA 94043, USA, or:

  http://www.pathscale.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

 */

#ifndef __IPA_LD_H__
#define __IPA_LD_H__

#define BCOPY(src, dst, len) \
    bcopy((const void *)(src), (void *)(dst), (int)(len))    	       

#define FREE(ptr) \
    free((void *) (ptr))

#define MALLOC_ASSERT(addr) \
    if (addr == 0) { perror("malloc failed: "); exit(1);}

#define MALLOC(nbytes) \
    malloc((size_t)(nbytes))

#define REALLOC(ptr, size) \
    realloc((void *)(ptr), (size_t)(size))

#define UNLINK(path) \
    unlink((const char *)(path))

#define MKDIR(path, mode) \
    mkdir((const char *)(path), (mode_t)(mode))

#define RMDIR(path) \
    rmdir((const char *)(path))	       

#define OPEN(path, oflag, mode) \
    open((char *)(path), (int)(oflag), (int)(mode))

#define CLOSE(fid) \
    close((int)(fid))

#define READ(fildes, buf, nbyte) \
    read((int) (fildes), (void *)(buf), (size_t) (nbyte))

#define ALLOCA(size) \
    alloca((unsigned int)(size))	       
	       
#define FCHMOD(fid, mode) \
    fchmod((int)(fid), (mode_t)(mode))

#define MMAP(addr, len, prot, flags, fd, off) \
    mmap((void *)(addr), (int)(len), (int)(prot), (int)(flags), (int)(fd), \
	 (off_t)(off))

#define MUNMAP(addr, len) \
    munmap((void *)(addr), (int)(len))

#define MEMCPY(s1, s2, n) \
    memcpy((void *)(s1), (void *)(s2), (size_t)(n))

#define ELF_WORD int

#define OBJ_ASSERT(EX, obj, str) \
    if (!(EX)) {fprintf(stderr,"%s: %s\n", obj->filename, str); exit(1);}

#define DEFAULT_TMP_LIST_SIZE 32
#define DEFAULT_TMPDIR "./ldtmp"
#define DEFAULT_COMPILATION_FLAGS { "cc", "-c", 0}
#define DEFAULT_COMPILATION_ARGC 2  /* number of arguments in */
#define WT_COMP_FLAGS   0x3         /* compilation flags for this object */

#define arch_eltdata(bfd) ((struct areltdata *)((bfd)->arelt_data))
#define arch_hdr(_bfd) ((struct ar_hdr *)arch_eltdata(_bfd)->arch_header)

#define FALSE 0
#define TRUE 1

typedef void *pointer;
typedef char *string;

/* These are taken from ipc_sumtab_merge.h in the ipa tree. */

enum AUX_ST_FLAG
{
    // attributes from an Elf symbols

    USED_IN_OBJ		= 0x00000001,	// referenced in an Elf object
    USED_IN_DSO		= 0x00000002,	// referenced in a DSO
    DEF_IN_OBJ		= 0x00000004,	// defined in an Elf object
    DEF_IN_DSO		= 0x00000008,	// defined in a DSO
    OBJ_COMMON		= 0x00000010,	// defined in OBJ/DSO as common
    ADDR_TAKEN_IN_OBJ	= 0x00000020,	// address taken by Elf object or dso
	// Update Print_AUX_ST_flags when adding new attribute

    OBJ_ATTR_MASK	= 0x0000003f,	// mask for the above bits

    // attributes from an ST
    
    COMMON_USED_IN_IO   = 0x00000040,	// common block passed to IO routines
    IGNORE_REFCOUNTS	= 0x00000080	// ignore the mod/ref counts even
					// when they hit zero
};



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
	TOS_IA64_64,
	TOS_IA64_32, 
	TOS_MAX
}targos_enum;

extern string tos_string[TOS_MAX];
extern string WB_flags;
extern string Y_flags;
extern string toolroot;
extern string tmpdir;
extern string outfilename;
extern string WB_flags;
extern string Y_flags;
extern char * __Release_ID;

extern void *(*p_ipa_open_input)(char *, off_t *);
extern void (*p_ipa_init_link_line)(int, char **);
extern void (*p_ipa_add_link_flag)(const char*);
extern void (*p_ipa_modify_link_flag)(char*, char*);
extern void (*p_ipa_driver)(int, char **);
#ifdef OSP_OPT
extern void (*p_process_whirl64)(void *, off_t, void *, int, const char *, off_t, bfd_boolean);
extern void (*p_process_whirl32)(void *, off_t, void *, int, const char *, off_t, bfd_boolean);
#else
extern void (*p_process_whirl64)(void *, off_t, void *, int, const char *, off_t);
extern void (*p_process_whirl32)(void *, off_t, void *, int, const char *, off_t);
#endif
extern void (*p_ipa_insert_whirl_marker)(void);
#ifdef KEY
extern void (*p_ipa_erase_link_flag)(const char*);
extern void (*p_Ipalink_Set_Error_Phase)(char *);
extern void (*p_Ipalink_ErrMsg_EC_Outfile)(char *);
#endif

/* Function declarations
 */
 
extern char *
always_demangle(char *, char );


extern void read_one_section(int , void *);

extern void 
merge_ext(void *, char *, int , void *) ;

extern void 
msg (int , int , ...);

extern char *
ipa_copy_of(char *);

extern string
concat_names(const string, const string);

extern int
do_compile (string *);

extern void
add_to_tmp_file_list (string);

extern void
cleanup_all_files (void);

extern int
create_tmpdir ( int);

extern string
create_unique_file (const string, char);

extern string *
get_command_line(bfd *, string , string , int *);

extern int
make_link (const string dest, const string src);

extern string
ld_compile (bfd *abfd);

extern void *
ld_slookup_mext(char *, bfd_boolean);

extern void
ld_set_st_idx (void *, int);

extern int
ld_get_st_idx (void *);

extern int
ipa_set_ndx (bfd *);

extern bfd_boolean
ld_resolved_to_obj (void *, void *);

extern char *
ld_get_section_base (void *, int );

extern unsigned long long
ld_get_section_size(void *, int );

extern char *
ld_get_section_name(void *, int );

extern void *
ld_get_mmap_addr(void *);

extern void 
ld_set_section_data(void *,int );

extern void
ld_release_section_data(void *,int );

extern void 
ld_set_cur_obj(bfd *);

void *
ld_get_cur_obj(void);

extern bfd_boolean
ipa_is_whirl(bfd *);

extern void
ipa_process_whirl ( bfd *);

extern void
ipa_process_whirl_in_archive ( bfd *, bfd *);

extern int 
Count_elf_external_gots (void);

extern void 
ipa_set_syms (void);

#ifdef OSP_OPT
extern char *
ipa_mmap_file_in_archive ( bfd *, int, off_t);
#endif

/* The following constant values are shared by both ld and ipa.  Each
   symtab entry in ld's merged symbol table has an ST_IDX field pointing
   back to the corresponding entry (if any) in the WHIRL merged symbol
   table.  When a new entry is added to ld's merged symbol table, if the
   symbol comes from an Elf object, we set the ST_IDX field to
   WHIRL_ST_IDX_NOT_AVAILABLE because there is no corresponding ST entry in
   the WHIRL merged symbol table. If the symbol comes from a WHIRL object,
   we set the ST_IDX field to WHIRL_ST_IDX_UNINITIALIZED, which means that
   there will be a corresponding entry in the WHIRL merged symtab but we
   don't know the value yet.
 */

#define WHIRL_ST_IDX_UNINITIALIZED (0)
#define WHIRL_ST_IDX_NOT_AVAILABLE (-1)



#endif /* __IPA_LD_H__ */
