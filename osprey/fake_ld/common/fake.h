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


#ifndef __FAKE_H__
#define __FAKE_H__

typedef struct an_object_file an_object_file, *an_object_file_ptr;

struct an_object_file {
    
    an_object_file_ptr next;
    string	name;		    /* Name of the file */
    char 	*map_addr;	    /* Address where this file is mapped */
    char	ftype;		    /* object file type: FT_... */
    int 	offset;		    /* File offset of this object. This is */
    struct {
        int	idx_to_ipa_file_hdr;/* index into IPA's IP_FILE_HDR structure */
        char	gp_status; 	    /* does this object contains a unique
				       partition group for all its symbols? */
	char	flags;		    /* tells us something more about the partitioning characteristics */
        int16	partition_grp; 	    /* All symbols defined in this object belongs
				       to this partition group */
    } ipa_info;
};

struct extern_sym {
    an_object_file_ptr pobj;
};

typedef struct extern_sym EXTSYM;


/*
 * Special Irix st_other
 */
#define STO_DEFAULT		0x0
#define STO_INTERNAL		0x1
#define STO_HIDDEN		0x2
#define STO_PROTECTED		0x3
#define STO_OPTIONAL		0x4
#define STO_SC_ALIGN_UNUSED	0xff	/* No longer used 		*/

#define ELF_COMMENT	".comment"

typedef enum {
    ER_ILLEGAL = -1,
    ER_NOERROR = 0,  
    ER_FATAL, 
    ER_WARNING, 
    ER_INFO, 
    ER_DEBUG, 
    ER_VERBOSE, 
    ER_ERROR, 
    ER_MSG, 
    ER_DEFAULT, 
    ER_OFF
}msgtype_t;

/*
 *  After the first release, no error type should ever be removed or
 *  rearranged, otherwise the -woff value used by customers makefiles will
 *  be wrong.  For obsolete error messages, leave dummy entries there.
 */
typedef enum {
    ERN_IGNORED,
    ERN_BAD_OPTION,
    ERN_INTERNAL_FATAL,
    ERN_USAGE,
    ERN_ILL_FLAG,
    ERN_NOT_IMP,
    ERN_MISSARG_FATAL,
    ERN_CONFLICT_RFLAG,
    ERN_BAD_HEX,
    ERN_IO_FATAL,
    ERN_NO_OUTFILE,
    ERN_OBJ_FATAL,
    ERN_OBJ_CLASS,
    ERN_MALLOC,
    ERN_SYM_TRACE,
    ERN_MULT_DEF,
    ERN_MISMATCH_GP,
    ERN_MISMATCH_USGP,
    ERN_MISMATCH_VAL,
    ERN_UNDEF_EXIST,
    ERN_0SIZE_EXT,
    ERN_HALF_OVFL,
    ERN_OFS_OVFL,
    ERN_BAD_RTYPE,
    ERN_MISS_RLO,
    ERN_JMP_OVFL,
    ERN_JMP_OVFLS,
    ERN_BAD_LIT,
    ERN_GP_OVFL,
    ERN_GP_OVFLS,
    ERN_PRED_CONFLICT,
    ERN_LIBLIST_UNMATCH,
    ERN_LIBLIST_NOTFOUND,
    ERN_UNDEFINED_SYMBOL_ERROR,
    ERN_CNFLCT_DEF_SYM,
    ERN_ARCHIVE_OBJ,
    ERN_LOAD_OBJ,
    ERN_LOAD_SO,
    ERN_UNLOADED_SEC,
    ERN_ROUNDED,
    ERN_OVERLAP,
    ERN_SAMEPAGE,
    ERN_SHARABLE_FLGS,
    ERN_MIX_OBJ,
    ERN_NS_PIC,
    ERN_S_NPIC,
    ERN_NO_OBJ,
    ERN_BAD_R4K,
    ERN_NO_REGISTRY,
    ERN_NO_UPDATE,
    ERN_BAD_REGISTRY,
    ERN_REGISTRY_FAIL,
    ERN_BAD_NAME,
    ERN_SYM_NOMATCH_SEC,
    ERN_BAD_LOCAL,
    ERN_NOT_SO,
    ERN_WARN_RANGE,
    ERN_BAD_WARN,
    ERN_BAD_MEMBER,
    ERN_MULT_TXT_SEG,
    ERN_MESSAGE,
    ERN_MESSAGE2,
    ERN_BAD_LCL_RELOC,
    ERN_BAD_SYMIDX,
    ERN_BAD_RELOC_SYM,
    ERN_R4K_JUMP,
    ERN_NO_BACKUP_FATAL,
    ERN_SEARCH_LIB,
    ERN_BAD_IFACE_REC,
    ERN_CONFLICT_IFACE,
    ERN_MISMATCH_PCNT,
    ERN_MISMATCH_PTYPE,
    ERN_MISMATCH_PSIZE,
    ERN_PARM_FLOAT,
    ERN_MISSING_EXT,
    ERN_LOCAL_UNDEF,
    ERN_UNREF_SYM,
    ERN_WARN_PLACEMENT,
    ERN_NOFILE,
    ERN_NOSTAT,
    ERN_LSPEC_SYNTAX,
    ERN_LSPEC_DEFAULT_UNPLACED,
    ERN_ELS_REL,
    ERN_NOLOAD_ATTRIBUTE,
    ERN_UNUSED_LIB,
    ERN_CONFLICT_SYM,
    ERN_ISCN_NOT_LOADED,
    ERN_DELETION_ILLEGAL,
    ERN_INVALID_RELFOR,
    ERN_MISMATCHED_QUOTE,
    ERN_INVALID_SYM,
    ERN_INVALID_SYMSPEC,
    ERN_SYMSPEC_BADSHNDX,
    ERN_CANT_RESTORE_REG,
    ERN_GP_TOO_FAR,
    ERN_AVD_SEC_MISSING,
    ERN_AVD_SYM_MISSING,
    ERN_GOT_OVERFLOW,
    ERN_INCOMPATIBLE_MERGE_SZ,
    ERN_GOT_2FAR,
    ERN_GP_DSO,
    ERN_NONEST,
    ERN_COM_DSO,
    ERN_NOT_PROTECTED,
    ERN_NO_GOTPAGE,
    ERN_COM_ALIGN,
    ERN_LSPEC_REGEXP,
    ERN_CONFLICT_IFACE_FPMASK,
    ERN_OMIT_HEADERS,
    ERN_MUL_SEG,
    ERN_VARARG_WITH_FP,
    ERN_NOTIMP,
    ERN_OLD_OBJ,
    ERN_NEW_OBJ,
    ERN_WRONG_ALIGN,
    ERN_XLOCAL_MISSING,
    ERN_COMPILE,
    ERN_EXEC,
    ERN_NO_MEMBER,
    ERN_XFLAG_REPLACED,
    ERN_NO_IPA,
    ERN_BAD_DYN_ORDER,
    ERN_BAD_SCNIDX,
    ERN_IFD_OVERFLOW,
    ERN_QI_NOLIB,
    ERN_CHNG_CLASS,
    ERN_ARCHIVE_EMPTY,
    ERN_TWO_SO,
    ERN_LS_DUP_OSCN,
    ERN_NO_EXPORT,
    ERN_T5_NOP,
    ERN_MULTW_DEF,
    ERN_NO_CORDING,
    ERN_BAD_R5K, 
    ERN_CONFLICT_WEAK_SYM, 
    ERN_LIBLIST_OUTNAME, 
    ERN_SPLIT_SUCCEDED,
    ERN_UNALIGNED_MEMBER,
    ERN_BAD_SEG_PLACEMENT,

/* additions */

    ERN_INTERNAL_ERROR,
    ERN_INTERNAL_WARNING,
    ERN_ILL_FLAG_FATAL,
    ERN_MISSARG_WARNING,
    ERN_IO_ERROR,
    ERN_OBJ_ERROR,
    ERN_OBJ_WARNING,
    ERN_OBJ_CLASS_WHIRL,
    ERN_MISMATCH_GP_ERROR,
    ERN_RTYPE_ERROR,
    ERN_GP_OVFL_FATAL,
    ERN_BAD_ENTRY_NAME,
    ERN_NO_ELSPEC_FILE,
    ERN_INFO_MESSAGE,
    ERN_INFO_MESSAGE2,
    ERN_FATAL_MESSAGE2,
    ERN_OPTION_SYNTAX,
    ERN_NO_BACKUP_WARNING,
    ERN_UNDEFINED_SYMBOL_WARNING,
    ERN_OBJ_CLASS_WARNING,

    ERN_SYM_UNRESOLVED_WEAK,
    ERN_SYM_UNRESOLVED_OPTIONAL,
    ERN_SYM_INHERIT_OPTIONAL,
    ERN_DELAY_LOAD_1,
    ERN_DELAY_LOAD_2,
    ERN_TEXT_DATA_MISMATCH, 
    ERN_INVALID_ALIGN, 

    ERN_MISMATCH_RTYPE,
    ERN_MISMATCH_RSIZE,
    ERN_FEEDBACK_MESG_1,
    ERN_FEEDBACK_MESG_2,
    ERN_FEEDBACK_MESG_3,
    ERN_MULTIGOT_INVOKED,
    ERN_FEEDBACK_MESG_4,
    ERN_LSPEC_NOGPREL,
    ERN_CONFLICT_WEAK_SYM_2, 
    ERN_CONFLICT_WEAK_SYM_3, 
    ERN_CONFLICT_WEAK_SYM_4, 
    ERN_CONFLICT_WEAK_SYM_5, 
    ERN_MISMATCH_DATA_FUNC, 

/* message count */
    MAX_ERN_MESSAGE

} error_number;

/* Architecture-specific definitions */
#ifdef _64BIT_OBJECTS

#if defined(__GNUC__)
typedef u_int64_t ADDR;	    /* 64-bit virtual address */
typedef int64_t OFFSET;	    /* 64-bit file offset */
typedef int64_t FILE_SZ;    /* 64-bit file size/length */
#else
typedef uint64 ADDR;	    /* 64-bit virtual address */
typedef int64 OFFSET;	    /* 64-bit file offset */
typedef int64 FILE_SZ;	    /* 64-bit file size/length */
#endif

#define ELF_ADDR	Elf64_Addr
#define ELF_HALF	Elf64_Half
#define ELF_OFF		Elf64_Off
#define ELF_SWORD	Elf64_Sword
#define ELF_SXWORD	Elf64_Sxword
#define ELF_WORD	Elf64_Word
#define ELF_XWORD	Elf64_Xword
#define ELF_BYTE	Elf64_Byte
#define ELF_SECTION	Elf64_Section
#define ELF_FLAGS	Elf64_Xword
#define ELF_SIZE	Elf64_Xword
#define ELF_INT		Elf64_Sxword /* the natural host unit size */

#define ELFCLASS	ELFCLASS64

/* printf formatting code */
#define _fmt_w        "w64"
#define _fmt_v        "lld"
#define _fmt_a        "llx"
#define _fmt_s        "llx"
#define _fmt_w_a      "016llx"
#define _fmt_w_s      "016llx"

#else

typedef uint32 ADDR;		    /* 32-bit virtual address */
typedef int32 OFFSET;		    /* 32-bit file offset */
typedef int32 FILE_SZ;		    /* 32-bit file size/length */

#define ELF_ADDR	Elf32_Addr
#define ELF_HALF	Elf32_Half
#define ELF_OFF		Elf32_Off
#define ELF_SWORD	Elf32_Sword
#define ELF_WORD	Elf32_Word
#define ELF_SIZE        Elf32_Word  /* 32-bit ELF size/length */
#define ELF_INT		Elf32_Sword  /* the natural host unit size */

typedef longlong_t	ELF_SXWORD; /* These three should never be used in */
typedef unsigned char	ELF_BYTE;   /* 32-bit mode? */
typedef unsigned short	ELF_SECTION;	
#define ELF_FLAGS	Elf32_Word

#define ELFCLASS	ELFCLASS32

/* printf formatting code */

#define _fmt_w        "w32"
#define _fmt_v        "d"
#define _fmt_a        "x"
#define _fmt_s        "x"
#define _fmt_w_a      "08x"
#define _fmt_w_s      "08x"

#endif /* _64BIT_OBJECTS */

/* system and library calls */

#define MALLOC_ASSERT(addr) \
    if (addr == 0) {perror("malloc failed");exit(1);}

#define FOPEN(filename, type) \
    fopen((const char *)(filename), (const char *)(type))

#define FDOPEN(fid, type) \
    fdopen((int)(fid), (const char *)(type))

#define FCLOSE(stream) \
    fclose ((FILE *)(stream))


#define FTRUNCATE(fildes, length) \
    ftruncate ((int)(fildes), (off_t)(length)

#define OPEN(path, oflag, mode) \
    open((char *)(path), (int)(oflag), (int)(mode))

#define LSEEK(fid, offset, whence) \
    lseek((int)(fid), (off_t)(offset), (int)(whence))

#define READ(fid, buf, nbyte) \
    read((int)(fid), (void *)(buf), (unsigned)(nbyte))

#define WRITE(fid, buf, nbyte) \
    write((int)(fid), (const void *)(buf), (unsigned)(nbyte))

#define CLOSE(fid) \
    close((int)(fid))

#define FCHMOD(fid, mode) \
    fchmod((int)(fid), (mode_t)(mode))

#define UNLINK(path) \
    unlink((const char *)(path))

#define MKDIR(path, mode) \
    mkdir((const char *)(path), (mode_t)(mode))

#define RMDIR(path) \
    rmdir((const char *)(path))	       

#define STAT(path, buf) \
    stat((const char *)(path), (struct stat *) (buf))

#define FSTAT(fid, buf) \
    fstat((int)(fid), (buf))

#define FSTATVFS(fid, buf) \
    fstatvfs((int)(fid), (buf))

#define MMAP(addr, len, prot, flags, fd, off) \
    mmap((void *)(addr), (int)(len), (int)(prot), (int)(flags), (int)(fd), \
	 (off_t)(off))

#define PERROR(s) \
    perror((char *) s)

#define MUNMAP(addr, len) \
    munmap((void *)(addr), (int)(len))

#define MALLOC(nbytes) \
    malloc((size_t)(nbytes))

#define FREE(ptr) \
    free((void *) (ptr))

#define REALLOC(ptr, size) \
    realloc((void *)(ptr), (size_t)(size))

#define CALLOC(nelem, elsize) \
    calloc((size_t)(nelem), (size_t)(elsize))

#define ALLOCA(size) \
    alloca((unsigned int)(size))	       
	       
#define MEMCCPY(s1, s2, c, n) \
    memccpy((void *)(s1), (void *)(s2), (int)(c), (size_t)(n))

#define MEMCHR(s, c, n) \
    memchr((void *)(s), (int)(c), (size_t)(n))

#define MEMCPY(s1, s2, n) \
    memcpy((void *)(s1), (void *)(s2), (size_t)(n))

#define MEMSET(s, c, n) \
    memset((void *)(s), (int)(c), (size_t)(n))

#define MEMCMP(s1, s2, n) \
    memcmp((void *)(s1), (void *)(s2), (size_t)(n))

#ifndef BZERO
#define BZERO(b, len) \
    bzero((void *)(b), (int)(len))
#endif

#ifndef BCOPY
#define BCOPY(src, dst, len) \
    bcopy((const void *)(src), (void *)(dst), (int)(len))    	       
#endif

#define PCREATEVE(path, argv, envp) \
    pcreateve((const char *)(path), (char *const *)(argv), \
	      (char *const *)(envp))

#define WAIT(stat) \
    wait((int *)(stat))	       
	       

#ifdef __cplusplus
extern "C" {
#endif

extern void msg(int, int, ...);

extern char* concat_names(const char*, const char*);

extern EXTSYM *
slookup_mext(char *);

extern void read_one_section (int, an_object_file_ptr);

extern void 
merge_ext(an_elf_sym_record *, char *, int, an_object_file *);

#ifdef __cplusplus
} /* Close extern "C" */
#endif

#endif /* __PROCESS_H__ */
