
/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

/*
 * ==========================================================================
 *
 * Module   : consumer.c -- IA64 unwind descriptor consumer general functions
 *
 * ==========================================================================
 */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <strings.h>
#include <string.h>
/* XXX also remove temporary sigcontext_t definition from unwind_consumer.h XXX */
/*
#include <signal.h>
*/
#include <fcntl.h>
#include <unistd.h>
#include <rld_interface.h>
#include <objlist.h>
#include <obj_list.h>
#include <elf.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/unwind.h>
#include <link.h>
#include "unwind_consumer.h"

/* unwind consumer verbose environment variable */
__uint32_t _unwind_verbose = 0;

/* unwind table space */
__unw_table_entry_t *_unwind_table = NULL;
__uint64_t _unwind_table_size = 0L;

/* unwind info space */
__unw_info_t *_unwind_info = NULL;
__uint64_t _unwind_info_size = 0L;

/* unwind state stack space */
__unw_state_info_t *_unwind_state_stack = NULL;
__uint64_t _unwind_state_stack_total_size = 0L;
__uint64_t _unwind_state_stack_size = 0L;

/* unwind register mappings from state info to real registers */
const __uint32_t _unw_gr_map[__UNW_MAX_GR_PRESERVED] =
	{ 4, 5, 6, 7, 12 };
const __uint32_t _unw_fr_map[__UNW_MAX_FR_PRESERVED] =
	{  2,  3,  4,  5, 16, 17, 18, 19, 20, 21,
	  22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };
const __uint32_t _unw_br_map[__UNW_MAX_BR_PRESERVED] =
	{ 0, 1, 2, 3, 4, 5 };
const __uint32_t _unw_ar_map[__UNW_MAX_AR_PRESERVED] =
        { 0, 0, 1, 3, 4, 5, 7 };

/* initialization and finalization flags */
static __uint32_t _unwind_initialized = 0;
static __uint32_t _unwind_finalized = 0;

#pragma set woff 1174
/* rld */
static __uint64_t _rld_start_addr = 0L;
static __uint64_t _rld_end_addr = 0L;
static __uint64_t _rld_runtime_offset = 0L;
static __unw_info_t *_rld_unwind_table = NULL;
static __uint64_t _rld_unwind_table_size = 0L;

/* non-shared executable */
static __uint64_t _xeq_start_addr = 0L;
static __uint64_t _xeq_end_addr = 0L;
static __uint64_t _xeq_runtime_offset = 0L;
static __unw_info_t *_xeq_unwind_table = NULL;
static __uint64_t _xeq_unwind_table_size = 0L;
#pragma reset woff 1174

/* debugger API */
static int (*__dbg_unwind_table_addr)(__uint64_t, __uint64_t *, __uint64_t *) = NULL;
static int (*__dbg_unwind_info_addr)(__uint64_t, __uint64_t *, __uint64_t *) = NULL;
static int (*__dbg_unwind_info_target_addr)(__uint64_t, __uint64_t *) = NULL;
static int (*__dbg_text_segment_target_addr)(__uint64_t, __uint64_t *) = NULL;
static int (*__dbg_restore_gp)(__uint64_t, __uint64_t *) = NULL;
static int (*__dbg_addr_read)(__uint64_t, __uint64_t, void *) = NULL;
static char *_debugger_string = "gdb";




/* debugger initialization (to be called by the debugger) */
void unwind_debugger_init(int (*dbg_unwind_table_addr_arg)(__uint64_t,
						__uint64_t *, __uint64_t *),
			int (*dbg_unwind_info_addr_arg)(__uint64_t,
						__uint64_t *, __uint64_t *),
			int (*dbg_unwind_info_target_addr_arg)(__uint64_t,
								__uint64_t *),
			int (*dbg_text_segment_target_addr_arg)(__uint64_t,
								__uint64_t *),
			int (*dbg_restore_gp_arg)(__uint64_t,
								__uint64_t *),
			int (*dbg_addr_read_arg)(__uint64_t,
						__uint64_t, void *)) {
	__dbg_unwind_table_addr = dbg_unwind_table_addr_arg;
	__dbg_unwind_info_addr = dbg_unwind_info_addr_arg;
	__dbg_unwind_info_target_addr = dbg_unwind_info_target_addr_arg;
	__dbg_text_segment_target_addr = dbg_text_segment_target_addr_arg;
	__dbg_restore_gp = dbg_restore_gp_arg;
	__dbg_addr_read = dbg_addr_read_arg;
}





#ifndef FOR_GDB

/* static function to find the elf and program headers */
/* and to calculate the runtime displacement and */
/* the text segment runtime address for a given IP */
/* if r_debug is deficient. This should be temporary */
static __unw_error_t unwind_get_obj_phdr( const char* name,
    char *dsoname,
#if (_MIPS_SZPTR== 64)
    __uint64_t *load_address,
#else
    __uint32_t *load_address,
#endif
    Elf64_Phdr **phdr,
    Elf64_Half *phnum )
{
    pid_t pid = getpid();

    char mapname[ FILENAME_MAX ];
    char fname[ FILENAME_MAX ];
    __uint32_t inode, device;
#if (_MIPS_SZPTR== 64)
    __uint64_t l_addr;
#else
    __uint32_t l_addr;
#endif

    FILE* f;
    char line[5000];
    struct stat statf;

    if( *name == 0 ) /* exec case */
    {
        char exe[ FILENAME_MAX ];
        int len;
        sprintf(exe, "/proc/%d/exe", pid );
        len = readlink( exe, fname, FILENAME_MAX );
        fname[len] = 0;
        strcpy( dsoname, fname ); 
    }
    else
    {
        int len;
        char *slash, dirname[ FILENAME_MAX ];

        strcpy( dirname, name );
        len = readlink( name, fname, FILENAME_MAX );
        fname[ len ] = 0;
        slash = strrchr( dirname, '/' );
        if( slash ) /* Should always happen */
        {
            *slash = 0;
            sprintf( dsoname, "%s/%s", dirname, fname );
        }
        else
            strcpy( dsoname, fname );
    }
    stat( dsoname, &statf );
    inode = statf.st_ino;
    device = statf.st_dev;

    sprintf(mapname, "/proc/%d/maps", pid );
    f = fopen(mapname, "r" );
    if( !f )
        return __UNW_NULL_ERROR;
    l_addr = 0;
    while( !l_addr && fgets( line, sizeof(line), f ) && !feof( f ))
    {
        __uint64_t addr, dum1, dum2;
        __uint32_t min, maj, in;
        char perm[5];
        if( sscanf( line, "%llx-%llx %s %llx %x:%x %d %s",
               &addr, &dum1, &perm, &dum2, &maj, &min, &in, fname ) )
        {
            if( in && perm[2] == 'x' )
            {
                if( inode == in && device == ((maj<<8)|min) )
                {
                    l_addr = addr;
                }
            }
        }
    }
    fclose( f );

    if( l_addr )
    {
        int i;
        Elf64_Ehdr* ehdr;
        ehdr = (Elf64_Ehdr*) l_addr;
        *phdr = (Elf64_Phdr*) ((char*) ehdr + ehdr->e_phoff);
        *phnum = ehdr->e_phnum;
        *load_address = l_addr;
	return __UNW_OK;
    }
    else
    {
        *phdr = NULL;
        *phnum = 0;
        *load_address = 0;
        return __UNW_NULL_ERROR;
    }
}


#endif

/* static function to find the elf and program headers */
/* and to calculate the runtime displacement and */
/* the text segment runtime address for a given IP */
static __unw_error_t unwind_get_obj_data(__uint64_t ip,
						Elf64_Ehdr **ehdr,
						Elf64_Phdr **phdr,
						__uint64_t *runtime_offset,
						__uint64_t *text_segment_addr) {
	__uint64_t i;

#ifndef FOR_GDB
	struct link_map *obj = (struct link_map *) _r_debug.r_map;
	__uint32_t region_found = 0;

	if (_r_debug.r_map) {
		while (obj) {
			__uint64_t region_start = 0L, region_end = 0L;
#if (_MIPS_SZPTR == 64)
                        __uint64_t l_addr;
#else
                        __uint32_t l_addr;
#endif

                        Elf64_Half phnum;
                        char objname[ FILENAME_MAX ], *l_name;
                        if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
                                unwind_output(
                                        "unwind_get_obj_data() INTERNAL MSG: obj %s @%llx (%llx, %d) for ip = %llx", 
                                        obj->l_name, 
                                        obj->l_addr, obj->l_phdr, obj->l_phnum, ip );
                        }
                        if( !*obj->l_name /* executable case */
                             || !obj->l_phdr   /* abnormal case - should not happen */
                             || !obj->l_phnum  /* abnormal case - should not happen */
                          )
                        {
                            l_name = objname;
                            if( __UNW_OK != unwind_get_obj_phdr( obj->l_name, objname, &l_addr, 
                                                   phdr, &phnum ))
                            {
		                if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			                unwind_output("unwind_get_obj_data() ERROR: %s %s",
			                 "program header address not found for", objname );
		                }
		                return __UNW_INTERNAL_ERROR;
	                    }
                            *ehdr = (Elf64_Ehdr*) l_addr;
/*
                            obj->l_phnum = phnum;
                            obj->l_phdr  = *phdr;
                            obj->l_addr  = l_addr;
*/
                        }
                        else
                        {
			    *phdr = (Elf64_Phdr *) obj->l_phdr;
                            *ehdr = (Elf64_Ehdr*) obj->l_addr;
                            phnum = obj->l_phnum;
                            l_addr = obj->l_addr;
                            l_name = obj->l_name;
                        }
			for (i = 0; i < (__uint64_t)phnum; i++) {
				if ((PT_LOAD == (*phdr)[i].p_type) &&
						((PF_R | PF_X) == (*phdr)[i].p_flags)) {
			                *runtime_offset = l_addr- (__uint64_t)(*phdr)[i].p_vaddr;
					region_start = (__uint64_t)(*phdr)[i].p_vaddr
                                             + *runtime_offset;
					region_end = region_start +
							(__uint64_t)(*phdr)[i].p_memsz;
					break;
				}
			}
                        if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
                                unwind_output("%s %s %s [ 0x%llx - %llx]",
                                        "unwind_get_obj_data() INTERNAL MSG:",
                                        "DSO ", l_name,  
                                        region_start, region_end );
                        }
			if ((ip >= region_start) && (ip < region_end)) {
				*text_segment_addr = region_start;
				region_found = 1;
				break;
			}
			obj = obj->l_next;
		}
		if (!region_found) {
			/* XXX check for rld XXX */
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_get_obj_data() ERROR: %s",
					"elf and/or program header not found");
			}
			return __UNW_INTERNAL_ERROR;
		}
	} else {
		/* XXX check for non-shared xeq XXX */
	    if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_get_obj_data() ERROR: %s",
					"program must be static" );
			}
	}
#endif // FOR_GDB
	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s %s 0x%llx for ip=0x%llx",
			"unwind_get_obj_data() INTERNAL MSG:",
			"found elf header @",
#if (_MIPS_SZLONG == 32)
			(unsigned long long)(__uint32_t)*ehdr,
#else
			(unsigned long long)*ehdr,
#endif
			(unsigned long long)ip);
		unwind_output("%s %s 0x%llx for ip=0x%llx",
			"unwind_get_obj_data() INTERNAL MSG:",
			"found program header @",
#if (_MIPS_SZLONG == 32)
			(unsigned long long)(__uint32_t)*phdr,
#else
			(unsigned long long)*phdr,
#endif
			(unsigned long long)ip);
		unwind_output("%s %s 0x%llx for ip=0x%llx",
			"unwind_get_obj_data() INTERNAL MSG:",
			"found text segment addr @",
			(unsigned long long)*text_segment_addr,
			(unsigned long long)ip);
		unwind_output("%s %s 0x%llx for ip=0x%llx",
			"unwind_get_obj_data() INTERNAL MSG:",
			"found runtime offset =",
			(unsigned long long)*runtime_offset,
			(unsigned long long)ip);
	}

	return __UNW_OK;
}



/* static function to find the unwind table, its size */
/* and the text segment address for a given IP */
static __unw_error_t unwind_get_unwind_table_data(__uint64_t ip,
					__uint64_t *unwind_table_addr,
					__uint64_t *unwind_table_size,
					__uint64_t *text_segment_addr) {
	Elf64_Ehdr *ehdr;
	Elf64_Phdr *phdr;
	__unw_error_t ret;
	__uint64_t i, runtime_offset;

	/* find elf and program headers */
	/* and calculate the runtime displacement and */
	/* the text segment runtime address */
	if (__UNW_OK != (ret = unwind_get_obj_data(ip, &ehdr, &phdr,
				&runtime_offset, text_segment_addr))) {
		return ret;
	}

	/* find .IA64.unwind section */
	for (i = 0; i < (__uint64_t)ehdr->e_phnum; i++) {
unwind_output("[%d] type = %x\n", i, phdr[i].p_type );
		if (PT_IA64_UNWIND == phdr[i].p_type) {
			*unwind_table_addr = (__uint64_t)phdr[i].p_vaddr;
			*unwind_table_addr += runtime_offset;
			*unwind_table_size = (__uint64_t)phdr[i].p_memsz;
			break;
		}
	}
	if ((__uint64_t)ehdr->e_phnum == i) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_get_unwind_table_data() ERROR: %s",
				".IA64.unwind section not found");
		}
		return __UNW_INTERNAL_ERROR;
	}

	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s %s 0x%llx %s %llu*%u for ip=0x%llx",
			"unwind_get_unwind_table_data() INTERNAL MSG:",
			"found unwind table @",
			(unsigned long long)*unwind_table_addr,
			"with size of",
			(unsigned long long)((*unwind_table_size) /
				sizeof(__unw_table_entry_t)),
			sizeof(__unw_table_entry_t),
			(unsigned long long)ip);
	}

	return __UNW_OK;
}



/* static function to find the GP for a given IP */
static __unw_error_t unwind_restore_gp(__uint64_t ip,
					__uint64_t *gp) {
	Elf64_Ehdr *ehdr;
	Elf64_Phdr *phdr;
	Elf64_Dyn *dynptr;
	__unw_error_t ret;
	__uint64_t i, dyn_addr, runtime_offset, text_segment_addr;

	/* find elf and program headers */
	/* and calculate the runtime displacement and */
	/* the text segment runtime address */
	if (__UNW_OK != (ret = unwind_get_obj_data(ip, &ehdr, &phdr,
				&runtime_offset, &text_segment_addr))) {
		return ret;
	}

	/* find .dynamic section */
	for (i = 0; i < (__uint64_t)ehdr->e_phnum; i++) {
		if (PT_DYNAMIC == phdr[i].p_type) {
			dyn_addr = (__uint64_t)phdr[i].p_vaddr;
			dyn_addr += runtime_offset;
			break;
		}
	}
	if ((__uint64_t)ehdr->e_phnum == i) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_restore_gp() ERROR: %s",
				".dynamic section not found");
		}
		return __UNW_INTERNAL_ERROR;
	}

	/* find GP value */
#if (_MIPS_SZLONG == 32)
	for (dynptr = (Elf64_Dyn *)(__uint32_t)dyn_addr; DT_NULL != dynptr->d_tag; dynptr++) {
#else
	for (dynptr = (Elf64_Dyn *)dyn_addr; DT_NULL != dynptr->d_tag; dynptr++) {
#endif
/* FIXXXX   DT_MIPS_GP_VALUE should be DT_PLTGOT */
		if (DT_MIPS_GP_VALUE == dynptr->d_tag) {
			*gp = (__uint64_t)dynptr->d_un.d_ptr;
			*gp += runtime_offset;
			break;
		}
	}
	if (DT_NULL == dynptr->d_tag) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_restore_gp() ERROR: %s",
				"gp address not found");
		}
		return __UNW_INTERNAL_ERROR;
	}

	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s %s 0x%llx for ip=0x%llx",
			"unwind_restore_gp() INTERNAL MSG:",
			"found gp @",
			(unsigned long long)*gp,
			(unsigned long long)ip);
	}

	return __UNW_OK;
}



/* static unwind table entry comparison function to be used in bsearch() */
static int unwind_table_entry_compare(const void *ptr1, const void *ptr2) {
	__unw_table_entry_t *t1 = (__unw_table_entry_t *)ptr1;
	__unw_table_entry_t *t2 = (__unw_table_entry_t *)ptr2;

unwind_output("unwind_table_entry_compare: comparing t1 %p and t2 %p\n", t1, t2 );
unwind_output("unwind_table_entry_compare: comparing t1 (%llx, %llx) and t2 (%llx, %llx )\n",
t1->_start, t1->_end,
t2->_start, t2->_end );
	if (t1->_start == t1->_end) {
		if ((t1->_start >= t2->_start) && (t1->_end < t2->_end)) {
			return 0;
		} else if (t1->_end < t2->_start) {
			return  -1;
		} else {
			return  1;
		}
	} else {
		if ((t2->_start >= t1->_start) && (t2->_end < t1->_end)) {
			return 0;
		} else if (t2->_end < t1->_start) {
			return  -1;
		} else {
			return  1;
		}
	}
}



/* consumer function for the state stack operation pop() */
__unw_error_t unwind_state_stack_pop(__uint64_t num) {

	/* check NULL argument */
	if (NULL == _unwind_state_stack) {
		return __UNW_NULL_ERROR;
	}

	/* check argument */
	if (num > _unwind_state_stack_size) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_state_stack_pop() ERROR: %s (%llu,%llu)",
				"too many elements to pop",
				(unsigned long long)num,
				(unsigned long long)_unwind_state_stack_size);
		}
		return __UNW_INV_ARG_ERROR;
	}

	/* decrement size */
	_unwind_state_stack_size -= num;

	return __UNW_OK;
}



/* consumer function for the state stack operation push() */
__unw_error_t unwind_state_stack_push(__unw_state_info_t **ptr) {
	__unw_state_info_t *prev_unwind_state_stack;
	int dzfd;

	/* check NULL argument */
	if (NULL == _unwind_state_stack) {
		return __UNW_NULL_ERROR;
	}

	/* check size */
	if (_unwind_state_stack_total_size - 1 == _unwind_state_stack_size) {
		prev_unwind_state_stack = _unwind_state_stack;

		/* reallocate unwind state stack space */
		if (NULL == _unwind_state_stack) {
			_unwind_state_stack_total_size *= 2;
			if (-1 == (dzfd = open("/dev/zero", O_RDONLY))) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_state_stack_push() ERROR: %s (errno=%d)",
						"open() failed",
						errno);
				}
				return __UNW_SYSCALL_ERROR;
			}
			if (NULL == (_unwind_state_stack =
					(__unw_state_info_t *)mmap(NULL,
					(size_t)(_unwind_state_stack_total_size *
					sizeof(__unw_state_info_t)),
					PROT_READ|PROT_WRITE,
					MAP_PRIVATE, dzfd,
					(off_t)0))) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_state_stack_push() ERROR: %s (errno=%d)",
						"mmap() failed",
						errno);
				}
				return __UNW_MMAP_ERROR;
			}
			close(dzfd);
			if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
				unwind_output("%s re-mmaped _unwind_state_stack @ 0x%llx, total_size=%llu*%u",
					"unwind_state_stack_push() INTERNAL MSG:",
#if (_MIPS_SZLONG == 32)
					(unsigned long long)(__uint32_t)_unwind_state_stack,
#else
					(unsigned long long)_unwind_state_stack,
#endif
					(unsigned long long)_unwind_state_stack_total_size,
					sizeof(__unw_state_info_t));
			}
		}

		/* copy previous unwind state stack over */
		bcopy((const void *)prev_unwind_state_stack, (void *)_unwind_state_stack,
			(size_t)((_unwind_state_stack_total_size/2) * sizeof(__unw_state_info_t)));

		/* deallocate previous unwind state stack space */
		munmap((void *)prev_unwind_state_stack,
			(size_t)((_unwind_state_stack_total_size/2) *
			sizeof(__unw_state_info_t)));
		if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
			unwind_output("%s munmapped previous _unwind_state_stack @ 0x%llx",
				"unwind_state_stack_push() INTERNAL MSG:",
#if (_MIPS_SZLONG == 32)
				(unsigned long long)(__uint32_t)prev_unwind_state_stack);
#else
				(unsigned long long)prev_unwind_state_stack);
#endif
		}
	}

	/* set return argument */
	if (_unwind_state_stack_size) {
		bcopy((const void *)&_unwind_state_stack[_unwind_state_stack_size-1],
			(void *)&_unwind_state_stack[_unwind_state_stack_size],
			(size_t)sizeof(__unw_state_info_t));
	} else {
		bzero((void *)&_unwind_state_stack[_unwind_state_stack_size],
			(size_t)sizeof(__unw_state_info_t));
	}
	*ptr = &_unwind_state_stack[_unwind_state_stack_size++];

	return __UNW_OK;
}



/* consumer function for the state stack operation push() */
__unw_error_t unwind_state_stack_top(__unw_state_info_t **ptr) {

	/* check NULL argument */
	if (NULL == _unwind_state_stack) {
		return __UNW_NULL_ERROR;
	}

	/* set return argument */
	if (_unwind_state_stack_size) {
		*ptr = &_unwind_state_stack[_unwind_state_stack_size - 1];
	} else {
		bzero((void *)&_unwind_state_stack[_unwind_state_stack_size],
			(size_t)sizeof(__unw_state_info_t));
		*ptr = &_unwind_state_stack[_unwind_state_stack_size++];
	}

	return __UNW_OK;
}



/* consumer function for the state stack operation search() */
__unw_error_t unwind_state_stack_search(__unw_state_info_t **ptr, __uint64_t label) {
	__int64_t i;

	/* check NULL argument */
	if (NULL == _unwind_state_stack) {
		return __UNW_NULL_ERROR;
	}

	/* search stack backwards */
	for (i = _unwind_state_stack_size - 1; i >= 0; i--) {
		if (_unwind_state_stack[i]._label == label) {
			*ptr = &_unwind_state_stack[i];
			_unwind_state_stack_size = i + 1;
			return __UNW_OK;
		}
	}

	if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
		unwind_output("unwind_state_stack_search() ERROR: %s",
			"element not found");
	}
	return __UNW_NOT_FOUND_ERROR;
}



/* consumer function to reset the state stack */
__unw_error_t unwind_state_stack_reset(void) {

	/* check NULL argument */
	if (NULL == _unwind_state_stack) {
		return __UNW_NULL_ERROR;
	}

	/* reset size */
	_unwind_state_stack_size = 0;

	return __UNW_OK;
}



/* consumer function to initialize the unwind process */
__unw_error_t unwind_init(void) {
	int dzfd;
	char *str;
	static int initializing = 0;

	/* check re-entrancy */
	if (initializing) {
		return __UNW_OK;
	}

	/* initializetion */
	if (!_unwind_initialized) {

		/* set initializing flag */
		initializing = 1;

		/* set verbose flag */
		if (NULL != (str = getenv(__UNW_CONSUMER_VERBOSE_ENV_VAR))) { 
			_unwind_verbose = atoi(str);
		}

		/* allocate unwind state stack space */
		if (NULL == _unwind_state_stack) {
			_unwind_state_stack_total_size = __UNW_STATE_STACK_ENTRIES_SIZE;
			if (-1 == (dzfd = open("/dev/zero", O_RDONLY))) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_init() ERROR: %s (errno=%d)",
						"open() failed",
						errno);
				}
				initializing = 0;
				return __UNW_SYSCALL_ERROR;
			}
			if (NULL == (_unwind_state_stack =
					(__unw_state_info_t *)mmap(NULL,
					(size_t)(_unwind_state_stack_total_size *
					sizeof(__unw_state_info_t)),
					PROT_READ|PROT_WRITE,
					MAP_PRIVATE, dzfd,
					(off_t)0))) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_init() ERROR: %s (errno=%d)",
						"mmap() failed",
						errno);
				}
				initializing = 0;
				return __UNW_MMAP_ERROR;
			}
			close(dzfd);
			if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
				unwind_output("%s mmaped _unwind_state_stack @ 0x%llx, total_size=%llu*%u",
					"unwind_init() INTERNAL MSG:",
#if (_MIPS_SZLONG == 32)
					(unsigned long long)(__uint32_t)_unwind_state_stack,
#else
					(unsigned long long)_unwind_state_stack,
#endif
					(unsigned long long)_unwind_state_stack_total_size,
					sizeof(__unw_state_info_t));
			}
		}

		/* set initialized flag */
		_unwind_initialized = 1;

		/* clear initializing flag */
		initializing = 0;
        }

	return __UNW_OK;
}



/* consumer function to finalize the unwind process */
__unw_error_t unwind_fini(void) {
	static int finalizing = 0;

	/* check re-entrancy */
	if (finalizing) {
		return __UNW_OK;
	}

	/* finalize */
	if (!_unwind_finalized) {

		/* set finalizing flag */
		finalizing = 1;

		/* deallocate unwind state stack space */
		if (_unwind_state_stack) {
			munmap((void *)_unwind_state_stack,
				(size_t)(_unwind_state_stack_total_size *
				sizeof(__unw_state_info_t)));
			if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
				unwind_output("%s munmapped _unwind_state_stack @ 0x%llx",
					"unwind_fini() INTERNAL MSG:",
#if (_MIPS_SZLONG == 32)
					(unsigned long long)(__uint32_t)_unwind_state_stack);
#else
					(unsigned long long)_unwind_state_stack);
#endif
			}
		}

		/* set finalized flag */
		_unwind_finalized = 1;

		/* clear finalizing flag */
		finalizing = 0;
	}

	return __UNW_OK;
}

/* consumer function to unwind past one activation record */
__unw_error_t unwind_frame(unw_sigcontext_t *scp) {
	__unw_error_t ret = __UNW_OK;
	__unw_info_t *unwind_info;
	__unw_table_entry_t *unwind_table, *ptr, key;
	__uint64_t unwind_table_addr, unwind_table_size;
	__uint64_t gp = 0L, text_segment_addr = 0L;
#ifdef FOR_GDB
	__uint64_t st_loc = 0ull;
	__uint64_t reg_st_loc = 0ull;
	__uint64_t st_loc_fr = 0ull;
        __UNW_STRUCT_FPREG fp_reg;
#else
	__uint64_t *stack_loc = NULL, *reg_stack_loc = NULL;
	__UNW_STRUCT_FPREG *st_loc_fr = NULL;
#endif	
	__uint64_t sp = 0L, psp = 0L, cfm = 0L, bsp = 0L;
	__uint64_t slots, preds;
	__unw_state_info_t state;
	__int32_t i;
	__unw_addr_t ip = (__unw_addr_t)__UNW_CONTEXT_ACCESS_IP(scp);
        __unw_addr_t newip;
	__uint64_t addr;
	__uint64_t unwind_info_addr, unwind_info_size, unwind_info_target_addr;

	/* check if initialized */
	if (!_unwind_initialized) {
		if (__UNW_OK != (ret = unwind_init())) {
			return ret;
		}
	}

	/* verbose msg */
	if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
		unwind_output("%s unwinding for ip=0x%llx",
			"unwind_frame() MSG:",
			(unsigned long long)ip);
	}

	/* find unwind table address, its size and the text segment runtime addr */
	/* for given IP */
#ifdef FOR_GDB
	if (!(*__dbg_unwind_table_addr)((__uint64_t)ip,
					&unwind_table_addr,
					&unwind_table_size)) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s: %s",
				_debugger_string,
				"unwind table extraction failed");
		}
		return __UNW_INTERNAL_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
			unwind_output("%s %s %s 0x%llx %s %llu*%u for ip=0x%llx",
				"unwind_frame() INTERNAL MSG:",
				_debugger_string,
				"found unwind table @",
				(unsigned long long)unwind_table_addr,
				"with size of",
				(unsigned long long)(unwind_table_size /
					sizeof(__unw_table_entry_t)),
				sizeof(__unw_table_entry_t),
				(unsigned long long)ip);
		}
	}
	if (!(*__dbg_unwind_info_addr)((__uint64_t)ip,
					&unwind_info_addr,
					&unwind_info_size)) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s: %s",
				_debugger_string,
				"unwind info extraction failed");
		}
		return __UNW_INTERNAL_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
			unwind_output("%s %s %s 0x%llx %s %llu for ip=0x%llx",
				"unwind_frame() INTERNAL MSG:",
				_debugger_string,
				"found unwind info @",
				(unsigned long long)unwind_info_addr,
				"with size of",
				(unsigned long long)unwind_info_size,
				(unsigned long long)ip);
		}
	}
	if (!(*__dbg_unwind_info_target_addr)((__uint64_t)ip,
					&unwind_info_target_addr)) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s: %s",
				_debugger_string,
				"unwind info target address extraction failed");
		}
		return __UNW_INTERNAL_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
			unwind_output("%s %s %s 0x%llx for ip=0x%llx",
				"unwind_frame() INTERNAL MSG:",
				_debugger_string,
				"unwind info target address @",
				(unsigned long long)unwind_info_target_addr,
				(unsigned long long)ip);
		}
	}
	if (!(*__dbg_text_segment_target_addr)((__uint64_t)ip,
					&text_segment_addr)) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s: %s",
				_debugger_string,
				"text segment target address extraction failed");
		}
		return __UNW_INTERNAL_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
			unwind_output("%s %s %s 0x%llx for ip=0x%llx",
				"unwind_frame() INTERNAL MSG:",
				_debugger_string,
				"text segment target address @",
				(unsigned long long)text_segment_addr,
				(unsigned long long)ip);
		}
	}
#else
	if (__UNW_OK != (ret = unwind_get_unwind_table_data((__uint64_t)ip,
					&unwind_table_addr,
					&unwind_table_size,
					&text_segment_addr))) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s ip=0x%llx",
				"can not find unwind table for runtime",
				(unsigned long long)ip);
		}
		return ret;
	}
#endif

#if (_MIPS_SZLONG == 32)
	unwind_table = (__unw_table_entry_t *)(__uint32_t)unwind_table_addr;
#else
	unwind_table = (__unw_table_entry_t *)unwind_table_addr;
#endif

	/* fixup IP to be an segment-relative offset */
	ip -= (__unw_addr_t)text_segment_addr;

	/* search unwind table for given IP */
	key._start = key._end = ip;
unwind_output("unwind_table_entry_compare: Coucou");
	ptr = (__unw_table_entry_t *)bsearch((const void *)&key,
		(const void *)unwind_table,
		(size_t)(unwind_table_size/sizeof(__unw_table_entry_t)),
		(size_t)sizeof(__unw_table_entry_t),
		unwind_table_entry_compare);
unwind_output("unwind_table_entry_compare: Coucou");

	/* no entry found in the unwind table, so assume defaults */
	if (NULL == ptr) {
		__UNW_CONTEXT_ACCESS_IP(scp) =
			__UNW_CONTEXT_ACCESS_BR(scp, 0);

		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s no entry in unwind table for ip=0x%llx (segment-relative)",
				"unwind_frame() MSG:",
				(unsigned long long)ip);
		}

		return __UNW_OK;
	}

	/* entry found in the unwind table, so set pointer to unwind info */
	addr = (__uint64_t)ptr->_info + text_segment_addr;
#ifdef FOR_GDB
	addr = unwind_info_addr + (__uint64_t)ptr->_info
	         - unwind_info_target_addr;
#endif
#if (_MIPS_SZLONG == 32)
	unwind_info = (__unw_info_t *)(__uint32_t) addr;
#else
	unwind_info = (__unw_info_t *)addr;
#endif

	/* entry found in the unwind table but the unwind info has zero size, */
	/* so assume defaults */
	if (0 == __UNW_LENGTH(unwind_info->_header)) {
		__UNW_CONTEXT_ACCESS_IP(scp) =
			__UNW_CONTEXT_ACCESS_BR(scp, 0);

		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s zero-size unwind info for ip=0x%llx (segment-relative)",
				"unwind_frame() MSG:",
				(unsigned long long)ip);
		}


		return __UNW_OK;
	}

	/* calculate number of slots from IP */
	slots = ((((__uint64_t)(ip - ptr->_start) & ~0xf) >> 4) * 3) +
					((__uint64_t)(ip - ptr->_start) & 0xf);

	/* process descriptors */
	if (__UNW_OK != (ret = unwind_process_desc(slots, unwind_info, &state))) {
		if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
			unwind_output("unwind_frame() ERROR: %s ip=0x%llx (slots=%llu)",
				"can not process",
				(unsigned long long)ip,
				(unsigned long long)slots);
		}
		return ret;
	}

	/* set CFM and restore BSP */
	cfm = __UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(__UNW_PFS)) & 0x0000003fffffffff;
	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s set cfm=0x%llx",
			"unwind_frame() INTERNAL MSG:",
			(unsigned long long)cfm);
	}
	bsp = __UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(__UNW_BSP));
	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s pre-set bsp=0x%llx",
			"unwind_frame() INTERNAL MSG:",
			(unsigned long long)bsp);
	}
	__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(__UNW_BSP)) =
		bsp - (__uint64_t)(((cfm & 0x0000000000003f80) >> 7) * sizeof(__uint64_t));
	if (_unwind_verbose >= __UNW_VERBOSE_INTERNAL_MSGS) {
		unwind_output("%s post-set bsp=0x%llx",
			"unwind_frame() INTERNAL MSG:",
			(unsigned long long)bsp);
	}

	/* restore SP */
	if (__UNW_RESTORE_OFF_GR == state._gr[__UNW_SP]._code) {
		sp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		if (state._gr[__UNW_SP]._reg >= 32) {
#ifdef FOR_GDB
			reg_st_loc = (__uint64_t)bsp +
				(state._gr[__UNW_SP]._reg - 32) *
					sizeof(__uint64_t);
			if (!(*__dbg_addr_read)(reg_st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) =
				addr;
#else
			reg_stack_loc = (__uint64_t *)((ulong)bsp +
				(state._gr[__UNW_SP]._reg - 32) *
					sizeof(__uint64_t));
			__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) =
				*reg_stack_loc;
#endif
		} else {
			__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) =
				__UNW_CONTEXT_ACCESS_GR(scp, state._gr[__UNW_SP]._reg);
		}
		psp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored sp=0x%llx (off gr %u)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]),
				state._gr[__UNW_SP]._reg);
		}
	} else if (__UNW_RESTORE_OFF_BR == state._gr[__UNW_SP]._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"sp restoration can not be off a br");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_SP_RELATIVE == state._gr[__UNW_SP]._code) {
		sp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
#ifdef FOR_GDB
		st_loc = (__uint64_t)sp +
			state._gr[__UNW_SP]._offset;
		if (!(*__dbg_addr_read)(st_loc,
					sizeof(__uint64_t),
					(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) =
			addr;
#else
		stack_loc = (__uint64_t *)(ulong)(sp +
			state._gr[__UNW_SP]._offset);
		__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) =
				*stack_loc;
#endif
		psp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored sp=0x%llx (sp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]),
				(unsigned long long)state._gr[__UNW_SP]._offset);
		}
	} else if (__UNW_RESTORE_PSP_RELATIVE == state._gr[__UNW_SP]._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"sp restoration can not be psp-relative");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._gr[__UNW_SP]._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s and %s",
				"sp restoration can not be psp-relative",
				"sp has no psp offset provided anyway");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_FIXED_VALUE == state._gr[__UNW_SP]._code) {
		sp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]) +=
			(state._frame_size * 16);
		psp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored sp=0x%llx (fixed value of %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]),
				(unsigned long long)state._frame_size);
		}
	} else {
		sp = psp = __UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]);
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored sp=0x%llx (unchanged)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[__UNW_SP]));
		}
	}

	/* restore standard preserved GR's */
	for (i = __UNW_GR_STD_START; i <= __UNW_GR_STD_END; i++) {
		if (__UNW_RESTORE_OFF_GR == state._gr[i]._code) {
			if (state._gr[i]._reg >= 32) {
#ifdef FOR_GDB
				reg_st_loc = (__uint64_t)bsp +
					(state._gr[i]._reg - 32) *
						sizeof(__uint64_t);
				if (!(*__dbg_addr_read)((__uint64_t)reg_st_loc,
							sizeof(__uint64_t),
							(void *)&addr)) {
					if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
						unwind_output("unwind_frame() ERROR: %s: %s",
							_debugger_string,
							"unwind read from target proc failed");
					}
					return __UNW_INTERNAL_ERROR;
				}
				__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]) =
					addr;
#else
				reg_stack_loc = (__uint64_t *)(ulong)((__uint64_t)bsp +
					(state._gr[i]._reg - 32) *
						sizeof(__uint64_t));
				__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]) =
					*reg_stack_loc;
#endif
			} else {
				__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]) =
					__UNW_CONTEXT_ACCESS_GR(scp, state._gr[i]._reg);
			}
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored gr[%u]=0x%llx (off gr %u)",
					"unwind_frame() MSG:",
					_unw_gr_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]),
					state._gr[i]._reg);
			}
		} else if (__UNW_RESTORE_OFF_BR == state._gr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: gr %d %s",
					_unw_gr_map[i],
					"restoration can not be off a br");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_SP_RELATIVE == state._gr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: gr %d %s",
					_unw_gr_map[i],
					"restoration can not be sp-relative");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_PSP_RELATIVE == state._gr[i]._code) {
#ifdef FOR_GDB
			st_loc = (__uint64_t)psp -
				state._gr[i]._offset;
			if (!(*__dbg_addr_read)((__uint64_t)st_loc,
							sizeof(__uint64_t),
							(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]) =
				addr;
#else
			stack_loc = (__uint64_t *)(ulong)((__uint64_t)psp -
				state._gr[i]._offset);
			__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]) =
				*stack_loc;
#endif
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored gr[%u]=0x%llx (psp offset %llu)",
					"unwind_frame() MSG:",
					_unw_gr_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]),
					(unsigned long long)state._gr[i]._offset);
			}
		} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._gr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: gr %d %s",
					_unw_gr_map[i],
					"has no psp offset provided");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_FIXED_VALUE == state._gr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: gr %d %s",
					_unw_gr_map[i],
					"restoration can not be a fixed value");
			}
			ret = __UNW_INV_OP_ERROR;
		} else {
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored gr[%u]=0x%llx (unchanged)",
					"unwind_frame() MSG:",
					_unw_gr_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_GR(scp, _unw_gr_map[i]));
			}
		}
	}

	/* restore preserved FR's */
	for (i = 0; i < __UNW_MAX_FR_PRESERVED; i++) {
		if (__UNW_RESTORE_OFF_GR == state._fr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: fr %d %s",
					_unw_fr_map[i],
					"restoration can not be off a gr");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_OFF_BR == state._fr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: fr %d %s",
					_unw_fr_map[i],
					"restoration can not be off a br");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_SP_RELATIVE == state._fr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: fr %d %s",
					_unw_fr_map[i],
					"restoration can not be sp-relative");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_PSP_RELATIVE == state._fr[i]._code) {
#ifdef FOR_GDB
			st_loc_fr = (__uint64_t)psp -state._fr[i]._offset;
			if (!(*__dbg_addr_read)((__uint64_t)st_loc_fr,
							sizeof(__UNW_STRUCT_FPREG),
							(void *)&fp_reg)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_FR(scp, _unw_fr_map[i]) =
				fp_reg;
#else
			st_loc_fr = (__UNW_STRUCT_FPREG*)(ulong)((__uint64_t)psp -state._fr[i]._offset);
			__UNW_CONTEXT_ACCESS_FR(scp, _unw_fr_map[i]) =
			        *st_loc_fr;
#endif
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored fr[%u]=%lf (psp offset %llu)",
					"unwind_frame() MSG:",
					_unw_fr_map[i],
					*((long double*) &__UNW_CONTEXT_ACCESS_FR(scp, _unw_fr_map[i])),
					(unsigned long long)state._fr[i]._offset);
			}
		} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._fr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: fr %d %s",
					_unw_fr_map[i],
					"has no psp offset provided");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_FIXED_VALUE == state._fr[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: fr %d %s",
					_unw_fr_map[i],
					"restoration can not be a fixed value");
			}
			ret = __UNW_INV_OP_ERROR;
		} else {
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored fr[%u]=0x%llx (unchanged)",
					"unwind_frame() MSG:",
					_unw_fr_map[i],
					*((unsigned long long*) &__UNW_CONTEXT_ACCESS_FR(scp, _unw_fr_map[i])));
			}
		}
	}

	/* restore standard preserved BR's */
	for (i = __UNW_BR_STD_START; i <= __UNW_BR_STD_END; i++) {
		if (__UNW_RESTORE_OFF_GR == state._br[i]._code) {
			if (state._br[i]._reg >= 32) {
#ifdef FOR_GDB
				reg_st_loc = (__uint64_t)bsp +
					(state._br[i]._reg - 32) *
						sizeof(__uint64_t);
				if (!(*__dbg_addr_read)((__uint64_t)reg_st_loc,
								sizeof(__uint64_t),
								(void *)&addr)) {
					if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
						unwind_output("unwind_frame() ERROR: %s: %s",
							_debugger_string,
							"unwind read from target proc failed");
					}
					return __UNW_INTERNAL_ERROR;
				}
				__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) =
					addr;
#else
				reg_stack_loc = (__uint64_t *)(ulong)((__uint64_t)bsp +
					(state._br[i]._reg - 32) *
						sizeof(__uint64_t));
				__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) =
						*reg_stack_loc;
#endif
			} else {
				__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) =
					__UNW_CONTEXT_ACCESS_GR(scp, state._br[i]._reg);
			}
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored br[%u]=0x%llx (off gr %u)",
					"unwind_frame() MSG:",
					_unw_br_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]),
					state._br[i]._reg);
			}
		} else if (__UNW_RESTORE_OFF_BR == state._br[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: br %d %s",
					_unw_br_map[i],
					"restoration can not be off a br");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_SP_RELATIVE == state._br[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: br %d %s",
					_unw_br_map[i],
					"restoration can not be sp-relative");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_PSP_RELATIVE == state._br[i]._code) {
#ifdef FOR_GDB
			st_loc = (__uint64_t)psp - state._br[i]._offset;
			if (!(*__dbg_addr_read)((__uint64_t)st_loc,
							sizeof(__uint64_t),
							(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) = addr;
#else
			stack_loc = (__uint64_t *)(ulong)((__uint64_t)psp -
				state._br[i]._offset);
			__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) = *stack_loc;
#endif
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored br[%u]=0x%llx (psp offset %llu)",
					"unwind_frame() MSG:",
					_unw_br_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]),
					(unsigned long long)state._br[i]._offset);
			}
		} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._br[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: br %d %s",
					_unw_br_map[i],
					"has no psp offset provided");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_FIXED_VALUE == state._br[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: br %d %s",
					_unw_br_map[i],
					"restoration can not be a fixed value");
			}
			ret = __UNW_INV_OP_ERROR;
		} else {
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored br[%u]=0x%llx (unchanged)",
					"unwind_frame() MSG:",
					_unw_br_map[i],
					(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]));
			}
		}
	}

	/* restore RP */
	if (__UNW_RESTORE_OFF_GR == state._br[__UNW_RP]._code) {
		if (state._br[__UNW_RP]._reg >= 32) {
#ifdef FOR_GDB
			reg_st_loc = (__uint64_t)bsp +
				(state._br[__UNW_RP]._reg - 32) *
					sizeof(__uint64_t);
			if (!(*__dbg_addr_read)(reg_st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
				addr;
#else
			reg_stack_loc = (__uint64_t *)(ulong)((__uint64_t)bsp +
				(state._br[__UNW_RP]._reg - 32) *
					sizeof(__uint64_t));
			__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
				*reg_stack_loc;
#endif
		} else {
			__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
				__UNW_CONTEXT_ACCESS_GR(scp, state._br[__UNW_RP]._reg);
		}
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored rp=0x%llx (off gr %u)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]),
				state._br[__UNW_RP]._reg);
		}
	} else if (__UNW_RESTORE_OFF_BR == state._br[__UNW_RP]._code) {
		__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
			__UNW_CONTEXT_ACCESS_BR(scp, state._br[__UNW_RP]._reg);
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored rp=0x%llx (off br %u)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]),
				state._br[__UNW_RP]._reg);
		}
	} else if (__UNW_RESTORE_SP_RELATIVE == state._br[__UNW_RP]._code) {
#ifdef FOR_GDB
	        st_loc = sp + state._br[__UNW_RP]._offset;
		if (!(*__dbg_addr_read)(st_loc,
					sizeof(__uint64_t),
					(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
			addr;
#else
		stack_loc = (__uint64_t *)(ulong)((__uint64_t)sp +
			state._br[__UNW_RP]._offset);
		__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
			*stack_loc;
#endif
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored rp=0x%llx (sp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]),
				(unsigned long long)state._br[__UNW_RP]._offset);
		}
	} else if (__UNW_RESTORE_PSP_RELATIVE == state._br[__UNW_RP]._code) {
#ifdef FOR_GDB
		st_loc = (__uint64_t)psp -
			state._br[__UNW_RP]._offset;
		if (!(*__dbg_addr_read)((__uint64_t)st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]) =
			addr;
#else
		stack_loc = (__uint64_t *)(ulong)((__uint64_t)psp -
			state._br[__UNW_RP]._offset);
		__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[i]) =
			*stack_loc;
#endif
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored rp=0x%llx (psp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]),
				(unsigned long long)state._br[__UNW_RP]._offset);
		}
	} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._br[__UNW_RP]._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"rp has no no psp offset provided");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_FIXED_VALUE == state._br[__UNW_RP]._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"rp restoration can not be a fixed value");
		}
		ret = __UNW_INV_OP_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored rp=0x%llx (unchanged)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]));
		}
	}

	/* restore IP */
	__UNW_CONTEXT_ACCESS_IP(scp) = __UNW_CONTEXT_ACCESS_BR(scp, _unw_br_map[__UNW_RP]);
	newip = __UNW_CONTEXT_ACCESS_IP(scp);
	if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
		unwind_output("%s restored ip=0x%llx (from rp)",
			"unwind_frame() MSG:",
			(unsigned long long)__UNW_CONTEXT_ACCESS_IP(scp));
	}

	/* restore GP */
#ifdef FOR_GDB
	if (!(*__dbg_restore_gp)(newip, &gp)) {
#else
	if (__UNW_OK != (ret = unwind_restore_gp(newip, &gp))) {
#endif
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"can not restore gp");
		}
	} else {
		__UNW_CONTEXT_ACCESS_GP(scp) = gp;
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored gp=0x%llx",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_GP(scp));
		}
	}

	/* restore preserved AR's */
	for (i = 0; i < __UNW_MAX_AR_PRESERVED; i++) {
		if (__UNW_RESTORE_OFF_GR == state._ar[i]._code) {
			if (state._ar[i]._reg >= 32) {
#ifdef FOR_GDB
				__uint64_t reg_stack_loc1 = bsp;
				__uint64_t reg = state._ar[i]._reg;
				__uint64_t off = reg - 32ull;
				__uint64_t sz = sizeof(__uint64_t);
				reg_stack_loc1 += off * sz;
				if (!(*__dbg_addr_read)(reg_stack_loc1,
							  sizeof(__uint64_t),
							  (void *)&addr)) {
				  	if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
						unwind_output("unwind_frame() ERROR: %s: %s",
							_debugger_string,
							"unwind read from target proc failed");
					}
					return __UNW_INTERNAL_ERROR;
				}
				__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) = addr;
#else
				reg_stack_loc = (__uint64_t *)(ulong)((__uint64_t)bsp +
					(state._ar[i]._reg - 32) *
						sizeof(__uint64_t));
				__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
						*reg_stack_loc;
#endif
			} else {
				__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
					__UNW_CONTEXT_ACCESS_GR(scp, state._ar[i]._reg);
			}
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored ar[%u]=0x%llx (off gr %u)",
					"unwind_frame() MSG:",
					__UNW_AR_MAP(i),
					(unsigned long long)__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)),
					state._ar[i]._reg);
			}
		} else if (__UNW_RESTORE_OFF_BR == state._ar[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: ar %d %s",
					__UNW_AR_MAP(i),
					"restoration can not be off a br");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_SP_RELATIVE == state._ar[i]._code) {
#ifdef FOR_GDB
		        st_loc = sp + state._ar[i]._offset;
			if (!(*__dbg_addr_read)(st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
				addr;
#else
			stack_loc = (__uint64_t *)(ulong)((__uint64_t)sp +
				state._ar[i]._offset);
			__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
				*stack_loc;
#endif
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored ar[%u]=0x%llx (sp offset %llu)",
					"unwind_frame() MSG:",
					__UNW_AR_MAP(i),
					(unsigned long long)__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)),
					(unsigned long long)state._ar[i]._offset);
			}
		} else if (__UNW_RESTORE_PSP_RELATIVE == state._ar[i]._code) {
#ifdef FOR_GDB
		        __uint64_t st_loc = psp - state._ar[i]._offset;
			if (!(*__dbg_addr_read)((__uint64_t)st_loc,
							sizeof(__uint64_t),
							(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
				addr;
#else
			stack_loc = (__uint64_t *)(ulong)((__uint64_t)psp -
				state._ar[i]._offset);
			__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)) =
					*stack_loc;
#endif

			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored ar[%u]=0x%llx (psp offset %llu)",
					"unwind_frame() MSG:",
					__UNW_AR_MAP(i),
					(unsigned long long)__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)),
					(unsigned long long)state._ar[i]._offset);
			}
		} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._ar[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: ar %d %s",
					__UNW_AR_MAP(i),
					"has no psp offset provided");
			}
			ret = __UNW_INV_OP_ERROR;
		} else if (__UNW_RESTORE_FIXED_VALUE == state._ar[i]._code) {
			if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
				unwind_output("unwind_frame() WARNING: ar %d %s",
					__UNW_AR_MAP(i),
					"restoration can not be a fixed value");
			}
			ret = __UNW_INV_OP_ERROR;
		} else {
			if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
				unwind_output("%s restored ar[%u]=0x%llx (unchanged)",
					"unwind_frame() MSG:",
					__UNW_AR_MAP(i),
					(unsigned long long)__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(i)));
			}
		}
	}

	/* restore special AR's (ar.ec) */
	__UNW_CONTEXT_ACCESS_AR_EC(scp) = ((__uint64_t)(__UNW_CONTEXT_ACCESS_AR(scp, __UNW_AR_MAP(__UNW_PFS)) && 0x03f0000000000000ull) >> 52);

	/* restore PREDS */
	if (__UNW_RESTORE_OFF_GR == state._preds._code) {
		if (state._preds._reg >= 32) {
#ifdef FOR_GDB
			reg_st_loc = (__uint64_t)bsp +
				(state._preds._reg - 32) *
					sizeof(__uint64_t);
			if (!(*__dbg_addr_read)(reg_st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			preds = addr;
#else
			reg_stack_loc = (__uint64_t *)(ulong)((__uint64_t)bsp +
				(state._preds._reg - 32) *
					sizeof(__uint64_t));
			preds = *reg_stack_loc;
#endif
		} else {
			preds = __UNW_CONTEXT_ACCESS_GR(scp, state._preds._reg);
		}
		__UNW_CONTEXT_ACCESS_PREDS(scp) = preds;
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored preds=0x%016llx (off gr %d)",
				"unwind_frame() MSG:",
				(unsigned long long)preds,
				state._preds._reg);
		}
	} else if (__UNW_RESTORE_OFF_BR == state._preds._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"preds restoration can not be off a BR");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_SP_RELATIVE == state._preds._code) {
#ifdef FOR_GDB
		st_loc = (__uint64_t)sp + state._preds._offset;
		if (!(*__dbg_addr_read)((__uint64_t)st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		preds = addr;
#else
		stack_loc = (__uint64_t *)(ulong)((__uint64_t)sp +
			state._preds._offset);
		preds = *stack_loc;
#endif
		__UNW_CONTEXT_ACCESS_PREDS(scp) = preds;
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored preds=0x%llx (sp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)preds,
				(unsigned long long)state._preds._offset);
		}
	} else if (__UNW_RESTORE_PSP_RELATIVE == state._preds._code) {
#ifdef FOR_GDB
		st_loc = (__uint64_t)psp - state._preds._offset;
		if (!(*__dbg_addr_read)((__uint64_t)st_loc,
						sizeof(__uint64_t),
						(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		preds = addr;
#else
		stack_loc = (__uint64_t *)(ulong)((__uint64_t)psp -
			state._preds._offset);
		preds = *stack_loc;
#endif
		__UNW_CONTEXT_ACCESS_PREDS(scp) = preds;
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored preds=0x%llx (psp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)preds,
				(unsigned long long)state._preds._offset);
		}
	} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._preds._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"preds has no psp offset provided");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_FIXED_VALUE == state._preds._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"preds restoration can not be a fixed value");
		}
		ret = __UNW_INV_OP_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored preds=0x%llx (unchanged)",
				"unwind_frame() MSG:",
				(unsigned long long)preds);
		}
	}

/* ifdef FOR_GDB VERY QUESTIONABLE */
/* priunat is not in the sigcontext !! */


	/* restore PRIUNAT */
	if (__UNW_RESTORE_OFF_GR == state._priunat._code) {
		if (state._priunat._reg >= 32) {
#ifdef FOR_GDB
			reg_st_loc = (__uint64_t)bsp +
				(state._priunat._reg - 32) *
					sizeof(__uint64_t);
			if (!(*__dbg_addr_read)((__uint64_t)reg_st_loc,
							sizeof(__uint64_t),
							(void *)&addr)) {
				if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
					unwind_output("unwind_frame() ERROR: %s: %s",
						_debugger_string,
						"unwind read from target proc failed");
				}
				return __UNW_INTERNAL_ERROR;
			}
			__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = addr;
#else
			reg_stack_loc = (__uint64_t *)((__uint64_t)bsp +
				(state._priunat._reg - 32) *
					sizeof(__uint64_t));
			__UNW_CONTEXT_ACCESS_PRIUNAT(scp) =
				*reg_stack_loc;
#endif
		} else {
			__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = 
				__UNW_CONTEXT_ACCESS_GR(scp, state._priunat._reg);
		}
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored priunat=0x%llx (off gr %d)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_PRIUNAT(scp),
				state._priunat._reg);
		}
	} else if (__UNW_RESTORE_OFF_BR == state._priunat._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"priunat restoration can not be off a BR");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_SP_RELATIVE == state._priunat._code) {
#ifdef FOR_GDB
		st_loc = (__uint64_t)sp + state._priunat._offset;
		if (!(*__dbg_addr_read)(st_loc, 
					sizeof(__uint64_t),
					(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = addr;
#else
		stack_loc = (__uint64_t *)((__uint64_t)sp +
			state._priunat._offset);
		__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = *stack_loc;
#endif
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored priunat=0x%llx (sp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_PRIUNAT(scp),
				(unsigned long long)state._priunat._offset);
		}
	} else if (__UNW_RESTORE_PSP_RELATIVE == state._priunat._code) {
#ifdef FOR_GDB
		st_loc = (__uint64_t)psp -
			state._priunat._offset;
		if (!(*__dbg_addr_read)(st_loc,
					sizeof(__uint64_t),
					(void *)&addr)) {
			if (_unwind_verbose >= __UNW_VERBOSE_ERRORS) {
				unwind_output("unwind_frame() ERROR: %s: %s",
					_debugger_string,
					"unwind read from target proc failed");
			}
			return __UNW_INTERNAL_ERROR;
		}
		__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = addr;
#else
		stack_loc = (__uint64_t *)((__uint64_t)psp -
			state._priunat._offset);
		__UNW_CONTEXT_ACCESS_PRIUNAT(scp) = *stack_loc;
#endif
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored priunat=0x%llx (psp offset %llu)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_PRIUNAT(scp),
				(unsigned long long)state._priunat._offset);
		}
	} else if (__UNW_TO_RESTORE_PSP_RELATIVE == state._priunat._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"priunat has no psp offset provided");
		}
		ret = __UNW_INV_OP_ERROR;
	} else if (__UNW_RESTORE_FIXED_VALUE == state._priunat._code) {
		if (_unwind_verbose >= __UNW_VERBOSE_WARNINGS) {
			unwind_output("unwind_frame() WARNING: %s",
				"priunat restoration can not be a fixed value");
		}
		ret = __UNW_INV_OP_ERROR;
	} else {
		if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
			unwind_output("%s restored priunat=0x%llx (unchanged)",
				"unwind_frame() MSG:",
				(unsigned long long)__UNW_CONTEXT_ACCESS_PRIUNAT(scp));
		}
	}
/* endif */
    if (_unwind_verbose >= __UNW_VERBOSE_MSGS) {
        unwind_output("unwind_frame done, returning %d\n", ret );
    }

    return ret;
}


/*
 *	Given a context initialized with at least the IP cache
 * the associated unwind_table entry and unwind_info entry.
 */
static void
_Unwind_Find_Entry(struct _Unwind_Context* scp)
{
    __unw_error_t ret = __UNW_OK;
    __unw_addr_t ip = (__unw_addr_t)__UNW_CONTEXT_ACCESS_IP(scp);
    __uint64_t unwind_table_addr, unwind_table_size;
    __uint64_t gp = 0L, text_segment_addr = 0L;
    __unw_table_entry_t *unwind_table, *ptr, key;
    __unw_info_t *unwind_info;
    
    if (__UNW_OK != (ret = unwind_get_unwind_table_data((__uint64_t)ip,
					&unwind_table_addr,
					&unwind_table_size,
					&text_segment_addr)))
      {
        if (_unwind_verbose >= __UNW_VERBOSE_ERRORS)
	  {
	    unwind_output("unwind_frame() ERROR: %s ip=0x%llx",
			  "can not find unwind table for runtime",
			  (unsigned long long)ip);
	  }
	return;
      }
    unwind_table = (__unw_table_entry_t *)unwind_table_addr;

    /* fixup IP to be an segment-relative offset */
    ip -= (__unw_addr_t)text_segment_addr;

    /* search unwind table for given IP */
    key._start = key._end = ip;
unwind_output("unwind_table_entry_compare: Coucou");
    ptr = (__unw_table_entry_t *)bsearch((const void *)&key,
	(const void *)unwind_table,
	(size_t)(unwind_table_size/sizeof(__unw_table_entry_t)),
	(size_t)sizeof(__unw_table_entry_t),
	unwind_table_entry_compare);
unwind_output("unwind_table_entry_compare: Coucou");

    if (ptr == NULL) {
        scp->unwind_table_addr = 0ull;
        scp->unwind_info_addr = 0ull;
    }
/* HERE */
}

/***********************************************************************
 *
 *	This is the standard API as defined in the Base API part of
 * "C++ ABI for IA-64: Exception Handling"
 *
 ************************************************************************/

static const char* _Unwind_Reason_Strings[_URC_INSTALL_CONTEXT+1] =
{
    "_URC_NO_REASON",
    "_URC_FOREIGN_EXCEPTION_CAUGHT",
    "_URC_FATAL_PHASE2_ERROR",
    "_URC_FATAL_PHASE1_ERROR",
    "_URC_NORMAL_STOP",
    "_URC_END_OF_STACK",
    "_URC_HANDLER_FOUND",
    "_URC_INSTALL_CONTEXT"
};


/*
 *	The _Unwind_RaiseException routine is called by the runtime
 * system that is raising the exception (i.e., the point of the throw).
 * The exception_object must have the exception_class and the
 * exception_cleanup fields set.  _Unwind_RaiseException never returns
 * unless there is an error:
 *
 *   _URC_END_OF_STACK: implies that no handler has been found and,
 *	for C++, uncaught_exception() should be called.
 *
 *   _URC_FATAL_PHASE1_ERROR: implies that the unwind process itself
 *      failed, probably due to stack corruption, and terminate()
 *      should be called.
 */

_Unwind_Reason_Code
_Unwind_RaiseException(
    struct _Unwind_Exception *exception_object)
{
    struct _Unwind_Context *context;
    /* Get Registers */

    {
      return _URC_END_OF_STACK;
    }


    /* Phase 2 unwinding get URC_FATAL_PHASE2_ERROR */
    {
        _Unwind_Exception_Cleanup_Fn destructor
		= exception_object->exception_cleanup;

	(*destructor)(_URC_FATAL_PHASE2_ERROR, exception_object);
	abort();
    }

}


/*
 *	The Unwind_ForceUnwind routineis called when doing a longjump
 * or the second phase of the exception handling process.
 */
 
_Unwind_Reason_Code
_Unwind_ForceUnwind(
    struct _Unwind_Exception *exception_object,
    _Unwind_Stop_Fn stop,
    void* stop_parameter)
{
    _Unwind_Reason_Code reason;
    _Unwind_Exception_Cleanup_Fn destructor
		= exception_object->exception_cleanup;

    struct _Unwind_Context context;

    /* setup context */

    while ((reason = (*stop)(VERSION_NUMBER,
			     _UA_FORCE_UNWIND | _UA_CLEANUP_PHASE,
			     exception_object->exception_class,
			     exception_object, &context, stop_parameter))
	   == _URC_NO_REASON)
      {
	call_personality();
	__unwind_frame(context);
      }

#ifdef DEBUG
    fprintf(stderr, "_Unwind_ForceUnwind; stop function failed because ");
    if (reason < 0 || reason > _URC_INSTALL_CONTEXT) {
      fprintf(stderr, " reason=%d (0x%x) is garbage\n", reason, reason);
    } else {
      fprintf(stderr, " %s\n", _Unwind_Reason_Strings[reason]);
    }
#endif

    (*destructor)(_URC_FATAL_PHASE2_ERROR, exception_object);
    abort();
}

void
_Unwind_Resume(
    struct _Unwind_Exception *exception_object)
{
}


/*
 *	I believe that _Unwind_DeleteException is only called
 * when a runtime resumes normal execution, in which case I don't
 * believe a special reason flag is useful.
 */

void
_Unwind_DeleteException(
    struct _Unwind_Exception *exception_object)
{
    _Unwind_Exception_Cleanup_Fn destructor
        = exception_object->exception_cleanup;

    (*destructor)(_URC_FOREIGN_EXCEPTION_CAUGHT, exception_object);
}

uint64_t
_Unwind_GetGR(
    struct _Unwind_Context *context,
    int index)
{
  assert((0 <= index) && (index < 128));
  return __UNW_CONTEXT_ACCESS_GR(context, index);
}

void
_Unwind_SetGR(
    struct _Unwind_Context *context,
    int index,
    uint64_t new_value)
{
  assert((0 <= index) && (index < 128));
  __UNW_CONTEXT_ACCESS_GR(context, index) = new_value;
  /* FIX? This appears not to be causing the */
  /* desired behavior under Medusa.  GMB     */
  if (index < 63)
    {
      __UNW_CONTEXT_ACCESS_AR(context, _unw_ar_map[__UNW_RNAT])
	&= ~(0x1 << index);
    }
  else
    {
      __UNW_CONTEXT_ACCESS_AR(context, _unw_ar_map[__UNW_UNAT])
	&= ~(0x1 << index);
    }
}

uint64_t
_Unwind_GetIP(
    struct _Unwind_Context *context)
{
  return __UNW_CONTEXT_ACCESS_IP(context);
}

void
_Unwind_SetIP(
    struct _Unwind_Context *context,
    uint64_t new_value)
{
  __UNW_CONTEXT_ACCESS_IP(context) = new_value;
}

uint64_t
_Unwind_GetLanguageSpecificData(
    struct _Unwind_Context *context)
{
  uint64_t* addr = (uint64_t *) context->unwind_info_addr;
  
  return ((uint64_t) addr) + __UNW_LENGTH(*addr);
}


uint64_t
_Unwind_GetRegionStart(
    struct _Unwind_Context *context)
{
  __unw_table_entry_t* te = (__unw_table_entry_t *) context->unwind_table_addr;
  return te->_start;
}
