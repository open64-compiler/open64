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


/* $Header$ */

/*
 Everything in this header is intended solely for use by
 the implementation (by libc.so and libpthread.so, for example).

 Nothing here is intended for direct use by applications.
 Direct call of any of the interfaces by any
 user application or use library is unsupported, and interfaces
 may change without notice.

*/

#ifndef __RLD_INTERFACE_H__
#define __RLD_INTERFACE_H__

#ifdef __cplusplus
extern "C" {
#endif


/* _RLD_OP_MODIFY_LIST's op codes */
#define _RLD_OP_NONE		0 	/* nop */

#define _RLD_OP_INSERT		1	/* insert new object 'name' */
					/* before element */
					/* DO NOT USE. will be deleted. */
					/* Currently unused */

#define _RLD_OP_ADD		2	/* add new object 'name' after */
					/* element */

#define _RLD_OP_DELETE		3	/* delete element */

#define _RLD_OP_REPLACE		4	/* replace element with new */
					/* object 'name' */
					/* DO NOT USE. will be deleted. */
					/* Currently unused */

/* _rld_new_interface's op codes */
#define _SHUT_DOWN		0	/* execute all .fini sections */
#define _RLD_FIRST_PATHNAME	1	/* get to the first obj on the list */
#define _RLD_NEXT_PATHNAME	2	/* get to the next obj on the list */
#define _RLD_MODIFY_LIST	3	/* modify the current obj list */

#define _RLD_ADDR_TO_NAME	4	/* get global symbol name */
					/* from addr of symbol. Must
					   be exact symbol address, else
					   name not returned.
					   _RLD_DLADDR, below, is
					   a more usable service.
					*/

#define _RLD_NAME_TO_ADDR	5	/* get address of global symbol */
					/* from its name. 
					*/

#define _RLD_LIBDL_INTERFACE	6	/* libdl simulation      */
#define _RLD_VERSION_EXPECTED   7       /* get version string    */
					/* corresponding to name */
#define _RLD_SPROC_NOTIFY       8       /* tell rld that it needs to use */
					/* multi-threaded locking        */
#define _RLD_DSO_VERSION        9       /* get the version of a certain dso */
#define _RLD_SHUTDOWN_THREAD	10	/* when any thread calls "exit", */
					/* cleanup sync for that thread. */
#define _RLD_SPROC_FINI		11	/* when we are no longer         */
					/* multithreaded, such as after  */
					/* "fork", don't do MP locks.    */
#define _RLD_PTHREADS_START	12	/* pthreads initialization       */
				        /* beginning.                    */

#define _RLD_NOP                13      /* Does nothing.                 */
					/* Useful for preloading the GOT */
					/* so that the follow-on         */
					/* call to _rld_new_interface    */
					/* does not require              */
					/* lazy-evalation.               */

#define _RLD_DLADDR             14      /* for dladdr(), returning */
				   	/* dso name etc thru pointer arg
					   (3rd arg pointer to Dl_info).
				   	*/

/* __rld_libdl_interface op codes */
#define _LIBDL_RLD_DLOPEN	0
#define _LIBDL_RLD_DLSYM	1
#define _LIBDL_RLD_DLCLOSE	2
#define _LIBDL_RLD_DLERROR	3
#define _LIBDL_SGI_RLD_DLADD  	4
#define _LIBDL_RLD_DLOPEN_VERSION 5

	/* The next two used internally by rld: must not be used
	** by any system code or any user code.
	*/
#define _LIBDL_SGI_LIBLIST_DL_ADD  6
	/* _LIBDL_SGI_LIBLIST_DL_ADD means that rld is doing a 
	   delay-load of a DSO thru _ rld_new_interface 
	   and the DSO doing the call was
	   sgi_dladded (by a user-level sgidladd)
	*/ 
#define _LIBDL_SGI_LIBLIST_DL_OPEN 7
	/* _LIBDL_SGI_LIBLIST_DL_OPEN means that rld is doing a 
	   delay-load of a DSO thru _ rld_new_interface
	   and the DSO doing the call was
	   dlopened (by a user-level dlopen)
	*/ 

	/* The highest LIBDL interface number in use */
#define _LIBDL_RLD_MAX_CODE   	7

#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))

#include <elf.h>

#if (_MIPS_SZPTR == 64)

/* The following six are an older rld interface and
   cannot be used unless the _RLD_ARGS -old_interface
   argument is given to rld.debug  
   DO NOT USE. Will be removed.
   
*/
extern char      *_rld_first_pathname(void);
extern char      *_rld_next_pathname(void);
extern char      *_rld_modify_list(Elf64_Word	operation,
		      char *original_pathname,
		      char *name);
extern char      *_rld_address_to_name(Elf64_Addr address);
extern Elf64_Addr _rld_name_to_address(char *name);
extern int        _rld_interface(Elf64_Word operation);

/* The following is the current rld interface.
   It is intended for use by the implementation, not
   by applications.
*/
extern void      *_rld_new_interface(Elf64_Word operation, ...);

/* The following is not obsolete, but applications
   should not be using it. It is intended for use
   by the rld implementation.
*/
extern void      *__rld_libdl_interface(int what, unsigned long arg1,
		    unsigned long arg2, unsigned long arg3, 
		    unsigned long arg4);

#else /* 32 bit world */
/* The following six are an older rld interface and
   cannot be used unless the _RLD_ARGS -old_interface
   argument is given to rld.debug
   DO NOT USE. Will be removed.
*/
extern char      *_rld_first_pathname(void);
extern char      *_rld_next_pathname(void);
extern char      *_rld_modify_list(Elf32_Word	operation,
		      char *original_pathname,
		      char *name);
extern char      *_rld_address_to_name(Elf32_Addr address);
extern Elf32_Addr _rld_name_to_address(char *name);
extern int        _rld_interface(Elf32_Word operation);

/* The following is the current rld interface.
   It is intended for use by the implementation, not
   by applications.
*/
extern void      *_rld_new_interface(Elf32_Word operation, ...);

/* The following is not obsolete, but applications
   should not be using it. It is intended for use
   by the rld implementation.
*/
extern void      *__rld_libdl_interface(int what, unsigned long arg1, 
		    unsigned long arg2, unsigned long arg3, 
		    unsigned long arg4);

#endif /* _MIPS_SZPTR */

#ifndef _RLD_INTERFACE_DLFCN_H_DLADDR
#define _RLD_INTERFACE_DLFCN_H_DLADDR
struct Dl_info {
        const char * dli_fname;
        void       * dli_fbase;
        const char * dli_sname;
        void       * dli_saddr;
        int          dli_version;
        int          dli_reserved1;
        long         dli_reserved[4];
};
#endif

#endif /* _LANGUAGE_C */

#ifdef __cplusplus
}
#endif

#endif  /* __RLD_INTERFACE_H__ */
