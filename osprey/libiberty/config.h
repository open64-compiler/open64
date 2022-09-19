/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

/* config.h.  Generated automatically by configure.  */
/* config.in.  Generated automatically from configure.in by autoheader.  */

/* Define to empty if the keyword does not work.  */
/* #undef const */

/* Define if you have a working `mmap' system call.  */
#define HAVE_MMAP 1

#ifndef __MINGW32__
/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1
#endif /* __MINGW32__ */

/* Define if you have <vfork.h>.  */
/* #undef HAVE_VFORK_H */

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define if your C compiler doesn't accept -c and -o together.  */
/* #undef NO_MINUS_C_MINUS_O */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define vfork as fork if vfork does not work.  */
/* #undef vfork */

/* Define to `unsigned long' if <sys/types.h> doesn't define.  */
#ifndef __APPLE__
#define uintptr_t unsigned long
#endif

/* Define if you have the _doprnt function.  */
/* #undef HAVE__DOPRNT */

/* Define if you have the asprintf function.  */
#define HAVE_ASPRINTF 1

/* Define if you have the atexit function.  */
#define HAVE_ATEXIT 1

/* Define if you have the basename function.  */
#define HAVE_BASENAME 1

#ifndef __MINGW32__
/* Define if you have the bcmp function.  */
#define HAVE_BCMP 1
#endif /* __MINGW32__ */

#ifndef __MINGW32__
/* Define if you have the bcopy function.  */
#define HAVE_BCOPY 1
#endif /* __MINGW32__ */

/* Define if you have the bsearch function.  */
#define HAVE_BSEARCH 1

#ifndef __MINGW32__
/* Define if you have the bzero function.  */
#define HAVE_BZERO 1
#endif /* __MINGW32__ */

/* Define if you have the calloc function.  */
#define HAVE_CALLOC 1

/* Define if you have the clock function.  */
#define HAVE_CLOCK 1

/* Define if you have the ffs function.  */
#define HAVE_FFS 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

#ifndef __MINGW32__
/* Define if you have the getpagesize function.  */
#define HAVE_GETPAGESIZE 1
#endif /* __MINGW32__ */

#ifndef __MINGW32__
/* Define if you have the getrusage function.  */
#define HAVE_GETRUSAGE 1
#endif /* __MINGW32__ */

/* Define if you have the getsysinfo function.  */
/* #undef HAVE_GETSYSINFO */

#ifndef __MINGW32__
/* Define if you have the gettimeofday function.  */
#define HAVE_GETTIMEOFDAY 1
#endif /* __MINGW32__ */

#ifndef __MINGW32__
/* Define if you have the index function.  */
#define HAVE_INDEX 1
#endif /* __MINGW32__ */

/* Define if you have the insque function.  */
#define HAVE_INSQUE 1

/* Define if you have the memchr function.  */
#define HAVE_MEMCHR 1

/* Define if you have the memcmp function.  */
#define HAVE_MEMCMP 1

/* Define if you have the memcpy function.  */
#define HAVE_MEMCPY 1

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the memset function.  */
#define HAVE_MEMSET 1

/* Define if you have the mkstemps function.  */
/* #undef HAVE_MKSTEMPS */

/* Define if you have the on_exit function.  */
#define HAVE_ON_EXIT 1

#ifndef __MINGW32__
/* Define if you have the psignal function.  */
#define HAVE_PSIGNAL 1
#endif /* __MINGW32__ */

/* Define if you have the pstat_getdynamic function.  */
/* #undef HAVE_PSTAT_GETDYNAMIC */

/* Define if you have the pstat_getstatic function.  */
/* #undef HAVE_PSTAT_GETSTATIC */

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the random function.  */
#define HAVE_RANDOM 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

#ifndef __MINGW32__
/* Define if you have the rindex function.  */
#define HAVE_RINDEX 1
#endif /* __MINGW32__ */

#ifndef __MINGW32__
/* Define if you have the sbrk function.  */
#define HAVE_SBRK 1
#endif /* __MINGW32__ */

/* Define if you have the setenv function.  */
#define HAVE_SETENV 1

/* Define if you have the sigsetmask function.  */
#define HAVE_SIGSETMASK 1

/* Define if you have the strcasecmp function.  */
#define HAVE_STRCASECMP 1

/* Define if you have the strchr function.  */
#define HAVE_STRCHR 1

/* Define if you have the strdup function.  */
#define HAVE_STRDUP 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the strncasecmp function.  */
#define HAVE_STRNCASECMP 1

/* Define if you have the strrchr function.  */
#define HAVE_STRRCHR 1

/* Define if you have the strsignal function.  */
#ifndef __MINGW32__
#define HAVE_STRSIGNAL 1
#endif /* __MINGW32__ */

/* Define if you have the strstr function.  */
#define HAVE_STRSTR 1

/* Define if you have the strtod function.  */
#define HAVE_STRTOD 1

/* Define if you have the strtol function.  */
#define HAVE_STRTOL 1

/* Define if you have the strtoul function.  */
#define HAVE_STRTOUL 1

/* Define if you have the sysconf function.  */
#define HAVE_SYSCONF 1

/* Define if you have the sysctl function.  */
/* #define HAVE_SYSCTL */

/* Define if you have the sysmp function.  */
/* #undef HAVE_SYSMP */

/* Define if you have the table function.  */
/* #undef HAVE_TABLE */

#ifndef __MINGW32__
/* Define if you have the times function.  */
#define HAVE_TIMES 1
#endif /* __MINGW32__ */

/* Define if you have the tmpnam function.  */
#define HAVE_TMPNAM 1

#ifndef __MINGW32__
/* Define if you have the vasprintf function.  */
#define HAVE_VASPRINTF 1
#endif /* __MINGW32__ */

/* Define if you have the vfprintf function.  */
#define HAVE_VFPRINTF 1

/* Define if you have the vprintf function.  */
#define HAVE_VPRINTF 1

/* Define if you have the vsprintf function.  */
#define HAVE_VSPRINTF 1

#ifndef __MINGW32__
/* Define if you have the waitpid function.  */
#define HAVE_WAITPID 1
#endif /* __MINGW32__ */

/* Define if you have the <alloca.h> header file.  */
#define HAVE_ALLOCA_H 1

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <limits.h> header file.  */
#define HAVE_LIMITS_H 1

/* Define if you have the <machine/hal_sysinfo.h> header file.  */
/* #undef HAVE_MACHINE_HAL_SYSINFO_H */

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

#ifndef __MINGW32__
/* Define if you have the <strings.h> header file.  */
#define HAVE_STRINGS_H 1
#endif /* __MINGW32__ */

/* Define if you have the <sys/file.h> header file.  */
#define HAVE_SYS_FILE_H 1

/* Define if you have the <sys/mman.h> header file.  */
#define HAVE_SYS_MMAN_H 1

/* Define if you have the <sys/param.h> header file.  */
#define HAVE_SYS_PARAM_H 1

/* Define if you have the <sys/pstat.h> header file.  */
/* #undef HAVE_SYS_PSTAT_H */

#ifndef __MINGW32__
/* Define if you have the <sys/resource.h> header file.  */
#define HAVE_SYS_RESOURCE_H 1
#endif /* __MINGW32__ */

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/sysctl.h> header file.  */
/*
#if !defined(__CYGWIN__) && !defined(__MINGW32__)
#define HAVE_SYS_SYSCTL_H 1
#endif
*/
/* __CYGWIN__ __MINGW32__ */

/* Define if you have the <sys/sysinfo.h> header file.  */
#define HAVE_SYS_SYSINFO_H 1

/* Define if you have the <sys/sysmp.h> header file.  */
/* #undef HAVE_SYS_SYSMP_H */

/* Define if you have the <sys/systemcfg.h> header file.  */
/* #undef HAVE_SYS_SYSTEMCFG_H */

/* Define if you have the <sys/table.h> header file.  */
/* #undef HAVE_SYS_TABLE_H */

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <time.h> header file.  */
#define HAVE_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the <malloc.h> header file.  */
#define HAVE_MALLOC_H 1

/* Define if errno must be declared even when <errno.h> is included. */
/* #undef NEED_DECLARATION_ERRNO */

/* Define if you have the `uintptr_t' type. */
#define HAVE_UINTPTR_T 1

/* Define if you have the sys_errlist variable. */
#define HAVE_SYS_ERRLIST 1

/* Define if you have the sys_nerr variable. */
#define HAVE_SYS_NERR 1

#ifndef __MINGW32__
/* Define if you have the sys_siglist variable. */
#define HAVE_SYS_SIGLIST 1
#endif /* __MINGW32__ */

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP
   systems. This function is required for alloca.c support on those
   systems. */
/* #undef CRAY_STACKSEG_END */

/* Define if you know the direction of stack growth for your system;
   otherwise it will be automatically deduced at run-time.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown */
#define STACK_DIRECTION -1

/* Define if you have the _system_configuration variable. */
/* #undef HAVE__SYSTEM_CONFIGURATION */

