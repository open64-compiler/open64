
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

#ifndef __SYSSGI_H__
#define __SYSSGI_H__

#ifdef __cplusplus
extern "C" {
#endif

#ident "$Revision: 1.1.1.1 $"

#include <sys/types.h>

/*
** syssgi() system call commands.
*/

#define SGI_SYSID		1	/* get the system ID */
#define SGI_BUFINFO		2	/* detailed buffer cache info */
#define	SGI_TUNE_SET		3	/* change tune values --
						see sys/tuneable.h */
#define SGI_TUNE		SGI_TUNE_SET
#define	SGI_IDBG		4	/* access to internal debugger */
#define SGI_INVENT		5	/* hardware inventory */
#define SGI_RDNAME		6	/* get name of process given pid */
#define SGI_SETLED		7	/* set cpu board led state(s) */
#define SGI_SETNVRAM		8	/* set nvram */
#define SGI_GETNVRAM		9	/* get nvram */
#define SGI_SETKOPT		10	/* call kopt_set */
#define SGI_QUERY_FTIMER        12
#define SGI_QUERY_CYCLECNTR	13

/* Other POSIX system calls */
#define SGI_SETSID		20
#define SGI_SETPGID		21
#define SGI_SYSCONF		22
#define SGI_PATHCONF		24


#define SGI_TITIMER		29	/* set [get] curthread itimer */

/* Block read & Block write system calls: to allow access to > 2 gig
 * of space on a disk or logical volume for large filesystems.
 */
#define SGI_READB	30
#define SGI_WRITEB	31

/* setgroups() and getgroups() system calls */
#define SGI_SETGROUPS	40
#define SGI_GETGROUPS	41

#define SGI_SETTIMEOFDAY	52	/* set time including microseconds */
#define SGI_SETTIMETRIM		53	/* change crystal trimmer */
#define SGI_GETTIMETRIM		54
#define SGI_SPROFIL		55	/* sprofil(2) entry point */
#define SGI_RUSAGE		56	/* Berkeley's getrusage(2) */
#define SGI_SIGSTACK		57	/* Berkeley's sigstack(2) */
#define SGI_NETPROC		59	/* start network process */
#define SGI_SIGALTSTACK		60	/* SVR4 sigaltstack(2) */
#define SGI_BDFLUSHCNT		61	/* schedule next bdflush run */
#define SGI_SSYNC		62	/* synchronous sync */
#define SGI_NFSCNVT		63	/* convert file handle to descriptor */
#define SGI_GETPGID             64      /* SVR4 getpgid */
#define SGI_GETSID              65      /* SVR4 getsid */
#define SGI_IOPROBE		66	/* I/O probing */
#define SGI_CONFIG		67	/* get configuration data */
#define SGI_ELFMAP		68	/* atomically mmap in an elf dso */
#define SGI_MCONFIG		69	/* loadable module configuration */

/*
 * Trusted IRIX system calls
 */
/* mandatory access (MAC) */
#define SGI_GETPLABEL		70	/* Get process label */
#define SGI_SETPLABEL		71	/* Set process label */
#define SGI_GETLABEL		72	/* Get file label -- SGI_MAC_GET */
#define SGI_SETLABEL		73	/* Set file label -- SGI_MAC_SET */
/* audit (SAT) */
#define SGI_SATREAD		74	/* Read audit rec from the kernel */
#define SGI_SATWRITE		75	/* Write audit rec to the kernel */
#define SGI_SATCTL		76	/* Control/query the audit stream */
/* extended attribute handling */
#define SGI_LOADATTR		77	/* Load attributes database */
#define SGI_UNLOADATTR		78	/* Unload attributes database */
/* trusted sockets */
#define SGI_RECVLUMSG		79	/* recvmsg() with label and uid */
/* more extended attribute handling */
#define SGI_PLANGMOUNT		80	/* mount() with PlanG info */
/* trusted sockets with DAC */
#define SGI_GETPSOACL		81	/* Get process socket ACL. */
#define SGI_SETPSOACL		82	/* Set process socket ACL.  */
/* still more extended attribute handling */
#define	SGI_CAP_GET		83	/* Get capability set from file */
#define	SGI_CAP_SET		84	/* Set capability set on file */
#define SGI_PROC_ATTR_GET	85	/* Get the named process attribute */
#define SGI_EAG_GETPROCATTR	SGI_PROC_ATTR_GET	/* Old Name */
#define SGI_PROC_ATTR_SET	86	/* Set the named process attributes */
#define SGI_EAG_SETPROCATTR	SGI_PROC_ATTR_SET	/* Old Name */
#define SGI_REVOKE		87	/* Revoke access to a device */
#define SGI_FREVOKE		SGI_REVOKE		/* Old Name */
#define SGI_ACL_GET		88	/* Get Access Control List(s) */
#define SGI_ACL_SET		89	/* Set Access Control List(s) */
#define SGI_MAC_GET		90	/* Get MAC Label */
#define SGI_MAC_SET		91	/* Set MAC Label */
#define SGI_RXEV_GET		92
/*
 * End of Trusted IRIX system calls
 */
#define SGI_SBE_GET_INFO        98      /* get SBE count on a mc3 board*/
#define SGI_SBE_CLR_INFO        99      /* clear SBE count on a mc3 board */
#define SGI_GET_EVCONF		102	/* Get the "evconfig" structure
					 * built by the IP19 prom. */

#define SGI_MPCWAROFF		103	/* turn off libmpc WAR handling */
#define SGI_SET_AUTOPWRON	104	/* set auto power on time */
#define SGI_SPIPE		105	/* set stream pipe */
#define SGI_SYMTAB		106	/* get runtime symtab info */
#define	SGI_SET_FP_PRECISE	107	/* set/clear precise FP exception mode*/
#define SGI_TOSSTSAVE		108	/* toss any saved pregions */
#define SGI_FDHI		109	/* return highest valid fd */
#define	SGI_SET_CONFIG_SMM	110	/* set/clear sequential memory mode */
#define	SGI_SET_FP_PRESERVE	111	/* preserve p_fpflags across exec */
#define SGI_MINRSS		112	/* minrss */
#define	SGI_GRIO		113	/* guaranteed rate I/O */
#define	SGI_XLV_SET_TAB		114	/* set incore logical volume config */
#define	SGI_XLV_GET_TAB		115	/* get incore logical volume config */
#define	SGI_GET_FP_PRECISE	116	/* get precise FP exception mode */
#define	SGI_GET_CONFIG_SMM	117	/* get sequential memory mode */
#define	SGI_FP_IMPRECISE_SUPP	118	/* does hw support imprecise mode? */
#define	SGI_CONFIG_NSMM_SUPP	119	/* does hw support non-seq mem mode? */
					/* 120 and 121 are available */

/* Frame Scheduler Timestamping Control */
						
#define SGI_RT_TSTAMP_CREATE    122 /* create timestamping buffer for specific cpu */
#define SGI_RT_TSTAMP_DELETE    123 /* delete timestamping buffer */
#define SGI_RT_TSTAMP_START     124 /* start logging timestamps */
#define SGI_RT_TSTAMP_STOP      125 /* stop logging timestamps */
#define SGI_RT_TSTAMP_ADDR      126 /* get physical addr for timestamp buffer */
#define SGI_RT_TSTAMP_MASK      127 /* set tstamp mask */
#define SGI_RT_TSTAMP_EOB_MODE  128 /* set end-of-buffer action */
					
#define	SGI_USE_FP_BCOPY	129	/* should bcopy/bzero use fp? */

#define SGI_GET_UST		130	/* get unadjusted system time value */

#define SGI_SPECULATIVE_EXEC	131	/* turn speculative execution on/off */

#define SGI_XLV_NEXT_RQST	132	/* wait for next xlv configuration
					   request. */
#define SGI_XLV_ATTR_CURSOR	133	/* get cursor for xlv attributes */
#define SGI_XLV_ATTR_GET	134	/* get xlv attribute value */
#define SGI_XLV_ATTR_SET	135	/* set xlv attribute */

/* btool - code coverage - only used when compiled with -DBTOOL */
#define SGI_BTOOLSIZE		136
#define SGI_BTOOLGET		137
#define SGI_BTOOLREINIT		138

#define	SGI_CREATE_UUID		139	/* create a DCE-defined UUID */

/* disable CSR_EXCEPT while in GL address space */
#define SGI_NOFPE		140	/* disable CSR_EXCEPT */

#define SGI_OLD_SOFTFP		141	/* use old (asm) softfp code */
#define	SGI_FS_INUMBERS		142	/* xfs get inode number table */
#define	SGI_FS_BULKSTAT		143	/* xfs get stat64 info in bulk */

/* more Frame Scheduler calls */
#define SGI_RT_TSTAMP_WAIT	144	/* wait for tstamp buffer to reach 2/3 watermark */
#define SGI_RT_TSTAMP_UPDATE    145     /* update fifo buffer head index */

/* things needed by xFS dump and xFS-based DMIG interfaces */
#define	SGI_PATH_TO_HANDLE	146	/* get a file's file handle	   */
#define	SGI_PATH_TO_FSHANDLE	147	/* get a file's file system handle */
#define	SGI_FD_TO_HANDLE	148	/* get an open file's file handle  */
#define	SGI_OPEN_BY_HANDLE	149	/* open a file given a file handle */
#define	SGI_READLINK_BY_HANDLE	150	/* read a link using a file handle */

#define SGI_READ_DANGID		151	/* Probe for Dang existance */

/* Sizing constants used by kmem readers */
#define SGI_CONST		152	/* System sizing constants */
#define SGI_XFS_FSOPERATIONS	153	/* entry of xfs extended operations */


/* Extended accounting functions */
#define SGI_SETASH		154	/* set array session handle */
#define SGI_GETASH		155	/* get array session handle */
#define SGI_SETPRID		156	/* set project ID */
#define SGI_GETPRID		157	/* get project ID */
#define SGI_SETSPINFO		158	/* set service provider info */
#define SGI_GETSPINFO		159	/* get service provider info */
#define SGI_SHAREII		160	/* ShareII product syscall */
#define SGI_NEWARRAYSESS	161	/* start new array session */
#define SGI_GETDFLTPRID		162	/* get system default project ID */
#define	SGI_SET_DISMISSED_EXC_CNT 163	/* set dismissed exception count */
#define	SGI_GET_DISMISSED_EXC_CNT 164	/* get dismissed exception count */
/* More cycle counter support */
#define SGI_CYCLECNTR_SIZE	165	/* Size user needs to use to read CC */
#define SGI_QUERY_FASTTIMER	166	/* period of fast itimers in ns */
#define SGI_PIDSINASH		167	/* List PIDs in given array session */
#define SGI_ULI			168

#define SGI_CACHEFS_SYS		171		/* CacheFS system call */
#define SGI_NFSNOTIFY		172		/* lockd client/server failure */
#define SGI_LOCKDSYS		173		/* set lockd options & client name */

/*
 * Performance monitoring calls
 */
#define SGI_EVENTCTR            174
#define SGI_GETPRUSAGE          175

#define SGI_PROCMASK_LOCATION	176

#define	SGI_CKPT_SYS		178     /* checkpoint/restart system call */
#define	SGI_GETGRPPID		179     /* return a list of pids for a given group */
#define	SGI_GETSESPID		180     /* return a list of pids for a given session */

#define SGI_ENUMASHS		181	/* return a list of all active ASHs */
#define SGI_SETASMACHID		182	/* set array machine ID */
#define SGI_GETASMACHID		183	/* get array machine ID */
#define SGI_GETARSESS		184	/* retrieve arsess info */
#define SGI_JOINARRAYSESS	185	/* join existing array session */

#define SGI_DBA_CONFIG		187	/* DBA: get/set database accelerator */
					/* features */


/* similar to uname() system call, but returns "official name" for hardware
 * specific releases from the base release.  Takes a count and pointer
 * to a buffer in which to place the name.  name will be no more than 256
 * characters.
*/
#define SGI_RELEASE_NAME	188

/* return handler to synchronize icache with dcache */
#define SGI_SYNCH_CACHE_HANDLER 189
#define SGI_SWASH_INIT		190	/* SoftWindows Address Space Helper */


/*
 * These calls are to support multiple serial numbers for systems built from
 *  SN0/SN0 modules.
*/

#define SGI_NUM_MODULES		191
#define SGI_MODULE_INFO		192

/*
 * Return information about an execution context (uproc, uthread, sthread,
 * ithread, etc.).  These are currently for internal use only.  They may be
 * replaced by a completely new context information facility.
 */
#define SGI_GET_CONTEXT_NAME	193	/* return name */
#define SGI_GET_CONTEXT_INFO	194	/* reserved */

/*
 * Partition management.
 */
#define	SGI_PART_OPERATIONS	195	/* Partition management support */

/*
 * Swap control
 */
#define SGI_EARLY_ADD_SWAP	197	/* Used to add swap early in boot */

/*
 * Numa Memory Management
 */

#define SGI_NUMA_MIGR_PAGE	200	/* migrate a page */
#define SGI_NUMA_MIGR_PAGE_ALT	201	/* migrate a page of other processes */

#define SGI_KAIO_USERINIT	202	/* DBA: kernel asyncio process initialization */
#define SGI_KAIO_READ		203	/* DBA: kernel asyncio read request */
#define SGI_KAIO_WRITE		204	/* DBA: kernel asyncio write request */
#define SGI_KAIO_SUSPEND	205	/* DBA: kernel asyncio nap till N I/Os complete */
/*#define SGI_KAIO_STATS	206*/	/* use (SGI_DBA_GETSTATS,dba_stat_t *buf,sz,-1) */
#define SGI_DBA_GETSTATS	206	/* database accelerator stats (see 232)*/

/*
 * syscall to get the auxlliary info for a device
 */
#define SGI_IO_SHOW_AUX_INFO	207

#define SGI_PMOCTL		208     /* policy control */

#define SGI_ALLOCSHARENA	209	/* allocate shared arena & pin pages */
#define SGI_SETVPID		210	/* set vpid in proc structure	     */
#define SGI_GETVPID		211	/* get vpid in proc structure	     */

/*
 * Tune NUMA migration related parameters
 */
#define SGI_NUMA_TUNE           212

#define SGI_ERROR_FORCE		214	/* Error injection for kernel testing*/

/* numa statictics */
#define SGI_NUMA_STATS_GET      218

/*
 * syscall for data pipe file system pipe end 
 */
#define SGI_DPIPE_FSPE_BIND     219     /* bind a scatter gather list with
					   a (pipe_id, transfer_id) pair. */

#define SGI_DYIELD		220	/* Yield processor to specified process */
#define SGI_TUNE_GET		221	/* Get tuneable value */
#define SGI_CHPROJ		222	/* chproj - project id of an inode */
#define SGI_LCHPROJ		223	/* lchproj */
#define SGI_FCHPROJ		224	/* fchproj */

#define SGI_ARSESS_CTL		225	/* Global array session controls */
#define SGI_ARSESS_OP		226	/* Array session operation */

/* setup fault handler to make a vaddr range fetchopable */
#define SGI_FETCHOP_SETUP       227

#define	SGI_FS_BULKSTAT_SINGLE	228	/* xfs get stat64 info for 1 inode */

#define SGI_WRITE_IP32_FLASH    230	/* write IP32 prom image */

#define SGI_ROUTERSTATS_ENABLED 231     /* access to state of 
					   gather_craylink_routerstats */

#define SGI_DBA_CLRSTATS	232	/* database accelerator stats (see 206)*/
#define SGI_IPC_AUTORMID_SHM	233	/* is IPC_AUTORMID for shm present? */

#define SGI_IS_DEBUG_KERNEL	300	/* is this a debug kernel? */
#define SGI_IS_TRAPLOG_DEBUG_KERNEL 301	/* is this a TRAPLOG debug kernel? */

#define SGI_POKE		320	/* kernel mode poke DEBUG only */
#define SGI_PEEK		321	/* kernel mode peek DEBUG only */

#define SGI_XLV_INDUCE_IO_ERROR		350  /* insert an I/O error at random 
						into XLV. This is used in XFS
						error handling testing. */
#define SGI_XLV_UNINDUCE_IO_ERROR	351
#define SGI_DKSC_INDUCE_IO_ERROR	352
#define SGI_DKSC_UNINDUCE_IO_ERROR	353

/*
 * enable/disable up to XFS_NUM_INJECT_ERROR (10) tests in the kernel
 * that will be failed the next time they're hit.  The first two take
 * one extra parameter, the tag of the error to be enabled/disabled and
 * return 1 if successful and 0 if not.
 * The third (clearall) takes no extra parameters and returns nothing.
 */
#define SGI_XFS_INJECT_ERROR	360
#define SGI_XFS_CLEAR_ERROR	361
#define SGI_XFS_CLEARALL_ERROR	362

#define SGI_XFS_MAKE_SHARED_RO	363	/* mark an XFS filesystem as */
					/* a shared read-only filesystem */
					/* on the next unmount */
#define SGI_XFS_CLEAR_SHARED_RO	364	/* will undo above operation if */
					/* called *before* the filesystem */
					/* is unmounted */

#define SGI_FO_DUMP             400
#define SGI_FO_SWITCH           401
#define	SGI_NOHANG		402	/* don't hang on NFS servers */
#define	SGI_UNFS		403	/* unwedge NFS at exit */


#define	SGI_ATTR_LIST_BY_HANDLE 404	/* attr_list syscall using file handle */
#define	SGI_ATTR_MULTI_BY_HANDLE 405	/* attr_multi syscall using a file handle */
#define SGI_FSSETDM_BY_HANDLE	406	/* set DMAPI attributes by handle */

#define SGI_PHYSP		1011	/* get phys pgno for vaddr */

/*
 * kernel threads temporary
 */
#define	SGI_KTHREAD		1012

/*
 * Special (sgi use only) diagnostic interfaces
 */
#define SGI_FLUSH_ICACHE	1015	/* (sgi use only) flush icache for diagnostics */

/*reserved			1020*/

/*
 * Allocate a large page.
 */
#define	SGI_DEBUGLPAGE		1030

/*
 * Map the address space of a process to a different page size.
 */
#define	SGI_MAPLPAGE		1031


#ifdef DEBUG
#define SGI_MUTEX_TEST		 1040
#define SGI_MUTEX_TEST_INIT	 1041
#define SGI_MUTEX_TESTER_INIT	 1042
#endif

/*
 * Miser test syssgi calls.
 */
#define	SGI_CREATE_MISER_POOL	1043
#define	SGI_CREATE_MISER_JOB	1044
#define SGI_MISER_CRITICAL	1045

/*
 * Context switch testing
 */
#define SGI_CONTEXT_SWITCH	 1046
#define SGI_MRLOCK_TEST_INIT	1047
#define SGI_MRLOCK_TEST_RLOCK	1048

/*
 * Kmem zone perf. test call and other shaked test hooks.
 */
#define SGI_KMEM_TEST		1051
#define	SGI_SHAKE_ZONES		1052	/* Shake a zone */

/*
 * CA UniCenter entry point.
 */
#define	SGI_UNICENTER		1053


/*
 * Cell entry point.
 */
#define SGI_CELL		1060

/* SGI_CELL suboptions */
enum {
	SGI_IS_OS_CELLULAR ,     /* OS cellular or not */
	SGI_CELL_PID_TO_CELLID,	/* cell a proc is running on */
	SGI_CELL_OBJ_EVICT,	/* evict an object */
	SGI_LEAVE_MEMBERSHIP,	/* Cell leaves a membership */	
	SGI_SEND_TEST_MESG,	/* Send a test message to a given cell*/
	SGI_FAIL_CELL,		/* Cell commits suicdie */	
	SGI_MEMBERSHIP_STATS ,  /* cell membership statistics */
	SGI_MESG_STATS         /* inter cell message statistice */
};

/* suboptions for SGI_MESG_STATS */
enum {
	SGI_RESET_COUNT , /* Reset message statistice */
	SGI_REPORT_MESSAGE_COUNT, /* Report message count */
	SGI_REPORT_MESSAGE_STATS, /* Report message statistics */
	SGI_REPORT_MESSAGESIZE_COUNT , /*Reports message size counts */
	SGI_REPORT_MESSAGESIZE_STATS /*Report message size statistice */
};


/* 
 * 1100 - 1110 are reserved for NUMA Memory Management tests
 */

/* testing hooks for NUMA simulation on Everest */
#define SGI_NUMA_MIGR_INT_VADDR  1100
#define SGI_NUMA_MIGR_INT_PFN	 1101
#define SGI_NUMA_PAGEMIGR_TEST   1102
#define SGI_NUMA_TESTS           1103
#define SGI_NUMA_RESERVED        1104

#define SGI_MEMPROF_START        1105
#define SGI_MEMPROF_GET          1106
#define SGI_MEMPROF_CLEARALL     1107
#define SGI_MEMPROF_STOP         1108

#define	SGI_HW_CPU_CONFREG	 1200
#define	SGI_UPANIC_SET		 1201
#define	SGI_UPANIC    		 1202

/* Flags needed for SGI_NUMA_PAGEMIGR_TEST syscall */
#define VA_PROC 0
#define VA_INTR 1
#define PA_PROC 2
#define PA_INTR 3

/*
 * kernel pathconf() must know whether called by fpathconf or 
 * pathconf--i.e how to interpret the 1st parameter.  (It is not
 * currently used at all, but may be in the future.)
 */
#define PATHCONF	1
#define FPATHCONF	2


#define	GET_SYSID	SGI_SYSID	/* compatability */
#define	MAXSYSIDSIZE	64	/* maximum size in bytes of the system id   */

/* hardware inventory options */
#define SGI_INV_SIZEOF	1	/* get sizeof inventory struct */
#define SGI_INV_READ	2	/* read inventory table */

/* configuration data options */
#define ADAP_READ 	1
#define ADD_NODE  	2
#define DELETE_NODE  	3
#define GET_NODE  	4

/* IO probe directions */
#define IOPROBE_READ	0
#define IOPROBE_WRITE	1

/* nvram options */
#define SGI_NVSTRSIZE	4096	/* Fru output buf 3K, need at least this*/

/* SGI_MINRSS options */
#define MINRSS_ADDPNAME		1
#define MINRSS_DELPNAME		2
#define MINRSS_LISTPNAME	3
#define MINRSS_ADDVNODE		4
#define MINRSS_DELVNODE		5
#define MINRSS_LISTVNODE	6
/* struct for LISTPNAME */
#define MINRSS_PNAMESZ		20

typedef long pgno_t; /* added for rehosting - sunchan */

struct getpname {
	char g_name[MINRSS_PNAMESZ];
	pgno_t g_minrss;
};
/* struct for LISTVNODE */
struct getvnode {
	dev_t g_fsid;
	ino_t g_nodeid;
	pgno_t g_minrss;
};

#ifdef _KERNEL
struct irix5_getvnode {
	__int32_t g_fsid;
	__uint32_t g_nodeid;
	__int32_t g_minrss;
};
#endif

/* EXTENDED MEMORY operations (though sgifastpath syscall)
 * Intended to be used by the HIPPI controller to provide a fastpath for
 * low latency memory operations.  Device driver will specify a range of
 * addresses which the user can read/write using the following syssgi calls.
 */
#define SGIFAST_PIOMEM_BREAD32	0	/* extended memory block read */
#define	SGIFAST_PIOMEM_BWRITE32	1	/* extended memory block write */
#define	SGIFAST_PIOMEM_NULL	2	/* extended memory NOP */

/*
 * SGI_CONST parameters
 */
#define SGICONST_MBUF		1	/* Mbuf */
#define SGICONST_PTE		2	/* Pte */
#define SGICONST_PAGESZ		3	/* _PAGESZ */
#define SGICONST_PARAM		4	/* Param */

/* SGI_ULI options */
enum {
    /* These options are used from the top half of a process, i.e. they
     * are ordinary system calls
     */

    ULI_SLEEP,
    ULI_DEBUG,
    ULI_SET_DEBUG_SIG,

    /* These options are used from the bottom half of the process, i.e.
     * they are interrupt mode calls
     */

    ULI_RETURN,
    ULI_CPUID,
    ULI_WAKEUP,
    ULI_CONWRITE,

    ULI_MAXCALL
};

/*
 * SGI_PROCMASK_LOCATION options
 */
#define USER_LEVEL	1
#define KERNEL_LEVEL	2

#if defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS)
#ifndef _KERNEL
#include <stddef.h>
extern ptrdiff_t syssgi(int, ...);
#endif /* !_KERNEL */
#endif

#if _KERNEL
#include <sys/systm.h>

struct syssgia {
	sysarg_t cmd;
	sysarg_t arg1, arg2, arg3, arg4, arg5, arg6, arg7;
};

extern int syssgi(struct syssgia *, rval_t *);

/* procscan parameters for findash */
struct findashinfo {
	ash_t	ash;
	pid_t	*useraddr;
	int	usermax;
	int	count;
	int	current;
	int	errno;
};
#endif	/* _KERNEL */

#ifdef __cplusplus
}
#endif
#endif /* __SYSSGI_H__ */
