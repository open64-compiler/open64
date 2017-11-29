/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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



/* USMID:  "\n@(#)5.0_pl/headers/p_directiv.h	5.5	10/08/99 08:26:21\n" */

struct		dir_info_entry	{boolean	on_platform;
				 Uint		unused		: 7;
				 Uint		msg_num		: 16;
				 Uint		unused1		: 6;
				 boolean	issue_795;
				 boolean	issue_531;
				 char		name[32];
				};

typedef		struct	dir_info_entry	dir_info_type;

/* Following is a table that describes what directives are allowed where */
/* and what types of semantics are needed for each table.  The CDIR_     */
/* macros are defined in target.m based on platform and are set to TRUE  */
/* or FALSE.                                                             */

/* Directive notes:                                                      */
/*    Obsolete directives that we still recognize for some reason.       */
/*    Block, Dynamic, Regfile, Semextern, Shortsequence, Static,Taskhead */
   
dir_info_type	cdir_info[Tok_Dir_End - Tok_Dir_Start] = {
   {0,			0,    0,    0,	0,	0,  "NULL entry"},
   {CDIR_ALIGN,		0,  801,    0,	1,	0,  "ALIGN"},
   {TRUE,		0,    0,    0,	0,	0,  "ATOMICUPDATE"},
   {0,			0,  790,    0,	0,	0,  "AUTOSCOPE"},
   {CDIR_AUXILIARY,	0,  801,    0,	1,	1,  "AUXILIARY"},
   {TRUE,		0,    0,    0,	0,	0,  "BARRIER"},
   {CDIR_BL,		0,  801,    0,	1,	0,  "BL"},
   {0,			0,  801,    0,	0,	0,  "BLOCK"},
   {CDIR_BLOCKABLE,	0,  801,    0,	0,	0,  "BLOCKABLE"},
   {CDIR_CACHE_BLOCK,	0,  801,    0,	0,	0,  "BLOCKINGSIZE"},
   {TRUE,		0,    0,    0,	1,	0,  "BOUNDS"},
   {CDIR_CACHE_ALIGN,	0,  801,    0,	1,	0,  "CACHE_ALIGN"},
   {CDIR_CACHE_BYPASS,	0,  801,    0,	1,	0,  "CACHE_BYPASS"},
   {CDIR_CACHE_NOALLOCATE,0,801,    0,	1,	0,  "CACHE_NOALLOCATE"},
   {0,			0,  790,    0,	0,	0,  "CHUNKSIZE"},
   {TRUE,		0,    0,    0,	0,	0,  "CNCALL"},
   {0,			0,  790,    0,	0,	0,  "CODE"},
   {CDIR_TASKCOMMON,	0,  801,    0,	1,	1,  "COMMON"},
   {CDIR_CONCURRENT,	0,  801,    0,	1,	0,  "CONCURRENT"},
   {0,			0,  790,    0,	0,	0,  "CONTROL"},
   {CDIR_COPY_ASSUMED_SHAPE, 0,801, 0,	1,	0,  "COPY_ASSUMED_SHAPE"},
   {TRUE,		0,    0,    0,	0,	0,  "CRITICAL"},
   {TRUE,		0,    0,    0,	0,	0,  "DOSHARED"},
   {0,			0,  801,    0,	0,	0,  "DYNAMIC"},
   {TRUE,		0,  801,    0,	0,	0,  "EJECT"},
   {TRUE,		0,    0,    0,	0,	0,  "END CRITICAL"},
   {TRUE,		0,    0,    0,	0,	0,  "END MASTER"},
   {TRUE,		0,    0,    0,	0,	0,  "FIXED"},
   {CDIR_FLOW,		0,  801,    0,	1,	0,  "FLOW"},
   {TRUE,		0,    0,    0,	0,	0,  "FREE"},
   {TRUE,		0,    0,    0,	0,	0,  "GEOMETRY"},
   {0,			0,  790,    0,	0,	0,  "GETFIRST"},
   {0,			0,  790,    0,	0,	0,  "GUIDED"},
   {TRUE,		1,    0,    0,	0,	0,  "ID"},
   {0,			0,  790,    0,	0,	0,  "IF	"},
   {TRUE,		0,    0,    0,	1,	1,  "IGNORE_TKR"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "INLINE"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "INLINE ALWAYS"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "INLINE NEVER"},
   {0,			0,  790,    0,	0,	0,  "INTEGER"},
   {CDIR_INTERCHANGE,	0,  801,    0,	1,	0,  "INTERCHANGE"},
   {CDIR_IVDEP,		0,  801,    0,	1,	0,  "IVDEP"},
   {TRUE,	 	0,  801,    0,	1,	0,  "LIST"},
   {CDIR_PATTERN,	0,  801,    0,	1,	0,  "MARK"},
   {TRUE,		0,    0,    0,	0,	0,  "MASTER"},
   {TRUE,		0,    0,    0,	0,	0,  "MAXCPUS"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "MODINLINE"},
   {TRUE,		0,    0,    0,	1,	1,  "NAME"},
   {0,			0,  790,    0,	0,	0,  "NCPUS_CHUNKS"},
   {CDIR_VECTOR,	0,  801,    0,	1,	0,  "NEXTSCALAR"},
   {TRUE,		0,    0,    0,	0,	0,  "NO BARRIER"},
   {CDIR_BL,		0,  801,    0,	1,	0,  "NOBL"},
   {CDIR_CACHE_BLOCK,	0,  801,    0,	0,	0,  "NOBLOCKING"},
   {TRUE,		0,    0,    0,	1,	0,  "NOBOUNDS"},
   {0,	 		0,  790,    0,	0,	0,  "NOCINV"},
   {0,			0,  790,    0,	1,	0,  "NOCODE"},
   {CDIR_FLOW,		0,  801,    0,	1,	0,  "NOFLOW"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "NOINLINE"},
   {TRUE,		0,    0,    0,	1,	0,  "NOINTERCHANGE"},
   {CDIR_INTERCHANGE,	0,  801,    0,	0,	0,  "NOLIST"},
   {CDIR_PATTERN,	0,  801,    0,	1,	0,  "NOMARK"},
   {CDIR_INLINE,	0,  801,    0,	1,	0,  "NOMODINLINE"},
   {CDIR_PATTERN,	0,  801,    0,	1,	0,  "NOPATTERN"},
   {CDIR_RECURRENCE,	0,  801,    0,	1,	0,  "NORECURRENCE"},
   {CDIR_NOSIDEEFFECTS,	0,  801,    0,	1,	1,  "NOSIDEEFFECTS"},
   {CDIR_SPLIT,		0,  801,    0,	1,	0,  "NOSPLIT"},
   {CDIR_STREAM,	0,  801,    0,	1,	0,  "NOSTREAM"},
   {CDIR_TASK,		0,  801,    0,	1,	0,  "NOTASK"},
   {CDIR_UNROLL,	0,  801,    0,	1,	0,  "NOUNROLL"},
   {CDIR_VECTOR,	0,  801,    0,	1,	0,  "NOVECTOR"},
   {CDIR_VSEARCH,	0,  801,    0,	1,	0,  "NOVSEARCH"},
   {0,			0,  790,    0,	0,	0,  "NUMCHUNKS"},
   {TRUE,		0,    0,    0,	0,	0,  "NUMCPUS"},
   {0,			0,  801,    0,	0,	0,  "PARALLEL_ONLY"},
   {CDIR_PATTERN,	0,  801,    0,	1,	0,  "PATTERN"},
   {TRUE,		0,    0,    0,	0,	0,  "PE_PRIVATE"},
   {TRUE,		0,    0,    0,	0,	0,  "PE_RESIDENT"},
   {TRUE,		0,    0,    0,	0,	0,  "PERMUTATION"},
   {CDIR_STREAM,	0,  801,    0,	1,	0,  "PREFERSTREAM"},
   {CDIR_PREFERTASK,	0,  801,    0,	1,	0,  "PREFERTASK"},
   {CDIR_PREFERVECTOR,	0,  801,    0,	1,	0,  "PREFERVECTOR"},
   {0,			0,  790,    0,	0,	0,  "PRIVATE"},
   {CDIR_RECURRENCE,	0,  801,    0,	1,	0,  "RECURRENCE"},
   {0,			0,  801,    0,	0,	0,  "REGFILE"},
   {0,			0,  790,    0,	0,	0,  "SAVELAST"},
   {0,			0,  801,    0,	0,	0,  "SEMEXTERN"},
   {TRUE,		0,    0,    0,	0,	0,  "SERIAL_ONLY"},
   {TRUE,		0,    0,    0,	0,	0,  "SHARED"},
   {CDIR_SHORTLOOP,	0,  801,    0,	1,	0,  "SHORTLOOP"},
   {0,			0,  801,    0,	0,	0,  "SHORTSEQUENCE"},
   {0,			0,  790,    0,	0,	0,  "SINGLE"},
   {CDIR_SPLIT,		0,  801,    0,	1,	0,  "SPLIT"},
   {CDIR_STACK,		0,  801,    0,	1,	0,  "STACK"},
   {0,			0,  801,    0,	0,	0,  "STATIC"},
   {CDIR_STREAM,	0,  801,    0,	1,	0,  "STREAM"},
   {CDIR_SUPPRESS,	0,  801,    0,	1,	0,  "SUPPRESS"},
   {CDIR_SYMMETRIC,	0,  801,    0,	1,	1,  "SYMMETRIC"},
   {TRUE,		0,    0,    0,	1,	1,  "SYSTEM_MODULE"},
   {CDIR_TASK,		0,  801,    0,	1,	0,  "TASK"},
   {CDIR_TASKCOMMON,	0,  801,    0,	1,	1,  "TASKCOMMON"},
   {0,			0,  801,    0,	0,	0,  "TASKHEAD"},
   {TRUE,		0,    0,    0,	0,	0,  "UNKNOWN"},
   {TRUE,		0,    0,    0,	0,	0,  "UNKNOWN_SHARED"},
   {CDIR_UNROLL,	0,  801,    0,	1,	0,  "UNROLL"},
   {CDIR_USES_EREGS,	0,  801,    0,	1,	0,  "USES EREGS"},
   {CDIR_VECTOR,	0,  801,    0,	1,	0,  "VECTOR"},
   {CDIR_VFUNCTION,	0,  801,    0,	1,	1,  "VFUNCTION"},
   {CDIR_VSEARCH,	0,  801,    0,	1,	0,  "VSEARCH"},
};

char	*(mp_dir_str[Num_Mp_Values]) = {
				"DOACROSS",
				"PDO",
				"PARALLEL DO",
				"PARALLEL",
				"PSECTION",
				"SINGLEPROCESS"
					};

char    *(open_mp_dir_str[Num_Omp_Values]) = {
                                "PARALLEL",
                                "DO",
                                "SECTIONS",
                                "SINGLE",
                                "WORKSHARE", /* by jhs, 02/7/18 */
                                "PARALLEL DO",
                                "PARALLEL SECTIONS",
                                "PARALLEL WORKSHARE" /* by jhs, 02/7/18 */
                                        };


boolean	clause_allowed[Num_Mp_Values][Last_Clause] = {
					{
/* Doacross */
	/* If_Clause		*/	TRUE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	TRUE,
	/* Lastlocal_Clause	*/	TRUE,
	/* Reduction_Clause	*/	TRUE,
	/* Mp_Schedtype_Clause	*/	TRUE,
	/* Chunk_Clause		*/	TRUE,
	/* Blocked_Clause	*/	TRUE,
	/* Affinity_Clause	*/	TRUE,
	/* Mode_Clause		*/	FALSE,
	/* Ordered_Clause	*/	FALSE,
	/* Onto_Clause		*/	TRUE,
	/* Nest_Clause		*/	TRUE,
	/* Lastthread_Clause	*/	TRUE,
					},
					{
/* Pdo */
	/* If_Clause		*/	FALSE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	FALSE,
	/* Lastlocal_Clause	*/	TRUE,
	/* Reduction_Clause	*/	TRUE,
	/* Mp_Schedtype_Clause	*/	TRUE,
	/* Chunk_Clause		*/	TRUE,
	/* Blocked_Clause	*/	TRUE,
	/* Affinity_Clause	*/	TRUE,
	/* Mode_Clause		*/	TRUE,
	/* Ordered_Clause	*/	TRUE,
	/* Onto_Clause		*/	TRUE,
	/* Nest_Clause		*/	TRUE,
	/* Lastthread_Clause	*/	TRUE,
					},
					{
/* Parallel_Do */
	/* If_Clause		*/	TRUE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	TRUE,
	/* Lastlocal_Clause	*/	TRUE,
	/* Reduction_Clause	*/	TRUE,
	/* Mp_Schedtype_Clause	*/	TRUE,
	/* Chunk_Clause		*/	TRUE,
	/* Blocked_Clause	*/	TRUE,
	/* Affinity_Clause	*/	TRUE,
	/* Mode_Clause		*/	TRUE,
	/* Ordered_Clause	*/	FALSE,
	/* Onto_Clause		*/	TRUE,
	/* Nest_Clause		*/	TRUE,
	/* Lastthread_Clause	*/	TRUE,
					},
					{
/* Parallel */
	/* If_Clause		*/	TRUE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	TRUE,
	/* Lastlocal_Clause	*/	FALSE,
	/* Reduction_Clause	*/	FALSE,
	/* Mp_Schedtype_Clause	*/	FALSE,
	/* Chunk_Clause		*/	FALSE,
	/* Blocked_Clause	*/	FALSE,
	/* Affinity_Clause	*/	FALSE,
	/* Mode_Clause		*/	FALSE,
	/* Ordered_Clause	*/	FALSE,
	/* Onto_Clause		*/	FALSE,
	/* Nest_Clause		*/	FALSE,
	/* Lastthread_Clause	*/	FALSE,
					},
					{
/* Psection */
	/* If_Clause		*/	FALSE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	FALSE,
	/* Lastlocal_Clause	*/	FALSE,
	/* Reduction_Clause	*/	FALSE,
	/* Mp_Schedtype_Clause	*/	FALSE,
	/* Chunk_Clause		*/	FALSE,
	/* Blocked_Clause	*/	FALSE,
	/* Affinity_Clause	*/	FALSE,
	/* Mode_Clause		*/	FALSE,
	/* Ordered_Clause	*/	FALSE,
	/* Onto_Clause		*/	FALSE,
	/* Nest_Clause		*/	FALSE,
	/* Lastthread_Clause	*/	FALSE,
					},
					{
/* Singleprocess */
	/* If_Clause		*/	FALSE,
	/* Local_Clause		*/	TRUE,
	/* Share_Clause		*/	FALSE,
	/* Lastlocal_Clause	*/	FALSE,
	/* Reduction_Clause	*/	FALSE,
	/* Mp_Schedtype_Clause	*/	FALSE,
	/* Chunk_Clause		*/	FALSE,
	/* Blocked_Clause	*/	FALSE,
	/* Affinity_Clause	*/	FALSE,
	/* Mode_Clause		*/	FALSE,
	/* Ordered_Clause	*/	FALSE,
	/* Onto_Clause		*/	FALSE,
	/* Nest_Clause		*/	FALSE,
	/* Lastthread_Clause	*/	FALSE,
					}
				};

boolean open_mp_clause_allowed[Num_Omp_Values][Last_Omp_Clause] = {
/* Parallel_Omp			*/
						{
	/* If_Omp_Clause		*/	TRUE,
       /* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */ TRUE,
	/* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	TRUE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	TRUE,
	/* Copyin_Omp_Clause		*/	TRUE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	FALSE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Do_Omp			*/
						{
	/* If_Omp_Clause		*/	FALSE,
       /* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */ FALSE,
       /* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	FALSE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	FALSE,
	/* Copyin_Omp_Clause		*/	FALSE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	TRUE,
	/* Ordered_Omp_Clause		*/	TRUE,
	/* Schedule_Omp_Clause		*/	TRUE,
	/* Affinity_Omp_Clause		*/	TRUE,
	/* Nest_Omp_Clause    		*/	TRUE,
	/* Onto_Omp_Clause    		*/	TRUE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Sections_Omp			*/
						{
	/* If_Omp_Clause		*/	FALSE,
       /* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */ FALSE,
       /* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	FALSE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	FALSE,
	/* Copyin_Omp_Clause		*/	FALSE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	TRUE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Single_Omp			*/
						{
	/* If_Omp_Clause		*/	FALSE,
	/* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */ FALSE,
	/* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	FALSE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	FALSE,
	/* Copyin_Omp_Clause		*/	FALSE,
	/* Reduction_Omp_Clause		*/	FALSE,
	/* Lastprivate_Omp_Clause	*/	FALSE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Workshare_Omp */  /* by jhs, 02/7/18 */
						{
	/* If_Omp_Clause		*/	FALSE,
	/* Num_Threads_Omp_Clause */ FALSE,
	/* Private_Omp_Clause		*/	FALSE,
	/* Shared_Omp_Clause		*/	FALSE,
	/* Firstprivate_Omp_Clause	*/	FALSE,
	/* Default_Omp_Clause		*/	FALSE,
	/* Copyin_Omp_Clause		*/	FALSE,
	/* Reduction_Omp_Clause		*/	FALSE,
	/* Lastprivate_Omp_Clause	*/	FALSE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Parallel_Do_Omp		*/
						{
	/* If_Omp_Clause		*/	TRUE,
       /* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */TRUE,
       /* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	TRUE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	TRUE,
	/* Copyin_Omp_Clause		*/	TRUE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	TRUE,
	/* Ordered_Omp_Clause		*/	TRUE,
	/* Schedule_Omp_Clause		*/	TRUE,
	/* Affinity_Omp_Clause		*/	TRUE,
	/* Nest_Omp_Clause    		*/	TRUE,
	/* Onto_Omp_Clause    		*/	TRUE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Parallel_Sections_Omp	*/
						{
	/* If_Omp_Clause		*/	TRUE,
       /* Num_Threads_Omp_Clause */ /* by jhs, 02/7/18 */ TRUE,
       /* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	TRUE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	TRUE,
	/* Copyin_Omp_Clause		*/	TRUE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	TRUE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
/* Parallel_Workshare_Omp */ /* by jhs, 02/7/18 */
						{
	/* If_Omp_Clause		*/	TRUE,
       /* Num_Threads_Omp_Clause */ TRUE,
	/* Private_Omp_Clause		*/	TRUE,
	/* Shared_Omp_Clause		*/	TRUE,
	/* Firstprivate_Omp_Clause	*/	TRUE,
	/* Default_Omp_Clause		*/	TRUE,
	/* Copyin_Omp_Clause		*/	TRUE,
	/* Reduction_Omp_Clause		*/	TRUE,
	/* Lastprivate_Omp_Clause	*/	FALSE,
	/* Ordered_Omp_Clause		*/	FALSE,
	/* Schedule_Omp_Clause		*/	FALSE,
	/* Affinity_Omp_Clause		*/	FALSE,
	/* Nest_Omp_Clause    		*/	FALSE,
	/* Onto_Omp_Clause    		*/	FALSE,
        /* Copyprivate_Omp_Clause       */      FALSE,
                                                },
				};


char	*(directive_region_str[Last_Region]) = {
		 "CMIC$ PARALLEL region",	/* Parallel_Region	*/
		 "CMIC$ DOALL region",		/* Doall_Region		*/
		 "CMIC$ DO PARALLEL region",	/* Do_Parallel_Region	*/
		 "CMIC$ GUARD region",		/* Guard_Region		*/
		 "CMIC$ CASE region",		/* Case_Region		*/
		 "C*$* REGIONBEGIN region",	/* Region_Region	*/
		 "C$PAR PARALLEL region",	/* Sgi_Parallel_Region	*/
		 "C$ DOACROSS region",		/* Doacross_Region	*/
		 "C$PAR PARALLEL DO region",	/* Parallel_Do_Region	*/
		 "C$PAR PDO region",		/* Pdo_Region		*/
		 "C$PAR PSECTION region",	/* Parallel_Section_Region */
		 "C$PAR CRITICAL SECTION region", /* Critical_Section_Region */
		 "C$PAR SINGLE PROCESS region",	/* Single_Process_Region */
                 "C$OMP PARALLEL region",	/* Open_Mp_Parallel_Region */
                 "C$OMP DO region",		/* Open_Mp_Do_Region */
                 "C$OMP PARALLEL SECTIONS region",
                                         /* Open_Mp_Parallel_Sections_Region */
                 "C$OMP SECTIONS region",	/* Open_Mp_Sections_Region */
                 "C$OMP SECTION region",	/* Open_Mp_Section_Region */
                 "C$OMP SINGLE region",		/* Open_Mp_Single_Region */
                 "C$OMP PARALLEL DO region",	/* Open_Mp_Parallel_Do_Region */
                 "C$OMP MASTER region",		/* Open_Mp_Master_Region */
                 "C$OMP CRITICAL region",	/* Open_Mp_Critical_Region */
                 "C$OMP ORDERED region",	/* Open_Mp_Ordered_Region */
                 "C$OMP WORKSHARE region", /* Open_Mp_Workshare_Region */ /* by jhs, 02/7/18 */
                 "C$OMP PARALLEL WORKSHARE region", /* Open_Mp_Parallel_Workshare_Region */ /* by jhs, 02/7/18 */
				};

enum	directive_stmt_values  {Case_Dir,
			        End_Case_Dir,
                                Doall_Dir,
				Do_Parallel_Dir,
				End_Do_Dir,
				Guard_Dir,
				End_Guard_Dir,
				Parallel_Dir,
				End_Parallel_Dir,
				Doacross_Dir,
				Sgi_Parallel_Dir,
				Sgi_End_Parallel_Dir,
				Psection_Dir,
				Section_Dir,
				End_Psection_Dir,
				Pdo_Dir,
				End_Pdo_Dir,
				Parallel_Do_Dir,
				Barrier_Dir,
				Critical_Section_Dir,
				End_Critical_Section_Dir,
				Single_Process_Dir,
				End_Single_Process_Dir,
				Copyin_Dir,
				Regionbegin_Dir,
				Regionend_Dir,
				Opaque_Dir,
				Optional_Dir,
                                Atomic_Open_Mp_Dir,
                                Barrier_Open_Mp_Dir,
                                Critical_Open_Mp_Dir,
                                Do_Open_Mp_Dir,
                                Endcritical_Open_Mp_Dir,
                                Enddo_Open_Mp_Dir,
                                Endparallel_Open_Mp_Dir,
                                Endparalleldo_Open_Mp_Dir,
                                Endparallelsections_Open_Mp_Dir,
                                Endparallelworkshare_Open_Mp_Dir, /* by jhs, 02/7/18 */
                                Endmaster_Open_Mp_Dir,
                                Endordered_Open_Mp_Dir,
                                Endsections_Open_Mp_Dir,
                                Endsingle_Open_Mp_Dir,
                                Endworkshare_Open_Mp_Dir, /* by jhs, 02/7/18 */
                                Flush_Open_Mp_Dir,
                                Master_Open_Mp_Dir,
                                Ordered_Open_Mp_Dir,
                                Parallel_Open_Mp_Dir,
                                Paralleldo_Open_Mp_Dir,
                                Parallelsections_Open_Mp_Dir,
                                Parallelworkshare_Open_Mp_Dir, /* by jhs, 02/7/18 */
                                Section_Open_Mp_Dir,
                                Sections_Open_Mp_Dir,
                                Single_Open_Mp_Dir,
                                Workshare_Open_Mp_Dir, /* by jhs, 02/7/18 */

				Last_Dir};

typedef enum directive_stmt_values directive_stmt_type;

char	*(directive_stmt_str[Last_Dir]) =   {
		"CMIC$ CASE",			/* Case_Dir		*/
	        "CMIC$ END CASE",		/* End_Case_Dir		*/
                "CMIC$ DOALL",			/* Doall_Dir		*/
		"CMIC$ DO PARALLEL",		/* Do_Parallel_Dir	*/
		"CMIC$ END DO",			/* End_Do_Dir		*/
		"CMIC$ GUARD",			/* Guard_Dir		*/
		"CMIC$ END GUARD",		/* End_Guard_Dir	*/
		"CMIC$ PARALLEL",		/* Parallel_Dir		*/
		"CMIC$ END PARALLEL",		/* End_Parallel_Dir	*/
		"C$ DOACROSS",			/* Doacross_Dir		*/
		"C$PAR PARALLEL",		/* Sgi_Parallel_Dir	*/
		"C$PAR END PARALLEL",		/* Sgi_End_Parallel_Dir */
		"C$PAR PSECTION",		/* Psection_Dir		*/
		"C$PAR SECTION",		/* Section_Dir		*/
		"C$PAR END PSECTION",		/* End_Psection_Dir	*/
		"C$PAR PDO",			/* Pdo_Dir		*/
		"C$PAR END PDO",		/* End_Pdo_Dir		*/
		"C$PAR PARALLEL DO",		/* Parallel_Do_Dir	*/
		"C$PAR BARRIER",		/* Barrier_Dir		*/
		"C$PAR CRITICAL SECTION",	/* Critical_Section_Dir */
		"C$PAR END CRITICAL SECTION",	/* End_Critical_Section_Dir */
		"C$PAR SINGLE PROCESS",		/* Single_Process_Dir	*/
		"C$PAR END SINGLE PROCESS",	/* End_Single_Process_Dir */
		"C$ COPYIN",			/* Copyin_Dir		*/
		"C*$* REGIONBEGIN",		/* Regionbegin_Dir	*/
		"C*$* REGIONEND",		/* Regionend_Dir	*/
		"C*$* OPAQUE",			/* Opaque_Dir		*/
		"C*$* OPTIONAL",		/* Optional_Dir		*/
                "C$OMP ATOMIC",			/* Atomic_Open_Mp_Dir	*/
                "C$OMP BARRIER",		/* Barrier_Open_Mp_Dir	*/
                "C$OMP CRITICAL",		/* Critical_Open_Mp_Dir	*/
                "C$OMP DO",			/* Do_Open_Mp_Dir	*/
                "C$OMP END CRITICIAL",		/* Endcritical_Open_Mp_Dir */
                "C$OMP END DO",			/* Enddo_Open_Mp_Dir	*/
                "C$OMP END PARALLEL",		/* Endparallel_Open_Mp_Dir */
                "C$OMP END PARALLEL DO",	/* Endparalleldo_Open_Mp_Dir */
                "C$OMP END PARALLEL SECTIONS",	
					/* Endparallelsections_Open_Mp_Dir */
		  "C$OMP END PARALLEL WORKSHARE", /* Endparallelworkshare_Open_Mp_Dir */ /* by jhs, 02/7/18 */
                "C$OMP END MASTER",		/* Endmaster_Open_Mp_Dir */
                "C$OMP END ORDERED",		/* Endordered_Open_Mp_Dir */
                "C$OMP END SECTIONS",		/* Endsections_Open_Mp_Dir */
                "C$OMP END SINGLE",		/* Endsingle_Open_Mp_Dir */
                "C$OMP END WORKSHARE", /* Endworkshare_Open_Mp_Dir */ /* by jhs, 02/7/18 */
                "C$OMP FLUSH",			/* Flush_Open_Mp_Dir	*/
                "C$OMP MASTER",			/* Master_Open_Mp_Dir	*/
                "C$OMP ORDERED",		/* Ordered_Open_Mp_Dir	*/
                "C$OMP PARALLEL",		/* Parallel_Open_Mp_Dir	*/
                "C$OMP PARALLEL DO",		/* Paralleldo_Open_Mp_Dir */
                "C$OMP PARALLEL SECTIONS", /* Parallelsections_Open_Mp_Dir */
                "C$OMP PARALLEL WORKSHARE", /* Parallelworkshare_Open_Mp_Dir */ /* by jhs, 02/7/18 */
                "C$OMP SECTION",		/* Section_Open_Mp_Dir	*/
                "C$OMP SECTIONS",		/* Sections_Open_Mp_Dir	*/
                "C$OMP SINGLE",			/* Single_Open_Mp_Dir	*/
                "C$OMP WORKSHARE" /* Workshare_Open_Mp_Dir */ /* by jhs, 02/7/18 */
				};

long directive_cant_be_in[Last_Dir] = {

			((0 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

				/* Case_Dir			*/

			((0 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Case_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Doall_Dir			*/

			((0 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Do_Parallel_Dir		*/

			((0 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Do_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Guard_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Guard_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Dir			*/

			((0 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Parallel_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Doacross_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sgi_Parallel_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sgi_End_Parallel_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Psection_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Section_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Psection_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Pdo_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Pdo_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Do_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Barrier_Dir			*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Critical_Section_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Critical_Section_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Single_Process_Dir		*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Single_Process_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (1 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Copyin_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (1 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Regionbegin_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Regionend_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Opaque_Dir			*/

                        ((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Optional_Dir                 */

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Atomic_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Barrier_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Critical_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Do_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endcritical_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Enddo_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallel_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparalleldo_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallelsections_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallelworkshare_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endmaster_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endordered_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endsections_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endsingle_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endworkshare_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Flush_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Master_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Ordered_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Paralleldo_Open_Mp_Dir	*/

                        ((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallelsections_Open_Mp_Dir       */

                        ((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallelworkshare_Open_Mp_Dir       */

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Section_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sections_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Single_Open_Mp_Dir	*/

			((1 << Parallel_Region) |
                         (1 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (1 << Doacross_Region) |
                         (1 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (1 << Open_Mp_Do_Region) |
                         (1 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (1 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (1 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (1 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Workshare_Open_Mp_Dir	*/

			};

long directive_must_be_in[Last_Dir] = {
			((1 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

				/* Case_Dir			*/

			((1 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (1 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Case_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Doall_Dir			*/

			((1 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Do_Parallel_Dir		*/

			((1 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (1 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Do_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Guard_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (1 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Guard_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Dir			*/

			((1 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Parallel_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Doacross_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sgi_Parallel_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sgi_End_Parallel_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Psection_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Section_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (1 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Psection_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Pdo_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (1 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Pdo_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Do_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Barrier_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Critical_Section_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (1 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Critical_Section_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (1 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Single_Process_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (1 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* End_Single_Process_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Copyin_Dir			*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Regionbegin_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (1 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Regionend_Dir		*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (1 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Opaque_Dir			*/

                        ((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Optional_Dir                 */

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Atomic_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Barrier_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Critical_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Do_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endcritical_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Enddo_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallel_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparalleldo_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallelsections_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (1 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endparallelworkshare_Open_Mp_Dir	*/


			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (1 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endmaster_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (1 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endordered_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (1 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endsections_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (1 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endsingle_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (1 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Endworkshare_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Flush_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Master_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Ordered_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallel_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Paralleldo_Open_Mp_Dir	*/

                        ((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallelsections_Open_Mp_Dir       */


                        ((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Parallelworkshare_Open_Mp_Dir       */

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Section_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Sections_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Single_Open_Mp_Dir	*/

			((0 << Parallel_Region) |
                         (0 << Doall_Region) |
                         (0 << Do_Parallel_Region) |
                         (0 << Guard_Region) |
                         (0 << Case_Region) |
                         (0 << Region_Region) |
                         (0 << Sgi_Parallel_Region) |
                         (0 << Doacross_Region) |
                         (0 << Parallel_Do_Region) |
                         (0 << Pdo_Region) |
                         (0 << Parallel_Section_Region) |
                         (0 << Critical_Section_Region) |
                         (0 << Single_Process_Region) |
                         (0 << Open_Mp_Parallel_Region) |
                         (0 << Open_Mp_Do_Region) |
                         (0 << Open_Mp_Parallel_Sections_Region) |
                         (0 << Open_Mp_Sections_Region) |
                         (0 << Open_Mp_Section_Region) |
                         (0 << Open_Mp_Single_Region) |
                         (0 << Open_Mp_Parallel_Do_Region) |
                         (0 << Open_Mp_Master_Region) |
                         (0 << Open_Mp_Critical_Region) |
                         (0 << Open_Mp_Ordered_Region) |
			 (0 << Open_Mp_Workshare_Region) |
			 (0 << Open_Mp_Parallel_Workshare_Region) ),

                                /* Workshare_Open_Mp_Dir	*/

			};


long	directive_state;

long_type	global_schedtype_value = -1;
int		global_schedtype_line;
int		global_schedtype_col;
