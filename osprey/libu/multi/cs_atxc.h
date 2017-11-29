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


/* USMID @(#) libu/multi/cs_atxc.h	92.0	10/08/98 14:57:41 */
#ifndef _ATXC_H_
#define _ATXC_H_

#ifndef _SDRAGON
#ifndef sun
#define _ATXCRAY
#endif
#endif

#ifndef _ATXCRAY
/*
 *	The following structure definition is
 *	valid for suns and Super Dragons.
 */
struct	ATX_MNX {
	long	max;			/* Maximum value */
	long	min;			/* Minimum value */
	long	total;			/* Total */
};

struct	ATX_CV {
	long	count;			/* Number of times value incremented */
	long	value;			/* Value */
};

struct	ATX_MNX_CV {
	long	max;			/* Max value */
	long	min;			/* Min value */
	long	total;			/* Total value */
	long	count;			/* Number time total incremented */
};
#else
/*
 *	The following structure definition should
 *	be used if this is being built for a Cray.
 */
struct	ATX_MNX {
	int	max:32,			/* Maximum value */
		min:32;			/* Minimum value */
	int	total;			/* Total */
};
struct	ATX_CV {
	int	count:32,		/* Number of times value incremented */
		value:32;		/* Value */
};
struct	ATX_MNX_CV {
	int	max:32,			/* Max value */
		min:32;			/* Min value */
	int	total:32,		/* Total value */
		count:32;		/* Number time total incremented */
};
#endif

/*
 *	Following are defines that could be
 *	be machine specific.
 */
#ifndef _ATXCRAY
#define	_ATX_MAXCPUS	64  		/* Max number of cpus */
#else
#define _ATX_MAXCPUS	16		/* Max number of cpus */
#endif
#define _ATX_MAXCS	16 		/* Max number of control structures */
#ifndef _ATXCRAY
#define _ATX_MXTIME	07777777777
#else
#define _ATX_MXTIME	037777777777
#endif

/*
 *	The ATX_COM structure mirrors the Fortran
 *	ATX@COM common block.  Note that for the
 *	multi dimensioned arrays, the subscripts have
 *	been reversed from the Fortran notation since
 *	Fortran stores in column order and C stores in
 *	row order.
 *
 *	maxcpu				- Max # of cpus allowed
 *	cpu
 *	pst	 			- Prev serial time 
 *	bp		 		- Begin parallel time 
 *	ep		 		- End parallel time 
 *	bcs[_ATX_MAXCS][_ATX_MAXCPUS]  	- Begin Control Structure time 
 *	top[_ATX_MAXCS][_ATX_MAXCPUS]  	- Top of iteration loop time 
 *	bot[_ATX_MAXCS][_ATX_MAXCPUS]  	- Bottom of iteration loop time 
 *	itc[_ATX_MAXCS][_ATX_MAXCPUS]  	- Iteration counts 
 *	itr[_ATX_MAXCS]			- Total iteration trips 
 *	do[_ATX_MAXCS]			- Do loop number 
 *	type[_ATX_MAXCS]		- Type of loop 
 *	savl[_ATX_MAXCS]
 *	guard[_ATX_MAXCS]
 *	parm[_ATX_MAXCS]
 *	wt[_ATX_MAXCS][_ATX_MAXCPUS]
 */
struct	ATX_COM {
	long		maxcpu;				
	long		cpu;
	long long	pst;
	long long	bp;
	long long	ep;	
	long long	bcs[_ATX_MAXCS][_ATX_MAXCPUS];
	long long	top[_ATX_MAXCS][_ATX_MAXCPUS];
	long long	bot[_ATX_MAXCS][_ATX_MAXCPUS];
	long		itc[_ATX_MAXCS][_ATX_MAXCPUS];
	long		itr[_ATX_MAXCS];
	long		do_name[_ATX_MAXCS];
	long		type[_ATX_MAXCS];
	long		savl[_ATX_MAXCS];
	long		guard[_ATX_MAXCS];
	long		parm[_ATX_MAXCS];
	long		wt[_ATX_MAXCS][_ATX_MAXCPUS];
};

/*
 *	The ATX_COM_SAVE structure forms a linked
 *	list of ATX_COM structures.
 */
struct	ATX_COM_SAVE {
	struct	ATX_COM atxcom;
	struct	ATX_COM_SAVE *next;
};

/*	The UT_COM structure mirrors the Fortran 
 *	UT@COM common block.
 */
struct	UT_COM {
	char	*subr;		/* Subroutine name */
	char	*slave;		/* Slave name */
	int	rloc;		/* Location */
	int	trips;
	int	sched;
	int	icase;
	int	label;
	int	savelast;
	int	guard;
	int	param;
	int	wait;
};
	
/*
 *	ATX_pr is the parallel region structure.
 *	One is allocated per parallel region
 *	seen in the program.
 */
struct	ATX_pr {
	int		 ident;		/* Parallel region identifier */
	char		 *sub_name;	/* Subroutine name */
	char		 *slv_name;	/* Slave name */
	int		 num_loops;	/* Number of loop structures */
	int		 first_pass_done;/* First pass flag for unitasking */
	struct	ATX_loop *loops;	/* List of loop structures */
	struct	ATX_pst  *pst;		/* List of PST structures */
	struct	ATX_MNX_CV bpt;		/* Begin parallel times */
	struct	ATX_MNX_CV sat;		/* Slave arrival times */
	struct	ATX_MNX_CV wst;		/* Wait slave times */
	struct	ATX_CV	 work_cpus;	/* Number of CPU's that did work */
	struct	ATX_pr 	 *next;		/* Pointer to next one in list */
};

/*
 *	ATX_pst is the PST structure (Preceeding
 *	Serial Time).  This holds the preceeding
 *	serial times for each place a routine
 *	was called from.
 */
struct	ATX_pst {
	struct	ATX_MNX times;		/* Min/Max/ total times */
	char		*name;		/* Name of routine that called us */
	int		count;		/* Number of times hit */
	struct	ATX_pst *next;		/* Pointer to next one in list */
};

/*
 *	ATX_loop is the loop structure.
 *	One structure exists for each parallel
 *	loop within a parallel region.
 *
 *	Total_time[_ATX_MAXCPUS] - # of sample for n cpu's where n = 1..maxcpu 
 *	iter_time[_ATX_MAXCPUS]	- Per iteration times  for n cpu's
 *	u_iter_time		- Min/max time spent per iteration for unitasked runs
 *		(NuTu - mnxTu)
 *	u_min_time_max_iter	- Min time per iteration for largest unitasked runs
 *		(mnTuIOT)
 *	u_iter_sample		- Min/Max # of unitasked iterations for a sample 
 *		(NuIter - mnxNuIter)
 *	c_iter_sample		- Min/Max #  of concurrent iterations for a sample
 *		(NcIter - mnxNcItr)
 *	ATX_MNX	c_iter_overhead	- Min/Max time per iteration for largest concurrent runs
 *		(mnxIOT - totIOT)
 *	wait_time		- Min/Max/total wait times 
 *		(mnxWT - totWT)
 *
 *	loop_sync_time		- Min/Max/total loop syncronization time 
 *		(mnxLST - totLST)
 *	inter_loop_time	 	- Min/Max/total inter loop time 
 *		(mnxILT - totILT)
 *	bcs_time		- Min/Max/total begin control structure time 
 *		(mnxBCT - totBCT)
 *	proc_delta_time	 	- Min/Max/total processor delta time - time
 *		(mnxPDT - totPDT)
 *				  between arriving cpus after a begin
 *				  parallel and after interloop code 
 *	parm			- Schedualing parameter (Normaly chunk size)
 *	do_loop			- Do loop number 
 *	flags			- Loop flags 
 *	type			- Loop type 
 *	*next			- Pointer to next one in list 
 */
struct	ATX_loop {
	struct	ATX_CV		Total_time[_ATX_MAXCPUS];
	struct	ATX_MNX		iter_time[_ATX_MAXCPUS];	
	struct	ATX_MNX_CV	u_iter_time;
	int			u_min_time_max_iter;
	struct	ATX_MNX_CV 	u_iter_sample;
	struct	ATX_MNX_CV	c_iter_sample;
	struct	ATX_MNX_CV	c_iter_overhead;
	struct	ATX_MNX_CV	wait_time;
	struct	ATX_MNX_CV	loop_sync_time;
	struct	ATX_MNX_CV	inter_loop_time;
	struct	ATX_MNX_CV	bcs_time;
	struct	ATX_MNX_CV	proc_delta_time;
	int			parm;
	int			do_loop;
	int			flags;
	int			type;
	struct	ATX_loop 	*next;
};
/*
 *  Type values
 */
#define	_ATX_CASE	0
#define _ATX_CHNKSZ	1
#define _ATX_GUIDED	2
#define _ATX_NCHNKS	3
#define	_ATX_SINGLE	4
#define _ATX_VECTOR	5
/*
 *  Flag Values
 */
#define	_ATX_VALID	1
#define _ATX_GUARDED	2
#define _ATX_ENDCASE	4
#define _ATX_SAVELAST	8

#endif /* ! _ATXC_H_ */
