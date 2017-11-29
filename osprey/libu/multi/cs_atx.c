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


#pragma ident "@(#) libu/multi/cs_atx.c	92.3	06/25/99 14:32:18"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <sys/utsname.h>
#include "cs_atxc.h"
#include <sys/types.h>			/* For time() call */
#include <sys/time.h>			/* For time() call */

#ifdef _ATXCRAY
void	ATX_CP();
#else
void	atx_cp_();
#endif
void	atx_cs();
void	atx_delete_cs();
void	atx_exit();
void	atx_slave_();
long long	atx_rtc_();
long long	ATX_IRTC();
long	atx_update_rtc_();
double	ATX_TSECND();
double	ATX_SECOND();
double	ATX_TIMEF();
struct	ATX_loop *atx_mklp();

struct	ATX_pr	*_ATX_TOP_PR = 0;	/* Top parallel region (pr) structure
					   in pr list */
struct	ATX_COM_SAVE *_ATXCOM_TOP = 0;	/* Top atxcom save structure in list */
/*
 *  	The following structure provides global variables
 *	that need to be saved across routine calls.
 */
struct	ATX_GLOBAL {
	struct	ATX_pr	*current;	/* Current parallel region */
	struct	ATX_pr	*prev;		/* Previous parallel region */
	int	bpmin;			/* Min time for a begin parallel */
	int	maxvl;
	int	npr;
	int	ipatxptr;
	int	atxptrsz;
	int	ipatxnam;
	int	atxnamsz;
	int	maxcpu;
	int	maxmem;
	double	clkrate;
	int	itsstart;
	int	atxtrace;
	int	atxon;			/* Rework this flag */
	int	ispread;
	int	uspread;
	int	nbpx;
	int	nunitask;		/* Number of times unitasked */
	int	unitask;
	int	iutqbp;
	int	ncs;			/* Number of Unitasked control structures
					   or total number of control structures. 
					   The number of control structures roughly
					   equates to the number of loops. */
	int	csx;
	int	sched;
	int	nworks;
	int	noworks;
	int	tdisconp;		/* Time spent in parallel disconnects */
	int	tdiscons;		/* Time spent in serial disconnects */
	int	ndisconp;		/* Number of parallel disconnects */
	int	ndiscons;		/* Number of serial disconnects */
	int	ncstot;
	int	traceon;		/* Atexpert trace flag */
	/*
	 *  Everything above this point gets initialized
	 *  in the declaration of _atx.  Everything
	 *  below this point, doesn't.
	 */
	int	atxargs[2];
	int	ts_ovhd;		/* ATX_TSECND or getrusage overhead */
					/* Used to be itsovhd */
	int	atxpstsv;		/* Saved value of preceeding serial time */
	int	atxbpsv;
	int	atxonsv;
	int	outerpst;
	int	uttripsv;		/* Saved value of utcom->trips */
	int	initlpst;
	char	machtype[16];		/* Machine type */
};

/*
 *  Initial values for _atx
 */
struct ATX_GLOBAL _atx = {
	 0,	/* current */
	 0,	/* prev */
	 600,	/* bpmin */
	 64,	/* maxvl */
	 0,	/* npr */
	 0,	/* ipatxptr */
	 0,	/* atxptrsz */
	 0,	/* ipatxnam */
	 0,	/* atxnamsz */
	 1,	/* maxcpu */
	 0,	/* maxmem */
	 0.0,	/* clkrate */
	 0,	/* itsstart */
	 0,	/* atxtrace */
	 0,	/* atxon */			/* Rework this flag */
	 1,	/* ispread */
	 1,	/* uspread */
	 0,	/* nbpx */
	 0,	/* nunitask */
	 0,	/* unitask */
	 0,	/* iutqbp */
	 0,	/* ncs */			/* Number of unitasked loops */
	 0,	/* csx */
	 1,	/* sched */
	 0,	/* nworks */
	 0,	/* noworks */
	 0,	/* tdisconp */
	 0,	/* tdiscons */
	 0,	/* ndisconp */
	 0,	/* ndiscons */
	 0,	/* ncstot */
	 0,	/* traceon */
};

/*
 *  If we are not executing on a cray, we define
 *  the timing routines here.
 */
#ifndef _ATXCRAY

#include <sys/time.h>		/* For gettimeofday() and /proc */
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/fcntl.h>
#include <sys/procfs.h>

long	_ATX_TBASE = 0;
double	_ATX_TIMEF = 0;
int	_ATX_PROC_FID = 0;


long long	ATX_IRTC()

{
	struct	timeval	tp;		
	struct	timezone tzp;		
	long	i;			

	i = gettimeofday(&tp,&tzp);
	return((tp.tv_sec * 1000000) + tp.tv_usec);
}
long long	atx_rtc_(mem)
	long long	*mem;

{
	struct	timeval	tp;		
	struct	timezone tzp;		
	long	i;			

	i = gettimeofday(&tp,&tzp);
	*mem = ((tp.tv_sec * 1000000) + tp.tv_usec);
}

long	atx_update_rtc_(a)
	long long	*a;
{
	*a = ATX_IRTC() - *a;
}

	

double	ATX_TSECND()

{
	int	i;			/* Temp integers */
	struct 	prusage	pr;		/* Usage structure */
	char	myfile[24];		/* Open /proc file string */
	int	me;			/* My pid */

	if(!_ATX_PROC_FID) {
		me = getpid();
		sprintf(myfile,"/proc/%d",me);
		_ATX_PROC_FID = open(myfile,O_RDONLY);
		if(_ATX_PROC_FID < 0) _ATX_PROC_FID = 0;
	}
	i = ioctl(_ATX_PROC_FID,PIOCUSAGE,&pr);
	if(i < 0) return(0);
	return((float) pr.pr_utime.tv_sec + (pr.pr_utime.tv_nsec/1000000000));
}

double ATX_TIMEF()
{
	struct	timeval	tp;		/* gettimeofday() structure */
	struct	timezone tzp;		/* gettimeofday() structure */
	long	i;			/* Temp integers */
	double	di;			/* Temp doubles */

	i = gettimeofday(&tp,&tzp);
	di = (float)tp.tv_sec * 1000.0 + (float)tp.tv_usec / 1000.0;
	if(_ATX_TIMEF == 0.0) 
		_ATX_TIMEF = di;
	return(di - _ATX_TIMEF);
}

double	ATX_SECOND()

{
	int	i;			/* Temp integers */
	int	tsec;			/* Total seconds */
	int	tnsec;			/* Total nsecs */
	struct 	prusage	pr;		/* Usage structure */
	char	myfile[24];		/* Open /proc file string */
	int	me;			/* My pid */

	if(!_ATX_PROC_FID) {
		me = getpid();
		sprintf(myfile,"/proc/%d",me);
		_ATX_PROC_FID = open(myfile,O_RDONLY);
		if(_ATX_PROC_FID < 0) _ATX_PROC_FID = 0;
	}
	i = ioctl(_ATX_PROC_FID,PIOCUSAGE,&pr);
	if(i < 0) return((float) 0);
	tsec = pr.pr_utime.tv_sec + pr.pr_stime.tv_sec + pr.pr_ttime.tv_sec;
	tnsec = pr.pr_utime.tv_nsec + pr.pr_stime.tv_nsec + pr.pr_ttime.tv_nsec;
	return((float) tsec + (tnsec / 1000000000));
}

#endif

/*
 *	ATX_BP is called at a begin parallel region by the
 *	user code.
 */
#ifdef _ATXCRAY
ATX_BP() 
#else
atx_bp_() 
#endif

{
	long long	i,j;		/* Temp integers */
	double	di;			/* Temp doubles */
#ifdef _ATXCRAY
	int	mc[128];		/* GETHMC() structure */
#endif
	struct	ATX_COM	*atxcom;	/* ATX@COM common block pointer */
	struct	UT_COM 	*utcom;		/* UT@COM common block pointer */
	struct	ATX_pr	*pr;		/* Parallel region structure */
	struct	ATX_pr	*pr_follow;	/* Temp parallel region pointer */
	struct	ATX_pst	*pst;		/* Preceeding Serial Time structure */
	struct	ATX_pst	*pst_follow;	/* Preceeding Serial Time structure */
	struct	ATX_loop *loop;		/* Loop structure pointer */
	long long	init_time0;
	long long	init_time1;
	double	process_time;		/* Time process has run so far */
					/* Used to be tatxbp */
	int	ign;			/* Dummy argument to UTQGN */
	long	*slave;			/* Pointer to ATX@SLAVE external */
	int	save_pst;		/* Preceeding serial time */
	int	Td;			/* Disconnect time */


	/*
	 *   The first thing we do is read the clock
	 */
	init_time0 = ATX_IRTC();
	process_time = ATX_TSECND();
	init_time1 = ATX_IRTC();
	/*
	 *  The following calls to GETATXC and GETUT get
	 *  pointers to the ATX@COM common block and the
	 *  UT@COM common block.  
	 */
#ifdef _ATXCRAY
	GETATXC(&atxcom);
	GETUT(&utcom);
/*
	GETSLAVE(&slave);
*/
#else
	getatxc_(&atxcom);
	getut_(&utcom);
/*
	getslave_(&slave);
*/
#endif
	/*
	 *  The following call to _UTQGN() is called to get and
	 *  set a psuedo semaphore (number 59).  This causes the
	 *  code to be single threaded so we can protect the
	 *  updating of atxon.  If we are the outer loop, atxon will
	 *  equal 0 (zero).  Otherwise, atxon is non-zero and we
	 *  do some updating and return at this point.
	 */
	ign = 0;
	ign = _UTQGN(59,ign);
	if(_atx.atxon) {
		if(_atx.atxon > 0) {
			_atx.atxonsv = _atx.atxon;
			_atx.atxon = -1;
			_atx.atxpstsv = _atx.outerpst;
			_atx.atxbpsv = atxcom->bp;
			_atx.uttripsv = utcom->trips;
			/*
			 *  Check here to see if we have forced unitasking
			 *  yet.  If not, call _UTQBP to tell it that tasking
			 *  is occurring at a higher level.  This will "fool"
			 *  the code into running the current parallel region
			 *  in unitasked mode.
			 */
			if(!_atx.iutqbp) {
				_atx.atxargs[0] = 1;
				_atx.atxargs[1] = (int)&_atx.maxmem;
				if(_UTQBP(&atx_slave_,&_atx.atxargs[0], 10000))
					_atx.iutqbp = 1;
			}
		} else
			--_atx.atxon;
		ign = _UTQEGN(59,ign);
		return;
	}
	/*
	 *  This rest of this code is only executed by the outermost parallel
	 *  loop, therefore, we can release the semaphore.
	 */
	ign = _UTQEGN(59,ign);
	_atx.outerpst = atxcom->pst;	/* Get preceeding serial time */
	/*
	 *  Now we walk through all pr (parallel region) structures
	 *  to see if this parallel region already has been executed
	 *  (and already has a structure allocated for it).
	 */
	_atx.prev = _atx.current;
	pr = pr_follow = _ATX_TOP_PR;
	while (pr != 0) {
		if(pr->ident == utcom->rloc)
			break;			/* We found it */
		pr_follow = pr;
		pr = pr->next;
	}
	if(!pr) {	
		/*
		 *  pr structure has never been allocated for
		 *  this parallel region.  Create a new one
		 *  and link it into the list;
		 */
		++_atx.npr;
		pr = (struct ATX_pr *)malloc(sizeof(struct ATX_pr));
		if(!pr) {
			printf("Fatal ATEXPERT Error--Could not allocate pointer space\n");
			atx_abt(1);
		}
		i = (int) pr;
		if(i > _atx.maxmem)
			_atx.maxmem = i;
		if(_ATX_TOP_PR) {
			/*
			 *  This isn't the first time through.
			 *  Link new pr structure into list.
			 */
			pr_follow->next = pr;
		} else {
			/*
			 *  This is the first time through.
			 *  Do ATEXPERT initialization here.
			 */
			_ATX_TOP_PR = pr;
#ifndef _ATXCRAY
			atexit(atx_exit);
			strcpy(_atx.machtype,"_SDRAGON");
			_atx.bpmin = 200;
			_atx.clkrate = 1.0/1.0e-6;
			_atx.maxvl = 0;
			di = ATX_TIMEF();
			j = ATX_IRTC();
			di = ATX_TSECND();
			_atx.ts_ovhd = ATX_IRTC() - j;
#else
		/*
		 *  Following is for CRAY's only
		 */
			ATEXIT(atx_exit);
			GETHMC(mc);
			strncpy(_atx.machtype,(char *)&mc[0],8);
			_atx.machtype[8] = '\0';
			if(strncmp((char *)&mc[0],"CRAY-YMP",8) == 0) {
				_atx.bpmin = 600;
			} else if(strncmp((char *)&mc[0],"CRAY-XMP",8) == 0) {
				_atx.bpmin = 200;
			} else if(strncmp((char *)&mc[0],"CRAY-C90",8) == 0) {
				_atx.bpmin = 200;
				_atx.maxvl = 128;
			} else if(strncmp((char *)&mc[0],"CRAY-2",6) == 0) {
				_atx.bpmin = 200;
			}
			_atx.clkrate = 1.0/(mc[6]*1.0e-12);
			di = ATX_TIMEF() * 0.001;
			j = ATX_IRTC();
			di = ATX_TSECND();
			_atx.ts_ovhd = ATX_IRTC() - j;
#endif
			_atx.outerpst = (int)(process_time * _atx.clkrate);
			_atx.initlpst = _atx.outerpst;
		}
		/*
		 *  Now we initialize some values in the
		 *  the new pr structure.
		 */
		pr->next = 0;
		pr->loops = 0;
		pr->num_loops = 0;
		pr->pst = 0;
		pr->bpt.max = 0;
		pr->bpt.min = _ATX_MXTIME;
		pr->bpt.total = 0;
		pr->sat.max = 0;
		pr->sat.min = _ATX_MXTIME;
		pr->sat.total = 0;
		pr->sat.count = 0;
		pr->wst.max = 0;
		pr->wst.min = _ATX_MXTIME;
		pr->wst.total = 0;
		pr->wst.count = 0;
		pr->work_cpus.count = 0;
		pr->work_cpus.value = 0;
		pr->first_pass_done = 0;
		/*
		 *  Pick up and save the subroutine and slave name
		 */
		pr->sub_name = (char *)malloc(strlen(utcom->subr) + 1);
		pr->slv_name = (char *)malloc(strlen(utcom->slave) + 1);
		strcpy(pr->sub_name,utcom->subr);
		strcpy(pr->slv_name,utcom->slave);

		pr->ident = utcom->rloc;
	}
	_atx.atxon = 1;
	++_atx.nbpx;
	/*
	 *  Now we find the minimum overhead for calls to
	 *  getrusage (Super Dragon) or ATX_TSECND (Crays).
	 */
	i = init_time1 - init_time0;
	if( i < _atx.ts_ovhd)
		_atx.ts_ovhd = i;
	/*
	 *  Now we check for disconnects during the preceding
	 *  serial time (stored in _atx.outerpst).
	 *  We do this by comparing the preceeding serial time
	 *  with the current process time minus the start time
	 *  of the last serial region.  They should be about
	 *  the same.  If they are off by more than 1000 clocks,
	 *  we assume we have suffered a disconnect.
	 */
	Td = _atx.outerpst - (process_time * _atx.clkrate - _atx.itsstart);
	if(Td > 1000) {
		if(_atx.outerpst > Td)
			_atx.outerpst -= Td;
		++_atx.ndiscons;
		_atx.tdiscons += Td;
	}
	/*
	 *  Now we set up the pst (preceeding serial time) structure
	 *  for this execution of a parallel region.
	 */
	pst = pst_follow = pr->pst;
	while(pst && _atx.prev) {
		if(pst->name == _atx.prev->slv_name)
			break;
		pst_follow = pst;
		pst = pst->next;
	}
	if(!pst) {
		/*
		 *  First time this parallel region was executed
		 *  after the last parallel region seen.  We need
		 *  to allocate a new structure for it.
		 */
		pst = (struct ATX_pst *)malloc(sizeof(struct ATX_pst));
		i = (int) pst;
		if(i > _atx.maxmem)
			_atx.maxmem = i;
		if(!pst) {
			printf("Fatal ATEXPERT Error--Could not allocate pointer space\n");
			atx_abt(1);
		}
		if(!pst_follow) {
			/*
			 *  First pst structure in parallel region.
			 *  Link into the pr structure.
			 */
			pr->pst = pst;
		} else {
			/*
			 *  Link into list.
			 */
			pst_follow->next = pst;
		}
		pst->count = 0;
		pst->times.min = _ATX_MXTIME;
		pst->times.max = 0;
		pst->times.total = 0;
		if(!_atx.prev) {
			pst->name = (char *)malloc(16);
			strcpy(pst->name,"$START");
		} else {
			pst->name = _atx.prev->slv_name;
		}
		pst->next = NULL;
	}
	++pst->count;
	if(_atx.outerpst > 1)
		save_pst = _atx.outerpst;
	else
		save_pst = 1;
	if(save_pst < pst->times.min)
		pst->times.min = save_pst;
	if(save_pst > pst->times.max)
		pst->times.max = save_pst;
	pst->times.total += save_pst;
	/*
	 *  Now reset the current 
	 */
	_atx.current = pr;
	utcom->guard = 0;
	utcom->wait = 0;
	_atx.sched = 1;
	atxcom->cpu = 1;
	/*
	 *  The following code selects how to run the
	 *  parallel region.  It can run the code
	 *  unitasked or run the code on any number
	 *  of cpu's (up to the machine limit).
	 */
	_atx.unitask = 0;
	atxcom->maxcpu = _ATX_MAXCPUS;
	if(!pr->first_pass_done) {
		/*
		 *  This is the first pass for this parallel
		 *  region.  We always run the first pass unitasked.
		 */
		pr->first_pass_done = 1;
		_atx.unitask = 1;
	} else {
		loop = pr->loops;
		if(!loop->u_iter_sample.count) {
			/*
			 *  Force unitasking on first sample 
			 */
			_atx.unitask = 1;
		} else if(!loop->Total_time[0].count) {
			/*
			 *  We haven't run a 1 cpu parallel run
			 *  yet.  Do it now.
			 */
			atxcom->maxcpu = 1;
		} else if(loop->u_iter_sample.count < 8) {
			/*
			 *  If we have less then 8 unitask samples
			 *  and the number of samples is > 2**u_iter_sample.count
			 *  then we unitask.
			 */
			i = loop->u_iter_sample.count;
			for(j = 0; j < _atx.maxcpu; j++) 
				i += loop->Total_time[j].count;
			if(i >> loop->u_iter_sample.count)
				_atx.unitask = 1;
			else if(i > 10 && loop->Total_time[0].count < 3)
				atxcom->maxcpu = 1;
/*
		} else if((ATX_IRTC() & 037) == 0) {
*/
		} else if(--_atx.uspread == 0) {
			/*
			 *  Random unitasking
			 */
			_atx.uspread = 3;
			_atx.unitask = 1;
		} else {
			/*
			 *  We spread the sample across
			 *  1 .. maxcpu's
			 */
			--_atx.ispread;
			if(_atx.ispread == 0) {
				_atx.ispread = 2;
				for(atxcom->maxcpu = 1;
				    atxcom->maxcpu <= _atx.maxcpu;
				    atxcom->maxcpu++) {
					if(loop->Total_time[atxcom->maxcpu-1].count < 3)
						break;
				}
/*
				if(loop->Total_time[atxcom->maxcpu-1].count > 2)
					atxcom->maxcpu = _ATX_MAXCPUS;
*/
			}
		}
	}
	/*
	 *  Now we check to see if we need to force
	 *  unitasking.
	 */
	if(_atx.unitask == 1) {
		_atx.atxargs[0] = 1;
		_atx.atxargs[1] = (int)&_atx.maxmem;
		if(_UTQBP(&atx_slave_,&_atx.atxargs[0],1))
			_atx.iutqbp = 1;
		else
			_atx.iutqbp = 0;
	}
	/*
	 *  Last thing we do is set the itsstart variable with
 	 *  the current time.
	 */
	_atx.itsstart = ATX_TSECND() * _atx.clkrate + 50 + _atx.ts_ovhd;
}

/*
 *  ATX_EP is called at the end of a parallel region.
 */
#ifdef _ATXCRAY
ATX_EP(csn)
#else
atx_ep_(csn)
#endif
	int	*csn;		/* Total number of control structures */

{
	int	i,j,k;			/* Temp integers */
	struct	ATX_COM	*atxcom;	/* ATX@COM common block pointer */
	struct	UT_COM 	*utcom;		/* UT@COM common block pointer */
	struct	ATX_pr	*pr;		/* Parallel region structure */
	struct	ATX_loop *loop;		/* Loop structure pointer */
	struct	ATX_loop *lwalker;	/* Loop structure walker pointer */
	long long	init_time;
	double	process_time;		/* Time process has run so far */
					/* Used to be tatxbp */
	int	ign;			/* Dummy argument to UTQGN */
	int	nconnect;
	int	**g_tsbptr;		/* Pointer to g@tsbptr common */
	int	*tsbptr;
	int	reconnect[64];
	int	taskcps[64];
	int	bcs[_ATX_MAXCPUS];	/* Begin control structure times */
	int	top[_ATX_MAXCPUS];	/* Top of loop times */
	int	bot[_ATX_MAXCPUS];	/* Bottom of loop times */
	int	itc[_ATX_MAXCPUS];	/* Iteration count per cpu */
	int	wsw[_ATX_MAXCPUS];
	int	iter;			/* Iteration count */
	int	donm;			/* Do loop name */
	int	type;			/* Loop type */
	int	savl;
	int	guard;
	int	parm;			/* Sched parameter */
	int	donm1;			/* First do loop name in control structures */
	int	Td;			/* Disconnect time (if any) */
	int	Tu,Tio;
	int	minTw,maxTw,Tw;		/* Wait time (if any) */
	int	ics;			/* The current control structure index */
	int	niter,inctime;		/* Temp interation count/time */
	int	time_per_iter;		/* Time per iteration */
	long long	maxbcs,minbcs;		/* Min/max begin control struct time */
	int	bcstot;			/* Total begin control struct time */
	int	bp_time;		/* Begin Parallel time */
	long long	min_arrv;		/* Minimum slave arrival time */
	int	mxcpu;			/* Maximum number of entering cpus */
	int	ncwrk;			/* Number of working cpus */
	int	ncwrkd;
	int	valid;			/* Valid sample flag */
	int	slave;			/* Slave arrival time */
	int	maxbot,maxbots,maxbotsv;/* Max bottom loop times */
	int	maxt;
	int	minbot,mint,lastbot0;	/* Min bottom/top loop times */
	int	mincasebc;		/* Min CASE begin control structure time */
	int	Tdloops;		/* Number of loops that suffered a disconnet */
	int	idiscon;		/* Number of disconnected cpus */
	int	nditers;		/* Number of disconnected iterations */
	int	Tcitr,niterc,num_iters;	/* Number of iterations */
	int	Tctot,avgTcitr;		/* Total/avg iteration time */
	int	tottc;
	long long	maxTcitr,minTcitr;	/* Min/Max iteration time */
	int	nxttally,iltally;	/* ? */
	int	lasttype;		/* Last loop type seen */
	int	mintops,maxtops;	/* Top of loop times */
	int	ncpu;
	int	mintim;
	int	incws,totws,avgws;	/* Temp/Total/avg wait/send time */
	int	tally;			/* Tally flag */
	int	Til;			/* Inter Loop Tally */
	int	Tbc;			/* Begin Control Struct tally */
	int	Tls;			/* Loop Sync tally */
	int	Tws;			/* Wait/send tally */
	int	Tpd;			/* Proc Delta tally */
	int	cflag;			/* Continue loop flag */
	/*
	 *   The first thing we do is read the clock
	 */
	process_time = ATX_TSECND();
	init_time = ATX_IRTC();
	/*
	 *  The following calls to GETATXC and GETUT get
	 *  pointers to the ATX@COM common block and the
	 *  UT@COM common block.  
	 */
#ifdef _ATXCRAY
	GETATXC(&atxcom);
	GETUT(&utcom);
	GETGTSB(&g_tsbptr);
#else
	getatxc_(&atxcom);
	getut_(&utcom);
	getgtsb_(&g_tsbptr);
#endif
	/*
	 *  The following call to _UTQGN() is called to get and
	 *  set a psuedo semaphore (number 59).  This causes the
	 *  code to be single threaded so we can protect the
	 *  updating of atxon. 
	 */
	j = _ATX_MAXCPUS;
	ign = 0;
	ign = _UTQGN(59,ign);
	if(_atx.atxon <= 0) {
		if(_atx.atxon == 0) return;
		++_atx.atxon;
		if(_atx.atxon == 0) {
			_atx.atxon = _atx.atxonsv;
			atxcom->bp = _atx.atxbpsv;
			_atx.outerpst = _atx.atxpstsv;
			utcom->trips = _atx.uttripsv;
		}
		ign = _UTQEGN(59,ign);
		return;
	}
	_atx.atxon = 0;
	/*
	 *  Release semaphore
	 */
	ign = _UTQEGN(59,ign);
	nconnect = 0;
	for(i = 0; i < _ATX_MAXCPUS; i++) {
		tsbptr = g_tsbptr[i];
		if(tsbptr) {
			reconnect[i] = tsbptr[0];
			taskcps[i] = tsbptr[1];
			nconnect = i + 1;
		} else {
			reconnect[i] = 0;
			taskcps[i] = 0;
		}
	}
	/*
	 *  Clear unitasking if set by ATX_BP earlier
	 */
	if(_atx.iutqbp == 1) {
		_atx.iutqbp = 0;
		_UTQEP();
	}
	/*
	 *  Do some sanity checks here.
	 */
	if(!_atx.ncs && atxcom->cpu > _ATX_MAXCPUS) {
		printf("Fatal ATEXPERT Error--# cpus = %i  max is _ATX_MAXCPUS\n",
			atxcom->cpu);
		atx_abt(1);
	}
	if(_atx.ncs) {
		/*
		 *  This was unitasked.  Note that unitasking can occurr
		 *  regardless whether ATEXPERT forced it or not.
		 */
		_atx.unitask = 1;
		atxcom->cpu = 1;
		_atx.csx = ((_atx.ncs - 1) % _ATX_MAXCS) + 1;
		atxcom->top[_atx.csx-1][0] = atxcom->bp;
		atxcom->bot[_atx.csx-1][0] = atxcom->ep;
		atxcom->guard[_atx.csx-1] = utcom->guard;
		if(!_atx.sched) {
			/*
			 *  Mark end of CASE structure
			 */
			atxcom->guard[_atx.csx-1] |= 2;
		}
	} else {
		_atx.unitask = 0;
		_atx.ncs = *csn;
		atxcom->guard[((_atx.ncs - 1) % _ATX_MAXCS)] = utcom->guard;
	}
	if(_atx.ncs < 0) {
		printf("Fatal ATEXPERT Error--# parallel loops = %i  minimum is 1\n",
			_atx.ncs);
		atx_abt(1);
	}
	/*
	 *  Now we check for disconnects during this last parallel region.
	 */
	Td = (atxcom->ep - atxcom->bp) - 
	     (process_time * _atx.clkrate - 100 - _atx.itsstart);
	if(Td < 1000)
		Td = 0;
	else {
		if((init_time - atxcom->ep) > Td)
			Td = 0;
		else if(_atx.unitask == 1) {
			if((atxcom->ep - atxcom->bp - 1000) > Td) 
				atxcom->bcs[_atx.csx-1][0] = -Td;
			else
				Td = 0;
		} else {
			++_atx.ndisconp;
			_atx.tdisconp += Td;
		}
	}
	/*
	 *  Store away the current atx@com.
	 */
	i = -_atx.ncs;
#ifdef _ATXCRAY
	ATX_CP(&i);
#else
	atx_cp_(&i);
#endif
	/*
	 *  Load in the first set of values and store into atxcom.
	 */
	atx_cs(0,bcs,top,bot,itc,wsw,
		&iter,&donm,&type,&savl,&guard,&parm);
	atxcom->itr[0] = iter;
	atxcom->do_name[0] = donm;
	atxcom->type[0] = type;
	atxcom->savl[0] = savl;
	atxcom->guard[0] = guard;
	atxcom->parm[0] = parm;
	for(i = 0; i < atxcom->cpu; i++) {
		atxcom->bcs[0][i] = bcs[i];
		atxcom->top[0][i] = top[i];
		atxcom->bot[0][i] = bot[i];
		atxcom->itc[0][i] = itc[i];
/*
		atxcom->wt[0][i] = wsw[i];
*/
	}
	pr = _atx.current;		/* Get current pr structure */
	if(_atx.traceon) {
		int	minics;
		int	maxics;
		int	ntopbot;
		int	itype,ntype;
		int	n,nc;
		/*
		 *  Put out traces here
		 */
		printf("%10d  1  BP   %s\n",_atx.outerpst,pr->slv_name);
		mintim = atxcom->bp;
		if(_atx.unitask) {
			lastbot0 = bot[0];
			bot[0] = atxcom->top[0][0] - _atx.bpmin * 0.1;
			for(ics = 0; ics < _atx.ncs; ics++) {
				atx_cs(ics,bcs,top,bot,itc,wsw,
			       		&iter,&donm,&type,&savl,&guard,&parm);
				if(ics == _atx.ncs - 1)
					bot[0] = atxcom->ep;
				if(top[0] - lastbot0 < 0)
					printf("**********  1  TOP  %d\n",
						type+(ics+1)*10);
				else
					printf("%10d  1  TOP  %d\n",top[0] - lastbot0,
						type+(ics+1)*10);
				printf("%10d  1  BOT  %d\n",bot[0]-top[0],iter);
			}
			mintim = bot[0];
		} else if(_atx.ncs > 0) {
			mintim = atxcom->bp;
			mint = mintim;
			minics = 1;
			maxics = 1;
DOAGAIN:
			n = 0;
			itype = 0;
			ntopbot = 0;
			ics = minics;
			while(ics <= maxics && itype <= 1) {
				atx_cs(ics-1,bcs,top,bot,itc,wsw,
			       		&iter,&donm,&type,&savl,&guard,&parm);
				for(i = 0; i < atxcom->cpu; i++) {
					if(bcs[i] > mintim) {
						if(!n || bcs[i] < mint) {
							n = i+1;
							nc = ics;
							mint = bcs[i];
							ntype = type;
							itype = 1;
						} else if(bcs[i] == mint) {
							bcs[i] += 1;
						}
					}
					if(top[i] > mintim) {
						++ntopbot;
						if(!n || top[i] < mint) {
							n = i+1;
							nc = ics;
							mint = top[i];
							ntype = type;
							niter = itc[i];
							itype = 2;
						} else if(top[i] == mint) {
							++top[i];
						}
					}
					if(bot[i] > mintim) {
						++ntopbot;
						if(!n || bot[i] < mint) {
							n = i+1;
							nc = ics;
							mint = bot[i];
							ntype = type;
							niter = iter;
							itype = 3;
						} else if(bot[i] == mint) {
							++bot[i];
						}
					}
				}
				if(maxics > minics && ics == minics && !n)
					++minics;
				++ics;
			}
			if(n) {
				if(itype == 1) {
					printf("%10d  %d  BCS  %d\n",mint-mintim,
						n, ntype+nc*10);
				} else if(itype == 2) {
					if(mint - mintim < 0)
						printf("**********  %d  TOP  %d\n",
							n,niter);
					else
						printf("%10d  %d  TOP  %d\n", mint-mintim,
							n,niter);
				} else if(itype == 3) {
					printf("%10d  %d  BOT  %d\n",mint-mintim,
						n,niter);
				}
				if(ntopbot <= 1) {
					++maxics;
					if(maxics > _atx.ncs)
						maxics = _atx.ncs;
				}
				mintim = mint;
				goto DOAGAIN;
			}
		}
		printf("%10d  1  EP   \n",atxcom->ep - mintim);
	}		/* End of trace code */
	/*
	 *  Allocate and initialize Loop structures if
	 *  needed.
	 */
	loop = pr->loops;
	if(!loop) {
		/*
		 *  Loop structures haven't been allocated yet
		 *  for this parallel region.  Do so now.
		 */
		donm1 = -1;
		for(ics = 0; ics < _atx.ncs; ics++) {
			atx_cs(ics,bcs,top,bot,itc,wsw,
			       &iter,&donm,&type,&savl,&guard,&parm);
			if(type == _ATX_VECTOR) 
				parm = _atx.maxvl;
			if(!ics) {
				/* 
				 *  First pass only 
				 */
				pr->loops = atx_mklp();
				donm1 = donm;
				loop = pr->loops;
			} else {
				/*
				 *  All other passes
				 */
				if(donm == donm1)
				/*	
				 *  If this is the same as the
				 *  first do loop name, then we
				 *  are in a loop.  Break out if
				 *  it now.
				 */
					break;
				lwalker = pr->loops;
				while(lwalker != 0 && lwalker->do_loop != donm)
					lwalker = lwalker->next;
				/*
				 *  If this do loop name matches one we've
				 *  already seen, don't remake the loop.
				 *  Just continue.
				 */
				if(lwalker)
					continue;
				loop->next = atx_mklp();
				loop = loop->next;
			}
			loop->do_loop = donm;
			loop->type = type;
			loop->parm = parm;
			if(savl == 1)
				loop->flags |= _ATX_SAVELAST;
			if(guard & 1)
				loop->flags |= _ATX_GUARDED;
			if(guard & 2)
				loop->flags |= _ATX_ENDCASE;
		}
		lwalker = pr->loops;
		while(lwalker) {
			++pr->num_loops;
			lwalker = lwalker->next;
		}
	}
	/*
	 *  Here we check for the possiblity that we entered a
	 *  parallel region but made zero trips through the
	 *  loops.  In this case, we abort the timing process.
	 */
	if(!_atx.ncs)
		goto END_TIMING;
	if(_atx.unitask == 1) {
	/*
	 *  Handle all unitasked timings here
	 */
		++_atx.nunitask;
		/*
		 *  Now we loop through all stored control structures
		 */
		for(ics = 0; ics < _atx.ncs; ics++) {
			atx_cs(ics,bcs,top,bot,itc,wsw,
			       &iter,&donm,&type,&savl,&guard,&parm);
			if(type == _ATX_VECTOR)
				parm = _atx.maxvl;
			/*
			 *  Find a loop structure that corrisponds to
			 *  to this control structure.  If one doesn't
			 *  exist yet, make a new one.
			 */
			loop = lwalker = pr->loops;
			while(loop != 0 && loop->do_loop != donm) {
				lwalker = loop;
				loop = loop->next;
			}
			if(!loop) {
				/*
				 *  Loop doesn't exist yet.
				 *  Make one now.
				 */
				lwalker->next = atx_mklp();
				loop = lwalker->next;
				loop->do_loop = donm;
				loop->type = type;
				loop->parm = parm;
				if(savl == 1)
					loop->flags |= _ATX_SAVELAST;
				if(guard & 1)
					loop->flags |= _ATX_GUARDED;
				if(guard & 2)
					loop->flags |= _ATX_ENDCASE;
			}
			if(type == _ATX_CASE) {
				iter = niter = 1;
			} else if(type == _ATX_GUIDED || type == _ATX_VECTOR) 
				niter = (iter + parm - 1)/parm;
			else
				niter = iter;
			if(iter > 0) {
				/*
				 *  If bcs[0] is less than zero,
				 *  this signals that a disconnect
				 *  occured.  The disconnect time
				 *  is -bcs[0].
				 *  Store this away in our disconnect
				 *  counts.
				 */
				if(bcs[0] < 0) {
					top[0] -= bcs[0];
					++_atx.ndisconp;
					_atx.tdisconp -= bcs[0];
				}
				/*
				 *  Calculate the time per iteration 
				 */
				time_per_iter = (((bot[0] - top[0]) / niter) + 0.5);
				++loop->u_iter_time.count;
				loop->u_iter_time.total += time_per_iter;
				if(time_per_iter < loop->u_iter_time.min)
					loop->u_iter_time.min = time_per_iter;
				if(time_per_iter > loop->u_iter_time.max)
					loop->u_iter_time.max = time_per_iter;
				if(loop->u_iter_sample.max < iter) {
					/*
					 *  This is the largest sample
					 *  seem thus far.  This is probably
					 *  the most accurate timing wise.
					 */
					loop->u_min_time_max_iter = time_per_iter;
					loop->c_iter_overhead.max = _ATX_MXTIME;
					if(loop->u_iter_sample.max < 3) {
						loop->u_iter_time.min = time_per_iter;
						loop->u_iter_time.max = time_per_iter;
					}
				} else if(loop->u_iter_sample.max == iter) {
					if(time_per_iter < loop->u_min_time_max_iter)
						loop->u_min_time_max_iter = time_per_iter;
				}
				++loop->u_iter_sample.count;
				loop->u_iter_sample.total += iter;
				if(iter < loop->u_iter_sample.min)
					loop->u_iter_sample.min = iter;
				if(iter > loop->u_iter_sample.max)
					loop->u_iter_sample.max = iter;
				if(wsw[0]) {
					Tw = _atx.bpmin * 0.3;
					++loop->wait_time.count;
					loop->wait_time.total += Tw;
					if(Tw < loop->wait_time.min)
						loop->wait_time.min = Tw;
					if(Tw > loop->wait_time.max)
						loop->wait_time.max = Tw;
				}
			} else 
				++loop->u_iter_time.count;
		}	/* End for loop */
		goto END_TIMING;
	}
	/*
	 *  Handle multiprocessing here.
	 */
/*
	dump_common(atxcom);
*/
	if(Td > 0 && Td < (atxcom->bcs[0][0] - atxcom->bp - _atx.bpmin)) {
		/*
		 *  See if we can factor out the Td time.
		 */
		minbcs = atxcom->bcs[0][0];
		for(i = 1; i < atxcom->cpu; i++) {
			if(atxcom->bcs[0][i] && (atxcom->bcs[0][i] < minbcs))
				minbcs = atxcom->bcs[0][i];
		}
		if(minbcs > atxcom->bcs[0][0]) {
			atxcom->bp += Td;
			Td = 0;
		}
	}
	/*
	 *  Tally begin parallel (bp) times
	 */
	bp_time = atxcom->bcs[0][0] - atxcom->bp;
	if(bp_time < pr->bpt.min)
		pr->bpt.min = bp_time;
	if(bp_time > pr->bpt.max)
		pr->bpt.max = bp_time;
	if(!Td || bp_time < 1000) {
		++pr->bpt.count;
		pr->bpt.total += bp_time;
		/*
		 *  Reset _atx.bpmin if the measured begin parallel
		 *  time is less then what we have already stored
		 *  as our minimum time.
		 */
		if(bp_time < _atx.bpmin) 
			_atx.bpmin = bp_time;
	}
	/*
	 *  Find 1rst slave arrival time and count
	 *  the number of entering cpus (mxcpu)
	 */
/*
	min_arrv = init_time + 1;
*/
	min_arrv = ATX_IRTC();
	mxcpu = 1;
	for(i = 1; i < atxcom->cpu; i++) {
		if(atxcom->bcs[0][i]) {
			if(atxcom->bcs[0][i] < min_arrv) 
				min_arrv = atxcom->bcs[0][i];
			mxcpu = i + 1;
		}
	}
	ncwrk = mxcpu;
	_atx.nworks += mxcpu * _atx.ncs;
	_atx.ncstot += _atx.ncs;
	pr->work_cpus.value += mxcpu;
	loop = pr->loops;
	/*
	 *  If we have suffered a disconnect and already
	 *  have a valid result, then we ignore this
	 *  sample.
	 */
	if(Td) {
		if(loop->flags & _ATX_VALID) {
			if(loop->Total_time[mxcpu-1].count > 2) {
				/*
				 *  We already have at least 3 samples.
				 *  Skip this one.
				 */
				++pr->work_cpus.count;
				goto END_TIMING;
			}
		}
		valid = 0;
	} else {
		valid = 1;
	}
	/*
	 *  Tally slave arrival time if at least 1 slave
	 *  participated.
	 */
	if(mxcpu > 1) {
		if(mxcpu > _atx.maxcpu)
			_atx.maxcpu = mxcpu;
		i = min_arrv - atxcom->bp;
		if(i > _ATX_MXTIME) i = _ATX_MXTIME;
		if(_atx.bpmin > i)
			slave = _atx.bpmin;
		else
			slave = i;
		if(valid || slave < (100 * _atx.bpmin)) {
			if(slave < pr->sat.min)
				pr->sat.min = slave;
			if(slave > pr->sat.max)
				pr->sat.max = slave;
			++pr->sat.count;
			pr->sat.total += slave;
		}
	} else {
		slave = _ATX_MXTIME;
	}
	/*
	 *  Loop through and tally information about each
	 *  parallel loop (after doing some initialization).
	 */
	Tdloops = Tcitr = maxbot = 0;
	maxbots = mincasebc = atxcom->ep;
	type = -1;
	bot[0] = atxcom->bp;
	nxttally = 1;
	for(ics = 0; ics < _atx.ncs; ics++) {
		lasttype = type;
		lastbot0 = bot[0];
		atx_cs(ics,bcs,top,bot,itc,wsw,
		       &iter,&donm,&type,&savl,&guard,&parm);
		if(type == _ATX_VECTOR)
			parm = _atx.maxvl;
		/*
		 *  Find a loop structure that corrisponds to
		 *  to this control structure.  If one doesn't
		 *  exist yet, make a new one.
		 *  If "donm" = 0, this means that we have
		 *  been dealing with a CASE parallel structure.
		 *  In this case, the loops are in order and we
	 	 *  just set the loop to the next one in the
		 *  sequence.
		 */
		if(!donm) {
			if(!ics)
				loop = pr->loops;
			else
				loop = loop->next;
		} else {
			loop = lwalker = pr->loops;
			while(loop != 0 && (loop->do_loop != donm)) {
				lwalker = loop;
				loop = loop->next;
			}
		}
		if(!loop) {
			/*
			 *  Loop doesn't exist yet.
			 *  Make one now.
			 */
			lwalker->next = atx_mklp();
			loop = lwalker->next;
			loop->do_loop = donm;
			loop->type = type;
			loop->parm = parm;
			if(savl == 1)
				loop->flags |= _ATX_SAVELAST;
			if(guard & 1)
				loop->flags |= _ATX_GUARDED;
			if(guard & 2)
				loop->flags |= _ATX_ENDCASE;
		}
		/*
		 *  Check for inter-loop disconnect
		 */
		if(Td && (type != _ATX_CASE) && (lasttype != _ATX_CASE)) {
			if(bcs[0] && ((bcs[0] - maxbots) > (10 * _atx.bpmin))) {
				if((Td - (bcs[0] - lastbot0)) > 0)
					Td -= (bcs[0] - lastbot0);
				else
					Td = 0;
				nxttally = 0;
			}
		}
		/*
		 *  Set flag for inter-loop timing tallies
		 */
		iltally = nxttally;
		if(ics && ((ics % _ATX_MAXCS) == 0)) 
			iltally = 0;
		nxttally = 1;
		maxbotsv = maxbots;
		maxbots = 0;
		if(type == _ATX_CASE) {
	/*
	 *  Handle parallel CASE constructs here 
	 */
			/*
			 *  Attempt to factor out disconnect
			 *  time if possible.
			 */
			if((Td > 1000) && top[0]) {
				if(top[0] - bcs[0] > Td) 
					Td = 0;
			}
			/*
			 *  Summarize CASE loop here
			 */
			ncwrkd = mintops = maxtops = ncpu = 0;
			minbcs = maxbotsv;
			for(i = 0; i < mxcpu; i++) {
				/*
				 *  Note: It is possible for a cpu to
				 *   	  skip past all CASEs--abort
				 *	  the timing analysis in this
				 *	  case (bcs[i] = 0).
				 */
				if(!bcs[i]) 
					goto END_TIMING;
				if(top[i]) {
					ncpu = i + 1;
					if(top[i] > maxbots)
						maxbots = top[i];
				} else {
					if(bcs[i] > maxbots)
						maxbots = bcs[i];
				}
				if(lasttype != _ATX_CASE) {
					if(bcs[i] < mincasebc)
						mincasebc = bcs[i];
					if(i > 1) {
						if(!mintops)
							mintops = bcs[i];
						else {
							maxtops = bcs[i];
							++ncwrkd;
						}
					}
				}
			}
			if(!ncpu) 
				goto END_TIMING;
			ncwrk = iter = niterc = 1;
			bcstot = top[ncpu-1] - mincasebc;
			minbot = maxbot = 0;
			mint = top[ncpu-1];
			if(ics == (_atx.ncs - 1))	/* Last pass */
				maxt = atxcom->ep;
			else {
				atx_cs(ics+1,bcs,top,bot,itc,wsw,
		       		       &i,&donm,&type,&savl,&guard,&parm);
				if(type == _ATX_VECTOR)
					parm = _atx.maxvl;
				if(bcs[ncpu-1])
					maxt = bcs[ncpu-1];
				else
					maxt = atxcom->ep;
			}
			if(loop->Total_time[0].count > 2) {
				cflag = 0;
				for(i = 0; i < nconnect; i++) {
					if(reconnect[i] > mint &&
					   reconnect[i] < maxt)
						cflag = 1;
				}
				if(cflag)
					continue;
			}
			Tctot = maxt - mint;
			maxt = Tctot;
			mint = 0;
		} else {
	/*
	 *  Handle all other parallel constructs
	 *  here (non CASE).
	 */
			/*
			 *  Analyze for disconnects and/or late
			 *  reconnects.  Gather min/max arrival
			 *  times and work time/iteration.
			 */
/*
			minTcitr = init_time + 1;
*/
			minTcitr = ATX_IRTC();
			minbcs = atxcom->ep;
			maxbcs = maxbot = ncwrk = 0;
			for(i = 0; i < mxcpu; i++) {
				if(itc[i]) {
					if(bcs[i] < minbcs) minbcs = bcs[i];
					if(bcs[i] > maxbcs) maxbcs = bcs[i];
					++ncwrk;
					if(bot[i] > maxbot) maxbot = bot[i];
					Tcitr = (bot[i] - top[i] + itc[i] - 1) /
						itc[i];
					if(Tcitr < minTcitr) minTcitr = Tcitr;
				}
			}
			/*
			 *  If master is only working cpu on this loop,
			 *  test it for disconnect.  After that, we skip
			 *  the code that deals with disconnects in
			 *  multiple cpus.
			 */
			if((ncwrk == 1) && itc[0] && Td) {
				mintim = 1.25 * iter;
				if(mintim < (bot[0] - top[0] - Td)) {
					/*
					 *  This loop has a disconnect.
					 */
					++Tdloops;
					if((ics == _atx.ncs) && (Tdloops == 1)) {
						if(ics == 1) atxcom->bp += Td;
						bcs[0] += Td;
						top[0] += Td;
						Td = 0;
					} else if(loop->u_iter_time.count > 1) {
						if(loop->type == _ATX_GUIDED ||
						   loop->type == _ATX_VECTOR) {
							niter = (iter+parm-1)/parm;
						} else {
							niter = iter;
						}
						time_per_iter=loop->u_iter_time.min;
						j=((bot[0]-top[0]-Td)/niter+0.5) -
						   time_per_iter;
						if(j < 0) j = -j;
						if(10 * j < time_per_iter) {
							if(ics == 1)
								atxcom->bp += Td;
							bcs[0] += Td;
							top[0] += Td;
							Td = 0;
						}
					} else if(Tdloops == 1) {
						if(2 * _atx.bpmin < mintim)
							j = 2 * _atx.bpmin;
						else
							j = mintim;
						if((atxcom->ep - bot[0] - j) *
						   (_atx.ncs - ics) < Td) {
							if(ics == 1)
								atxcom->bp += Td;
							bcs[0] += Td;
							top[0] += Td;
							Td = 0;
						}
					}
					if(!Td) nxttally = 0;
				}
				/*
				 *  Since only the master was working,
				 *  we can skip looking for arrival times
				 *  and other such things.
				 */
				goto END_REDIST;
			} else if(ncwrk == 1) {
				goto END_REDIST;
			} 
			/*
			 *  More than the master cpu did work.
			 *  The code between here and label END_REDIST
			 *  deals with disconnects in multiple cpu
			 *  runs.
			 */
			if((2 * _atx.bpmin * mxcpu) > minTcitr) 
				maxt = 2 * _atx.bpmin * mxcpu + minbcs;
			else
				maxt = minTcitr + minbcs;
			/*
			 *  Skip loop types with non-uniform time
			 *  per concurrent iteration.
			 */
			if(type == _ATX_GUIDED || type == _ATX_VECTOR ||
			   loop->wait_time.max != 0) {
				if(maxbcs >= maxt) iltally = 0;
				/*
				 *  Skip this loop
				 */
				goto END_REDIST;
			}
	/*
	 *  Remove cpus that were disconnected/reconnected.
	 *  Tally their iterations.  Calculate average work/iterations
	 *  in remaining cpus.
	 */
			idiscon = 0;
			maxbcs = maxt;
			/*
			 *  Tally number of cpus which were reconnected
			 *  during top - bottom interval.
			 */
			for(i = 0; i < nconnect; i++) {
				if(reconnect[i] > maxbcs &&
				   reconnect[i] < maxbot) 
					++idiscon;
			}
			/*
			 *  If none were reconnected, we can skip past
			 *  a lot of code.
			 */
			if(!idiscon)
				goto END_REDIST;
			/*
			 *  Now we check to see if all cpus were
			 *  disconnected.  If so, we assume that
			 *  there was large granularity work in
			 *  which case, disconnect time has a minimal
			 *  effect.  We remove 1 cpu from the batch
			 *  to compensate.
			 */
			if(idiscon == ncwrk) {
				if(loop->Total_time[ncwrk-2].count > 2) {
					/*
					 *  We already have 3 samples.
					 *  Skip this.
					 */
					maxbots = maxbot;
					continue;
				}
				/*
				 *  Setting idiscon to 1 will result
				 *  in a single cpu being removed.
				 */
				idiscon = 1;
			}
			/*
			 *  Now we start eliminating cpus.  The
			 *  first ones we get rid of are those
			 *  cpus that started iterations after
			 *  maxbcs (max begin control structure).
			 */
			nditers = 0;
			ncwrk = mxcpu;
			for(i = 0; i < mxcpu; i++) {
				if(itc[i]) {
					if(bcs[i] > maxbcs) {
						/*
						 *  We are going to eliminate this
						 *  cpu, right after we store away
						 *  its iteration count.
						 */
						nditers += itc[i];
						top[i] = 0;
						bot[i] = 0;
						itc[i] = 0;
						--idiscon;
						--ncwrk;
					}
				} else 
					--ncwrk;
			}
			/*
			 *  Now we eliminate the rest by selecting the
			 *  cpus with the largest times/iteration.
			 */
			while(idiscon > 0) {
				j = 0;
				maxTcitr = 0;
				for(i = 0; i < _atx.maxcpu; i++) {
					if(itc[i]) {
						Tcitr=(bot[i] - top[i] + itc[i]-1) /
							itc[i];
						if(maxTcitr < Tcitr) {
							maxTcitr = Tcitr;
							j = i;
						}
					}
				}
				/*
				 *  Set nditers to the total number
				 *  of iterations that will later be
				 *  distributed to remaining cpus.
				 */
				nditers += itc[j];
				top[j] = bot[j] = itc[j] = 0;
				--ncwrk;
				--idiscon;
			}
			/*
			 *  Recompute the average time per iteration
			 *  using only the connected cpus.
			 */
			tottc = totws = num_iters = 0;
			for(i = 0; i < mxcpu; i++) {
				if(itc[i]) {
					Tcitr = (bot[i]-top[i]+itc[i]-1)/itc[i];
					tottc += bot[i] - top[i];
					num_iters += itc[i];
					ncpu = i;
				}
			}
			if(!ncwrk) {
				++pr->work_cpus.count;
				goto END_TIMING;
			}
			if(!num_iters) goto END_REDIST;
			avgTcitr = (tottc + num_iters - 1) / num_iters;
			avgws = (totws + num_iters - 1) / num_iters;
			/*
			 *  Redistribute work to cpus that were connected
			 *  the whole time.  Assume uniform concurrent
			 *  iteration size.  Attempt to evenly divide the
			 *  iterations accross the cpus.
			 */
			num_iters = nditers / ncwrk;
			if(num_iters) {
				inctime = num_iters * avgTcitr;
				incws = num_iters * avgws;
				/*
				 *  Set nditers to number of remaining
				 *  iterations that didn't divide evenly
				 *  into the number of cpus.
				 */
				nditers -= num_iters * ncwrk;
				for(i = 0; i < mxcpu; i++) {
					if(top[i]) {
						itc[i] += num_iters;
						wsw[i] += incws;
						bot[i] += inctime;
						if(bot[i] > maxbot) {
							maxbot = bot[i];
							if(ics == _atx.ncs)
								atxcom->ep += avgTcitr;
						}
					}
				}
			}
			/*
			 *  Handle any remaining iterations here.  We
			 *  send iterations to those cpus with minimum
			 *  bottom times.
			 */
			for(j = 0; j < nditers; j++) {
				for(i = 0; i < mxcpu; i++) {
					if(top[i]) {
						if(bot[i] < bot[ncpu])
							ncpu = i;
					}
				}
				itc[ncpu] += 1;
				wsw[ncpu] += avgws;
				bot[ncpu] += avgTcitr;
				if(bot[i] > maxbot) {
					maxbot = bot[i];
					if(ics == _atx.ncs)
						atxcom->ep += avgTcitr;
				}
			}
			/*
			 *  Set flag to reflect the redistribution of
			 *  work.  We skip interloop tallies.
			 */
			nxttally = 0;
 END_REDIST:
			/*
			 *  Check the master cpu for a distinguishable
			 *  disconnect before or after the loop
			 */
			if(Td && bcs[0]) {
				if(!top[0]) {
					/*
					 *  Master did not participate in
					 *  the CS.  Adjust Td.
					 */
					if(bcs[0] < minbcs)
						minbcs = bcs[0];
					if(ics == _atx.ncs) 
						Td -= (atxcom->ep - minbcs);
					else
						Td -= (maxbot - minbcs);
					if(Td < 0) Td = 0;
				} else {
					/*
					 *  Check master for distiguishable
					 *  preceeding gap.
					 */
					if((top[0]-bcs[0]) > (10*_atx.bpmin)) {
						i = Td - (top[0]-bcs[0]-2*_atx.bpmin);
						if(i > 0)
							Td = i;
						else
							Td = 0;
					}
				}
			}
			/*
			 *  Initialize loop summarization variables
			 */
			mincasebc = mint = minbot = atxcom->ep;
			if(!ics)
				mint = atxcom->bp;
			niterc = bcstot = maxt = Tctot = totws =
			ncwrk  = ncwrkd = mintops = maxtops = maxbot = 0;
/*
			minbcs = init_time + 1;
*/
			minbcs = ATX_IRTC();
			/*
			 *  Summarize the parallel loop
			 */
			for(i = 0; i < mxcpu; i++) {
				if(bcs[i]) {
					if(bcs[i] < mint) mint = bcs[i];
					if(bcs[i] < minbcs) minbcs = bcs[i];
					if(top[i]) {
						niterc += itc[i];
						++ncwrk;
						bcstot += (top[i] - bcs[i]);
						Tctot += (bot[i] - top[i]);
						if(bot[i] > maxt) maxt = bot[i];
						totws += wsw[i];
						if(!mintops) {
							if(i || ics)
								mintops = top[i];
						} else {
							if(top[i] < mintops)
								mintops = top[i];
							++ncwrkd;
						}
						if(top[i] > maxtops) maxtops = top[i];
						if(bot[i] < minbot) minbot = bot[i];
						if(bot[i] > maxbot) maxbot = bot[i];
					}
				}
			}
			if(maxbot > maxbots) maxbots = maxbot;
			_atx.noworks += (mxcpu - ncwrk);
		}	/* END PARALLEL CASE/NON-CASE WORK */
		if(!ncwrk || !niterc)
			continue;
		/*
		 *  Determine whether to tally loop overhead
		 *  information.
		 */
		tally = 1;
		/*
		 *  CHECK HERE ON _atx.maxcpu - 1
		 */
		j = loop->Total_time[_atx.maxcpu-1].count;
		if(ncwrk == _atx.maxcpu) {
			if(!j) loop->c_iter_overhead.max = _ATX_MXTIME;
		} else if(j) {
			tally = 0;
		}
		/*
		 *  Increment time for n cpu sample.
		 *  Store away total, min, and max iteration
		 *  times for n cpu sample.
		 */
		if(type == _ATX_GUIDED || type == _ATX_VECTOR)
			niter = (iter + parm - 1) / parm;
		else
			niter = iter;
		Tcitr = (int)((maxt-mint)/niter + 0.5);
		++loop->Total_time[ncwrk-1].count;
		loop->Total_time[ncwrk-1].value += Tcitr;
		if(loop->iter_time[ncwrk-1].min > Tcitr)
			loop->iter_time[ncwrk-1].min = Tcitr;
		if(loop->iter_time[ncwrk-1].max < Tcitr)
			loop->iter_time[ncwrk-1].max = Tcitr;
		/*
		 *  Set loop valid flag if this was a valid sample.
		 */
		if(valid)
			loop->flags |= _ATX_VALID;
		/*
		 *  Estimate iteration overhead per iteration
		 *  with one and max cpus.
		 */
		if((type != _ATX_CASE) && (iter == loop->u_iter_sample.max)) {
			Tu = niter * loop->u_min_time_max_iter;
			if(Tctot - Tu > 0) 
				Tio = Tctot - Tu;
			else
				Tio = 0;
			if(Tio < (10*Tu*ncwrk) || totws > 0) {
				Tio = Tio/niter;
				if(!Tio && (totws > 0))
					Tio = 0.1 * _atx.bpmin;
				if(Tio < totws/niterc)
					Tw = Tio;
				else
					Tw = totws / niterc;
				Tio -= Tw;
				if(Tw > 0.4*_atx.bpmin)
					maxTw = Tw;
				else
					maxTw = 0.4 * _atx.bpmin;
				if(loop->wait_time.min < 0.2*_atx.bpmin) {
					if(Tw < loop->wait_time.min)
						minTw = Tw;
					else
						minTw = loop->wait_time.min;
				} else {
					if(Tw < 0.2 * _atx.bpmin)
						minTw = Tw;
					else
						minTw = 0.2 * _atx.bpmin;
				}
				if(loop->c_iter_overhead.min < Tio)
					mint = loop->c_iter_overhead.min;
				else
					mint = Tio;
				++loop->c_iter_overhead.count;
				loop->c_iter_overhead.total += Tio;
				++loop->wait_time.count;
				loop->wait_time.total += Tw;
				loop->c_iter_overhead.min = mint;
				loop->wait_time.min = minTw;
				if(!j) {
					loop->c_iter_overhead.max = Tio;
					loop->wait_time.max = maxTw;
				} else if(ncwrk == _atx.maxcpu) {
					if(Tio < loop->c_iter_overhead.max)
						loop->c_iter_overhead.max = Tio;
					if(maxTw < loop->wait_time.max)
						loop->wait_time.max = maxTw;
				}
			}
		}
		/*
		 *  Tally number of concurrent iterations
		 */
		++loop->c_iter_sample.count;
		loop->c_iter_sample.total += niterc;
		if(niterc < loop->c_iter_sample.min)
			loop->c_iter_sample.min = niterc;
		if(niterc > loop->c_iter_sample.max)
			loop->c_iter_sample.max = niterc;
		if(!tally || !iltally) {
			continue;
		}
		/*
		 *  Tally inter-loop (IL) time
		 */
		if(ics) {
			Til = minbcs - maxbotsv;
			if(Til > 0) {
				if(Til < loop->inter_loop_time.min)
					loop->inter_loop_time.min = Til;
				if(Til > loop->inter_loop_time.max)
					loop->inter_loop_time.max = Til;
				++loop->inter_loop_time.count;
				loop->inter_loop_time.total += Til;
			}
		}
		/*
		 *  Tally begin control structure (bcs) time
		 */
		Tbc = bcstot / ncwrk;
		if(Tbc > 0) {
			if(Tbc < loop->bcs_time.min)
				loop->bcs_time.min = Tbc;
			if(Tbc > loop->bcs_time.max)
				loop->bcs_time.max = Tbc;
			++loop->bcs_time.count;
			loop->bcs_time.total += Tbc;
		}
		/*
		 *  Tally processor delta (pd) time.  This is the
		 *  time between arriving cpus.
		 */
		if(ncwrkd) {
			Tpd = (maxtops - mintops) / ncwrkd;
			if(Tpd > 0) {
				if(Tpd < loop->proc_delta_time.min)
					loop->proc_delta_time.min = Tpd;
				if(Tpd > loop->proc_delta_time.max)
					loop->proc_delta_time.max = Tpd;
				++loop->proc_delta_time.count;
				loop->proc_delta_time.total += Tpd;
			}
		}
		/*
		 *  Tally loop synchronzation (ls) time
		 */
		if(_atx.ncs != 1) {
			Tls = maxbot - minbot;
			if(Tls > 0) {
				if(Tls < loop->loop_sync_time.min)
					loop->loop_sync_time.min = Tls;
				if(Tls > loop->loop_sync_time.max)
					loop->loop_sync_time.max = Tls;
				++loop->loop_sync_time.count;
				loop->loop_sync_time.total += Tls;
			}
		}
	}	/* End of do loop */
	/*
	 *  Tally time from last loop bottom to end parallel
	 *  and count it as master wait slave time.
	 */
	if(ncwrk > 1) {
		Tws = atxcom->ep - maxbot;
		if((Tws > 0) && (Tws < (2 * slave))) {
			if(Tws < pr->wst.min)
				pr->wst.min = Tws;
			if(Tws > pr->wst.max)
				pr->wst.max = Tws;
			++pr->wst.count;
			pr->wst.total += Tws;
		}
	}
	/*
	 *  Check for ending disconnect
	 */
	if(Td && ((atxcom->ep - maxbots) > (10 * _atx.bpmin))) {
		Td -= (atxcom->ep - bot[0]);
		if(Td < 0) Td = 0;
	}
	if(Td >= 1000) ++pr->work_cpus.count;
 END_TIMING:
	/*
	 *  All done for this pass.
	 *  Now we re-initialize atxcom for
	 *  the next pass.
	 */
	if(_atx.ncs > 1) {
		if(_atx.ncs < _ATX_MAXCS)
			j = _atx.ncs;
		else
			j = _ATX_MAXCS;
	} else
		j = 1;
	for(i = 0; i < _ATX_MAXCS; i++) {
		atxcom->do_name[i] = _ATX_CASE;
		atxcom->type[i] = _ATX_CASE;
		atxcom->savl[i] = 0;
		atxcom->guard[i] = 0;
		atxcom->parm[i] = 1;
		atxcom->itr[i] = 0;
		for(k = 0; k < _ATX_MAXCPUS; k++) {
			atxcom->bcs[i][k] = 0;
			atxcom->top[i][k] = 0;
			atxcom->bot[i][k] = 0;
			atxcom->itc[i][k] = 0;
/*
			atxcom->wt[i][k] = 0;
*/
		}
	}
	atxcom->itc[_ATX_MAXCS-1][_ATX_MAXCPUS-1] = 0;
	atxcom->cpu = 1;
	atxcom->bp = 0;
	atxcom->ep = 0;
	_atx.ncs = 0;
	_atx.csx = 0;
	utcom->param = 1;
	atx_delete_cs();
	_atx.itsstart = ATX_TSECND() * _atx.clkrate + 50 + _atx.ts_ovhd;
	return;
}	/* END OF ATX_EP */


/*
 *  ATX_CP will copy the contents of the atx@com common block
 *  into allocated storage to prevent overflow of the common
 *  block.
 */
#ifdef _ATXCRAY
void	ATX_CP(ics)
#else
void	atx_cp_(ics)
#endif
	int	*ics;		/*  Total number of control structures 
				 *  unless called from ATX_EP.  In that
				 *  case, ics = -ncs.
				 */
{
	struct	UT_COM		*utcom;		/* Pointer to ut@com common block */
	struct	ATX_COM		*atxcom;	/* Pointer to atx@com common block */
	struct	ATX_COM_SAVE	*new;		/* New allocated area */
	struct	ATX_COM_SAVE	*walker;	/* Structure walker */
	struct	ATX_COM_SAVE	*top;		/* Top structure */
	struct	ATX_COM_SAVE	*bottom;	/* Bottom structure */
	struct	ATX_COM_SAVE	*follow;	/* Temp structure */
	int			i,j,k,l;	/* Temp integers */
	int			ign;		/* Dummy argument to UTQGN */
	
#ifdef _ATXCRAY
	GETATXC(&atxcom);		/* Get pointer to atx@com */
	GETUT(&utcom);			/* Get pointer to ut@com */
#else
	getatxc_(&atxcom);		/* Get pointer to atx@com */
	getut_(&utcom);			/* Get pointer to ut@com */
#endif
	j = sizeof(struct ATX_COM);
	if(*ics < 0) 
		_atx.ncs = -*ics;
	else {
		ign = 0;
		ign = _UTQGN(59,ign);
		atxcom->guard[_ATX_MAXCS-1] = utcom->guard;
		if(!atxcom->bcs[_ATX_MAXCS-1][0])
			atxcom->wt[_ATX_MAXCS-1][0] = utcom->wait;
	}
	walker = new = _ATXCOM_TOP;
	while(new) {
		walker = new;
		new = new->next;
	}
	new = (struct ATX_COM_SAVE *)malloc(sizeof(struct ATX_COM_SAVE));
	if(!new) {
		printf("Fatal ATEXPERT Error--Could not allocate pointer space\n");
		atx_abt(1);
	}
	i = (int) new;
	if(i > _atx.maxmem)
		_atx.maxmem = i;
	if(!walker) {
		/*
		 *  This is the first time through.
		 *  Start the list.
		 */
		walker = _ATXCOM_TOP = new;
	} else {
		/*
		 *  Link new structure into list.
		 */
		walker->next = new;
	}
	/*
	 *  Copy over the common block
	 */
	j = sizeof(struct ATX_COM);
	i = (int)memcpy((char *)&new->atxcom,(char *)atxcom,
	       sizeof(struct ATX_COM));
	new->next = 0;
	/*
	 *  If not called from ATX_EP, then reset the atx@com common
	 *  block to its initial values.
	 */
	if(*ics > -1) {
		for(i = 0; i < _ATX_MAXCS; i++) {
			for(j = 0; j < _ATX_MAXCPUS; j++) {
				atxcom->bcs[i][j] = 0;
				atxcom->top[i][j] = 0;
				atxcom->bot[i][j] = 0;
				atxcom->itc[i][j] = 0;
/*
				atxcom->wt[i][j] = 0;
*/
			}
			atxcom->itr[i] = 0;
			atxcom->do_name[i] = 0;
			atxcom->type[i] = 0;
			atxcom->savl[i] = 0;
			atxcom->guard[i] = 0;
			atxcom->parm[i] = 1;
		}
		ign = _UTQEGN(59,ign);
	} else {
		/*
		 *  Here we handle the case of a late cpu arriving
		 *  that arrived AFTER the first set of control
		 *  structures were stored away.  This causes the
		 *  first set to have a zero value in that cpu's position
		 *  for begin control structure times.
		 *  Since this is the last control structure to be stored
		 *  away, we can check for this condition now and correct
		 *  it.
		 */
		top = follow = bottom = _ATXCOM_TOP;
		while(bottom) {
			follow = bottom;
			bottom = bottom->next;
		}
		bottom = follow;
		/*
		 *  Ok...top and bottom structure pointers are set.
		 *  We check for a top bcs time of zero and a
		 *  bottom bcs time of non-zero to spot this condition.
		 */
		for(j = 0; j < _ATX_MAXCS; j++) {
			for(i = 0; i < atxcom->cpu; i++) {
				if(!top->atxcom.bcs[j][i] && 
				   bottom->atxcom.bcs[j][i]) {
					/*
					 *  Now we find the first 
					 *  control structure that 
					 *  has a non-zero value 
					 *  for this cpu in it.
					 */
					walker = top;
					while(walker != bottom) {
						for(k = 0; k < _ATX_MAXCS; k++) {
							if(walker->atxcom.bcs[k][i])
								break;
						}
						if(walker->atxcom.bcs[k][i])
							break;
						walker = walker->next;
					}
					l = 1;
					while(walker != bottom) {
						++l;
						walker = walker->next;
					}
					top->atxcom.bcs[j][i]=bottom->atxcom.bcs[j][i]
							      - (10 * l * _ATX_MAXCS);
				}
			}
		}
	}
	return;
}
/*
 *  atx_cs will return the values from a single control structure.
 */
void	atx_cs(ics,bcs,top,bot,itc,wsw,iter,donm,type,savl,guard,parm)
	int	ics;		/* Control structure to return */
	int	*bcs;		/* Begin control structure times */
	int	*top;		/* Top of loop times */
	int	*bot;		/* Botton of loop times */
	int	*itc;		/* Iteration counts */
	int	*wsw;
	int	*iter;
	int	*donm;		/* Do loop name */
	int	*type;		/* Type of loop */
	int	*savl;
	int	*guard;
	int	*parm;		/* Scheduling paramter */

{
	int	i;				/* Temp integers */
	struct	ATX_COM		*atxcom;	/* Pointer to atx@com common block */
	struct	ATX_COM_SAVE	*walker;	/* Structure walker */
	int			newics;		/* Index within saved control structures */
	int			main_cs;	/* Main control structure */

#ifdef _ATXCRAY
	GETATXC(&atxcom);
#else
	getatxc_(&atxcom);
#endif
	if(ics <= _atx.ncs) {
		main_cs = ics / _ATX_MAXCS;
		newics = ics % _ATX_MAXCS;
		walker = _ATXCOM_TOP;
		for(i = 0; i < main_cs; i++) {
			if(!walker) {
				printf("Internal CS pointer error in ATEXPERT\n");
				atx_abt(1);
			}
			walker = walker->next;
		}
		/*
	 	*  Walker now points to the correct set of control structures.
	 	*  Now we use newics to index into this set.
	 	*/
		*iter = walker->atxcom.itr[newics];
		*donm = walker->atxcom.do_name[newics];
		*type = walker->atxcom.type[newics];
		*savl = walker->atxcom.savl[newics];
		*guard = walker->atxcom.guard[newics];
		*parm = walker->atxcom.parm[newics];
		for(i = 0; i < _ATX_MAXCPUS; i++) {
			bcs[i] = walker->atxcom.bcs[newics][i];
			top[i] = walker->atxcom.top[newics][i];
			bot[i] = walker->atxcom.bot[newics][i];
			itc[i] = walker->atxcom.itc[newics][i];
			wsw[i] = walker->atxcom.wt[newics][i];
		}
	} else {
		/*
		 *  This takes care of the odd case where
		 *  no control structures were ever created
		 *  but ATX_EP is still calling for the first
		 *  one.  We dummy up values in this case.
		 */
		*iter = 0;
		*donm = 0;
		*type = -1;
		*savl = 0;
		*guard = 0;	
		*parm = 1;
		for(i = 0; i < atxcom->cpu; i++) {
			bcs[i] = top[i] = bot[i] = atxcom->ep;
			itc[i] = wsw[i] = 0;
		}
	}
	return;
}

/*
 *  atx_delete_cs will delete all control structures used and
 *  will reset the _ATXCOM_TOP to zero.
 */

void	atx_delete_cs()

{
	struct	ATX_COM_SAVE	*walker;
	struct	ATX_COM_SAVE	*next;

	walker = _ATXCOM_TOP;
	while(walker) {
		next = walker->next;
		free(walker);
		walker = next;
	}
	_ATXCOM_TOP = 0;
}

/*
 *  atx_mklp will malloc and initialize a loop structure
 */
struct	ATX_loop *atx_mklp()

{
	int	i;			/* Loop index */
	struct	ATX_loop	*loop;	/* loop structure */

	loop = (struct ATX_loop *)malloc(sizeof(struct ATX_loop));
	if(!loop) {
		printf("Fatal ATEXPERT Error--Could not allocate pointer space\n");
		atx_abt(1);
	}
	i = (int) loop;
	if(i > _atx.maxmem)
		_atx.maxmem = i;
	for(i = 0; i < _ATX_MAXCPUS; i++) {
		loop->Total_time[i].count = 0;
		loop->Total_time[i].value = 0;
		loop->iter_time[i].max = 0;
		loop->iter_time[i].min = _ATX_MXTIME;
		loop->iter_time[i].total = 0;
	}
	loop->u_min_time_max_iter = _ATX_MXTIME;
	loop->parm = 0;
	loop->do_loop = 0;
	loop->flags = 0;
	loop->type = 0;
	loop->next = 0;
	loop->u_iter_time.min = _ATX_MXTIME;
	loop->u_iter_time.max = 0;
	loop->u_iter_time.count = 0;
	loop->u_iter_time.total = 0;
	loop->u_iter_sample.min = _ATX_MXTIME;
	loop->u_iter_sample.max = 0;
	loop->u_iter_sample.count = 0;
	loop->u_iter_sample.total = 0;
	loop->c_iter_sample.min = _ATX_MXTIME;
	loop->c_iter_sample.max = 0;
	loop->c_iter_sample.count = 0;
	loop->c_iter_sample.total = 0;
	loop->c_iter_overhead.min = _ATX_MXTIME;
	loop->c_iter_overhead.max = 0;
	loop->c_iter_overhead.count = 0;
	loop->c_iter_overhead.total = 0;
	loop->wait_time.min = _ATX_MXTIME;
	loop->wait_time.max = 0;
	loop->wait_time.count = 0;
	loop->wait_time.total = 0;
	loop->loop_sync_time.min = _ATX_MXTIME;
	loop->loop_sync_time.max = 0;
	loop->loop_sync_time.count = 0;
	loop->loop_sync_time.total = 0;
	loop->inter_loop_time.min = _ATX_MXTIME;
	loop->inter_loop_time.max = 0;
	loop->inter_loop_time.count = 0;
	loop->inter_loop_time.total = 0;
	loop->bcs_time.min = _ATX_MXTIME;
	loop->bcs_time.max = 0;
	loop->bcs_time.count = 0;
	loop->bcs_time.total = 0;
	loop->proc_delta_time.min = _ATX_MXTIME;
	loop->proc_delta_time.max = 0;
	loop->proc_delta_time.count = 0;
	loop->proc_delta_time.total = 0;
	return(loop);
}

/*
 *  ATX_SLAVE is the routine registered as the slave
 *  code to be run whenever atexpert forces
 *  unitasking.
 */

#ifdef _ATXCRAY
void ATX_SLAVE(maxmem)
#else
void atx_slave_(maxmem)
#endif
	int	*maxmem;
{
	int	i;
	int	ign;

	ign = 0;
	ign = _UTQGN(59,ign);
	i = (int) &ign;
	if(i > _atx.maxmem)
		_atx.maxmem = i;
	ign = _UTQEGN(59,ign);
}

/*
 *  ATX_UT is called whenever code is entering into
 *  a unitasked section of parallel code.
 */

#ifdef _ATXCRAY
ATX_UT()
#else
atx_ut_()
#endif

{
	int	tatxep;		/* Current process time used */
	long long	irtatxep;	/* Current clock */
	int	Td;		/* Disconnect time */
	struct	UT_COM		*utcom;		/* Pointer to ut@com common block */
	struct	ATX_COM		*atxcom;	/* Pointer to atx@com common block */

	tatxep = ATX_TSECND();
	irtatxep = ATX_IRTC();
#ifdef _ATXCRAY
	GETATXC(&atxcom);		/* Get pointer to atx@com */
	GETUT(&utcom);			/* Get pointer to ut@com */
#else
	getatxc_(&atxcom);		/* Get pointer to atx@com */
	getut_(&utcom);			/* Get pointer to ut@com */
#endif
	if(_atx.atxon < 0)
		return;
	if(_atx.ncs > 0) {
		atxcom->top[_atx.csx - 1][0] = atxcom->bp;
		atxcom->bot[_atx.csx - 1][0] = atxcom->ep;
		Td = (atxcom->ep - atxcom->bp) - (tatxep*_atx.clkrate-100-_atx.itsstart);
		if(Td > 1000) {
			if((irtatxep - atxcom->ep) < Td) {
				if((atxcom->ep-atxcom->bp-1000) > Td) 
					atxcom->bcs[_atx.csx][0] = -Td;
			}
		}
		atxcom->guard[_atx.csx-1] = utcom->guard;
		if(!_atx.sched && (utcom->sched + utcom->icase)) 
			atxcom->guard[_atx.csx] |= 2;
		if(_atx.csx == _ATX_MAXCS) {
#ifdef _ATXCRAY
			ATX_CP(&_atx.ncs);
#else
			atx_cp_(&_atx.ncs);
#endif
			_atx.csx = 0;
		}
	}
	++_atx.csx;
	++_atx.ncs;
	atxcom->itr[_atx.csx-1] = utcom->trips;
	atxcom->do_name[_atx.csx-1] = utcom->label;
	atxcom->type[_atx.csx-1] = utcom->sched;
	atxcom->parm[_atx.csx-1] = utcom->param;
	atxcom->savl[_atx.csx-1] = utcom->savelast;
	atxcom->wt[_atx.csx-1][0] = utcom->wait;
	utcom->guard = utcom->wait = utcom->icase = 0;
	_atx.sched = utcom->sched;
	_atx.itsstart = ATX_TSECND() * _atx.clkrate + 50 + _atx.ts_ovhd;
}


/*
 *  atx_exit is called during the programs exit sequence and
 *  take all the information stored away by ATX_EP and will
 *  print out the atx.raw file.
 */

void	atx_exit()

{

	int	i,j,k,l;		/* Temp integers */
	double	di,dj;			/* Temp doubles */
	double	tatxbp;			/* timeing variable */
	long long	finalpst;		/* Last preceeding serial time */
	long	Td;			/* Time disconnected */
	char	*atxfile;		/* File name to write to */
	FILE	*fid;			/* File descriptor */
	struct	ATX_pr	*pr;		/* Parallel region structure */
	struct	ATX_loop *loop;		/* Loop structure */
	struct	ATX_pst	*pst;		/* Preceeding serial time structure */
	struct	UT_COM	*utcom;		/* Pointer to ut@com common block */
	struct	ATX_COM	*atxcom;	/* Pointer to atx@com common block */
	struct	utsname	utsname;	/* uname structure */
	time_t	dt;			/* Date and time structure */
	char	dtstring[80];		/* Date and time string */
	struct	tm	*tm;		/* Time structure */
	char	guarded[16];		/* Loop guarded string */
	char	savelast[16];		/* Save last loop string */
	char	*slvname;
	char	*subname;
	int	tumin,timin;
	int	numui,numpi;		/* Number of unitask/parallel iterations */
	int	mint,maxt,avgt;		/* min/max/avg times */
	int	nx;

	finalpst = ATX_IRTC();
	tatxbp = ATX_TSECND();
#ifdef _ATXCRAY
	GETATXC(&atxcom);		/* Get pointer to atx@com */
	GETUT(&utcom);			/* Get pointer to ut@com */
#else
	getatxc_(&atxcom);		/* Get pointer to atx@com */
	getut_(&utcom);			/* Get pointer to ut@com */
#endif
	finalpst -= atxcom->pst;
	/*
	 *  Check for disconnects since last parallel
	 *  region ended.  Subtract any disconnect time
	 *  out of final serial time.
	 */
	Td = finalpst - (tatxbp * _atx.clkrate - _atx.itsstart);
	if(Td > 1000) {
		if(_ATX_TOP_PR) {
			if(finalpst > Td)
				finalpst -= Td;
			++_atx.ndiscons;
			_atx.tdiscons += Td;
		}
	}
	/*
	 *  If we had forced unitasking before,
	 *  undo it now.
	 */
	if(_atx.iutqbp == 1)
		_UTQEP();
	/*
	 *  Check to see if the user wants a different
	 *  atxepert file name.
	 */
	atxfile = getenv("ATXFILE");
	if(atxfile == NULL) 
		atxfile = "atx.raw";
	fid = fopen(atxfile,"w");
	if(fid == NULL) {
		printf("Unable to open output trace file for ATExpert \n");
		printf("ATExpert tracing has been aborted \n");
		atx_abt(1);
	}
	l = fprintf(fid,"VS ATEXPERT 8.0 02/15/93 \n");
	/*
	 *  Now we get the date, time, wall clock time, and
	 *  the elasped cpu time for the DT output line.
	 */
	dt = time(0);
	tm = localtime(&dt);
	i = strftime(dtstring,80,"%D %T",tm);
	if(i > 0 && i < 80) 
		dtstring[i] = '\0';
	else
		dtstring[0] = '\0';
	di = ATX_TIMEF() * 0.001;
	dj = ATX_SECOND();
	l = fprintf(fid,"DT %s %.3f %.3f \n",dtstring,di,dj);
	/*
	 *  Now we get the system name, release version,
	 *  and machine name for the SY output line.
	 */
	i = uname(&utsname);
	l = fprintf(fid,"SY %s %s %s \n",utsname.sysname,
		utsname.release,utsname.machine);
	/*
	 *  Now we display the cpu's used and 
	 *  the memory used.
	 */
	if(_atx.ncstot < 1) 
		di = 1.0;
	else
		di = (double)_atx.nworks / _atx.ncstot;
	dj = _atx.maxmem * 1.0e-6;
	l = fprintf(fid,"MX %d %.2f %.2f \n",_atx.maxcpu,di,dj);
	/*
	 *  Now we output the number and average time
	 *  of both serial and parallel disconnects.
	 */
	if(_atx.ndiscons < 1) 
		i = 1;
	else
		i = _atx.ndiscons;
	l = fprintf(fid,"DS %d %d \n",_atx.ndiscons,_atx.tdiscons/i);
	if(_atx.ndisconp < 1)
		i = 1;
	else
		i = _atx.ndisconp;
	l = fprintf(fid,"DP %d %d \n",_atx.ndisconp,_atx.tdisconp/i);
	/*
	 *  Now we spit out the number of parallel regions,
	 *  the number of times run, and the
	 *  the number of times run in unitask mode.
	 */
	l = fprintf(fid,"NP %d %d %d \n",_atx.npr,_atx.nbpx,_atx.nunitask);
	/*
	 *  Now we loop through all parallel regions 
	 */
	tumin = _atx.bpmin / 10;
	timin = _atx.bpmin + tumin;
	pr = _ATX_TOP_PR;
	while(pr) {
		pst = pr->pst;
		nx = 0;
		while (pst) {
			nx += pst->count;
			pst = pst->next;
		}
		/*
		 *  Print out all info needed on the PR line.
		 */
		i = strlen(pr->sub_name);
		subname = malloc(i+1);
		for(j = 0; j < i; j++) {
			if(pr->sub_name[j] == ' ')
				break;
		}
		strncpy(subname,pr->sub_name,j);
		subname[j] = '\0';
		i = strlen(pr->slv_name);
		slvname = malloc(i+1);
		for(j = 0; j < i; j++) {
			if(pr->slv_name[j] == ' ')
				break;
		}
		strncpy(slvname,pr->slv_name,j);
		slvname[j] = '\0';
		l = fprintf(fid,"PR %s %s %d %d %d %d \n",
			subname, slvname,
			nx, pr->num_loops, pr->work_cpus.value,
			pr->work_cpus.count);
		/*
		 *  Print out min, max, and average times
		 *  for each preceeding serial time (pst) structures. 
		 */
		pst = pr->pst;
		while(pst) {
			i = strlen(pst->name);
			slvname = malloc(i+1);
			for(j = 0; j < i; j++) {
				if(pst->name[j] == ' ')
					break;
			}
			strncpy(slvname,pst->name,j);
			slvname[j] = '\0';
			l = fprintf(fid,"PS %d %d %d %s %d \n",
				pst->times.min, pst->times.max,
				pst->times.total / pst->count,
				slvname, pst->count);
			pst = pst->next;
		}
		if(pr->bpt.total) {
			/*
			 *  Print out min, max, and average
			 *  begin parallel times.
			 */
			avgt = pr->bpt.total / pr->bpt.count;
			l = fprintf(fid,"BP %d %d %d \n", pr->bpt.min,
				pr->bpt.max, pr->bpt.total / pr->bpt.count);
		}
		if(pr->sat.total) {
			/*
			 *  Print out min, max, and average
			 *  slave arrival times.
			 */
			l = fprintf(fid,"SA %d %d %d \n", pr->sat.min,
				pr->sat.max, pr->sat.total / pr->sat.count);
		}
		/*
		 *  Now we loop through all "loop" structures. 
		 */
		loop = pr->loops;
		while(loop) {
			numui = loop->u_iter_time.count;
			numpi = 0;
			for(i = 0; i < _atx.maxcpu; i++)
				numpi += loop->Total_time[i].count;
			k = loop->u_iter_sample.max;
			if(!k)
				k = loop->c_iter_sample.max;
			if(loop->type == _ATX_GUIDED) 
				j = (k + loop->parm - 1) / loop->parm;
			else if(loop->type == _ATX_VECTOR)
				j = (k + _atx.maxvl - 1) / _atx.maxvl;
			else
				j = k;
			if(loop->flags & _ATX_GUARDED)
				strcpy(guarded,"GUARDED");
			else
				guarded[0] = '\0';
			if(loop->flags & _ATX_SAVELAST)
				strcpy(savelast,"SAVELAST");
			else
				savelast[0] = '\0';
			if(loop->type == _ATX_GUIDED) {
				l = fprintf(fid,"DO %d GUIDED %d %s %s \n",
					loop->do_loop, loop->parm,
					guarded,savelast);
			} else if(loop->type == _ATX_CASE) {
				if(loop->flags & _ATX_ENDCASE)
					l = fprintf(fid,"DO %d ENDCASE %s %s \n",
						loop->do_loop,
						guarded,savelast);
				else
					l = fprintf(fid,"DO %d CASE %s %s \n",
						loop->do_loop,
						guarded,savelast);
			} else if(loop->type == _ATX_VECTOR) {
				if(loop->flags & _ATX_GUARDED) 
					l = fprintf(fid,"DO %d VECTOR %s \n",
						loop->do_loop, savelast);
				else
					l = fprintf(fid,"DO %d VECRED %s \n",
						loop->do_loop, savelast);
			} else if(guarded[0] == '\0') {
				l = fprintf(fid,"DO %d %s \n",
					loop->do_loop, savelast);
			} else if(savelast[0] == '\0') {
				l = fprintf(fid,"DO %d %s \n",
					loop->do_loop, guarded);
			} else {
				l = fprintf(fid,"DO %d %s %s \n",
					loop->do_loop, guarded,
					savelast);
			}
			/*
			 *  Print iteration times for both unitasked
			 *  and parallel iterations
			 */
			if(loop->type != _ATX_CASE) {
				if(numui > 0) {
					i = loop->u_iter_sample.count;
					if(i < 1) i = 1;
					l = fprintf(fid,"UI %d %d %d \n",
						loop->u_iter_sample.min,
						loop->u_iter_sample.max,
						loop->u_iter_sample.total / i);
				}
				if(numpi > 0) {
					i = loop->c_iter_sample.count;
					if(i < 1) i = 1;
					l = fprintf(fid,"CI %d %d %d \n",
						loop->c_iter_sample.min,
						loop->c_iter_sample.max,
						loop->c_iter_sample.total / i);
				}
			}
			/*
			 *  Print inter-loop times
			 */
			i = loop->inter_loop_time.count;
			if(i > 0)
				l = fprintf(fid,"IL %d %d %d \n",
					loop->inter_loop_time.min,
					loop->inter_loop_time.max,
					loop->inter_loop_time.total / i);
			/*
			 *  Print begin control structure times
			 */
			i = loop->bcs_time.count;
			if(i > 0)
				l = fprintf(fid,"BC %d %d %d \n",
					loop->bcs_time.min,
					loop->bcs_time.max,
					loop->bcs_time.total / i);
			/*
			 *  Print processor delta times
			 */
			i = loop->proc_delta_time.count;
			if(i > 0)
				l = fprintf(fid,"PD %d %d %d \n",
					loop->proc_delta_time.min,
					loop->proc_delta_time.max,
					loop->proc_delta_time.total / i);
				
			/*
			 *  Print iteration overhead times
			 */
			i = loop->c_iter_overhead.count;
			if(i > 0) {
				mint = loop->c_iter_overhead.min;
				maxt = loop->c_iter_overhead.max;
				avgt = loop->c_iter_overhead.total / i;
				if(mint == _ATX_MXTIME) {
					if(maxt == _ATX_MXTIME) 
						mint = maxt = avgt;
					else
						mint = maxt;
				} else if(maxt == _ATX_MXTIME)
					maxt = mint;
				l = fprintf(fid,"IO %d %d %d \n",
					mint,maxt,avgt);
			}
			/*
			 *  Print wait times
			 */
			i = loop->wait_time.count;
			if(i > 0) {
				mint = loop->wait_time.min;
				if(mint) {
					maxt = loop->wait_time.max;
					avgt = loop->wait_time.total / i;
					if(mint == _ATX_MXTIME) {
						if(maxt == _ATX_MXTIME) 
							mint = maxt = avgt;
						else
							mint = maxt;
					} else if(maxt == _ATX_MXTIME)
						maxt = mint;
					l = fprintf(fid,"WT %d %d %d \n",
						mint,maxt,avgt);
				}
			}
			/*
			 *  Print loop sync times
			 */
			i = loop->loop_sync_time.count;
			if(i > 0) 
				l = fprintf(fid,"LS %d %d %d \n",
					loop->loop_sync_time.min,
					loop->loop_sync_time.max,
					loop->loop_sync_time.total / i);
			/*
			 *  Check to see if this is the last loop
			 *  pass.  If so, calculate min average wait
			 *  slave time in k.
			 */
			if(loop->next) 
				k = 0;
			else {
				/*
				 *  This is last loop pass
				 */
				k = pr->wst.min;
				if(k == _ATX_MXTIME) 
					k = 0;
				else
					k = (k * nx)/(numui + numpi);
			}
			/*
			 *  Print out unitasked iteration times
			 */
			if(numui) {
				mint = loop->u_iter_time.min * j;
				maxt = loop->u_iter_time.max * j;
				avgt = loop->u_iter_time.total*j / numui;
				if(tumin > mint) mint = tumin;
				if(tumin > maxt) maxt = tumin;
				if(tumin > avgt) avgt = tumin;
				l = fprintf(fid,"TU %d %d %d %d \n",
					numui,mint,maxt,avgt);
			}
			/*
			 *  Print out parallel iteration times
			 */
			for(i = 0; i < _atx.maxcpu; i++) {
				numpi = loop->Total_time[i].count;
				if(numpi) {
					mint = loop->iter_time[i].min * j;
					maxt = loop->iter_time[i].max * j;
					avgt = loop->Total_time[i].value * j / numpi;
					if(timin > mint) mint = timin;
					if(timin > maxt) maxt = timin;
					if(timin > avgt) avgt = timin;
					mint += k;
					maxt += k;
					avgt += k;
					l = fprintf(fid,"T%d %d %d %d %d \n", 
						i+1, numpi, mint, maxt, avgt);
				}
			}
			/*
			 *  Get next loop pointer
			 */
			loop = loop->next;
		} 
		/*
		 *  Print out wait slave times for this parallel region
		 */
		if(pr->wst.count) 
			l = fprintf(fid,"WS %d %d %d \n",
				pr->wst.min,
				pr->wst.max,
				pr->wst.total / pr->wst.count);
		/*
		 *  Get next parallel region pointer
		 */
		pr = pr->next;
	} 
	/*
	 *  Print out ending serial time
	 */
/*
	l = fprintf(fid,"ES %d %s \n", finalpst, _atx.current->slv_name);
*/
	i = (long)finalpst;
	l = fprintf(fid,"ES %d %s \n", i, _atx.current->slv_name);
	/*
	 *  Close the output file and exit.
	 */
	fclose(fid);
	return;
}

atx_abt()

{
	printf("Aborting run\n");
	exit(-1);
}

ATX_TRON()

{
	_atx.traceon = 1;

}

ATX_TROFF()

{

	_atx.traceon = 0;

}

dump_common(a)

	struct	ATX_COM	*a;
{
	int	i,j;

	printf("\t\tATX Common Block Dump\n");
	printf("\tmaxcpu = %d\n",a->maxcpu);
	printf("\tcpu = %d\n",a->cpu);
	printf("\tpst = %d\n",a->pst);
	printf("\tbp = %d\n",a->bp);
	printf("\tep = %d\n",a->ep);
	printf("ABOUT TO PRINT BCS\n");
	for(i = 0; i < _ATX_MAXCS; i++) {
		printf("\t\tCONTROL STUCTURE %d\n",i);
		for(j = 0; j < _ATX_MAXCPUS; j++) {
			printf("\t\ta->bcs[%d][%d] = %d\n",i,j,a->bcs[i][j]);
		}
	}
	printf("ABOUT TO PRINT TOP\n");
	for(i = 0; i < _ATX_MAXCS; i++) {
		printf("\t\tCONTROL STUCTURE %d\n",i);
		for(j = 0; j < _ATX_MAXCPUS; j++) {
			printf("\t\ta->top[%d][%d] = %d\n",i,j,a->top[i][j]);
		}
	}
	printf("ABOUT TO PRINT BOT\n");
	for(i = 0; i < _ATX_MAXCS; i++) {
		printf("\t\tCONTROL STUCTURE %d\n",i);
		for(j = 0; j < _ATX_MAXCPUS; j++) {
			printf("\t\ta->bot[%d][%d] = %d\n",i,j,a->bot[i][j]);
		}
	}
	printf("ABOUT TO PRINT ITC\n");
	for(i = 0; i < _ATX_MAXCS; i++) {
		printf("\t\tCONTROL STUCTURE %d\n",i);
		for(j = 0; j < _ATX_MAXCPUS; j++) {
			printf("\t\ta->itc[%d][%d] = %d\n",i,j,a->itc[i][j]);
		}
	}
	printf("ABOUT TO PRINT TYPE\n");
	for(i = 0; i < _ATX_MAXCS; i++) {
		printf("\t\ta->type[%d] = %d\n",i,a->type[i]);
	}
	
}
