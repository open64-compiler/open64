/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdio.h>
#include <sys/time.h>
#ifdef __MINGW32__
#include <time.h>
#else
#include <sys/resource.h>
#endif /* __MINGW32__ */
#include "timelib.h"

#ifdef __cplusplus
extern "C" {
#endif

/* utility routine */

static double get_cpu()
{
#ifdef __MINGW32__
   return clock();
#else
    struct rusage ru;
   double cpu;

   getrusage (RUSAGE_SELF, &ru);
   cpu = (double)ru.ru_utime.tv_sec +
     (double)ru.ru_utime.tv_usec * 1e-6 +
       (double)ru.ru_stime.tv_sec +
	 (double)ru.ru_stime.tv_usec * 1e-6;
   return(cpu);
#endif /* __MINGW32__ */
}

#define MAX_TIMERS 100

static double cumtime[MAX_TIMERS];
static double start_time[MAX_TIMERS];
static int timer_running[MAX_TIMERS];
static int initialized=0;


/* Timer library routines

   start_timer(timer) 
       starts timer number <timer> running. <timer> must be between 1
       and MAX_TIMERS, currently set to 100.

   stop_timer(timer)
       stop the timer and adds its time to the total time for that timer.

   clear_timer(timer)
       sets the time for a timer to 0

   double get_timer_time(timer)
       gets the elapsed time for timer. If the timer is running,
       this does a stop and restart.

*/


static void init_package()
{
   int i;
   if (!initialized) {
      for (i=0; i < MAX_TIMERS; i++) {
	 cumtime[i] = 0.;
	 start_time[i] = 0.;
	 timer_running[i] = 0;
      }
      initialized = 1;
   }
}

void start_timer (int timer)
{
   int timer_index;

   init_package();
   timer_index  = timer-1;
   if (timer_index < 0 || timer_index >=MAX_TIMERS) {
      fprintf(stderr,"### Timer error: %d is not a valid timer value\n",timer);
      return;
   }

   if (timer_running[timer_index]) return;
   timer_running[timer_index] = 1;
   start_time[timer_index] = get_cpu();
   return;
}

void stop_timer (int timer)
{
   int timer_index;

   init_package();
   timer_index  = timer-1;
   if (timer_index < 0 || timer_index >=MAX_TIMERS) {
      fprintf(stderr,"### Timer error: %d is not a valid timer value\n",timer);
      return;
   }

   if (!timer_running[timer_index]) return;
   timer_running[timer_index] = 0;
   cumtime[timer_index] += (get_cpu() - start_time[timer_index]);
   return;
}

void clear_timer(int timer) 
{
   int timer_index;

   init_package();
   timer_index  = timer-1;
   if (timer_index < 0 || timer_index >=MAX_TIMERS) {
      fprintf(stderr,"### Timer error: %d is not a valid timer value\n",timer);
      return;
   }
   timer_running[timer_index] = 0;
   cumtime[timer_index] = 0.0;
}


double get_timer_time (int timer)
{
   int timer_index,running;
   double cputime;
   
   init_package();
   timer_index  = timer-1;
   if (timer_index < 0 || timer_index >=MAX_TIMERS) {
      fprintf(stderr,"### Timer error: %d is not a valid timer value\n",timer);
      return (0.0);
   }
   running = timer_running[timer_index];
   stop_timer(timer);
   cputime = cumtime[timer_index];
   if (running) start_timer(timer);
   return (cputime);
}


#ifdef __cplusplus
}
#endif
