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


#pragma ident "@(#) libu/multi/posix/mactdata.c	92.1	06/25/99 14:35:10"

#include "macdefs.h"
#include <stdio.h>
#include <thread.h>
#include "mactdefs.h"


/*
 * Per-task information.  We start with 10 descriptors, all free.
 */
static mact_task_t      initial_tasks[] =
                        {
                            { 1, TS_Running, 1, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              NULL, 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[2], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[3], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[4], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[5], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[6], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[7], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[8], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              &initial_tasks[9], 0,
                              { 0 }
                            },
                            { 0, TS_Nonexistent, 0, 0, NULL, NULL, NULL, 0,
                              NULL, 0, NULL,
                              NULL,
                              NULL, 0,
                              { 0 }
                            },
                        };

volatile mact_task_t   *_mact_tasks             = &initial_tasks[0];
volatile mact_task_t   *_mact_free              = &initial_tasks[1];
volatile int            _mact_free_sem          = 0;
volatile mact_task_t   *_mact_hash[MTHASH_SIZE] = { NULL,
                                                  &initial_tasks[0],
                                                  NULL,
                                                };
volatile int            _mact_hash_sem          = 0;


/*
 * Total number of macrotask descriptors we have, and the number
 * of free ones.
 */
volatile unsigned int   _mact_total_count
                        =   sizeof(initial_tasks)
                          / sizeof(initial_tasks[0]);
volatile unsigned int   _mact_free_count
                        =   (  sizeof(initial_tasks)
                             / sizeof(initial_tasks[0])
                            )
                          - 1;


/*
 * Total number of runnable macrotasks there are, and a semaphore
 * to protect access.
 */
volatile unsigned int   _mact_runnable_tasks = 1;
volatile int            _mact_runnable_tasks_sem = 0;


/*
 * Macrotask stack sizes, and the associated semaphore.
 */
volatile size_t         _mact_stack_size     = 0;
volatile int            _mact_stack_size_sem = 0;
