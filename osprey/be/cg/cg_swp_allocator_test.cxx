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


//-*-c++-*-
// =======================================================================
// =======================================================================
//
//  Module: cg_swp_allocator_test.cxx
//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:21-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_swp_allocator_test.cxx $
//
//  Revision comments:
//
//  01-May-1999 - Initial version
//
//  Description:
//  ------------
//  Build the tester as follows to produce an a.out file.
//
//     $TOOLROOT/usr/bin/CC -g -DSTANDALONE_SWP_ALLOCATOR
//       -I../../common/util -I../../common/com -I../../common/stl
//       cg_swp_allocator.cxx cg_swp_allocator_test.cxx
//
//  The tester provides a simple command-line interface to enter
//  lifetimes and test the register allocatator for correctness and
//  optimality.  It outputs time vs register plots of the resultant
//  allocation, and indicates illegal allocations by the '*' character
//  in the plot (i.e. for overlapping register usages).
//
//  An example, taken from Rau's paper, is built into this 
//  implementation, and an interesting supplement to his example
//  can be created by appending the following lifetimes:
//
//    10 15 2 3
//    20 27 1 0
//    30 35 0 2
//    10 20 2 2
//    1 2 0 0
//
//  where using a sort based only on start-time does better than the
//  adjacency sort.  Appending
//
//    6 8 0 0
//
//  will cause adjacency sort to do better.
//
// ====================================================================
// ====================================================================

#include <stdio.h>
#include <vector>
#include "defs.h"
#include "cg_swp_allocator.h"


static const char *
Read_Line()
{
   static char line[512];
   if (gets(line) == NULL)
      line[0] = '\0';
   return line;
}

static INT32
Get_A_Number()
{
   INT32 num, status = 0;

   while (status != 1)
   {
      status = sscanf(Read_Line(), "%d", &num);
      if (status != 1)
	 printf("\n<Try again! Enter a single integer> ");
   }
   return num;
} // Get_A_Number


static INT32
Get_Bounded_Number(INT32 min, INT32 max)
{
  INT32 num = Get_A_Number();
  while (num > max || num < min)
  {
    printf("\nInvalid option! Enter a digit between %d and %d>", min, max);
    num = Get_A_Number();
  }
  return num;
}


static void
Get_A_Lifetime(SWP_LIFETIME &lt)
{
   INT32 from_tunit, to_tunit, omega, alpha, status = 0;

   printf("\n<Enter lifetime (start_cycle end_cycle omega alpha)> ");
   while (status != 4)
   {
      status = sscanf(Read_Line(), "%d %d %d %d", 
		      &from_tunit, &to_tunit, &omega, &alpha);
      if (status != 4)
	 printf("\n<Try again! Enter 4 space separated integers> ");
   }
   lt = SWP_LIFETIME(from_tunit, to_tunit, omega, alpha);
} // Get_A_Lifetime


static void
Get_Lifetimes(std::vector<SWP_LIFETIME> &ltv)
{
   INT32        number_of_values = -1;
   SWP_LIFETIME lt;

   printf("\n<How many lifetimes do you wish to enter> ");
   number_of_values = Get_Bounded_Number(0, 100);

   for (INT32 i = 0; i < number_of_values; i++)
   {
      Get_A_Lifetime(lt);
      ltv.push_back(lt);
   } // for each value
} // Get_Lifetimes


void main()
{
  BOOL  more = TRUE;
  INT32 option;
  std::vector<SWP_LIFETIME> ltv;
  INT32 ii = 2, sc = 10, num_regs = 64, num_its = 15, num_cols = 70;
  
  while (more)
  {
    printf("\n----------- Testing swp allocator -----------");
    printf("\n");
    printf("\n 0)  Exit");
    printf("\n 1)  Set ii (default %d)", ii);
    printf("\n 2)  Set sc (default %d)", sc);
    printf("\n 3)  Set max registers (default %d)", num_regs);
    printf("\n 4)  Set number of iterations to plot  (default %d)", num_its);
    printf("\n 5)  Set column-width of plot (default %d)", num_cols);
    printf("\n 6)  Enter new set of lifetimes");
    printf("\n 7)  Append to existing set of lifetimes");
    printf("\n 8)  Allocate and trace with adjacency ordering");
    printf("\n 9)  Allocate and trace without adjacency ordering");
    printf("\n 10) Example from Rau's paper (page 287)");
    printf("\n");
    printf("\nNOTES on plot:\n");
    printf("\n   \">\" indicates live value carried in/out of loop");
    printf("\n   \"*\" indicates overlapped lifetimes (i.e. an error)");
    printf("\n");
    printf("\n<Enter an option (0-10)> ");

    option = Get_Bounded_Number(0, 10);
    
    switch (option)
    {
    case 0: // Exit
      more = FALSE;
      break;

    case 1: // Set ii
      printf("\nEnter ii (1..1000)> ");
      ii = Get_Bounded_Number(1, 1000);
      break;
    case 2: // Set sc
      printf("\nEnter sc (1..1000)> ");
      sc = Get_Bounded_Number(1, 1000);
      break;
    case 3: // Set max registers
      printf("\nEnter number of registers (1..256)> ");
      num_regs = Get_Bounded_Number(1, 256);
      break;
    case 4: // Set number of iterations to plot
      printf("\nEnter number of iteration (1..256)> ");
      num_its = Get_Bounded_Number(1, 256);
      break;
    case 5: // Set column-width of plot
      printf("\nEnter number of columns (1..256)> ");
      num_cols = Get_Bounded_Number(1, 256);
      break;
    case 6: // Enter new set of lifetimes
      ltv.clear();
      Get_Lifetimes(ltv);
      break;
    case 7: // Add to existing set of lifetimes
      Get_Lifetimes(ltv);
      break;
    case 8: // Allocate and trace
      {
	SWP_ALLOCATOR reg_alloc(ii, sc, num_regs, ltv.begin(), ltv.end(),
				Malloc_Mem_Pool, TRUE);
	reg_alloc.print(stdout, num_its, num_cols);
	break;
      }
    case 9: // Allocate and trace without adjacency ordering
      {
	SWP_ALLOCATOR reg_alloc(ii, sc, num_regs, ltv.begin(), ltv.end(),
				Malloc_Mem_Pool, FALSE);
	reg_alloc.print(stdout, num_its, num_cols);
	break;
      }
    case 10: // Rau's example
      {
	ii = 2;
	sc = 10;
	num_regs = 64;
	ltv.clear();
	ltv.push_back(SWP_LIFETIME(13,17,1,1));
	ltv.push_back(SWP_LIFETIME(18,21,0,0));
	ltv.push_back(SWP_LIFETIME(15,20,0,0));
	ltv.push_back(SWP_LIFETIME(0,20,0,0));
	ltv.push_back(SWP_LIFETIME(0,23,1,0));
	SWP_ALLOCATOR reg_alloc(ii, sc, num_regs, ltv.begin(), ltv.end());
	reg_alloc.print(stdout, num_its, num_cols);
	break;
      }
    } // switch on option
  } // while more
} // main
