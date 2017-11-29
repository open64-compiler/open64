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


#ifndef	eh_region_INCLUDED
#define	eh_region_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * This module encapsulates the generation of exception region tables.
 */

struct EH_RANGE;

extern void EH_Generate_Range_List(WN *);
extern void EH_Set_Start_Label(struct EH_RANGE *);
extern void EH_Set_End_Label(struct EH_RANGE *);
#ifdef __cplusplus
}
#endif /* __cplusplus */



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */
extern void EH_Set_Has_Call(struct EH_RANGE *);
extern void EH_Prune_Range_List(void);
extern void EH_Write_Range_Table(WN *);

extern ST * EH_Get_PU_Range_ST (void);
extern void EH_Print_Range_List(void);
#if defined(TARG_IA64)
extern INITO* EH_Get_PU_Range_INITO(bool);
extern void Print_PU_EH_Entry(PU &, ST *, FILE *);
extern bool PU_Need_Not_Create_LSDA (void);
extern bool pu_need_LSDA;
extern void EH_Dump_INITOs (WN *, FILE *);
extern void EH_Dump_LSDA (FILE *);
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* eh_region_INCLUDED */
