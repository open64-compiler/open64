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


static char USMID[] = "@(#) libcif/ciffree.c	30.5	07/26/96 07:19:13";


/*
 * Cif_Free releases all space associated with a CIF record structure.  The
 * structure itself is free'd and any associated space is also released.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdio.h>
#include <string.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */

#include "cif_int.h"

void Cif_Free
#ifdef __STDC__
(struct Cif_generic *cr)
#else
(cr)
struct Cif_generic *cr;
#endif
{

	register int i;

	switch (cr->rectype) {
	case CIF_CALLSITE:
		if (CIFCS(cr)->argids != NULL)
			(void) free ( (char *)CIFCS(cr)->argids );
		break;	

	case CIF_CDIR:
		if (CIFCDIR(cr)->ids != NULL)
			(void) free ( (char *)CIFCDIR(cr)->ids );
		break;	

	case CIF_CDIR_DOSHARED:
		if (CIFCDIRDO(cr)->ids != NULL)
			(void) free ( (char *)CIFCDIRDO(cr)->ids );
		break;

	case CIF_GEOMETRY:
		if (CIFGEOM(cr)->dim != NULL)
			(void) free ( (char *)CIFGEOM(cr)->dim );
		if (CIFGEOM(cr)->name != (char *) NULL)
			(void) free ( (char *)CIFGEOM(cr)->name );
		break;


	case CIF_COMBLK:
		if (_cif_version == 1) { /* must use v1 cif records */
		  	(void) free ( (char *)CIFCB1(cr)->name );
		}
		else { /* use v2 cif records */
		  	(void) free ( (char *)CIFCB(cr)->name );
		}
		break;

	case CIF_CONST:
		(void) free ( (char *)CIFCON(cr)->name );
		(void) free ( (char *)CIFCON(cr)->value );
		if (CIFCON(cr)->origform)
		  (void) free ( (char *)CIFCON(cr)->oform );
		break;

	case CIF_ENTRY:
		if (CIFENTRY(cr)->argids != NULL)
			(void) free ( (char *)CIFENTRY(cr)->argids );
		(void) free ( (char *)CIFENTRY(cr)->name );
		break;

	case CIF_FILE:
		(void) free ( (char *)CIFFILE(cr)->name );
		break;

	case CIF_LABEL:
		(void) free ( (char *)CIFLABEL(cr)->name );
		break;

	case CIF_ORIG_CMD:
		(void) free ( (char *)CIFOCMD(cr)->name );
		break;

	case CIF_MESSAGE:
		if (_cif_version < 3) { /* must use v1 cif records */
		  for (i = 0; i < (int) CIFMSG1(cr)->nargs; i++)
		      (void) free ((char *)CIFMSG1(cr)->args[i] );
		  if (CIFMSG1(cr)->args != NULL)
		      (void) free ((char *)CIFMSG1(cr)->args);
		}
		else {  /* Version 3 CIF */
		  for (i = 0; i < (int) CIFMSG(cr)->nargs; i++)
		      (void) free ((char *)CIFMSG(cr)->args[i] );
		  if (CIFMSG(cr)->args != NULL)
		      (void) free ((char *)CIFMSG(cr)->args);
		  (void) free ( (char *)CIFMSG(cr)->name );
		}
		break;

	case CIF_MISC_OPTS:
		if (_cif_version == 1) { /* must use v1 cif records */

		  if (CIFMO1(cr)->msgno != NULL)
		    (void) free ((char *)CIFMO1(cr)->msgno);
		  for (i = 0; i < (int) CIFMO(cr)->ncdirs; i++)
		    (void) free (CIFMO(cr)->cdirs[i] );
		  if (CIFMO1(cr)->cdirs != NULL)
		    (void) free ((char *)CIFMO1(cr)->cdirs);
		  if (CIFMO1(cr)->objname != NULL)
		    (void) free ((char *)CIFMO1(cr)->objname);
		  if (CIFMO1(cr)->calname != NULL)
		    (void) free ((char *)CIFMO1(cr)->calname);
		  if (CIFMO1(cr)->inname != NULL)
		    (void) free ((char *)CIFMO1(cr)->inname);

		}
		else { /* v2 records */

		  if (CIFMO(cr)->msgno != NULL)
		    (void) free ((char *)CIFMO(cr)->msgno);
		  for (i = 0; i < (int) CIFMO(cr)->ncdirs; i++)
		    (void) free (CIFMO(cr)->cdirs[i] );
		  for (i = 0; i < (int) CIFMO(cr)->numincs; i++)
		    (void) free (CIFMO(cr)->incdirs[i] );
		  if (CIFMO(cr)->cdirs != NULL)
		    (void) free ((char *)CIFMO(cr)->cdirs);
		  if (CIFMO(cr)->objname != NULL)
		    (void) free ((char *)CIFMO(cr)->objname);
		  if (CIFMO(cr)->calname != NULL)
		    (void) free ((char *)CIFMO(cr)->calname);
		  if (CIFMO(cr)->inname != NULL)
		    (void) free ((char *)CIFMO(cr)->inname);
		  if (CIFMO(cr)->lname != NULL)
		    (void) free ((char *)CIFMO(cr)->lname);
		}

		break;

	case CIF_NAMELIST:
		(void) free ( (char *)CIFNL(cr)->name);
		(void) free ((char *)CIFNL(cr)->ids);
		break;

	case CIF_ND_MSG:
		for (i = 0; i < (int) CIFNMSG(cr)->nargs; i++)
			(void) free ((char *)CIFNMSG(cr)->args[i] );
		if (CIFNMSG(cr)->args != NULL)
			(void) free ((char *)CIFNMSG(cr)->args);
		break;

	case CIF_OBJECT:
		if (_cif_version == 1) { /* must use v1 cif records */
		  	if (CIFOBJ1(cr)->name != NULL)
			  	(void) free ( (char *)CIFOBJ1(cr)->name);
			if (CIFOBJ1(cr)->ndims > 0)
			  	(void) free ( (char *)CIFOBJ1(cr)->dim );
		 }
		else { /* use v2 cif records */
		  	if (CIFOBJ(cr)->name != NULL)
			  	(void) free ( (char *)CIFOBJ(cr)->name);
			if (CIFOBJ(cr)->ndims > 0)
			  	(void) free ( (char *)CIFOBJ(cr)->dim );
		}
		break;

	case CIF_UNIT:
		(void) free ( (char *)CIFUNIT(cr)->name);
		break;

	case CIF_ENDUNIT:
		(void) free ( (char *)CIFENDU(cr)->name);
		break;

	case CIF_USAGE:
		if (_cif_version == 1) { /* must use v1 cif records */
		  (void) free ( (char *)CIFUSAGE1(cr)->use);
		}
		else { /* use v2 cif records */
		  (void) free ( (char *)CIFUSAGE(cr)->use);
		  if (CIFUSAGE(cr)->nmembs > 0) {
		    (void) free ( (char *)CIFUSAGE(cr)->membs);
		  }
		}
		break;

	case CIF_FILEDIR:
		for (i = 0; i < (int) CIFFDIR(cr)->nunits; i++)
			(void) free ( (char *)(CIFFDIR(cr)->ut)[i].name );
		(void) free ( (char *)CIFFDIR(cr)->ut);
		break;

	case CIF_UNITDIR:
		(void) free ( (char *)CIFUDIR(cr)->ur);
		break;
	
	case CIF_C_TAG:
		(void) free ((char *)CIFCTAG(cr)->memids);
		(void) free ((char *)CIFCTAG(cr)->name);
		break;

	case CIF_C_OPTS:
		for (i = 0; i < (int) CIFCOPTS(cr)->nincs; i++)
			(void) free ((char *)(CIFCOPTS(cr)->incs)[i]);
		for (i = 0; i < (int) CIFCOPTS(cr)->ndefs; i++)
			(void) free ((char *)(CIFCOPTS(cr)->defs)[i]);
		for (i = 0; i < (int) CIFCOPTS(cr)->nudefs; i++)
			(void) free ((char *)(CIFCOPTS(cr)->udefs)[i]);
		(void) free ((char *)CIFCOPTS(cr)->name);
		(void) free ((char *)CIFCOPTS(cr)->incs);
		(void) free ((char *)CIFCOPTS(cr)->defs);
		(void) free ((char *)CIFCOPTS(cr)->udefs);
		break;

	case CIF_C_MESSAGE:
		if (_cif_version == 1) { /* must use v1 cif records */

			for (i = 0; i < (int) CIFCMSG1(cr)->nargs; i++)
			  	(void) free ((char *)(CIFCMSG1(cr)->args)[i]);
			(void) free ((char *)CIFCMSG1(cr)->args);

		      }
		else {  /* use v2 cif records */

			for (i = 0; i < (int) CIFCMSG(cr)->nargs; i++)
			  	(void) free ((char *)(CIFCMSG(cr)->args)[i]);
			(void) free ((char *)CIFCMSG(cr)->args);

		}

		break;

	case CIF_C_CONST:
		(void) free ((char *)CIFCCON(cr)->value);
		break;

	case CIF_C_ENTRY:

		if (_cif_version == 1) { /* must use v1 cif records */

		  	(void) free ((char *)CIFCENTRY1(cr)->mods);
			(void) free ((char *)CIFCENTRY1(cr)->argids);
			(void) free ((char *)CIFCENTRY1(cr)->name);

		      }
		else {  /* use v2 cif records */

		  	(void) free ((char *)CIFCENTRY(cr)->mods);
			(void) free ((char *)CIFCENTRY(cr)->argids);
			(void) free ((char *)CIFCENTRY(cr)->name);

		}

		break;

	case CIF_C_OBJECT:
		(void) free ((char *)CIFCOBJ(cr)->mods);
		(void) free ((char *)CIFCOBJ(cr)->name);
		break;

	case CIF_C_LINT_DIRECTIVE:
		(void) free ((char *)CIFCLDIR(cr)->name);
		break;

	case CIF_C_MACRO_DEF:
		(void) free ((char *)CIFCMDEF(cr)->name);
		break;

	case CIF_C_ENTRY_END:
		(void) free ((char *)CIFCEEND(cr)->name);
		break;


	case CIF_F90_CALLSITE:
		if (CIFF90CS(cr)->argids != NULL)
		  	(void) free ((char *)CIFF90CS(cr)->argids);
		if (CIFF90CS(cr)->nmembs != NULL)
		  	(void) free ((char *)CIFF90CS(cr)->nmembs);
		if (CIFF90CS(cr)->membs != NULL)
		  	(void) free ((char *)CIFF90CS(cr)->membs);
		if (CIFF90CS(cr)->rank == 1 &&
		    CIFF90CS(cr)->ranks != NULL)
		  	(void) free ((char *)CIFF90CS(cr)->ranks);

		break;

	case CIF_F90_SCOPE_INFO:
		if (CIFF90SI(cr)->entryids != NULL)
		  	(void) free ((char *)CIFF90SI(cr)->entryids);
		break;

	case CIF_F90_COMBLK:
		(void) free ((char *)CIFF90CB(cr)->name);
		break;

	case CIF_F90_CONST:
		(void) free ((char *)CIFF90CON(cr)->value);
		break;

	case CIF_F90_ENTRY:
		if (CIFF90ENTRY(cr)->argids != NULL)
		  	(void) free ((char *)CIFF90ENTRY(cr)->argids);

		(void) free ((char *)CIFF90ENTRY(cr)->name);
		break;

	case CIF_F90_DERIVED_TYPE:
		if (_cif_version == 2) { /* must use v2 cif records */
		  (void) free ((char *)CIFF90DTYPE2(cr)->name);
		  if (CIFF90DTYPE2(cr)->memids != NULL)
		      (void) free ((char *)CIFF90DTYPE2(cr)->memids);
		}
		else { /* Version 3 CIF */
		  (void) free ((char *)CIFF90DTYPE(cr)->name);
		  if (CIFF90DTYPE(cr)->memids != NULL)
		      (void) free ((char *)CIFF90DTYPE(cr)->memids);
		}

		break;

	case CIF_F90_LABEL:
		(void) free ((char *)CIFF90LABEL(cr)->name);
		break;

	case CIF_F90_NAMELIST:
		(void) free ((char *)CIFF90NL(cr)->name);
		if (CIFF90NL(cr)->ids != NULL)
		  (void) free ((char *)CIFF90NL(cr)->ids);
		break;

	case CIF_F90_OBJECT:
		(void) free ((char *)CIFF90OBJ(cr)->name);
		if (CIFF90OBJ(cr)->dim != NULL)
		  (void) free ((char *)CIFF90OBJ(cr)->dim);
		break;


   case CIF_F90_MISC_OPTS:
		if (CIFF90MO(cr)->cifname != NULL)
		  (void) free ((char *)CIFF90MO(cr)->cifname);

		for (i = 0; i < (int) CIFF90MO(cr)->ncdirs; i++)
			(void) free (CIFF90MO(cr)->cdirs[i] );
		if (CIFF90MO(cr)->cdirs != NULL)
			(void) free ((char *)CIFF90MO(cr)->cdirs);

		for (i = 0; i < (int) CIFF90MO(cr)->nPdirs; i++)
			(void) free (CIFF90MO(cr)->Pdirs[i] );
		for (i = 0; i < (int) CIFF90MO(cr)->npdirs; i++)
			(void) free (CIFF90MO(cr)->pdirs[i] );
		if (CIFF90MO(cr)->cdirs != NULL)
			(void) free ((char *)CIFF90MO(cr)->cdirs);

		if (CIFF90MO(cr)->objname != NULL)
			(void) free ((char *)CIFF90MO(cr)->objname);
		if (CIFF90MO(cr)->calname != NULL)
			(void) free ((char *)CIFF90MO(cr)->calname);
		if (CIFF90MO(cr)->inname != NULL)
			(void) free ((char *)CIFF90MO(cr)->inname);
		break;

	case CIF_F90_OPT_OPTS:
		if (CIFF90OPTOPT(cr)->lopts != NULL)
		  (void) free ((char *)CIFF90OPTOPT(cr)->lopts);
		break;

     	case CIF_F90_INT_BLOCK:
		if (_cif_version == 2) { /* must use v2 cif records */
		  if (CIFF90IB2(cr)->name != NULL)
		      (void) free ((char *)CIFF90IB2(cr)->name);
		  if (CIFF90IB2(cr)->procids != NULL)
		      (void) free ((char *)CIFF90IB2(cr)->procids);
		}
		else { /* VErsion 3 CIF */
		  if (CIFF90IB(cr)->name != NULL)
		      (void) free ((char *)CIFF90IB(cr)->name);
		  if (CIFF90IB(cr)->procids != NULL)
		      (void) free ((char *)CIFF90IB(cr)->procids);
		}
		break;

	case CIF_BE_NODE:
		if (_cif_version == 2) { /* must use v2 cif records */
		    if ((char *)CIFBENODE2(cr)->succs != (char *) NULL) {
		      (void) free ((char *)CIFBENODE2(cr)->succs);
		    }
		    if ((char *)CIFBENODE2(cr)->lines != (char *) NULL) {
		      (void) free ((char *)CIFBENODE2(cr)->lines);
		    }
		    if ((char *)CIFBENODE2(cr)->label != (char *) NULL) {
		      (void) free ((char *)CIFBENODE2(cr)->label);
		    }
		} else {
		    if ((char *)CIFBENODE(cr)->succs != (char *) NULL) {
		      (void) free ((char *)CIFBENODE(cr)->succs);
		    }
		    if ((char *)CIFBENODE(cr)->fid != (char *) NULL) {
		      (void) free ((char *)CIFBENODE(cr)->fid);
		    }
		    if ((char *)CIFBENODE(cr)->lines != (char *) NULL) {
		      (void) free ((char *)CIFBENODE(cr)->lines);
		    }
		    if ((char *)CIFBENODE(cr)->label != (char *) NULL) {
		      (void) free ((char *)CIFBENODE(cr)->label);
		    }
		}
		break;

	case CIF_BE_FID:
		if ((char *)CIFBEFID(cr)->fid != (char *) NULL) {
		  (void) free ((char *)CIFBEFID(cr)->fid);
		}
		break;

	case CIF_CC_TYPE:
		if (CIFCCTYPE(cr)->name != NULL) {
		  (void) free (CIFCCTYPE(cr)->name);
		}
		if (CIFCCTYPE(cr)->mem != NULL) {
		  (void) free ((char *)CIFCCTYPE(cr)->mem);
		}
		break;

	case CIF_CC_ENTRY:
		if (CIFCCENT(cr)->name != NULL) {
		  (void) free (CIFCCENT(cr)->name);
		}
		if (CIFCCENT(cr)->param != NULL) {
		  (void) free ((char *)CIFCCENT(cr)->param);
		}
		break;

	case CIF_CC_OBJ:
		if (CIFCCOBJ(cr)->name != NULL) {
		  (void) free (CIFCCOBJ(cr)->name);
		}
		break;

	case CIF_CC_ENUM:
		if (CIFCCENUM(cr)->name != NULL) {
		  (void) free (CIFCCENUM(cr)->name);
		}
		if (CIFCCENUM(cr)->value != NULL) {
		  (void) free (CIFCCENUM(cr)->value);
		}
		break;

	case CIF_CC_EXPR:
		if (CIFCCEXPR(cr)->oper != NULL) {
		  (void) free ((char *)CIFCCEXPR(cr)->oper);
		}
		break;


	default:
		break;
	}

	(void) free ((char *)cr);
	return;
}
