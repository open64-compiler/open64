/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


static char USMID[] = "@(#) libcif/cifdup.c	30.5	07/26/96 07:19:13";


/*
 * Cif_Duplicate copies the input Cif structure to newly malloc'd space.
 */

#define CIF_VERSION 3

#ifdef _ABSOFT
#include "cif.h"
#else
#include <cif.h>
#endif

#include <stdlib.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <stdio.h>
#include <string.h>

#include "cif_int.h"

struct Cif_generic *Cif_Duplicate
#ifdef __STDC__
(struct Cif_generic *cr)
#else
(cr)
struct Cif_generic *cr;
#endif
{

	register int i, rectype, size;
	struct Cif_generic *ncr;
	char *cp, *ncp;
	
	/* Copy the basic structure */

	rectype = cr->rectype;
	if (rectype < 0 || rectype > CIF_MAXRECORD)
		return (NULL);
	if ((size = _Cif_structsize[rectype][_cif_version]) == 0)
		return (NULL);
	if ((ncr = (struct Cif_generic *)malloc(size)) == NULL)
		return (NULL);
	(void) memcpy ((char *)ncr, (char *)cr, size);

	/* Copy other items associated with each particular structure */

	switch (rectype) {

	case CIF_CALLSITE:
		if ((size = CIFCS(cr)->nargs) > 0) {
			if ((CIFCS(ncr)->argids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFCS(ncr)->argids)[i] = (CIFCS(cr)->argids)[i];
		}
		break;	

	case CIF_COMBLK:
		if (_cif_version == 1) { /* must use v1 cif records */
		  	if ((CIFCB1(ncr)->name = strdup (CIFCB1(cr)->name)) == NULL)
			  	return (NULL);
		}
		else { /* use v2 cif record2 */
		  	if ((CIFCB(ncr)->name = strdup (CIFCB(cr)->name)) == NULL)
			  	return (NULL);
		}
		break;

	case CIF_CONST:
		if ((CIFCON(ncr)->name = strdup (CIFCON(cr)->name)) == NULL)
			return (NULL);
		if ((CIFCON(ncr)->value = strdup (CIFCON(cr)->value)) == NULL)
			return (NULL);
		if (CIFCON(cr)->origform)
		  if ((CIFCON(ncr)->oform = strdup (CIFCON(cr)->oform)) == NULL)
		    return (NULL);
		break;

	case CIF_ENTRY:
		if ((CIFENTRY(ncr)->name = strdup (CIFENTRY(cr)->name)) == NULL)
			return (NULL);
		if ((size = CIFENTRY(cr)->nargs) > 0) {
			if ((CIFENTRY(ncr)->argids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFENTRY(ncr)->argids)[i] = (CIFENTRY(cr)->argids)[i];
		}
		break;

	case CIF_CDIR:
		if ((size = CIFCDIR(cr)->nids) > 0) {
			if ((CIFCDIR(ncr)->ids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFCDIR(ncr)->ids)[i] = (CIFCDIR(cr)->ids)[i];
		}
		break;

	case CIF_CDIR_DOSHARED:
		if ((size = CIFCDIRDO(cr)->nids) > 0) {
			if ((CIFCDIRDO(ncr)->ids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFCDIRDO(ncr)->ids)[i] = (CIFCDIRDO(cr)->ids)[i];
		}
		break;

	case CIF_GEOMETRY:
		if (CIFGEOM(cr)->ndims > 0) {
			size = sizeof(struct Cif_geometry_dim) * CIFGEOM(cr)->ndims;
			if ((CIFGEOM(ncr)->dim = (struct Cif_geometry_dim *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFGEOM(ncr)->dim, (char *)CIFGEOM(cr)->dim,size);

		}
		if (CIFGEOM(cr)->name != (char *) NULL)
		  	CIFGEOM(ncr)->name = strdup (CIFGEOM(cr)->name);
		break;

	case CIF_FILE:
		if (_cif_version == 3) {

		  if ((CIFFILE(ncr)->name = strdup (CIFFILE(cr)->name)) == NULL)
		      return(NULL);

		  if (CIFFILE(cr)->oname == NULL)
		      CIFFILE(ncr)->oname = NULL;
		  else
		      if ((CIFFILE(ncr)->oname = strdup (CIFFILE(cr)->oname)) == NULL)
			  return(NULL);
		}
		else { /* must use v1/v2 version of the cif_file record */

		  if ((CIFFILE1(ncr)->name = strdup (CIFFILE1(cr)->name)) == NULL)
		      return(NULL);

		}
		break;

	case CIF_LABEL:
		if ((CIFLABEL(ncr)->name = strdup (CIFLABEL(cr)->name)) == NULL)
			return(NULL);
		break;

	case CIF_ORIG_CMD:
		if ((CIFOCMD(ncr)->name = strdup (CIFOCMD(cr)->name)) == NULL)
			return(NULL);
		break;


	case CIF_MESSAGE:
		if (_cif_version < 3) { /* must use v1/2 cif records */

		  if ((size = CIFMSG1(cr)->nargs) > 0) {
		    if ((CIFMSG1(ncr)->args = (char **) malloc (sizeof(char *)*size))
			== NULL) return (NULL);
		    for (i = 0; i < size; i++)
			if (((CIFMSG1(ncr)->args)[i] = strdup ((CIFMSG1(cr)->args)[i]))
			    == NULL) return (NULL);
		  }
		}
		else { /* V3 CIF */

		  if (CIFMSG(cr)->nlen > 0 &&
		      (CIFMSG(ncr)->name = strdup (CIFMSG(cr)->name)) == NULL)
		      return(NULL);

		  if ((size = CIFMSG(cr)->nargs) > 0) {
		    if ((CIFMSG(ncr)->args = (char **) malloc (sizeof(char *)*size))
			== NULL) return (NULL);
		    for (i = 0; i < size; i++)
			if (((CIFMSG(ncr)->args)[i] = strdup ((CIFMSG(cr)->args)[i]))
			    == NULL) return (NULL);
		  }

		}
		break;

	case CIF_MISC_OPTS:

		if (_cif_version == 1) { /* must use v1 cif records */

		  if ((size = CIFMO1(cr)->nmsgs) > 0) {
		    if ((CIFMO1(ncr)->msgno = (long *) malloc (sizeof(long)*size))
			== NULL) return (NULL);
		    (void) memcpy ((char *)CIFMO1(ncr)->msgno, (char *)CIFMO1(cr)->msgno,
				   sizeof(long)*size);
		  }
		  if ((size = CIFMO1(cr)->ncdirs) > 0) {
		    if ((CIFMO1(ncr)->cdirs = (char **) malloc (sizeof(char *)*size))
			== NULL) return (NULL);
		    for (i = 0; i < size; i++)
		      if (((CIFMO1(ncr)->cdirs)[i] = strdup ((CIFMO1(cr)->cdirs)[i]))
			  == NULL) return (NULL);
		  }
		  if (CIFMO1(cr)->objname != NULL)
		    if ((CIFMO1(ncr)->objname = strdup (CIFMO1(cr)->objname)) == NULL)
		      return(NULL);
		  if (CIFMO1(cr)->calname != NULL)
		    if ((CIFMO1(ncr)->calname = strdup (CIFMO1(cr)->calname)) == NULL)
		      return(NULL);
		  if (CIFMO1(cr)->inname != NULL)
		    if ((CIFMO1(ncr)->inname = strdup (CIFMO1(cr)->inname)) == NULL)
		      return(NULL);

		}
		else { /* use v2 cif records */


		  if ((size = CIFMO(cr)->nmsgs) > 0) {
		    if ((CIFMO(ncr)->msgno = (long *) malloc (sizeof(long)*size))
			== NULL) return (NULL);
		    (void) memcpy ((char *)CIFMO(ncr)->msgno, (char *)CIFMO(cr)->msgno,
				   sizeof(long)*size);
		  }
		  if ((size = CIFMO(cr)->ncdirs) > 0) {
		    if ((CIFMO(ncr)->cdirs = (char **) malloc (sizeof(char *)*size))
			== NULL) return (NULL);
		    for (i = 0; i < size; i++)
		      if (((CIFMO(ncr)->cdirs)[i] = strdup ((CIFMO(cr)->cdirs)[i]))
			  == NULL) return (NULL);
		  }
		  if ((size = CIFMO(cr)->numincs) > 0) {
		    if ((CIFMO(ncr)->incdirs = (char **) malloc (sizeof(char *)*size))
			== NULL) return (NULL);
		    for (i = 0; i < size; i++)
		      if (((CIFMO(ncr)->incdirs)[i] = strdup ((CIFMO(cr)->incdirs)[i]))
			  == NULL) return (NULL);
		  }
		  if (CIFMO(cr)->objname != NULL)
		    if ((CIFMO(ncr)->objname = strdup (CIFMO(cr)->objname)) == NULL)
		      return(NULL);
		  if (CIFMO(cr)->lname != NULL)
		    if ((CIFMO(ncr)->lname = strdup (CIFMO(cr)->lname)) == NULL)
		      return(NULL);
		  if (CIFMO(cr)->calname != NULL)
		    if ((CIFMO(ncr)->calname = strdup (CIFMO(cr)->calname)) == NULL)
		      return(NULL);
		  if (CIFMO(cr)->inname != NULL)
		    if ((CIFMO(ncr)->inname = strdup (CIFMO(cr)->inname)) == NULL)
		      return(NULL);


		}
		break;

	case CIF_NAMELIST:
		if ((CIFNL(ncr)->name = strdup (CIFNL(cr)->name)) == NULL)
			return (NULL);
		size = sizeof(long) * CIFNL(cr)->nids;
		if ((CIFNL(ncr)->ids = (long *) malloc (size)) == NULL) return (NULL);
		for (i = 0; i < (int) CIFNL(cr)->nids; i++)
			(CIFNL(ncr)->ids)[i] = (CIFNL(cr)->ids)[i];
		break;

	case CIF_ND_MSG:
		if ((size = CIFNMSG(cr)->nargs) > 0) {
			if ((CIFNMSG(ncr)->args = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFNMSG(ncr)->args)[i] = strdup ((CIFNMSG(cr)->args)[i]))
					== NULL) return (NULL);
		}
		break;

	case CIF_OBJECT:
		if (_cif_version == 1) { /* must use v1 cif records */

		  	if (CIFOBJ1(cr)->name != NULL)
			  	if ((CIFOBJ1(ncr)->name = strdup (CIFOBJ1(cr)->name)) == NULL)
				  	return(NULL);
			if (CIFOBJ1(cr)->ndims > 0) {
			  	size = sizeof(struct Cif_dim) * CIFOBJ1(cr)->ndims;
				if ((CIFOBJ1(ncr)->dim = (struct Cif_dim *) malloc (size)) == NULL)
				  	return (NULL);
				(void) memcpy ((char *)CIFOBJ1(ncr)->dim, (char *)CIFOBJ1(cr)->dim,size);
			      }
		}
		else { /* use v2 cif records */

		  	if (CIFOBJ(cr)->name != NULL)
			  	if ((CIFOBJ(ncr)->name = strdup (CIFOBJ(cr)->name)) == NULL)
				  	return(NULL);
			if (CIFOBJ(cr)->ndims > 0) {
			  	size = sizeof(struct Cif_dim) * CIFOBJ(cr)->ndims;
				if ((CIFOBJ(ncr)->dim = (struct Cif_dim *) malloc (size)) == NULL)
				  	return (NULL);
				(void) memcpy ((char *)CIFOBJ(ncr)->dim, (char *)CIFOBJ(cr)->dim,size);
			      }

		}
		break;

	case CIF_UNIT:
		if ((CIFUNIT(ncr)->name = strdup (CIFUNIT(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_ENDUNIT:
		if ((CIFENDU(ncr)->name = strdup (CIFENDU(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_USAGE:
		if (_cif_version == 1) { /* must use v1 cif records */

		  size = sizeof(struct Cif_use) * CIFUSAGE1(cr)->nuses;
		  if ((CIFUSAGE1(ncr)->use = (struct Cif_use *) malloc (size)) == NULL)
		    return (NULL);
		  (void) memcpy ((char *)CIFUSAGE1(ncr)->use, (char *)CIFUSAGE1(cr)->use,
				 size);

		}
		else {  /* use v2 cif records */

		  size = sizeof(struct Cif_use) * CIFUSAGE(cr)->nuses;
		  if ((CIFUSAGE(ncr)->use = (struct Cif_use *) malloc (size)) == NULL)
		    return (NULL);
		  (void) memcpy ((char *)CIFUSAGE(ncr)->use, (char *)CIFUSAGE(cr)->use,
				 size);
		  if (CIFUSAGE(cr)->nmembs > 0) {
		    size = sizeof(long) * CIFUSAGE(cr)->nmembs;
		    if ((CIFUSAGE(ncr)->membs = (long *) malloc (size)) == NULL)
		      return (NULL);
		    (void) memcpy ((char *)CIFUSAGE(ncr)->membs, (char *)CIFUSAGE(cr)->membs,
				 size);
		  }

		}

		break;

	case CIF_FILEDIR:
	{
		struct Cif_unittbl *nut, *ut;

		size = sizeof(struct Cif_unittbl) * CIFFDIR(cr)->nunits;
		ut = CIFFDIR(cr)->ut;
		if ((nut = CIFFDIR(ncr)->ut = (struct Cif_unittbl *)malloc (size))
			== NULL) return (NULL);
		(void) memcpy ((char *)nut, (char *)ut, size);
		for (i = 0; i < (int) CIFFDIR(cr)->nunits; i++)
			if ((nut[i].name = strdup (ut[i].name)) == NULL) return (NULL);
		break;
	}

	case CIF_UNITDIR:
		size = sizeof(struct Cif_urectbl) * CIFUDIR(cr)->nsections;
		if ((CIFUDIR(ncr)->ur = (struct Cif_urectbl *) malloc (size))
			== NULL) return (NULL);
		(void) memcpy ((char *)CIFUDIR(ncr)->ur, (char *)CIFUDIR(cr)->ur, size);
		break;

	case CIF_C_TAG:
		if (CIFCTAG(cr)->nmods > 0) {
			size = sizeof(struct Cif_tmod) * CIFCTAG(cr)->nmods;
			if ((CIFCTAG(ncr)->mods = (struct Cif_tmod *) malloc (size))
				== NULL) return (NULL);
			(void) memcpy ((char *)CIFCTAG(ncr)->mods, (char *)CIFCTAG(cr)->mods,
			               size);
		}
		if (CIFCTAG(cr)->nmems > 0) {
			size = sizeof(long) * CIFCTAG(cr)->nmems;
			if ((CIFCTAG(ncr)->memids = (long *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFCTAG(ncr)->memids,
			               (char *)CIFCTAG(cr)->memids,
			               size);
		}
		if ((CIFCTAG(ncr)->name = strdup (CIFCTAG(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_C_OPTS:
	{
		char **ncp, **cp;

		if ((CIFCOPTS(ncr)->name = strdup (CIFCOPTS(cr)->name)) == NULL)
			return (NULL);
		if (CIFCOPTS(cr)->nincs > 0) {
			cp = CIFCOPTS(cr)->incs;
			size = CIFCOPTS(cr)->nincs;
			if ((ncp = (char **)malloc(sizeof(char *)*size)) == NULL)
				 return (NULL);
			CIFCOPTS(ncr)->incs = ncp;
			for (i = 0; i < size; i++)
				if ((ncp[i] = strdup (cp[i])) == NULL) return (NULL);
		}
		if (CIFCOPTS(cr)->ndefs > 0) {
			cp = CIFCOPTS(cr)->defs;
			size = CIFCOPTS(cr)->ndefs;
			if ((ncp = (char **)malloc(sizeof(char *)*size)) == NULL)
				return (NULL);
			CIFCOPTS(ncr)->defs = ncp;
			for (i = 0; i < size; i++)
				if ((ncp[i] = strdup (cp[i])) == NULL) return (NULL);
		}
		if (CIFCOPTS(cr)->nudefs > 0) {
			cp = CIFCOPTS(cr)->udefs;
			size = CIFCOPTS(cr)->nudefs;
			if ((ncp = (char **)malloc(sizeof(char *)*size)) == NULL)
				return (NULL);
			CIFCOPTS(ncr)->udefs = ncp;
			for (i = 0; i < size; i++)
				if ((ncp[i] = strdup (cp[i])) == NULL) return (NULL);
		}
		break;
	}

	case CIF_C_MESSAGE:

		if (_cif_version == 1) { /* must use v1 cif records */

		  if ((size = CIFCMSG1(cr)->nargs) > 0) {
			if ((CIFCMSG1(ncr)->args = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFCMSG1(cr)->args)[i] =
				     strdup ((CIFCMSG1(cr)->args)[i]))
					== NULL) return (NULL);
		      }

		}
		else { /* use v2 cif records */

		  if ((size = CIFCMSG(cr)->nargs) > 0) {
			if ((CIFCMSG(ncr)->args = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFCMSG(cr)->args)[i] = strdup ((CIFCMSG(cr)->args)[i]))
					== NULL) return (NULL);
		      }

		}
		break;

	case CIF_C_CONST:
		if ((CIFCCON(ncr)->value = strdup (CIFCCON(cr)->value)) == NULL)
			return (NULL);
		break;

	case CIF_C_ENTRY:

		if (_cif_version == 1) { /* must use v1 cif records */

		  if (CIFCENTRY1(cr)->nmods > 0) {
			size = sizeof(struct Cif_tmod) * CIFCENTRY1(cr)->nmods;
			if ((CIFCENTRY1(ncr)->mods = (struct Cif_tmod *) malloc (size))
				== NULL) return (NULL);
			(void) memcpy ((char *)CIFCENTRY1(ncr)->mods,
			               (char *)CIFCENTRY1(cr)->mods,
			               size);
		      }

		  if (CIFCENTRY1(cr)->nargs > 0) {
		    	size = sizeof(long) * CIFCENTRY1(cr)->nargs;
			if ((CIFCENTRY1(ncr)->argids = (long *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFCENTRY1(ncr)->argids,
			               (char *)CIFCENTRY1(cr)->argids,
			               size);
		      }
		  if ((CIFCENTRY1(ncr)->name = strdup (CIFCENTRY1(cr)->name)) == NULL)
		   	 return (NULL);

		}
		else { /* use v2 cif records */

		  if (CIFCENTRY(cr)->nmods > 0) {
			size = sizeof(struct Cif_tmod) * CIFCENTRY(cr)->nmods;
			if ((CIFCENTRY(ncr)->mods = (struct Cif_tmod *) malloc (size))
				== NULL) return (NULL);
			(void) memcpy ((char *)CIFCENTRY(ncr)->mods,
			               (char *)CIFCENTRY(cr)->mods,
			               size);
		      }

		  if (CIFCENTRY(cr)->nargs > 0) {
		    	size = sizeof(long) * CIFCENTRY(cr)->nargs;
			if ((CIFCENTRY(ncr)->argids = (long *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFCENTRY(ncr)->argids,
			               (char *)CIFCENTRY(cr)->argids,
			               size);
		      }
		  if ((CIFCENTRY(ncr)->name = strdup (CIFCENTRY(cr)->name)) == NULL)
		   	 return (NULL);
		}

		break;

	case CIF_C_OBJECT:
		if (CIFCOBJ(cr)->nmods > 0) {
			size = sizeof(struct Cif_tmod) * CIFCOBJ(cr)->nmods;
			if ((CIFCOBJ(ncr)->mods = (struct Cif_tmod *) malloc (size))
				== NULL) return (NULL);
			(void) memcpy ((char *)CIFCOBJ(ncr)->mods, (char *)CIFCOBJ(cr)->mods,
			               size);
		}
		if ((CIFCOBJ(ncr)->name = strdup (CIFCOBJ(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_C_LINT_DIRECTIVE:
		if ((CIFCLDIR(ncr)->name = strdup (CIFCLDIR(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_C_MACRO_DEF:
		if ((CIFCMDEF(ncr)->name = strdup (CIFCMDEF(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_C_ENTRY_END:
		if ((CIFCEEND(ncr)->name = strdup (CIFCEEND(cr)->name)) == NULL)
			return (NULL);
		break;


	case CIF_F90_CALLSITE:
		if ((size = CIFF90CS(cr)->nargs) > 0) {
			if ((CIFF90CS(ncr)->argids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			if (CIFF90CS(cr)->rank == 1)
			    if ((CIFF90CS(ncr)->ranks = (int *) malloc (sizeof(int)*size))
				== NULL) return (NULL);

			for (i=0; i < size; i++) {
				(CIFF90CS(ncr)->argids)[i] = (CIFF90CS(cr)->argids)[i];
				(CIFF90CS(ncr)->nmembs)[i] = (CIFF90CS(cr)->nmembs)[i];
				(CIFF90CS(ncr)->membs)[i] =
				  (long *) malloc ((CIFF90CS(cr)->nmembs)[i] * sizeof(long));
				(void) memcpy (((char *) (CIFF90CS(ncr)->membs)[i]),
					       ((char *) (CIFF90CS(cr)->membs)[i]),
					       (CIFF90CS(cr)->nmembs)[i] * sizeof(long));

				if (CIFF90CS(cr)->rank == 1)
				    (CIFF90CS(ncr)->ranks)[i] = (CIFF90CS(cr)->ranks)[i];

			      }
		}
		break;

	case CIF_F90_SCOPE_INFO:
		if ((size = CIFF90SI(cr)->numalts) > 0) {
			if ((CIFF90SI(ncr)->entryids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFF90SI(ncr)->entryids)[i] = (CIFF90SI(cr)->entryids)[i];
		}
		break;

	case CIF_F90_COMBLK:
		if ((CIFF90CB(ncr)->name = strdup (CIFF90CB(cr)->name)) == NULL)
			return (NULL);
		break;

	case CIF_F90_CONST:
		if ((CIFF90CON(ncr)->value = strdup (CIFF90CON(cr)->value)) == NULL)
			return (NULL);
		break;

	case CIF_F90_ENTRY:
		if ((CIFF90ENTRY(ncr)->name = strdup (CIFF90ENTRY(cr)->name)) == NULL)
			return (NULL);
		if ((size = CIFF90ENTRY(cr)->nargs) > 0) {
			if ((CIFF90ENTRY(ncr)->argids = (long *) malloc (sizeof(long *)*size))
				== NULL) return (NULL);
			for (i=0; i < size; i++)
				(CIFF90ENTRY(ncr)->argids)[i] = (CIFF90ENTRY(cr)->argids)[i];
		}

		break;

	case CIF_F90_DERIVED_TYPE:
		if (_cif_version == 2) { /* must use v2 cif records */

		  if ((CIFF90DTYPE2(ncr)->name = strdup (CIFF90DTYPE2(cr)->name)) == NULL)
		      return (NULL);
		  if ((size = CIFF90DTYPE2(cr)->nmembs) > 0) {
		    if ((CIFF90DTYPE2(ncr)->memids = (long *) malloc (sizeof(long *)*size))
			== NULL) return (NULL);
		    for (i=0; i < size; i++)
			(CIFF90DTYPE2(ncr)->memids)[i] = (CIFF90DTYPE2(cr)->memids)[i];
		  }
		}
		else {  /* Version 3 CIF */

		  if ((CIFF90DTYPE(ncr)->name = strdup (CIFF90DTYPE(cr)->name)) == NULL)
		      return (NULL);
		  if ((size = CIFF90DTYPE(cr)->nmembs) > 0) {
		    if ((CIFF90DTYPE(ncr)->memids = (long *) malloc (sizeof(long *)*size))
			== NULL) return (NULL);
		    for (i=0; i < size; i++)
			(CIFF90DTYPE(ncr)->memids)[i] = (CIFF90DTYPE(cr)->memids)[i];
		  }
		}
		break;

	case CIF_F90_LABEL:
		if ((CIFF90LABEL(ncr)->name = strdup (CIFF90LABEL(cr)->name)) == NULL)
			return(NULL);
		break;

	case CIF_F90_NAMELIST:
		if ((CIFF90NL(ncr)->name = strdup (CIFF90NL(cr)->name)) == NULL)
			return (NULL);
		size = sizeof(long) * CIFF90NL(cr)->nids;
		if ((CIFF90NL(ncr)->ids = (long *) malloc (size)) == NULL) return (NULL);
		for (i = 0; i < (int) CIFF90NL(cr)->nids; i++)
			(CIFF90NL(ncr)->ids)[i] = (CIFF90NL(cr)->ids)[i];
		break;

	case CIF_F90_OBJECT:
		if (CIFF90OBJ(cr)->name != NULL)
			if ((CIFF90OBJ(ncr)->name = strdup (CIFF90OBJ(cr)->name)) == NULL)
				return(NULL);
		if (CIFF90OBJ(cr)->ndims > 0) {
			size = sizeof(struct Cif_dim) * CIFF90OBJ(cr)->ndims;
			if ((CIFF90OBJ(ncr)->dim = (struct Cif_dim *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFF90OBJ(ncr)->dim, (char *)CIFF90OBJ(cr)->dim,size);
		}
		break;


   case CIF_F90_MISC_OPTS:
		if ((size = CIFF90MO(cr)->nmsgs) > 0) {
			if ((CIFF90MO(ncr)->msgno = (long *) malloc (sizeof(long)*size))
				== NULL) return (NULL);
			(void) memcpy ((char *)CIFF90MO(ncr)->msgno, (char *)CIFF90MO(cr)->msgno,
			                sizeof(long)*size);
		}
		if ((size = CIFF90MO(cr)->ncdirs) > 0) {
			if ((CIFF90MO(ncr)->cdirs = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFF90MO(ncr)->cdirs)[i] = strdup ((CIFF90MO(cr)->cdirs)[i]))
					== NULL) return (NULL);
		}
		if ((size = CIFF90MO(cr)->nPdirs) > 0) {
			if ((CIFF90MO(ncr)->Pdirs = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFF90MO(ncr)->Pdirs)[i] = strdup ((CIFF90MO(cr)->Pdirs)[i]))
					== NULL) return (NULL);
		}
		if ((size = CIFF90MO(cr)->npdirs) > 0) {
			if ((CIFF90MO(ncr)->pdirs = (char **) malloc (sizeof(char *)*size))
				== NULL) return (NULL);
			for (i = 0; i < size; i++)
				if (((CIFF90MO(ncr)->pdirs)[i] = strdup ((CIFF90MO(cr)->pdirs)[i]))
					== NULL) return (NULL);
		}
		if (CIFF90MO(cr)->objname != NULL)
			if ((CIFF90MO(ncr)->objname = strdup (CIFF90MO(cr)->objname)) == NULL)
				return(NULL);
		if (CIFF90MO(cr)->calname != NULL)
			if ((CIFF90MO(ncr)->calname = strdup (CIFF90MO(cr)->calname)) == NULL)
				return(NULL);
		if (CIFF90MO(cr)->inname != NULL)
			if ((CIFF90MO(ncr)->inname = strdup (CIFF90MO(cr)->inname)) == NULL)
				return(NULL);
		if (CIFF90MO(cr)->cifname != NULL)
			if ((CIFF90MO(ncr)->cifname = strdup (CIFF90MO(cr)->cifname)) == NULL)
				return(NULL);
		break;

	case CIF_F90_OPT_OPTS:
		if (CIFF90OPTOPT(cr)->noptlevels > 0) {
			size = sizeof(struct Cif_f90_level_opts) * CIFF90OPTOPT(cr)->noptlevels;
			if ((CIFF90OPTOPT(ncr)->lopts = (struct Cif_f90_level_opts *) malloc (size)) == NULL)
				return (NULL);
			(void) memcpy ((char *)CIFF90OPTOPT(ncr)->lopts, (char *)CIFF90OPTOPT(cr)->lopts,size);
		}
		break;

     	case CIF_F90_INT_BLOCK:
		if (_cif_version == 2) { /* must use v2 cif records */

		  if (CIFF90IB2(cr)->name != NULL)
		      if ((CIFF90IB2(ncr)->name = strdup (CIFF90IB2(cr)->name)) == NULL)
			  return(NULL);

		  if ((size = CIFF90IB2(cr)->numints) > 0) {
		    if ((CIFF90IB2(ncr)->procids = (long *) malloc (sizeof(long)*size))
			== NULL) return (NULL);
		    (void) memcpy ((char *)CIFF90IB2(ncr)->procids,
				   (char *)CIFF90IB2(cr)->procids,
				   sizeof(long)*size);
		  }
		}
		else {  /* Version 3 CIF */

		  if (CIFF90IB(cr)->name != NULL)
		      if ((CIFF90IB(ncr)->name = strdup (CIFF90IB(cr)->name)) == NULL)
			  return(NULL);

		  if ((size = CIFF90IB(cr)->numints) > 0) {
		    if ((CIFF90IB(ncr)->procids = (long *) malloc (sizeof(long)*size))
			== NULL) return (NULL);
		    (void) memcpy ((char *)CIFF90IB(ncr)->procids,
				   (char *)CIFF90IB(cr)->procids,
				   sizeof(long)*size);
		  }
		}
		break;


	case CIF_BE_NODE:
		if (_cif_version == 2) { /* must use v2 cif records */
		    size = CIFBENODE2(cr)->nsuccs * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		    CIFBENODE2(ncr)->succs = (int *) ncp;
		    cp = (char *) CIFBENODE2(cr)->succs;
		    (void) memcpy (ncp, cp, size);
		    size = CIFBENODE2(cr)->nlines * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		    CIFBENODE2(ncr)->lines = (int *) ncp;
		    cp = (char *) CIFBENODE2(cr)->lines;
		    (void) memcpy (ncp, cp, size);
		    if ((CIFBENODE2(ncr)->label = strdup (CIFBENODE2(cr)->label)) == NULL)
			return (NULL);
		} else {
		    size = CIFBENODE(cr)->nsuccs * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			    return( NULL );
		    CIFBENODE(ncr)->succs = (int *) ncp;
		    cp = (char *) CIFBENODE(cr)->succs;
		    (void) memcpy (ncp, cp, size);
		    size = CIFBENODE(cr)->nlines * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			    return( NULL );
		    CIFBENODE(ncr)->fid = (int *) ncp;
		    cp = (char *) CIFBENODE(cr)->fid;
		    (void) memcpy (ncp, cp, size);
		    if ((ncp = (char *) malloc( size )) == NULL )
			    return( NULL );
		    CIFBENODE(ncr)->lines = (int *) ncp;
		    cp = (char *) CIFBENODE(cr)->lines;
		    (void) memcpy (ncp, cp, size);
		    if ((CIFBENODE(ncr)->label = strdup (CIFBENODE(cr)->label)) == NULL)
			    return (NULL);
		}
		break;


	case CIF_BE_FID:
		size = CIFBEFID(cr)->nfid * sizeof( int );
		if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		CIFBEFID(ncr)->fid = (int *) ncp;
		cp = (char *) CIFBEFID(cr)->fid;
		(void) memcpy (ncp, cp, size);
		break;


	case CIF_CC_TYPE:
		if (CIFCCTYPE(cr)->nlen > 0) {
		    CIFCCTYPE(ncr)->name = strdup (CIFCCTYPE(cr)->name);
		    if (CIFCCTYPE(ncr)->name == NULL)
			return(NULL);
		}
		if (CIFCCTYPE(cr)->nmem > 0) {
		    size = CIFCCTYPE(cr)->nmem * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		    CIFCCTYPE(ncr)->mem = (int *) ncp;
		    cp = (char *) CIFCCTYPE(cr)->mem;
		    (void) memcpy (ncp, cp, size);
		}
		break;

	case CIF_CC_ENTRY:
		if (CIFCCENT(cr)->nlen > 0) {
		    CIFCCENT(ncr)->name = strdup (CIFCCENT(cr)->name);
		    if (CIFCCENT(ncr)->name == NULL)
			return(NULL);
		}
		if (CIFCCENT(cr)->nparam > 0) {
		    size = CIFCCENT(cr)->nparam * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		    CIFCCENT(ncr)->param = (int *) ncp;
		    cp = (char *) CIFCCENT(cr)->param;
		    (void) memcpy (ncp, cp, size);
		}
		break;

	case CIF_CC_OBJ:
		if (CIFCCOBJ(cr)->nlen > 0) {
		    CIFCCOBJ(ncr)->name = strdup (CIFCCOBJ(cr)->name);
		    if (CIFCCOBJ(ncr)->name == NULL)
			return(NULL);
		}
		break;

	case CIF_CC_ENUM:
		if (CIFCCENUM(cr)->nlen > 0) {
		    CIFCCENUM(ncr)->name = strdup (CIFCCENUM(cr)->name);
		    if (CIFCCENUM(ncr)->name == NULL)
			return(NULL);
		}
		if (CIFCCENUM(cr)->vlen > 0) {
		    CIFCCENUM(ncr)->value = strdup (CIFCCENUM(cr)->value);
		    if (CIFCCENUM(ncr)->value == NULL)
			return(NULL);
		}
		break;

	case CIF_CC_EXPR:
		if (CIFCCEXPR(cr)->noper > 0) {
		    size = CIFCCEXPR(cr)->noper * sizeof( int );
		    if ((ncp = (char *) malloc( size )) == NULL )
			return( NULL );
		    CIFCCEXPR(ncr)->oper = (int *) ncp;
		    cp = (char *) CIFCCEXPR(cr)->oper;
		    (void) memcpy (ncp, cp, size);
		}
		break;

	default:
		break;
	}

	return (ncr);
}
