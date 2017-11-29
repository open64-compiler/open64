/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/openutil.c	92.2	06/21/99 16:28:21"

#include <errno.h>
#include <ffio.h>
#include <memory.h>
#include <string.h>
#include <liberrno.h>
#include <stdarg.h>
#include <unistd.h>
#include <cray/assign.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"

/*
 *	_deduce_fstruct
 *
 *	Return the effective file structure FS_xxx code for a file based
 *	on the ufs field from the unit table, the (optional) ffio fffd,
 *	and the (optional) form for the file.  This routine's purpose is
 *	to return the best description of the file structure based on the
 *	information available.  For example, STD is translated into
 *	FS_TEXT or FS_UNBLOCKED when possible.  FS_FDC is translated into
 *	FS_COS, FS_UNBLOCKED, or FS_TEXT when possible and appropriate.
 *
 *	If ufs is FS_FDC, the ffio spec (if present) is examined.  If ufs
 *	is STD, the formatted/unformatted parameter will distinguish
 *	between FS_TEXT and FS_PURE.
 *
 *	_deduce_fstruct is callable in any of the following ways:
 *
 *		_deduce_fstruct(ufs, NULL, form);
 *
 *	Parameters:
 *		ufs	the ufs field from the unit table.
 *		fffd	a pointer to the ffio spec list, or NULL.
 *		form	1 if formatted, 0 if unformatted, -1 if 
 *			unknown.
 *
 *	Return values:
 *		0		if file structure is unknown
 *		FS_TEXT		if newline-delimited records
 *		FS_COS		if COS blocked
 *		FS_UNBLOCKED	if unblocked 
 *		FS_PURE		if pure data 
 *		FS_TAPE		if tape block format
 *		FS_FDC		if misc FFIO file
 *		STD		if unsure whether TEXT or UNBLOCKED
 */

int
_deduce_fstruct(
	int		ufs,
	struct fdinfo	*fffd,
	int		formatted)
{
	switch (ufs) {
	case FS_TEXT:
	case FS_COS:
	case FS_UNBLOCKED:
	case FS_PURE:
	case FS_TAPE:
	case FS_AUX:
		return(ufs);
	case FS_BIN:
	case FS_U:
		return(FS_UNBLOCKED);
	case STD:
		if (formatted == -1) 
			return(STD);
		else {		
			switch (formatted) {
			case 0:
				return(FS_UNBLOCKED);
			case 1: 
				return(FS_TEXT);
			default:
				return(STD);
			}
		}
	case FS_FDC:
		if (fffd == NULL)
			return(0);
		else {
			switch (fffd->class) {
			case CLASS_COS:
				return(FS_COS);
			case CLASS_TEXT:
				return(FS_TEXT);
#ifndef	_UNICOS
			case CLASS_X:
				return(FS_F77);	/* might be nosve on UNICOS */
#endif
			case CLASS_SYSTEM:
			case CLASS_SDS:
			case CLASS_MR:
			case CLASS_CACHE:
			case CLASS_CACHEA:
				return(FS_UNBLOCKED);
			default:
				return(0);
			}
		}
	}
	return(0);		/* MIPS compiler requires a return here */
}

/*
 *	_setup_cvrt
 *
 *	Set up the character set to use for character conversion.  The
 *	specification of numeric convert implies character conversion
 *	unless otherwise specified.
 *
 *	Internally, the unumcvrt and ucharset variables always control
 *	the conversion for numbers and characters respectively.
 */
void
_setup_cvrt(unit *cup)
{
	if (cup->ucharset == 0 && cup->unumcvrt != 0)
		switch(cup->unumcvrt) {
			case NCV_IBM:
				cup->ucharset	= CS_EBCDIC;
				break;
			case NCV_CDC:
				cup->ucharset	= CS_CDC;
				break;
			default:
				break;
		} /* switch */

	/* Suppress conversion if type is native. */

	if (cup->unumcvrt == NCV_NATIVE)
		cup->unumcvrt	= 0;

	if (cup->ucharset == CS_NATIVE)
		cup->ucharset	= 0;

	return;
}

/*
 *	_set_device_and_inode
 *
 *	Get a device and inode number for a file descriptor.  If sysfd == -1,
 *	then this Fortran file does not correspond to one system file.
 */
void
_set_device_and_inode(
	int	sysfd,
	dev_t	*devp,
	ino_t	*inodep)
{
 	struct stat	statbuf;
 
	if (sysfd >= 0 && fstat(sysfd, &statbuf) == 0) {
		*inodep	= statbuf.st_ino;
		*devp	= statbuf.st_dev;
	}
	else {
		*inodep	= -1;
		*devp	= -1;
	}

	return;
}

/*
 *	_UNIQINOD
 *
 *	Check that another unit hasn't been opened to the same file
 *
 *	If the inode is -1, something funny is going on.
 *	Most likely, the file has been assigned to SDS or another
 *	device by FDC.  So skip the check.
 *
 *	Return value:
 *	   0	If no other unit is connected to the file identified
 *		by the (inode, device number) pair.
 *	<> 0	If another unit is connected to the file.  The return
 *		value is FEMDUPOP or FEOPFNCN if this is true.
 *		
 */ 
int
_uniqinod(
	unit		*cup,		/* unit being checked */
	assign_info	*aip)		/* assign information, or NULL */
{
	register int	 multup;	/* 1 if "-m on" specified */
	unit		*uptr;
	ino_t		inode;
	dev_t		device;	
	struct stat	statbuf;

	inode	= cup->uinode;
	device	= cup->udevice;
	multup	= (aip != NULL && aip->m_multup_flg && aip->m_multup);

	if (RSVD_UNUM(cup->uid))
		return(0);	/* always allow units 100, 101, 102 to connect*/
/*
 *	Stdin, stdout, and stderr may actually be different file descriptors
 *	pointing to the same file.  So allow them to connect to multiple
 *	units.
 */
	if (STDIN_FILENO <= cup->usysfd && cup->usysfd <= STDERR_FILENO)
		return(0);

/*
 *	Allow mr.scr or sds.scr files to connect.
 */
	if (inode == -1)
		return(0);

	if (fstat(cup->usysfd, &statbuf) != -1) {
		/*
		 * If file exists, check for character special files, fifo 
		 * files, and stdin/stdout/stderr.
		 * Allow multiple units to be connected to these files.
		 */
		if(S_ISCHR(statbuf.st_mode))
			return(0);
		if(S_ISFIFO(statbuf.st_mode))
			return(0);
	}

/*
 *	Loop on _get_next_unit() to check for another open unit connected
 *	to the same file.
 */
	uptr	= _get_next_unit(NULL, 0, 1);

	for ( ; uptr != NULL ; uptr = _get_next_unit(uptr, 0, 1)) {

		if (uptr == cup)		continue;
		if (RSVD_UNUM(uptr->uid))	continue;
		if (multup && uptr->umultup)	continue;

		if (inode == uptr->uinode && device == uptr->udevice) {
			if (multup)
				return(FEMDUPOP);
			else
				return(FEOPFNCN);
		}
	}

	return(0);
}

/*
 *	_mixed_scope
 *
 *	Check that the same unit number is not open as both a private and
 *	a global unit.
 *
 *	Return value:
 *		   0	if ok
 *		<> 0	if the unit number is opened as both a global and a
 *			private.  Return value is error number if this is
 *			true.
 */ 
int
_mixed_scope(unit *cup)
{
	unit		*uptr;
	ino_t		inode;
	dev_t		device;	
	struct stat	statbuf;

/*
 *	Loop on _get_next_unit() to check for multiple units connected with
 *	the same unit number.
 */
	uptr	= _get_next_unit(NULL, 0, 0);

	for ( ; uptr != NULL ; uptr = _get_next_unit(uptr, 0, 0)) {
		if (uptr->uid == cup->uid && uptr->private != cup->private)
			return(FEMIXSCP);
	}

	return(0);
}
