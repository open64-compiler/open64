#
#
#  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of version 2 of the GNU General Public License as
#  published by the Free Software Foundation.
#
#  This program is distributed in the hope that it would be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
#  Further, this software is distributed without any warranty that it is
#  free of the rightful claim of any third person regarding infringement 
#  or the like.  Any license provided herein, whether implied or 
#  otherwise, applies only to this software file.  Patent licenses, if 
#  any, provided herein do not apply to combinations of this program with 
#  other software, or any other product whatsoever.  
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write the Free Software Foundation, Inc., 59
#  Temple Place - Suite 330, Boston MA 02111-1307, USA.
#
#  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
#  Mountain View, CA 94043, or:
#
#  http://www.sgi.com
#
#  For further information regarding this notice, see:
#
#  http://oss.sgi.com/projects/GenInfo/NoticeExplan
#
#

#!smake
#
# Makefile for include/sys
#

include $(COMMONDEFS)

PUBLIC_HFILES= \
	elf.h \
	elftypes.h \
	elf_whirl.h \
	unwindP.h \
	unwind.h


#if $(BUILD_TARGET) == "MIPS"
STD_LOC=/usr/include/sys
SUBDIRS=mips
#elif $(BUILD_TARGET) == "IA64"
STD_LOC=/ia64/usr/include/sys
SUBDIRS=ia64
#endif

default:

include ${COMMONRULES}

#ifdef BUILD_BASE

headers:
	cd $(BUILD_BASE); \
	$(STD_INSTALL) $(STD_INSTALL_READ_MASK) -F $(STD_LOC) $(PUBLIC_HFILES)
	$(SUBDIRS_MAKERULE)

install:
	cd $(BUILD_BASE); \
	$(INSTALL) $(STD_INSTALL_READ_MASK) -F $(STD_LOC) $(PUBLIC_HFILES)
	$(SUBDIRS_MAKERULE)

#else

headers install:
	$(INSTALL) -m 444 -F $(STD_LOC) $(PUBLIC_HFILES)
	$(SUBDIRS_MAKERULE)
#endif
