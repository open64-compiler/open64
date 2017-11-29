#
#
#  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
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

.SUFFIXES: .o .c .cxx

# Makes the test program for the predicate query system

IPATH = -I$(ROOT)/usr/include/CC
COPTS = -g -DPQSTEST $(IPATH) -no_auto_include

OBJS = pqstest.tab.o pqstest_lex.o pqs.o

tpqs: $(OBJS)
	CC $(COPTS) -o $@ $(OBJS)

pqstest.tab.h pqstest.tab.c : pqstest.y
	bison -d -t pqstest.y

pqstest_lex.c : pqstest_lex.l pqstest.tab.h
	flex -i -opqstest_lex.c pqstest_lex.l

pqstest_lex.o : pqstest_lex.c
	CC -c -MDupdate makedepend -woff 1110 $(COPTS) $<

clean:
	rm -f pqstest_lex.c pqstest.tab.h pqstest.tab.c
	rm -f $(OBJS) tpqs

clobber: clean
	rm -rf ii_files makedepend

.c.o:
	CC -c -MDupdate makedepend $(COPTS) $<

.cxx.o:
	CC -c -MDupdate makedepend $(COPTS) $<

#sinclude "makedepend"
