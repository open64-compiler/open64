#!/bin/sh 
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

### ====================================================================
### ====================================================================
### Module: gen_x_prop.sh
### $Revision: 1.1.1.1 $
### $Date: 2005/06/23 02:15:39 $
### $Author: sxyang $
### $Source: /u/cvs/Pathscale.MIPS/common/util/gen_x_list.sh,v $
### Revision history:
###   27-Feb-92 - Original version
###
### Synopsis: gen_x_list.sh -   Create an instantiation of externally linked
###                              lists onto a base type.  This is something
###                              you really can't do with the CPP.
###
### Usage:      sh  gen_x_list TYPE PREFIX ALLOC [INCLUDE...]
###
###             Creates the files type_prop.h and type_prop.cxx that
###             implement a module for manipulating very simple bit
###             sets over the univers of objects of the given TYPE.
###             See util/x_prop.h for documentation of the specific
###             functions generated.
###
###             The arguments are:
###
###             TYPE  -     is the complete name of the underlying
###                         type.  For structure types, you probably
###                         need to include the final *, as they are
###                         typically passed by reference.  For types
###                         not passed by reference, such as indices
###                         and the rare single word structure, omit
###                         the final *. TYPE is the actual complete
###                         declaration of the base type parameters of
###                         the new functions.
###
###             PREFIX -    A single identifier to be prepended to the
###                         name of the new type and newly created
###                         functions.
###
###             INCLUDE   - is a file name to be included in the generated
###                         .cxx file.  The above expressions all have
###                         to be parsed and their components defined.
###                         INCLUDES have to include all the .h files
###                         necessary for this.  In particular, it
###                         will probably include xxx.h and memory.h
###
### ====================================================================
### ====================================================================

###
### Parse the arguments:
###
BASE_TYPE="$1"
shift
PREFIX="$1"
shift

###
### Make the derived names:
###
rev='$Revision: 1.1.1.1 $'

L_PREFIX=`echo $PREFIX | tr '[A-Z]' '[a-z]'`

H_RCS_ID="${L_PREFIX}_list_rcs_id"
TYPE="${PREFIX}_LIST"
FIRST="${PREFIX}_LIST_first"
REST="${PREFIX}_LIST_rest"
PUSH="${PREFIX}_LIST_Push"
DELETE="${PREFIX}_LIST_Delete"
DELETE_IF="${PREFIX}_LIST_Delete_If"


###
### Make the names of the output files:
###
H_FILE="${L_PREFIX}_list.h"
C_FILE="${L_PREFIX}_list.cxx"


###
### Generate the .h file:
###
echo "/* Constructed by gen_x_list $rev"		> $H_FILE
echo " */"						>>$H_FILE
echo "#ifndef ${L_PREFIX}_list_included"		>>$H_FILE
echo "#define ${L_PREFIX}_list_included"		>>$H_FILE
for INCLUDE  in $*
do
  echo "#include " \""$INCLUDE"\"			>>$H_FILE
done
echo "#define _X_LIST_TYPE_ $TYPE"			>>$H_FILE
echo "#define _X_LIST_TAG_ $TYPE"			>>$H_FILE
echo "#define _X_BASE_TYPE_ $BASE_TYPE"			>>$H_FILE
echo "#define _X_RCS_ID_ $H_RCS_ID"			>>$H_FILE
echo "#define _X_PUSH_ $PUSH"				>>$H_FILE
echo "#define _X_DELETE_ $DELETE"			>>$H_FILE
echo "#define _X_DELETE_IF_ $DELETE_IF"			>>$H_FILE
echo "#define $FIRST(x) ((x)->first)"			>>$H_FILE
echo "#define $REST(x)  ((x)->rest)"			>>$H_FILE
echo "#define _X_LIST_LOCAL_BASE_TYPE_ ${PREFIX}_LIST_LOCAL_BASE_TYPE_" \
							>>$H_FILE
echo "#include " \""x_list.h"\"				>>$H_FILE
echo "#undef _X_LIST_TYPE_"				>>$H_FILE
echo "#undef _X_LIST_TAG_"				>>$H_FILE
echo "#undef _X_BASE_TYPE_"				>>$H_FILE
echo "#undef _X_RCS_ID_"				>>$H_FILE
echo "#undef _X_PUSH_"					>>$H_FILE
echo "#undef _X_DELETE_"				>>$H_FILE
echo "#undef _X_DELETE_IF_"				>>$H_FILE
echo "#undef _X_LIST_LOCAL_BASE_TYPE_"			>>$H_FILE
echo "#endif"						>>$H_FILE

###
### Generate the .cxx file:
###

echo "/* Constructed by gen_x_list $rev"		> $C_FILE
echo " */"						>>$C_FILE
for INCLUDE in $*
do
  echo "#include " \""$INCLUDE"\"			>>$C_FILE
done
echo "#define _X_LIST_TYPE_ $TYPE"			>>$C_FILE
echo "#define _X_BASE_TYPE_ $BASE_TYPE"			>>$C_FILE
echo "#define _X_RCS_ID_ $H_RCS_ID"			>>$C_FILE
echo "#define _X_PUSH_ $PUSH"				>>$C_FILE
echo "#define _X_DELETE_ $DELETE"			>>$C_FILE
echo "#define _X_DELETE_IF_ $DELETE_IF"			>>$C_FILE
echo "#define _X_LIST_LOCAL_BASE_TYPE_ ${PREFIX}_LIST_LOCAL_BASE_TYPE_" \
							>>$C_FILE
echo "#include " \""x_list.c"\"				>>$C_FILE
