#!/usr/local/bin/perl --	# -*-Perl-*-
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

# This tool generates the Fortran 90 intrinsics
#
#
# Input comes from the file f90intrindefs
#
# The format of this file is:
#
#  <NAME> ;<TYPES> ; <RETURN_TYPES> ; <libnames>
#
# Valid types are
#
# I1, I2, I4, I8, F4, F8, FQ, C4, C8, CQ
#
# in addition, combination types can be specified
# AF - same as F4, F8, FQ
# AC - same as C4, C8, CQ
# AI - same as I4, I8
# AJ - same as I1, I2, I4, I8
# L  - Library function, output returned in register, don't prefix a type name,
#                        use the same name as the library call 
#           (uses ENTER_F90_LIBFUNC)
# LS - Library subroutine, pure but has side effects, use the same name as the library call
#           (uses ENTER_F90_LIBFUNC)
# LR - Library subroutine, not pure and has side effects, use the same name as the library call
#           (uses ENTER_F90_LIBROUTINE)
# LC - Library subroutine, not pure and has side effects, uses libnames
#           (uses ENTER_F90_LIBROUTINE)
# V  - For use only by the F90 lowerer and front-end
#
# Return types may be empty, in whch case the list of types is used.
# For each type in the type list, an intrinsic is generated. For example, 
#
# FRACTION ; AF ; ; _frac4, _frac8, _fracq
# will generate
#
# INTRN_F4FRACTION, INTRN_F8FRACTION, INTRN_FQFRACTION in wintrinsic90.h
# and the names in wutil90.h. Also, in wint_tool90.h, it will generate macro calls
# to ENTER_F90_INTR(INTRN_F4FRACTION,RETURN_F4,"F4FRACTION","_frac4") etc. 
#
# Routines of type L don't prefix a type name, use the same name for the library call, and
# generate  ENTER_F90_LIBFUNC (which enters a pure function with side effects).
#
# Routines of type V generate return types of V in the back end tables, but they should never 
# be seen beyond the f90 lowerer. 
#
# Three files are generated:
# 
# ../com/wintrinsic90.h
# ../com/wutil90.h
# ./wint_tool90.h
#  
# In wintrinsic.h, this code looks for a definition of INTRN_FIRST_F90_INTRINSIC = xxx
# and uses it to start the enumeration. wintrinsic90.h also contains the definition
# of INTRN_LAST_F90_INTRINSIC, which is used for defining INTRINSIC_LAST.
#

$num = 0; #total number of intrinsics generated so far 

#find_first_f90_intrinsic();
open_files();

# Read the input file
while ($line = <FDEFS>) {
    chop $line;
    if ($line =~ /^\#.*/ || $line =~ /^\s*$/) {
	next;
    }
    
# split the string at the ;
    ($name,$ftypes,$rtypes,$libnames) = split(/\s*;\s*/,$line);

# process types. 

    if ($rtypes eq "") {
	$rtypes = $ftypes;
    }

    $rtypes = gettypes($rtypes);
    $ftypes = gettypes($ftypes);

# Split things into lists
    if ($ftypes eq "L") {
	$num_to_gen = 1;
	if ($libnames eq "") {
	    @libname = ($name);
	} else {
	    @libname = split(/\s*,\s*/,$libnames);
	}	    
	@rtype = ($rtypes);
	$funtype = "ENTER_F90_LIBFUNC";
	@ftype = ("");
    } elsif ($ftypes eq "LS") {
	$num_to_gen = 1;
	@libname = ($name);
	@rtype = ($rtypes);
	$funtype = "ENTER_F90_LIBFUNC";
	@ftype = ("");
    } elsif ($ftypes eq "LR") {
	$num_to_gen = 1;
	@libname = ($name);
	@rtype = ($rtypes);
	$funtype = "ENTER_F90_LIBROUTINE";
	@ftype = ("");
    } elsif ($ftypes eq "LC") {
	$num_to_gen = 1;
	@libname = split(/\s*,\s*/,$libnames);
	@rtype = ($rtypes);
	$funtype = "ENTER_F90_LIBROUTINE";
	@ftype = ("");
    } elsif ($ftypes eq "V") {
	$num_to_gen = 1;
	$funtype = "";
	@ftype = ("");
    } else {
# split things up
	@ftype = split(/,/,$ftypes);
	@rtype = split(/,/,$rtypes);
	@libname = split(/\s*,\s*/,$libnames);
	$funtype = "ENTER_F90_INTR";
	$num_to_gen = $#libname + 1;
    }

# output stuff to the files
    while ($num_to_gen > 0) {
	$fty = shift(@ftype);
	$rty = "RETURN_".shift(@rtype);
	if ($rty eq "RETURN_") {
	    $rty = "RETURN_V";
	}
	$iname = $fty.$name;
	$lname = shift(@libname);
	if ($lname eq "") {
	    $lname = $name;
	}
	
# output to wintrinsics90.h,wutil90.h
	if ($found_base_num) {
	    printf (WENUM "    INTRN_$iname = $num,\n");
	} else {
	    printf (WENUM "    INTRN_$iname = INTRN_FIRST_F90_INTRINSIC + $num,\n");
	}
	printf (WNAME "    INTRN_$iname, \"$iname\",\n");

# output to wint_tool.h
	if ($funtype ne "") {
	    printf (WTOOL "$funtype(INTRN_$iname,$rty,\"$iname\",\"$lname\")\n");
	}
	$num += 1;
	$num_to_gen -= 1;
    }
}

$num -= 1;
if ($found_base_num) {
    printf (WENUM "\n    INTRN_LAST_F90_INTRINSIC = $num,\n");
} else {
    printf (WENUM "\n    INTRN_LAST_F90_INTRINSIC = INTRN_FIRST_F90_INTRINSIC + $num,\n");
}
close WENUM;

close WNAME;

close WTOOL;

close FDEFS;


sub gettypes {
    local ($intype) = @_;
    local ($outtype);
    
    $outtype = $intype;
    $outtype =~ s/\s//g;
    if ($intype eq "AF") {
	$outtype = "F4,F8,FQ";
    } elsif ($intype eq "AC") {
	$outtype = "C4,C8,CQ";
    } elsif ($intype eq "AC") {
	$outtype = "C4,C8,CQ";
    } elsif ($intype eq "AI") {
	$outtype = "I4,I8";
    } elsif ($intype eq "AJ") {
	$outtype = "I1,I2,I4,I8";
    }
    return $outtype;
}


sub open_files {
    open (WENUM, ">../com/wintrinsic90.h") || open (WENUM, ">../com/wintrinsic90.h.new") || die;
    open (WNAME, ">../com/wutil90.h") ||  open (WNAME, ">../com/wutil90.h.new") || die;
    open (WTOOL, ">wint_tool90.h") ||  open (WTOOL, ">wint_tool90.h.new") || die;
    open (FDEFS, "<f90intrindefs") || die;

    printf (WENUM "/* This file was automatically generated by f90generate.pl */\n\n");
    printf (WNAME "/* This file was automatically generated by f90generate.pl */\n\n");
    printf (WTOOL "/* This file was automatically generated by f90generate.pl */\n\n");
}

sub find_first_f90_intrinsic {
    open (WFILE, "<../com/wintrinsic.h") || die;
    $found_base_num = 0;
    while ($line = <WFILE>) {
	if ($line =~ /\s*INTRN_FIRST_F90_INTRINSIC\s*=\s*(\d*),/) {
	    $found_base_num = 1;
	    $num = $1;
	    close WFILE;
	    return;
	}
    }
    close WFILE;
}
