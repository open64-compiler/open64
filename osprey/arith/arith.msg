$ USMID @(#)20/arith.msg	20.0	10/17/97 15:51:01
$
$
$  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
$
$  This program is free software; you can redistribute it and/or modify it
$  under the terms of version 2 of the GNU General Public License as
$  published by the Free Software Foundation.
$
$  This program is distributed in the hope that it would be useful, but
$  WITHOUT ANY WARRANTY; without even the implied warranty of
$  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
$
$  Further, this software is distributed without any warranty that it is
$  free of the rightful claim of any third person regarding infringement 
$  or the like.  Any license provided herein, whether implied or 
$  otherwise, applies only to this software file.  Patent licenses, if 
$  any, provided herein do not apply to combinations of this program with 
$  other software, or any other product whatsoever.  
$
$  You should have received a copy of the GNU General Public License along
$  with this program; if not, write the Free Software Foundation, Inc., 59
$  Temple Place - Suite 330, Boston MA 02111-1307, USA.
$
$  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
$  Mountain View, CA 94043, or:
$
$  http://www.sgi.com
$
$  For further information regarding this notice, see:
$
$  http://oss.sgi.com/projects/GenInfo/NoticeExplan
$
$
$msg 2000 INTERNAL COMPILER ERROR:  %s %d:  shiftcount is out of range in ar_dblshift.
$nexp 2000
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  shiftcount is out of range in ar_dblshift.
.PP
While performing a shift operation, the compiler detected a shift count out
of the valid range.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2001 INTERNAL COMPILER ERROR:  %s %d:  shiftcount is out of range in ar_divide_integer.
$nexp 2001
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  shiftcount is out of range in ar_divide_integer.
.PP
While performing a shift during an integral divide operation, the compiler
detected a shift count out of the valid range.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2002 INTERNAL COMPILER ERROR:  %s %d:  invalid multiply and/or add in AR_convert_str_to_int.
$nexp 2002
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  invalid multiply and/or add in AR_convert_str_to_int.
.PP
While evaluating an integral constant value, the compiler detected an error in
an internal multiplication and/or addition operation.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2003 INTERNAL COMPILER ERROR:  %s %d:  invalid divide in AR_convert_int_to_str.
$nexp 2003
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  invalid divide in AR_convert_int_to_str.
.PP
While converting an integral value to a printable string, the compiler detected
an error in an internal multiplication and/or addition operation.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2004 INTERNAL COMPILER ERROR:  %s %d:  ar_clear_unused_bits called with floating or other invalid type.
$nexp 2004
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  ar_clear_unused_bits called with floating or other invalid type.
.PP
The compiler internal function, \*Car_clear_unused_bits\fR, was called with an
an invalid argument of floating type.  This function is called when evaluating
compile-time expressions.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2005 INTERNAL COMPILER ERROR:  %s %d:  AR_c2fapprox not loaded.
$nexp 2005
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  AR_c2fapprox not loaded.
.PP
The CRAY-2 reciprocal and reciprocal square root approximation emulator
function, \*CAR_c2fapprox\fR, is not part of the \*Carith\fR library built into
the compiler.  The compiler is unable to evaluate \s-1CRAY-2\s+1 arithmetic
expressions at compilation time.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2006 INTERNAL COMPILER ERROR:  %s %d:  double complex routines not loaded.
$nexp 2006
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  double complex routines not loaded.
.PP
The interface to the double precision complex routines in the math libraries is
not part of the \*Carith\fR library built into the compiler.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2007 INTERNAL COMPILER ERROR:  %s %d:  ARERP%%/merrp%% called within math library.
$nexp 2007
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  ARERP%%/merrp%% called within math library.
.PP
During compilation outside the constant folding library, a call to a math
library function aborted and tried to call the math library error processing
routine \*CARERP%\fR (or, on CRAY-2 systems, \*Cmerrp%\fR).  This behavior
sometimes results from division by zero in the compiler.
.PP
The \*CARERP%\fR routine is overridden in the compiler so that the compiler can
attempt to fold calls to some math library intrinsic functions without danger 
of aborting compilation entirely on bad data.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2008 INTERNAL COMPILER ERROR:  %s %d:  truncation bits out of range.
$nexp 2008
INTERNAL COMPILER ERROR:  \fIfilename linenum\*C:  truncation bits out of range.
.PP
The compiler internal function, \*CAR_CRAY_64_trunc\fR, was called with an
an invalid truncation value.  This function is called when evaluating
compile-time floating-point expressions.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2009 INTERNAL COMPILER ERROR:  %s %d:  cannot open intrinsic simulation file.
$nexp 2009
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  cannot open intrinsic simulation file.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The file
containing the intrinsic function instructions and data could not be opened.
It is apparently not accessible on this system.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2010 INTERNAL COMPILER ERROR:  %s %d:  invalid intrinsic simulation file.
$nexp 2010
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  invalid intrinsic simulation file.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The file
containing the intrinsic function instructions and data is not in the
correct format or cannot be read.  The format should be that of a
truncated executable a.out file on the target platform with version
information consistent with the released version of Craylibs.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2011 INTERNAL COMPILER ERROR:  %s %d:  unsupported intrinsic function.
$nexp 2011
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  unsupported intrinsic function.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  Not all intrinsic
functions are available to the compiler.  The compiler has requested the
evaluation of an intrinsic function which is not available.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2012 INTERNAL COMPILER ERROR:  %s %d:  simulated intrinsic function error.
$nexp 2012
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  simulated intrinsic function error.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  This simulation
uses the exact instructions and data from the libraries on the target system.
Some end case situations may not be allowed in the simulation.  For example,
calls to memory management routines to allocate more space is not simulated.
Calls to these routines have been stubbed out to return an error condition.
The most probable cause of this error is a very long input numeric character
string.  Other causes may be due to changes in the system library routines
themselves which have not been reflected in the simulation routines or
attempts to write over read-only intrinsic data.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2013 INTERNAL COMPILER ERROR:  %s %d:  unsimulated instruction encountered.
$nexp 2013
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  unsimulated instruction encountered.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  This simulation
does not simulate all of the instructions of the target system hardware--only
those necessary to simulate the supported intrinsic functions.  An instruction
has been encountered which is not simulated.  The most probable cause of this
error may be a new version of a system library intrinsic routine containing a
new instruction heretofore not simulated.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2014 INTERNAL COMPILER ERROR:  %s %d:  simulated intrinsic interface error.
$nexp 2014
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  simulated intrinsic interface error.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The file
containing the intrinsic function instructions and data has a new interface
to the simulated intrinsic functions.  This is incompatible with an older
interface with which the compiler was built.  The compiler needs to be
rebuilt with a later version of Craylibs which supports the new interface.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2015 INTERNAL COMPILER ERROR:  %s %d %s:  unsupported intrinsic function.
$nexp 2015
INTERNAL COMPILER ERROR:  \fIfilename linenum intrname\*F:  unsupported intrinsic function.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The compiler
has requested the evaluation of an unknown intrinsic function.  This may be
because a newer compiler is using an old version of the file containing the
intrinsic function instructions and data.  This older file does not contain
all of the intrinsic functions that the newer compiler wants to evaluate at
compile time.  A version of Craylibs that is compatible with the newer
compiler needs to be installed.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2016 WARNING:  %s simulation:  potentially different intrinsic function values.
$nexp 2016
WARNING:  \fIarchitecture\*F simulation:  potentially different intrinsic function values.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The evaluation
performed at compile time is using a different version of the intrinsic
library than that which may be used to load the compiled program.  As a
consequence, slightly different numerical results are possible depending
upon which intrinsic functions are used and what changes have occurred
in the Craylibs libraries.  Generally the version of Craylibs released
with a compiler should be used with that compiler.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2017 INTERNAL COMPILER ERROR:  %s %d:  intrinsic routine not loaded.
$nexp 2017
INTERNAL COMPILER ERROR:  \fIintrinsicname linenum\*C:  intrinsic routine not loaded.
.PP
The interface to the given intrinsic routine in the math libraries is
not part of the \*Carith\fR library built into the compiler.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2018 INTERNAL COMPILER ERROR:  %s %d:  inconsistent floating point format.
$nexp 2018
INTERNAL COMPILER ERROR:  \fIfilename linenum\*F:  inconsistent floating point format.
.PP
The compiler simulates intrinsic functions for the target platform when it
encounters constant expressions involving these intrinsics.  The file
containing the intrinsic function instructions and data uses a floating
point format that differs from that requested by the compiler.  For example,
the opened file uses Cray floating point format, but the compiler is requesting
an IEEE floating point result.
.PP
This message should never be generated.  Notify your system support staff with
this error message number and any supporting information.  This message does not
indicate a problem with your code, although you may be able to change your code
so that this error is not encountered.
.ME
$msg 2019 INTERNAL COMPILER ERROR:  Internal error from target library.
$nexp 2019
INTERNAL COMPILER ERROR:  Internal error from target library.
.PP
The compiler is unable to determine either the host
or the target architecture for the compilation.
The library routine that should have yielded that information
has instead returned an error code.
.PP
This message should never be generated.
Notify your system support staff with this error message number
and any supporting information.
This message does not indicate a problem with your code,
although you may be able to change your code
so that this error is not encountered.
.ME
