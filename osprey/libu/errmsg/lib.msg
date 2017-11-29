$ USMID @(#) libu/errmsg/lib.msg	92.6	04/29/99 14:04:27
$
$  Copyright (C) 2007. PathScale, LLC. All Rights Reserved.
$

$
$  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
$

$
$
$  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
$
$  This program is free software; you can redistribute it and/or modify it
$  under the terms of version 2.1 of the GNU Lesser General Public License 
$  as published by the Free Software Foundation.
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
$  You should have received a copy of the GNU Lesser General Public 
$  License along with this program; if not, write the Free Software 
$  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
$  USA.
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

#define	SYMSONLY

#include "numbers.h"

$
$ lib.msg - Library error messages
$
$	Library error messages (BASE - BASE+999)
$
$	Flowtrace error messages (3000 - 3199)  UNICOS only
$
$	FFIO error messages (5000 - 5999)
$
$
$	Message guidelines:
$
$	    1)	All messages and explanations must be in ascending order.
$
$	    2)	The first line of the explanation should be the same as
$		the error message (so that the user, when using explain(1),
$		knows he or she is getting the desired explanation.  Note
$		that continuations of $msg and $nexp lines must be properly
$		escaped:
$
$		$msg n Now is the time for all good men\n\
$		  to come to the aid of their country.
$		$nexp n Now is the time for all good men \
$		to come to the aid of their country.
$
$	    3)  The "sg" nroff macro package (nroff -msg) is used to process
$		the text of the error explanations.  More information
$		about "sg" macors is available:
$
$		    1) on UNICOS, type "man 7d msg" 
$
$		    2) on SUN/Solaris, type 
$
$			module load sistools
$			format -xp ~dev/async/uman/82/uman/man/man7/msg.7d
$
$		Since nroff processes the explanation text, appropriate 
$		safeguards must be taken to protect special characters.   
$		For example, any apostrophes which follows whitespace must
$		be protected with a \& sequence.  Otherwise text formatter
$		macros might move the apostrophe to the beginning of a line
$		and cause it to behave like a comment (with resultant loss
$		of text).   
$
$		\*F'SEQUENTIAL'\fR	-->	\*F\&'SEQUENTIAL'\fR
$		\*F'FORMATTED   '\fR	-->	\*F\&'FORMATTED   \&'\fR
$
$	    4)  Deprecated messages use a symbolic identifier of FE__NNNN,
$		as per <liberrno.h>.  They should be retained for at least
$		one release beyond their last use (the explanation should
$		indicate in which release they were deprecated).  This will
$		allow existing binaries to continue to get the proper
$		error messages and explanations.
$
$msg FERDPEOF A READ operation tried to read past the end-of-file.
$nexp FERDPEOF
'\"-----------------------------------------------------------------------------
'\" 
'\"      	NROFF STRING AND MACRO DEFINITIONS
'\" 
'\"  The following nroff/troff string and macro definitions must be included in 
'\"  the first $nexp section of the message catalog to ensure that they are 
'\"  defined throughout.
'\"
.ds aL the \fIApplication Programmer's Library Reference Manual\fR, publication \s-1SR\-2165\s+1
.ds aI the \fI\Application Programmer's \s-1I/O\s+1 Guide\fR, publication \s-1SG\-2168\s+1
.ds uL the \fI-1UNICOS\s+1*u System Library Reference Manual\fR, publication \s-1SR\-2080\s+1
'\"
'\"  The Ma macro takes one or more arguments.  Argument 1 is a mnemonic which
'\"  gets expanded, and optional argument 2 is used in various ways which 
'\"  depend on argument 1.
'\"
.de Ma
.ds zZ ERR
.if "\\$1"CRI_ONLY" \{\
This error message is issued only on Cray \s-1MPP\s+1 and \%\s-1CRAY\ PVP\s+1
systems.\\$2
.ds zZ OK\}
'\"
.if "\\$1"NONUNICOS_ONLY" \{\
This error message is issued only on IRIX systems.\\$2
.ds zZ OK\}
'\"
.if "\\$1"UNRECOVERABLE" \{\
The error class is \s-1UNRECOVERABLE\s+1 (issued by the run-time library).\\$2
.ds zZ OK\}
'\"
.if "\\$1"WARNING" \{\
The error class is \s-1WARNING\s+1 (issued by the run-time library).
This message is issued as a warning.  Execution of the program or
command will continue.\\$2
.ds zZ OK\}
'\"
.if "\\$1"RETIRED" \{\
This error message is no longer issued in CrayLibs \\$2 and later releases.
.ds zZ OK\}
\"
.if "\\*(zZ"ERR" .ab FORMAT ERROR: the .Ma macro is passed an unrecognized argument: \\$1
..  \" end of .Ma macro definition
'\"
'\"-----------------------------------------------------------------------------
A READ operation tried to read past the end-of-file.
.PP
A Fortran \*FREAD\fR operation encountered the endfile record
(end-of-file),
and neither an \*FEND\fR nor an \*FIOSTAT\fR
specifier was present on the \*FREAD\fR statement.
.PP
Either 1) add an \*FEND=\fIs\fR specifier
(\fIs\fR is a statement label)
and/or an \*FIOSTAT=\fIi\fR specifier
(\fIi\fR is an integer variable) to the \*FREAD\fR statement,
or 2) modify the program so that it reads no more data than is in the file.
.PP
For more information, see the input/output section of your Fortran
reference manual.
.PP
Because this is an end-of-file condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef KEY /* F2003 */
$msg FE__RDEMPT A READ operation tried to read an empty file.
$nexp FE__RDEMPT
#else /* KEY F2003 */
$msg FERDEMPT A READ operation tried to read an empty file.
$nexp FERDEMPT
#endif /* KEY F2003 */
A READ operation tried to read an empty file.
.PP
A Fortran \*FREAD\fR operation read an empty file, and neither
an \*FEND\fR nor an \*FIOSTAT\fR specifier was present
on the \*FREAD\fR statement.
.PP
Either 1) add an \*FEND=\fIs\fR specifier
(\fIs\fR is a statement label)
and/or an \*FIOSTAT=\fIi\fR specifier
(\fIi\fR is an integer variable) to the \*FREAD\fR
statement,
or 2) modify the program so that it reads no more data than is in the file.
Check the data file being opened to ensure that it is the correct
file, and ensure that the program/process that creates the file is
functioning correctly.
.PP
For more information, see the input/output section of your
Fortran reference manual.
.PP
Because this is an end-of-file condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef KEY /* F2003 */
$msg FE__RDENDR A READ is invalid when a file is positioned after the end-of-file.
$nexp FE__RDENDR
#else /* KEY F2003 */
$msg FERDENDR A READ is invalid when a file is positioned after the end-of-file.
$nexp FERDENDR
#endif /* KEY F2003 */
A READ is invalid when a file is positioned after the end-of-file.
.PP
A Fortran \*FREAD\fR operation was initiated on a
file that was positioned after the endfile record (end-of-file).
.PP
Either 1) add an \*FEND=\fIs\fR specifier
(\fIs\fR is a statement label) and/or an \*FIOSTAT=\fIi\fR
specifier (\fIi\fR is an integer variable)
to the \*FREAD\fR statement,
or 2) modify the program so that it does not read past the endfile record.  This
error can occur only after the endfile record is read.
.PP
For more information, see the input/output section of your
Fortran reference manual.
.PP
Because this is an end-of-file condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef KEY /* F2003 */
$msg FE__RDNLEF A namelist READ operation tried to read past the end-of-file.
$nexp FE__RDNLEF
#else /* KEY F2003 */
$msg FERDNLEF A namelist READ operation tried to read past the end-of-file.
$nexp FERDNLEF
#endif /* KEY F2003 */
A namelist READ operation tried to read past the end-of-file.
.PP
A Fortran namelist \*FREAD\fR operation encountered the endfile record,
and an \*FEND\fR specifier was not included on the namelist
\*FREAD\fR statement.
.PP
Add an \*FEND=\fIs\fR specifier
(\fIs\fR is a statement label) to the
\*FREAD\fR statement, or modify the program so that it reads no more data than
is in the file.
.PP
Because this is an end-of-file condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
For more information, see the description of the \*FNAMELIST\fR
statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef KEY /* F2003 */
$msg FE__RDIEOF A READ operation on an internal file tried to read past the end-of-file.
$nexp FE__RDIEOF
#else /* KEY F2003 */
$msg FERDIEOF A READ operation on an internal file tried to read past the end-of-file.
$nexp FERDIEOF
#endif /* KEY F2003 */
A READ operation on an internal file tried to read past the end-of-file.
.PP
A Fortran \*FREAD\fR operation tried to read beyond the end of the internal
file, and neither an \*FEND\fR nor an \*FIOSTAT\fR specifier was included
on the internal \*FREAD\fR statement.
.PP
Either 1) add an \*FEND=\fIs\fR specifier
(\fIs\fR is a statement label) and/or an \*FIOSTAT=\fIi\fR
specifier (\fIi\fR is an integer variable)
to the \*FREAD\fR statement,
or 2) modify the program so that it does not read beyond the end of the
internal file.
.PP
Because this is an end-of-file condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
For more information, see the description of internal records and files
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEEORCND An end-of-record condition occurred when processing a nonadvancing READ statement.
$nexp FEEORCND
An end-of-record condition occurred when processing a nonadvancing READ statement.
.PP
An end-of-record condition occurs when a nonadvancing Fortran \*FREAD\fR 
statement tries to read beyond the end of a record.
.PP
You can suppress the end-of-record condition by using
an advancing Fortran \*FREAD\fR statement without \*FADVANCE='NO'\fR specified
or by changing the Fortran \s-1I/O\s+1 statement to prevent the reading of 
data from a position beyond the end of the record.
.PP
Because this is an end-of-record condition, the negative of this error
number is returned in the \*FIOSTAT\fR variable, if specified.
.PP
For more information, see the description of nonadvancing \s-1I/O\s+1 
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Invalid unit number/record number errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____10 The unit number (%d) must be nonnegative.
$nexp FE____10
The unit number (\fIun\*F) must be nonnegative.
.PP
Modify the Fortran unit number so that it is in the range of valid unit
numbers.
.PP
Special unit numbers 100 through 102 are associated with \*Cstdin\fR,
\*Cstdout\fR, and \*Cstderr\fR.  These units may not be closed or reopened.  
.PP
.Ma RETIRED 3.0.2.1
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____11 The unit number (%d) in the OPEN statement must be nonnegative and outside\n\
  the range of 100 through 102.
$nexp FE____11
The unit number (\fIun\*F) in the OPEN statement must be nonnegative and \
outside the range of 100 through 102.
.PP
Modify the Fortran unit number so that it is in the range of valid unit
numbers for Fortran \*FOPEN\fR statements.
.PP
Special unit numbers 100 through 102 are associated with \*Cstdin\fR,
\*Cstdout\fR, and \*Cstderr\fR.  These units may not be closed or reopened.  
.PP
.Ma RETIRED 3.0.2.1
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEIVUNIT The unit number (%lld) must be nonnegative.
$nexp FEIVUNIT
The unit number (\fIun\*F) must be nonnegative.
.PP
Modify the Fortran unit number so that it is in the range of valid unit
numbers.
.PP
Special unit numbers 100 through 102 are associated with \*Cstdin\fR,
\*Cstdout\fR, and \*Cstderr\fR.  These units may not be closed or reopened.  
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEIVUNTO The unit number (%lld) in the OPEN statement must be nonnegative and outside\n\
  the range of 100 through 102.
$nexp FEIVUNTO
The unit number (\fIun\*F) in the OPEN statement must be nonnegative and \
outside the range of 100 through 102.
.PP
Modify the Fortran unit number so that it is in the range of valid unit
numbers for Fortran \*FOPEN\fR statements.
.PP
Special unit numbers 100 through 102 are associated with \*Cstdin\fR,
\*Cstdout\fR, and \*Cstderr\fR.  These units may not be closed or reopened.  
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOTOPN The requested action requires that unit %lld be connected to a file.
$nexp FENOTOPN
The requested action requires that unit \&'\fIun\*F\&' be connected to a file.
.PP
The operation attempted can be requested only for a connected
Fortran unit.   A Fortran unit can become connected by the execution
of an \*FOPEN\fR statement or when a Fortran \*FREAD\fR or \*FWRITE\fR statement
implicitly opens the file.
.PP
Ensure that the Fortran unit is connected to a file before
trying the requested operation.
.PP
See the description of the \*FOPEN\fR statement in your Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEIVRECN The record number (%lld) must be a positive (nonzero) integer.
$nexp FEIVRECN
The record number (\fIrecn\*F) must be a positive (nonzero) integer.
.PP
On a Fortran \*FREAD\fR or \*FWRITE\fR statement, the \*FREC\fR (record number)
specifier was negative or 0.  It must be a positive integer corresponding to
the desired record number.
Check the value of the \*FREC\fR specifier to ensure that it is a valid
record number.
.PP
Ensure that the correct file is being read or written.
.PP
See the description of input/output statements in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENORECN A READ operation tried to read a nonexistent record (%lld).
$nexp FENORECN
A READ operation tried to read a nonexistent record (\fIrecn\*C).
.PP
On a Fortran \*FREAD\fR statement, the \*FREC\fR (record number) specifier
was larger than the largest record number for that direct-access file.
.PP
Check the value of the \*FREC\fR specifier to ensure that it is a valid
record number.  Check the file being read to ensure that it is the
correct file.
.PP
See the description of input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Implicit OPEN failures
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FECONNDA The unit is not connected for direct access; use an OPEN statement.
$nexp FECONNDA
The unit is not connected for direct access; use an OPEN statement.
.PP
A direct-access \s-1I/O\s+1 operation (\*FREAD\fR or \*FWRITE\fR
with the \*FREC=\fIn\fR
specifier) was attempted on an unconnected unit.  The unit must be
explicitly opened for direct-access \s-1I/O\s+1 with a Fortran \*FOPEN\fR
statement.
.PP
Either ensure that the Fortran unit is connected for direct-access \s-1I/O\s+1
with a Fortran \*FOPEN\fR statement containing
\*FACCESS='DIRECT',RECL=\fIn\fR,
or remove the \*FREC\fR specifier and use sequential-access \s-1I/O\s+1.
.PP
For more information, see the section on input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FECONNTP The requested action requires that the unit be connected to a tape file.
$nexp FECONNTP
The requested action requires that the \
unit be connected to a tape file.
.PP
An operation permitted only on a tape file was attempted on a file
that is not a tape file.
.PP
Ensure that the Fortran unit is connected to a tape file by using
the \*Ftpmnt\fR(1) and \*Fassign\fR(1) commands.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____23 The requested action requires that unit %d be connected to a file.
$nexp FE____23
The requested action requires that unit \&'\fIun\*F\&' be connected to a file.
.PP
The operation attempted can be requested only for a connected
Fortran unit.   A Fortran unit can become connected by the execution
of an \*FOPEN\fR statement or when a Fortran \*FREAD\fR or \*FWRITE\fR statement
implicitly opens the file.
.PP
Ensure that the Fortran unit is connected to a file before
trying the requested operation.
.PP
See the description of the \*FOPEN\fR statement in your Fortran
reference manual.
.PP
.Ma RETIRED 3.0.2.1
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____24 The named file cannot be opened. The maximum number of named files is already open.
$nexp FE____24
The named file cannot be opened. The maximum number \
of named files is already open.
.PP
The program attempted an explicit or implicit open of a named file.  The program
has already opened the maximum number of named files.
.PP
In \s-1UNICOS\s+1 7.0 and later releases, the maximum number of named files
that can be opened at the same time is configurable with \*Fsegldr\fR directives.
Your system administrator can change the site defaults, or you can use your
own directives.  Increasing the maximum number of named files will increase
the size of your program.
.PP
If possible, close some of the named files, or use \*Fsegldr\fR directives to 
increase the maximum number of named files that can be opened.
.PP
See the description of the \*FOPEN\fR statement in your Fortran
reference manual.  
Also see 
\fI\s-1UNICOS\s+1 System Administration\fR, publication \s-1SG\-2113\s+1.
.PP
.Ma RETIRED 3.0
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPOTHR The open failed because the file is already open for a different type of I/O.
$nexp FEOPOTHR
The open failed because the file is already \
open for a different type of I/O.
.PP
The program attempted to open a file for asynchronous queued \s-1I/O\s+1 
(\*FAQOPEN\fR(3)),
word addressable \s-1I/O\s+1 (\*FWOPEN\fR(3)), \s-1MSIO\s+1 (\*FOPENMS\fR(3)),
or 
\s-1DRIO\s+1 (\*FOPENDR\fR) and the
file is already opened for another type of \s-1I/O\s+1.  For example,
the following is not valid:
.CS
OPEN(10,form='unformatted')
CALL AQOPEN(aqp, aqpsize, 10, istat)
.CE
.PP
If possible, close the file before trying the open that produced this
error.
.PP
See the appropriate description in
your Fortran reference manual.
Also see \*(aI.
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPAUXT The open failed because the file is already opened for a different type of I/O.
$nexp FEOPAUXT
The open failed because the file is already \
opened for a different type of I/O. 
.PP
A Fortran \*FOPEN\fR is not valid if the file has been opened for asynchronous
queued \s-1I/O\s+1 (\*FAQOPEN\fR(3)), word addressable \s-1I/O\s+1
(\*FWOPEN\fR(3)), \s-1MSIO\s+1 (\*FOPENMS\fR(3)), or \s-1DRIO\s+1 (\*FOPENDR\fR).
For example, the following is not valid and will produce this error:
.CS
CALL AQOPEN(aqp, aqpsize, 10, istat)
OPEN(10,form='unformatted')
.CE
.PP
If possible, close the file before trying the open that produced this
error.
.PP
See the appropriate description in
your Fortran reference manual.
Also see \*(aI.
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFILACT The system file permissions are incompatible with the ACTION specifier value\n\
  in the OPEN statement.
$nexp FEFILACT
The system file permissions are incompatible with the ACTION specifier \
value in the OPEN statement.
.PP
A file must have system file permissions that permit the \*FREAD\fR or 
\*FWRITE\fR 
actions specified with the \*FACTION\fR specifier in an \*FOPEN\fR statement. 
.PP
When connecting to standard input, standard output, and standard error, the
\*FACTION\fR specifier must have the values \*FREAD\fR, \*FWRITE\fR, and
\*FWRITE\fR, respectively.  
.PP
Either use the \*Fchmod\fR(1) command to change the file permissions, or change the 
\*FACTION\fR specifier value in the \*FOPEN\fR statement. 
.PP
For a description of the \*FOPEN\fR statement specifiers, see your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOSKPB The file does not support automatic skipping of bad data.
$nexp FENOSKPB
The file does not support automatic skipping of bad data.
.PP
Automatic skipping of bad data was specified for a file that does not
support this operation.  Automatic skipping of bad data is specified
with the \*F-d\fR option of the \*Fassign\fR(1) command.
.PP 
Change the \*Fassign\fR(1) command specification.
.PP 
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESTIOER An underlying C library read or write request failed.
$nexp FESTIOER
An underlying C library read or write request failed.
.PP
For default formatted files, or for files assigned the -s text or -s sbin
attributes, the Fortran runtime library uses underlying requests to C library
functions such as fread(3), fwrite(3), getc(3), and putc(3).  One
of these functions returned an error status.
.PP
The error may have been due to a system I/O error, a filesystem or quota
limit, or inability to allocate a library buffer for this file.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	OPEN specifier errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPSTAT The STATUS specified on OPEN must be 'OLD', 'NEW', 'SCRATCH', or 'UNKNOWN'.
$nexp FEOPSTAT
The STATUS specified on OPEN must \
be \&'OLD', \&'NEW', \&'SCRATCH', or \&'UNKNOWN'.
.PP
A Fortran \*FOPEN\fR statement used an
unrecognizable \*FSTATUS\fR.  If specified, the \*FSTATUS\fR must
be \*F\&'OLD'\fR, \*F\&'NEW'\fR, \*F\&'SCRATCH'\fR,
or \*F\&'UNKNOWN'\fR.  When \*FSTATUS\fR is not
specified, \*F\&'UNKNOWN'\fR is assumed.
Trailing blanks are allowed in the value (for example, \*F\&'OLD   \&'\fR).
.PP
Ensure that the \*FSTATUS\fR is spelled correctly.
.PP
See the description of the \*FOPEN\fR statement in your Fortran
reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPACCS The ACCESS specified on OPEN must be 'SEQUENTIAL' or 'DIRECT'.
$nexp FEOPACCS
The ACCESS specified on OPEN must be \&'SEQUENTIAL' or \&'DIRECT'.
.PP
A Fortran \*FOPEN\fR statement used an
unrecognizable \*FACCESS\fR.  If specified, the \*FACCESS\fR must be
\*F\&'SEQUENTIAL'\fR or \*F\&'DIRECT'\fR.
When \*FACCESS\fR is not specified, \*F\&'SEQUENTIAL'\fR is assumed.
Trailing blanks are allowed in the value (for example, \*F\&'DIRECT    \&'\fR).
.PP
Ensure that the \*FACCESS\fR is spelled correctly.
.PP
Also see the description of the \*FOPEN\fR statement
in your Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPFORM The FORM specified on OPEN must be 'FORMATTED' or 'UNFORMATTED'.
$nexp FEOPFORM
The FORM specified on OPEN must be \&'FORMATTED' or \&'UNFORMATTED'.
.PP
A Fortran \*FOPEN\fR statement used an
unrecognizable \*FFORM\fR.  If specified, the \*FFORM\fR must be
\*F\&'FORMATTED'\fR
or \*F\&'UNFORMATTED'\fR.  When \*FFORM\fR is
not specified, \*F\&'FORMATTED'\fR is assumed if the file is opened for
sequential-access \s-1I/O\s+1, and \*F\&'UNFORMATTED'\fR is assumed if the 
file is opened for direct-access \s-1I/O\s+1.  Trailing blanks are allowed 
in the value (for example,
.br
\%\*F\&'FORMATTED\   \&'\fR).
.PP
Ensure that the \*FFORM\fR is spelled correctly.
.PP
See the description of the \*FOPEN\fR statement
in your Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPRECL The RECL specified on OPEN must be a positive (nonzero) integer.
$nexp FEOPRECL
The RECL specified on OPEN must be a positive (nonzero) integer.
.PP
A Fortran \*FOPEN\fR statement used a \*FRECL\fR that was not valid.
The \*FRECL\fR, which
is required on direct-access files, must be a positive (nonzero)
integer.
.PP
Ensure that the \*FRECL\fR is correct.
.PP
See the description of the \*FOPEN\fR statement in
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPBLNK The BLANK specifier on OPEN must be 'NULL' or 'ZERO'.
$nexp FEOPBLNK
The BLANK specifier on OPEN must be \&'NULL' or \&'ZERO'.
.PP
A Fortran \*FOPEN\fR statement used an
unrecognizable \*FBLANK\fR.  The \*FBLANK\fR
specifier must be \*F\&'NULL'\fR or \*F\&'ZERO'\fR.
When \*FBLANK\fR is not specified,
\*F\&'NULL'\fR is assumed.  Trailing blanks are allowed in the value
(for example, \*F\&'ZERO   \&'\fR).
.PP
Ensure that the \*FBLANK\fR specifier value is spelled correctly.
.PP
See the \*FOPEN\fR description in your
Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPPOSN The POSITION specified on OPEN must be 'REWIND', 'APPEND', or 'ASIS'.
$nexp FEOPPOSN
The POSITION specified on OPEN must be \&'REWIND', \&'APPEND', or \&'ASIS'.
.PP
A Fortran \*FOPEN\fR statement used an
unrecognizable \*FPOSITION\fR.  If specified, the
\*FPOSITION\fR must be \*F\&'REWIND'\fR,
\*F\&'APPEND'\fR, or \*F\&'ASIS'\fR.
When \*FPOSITION\fR is not specified, \*F\&'ASIS'\fR is assumed.
Trailing blanks
are allowed (for example, \*F\&'ASIS   \&'\fR).
.PP
Ensure that the \*FPOSITION\fR is spelled correctly.
.PP
See the \*FOPEN\fR description in your Fortran reference
manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPACTB The ACTION specifier on OPEN must be 'READ', 'WRITE', or 'READWRITE'.
$nexp FEOPACTB
The ACTION specifier on OPEN must be \&'READ', \&'WRITE', or \&'READWRITE'.
.PP
A Fortran \*FOPEN\fR statement specified an unrecognizable value
for the \*FACTION\fR specifier.  The value must be \*F\&'READ'\fR, 
\*F\&'WRITE'\fR, or \*F\&'READWRITE'\fR.  Trailing blanks are allowed (for 
example, \*F\&'WRITE   \&'\fR).
When \*FACTION\fR is not specified, the default depends on the file.
.PP
Ensure that the \*FACTION\fR specifier is spelled correctly.
.PP 
See the \*FOPEN\fR statement in your Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPDLMB The DELIM specifier on OPEN must be 'QUOTE', 'APOSTROPHE', or 'NONE'.
$nexp FEOPDLMB
The DELIM specifier on OPEN must be \&'QUOTE', \&'APOSTROPHE', or \&'NONE'.
.PP
A Fortran \*FOPEN\fR statement specified an unrecognizable value for
the \*FDELIM\fR specifier.  The value must be \*F\&'QUOTE'\fR, \*F\&'APOSTROPHE'\fR,
or \*F\&'NONE'\fR.  Trailing blanks are allowed (for example,
\*F\&'QUOTE   \&'\fR).  When  
\*FDELIM\fR is not specified, the default is \*F\&'NONE'\fR.
.PP
Ensure that the \*FDELIM\fR specifier is spelled correctly.
.PP 
See the \*FOPEN\fR statement in your Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	OPEN conflict errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPFNRQ A FILE specifier is required on OPEN if STATUS is 'OLD', 'NEW', or 'REPLACE'.
$nexp FEOPFNRQ
A FILE specifier is required on OPEN if STATUS is \&'OLD', \&'NEW', or \&'REPLACE'.
.PP
A Fortran \*FOPEN\fR statement contained a \*FSTATUS\fR
of \*F\&'OLD'\fR, \*F\&'NEW'\fR, or \*F\&'REPLACE'\fR
but did not contain a \*FFILE\fR specifier.
A \*FFILE\fR specifier is
required for files opened with these \*FSTATUS\fR specifier values.
.PP
Either add a \*FFILE\fR specifier or change \*FSTATUS\fR
to \*F\&'UNKNOWN'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPFNIV A FILE specifier is invalid on OPEN if STATUS is 'SCRATCH'.
$nexp FEOPFNIV
A FILE specifier is invalid on OPEN if STATUS is \&'SCRATCH'.
.PP
A Fortran \*FOPEN\fR statement contained a \*FSTATUS\fR
of \*F\&'SCRATCH'\fR and a
\*FFILE\fR specifier.  The \*FFILE\fR
specifier cannot be used with \*FSCRATCH\fR files.
.PP
Either remove the \*FFILE\fR specifier or change 
\*FSTATUS\fR to \*F\&'NEW'\fR and
close the file with \*FSTATUS='DELETE'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
A \*FFILE\fR specifier may be used with scratch
files on \s-1Irix\s+1 and \s-1Cray PVP\s+1 systems to allow assign-by-file
to function. The assign-by-unit capability
removes the need for this extension to the current Fortran standard.
Also see the \*Fassign\fR(1) command.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPRCRQ A RECL specifier is required on OPEN of a direct-access file.
$nexp FEOPRCRQ
A RECL specifier is required on OPEN of a direct-access file.
.PP
A Fortran \*FOPEN\fR statement contained an \*FACCESS\fR
of \*F\&'DIRECT'\fR but did
not contain a \*FRECL\fR specifier.  A \*FRECL\fR
specifier is required for direct-access files.
.PP
Either add a \*FRECL\fR specifier or change 
\*FACCESS\fR to \*F\&'SEQUENTIAL'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPBKIV A BLANK specifier is invalid on OPEN of an unformatted file.
$nexp FEOPBKIV
A BLANK specifier is invalid on OPEN of an unformatted file.
.PP
A Fortran \*FOPEN\fR statement contained a \*FFORM\fR
of \*F\&'UNFORMATTED'\fR (or
did not contain a \*FFORM\fR) and a \*FBLANK\fR specifier.
A \*FBLANK\fR specifier is not valid for unformatted files.
.PP
Either remove the \*FBLANK\fR specifier or change (or add) 
\*FFORM\fR to \*F\&'FORMATTED'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPPSIV A POSITION specifier is invalid on OPEN of a direct-access file.
$nexp FEOPPSIV
A POSITION specifier is invalid on OPEN of a direct-access file.
.PP
A Fortran \*FOPEN\fR statement contained an \*FACCESS\fR
of \*F\&'DIRECT'\fR and a \*FPOSITION\fR specifier.
A \*FPOSITION\fR specifier is not valid for direct-access files.
.PP
Either remove the \*FPOSITION\fR specifier or change \*FACCESS\fR
to \*F\&'SEQUENTIAL'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPASGN The assign and asgcmd commands are in conflict.
$nexp FEOPASGN
The assign and asgcmd commands are in conflict.
.PP
A conflict exists between the \*Fassign\fR(1) and \*Fasgcmd\fR(1) commands.
The Fortran run-time library cannot process attributes assigned to the same
file by both commands.
.PP
Remove the \*Fassign\fR and/or \*Fasgcmd\fR specification.
.PP
See Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____47 Concurrent assigns to unit %d and file '%s' are in conflict.
$nexp FE____47 
Concurrent assigns to unit \fIun\*C and file \&'\fIfn\*C' are in conflict.
.PP
The file name and the unit number in the current Fortran \*FOPEN\fR
statement or implicit open both have \*Fassign\fR(1) commands in
effect.  The Fortran run-time library requires that only one
\*Fassign\fR be in effect for any Fortran open operation.
.PP
Remove the \*Fassign\fR specification for either the unit or the file.
.PP
See the description for \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPPADB The PAD specifier on OPEN must be 'YES' or 'NO'.
$nexp FEOPPADB
The PAD specifier on OPEN must be \&'YES' or \&'NO'.
.PP
An \*FOPEN\fR statement specified an unrecognizable value
for the \*FPAD\fR specifier.  The value must be either \*F\&'YES'\fR or \*F\&'NO'\fR.
Trailing blanks are allowed.
If \*FPAD\fR
is not specified, the default is \*F\&'YES'\fR.
.PP
Ensure that the \*FPAD\fR specifier is spelled correctly.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPDLMI A DELIM specifier is invalid in an OPEN of an unformatted file.
$nexp FEOPDLMI
A DELIM specifier is invalid in an OPEN of an unformatted file.
.PP
A Fortran \*FOPEN\fR statement contains a \*FFORM\fR specifier of
\*F\&'UNFORMATTED'\fR (or a default value of unformatted) and a \*FDELIM\fR
specifier.  A \*FDELIM\fR specifier is not valid for unformatted files.
.PP
Either remove the \*FDELIM\fR specifier or change (or add) a \*FFORM\fR
specifier with the value \*F\&'FORMATTED'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$ 
$	OPEN failures 
$ 
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____50 The file must exist prior to OPEN if STATUS is 'OLD'.
$nexp FE____50
The file must exist prior to OPEN if STATUS is \&'OLD'.
.PP
An \*FOPEN\fR statement with a \*FSTATUS\fR
of \*F\&'OLD'\fR is expecting that the specified file already exists.
.PP
Either ensure that the file exists before the \*FOPEN\fR
(check the \*FFILE\fR
specifier) or change \*FSTATUS\fR to \*F\&'NEW'\fR
or \*F\&'UNKNOWN'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPFNNX The file must not exist prior to OPEN if STATUS is 'NEW'.
$nexp FEOPFNNX
The file must not exist prior to OPEN if STATUS is \&'NEW'.
.PP
An \*FOPEN\fR statement with a \*FSTATUS\fR
of \*F\&'NEW'\fR is expecting that the specified file does not exist.
.PP
Either ensure that the file does not exist before the \*FOPEN\fR (check the
\*FFILE\fR specifier) or change \*FSTATUS\fR
to \*F\&'OLD'\fR or \*F\&'UNKNOWN'\fR.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPFNCN The specified file is already connected to another unit.
$nexp FEOPFNCN
The specified file is already connected to another unit.
.PP
An \*FOPEN\fR statement attempted to connect a unit to a file that was
already connected to another unit.  The current Fortran standard
requires that files be connected to only one unit at a time.
.PP
Close the file before trying to connect it to another unit.
Check the \*FFILE\fR specifier to ensure that the proper file is being
selected.
.PP
See the description of the \*FOPEN\fR statement in your
Fortran reference manual.
.PP
The standard files (standard input, standard output,
and standard error) may be connected to up to two units simultaneously.
This is an extension to the current Fortran standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPCBNK Only the BLANK, DELIM and PAD specifiers may be changed on an OPEN of an opened file.
$nexp FEOPCBNK
Only the BLANK, DELIM and PAD specifiers may be changed on an OPEN of an opened file.
.PP
An \*FOPEN\fR statement on an already opened unit attempted to change
an attribute of the connection other than the \*FBLANK\fR, \*FDELIM\fR and
\*FPAD\fR specifiers.
Only these specifiers can be changed on a unit that is already connected.
All other specifiers on the \*FOPEN\fR statement, if selected, must match
those on the original \*FOPEN\fR statement and must not change the existing
attributes of the connection.  For example, a \*FPOSITION\fR of
\*F\&'REWIND'\fR is an attempt to change the existing attributes of a file
connection if the file is not rewound at the time of the subsequent
\*FOPEN\fR statement.
.PP
Either remove all but the \*FBLANK\fR, \*FDELIM\fR, \*FPAD\fR and \*FUNIT\fR
specifiers on the \*FOPEN\fR statement or explicitly
\*FCLOSE\fR the file and reopen it with another \*FOPEN\fR statement.
.PP
See the description of the \*FOPEN\fR statement in
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPNOFS The file cannot be opened (%s file structure does not support\n\
  %s %s access).
$nexp FEOPNOFS
The file cannot be opened (\&'fs' file structure does not support \
\&'fmt' \&'acc' access).
.PP
An \*FOPEN\fR statement failed because the specified file structure (\*Ffs\fR)
cannot support the type of access (\*Ffmt\fR and \*Facc\fR) requested.
This occurs only if the \*Fassign\fR(1) or \*Fasgcmd\fR(1) command selects a
nondefault file structure that does not support the type of access
desired (for example, text file structure selected for unformatted,
direct-access \s-1I/O\s+1).
.PP
Check the \*Fassign\fR(1) or \*Fasgcmd\fR(1) attributes in effect for the unit
and file to ensure that they do not preclude the type of access being
requested.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPNNDA Direct access is not allowed with the specified -s or -F\n\
  assign/asgcmd option.
$nexp FEOPNNDA
Direct access is not allowed with the \
specified -s or -F assign/asgcmd option.
.PP
An \*FOPEN\fR statement failed because the file could not be opened for
direct-access \s-1I/O\s+1.
This can occur if an \*Fassign\fR(1) or \*Fasgcmd\fR(1)
command selects a file structure or filter layer that cannot support
direct-access operations.
.PP
Check the \*Fassign\fR(1) or \*Fasgcmd\fR(1) attributes in effect for
the file to ensure that they do not preclude direct-access operations.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPCAPY The file cannot be opened (FFIO options do not support this type of OPEN).
$nexp FEOPCAPY
The file cannot be opened (FFIO options do not \
support this type of OPEN).
.PP
An \*FOPEN\fR statement failed because the file could not be opened for
the type of access requested.  This can occur if an \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command selects an \s-1FFIO\s+1 option that cannot support the
type of operations desired.
.PP
Check the \*Fassign\fR(1) or \*Fasgcmd\fR(1) attributes in effect for the
file to  ensure that they do not preclude the desired operations.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPNNEW STATUS specifier must not be NEW on an OPEN of an opened unit.
$nexp FEOPNNEW
STATUS specifier must not be NEW on an OPEN of an opened unit.
.PP
An \*FOPEN\fR statement on an already opened unit contained a \*FSTATUS\fR
specifier with a value of \*F\&'NEW'\fR.  The value of the \*FSTATUS\fR
specifier must be changed from \*F\&'NEW'\fR to \*F\&'OLD'\fR on a unit
that is already connected.
.PP
The \*FSTATUS\fR specifier can be \*F\&'NEW'\fR only on the
initial \*FOPEN\fR statement.  Change the \*FSTATUS\fR specifier
from \*F\&'NEW'\fR to \*F\&'OLD'\fR on subsequent \*FOPEN\fR statements
on a unit that is already connected, or remove all specifiers but
the \*FUNIT\fR, \*FBLANK\fR, \*FDELIM\fR, \*FPAD\fR, \*FERR\fR,
or \*FIOSTAT\fR specifiers on the subsequent \*FOPEN\fR statements.
.PP
See the description of the \*FOPEN\fR statement in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPIVRL An incorrect RECL was specified for an existing direct-access file.
$nexp FEOPIVRL
An incorrect RECL was specified for an existing direct-access file.
.PP
An \*FOPEN\fR statement failed because the file contained records that
were of a different size than that indicated with the \*FRECL\fR specifier.
.PP
Ensure that the \*FRECL\fR matches the size of the records in the file
specified with \*Fassign\fR(1) or \*Fasgcmd\fR(1).
.PP
See the description of the
\*FOPEN\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPSTFN A standard file cannot be opened as an unformatted, direct-access, or\n\
  nontext file.
$nexp FEOPSTFN
A standard file cannot be opened as \
an unformatted, direct-access, or \
nontext file.
.PP
An \*FOPEN\fR statement failed because the unit was associated with a
standard file (standard input, standard output, or standard
error; units 5, 6, or 0) and
either the \*FOPEN\fR specified a connection that
was unformatted or direct access, or the \*Fassign\fR(1) command specified
a file structure that was incompatible with a standard file.
Standard files must be connected as formatted, sequential-access
text files.
.PP
Ensure that the standard file (standard input, standard output, or
standard error) is being connected as a
formatted, sequential-access text file.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPAQPZ An invalid AQPSIZE parameter was specified in AQOPEN.
$nexp FEOPAQPZ
An invalid AQPSIZE parameter was specified in AQOPEN.
.PP
The \*FAQPSIZE\fR parameter to the \*FAQOPEN\fR routine was less than or equal
to 0.  The \*FAPQSIZE\fR parameter must be a positive integer that
specifies the maximum number of queued \s-1I/O\s+1 requests.
.PP
Check the value of the \*FAQPSIZE\fR parameter in the \*FAQOPEN\fR call.
.PP
See \*FAQOPEN\fR(3) in your Fortran reference manual,
and see \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____62 The FILE specifier on OPEN of unit %d conflicts with the -a or -D\n\
  assign attribute.
$nexp FE____62
The FILE specifier on OPEN of unit \fIun\*F \
conflicts with the -a or -D \
assign attribute.
.PP
A conflict exists between the \*Fassign\fR(1) command and the Fortran
\*FOPEN\fR statement.  Example:
.CS
assign -a xyz u:1    (alias unit 1
                      to file 'xyz')
OPEN (1, FILE='abc') (connect unit 1
                      to file 'abc')
.CE
.PP
The Fortran library cannot resolve the conflict over which file
to connect to unit 1.
.PP
Remove either the assign alias or the \*FFILE\fR specifier on the \*FOPEN\fR
statement.
.PP
See the description of the
\*FOPEN\fR statement
in your Fortran reference manual,
and \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWTOMNY The program tried to open too many word-addressable files.
$nexp FEWTOMNY
The program tried to open too many word-addressable files.
.PP
A Fortran \*FOPEN\fR statement or a call to \*FWOPEN\fR attempted to open
a word-addressable file, and the maximum number of
word-addressable files were already open.  The
word-addressable \s-1I/O\s+1 package is used to process \s-1I/O\s+1
requests for \*FBIN\fR file structure.
.PP
Reduce the number of open word-addressable files.
.PP
See \*FWOPEN\fR in your Fortran reference manual,
and \*Fassign\fR(1) in \*(aI.
.PP
The error class is \s-1FATAL\s+1 (issued by the Fortran run-time library).
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWABLKS The buffer size for a word-addressable file exceeds the maximum.
$nexp FEWABLKS
The buffer size for a word-addressable file exceeds the maximum.
.PP
Either the \*FBLOCKS\fR parameter to the \*FWOPEN\fR routine exceeds the
maximum value of 4095 or the buffer size specified
by the \*Fassign\fR(1) command for this file exceeds the maximum value.
The word-addressable \s-1I/O\s+1 package is used to process \s-1I/O\s+1
requests for \*FBIN\fR file structure.
.PP
If \*FWOPEN\fR is used, ensure that the \*FBLOCKS\fR parameter is less than
4095. Otherwise, ensure that the buffer size specified by using \*Fassign\fR
is less than 4095.
.PP
See \*FWOPEN\fR in your Fortran reference manual,
and \*Fassign\fR(1) in \*(aI.
.PP
The error class is \s-1FATAL\s+1 (issued by the Fortran run-time library).
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERQTRNC Truncation after write may not be suppressed for this file.
$nexp FERQTRNC
Truncation after write may not be suppressed for this file.
.PP 
The \*F-T off\fR option was requested with the \*Fassign\fR(1) command or
an \*FASSIGN\fR library routine, but truncation after write may not be
suppressed.  Some \s-1FFIO\s+1 layers or \*C-s assign\fR option values that
specify a type of blocked file structure require truncation after
a write operation.
.PP
Either remove the \*F\-T off\fR assign attribute for the unit number or file
name or choose a \*F\-F\fR or \*C-s assign\fR option that permits the
suppression of truncation after a write operation.
.PP
See \*FASSIGN\fR in your Fortran reference manual,
and \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____66 Truncation after write is not supported for this file type.
$nexp FE____66
Truncation after write is not supported for this file type.
.PP
The \*F\-T on\fR assign option was requested in conjunction with the
\*F\-s bin\fR option.  Truncation of \*FBIN\fR files is not supported.
.PP
Choose a \*F\-F\fR or \*C-s assign\fR option for a style of file processing
that permits truncation.
.PP
See \*FASSIGN\fR(3) in your Fortran reference manual,
and \*Fassign\fR(1) in \*(aI.
.PP
.Ma CRI_ONLY
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPPDIV A PAD specifier is invalid in an OPEN of an unformatted file.
$nexp FEOPPDIV
A PAD specifier is invalid in an OPEN of an unformatted file.
.PP
A Fortran \*FOPEN\fR statement contains a \*FFORM\fR specifier of
\*F\&'UNFORMATTED'\fR (or a default value of unformatted) and a \*FPAD\fR
specifier.  A \*FPAD\fR specifier is not valid for unformatted files.
.PP
Either remove the \*FPAD\fR specifier or change (or add) a \*FFORM\fR specifier
with the value \*F\&'FORMATTED'\fR.
.PP
See the description of the
\*FOPEN\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPNUNF The file cannot be opened for UNFORMATTED I/O.
$nexp FEOPNUNF
The file could not be opened for UNFORMATTED I/O.
.PP
An \s-1I/O\s+1 statement requested \*F\&'UNFORMATTED'\fR \s-1I/O\s+1 on a
file that cannot support \*F\&'UNFORMATTED'\fR access.
This occurs only when the \*Fassign\fR(1) or \*Fasgcmd\fR(1) command selects a
nondefault file structure that does not support the type of access
desired.
.PP
Check the \*Fassign\fR(1) or \*Fasgcmd\fR(1) attributes in effect for the unit
and file to ensure that they do not preclude the type of access being
requested.  This usually occurs when a text-type blocking is used on
the \*F\-F\fR option
of \*Fassign\fR(1) with \*F\&'UNFORMATTED'\fR \s-1I/O\s+1.
.PP
See the \*F\-F\fR and \*F\-s\fR parameters on the
\*Fassign\fR(1) or \*Fasgcmd\fR(1) commands in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEOPNFMT The file cannot be opened for FORMATTED I/O.
$nexp FEOPNFMT
The file could not be opened for FORMATTED I/O.
.PP
An \s-1I/O\s+1 statement requested \*F\&'FORMATTED'\fR \s-1I/O\s+1 on a
file that cannot support \*F\&'FORMATTED'\fR access.
This occurs only when the \*Fassign\fR(1) or \*Fasgcmd\fR(1) command selects a
nondefault file structure that does not support the type of access
desired.
.PP
Check the \*Fassign\fR(1) or \*Fasgcmd\fR(1) attributes in effect for the unit
and file to ensure that they do not preclude the type of access being
requested.  This usually occurs when a non-blocking \s-1FFIO\s+1 specification
is used on the \*F\-F\fR option
of \*Fassign\fR(1) with \*F\&'FORMATTED'\fR \s-1I/O\s+1.
.PP
See the \*F\-F\fR and \*F\-s\fR parameters on the
\*Fassign\fR(1) or \*Fasgcmd\fR(1) commands in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	CLOSE errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FECLSTAT The STATUS specified on a CLOSE must be 'KEEP' or 'DELETE'.
$nexp FECLSTAT
The STATUS specified on a CLOSE must be \&'KEEP' or \&'DELETE'.
.PP
A Fortran \*FCLOSE\fR statement used an unrecognizable \*FSTATUS\fR.
\*FSTATUS\fR, when specified, must be \*F\&'KEEP'\fR or \*F\&'DELETE'\fR.
When \*FSTATUS\fR is not specified, \*F\&'KEEP'\fR
is assumed unless the file was opened with a
\*FSTATUS\fR of \*F\&'SCRATCH'\fR,
in which case \*F\&'DELETE'\fR is assumed.  Trailing
blanks are allowed (for example, \*F\&'KEEP   \&'\fR ).
.PP
Ensure that the \*FSTATUS\fR specifier is spelled correctly.
.PP
See the description of the
\*FCLOSE\fR statement
in your Fortran reference manual.
.PP
Lowercase letters are allowed in lieu of uppercase letters for \*FOPEN\fR
specifiers as an extension to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FECLSTIV A STATUS specifier of 'KEEP' is invalid on a CLOSE of a scratch file.
$nexp FECLSTIV
A STATUS specifier of \&'KEEP' is invalid on a CLOSE of a scratch file.
.PP
A Fortran \*FCLOSE\fR statement specified a \*FSTATUS\fR
of \*F\&'KEEP'\fR on a file
that was opened with a \*FSTATUS\fR of \*F\&'SCRATCH'\fR.
.PP
Change \*FSTATUS\fR to \*F\&'DELETE'\fR; remove the \*FSTATUS\fR specifier;
or change the \*FSTATUS\fR on the \*FOPEN\fR to \*F\&'OLD'\fR,
\*F\&'NEW'\fR, or \*F\&'UNKNOWN'\fR.
.PP
See the description of the
\*FCLOSE\fR statement
in your Fortran reference manual.
.PP
A \*FFILE\fR specifier may be used 
with scratch files to allow assign-by-file to function.  The 
assign-by-unit
capability removes the need for this extension to the current Fortran
standard.  See the \*Fassign\fR(1) \fRcommand.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINCZER The increment value on an implied do loop is zero.
$nexp FEINCZER
The increment value on an implied do loop is zero.
.PP
Correct the implicit do loop specification in the \s-1I/O\s+1 statement.  If
a variable is passed as the increment, add a test for zero before executing
the \s-1I/O\s+1 statement.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEAQBADH An AQIO routine was called with an invalid file handle.
$nexp FEAQBADH
An AQIO routine was called with an invalid file handle.
.PP
The \*FAQP\fR argument on a call to \*FAQIO\fR routines \*FAQREAD\fR, \*FAQREADC\fR, \*FAQWRITE\fR, 
\*FAQWRITEC\fR, \*FAQSTAT\fR, \*FAQRECALL\fR, \*FAQWAIT\fR, or \*FAQCLOSE\fR does not
contain a valid file 
handle.  The file handle must contain a value which was returned from a 
previous call to \*FAQOPEN\fR.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEMDUPOP The specified file is connected to another unit which does not have the\n\
  "-m on" assign attribute.
$nexp FEMDUPOP
The specified file is connected to another unit which does not have the
"-m on" assign attribute.
.PP
When several Fortran units are connected to the same file, they must all
have been opened with the "-m on" assign attribute.
.PP
See the description of the -m option on 
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEANOSUP The file type does not support the "%s" assign attribute.
$nexp FEANOSUP
The file type does not support the \&"\fIasn_att\*C" assign attribute.
.PP
The default file type or the file type assigned with the -s or -F assign 
attribute does not support the \&"\fIasn_att\*C" assign attribute.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEMIXSCP A Fortran unit cannot be both private and global.
$nexp FEMIXSCP
A Fortran unit cannot be both private and global.
.PP
An attempt was made to open a Fortran unit globally when a different task
had opened the unit privately.   The runtime library cannot process a unit 
number which is scoped differently on different tasks.
.PP
Fortran units can be designated as private by the \&"-P global" assign option,
and global by the \&"-P global" assign option.
.PP
Change the -P option value specified at connect time for the Fortran unit on
one of the tasks, or ensure that an assign-by-unit number is issued prior
to the time of connection of the Fortran unit on either task.  A Fortran
unit is connected by an \*FOPEN\fR statement or by an implicit open which
occurs at the first I/O statement executed for a unit.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOGLOB The specified Fortran unit cannot be assigned -P global.
$nexp FENOGLOB
The specified Fortran unit cannot be assigned \&"-P global".
.PP
The \&"-P global"  assign option is not supported on the current system.
Use \&"P process" or \&"-P team" instead.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
This error message is issued only on Cray \s-1MPP\s+1 systems.  On Cray
\s-1PVP\s+1 systems, \&"-P global" means the same as \&"-P process".
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOPRIV The specified Fortran unit cannot be private to a thread.
$nexp FENOPRIV
The specified Fortran unit cannot be private to a thread.
.PP
The \&"-P private"  assign option was specified for a unit, but this unit 
must be global to all threads in a process on the current system.   
On Cray \s-1PVP\s+1 systems,
units connected to \*Cstdin\fR, \*Cstdout\fR, and \*Cstderr\fR must not be
assigned \&"-P private".  On \s-1Irix\s+1 systems, \&"-P private" is not supported.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
This error message is issued only on Cray \s-1PVP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Access errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBSPNRD A BACKSPACE requires read permission for the file.
$nexp FEBSPNRD
A BACKSPACE requires read permission for the file.
.PP
A Fortran \*FBACKSPACE\fR statement was attempted on a file for which the
user does not have read permission, but the file is a type which
requires read permission to perform the \*FBACKSPACE\fR operation.
.PP
Use the \*Fchmod\fR(1) command to change the permissions mode of the
file so that the program has read permission.
.PP
See the man page for the \*Fchmod\fR(1) command (enter \*Fman chmod\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTTIV A formatted read or write is not allowed on an unformatted file.
$nexp FEFMTTIV
A formatted read or write is not allowed on an unformatted file.
.PP
A Fortran \*FREAD\fR, \*FWRITE\fR, or \*FPRINT\fR statement
attempted a formatted \s-1I/O\s+1
operation on a file that was opened for unformatted \s-1I/O\s+1.
.PP
Either change the \s-1I/O\s+1 statement to unformatted (remove the \*FFORMAT\fR
specifier) or open the file for formatted \s-1I/O\s+1.
.PP
See the description of input/output statements in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEUNFMIV An unformatted read or write is not allowed on a formatted file.
$nexp FEUNFMIV
An unformatted read or write is not allowed on a formatted file.
.PP
A Fortran \*FREAD\fR or \*FWRITE\fR statement
attempted an unformatted
\s-1I/O\s+1 operation on a file that was opened for formatted \s-1I/O\s+1.
.PP
Either change the \s-1I/O\s+1 statement to formatted (add a \*FFORMAT\fR specifier)
or open the file for unformatted \s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEDIRTIV A direct access read or write is not allowed on a sequential-access file.
$nexp FEDIRTIV
A direct access read or write is not allowed on a sequential-access file.
.PP
A Fortran \*FREAD\fR or \*FWRITE\fR statement attempted
a direct-access
\s-1I/O\s+1 operation on a file that was opened for sequential-access \s-1I/O\s+1.
.PP
Either change the \s-1I/O\s+1 statement to sequential-access \s-1I/O\s+1
(remove the \*FREC\fR specifier) or open the file for direct-access \s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESEQTIV A sequential access read or write is not allowed on a direct-access file.
$nexp FESEQTIV
A sequential-access read or write is not allowed on a direct-access file.
.PP
A Fortran \*FREAD\fR, \*FWRITE\fR, or \*FPRINT\fR statement attempted a
sequential-access \s-1I/O\s+1 operation on a file that was opened for
direct-access \s-1I/O\s+1.
.PP
Either change the \s-1I/O\s+1 statement to direct access (add a \*FREC\fR
specifier) or open the file for sequential-access \s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBKSPIV A BACKSPACE is invalid on a direct-access file.
$nexp FEBKSPIV
A BACKSPACE is invalid on a direct-access file.
.PP
A Fortran \*FBACKSPACE\fR was attempted on a file that was opened for
direct-access \s-1I/O\s+1.
.PP
Either remove the \*FBACKSPACE\fR statement (the previous record can be read
or written if you specify its record number with the \*FREC\fR specifier;
its record number can be computed from the \*FNEXTREC\fR specifier on an
\*FINQUIRE\fR statement), or open the file for sequential-access \s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEENDFIV An ENDFILE is invalid on a direct-access file.
$nexp FEENDFIV
An ENDFILE is invalid on a direct-access file.
.PP
A Fortran \*FENDFILE\fR was attempted on a file that was opened for
direct-access \s-1I/O\s+1.
.PP
Either remove the \*FENDFILE\fR statement or open the file for sequential-access
\s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERWNDIV A REWIND is invalid on a direct-access file.
$nexp FERWNDIV
A REWIND is invalid on a direct-access file.
.PP
A Fortran \*FREWIND\fR was attempted on a file that was opened for
direct-access \s-1I/O\s+1.
.PP
Either remove the \*FREWIND\fR statement (the first record can be read or
written if you specify record number one with the \*FREC\fR specifier), or
open the file for sequential-access \s-1I/O\s+1.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDAFWR A read after write is invalid on a sequential-access file.
$nexp FERDAFWR
A read after write is invalid on a sequential-access file.
.PP
A Fortran \*FREAD\fR was attempted on a file (opened for
sequential-access \s-1I/O\s+1) on which the previous operation was a
\*FPRINT\fR, \*FWRITE\fR, or \*FENDFILE\fR statement.
.PP
A \*FREWIND\fR or \*FBACKSPACE\fR statement must be performed after a write
operation and before a read operation.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____88 The record number (%d) must be a positive (nonzero) integer.
$nexp FE____88
The record number (\fIrecn\*F) must be a positive (nonzero) integer.
.PP
On a Fortran \*FREAD\fR or \*FWRITE\fR statement, the \*FREC\fR (record number)
specifier was negative or 0.  It must be a positive integer
corresponding to the desired record number.
Check the value of the \*FREC\fR specifier to ensure that it is a valid
record number.
.PP
Ensure that the correct file is being read or
written.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma RETIRED 3.0.2.1
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$ 
$	Operation errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOREAD A READ operation was attempted on a file with no read permission.
$nexp FENOREAD
A READ operation was attempted on a file with no read permission.
.PP
A Fortran \*FREAD\fR or\*F BUFFER IN\ \fRstatement was specified for a file
for which the user does not have read permission.
.PP
Change the permissions of the file so that your program has
read permission (or have the owner of the file change the
permissions) by using the \*Fchmod\fR(1) command.
.PP
See the man page for the \*Fchmod\fR(1) command (enter \*Fman chmod\fR).
See the description of input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOWRIT A WRITE operation was attempted on a file with no write permission.
$nexp FENOWRIT
A WRITE operation was attempted on a file with no write permission.
.PP
A Fortran \*FPRINT\fR, \*FWRITE \fR,
or\*F BUFFER OUT\ \fRstatement was specified for a
file for which the user does not have write permission.
.PP
Change the permissions of the file so that your program has
write permission (or have the owner of the file change the
permissions) by using the \*Fchmod\fR(1) command.
.PP
See the man page for the \*Fchmod\fR(1) command (enter \*Fman chmod\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOBKSP The file does not support a BACKSPACE operation.
$nexp FENOBKSP
The file does not support a BACKSPACE operation.
.PP
A Fortran \*FBACKSPACE\fR statement was specified for a file whose 
file structure does not support backspace.
.PP
Either change the file structure (by using the \*Fassign\fR(1) command) or
remove the \*FBACKSPACE\fR statement.
.PP
See the description of the \*Fassign\fR(1) command in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOENDF The file does not support an ENDFILE operation.
$nexp FENOENDF
The file does not support an ENDFILE operation.
.PP
A Fortran \*FENDFILE\fR statement was specified for a file with 
a file structure that does not support either a logical
or physical endfile record.  This can occur when an
\*FENDFILE\fR is attempted during the reading of a tape file that was not
mounted with the \*F\-T\fR option to allow tape marks to be written by
the user.
.PP
Either change the file structure (by using the \*Fassign\fR(1) command) or
tape file capabilities (by using the \*Ftpmnt\fR(1) command) or remove the
\*FENDFILE\fR statement.
.PP
See the description of
the \*Fassign\fR(1) command in \*(aI 
and the \*Ftpmnt\fR(1) command
in the \fI\s-1UNICOS\s+1 User Commands Reference
Manual\fR, publication \s-1SR\-2011\s+1.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____94 The file does not support a REWIND operation.
$nexp FE____94
The file does not support a REWIND operation.
.PP
A Fortran \*FREWIND\fR statement was attempted on a file that was
assigned a file structure that cannot be rewound.
.PP
Either change the file structure (by using the \*Fassign\fR(1) command) or
remove the \*FREWIND\fR statement.
.PP
See the description of the \*Fassign\fR(1) command in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWRAFEN A WRITE operation is invalid if the file is positioned after the end-of-file.
$nexp FEWRAFEN
A WRITE operation is invalid if the file is positioned after the end-of-file.
.PP
A Fortran \*FWRITE\fR or \*FPRINT\fR statement
was specified for a file that was
positioned after the endfile record (end-of-file).  The endfile
record must be the last record in a sequential-access file.
.PP
Reposition the file (by using a \*FBACKSPACE\fR
or \*FREWIND\fR statement) before specifying 
the \*FWRITE\fR or \*FPRINT\fR statement.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEENAFEN An ENDFILE is invalid if the file is positioned after the end-of-file.
$nexp FEENAFEN
An ENDFILE is invalid if the file is positioned after the end-of-file.
.PP
A Fortran \*FENDFILE\fR statement was attempted on a file that was
positioned after the endfile record (end-of-file).  The endfile
record must be the last record on a sequential-access file.
.PP
Reposition the file (by using a \*FBACKSPACE\fR or
\*FREWIND\fR statement) before specifying to
the \*FENDFILE\fR statement.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE____97 A READ operation tried to read a nonexistent record (%d).
$nexp FE____97
A READ operation tried to read a nonexistent record (\fIrecn\*C).
.PP
On a Fortran \*FREAD\fR statement,
the \*FREC\fR (record number) specifier was
larger than the largest record number for that direct-access file.
.PP
Check the value of the \*FREC\fR specifier to ensure that it is a valid
record number.  Check the file being read to ensure that it is the
correct file.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
.Ma RETIRED 3.0.2.1
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOBKPI A BACKSPACE is invalid on a piped file.
$nexp FENOBKPI
A BACKSPACE is invalid on a piped file.
.PP
A Fortran \*FBACKSPACE\fR statement was attempted on a named or unnamed
pipe (\s-1FIFO\s+1 file) that does not support backspace.
.PP
Either remove the \*FBACKSPACE\fR statement or change the file so that it
is not a pipe.
.PP
See the man pages for
\*Fpipe\fR(2), \*Fread\fR(2), and \*Fwrite\fR(2).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENORWPI A REWIND is invalid on a piped file.
$nexp FENORWPI
A REWIND is invalid on a piped file.
.PP
A Fortran \*FREWIND\fR statement was specified for a named or unnamed
pipe (\s-1FIFO\s+1 file) that does not support rewind operations.
.PP
Either remove the \*FREWIND\fR statement or change the file so that it is
not a pipe.
.PP
See the man pages for \*Fpipe\fR(2), \*Fread\fR(2), and \*Fwrite\fR(2).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___100 A READ operation tried to read a nonexistent record.
$nexp FE___100
A READ operation tried to read a nonexistent record.
.PP
On a Fortran \*FREAD\fR statement,
the \*FREC\fR (record number) specifier was
larger than the largest record number for that direct-access file.
.PP
Check the value of the \*FREC\fR specifier to ensure that it is
a valid record number.  Check the file being read to ensure
that it is the correct file.
.PP
See the description of input/output statements in your
Fortran reference manual.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENODELT The file could not be deleted while executing the CLOSE statement.
$nexp FENODELT
The file could not be deleted while executing the CLOSE statement.
.PP
The Fortran run-time library was unable to delete a file which was either
opened as a scratch file or was closed with a STATUS='DELETE' specifier.
A file with the same name but with a different device and/or inode number
was located, probably indicating that the original file was renamed after
it was opened--possibly by an external process.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOTHRD The specified Fortran unit may not be private to a thread.
$nexp FENOTHRD
The specified Fortran unit may not be private to a thread.
.PP
The \&"-P thread" assign option was specified for a unit
which must be global to a process or PE.
.PP
On \s-1CRAY PVP\s+1 systems, units connected to \*Cstdin\fR, \*Cstdout\fR,
and \*Cstderr\fR must have process scope.
On \s-1CRAY MPP\s+1 systems, the \&"-P thread" option is not supported.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOTEAM The specified Fortran unit may not be global to an application team
.
$nexp FENOTEAM
The specified Fortran unit may not be global to an application team.
.PP
The \&"-P team"  assign option was specified for a unit, but this unit
may not be global to an application team (have "application team scope")
on the current system.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTILF The format and specified I/O list produce an infinite loop.
$nexp FEFMTILF
The format and specified I/O list produce an infinite loop.
.PP
The format used with a \*FREAD\fR
or \*FWRITE\fR statement, with the I/O list used,
caused an infinite loop.
.PP
Example:
.CS
WRITE (1,1) I, J
1 FORMAT(I5,('\fIHi\*F'))\fR
.CE
.PP
(\*C'\fIHi\*C'\fR) is an embedded format specification that repeats
infinitely, trying to process the I/O list variable \*F\&'J'\fR.
.PP
Check the format to ensure that it contains at least one
repeatable data edit descriptor in the repeated parentheses
group for the I/O list item being processed.
.PP
See the description of input/output statements in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTLII A literal string is invalid in an input format.
$nexp FEFMTLII
A literal string is invalid in an input format.
.PP
A format containing a literal or an \*FH\fR edit descriptor
was used on a \*FREAD\fR statement.
.PP
Change the literal or \*FH\fR edit descriptor to an \*FX\fR field.
For example, change \*F("---")\fR or \*F(3H---)\fR to \*F(3X)\fR.
.PP
See the description of input/output statements in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTQIO The Q data edit-descriptor is invalid in an output format.
$nexp FEFMTQIO
The Q data edit-descriptor is invalid in an output format.
.PP
A format containing a Q data edit-descriptor was used on a
\*FPRINT\fR or \*FWRITE\fR statement.
.PP
The Q data edit-descriptor can only be used on an input format.
.PP
See the description of the Q data edit-descriptor in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Operation errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOSKPF The file does not support a SKIPF operation.
$nexp FENOSKPF
The file does not support a SKIPF operation.
.PP
A \*FSKIPF\fR statement was specified on a file that does not
support this operation.  Currently only online tape files
support \*FSKIPF\fR.
.PP 
Either remove the \*FSKIPF\fR statement or change the file to be an
online tape.
.PP 
See \*FSKIPF\fR(3) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Invalid input/output fields
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDTYPE An input list item is incompatible with its data edit-descriptor.
$nexp FERDTYPE
An input list item is incompatible with its data edit-descriptor.
.PP
The data type of an input list item being used on a \*FREAD\fR statement
is incompatible with its data edit descriptor in the format.
.PP
Example:
.CS
    CHARACTER * 10, C
    READ (1,1) C
1   FORMAT (I4)
.CE
.PP
Check the format and ensure that each
\*FA\fR, \*FB\fR, \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
\*FG\fR, \*FI\fR, \*FL\fR, \*FO\fR, \*FR\fR, and \*FZ\fR
format data edit descriptor is appropriate for the data type of each
input list item.
.PP
The \*FB\fR, \*FEN\fR, \*FES\fR, \*FO\fR, \*FR\fR, and \*FZ\fR data
edit descriptors are extensions to the \s-1ANSI
X\s+13.9\-1978 standard.  The \*FR\fR data edit descriptor is an 
extension to the current Fortran standard.
.PP
You can remove many data edit descriptor / input list restrictions by
selecting an alternate validation table with the use of \*Fsegldr\fR(1)
directives.
See the description of
input/output formatting and
data edit descriptors in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWRTYPE An output list item is incompatible with its data edit-descriptor.
$nexp FEWRTYPE
An output list item is incompatible with its data edit-descriptor.
.PP
The data type of an output list item being used on a \*FWRITE\fR or \*FPRINT\fR
statement is incompatible with its data edit descriptor in the format.
.PP
Example:
.CS
    CHARACTER * 10, C
    WRITE (1,1) C
1   FORMAT (I4)
.CE
.PP
Check the format to ensure that each
\*FA\fR, \*FB\fR, \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
\*FG\fR, \*FI\fR, \*FL\fR, \*FO\fR, \*FR\fR, and \*FZ\fR
format data edit descriptor is appropriate for the data type of each
output list item.
.PP
The \*FB\fR, \*FEN\fR, \*FES\fR, \*FO\fR, \*FR\fR, and \*FZ\fR data
edit descriptors are extensions to the \s-1ANSI
X\s+13.9\-1978 standard.  The \*FR\fR data edit descriptor is an
extension to the current Fortran standard.
.PP
You can remove many data edit descriptor / output list restrictions by
selecting an alternate validation table with the use of \*Fsegldr\fR(1)
directives.
See the description of
input/output formatting and
data edit descriptors in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDIVLG The input field for a logical variable is invalid.
$nexp FERDIVLG
The input field for a logical variable is invalid.
.PP
A formatted \*FREAD\fR statement, using an \*FL\fR edit descriptor,
encountered input that was not valid for a logical variable.
Logical input must consist of
zero or more blanks, optionally followed by
zero or one period, optionally followed by
zero or more blanks, followed by
\*FT\fR, \*FF\fR, \*Ft\fR, or \*Ff\fR, optionally followed by
zero or more characters.
.PP
Examples of valid logical input are \*F\.TRUE.\fR, \*F\.false.\fR,
\*FTRUE\fR, \*Ff\fR, \*F   T\fR, and \*F  .  f\fR.
.PP
Examples of logical inputs that are not valid are \*F\..TRUE.\fR, \*F.\fR,
\*FXT\fR, and \*F. Y\fR.
.PP
A logical input field containing only blanks will be interpreted as \*F.FALSE.\fR
.PP
Either check the input file to ensure that the logical data is correct,
or if nonlogical data is desired, change the format and the type
of the input list item.
.PP
See the description of input/output formatting in the Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	List-directed input errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FELDUNKI The input on a READ statement with list-directed formatting was unrecognized.
$nexp FELDUNKI
The input on a READ statement with list-directed formatting was unrecognized.
.PP
Unrecognized input was
encountered on a list-directed \*FREAD\fR statement.
For example, a letter may have been encountered during an attempt 
to read an integer.
.PP
Correct the input to match the data being read.
.PP
See the description of list-directed input in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FELDNOCX A misformed complex number was encountered on a list-directed READ.
$nexp FELDNOCX
A misformed complex number was encountered on a list-directed READ.
.PP
Input was encountered 
on a list-directed \*FREAD\fR statement 
that was not a valid complex number.  Complex numbers must be in the following
form:
.CS
( \fInumber\fR , \fInumber\fR )
.CE
.PP
\fInumber\fR is a valid integer or floating-point number.
.PP
Either correct the input to conform to the requirements of complex
data or, if noncomplex data is desired, change the type of the
input list item.
.PP
See the description of list-directed input in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FELDSTRL A string is too long for READ with list-directed formatting.
$nexp FELDSTRL
A string is too long for READ with list-directed formatting.
.PP
A character string 
that was longer than the character variable
into which it was being read
was encountered on a list-directed \*FREAD\fR statement. 
.PP
Either correct the input to conform to the length of the character
variable or increase the size of the character variable.
.PP
See the description of list-directed input in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	NICV errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENICVIC A numeric input field contains an invalid character.
$nexp FENICVIC
A numeric input field contains an invalid character.
.PP
An invalid character was encountered while reading a number.
Typically a letter is encountered when a digit is expected,
or a floating-point number is encountered when an integer is
expected.  A tab character (instead of one or more blanks)
in the input field can also cause this error.
.PP
Correct the input to match the data being read.
.PP
See the description of formatted \s-1I/O\s+1 in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENICVOF An overflow occurred during the conversion of fixed-point (integer) input data.
$nexp FENICVOF
An overflow occurred during the conversion of fixed-point (integer) input data.
.PP
An overflow occurred while a fixed-point number was being read.  This
occurs when the number to be read is larger than can be contained
in the variable being read. On \s-1CRAY PVP\s+1 and \s-1MPP\s+1 systems,
the largest signed fixed-point value is (2**63)\-1. On \s-1Irix\s+1 systems,
the largest signed fixed-point value for a default kind integer is
(2**31)\-1. Check the documentation on models in
the Fortran reference manual for information on other kind types.
.PP
Check the number being read to ensure that it is not too large.
.PP
See the description of formatted \s-1I/O\s+1 and machine representation of
data in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENICVEU An underflow occurred during the conversion of floating-point input data.
$nexp FENICVEU
An underflow occurred during the conversion of floating-point input data.
.PP
An underflow occurred while reading a floating-point number.  This
occurs when the absolute value of the number being read is smaller
than (approximately) 0.367E-2465 (2.225E-308 on Cray \s-1MPP\s+1 systems).
.PP
For \s-1CF90\s+1 on \s-1IEEE\s+1 systems, the real data type has more than
one absolute value that may cause underflow.  The TINY numeric
inquiry function returns the smallest floating point value for
a particular kind type.  Check the documentation on models in
the Fortran reference manual.
.PP
Check the number being read to ensure that it is not too small.
.PP
See the description of formatted \s-1I/O\s+1 and machine representation of
data in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENICVEO An overflow occurred during the conversion of floating-point input data.
$nexp FENICVEO
An overflow occurred during the conversion of floating-point input data.
.PP
An overflow occurred while reading a floating-point number.  This
occurs when the absolute value of the number being read is larger
than (approximately) 0.273E+2466 (1.798E+308 on Cray \s-1MPP\s+1 systems).
.PP
For \s-1CF90\s+1 on \s-1IEEE\s+1 systems, the real data type has more than
one absolute value that may cause overflow.  The HUGE numeric
inquiry function returns the largest floating point value for
a particular kind type.  Check the documentation on models in
the Fortran reference manual.
.PP
Check the number being read to ensure that it is not too large.
.PP
See the description of formatted \s-1I/O\s+1 and machine representation of
data in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENICVBK A numeric input field has incomplete data.
$nexp FENICVBK
A numeric input field has incomplete data.
.PP
A missing subfield was encountered while a number was being read.
.PP
For instance, a floating point number "1.2-" is missing the
exponent subfield and a floating point number ".E+13" is missing the 
mantissa subfield.  The integer or floating point input "   -" 
will also generate this error since it is missing the 
magnitude or mantissa subfields. Likewise, with a floating 
point input of "   ." the mantissa subfield is missing. 
.PP
Correct the input to match the data being read.
.PP
See the description of formatted \s-1I/O\s+1
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	General run-time errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDPEOR A READ operation tried to read past the end-of-record.
$nexp FERDPEOR
A READ operation tried to read past the end-of-record.
.PP
A Fortran \*FREAD\fR operation tried to read more data than was in the
record.
.PP
Either change the program to read less data from the record, or increase
the length of the records being read.
.PP
See the description of unformatted and formatted \s-1I/O\s+1
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDWRER The program read or wrote less data than was requested.
$nexp FERDWRER
The program read or wrote less data than was requested.
.PP
An \*FAQREAD\fR(3) or \*FAQWRITE\fR(3) request generated a \*Flistio\fR(2) system
call that read
or wrote less data than was requested.  This can occur when the user
calls \*FAQREAD\fR to read more data than is in the file.
.PP
See the description of
\*FAQREAD\fR(3) and \*FAQWRITE\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDMEMY The program was unable to request more memory space during a\n\
  formatted READ statement.
$nexp FERDMEMY
The program was unable to request more memory space during a formatted READ statement.
.PP
The Fortran run-time library was unable to request additional memory
space during a formatted READ statement.
.PP
Ensure that the data file being read is, in fact, a formatted file.
Review the use of memory space by your program, and if possible,
reduce its memory use.
.PP
#if _UNICOS
See the description of the
\*Flimit\fR(1) command in the \fI\s-1UNICOS\s+1 User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
.PP
#endif
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOMEMY The program was unable to request more memory space.
$nexp FENOMEMY
The program was unable to request more memory space.
.PP
A routine in the run-time library was unable to request
additional memory space.  This usually occurs when the process
has exceeded its memory allocation.
.PP
Review the use of memory space by your program, and if possible,
reduce its memory use.
.PP
#if _UNICOS
See the description of the
\*Flimit\fR(1) command in the \fI\s-1UNICOS\s+1 User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
.PP
#endif
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOSDSP The program was unable to request more SDS space.
$nexp FENOSDSP
The program was unable to request more SDS space.
.PP
The \*Fffopen\fR(3) routine in the Fortran run-time library was unable
to
request additional secondary data storage (\s-1SDS\s+1) space.  This usually
occurs when the process has exceeded its \s-1SDS\s+1 allocation.
.PP
Review the use of \s-1SDS\s+1 space by your program, and
reduce its \s-1SDS\s+1 use if possible.
.PP
See the description of the
\*Flimit\fR(1) command in the \fI\s-1UNICOS\s+1 User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEIOACTV An I/O statement was invoked while another was active.
$nexp FEIOACTV
An I/O statement was invoked while another was active.
.PP
A Fortran \s-1I/O\s+1 statement was executed while another
\s-1I/O\s+1 statement was active.
.PP
Example:
.CS
      WRITE (1) X, FUNC(X)  (write value
C     ...                   of function)
      FUNCTION FUNC(R)
      PRINT *,'In FUNC'   (I/O while WRITE
                           is active)
.CE
.PP
Recursive \s-1I/O\s+1 statements are not allowed.
Remove the recursive \s-1I/O\s+1 statements.
.PP
See the description of
input/output statements in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESDSFSS You must specify the file size when using 'assign -a SDS'.
$nexp FESDSFSS
You must specify the file size when using \&'assign -a SDS'.
.PP
An \*FOPEN\fR operation failed because the \*Fassign\fR(1) command specified an
\s-1SDS\s+1 file but did not specify the file size.
.PP
Either modify the \*Fassign\fR(1) command to specify the file size or
do not specify \*F-a SDS\fR.
.PP
See the description of the \*Fassign\fR(1) command in \*(aI.
.PP
This error message is issued only on Cray \s-1MPP\s+1 and \%\s-1CRAY\ PVP\s+1
systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWRLONG A WRITE operation tried to write a record that was too long.
$nexp FEWRLONG
A WRITE operation tried to write a record that was too long.
.PP
A Fortran \*FWRITE\fR operation tried to write a record that was too long.
.PP
Either reduce the size of the record to be written or increase the
size of the largest record that can be written.
.PP
The maximum formatted record size (for either sequential- or direct-
access files) can be increased with the use of RECL parameter on the
\*FOPEN\fR statement.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEWRIEND An internal WRITE tried to write beyond the end of an internal file.
$nexp FEWRIEND
An internal WRITE tried to write beyond the end of an internal file.
.PP
A Fortran internal \*FWRITE\fR operation tried to write beyond the end of an
internal file.
.PP
If an array element or variable was specified as the internal file, change
the specification to an array.  If an array was specified as the internal
file, either increase the dimensions of the internal file to allow more records
to be written to the internal file or reduce the number of records to be
written to the internal file.
.PP
For more information, see the description of internal records and files
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEPTRNAS A pointer or allocatable array in an I/O list has not been associated\n\
  or allocated.
$nexp FEPTRNAS
A pointer or allocatable array in an I/O list has not been associated or allocated.
.PP
Storage should be allocated for an allocatable array by using
an \*FALLOCATE\fR statement before using the array in an I/O list.
A target should be associated with a pointer by using a pointer-assignment 
statement or an \*FALLOCATE\fR statement before using the pointer in an 
I/O list.
.PP
For more information, see the description of pointers and allocatable arrays
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTPAL A pointer or allocatable array used as a format specifier has not been\n\
 associated or allocated.
$nexp FEFMTPAL
A pointer or allocatable array used as a format specifier has not been associated or allocated.
.PP
Storage should be allocated for an allocatable array by using
an \*FALLOCATE\fR statement before using the array as a format.
A target should be associated with a pointer by using a pointer-assignment 
statement or an \*FALLOCATE\fR statement before using the pointer as a
format. 
.PP
For more information, see the description of pointers and allocatable arrays
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEUNOTAL A pointer or allocatable array used as an internal file has not been\n\
 associated or allocated.
$nexp FEUNOTAL
A pointer or allocatable array used as an internal file has not been \
associated or allocated.
.PP
Storage should be allocated for an allocatable array by using
an \*FALLOCATE\fR statement before using the array as an internal file.
A target should be associated with a pointer by using a pointer-assignment 
statement or an \*FALLOCATE\fR statement before using the pointer as an 
internal file.
.PP
For more information, see the description of pointers and allocatable arrays
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTNUL A zero-sized array or scalar was used as a format specifier.
$nexp FEFMTNUL
A zero-sized array or scalar was used as a format specifier.
.PP
The character expression used as a format specifier must contain a valid
format specification that begins with a left parenthesis and ends with a
right parenthesis.  An array with an extent of zero or a zero-sized
character variable does not contain a valid format specification.
.PP
For more information, see the description of format specifications 
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERDMALR A formatted READ operation encountered a malformed record.
$nexp FERDMALR
A formatted READ operation encountered a malformed record.
.PP
A Fortran formatted \*FREAD\fR operation tried to read a record that was
malformed.
This usually occurs in a text file where the last record does not have a
new-line character prior to the end of the file.
If the last record of the file has no terminating new-line character, add the
new line.
.PP
See the description of text files in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Internal errors (should never occur)
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINTUNK An internal library run-time error has occurred.
$nexp FEINTUNK
An internal library run-time error has occurred.
.PP
A Fortran \s-1I/O\s+1 operation failed because of an internal error in
the run-time library.
The program may have overwritten itself, corrupting the internal
data used by the run-time library.
.PP
This represents an internal error that usually does not occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINTFST An internal Fortran unit table contains an unknown file structure.
$nexp FEINTFST
An internal Fortran unit table contains an unknown file structure.
.PP
A Fortran \s-1I/O\s+1 operation failed because the internal Fortran unit
table contained a code representing the file structure that is not valid.
The program may have overwritten itself, corrupting the internal
unit tables used by the Fortran run-time library.
.PP
See the description of debugging in your
Fortran reference manual.
.PP
This represents an internal error that usually does not
occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINTDTY An unknown data type was passed to the library from the compiler.
$nexp FEINTDTY
An unknown data type was passed to the library from the compiler.
.PP
A Fortran \s-1I/O\s+1 operation failed because the data type of an \s-1I/O\s+1
list variable was not valid.
The program may have overwritten itself, corrupting the \s-1I/O\s+1 list
variable type passed to the Fortran run-time library by the compiler.
.PP
See the description of debugging in your
Fortran reference manual.
.PP
This represents an internal error that usually does not occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINTIPF A preprocessed format contained an invalid format code.
$nexp FEINTIPF
A preprocessed format contained an invalid format code.
.PP
A Fortran formatted \s-1I/O\s+1 processor encountered a format code that is not valid
while processing a preprocessed format.
The program may have overwritten itself, corrupting the parsed
format structure used by the Fortran run-time library.
.PP
See the description of debugging in your Fortran reference manual.
.PP
This represents an internal error that usually does not occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEINTTAP An internal error occurred on a tape read.
$nexp FEINTTAP
An internal error occurred on a tape read.
.PP
A Fortran \*FREAD\fR operation on a tape did not receive the expected
status after the read completed.
.PP
Contact your system support staff.
.PP
See the description of debugging in your
Fortran reference manual.
.PP
This represents an internal error that usually does not occur.
.PP
releases.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$ 
$	NAMELIST errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSTRN Empty parentheses followed character variable "%s" in\n\
  namelist input record.
$nexp FENLSTRN
Empty parentheses followed character variable \&'\fIname\*C' in\
namelist input record.
.PP
An empty set of parentheses followed a character variable name
in a namelist input record.  If a substring is present, the
substring reference must contain either integer constants
separated by a colon to indicate a substring or a colon to
indicate the entire character variable.
.PP
See the description of namelist I/O in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSTRG Substring value of character variable "%s" in\n\
  namelist input record must be numeric.
$nexp FENLSTRG
Substring value of character variable \&'\fIname\*C' in\
namelist input record must be numeric.
.PP
An empty set of parentheses followed a variable name in a
namelist input record.  Function references are not allowed
in a namelist input record.
.PP
See the description of namelist I/O in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSUBN Empty parentheses followed array "%s" in\n\
  namelist input record.
$nexp FENLSUBN
Empty parentheses followed array \&'\fIname\*C' in\
namelist input record.
.PP
An empty set of parentheses must not follow an array name in a
namelist input record.
.PP
See the description of namelist I/O in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSUBD Character between subscripts of array "%s" in\n\
  namelist input record must be a comma.
$nexp FENLSUBD
Character between subscripts of array \&'\fIname\*C' in\
namelist input record must be a comma.
.PP
Subscripts of an array element reference in a namelist input
record must be separated by commas.  Subscript triplets in an
array section reference in a namelist input record must be
separated by commas.
.PP
See the description of namelist I/O in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSUBS Subscript value of array "%s" in namelist\n\
  input record must be numeric.
$nexp FENLSUBS
Subscript value of array \&'\fIname\*C' in namelist\
input record must be numeric.
.PP
A subscript value following an array name in a namelist input
record must be an integer constant or must be a substring
triplet with integer constants or null values.  Vector-valued
subscripts and variables must not be used as subscripts in
the namelist input record.
.PP
See the description of namelist I/O in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLLGNM Namelist variable name "%s" is longer\n\
  than 63 characters.
$nexp FENLLGNM
Namelist variable name \&'\fIname\*C' is longer\
than 63 characters.
.PP
A namelist \*FREAD\fR statement encountered a variable name that
was too long (greater than 63 characters).  If the name is part
of a derived type entity, each component name must be less
than 64 characters.
.PP
Check the namelist input file to ensure that the variable name
is correct.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIVGP Namelist input group name "%s" does not\n\
  match READ group name. 
$nexp FENLIVGP
The namelist input group name \&'\fIname\*C' does not\
match READ group name.
.PP
A namelist \*FREAD\fR statement encountered a group name that
does not match the name of the namelist group in the READ
statement.  By default on PVP systems, an error is given
for the mismatched name.  By default on IRIX systems,
the namelist input record is skipped and no message
is issued for the skipped record.
.PP
To get the IRIX behavior on PVP systems, use
the assign option -Y on.
To get the PVP behavior on IRIX systems, use
the assign option -Y off.
.PP
For Fortran files using the assign -f 77 option, the record
is skipped by default and a message indicating the skip
occurred.  The skip can be suppressed by
the \*FRNLSKIP\fR(3) subroutine.
.PP
Check the namelist input file to ensure that the group matches
the group in the namelist READ statement.
.PP
See the description of \*FNAMELIST\fR input
and \*FRNLSKIP\fR(3) in your Fortran
reference manual.  See the \*Fassign\fR(1)
command.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIVIT Integer namelist variable "%s" equivalenced\n\
  to floating point data.
$nexp FENLIVIT
Integer namelist variable \&'\fIname\*C' equivalenced\
to floating point data.
.PP
When the RNLTYPE mode is zero, no conversion occurs if
floating point data is supplied for an integer variable.
Provide only integer input data for the integer entity
when the mode is zero and nonstandard namelist I/O is
specified for the file.
.PP
Check the namelist input file to ensure that it contains the
proper data.
.PP
See the man page for the \*FRNLSKIP\fR subroutine.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLONEC The leading character of a NAMELIST name or termination was unrecognized.
$nexp FENLONEC
The leading character of a NAMELIST name or termination was unrecognized.
.PP
The leading character of a NAMELIST name was not an ampersand or a
dollar sign or the character to terminate the namelist input record
was not a slash or the character preceding END did not match the
character used before the NAMELIST name.
.PP
Check the namelist input file to ensure that it contains the proper data.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLUNKI Incorrect namelist input variable "%s" or\n\
  mismatched input value.
$nexp FENLUNKI
The namelist input variable \&'\fIname\*C' or the value\
is incorrect.
.PP
The input qualification on a namelist variable name was incorrect
or the type of the data does not match the type of the variable.
The input may contain a subscript or substring when the namelist
name is not an array or character item.  A subscript or substring
may be incorrect.  For standard namelist input, the type of the
input data must match the type of the variable.
.PP
Check the namelist input file to ensure that it contains the
name qualification or the proper input value.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLZRCH A character variable declared as zero length appeared in namelist input.
$nexp FENLZRCH
A character variable declared as zero length appeared in namelist input.
.PP
The character variable name in a namelist input statement must not be
the name of a zero-length character string.
.PP
Check the namelist input file to ensure that it contains the proper data.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLARSC Array section "%s" in nonstandard namelist\n\
  input is not allowed.
$nexp FENLARSC
Array section \&'\fIname\*C' in nonstandard namelist\
input is not allowed.
.PP
A namelist \*FREAD\fR statement encountered an array section while
processing a nonstandard input record.  The array section is only
allowed in a standard namelist input record.
.PP
Check the namelist input file to ensure that it contains the proper
array variables.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIOER An error occurred on a namelist READ operation.
$nexp FENLIOER
An error occurred on a namelist READ operation.
.PP
A namelist \*FREAD\fR statement encountered an error that made further
processing impossible.
.PP
Check the namelist input file to ensure that it is in the proper
format.
.PP
See the \*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLTYPE An invalid character was passed to a namelist output control routine.
$nexp FENLTYPE
An invalid character was passed to a namelist output control routine.
.PP
A character that was not valid was passed to a namelist output routine.  The
\*FWNLDELM\fR, \*FWNLFLAG\fR, \*FWNLREP\fR, and \*FWNLSEP\fR routines 
all require a character
argument that has an ordinal value in the range of 1 to 127 (decimal) inclusive.
.PP
Ensure that the character being passed to the namelist output
control routine is a valid character.
.PP
See the description of \*FWNL\fR(3) in \*(aL
and the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLLONG A namelist variable name must consist of fewer than 64 characters.
$nexp FENLLONG
A namelist variable name must consist of fewer than 64 characters.
.PP
A namelist \*FREAD\fR statement encountered a variable name that was too
long (greater than 63 characters).
.PP
Check the namelist input file to ensure that the variable name is correct.
.PP
See the \*FNAMELIST\fR statement in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIVGN The namelist input group name does not match the name being sought. 
$nexp FENLIVGN
The namelist input group name does not match the name being sought.
.PP
A namelist \*FREAD\fR statement encountered a group name that did not
match the name of the group being sought.  By default, namelist input
issues a message and skips the record in this case; but the
\*FRNLSKIP\fR(3) routine can be used to suppress record skipping and
enable this error condition.
.PP
Check the namelist input file to ensure that the group matches
the group being read.
.PP
See the description of
\*FRNLSKIP\fR(3) and
the \*FNAMELIST\fR statement in your
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLUNKN The variable name "%s" is not a member of the\n\
  current namelist group.
$nexp FENLUNKN
The variable name \&'\fIname\*C' is not a member of the\
current namelist group.
.PP
A namelist \*FREAD\fR statement encountered a variable name in the
current namelist record that is not a member of the current namelist
group.
.PP
Check the namelist input file to ensure that input is in the
proper format.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLNOVL The namelist input is missing an equal (=) separator.
$nexp FENLNOVL
The namelist input is missing an equal (=) separator.
.PP
A namelist \*FREAD\fR statement encountered input that was not
in the proper format for namelist input.  Specifically, the equal
character (=) that separates the namelist variable name from
its value(s) is missing.  For example, the following namelist input
record is missing the equal character:
.CS
&X   A   3.14159  &END
       ^  equal character missing
.CE
.PP
Check the namelist input file to ensure that input is in the
proper format.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIVLG A namelist variable is equivalenced to invalid logical data.
$nexp FENLIVLG
A namelist variable is equivalenced to invalid logical data.
.PP
A namelist \*FREAD\fR statement encountered input that was not
a valid logical value.  Logical input must begin with the character
sequences \*F.T\fR, \*F.F\fR, \*FT\fR, \*FF\fR, or
any lowercase variant.
.PP
Check the input file to ensure that the logical data is correct,
or if nonlogical data is desired, change the type of the namelist
variable(s) being read.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLIVCX A namelist variable is equivalenced to invalid complex data.
$nexp FENLIVCX
A namelist variable is equivalenced to invalid complex data.
.PP
A namelist \*FREAD\fR statement encountered input that was
not a valid complex number.
Complex numbers must be in the following form:
.CS
( \fInumber\*C , \fInumber\*C )
.CE
.PP
\fInumber\fR is a valid integer or floating-point number.
.PP
Correct the input to conform to the requirements of complex
data, or if noncomplex data is desired, change the type of the
namelist variable(s) being read.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLRECL The input record is too long for a namelist READ.
$nexp FENLRECL
The input record is too long for a namelist READ.
.PP
A namelist \*FREAD\fR operation tried to read a record that was larger
than the largest record that can be processed, or the final record in a
file contained no terminating newline.
.PP
Reduce the size of the record to be read or increase the
size of the largest record that can be read.
If the last record of the file has no terminating newline, add the newline.
.PP
See the descriptions of the
\*FNAMELIST\fR statement and \*FWNLLONG\fR(3)
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLBNDY A namelist read was attempted for a variable outside an array boundary.
$nexp FENLBNDY
A namelist read was attempted for a variable outside an array boundary.
.PP
A namelist \*FREAD\fR operation was requested for variables
outside of the array.
.PP
Example:
.CS
DIMENSION A(10)
NAMELIST /X/ A
READ (1,X)
.CE
.PP
The file connected to unit 1 contains the following:
.CS
&X   A(11) =  1.  &END
.CE
.PP
Ensure that the array subscripts being read are valid.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLTOOM Too many namelist elements were specified.
$nexp FENLTOOM
Too many namelist elements were specified.
.PP
A namelist \*FREAD\fR operation encountered an input value that was
repeated more times than was necessary.
.PP
Example:
.CS
DIMENSION A(10)
NAMELIST /X/ A
READ (1,X)
.CE
.PP
The file connected to unit 1 contains the following:
.CS
&X   A =  12*1.  &END
.CE
.PP
Ensure that the number of namelist items being read matches the
number of items that were written.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLNREC The variable name '%s' is unrecognized in namelist input.
$nexp FENLNREC
The variable name \&'\fIname\*F' is unrecognized in namelist input.
.PP
A namelist \*FREAD\fR statement encountered a variable name in the current
namelist record that is not a member of the current namelist group.
.PP
Check the namelist input file to ensure that input is in the
proper format.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLTYPI The namelist variable "%s" is equivalenced to data of the wrong type.\n\
  This variable is declared type "%s".
$nexp FENLTYPI
The namelist variable \&'\fIname\*F' is equivalenced to data of the wrong \
type. This variable is declared type \&'\fItype\*F'.
.PP
A namelist \*FREAD\fR operation tried to read a variable that was of an
incompatible type.  The integer, real, and double-precision data
types are generally interchangeable (among themselves) as namelist
input.  Other Fortran data types (complex, logical, and character)
are not interchangeable among themselves or with the
other data types.
.PP
Ensure that the data type being read matches the data type that
was written.
.PP
See the description of the
\*FNAMELIST\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLNMSZ The namelist name is larger than the size of the output record.
$nexp FENLNMSZ
The namelist name is larger than the size of the output record.
.PP
A namelist \*FWRITE\fR statement tried to write a namelist group name
or a group object name that was longer than the size of the output record.
.PP
See the description of \*FNAMELIST\fR \s-1I/O\s+1 in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLDBCP Complex(kind=16) data must be accessed in namelist I/O from an F90 file.
$nexp FENLDBCP
Complex(kind=16) data must be accessed in namelist I/O from an F90 file.
.PP
The namelist specified in a namelist \*FREAD\fR or \*FWRITE\fR statement
contained a variable declared as \*CCOMPLEX(kind=16)\fR.  The file specified
in the namelist input/output  statement was opened by a program compiled
with CF77 or assigned as a CF77-compatible file.  Variables declared as 
\*CCOMPLEX(kind=16)\fR may only occur in CF90 files.
.PP
See the description of \*FNAMELIST\fR \s-1I/O\s+1 in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLSTCT Structure data must be accessed in namelist I/O from a CF90 file.
$nexp FENLSTCT
Structure data must be accessed in namelist I/O from a CF90 file.
.PP
The namelist specified in a namelist \*FREAD\fR or \*FWRITE\fR statement
contained a variable declared as a structure.  The file specified in the
namelist input/output  statement was opened by a program compiled with
CF77 or assigned as a CF77-compatible file.  Variables declared as structures
or structure components may only occur in CF90 files.
.PP
See the description of \*FNAMELIST\fR \s-1I/O\s+1 in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENLPRAM A nonblank character precedes the ampersand indicating a namelist group name.
$nexp FENLPRAM
A nonblank character precedes the ampersand indicating a namelist group name.
.PP
The CF90 namelist feature does not allow nonblank characters before the
ampersand that precedes the namelist group name.  If the file specified
in the namelist \*FREAD\fR statement is opened by a program unit compiled
with CF77 or is assigned as a CF77-compatible file, the run-time library will
process the namelist input file as CF77 namelist input file.
.PP
See the description of \*FNAMELIST\fR \s-1I/O\s+1 in your Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	BUFFER IN / BUFFER OUT / GETPOS / SETPOS errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIONDA A direct access file cannot be used with %s.
$nexp FEBIONDA
A direct access file cannot be used with \&'\fIoperation\*F'.
.PP
A \*FBUFFER IN\fR, \*FBUFFER OUT\fR, \*FGETPOS\fR(3), or
\*FSETPOS\fR operation was attempted
on a file opened for direct-access \s-1I/O\s+1.
.PP
Either open the file for sequential-access \s-1I/O\s+1 or remove the operation
that caused the error.
.PP
See the description of
\*F BUFFER IN/BUFFER OUT \fRstatements
and the descriptions of
\*FGETPOS\fR(3) and \*FSETPOS\fR 
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIONFM A formatted file cannot be used with %s.
$nexp FEBIONFM
A formatted file cannot be used with \&'\fIoperation\*F'.
.PP
A \*F BUFFER\ IN\ \fRor\*F BUFFER OUT\ \fRoperation was attempted 
on a file opened for formatted \s-1I/O\s+1.
.PP
Either open the file for unformatted \s-1I/O\s+1 or remove the operation
that caused the error.
.PP
See the description of
\*F BUFFER IN/BUFFER OUT \fRstatements
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIOFWA The starting address is greater than the ending address on a %s.
$nexp FEBIOFWA
The starting address is greater than the ending address on a \&'\fIoperation\*C'.
.PP
A\*F BUFFER IN\ \fRor\*F BUFFER OUT\ \fRoperation was attempted with a starting
address greater than the ending address.
.PP
Check the starting and ending addresses on the \*FBUFFER IN/BUFFER OUT\fR
statement to ensure that the starting address is less than the
ending address.
.PP
See the description of \*FBUFFER IN/BUFFER OUT\fR
statements in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIOISP The position argument to SETPOS is invalid.
$nexp FEBIOISP
The position argument to SETPOS is invalid.
.PP
The position parameter to \*FSETPOS\fR did not correspond to a valid
file position.
It should be positioned as reflected in the return value from a call to
the \*FGETPOS\fR routine.
.PP
See the description of
\*FSETPOS\fR in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIOSNT The requested positioning operation is not supported.
$nexp FEBIOSNT
The requested positioning operation is not supported.
.PP
\*FSETPOS\fR is not supported for files assigned a structure of
\*Fbin\fR.
\*FSETPOS\fR with a position parameter of \-1 is not supported for tape
files, nor is \*FGETPOS\fR(3) (with old-style position parameters).
The \*FLENGTH\fR parameter to \*FGETPOS\fR or \*FSETPOS\fR
(with new-style position parameters) was not valid.
.PP
Review the file structure and media, along with the position
parameters on the \*FGETPOS\fR and \*FSETPOS\fR requests, to ensure that the
requested operation is supported.
.PP
See the description of
\*Fassign\fR(1) in \*(aI
and the descriptions of
\*FGETPOS\fR(3) and \*FSETPOS\fR in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEMIXAUX Fortran I/O statements cannot be used while a file is open for\n\
  nonstandard types of I/O.
$nexp FEMIXAUX
Fortran I/O statements cannot be used while a file is open for \
nonstandard types of I/O.
.PP
Fortran \s-1I/O\s+1 statements (for example, \*FWRITE\fR) are not valid while the file 
is open for asynchronous queued \s-1I/O\s+1 (\*FAQOPEN\fR), word addressable
\s-1I/O\s+1 
(\*FWOPEN\fR), \s-1MSIO\s+1 (\*FOPENMS\fR), or \s-1DRIO\s+1 (\*FOPENDR\fR).
For example, the following is not valid:
.CS
CALL AQOPEN(aqp, aqpsize, 10, istat)
WRITE(10)BUF
.CE
.PP
If possible, replace the statement in your program that caused the error with
a routine from the \s-1I/O\s+1 package used to open the file.
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$       ENCODE / DECODE errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEDECDRL The DECODE record length must be between 1 and the maximum record length.
$nexp FEDECDRL
The DECODE record length must be between 1 and the maximum record length.
.PP
The record length (number of characters) on a \*FDECODE\fR statement was
less than 1 or greater than the maximum record length.
.PP
Correct the \*FDECODE\fR statement.
.PP
See the description of
outmoded features
in your Fortran reference manual.
.PP
\*FDECODE\fR is an outmoded feature; use internal files instead.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEENCDRL The ENCODE record length must be between 1 and the maximum record length.
$nexp FEENCDRL
The ENCODE record length must be between 1 and the maximum record length.
.PP
The record length (number of characters) on an \*FENCODE\fR statement was
less than 1 or greater than the maximum record length.
.PP
Correct the \*FENCODE\fR statement.
.PP
See the description of
outmoded features
in your Fortran reference manual.
.PP
\*FENCODE\fR is an outmoded feature; use internal files instead.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBIOFWD An invalid mixture of data types is being used.
$nexp FEBIOFWD
An invalid mixture of data types is being used.
.PP
For some file types, only certain combinations of data types are allowed for
the items between the beginning and end locations of the \*F BUFFER IN\ \fR 
and\*F BUFFER OUT\ \fRstatements.  For example, the following code segment
will cause this error:
.CS
COMPLEX COMPX1, COMPX2
REAL REAL1
COMMON /BLK/ COMPX1, REAL1, COMPX2
CALL ASSIGN('ASSIGN -s bin u:1')
BUFFEROUT(1,1)(COMPX1, COMPX2)
.CE
.PP
Correct the\*F BUFFER IN\ \fRor\*F BUFFER OUT\ \fRstatement.
.PP
See \*Fassign\fR(1) in \*(aI
and the descriptions of the\*F BUFFER IN\ \fR and\*F BUFFER OUT\ \fRstatements in
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEADVSPC The value of the ADVANCE= specifier on a READ or WRITE statement was not \n\
  'YES' or 'NO'.
$nexp FEADVSPC
The value of the ADVANCE= specifier on a READ or WRITE statement was not 'YES' or 'NO'.
.PP
A Fortran \*FREAD\fR or \*FWRITE\fR statement specified an unrecognizable
\*FADVANCE=\fR specifier value.  The value of the \*FADVANCE=\fR specifier 
must be \*F\&'YES'\fR or \*F'NO'\fR.
When \*FADVANCE=\fR is not specified, \*F\&'YES'\fR is assumed.
Trailing blanks are allowed in the value (for example, \*F\&'NO \&'\fR ).
.PP
Ensure that the \*FADVANCE\fR specifier is spelled correctly.
.PP
Uppercase \*FADVANCE=\fR specifier values are equivalent to lowercase.
.PP
See the description of the \*FREAD\fR and \*FWRITE\fR statements
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEADVSIZ The SIZE= specifier was provided on a READ statement without the\n\
  ADVANCE='NO' specifier.
$nexp FEADVSIZ
The SIZE= specifier was provided on a READ statement without the \
ADVANCE='NO' specifier.
.PP
The \*FSIZE=\fR specifier can be used on a formatted Fortran \*FREAD\fR 
statement only when nonadvancing \s-1I/O\s+1 is requested with the \*FADVANCE='NO'\fR
specifier.
.PP
When \*FADVANCE\fR is not specified, \*F\&'YES'\fR is assumed.
.PP
See the description of the \*FREAD\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEADVEOR A EOR= specifier was provided on a READ statement without ADVANCE='NO'.
$nexp FEADVEOR
A EOR= specifier was provided on a READ statement without ADVANCE='NO'.
.PP
The \*FEOR=\fR specifier can be used on a Fortran \*FREAD\fR statement only
if nonadvancing \s-1I/O\s+1 is requested with \*FADVANCE='NO'\fR.
.PP
When \*FADVANCE\fR is not specified, \*F\&'YES'\fR is assumed.
.PP
See the description of the \*FREAD\fR statement
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	TAPE routine errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPNBN A negative tape block number is invalid.
$nexp FETAPNBN
A negative tape block number is invalid.
.PP
The tape block number passed to the \*FSETTP\fR(3) routine was negative.
.PP
Set the \fInbs\fR parameter to \*F1H-\fR to indicate a negative tape block
number, and use the absolute value of the block number.
Correct the call to the \*FSETTP\fR routine.
.PP
See the description of
\*FSETTP\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPNBS The NBS parameter on a tape positioning request is invalid.
$nexp FETAPNBS
The \fInbs\*C parameter on a tape positioning request is invalid.
.PP
The tape block number sign passed to the \*FSETTP\fR(3) routine was
not valid.  The \fInbs\fR parameter must be \*F1H+\fR, \*F1H-\fR, or \*F1H \fR.
.PP
Correct the call to the \*FSETTP\fR routine.
.PP
See the description of
\*FSETTP\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPNVS The NVS parameter on a tape positioning request is invalid.
$nexp FETAPNVS
The \fInvs\*C parameter on a tape positioning request is invalid.
.PP
The tape volume number sign passed to the \*FSETTP\fR(3) routine was
not valid.  The \fInvs\fR parameter must be \*F1H+\fR, \*F1H-\fR, or \*F1H \fR.
In \s-1UNICOS\s+1 6.1 and earlier systems, only \*F1H\fR  (blank) is allowed.
.PP
Correct the call to the \*FSETTP\fR routine.
.PP
See the description of
\*FSETTP\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPBSX The tape block size exceeds the maximum allowed.
$nexp FETAPBSX
The tape block size exceeds the maximum allowed.
.PP
The program attempted to read or write a tape block that was larger
than the maximum block size specified with the \*Ftpmnt\fR(1) command.
.PP
Either decrease the size of the block or increase the maximum tape
block size on the \*Ftpmnt\fR(1) command.
.PP
See the description of
\*Ftpmnt\fR(1) in the \fI\s-1UNICOS\s+1 User Commands Reference
Manual\fR, publication \s-1SR\-2011\s+1.
.PP
This error message is issued only on \s-1CRAY PVP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPCMB This combination of tape positioning parameters is invalid.
$nexp FETAPCMB
This combination of tape positioning parameters is invalid.
.PP
The parameters to the \*FSETTP\fR(3) routine specify an action that is not valid.
For example, if the block number sign (\fInbs\fR) parameter is either \*F1H+\fR
or \*F1H-\fR , neither the volume number (\fInv\fR) nor volume identifier
(\fIvi\fR) parameter can be specified.  If the volume number (\fInv\fR)
parameter is specified, the volume identifier (\fIvi\fR) parameter
must not be specified.
.PP
Correct the call to the \*FSETTP\fR routine.
.PP
See the description of
\*FSETTP\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPUTE A READ operation encountered an unrecovered tape error.
$nexp FETAPUTE
A READ operation encountered an unrecovered tape error.
.PP
A Fortran \*FREAD\fR operation encountered an unrecovered error on a tape
file.
.PP
You can try rerunning your program or, if you want to process or
skip the bad data, you can call the \*FACPTBAD\fR(3) and \*FSKIPBAD\fR(3) routines.
.PP
See the description of
\*FACPTBAD\fR(3) and \*FSKIPBAD\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETASKPF A SKIPF operation could not be performed. 
$nexp FETASKPF
A SKIPF operation could not be performed.
.PP 
The program attempted a \*FSKIPF\fR operation with a positive value
in the file parameter for an online tape file mounted without
the \*F\-T\fR parameter on the \*Ftpmnt\fR(1) command.  This is not supported.
.PP
Either remove the \*FSKIPF\fR operation or change the \*Ftpmnt\fR command.
.PP
See the description of
\*Ftpmnt\fR(1) in the \fI\s-1UNICOS\s+1 User Commands Reference
Manual\fR, publication \s-1SR\-2011\s+1,
and \*FSKIPF\fR(3) in your Fortran reference manual.
.PP
This error message is issued only on \s-1CRAY PVP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETAPNVY The NV parameter on a tape positioning request is invalid.
$nexp FETAPNVY
The NV parameter on a tape positioning request is invalid.
.PP 
The tape volume number passed to the \*FSETTP\fR(3) routine was
not valid.  This error occurs when the program requests positioning
backward past the first volume in the \s-1VSN\s+1 list or forward past
the last volume in the \s-1VSN\s+1 list.
.PP 
Correct the call to the \*FSETTP\fR routine.
.PP
See \*FSETTP\fR(3) in your Fortran reference manual.
.PP
This error message is issued only on \s-1CRAY PVP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Table management errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETBARGS Invalid number of arguments passed to library routine "%s".
$nexp FETBARGS
Invalid number of arguments passed to library routine \&'\fIname\*F'.
.PP 
The \fIname\fR library routine was called with the incorrect number of
arguments; all arguments must be present unless marked as
optional.
.PP 
Correct the call to the routine.
.PP
See Table Management Routines in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETBNTAB NTAB not in the range 1<=NTAB<=64 for library routine "%s".
$nexp FETBNTAB
NTAB not in the range 1 <= NTAB <= 64 for library routine \&'\fIname\*F'.
.PP
The \fIname\fR library routine was called with the value of \*FNTAB\fR not
in the range of 1 to 64.  Any other value is not valid.
.PP
Correct the value of \*FNTAB\fR on the call to the routine.
.PP
See Table Management Routines in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETBTNUM Invalid table number passed to library routine "%s".
$nexp FETBTNUM
Invalid table number passed to library routine \&'\fIname\*F'.
.PP 
The \fIname\fR library routine was called with a table number not
in the range of 1 to \*FNTAB\fR.  Any other value is not valid.
.PP 
Correct the call to the routine.
.PP
See Table Management Routines in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FETBINCR Increment specified would make table length negative for routine "%s".
$nexp FETBINCR
Increment specified would make table length negative for routine \&'\fIname\*F'.
.PP
The \fIname\fR library routine was called with an increment that would
make the specified table length negative.
.PP
Check the increment and the current table length and correct
the call to the routine.
.PP
See Table Management Routines in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Miscellaneous errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEUBCINV The reading or writing of partial word data is not supported on this file.
$nexp FEUBCINV
The reading or writing of partial word data is not supported on this file.
.PP
A call to the \*FREAD\fR(3), \*FREADP\fR,
\*FWRITE\fR(3), or \*FWRITEP\fR
routine specified an unused bit count (selected a nonword-multiple
of data) that is not supported for this type of file.
.PP
Change the file structure to one that supports partial word \s-1I/O\s+1,
or do not attempt to read or write partial words.
.PP
See the description of
\*FREAD\fR(3), \*FREADP\fR, \*FWRITE\fR(3),
and \*FWRITEP\fR in your Fortran reference manual.
.PP
This error message is issued only on \s-1CRAY PVP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FELDDCNV Implicit data conversion for the specified unit cannot be performed\n\
  because the selected foreign data conversion is disabled.
$nexp FELDDCNV
Implicit data conversion for the specified unit cannot be performed \
because the selected foreign data conversion is disabled.
.PP
A \*FREAD\fR or \*FWRITE\fR statement was attempted for a unit with implicit
data conversion selected by the \*C\-N\fR or \*C\-C\fR option on the
\*Cassign\fR(1) command, but the corresponding data conversion type is
disabled by default.  If a program uses data conversion types that are disabled
by default, the \*F\-j \fIdirfile_name\fR option must be specified on the
\*Fsegldr\fR(1) command line; \fIdirfile_name\fR is the name of the \*Fsegldr\fR
directives file that enables the data conversion type.
.PP
For example, to enable \s-1CDC \s+1data conversion at a site where it is disabled
by default, specify the \*F-j ff_cdc\fR option on the \*Fsegldr\fR(1) command
line.
.PP
Some types of data conversion are unavailable on \s-1Cray MPP\s+1 and
\s-1Irix\s+1 systems.
.PP
See \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENCNV90 Unformatted I/O encountered an I/O list item of type %s%s.\n\
  The current implicit data conversion does not support this data type.
$nexp FENCNV90
Unformatted I/O encountered an I/O list item of type dtype. \
The current implicit data conversion does not support this data type.
.PP
The implicit data conversion type specified with the \*F-N\fR or \*F-C assign\fR options
for the current unit does not support conversion between the CRI and foreign
data formats for a variable or value with type \fIdtype\fR.
.PP
This error condition is most often encountered with variables declared with
a \*F*n\fR or \*FKIND=k\fR specification.  In these cases the library must usually 
convert an item between CRI and foreign data formats of the same size.
Many of these data conversion combinations are not supported.
.PP
See \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEKNTSUP An I/O list contains a variable with an unsupported type and kind.
$nexp FEKNTSUP
An I/O request list has a variable with an unsupported type and kind.
.PP
A variable in the \s-1I/O\s+1 list for a Fortran \*FREAD\fR or \*FWRITE\fR statement
was declared with a nondefault \*FKIND\fR value.   A variable with the
specified type and \*FKIND\fR value is not supported for the current
type of \s-1I/O\s+1.
.PP
Remove this \s-1I/O\s+1 list item from the current \s-1I/O\s+1 statement.
.PP
This message is usually issued when an old library set is linked with
a relocatable binary generated with a newer compiler which supports the
type with the specified \*FKIND\fR value.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESHRSUP Shared variables are unsupported for this type of I/O.
$nexp FESHRSUP
Shared variables are unsupported for this type of I/O.
.PP
An \s-1I/O\s+1 statement was called to read or write a shared variable.
This is unsupported for the current type of \s-1I/O\s+1.
.PP
Use a different type of \s-1I/O\s+1 statement, or use private variables.
Shared variables are allowed with Fortran unformatted \*FREAD\fR and \*FWRITE\fR
statements. Shared variables are also allowed with \*FBUFFER IN\fR or
\*FBUFFER OUT\fR statements, provided the file is not assigned with
\*F-F system\fR or \*F-s u\fR.
.PP
This error message is issued only on Cray \s-1MPP\s+1 systems.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENOICNV Data conversion is not supported on this type of file.
$nexp FENOICNV
Data conversion is not supported on this type of file.
.PP
The \*F-C\fR or \*F-N\fR assign options are selected for a Fortran file open,
but data conversion is not supported for this type of file.
.PP
The Fortran run-time library is unable to handle implicit data conversion
when the \*FFFIO\fR routines are not being used as the underlying I/O handlers.
This can happen when the '-s sbin' assign option is in effect or the file is 
a direct access formatted file.    
.PP
This error code will be avoided if a \*F-F\fR assign option is specified for 
this open.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEARGLST Invalid call to library routine "%s". 
$nexp FEARGLST
Invalid call to library routine \&'\fIname\*C'.
.PP
A library routine was called with the wrong number of arguments or with the
wrong type of arguments. 
.PP
Correct the call to the \fIname\fR routine.
.PP
See the man page for the specified routine (enter \*Fman \fIname\fP\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEKLUDG1 The READ or READP routine received a system error 1.
$nexp FEKLUDG1
The READ or READP routine received a system error 1.
.PP
A call to the \*FREAD\fR(3) or \*FREADP\fR
routine generated a system
error 1.  Because 1 is already defined as a different error, it is
mapped to this error number.
.PP
See the explanation for system error 1 (enter \*Fexplain sys-1\fR).
.PP
See the description of
\*FREAD\fR(3) and \*FREADP\fR
in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEKLUDG2 The READ or READP routine received a system error 2.
$nexp FEKLUDG2
The READ or READP routine received a system error 2.
.PP
A call to the \*FREAD\fR(3) or \*FREADP\fR routine generated a system
error 2.  Because 2 is already defined as a different error, it is
mapped to this error number.
.PP
Refer to the explanation for system error 2 (enter \*Fexplain sys-2\fR).
.PP
See the description of
\*FREAD\fR(3) and \*FREADP\fR in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEKLUDG3 The READ or READP routine received a system error 3.
$nexp FEKLUDG3
The READ or READP routine received a system error 3.
.PP
A call to the \*FREAD\fR(3) or \*FREADP\fR routine generated a system
error 3.  Because 3 is already defined as a different error, it is
mapped to this error number.
.PP
Refer to the explanation for system error 3 (enter \*Fexplain sys-3\fR).
.PP
See the description of
\*FREAD\fR(3) and \*FREADP\fR in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEKLUDG4 The READ or READP routine received a system error 4.
$nexp FEKLUDG4
The READ or READP routine received a system error 4.
.PP
A call to the \*FREAD\fR(3) or \*FREADP\fR routine generated a system
error 4.  Because 4 is already defined as a different error, it is
mapped to this error number.
.PP
See the description of
\*FREAD\fR(3) and \*FREADP\fR in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	General routine errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEARGCNT Invalid call to library routine "%s"; %d argument(s) were passed,\n\
  but %s argument(s) are required.
$nexp FEARGCNT
Invalid call to library routine \&'\fIname\*C'; \fIn1\*C argument(s) were passed,\
but \fIn2\*C argument(s) are required.
.PP
The \fIname\fR routine was called with an unsupported number of arguments; 
\fIn2\fR argument(s) are required.
.PP
Correct the call to the \fIname\fR routine.
.PP
See the man page for the specified routine (enter \*Fman \fIname\fP\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEARGSHP For the entry '%s', called from '%s' at line %d:\n\
  Shape mismatch for argument "%s".\n\
    actual argument is %s\n\
    dummy  argument is %s
$nexp FEARGSHP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*
Z \
Shape mismatch for argument \&"\fIarg\*C".
.ES
    actual argument is \&"\fIashape\*C"
    dummy  argument is \&"\fIdshape\*C"
.EE
.PP
A shared dummy argument has more declared dimensions than its corresponding
actual parameter or the extent of one or more dimensions of a dummy
argument are inconsistent with their corresponding actual parameter.
.PP
If argument shape checking is enabled, this message is issued when
a shared dummy argument has more declared dimensions than its corresponding
actual parameter or the extent of one or more dimensions of a dummy
argument are inconsistent with their corresponding actual parameter.
Argument shape consistency is formally defined as follows:
.PP 
.ES
Let (a_1, a_2, .., a_r) = the extents of the actual parameter.
    (f_1, f_2, .., f_s) = the extents of the corresponding dummy argument.
 
Weak shape conformance (WSC) means:
   s <= r and ( 1 <= i <= s - 1 ==> a_i = f_i ).
 
Strong shape conformance (SSC) means:  WSC and f_s <= a_s.
.EE
.PP
When shape checking in enabled:
.CS
if      ( WSC is violated ) FATAL ERROR
else if ( SSC is violated ) WARN (first occurence only).
.CE
.PP
Since sequence association is not supported for distributed data,
programs which violate this constraint are illegal.
.PP
Modify the program so that dummy argument extents are consistent
with all corresponding actual parameters.
.PP
See your Fortran reference manual.
.PP
The error class is \s-1ERROR\s+1 (issued by Fortran program).
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESCIDIM Illegal value for DIM argument to an array intrinsic function.
$nexp FESCIDIM
Illegal value for DIM argument to an array intrinsic function.
.PP
The value of the \*CDIM\fR argument to an array intrinsic function must be
within the range:
.CS
   1 .LE. DIM .LE. N
.CE
.PP
where \*FN\fR is the number of dimensions.
.PP
See the Fortran reference manual for more information on the \fIdim\fR 
argument
for the specific array intrinsic.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESCIRNK Illegal rank for matrix argument to MATMUL array intrinsic.
$nexp FESCIRNK
Illegal rank for matrix argument to MATMUL array intrinsic.
.PP
If argument\*C IMATRIX B\ \fRto the array intrinsic function \*FMATMUL\fR has one
dimension, then argument\*C MATRIX A\ \fRmust have two dimensions.
.PP
See the description of array intrinsic \*FMATMUL\fR in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESCICNF Matrix arguments to MATMUL array intrinsic are not conformable.
$nexp FESCICNF
Matrix arguments to MATMUL array intrinsic are not conformable.
.PP
The shape of the arguments and the result matrix of \*FMATMUL\fR must be
conformable.  Two arrays are conformable if they have the same shape.  The
shape of an array is determined by its rank and its extent in each dimension.
The rank of an array is the number of dimensions.
.PP
See the description of the \*FMATMUL\fR array intrinsic in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERSHNPD Size of SOURCE array for RESHAPE array intrinsic is less than size\n\
  of SHAPE array when PAD argument is zero or is not present. 
$nexp FERSHNPD
Size of SOURCE array for RESHAPE array intrinsic is less than size of SHAPE \
array when PAD argument is zero or is not present.
.PP
If the \fIpad\fR argument of \*FRESHAPE\fR is zero or is not present,
the size of the source array must be greater than or equal to
\fIproduct(shape).
.PP
See the description of the \*FRESHAPE\fR array intrinsic in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERSHNEG The SHAPE array for the RESHAPE array intrinsic function contains\n\
  a negative value.
$nexp FERSHNEG
The SHAPE array for the RESHAPE array intrinsic function contains a negative value.
.PP
The \fIshape\fR argument of \*FRESHAPE\fR is an integer rank one array with
a positive size less than 8.  It must not have an element whose value is
negative.
.PP
See the description of the \*FRESHAPE\fR array intrinsic in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEPTRARR An inquiry function argument is a disassociated pointer or an\n\
  unallocated array.
$nexp FEPTRARR
An inquiry function argument is a disassociated pointer or an unallocated array.
.PP
The array or pointer argument to the \*FALLOCATED\fR, \*FLBOUND\fR,
\*FSHAPE\fR, \*FSIZE\fR, or \*FUBOUND\fR array inquiry functions or to
the \*FASSOCIATED\fR pointer inquiry function is not currently accessible.
If the argument is a pointer, it is currently disassociated.  If the
argument is an allocatable array, it is currently not allocated.
.PP
See the description of the pointer and array inquiry functions in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEALALLO An allocatable array in the ALLOCATE statement is already allocated.
$nexp FEALALLO
An allocatable array in the ALLOCATE statement is already allocated.
.PP
An allocatable array in an \*FALLOCATE\fR statement must not be currently
allocated.  The \*FALLOCATED\fR intrinsic function can be used to determine
if the allocatable array is currently allocated.
.PP
See the description of the \*FALLOCATE\fR statement in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENODEAL An argument in the DEALLOCATE statement is a disassociated pointer or an\n\
unallocated array.
$nexp FENODEAL
An argument in the DEALLOCATE statement is a disassociated pointer or an unallocated array.
.PP
An allocatable array in a \*FDEALLOCATE\fR statement must be currently
allocated.  The \*FALLOCATED\fR intrinsic function can be used to determine
if the allocatable array is currently allocated.
.PP
A pointer in a \*FDEALLOCATE\fR statement must be currently associated.
The \*FASSOCIATED\fR intrinsic function may be used to determine
if the pointer is currently associated.
.PP
See the description of the \*FALLOCATE\fR statement in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEDEALSZ An argument in the DEALLOCATE statement points to a portion of the original\n\
 allocation. 
$nexp FEDEALSZ
An argument in the DEALLOCATE statement points to a portion of the original allocation.
.PP
The current size of the allocatable array does not match the size of the
original allocation of the array.  A section of the array must not be
deallocated.  The size of the pointer does not match the size of the
target object.  A pointer that is not associated with an entire target
object must not be deallocated.
.PP
See the description of the \*FDEALLOCATE\fR statement in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FERPTNEG The ncopies argument to the REPEAT function is negative.
$nexp FERPTNEG
The ncopies argument to the REPEAT function is negative.
.PP
The \fIncopies\fR argument to the \*FREPEAT\fR function must be scalar and of type
integer.  Its value cannot be negative.
.PP
See the description of the \*FREPEAT\fR intrinsic in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENEARZS The S argument to the NEAREST function is zero.
$nexp FENEARZS
The S argument to the NEAREST function is zero.
.PP
The \fIS\fR argument to the \*FNEAREST\fR function must be scalar and of type
real.  Its value cannot be zero.
.PP
See the description of the \*FNEAREST\fR intrinsic function in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEIPOWZR A zero integer base is being raised to a zero integer power.
$nexp FEIPOWZR
A zero integer base is being raised to a zero integer power.
.PP
A scalar integer value of zero is being raised to a scalar integer power value
of zero.  The zero**zero operation is undefined.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBDORDR ORDER argument to the RESHAPE function contains illegal values.
$nexp FEBDORDR
ORDER argument to the RESHAPE function contains illegal values.
.PP
The values in the ORDER argument to the RESHAPE function are illegal.  The \
ORDER argument must be an integer array of the same shape as the SHAPE \
argument, and must contain a permutation of (1,2,...,n), where n is the rank \
of the result.
.PP
See the description of the \*FRESHAPE\fR array intrinsic in the Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEBADMLD Result of TRANSFER intrinsic with no SIZ argument smaller than source.
$nexp FEBADMLD
Result of TRANSFER intrinsic with no SIZ argument smaller than source.
.PP
The current Fortran standard states that if the MOLD argument to a transfer
call is array valued, and the SIZ argument is absent, the result is array 
valued and of rank one, with its size as small as possible such that its 
physical representation is not shorter than that of source.  A zero-sized MOLD
with no SIZ will return a zero-sized result, which is illegal if the source is 
not also zero-sized.
.PP
See the description of the \*FTRANSFER\fR intrinsic in the Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FESHPSZZ SHAPE argument to RESHAPE function is zero sized array.
$nexp FESHPSZZ
SHAPE argument to RESHAPE function is zero sized array.
.PP
The SHAPE array to the RESHAPE function must be of type integer, rank one, and
constant size.  Its size must be positive and less than 8.  It must not have
an element whose value is negative.  In order to meet these requirements, it
cannot be of size zero.
.PP
See the description of the \*FRESHAPE\fR intrinsic in the Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEVECUNP The VECTOR argument to UNPACK function contains insufficient values\n\
  for the true elements of the MASK array argument.
$nexp FEVECUNP
The VECTOR argument to UNPACK function contains insufficient values for the true elements of the MASK array argument.
.PP
The VECTOR array argument to the UNPACK function must be a rank one
array with a size at least as large as the number of true elements in
the MASK array argument.
.PP
See the description of the \*FUNPACK\fR intrinsic in the Fortran reference
manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENGFLCL Array %s is scoped GETFIRST starting in %s at line %d,\n\
  but has no associated storage on entry to the taskloop.
$nexp FENGFLCL
Array \&'\fIarry\*C' is scoped GETFIRST starting in \&'\fIcaller\*C' at line \fIx\*F, \
but has no associated storage on entry to the taskloop.
.PP
An allocatable array must be allocated and a Fortran pointer
must be associated on entry to any task loop for which the
array or Fortran pointer is scoped GETFIRST.  When a variable
is scoped GETFIRST, each slave thread creates a local copy of
the variable and references its local copy and not the original
object.  A local copy cannot be created before the original
object is allocated or associated.
.PP
Allocate the allocatable array or the Fortran pointer through
the \*FALLOCATE\fR statement.  The \*FALLOCATED\fR intrinsic
function can be used to determine if the allocatable array is
currently allocated.  The \*FASSOCIATED\fR intrinsic function
may be used to determine if the pointer is currently associated.
.PP
See the description of the \*FALLOCATE\fR statement in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEDEASIZ A DEALLOCATE statement argument points to a portion of the\n\
 original allocation.\n\
  Original size in bytes for the argument is %d\n\
  Current size in bytes for the argument is %d
$nexp FEDEASIZ
A DEALLOCATE statement argument points to a portion of the original allocation. \*Z \
  Original size in bytes for the argument is \&'\fIorignum\*F' \*Z \
  Current size in bytes for the argument is \&'\fIcurnum\*F'
.PP
The current size of the allocatable array does not match the size of the
original allocation of the array.  A section of the array must not be
deallocated.  The size of the pointer does not match the size of the
target object.  A pointer that is not associated with an entire target
object must not be deallocated.
.PP
See the description of the \*FDEALLOCATE\fR statement in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENMPTAR An argument to inquiry function %s is a disassociated\n\
  pointer or an unallocated array.
$nexp FENMPTAR
An argument to inquiry function \&'\fIinqfunc\*C' is a disassociated pointer or an unallocated array.
.PP
The array or pointer argument to the \*FALLOCATED\fR, \*FLBOUND\fR,
\*FSHAPE\fR, \*FSIZE\fR, or \*FUBOUND\fR array inquiry functions or to
the \*FASSOCIATED\fR pointer inquiry function is not currently accessible.
If the argument is a pointer, it is currently disassociated.  If the
argument is an allocatable array, it is currently not allocated.
.PP
See the description of the pointer and array inquiry functions in the
Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FENMSCDM Illegal value for DIM argument to array intrinsic %s.
$nexp FENMSCDM
Illegal value for DIM argument to array intrinsic \&'\fIarrfunc\*C'.
.PP
The value of the \*CDIM\fR argument to the specified array intrinsic
function must be within the range:
.CS
   1 .LE. DIM .LE. N
.CE
.PP
where \*FN\fR is the number of dimensions.
.PP
See the Fortran reference manual for more information on the \fIdim\fR 
argument
for the specific array intrinsic.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Libu assign errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_UNFILE The assign environment file cannot be read; check read permissions\n\
  or reassign the FILENV environment variable.
$nexp ERAS_UNFILE
The assign environment file cannot be read; check read permissions or reassign the FILENV environment variable.
.PP
You do not have read permission for the current \*Fassign\fR
environment file.  Read permission is needed for the processing of an
\*Fassign\fR(1) command, for an \*FASSIGN\fR library routine,
or for Fortran \*FOPEN\fR
processing.  The \*Fassign\fR environment file is \*F$TMPDIR/.assign\fR by
default or is the file name stored in the \*FFILENV\fR environment
variable.
.PP
Use the \*Fchmod\fR(1) command to change the permissions mode of the
\*Fassign\fR environment file, giving your program read
permission.  You can invoke the necessary \*Fchmod\fR(1) command in one
of the following ways:
.CS
chmod u+r $FILENV
chmod u+r $TMPDIR/.assign
.CE
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
Also see the man page for the \*Fchmod\fR(1) command (enter \*Fman chmod\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_WRERR The assign environment file cannot be written; check write permissions\n\
   or reassign the FILENV environment variable.
$nexp ERAS_WRERR
The assign environment file cannot be written; check write permissions \
or reassign the FILENV environment variable.
.PP
Update of the \*Fassign\fR environment file was attempted from an \*Fassign
\fRcommand or \*FASSIGN\fR library routine, but you do not have write
permission for the current \*Fassign\fR environment file.  The \*Fassign\fR
environment file is \*F$TMPDIR/.assign\fR by default or is the file
name stored in the \*FFILENV\fR environment variable.
.PP
Use the \*Fchmod\fR(1) command to change the permissions mode of the
\*Fassign\fR environment file to allow your program to have write
permission.  You can invoke the necessary \*Fchmod\fR(1) command in
either of the following ways:
.CS
chmod u+w $FILENV
chmod u+w $TMPDIR/.assign
.CE
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
Also see the man page for the \*Fchmod\fR(1) command (enter \*Fman chmod\fR).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_ATTFMT Attributes in the assign environment file have an unreadable format.
$nexp ERAS_ATTFMT
Attributes in the assign environment file have an unreadable format.
.PP
The \*Fassign\fR attributes or options for a unit or file name in the 
\*Fassign\fR
environment file have a format that is not valid.  The correct format for
each \*Fassign\fR attribute is "\fI\-x value\fR" or "\fI\-x\fR" (\fIx\fR is the
\*Fassign\fR(1) option letter, and \fIvalue\fR is the option value).
.PP
Ensure that all attributes in the \*Fassign\fR environment file have
the correct format.  One way to ensure this is by reissuing all
\*Fassign\fR(1) commands currently in effect.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___504 The value on assign option -%c is invalid.
$nexp FE___504
The value on assign option \fI opt\*C is invalid.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR library routine
(\*FASSIGN\fR(3), \*FASNUNIT\fR, or \*FASNFILE\fR) was called
with an option value that was not valid for the specified option \fIopt\fR.
.PP
Valid option values for \*Fassign\fR options are listed in the
\*Fassign\fR(1) man page.
Ensure that the option value is supported
in the current \s-1UNICOS\s+1 release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___505 Value %s on assign option -%c is invalid.
$nexp FE___505
Value\ \fIoptval\*C on assign option\ \fIopt\*C is invalid.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR, or \*FASNFILE\fR) was called with an option value
\fIoptval\fR that was not valid for option \fIopt\fR.
.PP
Valid option values for \*Fassign\fR options are listed in the
\*Fassign\fR(1) man page.  Ensure that the option value is supported
in the current \s-1UNICOS\s+1 release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___506 Assign option -%c is unrecognized.
$nexp FE___506
Assign option\ \fIopt\*C is unrecognized.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR, or \*FASNFILE\fR) was called with an option \fIopt\fR that was
not valid.
.PP
Valid options for \*Fassign\fR options are listed in
the \*Fassign\fR(1)
man page.  Ensure that option \fIopt\fR is supported in the current
\s-1UNICOS\s+1 release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_FSNSUP The option value "%s" for assign option -s is not supported.
$nexp ERAS_FSNSUP
The option value\ \fIsvalue\*C for assign option -s is not supported.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine
(\*FASSIGN\fR, \*FASNUNIT\fR\, or \*FASNFILE\fR) was called with a value
for the \*F-s\fR option that was not valid.
The \*F-s\fR option value is not supported on this system.
.PP
Valid options for \*Fassign\fR(1) option \*F-s\fR are listed in the
\*Fassign\fR(1) manual page.  Choose a \*F-s\fR option value that is
supported on the system you are using.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_MIXFS Assign options -F and -s cannot be specified together.
$nexp ERAS_MIXFS
Assign options -F and -s cannot be specified together.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR) was called with both a \*F-F\fR and a \*F-s\fR option.
These options
cannot be used together.
.PP
Choose the \*F-s\fR or \*F-F\fR option value most appropriate for your
needs.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_MIXFO Assign option -F cannot be specified with options -b, -o, or -u.
$nexp ERAS_MIXFO
Assign option -F cannot be specified with options -b, -o, or -u.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with the \*F-F\fR option and another option that  
cannot be used with the \*F-F\fR option.
.PP
If you need the functionality offered by the \*F-b\fR, \*F-o\fR, or
\*F-u\fR option, use the \*F-s\fR option instead of the \*F-F\fR option if
possible.  If you need to specify a buffer size (\*F-b\fR option), use one of
the \*F-F\fR option values that allow the specification of a buffer size.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___510 Assign options -N and -C require that option -F also be specified.
$nexp FE___510
Assign options -N and -C require that option -F also be specified.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR, or \*FASNFILE\fR)
was called with the \*F-C\fR or \*F-N\fR option but no \*F-F\fR option.
The \*F-F\fR option must also be specified when \*F-C\fR or \*F-N\fR is used.
.PP
Specify a \*F-F\fR option with the \*F-C\fR or \*F-N\fR option.  The \*F-F\fR
option allows
specification of the correct \s-1FFIO\s+1 file structure.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 1.2
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_ASNCTL The IVALUE argument passed to ASNCTL is invalid.
$nexp ERAS_ASNCTL
The IVALUE argument passed to ASNCTL is invalid.
.PP
The \*FASNCTL\fR(3) library routine was called
with an \fIivalue\fR argument that was not valid.
The calling sequence for \*FASNCTL\fR is as follows:
.CS
CALL ASNCTL(OPTION,IVALUE,IER)
.CE
.PP
Choose an \fIivalue\fR argument from the valid ones described in the
\*FASNCTL\fR man page.
.PP
See \*FASNCTL\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___512 Local assign is already in effect and cannot be specified again.
$nexp FE___512
Local assign is already in effect and cannot be specified again.
.PP
The \*FASNCTL\fR(3) library routine was called with the \*FLOCAL\fR
or \*FNEWLOCAL\fR
options a second time in this program.  The local \*Fassign\fR environment
was already activated at the time of the second request.
.PP
Ensure that \*FASNCTL\fR is not repeatedly called with the
\*F\&'LOCAL'\fR or
\*F\&'NEWLOCAL'\fR options.  If you are trying to remove \*Fassign\fR attributes
currently in effect, call the \*FASNRM\fR(3) routine.
.PP
See \*Fassign\fR(1) in \*(aI and \*FASNCTL\fR(3) in \*(aL.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADTYPE An unrecognized assign object was specified; valid assign object\n\
  prefixes are f: and u:.
$nexp ERAS_BADTYPE
An unrecognized assign object was specified; valid assign object \
prefixes are f: and u:.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine was called
with an
assign object with a format that was not valid.  Assign objects may have any
of the following forms:
.CS
file_name
f:file_name
u:unit_number
.CE
.PP
Only prefixes \*Ff:\fR and \*Fu:\fR are supported.
.PP
Check the spelling of the \*Fassign\fR object.  If the second character
in a file name is a colon, add the \*Ff:\fR prefix at the beginning of the
file name and reissue the \*Fassign\fR command or \*FASSIGN\fR library
routine.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADUNIT An assign to a file name may not use the u: prefix.
$nexp ERAS_BADUNIT
An assign to a file name may not use the u: prefix.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine was called
with a
An numeric unit number as the \*Fassign\fR object.  If assign-by-file name
is desired, the \*Ff:\fR prefix may be used at the beginning of the file name.
If assign-by-unit is desired, specify an \*Fassign\fR object of \*Fu:\fIn\fR, in
which \fIn\fR is the unit number.
.PP
If assign-by-unit is desired, specify an \*Fassign\fR object of
\*Fu:\fIn\fR, in which \fIn\fR is the unit number.
.PP
Check the assign object specifed either on the \*Fassign\fR command
or on the call to the \*FASSIGN\fR library routine.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_INCOV Assign options -I and -O cannot be specified together.
$nexp ERAS_INCOV
Assign options -I and -O cannot be specified together.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with both the \*F-I\fR and \*F-O\fR options.  These options
cannot be used together.  The \*F-I\fR option causes new \*Fassign\fR attributes
to be added without erasing those already active for an \*Fassign\fR
object.  This is called \fIincremental assign\fR.  The \*F\-O\fR option causes
existing \*Fassign\fR attributes to be erased when the new attributes are
added.
.PP
Decide whether incremental or nonincremental \*Fassign\fR is desired.
Reissue the \*Fassign\fR without the \*F-O\fR option if incremental 
\*Fassign\fR is desired.  Otherwise reissue the assign without 
the \*F-I\fR option.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_REMV Assign options -R and -V cannot be specified together.
$nexp ERAS_REMV
Assign options -R and -V cannot be specified together.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with both the \*F-R\fR and \*F-V\fR options.  These options
cannot be used together.
.PP
Issue two \*Fassign\fR commands instead of one.  Use the \*F-R\fR option on
one of them and \*F-V\fR on the other.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_REMAT Assign option -R cannot be specified with other options.
$nexp ERAS_REMAT
Assign option -R cannot be specified with other options.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with the \*F-R\fR option and other options as well.
The \*F-R\fR option causes the deletion of an entry from the \*Fassign\fR
environment; therefore, the addition of attributes at the same time is
incongruous.
.PP
Issue two \*Fassign\fR commands instead of one.  Use the \*F-R\fR option on
the first and the other options on the second.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_VIEWAT Assign option -V cannot be specified with other options.
$nexp ERAS_VIEWAT
Assign option -V cannot be specified with other options.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with the \*F-V\fR option and other options as well.
The \*F-V\fR option allows you to view attributes for a file or unit,
whereas other options allow you to add attributes to the
\*Fassign\fR environment.
.PP
Issue two \*Fassign\fR(1) commands instead of one.  Issue the first
\*Fassign\fR command without \*F-V\fR and the second with
\*F-V\fR as the only option.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___519 More than one assign object was specified.
$nexp FE___519
More than one assign object was specified.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine was issued with
more than one \*Fassign\fR object at a time.
.PP
Issue one \*Fassign\fR(1) command for every \*Fassign\fR object.  
From \*Fsh\fR(1),
the POSIX shell, or \*Fksh\fR(1), the Korn Shell, you can use the
following syntax:
.CS
for i in obj1 obj2 obj3 ;
   do assign <options> $i ; done
.CE
.PP
From \*Fcsh\fR(1), the C shell, you can either use a \*CWHILE\fR loop or
issue individual \*Fassign\fR commands.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_NOOBJS At least one assign object must be specified.
$nexp ERAS_NOOBJS
At least one assign object must be specified.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine was issued with
no \*Fassign\fR objects.  An \*Fassign\fR object is required unless either the
\*F-R\fR or \*F-V\fR option is specified.
.PP
Specify an \*Fassign\fR object when the \*Fassign\fR command is reissued.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___521 The assign to file '%s' conflicts with the assign to unit %d.
$nexp FE___521
The assign to file \&'\fIfname\*C' conflicts with the assign to unit \fIunum\*C.
.PP
An \*FOPEN\fR statement or implicit open for unit \fIunum\fR
tried to connect to file \fIfname\fR.  Both unit \fIunum\fR and
file name \fIfname\fR have \*Fassign\fR attributes associated with them.
The run-time library cannot combine the unit and file name attributes
on the same open.
.PP
Either remove the \*Fassign\fR attributes for file \fIfname\fR or unit
\fIunum\fR by using the \*F-R\fR option on the \*Fassign\fR(1)
command, or remove or change the \*FFILE=\fR specifier
on the \*FOPEN\fR statement.  Another
alternative is to use a different unit number.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___522 An internal library error occurred while reading a UNICOS 5.0 assign\n\
  environment file.
$nexp FE___522
An internal library error occurred while reading a UNICOS 5.0 assign \
environment file.
.PP
The run-time library has encountered an internal limit for line
length.  This occurs only when an \*Fassign\fR environment file generated
by a version of the \*Fassign\fR(1) command from \*u 5.0 or earlier release
is being processed by the \*Fassign\fR command from \*u 6.0 or later,
and the size of the \*Fassign\fR information record exceeds an internal
limit (5000 characters).
.PP
Remove the \*Fassign\fR environment file with the \*Frm\fR(1) command, and
reissue the \*Fassign\fR commands with the \*u 6.0 version of
\*Fassign\fR.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_DIRFMT An unrecognized directive was passed to the ASSIGN routine or was\n\
  encountered in the assign environment file.
$nexp ERAS_DIRFMT
An unrecognized directive was passed to the ASSIGN routine or was \
encountered in the assign environment file.
.PP
The first word in a directive in the \*Fassign\fR environment file is
not \*Fassign\fR.  The run-time library cannot process a line that
contains directives other than \*Fassign\fR directives.  Either 
the \*Fassign\fR
environment file is \*F$TMPDIR/.assign\fR
by default, or it is the file name
stored in the \*FFILENV\fR environment variable.
.PP
Ensure that strings passed to the \*FASSIGN\fR routine begin with the
word \*Fassign\fR.  Check to see that directives in the \*Fassign\fR
environment
file start with the word \*Fassign\fR or have the format
generated by the \*Fassign\fR(1) command.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in
your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_FNBL The file name passed to the assign command or ASNFILE has an embedded\n\
  blank.
$nexp ERAS_FNBL
The file name passed to the assign command or ASNFILE has an embedded blank.
.PP
A file name passed to the \*Fassign\fR(1) command or \*FASNFILE\fR(3) library
routine may not have embedded blanks.
.PP
Either remove extra blanks from the file name or rename the file so that
the name has no embedded blanks.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_ATTRP Value portion %s of the -p option cannot be processed.  A list\n\
  or range of file system stripe partition numbers should be specified\n\
  in the form '1:3', '0-4', or '0:3-4'.
$nexp ERAS_ATTRP
Value portion\ \fIpval\*C of the -p option cannot be processed.  A list \
or range of file system stripe partition numbers should be specified \
in the form \&'1:3', \&'0-4', or \&'0:3-4'.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with a \*F-p\fR option value that is not valid.  The \*F-p\fR
option value must be a colon-separated list of stripe partition
numbers and stripe partition ranges.  A stripe partition range is
denoted by the form \fIx-y\fR, in which
\fIx\fR and \fIy\fR are the first and last stripe
partitions in the range, respectively.
.PP
Change the \*F-p\fR option value to a recognized format.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_COREQN Assign option -c requires that option -n also be specified.
$nexp ERAS_COREQN
Assign option -c requires that option -n also be specified.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with the \*F-c\fR option but no \*F-n\fR option.  The
\*F-c\fR option specifies the contiguous allocation of preallocated file
space.  The \*F-n\fR option must be present to specify the amount of
file space to allocate.
.PP
Add the \*F-n\fR option to the \*Fassign\fR command options, and reissue the
command.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_FILENV Assign processing requires that environment variable FILENV be set when\n\
  environment variable TMPDIR is not set.
$nexp ERAS_FILENV
Assign processing requires that environment variable FILENV be set when \
environment variable TMPDIR is not set.
.PP
Either an \*Fassign\fR(1) command was issued or one of the \*FASSIGN\fR(3) library
routines (\*FASSIGN\fR, \*FASNUNIT\fR, \*FASNFILE\fR) or \*FASNRM\fR was called
to update the
\*Fassign\fR environment, but environment variables
\*FFILENV\fR and \*FTMPDIR\fR were
not set.  
.PP
Assign processing updates the \*Fassign\fR environment file or
the \*Fassign\fR environment variable.
If the \*FFILENV\fR environment variable is set to a name without a leading
"$" character, an \*Fassign\fR environment file is implied.
If the \*FFILENV\fR environment variable is set to a name with a leading
"$" character, an \*Fassign\fR environment variable is implied.
.PP
If \*FFILENV\fR is not set, \*F${TMPDIR}/.assign\fR
is used as the \*Fassign\fR environment file.
.PP
Set the \*FFILENV\fR environment variable to the name of the \*Fassign\fR
environment file, and either reissue the \*Fassign\fR(1) command or rerun the
program.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in \*(aL.
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_NOPOP Cannot restore assign environment because assign environment stack is empty.
$nexp ERAS_NOPOP
Cannot restore assign environment because assign environment stack is empty.
.PP
\*FASNCTL\fR(3) was called with the \*FRESTORE\fR option, but the local 
\*Fassign\fR
environment stack is empty.  A \*FRESTORE\fR of the \*Fassign\fR environment
must be preceded by an earlier call to \*FASNCTL\fR with the \*FLOCAL\fR or
\*FNEWLOCAL\fR option.
.PP
Defer the call to \*FASNCTL\fR with the \*F\&'RESTORE'\fR request until
new local \*Fassign\fR environments are established.
.PP
See \*Fassign\fR(1) in \*(aI and \*FASNCTL\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_MIXAD The -a and -D assign options may not both be assigned to a unit or file name.
$nexp ERAS_MIXAD
The -a and -D assign options may not both be assigned to a unit or file name.
.PP
An attempt was made to assign the \*F-a\fR and \*F-D assign\fR attributes to the
same unit number or file name with the \*Fassign\fR(1) command or an
\*FASSIGN\fR library routine.
.PP
Choose the \*F-a assign \fRoption if you would like to specify an actual
file name for a Fortran file connection, or choose the \*F-D\fR 
option if you would like to connect a unit to a standard file.
.PP
See \*Fassign\fR(1) in \*(aI and \*FASNCTL\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_ATTSPC The Fortran character variable passed to ASNQFILE or ASNQUNIT is too short.
$nexp ERAS_ATTSPC
The Fortran character variable passed to ASNQFILE or ASNQUNIT is too short.
.PP
The Fortran character variable passed to \*FASNQFILE\fR or \*FASNQUNIT\fR to
receive the assign attributes is not long enough for the attributes
currently assigned to the file name or unit number.
.PP
Pass a longer character variable or array element to \*FASNQFILE\fR or
\*FASNQUNIT\fR.
.PP
See \*Fassign\fR(1) in \*(aI and \*FASNQFILE\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADRECF Line %d in the assign environment file has an unrecognized format.
$nexp ERAS_BADRECF
Line\ \fInum\*C in the assign environment file has an unrecognized format.
.PP
Check that the \*FFILENV\fR environment variable contains the name of a
valid assign environment variable.  If \*FFILENV\fR is not set, check file
\*F${TMPDIR}/.assign\fR, which is the default assign environment file.
.PP
#if _UNICOS
If the assign environment file was generated by a version of the
\*Fassign\fR(1) command from \s-1UNICOS\s+1 release 5.0 or earlier, the
\*Fassign\fR commands must be reissued with a current version of \*Cassign\fR.
#endif
.PP
Before reissuing the \*Fassign\fR commands, remove the
unreadable assign environment file with the \*Fassign -R\fR command.
.PP
See the description of \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADRECE Assign record %d in the assign environment has an unrecognized format:\n\
   "%s"
$nexp ERAS_BADRECE
Assign record\ \fInum\*C in the assign environment has an unrecognized format: "\fItext\*C".
.PP
Check that the \*FFILENV\fR environment variable contains the name of a
valid assign environment variable.  The environment variable must contain
only text set up by a previous call to the \*Fassign\fR(1) command.
.PP
If the environment variable contains contents that are not valid, unset the environment
variable and reissue the \*Fassign\fR(1) command. 
.PP
See the description of \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_NULLVNAM FILENV contains a null environment variable name.
$nexp ERAS_NULLVNAM
FILENV contains a null environment variable name.
.PP
When you select to store assign attributes in the process environment
the \*FFILENV\fR environment variable must be set to the form \*C$\fIvarname\fR,
where \fIvarname\fR is the name of an environment variable to be used by
the \*Cassign\fR command for storing assign information.
.PP
Check the \*FFILENV\fR environment variable for a correct variable 
specification.
.PP
See the description of \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_PATCONF The file name %s matches more than one filename pattern in\n\
  the assign environment.
$nexp ERAS_PATCONF
The file name 'fname' matches more than one filename pattern in the assign environment.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been used to assign
attributes to two different file name patterns which match the file being
opened.   A file name pattern is assigned by specifying an assign object of 
the form \fIp\*F:_\fR.
.PP
The only case in which a file name can match two filename patterns is when
one of the filename patterns is \fIp\*F:%\fR.   Any other filename pattern takes
precedence over \fIp\*F:%\fR in filename pattern matches.
.PP
Either replace one of the filename patterns with assignment to a filename,
or make one of the patterns more specific so that the multiple file name match
does not occur.
.PP
See the description of \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_MTRUNC The "-T on" assign attribute may not be specified with "-m on".
$nexp ERAS_MTRUNC
The "-T on" assign attribute may not be specified with "-m on".
.PP
Specification of special handling for concurrent file access with the "-m on"
assign attribute implies "-T off".  A setting of "-T on" is in conflict with
the special concurrent file handling.
.PP
The user is required to ensure that each process, task or PE accessing the
file concurrently accesses a different region of the file without truncation.   
It is not possible to do this with sequential files if truncation after
write is in effect.
.PP
See the description of \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_FILENVPOS Assign processing requires that environment variable FILENV be set.
$nexp ERAS_FILENVPOS
Assign processing requires that environment variable FILENV be set.
.PP
Either an \*Fassign\fR(1) command was issued or one of the \*FASSIGN\fR(3) library
routines (\*FASSIGN\fR, \*FASNUNIT\fR, \*FASNFILE\fR) or \*FASNRM\fR was called
to update the
\*Fassign\fR environment, but environment variable \*FFILENV\fR was
not set.  
.PP
Assign processing updates the \*Fassign\fR environment file or
the \*Fassign\fR environment variable.  
If the \*FFILENV\fR environment variable is set to a name without a leading
"$" character, an \*Fassign\fR environment file is implied.  
If the \*FFILENV\fR environment variable is set to a name with a leading
"$" character, an \*Fassign\fR environment variable is implied.  
.PP
Set the \*FFILENV\fR environment variable to the name of the \*Fassign\fR
environment file or variable, and either reissue the \*Fassign\fR(1) command 
or rerun the program.
.PP
See \*Fassign\fR(1) in \*(aI
and \*FASSIGN\fR(3) in \*(aL.
.PP
.Ma NONUNICOS_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADCLASS The FFIO file structure %s is invalid.
$nexp ERAS_BADCLASS
The FFIO file structure 'filestruct' is invalid.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given an invalid \*F-F\fR option. 
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  Ensure that the option value is supported
in the current release.
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADOPT The FFIO option '%s' is invalid for file structure\n\
 '%s'.
$nexp ERAS_BADOPT
The FFIO option 'opt' is invalid for file structure 'filestruct'.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given an invalid -F option. 
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  Ensure that the option value is supported
in the current release.
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_CONFLICT The FFIO option '%s' conflicts with another option \n\
specified for this file structure.
$nexp ERAS_CONFLICT
The FFIO option 'opt' conflicts with another option specified for this file \
structure.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification with conflicting options. For example, in
the specification \*Fassign -F mr.ovfl.novfl\fR the option \*Fnovfl\fR conflicts
with the option \*Fovfl\fR.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADSYNT The FFIO option '%s' is not correctly specified.
$nexp ERAS_BADSYNT
The FFIO option 'opt' is not correctly specified.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that is incorrect. Check the syntax of
the option.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_OPTREQ The FFIO specification '%s' is incomplete.
$nexp ERAS_OPTREQ
The FFIO specification 'spec' is incomplete.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that is incomplete. An option is required
for the file structure chosen. For example, \*Fassign -F ibm\fR
requires an option specifying the record format.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_PSTRING The FFIO specification '%s' is too long.
$nexp ERAS_PSTRING
The FFIO specification 'spec' is too long.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that is too long. 
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_TOKUNI The unit specification in the FFIO option '%s' is\n\
invalid.
$nexp ERAS_TOKUNI
The unit specification in the FFIO option 'opt' is invalid.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that has an invalid unit specification.
Unit specifications may be given with some numeric values.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADMBS The maximum block size in the FFIO specification is\n\
invalid.
$nexp ERAS_BADMBS
The maximum block size in the FFIO specification is invalid.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that has an invalid maximum block size.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_INMAX The FFIO numeric parameters %d and %d are an invalid combination.
$nexp ERAS_INMAX
The FFIO numeric parameters 'num1' and 'num2' are an invalid combination.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that has invalid numeric parameters.
In this case, the numeric parameters conflict with each other. For example,
the \*Fassign -F mr:100:50\fR specification is invalid, because the
initial size (100) is greater than the maximum size (50).
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADREC The record size in the FFIO specification is invalid.
$nexp ERAS_BADREC
The record size in the FFIO specification is invalid.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that has invalid record size.
For example, the \*Fassign -F ibm.fb:256:500\fR specification is invalid,
because the maximum block size (500) is not an exact multiple of the
record size (256).
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_BADRFMT The record format in the FFIO specification is invalid.
$nexp ERAS_BADRFMT
The record format in the FFIO specification is invalid.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that has an invalid record format.
For example, the \*Fassign -F vms.s\fR specification is invalid,
because a subtype is required.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_NUMREQ A numeric value is required for '%s' when FFIO options\n\
'%s' and '%s' are specified.
$nexp ERAS_NUMREQ
A numeric value is required for 'numericfield' when FFIO options 'opt1' and \
'opt2' are specified.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that requires a numeric value.
For example, the \*Fassign -F ibm.f\fR specification is invalid,
because a numeric value is required to specify the logical record size.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef __mips
$msg ERAS_TOOBIG The numeric value %lld exceeds the maximum allowed (%lld)\n\
for %s.
#else
$msg ERAS_TOOBIG The numeric value %d exceeds the maximum allowed (%d)\n\
for %s.
#endif
$nexp ERAS_TOOBIG
The numeric value 'num' exceeds the maximum allowed ('max') for field 'field'.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that contains a numeric value which is too
large.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef __mips
$msg ERAS_TOOSMALL The numeric value %lld is less than the minimum allowed \n\
(%lld) for %s.
#else
$msg ERAS_TOOSMALL The numeric value %d is less than the minimum allowed \n\
(%d) for %s.
#endif
$nexp ERAS_TOOSMALL
The numeric value 'num' is less than the minimum allowed ('min') for \
field 'field'.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been 
given a \*F-F\fR specification that contains a numeric value which is too
small.
.PP
Valid option values for \*Fassign\fR \*F-F\fR are listed in the
\*Fintro_ffio\fR(1) man page.  
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_TOOLAY The FFIO specification contains too many layers.
$nexp ERAS_TOOLAY
The FFIO specification contains too many layers.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been
given a \*F-F\fR specification that contains too many layers.
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_OPTVAL1 The value on assign option %s is invalid.
$nexp ERAS_OPTVAL1
The value on assign option \fI opt\*C is invalid.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR library routine
(\*FASSIGN\fR(3), \*FASNUNIT\fR, or \*FASNFILE\fR) was called
with an option value that was not valid for the specified option \fIopt\fR.
.PP
Valid option values for \*Fassign\fR options are listed in the
\*Fassign\fR(1) man page.
Ensure that the option value is supported
in the current release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_OPTVAL2 Value %s on assign option %s is invalid.
$nexp ERAS_OPTVAL2
Value\ \fIoptval\*C on assign option\ \fIopt\*C is invalid.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR, or \*FASNFILE\fR) was called with an option value
\fIoptval\fR that was not valid for option \fIopt\fR.
.PP
Valid option values for \*Fassign\fR options are listed in the
\*Fassign\fR(1) man page.  Ensure that the option value is supported
in the current release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_UNOPT Assign option %s is unrecognized.
$nexp ERAS_UNOPT
Assign option\ \fIopt\*C is unrecognized.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR, or \*FASNFILE\fR) was called with an option \fIopt\fR that was
not valid.
.PP
Valid options for \*Fassign\fR options are listed in
the \*Fassign\fR(1)
man page.  Ensure that option \fIopt\fR is supported in the current
release.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ERAS_LRAW The "-l" assign option may not be specified with "-o_raw" or "-o_ldraw".
$nexp ERAS_LRAW
The "-l" assign option may not be specified with "-o_raw" or "-o_ldraw".
.PP
The "-o_raw" and "-o_ldraw" options provide a way to specify the settings of 
the  \s-1O_RAW\s+1 and \s-1O_LDRAW\s+1 flags on the  \*Fopen\fR(2) system call.
The "-l" option provides an alternative method to specify the same settings.
.PP
See the description of
\*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Assign warnings
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg WNAS_ANSUPY Assign option %s is not supported.  This option will be ignored.
$nexp WNAS_ANSUPY
Assign option\ \fIopt\*C is not supported.  This option will be ignored.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR, or \*FASNFILE\fR)
was called with option \fIopt\fR, which is not supported.
The option will be ignored.
.PP
Remove the inactive option from the \*Fassign\fR command line, reissue
the \*Fassign\fR command or library routine, and re-execute the program.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___583 Option '-s sbin' may not be supported in future UNICOS releases.\n\
  Use '-s unblocked' or '-F system' for unformatted files and '-s text' for\n\
  formatted files.
$nexp FE___583
Option \&'-s sbin' may not be supported in future UNICOS releases. \
Use \&'-s unblocked' or \&'-F null' for unformatted files and \&'-s text' \
for formatted files.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with option \*F\-s sbin\fR.  This specification
may not be supported in future \*u releases.
.PP
Replace the \*F\-s sbin\fR option with \*F\-s unblocked\fR,
\*F\-F null\fR, or \*F\-s text\fR.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___584 Assign option -p requires that -n also be specified;  without the -n\n\
 option, -p will be ignored.
$nexp FE___584
Assign option -p requires that -n also be specified; without the -n 
option, -p will be ignored.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR(3),
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with option \*F-p\fR but no \*F-n\fR option.  
.PP
Either remove the \*F-p\fR option or add the \*F-n\fR option and reissue the 
\*Fassign\fR command or \*FASSIGN\fR library routine.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 1.2
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___585 Assign option -r will not be supported in future releases.  Use\n\
  option -p instead.
$nexp FE___585
Assign option -r will not be supported in future releases.  Use \
option -p instead.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with option \*F-r\fR.  The \*F-r\fR option may not be
supported in future \s-1UNICOS\s+1 releases.
.PP
Replace the \*F-r\fR option with an equivalent \*F-p\fR option.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg WNAS_DEPASDS The assign options sequence '-a SDS -s u -n x' may not be supported in future\n\
  UNICOS releases.  Use '-F sds:x -T off' to get an SDS file with no truncation.
$nexp WNAS_DEPASDS
The assign options sequence \&'-a SDS -s u -n x' \
may not be supported in future \
UNICOS releases.  Use \&'-F sds:x -T off' to get an \
SDS file with no truncation.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine (\*FASSIGN\fR,
\*FASNUNIT\fR,
or \*FASNFILE\fR)
was called with option \*F\-a SDS\fR.  The meaning of
this specification may not be supported in future \*u releases.
In \*u releases 6.0 and earlier,\*F \-a SDS -s u -n x\ \fRallowed the 
assignment of a file on \s-1CRAY PVP\s+1 systems to a secondary data
segment (\s-1SDS\s+1), with truncation after sequential write disabled.
.PP
Replace the\*F \-a SDS -s u -n x \fRoptions with\*F \-F sds:\fIx\*F -T off\fR, 
in which \fIx\fR is the number of 512-word \s-1SDS\s+1 blocks to be allocated
for the file.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg WNAS_UBMX Assign option '-u' is valid only with file types 'unblocked', 'tape',\n\
  'bmx', 'tape50' or 'bmx50'.  If a different file type is\n\
  specified, the -u option will be ignored.
$nexp WNAS_UBMX
Assign option -u is valid only with file
types \&'unblocked', \&'tape', \&'bmx', \*Z
\&'tape50', or \&'bmx50'.
If a different file type is specified, the -u option will be ignored.
.PP
An \*Fassign\fR(1) command or \*FASSIGN\fR(3) library routine 
was called with the 
\*F-u\fR option.  The 
\*F-u\fR option is ignored unless the file type specification indicates an
unblocked file or an online tape (\*F-s tape\fR, or \*F-s bmx\fR, or \*F-s bmx50\fR, 
or \*F-s tape50\fR).
.PP
Either remove the \*F-u\fR option or specify with the \*F-s\fR option a file type
that 
supports the \*F-u\fR option.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FE___588 Assign option -V is ignored when specified with other options.  This\n\
  will be an error in future UNICOS releases.
$nexp FE___588
Assign option -V is ignored when specified with \
other options.  This will be an error in future UNICOS releases.
.PP
An \*Fassign\fR(1) command 
was called with a \*F-V\fR option and other options as well.
The \*F-V\fR option allows you to view attributes for a file or unit,
whereas other options allow you to add attributes to the
\*Fassign\fR environment.
.PP
Issue two \*Fassign\fR(1) commands instead of one.  Issue the first
\*Fassign\fR command without \*F-V\fR, and issue the second command with 
\*F-V\fR as the only option.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma RETIRED 3.0
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg WNAS_QNSTRD The -q assign option value (%d) overrides the stride from the\n\
-n assign attribute (%d).  The -q attribute value will be used.
$nexp WNAS_QNSTRD
The -q assign option value (\fIQ\*C) overrides the stride \
from the -n assign option (\fIS\*C).  The -q option value will be used.
.PP
The \*F-q\fI assign\fR option is the preferred means of specifying the number of
file blocks to be allocated in every stripe partition of a file.   The \*F-n\fI
assign\fR option value has the form \fIsz:stride\fR.   The \fIstride\fR part of 
the \*F-n\fR option value is the old way of specifying the same thing.
.PP
Remove the \fIstride\fR part of the \*F-n\fR attribute.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg WNAS_NUMERIC The FFIO specification contains too many numeric parameters.\n\
'%d' parameters will be used; excess parameters will be ignored.
$nexp WNAS_NUMERIC
The FFIO specification contains too many numeric parameters. \
'num' parameters will be used; excess parameters will be ignored.
.PP
The \*Fassign\fR command or an \*FASSIGN\fR(3) library routine has been
given a \*F-F\fR specification that contains too many numeric parameters.
Excess parameters will be ignored.
.PP
See the description of \*Fassign\fR(1) and \*Fintro_ffio\fR(1) in \*(aI.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Format parsing errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTELP Missing opening (left) parenthesis in format.
$nexp FEFMTELP
Missing opening (left) parenthesis in format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement did not contain an opening (left) parenthesis
'\*F(\fR'.
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements, however,  
are parsed by the compiler at compile time and are 
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTERP Missing closing (right) parenthesis in format.
$nexp FEFMTERP
Missing closing (right) parenthesis in format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement did not contain a closing (right) parenthesis.
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements, 
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTEIN Missing numeric field in format.
$nexp FEFMTEIN
Missing numeric field in format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement did not contain a numeric field
where one was expected.
For example, after a \*FB\fR, \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
\*FG\fR, \*FI\fR, \*FL\fR, \*FO\fR, \*FR\fR, or \*FZ\fR data edit descriptor;
after a period in a data edit descriptor (I10.);
after an exponent field indicator in a data edit descriptor (E12.2E);
after a plus or minus sign (+);
or before a P edit descriptor (P).
.PP
The \*FB\fR, \*FEN\fR, \*FES\fR, \*FO\fR, \*FR\fR and \*FZ\fR data
edit descriptors are extensions to the \s-1ANSI
X\s+13.9\-1978 standard.  The \*FR\fR data edit descriptor is an
extension to the current Fortran standard.
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTEPE Period expected in format data edit-descriptor.
$nexp FEFMTEPE
Period expected in format data edit-descriptor.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement did not contain a period after the width
field, where one was expected.
For example, after a \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
or \*FG\fR data edit descriptor (F10).
.PP
The \*FEN\fR and \*FES\fR data edit descriptors are 
extensions to the \s-1ANSI X\s+13.9\-1978 standard.
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTEPX A signed value is incorrectly placed in the format.
$nexp FEFMTEPX
A signed value is incorrectly placed in the format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement did not contain a \*FP\fR or \*FX\fR edit descriptor
after a signed value (+2P, \-3X).
Only the \*FP\fR or \*FX\fR edit descriptors are permitted to have
signed values.
.PP
A negative value on the \*FX\fR edit descriptor is allowed 
as an extension to the \s-1ANSI X\s+13.9\-1978 standard and the
current Fortran standard.
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTIRP A repetition count is incorrectly placed in the format.
$nexp FEFMTIRP
A repetition count is incorrectly placed in the format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained a repetition count in a location
that is not valid.
Repetition counts are permitted only in front of data edit descriptors
(\*FB\fR, \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
\*FG\fR, \*FI\fR, \*FL\fR, \*FO\fR, \*FR\fR, or \*FZ\fR),
a slash '/' or Hollerith 'H' edit descriptor,
or in front of repeat groups (parentheses).
.PP
The \*FB\fR, \*FEN\fR, \*FES\fR, \*FO\fR, \*FR\fR, and \*FZ\fR data
edit descriptors are extensions to the \s-1ANSI
X\s+13.9\-1978 standard.  The \*FR\fR data edit descriptor is an
extension to the current Fortran standard.
.PP
Check the variable format and ensure that it is a valid format.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTZRP A zero repetition count is invalid in a format.
$nexp FEFMTZRP
A zero repetition count is invalid in a format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained a zero repetition count.
A repetition count, if specified, must be greater than zero.
.PP
Check the variable format and ensure that it is a valid format.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTZFW A zero length field width is invalid in a format.
$nexp FEFMTZFW
A zero length field width is invalid in a format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained a data edit descriptor with a
zero length field width (I0) or a zero length exponent field width
(E12.3E0).
.PP
Check the variable format and ensure that it is a valid format.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTFTL A field is too large in a format.
$nexp FEFMTFTL
A field is too large in a format.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained a
repetition count which was too large (greater than 1,073,741,824),
or a data edit descriptor with
a field width or decimal field which was too large (greater than 16,777,215),
or an exponent field that was too large (greater than 63).
.PP
Check the variable format and ensure that it is a valid format.
.PP
Also see the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats,
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTZMH A Hollerith edit-descriptor has a zero or missing count.
$nexp FEFMTZMH
A Hollerith edit-descriptor has a zero or missing count.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained a Hollerith 'H' edit descriptor
that had a zero count (0H) or was missing a count (X,H,X).
.PP
Check the variable format and ensure that it is a valid format.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTIED A format contained an unknown edit-descriptor.
$nexp FEFMTIED
A format contained an unknown edit-descriptor.
.PP
A format (probably a variable format of type character)
on a \*FREAD\fR
or \*FWRITE\fR statement contained an unknown edit descriptor.
Valid edit descriptors are
\*FB\fR, \*FBN\fR, \*FBZ\fR, \*FD\fR, \*FE\fR, \*FEN\fR, \*FES\fR, \*FF\fR,
\*FG\fR, \*FI\fR, \*FL\fR, \*FO\fR, \*FP\fR, \*FR\fR, \*FS\fR, \*FSP\fR,
\*FSS\fR, \*FT\fR, \*FTL\fR, \*FTR\fR, \*FZ\fR, \*F:\fR, \*F/\fR,
or \*F$\fR.
.PP
The \*FB\fR, \*FEN\fR, \*FES\fR, \*FO\fR, \*FR\fR, \*FZ\fR, and \*F$\fR
edit descriptors are extensions to the \s-1ANSI
X\s+13.9\-1978 standard.  The \*FR\fR data edit descriptor is an
extension to the current Fortran standard.
.PP
Check the variable format and ensure that it is a valid format.
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTNLS The format contains an unterminated literal string.
$nexp FEFMTNLS
The format contains an unterminated literal string.
.PP
A format (probably a variable format of type character) on a \*FREAD\fR
or \*FWRITE\fR statement contained an unterminated literal string.
.PP
Example:
.CS
      F = \&'(X,"Total = ,F10.3)'
C                     ^ missing double quote
      PRINT F, TOTAL
.CE
.PP
Check the variable format and ensure that all literal strings are
properly terminated.  For example, the preceding code should be as follows:
.CS
      F = \&'(X,"Total = ",F10.3)'
      PRINT F, TOTAL
.CE
.PP
See the description of
input/output statements in your
Fortran reference manual.
.PP
This message is usually issued for variable formats;
that is, formats that exist as character constants or variables.
.PP
Example:
.CS
READ(1,FMT=CHARVAR) I
.CE
.PP
\*FFORMAT\fR statements,
however, are parsed by the compiler at compile time and are
less likely to cause a run-time error.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEFMTMEM The program was unable to obtain more memory while processing a format.
$nexp FEFMTMEM
The program was unable to obtain more memory while processing a format.
.PP
A format processing routine in the Fortran run-time library was unable
to request additional memory space.  This usually occurs when the process
has exceeded its memory allocation or an extremely large format is being
processed.
.PP
Review the use of memory space by your program, and if possible,
reduce its memory use.
.PP
#if _UNICOS
Also see the description of the
\*Flimit\fR(1) command in 
the \fI\s-1UNICOS\s+1 User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
#endif
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	PACK/UNPACK errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEPCKARG The PACK or UNPACK routine was called with too few arguments.
$nexp FEPCKARG
The PACK or UNPACK routine was called with too few arguments.
.PP
The PACK or UNPACK library routine was called with less than four arguments.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEPCKNEG The PACK or UNPACK routine was called with a negative COUNT.
$nexp FEPCKNEG
The PACK or UNPACK routine was called with a negative COUNT.
.PP
The PACK or UNPACK library routine was called with a negative COUNT (the fourth
argument) of items to pack or unpack.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FEPCKPW2 The PACK or UNPACK routine was called with an invalid NBITS.
$nexp FEPCKPW2
The PACK or UNPACK routine was called with an invalid NBITS.
.PP
The PACK or UNPACK library routine was called with an invalid NBITS (the second
argument) indicating the size of the items of pack or unpack.
The NBITS argument must be a positive power of two less than 64 (that is,
1, 2, 4, 8, 16 or 32).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	BMM (Bit Matrix Multiply) errors
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg BMMVLCHK A BMM simulation routine was called with a \n\
  VL (vector length) that is not valid. The maximum supported VL is 64.
$nexp BMMVLCHK
A BMM simulation routine was called with a VL (vector length) that is not valid.
.PP
The maximum supported vector length (VL) is 64.
.PP
.Ma UNRECOVERABLE
.ME
$ ---------|--------|--------|---------|---------|---------|---------|---------|
$
$	Errors from Posix 1003.9-1992 
$
$ ---------|--------|--------|---------|---------|---------|---------|---------|
$msg ENONAME An invalid constant, structure, or component name was \n\
  passed to a subroutine or function.
$nexp ENONAME
An invalid constant, structure, or component name was passed to a subroutine \
or function. 
.PP
See the manual page of the POSIX subroutine or function called for a listing
of the valid names.
.PP
See the IEEE POSIX FORTRAN 77 Language Interfaces manual, IEEE Std 1003.9-1992.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ENOHANDLE A handle could not be created.
$nexp ENOHANDLE
A handle could not be created. 
.PP
This may occur because the library was unable to request sufficient memory
space for the new handle.
.PP
See the IEEE POSIX FORTRAN 77 Language Interfaces manual, IEEE Std 1003.9-1992.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg ETRUNC An output character parameter was truncated.
$nexp ETRUNC
An output character parameter was truncated.
.PP
The declared length of the output character argument was insufficient to contain
the string to be returned.  The output length argument associated with the
output character argument will contain the original length of the data before
truncation.
.PP
See the IEEE POSIX FORTRAN 77 Language Interfaces manual, IEEE Std 1003.9-1992.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg EARRAYLEN Too many array elements to be returned.
$nexp EARRAYLEN
Too many array elements to be returned.
.PP
For \*Cget\fR routines, the number of array elements to be returned exceeds
\*FIALEN\fR, and only the first \*FIALEN\fR elements of the array argument have
been set.  For \*Cset\fR routines, \*FIALEN\fR exceeds the number of array
elements in the target array.  Only the available elements of the array in the
target array have been set.
.PP
See the IEEE POSIX FORTRAN 77 Language Interfaces manual, IEEE Std 1003.9-1992.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg EEND The end of file, record, or directory stream has been encountered.
$nexp EEND
The end of file, record, or directory stream has been encountered.
.PP
No more data is available from the file, record or directory stream.
.PP
See the IEEE POSIX FORTRAN 77 Language Interfaces manual, IEEE Std 1003.9-1992.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Fortran library warning messages
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWDEFBSZ The DEFBUFSIZ environment variable no longer specifies a default buffer size.
$nexp FWDEFBSZ
The DEFBUFSIZ environment variable no longer specifies a default buffer size.
.PP
In \s-1UNICOS\s+1 6.0, the \*FDEFBUFSIZ\fR environment variable specified a
default buffer size for Fortran files.   In \s-1UNICOS\s+1 7.0 and later
releases, better default buffer sizes are automatically chosen by the Fortran
\s-1I/O\s+1 library.  The use of \*FDEFBUFSIZ\fR has been discontinued.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	Fortran compiler-initiated run-time messages
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNKTP For the entry '%s', called from '%s' at line %d:\n\
  Kind type conflict for Call\n\
  '%s' was called as kind type %d\n\
  '%s' is defined as kind type %d
$nexp FWFUNKTP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Kind type conflict for Call \*Z \
  \&'\fIcallee\*F' was called as kind type \&'\fIk1\*F' \*Z \
  \&'\fIcallee\*F' is defined as kind type \&'\fIk2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the kind type of a function in a routine
that calls it does not match the kind type of the function
definition.
.PP
Modify the program so as to declare the function with the same kind
type specified in its definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNRNK For the entry '%s', called from '%s' at line %d:\n\
  Rank conflict for Call\n\
  '%s' was called as rank %d\n\
  '%s' is defined as rank %d
$nexp FWFUNRNK
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Rank conflict for Call \*Z \
  \&'\fIcallee\*F' was called as rank \&'\fIr1\*F' \*Z \
  \&'\fIcallee\*F' is defined as rank \&'\fIr2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the rank of a function in a routine that
calls it does not match the rank of the function definition.
.PP
Modify the program so as to declare the function as the same rank
specified in its definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNSIZ For the entry '%s', called from '%s' at line %d:\n\
  Array size conflict for Call\n\
  '%s' was called with total array size %d\n\
  '%s' is defined with total array size %d
$nexp FWFUNSIZ
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Array size conflict for Call \*Z \
  \&'\fIcallee\*F' was called with total array size \&'\fIs1\*F' \*Z \
  \&'\fIcallee\*F' is defined with total array size \&'\fIs2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the total array size of a function in a
routine that calls it does not match the total array size of
the function definition.
.PP
Modify the program so as to declare the function with the same
total array size specified in its definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNCHL For the entry '%s', called from '%s' at line %d:\n\
  Character length conflict for Call\n\
  '%s' was called with character length %d\n\
  '%s' is defined with character length %d
$nexp FWFUNCHL
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Character length conflict for Call \*Z \
  \&'\fIcallee\*F' was called with character length \&'\fIl1\*F' \*Z \
  \&'\fIcallee\*F' is defined with character length \&'\fIl2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the total character length of a function
in a routine that calls it does not match the character length
of the function definition.
.PP
Modify the program so as to declare the function with the same
character length specified in its definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNDVT For the entry '%s', called from '%s' at line %d:\n\
  Derived types do not match for Call.
$nexp FWFUNDVT
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Derived types do not match for Call
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the derived type of a function in a
routine that calls it does not match the derived type of the
definition.
.PP
Modify the program so as to declare the function with the same
derived type specified in its definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNNPT For the entry '%s', called from '%s' at line %d:\n\
  Interface conflict for Call.  '%s' was called as a\n\
  POINTER function but does not have the POINTER attribute.
$nexp FWFUNNPT
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Interface conflict for Call. \&'\fIcallee\*F' was called as a \*Z \
  POINTER function but does not have the POINTER attribute.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the function is referenced as a POINTER
function in the routine that calls it but the function
definition does not contain the POINTER attribute.
.PP
Modify the program to reference the function as specified in the
function definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNPTR For the entry '%s', called from '%s' at line %d:\n\
  Interface conflict for Call.  '%s' has the\n\
  POINTER attribute but was not called as a POINTER function.
$nexp FWFUNPTR
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Interface conflict for Call. \&'\fIcallee\*F' has the \*Z \
  POINTER attribute but was not called as a POINTER function.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the function is not referenced as a
POINTER function in a routine that calls it but the function
definition contains the POINTER attribute.
.PP
Modify the program to reference the function as specified in the
function definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGOPT For the entry '%s', called from '%s' at line %d:\n\
  Interface conflict for Call\n\
  Argument number %d is not PRESENT but is not optional.
$nexp FWARGOPT
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Interface conflict for Call \*Z \
  Argument number \&'\fIno\*F' is not PRESENT but is not optional.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the function is not referenced with the
same number of nonoptional arguments as specified in the function
definition.
.PP
Modify the program to reference the function with the same number
of required arguments as specified in the function definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGKTP For the entry '%s', called from '%s' at line %d:\n\
  Kind type conflict for Argument number %d\n\
  Actual argument kind type is kind type %d\n\
  Dummy argument kind type  is kind type %d
$nexp FWARGKTP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Kind type conflict for Argument number \&'\fInum\*F' \*Z \
  Actual argument kind type is kind type \&'\fIk1\*F' \*Z \
  Dummy argument kind type  is kind type \&'\fIk2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the kind type of an actual argument 
passed to a subroutine or function does not match the kind
type of the corresponding dummy argument with which the
routine is declared.
.PP
Modify the program to pass an argument of the same kind type
as that of the corresponding dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGDVT For the entry '%s', called from '%s' at line %d:\n\
  Type conflict for Argument number %d.  Derived types do not match.
$nexp FWARGDVT
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Type conflict for Argument number \&'\fInum\*F'. Derived types do not match.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the derived type of an actual argument 
passed to a subroutine or function does not match the derived
type of the corresponding dummy argument with which the
routine is declared.
.PP
Modify the program to pass an argument of the same derived type
as that of the corresponding dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGOUT For the entry '%s', called from '%s' at line %d:\n\
  Dummy argument number %d has INTENT(OUT) or INTENT(INOUT)\n\
  but the actual argument is not definable. 
$nexp FWARGOUT
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Dummy argument number \&'\fIdnum\*F' has INTENT(OUT) or INTENT(INOUT) \*Z \
  but the actual argument is not definable.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the INTENT attribute of a dummy argument 
in a subroutine or function specifies either INTENT(OUT) or
INTENT(INOUT) but the actual argument is not definable in the
called procedure.  Definable means to be assigned a value.  
.PP
Modify the program to pass an actual argument that can be assigned
assigned a value.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGPTR For the entry '%s', called from '%s' at line %d:\n\
  Interface conflict for Call\n\
  Dummy argument number %d has the POINTER attribute\n\
  but the actual argument is not a POINTER. 
$nexp FWARGPTR
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Interface conflict for Call \*Z \
  Dummy argument number \&'\fIdnum\*F' has the POINTER attribute \*Z \
  but the actual argument is not a POINTER. 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the dummy argument passed to a 
subroutine or function specifies the POINTER attribute but
the actual argument is not a POINTER.
.PP
Modify the program to pass an actual argument that has the
attributes as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGRNK For the entry '%s', called from '%s' at line %d:\n\
  Rank conflict for POINTER/ASSUMED SHAPE argument number %d\n\
  Actual argument is rank %d.  Dummy argument is rank %d
$nexp FWARGRNK
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Rank conflict for POINTER/ASSUMED SHAPE argument number \&'\fInum\*F' \*Z \
  Actual argument is rank \&'\fIr1\*F'.  Dummy argument is rank \&'\fIr2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the rank of a actual argument passed
to a subroutine or function does not match the rank of the
dummy argument.  The rank is the number of dimensions of
the array.
.PP
Modify the program to pass an actual argument with the same
rank as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGPCL For the entry '%s', called from '%s' at line %d:\n\
  Character length conflict for POINTER/ASSUMED SHAPE argument number %d\n\
  Character length of actual argument is %d\n\
  Character length of dummy argument is %d
$nexp FWARGPCL
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Character length conflict for POINTER/ASSUMED SHAPE argument number \&'\fInum\*F' \*Z \
  Character length of actual argument is \&'\fIr1\*F' \*Z \
  Character length of dummy argument is \&'\fIr2\*F' 
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the character length of an actual argument 
passed to a subroutine or function does not match the character
length of the dummy argument.
.PP
Modify the program to pass an actual argument with the same
character length as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGASS For the entry '%s', called from '%s' at line %d:\n\
  Interface conflict for Call\n\
  Dummy argument number %d is an ASSUMED SHAPE array\n\
  Actual argument number %d is a sequence array or scalar.
$nexp FWARGASS
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Interface conflict for Call \*Z \
  Dummy argument number \&'\fIdnum\*F' is an ASSUMED SHAPE array \*Z \
  Actual argument number \&'\fIanum\*F' is a sequence array or scalar.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the dummy argument passed to a subroutine
or function specifies an ASSUMED SHAPE array but the actual
argument is a sequence array or scalar.
.PP
Modify the program to pass an actual argument with the same
array specifications as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSCA For the entry '%s', called from '%s' at line %d:\n\
  Actual argument number %d is an array\n\
  Dummy argument number %d is a scalar.
$nexp FWARGSCA
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Actual argument number \&'\fIanum\*F' is an array \*Z \
  Dummy argument number \&'\fIdnum\*F' is a scalar.
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the actual argument passed to a subroutine
or function is an array but the dummy argument is a scalar.
.PP
Modify the program to pass an actual argument with the same
specifications as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGCHL For the entry '%s', called from '%s' at line %d:\n\
  Character length of dummy argument number %d is greater\n\
  than the character length of the actual argument\n\
  Character length of actual argument is %d\n\
  Character length of dummy argument is %d
$nexp FWARGCHL
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Character length of dummy argument number \&'\fIdnum\*F' is greater \*Z \
  than the character length of the actual argument \*Z \
  Character length of actual argument is \&'\fIalen\*F' \*Z \
  Character length of dummy argument is \&'\fIdlen\*F'
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the character length of the actual
argument passed to a subroutine or function is less than the
character length of the dummy argument.
.PP
Modify the program to pass an actual argument with the same
character length as the dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGARS For the entry '%s', called from '%s' at line %d:\n\
  Actual argument number %d is a scalar and not an array element\n\
  Dummy argument number %d is an array
$nexp FWARGARS
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Actual argument number \&'\fIdnum\*F' is a scalar and not an array element \*Z \
  Dummy argument number \&'\fIdnum\*F' is an array
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the actual argument passed to a subroutine
or function is declared as a scalar and not an array element
but the dummy argument is an array.
.PP
Modify the program to pass an actual argument with the same array
specifications as the dummy array.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSIZ For the entry '%s', called from '%s' at line %d:\n\
  Total array size of dummy argument number %d is greater\n\
  than the total array size of the actual argument\n\
  Total array size of actual argument is %d\n\
  Total array size of dummy argument is %d
$nexp FWARGSIZ
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Total array size of dummy argument number \&'\fIdnum\*F' is greater \*Z \
  than the total array size of the actual argument \*Z \
  Total array size of actual argument is \&'\fIalen\*F' \*Z \
  Total array size of dummy argument is \&'\fIdlen\*F'
.PP
When runtime checking of procedure interface is enabled, this
message is issued when the INTENT attribute of a dummy argument 
passed to a subroutine or function specifies either INTENT(OUT)
or INTENT(INOUT) but the actual argument is not definable in the
called procedure.  Definable means to be assigned a value.  
.PP
Modify the program to pass an actual argument that can be assigned
assigned a value.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWBNDCHK At line %d in Fortran routine '%s', subscript value %d\n\
  is out of bounds for array '%.*s'.
$nexp FWBNDCHK
At line \fIn\*C in Fortran routine \&'\fIname\*C', \
subscript value \fIi\*C is out of bounds for array \&'\fIa\*C'.
.PP
When bounds checking is enabled, this message is issued if an
array reference exceeds the bounds of the array.  Line number
\fIn\fR in Fortran routine \fIname\fR
is where the subscript value \fIi\fR
exceeded the bounds of the array named \fIa\fR.
.PP
Either modify the program to not exceed the bounds of the array or
increase the size of the array.
.PP
See the description of compiler options in your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWCONFCK At line %d in Fortran routine '%s', in dimension %d,\n\
  extents %d and %d are not equal.
$nexp FWCONFCK
At line \fIn\*C in Fortran routine \&'\fIname\*C', \
in dimension \fId\*C, extents \fIe1\*C and \fIe2\*C are not equal.
.PP
When bounds checking is enabled, this message is issued if an
array assignment exceeds the bounds of the result array.  Line
number \fIn\fR in Fortran routine \fIname\fR is where the two array
extents (\fIe1\fR and \fIe2\fR) did not match.
.PP
Either modify the program to not exceed the bounds of the array, or
ensure that the array extents are equal.
.PP
See the description of array options in your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWNUMARG For the entry '%s', called from '%s' at line %d:\n\
  Number of arguments for call, %d, does not equal expected number of\n\
  arguments %d.
$nexp FWNUMARG
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
Number of arguments for call, \&'\fIn1\*F', does \
not equal expected number of arguments \&'\fIn2\*F'.
.PP
When subroutine call argument checking is enabled, this message is
issued when the number of actual arguments passed to a subroutine or
function does not match the number of dummy arguments with which the
routine is declared.
.PP
Modify the program to pass the same number of arguments 
declared in the function or subroutine definition.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGTYP For the entry '%s', called from '%s' at line %d:\n\
  Type conflict for argument #%d\n\
  actual argument type =     %s\n\
  dummy argument type  =     %s
$nexp FWARGTYP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Type conflict for argument \&'#\fIargnum\*F' \*Z \
  actual argument type =     \&'\fIta\*F' \*Z \
  dummy argument type  =     \&'\fItd\*F'
.PP
When subroutine call argument checking is enabled, this message is
issued when the type of an actual argument passed to a subroutine or
function does not match the type of the corresponding dummy argument
with which the routine is declared.
.PP
Modify the program so as to pass an argument of the same type as
that of the corresponding dummy argument.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWFUNTYP For the entry '%s', called from '%s' at line %d:\n\
  Type conflict for Call\n\
  '%s' was called as %s\n\
  '%s' is defined as %s
$nexp FWFUNTYP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
  Type conflict for Call \*Z \
  \&'\fIcallee\*F' was called as \&'\fIt1\*F' \*Z \
  \&'\fIcallee\*F' is defined as \&'\fIt2\*F' 
.PP
When subroutine call checking is enabled, this message is issued
when the type of a function in a routine that calls it does not
match the type of the function definition.
.PP
Modify the program so as to declare the function as the same type
for which it is defined.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSHP For the entry '%s', called from '%s' at line %d:\n\
  Shape mismatch for argument "%s".\n\
    actual argument is %s\n\
    dummy  argument is %s
$nexp FWARGSHP
For the entry \&'\fIcallee\*F', called from \&'\fIcaller\*F' at line \fIx\*F: \*Z \
Shape mismatch for argument \&"\fIarg\*C".
.ES
    actual argument is \&"\fIashape\*C"
    dummy  argument is \&"\fIdshape\*C"
.EE
.PP
The extent of the last declared dimension of a shared dummy argument
exceeds the extent of the corresponding dimension of its actual argument.
.PP
If argument shape checking is enabled, this message is issued the first
time the extent of the last declared dimension of a shared dummy argument
is greater than the extent of the corresponding actual dimension.
Since sequence association is not supported for distributed data,
programs which index beyond the declared extent of an array may have
unpredictable behavior.
.PP
Modify the program so that dummy argument extents are consistent
with all corresponding actual arguments.
.PP
See your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGDIM Conformance error for dimension %d at line %d in file '%s'.
$nexp FWARGDIM
Conformance error for dimension \ftd\*F at line \fIx\*F in file \&'\fIrtn\*C'.
.PP
When conformance checking is enabled, this message is issued if
an array does not conform because of its shape.  Two arrays are
conformable if they have the same shape.  The shape of an array
is determined by its rank and its extent in each dimension.
Dimension \fId\fR at line number \fIx\fR in file \fIrtn\fR does
not have the same extent.  There may be more than one occurrance
of dimension \fId\fR at line number \fIn\fR.
.PP
Either modify the program so that the subscript is within the bounds
for its dimension of the array or ensure the array extents are equal.
.PP
See the description of array conformance checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSBV Subscript %d is out of range for dimension %d for array\n\
  '%s' at line %d in file '%s' with bounds %d:%d.
$nexp FWARGSBV
Subscript \fIs\*F is out of range for dimension \fId\*F for array \*Z \
\&'\fIary\*C' at line \fIx\*F in file \&'\fIrtn\*C' with \*Z \
bounds \fIlob\*F : \fIupb\*F.
.PP
When bounds checking is enabled, this message is issued if an array
reference exceeds the bounds of the array.  At line number \fIx\fR
in file \fIrtn\fR, the subscript value \fIs\fR exceeded the lower and
upper bounds \fIlob\*F and \fIupb\*F of the array named \fIary\fR for
dimension \fId\fR.
.PP
Either modify the program so that each subscript is within the bounds
for its dimension of the array or ensure the array extents are equal.
.PP
See the description of array bounds checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSBR Subscript %d:%d:%d is out of range for dimension %d for array\n\
  '%s' at line %d in file '%s' with bounds %d:%d.
$nexp FWARGSBR
Subscript \fIi1\*F:\fIi2\*F:\fIi3\*F is out of range for dimension \fId\*F for \*Z \
array \&'\fIary\*C' at line \fIx\*F in file \&'\fIrtn\*C' with \*Z \
bounds \fIlob\*F : \fIupb\*F .
.PP
When bounds checking is enabled, this message is issued if an
array reference exceeds the bounds of the array.  At line number
\fIx\fR in file \fIrtn\fR, the beginning subscript \fIi1\fR and the
ending subscript \fIi2\fR of a subscript triplet exceeded the lower
bound \fIlob\fR or upper bound \fIupb\fR of array \fIary\fR for
dimension \fId\fR.
.PP
Either modify the program so that each subscript is within the bounds
for its dimension of the array or ensure the array extents are equal.
.PP
See the description of array bounds checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSTR Substring %d:%d is out of range for character variable '%s'\n\
  at line %d in file '%s'.  Parent string size is %d.
$nexp FWARGSTR
Substring \fIi1\*F:\fIi2\*F is out of range for character \*Z \
variable \&'\fIcharvar\*C' at line \fIx\*F in file \&'\fIrtn\*C'. \*Z \
Parent string size is \fIssize\*F .
.PP
When substring bounds checking is enabled, this message is issued if a
substring reference exceeds the size of the parent character string.
At line number \fIx\fR in file \fIrtn\fR, the string size specified by
the starting character point \fIi1\fR and ending character point \fIi2\fR
of a substring exceeded the size \fIssize\fR of the parent
string \fIcharvar\fR.
.PP
Modify the program so the substring is within the bounds for the parent
string.
.PP
See the description of substring bounds checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGSVB Subscript is out of range for dimension %d for array\n\
  '%s' at line %d in file '%s',\n\
  diagnosed in routine '%s'.
$nexp FWARGSVB
Subscript is out of range for dimension \fId\*F for array \*Z \
\&'\fIary\*C' at line \fIx\*F in file \&'\fIrtn\*C' \*Z \
diagnosed in routine \&'\fIlibrtn\*C'.
.PP
When bounds checking is enabled, this message is issued if an array
reference exceeds the bounds of the array.  At line number \fIx\fR
in file \fIrtn\fR, the subscript value exceeded either the lower
or upper bounds of the array named \fIary\fR for dimension \fId\fR.
.PP
Either modify the program so that each subscript is within the bounds
for its dimension of the array or ensure the array extents are equal.
.PP
See the description of array bounds checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGDMD Conformance error for dimension %d at line %d in\n\
  file '%s'.  Extents %lld and %lld are not equal.\n\
  Diagnosed in routine '%s'.
$nexp FWARGDMD
Conformance error for dimension \ftd\*F at line \fIn\*C in \
file \&'\fIrtn\*C'.  Extents \fIe1\*C and \fIe2\*C are not \
equal.  Diagnosed in routine \&'\fIlibrtn\*C'.
.PP
When conformance checking is enabled, this message is issued if
an array does not conform because of its shape.  Two arrays are
conformable if they have the same shape.  The shape of an array
is determined by its rank and its extent in each dimension.
Dimension \fId\fR at line number \fIx\fR in file \fIrtn\fR does
not have the same extent.  The two array extents (\fIe1\fR
and \fIe2\fR) do not match.
.PP
Either modify the program to not exceed the bounds of the array, or
ensure that the array extents are equal.
.PP
See the description of array conformance checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWARGDMZ Conformance error for a dimension in transformational\n\
  function reference at line %d in file '%s'.\n\
  Extents %lld and %lld are not equal.  Diagnosed in routine '%s'.
$nexp FWARGDMZ
Conformance error for a dimension in transformational \
function reference at line \fIn\*C in file \&'\fIrtn\*C'. \
Extents \fIe1\*C and \fIe2\*C are not equal.  Diagnosed  \
in routine \&'\fIlibrtn\*C'.
.PP
When conformance checking is enabled, this message is issued if
an array does not conform because of its shape.  Two arrays are
conformable if they have the same shape.  The shape of an array
is determined by its rank and its extent in each dimension.  A
dimension \fId\fR in a transformational function reference at line
number \fIx\fR in file \fIrtn\fR does not have the same extent.
The two array extents (\fIe1\fR and \fIe2\fR) do not match.
.PP
Modify the program to ensure that the array extents are equal.
.PP
See the description of array conformance checking in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWASOCPT A disassociated pointer '%s' is referenced at line %d\n\
  in file '%s'.
$nexp FWASOCPT
A disassociated pointer \&'\fIptr\*C' is referenced at \
line \fIn\*C in file \&'\fIrtn\*C'.
.PP
Pointer association between a pointer and a target allows the
target to be referenced by the pointer.  When runtime checking
is enabled, this message is issued if the pointer has a pointer
association status of disassociated, i.e., the pointer is not
associated with a target.  The pointer \fIptr\fR at line
number \fIx\fR in file \fIrtn\fR is a disassociated pointer.
.PP
Modify the program to cause the pointer to become associated
with a target.
.PP
See the description of pointer association and pointer association
status in your Fortran reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWALOCAR An unallocated allocatable array '%s' is referenced at\n\
  at line %d in file '%s'.
$nexp FWALOCAR
An unallocated allocatable array \&'\fIarry\*C' is referenced \
at line \fIn\*C in file \&'\fIrtn\*C'.
.PP
An allocatable array is a named array that has the ALLOCATABLE
attribute and a specified rank, but its bounds and thus its
shape are determined when space is allocated for the array by
execution of an ALLOCATE statement.  The allocatable
array \fIarry\fR at line number \fIx\fR in file \fIrtn\fR is
not allocated.
.PP
Modify the program to specify the array in an ALLOCATE statement
before the array is referenced.
.PP
See the description of allocatable arrays in your Fortran
reference manual.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWASASAR A disassociated assumed shape array '%s' is referenced\n\
  at line %d in file '%s'.
$nexp FWASASAR
A disassociated assumed shape array \&'\fIarry\*C' is referenced \
at line \fIn\*C in file \&'\fIrtn\*C'.
.PP
An assumed-shape array is a nonpointer dummy argument that takes
its shape from the associated actual argument array.  When runtime
checking is enabled, this message is issued if the assumed shape
array is associated with a pointer or allocatable array actual
argument and that actual argument is not associated or allocated.
The array \fIarry\fR is associated with an actual argument array
that is a disassociated pointer or an unallocated allocatable
at line number \fIx\fR in file \fIrtn\fR.
.PP
Modify the program to cause the pointer or allocatable actual
argument array to be associated or allocated.
.PP
See the description of assumed shape arrays in your Fortran
reference manual.
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#ifdef KEY /* Bug 4260 */
$msg FWFILENV The FILENV environment variable overrides compile-time switches\n\
such as "-byteswap" or "-convert"
$nexp FWFILENV
The FILENV environment variable overrides compile-time switches \
such as "-byteswap" or "-convert"
.PP
The Fortran main program was compiled with a command line option such as
"-byteswap" or "-convert" which causes file I/O to swap bytes so that a
little-endian processor can use big-endian files (or vice versa.) But
the environment variable FILENV is set, which overrides the compile-time
settings and uses the environment established by the command assign(1).
.PP
If you do not want to override the compile-time switches, unset the FILENV
environment variable.
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#endif /* KEY Bug 4260 */
#ifdef KEY /* Bug 7479 */
$msg FWSTKNOMOD PSC_STACK_LIMIT is empty, so will not modify stack size limit
$nexp FWSTKNOMOD
PSC_STACK_LIMIT is empty, so will not modify stack size limit
.PP
When the environment variable PSC_STACK_LIMIT is set to the empty string, the
Fortran runtime system will leave the stack size limit unchanged. You may set
the variable to a specific size (for example, "256K", "512M", "2G", or
"50%"), or you may set the variable to the empty string, in which case the
runtime system will use the size of physical memory minus 128Mbyte per CPU.
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKNOCALC Could not discover prior stack size limit (error: %s)
$nexp FWSTKNOCALC
Could not discover prior stack size limit (error: \fIx\fR)
.PP
The Fortran runtime system was attempted to obtain the prior stack size limit
information from the operating system prior to changing the limit, but
encountered the error shown
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKNOLIM Stack size was unlimited prior to start of program
$nexp FWSTKNOLIM
Stack size was unlimited prior to start of program
.PP
Before the Fortran runtime system started work, the stack size was unlimited.
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKLIM Prior stack size limits: %ld current, %ld maximum
$nexp FWSTKLIM
Prior stack size limits: \fIn\fR current, \fIm\fR maximum
.PP
Before the Fortran runtime system started work, these parameters set the limit
on the stack size.
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKPHYSMEM Physical memory: %lld bytes\nNumber of CPUs: %lld
$nexp FWSTKPHYSMEM
Physical memory: \fIn\fR bytes
.br
Number of CPUs: \fIm\fR
.PP
This message is purely for your information
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKPHYSSHR Physical memory is shared between %d threads
$nexp FWPHYSSHR
Physical memory is shared between \fIn\fR threads
.PP
This message is purely for your information
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKAUTOMAX Automatic maximum stack size limit: %lld (%lld%% of RAM)
$nexp FWSTKAUTOMAX
Automatic maximum stack size limit: \fIn\fR (\fIm\fR% of RAM)
.PP
This message is purely for your information
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKILLFORM Requested stack size limit of "%s" is not a valid number
$nexp FWSTKILLFORM
Requested stack size limit of "%s" is not a valid number
.PP
An error occurred when the Fortran runtime system tried to convert the value
of environment variable PSC_STACK_LIMIT to a number. Examples of acceptable
values are "256K", "512M", "2G", or "50%".
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKBADMAX Bad maximum stack size limit of %lld (specified as "%s")
$nexp FWSTKBADMAX
Bad maximum stack size limit of \fIn\fR (specified as "\fIx\fR")
.PP
The stack size limit you specified is less than the minimum value allowed
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKUNLIM Treating requested stack size limit of %lld as no limit
$nexp FWSTKUNLIM
Treating requested stack size limit of \fIn\fR as no limit
.PP
You requested such a large stack size limit that there will be no limit
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKINFIN You requested no limit on the stack size
$nexp FWSTKINFIN
You requested no limit on the stack size
.PP
This message is purely for your information
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKLIMIT You have asked for a stack size limit of %lld (%lld%% of RAM)
$nexp FWSTKLIMIT
You have asked for a stack size limit of \fIn\fR (\fIm\fR% of RAM)
.PP
This message is purely for your information
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKVSPHYS Your requested stack size limit is %lld%% of physical memory
$nexp FWSTKVSPHYS
Your requested stack size limit is \fIn\fR% of physical memory
.PP
You have requested a stack size limit exceeding physical memory, which could impact execution speed.
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKNOREDUCE Stack limit is already greater than the default
$nexp FWSTKNOREDUCE
Stack limit is already greater than the default
.PP
Although the environment variable PSC_STK_LIMIT was not set, the Fortran
runtime system will not change the stack limit, because the prior limit was
already greater than the default value which the runtime system would use
.Ma COMMENT
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKNOCHG Could not change stack size limit (error was: %s)
$nexp FWSTKNOCHG
Could not change stack size limit (error: \fIx\fR)
.PP
The Fortran runtime system attempted to change the stack size limit by
calling the operating system, but encountered the error shown
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FWSTKINCORRECT Stack size limit was not set correctly; asked for %lld, got %ld
$nexp FWSTKINCORRECT
Stack size limit was not set correctly; asked for \fIn\fR, got \fIm\fR
.PP
The Fortran runtime system encountered no error when calling the operating
system to change the stack size, but the change did not have the desired
effect
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
#endif /* KEY Bug 7479 */
$
$	C Compiler-initiated run-time messages
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg CWPTRCHK At line %d in C file '%s', the pointer expression involving\n\
  '%s' is either less than or equal to zero or exceeds the memory\n\
  limit of this machine.
$nexp CWPTRCHK
At line \fIn\*C in C file \&'\fIname\*C', the pointer \
expression involving \&'\fIa\*C' is either less \
than or equal to zero or exceeds the memory \
limit of this machine.
.PP
When bounds checking is enabled, this message is issued if a
pointer dereference occurred when the pointer expression was
less than or equal to 0 or when it exceeded the memory limit of this
machine.  Line number \fIn\fR in C file \fIname\fR is where the
pointer dereference was made.
.PP
Modify the program so that it assigns the pointer to a value that
allows the dereference to be within the available memory of the program.
.PP
See the description of compiler options in
\fICray Standard C Reference Manual\fR, publication 
\s-1SR\-2074\s+1.
.PP
.Ma WARNING
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg CWBNDCHK At line %d in C file '%s', subscript value %d\n\
  is out of bounds for dimension %d, which has a size of %d,\n\
  for array '%s'.
$nexp CWBNDCHK
At line \fIn\*C in C file \&'\fIname\*C', subscript \
value \fIi\*C is out of bounds for dimension \fId\*C, \
which has a size of \fIs\*C, for array '\fIa\*C'.
.PP
When bounds checking is enabled, this message is issued if an
array reference exceeds the bounds of the array.  Line number
\fIn\fR in C file \fIname\fR is where the subscript value \fIi\fR
exceeded the bounds of the array named \fIa\fR for dimension \fId\fR.
The declared size of the dimension is \fIs\fR.
.PP
Either modify the program to not exceed the bounds of the array or
increase the size of the array.
.PP
See the description of compiler options in
\fICray Standard C Reference Manual\fR, publication
\s-1SR\-2074\s+1.
.PP
.Ma WARNING
.ME
#ifdef	_UNICOS
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	error messages and explanations for Flowtrace
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3001 Flowtrace: an internal error has occurred: too much heap requested.
$nexp 3001 
Flowtrace: an internal error has occurred: too much heap requested.
.PP
This is an error internal to Flowtrace.  However, it is
possible for a user program to cause this error by 
overwriting memory locations that do not belong to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3002 Flowtrace: an internal heap allocation error has occurred.
$nexp 3002 
Flowtrace: an internal heap allocation error has occurred.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
This error can also occur if you have specified a
fixed heap size on the \*Csegldr\fR(1) command.  If you
must run with a fixed heap size and want 
to execute with Flowtrace, you must set aside 
additional heap memory to hold Flowtrace tables.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3003 Flowtrace: an internal error has occurred: maximum routine name limit exceeded.
$nexp 3003 
Flowtrace: an internal error has occurred: maximum routine name limit exceeded.
.PP
It is unlikely that a user program will contain the maximum
number of routine names allowed, which is 32,760.  If
such a program exists, it will be necessary
to limit the number of routines being traced, including
\s-1FLOWMARK\s+1ed names, to run Flowtrace correctly.
.PP
It is more likely that this error is caused by 
a user program overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3005 Flowtrace: a calling stack overflow has occurred.  Routines\n\
called too deep, possible improper use of FLOWMARK, recursive\n\
routines being traced, or the program uses setjmp()/longjmp().
$nexp 3005 
Flowtrace: a calling stack overflow has occurred.  Routines called \
too deep, possible improper use of FLOWMARK, recursive routines \
being traced, or the program uses setjmp()/longjmp().
.PP
This error occurs when the fixed calling stack in Flowtrace overflows.
The stack used is 500 routines deep, a size that handles a great
majority of programs.  However, some behaviors of
programs can easily overrun this stack:
.TL 4
1.
Recursive routines.  Flowtrace cannot handle routines that call
themselves, or that are part of a long loop of
routines that folds in on itself.  If you still 
want to run this program with Flowtrace,
disable tracing for the recursive routine(s).
.TL
2.
Improper use of \s-1FLOWMARK\s+1 calls. 
For every call they execute to start a marked area,
you must execute a call to \s-1FLOWMARK\s+1 to terminate the marked area.
If a marked area is not terminated, it will remain in the stack
and can eventually overrun the stack.  If you are using \s-1FLOWMARK\s+1,
ensure that your program executes a starting \s-1FLOWMARK\s+1 
call, matched by the execution of a terminating \s-1FLOWMARK\s+1 call.
Be especially careful when you use \s-1FLOWMARK\s+1 around conditional
(\s-1IF\s+1) statements.  Note that a terminating call such as
"\s-1CALL FLOWMARK\s+1(0)" is not the same as 
"\s-1CALL FLOWMARK\s+1("0")".
.TL
3.
C programs that use the \*Csetjmp\fR/\*Clongjmp\fR functions can cause
major problems for Flowtrace.  Because Flowtrace does
not know the new calling tree caused by the
\*Clongjmp\fR, it can become confused and overrun the
calling stack.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3006 Flowtrace: this is an internal error, plimq is negative (type 1).
$nexp 3006 
Flowtrace: this is an internal error, plimq is negative (type 1).
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3007 Flowtrace: user-specified plimq value is negative.
$nexp 3007 
Flowtrace: user-specified plimq value is negative.
.PP
When you call the \*CSETPLIMQ\fR routine, you must supply
an integer value that specifies twice the
number of routine calls you wanted traced.  This
number must be positive.  This error occurs when
Flowtrace detects a negative number as the
calling parameter value for \*CSETPLIMQ\fR.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3009 Flowtrace: this is an internal error, plimq is negative (type 3).
$nexp 3009 
Flowtrace: this is an internal error, plimq is negative (type 3).
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3010 Flowtrace: program stopped while multitasking routines still active.
$nexp 3010 
Flowtrace: program stopped while multitasking routines still active.
.PP
This error occurs when a program that was running in
parallel stops in one of the parallel routines.  Usually
a program should be written so that it finishes
all of its work (parallel or serial) before it
terminates.  
.PP
Because some routines are still active during Flowtrace 
termination, it cannot properly gather all timers.
.PP
Ensure that your program terminates usually only
after all known parallel sections of code have
completed.  Study your program to ensure
no \*CSTOP\fR statements appear in a parallel
region.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3011 Flowtrace: internal error, plimq is negative (type 4).
$nexp 3011 
Flowtrace: internal error, plimq is negative (type 4).
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3014 Flowtrace: flowexit called out of order.  Possible improper\n\
use of FLOWMARK, or program uses setjmp()/longjmp().  Debug\n\
information follows: '%s'
$nexp 3014 
Flowtrace: flowexit called out of order.  Possible improper \
use of FLOWMARK, or program uses setjmp()/longjmp().  Debug \
information follows: \&'\*Vmore information\*C'
.PP
When the order of call is correct, Flowtrace senses
an exit from a routine, matching the entry to that 
routine.  When an exit that does not
belong to the current routine on the stack is sensed,
this message is issued.  Some behaviors of
programs can easily cause this error:
.TL 4
1.
Improper use of \s-1FLOWMARK\s+1 calls.  For every 
call they execute to start a marked area, you must
execute a call to \s-1FLOWMARK\s+1 to terminate the marked area.
If a marked area is not terminated, it will remain in the stack
and can eventually overrun the stack.  If you are using
\s-1FLOWMARK\s+1, ensure that your program executes a starting
\s-1FLOWMARK\s+1 call, matched by the execution of a terminating
\s-1FLOWMARK\s+1 call.  Be especially careful when you use
\s-1FLOWMARK\s+1 around conditional (\s-1IF\s+1) 
statements.  Note that a terminating call such as
"\s-1CALL FLOWMARK\s+1(0)" is not the same as 
"\s-1CALL FLOWMARK\s+1("0")".
.TL
2.
C programs that use the \*Csetjmp\fR/\*Clongjmp\fR functions can
cause major problems for Flowtrace.  Because Flowtrace does
not know that your program is leaving from
a different routine, it can become confused and issue
this error.
.PP
This error can also be caused when a user program overwrites memory 
locations that do not belong to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.PP
The debugging information provided with the original error
message may help you determine which routine was
running at the time of the error.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3015 Flowtrace: measured CPU times may be inaccurate, because of the\n\
presence of nontraced multitasked routines.  To ensure more correct timings,\n\
rerun with environment variable NCPUS set to 1.
$nexp 3015 
Flowtrace: measured CPU times may be inaccurate, because \
of the presence of nontraced multitasked routines.  To ensure \
more correct timings, rerun with environment variable \
NCPUS set to 1.
.PP
Although your program may not be designed to run in parallel, some
library routines provided by \s-1CRI\s+1 (principally in \*Clibsci.a\fR)
will take advantage of extra available \s-1CPU\s+1s on your machine.
.PP
This method speeds the processing of your program,
but it may lead to inaccurate timings from Flowtrace.
.PP
Your program is not written as a parallel program, and you should
re-execute with the \*CNCPUS\fR environment variable set to 1.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3016 Flowtrace: internal sort type error.
$nexp 3016 
Flowtrace: internal sort type error.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3018 Flowtrace: failed to get memory during '%s'.
$nexp 3018 
Flowtrace: failed to get memory during \&'activity'.
.PP
This error occurs if a call to \*Cmalloc\fR(3) fails.  It
may mean that you have reached your memory limit for 
this process.
.PP
This error can also occur if you have specified a
fixed heap size on the \*Csegldr\fR(1) command.  If you must
run with a fixed heap size and want
to execute with Flowtrace, you must set aside 
additional heap memory to hold Flowtrace tables.
.PP
Finally, it is possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3019 Flowtrace: unable to open file for writing: '%s'.
$nexp 3019 
Flowtrace: unable to open file for writing: \&'\*Vfilename\*C'.
.PP
By default, Flowtrace writes raw performance statistics
to the \*Cflow.data\fR file in the current directory.
You can also override this name
and specify a particular name and path on which
to write (setting the \*CFLOW_DATA\fR environment variable).
.PP
Flowtrace attempts to open this file with write
permission.  This message appears when the open fails.
The failure to open a file can occur for many reasons:
.TL 4
1.
The user is not allowed to generate files in the current directory.
.TL
2.
The file already exists, but the user does not have write permission
for it.
.TL
3.
The user has exceeded file limits imposed in this file system.
.TL
4.
The file name specified is not valid.
.TL
5.
The directory path specified by the user does not exist, is not valid,
or does not allow this user to generate files in it.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3021 Flowtrace: internal signal catch error.
$nexp 3021 
Flowtrace: internal signal catch error.
.PP
This error indicates an internal problem with
signals.  Normally, Flowtrace catches the following signals:
.CS
SIGHUP
SIGINT
SIGTERM
SIGCPULIM
.CE
.PP
If the user's program also catches this signal, the
output of Flowtrace may be nonexistent or this
error message may be issued.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3022 Flowtrace: this is an internal error:  incorrect signal return.
$nexp 3022 
Flowtrace: this is an internal error:  incorrect signal return.
.PP
This error indicates an internal problem with signals.  Normally
Flowtrace catches the following signals:
.CS
SIGHUP
SIGINT
SIGTERM
SIGCPULIM
.CE
.PP
If the user's program also catches this signal, the
output of Flowtrace may be nonexistent or this
error message may be issued.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3023 flodump: unable to open core file for reading.
$nexp 3023 
flodump: unable to open core file for reading.
.PP
The \*Cflodump\fR program attempts to recover Flowtrace
data from an unusually terminated program run.  It
does this by reading the \*Ccore\fR file generated by
that program run.  When you execute \*Cflodump\fR(1), the
\*Ccore\fR file from the original program must be
present in the current directory.  You must
have read privileges for this file.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3024 flodump: core file does not contain Flowtrace tables.
$nexp 3024 
flodump: core file does not contain Flowtrace tables.
.PP
The \*Cflodump\fR program attempts to recover Flowtrace
data from an abnormally terminated program run.  It
does this by reading the \*Ccore\fR file generated by
that program run.  
.PP
This message has several possible causes:
.TL 4
1.
The original program may have not been compiled and 
loaded with Flowtrace enabled.
.TL
2.
The original program may not have executed long enough
for Flowtrace to be called and to initialize its tables.
.TL
3.
The memory area containing Flowtrace tables may have been
corrupted.  When a program is overwriting
memory, it often writes over its own data as well
as that for Flowtrace.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3025 flodump: dumping with different version of\n\
Flowtrace libraries than original run.
$nexp 3025 
flodump: dumping with different version of Flowtrace \
libraries than original run.
.PP
This message indicates that there may be a mismatch
between versions of the Flowtrace library and the
\*Cflodump\fR routine.  Usually this is not a cause for
alarm, but your Systems Administrator should be
informed.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3026 flodump: flowtrace was never run, nothing to recover.
$nexp 3026 
flodump: flowtrace was never run, nothing to recover.
.PP
This message indicates that the original user program
terminated unusually and generated a \*Ccore\fR file
before Flowtrace could initialize its tables.  There
is no performance statistical data to be recovered.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3028 flodump: There is an internal error reconstructing\n\
flowtrace caller tables.
$nexp 3028 
flodump: There is an internal error reconstructing flowtrace caller tables.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail and cause \*Cflodump\fR(1) to issue this message.  In fact, 
these programs may be overwriting memory in a "safe" location, 
which becomes unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3029 flodump: There is an internal error reconstructing flowtrace\n\
stack tables.
$nexp 3029 
flodump: There is an internal error reconstructing flowtrace stack tables.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace,
may fail and cause \*Cflodump\fR(1) to issue this message.  In fact, 
these programs may be overwriting memory in a "safe" location, 
which becomes unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3030 flodump: maximum tasks exceeded: '%s'
$nexp 3030 
flowdump: maximum tasks exceeded: \&'\*Vnumber\*C'
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail and cause \*Cflodump\fR(1) to issue this error.  In fact, 
these programs may be overwriting memory in a "safe" location, 
which becomes unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3031 Flowtrace: This is an internal error: callee name not found.
$nexp 3031 
Flowtrace: This is an internal error: callee name not found.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3032 Flowtrace: This is an internal error: caller name not found.
$nexp 3032 
Flowtrace: This is an internal error: caller name not found.
.PP
This error is internal to Flowtrace.  However, it is
possible for a user program to cause this error
by overwriting memory locations that do not belong
to it.
.PP
Some programs that appear to run correctly without Flowtrace
may fail with this error.  In fact, these programs may
be overwriting memory in a "safe" location, which becomes
unsafe when Flowtrace tables are located there.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3033 This is an internal error: cannot register exit processor.\n\
   Too many "atexit()" routines.
$nexp 3033 
This is an internal error: cannot register exit processor.  Too many \
"atexit()" routines.
.PP
This message indicates that there are too many different
exit routines registered with the \*Catexit\fR(3) function.
Usually, many such routines are allowed.  However,
it is possible for a user program to have many such
registrations present.  To run Flowtrace
with this program, you must disable one or more
uses of the \*Catexit\fR(3) function.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg 3034 Flowtrace: multi-streaming programs are not supported.
$nexp 3034
Flowtrace: multi-streaming programs are not supported.
.PP
The executable file has been compiled to run on one or more multi-streaming
processors.  The multi-streaming feature is not supported by flowtracing.
To use flowtracing on your program, recompile those source files
which you want flowtracing to analyze without the \*Cstream\fP
compile option.
#endif	/* _UNICOS */
$ -------|---------|---------|---------|---------|---------|---------|---------|
$
$	FFIO error messages.
$
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_INTERR An internal FFIO error has been encountered.
$nexp FDC_ERR_INTERR
An internal FFIO error has been encountered.
.PP
An internal library run-time error has occurred.  The
Fortran \s-1I/O\s+1 operation or \s-1FFIO\s+1 operation failed because
of an internal error in the run-time library.
.PP
The program may have overwritten itself, corrupting the data
used by the run-time library.
.PP
This represents an internal error that usually does not occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_CHAIN The FFIO layer specification is inconsistent.
$nexp FDC_ERR_CHAIN
The FFIO layer specification is inconsistent.
.PP
The specification on the \*Fassign\fR(1) or \*Fasgcmd\fR
command is not correct.
This usually means that the specification includes a record
translation layer that has its own system interface (such as
\*Ftape\fR/\*Fbmx\fR) and is followed by another specification.
.PP
Change the specification on the \*Fassign\fR or \*Fasgcmd\fR
command for the associated file.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOSUP This FFIO request is not supported.
$nexp FDC_ERR_NOSUP
This FFIO request is not supported.
.PP
The request requires features that are not supported.
For example, \*Fffseek\fR\(3) with \fIwhence\fR = 1 (\*FSEEK_CUR\fR) is not
supported for some file specifications.  This error can
also occur if the format of the file is not consistent
with the file specification.  For example, if the specification
requests that a blocked file be unblocked and held memory
resident, but the blocked file contains endfile records
followed by data, this error will be returned.
.PP
Ensure that the file specification chosen is capable of handling
the data in the file.  When possible, recode the application so that
the unsupported request is not used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOBDRY The format used requires lower-level record boundaries.
$nexp FDC_ERR_NOBDRY
The format used requires lower-level record boundaries.
.PP
Some file formats require lower levels to provide block
boundary information. When they are not found, this error is
issued.  This can be caused if the file specification is wrong,
or if a lower-level layer does not handle block boundaries.
It may also be caused by a corrupted file.
.PP
Ensure that the file specification chosen is capable of handling
the data in the file.  If the file is corrupted,
.br
\%re-create it if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_UBC The UBC (unused bit count) is invalid for this FFIO request.
$nexp FDC_ERR_UBC
The UBC (unused bit count) is invalid for this FFIO request.
.PP
The \s-1UBC\s+1 (unused bit count) specified is not valid for this \s-1FFIO\s+1
request.  Some file specifications are not capable of handling
a nonzero unused bit count.  This error can also be caused by
mismatched \*F-N\fR and \*F-F\fR parameters to the
\*Fassign\fR or \*Fasgcmd\fR command.
.PP
Either change the \*FREAD\fR or \*FWRITE\fR statement so that the \s-1UBC\s+1
is valid or change the file specification to one that will handle this
request.  Ensure that the \*F-N\fR and \*F-F\fR parameters to the \*Fassign\fR
or \*Fasgcmd\fR command are compatible.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_FMT The record format is invalid.  File corruption is possible.
$nexp FDC_ERR_FMT
The record format is invalid.  File corruption is possible.
.PP
This message is issued when the control information (for example, record
control bits) in a file is found to be in error.  It can occur
if the file specification was incorrect, or if
the file has been corrupted.
.PP
Change the file specification to match the file type.  If
the file is corrupted, \%re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_SCC The FFIO file contains an invalid segment control code.
$nexp FDC_ERR_SCC
The FFIO file contains an invalid segment control code.
.PP
This message is issued when the control information (for example, segment
control bits) in a file is found to be in error.  It can occur
if the file specification was incorrect or if
the file has been corrupted.
.PP
Change the file specification to match the file type.  If
the file is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_MXREC This request exceeds the maximum record size.
$nexp FDC_ERR_MXREC
This request exceeds the maximum record size.
.PP
A Fortran \*FWRITE\fR or \*Fffwrite\fR\(3) attempted to write a record that
exceeds the maximum record size.  The maximum record size is
specified through the use of the \*Fassign\fR(1) or \*Fasgcmd\fR(1) command.
.PP
Either increase the record size specified with the \*Fassign\fR or
\*Fasgcmd\fR command or decrease the size of the record being written.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_MXBLK This request exceeds the maximum block size.
$nexp FDC_ERR_MXBLK
This request exceeds the maximum block size.
.PP
The program attempted to write a record to a tape file that
exceeded the maximum block size specified with the \*F-b\fR parameter
on the \*Ftpmnt\fR(1) command.
.PP
Either increase the maximum block size specified with the \*F-b\fR parameter
on the \*Ftpmnt\fR command or decrease the size of the records being
written by the program.
.PP
See \*Ftpmnt\fR(1) in the \fI\*u User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
.PP
.Ma UNRECOVERABLE
.PP
This message is issued only on \*2 systems.
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_RAWR Read-after-write is invalid on this file type.
$nexp FDC_ERR_RAWR
Read-after-write is invalid on this file type.
.PP
A Fortran \*FREAD\fR or \*Fffread\fR() was attempted on a file on which
the previous operation was a write.  Because a write operation
implies a file truncation immediately following the write,
a following read is not valid.
.PP
Reposition the file after the write operation and before
the read operation.
.PP
See the description of input/output statements in your Fortran
reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOMEM A request for more memory has failed.
$nexp FDC_ERR_NOMEM
A request for more memory has failed.
.PP
A routine in the run-time library was unable to request
additional memory space.  This usually occurs when the
process has exceeded its memory allocation.
.PP
Review the use of memory space by your program, and
reduce its memory use if possible.
#ifdef _UNICOS
.PP
See \*Flimit\fR(1) in the \fI\*u User Commands
Reference Manual\fR, publication \s-1SR\-2011\s+1.
#endif
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_REQ This FFIO request is invalid.
$nexp FDC_ERR_REQ
This FFIO request is invalid.
.PP
The value of a parameter passed to one of the \s-1FFIO\s+1 library
routines was not valid.  For example, if \*Fffread\fR\(3) is called
with a negative value for the \fInbytes\fR parameter, this error
may be returned.
.PP
Ensure that you are using the correct parameters.
.PP
See \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_PITM Numeric conversion of a partial item is invalid.
$nexp FDC_ERR_PITM
Numeric conversion of a partial item is invalid.
.PP
During an attempt to read a file and perform numeric conversion
on its contents, a partial item was read.  The numeric
conversion could not be performed.  The \*F-N\fR parameter to the
\*Fassign\fR(1) or \*Fasgcmd\fR(1) command
may be incorrect.  The error can also occur
if the data in the file does not match the type of the data
being read, or if the file is corrupted.
.PP
Ensure that the data in the file matches the type of data
in the \*FREAD\fR statement.  Ensure that the \*F-N\fR parameter to the
\*Fassign\fR or \*Fasgcmd\fR command matches the file type.  If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_WPEOD Writing past EOD (end-of-data) is not permitted.
$nexp FDC_ERR_WPEOD
Writing past EOD (end-of-data) is not permitted.
.PP
A write request was issued past the logical end of data.
For example, if a Fortran \*FENDFILE\fR operation was performed
on a file whose file specification does not have physical
endfile representations, a following write will fail.  The
error is also issued when a write operation immediately follows
a read statement that has returned end-of-data status.
.PP
Reposition the file before issuing the write request.  If you want
to write a file with physical endfile representations, use
a file specification that supports this.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_RPEOD Reading past EOD (end-of-data) is not permitted.
$    Not used
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_PADD The block size must be a multiple of record size.
$nexp FDC_ERR_PADD
The block size must be a multiple of record size.
.PP
A Fortran \*FREAD\fR or \*Fffread\fR() statement encountered a block
whose size is not a multiple of the record size.  For the
file specification given, the block size must be a multiple
of the record size.  The error can also be caused by a corrupted
file.
.PP
Ensure that the file specification is correct.  If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_UXEND An EOF or EOD has been encountered unexpectedly.
$nexp FDC_ERR_UXEND
An EOF or EOD has been encountered unexpectedly.
.PP
The file terminated unexpectedly, perhaps without the proper
end-of-file record or end-of-data control information.  The
file specification may be incorrect, or the file may be corrupted.
.PP
Ensure that the file specification is correct.  If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_CDCBT The CDC file contains an invalid block terminator.
$nexp FDC_ERR_CDCBT
The CDC file contains an invalid block terminator.
.PP
A Fortran \*FREAD\fR or \*Fffread\fR() statement encountered a block terminator
that is not valid in a \s-1CDC\s+1 format file.  The file specification
may be incorrect, or the file may be corrupted.
.PP
Ensure that the file specification is correct.  If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$ Following message is unused.
$msg FDC_ERR_CDCICW The CDC file contains an invalid I control word.
$nexp FDC_ERR_CDCICW
The CDC file contains an invalid I control word.
.PP
A Fortran \*FREAD\fR or \*Fffread\fR() statement encountered an \*FI\fR control word
that is not valid in a \s-1CDC\s+1 format file.  The file specification
may be incorrect, or the file may be corrupted.
.PP
Ensure that the file specification is correct.  If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_CDCWCW The CDC file contains an invalid W control word.
$nexp FDC_ERR_CDCWCW
The CDC file contains an invalid W control word.
.PP
A Fortran \*FREAD\fR or \*Fffread\fR() statement encountered a \*FW\fR control word
that is not valid in a \s-1CDC\s+1 format file.  The file specification
may be incorrect, or the file may be corrupted.
.PP
Ensure that the file specification is correct. If the file
is corrupted, re-create the data if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$ Here - used to be WAEOD Now unused.
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_WRARD A write-after-read operation is not yet supported.
$nexp FDC_ERR_WRARD
A write-after-read operation is not yet supported.
.PP
A Fortran \*FWRITE\fR or \*Fffwrite\fR() statement immediately followed
a read statement.  This operation is not yet supported.
.PP
If possible, recode the application so that a write-after-read
is not used.
.PP
See the description of input/output statements in your Fortran reference manual.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_DISABL The requested FFIO layer has been disabled.
$nexp FDC_ERR_DISABL
The requested FFIO layer has been disabled.
.PP
Disabled layers are sets of
routines that are not loaded by default.  They must be explicitly
requested, either by the system administrator or by an individual
user.  A system administrator can change the default set of layers
for a given site.
.PP
Either the \s-1FFIO\s+1 file specification is in error or
the requested \s-1FFIO\s+1 layer was disabled when the libraries
you are using were built.
.PP
On \s-1Cray PVP\s+1 systems, the desired \s-1FFIO\s+1 layer(s) can be enabled
at load time with \*F segldr -j ff_\fIxxx\ \fRwhere \fIxxx\fR is the 
class desired.  For example, \*Fsegldr -j ff_ibm\ \fRwould load the handlers
for \s-1IBM\s+1 record translation.
\*Fsegldr -j ff_all \fRwill load all of the optional capabilities if there is
any doubt about which option is needed.
On \s-1Cray T3E\s+1 systems, the \s-1cld\s+1 directive file ff_\fIxxx\fR.cld, where \fIxxx\fR
is the class desired, can be used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_CCVRT A character conversion operation has failed.
$nexp FDC_ERR_CCVRT
A character conversion operation has failed.
.PP
A character conversion error was encountered during an
implicit character conversion of an item being read or written.
.PP
Ensure that the type of the data in the file matches
the type on the \s-1I/O\s+1 statement.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NCVRT A numeric conversion operation has failed.
$nexp FDC_ERR_NCVRT
A numeric conversion operation has failed.
.PP
A numeric conversion error was encountered during an
implicit numeric conversion of an item being read or written.
.PP
Ensure that the type of the data in the file matches
the type on the \s-1I/O\s+1 statement. If the error occurs on
a write statement, ensure that the variable contains a
valid numeric value.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADSPC The open specification is invalid.
$nexp FDC_ERR_BADSPC
The open specification is invalid.
.PP
A request to open a file failed because a parameter from the
associated \*Fassign\fR(1) or
\*Fasgcmd\fR(1) specification was out of range.  This
error is returned when the \*Fffopens\fR\(3) routine is called with a \fIstr\fR parameter
that is not valid.  It can also occur if the
\*Fassign\fR or \*Fasgcmd\fR command used is newer than the libraries
loaded with the program.
.PP
Ensure that the parameters used in the \*Fassign\fR or \*Fasgcmd\fR
command or \*Fffopens\fR call are valid.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADBCW The COS blocked file contains an invalid BCW (block control word).
$nexp FDC_ERR_BADBCW
The COS blocked file contains an invalid BCW (block control word).
.PP
A Fortran \s-1I/O\s+1 or positioning operation or an \*FFFIO\fR() operation
on a \s-1COS\s+1 blocked file encountered a \s-1BCW\s+1 (block
control word) that is not valid.  The file may not be a \s-1COS\s+1 blocked file,
or it may be corrupted.
.PP
Ensure that the file is \s-1COS\s+1 blocked; if it is not, use the
appropriate specification on the \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command. If the file is corrupted, re-create it if possible.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADRCW The COS blocked file contains an invalid RCW (record control word).
$nexp FDC_ERR_BADRCW
The COS blocked file contains an invalid RCW (record control word).
.PP
A Fortran \s-1I/O\s+1 or positioning operation or an \*FFFIO\fR() operation
on a \s-1COS\s+1 blocked file encountered a \s-1RCW\s+1 (record
control word) that is not valid.  The file may not be a \s-1COS\s+1 blocked file,
or it may be corrupted.
.PP
Ensure that the file is \s-1COS\s+1 blocked; if it is not, use the
appropriate specification on the \*Fassign\fR(1) or \*Fasgcmd\fR(1)
command. If the file is corrupted, re-create it if possible.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_WRDEV The device type is wrong for the requested I/O.
$nexp FDC_ERR_WRDEV
The device type is wrong for the requested I/O.
.PP
An \*Fassign\fR(1) or \*Fasgcmd\fR(1) command specification was
intended for a specific device type.  At open time, the device
was found to be of a different type.  This can occur if, for example,
you assign a file as type tape or bmx (\*Fassign -s tape...\fR),
and the file is not a tape.
.PP
Ensure that the specification matches the type of the file.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADNVE The NOS/VE V-format file contains an invalid control word.
$nexp FDC_ERR_BADNVE
The NOS/VE V-format file contains an invalid control word.
.PP
A record control word that was not valid was detected in a \s-1NOS/VE V\s+1 file.
The file may not be a \s-1NOS/VE V\s+1 file, or it may be corrupted.
.PP
Verify that the file is in \s-1NOS/VE V\s+1-format.  If it is not, change the
file specification. If the file is corrupted,
.br
re-create it if possible.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_FSSALO An SDS allocation or reallocation operation failed.
$nexp FDC_ERR_FSSALO
An SDS allocation or reallocation operation failed.
.PP
An internal inconsistency was found in the library tables
associated with \s-1SDS\s+1 space.
.PP
The program may have overwritten itself, corrupting the data
used by the run-time library.
.PP
See \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_SDSIO An SDS I/O operation (ssread() or sswrite()) failed.
$nexp FDC_ERR_SDSIO
An SDS I/O operation (ssread() or sswrite()) failed.
.PP
An attempt to read or write to a file resident on the
\s-1SDS\s+1 failed.  This can occur if the user's program calls
\*Fssbreak\fR(2) with a negative increment to deallocate \s-1SDS\s+1 space
that the library has allocated.  This error can also be caused
when hardware errors occur during the data transfer.
.PP
Ensure that you are not calling \*Fssbreak\fR to deallocate space
that is used by \s-1SDS\s+1 resident files.
.PP
See \*Fssbreak\fR(2) in
the \fI\*u System Calls Reference Manual\fR, publication 
\s-1SR\-2012\s+1.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADSK A parameter to an ffseek() operation is invalid.
$nexp FDC_ERR_BADSK
A parameter to an ffseek() operation is invalid.
.PP
A call to \*Fffseek\fR(3) was made with one or more parameters that are not valid.
.PP
Ensure that the parameters to \*Fffseek\fR are valid.
.PP
See \*Fffseek\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADCOS The COS blocked file has an invalid format.
$nexp FDC_ERR_BADCOS
The COS blocked file has an invalid format.
.PP
An inconsistency was found in the data in a file being processed
as \s-1COS\s+1 blocked.  The file may not be a \s-1COS\s+1 blocked file, or it
may be corrupted.
.PP
Ensure that the file is \s-1COS\s+1 blocked; if it is not, use the
appropriate specification on the \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command.  If the file is corrupted, re-create it if possible.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_FSSOVF FSS (fast secondary storage) overflow is not permitted by user request.
$nexp FDC_ERR_FSSOVF
FSS (fast secondary storage) overflow is not permitted by user request.
.PP
Through use of the \*F-F\fR parameter of the \*Fassign\fR(1) or
\*Fasgcmd\fR(1) commands you 
selected no overflow on the \s-1SDS\s+1 or \s-1MR\s+1 (memory resident)
layer, and
the layer needed to overflow.  A layer must overflow when the
size of the file exceeds the maximum allocation specified with the
\*F-F\fR parameter.
.PP
Increase the maximum allocation specified for the file with the
\*F-F\fR parameter of the \*Fassign\fR or \*Fasgcmd\fR command, or change the
specification to allow the file to overflow, or decrease the
amount of data being written to the file.
.PP
See \*Fassign\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NWEOF The requested layer cannot write an EOF (end-of-file).
$nexp FDC_ERR_NWEOF
The requested layer cannot write an EOF (end-of-file).
.PP
An \*Fffweof\fR\(3) call has failed because one of the layers in the
file specification has no representation for a physical
end-of-file record.
.PP
Change the file specification to a type that allows physical
end-of-file records if one is necessary.
.PP
See \*Fassign\fR(1) in \*(aI and \*Fffweof\fR(3) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADPRI An invalid PRI field has been found in the RCW of a COS blocked file.
$nexp FDC_ERR_BADPRI
An invalid PRI field has been found in the RCW of a COS blocked file.
.PP
During a backspace operation on a \s-1COS\s+1 blocked file, a \s-1PRI\s+1
(previous record index) field that was not valid was found in the \s-1RCW\s+1 (record
control word).  The file may be corrupted.
.PP
If the file is corrupted, re-create it if possible.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADPFI An invalid PFI field has been found in the RCW of a COS blocked file.
$nexp FDC_ERR_BADPFI
An invalid PFI field has been found in the RCW of a COS blocked file.
.PP
During a backfile operation on a \s-1COS\s+1 blocked file, a \s-1PFI\s+1 (previous
file index) field that was not valid was found in the \s-1RCW\s+1 (record
control word).  The file may be corrupted.
.PP
If the file is corrupted, re-create it if possible.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOPARM An FFIO routine was called with insufficient parameters.
$nexp FDC_ERR_NOPARM
An FFIO routine was called with insufficient parameters.
.PP
A call was made to a routine, but not enough parameters were
passed to process the request.
.PP
Ensure that the subroutine call contains the correct number of
parameters.
.PP
See \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOGPOS GETPOS is not supported for this file specification.
$nexp FDC_ERR_NOGPOS
The GETPOS operation is not supported for this file specification.
.PP
If possible, select a file specification that supports this
request, or recode the application so that the unsupported request
is no longer used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOSPOS SETPOS is not supported for this file specification.
$nexp FDC_ERR_NOSPOS
The SETPOS operation is not supported for this file specification.
.PP
If possible, select a file specification that supports this
request, or recode the application so that the unsupported request
is no longer used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOTREC The file must be positioned on a record boundary to honor this request.
$nexp FDC_ERR_NOTREC
The file must be positioned on a record boundary to honor this request.
.PP
The file must be positioned on a record boundary to process the request, and
it is not.  This can occur in Fortran when you use \*FGETPOS\fR(3) and
\*FSETPOS\fR(3) on \s-1COS\s+1 blocked files.  It can also happen when a
multilayer \s-1FFIO\s+1 specification is specified in the wrong order.
.PP
Ensure that the file is positioned on a record boundary before
issuing the request.  Ensure that the \*Fassign\fR or \*Fasgcmd\fR
specification is correct.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADPTR The fd parameter passed to an FFIO routine is invalid.
$nexp FDC_ERR_BADPTR
The fd parameter passed to an FFIO routine is invalid.
.PP
The \*Ffd\fR parameter passed to an \s-1FFIO\s+1 routine, such as
\*Fffread\fR(3), is
not valid.  This error can occur if the program has
overwritten itself, corrupting data used by the run-time library.
.PP
The \*Cfd\fR parameter passed to an \s-1FFIO\s+1 routine must be an integer
returned by \*Fffopen\fR(3) or \*Fffopens\fR.
Ensure that the \*Fffopen\fR(s) call has succeeded before using the \*Cfd\fR
parameter it returns.
.PP
See \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NBUF0 The number of buffers requested is invalid.
$nexp FDC_ERR_NBUF0
The number of buffers requested is invalid.
.PP
The number of buffers requested with an \s-1FFIO\s+1 specification is zero,
negative or too large.  This error can occur with the \*Fcache, cachea, or bufa\fR
layers.  The specification\*F -F cache:bs:nb\fR requests \*Fnb\fR buffers of
size \*Fbs\fR.
.PP
Specify the number of buffers with a positive integer.
.PP
See \*Fassign\fR(1) in \*(aI and \*Fintro_ffio\fR(1) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BUFSIZ The buffer size requested is invalid.
$nexp FDC_ERR_BUFSIZ
The buffer size requested is invalid.
.PP
The buffer size requested with an \s-1FFIO\s+1 specification is  zero,
negative or is too large.  This error can occur with the \*Fcache, cachea, or bufa\fR
layers.  The specification \*F-F cache:bs:nb\fR requests \*Fnb\fR buffers of
size \*Fbs\fR.
.PP
Specify the buffer size with a positive integer.
.PP
See \*Fassign\fR(1) in \*(aI and \*Fintro_ffio\fR(1) in \*(aL.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOSTRM An underlying FFIO layer is not byte-stream capable.
$nexp FDC_ERR_NOSTRM
An underlying FFIO layer is not byte-stream capable.
.PP
An attempt was made to open a file with an \s-1FFIO\s+1 specification in
which 
at least one of the layers requires that the underlying layer be a byte-stream
type (as opposed to a blocking layer, such as \s-1COS\s+1-blocked).
.PP
This is caused by an incorrect \s-1FFIO\s+1 specification
such as the \*F-F\fR parameter on an \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command.
.PP
Change the \s-1FFIO\s+1 specification for the file.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADOVF FSS (fast secondary storage) overflow has failed.
$nexp FDC_ERR_BADOVF
FSS (fast secondary storage) overflow has failed.
.PP
A file being handled by an \s-1SDS\s+1 or \s-1MR\s+1 (memory resident)
\s-1FFIO\s+1 layer has grown to a size that requires that it overflow.
.PP
This error condition has a number of possible causes.
The most common cause is an incorrect \s-1FFIO\s+1 specification
(\*F-F\fR parameter) on an \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command.  For instance, for a COS blocked file residing on
SDS, the correct specification would be \*F-F cos,sds\fR.  If the
specification is reversed, (\*F-F sds,cos\fR) and an overflow occurs, 
this error will be issued.
.PP
This condition can also be caused by an overflow to something other than a
regular (disk) file. Overflow to a pipe or a tape can cause this error.
.PP
Check the \s-1FFIO\s+1 specification for correctness.  If it is
correct, prevent the overflow by
increasing the maximum allocation specified for the file with the
\*F-F\fR parameter of the \*Fassign\fR/\*Fasgcmd\fR command, change the
specification to allow the file to overflow, or decrease the
amount of data being written to the file.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_SDSWB I/O for an SDS-buffered file was attempted to a non-word aligned\n\
  memory location.
$nexp FDC_ERR_SDSWB
\s-1I/O\s+1 for an SDS-buffered file was attempted to a non-word aligned memory location.
.PP
A file with an FFIO specification including a layer which buffers
data in an SDS segment must be requested to do \s-1I/O\s+1 to word-aligned
user memory.   This requirement arises because the underlying \*Cssread\fR(2)
and \*Csswrite\fR(2) system calls are used to transfer data between the \s-1I/O\s+1
buffers and the user memory.
.PP
FFIO specifications which utilize SDS buffering include
"cache.sds", and "cachea.sds".
.PP
Either assign a different FFIO specification, or ensure that the
user data area is word-aligned.
.PP
See \*Fassign\fR(1) in \*(aI
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_OPNGRAN A file has a size which is not a multiple of the FFIO layer's granularity.
$nexp FDC_ERR_OPNGRAN
A file has a size which is not a multiple of the FFIO layer's granularity.
.PP
This error is encountered when an FFIO specification contains a layer
which has a granularity greater than 8 bits.   For example, some FFIO
layers which have \%SDS-resident buffers have a granularity of 512 words.
.PP
Take measures to extend the file to a size that is a multiple of the
granularity, or use a different FFIO specification.
.PP
See \*Fassign\fR(1) in \*(aI.  
.PP
.Ma CRI_ONLY
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_GRAN An operation on a file has left the file positioned at a location which\n\
  is not a multiple of the FFIO layer's granularity.
$nexp FDC_ERR_GRAN
An operation on a file has left the file positioned at a location which is not a multiple of the FFIO layer's granularity.
.PP
Some FFIO layers require that \s-1I/O\s+1 requests be aligned on boundaries
which are multiples of the file's granularity.
.PP
Take measures to align \s-1I/O\s+1 operations on the file to multiples of the
granularity, or use a different FFIO specification.
.PP
See \*Fassign\fR(1) in \*(aI.  
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BCKDOOR The "cache.ssd" and "cachea.ssd" FFIO layer specifications require\n\
  an underlying layer which supports O_SSD.
$nexp FDC_ERR_BCKDOOR
The "cache.ssd" and "cachea.ssd" FFIO layer specifications require an underlying layer which supports O_SSD.
.PP
When specified with the \*F.ssd\fR layer option, the cache and cachea layers 
require that underlying FFIO layers support the \*FO_SSD\fR open mode.
This is necessary because these caching layers must transfer data between
the underlying file and an SDS-resident buffer.
.PP
This error may be caused by an unsupported \s-1FFIO\s+1 specification
such as the \*F-F\fR parameter on an \*Fassign\fR(1) command or an \*Fasgcmd\fR(1) command,
or the last argument of the \*Fffopens()\fR function call.
.PP
Check the \s-1FFIO\s+1 layer specification for the layer which underlies
the cache or cachea \s-1FFIO\s+1 layer.   Change this layer
to some \s-1FFIO\s+1 layer which supports the \*FO_SSD\fR mode, for
example the system layer.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOER90 The file does not reside on an ER90 device.
$nexp FDC_ERR_NOER90
The file does not reside on an ER90 device.
.PP
An attempt was made to open a file which was assigned to an ER90 device.
The file does not reside on an ER90 device, however.
.PP
This can be caused by an incorrect \s-1FFIO\s+1 specification
(the \*F-F\fR parameter) on an \*Fassign\fR(1) or
\*Fasgcmd\fR(1) command.  Verify that the \*Ftpmnt\fR command was successful.
.PP
Change the \s-1FFIO\s+1 specification for the file, or reissue the \*Ftpmnt\fR command.
.PP
See \*Fassign\fR(1) in \*(aI.
Also see the \fI\s-1UNICOS\s+1 Tape Subsystem
User's Guide\fR, publication \s-1SR\-2051\s+1.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_OAPPEND The O_APPEND flag is unsupported by the specified FFIO layers.
$nexp FDC_ERR_OAPPEND
The \s-1O_APPEND\s+1 flag is unsupported by the specified \s-1FFIO\s+1 layers.
.PP
An attempt was made to open a file using \*Fffopen\fR(3) or \*Fffopens\fR(3)
with the \*FO_APPEND\fR open mode and an \s-1FFIO\s+1 
specification which does not support \*Fappend\fR mode.
.PP
This can be caused by an incorrect \s-1FFIO\s+1 specification
(the \*F-F\fR parameter) on an \*Fassign\fR(1) or \*Fasgcmd\fR(1) command or
the last argument of the \*Fffopens()\fR function call.
.PP
Check the top \s-1FFIO\s+1 layer specification to establish the conditions
under which the layer supports append mode.   If the layer does not support
append mode under any conditions, a different \s-1FFIO\s+1 specification
must be used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_OSSD The O_SSD flag is unsupported by the specified FFIO layers.
$nexp FDC_ERR_OSSD
The \s-1O_SSD\s+1 flag is unsupported by the specified \s-1FFIO\s+1 layers.
.PP
An attempt was made to open a file by using \*Fffopen\fR(3) or \*Fffopens\fR(3)
with the \*FO_SSD\fR open mode and an \s-1FFIO\s+1 
specification which does not support access of memory in a secondary data 
segment (SDS).
.PP
This can be caused by an incorrect \s-1FFIO\s+1 specification
(the \*F-F\fR parameter) on an \*Fassign\fR(1) or \*Fasgcmd\fR(1) command or
the last argument of the \*Fffopens()\fR function call.
.PP
Check the top \s-1FFIO\s+1 layer specification to establish the conditions
under which the layer supports \*FO_SSD\fR mode.   If the layer does not 
support \s-1O_SSD\s+1 mode under any conditions, a different \s-1FFIO\s+1 
specification must be used.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_LSTIO An internal listio subrequest references more than one FFIO layer.
$nexp FDC_ERR_LSTIO
An internal listio subrequest references more than one \s-1FFIO\s+1 layer.
.PP
This represents an internal error that usually does not occur.
.PP
The list of \s-1I/O\s+1 requests passed to an FFIO layer's listio handler 
contained a request for an FFIO file descriptor with a different top-level
\s-1FFIO\s+1 layer.
.PP
\s-1FFIO\s+1 file descriptors listed in the \*Cfflistreq\fR array of \s-1I/O\s+1 requests may
be for different files, but they must all be for files which have the same 
top-level \s-1FFIO\s+1 layer.   
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOSCR The file cannot be made scratch by use of fffcntl() with the FC_SCRATCH\n\
function code.
$nexp FDC_ERR_NOSCR
The file cannot be made scratch by use of fffcntl() with the FC_SCRATCH function code.
.PP
An FFIO file cannot be made scratch because its name is not known.
This is possible by use of some layers, for example the \*Ffd\fR layer,
which connect to a file without a name.
.PP
See \*Fassign\fR(1) in \*(aI. 
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOTAPE An ER90 file was not assigned an appropriate -F attribute.
$nexp FDC_ERR_NOTAPE
An ER90 file was not assigned an appropriate -F attribute.
.PP
An attempt was made to open a file which resides on an ER90 device. 
Either the file was assigned with an \s-1FFIO\s+1 specification that
is not supported for this device, or the file was not assigned.
If the ER90 tape is mounted in byte-stream mode, then the file must
be assigned with an \s-1FFIO\s+1 specification whose lower layer
is \*Fer90\fR, \*Fsyscall\fR, or \*Fsystem\fR. If the ER90 tape is mounted in
block mode, then the file may be assigned with an \s-1FFIO\s+1 specification 
whose lower layer is \*Fbmx\fR, \*Fsyscall\fR, or \*Fsystem\fR.
.PP
See \*Fassign\fR(1) and \*Fasgcmd\fR(1) in \*(aI.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_WRTERR A write system call wrote less data than expected.
$nexp FDC_ERR_WRTERR
A write system call wrote less data than expected.
.PP
A write() or writea() system call wrote less data than expected.
The system call wrote some of the data, but not the total amount
requested. This error can occur if the write() requests that more bytes
be written than there is room for (e.g., there is no space left on the disk).
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_RDERR A read system call read less data than expected.
$nexp FDC_ERR_RDERR
A read system call read less data than expected.
.PP
A read() or reada() system call read less data than expected.
The system call did not read the number of bytes requested.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_SHCAC Stacked shared caches are not supported in multitasked programs.
$nexp FDC_ERR_SHCAC
Stacked shared caches are not supported in multitasked programs on \s-1UNICOS\s+1
systems. Stacked shared caches are not supported on \s-1IRIX\s+1 systems.
.PP
An attempt was made to open a file using more than one shared cache
(for example, \*F assign -F cachea::::1,cachea::::2\ \fR).
This is unsupported in a multitasked program on \s-1UNICOS\s+1 systems. 
It is also unsupported on \s-1IRIX\s+1 systems.
.PP
Change the \s-1FFIO\s+1 specification for the file, so that multiple shared
caches are not used. 
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_MTLSTIO An FFIO layer has indicated that it cannot handle fflistio requests.
$nexp FDC_ERR_MTLSTIO
An FFIO layer has indicated that it cannot handle fflistio requests.
.PP
An FFIO layer has indicated that it is multitasking safe, but it has not
indicated that it can handle fflistio requests. A layer must be able
to handle fflistio requests in order for it to be multitasking safe.
This error should only occur with user or site layers.
.PP
The layer should be changed so that it does not indicate that it is
multitasking safe, or it should be made capable of handling fflistio requests.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_MTLOCK The FFIO layers cannot be properly locked for multitasking.
$nexp FDC_ERR_MTLOCK
The FFIO layers cannot be properly locked for multitasking.
.PP
This represents an internal error that usually does not occur.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_BADSHC This specification of stacked shared caches is not supported.
$nexp FDC_ERR_BADSHC
This specification of stacked shared caches is not supported.
You have specified 2 or more files that are using stacked shared caches.
Stacked shared caches are supported on \s-1UNICOS\s+1 systems, but in
multitasked programs different files must not mix the order of the
shared caches.
.PP
For example, the following combination of specifications is supported:
\*F assign -F cachea::::1,cachea::::2 u:1\ \fR 
\*F assign -F cachea::::1,cachea::::2 u:2\ \fR
\*F assign -F cachea::::2 u:3\ \fR
\*F assign -F cachea::::2,cachea::::3 u:4\ \fR
.PP
This combination of specifications is not supported, however:
\*F assign -F cachea::::1,cachea::::2 u:1\ \fR 
\*F assign -F cachea::::2,cachea::::1 u:2\ \fR
.PP
Change the \s-1FFIO\s+1 specification for the file.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_NOSTCA Stacked shared caches are not supported.
$nexp FDC_ERR_NOSTCA
Stacked shared caches are not supported on \s-1IRIX\s+1 systems.
.PP
An attempt was made to open a file using more than one shared cache
(for example, \*F assign -F cachea::::1,cachea::::2\ \fR).
This is unsupported on \s-1IRIX\s+1 systems.
.PP
Change the \s-1FFIO\s+1 specification for the file.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_EOVDIS End of volume processing is not enabled.
$nexp FDC_ERR_EOVDIS
End of volume processing is not enabled.
.PP
End of volume processing must be enabled before this operation
can take place. See the man pages for \*FSETSP\fR or \*Ffffcntl\fR.
.PP
.Ma UNRECOVERABLE
.ME
$ -------|---------|---------|---------|---------|---------|---------|---------|
$msg FDC_ERR_EOVALOW This operation is not allowed while in special eov processing.
$nexp FDC_ERR_EOVALOW
This operation is not allowed while in special eov processing.
.PP
Special end-of-volume processing is currently in effect. This
operation cannot be performed while in special end-of-volume
processing. 
.PP
.Ma UNRECOVERABLE
.ME
