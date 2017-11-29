/*
Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.

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
*/

#ifdef KEY /* Bug 8290 */

#ifdef _UNICOS

extern _f_int _DATE_(_fcd dayofyear);
extern _f_int _JDATE_(_fcd julianday);

#else /* _UNICOS */

extern _fcd _DATE_(void	*dayofyear, int	iffcd);
extern _fcd _JDATE_(void *julianday, int iffcd);

#endif /* _UNICOS */

#endif /* KEY Bug 8290 */
