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


int
NAMEf_dfnf (struct TYPEdfnf_struct *b)
{
	static ftnint i = 0;
	return(NAMEf_dfnf1(b, &i));
}

int
NAMEf_dfnf1 (struct TYPEdfnf_struct *b, ftnint *mask)
{
   char            nbuf[10];
   TYPEolist           a;
   int n, rlflag;

#if SIZEOF_LUNO_IS_64
   (void) sprintf (nbuf, "fort.%lld", b->unit);
#else
   (void) sprintf (nbuf, "fort.%d", b->unit);
#endif
   a.oerr = 0;
   a.ounit = b->unit;
   a.ofnm = nbuf;
   a.ofnmlen = (int) strlen (nbuf);
   a.osta = NULL;
   a.oacc = "d";
   a.oorg = "r";
   a.ofm = "u";
   a.occ = "n";
   a.orl = b->recl << 1;
   a.oblnk = NULL;
   a.oassocv = b->assocv;
   a.odisp = NULL;
   a.omaxrec = b->maxrec;
   a.orectype = NULL;
   a.odfnm = NULL;
/* 
 * Fix BN 11785 
 * Set the readonly switch to zero.
 * ---ravi---2/20/92
 */
   a.oreadonly = 0;
   /* Make sure that the OLD_RL flag is turned on so that the length
   will be interpreted as number of bytes */
   rlflag = f77vms_flag_[OLD_RL];
   f77vms_flag_[OLD_RL] = 1;
   n = f_open1 (&a, mask);
   f77vms_flag_[OLD_RL] = (unsigned short) rlflag;
   return(n);
}
