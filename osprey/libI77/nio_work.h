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


/* function bodies - included in nio.c */

static int NAMEfindit(unit *ftnunit, char *key, TYPENlentry *list, TYPENlentry **result);
static int NAMEgetvar(unit *ftnunit, TYPENlentry *pnlent, int use_match_type);
static int
NAMEs_wsNe_work (TYPEcilist *pnlarg, unit **fu, int use_match_type);


static int variable_name_len; /* The length of the namelist key just seen,
				 for better error handling */



/* s_rsNe_work is function that does the work of rsne and rsNe, has parameter
   ito decide whether to call s_wsne or s_wsNe, and another to decide whether
   to use match_type in getvar.  Fortran 90 differs from Fortran 77.i
   Dont know why. */

static int
NAMEs_rsNe_work(TYPEcilist *pnlarg, unit **fu, int call_wsne, int use_match_type)
    /*
    **  pnlarg = the namelist argument
    **  call_wsne = is set for I90
    **  use_match_type = is set for 2.20 backward compatibility
    */
{
   TYPENamelist       *pnl;
   TYPENlentry        *pnlent;
   ftnint             lunit;
   int		ch, n, e;
   char            buf[BSIZ];
   unit		  *ftnunit;
   unit		  **tmpfu;

   if (!f77init)
      f_init ();
   if (n = c_nle (pnlarg, fu)) {
      return (n);
   }
   ftnunit = *fu;
#ifdef I90
   ftnunit->f90sw = 0;
#endif
   ftnunit->uacc = SEQUENTIAL;
   ftnunit->f77getn = t_getc;
   ftnunit->f77gets = t_gets;
   ftnunit->f77ungetn = t_ungetc;
#ifdef I90
	if (ftnunit->uaction == WRITEONLY ) 
	   errret(ftnunit->f77errlist.cierr,180,"startread");
#endif
   ftnunit->lquit = 0;
   ftnunit->lcount = 0;
   (void) f77nowreading (ftnunit);
   pnl = (TYPENamelist *) pnlarg->cinml;
reread:
   while (GETC (ch) != ' ' || (GETC (ch) != '$' && ch != '&'))
      for (UNGETC (ch); GETC (ch) != '\n';)
/* If end of file is incured then go to the END label instead of the the
 * ERR label.
 *
 * ---ravi---   12/6/91
 *
 */
	 if (ch == EOF)
	    errret(pnlarg->ciend ? pnlarg->ciend : pnlarg->cierr
		 ,pnlarg->ciend ? EOF : 170, nlrs);

  /* In the call to getword below, the fourth argument is zero, because 
     we may be at a line that has just a '$' and white spaces and a 
     newline, and we want to detect this as the termination of a namelist;
     By setting the last argument to zero, getword won't read past the
     newline character in such a case: PV 272510 */

   if (strcasecmp (pnl->nlname, getword (ftnunit, buf, BSIZ - 1, 0, 1)))
/* This gives an error if the first namelist name read does not match the
 * namelist being read. Instead loop back and start reading the next line
 * from the input. Continue to do this until an EOF or the desired namelist
 * is found 
 * This also fixes BN 8456
 * 
 * ---ravi---   12/6/91
 */
      goto reread;
   ftnunit->f77errlist.cierr = pnlarg->cierr;
   ftnunit->f77errlist.ciend = pnlarg->ciend;
   ftnunit->f77errlist.cieor = pnlarg->cieor;
   ftnunit->f77errlist.cisize = pnlarg->cisize;
   ftnunit->f77errlist.iciunit = 0;
loop:
   if (!getword (ftnunit, buf, BSIZ - 1, 1, 0))
      errret(pnlarg->cierr, 120, nlrs);
/* Fix BN 11233. If a '$' or '&' is encountered that means that this is the
 * end of the namelist, so flush the rest of the line by reading till a
 * '\n' is encountered.
 * ---ravi---12/30/91
 */
/* Must also test for EOF so we don't loop forever (bug 258538) */
/*   R. Shapiro, 4/19/95 */	   

   if (*buf == '$' || *buf == '&') {
      while (GETC (ch) != '\n' && ch != EOF);
      ftnunit->lock_unit = 0;
      return NULL;
   }
   /* If the letter following a successful namelist-directed READ is a '?'
      then display the group name and the current values of the items
      in that group.  Since this involves switching the logical unit
      from the current logical unit to logical unit 6 it won't create
      a deadlock.  
   */
   if (*buf == '?') {
      lunit = pnlarg->ciunit;
      pnlarg->ciunit = 6;
      if (call_wsne)
         s_wsne64_mp (pnlarg, tmpfu);
      else
         NAMEs_wsNe_work (pnlarg, tmpfu, 0);
      pnlarg->ciunit = lunit;
      /*
      Don't need to do this anymore since the logical unit pointed to
      by 'fu' still retained its integrity after the display to logical
      unit 6.  Executing this call to c_nle() would cause a deadlock
      when locking is turned on.
      if (n = c_nle (pnlarg, fu)) {
	 return n;
      }
      */
      goto loop;
   }
   if (e = NAMEfindit (ftnunit, buf, pnl->nlvnames, &pnlent)) {
      char * emsg;
      /* Note that this will leak memory in the case where the 
	 user has an error handler */
      emsg = (char *) malloc(variable_name_len + strlen(nlrs) + 4);
      if (emsg) {
	 strcpy(emsg,nlrs);
	 strcat(emsg," (");
	 strncat(emsg,buf,variable_name_len);
	 strcat(emsg,")");
	 errret(pnlarg->cierr, e, emsg);
      } else {      
	 errret(pnlarg->cierr, e, nlrs);
      }
   }
#ifdef I90
   if (NAMEgetvar (ftnunit, pnlent, use_match_type))
#else
   if (NAMEgetvar (ftnunit, pnlent, 0))	/* fortran 77 always use_match_type == 0 */
				/* we can safely remove this arm of the ifdef
				   and the ifdef test.  This is really here
				   for illustration purposes. */
#endif
      errret(pnlarg->cierr, 120, nlrs);
   ftnunit->subscript = 0;
   ftnunit->suboffset = 0;
   goto loop;
}

/* findit - find key in list of Nlentrys */

static int NAMEfindit(unit *ftnunit, char *key, TYPENlentry *list, TYPENlentry **result)
{
   register int    i, k, lp, rp, ten, n = 0;

   while (key[++n] && key[n] != ' ' && key[n] != '\t' && key[n] != '(');
   variable_name_len = n;

   while (strlen (list->varname)) {
      if (strncasecmp (key, list->varname, n) ||
	  (list->varname[n] && list->varname[n] != '('))
	 list++;
      else {
	 while (key[n] && key[n++] != '(');
	 if (key[n])
	    lp = n - 1;
	 else
	    goto ret;
	 while (key[n] && key[n] != ':' && key[n] != ',' && key[n] != ')')
	    n++;
	 switch (key[n]) {
	 case ':':
      charray:n = lp;
	    while (isspace (key[++n]));
	    for (ftnunit->substr_lb = 0; isdigit (key[n]); n++)
	       ftnunit->substr_lb = ftnunit->substr_lb * 10 + key[n] - '0';
	    while (isspace (key[n]))
	       n++;
	    if (key[n++] != ':')
	       return 115;
	    while (isspace (key[n]))
	       n++;
	    if (key[n] != ')')
	       for (ftnunit->substr_ub = 0; isdigit (key[n]); n++)
		  ftnunit->substr_ub = ftnunit->substr_ub * 10 + key[n] - '0';
	    break;
	 case ',':
	    while (key[++n] && key[n] != ')');
	    if (!key[n])
	       return 115;
	 case ')':
	    if (list->dimp && (k = list->dimp->ndims)) {
	       rp = n;
	       ftnunit->subscript = 1;
	       while (k-- && n > lp) {
		  while (isspace (key[--n]));
		  for (i = 0, ten = 1; isdigit (key[n]); ten *= 10, n--)
		     i += ten * (key[n] - '0');
		  if (i > list->dimp->span[k])
		     return 121;
		  ftnunit->suboffset = i + list->dimp->span[k] * ftnunit->suboffset;
		  while (isspace (key[n--]));
		  if (key[++n] == '(')
		     break;
		  if (key[n] != ',')
		     return 115;
	       }
	       ftnunit->suboffset -= list->dimp->baseoff;
	       n = rp;
	       while (key[n] && key[n++] != '(');
	       if (key[n - 1] == '(') {
		  lp = n - 1;
		  while (key[n] && key[n++] != ':');
		  if (key[--n] == ':')
		     goto charray;
		  else
		     return 115;
	       }
	    }
	    break;
	 default:
	    return 115;
	 }
   ret: *result = list;
	 return NULL;
      }
   }
   return 119;
}


/* getvar - read values for namelist io
 *
 * use_match_type is a flag that changes the variable type,
 * vtype, from the old type enumeration by the match_type array located
 * in lio.h.  Used by Fortran 90.
 *
 * getvar uses l_read of list io to do all the dirty work, therefore
 * it should be inserted into the library before lread.c (on UNIX
 * systems with barbaric topologically sorted libraries)
 *
 * It sets the cierr flag so that l_read (and its subordinates) will
 * not report errors, but pass them back so that the diagnostic message
 * will appear to come from "namelist read".
 */

static int NAMEgetvar(unit *ftnunit, TYPENlentry *pnlent, int use_match_type)
{
   int             size, vtype, char_off = 0, strsize = 0, i = 0;
   char           *p = pnlent->varaddr.pchar;
   XINT          n = 1;

   int             old_cierr = ftnunit->f77errlist.cierr;

   if (pnlent->dimp)
      n = pnlent->dimp->nels - (ftnunit->subscript ? ftnunit->suboffset : 0);

   ftnunit->f77errlist.cierr = 1;

#ifdef I90
   if (pnlent->type < 0 || !use_match_type) {
	vtype = pnlent->type;
   } else {
	vtype = match_type[pnlent->type];
   }
#else
	/* actually for Fortran 77, this insured in above case also as long
	   as you call with use_match_type==0.  We can safely remove the
	   ifdef and this arm. */
	vtype = pnlent->type;
#endif

   switch (vtype) {
   case TYADDR:
      size = sizeof (char *);
      break;
   case TYLOGICAL1:
   case TYBYTE:
      size = sizeof (char);
      break;
   case TYLOGICAL2:
   case TYSHORT:
      size = sizeof (short);
      break;
   case TYLOGICAL4:
   case TYINT:
      size = sizeof (int);
      break;
   case TYLOGICAL8:
   case TYLONGLONG:
      size = sizeof (long long);
      break;
   case TYREAL:
      size = sizeof (float);
      break;
   case TYCOMPLEX:
      size = 2 * sizeof (float);
      break;
   case TYDREAL:
      size = sizeof (double);
      break;
   case TYQUAD:
      size = 2 * sizeof (double);
      break;
   case TYDCOMPLEX:
      size = 2 * sizeof (double);
      break;
   case TYQUADCOMPLEX:
      size = 2 * sizeof (long double);
      break;
   default:
      if (vtype < 0)
	 size = -vtype;
      else {
	 ftnunit->f77errlist.cierr = 0;
	 err(ftnunit->f77errlist.cierr, 117, nlrs);
     }
   }
   p += size * ftnunit->suboffset;
   if (ftnunit->substr_lb && vtype < 0) {
      if (!ftnunit->substr_ub)
	 ftnunit->substr_ub = size;
      strsize = ftnunit->substr_ub - (char_off = ftnunit->substr_lb - 1);
      if (!*p)
	 while (i < char_off)
	    p[i++] = ' ';
      if (ftnunit->substr_lb > size || strsize > size) {
	 ftnunit->f77errlist.cierr = 0;
	 err(ftnunit->f77errlist.cierr, 121, nlrs);
     }
   }
/* LHL 5/29
 * fix bug 4573
 * tells l_read that it's for namelist read
 * by setting the 16th bit of the type parameter.
 * This will make l_read to take the same number of arguments as l_write
 * and f77lioproc in UNIT
 */

   /* Set this to 0 since there will not be any valid
      values hanging around. This fixes bug 276086 */
   ftnunit->lcount = 0;
   if (n = l_read (ftnunit, &n, (flex *)(p + char_off), (ftnlen) (strsize ? strsize : size),
		   1<<16 | (vtype < 0 ? TYCHAR : vtype))) {
       /* ftnunit->f77errlist.cierr = 0; */
       err(ftnunit->f77errlist.cierr, (int) n, nlrs);
   }
   ftnunit->f77errlist.cierr = old_cierr;
   return 0;
}



static int
NAMEs_wsNe_work (TYPEcilist *pnlarg, unit **fu, int use_match_type)
{
   TYPENamelist       *pnl;
   TYPENlentry        *pnlent;
   Pointer         ptr;
   XINT             n;
   int             vtype, indent;
   char           *pch;
   unit           *ftnunit;

   if (!f77init)
      f_init ();
   if (n = c_nle (pnlarg, fu)) {
      return (n);
   }
   ftnunit = *fu;
#ifdef I90
	ftnunit->f90sw = 0;
#endif
   f77nowwriting( ftnunit );
   if (ftnunit->ualias->ucc == CC_FORTRAN && ftnunit->ualias->ucchar) {
      putc (ftnunit->ualias->ucchar, ftnunit->ualias->ufd);
      ftnunit->ualias->ucchar = '\0';
   }
   if (ftnunit->uwrt != WR_READY && f77nowwriting (ftnunit))
      errret(pnlarg->cierr, 160, "namelist io");
   pnl = (TYPENamelist *) pnlarg->cinml;
   t_putc (ftnunit, 2, 0, " $");
   t_putc (ftnunit, (int) strlen (pnl->nlname), 0, pnl->nlname);
   t_putc (ftnunit, 1, '\n', NULL);
   pnlent = pnl->nlvnames;
   do {
      ptr = pnlent->varaddr;
      t_putc (ftnunit, 1, ' ', NULL);
      t_putc (ftnunit, (indent = (int) strlen (pnlent->varname)), 0, pnlent->varname);
      indent++;
      t_putc (ftnunit, 4, 0, "\t=  ");
      n = (pnlent->dimp ? pnlent->dimp->nels : 1);
      ftnunit->f77putn = t_putc;
#ifdef I90
	   if (pnlent->type < 0 || !use_match_type) {
		vtype = pnlent->type;
	   } else {
		vtype = match_type[pnlent->type];
	   }
#else
	vtype = pnlent->type;
#endif
      if (vtype < 0 && (pch = ptr.pchar))
	 do {
	    if ((ftnunit->f77recpos > indent) && (ftnunit->f77recpos - vtype + 2 >= LINE)) {
	       ftnunit->f77putn (ftnunit, 1, '\n', NULL);
	       ftnunit->f77recpos = 0;
	       t_putc (ftnunit, indent, ' ', NULL);
	       t_putc (ftnunit, 4, 0, "\t   ");
	    }
	    t_putc (ftnunit, 1, '\'', NULL);
	    t_putc (ftnunit, -vtype, 0, pch);
	    pch -= vtype;
	    t_putc (ftnunit, 1, '\'', NULL);
	    if (n > 1)
	       t_putc (ftnunit, 2, 0, ", ");
	 } while (--n);
      else
      do {
	 switch (vtype) {
	 case TYADDR:
#if (_MIPS_SZPTR == 64)
	    lwrt_I (ftnunit, (uinteger *)ptr.pptr++, 21, 8, indent);
#else
	    lwrt_I (ftnunit, (uinteger *)ptr.pptr++, 12, 4, indent);
#endif
	    break;
	 case TYLOGICAL1:
	 case TYBYTE:
	    lwrt_I (ftnunit, (uinteger *)ptr.pbyte++, 5, sizeof (char), indent);
	    break;
	 case TYSHORT:
	    lwrt_I (ftnunit, (uinteger *)ptr.pshort++, 7, sizeof (short), indent);
	    break;
	 case TYINT:
	    lwrt_I (ftnunit, (uinteger *)ptr.pint++, 12, sizeof (int), indent);
	    break;
	 case TYLONGLONG:
	    lwrt_I (ftnunit, (uinteger *)ptr.plonglong++, 21, sizeof (long long), indent);
	    break;
	 case TYREAL:
	    lwrt_G (ftnunit, (ufloat *)ptr.pfloat++, 15, 7, 2, sizeof (float), vtype, 0, indent);
	    break;
	 case TYDREAL:
	    lwrt_G (ftnunit, (ufloat *)ptr.pdouble++, 24, 16, 2, sizeof (double), vtype, 0, indent);
	    break;
	 case TYQUAD:
	    lwrt_G (ftnunit, (ufloat *)ptr.plongdouble++, 40, 31, 2, sizeof (long double), vtype, 0, indent);
	    break;
	 case TYCOMPLEX:
	    lwrt_C (ftnunit, (ufloat *)ptr.pfloat, (ufloat *)(ptr.pfloat + 1), 15, 7, 2,
		    2 * sizeof (float), vtype, indent);
	    ptr.pfloat += 2;
	    break;
	 case TYDCOMPLEX:
	    lwrt_C (ftnunit, (ufloat *)ptr.pdouble, (ufloat *)(ptr.pdouble + 1), 24, 16, 2,
		    2 * sizeof (double), vtype, indent);
	    ptr.pdouble += 2;
	    break;
	 case TYQUADCOMPLEX:
	    lwrt_C (ftnunit, (ufloat *)ptr.plongdouble, (ufloat *)(ptr.plongdouble + 1), 40, 31, 2,
		    2 * sizeof (long double), vtype, indent);
	    ptr.plongdouble += 2;
	    break;
	 case TYLOGICAL2:
	    lwrt_L (ftnunit, (uinteger *)ptr.pshort++, 2, sizeof (short), indent);
	    break;
	 case TYLOGICAL4:
	    lwrt_L (ftnunit, (uinteger *)ptr.pint++, 2, sizeof (int), indent);
	    break;
	 case TYLOGICAL8:
	    lwrt_L (ftnunit, (uinteger *)ptr.plonglong++, 2, sizeof (long long), indent);
	    break;
	 default:
	    errret(pnlarg->cierr, 117, "namelist io");
	 }
	 if (n > 1)
	    t_putc (ftnunit, 2, 0, ", ");
      } while (--n);
      t_putc (ftnunit, 1, '\n', NULL);
      ftnunit->f77recpos = 0;
      ++pnlent;
   } while (strlen (pnlent->varname));
   t_putc (ftnunit, 6, 0, " $end\n");
   ftnunit->lock_unit = 0;
   return (0);
}


