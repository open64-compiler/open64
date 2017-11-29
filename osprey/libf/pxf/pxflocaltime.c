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


#pragma ident "@(#) libf/pxf/pxflocaltime.c	92.2	10/29/99 21:41:49"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <time.h>
#include <limits.h> /* for TZNAME_MAX */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef _SOLARIS
#include <string.h>
#endif

#define POSIX_BASE_YEAR 1900 /* An offset to convert the tm_year component to
			      * to correspond to the Posix 1003.9-1992
			      * specification for the year.
			      */
#define MONTH_OFFSET 1       /* An offset to make the month element correspond
			      * to the Posix 1003.9-1992 specification for the
			      * month.
			      */
#define TIME_ZONE "TZ"
#define PLUSMINUS 1

/*
 *  PXFLOCALTIME  -- convert to local time
 *  (section 8.1.1 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *     SUBROUTINE PXFLOCALTIME(ISECNDS,IATIME,IERROR)
 *     INTEGER ISECNDS,IATIME(9),IERROR
 *
 *  Where:
 *
 *  ISECNDS  is an input integer variable containing the number of
 *           seconds since 00:00:00 UTC, January 1, 1970, the epoch.
 *
 *  IATIME   is an output integer array with the elements:
 *
 *              IATIME(1) = seconds (0-61)
 *              IATIME(2) = minutes (0-59)
 *              IATIME(3) = hours (0-23)
 *              IATIME(4) = day of the month (1-31)
 *              IATIME(5) = month of the year (1-12)
 *              IATIME(6) = Gregorian year (e.g., 1995)
 *              IATIME(7) = Day of the week (0 = Sunday)
 *              IATIME(8) = Day of the year (1-366)
 *              IATIME(9) = Daylight savings flag
 *                          (0 = standard, nonzero = daylight savings)
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFLOCALTIME was successful.
 *
 *            nonzero - PXFLOCALTIME was unsuccessful.
 *
 *           PXFLOCALTIME may return any of the following
 *           error values:
 *
 *            EINVAL If the current value of the TZ environement
 *                   variable is invalid.
 */


#ifdef _UNICOS
void
PXFLOCALTIME(
#else
void
_PXFLOCALTIME(
#endif
	     _f_int *ISECNDS,
	     _f_int *IATIME,
	     _f_int *IERROR
)
{
  int i, itemp;
  time_t cisecnds;
  struct tm *tmstruct;
  char *tzstr;

  cisecnds = *ISECNDS;
  *IERROR = 0;
  
  /* find TZ variable */
  if ((tzstr = getenv(TIME_ZONE)) != NULL) {

#if defined(_UNICOS) || defined(_MIPSEB)

    /* check TZ environment variable for proper format according to
     * section 8.1.1 of Posix 1003.1-1990. */

    if (tzstr[0] != ':') {
      /* check TZ environment variable since it's not implementation defined */

      i = 0;
      /* std part */
      if (is_stddst(tzstr, &i)) {
	/* check std offset part */
	if (is_offset(tzstr, &i, PLUSMINUS)) {
	  if(tzstr[i] != '\0') {
	    /* check dst part */
	    if (is_stddst(tzstr, &i)) {
	      if (tzstr[i] != '\0') {
		/* check dst offset */
		itemp = i;
		if (is_offset(tzstr, &itemp, PLUSMINUS)) {
		  i = itemp;
		  if (tzstr[i] != '\0') {
		   /* check rule */
		    if (!is_rule(tzstr, &i)) {
		      *IERROR = EINVAL;
		      return;
		    }
		  }
		} else {
		  /* check rule */
		  if (!is_rule(tzstr, &i)) {
		    *IERROR = EINVAL;
		    return;
		  }
		}
	      }
	    } else {
	      *IERROR = EINVAL;
	      return;
	    }
	  }
	} else {
	  *IERROR = EINVAL;
	  return;
	}
      } else {
	*IERROR = EINVAL;
	return;
      }
    }
 

#else
    /* for Solaris, which doesn't use the Posix definition for TZ.
     * Solaris' TZ environment variable format appears to be
     * Two-Letter-Country-Abbreviation/Region */

    if (((int)strlen(tzstr) < 4) || !isalpha(tzstr[0])
	|| !isalpha(tzstr[1]) || tzstr[2] != '/') {
      *IERROR = EINVAL;
      return;
    }
      

#endif

  }

  tmstruct = localtime(&cisecnds);

  IATIME[0] = tmstruct->tm_sec;
  IATIME[1] = tmstruct->tm_min;
  IATIME[2] = tmstruct->tm_hour;
  IATIME[3] = tmstruct->tm_mday;
  IATIME[4] = tmstruct->tm_mon + MONTH_OFFSET;
  IATIME[5] = tmstruct->tm_year + POSIX_BASE_YEAR;
  IATIME[6] = tmstruct->tm_wday;
  IATIME[7] = tmstruct->tm_yday;
  IATIME[8] = tmstruct->tm_isdst;

}


#ifndef _UNICOS
void
pxflocaltime_(
	     _f_int *ISECNDS,
	     _f_int *IATIME,
	     _f_int *IERROR
)
{
  _PXFLOCALTIME(ISECNDS,IATIME,IERROR);
}
#endif


#if defined(_UNICOS) || defined(_MIPSEB)

/* check std part of the TZ environment variable string
 *
 * arguments:
 *  str (in)      TZ environment variable string to be checked
 *  pos (in/out)  index for the starting point of the check. On a return with any
 *                errors detected, it contains the index to the next character
 *                after the valid std part.
 *
 * return values:
 *  1 if a std compliant part is found starting at position pos
 *  0 if a std compliant part is not found starting at position pos
 *
 * pseduo-BNF for the std part:
 *  std             ::= <start-character><character><additional>
 *  character       ::= any character except digits, the comma (,), the minus (-),
 *                      the plus (+), and the null character
 *  start-character ::= <character> except colon
 *  additional      ::= <character> | <character><additional>
 *
 * NOTE: The limit of recursion for the <additional> rule is TZNAME_MAX - 3.
 *
 */
static
int
is_stddst(
	     const char *str,
	     int *pos
)
{
  int i, original;
  char c;
  
  i = *pos;

  if (str[i] != ':') {
    for(original = i, c = str[i];
	i - original < TZNAME_MAX && c != '\0' && !isdigit(c) && c != ','
	  && c != '-' && c != '+';) {
      c = str[++i];
    }
  }

  *pos = i;
 
  return (i - original < TZNAME_MAX && i - original >= _POSIX_TZNAME_MAX);
}


/* check offset part of TZ environment variable string
 *
 * arguments:
 *  str (in)       TZ environment variable string to be checked
 *  pos (in/out)   index for the starting point of the check. On a return with any
 *                 errors detected, it contains the index to the next character
 *                 after the valid std part.
 *  plusminus (in) used to determine of leading plus or minus check should be
 *                 performed.
 *
 * return values:
 *  1 if a offset compliant part is found starting at position pos
 *  0 if a offset compliant part is not found starting at position pos
 *
 * pseduo-BNF for the offset part:
 *  offset  ::= ["+"|"-"]<time>
 *  time    ::= <hour>[":"<minutes>[":"<seconds>]]
 *  hour    ::= ["1"|"2"]<digit>
 *  minutes ::= <0-6><digit>
 *  seconds ::= <0-6><digit>
 *  0-6     ::= "0" | "1" | "2" | "3" | "4" | "5" | "6"
 *  digit   ::= <0-6> | "7" | "8" | "9"
 *
 */
static
int
is_offset(
	  const char *str,
	  int *pos,
	  int plusminus
)
{
  int i, j, original, val;
  char c, temp[3];

  i = *pos;
  c = str[i];
  temp[1] = '\0';
  temp[2] = '\0';

  /* check for leading '+' or '-' */
  if (plusminus && (c == '+' || c == '-')) {
    c = str[++i];
  }

  /* check hour field */
  for (j = 0;
       (j <= 2) && c != '\0' && isdigit(c);) {
    temp[j++] = c;
    c = str[++i];
  }
  val = atoi(temp);
  if (j <= 2 && j != 0 && val >= 0 && val <= 23) {
    if (c == ':') {
      /* check minutes field */
      if (str[i+1] != '\0' && isdigit(str[i+1]) &&
	  str[i+2] != '\0' && isdigit(str[i+2])) {
	temp[0] = str[i+1];
	temp[1] = str[i+2];
	val = atoi(temp);
	if (val >= 0 && val <= 60) {
	  i += 3;
	  if (str[i] == ':') {
	    /* check seconds field */
	    if (str[i+1] != '\0' && isdigit(str[i+1]) &&
		str[i+2] != '\0' && isdigit(str[i+2])) {
	      temp[0] = str[i+1];
	      temp[1] = str[i+2];
	      val = atoi(temp);
	      if (val >= 0 && val <= 60) {
		i += 3;	  
	      } else {
		return 0; /* bad seconds field -- out of range */
	      }
	    } else {
	      return 0; /* bad seconds field */
	    }
	  }
	} else {
	  return 0; /* bad minutes field -- out of range */
	}
      } else {
	return 0; /* bad minutes field */
      }
    }
  } else {
    return 0; /* bad hours field */
  }

  *pos = i;
  return 1;
}


/* check rule part of TZ environment variable string
 *
 * arguments:
 *  str (in)      TZ environment variable string to be checked
 *  pos (in/out)  index for the starting point of the check. On a return with any
 *                errors detected, it contains the index to the next character
 *                after the valid std part.
 *
 * return values:
 *  1 if a rule compliant part is found starting at position pos
 *  0 if a rule compliant part is not found starting at position pos
 *
 * pseduo-BNF for the rule part:
 *  part   ::= ","<date>["/"<offset>]<date>["/"<offset>]
 *  date   ::= (see is_date() function)
 *  offset ::= (see is_offset() function. same except leading plus or minus)
 *
 */
static
int
is_rule(
	const char *str,
	int *pos
)
{
  int i;

  i = *pos;

  if (str[i++] == ',') {
    if (is_date(str, &i)) {
      if (str[i] == '/') {
	i++;
	if (!is_offset(str, &i, !PLUSMINUS)) {
	  return 0;
	}
      }
      if (str[i++] == ',') {
	if (is_date(str, &i)) {
	  if (str[i] == '/') {
	    i++;
	    if (is_offset(str, &i, !PLUSMINUS)) {
	      if (str[i] == '\0') {
		*pos = i;
		return 1;
	      }
	    }
	  } else if (str[i] == '\0') {
	    *pos = i;
	    return 1;
	  }
	}
      }
    }
  }

  return 0;
}


/* check date subpart of TZ environment variable string
 *
 * arguments:
 *  str (in)      TZ environment variable string to be checked
 *  pos (in/out)  index for the starting point of the check. On a return with any
 *                errors detected, it contains the index to the next character
 *                after the valid std part.
 *
 * return values:
 *  1 if a offset compliant part is found starting at position pos
 *  0 if a offset compliant part is not found starting at position pos
 *
 * pseduo-BNF for the rule part:
 *  rule              ::= <julian> | <zero-based julian> | <month-week-day>
 *  julian            ::= "J"<1-365>
 *  1-365             ::= 1 through 365 using character represenatation
 *  zero-based julian ::= <0-365>
 *  0-365             ::= "0" | <1-365>
 *  month-week-day    ::= "M"<month>"."<week>"."<day>
 *  month             ::= 1 through 12 using character representation
 *  week              ::= 1 through 5 using character represenatation
 *  day               ::= 0 through 6 using character represenatation
 *
 */
static
int
is_date(
	char *str,
	int *pos
)
{
  int i, j, day, val, is_julian;
  char temp[4];

  is_julian = 0;
  i = *pos;

  switch (str[i]) {
  case 'M': /* explicit day/week/month */
    i++;
    /* check month */
    for (j = 0; str[i+j] != '\0' && isdigit(str[i+j]); j++) {
      temp[j] = str[i+j];
    }
    if (j > 0) {
      temp[j] = '\0';
      val = atoi(temp);
      if (val >= 1 && val <= 12) {
	i += j;
	if (str[i++] == '.') {
	  /* check week */
	  temp[0] = str[i++];
	  temp[1] = '\0';
	  val = atoi(temp);
	  if (val >= 1 && val <= 5 && str[i++] == '.') {
	    /* check day */
	    temp[0] = str[i++];
	    val = atoi(temp);
	    if (val >= 0 && val <= 6) {
	      *pos = i;
	      return 1;
	    }
	  }
	}
      }
    }
    
    break;

  case 'J': /* Julian day, i.e. 365 day year */
    is_julian = 1;
    i++;

  default: /* zero-based Julian day, i.e. allows leap days */
    for (j = 0; j < 3 && str[i+j] != '\0' && isdigit(str[i+j]); j++) {
      temp[j] = str[i+j];
    }
    if (j > 0) {
      temp[j] = '\0';
      day = atoi(temp);
      if (day <= 365 &&
	  ((isdigit(str[i]) && day >= 0) || (is_julian && day >= 1))) {
	*pos = i + j;
	return 1;
      }
    }
  }
  return 0;
}

#endif
