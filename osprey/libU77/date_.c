/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

#include <time.h>
#include <string.h>
#include <stdio.h>

/*
 * For g77 compatibility, take string from ctime(3), which is assumed to
 * have the form:
 *	Sun Nov  7 21:24:03 PST 2004
 * and reformat it to:
 *	 7-Nov-04
 * (Note blank in front of 7.) This seems fragile, but it's what the g77
 * code, which was generated automatically by f2c, does.
 */
void
pathf90_date(char *buf, int len)
{
# define MONTH_INDEX	4
# define DAY_INDEX	8
# define OUTP_LEN	9
  char inp[80], outp[OUTP_LEN + 1 /* null terminator */];
  time_t t = time(0);
  ctime_r(&t, inp);
  char *year = inp + strlen(inp) - 3 /* newline and 2 digits */;
  inp[MONTH_INDEX + 3] = 0;
  inp[DAY_INDEX + 2] = 0;
  sprintf(outp, "%s-%s-%s",
    inp + DAY_INDEX,
    inp + MONTH_INDEX,
    year);
  memset(buf, ' ', len);
  memcpy(buf, outp, (len < OUTP_LEN) ? len : OUTP_LEN);
}
