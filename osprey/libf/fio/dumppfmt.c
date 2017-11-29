/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/dumppfmt.c	92.1	06/18/99 10:21:14"
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <cray/format.h>
#include <cray/portdefs.h>

void
_dumppfmt(
	fmt_type	*parsed_format
)
{
	register int	index;
	int64		*ptr;
	fmt_type	ent;

	if (parsed_format == NULL)
		(void) printf("     Parsed format pointer is NULL\n");
	else {

		ptr	= (int64 *) parsed_format;
		ent	= *(fmt_type *) ptr;
		index	= 0;

		(void) printf(" %03o %01o %08o  %02o %01o %08o     0: Header word\n",
			ent.op_code, ent.default_digits, ent.digits_field,
			ent.exponent, ent.reserved2, ent.field_width);
		(void) printf(" %1o %05o %06o    %011o        Parser level = %d, Max depth = %d\n",
			ent.rgcdedf, ent.reserved3, ent.offset, ent.rep_count & 037777777777,
			ent.offset, ent.rep_count & 037777777777);

		for ( ; ; ) {
			ptr	= ptr + FMT_ENTRY_WORD_SIZE;
			ent	= *(fmt_type *) ptr;
			index	= index + 1;

			(void) printf(" %03o", ent.op_code);
			(void) printf(" %01o", ent.default_digits);
			(void) printf(" %08o", ent.digits_field);
			(void) printf("  %02o", ent.exponent);
			(void) printf(" %01o", ent.reserved2);
			(void) printf(" %08o", ent.field_width);
			(void) printf("    %2d: ", index);

			switch (ent.op_code) {

				case A_ED:
					(void) printf("%d A%d", ent.rep_count,
						ent.field_width);
					break;

				case B_ED:
					(void) printf("%d B%d.%d", ent.rep_count,
						ent.field_width,
						ent.digits_field);
					break;

				case D_ED:
					(void) printf("%d D%d.%de%d", ent.rep_count,
						ent.field_width,
						ent.digits_field,
						ent.exponent);
					break;

				case E_ED:
					(void) printf("%d E%d.%de%d", ent.rep_count,
						ent.field_width,
						ent.digits_field,
						ent.exponent);
					break;

				case EN_ED:
					(void) printf("%d EN%d.%de%d", ent.rep_count,
						ent.field_width,
						ent.digits_field,
						ent.exponent);
					break;

				case ES_ED:
					(void) printf("%d ES%d.%de%d", ent.rep_count,
						ent.field_width,
						ent.digits_field,
						ent.exponent);
					break;

				case F_ED:
					(void) printf("%d F%d.%d", ent.rep_count,
						ent.field_width,
						ent.digits_field);
					break;

				case G_ED:
					(void) printf("%d G%d.%de%d", ent.rep_count,
						ent.field_width,
						ent.digits_field,
						ent.exponent);
					break;

				case I_ED:
					(void) printf("%d I%d.%d", ent.rep_count,
						ent.field_width,
						ent.digits_field);
					break;

				case L_ED:
					(void) printf("%d L%d", ent.rep_count,
						ent.field_width);
					break;

				case O_ED:
					(void) printf("%d O%d.%d", ent.rep_count,
						ent.field_width,
						ent.digits_field);
					break;

				case R_ED:
					(void) printf("%d R%d", ent.rep_count,
						ent.field_width);
					break;

				case Z_ED:
					(void) printf("%d Z%d.%d", ent.rep_count,
						ent.field_width,
						ent.digits_field);
					break;

				case SLASH_ED:
					(void) printf("%d /%d", ent.rep_count,
						ent.field_width);
					break;

				case P_ED:
					(void) printf("%dP", ent.rep_count);
					break;

				case Q_ED:
					(void) printf("%dQ", ent.rep_count);
					break;

				case STRING_ED:
					(void) printf("%d STRING %d", ent.rep_count,
						ent.field_width);
					break;

				case BN_ED:
					(void) printf("%d BN", ent.rep_count);
					break;

				case BZ_ED:
					(void) printf("%d BZ", ent.rep_count);
					break;

				case COLON_ED:
					(void) printf("%d :", ent.rep_count);
					break;

				case S_ED:
					(void) printf("%d S", ent.rep_count);
					break;

				case SP_ED:
					(void) printf("%d SP", ent.rep_count);
					break;

				case SS_ED:
					(void) printf("%d SS", ent.rep_count);
					break;

				case T_ED:
					(void) printf("%d T%d", ent.rep_count,
						ent.field_width);
					break;

				case TL_ED:
					(void) printf("%d TL%d", ent.rep_count,
						ent.field_width);
					break;

				case TR_ED:
					(void) printf("%d TR%d", ent.rep_count,
						ent.field_width);
					break;

				case DOLLAR_ED:
					(void) printf("%d $", ent.rep_count);
					break;

				case REPEAT_OP:
					(void) printf("REPEAT %d", ent.rep_count);
					break;

				case ENDREP_OP:
					(void) printf("ENDREPEAT %d", ent.rep_count);
					break;

				case REVERT_OP:
					(void) printf("REVERT %d", ent.rep_count);
					if (!(ent.rgcdedf))
						(void) printf(" (no data edit-descriptors in revert group)");
					break;

				default:
					(void) printf("ERROR - Unknown Op Code");
					break;

			} /* switch */

			if (ent.default_digits)
				(void) printf(" (default digits field)");
			else if (ent.op_code < Q_ED && ent.op_code > A_ED &&
				 ent.op_code != L_ED && ent.op_code != R_ED)
				(void) printf(" (user specified digits field)");
			(void) printf("\n");
			(void) printf(" %01o", ent.rgcdedf);
			(void) printf(" %05o", ent.reserved3);
			(void) printf(" %06o", ent.offset);
			(void) printf("    %011o",ent.rep_count & 037777777777);
			(void) printf("        (character position %d in format)\n", ent.offset);

			if (ent.op_code == REVERT_OP)
				break;

			if (ent.op_code == STRING_ED) {
				register int	j;

				ptr	= ptr + FMT_ENTRY_WORD_SIZE;
				index	= index + 1;

				(void) printf("        %022llo        '%.*s'\n", *ptr,
					ent.field_width, (char *) ptr);
				(void) printf("        %022llo\n", *(ptr + 1));

				for (j = 0; j < ((ent.field_width - 1)/FMT_ENTRY_BYTE_SIZE); j++) {
					ptr	= ptr + FMT_ENTRY_WORD_SIZE;
					index	= index + 1;
					(void) printf("        %022llo\n", *ptr);
					(void) printf("        %022llo\n", *(ptr + 1));
				}
			}
		} /* for */
	}

	return;
}
