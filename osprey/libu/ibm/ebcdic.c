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


#pragma ident "@(#) libu/ibm/ebcdic.c	92.1	06/25/99 17:25:23"

#include <cray/portdefs.h>

/*
 * ASCII-to-EBCDIC and EBCDIC-to-ASCII character conversion
 * tables.
 *
 * These tables are used to convert characters.  The upper 5
 * (EBCDIC) or 4 (ASCII) bits of the source character selects
 * the appropriate word in the conversion table.  The lower 3
 * bits of the source character selects the byte in the word.
 * Note that the bytes are addressed "little-endian"; that is,
 * byte 0 is the low-order byte.
 */

/*
 *	EBCDIC to ASCII character conversion table
 */

uint64	_EBCDIC_TO_ASCII[32] = {
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x202B283C2E202020,
	0x2020202020202026, 0x5E3B292A24212020,
	0x2020202020202F2D, 0x3F3E5F252C7C2020,
	0x2020202020202020, 0x223D2740233A6020,
	0x6766656463626120, 0x2020202020206968,
	0x706F6E6D6C6B6A20, 0x2020202020207271,
	0x7877767574737E20, 0x20205B2020207A79,
	0x2020202020202020, 0x20205D2020202020,
	0x474645444342417B, 0x2020202020204948,
	0x504F4E4D4C4B4A7D, 0x2020202020205251,
	0x585756555453205C, 0x2020202020205A59,
	0x3736353433323130, 0x2020202020203938
};

/*
 *	EBCDIC to ASCII character conversion table, with
 *	lower-case characters folded to upper-case.
 */

uint64	_EBCDIC_TO_ASCII_UC[32] = {
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x2020202020202020,
	0x2020202020202020, 0x202B283C2E202020,
	0x2020202020202026, 0x5E3B292A24212020,
	0x2020202020202F2D, 0x3F3E5F252C7C2020,
	0x2020202020202020, 0x223D2740233A6020,
	0x4746454443424120, 0x2020202020204948,
	0x504F4E4D4C4B4A20, 0x2020202020205251,
	0x5857565554537E20, 0x20205B2020205A59,
	0x2020202020202020, 0x20205D2020202020,
	0x474645444342417B, 0x2020202020204948,
	0x504F4E4D4C4B4A7D, 0x2020202020205251,
	0x585756555453205C, 0x2020202020205A59,
	0x3736353433323130, 0x2020202020203938
};

/*
 *	ASCII to EBCDIC character conversion table
 */

uint64	_ASCII_TO_EBCDIC[16] = {
	0x4040404040404040, 0x4040404040404040,
	0x4040404040404040, 0x4040404040404040,
	0x7D506C5B7B7F5A40, 0x614B606B4E5C5D4D,
	0xF7F6F5F4F3F2F1F0, 0x6F6E7E4C5E7AF9F8,
	0xC7C6C5C4C3C2C17C, 0xD6D5D4D3D2D1C9C8,
	0xE6E5E4E3E2D9D8D7, 0x6D5FBDE0ADE9E8E7,
	0x8786858483828179, 0x9695949392918988,
	0xA6A5A4A3A2999897, 0x40A1D06AC0A9A8A7
};

/*
 *	ASCII to EBCDIC character conversion table, with
 *	lower-case characters folded to upper-case.
 */

uint64	_ASCII_TO_EBCDIC_UC[16] = {
	0x4040404040404040, 0x4040404040404040,
	0x4040404040404040, 0x4040404040404040,
	0x7D506C5B7B7F5A40, 0x614B606B4E5C5D4D,
	0xF7F6F5F4F3F2F1F0, 0x6F6E7E4C5E7AF9F8,
	0xC7C6C5C4C3C2C17C, 0xD6D5D4D3D2D1C9C8,
	0xE6E5E4E3E2D9D8D7, 0x6D5FBDE0ADE9E8E7,
	0xC7C6C5C4C3C2C179, 0xD6D5D4D3D2D1C9C8,
	0xE6E5E4E3E2D9D8D7, 0x40A1D06AC0E9E8E7
};
