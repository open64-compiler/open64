/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: em_elf.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/em_elf.cxx,v $
 *
 * Description:
 *
 * Generate the elf headers and sections for the object file.
 *
 * ====================================================================
 * ====================================================================
 */


#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <bstring.h>
#include "elf_stuff.h"
#include <elfaccess.h>
#include "libelf/libelf.h"
#include <stamp.h>
#include <alloca.h>
#include <assert.h>
#include <cmplrs/leb128.h>
#include <cmplrs/elf_interfaces.h>

#define	USE_STANDARD_TYPES 1
#include "defs.h"
#include "erlib.h"
#include "erglob.h"
#include "config.h"
#include "targ_const.h"
#include "glob.h"
#include "config.h"
#include "config_elf_targ.h"
#include "em_elf.h"
#include "targ_em_elf.h"


/* we always create the following 3 sections for all object files. */
static SCNINFO Shstrtab_Info;
static SCNINFO Strtab_Info;
static SCNINFO Symtab_Info_struct;
/* not static because it is needed in em_dwarf.c */
pSCNINFO Symtab_Info = &Symtab_Info_struct;

static pSCNINFO Comment_Scn;
static char *object_file_name;	/* remember the .o name for the .comment scn. */
 
/* assume we work on only one elf file at a time. If not, make Elf_Ptr
   a parameter to all routines that use it. 
*/
static Elf *Elf_Ptr;

pSCNINFO Options_Scn;
BOOL Sixtyfour_Bit;

/* This MAY be used by code in targ_em_elf.h
 */
static BOOL Big_Endian;


/* ====================================================================
 *
 * Increase_Data_Buffer_Size
 *
 * Increase the size of the data buffer for the section.
 *
 * NOTE: This uses realloc, which may not be the most efficient thing
 * to do here.
 *
 * WARNING/TODO: We can't allocate more than 32 bits of space for now.
 *
 * ====================================================================
 */

static void
Increase_Data_Buffer_Size ( pSCNINFO scninfo, Elf64_Xword newsize )
{
  char *newbuf;
  Elf64_Word newsize32 = newsize;

  /* If new size of buffer is zero, do nothing. */
  if (newsize == 0) return;

  /* WARNING: We can't allocate more than 32 bits of space for now. */
  if ( newsize32 != newsize ) {
    ErrMsg ( EC_Elf_Size64, newsize, "Increase_Data_Buffer_Size" ); 
  }

  if ( SCNINFO_buffer(scninfo) == NULL ) {
    newbuf = (char *) malloc ( newsize32 );
  } else {
    newbuf = (char *) realloc ( SCNINFO_buffer(scninfo), newsize32 );
  }
  if ( newbuf == NULL ) {
    ErrMsg ( EC_No_Mem, "Increase_Data_Buffer_Size" );
  }
  BZERO ( newbuf + (INTPS) SCNINFO_limit(scninfo),
	  newsize32 - (INT32) SCNINFO_limit(scninfo) );

  SCNINFO_buffer(scninfo) = newbuf;
  SCNINFO_limit(scninfo) = newsize32;
}

/* Change the current location within the section */
void
Em_Change_Section_Origin ( pSCNINFO scn, Elf64_Xword scn_ofst )
{
    SCNINFO_offset(scn) = scn_ofst;
}

/* Change the section alignment. */
void
Em_Change_Section_Alignment (pSCNINFO scn, Elf64_Word scn_align)
{
  if (SCNINFO_align(scn) < scn_align) SCNINFO_align (scn) = scn_align;
}

/* returns the index in the buffer where the new bytes are added. If bytes
   are to be added to a NOBITS section, input_buf is passed as NULL. 
*/
Elf64_Xword
Em_Add_Bytes_To_Scn (
    pSCNINFO scninfo, 
    const void *input_buf, 
    Elf64_Xword length, 
    Elf64_Word align)
{
    Elf64_Xword	index;
    Elf64_Xword	newoffset;
    Elf64_Xword	newsize;

    index = SCNINFO_offset(scninfo);
    /* roundup the size to the specified alignment */
    index = Roundup (index, align);
    if (SCNINFO_align(scninfo) < align) SCNINFO_align(scninfo) = align;

    newoffset = index + length;
    if (newoffset > SCNINFO_size(scninfo)) {
	SCNINFO_size(scninfo) = newoffset;
    }

    /* If input_buf is NULL, don't actually increase the buffer size or
     * copy anything into the buffer.
     */
    if (input_buf != NULL) {

      /* WARNING: We can't allocate more than 2GB of space for now. */
      newsize = newoffset + BUFSIZ;
      if ( (INT32)newsize != newsize ) {
	ErrMsg ( EC_Elf_Size64, newsize, "Em_Add_Bytes_To_Scn" ); 
      }

      if (newoffset > SCNINFO_limit(scninfo)) {
	Increase_Data_Buffer_Size (scninfo, newoffset + BUFSIZ);
      }
      if (length > 0) 
	memcpy(SCNINFO_buffer(scninfo) + index, input_buf, length);

    }

    SCNINFO_offset(scninfo) = newoffset;
    return index;
}

/* write out 'length' number of bytes containing zero to the section
 * 'scninfo' with alignment 'align.
 */
Elf64_Xword
Em_Add_Zeros_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word length,
    Elf64_Word align)
{
    Elf64_Xword index;
    static char *buf = NULL;
    static Elf64_Sword buflen = 0;

    if ( buflen < length ) {
      if ( buf != NULL ) free ( buf );
      buflen = length + 128;
      buf = (char *) calloc (buflen, 1);
      if (buf == NULL) 
	   ErrMsg ( EC_No_Mem, "Em_Add_Zeros_To_Scn" );
    }
    index = Em_Add_Bytes_To_Scn (scninfo, buf, length, align);
    return index;
}


/* Write out an "symbol + constant" expression at the current location
 * in the section 'scninfo'.  The 'symindex' points to the symbol and
 * 'addend' is the constant value.  If the scninfo has the
 * SHF_MIPS_MERGE attribute, then emit a .rela instead of a .rel .  It
 * is more efficient for ld to  process a rela section when merging.
 */
Elf64_Xword
Em_Add_Address_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word symindex,
    Elf64_Sxword addend,
    Elf64_Word align)
{
    Elf64_Xword index;

    if (Sixtyfour_Bit) {
	INT64 value = addend;
	Elf64_Shdr *scn_hdr64 = elf64_getshdr (SCNINFO_scnptr(scninfo));

	if (scn_hdr64->sh_flags & SHF_MERGE) {
	    index = Em_Add_Zeros_To_Scn (scninfo, sizeof(value), align);
	    Em_Add_New_Rela (symindex, R_WORD64, index, addend, scninfo);
	}
	else {
	    index = Em_Add_Bytes_To_Scn (scninfo, &value, sizeof(value), align);
	    Em_Add_New_Rel (symindex, R_WORD64, index, scninfo);
	}
    }
    else {
	INT32 value = addend;
	Elf32_Shdr *scn_hdr32 = elf32_getshdr (SCNINFO_scnptr(scninfo));

	if (scn_hdr32->sh_flags & SHF_MERGE) {
	    index = Em_Add_Zeros_To_Scn (scninfo, sizeof(value), align);
	    Em_Add_New_Rela (symindex, R_WORD32, index, addend, scninfo);
	}
	else {
	    index = Em_Add_Bytes_To_Scn (scninfo, &value, sizeof(value), align);
	    Em_Add_New_Rel (symindex, R_WORD32, index, scninfo);
	}
    }
    return index;
}

/* Write out a section displacement expression at the current location
 * in the section 'scninfo'.  The 'symindex' points to the symbol and
 * 'addend' is the constant value.  This will be similar to a Address
 * expression above, but will instead be an offset from the section.
 */
Elf64_Xword
Em_Add_Displacement_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word symindex,
    Elf64_Sxword addend,
    Elf64_Word align)
{
    Elf64_Xword index;
    INT32 value = addend;

    index = Em_Add_Bytes_To_Scn (scninfo, &value, sizeof(value), align);
    Em_Add_New_Rel (symindex, R_SCN_DISP, index, scninfo);
    return index;
}


/* check if the section name is already in the section string table. Return
   its index if it is already in. Otherwise add it to the string table 
   and return the index.
*/
static Elf64_Word
String_To_Index (pSCNINFO strinfo, const char *scn_name)
{
    Elf64_Word i;
    char *buf;

    /* index 0 always points to the empty string, so return that. */
    if (scn_name == NULL) return 0;

    buf = SCNINFO_buffer(strinfo);
    for (i = 0; i < SCNINFO_size(strinfo); i += strlen(&buf[i]) + 1) {
	if (strcmp (&buf[i], scn_name) == 0) return i;
    }
    return Em_Add_Bytes_To_Scn (strinfo, scn_name, strlen(scn_name) + 1, 1);
}

static char *
Index_To_String (pSCNINFO strinfo, Elf64_Word index)
{
  return SCNINFO_buffer(strinfo) + index;
}


/* print an error message when a libelf routine returns with an error code. */
static void 
print_elf_error (void)
{
    int err;

    err = elf_errno();
    if (err != 0) ErrMsg (EC_Elf_Error, elf_errmsg (err));
}


/* create a new section and intialize the section header. The parameters
   are sized for 64bit case. For 32bit case, they get truncated.
*/
static Elf_Scn *
Create_New_Section (
    const char *scn_name,
    Elf64_Word 	scn_type,
    Elf64_Xword	scn_flags,
    Elf64_Xword scn_entsize)
{
    Elf_Scn 	*scn_ptr;
    Elf64_Word string_index;

    if ((scn_ptr = elf_newscn(Elf_Ptr)) == 0) {
	print_elf_error ();
	assert(FALSE);
    }

    string_index = String_To_Index (&Shstrtab_Info, scn_name);
    if (Sixtyfour_Bit) 
    {
	Elf64_Shdr *scn_hdr64;

	if ((scn_hdr64 = elf64_getshdr(scn_ptr)) == 0) {
	    print_elf_error ();
	    assert(FALSE);
	}
	scn_hdr64->sh_name = string_index;
	scn_hdr64->sh_type = scn_type;
	scn_hdr64->sh_flags = scn_flags;
	scn_hdr64->sh_addr = 0;
	scn_hdr64->sh_entsize = scn_entsize;
    }
    else {
	Elf32_Shdr *scn_hdr32;

	if ((scn_hdr32 = elf32_getshdr(scn_ptr)) == 0) {
	    print_elf_error ();
	    assert(FALSE);
	}
	scn_hdr32->sh_name = string_index;
	scn_hdr32->sh_type = scn_type;
	scn_hdr32->sh_flags = scn_flags;
	scn_hdr32->sh_addr = 0;
	scn_hdr32->sh_entsize = scn_entsize;
    }
    return scn_ptr;
}

/* Set the sh_link field of a section. */
void
Em_Set_sh_link (pSCNINFO scn, Elf64_Word link)
{
    if (Sixtyfour_Bit) {
	Elf64_Shdr *sh64;
	sh64 = elf64_getshdr (SCNINFO_scnptr(scn));
	sh64->sh_link = link;
    }
    else {
	Elf32_Shdr *sh32;
	sh32 = elf32_getshdr (SCNINFO_scnptr(scn));
	sh32->sh_link = link;
    }
}

/* Set the sh_info field of a section. */
void
Em_Set_sh_info (pSCNINFO scn, Elf64_Word info)
{
    if (Sixtyfour_Bit) {
	Elf64_Shdr *sh64;
	sh64 = elf64_getshdr (SCNINFO_scnptr(scn));
	sh64->sh_info = info;
    }
    else {
	Elf32_Shdr *sh32;
	sh32 = elf32_getshdr (SCNINFO_scnptr(scn));
	sh32->sh_info = info;
    }
}


/* For a given section 'scninfo', this routine writes out the current 
   data buffer to a Elf_Data structure. The fields in the scninfo 
   are reset to allow for a new data buffer.
*/
static void Update_Data (pSCNINFO scninfo, Elf_Type type)
{
    Elf_Data *secdata;

    /* If there is nothing to write out then return. */
    if (SCNINFO_size(scninfo) == 0) return;

    if ((secdata = elf_newdata (SCNINFO_scnptr(scninfo))) == 0) {
	print_elf_error ();
    }

    secdata->d_buf = SCNINFO_buffer(scninfo);
    secdata->d_size = SCNINFO_size(scninfo);
    secdata->d_type = type;
    secdata->d_align = SCNINFO_align(scninfo);
    /* reset the fields in the scninfo for a new data buffer. */
    SCNINFO_buffer(scninfo) = NULL;
    SCNINFO_size(scninfo) = 0;
    SCNINFO_limit(scninfo) = 0;
    SCNINFO_align(scninfo) = 0;
}

/* For a given 'scn' return its elf section index. */
size_t
Em_Get_Section_Index (pSCNINFO scn)
{
    return SCNINFO_index (scn);
}

/* For a given 'scn' return its current offset */
Elf64_Xword 
Em_Get_Section_Offset (pSCNINFO scn)
{
  return SCNINFO_offset(scn);
}

/* return a pointer to the name of the section. */
char *
Em_Get_Section_Name (pSCNINFO scninfo)
{
    char *scnname;
    Elf64_Shdr *sh64;
    Elf32_Shdr *sh32;

    if (Sixtyfour_Bit) {
	sh64 = elf64_getshdr (SCNINFO_scnptr(scninfo));
	scnname = Shstrtab_Info.buffer + sh64->sh_name;
    }
    else {
	sh32 = elf32_getshdr (SCNINFO_scnptr(scninfo));
	scnname = Shstrtab_Info.buffer + sh32->sh_name;
    }
    return scnname;
}

Elf64_Word
Em_Get_Section_Type (pSCNINFO scninfo)
{
  if (Sixtyfour_Bit) {
    return elf64_getshdr(SCNINFO_scnptr(scninfo))->sh_type;
  }
  else {
    return elf32_getshdr(SCNINFO_scnptr(scninfo))->sh_type;
  }
}

Elf64_Word
Em_Get_Section_Flags (pSCNINFO scninfo)
{
  if (Sixtyfour_Bit) {
    return elf64_getshdr(SCNINFO_scnptr(scninfo))->sh_flags;
  }
  else {
    return elf32_getshdr(SCNINFO_scnptr(scninfo))->sh_flags;
  }
}

Elf64_Word
Em_Get_Section_Entsize (pSCNINFO scninfo)
{
  if (Sixtyfour_Bit) {
    return elf64_getshdr(SCNINFO_scnptr(scninfo))->sh_entsize;
  }
  else {
    return elf32_getshdr(SCNINFO_scnptr(scninfo))->sh_entsize;
  }
}

Elf64_Word
Em_Get_Section_Align (pSCNINFO scninfo)
{
  if (Sixtyfour_Bit) {
    return elf64_getshdr(SCNINFO_scnptr(scninfo))->sh_addralign;
  }
  else {
    return elf32_getshdr(SCNINFO_scnptr(scninfo))->sh_addralign;
  }
}

Elf64_Word
Em_Get_Section_Info (pSCNINFO scninfo)
{
  if (Sixtyfour_Bit) {
    return elf64_getshdr(SCNINFO_scnptr(scninfo))->sh_info;
  }
  else {
    return elf32_getshdr(SCNINFO_scnptr(scninfo))->sh_info;
  }
}

/* create a new relocation section for 'scninfo'. The 'is_rela' argument
   determines if we want to create a .rela or a .rel section.
*/
static void 
Create_New_Relocation_Section (BOOL is_rela, pSCNINFO scninfo)
{
    char *scnname;
    char *relname;
    pSCNINFO newscn;

    scnname = Em_Get_Section_Name (scninfo);
    relname = (char *) alloca (strlen(scnname) + 10);
    if (is_rela) {
	strcpy (relname, ".rela");
    }
    else {
	strcpy (relname, ".rel");
    }
    strcpy (relname+strlen(relname), scnname);

    if (is_rela) {
	newscn = Em_New_Section (relname, SHT_RELA, 
		    0,
		    Sixtyfour_Bit ? sizeof(Elf64_Rela) : sizeof(Elf32_Rela),
		    Sixtyfour_Bit ? ELF64_FSZ_XWORD : ELF32_FSZ_WORD);
	SCNINFO_relainfo(scninfo) = newscn; 
    }
    else {
	newscn = Em_New_Section (relname, SHT_REL, 
		    0,
		    Sixtyfour_Bit ? sizeof(Elf64_Rel) : sizeof(Elf32_Rel),
		    Sixtyfour_Bit ? ELF64_FSZ_XWORD : ELF32_FSZ_WORD);
	SCNINFO_relinfo(scninfo) = newscn;
    }

    Em_Set_sh_link (newscn, SCNINFO_index(Symtab_Info));
    Em_Set_sh_info (newscn, SCNINFO_index(scninfo));
}


/* Add a new relocation entry to the Rel section for  'scninfo'. */
void
Em_Add_New_Rel (
    Elf64_Word symindex, 
    unsigned char reltype,
    Elf64_Addr reloffset,
    pSCNINFO scninfo)
{
    if (symindex == 0) {
	ErrMsg (EC_Elf_Idx, symindex, "Em_Add_New_Rel");
    }
    if (SCNINFO_relinfo(scninfo) == NULL) {
	Create_New_Relocation_Section (FALSE, scninfo);
    }
    if (Sixtyfour_Bit) {
	Elf64_AltRel reloc;

	REL_offset(reloc) = reloffset; 
	Set_REL64_info ( reloc, symindex, reltype );
	REL64_ssym(reloc) = 0;
	REL64_type2(reloc) = 0;
	REL64_type3(reloc) = 0;
	Em_Add_Bytes_To_Scn (SCNINFO_relinfo(scninfo), &reloc, 
			    sizeof (Elf64_Rel), ELF64_FSZ_XWORD);
    }
    else {
	Elf32_Rel reloc;

	REL_offset(reloc) = reloffset; 

	/* TODO: do this under an -abi flag or something.
	switch (reltype) {
	case R_MIPS_GOT_DISP:
	    reltype = R_MIPS_GOT16;
	    break;
	case R_MIPS_GOT_PAGE:
	    reltype = R_MIPS_GOT16;
	    break;
	case R_MIPS_GOT_OFST:
	default:
	    break;
	}
	*/

	Set_REL32_info ( reloc, symindex, reltype );
	Em_Add_Bytes_To_Scn (SCNINFO_relinfo(scninfo), &reloc, 
			sizeof (Elf32_Rel), ELF32_FSZ_WORD);
    }
}


/* Add a new relocation entry to the Rela section for 'scninfo'. */
void
Em_Add_New_Rela (
    Elf64_Word symindex, 
    unsigned char reltype,
    Elf64_Addr reloffset,
    Elf64_Sxword addend,
    pSCNINFO scninfo)
{
    if (SCNINFO_relainfo(scninfo) == NULL) {
	Create_New_Relocation_Section (TRUE, scninfo);
    }
    if (Sixtyfour_Bit) {
	Elf64_AltRela reloc;

	Set_REL64_info ( reloc, symindex, reltype );
	REL64_ssym(reloc) = 0;
	REL64_type2(reloc) = 0;
	REL64_type3(reloc) = 0;
	REL_offset(reloc) = reloffset; 
	REL_addend(reloc) = addend;
	Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), &reloc, 
				sizeof (Elf64_Rela), ELF64_FSZ_XWORD);
    }
    else {
	Elf32_Rela reloc;

	REL_offset(reloc) = reloffset; 
	Set_REL32_info ( reloc, symindex, reltype );
	REL_addend(reloc) = addend;
	Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), &reloc, 
				sizeof (Elf32_Rela), ELF32_FSZ_WORD);
    }
}


/* Add a new composite relocation entry to the Rela section. For the 32bit
   case, emit multiple relocations.
*/
void
Em_Add_New_Composite_Rela (Elf64_AltRela *preloc, pSCNINFO scninfo)
{
    /* if nothing to do, return. */
    if (REL64_type(*preloc) == R_NONE) return;

    if (SCNINFO_relainfo(scninfo) == NULL) {
	Create_New_Relocation_Section (TRUE, scninfo);
    }

    if (Sixtyfour_Bit) {
	Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), preloc, 
				sizeof (Elf64_Rela), ELF64_FSZ_XWORD);
    }
    else {
	Elf32_Rela reloc;

	REL_offset(reloc) = REL_offset(*preloc); 
	Set_REL32_info ( reloc, REL64_sym(*preloc), REL64_type(*preloc) );
	REL_addend(reloc) = REL_addend(*preloc);
	Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), &reloc, 
				sizeof (Elf32_Rela), ELF32_FSZ_WORD);
	if (REL64_type2(*preloc) != 0) {
	    Set_REL32_info (reloc, 0, REL64_type2(*preloc));
	    REL_addend(reloc) = 0;
	    Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), &reloc, 
				sizeof (Elf32_Rela), ELF32_FSZ_WORD);
	    if (REL64_type3(*preloc) != 0) {
		Set_REL32_info (reloc, 0, REL64_type3(*preloc));
		Em_Add_Bytes_To_Scn (SCNINFO_relainfo(scninfo), &reloc, 
				sizeof (Elf32_Rela), ELF32_FSZ_WORD);
	    }
	}
    }
}


/* Define the symbol with index 'symindex' to have the value 'symvalue' and
   the size 'symsize' in the section 'scninfo'. This is to define a symbol
   that was earlier entered as undefined.
*/
void
Em_Define_Symbol ( 
    Elf64_Word symindex, 
    Elf64_Addr symvalue,
    Elf64_Xword symsize,
    pSCNINFO scninfo)
{
    Elf64_Half scnindex;

    if (symindex == 0) {
	ErrMsg (EC_Elf_Idx, symindex, "Em_Define_Symbol");
    }
    scnindex = SCNINFO_index(scninfo);

    if (Sixtyfour_Bit) {
	Elf64_Sym *symtable;

	symtable = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Define_Symbol");
	symtable[symindex].st_value = symvalue;
	symtable[symindex].st_size = symsize;
	symtable[symindex].st_shndx = scnindex;
    }
    else {
	Elf32_Sym *symtable;

	symtable = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Define_Symbol");
	symtable[symindex].st_value = symvalue;
	symtable[symindex].st_size = symsize;
	symtable[symindex].st_shndx = scnindex;
    }
}

/* Undefine a symbol that was previously entered */
void
Em_Undefine_Symbol (Elf64_Word symindex)
{
    if (symindex == 0) {
	ErrMsg (EC_Elf_Idx, symindex, "Em_Undefine_Symbol");
    }
    if (Sixtyfour_Bit) {
	Elf64_Sym *symtable;

	symtable = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Undefine_Symbol");
	symtable[symindex].st_shndx = SHN_UNDEF;
    }
    else {
	Elf32_Sym *symtable;

	symtable = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Undefine_Symbol");
	symtable[symindex].st_shndx = SHN_UNDEF;
    }
}

/* Set the binding for a symbol that was previously entered */
void
Em_Set_Symbol_Binding (Elf64_Word symindex, unsigned char symbind)
{
    if (symindex == 0) {
	ErrMsg (EC_Elf_Idx, symindex, "Em_Set_Symbol_Binding");
    }
    if (Sixtyfour_Bit) {
	Elf64_Sym *sym64;
	unsigned char symtype;

	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym))) {
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Set_Symbol_Binding");
	}
	sym64 = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info) + symindex;
	symtype = ELF64_ST_TYPE(sym64->st_info);
	sym64->st_info = ELF64_ST_INFO(symbind, symtype);
    }
    else {
	Elf32_Sym *sym32;
	unsigned char symtype;

	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym))) {
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Set_Symbol_Binding");
	}
	sym32 = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info) + symindex;
	symtype = ELF32_ST_TYPE(sym32->st_info);
	sym32->st_info = ELF32_ST_INFO(symbind, symtype);
    }
}

#ifndef MONGOOSE_BE
Elf64_Addr
Em_Get_Symbol_Value (Elf64_Word symindex)
{
    if (Sixtyfour_Bit) {
	Elf64_Sym *symtable;

	symtable = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Get_Symbol_Value");
	return symtable[symindex].st_value;
    }
    else {
	Elf32_Sym *symtable;

	symtable = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Get_Symbol_Value");
	return symtable[symindex].st_value;
    }
}
#endif /* MONGOOSE_BE */

char *
Em_Get_Symbol_Name (Elf64_Word symindex)
{
    if (Sixtyfour_Bit) {
	Elf64_Sym *symtable;

	symtable = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Get_Symbol_Name");
	return Index_To_String(&Strtab_Info, symtable[symindex].st_name);
    }
    else {
	Elf32_Sym *symtable;

	symtable = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Get_Symbol_Name");
	return Index_To_String(&Strtab_Info, symtable[symindex].st_name);
    }
}

/* Add an entry to the .symtab section. Returns the index for the entry */
Elf64_Word
Em_Add_New_Symbol (
    char *symname,
    Elf64_Addr symvalue,
    Elf64_Xword symsize,
    unsigned char symbind,
    unsigned char symtype,
    unsigned char symother,
    Elf64_Half symshndx)
{
    Elf64_Word strindex;
    Elf64_Word symindex;

    strindex = (symname == NULL) ? 0 :
      (Elf64_Word) Em_Add_Bytes_To_Scn (&Strtab_Info, symname, 
					strlen(symname)+1, sizeof(char));
    if (Sixtyfour_Bit) {
	Elf64_Sym sym64;

	sym64.st_name = strindex;
	sym64.st_value = symvalue;
	sym64.st_size = symsize;
	sym64.st_info = ELF64_ST_INFO (symbind, symtype);
	sym64.st_other = symother;
	sym64.st_shndx = symshndx;
	symindex = (Elf64_Word) Em_Add_Bytes_To_Scn (Symtab_Info, &sym64, 
			    sizeof (Elf64_Sym), ELF64_FSZ_XWORD);
	symindex /= sizeof (Elf64_Sym);
    }
    else {
	Elf32_Sym sym32;

	sym32.st_name = strindex;
	sym32.st_value = symvalue;
	sym32.st_size = symsize;
	sym32.st_info = ELF32_ST_INFO (symbind, symtype);
	sym32.st_other = symother;
	sym32.st_shndx = symshndx;
	symindex = (Elf64_Word) Em_Add_Bytes_To_Scn (Symtab_Info, &sym32, 
			sizeof (Elf32_Sym), ELF32_FSZ_WORD);
	symindex /= sizeof (Elf32_Sym);
    }
    return symindex;
}


#ifndef MONGOOSE_BE
/* Add a new symbol with name 'symname', size 'symsize' and symbol binding
   'symbind' to the symbol table as a COMMON symbol.
*/
Elf64_Word 
Em_Add_New_Common_Symbol (
    char *symname, 
    Elf64_Xword symsize, 
    unsigned char symbind,
    unsigned char symother)
{
    return Em_Add_New_Symbol (symname, 0, symsize, symbind, STT_OBJECT, 
							symother, SHN_COMMON);
}
#endif /* MONGOOSE_BE */

/* enter a symbol with name 'symname' and type 'symtype'  as undefined 
   into the symbol table.
*/
Elf64_Word
Em_Add_New_Undef_Symbol (
    char *symname, 
    unsigned char symbind, 
    unsigned char symtype,
    unsigned char symother)
{
    return Em_Add_New_Symbol (symname, 0, 0, symbind, symtype, 
						    symother, SHN_UNDEF);
}


/* Add a new weak symbol entry with the name 'weakname'. Use the same 
   attributes as the symbol table entry with index 'symindex'.
*/
Elf64_Word
Em_Add_New_Weak_Symbol (
    char *weakname, 
    unsigned char symtype,
    unsigned char symother,
    Elf64_Word symindex)
{
    if (symindex == 0) {
	ErrMsg (EC_Elf_Idx, symindex, "Em_Add_New_Weak_Symbol");
    }
    if (Sixtyfour_Bit) {
	Elf64_Sym *symtable;

	symtable = (Elf64_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf64_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Add_New_Weak_Symbol");
	return Em_Add_New_Symbol (weakname, symtable[symindex].st_value,
				symtable[symindex].st_size, STB_WEAK,
				symtype, symother,
				symtable[symindex].st_shndx);
    }
    else {
	Elf32_Sym *symtable;

	symtable = (Elf32_Sym *)SCNINFO_buffer(Symtab_Info);
	if (symindex >= (SCNINFO_size(Symtab_Info) / sizeof(Elf32_Sym)))
	    ErrMsg (EC_Elf_Idx, symindex, "Em_Add_New_Weak_Symbol");
	return Em_Add_New_Symbol (weakname, symtable[symindex].st_value,
				symtable[symindex].st_size, STB_WEAK,
				symtype, symother,
				symtable[symindex].st_shndx);
    }
}



/* Update the size field of the last EK_ADDR_RESET entry in 'ev_scn'. */
static void
Update_Addr_Reset_Size (pSCNINFO ev_scn)
{
    UINT16 size;
    INT size_index;

    size = SCNINFO_offset(ev_scn) - SCNINFO_ev_offset(ev_scn);
    size_index = SCNINFO_ev_offset(ev_scn) + 5;
    SCNINFO_buffer(ev_scn)[size_index] = (size >> 8);
    SCNINFO_buffer(ev_scn)[size_index+1] = (size & 0xff);
}

/* Emit an EK_ADDR_RESET opcode into either the .events section or the
 * .contents section.
 */
void
Generate_Addr_Reset (pSCNINFO scn, BOOL is_events, Elf64_Xword ev_ofst)
{
    Elf64_Xword evscn_ofst; 
    char opcode = EK_ADDR_RESET;
    Elf64_Xword dummy = 0;
    pSCNINFO events_scn;
    Elf64_Word short_ofst = ev_ofst;

    if ( short_ofst != ev_ofst ) {
      ErrMsg ( EC_Elf_Ofst64, ev_ofst, "Generate_Addr_Reset" );
    }

    events_scn = (is_events) ? SCNINFO_events(scn) : SCNINFO_contents(scn);
    /* Use the ev_offset field of the .events or .contents section to 
     * keep the address of the last EK_ADDR_RESET entry.
     */
    SCNINFO_ev_offset(events_scn) = (Elf64_Word)
	    Em_Add_Bytes_To_Scn (events_scn, &opcode, 1, 1);
    evscn_ofst = Em_Add_Bytes_To_Scn (events_scn, &short_ofst, 4, 1);
    Em_Add_New_Rel (Em_Create_Section_Symbol(scn), R_SCN_DISP, 
			evscn_ofst, events_scn);
    /* Place holder for the number of bytes till next EK_ADDR_RESET */
    Em_Add_Bytes_To_Scn (events_scn, &dummy, 2, 1);
    if (is_events)
	SCNINFO_ev_offset(scn) = ev_ofst;
    else
	SCNINFO_con_offset(scn) = ev_ofst;
}

/* The maximum number of bytes in the .events section between two
   EK_ADDR_RESET entries.
*/
#define MAX_EV_SIZE 64000

void
Set_Current_Location (pSCNINFO scn, BOOL is_events, Elf64_Word ev_ofst)
{
    Elf64_Word cur_ofst, word_ofst;
    char opcode;
    pSCNINFO ev_scn;
    char leb_buf[10];
    UINT32 num_bytes;


    ev_scn = (is_events) ? SCNINFO_events(scn) : SCNINFO_contents(scn);

    /* Check if we need to insert another EK_ADDR_RESET entry. */
    if ((SCNINFO_offset(ev_scn) - SCNINFO_ev_offset(ev_scn)) > MAX_EV_SIZE) {
	Update_Addr_Reset_Size (ev_scn);
	Generate_Addr_Reset ( scn, is_events, (Elf64_Xword)ev_ofst );
	return;
    }

    cur_ofst = (is_events) ? SCNINFO_ev_offset(scn) : SCNINFO_con_offset(scn);
    if (ev_ofst < cur_ofst) {
	// don't think this can happen, but just to be sure....
	Update_Addr_Reset_Size (ev_scn);
	Generate_Addr_Reset ( scn, is_events, (Elf64_Xword)ev_ofst );
	return;
    }
    if ((ev_ofst - cur_ofst) > MAX_EV_SIZE) {
	// don't think this can happen, but just to be sure....
	Update_Addr_Reset_Size (ev_scn);
	Generate_Addr_Reset ( scn, is_events, (Elf64_Xword)ev_ofst );
	return;
    }
    /* If already at the right place, don't do anything. */
    if (ev_ofst == cur_ofst) return;

    word_ofst = (ev_ofst - cur_ofst) >> 2;
    if (word_ofst <= 127) {
	opcode = EK_INCR_LOC | (word_ofst & 0x7f);
	Em_Add_Bytes_To_Scn (ev_scn, &opcode, 1, 1);
    } else {
	opcode = EK_INCR_LOC_EXT;
	Em_Add_Bytes_To_Scn (ev_scn, &opcode, 1, 1);
	num_bytes = _leb128_unsigned_encode32 (word_ofst, leb_buf);
	Em_Add_Bytes_To_Scn (ev_scn, leb_buf, num_bytes, 1); 
    }
    if ( is_events)
      SCNINFO_ev_offset(scn) = ev_ofst;
    else
      SCNINFO_con_offset(scn) = ev_ofst;
}


/* Add an interface descriptor to the .interfaces section. */

void
Em_Add_New_Interface ( 
    Elf64_Word length, 
    Elf64_Byte *data )
{
    if ( Interface_Scn == NULL) {
	Interface_Scn = Em_New_Section (SECT_IFACE_NAME, SHT_IRIX_IFACE,
		      SHF_NOSTRIP, ELF32_FSZ_WORD, ELF32_FSZ_WORD);
	Em_Set_sh_link (Interface_Scn, SCNINFO_index(Symtab_Info));
    }
    Em_Add_Bytes_To_Scn (Interface_Scn, data, length, ELF32_FSZ_WORD);
}

/* Add a comment string; 
 * if the string contains no colons (':'),
 * then assume we are only passing the toolname,
 * and this routine will go ahead and add the version information.
 * E.g. can pass "be" or "be::4.00:".
 */
extern void
Em_Add_Comment (char *s)
{
    char *buff;
    if (Comment_Scn == NULL) {
	Comment_Scn = Em_New_Section (ELF_COMMENT, SHT_PROGBITS, 
				      0, 0, sizeof(char));
    }
    if (strchr(s,':') == NULL) {
	buff = (char *) alloca (strlen(s) + sizeof(INCLUDE_STAMP) + 
				strlen(object_file_name) + 4);
	sprintf(buff, "%s::%s:%s", s, INCLUDE_STAMP, object_file_name);
    } else {
	buff = s;
    }
    Em_Add_Bytes_To_Scn (Comment_Scn, buff, strlen(buff) + 1, sizeof(char));
}

/* Create the ELF header, section string table, the symbol table
   section and the symbol string table. 
*/
static void
Create_Elf_Header (INT isa, BOOL old_abi, BOOL big_endian, BOOL pic, 
	BOOL cpic, BOOL xgot, BOOL gp_groups)
{
    unsigned char e_ident_data;
    Elf64_Word 	  e_flags;
    Elf32_Ehdr	*ehdr32;
    Elf64_Ehdr	*ehdr64;


    /* which target architecture */
    e_flags = Config_ELF_From_Target (Sixtyfour_Bit, old_abi, isa);

    /* set the PIC flags. */
    if (pic) 
	e_flags |= EF_MIPS_PIC;
    else if (cpic) 
	e_flags |= EF_MIPS_CPIC;
    if (xgot)
	e_flags |= EF_MIPS_XGOT;
    if (gp_groups)
	e_flags |= EF_MIPS_OPTIONS_FIRST;

    /* what endian is this? */ 
    e_ident_data = (big_endian ? ELFDATA2MSB : ELFDATA2LSB);

    if (Sixtyfour_Bit) {
	ehdr64 = elf64_newehdr(Elf_Ptr);
	ehdr64->e_ident[EI_DATA]  = e_ident_data;
    	Set_Elf_Version ((unsigned char *) &(ehdr64->e_ident));
	ehdr64->e_machine = Get_Elf_Target_Machine();
	ehdr64->e_type = ET_REL;
#if ! (defined(linux) || defined(BUILD_OS_DARWIN))
	ehdr64->e_flags = e_flags;
#else
	ehdr64->e_flags = 0x23000000LL | e_flags;
#endif
    }
    else {
	ehdr32 = elf32_newehdr(Elf_Ptr);
	ehdr32->e_ident[EI_DATA]  = e_ident_data;
    	Set_Elf_Version ((unsigned char *) &(ehdr32->e_ident));
	ehdr32->e_machine = Get_Elf_Target_Machine();
	ehdr32->e_type = ET_REL;
	ehdr32->e_flags = e_flags;
    }

    /* allocate an empty string at the start of the string tables.
       This is for entries that have string table index of 0.
    */
    (void) Em_Add_Bytes_To_Scn (&Shstrtab_Info, "", 1, 1);
    (void) Em_Add_Bytes_To_Scn (&Strtab_Info, "", 1, 1);

    /* create symbol table section */
    SCNINFO_scnptr(Symtab_Info) = 
	Create_New_Section (ELF_SYMTAB, SHT_SYMTAB, SHF_ALLOC , 
		Sixtyfour_Bit ? sizeof(Elf64_Sym) : sizeof(Elf32_Sym));

    /* add an empty entry to the symbol table as the first entry */
    Em_Add_New_Symbol (NULL, 0, 0, STB_LOCAL, STT_NOTYPE, 0, SHN_UNDEF);

    /* create section string tbl */
    Shstrtab_Info.scnptr = Create_New_Section (ELF_SHSTRTAB, SHT_STRTAB, 
				SHF_ALLOC, sizeof(char));

    /* create symbol string tbl */
    Strtab_Info.scnptr = Create_New_Section (ELF_STRTAB, SHT_STRTAB, 
				SHF_ALLOC, sizeof(char));

    Em_Set_sh_link (Symtab_Info, SCNINFO_index(&Strtab_Info));
    if (Sixtyfour_Bit) {
	ehdr64->e_shstrndx = SCNINFO_index(&Shstrtab_Info);
    }
    else {
	ehdr32->e_shstrndx = SCNINFO_index(&Shstrtab_Info);
    }
}


static void
Read_Section (pSCNINFO scninfo, Elf64_Word scndx)
{
    Elf_Data *scndata;
    Elf_Scn *scn;

    /* initialize the SCNINFO to all zeros */
    BZERO (scninfo, sizeof(SCNINFO));
    scn = elf_getscn (Elf_Ptr, scndx);
    scndata = elf_getdata (scn, (Elf_Data *)0);
    elf_flagscn (scn, ELF_C_SET, ELF_F_DIRTY);
    elf_flagdata (scndata, ELF_C_SET, ELF_F_DIRTY);
    SCNINFO_scnptr(scninfo) = scn;
    Em_Add_Bytes_To_Scn (scninfo, scndata->d_buf, scndata->d_size, 
		    scndata->d_align);
    scndata->d_buf = NULL;
    scndata->d_size = 0;
}


static void
Read_Elf_File (void)
{
    Elf64_Word shndx;
    Elf64_Word scntype;
    Elf_Scn *scn;

    shndx = (Sixtyfour_Bit) ? elf64_getehdr(Elf_Ptr)->e_shstrndx :
				     elf32_getehdr(Elf_Ptr)->e_shstrndx;
    Read_Section (&Shstrtab_Info, shndx);

    scn = NULL;
    while ((scn = elf_nextscn (Elf_Ptr, scn)) != NULL) {
	scntype = (Sixtyfour_Bit) ? elf64_getshdr(scn)->sh_type :
					elf32_getshdr(scn)->sh_type;
	if (scntype == SHT_SYMTAB) {
	    Read_Section (Symtab_Info, elf_ndxscn(scn));
	    shndx = (Sixtyfour_Bit) ? elf64_getshdr(scn)->sh_link :
					  elf32_getshdr(scn)->sh_link;
	    Read_Section (&Strtab_Info, shndx);
	    break;
	}
    }
}


/* returns the file descriptor for the file opened. */
int Em_Begin_File (
    char *ofilename, 
    BOOL update, 
    BOOL elf64, 
    BOOL old_abi,
    INT  isa,
    BOOL big_endian,
    BOOL pic,
    BOOL cpic,
    BOOL xgot,
    BOOL gp_groups,
    BOOL elf_trace)
{
    int ofiledes;
    Elf_Cmd cmd;

    /* init various sections */
    Options_Scn = NULL;
    Comment_Scn = NULL;
    Interface_Scn = NULL;

    Sixtyfour_Bit = elf64;
    Big_Endian = big_endian;
    if (update) {
	ofiledes = open (ofilename, O_RDWR);
    }
    else {
	INT result;
	struct stat statstuff;

	result = stat (ofilename, &statstuff);
	/* If the file already exists, delete it. */
	if (result != -1) {
	    /* it exists */
	    result = unlink (ofilename);
	    if (result == -1) {
		ErrMsg (EC_Obj_Delete, ofilename, errno);
		return (int) 0;
	    }
	}
	ofiledes = open (ofilename, O_RDWR|O_TRUNC|O_CREAT, 0666);
    }
    if (ofiledes <= 0) {
	ErrMsg (EC_Obj_Create, ofilename, errno);
	return (int) 0;
    }

    object_file_name = ofilename;

    /* reset the elf error number */
    (void)elf_errno();
    /* check that we are not using an older version of libelf. */
    if (elf_version(EV_CURRENT) == EV_NONE) {
	print_elf_error ();
    }
    cmd = update ? ELF_C_RDWR : ELF_C_WRITE;
    if ((Elf_Ptr = elf_begin(ofiledes, cmd, (Elf * )0)) == 0) {
	print_elf_error ();
    }

    if (update) {
	Read_Elf_File ();
    }
    else {
	Create_Elf_Header (isa, old_abi, big_endian, pic, cpic, xgot, gp_groups);
    }
    return ofiledes;
}


/* update the elf object file */
void
Em_End_File (void)
{
    /* write out the .shstrtab data */
    Update_Data (&Shstrtab_Info, ELF_T_BYTE);
    /* write out the .strtab data */
    Update_Data (&Strtab_Info, ELF_T_BYTE);
    /* write out the .symtab data */
    Update_Data (Symtab_Info, ELF_T_SYM);

    /* write out the .options data */
    if (Options_Scn != NULL) Em_End_Section (Options_Scn);
    /* write out the .comment data */
    if (Comment_Scn != NULL) Em_End_Section (Comment_Scn);
    /* write out the .interface data */
    if (Interface_Scn != NULL) Em_End_Section (Interface_Scn);

    if (elf_update(Elf_Ptr, ELF_C_WRITE)  == -1 ||
	elf_end(Elf_Ptr) == 0) 
    {
	print_elf_error ();
    }
}


pSCNINFO Em_New_Section (
    const char *scnname,
    Elf64_Word 	scntype,
    Elf64_Xword	scnflags,
    Elf64_Xword scnentsize,
    Elf64_Xword scnalign)
{
    Elf_Scn *scn;
    pSCNINFO scninfo;

    scninfo = (pSCNINFO) malloc (sizeof (SCNINFO));
    if (scninfo == NULL) 
	ErrMsg ( EC_No_Mem, "Em_New_Section" );
    BZERO (scninfo, sizeof(SCNINFO));
    scn = Create_New_Section (scnname, scntype, scnflags, scnentsize);
    SCNINFO_scnptr(scninfo) = scn;
    SCNINFO_align(scninfo) = scnalign;
    return scninfo;
}


void
Em_End_Section (pSCNINFO scninfo)
{
    Update_Data (scninfo, ELF_T_BYTE);
    if (SCNINFO_relinfo(scninfo) != NULL) {
	Update_Data (SCNINFO_relinfo(scninfo), ELF_T_REL);
	free (SCNINFO_relinfo(scninfo));
    }
    if (SCNINFO_relainfo(scninfo) != NULL) {
	Update_Data (SCNINFO_relainfo(scninfo), ELF_T_RELA);
	free (SCNINFO_relainfo(scninfo));
    }
    if (SCNINFO_events(scninfo) != NULL) {
	Update_Addr_Reset_Size (SCNINFO_events(scninfo));
	Em_End_Section (SCNINFO_events(scninfo));
    }
    if (SCNINFO_contents(scninfo) != NULL) {
	Update_Addr_Reset_Size (SCNINFO_contents(scninfo));
	Em_End_Section (SCNINFO_contents(scninfo));
    }
    free (scninfo);
}

/* Allocate a new data buffer of 'size'. */
void Em_New_Data_Buffer (pSCNINFO scninfo, Elf64_Xword size, int alignment)
{
    Elf64_Xword newoffset;

    newoffset = SCNINFO_size(scninfo);
    newoffset = Roundup (newoffset, alignment);
    if (SCNINFO_align(scninfo) < alignment)
	SCNINFO_align(scninfo) = alignment;
    Increase_Data_Buffer_Size ( scninfo, newoffset + size );
}

Elf64_Word
Em_Create_Section_Symbol (pSCNINFO scninfo)
{
    Elf64_Word symindex;

    symindex = SCNINFO_scnidx(scninfo);
    if (symindex == 0) { 
	symindex = Em_Add_New_Symbol (Em_Get_Section_Name (scninfo),
				      (Elf64_Addr)0, (Elf64_Xword)0, 
				      STB_LOCAL, STT_SECTION, 0, 
				      SCNINFO_index(scninfo));
	SCNINFO_scnidx(scninfo) = symindex;
    }
    return symindex;
}
