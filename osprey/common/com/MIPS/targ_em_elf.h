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


#ifndef targ_em_elf_INCLUDED
#define targ_em_elf_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif


struct section_info {
    Elf_Scn *scnptr;		/* ptr to the elf section. */
    char *buffer;		/* data buffer for section. */
    Elf64_Xword limit;		/* maximum size of data buffer. */
    Elf64_Xword size;		/* current size of data buffer. */
    Elf64_Xword offset;		/* current offset in data buffer. */
    Elf64_Word align;		/* alignment of the section. */
    Elf64_Word scnidx;		/* symbol index of the section symbol. */
    pSCNINFO relinfo;		/* associated REL section. */
    pSCNINFO relainfo;		/* associated RELA section. */
    pSCNINFO events;		/* associated EVENTS section.  */
    Elf64_Word ev_offset;	/* offset of last entry in events scn. */
    pSCNINFO contents;		/* associated CONTENTS section. */
    Elf64_Word con_offset;	/* offset of last entry in contents scn. */
}; 

#define SCNINFO_scnptr(t)	((t)->scnptr)
#define SCNINFO_buffer(t)	((t)->buffer)
#define SCNINFO_limit(t)	((t)->limit)
#define SCNINFO_size(t)		((t)->size)
#define SCNINFO_offset(t)	((t)->offset)
#define SCNINFO_align(t)	((t)->align)
#define SCNINFO_scnidx(t)	((t)->scnidx)
#define SCNINFO_relinfo(t)	((t)->relinfo)
#define SCNINFO_relainfo(t)	((t)->relainfo)
#define SCNINFO_events(t)	((t)->events)
#define SCNINFO_ev_offset(t)	((t)->ev_offset)
#define SCNINFO_contents(t)	((t)->contents)
#define SCNINFO_con_offset(t)	((t)->con_offset)

#define SCNINFO_index(t)	(elf_ndxscn(SCNINFO_scnptr(t)))
extern char *Get_Section_Name (pSCNINFO scninfo);
extern void Generate_Addr_Reset (pSCNINFO scn, BOOL is_events, Elf64_Xword ev_ofst);
extern void Set_Current_Location (pSCNINFO scn, BOOL is_events, Elf64_Word ev_ofst);
extern pSCNINFO Interface_Scn;

/* this defines the common, other than name and enum value,
 *  parts of code for both IA-64 and Mips
 */

  /*
   * relocations that are the same other than the enum
   */
#define R_WORD32	(Big_Endian ? R_IA_64_DIR32MSB : R_IA_64_DIR32LSB)
#define R_WORD64	(Big_Endian ? R_IA_64_DIR64MSB : R_IA_64_DIR64LSB)
#define R_SCN_DISP	(Big_Endian ? R_IA_64_SECREL64MSB : R_IA_64_SECREL64LSB)
#define R_PLT_OFFSET      R_IA_64_PLTOFF22
#define R_NONE            R_IA_64_NONE

  /*
   * section flags that are the same other than the enum
   */
#define SHF_MERGE         SHF_IRIX_MERGE
#define SHF_NOSTRIP       SHF_MIPS_NOSTRIP

  /*
   * section types that are the same other than the enum
   */
#define SHT_EVENTS        SHT_IA64_EVENTS
#define SHT_CONTENT       SHT_IA64_CONTENT
#define SHT_IFACE         SHT_IA64_IFACE

  /*
   * section names
   */
#define SECT_OPTIONS_NAME IA64_OPTIONS
#define SECT_EVENTS_NAME  IA64_EVENTS
#define SECT_IFACE_NAME   IA64_INTERFACES
#define SECT_CONTENT_NAME MIPS_CONTENT
  
inline void
Set_Elf_Version (unsigned char *e_ident)
{
	/* temporary version until final objects */
	e_ident[EI_TVERSION] = EV_T_CURRENT;
}

#ifdef __cplusplus
}
#endif
#endif /* targ_em_elf_INCLUDED */

