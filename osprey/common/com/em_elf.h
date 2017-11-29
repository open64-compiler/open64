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


#ifndef em_elf_INCLUDED
#define em_elf_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: em_elf.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/em_elf.h,v $
 *
 * Description:
 *
 * External interface to write out an elf object file.
 *
 * ====================================================================
 * ====================================================================
 */

/* the default gp value for gprel and reginfo */
extern INT GP_DISP;

#define Roundup(_value,_align)        ((_value+(_align-1)) & (~(_align-1)))

typedef struct section_info SCNINFO, *pSCNINFO;

extern pSCNINFO Symtab_Info;
extern BOOL     Sixtyfour_Bit;
extern pSCNINFO Options_Scn;

/* Given a file name 'ofilename', this procedure opens the file. If the 
   flag 'update' is true, it opens the already existing elf file for
   updating. Otherwise it creates a new file. The 'elf64' argument 
   is true if a 64bit object file is to be created. If the 'elf_trace'
   argument is true, some tracing output is generated.

   It returns the file descriptor for the object file opened.
*/
extern INT
Em_Begin_File (
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
    BOOL elf_trace);


/* Write out the elf object file */
extern void 
Em_End_File (void);


/* Create a new section. Its attributes are described in the arguments.
   It returns a pointer to the SCNINFO for the new section.
*/
extern pSCNINFO 
Em_New_Section (
    const char *scnname,
    Elf64_Word scntype,
    Elf64_Xword scnflags,
    Elf64_Xword scnentsize,
    Elf64_Xword scnalign);


/* Finish processing of the section 'scninfo'. */
extern void 
Em_End_Section (pSCNINFO scninfo);


/* Initialize a new buffer with the given 'size' and 'alignment' in the 
   section 'scninfo'. This is used in the cases where we know apriori
   the size of the buffer to be written out. It helps by reducing the 
   number of reallocs needed as we add bytes to the section.
*/
extern void
Em_New_Data_Buffer (pSCNINFO scninfo, Elf64_Xword size, INT alignment);

/* Add an entry to the .symtab section. Returns the index for the entry */
extern Elf64_Word
Em_Add_New_Symbol (
    char *symname,
    Elf64_Addr symvalue,
    Elf64_Xword symsize,
    unsigned char symbind,
    unsigned char symtype,
    unsigned char symother,
    Elf64_Half symshndx);


/* enter a symbol with name 'symname', symbol binding 'symbind' and
   type 'symtype'  as UNDEFINED into the symbol table.
*/
extern Elf64_Word
Em_Add_New_Undef_Symbol (
    char *symname, 
    unsigned char symbind,
    unsigned char symtype,
    unsigned char symother);


/* Add a new symbol with name 'symname', size 'symsize' and symbol 
   binding 'symbind' to the symbol table as a COMMON symbol.
*/
extern Elf64_Word 
Em_Add_New_Common_Symbol (
    char *symname, 
    Elf64_Xword symsize, 
    unsigned char symbind,
    unsigned char symother);

/* Add a new weak symbol entry with the name 'weakname'. Use the same
   attributes as the symbol table entry with index 'symindex'.
*/
extern Elf64_Word
Em_Add_New_Weak_Symbol (
    char *weakname, 
    unsigned char symtype,
    unsigned char symother,
    Elf64_Word symindex);


/* Define the symbol with index 'symindex' to have the value 'symvalue' and
   the size 'symsize' in the section 'scninfo'. This is to define a symbol 
   that was earlier entered as undefined.
*/
extern void
Em_Define_Symbol (
    Elf64_Word symindex,
    Elf64_Addr symvalue,
    Elf64_Xword symsize,
    pSCNINFO scninfo);

/* Undefine a symbol that was previously entered */
extern void Em_Undefine_Symbol (Elf64_Word symindex);

/* Set the binding for a symbol that was previously entered */
extern void Em_Set_Symbol_Binding (Elf64_Word symindex, unsigned char symbind);

extern Elf64_Addr
Em_Get_Symbol_Value (Elf64_Word symindex);

extern char *
Em_Get_Symbol_Name (Elf64_Word symindex);

/* Add a new relocation entry to the section 'scninfo'. */
extern void 
Em_Add_New_Rel (
    Elf64_Word symindex,
    unsigned char reltype,
    Elf64_Addr reloffset,
    pSCNINFO scninfo);


/* Add a new entry to the Rela section for the section 'scninfo'. */
extern void 
Em_Add_New_Rela (
    Elf64_Word symindex,
    unsigned char reltype,
    Elf64_Addr reloffset,
    Elf64_Sxword addend,
    pSCNINFO scninfo);

/* Add a new composite relocation entry to the Rela section.
 * For the 32bit case, emit multiple relocations.
 */
extern void
Em_Add_New_Composite_Rela ( Elf64_AltRela *preloc, pSCNINFO scninfo );

/* Change the origin for the section.  Subsequent calls to
 * Em_Add_Bytes_To_Scn will add stuff at the new origin.
 */
extern void
Em_Change_Section_Origin ( pSCNINFO scn, Elf64_Xword scn_ofst );

/* Change the section alignment, if it is less than 'scn_align'. */
extern void
Em_Change_Section_Alignment ( pSCNINFO scn, Elf64_Word scn_align );


/* write out 'length' number of bytes from the buffer 'input_buf',  to the 
   section 'scninfo' with the alignment 'align'. Any empty space is padded 
   with zeros. 

   This routine can be called with 'length' equal to 0 to just align 
   the current location in the section to the specified alignment 'align'
   without acutally writing out any bytes.
*/
extern Elf64_Xword 
Em_Add_Bytes_To_Scn (
    pSCNINFO scninfo,
    const void *input_buf,
    Elf64_Xword length,
    Elf64_Word align);


/* write out 'length' number of bytes containing zero to the section
   'scninfo' with alignment 'align.
*/
extern Elf64_Xword
Em_Add_Zeros_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word length,
    Elf64_Word align);


/* Write out an "symbol + constant" expression at the current location in 
   the section 'scninfo'. The 'symindex' points to the symbol and 'addend'
   is the constant value.
*/
extern Elf64_Xword
Em_Add_Address_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word symindex,
    Elf64_Sxword addend,
    Elf64_Word align);

/* Write out a section displacement expression at the current location in 
   the section 'scninfo'. The 'symindex' points to the symbol and 'addend'
   is the constant value.
*/
extern Elf64_Xword
Em_Add_Displacement_To_Scn (
    pSCNINFO scninfo,
    Elf64_Word symindex,
    Elf64_Sxword addend,
    Elf64_Word align);

/* Set the sh_link field of a section. */
extern void Em_Set_sh_link (pSCNINFO scn, Elf64_Word link);

/* Set the sh_info field of a section. */
extern void Em_Set_sh_info (pSCNINFO scn, Elf64_Word info);

/* For a given 'scn' return its elf section index. */
extern size_t Em_Get_Section_Index (pSCNINFO scn);

/* For a given 'scn' return its section offset */
extern Elf64_Xword Em_Get_Section_Offset (pSCNINFO scn);

extern char *Em_Get_Section_Name (pSCNINFO scn);
extern Elf64_Word Em_Get_Section_Type (pSCNINFO scn);
extern Elf64_Word Em_Get_Section_Flags (pSCNINFO scn);
extern Elf64_Word Em_Get_Section_Entsize (pSCNINFO scn);
extern Elf64_Word Em_Get_Section_Align (pSCNINFO scn);
extern Elf64_Word Em_Get_Section_Info (pSCNINFO scn);

/* Create a section symbol for the given section, if it does not already
   exist. In any case, return the symindex of the section symbol. */
extern Elf64_Word Em_Create_Section_Symbol (pSCNINFO scninfo);

extern void
Em_Write_Reginfo (
    Elf64_Addr gprvalue, 
    Elf64_Word gprmask, 
    Elf64_Word fprmask,
    BOOL pure_abi);

/* Add a new event to the .events section. The 'operand1' and 'operand2'
   parameters are not needed for most of the event kinds. For those
   cases, the caller should pass 0 for these arguments. All offsets 
   passed as parameters should be byte offsets. The following event
   kinds need valid operands:

      EK_IF_ENTRY:             operand1 is offset in interface scn.
 
      EK_FCALL_LOCAL,
      EK_FCALL_EXTERN,
      EK_FCALL_EXTERN_BIG:     operand1 is elf symbol index of called proc.

      EK_SWITCH_{32,64}:	operand1 is gprel boolean, 
				operand2 is jumptable address,
				operand3 is number of table entries.
*/
extern void 
Em_Add_New_Event (
    Elf64_Word ev_kind,
    Elf64_Word ev_ofst,
    Elf64_Word operand1,
    Elf64_Word operand2,
    Elf64_Word operand3,
    pSCNINFO scn);

/* Add a new entry to the .contents section. */
extern void
Em_Add_New_Content (
    Elf64_Word con_kind,
    Elf64_Xword con_ofst,
    Elf64_Word operand1,
    Elf64_Word operand2,
    pSCNINFO scn);

/* Add a new entry to the .options section. */
extern void
Em_Add_New_Option (
    Elf32_Byte option_kind,
    Elf32_Section option_section,
    Elf32_Word option_info,
    void *buffer,
    Elf32_Byte length);

/* Add a new entry to the .interface section. */

extern void
Em_Add_New_Interface (
    Elf64_Word length,
    Elf64_Byte *data);

/* Add a comment string;
 * if the string contains no colons (':'),
 * then assume we are only passing the toolname,
 * and this routine will go ahead and add the version information.
 * E.g. can pass "be" or "be::4.00:".
 */
extern void Em_Add_Comment (char *s);

/* end unwind section (if any). 
 * must be called before Em_End_File.
 * if trace_file is set then will dump ascii form to file.
 */
extern void Em_End_Unwind (FILE *trace_file, pSCNINFO text_scn);

/* must be called after Em_End_File */
extern void Em_Cleanup_Unwind (void);

#ifdef __cplusplus
}
#endif
#endif /* em_elf_INCLUDED */
