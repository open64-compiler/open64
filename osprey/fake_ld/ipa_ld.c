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

static an_object_file_ptr p_obj_ir_list_head = NULL;

 	/*******************************************************
		Function: add_ir

		????

	 *******************************************************/
#ifdef  TARG_MIPS
void
add_ir (an_object_file_ptr p_obj)
{
    p_obj->next = p_obj_ir_list_head;
    p_obj_ir_list_head = p_obj;
    num_objects_linked++;
    num_ir++;
} /* add_ir */

#else
#endif

	/*******************************************************
		Function: get_next_ir

		Return the next obj structure in the ir linked list.

	 *******************************************************/
#ifdef  TARG_MIPS
an_object_file_ptr
get_next_ir (an_object_file_ptr p_obj)
{
    if (p_obj == 0)
	return (p_obj_ir_list_head);
    else
	return (p_obj->next);

} /* get_next_ir */

#else
#endif

	/*******************************************************
		Function: reload

		I don't know if this relavant with bfd
		because it uses its own cache system.

	 *******************************************************/
#ifdef  TARG_MIPS
static void
reload (an_object_file_ptr pobj, string path)
{
    string old_name = pobj->name;
    unsigned old_name_index = pobj->name_ndx;
    int fd;

    unread_sections (pobj);
    unread_obj (pobj, FALSE);
    BZERO (pobj, sizeof(an_object_file));
    pobj->other_name = old_name;
    pobj->other_name_ndx = old_name_index;
    pobj->name = path;
    pobj->name_ndx = string_find_offset(path);

    /* copied from pass1 */
    fd = open_file (path, TRUE, FALSE, 0);
    if (!use_mmap)
	pobj->access_method = AM_READ;
    pobj->ftype = read_headers (pobj, fd, FALSE);
    LD_ASSERT (pobj->ftype == FT_OBJ, thisfile,
	       "incorrect file type from back end");

    add_object (pobj);
    msg(ER_DEFAULT /* ER_VERBOSE */ , ERN_LOAD_OBJ, num_objects_linked, pobj->name);
    if (bsdmap)
	printf("%s\n", pobj->other_name);
    process_object (pobj);
    if (optsym[OPTSYM_MDEPEND].num)
	update_make_depend (pobj->other_name);
    unread_sections (pobj);
    close (fd);
    delay_load = DL_DEFAULT;
    if (hides != HS_IGNORE) {
	hides = HS_DEFAULT;
    }
} /* reload */

#else
#endif

	/*******************************************************
		Function: ld_set_st_idx

		
	 *******************************************************/
    asymbol* ext = (asymbol *) pext;
    ext->udata.whirl_st_idx = st_idx;
}



	/*******************************************************
		Function: ld_get_st_idx

		
	 *******************************************************/
ST_IDX
ld_get_st_idx (void* pext)
{
    const asymbol* ext = (asymbol *) pext;

    return (ext->udata.whirl_st_idx);
}


	/*******************************************************
		Function: ld_resolved_to_obj

		
	 *******************************************************/
int
ld_resolved_to_obj (void* pext, void* abfd)
{
    const asymbol* ext = (asymbol *) pext;

    return (ext->the_bfd == (struct _bfd *)abfd);
}


	/*******************************************************
		Function: Count_elf_external_gots

		
	 *******************************************************/
#ifdef  TARG_MIPS
INT 
Count_elf_external_gots (void)
{
    int i;
    int count = 0;
    EXTSYM *table;

    table = ext_tbl.u.ext.buf;
    for (i = 0; i < ext_tbl.u.ext.num; i++) {
		EXTSYM *pext = table + i;
		int address_taken;

		/*
		 * Skip this entry if there is a corresponding WHIRL symbol.
		 */
		if (pext->u0.whirl_st_idx != WHIRL_ST_IDX_UNINITIALIZED &&
		    pext->u0.whirl_st_idx != WHIRL_ST_IDX_NOT_AVAILABLE) 
		    continue;

		/* need to check for addr_taken and used_in_obj 
	 	 * if so, need got
 	 	 * else not
	 	 */
		if (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_CALL_SHARED) {
	    	/* All defined symbol whose addresses are taken will NOT
	     	* have a GOT since those addresses would not be moved 
	     	* ie no .dynrel entry will be generated for them
	     	*/
	    	    if (!IS_UNDEF(pext->ext->st_shndx))
    	    	    	address_taken = 0;
    	    	    else
            	    	address_taken = pext->flags & MF_ADDRESS_TAKEN;
    	    	}

    	    	if (	(pext->used)                                  || 
			(ELF_ST_BIND(pext->ext->st_info) == STB_WEAK) ||
	            	(   IS_UNDEF(pext->ext->st_shndx) &&
#ifdef _64BIT_OBJECTS
             	    	    (pext->ext->st_other == STO_HIDDEN) ||
             	    	    (	(pext->ext->st_other == STO_INTERNAL) && 
			    	(ELF_ST_TYPE(pext->ext->st_info) == STT_OBJECT)) ||
#endif
	     	    	    ((IS_DEFAULT(pext)) || address_taken))) { 
	    	    count++;
#ifdef DEBUG
			printf("symbol (used = %d, address_taken = %d, is_default = %d -------  %s  ------------", 
			       pext->used?1:0, 
				   address_taken?1:0, 
				   pext->ext->st_other, 
				   ext_tbl.str.buf+pext->iss);
			printf("has GOT entry\n");
#endif
		}
    }
	return count;
} // Count_elf_external_gots
#else
#endif



