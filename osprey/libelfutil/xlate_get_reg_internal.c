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


/*

  xlate_get_reg_internal.c

  $Revision: 1.1.1.1 $


*/

#include "xlateincl.h"


#define STORE_INSTR_INC 64

struct current_state_s {
	Dwarf_Regtable           cs_regtab;
	struct current_state_s * cs_next;
};

/*
   As of Oct 98, no longer have static variable as stack
   base: use function-local var instead (caller passes in).
   Making multiple table users independent.
*/
static int
_xlate_push_curr_state(struct current_state_s**base,
	Dwarf_Regtable * regtable)
{
	struct current_state_s * lcs;

	lcs = malloc(sizeof(struct current_state_s));
	if(!lcs) {
	  return XLATE_TB_STATUS_ALLOC_FAIL;
	}
	lcs->cs_next = *base;
	lcs->cs_regtab = *regtable;
	*base = lcs;
        return XLATE_TB_STATUS_NO_ERROR;
}
static int
_xlate_pop_curr_state(struct current_state_s **base,
		Dwarf_Regtable * regtable)
{
	struct current_state_s * lcs = *base;

	if(!lcs) {
		return XLATE_TB_STATUS_FRAME_RESTORE_INVALID;
	}
	
	*regtable = lcs->cs_regtab;
	*base = lcs->cs_next;
	free(lcs);
        return XLATE_TB_STATUS_NO_ERROR;
}

static void
_xlate_pop_all_state(struct current_state_s **base)
{
	while(*base) {
		struct current_state_s *lcl = *base;
		*base = lcl->cs_next;
		free(lcl);
	}
	return;
}

/*
	Pick up table-specific info and then call
	the routine which is independent of the xlate_table_con.
*/
int _xlate_expand_reg_info_internal(xlate_table_con tab,
        int wantInstrs, /* 0 if want instrs (ignore pc), 
		1 if want dwarf reg rules for all rules (at a pc),
		2 if want one reg rule (at a pc) */
	Elf64_Addr       inputpc, 
        Dwarf_Regtable * regtable, /* required */
        Elf64_Xword    * num_instrs, /* may be 0 */
        xlate_reg_instr2 ** instrs_out) /* may be 0 */
{
    int is64bit =          tab->xc_is64bit;
    char *              reginfo = tab->xc_reginfo_data;
    char *              reginfo_end = reginfo +
                          tab->xc_hdr.ich_total_reginfo_bytes;
    int res;
    if(tab->xc_hdr.ich_total_reginfo_bytes == 0) {
	return XLATE_TB_STATUS_NO_REG_INFO;
    }
    res = _xlate_expand_reg_info_internal_given_ptrs(
		reginfo,reginfo_end,
		is64bit,
		tab->xc_hdr.ich_version,
	wantInstrs,inputpc,regtable,num_instrs,instrs_out);

    return res;

}

/*
	This one is created so we can call it from
	the dso_movement routine, which
	has no  xlate_table_con. That way we have just
	one set of code decoding the register section.
*/
int _xlate_expand_reg_info_internal_given_ptrs(
	char *reginfo,
	char *reginfo_end,
	int  is64bit,
	int  table_version,
        int wantInstrs, /* 0 if want instrs (ignore pc), 
		1 if want dwarf reg rules for all rules (at a pc),
		2 if want one reg rule (at a pc) */
	Elf64_Addr       inputpc, 
        Dwarf_Regtable * regtable, /* required */
        Elf64_Xword    * num_instrs, /* may be 0 */
        xlate_reg_instr2 ** instrs_out) /* may be 0 */
{
    Elf64_Addr                  loc;
    Elf64_Addr                  newLoc;
    Elf64_Addr                  advPc;
    Dwarf_Unsigned              val1;
    Dwarf_Unsigned              val2;
    Dwarf_Small                 instr;
    Dwarf_Small                 op;
    Dwarf_Small                 extOp;
    __uint32_t                  reg;
    __uint32_t                  reg2;
    __uint32_t		   val32;
    Xuword		   offset;
    Uword                 storeInstrIndex;
    Uword                 storeInstrSize;
    int                 leb128_length;
    int			retstatus = XLATE_TB_STATUS_NO_ERROR;
    Uword 		i;
    char *              reginfo_base       = reginfo;
    Uword		instoffset         = 0;
    struct current_state_s *_xlate_cs_base = 0;
    xlate_reg_instr2       *instrs         = 0;

    if(instrs_out) {
	storeInstrSize = STORE_INSTR_INC;
	instrs = (xlate_reg_instr2 *)malloc(
		storeInstrSize*sizeof(xlate_reg_instr2));
	if(instrs == 0) {
	  return XLATE_TB_STATUS_ALLOC_FAIL;
	}
    }

    /* Since we expect all calls to be correctly done
    ** we do not do a sanity check on wantInstrs here.
    ** leave the check till all done since we should
    ** always pass the checks...
    */

    storeInstrIndex = 0;
    storeInstrSize = 0;

    for (i = 0; i < DW_REG_TABLE_SIZE; i++) {
        regtable->rules[i].dw_offset_relevant = 0;
        regtable->rules[i].dw_regnum = DW_FRAME_SAME_VAL;
        regtable->rules[i].dw_offset = 0;
    }


    while(reginfo < reginfo_end) {
	instoffset = reginfo - reginfo_base;
	instr = *(Dwarf_Small *)reginfo;
	extOp = instr & 0xc0;
	op = (extOp == 0) ? instr : extOp;
	reginfo++;

	advPc = 0;
	newLoc = 0;
	val1 = 0;
	val2 = 0;

	switch (op) {
	    case DW_CFA_advance_loc :
		advPc = val1 = (instr & 0x3f) << 2;
		break;

	    case DW_CFA_offset :
		reg = val1 = (instr & 0x3f);
		if(reg >= DW_REG_TABLE_SIZE) {
    			_xlate_pop_all_state(&_xlate_cs_base);
			if(instrs) {
				free(instrs); /* prevent leak */
			}
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}

		leb128_length = _leb128_unsigned_decode64(reginfo, &offset);
		offset <<= 2;
		val2 = offset;
		reginfo += leb128_length;

		regtable->rules[reg].dw_offset_relevant = 1;
		regtable->rules[reg].dw_regnum = DW_FRAME_CFA_COL;
		regtable->rules[reg].dw_offset = offset;
		break;
            case DW_CFA_restore:
		reg = val1 = (instr & 0x3f);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		break;

	    case DW_CFA_set_loc :
		/* old tables had 64 bits of set loc always */
		if(is64bit || (table_version <= XLATE_TB_OLD_V1)) {
		  memcpy(&newLoc, reginfo, sizeof(Elf64_Addr));
		  val1 = newLoc;
		  reginfo += sizeof(Elf64_Addr);
		} else {
		  memcpy(&val32, reginfo, sizeof(__uint32_t));
		  val1 = val32;
		  newLoc = val32;
		  reginfo += sizeof(__uint32_t);
		}
		break;

	    case DW_CFA_advance_loc1 :
		advPc = *(Dwarf_Small *)reginfo;
		advPc <<= 2;
		val1 = advPc;
		reginfo++;
		break;

	    case DW_CFA_advance_loc2 :
		{ unsigned short adv;
		if(table_version <= XLATE_TB_OLD_V1) {
		  /* old tables stored in little-endian order! silly. */
		  adv = (unsigned char)reginfo[0];
		  adv |= ((unsigned char)reginfo[1]) << 8;
		} else {
		  /* new version stores in native big-endian */
		  memcpy(&adv,reginfo,2);
		}
		advPc = adv;
		advPc <<= 2;
		val1 = advPc;
		reginfo += 2;
		}
		break;

	    case DW_CFA_advance_loc4 :
		{ __uint32_t adv;
		if(table_version <= XLATE_TB_OLD_V1) {
		  /* old tables stored in little-endian order! silly. */
		  adv = (unsigned char)reginfo[0];
		  adv |= ((unsigned char)reginfo[1]) << 8;
		  adv |= ((unsigned char)reginfo[2]) << 16;
		  adv |= ((unsigned char)reginfo[3]) << 24;
		} else {
		  /* new version stores in native big-endian */
		  memcpy(&adv,reginfo,4);
		}
		advPc = adv;
		advPc <<= 2;
		val1 = advPc;
		reginfo += 4;
		}
		break;

	    case DW_CFA_offset_extended :
	    case DW_CFA_def_cfa :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;

		leb128_length = _leb128_unsigned_decode64(reginfo, &offset);
		offset <<= 2;
		val2 = offset;
		reginfo += leb128_length;

                regtable->rules[DW_FRAME_CFA_COL].dw_offset_relevant = 1;
                regtable->rules[DW_FRAME_CFA_COL].dw_regnum = reg;
                regtable->rules[DW_FRAME_CFA_COL].dw_offset = offset;
		break;


	    case DW_CFA_restore_extended :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;
		break;

	    case DW_CFA_undefined :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
    			_xlate_pop_all_state(&_xlate_cs_base);
			if(instrs) {
				free(instrs); /* prevent leak */
			}
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;

                regtable->rules[reg].dw_offset_relevant = 0;
                regtable->rules[reg].dw_regnum = DW_FRAME_UNDEFINED_VAL;
                regtable->rules[reg].dw_offset = 0;
		break;

	    case DW_CFA_same_value :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;

                regtable->rules[reg].dw_offset_relevant = 0;
                regtable->rules[reg].dw_regnum = DW_FRAME_SAME_VAL;
                regtable->rules[reg].dw_offset = 0;
		break;
	
	    case DW_CFA_def_cfa_register :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;

                regtable->rules[DW_FRAME_CFA_COL].dw_regnum = reg;
		break;

	    case DW_CFA_register :
		leb128_length = _leb128_unsigned_decode32(reginfo, &reg);
		if(reg >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val1 = reg;
		reginfo += leb128_length;

		leb128_length = _leb128_unsigned_decode32(reginfo, &reg2);
		if(reg2 >= DW_REG_TABLE_SIZE) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_BAD_REG_VAL;
		}
		val2 = reg2;
		reginfo += leb128_length;

                regtable->rules[reg].dw_offset_relevant = 0;
                regtable->rules[reg].dw_regnum = reg2;
                regtable->rules[reg].dw_offset = 0;
		break;


	    case DW_CFA_remember_state:
		retstatus = _xlate_push_curr_state(&_xlate_cs_base,regtable);
		if(retstatus !=  XLATE_TB_STATUS_NO_ERROR) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
			_xlate_pop_all_state(&_xlate_cs_base);
			return retstatus;
		}
		break;
	    case DW_CFA_restore_state:
		retstatus = _xlate_pop_curr_state(&_xlate_cs_base,
				regtable);
		if(retstatus !=  XLATE_TB_STATUS_NO_ERROR) {
			/* oops. popped more than we pushed! */
			_xlate_pop_all_state(&_xlate_cs_base);
			if(instrs) {
				free(instrs); /* prevent leak */
			}
			return retstatus;
		}
		break;
	    case DW_CFA_nop :
		break;

	    case DW_CFA_def_cfa_offset :
		leb128_length = _leb128_unsigned_decode64(reginfo, &offset);
		offset <<= 2;
		val1 = offset;
		reginfo += leb128_length;

                regtable->rules[DW_FRAME_CFA_COL].dw_offset = offset;
		break;

	    default :
    		_xlate_pop_all_state(&_xlate_cs_base);
		if(instrs) {
			free(instrs); /* prevent leak */
		}
		return XLATE_TB_STATUS_BAD_FRAME_OP;
	}

	if(wantInstrs == ALL_REG_RULES) {
	    if(newLoc == 0 ) {
		newLoc = loc + advPc ;
	    }
	    if (newLoc > inputpc || reginfo >= reginfo_end) {
    		_xlate_pop_all_state(&_xlate_cs_base);
		if(instrs) {
			free(instrs); /* prevent leak */
		}
		return XLATE_TB_STATUS_NO_ERROR;
	    }
	    loc = newLoc;
	}else if(wantInstrs == REG_RULE_AT_PC) {
	    if(newLoc == 0 ) {
		newLoc = loc + advPc ;
	    }
	    if (newLoc > inputpc || reginfo >= reginfo_end) {
    		_xlate_pop_all_state(&_xlate_cs_base);
		if(instrs) {
			free(instrs); /* prevent leak */
		}
		return XLATE_TB_STATUS_NO_ERROR;
	    }
	    loc = newLoc;
	}else if(wantInstrs == ALL_INSTRUCTIONS) {
	    if(!instrs) {
    		_xlate_pop_all_state(&_xlate_cs_base);
		return XLATE_TB_STATUS_REG_REQUEST_BOGUS;
	    }
	    if (storeInstrIndex >= storeInstrSize) {
		xlate_reg_instr2 * newloc;

		storeInstrSize += STORE_INSTR_INC;
		newloc = realloc(instrs, 
			storeInstrSize * sizeof(xlate_reg_instr2));
		if(newloc == 0) {
			if(instrs) {
				free(instrs); /* prevent leak */
			}
    			_xlate_pop_all_state(&_xlate_cs_base);
			return XLATE_TB_STATUS_ALLOC_FAIL;
		}
		instrs = newloc;
	    }

	    instrs[storeInstrIndex].sr_op = op;
	    instrs[storeInstrIndex].sr_val1 = val1;
	    instrs[storeInstrIndex].sr_val2 = val2;
	    instrs[storeInstrIndex].sr_instr_offset =  instoffset;
	    ++storeInstrIndex;
	} else {
    		_xlate_pop_all_state(&_xlate_cs_base);
		if(instrs) {
			free(instrs); /* prevent leak */
		}
		return XLATE_TB_STATUS_REG_REQUEST_BOGUS;
	}

    }
	
    if(wantInstrs == ALL_REG_RULES) {
    	_xlate_pop_all_state(&_xlate_cs_base);
	if(instrs) {
			free(instrs); /* prevent leak */
	}
        return XLATE_TB_STATUS_NO_ERROR;
    }else if(wantInstrs == REG_RULE_AT_PC) {
	if(instrs) {
			free(instrs); /* prevent leak */
	}
    }else if(wantInstrs == ALL_INSTRUCTIONS) {
       *instrs_out = instrs;
       *num_instrs = storeInstrIndex;
    } else {
    	_xlate_pop_all_state(&_xlate_cs_base);
	if(instrs) {
			free(instrs); /* prevent leak */
	}
	return XLATE_TB_STATUS_REG_REQUEST_BOGUS;
    }

    _xlate_pop_all_state(&_xlate_cs_base);
    return XLATE_TB_STATUS_NO_ERROR;
}

