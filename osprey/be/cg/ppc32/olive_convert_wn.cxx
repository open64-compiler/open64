/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

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

*/

#include "olive_convert_wn.h"
#ifdef _OLIVE_AUTO_CG_
#include "olive_gen_expr.h"
#endif

#include "cgexp.h"

#ifdef _OLIVE_AUTO_CG_

int OPC2OP(OPCODE opc)
{
    OPERATOR opr = OPCODE_operator(opc);

    switch(opr)
    {
	case OPR_CALL:
	case OPR_ICALL:
	case OPR_PICCALL:
            return CALL;
    }

    switch(opr){ 
	case OPR_LDBITS:
	    switch(OPCODE_rtype(opc)){
		case MTYPE_I4:
		    return I4LDBITS;
		case MTYPE_U4:
		    return U4LDBITS;
		case MTYPE_I8:
		    return I8LDBITS;
		case MTYPE_U8:
		    return U8LDBITS;
	    }
	    break;
	case OPR_STBITS:
	    return STBITS;
	case OPR_ILDBITS:
	    switch(OPCODE_rtype(opc)){
		case MTYPE_I4:
		    return I4ILDBITS;
		case MTYPE_U4:
		    return U4ILDBITS;
		case MTYPE_I8:
		    return I8ILDBITS;
		case MTYPE_U8:
		    return U8ILDBITS;
	    }
	    break;
	case OPR_ISTBITS:
	    return ISTBITS;
   }

   switch (opc)
    {
        case OPC_I4INTCONST:
            return I4INTCONST;
        case OPC_I4ABS:
            return I4ABS;
        case OPC_I4NEG:
            return I4NEG;
        case OPC_I4ADD:
            return I4ADD;
        case OPC_I4SUB:
            return I4SUB;
        case OPC_I4MPY:
            return I4MPY;
        case OPC_I4HIGHMPY:
            return I4HIGHMPY;
        case OPC_I4DIV:
            return I4DIV;
        case OPC_I4REM:
            return I4REM;
        case OPC_I4MOD:
            return I4MOD;
        case OPC_U4INTCONST:
            return U4INTCONST;
        case OPC_U4NEG:
            return U4NEG;
        case OPC_U4ADD:
            return U4ADD;
        case OPC_U4SUB:
            return U4SUB;
        case OPC_U4MPY:
            return U4MPY;
        case OPC_U4HIGHMPY:
            return U4HIGHMPY;
        case OPC_U4DIV:
            return U4DIV;
        case OPC_U4REM:
            return U4REM;
        case OPC_U4MOD:
            return U4MOD;
        case OPC_BINTCONST:
            return BINTCONST;
        case OPC_BLNOT:
            return BLNOT;
        case OPC_BLAND:
            return BLAND;
        case OPC_BLIOR:
            return BLIOR;
        case OPC_I4LNOT:
            return I4LNOT;
        case OPC_I4LIOR:
            return I4LIOR;
        case OPC_I4LAND:
            return I4LAND;
        case OPC_I4BIOR:
            return I4BIOR;
        case OPC_I4BNOT:
            return I4BNOT;
        case OPC_I4BAND:
            return I4BAND;
        case OPC_I4BXOR:
            return I4BXOR;
        case OPC_I4BNOR:
            return I4BNOR;
        case OPC_U4BNOT:
            return U4BNOT;
        case OPC_U4BIOR:
            return U4BIOR;
        case OPC_U4BAND:
            return U4BAND;
        case OPC_U4BXOR:
            return U4BXOR;
        case OPC_U4BNOR:
            return U4BNOR;
        case OPC_BBEQ:
            return BBEQ;
        case OPC_BBNE:
            return BBNE;
        case OPC_BI4LT:
            return BI4LT;
        case OPC_BI4LE:
            return BI4LE;
        case OPC_BI4EQ:
            return BI4EQ;
        case OPC_BI4NE:
            return BI4NE;
        case OPC_BI4GE:
            return BI4GE;
        case OPC_BI4GT:
            return BI4GT;
        case OPC_I4I4LT:
            return I4I4LT;
        case OPC_I4I4LE:
            return I4I4LE;
        case OPC_I4I4EQ:
            return I4I4EQ;
        case OPC_I4I4NE:
            return I4I4NE;
        case OPC_I4I4GE:
            return I4I4GE;
        case OPC_I4I4GT:
            return I4I4GT;
        case OPC_U4I4LT:
            return U4I4LT;
        case OPC_U4I4LE:
            return U4I4LE;
        case OPC_U4I4EQ:
            return U4I4EQ;
        case OPC_U4I4NE:
            return U4I4NE;
        case OPC_U4I4GE:
            return U4I4GE;
        case OPC_U4I4GT:
            return U4I4GT;
        case OPC_I4MAX:
            return I4MAX;
        case OPC_I4MIN:
            return I4MIN;
        case OPC_I4SHL:
            return I4SHL;
        case OPC_I4ASHR:
            return I4ASHR;
        case OPC_I4LSHR:
            return I4LSHR;
        case OPC_BU4LT:
            return BU4LT;
        case OPC_BU4LE:
            return BU4LE;
        case OPC_BU4EQ:
            return BU4EQ;
        case OPC_BU4NE:
            return BU4NE;
        case OPC_BU4GE:
            return BU4GE;
        case OPC_BU4GT:
            return BU4GT;
        case OPC_I4U4LT:
            return I4U4LT;
        case OPC_I4U4LE:
            return I4U4LE;
        case OPC_I4U4EQ:
            return I4U4EQ;
        case OPC_I4U4NE:
            return I4U4NE;
        case OPC_I4U4GE:
            return I4U4GE;
        case OPC_I4U4GT:
            return I4U4GT;
        case OPC_U4U4LT:
            return U4U4LT;
        case OPC_U4U4LE:
            return U4U4LE;
        case OPC_U4U4EQ:
            return U4U4EQ;
        case OPC_U4U4NE:
            return U4U4NE;
        case OPC_U4U4GE:
            return U4U4GE;
        case OPC_U4U4GT:
            return U4U4GT;
        case OPC_U4MAX:
            return U4MAX;
        case OPC_U4MIN:
            return U4MIN;
        case OPC_U4SHL:
            return U4SHL;
        case OPC_U4ASHR:
            return U4ASHR;
        case OPC_U4LSHR:
            return U4LSHR;
	case OPC_F4CONST:
	    return F4CONST;
        case OPC_F4ADD:
            return F4ADD;
        case OPC_F4SUB:
            return F4SUB;
        case OPC_F4MPY:
            return F4MPY;
        case OPC_F4DIV:
            return F4DIV;
        case OPC_F4MADD:
            return F4MADD;
        case OPC_F4NMADD:
            return F4NMADD;
        case OPC_F4MSUB:
            return F4MSUB;
        case OPC_F4NMSUB:
            return F4NMSUB;
        case OPC_F4NEG:
            return F4NEG;
        case OPC_F4ABS:
            return F4ABS;
	case OPC_F8CONST:
	    return F8CONST;
        case OPC_F8ADD:
            return F8ADD;
        case OPC_F8SUB:
            return F8SUB;
        case OPC_F8MPY:
            return F8MPY;
        case OPC_F8DIV:
            return F8DIV;
        case OPC_F8MADD:
            return F8MADD;
        case OPC_F8NMADD:
            return F8NMADD;
        case OPC_F8MSUB:
            return F8MSUB;
        case OPC_F8NMSUB:
            return F8NMSUB;
        case OPC_F8NEG:
            return F8NEG;
        case OPC_F8ABS:
            return F8ABS;
        case OPC_BF4LT:
            return BF4LT;
        case OPC_BF4LE:
            return BF4LE;
        case OPC_BF4EQ:
            return BF4EQ;
        case OPC_BF4NE:
            return BF4NE;
        case OPC_BF4GE:
            return BF4GE;
        case OPC_BF4GT:
            return BF4GT;
        case OPC_I4F4LT:
            return I4F4LT;
        case OPC_I4F4LE:
            return I4F4LE;
        case OPC_I4F4EQ:
            return I4F4EQ;
        case OPC_I4F4NE:
            return I4F4NE;
        case OPC_I4F4GE:
            return I4F4GE;
        case OPC_I4F4GT:
            return I4F4GT;
        case OPC_U4F4LT:
            return U4F4LT;
        case OPC_U4F4LE:
            return U4F4LE;
        case OPC_U4F4EQ:
            return U4F4EQ;
        case OPC_U4F4NE:
            return U4F4NE;
        case OPC_U4F4GE:
            return U4F4GE;
        case OPC_U4F4GT:
            return U4F4GT;
        case OPC_F4MAX:
            return F4MAX;
        case OPC_F4MIN:
            return F4MIN;
        case OPC_BF8LT:
            return BF8LT;
        case OPC_BF8LE:
            return BF8LE;
        case OPC_BF8EQ:
            return BF8EQ;
        case OPC_BF8NE:
            return BF8NE;
        case OPC_BF8GE:
            return BF8GE;
        case OPC_BF8GT:
            return BF8GT;
        case OPC_I4F8LT:
            return I4F8LT;
        case OPC_I4F8LE:
            return I4F8LE;
        case OPC_I4F8EQ:
            return I4F8EQ;
        case OPC_I4F8NE:
            return I4F8NE;
        case OPC_I4F8GE:
            return I4F8GE;
        case OPC_I4F8GT:
            return I4F8GT;
        case OPC_U4F8LT:
            return U4F8LT;
        case OPC_U4F8LE:
            return U4F8LE;
        case OPC_U4F8EQ:
            return U4F8EQ;
        case OPC_U4F8NE:
            return U4F8NE;
        case OPC_U4F8GE:
            return U4F8GE;
        case OPC_U4F8GT:
            return U4F8GT;
        case OPC_F8MAX:
            return F8MAX;
        case OPC_F8MIN:
            return F8MIN;
        case OPC_I1STID:
            return I1STID;
        case OPC_I2STID:
            return I2STID;
        case OPC_U1STID:
            return U1STID;
        case OPC_U2STID:
            return U2STID;
        case OPC_I4I1LDID:
            return I4I1LDID;
        case OPC_I4I2LDID:
            return I4I2LDID;
        case OPC_I4I4LDID:
            return I4I4LDID;
        case OPC_I4I8LDID:
            return I4I8LDID;
        case OPC_I4BSLDID:
            return I4BSLDID;
        case OPC_I4STID:
            return I4STID;
        case OPC_U4U1LDID:
            return U4U1LDID;
        case OPC_U4U2LDID:
            return U4U2LDID;
        case OPC_U4U4LDID:
            return U4U4LDID;
        case OPC_U4U8LDID:
            return U4U8LDID;
        case OPC_U4BSLDID:
            return U4BSLDID;
        case OPC_U4STID:
            return U4STID;
        case OPC_F4F4LDID:
            return F4F4LDID;
        case OPC_F4STID:
            return F4STID;
        case OPC_F8F8LDID:
            return F8F8LDID;
        case OPC_F8STID:
            return F8STID;
        case OPC_I4BCVT:
            return I4BCVT;
        case OPC_I4U4CVT:
            return I4U4CVT;
        case OPC_I4F4CVT:
            return I4F4CVT;
        case OPC_I4F8CVT:
            return I4F8CVT;
        case OPC_I4F4RND:
            return I4F4RND;
        case OPC_I4F4TRUNC:
            return I4F4TRUNC;
        case OPC_I4F4CEIL:
            return I4F4CEIL;
        case OPC_I4F4FLOOR:
            return I4F4FLOOR;
        case OPC_I4F8RND:
            return I4F8RND;
        case OPC_I4F8TRUNC:
            return I4F8TRUNC;
        case OPC_I4F8CEIL:
            return I4F8CEIL;
        case OPC_I4F8FLOOR:
            return I4F8FLOOR;
        case OPC_U4BCVT:
            return U4BCVT;
        case OPC_U4I4CVT:
            return U4I4CVT;
        case OPC_U4F4CVT:
            return U4F4CVT;
        case OPC_U4F8CVT:
            return U4F8CVT;
        case OPC_U4F4RND:
            return U4F4RND;
        case OPC_U4F4TRUNC:
            return U4F4TRUNC;
        case OPC_U4F4CEIL:
            return U4F4CEIL;
        case OPC_U4F4FLOOR:
            return U4F4FLOOR;
        case OPC_U4F8RND:
            return U4F8RND;
        case OPC_U4F8TRUNC:
            return U4F8TRUNC;
        case OPC_U4F8CEIL:
            return U4F8CEIL;
        case OPC_U4F8FLOOR:
            return U4F8FLOOR;
        case OPC_F4I4CVT:
            return F4I4CVT;
        case OPC_F4U4CVT:
            return F4U4CVT;
        case OPC_F4F8CVT:
            return F4F8CVT;
        case OPC_F8I4CVT:
            return F8I4CVT;
        case OPC_F8U4CVT:
            return F8U4CVT;
        case OPC_F8F4CVT:
            return F8F4CVT;
        case OPC_I8I1LDID:
            return I8I1LDID;
        case OPC_I8I2LDID:
            return I8I2LDID;
        case OPC_I8I4LDID:
            return I8I4LDID;
        case OPC_I8I8LDID:
            return I8I8LDID;
        case OPC_I8BSLDID:
            return I8BSLDID;
        case OPC_I8STID:
            return I8STID;
        case OPC_U8U1LDID:
            return U8U1LDID;
        case OPC_U8U2LDID:
            return U8U2LDID;
        case OPC_U8U4LDID:
            return U8U4LDID;
        case OPC_U8U8LDID:
            return U8U8LDID;
        case OPC_U8BSLDID:
            return U8BSLDID;
        case OPC_U8STID:
            return U8STID;
        case OPC_I8INTCONST:
            return I8INTCONST;
        case OPC_I8ABS:
            return I8ABS;
        case OPC_I8NEG:
            return I8NEG;
        case OPC_I8ADD:
            return I8ADD;
        case OPC_I8SUB:
            return I8SUB;
        case OPC_I8MPY:
            return I8MPY;
        case OPC_I8HIGHMPY:
            return I8HIGHMPY;
        case OPC_I8DIV:
            return I8DIV;
        case OPC_I8REM:
            return I8REM;
        case OPC_I8MOD:
            return I8MOD;
        case OPC_U8INTCONST:
            return U8INTCONST;
        case OPC_U8NEG:
            return U8NEG;
        case OPC_U8ADD:
            return U8ADD;
        case OPC_U8SUB:
            return U8SUB;
        case OPC_U8MPY:
            return U8MPY;
        case OPC_U8HIGHMPY:
            return U8HIGHMPY;
        case OPC_U8DIV:
            return U8DIV;
        case OPC_U8REM:
            return U8REM;
        case OPC_U8MOD:
            return U8MOD;
        case OPC_I8BIOR:
            return I8BIOR;
        case OPC_I8BNOT:
            return I8BNOT;
        case OPC_I8BAND:
            return I8BAND;
        case OPC_I8BXOR:
            return I8BXOR;
        case OPC_I8BNOR:
            return I8BNOR;
        case OPC_U8BNOT:
            return U8BNOT;
        case OPC_U8BIOR:
            return U8BIOR;
        case OPC_U8BAND:
            return U8BAND;
        case OPC_U8BXOR:
            return U8BXOR;
        case OPC_U8BNOR:
            return U8BNOR;
        case OPC_BI8LT:
            return BI8LT;
        case OPC_BI8LE:
            return BI8LE;
        case OPC_BI8EQ:
            return BI8EQ;
        case OPC_BI8NE:
            return BI8NE;
        case OPC_BI8GE:
            return BI8GE;
        case OPC_BI8GT:
            return BI8GT;
        case OPC_I4I8LT:
            return I4I8LT;
        case OPC_I4I8LE:
            return I4I8LE;
        case OPC_I4I8EQ:
            return I4I8EQ;
        case OPC_I4I8NE:
            return I4I8NE;
        case OPC_I4I8GE:
            return I4I8GE;
        case OPC_I4I8GT:
            return I4I8GT;
        case OPC_U4I8LT:
            return U4I8LT;
        case OPC_U4I8LE:
            return U4I8LE;
        case OPC_U4I8EQ:
            return U4I8EQ;
        case OPC_U4I8NE:
            return U4I8NE;
        case OPC_U4I8GE:
            return U4I8GE;
        case OPC_U4I8GT:
            return U4I8GT;
        case OPC_I8I8LT:
            return I8I8LT;
        case OPC_I8I8LE:
            return I8I8LE;
        case OPC_I8I8EQ:
            return I8I8EQ;
        case OPC_I8I8NE:
            return I8I8NE;
        case OPC_I8I8GE:
            return I8I8GE;
        case OPC_I8I8GT:
            return I8I8GT;
        case OPC_U8I8LT:
            return U8I8LT;
        case OPC_U8I8LE:
            return U8I8LE;
        case OPC_U8I8EQ:
            return U8I8EQ;
        case OPC_U8I8NE:
            return U8I8NE;
        case OPC_U8I8GE:
            return U8I8GE;
        case OPC_U8I8GT:
            return U8I8GT;
        case OPC_I8MAX:
            return I8MAX;
        case OPC_I8MIN:
            return I8MIN;
        case OPC_BU8LT:
            return BU8LT;
        case OPC_BU8LE:
            return BU8LE;
        case OPC_BU8EQ:
            return BU8EQ;
        case OPC_BU8NE:
            return BU8NE;
        case OPC_BU8GE:
            return BU8GE;
        case OPC_BU8GT:
            return BU8GT;
        case OPC_I4U8LT:
            return I4U8LT;
        case OPC_I4U8LE:
            return I4U8LE;
        case OPC_I4U8EQ:
            return I4U8EQ;
        case OPC_I4U8NE:
            return I4U8NE;
        case OPC_I4U8GE:
            return I4U8GE;
        case OPC_I4U8GT:
            return I4U8GT;
        case OPC_U4U8LT:
            return U4U8LT;
        case OPC_U4U8LE:
            return U4U8LE;
        case OPC_U4U8EQ:
            return U4U8EQ;
        case OPC_U4U8NE:
            return U4U8NE;
        case OPC_U4U8GE:
            return U4U8GE;
        case OPC_U4U8GT:
            return U4U8GT;
        case OPC_I8U8LT:
            return I8U8LT;
        case OPC_I8U8LE:
            return I8U8LE;
        case OPC_I8U8EQ:
            return I8U8EQ;
        case OPC_I8U8NE:
            return I8U8NE;
        case OPC_I8U8GE:
            return I8U8GE;
        case OPC_I8U8GT:
            return I8U8GT;
        case OPC_U8U8LT:
            return U8U8LT;
        case OPC_U8U8LE:
            return U8U8LE;
        case OPC_U8U8EQ:
            return U8U8EQ;
        case OPC_U8U8NE:
            return U8U8NE;
        case OPC_U8U8GE:
            return U8U8GE;
        case OPC_U8U8GT:
            return U8U8GT;
        case OPC_U8MAX:
            return U8MAX;
        case OPC_U8MIN:
            return U8MIN;
        case OPC_I8I4LT:
            return I8I4LT;
        case OPC_I8I4LE:
            return I8I4LE;
        case OPC_I8I4EQ:
            return I8I4EQ;
        case OPC_I8I4NE:
            return I8I4NE;
        case OPC_I8I4GE:
            return I8I4GE;
        case OPC_I8I4GT:
            return I8I4GT;
        case OPC_U8I4LT:
            return U8I4LT;
        case OPC_U8I4LE:
            return U8I4LE;
        case OPC_U8I4EQ:
            return U8I4EQ;
        case OPC_U8I4NE:
            return U8I4NE;
        case OPC_U8I4GE:
            return U8I4GE;
        case OPC_U8I4GT:
            return U8I4GT;
        case OPC_I8U4LT:
            return I8U4LT;
        case OPC_I8U4LE:
            return I8U4LE;
        case OPC_I8U4EQ:
            return I8U4EQ;
        case OPC_I8U4NE:
            return I8U4NE;
        case OPC_I8U4GE:
            return I8U4GE;
        case OPC_I8U4GT:
            return I8U4GT;
        case OPC_U8U4LT:
            return U8U4LT;
        case OPC_U8U4LE:
            return U8U4LE;
        case OPC_U8U4EQ:
            return U8U4EQ;
        case OPC_U8U4NE:
            return U8U4NE;
        case OPC_U8U4GE:
            return U8U4GE;
        case OPC_U8U4GT:
            return U8U4GT;
        case OPC_I8SHL:
            return I8SHL;
        case OPC_I8ASHR:
            return I8ASHR;
        case OPC_I8LSHR:
            return I8LSHR;
        case OPC_U8SHL:
            return U8SHL;
        case OPC_U8ASHR:
            return U8ASHR;
        case OPC_U8LSHR:
            return U8LSHR;
        case OPC_I8F4LT:
            return I8F4LT;
        case OPC_I8F4LE:
            return I8F4LE;
        case OPC_I8F4EQ:
            return I8F4EQ;
        case OPC_I8F4NE:
            return I8F4NE;
        case OPC_I8F4GE:
            return I8F4GE;
        case OPC_I8F4GT:
            return I8F4GT;
        case OPC_U8F4LT:
            return U8F4LT;
        case OPC_U8F4LE:
            return U8F4LE;
        case OPC_U8F4EQ:
            return U8F4EQ;
        case OPC_U8F4NE:
            return U8F4NE;
        case OPC_U8F4GE:
            return U8F4GE;
        case OPC_U8F4GT:
            return U8F4GT;
        case OPC_I8F8LT:
            return I8F8LT;
        case OPC_I8F8LE:
            return I8F8LE;
        case OPC_I8F8EQ:
            return I8F8EQ;
        case OPC_I8F8NE:
            return I8F8NE;
        case OPC_I8F8GE:
            return I8F8GE;
        case OPC_I8F8GT:
            return I8F8GT;
        case OPC_U8F8LT:
            return U8F8LT;
        case OPC_U8F8LE:
            return U8F8LE;
        case OPC_U8F8EQ:
            return U8F8EQ;
        case OPC_U8F8NE:
            return U8F8NE;
        case OPC_U8F8GE:
            return U8F8GE;
        case OPC_U8F8GT:
            return U8F8GT;

        case OPC_I4I8CVT:
            return I4I8CVT;
	case OPC_I4U8CVT:
	    return I4U8CVT;
	case OPC_U4I8CVT:
	    return U4I8CVT;
        case OPC_U4U8CVT:
            return U4U8CVT;
        case OPC_I8I4CVT:
            return I8I4CVT;
	case OPC_I8U4CVT:
	    return I8U4CVT;
	case OPC_U8I4CVT:
	    return U8I4CVT;
        case OPC_U8U4CVT:
            return U8U4CVT;
        case OPC_I8U8CVT:
            return I8U8CVT;
        case OPC_U8I8CVT:
            return U8I8CVT;
        case OPC_I4CVTL:
            return I4CVTL;
        case OPC_I8CVTL:
            return I8CVTL;
        case OPC_U4CVTL:
            return U4CVTL;
        case OPC_U8CVTL:
            return U8CVTL;

        case OPC_F8U8CVT:
            return F8U8CVT;
        case OPC_F4U8CVT:
            return F4U8CVT;

        case OPC_I4PARM:
            return I4PARM;
        case OPC_U4PARM:
            return U4PARM;
        case OPC_I8PARM:
            return I8PARM;
        case OPC_U8PARM:
            return U8PARM;
        case OPC_F4PARM:
            return F4PARM;
        case OPC_F8PARM:
            return F8PARM;

	case OPC_I4I1ILOAD:
	case OPC_I4I2ILOAD:
	case OPC_I4I4ILOAD:
	    return I4ILOAD;
	case OPC_I8I1ILOAD:
	case OPC_I8I2ILOAD:
	case OPC_I8I4ILOAD:
	case OPC_I8I8ILOAD:
	    return I8ILOAD;
	case OPC_U4U1ILOAD:
	case OPC_U4U2ILOAD:
	case OPC_U4U4ILOAD:
	    return U4ILOAD;
	case OPC_U8U1ILOAD:
	case OPC_U8U2ILOAD:
	case OPC_U8U4ILOAD:
	case OPC_U8U8ILOAD:
	    return U8ILOAD;
	case OPC_F4F4ILOAD:
	    return F4ILOAD;
	case OPC_F8F8ILOAD:
	    return F8ILOAD;

	case OPC_I1ISTORE:
	case OPC_I2ISTORE:
	case OPC_I4ISTORE:
	    return I4ISTORE;
	case OPC_U1ISTORE:
	case OPC_U2ISTORE:
	case OPC_U4ISTORE:
	    return U4ISTORE;
	case OPC_I8ISTORE:
	    return I8ISTORE;
	case OPC_U8ISTORE:
	    return U8ISTORE;
	case OPC_F4ISTORE:
	    return F4ISTORE;
	case OPC_F8ISTORE:
	    return F8ISTORE;

	case OPC_I4COMPOSE_BITS:
	    return I4COMPOSE_BITS;
	case OPC_U4COMPOSE_BITS:
	    return U4COMPOSE_BITS;
	case OPC_I8COMPOSE_BITS:
	    return I8COMPOSE_BITS;
	case OPC_U8COMPOSE_BITS:
	    return U8COMPOSE_BITS;
	case OPC_I4EXTRACT_BITS:
	    return I4EXTRACT_BITS;
	case OPC_U4EXTRACT_BITS:
	    return U4EXTRACT_BITS;
	case OPC_I8EXTRACT_BITS:
	    return I8EXTRACT_BITS;
	case OPC_U8EXTRACT_BITS:
	    return U8EXTRACT_BITS;

	case OPC_I4DIVREM:
	    return I4DIVREM;
	case OPC_U4DIVREM:
	    return U4DIVREM;
	case OPC_I8DIVREM:
	    return I8DIVREM;
	case OPC_U8DIVREM:
	    return U8DIVREM;

	case OPC_I4DIVPART:
	    return I4DIVPART;
	case OPC_U4DIVPART:
	    return U4DIVPART;
	case OPC_I4REMPART:
	    return I4REMPART;
	case OPC_U4REMPART:
	    return U4REMPART;

	case OPC_F4RECIP:
	    return F4RECIP;
	case OPC_F8RECIP:
	    return F8RECIP;

	case OPC_I4MAXPART:
	    return I4MAXPART;
	case OPC_U4MAXPART:
	    return U4MAXPART;
	case OPC_I8MAXPART:
	    return I8MAXPART;
	case OPC_U8MAXPART:
	    return U8MAXPART;
	case OPC_F4MAXPART:
	    return F4MAXPART;
	case OPC_F8MAXPART:
	    return F8MAXPART;

	case OPC_I4MINPART:
	    return I4MINPART;
	case OPC_U4MINPART:
	    return U4MINPART;
	case OPC_I8MINPART:
	    return I8MINPART;
	case OPC_U8MINPART:
	    return U8MINPART;
	case OPC_F4MINPART:
	    return F4MINPART;
	case OPC_F8MINPART:
	    return F8MINPART;

	case OPC_I4MINMAX:
	    return I4MINMAX;
	case OPC_U4MINMAX:
	    return U4MINMAX;
	case OPC_I8MINMAX:
	    return I8MINMAX;
	case OPC_U8MINMAX:
	    return U8MINMAX;
	case OPC_F4MINMAX:
	    return F4MINMAX;
	case OPC_F8MINMAX:
	    return F8MINMAX;

	case OPC_I4SELECT:
	    return I4SELECT;
	case OPC_U4SELECT:
	    return U4SELECT;
	case OPC_I8SELECT:
	    return I8SELECT;
	case OPC_U8SELECT:
	    return U8SELECT;
	case OPC_F4SELECT:
	    return F4SELECT;
	case OPC_F8SELECT:
	    return F8SELECT;

	case OPC_U4ALLOCA:
	    return U4ALLOCA;
	case OPC_U8ALLOCA:
	    return U8ALLOCA;
	case OPC_DEALLOCA:
	    return DEALLOCA;

	case OPC_U4LDA:
	    return U4LDA;
	case OPC_U4LDA_LABEL:
	    return U4LDA_LABEL;

	case OPC_I1INTRINSIC_OP:
	case OPC_I2INTRINSIC_OP:
	case OPC_I4INTRINSIC_OP:
	    return I4INTRINSIC_OP;
	case OPC_U1INTRINSIC_OP:
	case OPC_U2INTRINSIC_OP:
	case OPC_U4INTRINSIC_OP:
	    return U4INTRINSIC_OP;
	case OPC_I8INTRINSIC_OP:
	    return I8INTRINSIC_OP;
	case OPC_U8INTRINSIC_OP:
	    return U8INTRINSIC_OP;
	case OPC_F4INTRINSIC_OP:
	    return F4INTRINSIC_OP;
	case OPC_F8INTRINSIC_OP:
	    return F8INTRINSIC_OP;


	case OPC_PREFETCH:
	    return PREFETCH;
	case OPC_PREFETCHX:
	    return PREFETCHX;
    }
    char errMsg[128];
    sprintf(errMsg, "invalid opcode in OPC2OP %s",  OPCODE_name(opc));
    FmtAssert(FALSE, (errMsg));
    return -1;
}

#endif





void Handle_Cond_Branch(TOP top_branch, TOP top_cmp, TN *dest, TN *src1, TN *src2, OPS *ops)
{
  BB* bb_entry      = Cur_BB;
  BB* bb_set_true   = Gen_And_Append_BB(bb_entry);
  BB* bb_exit       = Gen_And_Append_BB(bb_set_true);
  const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);
  
  
  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = label_bb_exit;
  
  Build_OP(TOP_li, dest, Gen_Literal_TN(1, 4), ops);
  Build_OP(top_cmp, Gen_CR_TN(0) , src1, src2, ops);
  Build_OP(top_branch, Gen_CR_TN(0) , Gen_Label_TN(label_bb_exit, 0), ops);

  if( ops != &New_OPs )
      OPS_Append_Ops(&New_OPs, ops);

  Process_New_OPs();
  BB_Append_Ops(bb_entry, &New_OPs);
  OPS_Init(&New_OPs);
  OPS_Init(ops);

  OPS* bb_set_ops = &New_OPs;
  Build_OP(TOP_li, dest, Gen_Literal_TN(0, 4), bb_set_ops);
  total_bb_insts = 0;
  Last_Processed_OP = NULL;
  Process_New_OPs();
  BB_Append_Ops(bb_set_true, bb_set_ops);
  OPS_Init(bb_set_ops);

  total_bb_insts = 0;
  Last_Processed_OP = NULL;
  Process_New_OPs();
  OPS_Init(&New_OPs);
  Cur_BB = bb_exit;

}

void Expand_Load_Address(WN * expr, TN ** p_base_tn, TN ** p_ofst_tn, OPS * ops)
{
    ST    * base_sym;
    INT64   base_ofst;
    TN    * tmp_tn;            
    ST    * sym  = WN_st(expr);
    INT64   ofst = WN_store_offset(expr);
            
    Allocate_Object(sym);         /* make sure sym is allocated */
    Base_Symbol_And_Offset_For_Addressing(sym, ofst, &base_sym, &base_ofst);
            
    FmtAssert((base_sym == SP_Sym || base_sym == FP_Sym), ("Only handle SP_Sym and FP_Sym now"));
    *p_base_tn = (base_sym == SP_Sym) ? SP_TN : FP_TN;
    if (sym == base_sym) {
        // can have direct reference to SP or FP,
        // e.g. if actual stored to stack.
        if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
            *p_ofst_tn = Gen_Literal_TN (base_ofst, 4);
        }
        else {
            tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
            Build_OP(TOP_lis, tmp_tn, 
                Gen_Literal_TN((short)(base_ofst >> 16), 4), ops);
            Build_OP(TOP_add, tmp_tn, tmp_tn, *p_base_tn, ops);
            *p_base_tn = tmp_tn;
            *p_ofst_tn = Gen_Literal_TN ((short)(base_ofst), 4);
        }
    }
    else {
        /* Because we'd like to see symbol name in .s file, 
         * still reference the symbol rather than the sp/fp base.  
         * Do put in the offset from the symbol.  
         * We put the symbol in the TN and then
         * let cgemit replace symbol with the final offset.
         * We generate a SW reg, <sym>, <SP> rather than SW reg,<sym>
         * because cgemit and others expect a separate tn for the
         * offset and base. 
         */
        if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
            *p_ofst_tn = Gen_Symbol_TN (sym, ofst, 0);
        }
        else {
            tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
            Build_OP(TOP_lis, tmp_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_HIGH16), ops);
            Build_OP(TOP_add, tmp_tn, tmp_tn, *p_base_tn, ops);
            *p_base_tn = tmp_tn;
            *p_ofst_tn = Gen_Symbol_TN(sym, ofst, TN_RELOC_LOW16);
        }
    }
}

TN * Handle_PREG_Load(WN * ldid, TN * result, OPS *ops)
{
    TN *ldid_result = PREG_To_TN (WN_st(ldid), WN_load_offset(ldid));
    if (result == NULL) {
        result = ldid_result;
    }
    else {
#ifdef EMULATE_LONGLONG
        extern void
            Expand_Copy (TN *result, TN *src, TYPE_ID mtype, OPS *ops);
        TYPE_ID mtype =  ST_mtype(WN_st(ldid));
        if (mtype == MTYPE_I8 || mtype == MTYPE_U8) {
            Expand_Copy (result, ldid_result, mtype, &New_OPs);
        } else {
            Exp_COPY (result, ldid_result, &New_OPs);
        }
#else
        Exp_COPY (result, ldid_result, &New_OPs);
#endif // EMULATE_LONGLONG
    }   
    return result;
}        

TN * Handle_Load(WN * ldid, TN * result, TOP top, OPS * ops)        
{
    if (ST_assigned_to_dedicated_preg(WN_st(ldid))) {
        // replace st with dedicated preg
        WN_offset(ldid) = Find_PREG_For_Symbol(WN_st(ldid));
        WN_st_idx(ldid) = ST_st_idx(MTYPE_To_PREG(ST_mtype(WN_st(ldid))));
    }

    if (WN_class(ldid) == CLASS_PREG) {
       return Handle_PREG_Load(ldid, result, ops);
    } else {
        ST    * base_sym;        
        TN    * base_tn;
        TN    * ofst_tn;        
        OPS     newops;
        OP    * op;
        
        OPS_Init(&newops);
        Expand_Load_Address(ldid, &base_tn, &ofst_tn, &newops);
        Build_OP(top, result, base_tn, ofst_tn, &newops);
     
        FOR_ALL_OPS_OPs (&newops, op) {
            if (ST_is_constant(WN_st(ldid)) && OP_load(op)) {
                Set_OP_no_alias(op);
            }        
        }    
        
        OPS_Append_Ops(ops, &newops);    
        return result;
    }
}

void Handle_PREG_Store(WN * stid, OPS *ops)
{
    WN * kid = WN_kid0(stid);
    
    TN * result = PREG_To_TN (WN_st(stid), WN_store_offset(stid));
    Expand_Expr(kid, stid, result);

    if (In_Glue_Region) {
        if ( Trace_REGION_Interface ) {
            fprintf(TFile,"set op_glue on preg store in bb %d\n", BB_id(Cur_BB));
        }
        Set_OP_glue(OPS_last(&New_OPs));
    }

   /* If the child is a PREG and has a corresponding TN, it was part of
    * a DIVREM or MTYPE_B pair. We need to create a correspondence for the 
    * STID, and do an assignment
    */
    if (WN_operator_is(kid, OPR_LDID) && WN_class(kid) == CLASS_PREG) {
        TN *ldidTN = PREG_To_TN (WN_st(kid), WN_load_offset(kid));

        TN *ldidTN2 = TN_CORRESPOND_Lookup(ldidTN);
        if (ldidTN2 != NULL) {
            TN *stidTN = TN_CORRESPOND_Get(result, kid);
            Exp_COPY(stidTN, ldidTN2, &New_OPs);
        } else if (Is_Predicate_REGISTER_CLASS(TN_register_class(ldidTN))) {
            Is_True(Is_Predicate_REGISTER_CLASS(TN_register_class(result)),
                ("result should be predicate register class"));
            PREG_NUM cpreg_num = WN_load_offset(kid) + 1;
            TN *ctn = PREG_To_TN_Array[cpreg_num];
            PREG_NUM cresult_num = WN_store_offset(stid) + 1;
            TN *cresult = PREG_To_TN_Array[cresult_num];
            Exp_COPY (cresult, ctn, &New_OPs);
        }
    }
}
        
void Handle_Store(WN * stid, TN * result, TOP top, OPS * ops)        
{
    if (ST_assigned_to_dedicated_preg(WN_st(stid))) {
        // replace st with dedicated preg
        WN_offset(stid) = Find_PREG_For_Symbol(WN_st(stid));
        WN_st_idx(stid) = ST_st_idx(MTYPE_To_PREG(ST_mtype(WN_st(stid))));
    }

    if (WN_class(stid) == CLASS_PREG) {
       Handle_PREG_Store(stid, ops);
    } else {
        ST    * base_sym;        
        TN    * base_tn;
        TN    * ofst_tn;        
        OPS     newops;
        OP    * op;
        
        OPS_Init(&newops);
        Expand_Load_Address(stid, &base_tn, &ofst_tn, &newops);
        Build_OP(top, result, base_tn, ofst_tn, &newops);
        OPS_Append_Ops(ops, &newops);        
    }
} 

TN * Handle_Float_Load(WN * fld, TN * result, TOP top, OPS * ops)        
{ 
    TN  * base_tn;
    TN  * ofst_tn;
    OPS   newops;
    OP  * op;
    
    OPS_Init(&newops);
    Expand_Load_Address(fld, &base_tn, &ofst_tn, &newops);    
    Build_OP(top, result, base_tn, ofst_tn, &newops);
    
    FOR_ALL_OPS_OPs (&newops, op) {
        if (ST_is_constant(WN_st(fld)) && OP_load(op)) {
            Set_OP_no_alias(op);
        }
    }
    
    /* Add the new OPs to the end of the list passed in */
    OPS_Append_Ops(ops, &newops);     
    return result;
}

void Handle_Float_Store(WN * fst, TN * result, TOP top, OPS * ops)        
{ 
    TN * base_tn;
    TN * ofst_tn;
    OPS  newops;
    
    OPS_Init(&newops);            
    Expand_Load_Address(fst, &base_tn, &ofst_tn, &newops);            
    Build_OP(top, result, base_tn, ofst_tn, &newops);    
    OPS_Append_Ops(ops, &newops); 
}

void Handle_Imm32(TN* dest, INT64 imm, OPS* ops)
{
    short low = (short)(imm & 0xffff);
    short high = (short)(imm >> 16);
    if (low == 0)
        Build_OP(TOP_lis, dest, Gen_Literal_TN(high, 4), ops);
    else
    {
        high += low < 0 ? 1 : 0;
        Build_OP(TOP_li, dest, Gen_Literal_TN(low, 4), ops);
        if (high != 0)
            Build_OP(TOP_addis, dest, dest, Gen_Literal_TN(high, 4), ops);
    }
}
void Handle_Add_Imm(TN* dest, TN* src, INT64 imm, OPS * ops)
{
    short low = (short)imm;
    short high = (short)(imm >> 16);
    if (low == 0)
        Build_OP(TOP_addis, dest, src, Gen_Literal_TN(high, 4), ops);
    else
    {
        high += low < 0 ? 1 : 0;
        Build_OP(TOP_addi, dest, src, Gen_Literal_TN(low, 4), ops);
        if (high != 0)
            Build_OP(TOP_addis, dest, src, Gen_Literal_TN(high, 4), ops);
    }
}

void Handle_Logic_Imm(TOP inst_low, TOP inst_high, TN* dest, TN* src, INT64 imm, OPS *ops)
{
    if (IN_RANGE(imm, UIMM16))
        Build_OP(inst_low, dest, src, Gen_Literal_TN(imm, 4), ops);
    else
    {
        unsigned short low = (unsigned short)imm;
        unsigned short high = (unsigned short)(imm >> 16);
        if (low == 0)
        {
            Build_OP(inst_high, dest, src, Gen_Literal_TN(high, 4), ops);
        }
        else
        {
            FmtAssert(FALSE, ("bad imm"));
        }
        
    }
}

void Handle_Cond_Move_Int(OPERATOR opr, TOP top_cmp, TN *dest, TN *src1, TN *src2, OPS *ops)
{
    TN * res = dest;
    dest = Dup_TN_Even_If_Dedicated(res);
    
    Build_OP(top_cmp, Gen_CR_TN(0) /*Gen_Literal_TN(0, 4)*/, src1, src2, ops);
    Build_OP(TOP_mfcr, dest, Gen_CR_TN(0), ops);
    switch (opr)
    {
        case OPR_GT:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(2, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_GE:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_xori, dest, dest, Gen_Literal_TN(1, 4), ops);
            break;
        case OPR_EQ:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(3, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_NE:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(3, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_xori, dest, dest, Gen_Literal_TN(1, 4), ops);
            break;
        case OPR_LE:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(2, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_xori, dest, dest, Gen_Literal_TN(1, 4), ops);
            break;
        case OPR_LT:
            Build_OP(TOP_rlwinm, dest, dest, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
    }
    Exp_COPY(res, dest, ops);
}

void Handle_MaxMin_Int(bool max, TN* dest, TN* src1, TN* src2, OPS* ops)
{
    TN* tn = Build_TN_Of_Mtype(MTYPE_I4);
    Build_OP(TOP_xoris, tn, src2, Gen_Literal_TN(0x8000, 4), ops);
    Build_OP(TOP_xoris, dest, src1, Gen_Literal_TN(0x8000, 4), ops);
    Build_OP(TOP_subfc, dest, dest, tn, ops);
    Build_OP(TOP_subfe, tn, tn, tn, ops);
    if (max)
        Build_OP(TOP_andc, dest, dest, tn, ops);
    else
        Build_OP(TOP_and, dest, dest, tn, ops);
    Build_OP(TOP_add, dest, dest, src1, ops);
}

void Handle_MaxMin_Uint(bool max, TN* dest, TN* src1, TN* src2, OPS* ops)
{
    TN* tn = Build_TN_Of_Mtype(MTYPE_U4);
    Build_OP(TOP_subfc, tn, src1, src2, ops);
    Build_OP(TOP_subfe, dest, src2, src2, ops);
    if (max)
        Build_OP(TOP_andc, tn, tn, dest, ops);
    else
        Build_OP(TOP_and, tn, tn, dest, ops);
    Build_OP(TOP_add, dest, src1, tn, ops);
}

void Handle_Cond_Move_Float(OPERATOR opr, TN *dest, TN *src1, TN *src2, OPS *ops)
{
    TN * cr = Gen_CR_TN(1);
    Build_OP(TOP_fcmpu,  cr, src1, src2, ops);
    TN* tn = Build_TN_Like(dest);
    switch (opr)
    {
        case OPR_GT:
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(6, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), dest, ops);
            break;
        case OPR_GE:
	     Build_OP(TOP_cror, cr, Gen_Literal_TN(0, 4), Gen_Literal_TN(5, 4), Gen_Literal_TN(6, 4), cr, cr, ops);
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_EQ:
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(7, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_NE:
	     Build_OP(TOP_cror, cr, Gen_Literal_TN(0, 4), Gen_Literal_TN(4, 4), Gen_Literal_TN(5, 4), cr, cr, ops);
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_LE:
            Build_OP(TOP_cror, cr, Gen_Literal_TN(0, 4), Gen_Literal_TN(4, 4), Gen_Literal_TN(6, 4), cr, cr, ops);
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
        case OPR_LT:
            Build_OP(TOP_mfcr, tn, cr, ops);
            Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(5, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
            break;
    }
}

void Handle_MaxMin_Float(bool max, TYPE_ID mtype, TN* dest, TN* src1, TN* src2, OPS* ops)
{
    TN* tn = Build_TN_Of_Mtype (mtype);
    if (mtype == MTYPE_F4)
        Build_OP(TOP_fsubs, tn, src1, src2, ops);
    else
        Build_OP(TOP_fsub, tn, src1, src2, ops);
    
    if (max)
        Build_OP(TOP_fsel, dest, tn, src1, src2, ops);
    else
        Build_OP(TOP_fsel, dest, tn, src2, src1, ops);
}

void Handle_Float_Int_Cvt(ROUND_MODE rm, TYPE_ID srcType, TN* dest, TN* src, OPS* ops)
{
    OP * st = OPS_last(ops);
    TN* tn = Build_TN_Of_Mtype(srcType);
    if (rm == ROUND_USER) {
        Build_OP(TOP_fctiw, tn, src, ops);
    } else if (rm == ROUND_CHOP) {
        Build_OP(TOP_fctiwz, tn, src, ops);
    } else {
        TN* savefr = Build_TN_Of_Mtype(MTYPE_F8);
        Build_OP(TOP_mffs, savefr, ops);
        switch (rm)
        {
        case ROUND_NEAREST:
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, tn, src, ops);
            break;
        case ROUND_PLUS_INF:
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, tn, src, ops);
        break;
        case ROUND_NEG_INF:
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, tn, src, ops);
        break;
        }
        Build_OP(TOP_mtfsf, Gen_Literal_TN(255, 4), savefr, ops);
    }
    
    ST* sym = CGSPILL_Get_TN_Spill_Location(tn, CGSPILL_LCL);
    INT64 ofst = TN_offset(tn);
    ST* base_sym;
    INT64 base_ofst;
    Base_Symbol_And_Offset_For_Addressing(sym, 0, &base_sym, &base_ofst);
    Exp_Store(MTYPE_F8, tn, sym, 0, ops, V_NONE);
    Exp_Load(MTYPE_I4, MTYPE_I4, dest, sym, 4, ops, V_NONE);

    if (st == NULL) 
      st = OPS_first(ops);

    for (; st != NULL; st = OP_next(st)) 
      Set_OP_no_move_before_gra(st);    
}

extern void Expand_Copy (TN *result_l, TN *src_l, TYPE_ID mtype, OPS *ops);
/*
    # FR0 = 0.0
    # FR1 = value to be converted
    # FR3 = 2^32 - 1
    # FR4 = 2^31
    # R3 = returned result
    # disp = displacement from R1
    fsel FR2,FR1,FR1,FR0 # use 0 if < 0
    fsub FR5,FR3,FR1 # use 2^32-1 if >= 2^32
    fsel FR2,FR5,FR2,FR3
    fsub FR5,FR2,FR4 # subtract 2**31
    fcmpu cr2,FR2,FR4
    fsel FR2,FR5,FR5,FR2 # use diff if >= 2^31
    # next part same as conversion to
    # signed integer word
    fctiw[z] FR2,FR2 # convert to integer
    stfd FR2,disp(R1) # copy unmodified to memory
    lwz R3,disp+4(R1) # load low-order word
    blt cr2,$+8 # add 2^31 if input
    xoris R3,R3,0x8000 # was >= 2^31
*/
void Handle_Float_Uint_Cvt(ROUND_MODE rm, TYPE_ID srcType, TN* dest, TN* src, OPS* ops)
{
    OP * st = OPS_last(ops);

    TN* fr0 = Build_TN_Of_Mtype(MTYPE_F8);
    
    Build_OP(TOP_fsub, fr0, fr0, fr0, ops);
    
    TCON tcon3 = Host_To_Targ_Float(MTYPE_F8, 4294967295.0); // 2^32 - 1
    TY_IDX ty_idx3 = MTYPE_F8 << 8;
    ST * st3   = New_Const_Sym(Enter_tcon(tcon3), ty_idx3);
    WN * wn3   = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx3), MTYPE_V, st3);  
    TN * fr3  = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);  
    Exp_Load(MTYPE_F8, MTYPE_F8, fr3, st3, 0, ops, 0); // load magic value

    TCON tcon4 = Host_To_Targ_Float(MTYPE_F8, 2147483648.0); // 2^31
    TY_IDX ty_idx4 = MTYPE_F8 << 8;
    ST * st4   = New_Const_Sym(Enter_tcon(tcon4), ty_idx4);
    WN * wn4   = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx4), MTYPE_V, st4);  
    TN * fr4  = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);  
    Exp_Load(MTYPE_F8, MTYPE_F8, fr4, st4, 0, ops, 0); // load magic value

    TN* fr1 = src;
    TN* fr2 = Build_TN_Of_Mtype(MTYPE_F8);
    TN* fr5 = Build_TN_Of_Mtype(MTYPE_F8);

    Build_OP(TOP_fsel, fr2, fr1, fr1, fr0, ops);
    Build_OP(TOP_fsub, fr5, fr3, fr1, ops);
    Build_OP(TOP_fsel, fr2, fr5, fr2, fr3, ops);
    Build_OP(TOP_fsub, fr5, fr2, fr4, ops);
    TN* cr = Gen_CR_TN(0);
    Build_OP(TOP_fcmpu, cr, fr2, fr4, ops);
    TN* tn1 = Build_TN_Like(dest);
    TN* tn2 = Build_TN_Like(dest);
    TN* tn3 = Build_TN_Like(dest);
    Build_OP(TOP_mfcr, tn1, cr, ops);
    Build_OP(TOP_rlwinm, tn2, tn1, Gen_Literal_TN(0, 4), Gen_Literal_TN(0, 4), Gen_Literal_TN(0, 4), ops);
    Build_OP(TOP_xoris, tn3, tn2, Gen_Literal_TN(0x8000, 4), ops);
    Build_OP(TOP_fsel, fr2, fr5, fr5, fr2, ops);

    if (rm == ROUND_USER) {
        Build_OP(TOP_fctiw, fr2, fr2, ops);
    } else if (rm == ROUND_CHOP) {
        Build_OP(TOP_fctiwz,  fr2, fr2, ops);
    } else {
        TN* savefr = Build_TN_Of_Mtype(MTYPE_I8);
        Build_OP(TOP_mffs, savefr, src, ops);
        switch (rm)
        {
        case ROUND_NEAREST:
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, fr2, fr2, ops);
            break;
        case ROUND_PLUS_INF:
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb0, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, fr2, fr2, ops);
        break;
        case ROUND_NEG_INF:
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(30, 4), ops);
            Build_OP(TOP_mtfsb1, Gen_Literal_TN(31, 4), ops);
            Build_OP(TOP_fctiw, fr2, fr2, ops);
        break;
        }
        Build_OP(TOP_mtfsf, Gen_Literal_TN(255, 4), savefr, ops);
    }
    
    ST* sym = CGSPILL_Get_TN_Spill_Location(fr2, CGSPILL_LCL);
    INT64 ofst = TN_offset(fr2);
    ST* base_sym;
    INT64 base_ofst;
    Base_Symbol_And_Offset_For_Addressing(sym, 0, &base_sym, &base_ofst);
    Exp_Store(MTYPE_F8, fr2, sym, 0, ops, V_NONE);
    TN* tn = Build_TN_Like(dest);
    Exp_Load(MTYPE_I4, MTYPE_I4, tn, sym, 4, ops, V_NONE);
    Build_OP(TOP_xor, dest, tn, tn3, ops);

    if (st == NULL) 
      st = OPS_first(ops);

    for (; st != NULL; st = OP_next(st)) 
      Set_OP_no_move_before_gra(st);  
    
}

extern void Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID rtype, TYPE_ID desc, OPS *ops);
/*         
    gpr0 -> fpr0    
   
    lis 11,.LC0@ha
    la 11,.LC0@l(11)
    lfd 13,0(11)

    lis 9,0x4330
    stw 9,16(31)
    xoris 0,0,0x8000
    stw 0,20(31)        
    
    lfd 0,16(31)
    
    fsub 0,0,13
    frsp 0,0   
*/
void Handle_Int_Float_Cvt(TN * dest, TN * src, OPS * ops, bool isUnsigned, bool isDouble)
{
#if HOST_IS_LITTLE_ENDIAN
    static unsigned char umagic[] = {0, 0, 0, 0, 0, 0, 0x30, 0x43};
    static unsigned char smagic[] = {0, 0, 0, 0x80, 0, 0, 0x30, 0x43};
#else
    static unsigned char umagic[] = {0x43, 0x30, 0, 0, 0, 0, 0, 0};
    static unsigned char smagic[] = {0x43, 0x30, 0, 0, 0x80, 0, 0, 0};    
#endif
    OP * opst = OPS_last(ops);
    TCON tcon;
    if (isUnsigned)
        tcon = Host_To_Targ_Float(MTYPE_F8, *(double *)umagic);
    else
        tcon = Host_To_Targ_Float(MTYPE_F8, *(double *)smagic);
    TY_IDX ty_idx = MTYPE_F8 << 8;
    ST * st   = New_Const_Sym(Enter_tcon(tcon), ty_idx);
    WN * wn   = WN_CreateConst(OPR_CONST, TY_mtype(ty_idx), MTYPE_V, st);  
    TN * tmp  = Gen_Register_TN(ISA_REGISTER_CLASS_float, 8);  
    Exp_Load(MTYPE_F8, MTYPE_F8, tmp, st, 0, ops, 0); // load magic value

    TN * tn   = Build_TN_Of_Mtype(MTYPE_F8);
    ST * sym1 = CGSPILL_Get_TN_Spill_Location(tn, CGSPILL_LCL);
    INT64 ofst;
    ST * sym;  
    Base_Symbol_And_Offset_For_Addressing(sym1, 0, &sym, &ofst);

    TN * tmp1   = Gen_Register_TN(ISA_REGISTER_CLASS_integer, Pointer_Size);  
    Build_OP(TOP_lis, tmp1, Gen_Literal_TN(0x4330, Pointer_Size), ops);
    Exp_Store(MTYPE_I4, tmp1, sym1, 0, ops, V_NONE);

    TN* tn1 = Build_TN_Of_Mtype(MTYPE_F8);
    if (!isUnsigned) {
        TN* tmp2 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4); 
        Build_OP(TOP_xoris, tmp2, src, Gen_Literal_TN(0x8000, Pointer_Size), ops);
        Exp_Store(MTYPE_I4, tmp2, sym1, 4, ops, V_NONE);
        Exp_Load(MTYPE_F8, MTYPE_F8, tn1, sym1, 0, ops, V_NONE);    
    } else {
        Exp_Store(MTYPE_I4, src, sym1, 4, ops, V_NONE);
        Exp_Load(MTYPE_F8, MTYPE_F8, tn1, sym1, 0, ops, V_NONE); 
    }

    
    if (!isDouble) {
        TN* tn2 = Build_TN_Like(dest);
        Build_OP(TOP_fsub, tn2, tn1, tmp, ops);
        Expand_Float_To_Float(dest, tn2, MTYPE_F4, MTYPE_F8, ops);
    } else {
        Build_OP(TOP_fsub, dest, tn1, tmp, ops);
    }

    if (opst == NULL) 
      opst = OPS_first(ops);

    for (; opst != NULL; opst = OP_next(opst)) 
      Set_OP_no_move_before_gra(opst);  // temp fix for local scheduler fails
}

// only handle ULL larger than 2^63 -1
/*
double:
	and tn0, low, 0x7FF
	cmp cr0, tn0, 0x400
	mfcr tn1
	rlwinm tn2, tn1, 2, 31, 31
	srwi tn3, low, 11
	add tn4, tn3, tn2
	rlwinm tn5, tn1, 3, 31, 31
	and tn6, tn3, tn5
	add tn7, tn4, tn6
	stw tn7
	rlwinm tn8, high, 21, 12, 31
	oris tn9, tn8, 0x43E0
	stw tn9
	lfd dest
float:
       and tn0, high, 0xFF
	cmp cr0, tn0, 0x80
	mfcr tn1
	rlwinm tn2, tn1, 2, 31, 31
	rlwinm tn3, high, 24, 9, 31
	add tn4, tn3, tn2
	rlwinm tn5, tn1, 3, 31, 31
	and tn6, tn3, tn5
	add tn7, tn4, tn6
	oris tn8, tn7, 0x5f00
	stw tn8
	lfs dest
*/
void Handle_ULonglong_Float_Cvt(TN* dest, TN* src_high, TN* src_low, OPS* ops, bool isDouble)
{
    OP * opst = OPS_last(ops);
    if (isDouble) {
        TN* tn0 = Build_TN_Like(src_low);
        TN* tn1 = Build_TN_Like(src_low);
        TN* tn2 = Build_TN_Like(src_low);
        TN* tn3 = Build_TN_Like(src_low);
        TN* tn4 = Build_TN_Like(src_low);
        TN* tn5 = Build_TN_Like(src_low);
        TN* tn6 = Build_TN_Like(src_low);
        TN* tn7 = Build_TN_Like(src_low);
        TN* tn8 = Build_TN_Like(src_low);
        TN* tn9 = Build_TN_Like(src_low);
        Build_OP(TOP_andi_, tn0, src_low, Gen_Literal_TN(0x7FF, 4), ops);
        TN* cr = Gen_CR_TN(0);
        Build_OP(TOP_cmplwi, cr, tn0, Gen_Literal_TN(0x400, 4), ops);
        Build_OP(TOP_mfcr, tn1, cr, ops);
        Build_OP(TOP_rlwinm, tn2, tn1, Gen_Literal_TN(2, 4),  Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_srwi, tn3, src_low, Gen_Literal_TN(11, 4), ops);
        Build_OP(TOP_add, tn4, tn3, tn5, ops);
        Build_OP(TOP_rlwinm, tn5, tn1, Gen_Literal_TN(3, 4),  Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_and, tn6, tn3, tn5, ops);
        Build_OP(TOP_add, tn7, tn4, tn6, ops);
        ST * sym1 = CGSPILL_Get_TN_Spill_Location(dest, CGSPILL_LCL);
        INT64 ofst;
        ST * sym;  
        Base_Symbol_And_Offset_For_Addressing(sym1, 0, &sym, &ofst);
        Exp_Store(MTYPE_I4, tn7, sym1, 4, ops, V_NONE);
        Build_OP(TOP_rlwinm, tn8, src_high, Gen_Literal_TN(21, 4),  Gen_Literal_TN(12, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_oris, tn9, tn8, Gen_Literal_TN(0x43E0, 4), ops);
        Exp_Store(MTYPE_I4, tn9, sym1, 0, ops, V_NONE);
        Exp_Load(MTYPE_F8, MTYPE_F8, dest, sym1, 0, ops, V_NONE);
    } else {
        TN* tn0 = Build_TN_Like(src_low);
        TN* tn1 = Build_TN_Like(src_low);
        TN* tn2 = Build_TN_Like(src_low);
        TN* tn3 = Build_TN_Like(src_low);
        TN* tn4 = Build_TN_Like(src_low);
        TN* tn5 = Build_TN_Like(src_low);
        TN* tn6 = Build_TN_Like(src_low);
        TN* tn7 = Build_TN_Like(src_low);
        TN* tn8 = Build_TN_Like(src_low);
        Build_OP(TOP_andi_, tn0, src_high, Gen_Literal_TN(0xFF, 4), ops);
        TN* cr = Gen_CR_TN(0);
        Build_OP(TOP_cmplwi, cr, tn0, Gen_Literal_TN(0x80, 4), ops);
        Build_OP(TOP_mfcr, tn1, cr, ops);
        Build_OP(TOP_rlwinm, tn2, tn1, Gen_Literal_TN(2, 4),  Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_rlwinm, tn3, src_high, Gen_Literal_TN(24, 4),  Gen_Literal_TN(9, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_add, tn4, tn3, tn5, ops);
        Build_OP(TOP_rlwinm, tn5, tn1, Gen_Literal_TN(3, 4),  Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
        Build_OP(TOP_and, tn6, tn3, tn5, ops);
        Build_OP(TOP_add, tn7, tn4, tn6, ops);
        Build_OP(TOP_oris, tn8, tn0, Gen_Literal_TN(0x5f00, 4), ops);
        ST * sym1 = CGSPILL_Get_TN_Spill_Location(dest, CGSPILL_LCL);
        INT64 ofst;
        ST * sym;  
        Base_Symbol_And_Offset_For_Addressing(sym1, 0, &sym, &ofst);
        Exp_Store(MTYPE_I4, tn8, sym1, 0, ops, V_NONE);
        Exp_Load(MTYPE_F4, MTYPE_F4, dest, sym1, 0, ops, V_NONE);
    }
    if (opst == NULL) 
      opst = OPS_first(ops);

    for (; opst != NULL; opst = OP_next(opst)) 
      Set_OP_no_move_before_gra(opst);
}

void Handle_Longlong_Ldst(WN* expr, TOP top, TN *low, TN* high, OPS* ops)
{
    ST    * base_sym;
    INT64   base_ofst;
    TN    * base_tn;
    TN    * tmp_tn;
    OPS     newops;
    OP    * op;
    ST    * sym  = WN_st(expr);
    INT64   ofst = WN_store_offset(expr);
    
    OPS_Init(&newops);   
    Allocate_Object(sym);         /* make sure sym is allocated */
    Base_Symbol_And_Offset_For_Addressing(sym, ofst, &base_sym, &base_ofst);
            
    FmtAssert((base_sym == SP_Sym || base_sym == FP_Sym), ("Only handle SP_Sym and FP_Sym now"));
    base_tn = (base_sym == SP_Sym) ? SP_TN : FP_TN;
    if (sym == base_sym) {
        // can have direct reference to SP or FP,
        // e.g. if actual stored to stack.
        if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
            Build_OP(top, high, base_tn, Gen_Literal_TN (base_ofst, 4), &newops);
            base_ofst += 4;
            if (ISA_LC_Value_In_Class(base_ofst, LC_simm16)) {
                Build_OP(top, low, base_tn, Gen_Literal_TN (base_ofst, 4), &newops);
            }
            else {
                tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
                Build_OP(TOP_lis, tmp_tn, Gen_Literal_TN((short)(base_ofst >> 16), 4), &newops);
                Build_OP(TOP_add, tmp_tn, tmp_tn, base_tn, &newops);
                base_tn = tmp_tn;
                Build_OP(top, low, base_tn, Gen_Literal_TN ((short)base_ofst, 4), &newops);
            }
        }
        else {
            tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
            Build_OP(TOP_lis, tmp_tn, Gen_Literal_TN((short)(base_ofst >> 16), 4), &newops);
            Build_OP(TOP_add, tmp_tn, tmp_tn, base_tn, &newops);
            base_tn = tmp_tn;
            Build_OP(top, high, base_tn, Gen_Literal_TN((short)base_ofst, 4), &newops);
            if ((base_ofst >> 16) != ((base_ofst + 4) >> 16)) {
                Build_OP(TOP_addi, base_tn, base_tn, Gen_Literal_TN(1, 4), &newops);
            }
            Build_OP(top, low, base_tn, Gen_Literal_TN((short)(base_ofst + 4), 4), &newops);
        }
    }
    else {
        /* Because we'd like to see symbol name in .s file, 
         * still reference the symbol rather than the sp/fp base.  
         * Do put in the offset from the symbol.  
         * We put the symbol in the TN and then
         * let cgemit replace symbol with the final offset.
         * We generate a SW reg, <sym>, <SP> rather than SW reg,<sym>
         * because cgemit and others expect a separate tn for the
         * offset and base. 
         */
        if (ISA_LC_Value_In_Class(base_ofst + ofst, LC_simm16)) {
            Build_OP(top, high, base_tn, Gen_Symbol_TN(sym, ofst, 0), &newops);
            ofst += 4;
            if (ISA_LC_Value_In_Class(base_ofst + ofst, LC_simm16)) {
                Build_OP(top, low, base_tn, Gen_Symbol_TN(sym, ofst, 0), &newops);
            }
            else {
                tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
                Build_OP(TOP_lis, tmp_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_HIGH16), &newops);
                Build_OP(TOP_add, tmp_tn, tmp_tn, base_tn, &newops);
                base_tn = tmp_tn;
                Build_OP(top, low, base_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_LOW16), &newops);
            }
        }
        else {
            tmp_tn = Build_TN_Of_Mtype(Pointer_Mtype);
            Build_OP(TOP_lis, tmp_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_HIGH16), &newops);
            Build_OP(TOP_add, tmp_tn, tmp_tn, base_tn, &newops);
            base_tn = tmp_tn;
            Build_OP(top, high, base_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_LOW16), &newops);
            if (((base_ofst + ofst) >> 16) != ((base_ofst + ofst + 4) >> 16)) {
                Build_OP(TOP_addi, base_tn, base_tn, Gen_Literal_TN(1, 4), &newops);
            }
            ofst += 4;
            Build_OP(top, low, base_tn, Gen_Symbol_TN(sym, ofst, TN_RELOC_LOW16), &newops);
            
        }
    }

    FOR_ALL_OPS_OPs (&newops, op) {
        if (ST_is_constant(WN_st(expr)) && OP_load(op)) {
            Set_OP_no_alias(op);
        }        
    }     
        
    OPS_Append_Ops(ops, &newops);
}

void Handle_Cond_Longlong(OPERATOR opr, TOP top_cmp, TN *dest, TN *src1_high, TN* src1_low, TN *src2_high,  TN *src2_low, OPS *ops)
{
    TN* cr0 = Gen_CR_TN(0);
    TN* cr1 = Gen_CR_TN(1);
    TN* cr2 = Gen_CR_TN(2);
    Build_OP(top_cmp, cr1, src1_high, src2_high, ops);
    Build_OP(TOP_cmplw, cr2, src1_low, src2_low, ops);
    switch (opr) {
        case OPR_EQ:
            Build_OP(TOP_crand, cr0, Gen_Literal_TN(0, 4), Gen_Literal_TN(6, 4), Gen_Literal_TN(10, 4), cr1, cr2, ops);
            break;
        case OPR_NE:
            Build_OP(TOP_crnand, cr0, Gen_Literal_TN(0, 4), Gen_Literal_TN(6, 4), Gen_Literal_TN(10, 4), cr1, cr2, ops);
            break;
        case OPR_GT:
        case OPR_LE:
            Build_OP(TOP_crand, cr0, Gen_Literal_TN(1, 4), Gen_Literal_TN(6, 4), Gen_Literal_TN(9, 4), cr1, cr2, ops);
            Build_OP(TOP_cror, cr0, Gen_Literal_TN(0, 4), Gen_Literal_TN(1, 4), Gen_Literal_TN(5, 4), cr0, cr1, ops);
            break;
        case OPR_LT:
        case OPR_GE:
            Build_OP(TOP_crand, cr0, Gen_Literal_TN(1, 4), Gen_Literal_TN(6, 4), Gen_Literal_TN(8, 4), cr1, cr2, ops);
            Build_OP(TOP_cror, cr0, Gen_Literal_TN(0, 4), Gen_Literal_TN(1, 4), Gen_Literal_TN(4, 4), cr0, cr1, ops);
            break;
        default:
            FmtAssert(false, ("Unknown OPR"));
    }
    TN* tn = Build_TN_Like(dest);
    Build_OP(TOP_mfcr, tn, cr0, ops);
    if (opr == OPR_LE || opr == OPR_GE) {
        TN* revert = Build_TN_Like(dest);
        Build_OP(TOP_nand, revert, tn, tn, ops);
        tn = revert;
    }
    Build_OP(TOP_rlwinm, dest, tn, Gen_Literal_TN(1, 4), Gen_Literal_TN(31, 4), Gen_Literal_TN(31, 4), ops);
}

void Handle_MaxMin_Longlong(bool max, TOP top_cmp, TN* dest_high, TN* dest_low, TN* src1_high, TN* src1_low, TN* src2_high, TN* src2_low, OPS* ops)
{
    TOP top_br1 = max ? TOP_bgt : TOP_blt;
    TOP top_br2 = max ? TOP_blt : TOP_bgt;
    TOP top_br3 = max ? TOP_bgt : TOP_blt;

    BB* bb_entry = Cur_BB;
    BB* bb_cmp_high = Gen_And_Append_BB(bb_entry);
    BB* bb_cmp_low = Gen_And_Append_BB(bb_cmp_high);
    BB* bb_assign2 = Gen_And_Append_BB(bb_cmp_low);
    BB* bb_assign1 = Gen_And_Append_BB(bb_assign2);
    BB* bb_exit =  Gen_And_Append_BB(bb_assign1);
    const LABEL_IDX label_bb_assign2 = Gen_Label_For_BB(bb_assign2);
    const LABEL_IDX label_bb_assign1 = Gen_Label_For_BB(bb_assign1);
    const LABEL_IDX label_bb_exit = Gen_Label_For_BB(bb_exit);

    BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_entry)) = NULL;
    WN_label_number(BB_branch_wn(bb_entry)) = label_bb_assign1;
    
    Build_OP(top_cmp, Gen_CR_TN(0) , src1_high, src2_high, ops);
    Build_OP(top_br1, Gen_CR_TN(0) , Gen_Label_TN(label_bb_assign1, 0), ops);
    
    if( ops != &New_OPs )
        OPS_Append_Ops(&New_OPs, ops);
    
    Process_New_OPs();
    BB_Append_Ops(bb_entry, &New_OPs);
    OPS_Init(&New_OPs);
    OPS_Init(ops);

    BB_branch_wn(bb_cmp_high) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_high)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_high)) = label_bb_assign2;
    
    OPS* bb_cmp_high_ops = &New_OPs;
    Build_OP(top_cmp, Gen_CR_TN(0) , src1_high, src2_high, ops);
    Build_OP(top_br2, Gen_CR_TN(0) , Gen_Label_TN(label_bb_assign2, 0), bb_cmp_high_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_high, bb_cmp_high_ops);
    OPS_Init(bb_cmp_high_ops);

    BB_branch_wn(bb_cmp_low) = WN_Create(OPC_TRUEBR,1);
    WN_kid0(BB_branch_wn(bb_cmp_low)) = NULL;
    WN_label_number(BB_branch_wn(bb_cmp_low)) = label_bb_assign1;
    
    OPS* bb_cmp_low_ops = &New_OPs;
    TN * cr = Gen_CR_TN(0);        
    Build_OP(TOP_cmplw, cr , src1_low, src2_low, bb_cmp_low_ops);
    Build_OP(top_br3, cr, Gen_Label_TN(label_bb_assign1, 0), bb_cmp_low_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_cmp_low, bb_cmp_low_ops);
    OPS_Init(bb_cmp_low_ops);

    BB_branch_wn(bb_assign2) = WN_Create(OPC_GOTO,0);
    WN_label_number(BB_branch_wn(bb_assign2)) = label_bb_exit;
    
    OPS* bb_assign2_ops = &New_OPs;
    Build_OP(TOP_mr,  dest_high, src2_high, bb_assign2_ops);
    Build_OP(TOP_mr,  dest_low, src2_low, bb_assign2_ops);
    Build_OP(TOP_b,  Gen_Label_TN(label_bb_exit, 0), bb_assign2_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_assign2, bb_assign2_ops);
    OPS_Init(bb_assign2_ops);

    OPS* bb_assign1_ops = &New_OPs;
    Build_OP(TOP_mr,  dest_high, src1_high, bb_assign1_ops);
    Build_OP(TOP_mr,  dest_low, src1_low, bb_assign1_ops);

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops(bb_assign1, bb_assign1_ops);
    OPS_Init(bb_assign1_ops);

     total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    OPS_Init(&New_OPs);
    Cur_BB = bb_exit;
}

