/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_emit_template.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_emit_template.h,v $
//
// Revision history:
//  03-OCT-96 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// ====================================================================
// ====================================================================


#ifndef opt_emit_template_INCLUDED
#define opt_emit_template_INCLUDED	"opt_emit_template.h"

#ifndef opt_combine_INCLUDED
#include "opt_combine.h"
#endif // opt_combine_INCLUDED

#ifndef opt_fold_INCLUDED
#include "opt_fold.h"
#endif // opt_fold_INCLUDED

#include "opt_project.h"

#include "be_symtab.h"		// for Preg_Home

#include "opt_cvtl_rule.h"
#include "wn_util.h"            // for WN_COPY_Tree_With_Map

#include "nystrom_alias_analyzer.h"

extern BOOL OPT_Enable_WHIRL_SSA;

template < class EMITTER >WN*
Gen_exp_wn(CODEREP *exp, EMITTER *emitter)
{
  WN      *wn;
  OPCODE   op;
  OPERATOR opr;
  BOOL     connect_cr_to_wn = TRUE;
  MTYPE    actual_type;
  MTYPE    actual_opnd_type;

  switch (exp->Kind()) {
  case CK_OP:
    opr = exp->Opr();
    switch (opr) {
    /* CVTL-RELATED start (performance) */
    case OPR_CVT:
      {
	if (WOPT_Enable_Cvt_Folding && !emitter->For_preopt()) {
	  INT    cvt_kind;
	  OPCODE opc;
          if (MTYPE_is_vector(exp->Dtyp()) && MTYPE_is_vector(exp->Dsctyp())
              && exp->Dtyp() != exp->Dsctyp()) {
            opc = OPCODE_make_op(OPR_CVT, exp->Dtyp(), exp->Dsctyp());
            cvt_kind = NEED_CVT;
          }
          else
          {
	    cvt_kind =
	      Need_type_conversion(exp->Dsctyp(), exp->Dtyp(), &opc);//326120
          }
	  CODEREP *kid = exp->Get_opnd(0);
	  if (cvt_kind == NEED_CVT) {
	    
	    if (MTYPE_is_integral(exp->Dsctyp())
#ifdef TARG_X8664 // bug 11797
                && ! MTYPE_is_vector(exp->Dsctyp())
#endif
                && MTYPE_is_integral(exp->Dtyp())
#ifdef TARG_X8664 // bug 11797
                && ! MTYPE_is_vector(exp->Dtyp())
#endif
		&& ! (Only_Unsigned_64_Bit_Ops && MTYPE_signed(exp->Dtyp()))
		&& MTYPE_size_min(kid->Dsctyp()) >= MTYPE_size_min(MTYPE_I4)
		&& ((kid->Kind() == CK_VAR && !kid->Is_var_volatile() &&
		     ST_class(emitter->Opt_stab()->
			      Aux_stab_entry(kid->Aux_id())->St()) != CLASS_PREG)
		    
		    ||
		    (kid->Kind() == CK_IVAR &&
		     OPERATOR_is_scalar_iload (kid->Opr()) &&
		     !kid->Is_ivar_volatile())
		    )
		) {
	      
	      MTYPE mtype_d = kid->Dtyp();
	      MTYPE mtype_dsc = kid->Dsctyp();
	      kid->Set_dtyp(exp->Dtyp());
	      if (exp->Dsctyp() == MTYPE_U4 && exp->Dtyp() == MTYPE_I8) {
		kid->Set_dtyp(MTYPE_U8);
	      }
	      kid->Set_dsctyp(Mtype_TransferSign(kid->Dtyp(), kid->Dsctyp()));
	      wn = Gen_exp_wn(kid, emitter);
	      kid->Set_dtyp(mtype_d);
	      kid->Set_dsctyp(mtype_dsc);
	      connect_cr_to_wn = FALSE;
#ifdef TARG_X8664 // bug 9680: avoid add/sub in V16F4
	    } else if (Roundoff_Level >= ROUNDOFF_ASSOC &&
		       exp->Dsctyp() == MTYPE_V8F4 &&
		       exp->Dtyp() == MTYPE_V16F8 &&
		       kid->Kind() == CK_OP &&
		       (kid->Opr() == OPR_ADD || kid->Opr() == OPR_SUB)) {
	      wn = Gen_exp_wn(kid, emitter);
	      WN_kid0(wn) = WN_CreateExp1(exp->Op(), WN_kid0(wn));
	      WN_kid1(wn) = WN_CreateExp1(exp->Op(), WN_kid1(wn));
	      WN_set_rtype(wn, MTYPE_V16F8);
	      connect_cr_to_wn = FALSE;
#endif
	    } else {
	      WN* wn_kid = Gen_exp_wn(kid, emitter);
	      BOOL enabled = WN_Simplifier_Enable(TRUE);
	      wn = WN_CreateExp1(exp->Op(), wn_kid);
	      WN_Simplifier_Enable(enabled);
	      if (WN_opcode(wn) != exp->Op()) {
		connect_cr_to_wn = FALSE;
	      }
	    }

	  } else if (cvt_kind == NOT_AT_ALL) {
	    wn = Gen_exp_wn(kid, emitter);
	    connect_cr_to_wn = FALSE;
	  } else
	    Is_True(FALSE, ("Gen_exp_wn: Bad type sequence"));
	  break;
	} else {
	  WN    *opnd = Gen_exp_wn(exp->Get_opnd(0), emitter);
	  INT    cvt_kind;
	  OPCODE opc;

	  cvt_kind =
	    Need_type_conversion(exp->Dsctyp(), exp->Dtyp(), &opc);//326120
	  if (cvt_kind == NEED_CVT)
	    wn = WN_CreateExp1(exp->Op(), opnd);
	  else if (cvt_kind == NOT_AT_ALL) {
	    wn = opnd;
	    connect_cr_to_wn = FALSE;
	  }
	  else
	    Is_True(FALSE, ("Gen_exp_wn: Bad type sequence"));
	  break;
	}
      }
    case OPR_CVTL:
      {
	if (WOPT_Enable_Cvt_Folding && ! emitter->For_preopt()
	    /* when exp->Offset() are 8, 16 , 32, 64 , load and then cvtl can be fold into
	     I8I1load  U8U2load ... etc*/
	    && (exp->Offset() == 8 || exp->Offset() == 16 
		|| exp->Offset() == 32 || exp->Offset() == 64 )) {
	  CODEREP *kid = exp->Get_opnd(0);
	  WN      *opnd;
	  actual_type = Actual_cvtl_type(exp->Op(), exp->Offset());

	  if ((kid->Kind() == CK_VAR && !kid->Is_var_volatile() &&
	       ST_class(emitter->Opt_stab()->Aux_stab_entry(kid->Aux_id())
			->St()) != CLASS_PREG)
	      ||
	      (kid->Kind() == CK_IVAR &&
	       OPERATOR_is_scalar_iload (kid->Opr()) &&
	       !kid->Is_ivar_volatile())) {
	    actual_opnd_type = kid->Dsctyp();
	    if (MTYPE_is_integral(actual_opnd_type) &&
		MTYPE_is_integral(exp->Dtyp()) &&
		! (Only_Unsigned_64_Bit_Ops && MTYPE_signed(exp->Dtyp()))) {
	      if (MTYPE_size_min(actual_opnd_type)<MTYPE_size_min(MTYPE_I4) &&
		  MTYPE_size_min(actual_type)==MTYPE_size_min(actual_opnd_type)) {
		MTYPE mtype_d = kid->Dtyp();
		MTYPE mtype_dsc = kid->Dsctyp();
		kid->Set_dtyp(exp->Dtyp());
		kid->Set_dsctyp(Mtype_TransferSign(exp->Dtyp(), kid->Dsctyp()));
		wn = Gen_exp_wn(kid, emitter);
		kid->Set_dtyp(mtype_d);
		kid->Set_dsctyp(mtype_dsc);
		connect_cr_to_wn = FALSE;
		break;
	      } else if (MTYPE_size_min(actual_opnd_type) >= MTYPE_size_min(actual_type) &&
			 MTYPE_size_min(actual_type) < MTYPE_size_min(MTYPE_I4)) {
		INT64 offset1 = MTYPE_size_min(actual_opnd_type) >> 3;
		INT64 offset2 = MTYPE_size_min(actual_type) >> 3;
		INT32 offset_old = kid->Offset();
		INT32 offset_new = offset_old;
		// Target_Byte_Sex is declared in config.h
		if (Target_Byte_Sex == BIG_ENDIAN) 
		  offset_new = offset1 - offset2 + offset_old;
		if ( offset_new <= 0x8fffffff ) {
		  kid->Set_offset( offset_new );
		  MTYPE mtype_d = kid->Dtyp();
		  MTYPE mtype_dsc = kid->Dsctyp();
		  kid->Set_dtyp(exp->Dtyp());
		  kid->Set_dsctyp(actual_type);
		  wn = Gen_exp_wn(kid, emitter);
		  kid->Set_offset( offset_old );
		  kid->Set_dtyp(mtype_d);
		  kid->Set_dsctyp(mtype_dsc);
		  connect_cr_to_wn = FALSE;
		  break;
		}
	      }
	    }
	    opnd = Gen_exp_wn(kid, emitter);
	  }
	  else {
	    opnd = Gen_exp_wn(kid, emitter);
	    actual_opnd_type = Actual_result_type(opnd);
	  }

	  if (WN_operator(opnd) == OPR_INTCONST) {
	    MTYPE new_type =
	      Adjust_signed_type(OPCODE_rtype(exp->Op()), exp->Offset(), opnd);
	    if (new_type) actual_type = new_type;
	  }

	  {
	    BOOL enabled = WN_Simplifier_Enable(TRUE);
	    wn = WN_CreateCvtl(exp->Op(), (INT16)exp->Offset(), opnd);
	    WN_Simplifier_Enable(enabled);
	    if (WN_opcode(wn) != exp->Op() || 
		WN_cvtl_bits(wn) != exp->Offset()) {
	      connect_cr_to_wn = FALSE;
	    }
	  }  

	  break;

	} else {
	  WN *opnd = Gen_exp_wn(exp->Get_opnd(0), emitter);
#if defined(TARG_X8664)
          // cannot remove CVTL when loading value from return register,
          // the return value needs to be zero/sign extended in order to
          // mask off the possible trash value in upper part of the
          // register
          
          if (WN_operator(opnd) == OPR_LDID && 
                WN_st_idx(opnd) != (ST_IDX) 0 && (ST_class(WN_st(opnd)) == CLASS_PREG)) {
            if (WN_st(opnd) == Return_Val_Preg ||
                 ( Preg_Is_Dedicated(WN_offset(opnd)) && Preg_Offset_Is_Int(WN_offset(opnd)) &&
                   Is_Return_Preg(WN_offset(opnd)) )) {
               wn = WN_CreateCvtl(exp->Op(), (INT16)exp->Offset(), opnd);
               break;
            }
          }
#endif // TARG_X8664
	  actual_type = Actual_cvtl_type(exp->Op(), exp->Offset());
	  actual_opnd_type = Actual_result_type(opnd);

	  if (exp->Get_opnd(0)->Kind() == CK_VAR ||
	      (exp->Get_opnd(0)->Kind() == CK_IVAR &&
	       OPERATOR_is_scalar_iload (exp->Get_opnd(0)->Opr())))
	    actual_opnd_type = exp->Get_opnd(0)->Dsctyp();

	  if (WN_operator(opnd) == OPR_INTCONST) {
	    MTYPE new_type =
	      Adjust_signed_type(OPCODE_rtype(exp->Op()),exp->Offset(),opnd);
	    if (new_type) actual_type = new_type;
	  }

	  if (actual_type != actual_opnd_type)
	    wn = WN_CreateCvtl(exp->Op(), (INT16)exp->Offset(), opnd);
	  else {
	    wn = opnd;
	    connect_cr_to_wn = FALSE;
	  }
	  break;
	}
      }
    /* CVTL-RELATED finish */
    case OPR_EXTRACT_BITS:
    case OPR_COMPOSE_BITS:
      {
	wn = WN_Create(exp->Op(), exp->Kid_count());
	WN_set_bit_offset_size(wn,exp->Op_bit_offset(),exp->Op_bit_size());
	for (INT i = 0; i < exp->Kid_count(); i++) {
	  CODEREP *opnd = exp->Get_opnd(i);
	  WN_kid(wn, i) = Gen_exp_wn(opnd, emitter);
	}
      }
      break;

    case OPR_ARRAY:
    case OPR_CALL:
    case OPR_ICALL: 
    case OPR_INTRINSIC_CALL: 
    case OPR_INTRINSIC_OP: 
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
    case OPR_DEALLOCA:
#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif
      {
	wn = WN_Create(exp->Op(), exp->Kid_count());
	if (opr == OPR_ARRAY) {
	  WN_element_size(wn) = exp->Elm_siz();
	}
	else if ( opr == OPR_INTRINSIC_CALL || opr == OPR_INTRINSIC_OP ) {
	  WN_intrinsic(wn) = exp->Intrinsic();
	}
#ifdef KEY
	else if ( opr == OPR_PURE_CALL_OP ) {
	  WN_st_idx (wn) = exp->Call_op_aux_id();
	}
#endif

	for (INT i = 0; i < exp->Kid_count(); i++) {
	  CODEREP *opnd = exp->Get_opnd(i);
	  WN_kid(wn, i) = Gen_exp_wn(opnd, emitter);
	}
      }
      break;

    case OPR_ASM_STMT:
      {
	wn = WN_Create(exp->Op(), exp->Kid_count() + 2);
	for (INT i = 0; i < exp->Kid_count(); ++i) {
	  CODEREP *opnd = exp->Get_opnd(i);
	  WN_kid(wn, i + 2) = Gen_exp_wn(opnd, emitter);
	}
      }
      break;

    case OPR_ASM_INPUT:
      {
	wn = WN_Create(exp->Op(), exp->Kid_count());
	WN_asm_opnd_num(wn) = exp->Asm_opnd_num();
	WN_st_idx(wn) = exp->Asm_constraint();
	for (INT i = 0; i < exp->Kid_count(); ++i) {
	  CODEREP *opnd = exp->Get_opnd(i);
	  WN_kid(wn, i) = Gen_exp_wn(opnd, emitter);
#ifdef KEY
          if (opnd->Kind() == CK_OP){
            if (opnd->Opr() == OPR_CVT){
              TYPE_ID ty = exp->Asm_input_dsctype();
// Fix bug 1766
              if (ty == MTYPE_I1 || ty == MTYPE_I2 || ty == MTYPE_U1 || ty == MTYPE_U2)
                WN_kid(wn, i) = WN_Int_Type_Conversion( WN_kid(WN_kid(wn, i),0), ty );
              else if (
		       ! MTYPE_is_float(exp->Asm_input_rtype()) &&
		       exp->Asm_input_rtype() != exp->Asm_input_dsctype()) {
                   if(Is_Valid_Opcode_Parts(WN_operator(WN_kid(wn, i)),
                      exp->Asm_input_rtype(),exp->Asm_input_dsctype()))
                   {
                     WN_set_rtype(WN_kid(wn, i), exp->Asm_input_rtype());
                     WN_set_desc(WN_kid(wn, i), exp->Asm_input_dsctype());
                   }
              }
            }
            else{
              WN_set_rtype(WN_kid(wn, i), exp->Asm_input_rtype());
	      if (WN_desc(WN_kid(wn, i)) != MTYPE_V &&
		  exp->Asm_input_rtype() != exp->Asm_input_dsctype())
                WN_set_desc(WN_kid(wn, i), exp->Asm_input_dsctype());
            }
          } 
          else{
#ifdef TARG_NVISA
            // Preserve initial types lest we lose alignment info
            // (e.g. if U4U2LDID becomes I4I4LDID)
            // This can happen if copyprop U4U2 into original I4I4).
            // But make sure rtypes are compatible.
            if (Mtype_TransferSign(WN_rtype(WN_kid(wn,i)), 
                                   exp->Asm_input_rtype()) 
               == WN_rtype(WN_kid(wn,i)))
            {
              continue; // skip following code
            }
#endif //TARG_NVISA
            WN_set_rtype(WN_kid(wn, i), exp->Asm_input_rtype());
	    // bug 13104
            if (! MTYPE_is_float(exp->Asm_input_rtype()) &&
				(opnd->Kind() == CK_VAR || opnd->Kind() == CK_IVAR))
			  // OSP_388 and OSP_390
              WN_set_desc(WN_kid(wn, i), Mtype_TransferSign(exp->Asm_input_rtype(), exp->Asm_input_dsctype()));
          }
#endif
	}
      }
      break;

    case OPR_TAS:
      wn = WN_Tas(exp->Dtyp(), 
		  exp->Ty_index(), 
		  Gen_exp_wn(exp->Get_opnd(0), emitter));
#if defined(TARG_X8664) || defined(TARG_LOONGSON) // bug 11752: make sure operand type has same size
      if (MTYPE_byte_size(WN_rtype(wn)) > MTYPE_byte_size(WN_rtype(WN_kid0(wn))) &&
          WN_operator(WN_kid0(wn)) == OPR_INTCONST)
        WN_set_rtype(WN_kid0(wn), Mtype_TransferSize(MTYPE_I8, WN_rtype(WN_kid0(wn))));
#endif

      // we may want to do a little tiny simplification
      if ( WOPT_Enable_Combine_Operations ) {
	WN *combined_operation;
	if ( Uncombine_Operations( wn, &combined_operation ) ) {
	  wn = combined_operation;
	}
      }
      break;

    default:
      {
	STMTREP *proj_defstmt;
	if (Projection_operation(exp->Op()) &&
	    exp->Opnd(0)->Kind() == CK_VAR) {
	  proj_defstmt = Proj_defstmt(exp->Opnd(0),
				      emitter->Opt_stab());
	}
	else {
	  proj_defstmt = NULL;
	}
	if (proj_defstmt != NULL &&
	    Stores_proj_op_to_temp(proj_defstmt,
				   emitter->Opt_stab()) &&
	    proj_defstmt->Proj_op_uses() == 1) {
	  // Defstmt of the operand of the projection op wasn't
	  // emitted because this use is the unique one. We should
	  // substitute back so we get the correct recombination in
	  // the routine Uncombine_operations (sic) below.
	  wn = WN_CreateExp1(exp->Op(),
			     Gen_exp_wn(exp->Opnd(0)->Defstmt()->Rhs(),
					emitter));
	}
	else if (exp->Kid_count() == 0) {
	  wn = WN_CreateExp0(exp->Op());
	} else if (exp->Kid_count() == 1) {
	  wn = WN_CreateExp1(exp->Op(),
			     Gen_exp_wn(exp->Get_opnd(0), emitter));
	} else if (exp->Kid_count() == 2) {
          WN *opnd0 = Gen_exp_wn(exp->Get_opnd(0), emitter);
          WN *opnd1 = Gen_exp_wn(exp->Get_opnd(1), emitter);
#ifdef KEY // make the desc type of the comparison smaller if possible
	  if (MTYPE_byte_size(WN_rtype(opnd0))==MTYPE_byte_size(WN_rtype(opnd1))
	     && MTYPE_byte_size(WN_rtype(opnd0))< MTYPE_byte_size(exp->Dsctyp())
	     && OPERATOR_is_compare(exp->Opr()))
	    exp->Set_dsctyp(WN_rtype(opnd0));
#endif
#ifdef KEY // bug 13230: fix INTCONSTs kids of compare unnecessarily made 64-bit
	  else if (OPERATOR_is_compare(exp->Opr()) &&
	      	   MTYPE_byte_size(exp->Dsctyp()) == 4) {
	    if (WN_operator(opnd0) == OPR_INTCONST && 
		MTYPE_byte_size(WN_rtype(opnd0)) == 8)
	      WN_set_rtype(opnd0, Mtype_TransferSize(exp->Dsctyp(), WN_rtype(opnd0)));
	    if (WN_operator(opnd1) == OPR_INTCONST && 
		MTYPE_byte_size(WN_rtype(opnd1)) == 8)
	      WN_set_rtype(opnd1, Mtype_TransferSize(exp->Dsctyp(), WN_rtype(opnd1)));
	  }
#endif
#ifdef KEY // bug 3347: fix INTCONSTs unnecessarily made 64-bit
	  else if (! OPERATOR_is_compare(exp->Opr()) &&
	      	   MTYPE_byte_size(exp->Dtyp()) == 4) {
	    if (WN_operator(opnd0) == OPR_INTCONST && 
		MTYPE_byte_size(WN_rtype(opnd0)) == 8) {
	      if ((WN_const_val(opnd0) << 32 >> 32) == WN_const_val(opnd0))
	        WN_set_rtype(opnd0, Mtype_TransferSize(exp->Dtyp(), WN_rtype(opnd0)));
	    }
	    if (WN_operator(opnd1) == OPR_INTCONST && 
		MTYPE_byte_size(WN_rtype(opnd1)) == 8) {
	      if ((WN_const_val(opnd1) << 32 >> 32) == WN_const_val(opnd1))
	        WN_set_rtype(opnd1, Mtype_TransferSize(exp->Dtyp(), WN_rtype(opnd1)));
	    }
	    if (OPCODE_is_load(WN_opcode(opnd0)) && 
		MTYPE_byte_size(WN_rtype(opnd0)) == 8 &&
		MTYPE_byte_size(WN_desc(opnd0)) == 4) {
	      WN_set_rtype(opnd0, Mtype_TransferSize(exp->Dtyp(), WN_rtype(opnd0)));
	    }
	    if (OPCODE_is_load(WN_opcode(opnd1)) && 
		MTYPE_byte_size(WN_rtype(opnd1)) == 8 &&
		MTYPE_byte_size(WN_desc(opnd1)) == 4) {
	      WN_set_rtype(opnd1, Mtype_TransferSize(exp->Dtyp(), WN_rtype(opnd1)));
	    }
	  }
	else if ((exp->Opr() == OPR_MPY || 
		  MTYPE_signed(exp->Dtyp()) &&
	  	  (exp->Opr() == OPR_SUB || exp->Opr() == OPR_ADD)) && 
	         MTYPE_byte_size(exp->Dtyp()) == 8 &&
	         (MTYPE_byte_size(exp->Dtyp()) != MTYPE_byte_size(WN_rtype(opnd0)) ||
	          MTYPE_byte_size(exp->Dtyp()) != MTYPE_byte_size(WN_rtype(opnd1))))
	    {
	      if (WN_operator(opnd0) != OPR_INTCONST)
		if (MTYPE_byte_size(WN_rtype(opnd0)) != 8) {
#ifdef TARG_X8664 // bug 11599
		  if (WN_operator(opnd0) == OPR_SUB)
		    opnd0 = WN_Cvt(MTYPE_I4, exp->Dtyp(), opnd0);
		  else
#endif
	    	  opnd0 = WN_Cvt(WN_rtype(opnd0), exp->Dtyp(), opnd0);
		}
	      if (WN_operator(opnd1) != OPR_INTCONST)
		if (MTYPE_byte_size(WN_rtype(opnd1)) != 8) {
#ifdef TARG_X8664 // bug 11599
		  if (WN_operator(opnd1) == OPR_SUB)
		    opnd1 = WN_Cvt(MTYPE_I4, exp->Dtyp(), opnd1);
		  else
#endif
	    	  opnd1 = WN_Cvt(WN_rtype(opnd1), exp->Dtyp(), opnd1);
		}
	    }
#endif
	  wn = WN_CreateExp2(exp->Op(), opnd0, opnd1);
	} else if (exp->Kid_count() == 3) {
          WN *opnd0 = Gen_exp_wn(exp->Get_opnd(0), emitter);
          WN *opnd1 = Gen_exp_wn(exp->Get_opnd(1), emitter);
          WN *opnd2 = Gen_exp_wn(exp->Get_opnd(2), emitter);
	  wn = WN_CreateExp3(exp->Op(), opnd0, opnd1, opnd2);
	} else {
	  Is_True(FALSE, ("Kid count > 3."));
	}

	// we may want to do a little tiny simplification
	if ( WOPT_Enable_Combine_Operations ) {
	  WN *combined_operation;
	  if ( Uncombine_Operations( wn, &combined_operation ) ) {
	    wn = combined_operation;
	  }
	}

      }
      break;
    }
    break;
  case CK_IVAR:
    if (exp->Opr() == OPR_MLOAD) {
      CODEREP *num_byte = exp->Mload_size();
      WN *kid0 = Gen_exp_wn(exp->Ilod_base(), emitter);
      WN *kid1 = Gen_exp_wn(num_byte, emitter);
      wn = WN_CreateMload(exp->Offset(), exp->Ilod_ty(), kid0, kid1);
      emitter->Alias_Mgr()->Gen_alias_id(wn, exp->Points_to(emitter->Opt_stab()));
      WN_set_field_id (wn, exp->I_field_id());
    }
    else if ( exp->Opr() == OPR_PARM ) {
      wn = Gen_exp_wn(exp->Ilod_base(), emitter);
#ifdef KEY // bug 12161: fix INTCONSTs unnecessarily made 64-bit
      if (WN_operator(wn) == OPR_INTCONST && MTYPE_byte_size(WN_rtype(wn)) == 8
	  && MTYPE_byte_size(exp->Dtyp()) == 4) {
	if ((WN_const_val(wn) << 32 >> 32) == WN_const_val(wn))
	  WN_set_rtype(wn, Mtype_TransferSize(exp->Dtyp(), WN_rtype(wn)));
      }
#endif
      wn = WN_CreateParm(exp->Dtyp(), wn, exp->Ilod_ty(), exp->Offset());
      // avoid cse of implicit aliases stuff
      if (WN_Parm_By_Reference(wn) || WN_Parm_Dereference(wn)) {
	POINTS_TO *pt = exp->Points_to(emitter->Opt_stab());
	Is_True(pt != NULL, ("Reference parameter has NULL POINTS_TO."));
	emitter->Alias_Mgr()->Gen_alias_id(wn, pt);
      }
#ifdef KEY // bug 7766
      else if (IS_FORTRAN && exp->Ilod_base()->Kind() == CK_LDA &&
	       ! exp->Ilod_base()->Is_flag_set(CF_LDA_LABEL) &&
	       ! WN_Parm_By_Reference(wn)) {
	WN_Set_Parm_By_Reference(wn);
	Set_ST_addr_passed(exp->Ilod_base()->Lda_base_st());
	AUX_ID vp_idx = exp->Ilod_base()->Lda_aux_id();
	emitter->Alias_Mgr()->Gen_alias_id(wn, emitter->Opt_stab()->Points_to(vp_idx));
	MU_NODE *mnode = CXX_NEW(MU_NODE, emitter->Mem_pool());
	mnode->Init(vp_idx);
	mnode->Set_OPND(emitter->Htable()->Ssa()->Get_zero_version_CR(vp_idx, emitter->Opt_stab(), 0));
	exp->Set_ivar_mu_node(mnode);
      }
#endif
    }
    else if ( exp->Opr() == OPR_ILOADX ) {
      WN *kid0 = Gen_exp_wn(exp->Ilod_base(), emitter);
      WN *kid1 = Gen_exp_wn(exp->Index(), emitter);
      wn = WN_CreateIloadx(exp->Op(), exp->Ilod_ty(), exp->Ilod_base_ty(), kid0, kid1);
      emitter->Alias_Mgr()->Gen_alias_id(wn, exp->Points_to(emitter->Opt_stab()));
      if (emitter->Gen_lno_info())
        WN_add_lno_info(wn, exp); // for mainopt
    }
    else {
      WN *kid0 = Gen_exp_wn(exp->Ilod_base(), emitter);

      wn = WN_CreateIload (exp->Opr(), exp->Dtyp(), exp->Dsctyp(),
			   exp->Offset(), exp->Ilod_ty(),
			   exp->Ilod_base_ty(), kid0, exp->I_field_id());
      if (exp->Opr() == OPR_ILDBITS)
	  WN_set_bit_offset_size (wn, exp->I_bit_offset(), exp->I_bit_size());
      emitter->Alias_Mgr()->Gen_alias_id(wn, exp->Points_to(emitter->Opt_stab()));
      if (emitter->Gen_lno_info())
        WN_add_lno_info(wn, exp); // for mainopt
    } 
    break;
  case CK_LDA:
    if (exp->Is_flag_set(CF_LDA_LABEL)) {
      wn = WN_LdaLabel(exp->Dtyp(), exp->Offset());
    }
    else {
      wn = WN_CreateLda(OPR_LDA, exp->Dtyp(), MTYPE_V, exp->Offset(), exp->Lda_ty(), exp->Lda_base_st(), exp->Afield_id());
      // Generate a small amount of alias information for LDA nodes as
      // well, since LNO may use the LDA's as handles on symbols that it
      // wants to equivalence. In that situation, it may need to
      // invalidate certain interprocedural alias classes.
      // Note: We don't do a full-blown Gen_alias_id for these nodes
      // for a number of reasons. They aren't real memops, we can't tell
      // an access size from them, etc.
      IDTYPE ip_alias_class = exp->Points_to(emitter->Opt_stab())->Ip_alias_class();
      if (ip_alias_class != OPTIMISTIC_AC_ID &&
	  ip_alias_class != PESSIMISTIC_AC_ID) {
	WN_MAP32_Set(WN_MAP_ALIAS_CLASS, wn, ip_alias_class);
      }
    }
    break;
  case CK_VAR:
    {
      AUX_STAB_ENTRY *aux_entry = 
	emitter->Opt_stab()->Aux_stab_entry(exp->Aux_id());
      wn = WN_Create((aux_entry->Bit_size() > 0 && aux_entry->Field_id() == 0)
			? OPR_LDBITS : OPR_LDID,
		     exp->Dtyp(), exp->Dsctyp(), 0); 
      TY_IDX ty_idx = exp->Lod_ty();
      Is_True(ty_idx, ("Gen_exp_wn: NULL Lod_ty() for CK_VAR node"));
      UINT16 field_id = exp->Field_id();
      ST* st = aux_entry->St();
      // when loading from a preg, the size of the preg might be larger
      // than the size of the high-level type, so fix it.
      if (ST_class (st) == CLASS_PREG &&
          exp->Dsctyp() != MTYPE_M &&  // added to fix bug #567932
	  TY_size (ty_idx) != MTYPE_byte_size (exp->Dsctyp())) {
	BOOL reset_type = TRUE;
	if (field_id != 0 && TY_kind(ty_idx) == KIND_STRUCT) {
	  // check if is referring to struct field which does match size
	  UINT cur_field_id = 0;
	  FLD_HANDLE fld = FLD_get_to_field (ty_idx, field_id, cur_field_id);
	  Is_True (! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                          field_id, ty_idx));
	  if (TY_size(FLD_type(fld)) == MTYPE_byte_size(exp->Dsctyp()))
		reset_type = FALSE;
	}
	if (reset_type) {
	  DevWarn("PREG (%s) has mismatching MTYPE-size and TY-size; refer to bug #567932", 
		  ST_name(st));
	  Set_TY_IDX_index (ty_idx,
			    TY_IDX_index(MTYPE_To_TY (exp->Dsctyp())));
	  field_id = 0;
	}
      }
      WN_set_ty(wn, ty_idx);
      WN_st_idx(wn) = ST_st_idx(st);
      if (aux_entry->Bit_size() > 0 && aux_entry->Field_id() == 0)
	WN_set_bit_offset_size (wn, exp->Bit_offset(), exp->Bit_size());
      else
	WN_set_field_id (wn, field_id);
	
      if (aux_entry->Is_non_dedicated_preg() &&
	  exp->Safe_to_renumber_preg()) {
	IDTYPE new_preg_num =
	  emitter->Preg_renumbering_map().Lookup(exp->Coderep_id());
	if (new_preg_num == 0) {
	  if (!aux_entry->Some_version_renumbered()) {
	    // We can use the base PREG number for this version
	    // because no one has used it yet.
	    aux_entry->Set_some_version_renumbered();
	    new_preg_num = exp->Offset();
	  }
	  else {
	    // We need to assign this version a new PREG number.
	    new_preg_num =
	      emitter->Opt_stab()->Alloc_preg(exp->Dtyp(),
					      "renumbered PREG",
					      NULL);
	  }
	  emitter->Preg_renumbering_map().Insert(exp->Coderep_id(),
						 new_preg_num);
	}
	WN_load_offset(wn) = new_preg_num;
      }
      else {
	WN_load_offset(wn) = exp->Offset();
      }
      emitter->Alias_Mgr()->Gen_alias_id(wn, exp->Points_to(emitter->Opt_stab()));
      // update the RVI home WN alias id
      if (ST_class(aux_entry->St()) != CLASS_PREG && aux_entry->Home_sym() != 0)
      {
	AUX_STAB_ENTRY *preg_entry = 
	  emitter->Opt_stab()->Aux_stab_entry(aux_entry->Home_sym());
	Is_True(ST_class(preg_entry->St()) == CLASS_PREG,
		("Gen_exp_wn: home preg is not a preg"));
	WN *preg_home = Preg_Home(preg_entry->St_ofst());
	if (preg_home) {
	  Copy_alias_info(emitter->Alias_Mgr(), wn, preg_home);
	}
      }
    }
    break;
  case CK_CONST:
    op = OPCODE_make_op(OPR_INTCONST, exp->Dtyp(), MTYPE_V);
    wn = WN_Create(op, 0);
    WN_const_val(wn) = exp->Const_val();
    break;
  case CK_RCONST:
    op = OPCODE_make_op(OPR_CONST, exp->Dtyp(), MTYPE_V);
    wn = WN_Create(op, 0);
    WN_st_idx(wn) = ST_st_idx(exp->Const_id());
    break;
  default:
    Warn_todo("Gen_exp_wn: CODEKIND is not implemented yet");
    break;
  }

#if defined(TARG_SL)
  // set flag for vbuf offset wn node. 
  if (exp->Is_flag_set(CF_INTERNAL_MEM_OFFSET)) {
    WN_Set_is_internal_mem_ofst(wn, TRUE);
  }

  extern INT Need_type_conversion(TYPE_ID from_ty, TYPE_ID to_ty, OPCODE *opc);
  
  OPERATOR opr_t = WN_operator(wn);
  TYPE_ID from_ty = WN_desc(wn);
  TYPE_ID to_ty = WN_rtype(wn);
  OPCODE opc = WN_opcode(wn);
  
  if ((opr_t == OPR_CVT) && (Need_type_conversion(from_ty, to_ty, &opc) == NOT_AT_ALL) && WN_kid0(wn))
     wn = WN_kid0(wn);
#endif 

  // connect up this cr and the resulting wn for def-use
  if ( emitter->Wn_to_cr_map() && connect_cr_to_wn )
    Connect_cr_wn( emitter->Wn_to_cr_map(), exp, wn );

  // When we'll later perform RVI, need to track information from
  // this coderep and this wn
  if ( emitter->Do_rvi() ) {
    if ( exp->Kind() == CK_VAR && ST_class(WN_st(wn)) != CLASS_PREG ) {
      Warn_todo( "CODEREP::Gen_wn: do not adjust bitpos by 1" );
      emitter->Rvi()->Map_bitpos( wn, exp->Bitpos()+1 );
    }
    if ( exp->Kind() == CK_IVAR && exp->Ivar_mu_node() != NULL )
      emitter->Rvi()->Map_mu_node( wn, exp->Ivar_mu_node() );
  }

  return wn;
}


template < class EMITTER >WN *
Gen_stmt_wn(STMTREP *srep, STMT_CONTAINER *stmt_container, EMITTER *emitter)
{
  // resource in emitter used are:
  // emitter->Cfg()->Rid()
  // emitter->Region_entry_stmt()
  // emitter->Opt_stab()
  // emitter->Alias_Mgr()
  // emitter->Wn_to_cr_map()
  // emitter->Mem_pool()
  // emitter->For_preopt()
  // emitter->Do_rvi()
  // emitter->Rvi()

  WN    *rwn = NULL;
  WN    *rhs_wn = NULL;
  WN    *base_wn = NULL;
  WN    *index_wn = NULL;
  OPCODE opcode, opc;

  switch (srep->Opr()) {
  case OPR_GOTO:
    rwn = WN_CreateGoto( srep->St(), srep->Label_number() );
    break;

  case OPR_REGION_EXIT:
    rwn = WN_CreateRegionExit( srep->St(), srep->Label_number() );
// RFE 454057
// TODO: these routines need to be rewritten to use SSA to be more exact.
// Only call for performance regions.
//    REGION_live_out_from_mu(emitter->Cfg()->Rid(),
//                            srep->Mu_list(),
//                            emitter->Opt_stab(),
//                            emitter->Alias_Mgr());
    break;

  case OPR_AGOTO:
    // FmtAssert( FALSE,
    //  ("Gen_stmt_wn: opcode OPR_AGOTO is not implemented yet") );
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
    rwn = WN_CreateAgoto(rhs_wn);
    break;
  case OPR_FALSEBR:
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
    rwn = WN_CreateFalsebr(srep->Label_number(), rhs_wn);
    break;
  case OPR_TRUEBR:
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
    rwn = WN_CreateTruebr(srep->Label_number(), rhs_wn);
    break;
  case OPR_COMPGOTO:
    Is_True( srep->Bb()->Switchinfo() != NULL,
      ("BB:%d has COMPGOTO, but no switchinfo", srep->Bb()->Id()) );
    {
      WN *default_wn = NULL;
      INT32 num_entries = srep->Bb()->Switchentries();
      WN *block_wn = WN_CreateBlock();

      // create a block of GOTOs (in reverse)
      for (INT32 num_entry = num_entries-1; num_entry >= 0; num_entry--) {
        WN_INSERT_BlockAfter( block_wn, NULL,
                             WN_CreateGoto((ST_IDX) NULL,
                                           srep->Bb()->Switchcase(num_entry)->Labnam() ) );
      }

      // create a default goto, if any
      if ( srep->Bb()->Switchdefault() != NULL ) {
        default_wn = 
          WN_CreateGoto((ST_IDX) NULL, srep->Bb()->Switchdefault()->Labnam());
      }
      rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
      rwn = WN_CreateCompgoto(num_entries, rhs_wn, block_wn, default_wn, 0);
    }
#ifdef TARG_SL //fork_joint
     WN_Set_is_compgoto_para(rwn, srep->Fork_stmt_flags());
     WN_Set_is_compgoto_for_minor(rwn, srep->Minor_fork_stmt_flags());
#endif 
    break;

  case OPR_ASM_STMT:
    {
      // Reconstruct the clobber and constraint pragmas from the
      // information saved aside about them.

      rwn = Gen_exp_wn(srep->Rhs(), emitter);
      WN_st_idx(rwn) = srep->Asm_string_idx();
      WN_asm_flag(rwn) = srep->Asm_stmt_flags();

      WN *clobber_block = WN_CreateBlock();

      vector<CLOBBER_PRAGMA_INFO,
             mempool_allocator<CLOBBER_PRAGMA_INFO> >::const_iterator p;
      for (p = srep->Asm_pragma()->clobber_info.begin();
	   p != srep->Asm_pragma()->clobber_info.end();
	   ++p) {
	WN *prag;
	if (p->preg_st_idx == (ST_IDX) 0) {
	  prag = WN_CreatePragma(WN_PRAGMA_ASM_CLOBBER,
				 (ST_IDX) 0,
				 p->clobber_string_idx,
				 0);
	}
	else {
          // bug fix for OSP_87 and OSP_90
	  prag = WN_CreateXpragma(WN_PRAGMA_ASM_CLOBBER,
				  (ST_IDX) p->clobber_string_idx,
				  1);
	  WN_kid0(prag) = WN_CreateIdname(p->preg_number,
					  p->preg_st_idx);
	  if(WN_kid0(prag) == 0)
	    WN_pragma_arg2(prag) = p->clobber_string_idx;
	}
	WN_INSERT_BlockAfter(clobber_block,
			     WN_last (clobber_block),
			     prag);
      }
      WN_asm_clobbers(rwn) = clobber_block;

      WN *output_constraint_block = WN_CreateBlock();

      vector<CONSTRAINT_PRAGMA_INFO,
             mempool_allocator<CONSTRAINT_PRAGMA_INFO> >::const_iterator q;
      for (q = srep->Asm_pragma()->constraint_info.begin();
	   q != srep->Asm_pragma()->constraint_info.end();
	   ++q) {
	WN *prag = WN_CreatePragma(WN_PRAGMA_ASM_CONSTRAINT,
				   emitter->Opt_stab()->St(q->preg_st_idx),
				   (INT32) q->constraint_st_idx,
				   q->asm_neg_preg,
				   q->asm_opnd_num);
	WN_INSERT_BlockAfter(output_constraint_block,
			     WN_last(output_constraint_block),
			     prag);
      }
      WN_asm_constraints(rwn) = output_constraint_block;
    }
    break;

  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
  case OPR_ALLOCA:
  case OPR_DEALLOCA:
    //rhs contains the whole node
    {
      OPCODE opc = srep->Op();
      OPERATOR opr = OPCODE_operator(opc);
      rwn = Gen_exp_wn( srep->Rhs(), emitter );

      // Restore the callsite id for the Nystrom alias analyzer
      if (srep->Get_constraint_graph_callsite_id() != 0)
        WN_MAP32_Set(WN_MAP_ALIAS_CGNODE, rwn, 
                     srep->Get_constraint_graph_callsite_id());
#ifdef KEY
      // bug 8941: If possible replace ICALL with a CALL.
      if (WN_operator (rwn) == OPR_ICALL)
      {
	mINT16 kidcount = WN_kid_count (rwn);
	WN * kid = WN_kid (rwn, kidcount - 1 );
        // cannot replace ICALL with a CALL if the types
        // are not match:
        //   extern void foo(void);  
        //   int i = ((int (*))foo)();
	if (WN_operator (kid) == OPR_LDA &&
	    ST_class (WN_st (kid)) == CLASS_FUNC &&
            srep->Ty() == ST_type(WN_st (kid)) )
	{
	  WN_set_operator (rwn, OPR_CALL);
	  WN_st_idx (rwn) = ST_st_idx (WN_st (kid));
	  WN_call_flag (rwn) = srep->Call_flags();
	  WN_set_kid_count (rwn, kidcount - 1);
	  WN_Delete (kid);
          // Since we are promoting the ICALL to a CALL, fix the CallSite
          // information in the Nystrom alias analyzer accordingly
          NystromAliasAnalyzer *naa = static_cast<NystromAliasAnalyzer *>
                                      (AliasAnalyzer::aliasAnalyzer());
          if (naa && !naa->isPostIPA() && naa->constraintGraph())
            naa->constraintGraph()->promoteCallSiteToDirect(
                           WN_MAP_CallSiteId_Get(rwn), WN_st_idx(rwn));
	  break;
	}
      }
#endif
      if (opr == OPR_ICALL)
	WN_set_ty(rwn, srep->Ty());
      if (OPCODE_is_call(opc))
	WN_call_flag(rwn) = srep->Call_flags();
      if (OPCODE_has_sym(opc))
	WN_st_idx(rwn) = ST_st_idx(srep->St());
      if (opr == OPR_FORWARD_BARRIER ||
	  opr == OPR_BACKWARD_BARRIER ||
	  opr == OPR_DEALLOCA) 
	emitter->Alias_Mgr()->Gen_alias_id_list(rwn, srep->Pt_list());

    }
    break;

  case OPR_STID:
  case OPR_STBITS:
    {
      if (Stores_proj_op_to_temp(srep, emitter->Opt_stab()) &&
	  srep->Proj_op_uses() == 1) {
	// The RHS of this stmt will be (re)combined with a
	// projection operation at the unique point where the
	// srep->Lhs() is used. So we do nothing here.
	return NULL;
      }
      else {
	// lhs contains the LOD node, need to reconstruct the STID node
	CODEREP *rhs_cr = srep->Rhs();
	CODEREP *lhs = srep->Lhs();
	/* CVTL-RELATED start (performance) */
	if ( !emitter->For_preopt() &&
	     WOPT_Enable_Cvt_Folding &&
             rhs_cr->Kind() == CK_OP && 
	     ( rhs_cr->Opr() == OPR_CVT &&
	       MTYPE_is_integral( rhs_cr->Dsctyp() ) 
	      || rhs_cr->Opr() == OPR_CVTL ) &&
	     MTYPE_is_integral( rhs_cr->Dtyp() ) && 
             MTYPE_is_integral( lhs->Dsctyp() )
#ifdef TARG_X8664
	     && ! MTYPE_is_vector(lhs->Dsctyp())
#endif
	    ) {
	  
	  MTYPE actual_type;
	  if (rhs_cr->Opr() == OPR_CVTL)
	    actual_type = Actual_cvtl_type(rhs_cr->Op(),rhs_cr->Offset());
	  else if (MTYPE_size_min(rhs_cr->Dsctyp()) <= MTYPE_size_min(rhs_cr->Dtyp()))
	    actual_type = rhs_cr->Dsctyp();
	  else
	    actual_type = rhs_cr->Dtyp();
	  if ( ST_class(emitter->Opt_stab()->St(lhs->Aux_id())) != CLASS_PREG
	      && MTYPE_size_min(lhs->Dsctyp())<=MTYPE_size_min(actual_type) ) {
	    rhs_cr = rhs_cr->Get_opnd(0);
	  } else {
	    FOLD ftmp;
	    CODEREP *cr;
	    cr = ftmp.Fold_Expr(rhs_cr);
	    if (cr) rhs_cr = cr;
	  }
	}  // PV 486445
	/* CVTL-RELATED finish */
	
	rhs_wn = Gen_exp_wn( rhs_cr, emitter );
	opcode = OPCODE_make_op(srep->Opr(), MTYPE_V, lhs->Dsctyp());
	AUX_STAB_ENTRY *aux_entry =
	  emitter->Opt_stab()->Aux_stab_entry(lhs->Aux_id());
	INT64 lhs_offset;
	if (aux_entry->Is_non_dedicated_preg() &&
	    lhs->Safe_to_renumber_preg()) {
	  lhs_offset =
	    emitter->Preg_renumbering_map().Lookup(lhs->Coderep_id());
	  if (lhs_offset == 0) {
	    if (!aux_entry->Some_version_renumbered()) {
	      aux_entry->Set_some_version_renumbered();
	      lhs_offset = lhs->Offset();
	    }
	    else {
	      lhs_offset =
		emitter->Opt_stab()->Alloc_preg(lhs->Dsctyp(),
						"renumbered PREG",
						NULL);
	    }
	    emitter->Preg_renumbering_map().Insert(lhs->Coderep_id(),
						   (IDTYPE) lhs_offset);
	  }
	}
	else {
	  lhs_offset = lhs->Offset();
	}
	ST* st = emitter->Opt_stab()->St(lhs->Aux_id());
	TY_IDX ty_idx = lhs->Lod_ty();
	UINT16 field_id = lhs->Field_id();
	// when saving to a preg, the size of the preg may be larger than
	// the size of the high-level type, need to reflect this.
	if (ST_class (st) == CLASS_PREG &&
            lhs->Dsctyp() != MTYPE_M &&   // added to fix bug #567932
	    TY_size(ty_idx) != MTYPE_byte_size (lhs->Dsctyp())) {
	  Set_TY_IDX_index (ty_idx,
			    TY_IDX_index(MTYPE_To_TY (lhs->Dsctyp())));
	  field_id = 0;
	}

	if (lhs->Dsctyp() == MTYPE_B && WN_rtype(rhs_wn) != MTYPE_B) {
	  Is_True(WN_operator(rhs_wn) == OPR_INTCONST,
	        ("Gen_stmt_wn: non-boolean value stored to boolean variable"));
	  WN_set_rtype(rhs_wn, MTYPE_B);
	}
#ifdef KEY
	if (OPERATOR_is_load(WN_operator(rhs_wn)) && 
	    lhs->Dsctyp() != MTYPE_BS /* bug 14453 */) {
	  if (MTYPE_byte_size(WN_rtype(rhs_wn)) < 
	    			MTYPE_byte_size(lhs->Dsctyp())) { // bug 5224
	    Is_True(MTYPE_is_integral(WN_rtype(rhs_wn)),
		    ("Gen_stmt_wn: inconsistent sizes in float type assignment"));
	    WN_set_rtype(rhs_wn, Mtype_TransferSize(lhs->Dsctyp(), WN_rtype(rhs_wn)));
	  }
	  else if (MTYPE_byte_size(WN_rtype(rhs_wn)) > 
	    			MTYPE_byte_size(lhs->Dsctyp()) &&
		   MTYPE_byte_size(WN_rtype(rhs_wn)) >
		   		MTYPE_byte_size(WN_desc(rhs_wn))) { // bug 7603
	    // prevent unnecessary expansion by the load to 64-bit
	    WN_set_rtype(rhs_wn, Mtype_TransferSize(MTYPE_I4, WN_rtype(rhs_wn)));
	  }
	}
#endif
	rwn = WN_CreateStid(opcode, lhs_offset, st, ty_idx, rhs_wn, field_id);
	if ( emitter->Do_rvi() ) {
	  Warn_todo( "Gen_stmt_wn: do not adjust bitpos by 1" );
	  emitter->Rvi()->Map_bitpos( rwn, lhs->Bitpos()+1 );
	}
	emitter->Alias_Mgr()->Gen_alias_id(rwn,
					   lhs->Points_to(emitter->Opt_stab()));
      }
    }
    break;
  case OPR_PREFETCH:
    {
      WN *addr_wn = Gen_exp_wn( srep->Rhs()->Ilod_base(), emitter );
      // Prefetch_wn already pre-allocated
      // update it with the canonicalized address
      rwn = srep->Prefetch_wn();
      WN_kid0(rwn) = addr_wn;
      WN_offset(rwn) = srep->Rhs()->Offset();
      break;
    }

  case OPR_ISTORE:
  case OPR_ISTBITS:
    {
      if (!emitter->For_preopt() &&
	  srep->Rhs()->Kind() == CK_IVAR &&
	  srep->Rhs()->Ilod_base() == srep->Lhs()->Istr_base() &&
	  srep->Rhs()->Offset() == srep->Lhs()->Offset() &&
	  MTYPE_size_min(srep->Rhs()->Dsctyp()) == MTYPE_size_min(srep->Lhs()->Dsctyp()) &&
	  !srep->Rhs()->Is_ivar_volatile() &&
	  !srep->Lhs()->Is_ivar_volatile()) {
        WN* rwn = NULL;
        if (OPT_Enable_WHIRL_SSA) {
          // WHIRL SSA: process chi list
          rwn = emitter->WSSA_Emitter()->WSSA_Copy_Equivalent_CHI(srep);
          if (rwn != NULL) {
            WN_Set_Linenum(rwn, srep->Linenum());
            stmt_container->Append(rwn);
          }
        }
        return rwn;
      }

      CODEREP *rhs_cr = srep->Rhs();
      CODEREP *lhs = srep->Lhs();
      /* CVTL-RELATED start (performance) */
      if (
#ifndef KEY // bug 5695
	  !emitter->For_preopt() &&
#endif
	  WOPT_Enable_Cvt_Folding &&
	  rhs_cr->Kind() == CK_OP && 
	  ( rhs_cr->Opr() == OPR_CVT &&
	   MTYPE_is_integral( rhs_cr->Dsctyp() ) 
#ifdef KEY // bug 10346
	   && !MTYPE_is_vector( rhs_cr->Dsctyp() )
	   && !MTYPE_is_vector( rhs_cr->Dtyp() )
#endif
	   || rhs_cr->Opr() == OPR_CVTL ) &&
	  MTYPE_is_integral( rhs_cr->Dtyp() ) && 
	  MTYPE_is_integral( lhs->Dsctyp() ) &&
	  lhs->Dsctyp() != MTYPE_BS /* bug 14453 */
	  ) {
	MTYPE actual_type;
	if (rhs_cr->Opr() == OPR_CVTL)
	  actual_type = Actual_cvtl_type(rhs_cr->Op(),rhs_cr->Offset());
	else if (MTYPE_size_min(rhs_cr->Dsctyp()) <= MTYPE_size_min(rhs_cr->Dtyp()))
	  actual_type = rhs_cr->Dsctyp();
	else
	  actual_type = rhs_cr->Dtyp();
	if ( MTYPE_size_min(lhs->Dsctyp()) <= MTYPE_size_min(actual_type) ) {
	  rhs_cr = rhs_cr->Get_opnd(0);
	} else {
	  FOLD ftmp;
	  CODEREP *cr;
	  cr = ftmp.Fold_Expr(rhs_cr);
	  if (cr) rhs_cr = cr;
	}
      }  // PV 486445
      /* CVTL-RELATED finish */
      
      rhs_wn = Gen_exp_wn( rhs_cr, emitter );

      Is_True(lhs->Istr_base() != NULL,
	      ("Gen_stmt_wn: istr_base has NULL lhs"));
      base_wn = Gen_exp_wn( lhs->Istr_base(), emitter );
      opcode = OPCODE_make_op(srep->Opr(), MTYPE_V, lhs->Dsctyp());
      rwn = WN_Create(opcode, 2);
      WN_kid0(rwn) = rhs_wn;
      WN_kid1(rwn) = base_wn;
      WN_store_offset(rwn) = lhs->Offset();
      WN_set_ty(rwn, lhs->Ilod_base_ty());
      WN_set_field_id(rwn, lhs->I_field_id());
#if defined(TARG_SL)
      // support vbuf istore automatic expansion 
      WN_Set_is_internal_mem_ofst(rwn, srep->SL2_internal_mem_ofst());  
#endif 
#if defined(TARG_X8664) || defined(TARG_LOONGSON) // bug 6910
      if (emitter->Htable()->Phase() != MAINOPT_PHASE &&
	  WN_operator(rhs_wn) == OPR_INTCONST &&
	  MTYPE_byte_size(WN_rtype(rhs_wn)) < MTYPE_byte_size(lhs->Dsctyp()) &&
	  lhs->Dsctyp() != MTYPE_BS /* bug 14453 */)
	WN_set_rtype(rhs_wn, Mtype_TransferSize(lhs->Dsctyp(), WN_rtype(rhs_wn)));
#endif
#if defined(TARG_X8664) || defined(TARG_LOONGSON) 
      if (Is_Target_32bit() && MTYPE_byte_size(WN_rtype(rhs_wn)) == 8 &&
	  WN_operator(rhs_wn) == OPR_INTCONST && 
	  MTYPE_byte_size(lhs->Dsctyp()) < 8 &&
	  lhs->Dsctyp() != MTYPE_BS /* bug 14453 */)
	WN_set_rtype(rhs_wn, Mtype_TransferSize(MTYPE_I4, WN_rtype(rhs_wn)));
#endif
      emitter->Alias_Mgr()->
	Gen_alias_id(rwn, lhs->Points_to(emitter->Opt_stab()));
      if (emitter->Gen_lno_info())
	WN_add_lno_info(rwn, lhs); // for mainopt
      break;
    }

  case OPR_ISTOREX:
    {
      CODEREP *rhs_cr = srep->Rhs();
      CODEREP *lhs = srep->Lhs();
      /* CVTL-RELATED start (performance) */
      if ( !emitter->For_preopt() &&
	  WOPT_Enable_Cvt_Folding &&
	  rhs_cr->Kind() == CK_OP && 
	  ( rhs_cr->Opr() == OPR_CVT &&
	   MTYPE_is_integral( rhs_cr->Dsctyp() ) 
	   || rhs_cr->Opr() == OPR_CVTL ) &&
	  MTYPE_is_integral( rhs_cr->Dtyp() ) && 
	  MTYPE_is_integral( lhs->Dsctyp() )
	  ) {
	MTYPE actual_type = (rhs_cr->Opr() == OPR_CVT) ? 
	  rhs_cr->Dsctyp() : Actual_cvtl_type(rhs_cr->Op(),rhs_cr->Offset());
	if ( MTYPE_size_min(lhs->Dsctyp()) <= MTYPE_size_min(actual_type) ) {
	  rhs_cr = rhs_cr->Get_opnd(0);
	} else {
	  FOLD ftmp;
	  CODEREP *cr;
	  cr = ftmp.Fold_Expr(rhs_cr);
	  if (cr) rhs_cr = cr;
	}
      }  // PV 486445
      /* CVTL-RELATED finish */
	
      rhs_wn = Gen_exp_wn( rhs_cr, emitter );

      base_wn = Gen_exp_wn( lhs->Istr_base(), emitter );
      index_wn = Gen_exp_wn( lhs->Index(), emitter );
      opcode = OPCODE_make_op(OPR_ISTOREX, MTYPE_V, lhs->Dsctyp());
      rwn = WN_Create(opcode, 3);
      WN_kid0(rwn) = rhs_wn;
      WN_kid1(rwn) = base_wn;
      WN_kid(rwn, 2) = index_wn;
      WN_set_ty(rwn, lhs->Ilod_base_ty());
      emitter->Alias_Mgr()->
	Gen_alias_id(rwn, lhs->Points_to(emitter->Opt_stab()));
      if (emitter->Gen_lno_info())
	WN_add_lno_info(rwn, lhs); // for mainopt
      break;
    }

  case OPR_MSTORE:
    {
      rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
      CODEREP *num_bytes = srep->Lhs()->Mstore_size();
      WN *num_bytes_wn = Gen_exp_wn(num_bytes, emitter );
      base_wn = Gen_exp_wn( srep->Lhs()->Istr_base(), emitter );
      rwn = WN_CreateMstore(srep->Lhs()->Offset(),
                            srep->Lhs()->Ilod_ty(), rhs_wn, 
			    base_wn, num_bytes_wn);
      WN_set_field_id (rwn, srep->Lhs()->I_field_id ());
      emitter->Alias_Mgr()->Gen_alias_id(rwn,
                              srep->Lhs()->Points_to(emitter->Opt_stab()));
    }
    break;

  case OPR_LABEL:
    {
      WN *loop_info = NULL;
      if ( srep->Bb()->Label_loop_info() != NULL ) {
        if (emitter->For_preopt()) {
          // note that we don't know how to update this loop_info without
          // a valid loop.  We punt by creating a new one with no iv or
          // trip count.
          WN *old_info = srep->Bb()->Label_loop_info();
          loop_info = WN_CreateLoopInfo( NULL/*induction*/, NULL/*trip*/,
                                        WN_loop_trip_est(old_info),
                                        WN_loop_depth(old_info),
                                        WN_loop_flag(old_info) );
        }
        else { // ML_WHIRL_EMITTER
          loop_info = emitter->Build_loop_info( srep->Bb() );
        }
      }
      Is_True(srep->Label_number() != 0, ("No label number."));
      rwn = WN_CreateLabel(srep->Label_number(),
			   srep->Label_flags(), 
			   loop_info);
    }
    break;

  case OPR_ASSERT:
  case OPR_RETURN_VAL:
    rwn = WN_COPY_Tree_With_Map(srep->Orig_wn());
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
#ifdef KEY // bug 5224
    if (srep->Opr() == OPR_RETURN_VAL &&
	OPERATOR_is_load(WN_operator(rhs_wn)) &&
	MTYPE_byte_size(WN_rtype(rhs_wn)) < MTYPE_byte_size(WN_rtype(rwn))){
      WN_set_rtype(rhs_wn, Mtype_TransferSize(WN_rtype(rwn), WN_rtype(rhs_wn)));
    }
#endif
    WN_kid0(rwn) = rhs_wn;
    break;
  
  case OPR_RETURN:
  case OPR_PRAGMA:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
    rwn = WN_COPY_Tree_With_Map(srep->Orig_wn());
    if (OPCODE_has_aux(srep->Op()))
      WN_st_idx(rwn) = ST_st_idx(emitter->Opt_stab()->St(WN_aux(rwn)));
    break;

  case OPR_XPRAGMA:
#ifdef KEY
    if (emitter->Htable()->Phase() == MAINOPT_PHASE && 
	WN_pragma(srep->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
      return NULL; // delete here instead of in opt_ssa.cxx
#endif
    rwn = WN_COPY_Tree_With_Map(srep->Orig_wn());
    if (OPCODE_has_aux(srep->Op()))
      WN_st_idx(rwn) = ST_st_idx(emitter->Opt_stab()->St(WN_aux(rwn)));
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
    WN_kid0(rwn) = rhs_wn;
    break;
  
  case OPR_EVAL:
    rhs_wn = Gen_exp_wn( srep->Rhs(), emitter );
    if (srep->Bb()->Kind() == BB_DOHEAD && srep->Bb()->Loop()->Trip_count_stmt() == srep) {
      IDTYPE preg = emitter->Opt_stab()->Alloc_preg(srep->Rhs()->Dtyp());
      ST *preg_st = MTYPE_To_PREG(srep->Rhs()->Dtyp());
      OPCODE opcode = OPCODE_make_op(OPR_STID, MTYPE_V, srep->Rhs()->Dtyp());
      rwn = WN_CreateStid(opcode,preg,preg_st,ST_type(preg_st),rhs_wn);
      emitter->Alias_Mgr()->Gen_alias_id(rwn, NULL);
      // WHIRL SSA
      if(OPT_Enable_WHIRL_SSA)
        emitter->WSSA_Emitter()->WSSA_Set_Ver(rwn, WSSA::VER_IDX_ZERO);
      opcode = OPCODE_make_op(OPR_LDID, srep->Rhs()->Dtyp(), srep->Rhs()->Dtyp());
      WN *lwn = WN_CreateLdid(opcode,preg,preg_st,ST_type(preg_st));
      emitter->Alias_Mgr()->Gen_alias_id(lwn, NULL);
      srep->Bb()->Loop()->Set_wn_trip_count(lwn);
      // WHIRL SSA
      if(OPT_Enable_WHIRL_SSA)
        emitter->WSSA_Emitter()->WSSA_Set_Ver(lwn,  WSSA::VER_IDX_ZERO);
    } else
      rwn = WN_CreateEval(rhs_wn);
    break;

  case OPR_REGION: // black box region, previously processed
    rwn = srep->Black_box_wn(); 
    break;

  case OPR_OPT_CHI:  // the entry chi statement
    {
      BB_NODE *entry_bb = srep->Bb();
      Is_True(entry_bb->Kind() == BB_ENTRY ||
	      entry_bb->Kind() == BB_REGIONSTART, 
         ("Gen_stmt_wn: cannot find entry bb (%s) for OPR_OPT_CHI node",
	  srep->Bb()->Kind_name()));
      if (entry_bb->Kind() == BB_ENTRY)
	emitter->Connect_sr_wn( srep, entry_bb->Entrywn() );
      else {
        // region wn doesn't exist yet so delay connect_sr_wn
        // until Raise_func_entry
        emitter->Set_region_entry_stmt(srep);
      }
      return NULL;  // do not do anything more
    }
  case OPR_IO:	// one of the "black-box" statements
    rwn = WN_COPY_Tree_With_Map(srep->Black_box_wn());
    emitter->Alias_Mgr()->Gen_black_box_alias(rwn);
    break;
#ifdef KEY
  case OPR_COMMENT:
    return NULL;
#endif

  case OPR_ZDLBR:
    {
      rwn = WN_CreateZDLBr(srep->Label_number());
    }
    break;

  default:
    FmtAssert(FALSE, ("Gen_stmt_wn: opcode %s is not implemented yet",
		      OPCODE_name(srep->Op())));
  }

  WN_Set_Linenum(rwn, srep->Linenum());

  if (emitter->Cfg()->Feedback())
    emitter->Cfg()->Feedback()->Emit_feedback( rwn, srep->Bb() );

  stmt_container->Append(rwn);

  if (emitter->Do_rvi()) {
    if ( WN_has_mu(rwn, emitter->Cfg()->Rgn_level()) && srep->Mu_list() != NULL ) {
      emitter->Rvi()->Map_mu_list( rwn, srep->Mu_list() );
    }
    if ( WN_has_chi(rwn, emitter->Cfg()->Rgn_level()) && srep->Chi_list() != NULL ) {
      emitter->Rvi()->Map_chi_list( rwn, srep->Chi_list() );
    }
  }

  // connect up this stmtrep and the resulting wn for def-use
  emitter->Connect_sr_wn( srep, rwn );
  return rwn;
}


template < class EMITTER > void
Gen_stmt_list_wn(STMT_LIST *stmt_list,
                 STMT_CONTAINER *stmt_container,
                 EMITTER *emitter)
{
  STMTREP_ITER stmt_iter(stmt_list);
  STMTREP  *tmp;
  FOR_ALL_NODE(tmp, stmt_iter, Init()) {
    WN *twn = Gen_stmt_wn(tmp, stmt_container, emitter);
  }
}


template < class EMITTER >void
Gen_bb_wn(BB_NODE *bb, EMITTER *emitter)
{
  // generate the WHIRL stmt list from the stmtlist
  STMT_CONTAINER stmt_cont(bb->Firststmt(), bb->Laststmt());
  Gen_stmt_list_wn(bb->Stmtlist(), &stmt_cont, emitter);
  bb->Set_firststmt(stmt_cont.Head());
  bb->Set_laststmt(stmt_cont.Tail());
  bb->Set_wngend();
}

#endif  // opt_emit_template_INCLUDED
