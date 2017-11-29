/*
 * Module: disp_instr.cxx
 *
 * Description:
 */

#include "disp_instr.h"
#include "mempool.h"
#include "defs.h"
#include "cxx_memory.h"
#include "tn.h"
#include "label_util.h"
#include "tag.h"
#include "cg_flags.h"
#include "annotations.h"
#include "whirl2ops.h"
#include <vector>
using std::vector;
/*
 * 5-bit sign immediate integer
 */
#define Min_INT5 -16
#define Max_INT5 15
/*
 * 5-bit unsign immediate integer 
 */
#define Min_uINT5 0
#define Max_uINT5 31
/*
 * 7-bit signed immediate integer
 */
#define Min_sINT7 -64
#define Max_sINT7 63
/*
 * special st/ld offset 5-bit unsign integer
 */
#define Min_uINT7 0
#define Max_uINT7 127

extern ST *SP_Sym;

void Replace_Size16_Instr (void);
static void Find_Candicate_OP (OP *op);
static OP *prev_16op =NULL;


/*
 *  Selecting some TOP with stricted imm or offset 
 */
inline BOOL
Repl_Size16_Instr::Replaceable_TOP_Imm(TOP top, INT32 imm){

  BOOL replaceable = FALSE;
  switch(top){
    //arithmatic op with 5-bit signed integer:  add16i. sub16i add16.sp
    case TOP_sll: 
    case TOP_srl:
    case TOP_sra:
    case TOP_ori:
    case TOP_xori:
    case TOP_andi:
      if ((imm >= Min_uINT5) && (imm <= Max_uINT5)) {
        replaceable = TRUE;
      }
      break;
    // special ld/st with 7-bit unsigned integer
    case TOP_lw: 
    case TOP_sw: 
      if ((imm >= Min_uINT7) && (imm <= Max_uINT7)) {
        replaceable = TRUE;
      }
      break;
    default :
      Is_True(FALSE, ("%s has no imm operand",top));

  }

  if (replaceable)
    return TRUE;
  else
    return FALSE;
 
}


inline BOOL 
Repl_Size16_Instr::Tn_Is_Reloc_TN (TN *tn) {
  return (TN_is_constant(tn) && TN_relocs(tn));
}

/*
 * Replace _cur_op with newop
 */
inline void
Repl_Size16_Instr::Replace_CurOP (OP *newop) {
  OP_srcpos (newop) = OP_srcpos(_cur_op);
  OP_flags(newop) = OP_flags(_cur_op);
  if (OP_has_tag(_cur_op)) {
    Set_OP_Tag(newop, Get_OP_Tag(_cur_op));
  }
  if (OP_code(newop) == TOP_mv16) {
    Set_OP_copy (newop);
  }
  if (Get_WN_From_Memory_OP(_cur_op)) {
    Copy_WN_For_Memory_OP(newop, _cur_op);	
  }
  BB_Insert_Op_Before(OP_bb(_cur_op), _cur_op, newop);
  OP_Change_To_Noop(_cur_op);
  _cur_op = newop;
  return; 
}

/*
 * Result tn equal One of two Operands
 * either == TRUE:  addu, and , or, xor,nor
 * either == FALSE: subu, sllv, srlv, srav
 */
BOOL
Repl_Size16_Instr::Replace_ResEqOper_OP (BOOL either = TRUE) {

  TOP newtop;
  TOP oldtop = OP_code(_cur_op);

  switch (oldtop){
    case TOP_addu: newtop = TOP_add16;   break;
    case TOP_and:  newtop = TOP_and16;   break;
    case TOP_or:   newtop = TOP_or16;    break;
    case TOP_xor:  newtop = TOP_xor16;   break;
    case TOP_nor:  newtop = TOP_inv16;   break;
    case TOP_subu: newtop = TOP_sub16;   break;
    case TOP_sllv: newtop = TOP_shll16;  break;
    case TOP_srlv: newtop = TOP_shrl16;  break;
    case TOP_srav: newtop = TOP_shra16;  break;
    default:
      Is_True(FALSE,("%s is not /result eq some operand/", OP_code(_cur_op)));
  }
  
  if (OP_results(_cur_op) > 1 || OP_opnds(_cur_op) > 2) {
    return FALSE;
  } 

  //just deal 2 operands and 1 result
  TN *rtn = OP_result(_cur_op,0);
  TN *op1tn = OP_opnd(_cur_op,0);
  TN *op2tn = OP_opnd(_cur_op,1);

  // nor -> norz16
  if (oldtop == TOP_nor) {
    if (tn_registers_identical (rtn, op1tn) && tn_registers_identical(op2tn, Zero_TN)) {
       OP_Change_Opcode(_cur_op, newtop);
       return TRUE;	
    } else
       return FALSE;
  }

  if (tn_registers_identical (rtn, op1tn)) {
    OP_Change_Opcode(_cur_op, newtop);
    return TRUE;
  }
  else if (either && tn_registers_identical (rtn, op2tn)) {
    OP *new_op = Mk_OP(newtop, rtn, op2tn, op1tn);
    Replace_CurOP(new_op);
    return TRUE;
  }

  if ( oldtop == TOP_addu || oldtop == TOP_or || oldtop == TOP_subu ) {
    if (tn_registers_identical(op2tn, Zero_TN)) {
      //special caes: [$rd, $rs, $0]
      OP *new_op = Mk_OP (TOP_mv16, rtn, op1tn, op2tn);
      Replace_CurOP(new_op);
      return TRUE;
    }
    else if (either && tn_registers_identical(op1tn , Zero_TN)) {
      //special case [$rd,$0, $rs]
      OP *new_op = Mk_OP (TOP_mv16, rtn, op2tn, op1tn);
      Replace_CurOP(new_op);
      return TRUE;
    }

  }
  return FALSE;
}

/*
 * deal with add.i
 */
BOOL 
Repl_Size16_Instr::Replace_ADDI (){
  Is_True(OP_code(_cur_op) == TOP_addiu,
          ("%s is not add.i \n", OP_code(_cur_op)));

 if (OP_results(_cur_op) > 1 || OP_opnds(_cur_op) > 2) {
    return FALSE;
  }   

  //just deal with one result and two operands

  TN *rtn = OP_result (_cur_op, 0);
  TN *op1tn = OP_opnd (_cur_op, 0);
  TN *op2tn = OP_opnd (_cur_op, 1);

  Is_True (TN_is_constant(op2tn),("function called by wrong phase"));
  if(Tn_Is_Reloc_TN (op2tn) || TN_is_label(op2tn) || TN_is_enum(op2tn)) 
    return FALSE; 

  INT32 op2value ;
  if (TN_has_value(op2tn)) {
  	op2value = TN_value (op2tn);
  } else if (TN_is_symbol(op2tn)) {
      ST *base_st;
      INT64 base_ofst;
      ST *st = TN_var(op2tn);
      Base_Symbol_And_Offset (st, &base_st, &base_ofst);
      if (base_st == SP_Sym ) {
	Is_True(tn_registers_identical(op1tn, SP_TN), ("Replace_ADDI:: op1tn is $sp when op2tn is sp related "));
        op2value= base_ofst + TN_offset(op2tn);
    } else {
       //case : add.i $10, $10, %lo(.bss) [./libc/inet/getaddrinfo.s]
       return FALSE;
    }
  }

  if (!tn_registers_identical (rtn, op1tn) && !tn_registers_identical(op1tn, Zero_TN) && !(op2value==0)) 
     return FALSE;

  // add.i $gpr, $sp, 0
  if (op2value == 0) {
    OP *new_op = Mk_OP(TOP_mv16, rtn, op1tn, Zero_TN);
    Replace_CurOP(new_op);
    return TRUE;
  } else if(tn_registers_identical(op1tn, Zero_TN) && (op2value <= Max_uINT5) && (op2value >= Min_uINT5)) {
    OP *new_op = Mk_OP (TOP_mv16_i, rtn, op1tn, op2tn);
    Replace_CurOP(new_op);
    return TRUE;
  } else if (tn_registers_identical(rtn, SP_TN) && tn_registers_identical (rtn, op1tn)
             && (op2value <= Max_sINT7) && (op2value >= Min_sINT7)) {
     Is_True(((op2value&3) == 0), ("addi.sp : imm must be 4 mulpitly"));
     OP_Change_Opcode(_cur_op, TOP_add16_sp);
     return TRUE;
  } else if (tn_registers_identical(rtn, op1tn) && (op2value <= Max_uINT5) && (op2value >= Min_uINT5)) {
     OP_Change_Opcode(_cur_op, TOP_add16_i);        
     return TRUE;
  } else if (tn_registers_identical(rtn, op1tn) && (op2value < Min_uINT5) && (op2value >=-Max_uINT5)) {
     TN *tmp = Gen_Literal_TN(-op2value, 4);
     OP *new_op = Mk_OP (TOP_sub16_i, rtn, op1tn, tmp);
     Replace_CurOP(new_op);
     return TRUE;
  }
  return FALSE;
}

/* replace ldub /lduh*/
BOOL 
Repl_Size16_Instr::Replace_LDU () {
  TOP newtop;
  TOP oldtop = OP_code(_cur_op);
  switch (oldtop) {
    case TOP_lbu:   newtop = TOP_ldub16_rs; break;
    case TOP_lhu:   newtop = TOP_lduh16_rs; break;
    default:
	Is_True(FALSE,("%s is not LDU", OP_code(_cur_op)));	
  }

  TN *op1tn, *offsettn, *basetn;
  // ld.w one result and two operands
    op1tn = OP_result(_cur_op,0);
    basetn = OP_opnd(_cur_op,0);
    offsettn = OP_opnd(_cur_op,1);

    if (tn_registers_identical(op1tn, basetn) && TN_has_value(offsettn) && (TN_value(offsettn)==0))	 {
      OP_Change_Opcode(_cur_op, newtop);	
      return TRUE;
    }
    return FALSE;
}

/*
 * Computer the pc-relative offset
 */
static int 
Label_Offset (OP *op, LABEL_IDX lab) {
  int i=0;
  BB *cur_bb = OP_bb(op);
  OP *tmp_op;
  if (OP_bb(op) == NULL)   return (Max_INT5+1);//false
 
  BB *targ_bb = Get_Label_BB(lab);

  i = 0;
  cur_bb = OP_bb(op);

  for (cur_bb = BB_next(cur_bb); cur_bb; cur_bb = BB_next(cur_bb)) {
      if (BB_asm(cur_bb)) 
	  return (Max_uINT5+1);
      else if (BB_call(cur_bb) || BB_exit(cur_bb) || BB_zdl_prolog(cur_bb)) {
	  i = i + 4;	 // jr/jplnk/loop/ret for quad-word alignment
      } else if ((i > Max_uINT5) || (cur_bb == targ_bb)) 
	  return i;
      i = i + BB_length(cur_bb);
    
  }

}

/*replace br.eq br.ne */
BOOL 
Repl_Size16_Instr::Replace_BR() {
  TOP newtop;
  TOP oldtop = OP_code(_cur_op);
  switch (oldtop) {
  	case TOP_beq: newtop= TOP_br16_eqz; break;
	case TOP_bne: newtop = TOP_br16_nez; break;
	default:
	  Is_True(FALSE,("%s is not branch", (oldtop)));	
  }

  
  TN *op1tn = OP_opnd (_cur_op, 0);
  TN *op2tn = OP_opnd (_cur_op, 1);
  TN *op3tn = OP_opnd (_cur_op, 2);
  
  if (tn_registers_identical(op2tn, Zero_TN)) {
  	if (TN_is_label(op3tn)) {
	 LABEL_IDX lab = TN_label(op3tn); 
      	 int offset = Label_Offset(_cur_op, lab);
         if ((offset <=Max_INT5) && (offset >= Min_uINT5) ) {
	   OP_Change_Opcode(_cur_op, newtop);
           return TRUE;
         } 
		
  	}
  }
  return FALSE;
}


/*
 *  Result tn equals One of two operands
   another operands is simm5 of arithmatic and uimm5 of logic
 */

BOOL
Repl_Size16_Instr::Replace_Restricted_Imm_OP () {
  
  TOP newtop;
  TOP oldtop = OP_code(_cur_op);
 
  switch (oldtop){
    case TOP_sll:   newtop = TOP_shll16_i; break;
    case TOP_srl:   newtop = TOP_shrl16_i; break;
    case TOP_sra:   newtop = TOP_shra16_i; break; 
    case TOP_ori:   newtop= TOP_or16_i;  break;
    case TOP_andi: newtop = TOP_and16_i; break;
    case TOP_xori:  newtop = TOP_xor16_i; break;
    default:
      Is_True(FALSE,("%s is not Restricted_Imm", OP_code(_cur_op)));
  }
  
  if (OP_results(_cur_op) > 1 || OP_opnds(_cur_op) > 2) {
    return FALSE;
  }   

  //just deal with one result and two operands

  TN *rtn = OP_result (_cur_op, 0);
  TN *op1tn = OP_opnd (_cur_op, 0);
  TN *op2tn = OP_opnd (_cur_op, 1);

  Is_True (TN_is_constant(op2tn),("function called by wrong phase"));

  if( Tn_Is_Reloc_TN (op2tn) ) 
    return FALSE; 

  INT32 op2value = TN_value (op2tn);

  //special case: mv16 mvfra16 mv16.i mvtra16
  if ( oldtop == TOP_ori ) {
    if ( !TN_is_symbol(op2tn) && (op2value== 0) ) {
       //special case: [$rd,$rs,0]
      OP *new_op = Mk_OP (TOP_mv16, rtn, op1tn, Zero_TN);
      Replace_CurOP(new_op);
      return TRUE;       
    } 
    else if ( tn_registers_identical(op1tn , Zero_TN) && 
              Replaceable_TOP_Imm( oldtop , op2value)) { 
      //special case: [$rd,$0,imm5]
      OP *new_op = Mk_OP (TOP_mv16_i, rtn, op1tn, op2tn);
      Replace_CurOP(new_op);
      return TRUE;
    }
   
  } 

  if (tn_registers_identical (rtn, op1tn) ) {
    if (Replaceable_TOP_Imm(oldtop , op2value)) {      
      OP_Change_Opcode(_cur_op, newtop);
      return TRUE;
    }
    
  }
 
  
  return FALSE;
}


/*
 * ld.w /st.w 
 */

BOOL
Repl_Size16_Instr::Replace_LdSt_OP () {
 
  TOP newtop;
  TOP oldtop = OP_code(_cur_op);

  switch (oldtop){
    case TOP_lw: newtop = TOP_pop16;  break;
    case TOP_sw: newtop = TOP_push16; break;
    default:
      Is_True(FALSE,("%s is not Special Register", OP_code(_cur_op)));
  }

  TN *op1tn, *offsettn, *basetn;
  
  // ld.w one result and two operands
  if(oldtop == TOP_lw){

    op1tn = OP_result(_cur_op,0);
    basetn = OP_opnd(_cur_op,0);
    offsettn = OP_opnd(_cur_op,1);
  }
  //st.w 3 operands
  else{
    op1tn = OP_opnd(_cur_op,0);
    basetn = OP_opnd(_cur_op,1);
    offsettn = OP_opnd(_cur_op,2);
  }
  
  Is_True( TN_is_register(op1tn) && TN_is_register(basetn) && TN_is_constant(offsettn), 
           ("function called by wrong phase") );

  if( Tn_Is_Reloc_TN (offsettn) )
    return FALSE;
  // case 1: basetn is $sp, offset is 7-bit unsignedimm
  if(tn_registers_identical( basetn, SP_TN) ) {
    int offset;
     /* special case for stack symbols */
    if ( TN_is_symbol(offsettn) ) {
      ST *base_st, *st;
      INT64 base_ofst;

      st = TN_var(offsettn);
      Base_Symbol_And_Offset (st, &base_st, &base_ofst);
      if (base_st == SP_Sym ) {
      offset = base_ofst + TN_offset(offsettn);
      	} else {
      	  Is_True(0,("load/store to 16-bit, must be sp related"));
      	}
    } else if (TN_has_value(offsettn)){
      offset = TN_value (offsettn);
    } else {
      Is_True(0, ("Replace_LdSt_OP::  offset must be imm or $sp related symbol"));
    }

    
    if(Replaceable_TOP_Imm(oldtop , offset)) {
      OP_Change_Opcode(_cur_op, newtop);
      return TRUE;
    }
  } 

//  case 2: offset is zero
  if (1) {
    int offset;
    if (TN_has_value(offsettn)){
      offset = TN_value (offsettn);
    } else {
      return FALSE;
    }
    if (offset == 0) {
	if (oldtop == TOP_lw)	{
	  OP_Change_Opcode(_cur_op, TOP_ldw16);
	} else {
	  OP_Change_Opcode(_cur_op, TOP_stw16);
	}
	return TRUE;
    } 
	
  } 

  return FALSE;
}

/*
 * current op could be replaced directly
 */
BOOL
Repl_Size16_Instr::Replace_Direct_OP () {
  
  TOP newtop;
  switch (OP_code(_cur_op)){
    case TOP_mvtc:  newtop = TOP_mvtc16; break;
    case TOP_mvfc:  newtop = TOP_mvfc16; break;
    case TOP_jr:    newtop = TOP_jr16;    break;
    case TOP_jalr:  newtop = TOP_jr16_lnk; break; 
    case TOP_ret:  newtop = TOP_ret16;  break;
    case TOP_mc_abs: newtop = TOP_abs16; break;
    default:
      Is_True(FALSE,("%s is not Directly ", OP_code(_cur_op)));
  }
  OP_Change_Opcode(_cur_op, newtop);
  return TRUE;
}

BOOL
Repl_Size16_Instr::Replace_NOP () {
  
  Is_True(OP_code(_cur_op) == TOP_nop,
            ("%s is not NOP\n", OP_code(_cur_op)));

  //if prev_op is 16-bit instr, replace this nop of nop16
  if(OP_16bit_op( prev_16op) && !OP_Paired_op(prev_16op) ) {
    OP_Change_Opcode(_cur_op, TOP_nop16);
    return TRUE;
  }
  return FALSE;
}

BOOL 
Repl_Size16_Instr::Replace_ABS () {
  Is_True(OP_code(_cur_op) == TOP_mc_abs,
            ("%s is not mc.zn.lt \n", OP_code(_cur_op)));
  TN *op1, *result;
  result = OP_result(_cur_op, 0);
  op1 = OP_opnd(_cur_op,0);
 
  if (tn_registers_identical(result, op1)) {
    OP_Change_Opcode(_cur_op, TOP_abs16);
    return TRUE;
  }
  return FALSE;
}


/*
 * current op is replaceable ? if yes, replace it
 */
static void Find_Candicate_OP (OP *op){
	
  Repl_Size16_Instr *candi_instr = CXX_NEW(Repl_Size16_Instr(op), &MEM_local_pool); 
  BOOL replaced= FALSE;
  OP *curop=NULL;
  switch(OP_code(op)){
    case TOP_mc_abs:
      replaced = candi_instr->Replace_ABS();
      break;
    case TOP_addu:
    case TOP_and:
    case TOP_or:
    case TOP_xor:
    case TOP_nor:
      replaced = candi_instr->Replace_ResEqOper_OP();
      break;
    case TOP_subu:
    case TOP_sllv:
      replaced = candi_instr->Replace_ResEqOper_OP(FALSE);	
      break;
    case TOP_addiu:
      replaced = candi_instr->Replace_ADDI();	
      break;	  
    case TOP_srl:
    case TOP_sll:	
    case TOP_sra:
    case TOP_ori:
    case TOP_andi:
    case TOP_xori:
      replaced = candi_instr->Replace_Restricted_Imm_OP();
      break;
    case TOP_lw:
    case TOP_sw:
      replaced = candi_instr->Replace_LdSt_OP();
      break;
    case TOP_jr:
    case TOP_jalr:
    case TOP_mvtc:
    case TOP_mvfc:
    case TOP_ret:
      replaced = candi_instr->Replace_Direct_OP();
      break;
  
    case TOP_lbu:
    case TOP_lhu:
      replaced = candi_instr->Replace_LDU();
      break;
    case TOP_beq:
    case TOP_bne:
      if (CG_Enable_br16)
        replaced = candi_instr->Replace_BR();
      break;
    default:
      //cann't be replaced
      break;
  }

  if (replaced) {
    Set_OP_16bit_op(candi_instr->Get_OP());
  }

}


BOOL Reback_Size32_Instr (OP *op) {
   Is_True(OP_16bit_op(op), ("should be 16bit op"));

   TOP top32;
   switch (OP_code(op)) {
     case TOP_abs16:    top32 = TOP_mc_abs; break;
     case TOP_add16:    top32 = TOP_addu; break;
     case TOP_add16_i:  top32 = TOP_addiu; break;
     case TOP_add16_sp: top32 = TOP_addiu; break;
     case TOP_and16:    top32 = TOP_and; break;
     case TOP_and16_i:  top32 = TOP_andi; break;
     case TOP_ldw16:    top32 = TOP_lw; break;
     case TOP_ldub16_rs: top32 = TOP_lbu; break;
     case TOP_lduh16_rs: top32 = TOP_lhu; break;
     case TOP_mv16:     top32 = TOP_addu; break;
     case TOP_mv16_i:   top32 = TOP_addiu; break;
     case TOP_inv16:    top32 = TOP_nor; break;
     case TOP_or16:     top32 = TOP_or; break;
     case TOP_or16_i:   top32 = TOP_ori; break;
     case TOP_pop16:	top32 = TOP_lw; break;
     case TOP_push16:   top32 = TOP_sw; break;
     case TOP_shll16:   top32 = TOP_sllv; break;
     case TOP_shll16_i: top32 = TOP_sll; break;
     case TOP_shra16:   top32 = TOP_srav; break;
     case TOP_shra16_i: top32 = TOP_sra; break; 
     case TOP_shrl16:   top32 = TOP_srlv; break; 
     case TOP_shrl16_i: top32 = TOP_srl; break; 
     case TOP_stw16:    top32 = TOP_sw; break;
     case TOP_sub16:    top32 = TOP_subu; break;
     case TOP_sub16_i:  top32 = TOP_addiu; break;
     case TOP_xor16:    top32 = TOP_xor; break;
     case TOP_xor16_i:  top32 = TOP_xori; break;	
     case TOP_br16_eqz: top32 = TOP_beq; break;
     case TOP_br16_nez: top32 = TOP_bne; break;
     case TOP_jr16:
     case TOP_jr16_lnk:
     case TOP_ret16:
     case TOP_mvfc16:
     case TOP_mvtc16:
         top32 = TOP_UNDEFINED;
         break;
     default:
       Is_True(0, ("unknown 16bit opcode"));
     }
     if (top32 != TOP_UNDEFINED) {
       if (OP_code(op) == TOP_sub16_i) {
         INT32 value = TN_value(OP_opnd(op, 1));
         TN *valuetn = Gen_Literal_TN(-value, 4);
         Set_OP_opnd(op, 1, valuetn); 
       }
       OP_Change_Opcode(op, top32);
       return 1;  
     }
     return 0;
}

/*
 * main entry
 */
void Replace_Size16_Instr (void) {

  BB* bb;
  OP* op;

  MEM_POOL_Push(&MEM_local_pool);

  //16-bit instr displacement
  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next (bb) ) {
     FOR_ALL_BB_OPs_FWD ( bb, op ) {
      Find_Candicate_OP (op); 
    }
  }	

  MEM_POOL_Pop(&MEM_local_pool);
	
}

/*new main entry*/
void Replace_Size16_Instr(BB *bb) {
   OP* op;	
   FOR_ALL_BB_OPs_FWD ( bb, op ) {
     Find_Candicate_OP (op); 
  }
}
  
 
void Guarantee_Paired_instr16 (void) {
  BB* bb;
  OP* op;	
    prev_16op = NULL;	
    for ( bb = REGION_First_BB; bb != NULL; bb = BB_next (bb) ) {
      FOR_ALL_BB_OPs_FWD ( bb, op ) {
	  if (!op)
	  	break;
	  if (OP_dummy(op)) 
	  	continue;
	  if (!prev_16op && OP_16bit_op(op) && !OP_Paired_op(op) ) {
	  	prev_16op = op;
		continue;
	  }
	  if (prev_16op && (OP_code(op) == TOP_nop)) {
	    if (!CG_rep_unpaired16) { 
	      OP_Change_Opcode(op, TOP_nop16);
	      Set_OP_Paired_op(prev_16op);
              Set_OP_Paired_op(op);
	      prev_16op = NULL;
	    } else {
	      // back to 32bit op
	      if (!Reback_Size32_Instr(prev_16op)) {
                OP_Change_Opcode(op, TOP_nop16);
                Set_OP_Paired_op(prev_16op);
                Set_OP_Paired_op(op); 
              }
	      prev_16op = NULL;
            }
	  	continue;
	  }
	  if (prev_16op && OP_16bit_op(op)) {
	    Set_OP_Paired_op(prev_16op);
	    Set_OP_Paired_op(op);
	    prev_16op = NULL;
	    continue;
	  }
	  if (prev_16op && !OP_16bit_op(op)) {
	    if (!CG_rep_unpaired16) {
	      //insert nop16 after current op
	      OP* nop16 = Mk_OP(TOP_nop16);
              BB_Insert_Op_After ( bb, prev_16op, nop16);
              Set_OP_Paired_op(prev_16op);
              Set_OP_Paired_op(nop16);
	      prev_16op = NULL;
	    } else {
	      // back to 32bit op
              if (!Reback_Size32_Instr(prev_16op)) {
                OP* nop16 = Mk_OP(TOP_nop16);
                BB_Insert_Op_After ( bb, prev_16op, nop16);
                Set_OP_Paired_op(prev_16op);
                Set_OP_Paired_op(nop16);
              }
              prev_16op = NULL;
	    }
            continue;
	  }
    }
    // last op is 16-bit
    if (prev_16op && !OP_Paired_op(prev_16op) ) {
      if (!CG_rep_unpaired16) {	
        OP* nop16 = Mk_OP(TOP_nop16);
        if (OP_br(prev_16op) || OP_xfer(prev_16op)) {
          BB_Insert_Op_Before( bb, prev_16op, nop16);
        } else {
          BB_Insert_Op_After( bb, prev_16op, nop16);
        }
        Set_OP_Paired_op(prev_16op);
        Set_OP_Paired_op(nop16);
        prev_16op = NULL;
      } else {
        // back to 32bit op
        if (!Reback_Size32_Instr(prev_16op)) {
          OP* nop16 = Mk_OP(TOP_nop16);
          if (OP_br(prev_16op) || OP_xfer(prev_16op)) {
            BB_Insert_Op_Before(bb, prev_16op, nop16);
          } else {
            BB_Insert_Op_After( bb, prev_16op, nop16);
          }
          Set_OP_Paired_op(prev_16op);
          Set_OP_Paired_op(nop16);
        }
        prev_16op = NULL;
      }
    }
  }

}	

/*return control register index*/
UINT32 Control_Register_Index( TN *tn) {
  INT idx =  -1;
  if (tn == RA_TN) 
  	idx = 4;
  else if (tn == JA_TN)
  	idx = 5;
  else if (tn == LC0_TN)
  	idx = 0;
  else if (tn == LC1_TN)
  	idx = 1;
  else if (tn == LC2_TN)
  	idx = 2;
  else if (tn ==  LC3_TN)
  	idx = 3;
  else {
  	Is_True(0, ("Not a control register"));
  }
  return idx;
}

/*
 * mvtc/mvtc16 and its consumer could not exist in the same quadword block
 */
void Check_QuadWord_Boundary_Absolute(void) {
  BB *bb= NULL;
  OP* op= NULL;
  UINT32 pc = 0;
 
  MEM_POOL_Push(&MEM_local_pool);
  vector<MVTCInfor *>  *mvtcop = CXX_NEW(vector<MVTCInfor *>, &MEM_local_pool);
  vector<MVTCInfor *>  *mvfcop = CXX_NEW(vector<MVTCInfor *>, &MEM_local_pool);

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next (bb) ) {
    FOR_ALL_BB_OPs_FWD ( bb, op ) {
	
      if (TOP_is_mvtc(OP_code(op))) {
         MVTCInfor *mvtcinfor = CXX_NEW((MVTCInfor), &MEM_local_pool) ;
         UINT32 control_register_idx = Control_Register_Index(OP_result(op, 0));
         mvtcinfor->cr_idx = control_register_idx;
         mvtcinfor->pc = pc;
         mvtcop->insert(mvtcop->begin(), mvtcinfor);	
      } else if ( TOP_is_use_ctrl_reg(OP_code(op))) {
         if (mvtcop->size() > 0) {
           int cr_idx = -1;
	   if (OP_code(op ) == TOP_loop) {
	     cr_idx = Control_Register_Index(OP_opnd(op, 2));
	   } else {
	     cr_idx = Control_Register_Index(OP_opnd(op, 0));
	   }
	   
           vector<MVTCInfor *>::iterator temp;
           for (temp = mvtcop->begin(); temp != mvtcop->end(); temp++) {
	     if ((*temp)->cr_idx == cr_idx) {
	       BB *cur_bb = OP_bb(op);	
	       INT num =(pc - ((*temp)->pc));	
	       if (((16-num)%4)==0) 
                 num = ((16-num)/4);	
	       else
	         num = ((16-num)/4)+1;
               if (num < 0) {
	         mvtcop->erase(temp);	
		 break;
	       }
               Is_True( (num >=0 && num <=4), ("nop num is between 0-3"));
               {
                 for (int j =0; j< num ; j++) {
		   OP *op1 = Mk_OP(TOP_nop);
	           BB_Insert_Op_Before(cur_bb, op, op1);
	           pc = pc + 4;		 
                 }
	       }

	       mvtcop->erase(temp);	
	       break;
	    }
          }  	
	}
     }
     if ((OP_code(op) == TOP_mvfc16) && (Control_Register_Index(OP_opnd(op, 0)) == 4)) {
            MVTCInfor *mvfcinfor = CXX_NEW((MVTCInfor), &MEM_local_pool) ;
            mvfcinfor->cr_idx = 4;
            mvfcinfor->pc = pc;
            mvfcop->insert(mvfcop->begin(), mvfcinfor);
     } else if ((mvfcop->size() > 0) && TOP_is_lnk(OP_code(op))) {
        vector<MVTCInfor *>::iterator temp;
        for (temp = mvfcop->begin(); temp != mvfcop->end(); temp++) {
            BB *cur_bb = OP_bb(op);
            INT num =(pc - ((*temp)->pc));
            if (((16-num)%4)==0)
               num = ((16-num)/4);
            else
               num = ((16-num)/4)+1;
            if (num < 0) {
               mvfcop->erase(temp);
               break;
            }
            Is_True( (num >=0 && num <=4), ("nop num is between 0-3"));
            {
              for (int j =0; j< num ; j++) {
	        OP *op1 = Mk_OP(TOP_nop);
                BB_Insert_Op_Before(cur_bb, op, op1);
                pc = pc + 4;
              }
             }
             mvfcop->erase(temp);
             break;
          }
     }
     INT num_inst_words = OP_Real_Inst_Words (op);
     pc = pc + (2*num_inst_words); // for SL, incr are in half words num_inst_words);
   }
  }
  MEM_POOL_Pop(&MEM_local_pool);
}


inline OP*
BB_last_real_op(BB *bb) {
  OP *last_op;
  for (last_op = BB_last_op(bb); last_op; last_op = OP_prev(last_op)) {
    if (!OP_dummy(last_op)) break;
  }
  return last_op;
}

inline OP* 
OP_prev_real_op(OP *op) {
 OP *prev_op;
 for (prev_op = OP_prev(op); prev_op; prev_op = OP_prev(prev_op)) {
   if (!OP_dummy(prev_op)) break; //skip noop
 }
 return prev_op;
}

void Check_Br16 (int first=0) {
  BB *bb= NULL;
  OP* op= NULL;
  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next (bb) ) {
    op = BB_last_real_op(bb);
    if (!op ) 
      continue;
    if (OP_code(op) == TOP_br16_eqz || OP_code(op) == TOP_br16_nez) {
      TN *op1tn = OP_opnd (op, 0);
      TN *op2tn = OP_opnd (op, 1);
      TN *op3tn = OP_opnd (op, 2);
      Is_True(tn_registers_identical(op2tn, Zero_TN), ("second operand is zero"));
      if (TN_is_label(op3tn)) {
	 LABEL_IDX lab = TN_label(op3tn); 
      	 int offset = Label_Offset(op, lab);
         int upbound = first == 1 ? Max_uINT5 :Max_INT5 ;
         if (offset > upbound) {
	   TOP newtop = (OP_code(op)==TOP_br16_eqz) ? TOP_beq : TOP_bne ;
           OP_Change_Opcode(op, newtop);
	    OP *prev16 = OP_prev_real_op(op);
	   
	   if (OP_code(prev16) == TOP_nop16) {
	     OP_Change_To_Noop(prev16);
	   } else if (OP_16bit_op(prev16)) {
	      BB_Insert_Op_Before(op->bb, op, Mk_OP(TOP_nop16)); 
	   }
         } 
      }
    }
  }

}




