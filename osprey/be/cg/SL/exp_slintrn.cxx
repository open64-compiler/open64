/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include <intrn_info.h>
#include <stdint.h>
#include <stdarg.h>
#include "defs.h"
#include "config.h"
#include "glob.h"
#include "util.h"
#include "tn.h"
#include "cg_flags.h"
#include "bb.h"
#include "symtab.h"
#include "opcode.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "op.h"
#include "cgexp.h"
#include "w2op.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "printsrc.h"
#include <cmplrs/rcodes.h>

#ifdef TARG_SL
#include <map>
using std::map;
#include "be_symtab.h"
static BOOL dotrace = FALSE; 
#define TFile stdout

#include "targ_const_private.h"
#endif 

extern std::map<INT32, TN*> var2acc;
extern std::map<INT32, TN*> var2addr;
extern INT32 AccPregN;
extern INT32 AddPregN;
extern void Set_OP_To_WN_Map(WN *wn);
extern void Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops);
extern INT32 CG_Max_Accreg;
extern INT32 CG_Max_Addreg;
UINT16 prev_alloc_AccIndex = 0;
UINT16 prev_alloc_AddrIndex = 0;

typedef enum {
  UIMM1,
  IMM1,
  UIMM2,
  UIMM3,
  IMM3,
  UIMM4,
  UIMM5,
  IMM5,
  UIMM9,
  IMM9,
  IMM10,
  UIMM16,
  IMM16,
  IMM_UNDEF
} IMM_Type;

void
User_Error(WN* intrncall, const char *format, ...)
{
  const char *func_name = Cur_PU_Name;
  const char *file_name = (Orig_Src_File_Name ? Orig_Src_File_Name : Src_File_Name);

  extern SRCPOS current_srcpos;
  int line_no = SRCPOS_linenum(current_srcpos);

  va_list args;
  va_start (args, format);
  fprintf(stderr, "%s : in function %s\n", file_name, func_name);
  fprintf(stderr, "%s:%d: error : ", file_name, line_no);

  if ((WN_intrinsic(intrncall) >= INTRN_C3_INIT_ACC) 
    && (WN_intrinsic(intrncall) <= INTRN_COPY_HI)) {
    const char *intrn_name = INTRN_c_name(WN_intrinsic(intrncall));
    fprintf(stderr, "%s : ", intrn_name);
  }
  
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  va_end (args);
  exit(RC_USER_ERROR);
}

/* Check user's parameter */
#define Is_Param_Valid(Cond, ParmList)				\
    ( Cond ? (void) 1						\
           : (User_Error ParmList) )

/*Given an acc tn, print the variable map to acc special register */
void Print_Acc (TN *acctn) {
  typedef std::map<INT32, TN*> Var2Acc;
  Var2Acc::iterator it = var2acc.begin();
  fprintf(TFile, "ACC[%d] --> ", (TN_number(acctn) - 271));
  for(;it != var2acc.end();++it) {
    if ((*it).second == acctn) {
      if ((*it).first >= 0)
        fprintf(TFile,  " GPR[%d]   ", (*it).first);
      else
        fprintf(TFile,  "  ST [%s]   ",  ST_name(-(*it).first));
    }
  }
  fprintf(stdout, "\n");
}

/*Given an addr tn, print the variable map to addr special register */
void Print_Addr (TN *addrtn) {
  typedef std::map<INT32, TN*> Var2Addr;
  Var2Addr::iterator it = var2addr.begin();
  fprintf(TFile, "ADDR[%d] --> ", (TN_number(addrtn) - 275));
  for(;it != var2addr.end();++it)
  {
    if ((*it).second == addrtn) {
      if ((*it).first >= 0)
        fprintf(TFile,  " GPR[%d]   ", (*it).first);
      else
        fprintf(TFile,  "  ST [%s]   ",  ST_name(-(*it).first));
    }
  }
  fprintf(stdout, "\n");
}


// clean var2acc and var2addr
void Initial_var2spe() {
  if (var2acc.size() > 0) {
    typedef std::map<INT32, TN*> Var2Acc;
    Var2Acc::iterator it = var2acc.begin();
    Var2Acc::iterator tempit;
    for(;it != var2acc.end();)
    {
      tempit = it;
      it++;
      var2acc.erase((*tempit).first);
    }
  }
  if (var2addr.size() > 0) {
    typedef std::map<INT32, TN*> Var2Addr;
    Var2Addr::iterator it = var2addr.begin();
    Var2Addr::iterator tempit;
    for(;it != var2addr.end();)
    {
      tempit = it;
      it++;
      var2addr.erase((*tempit).first);
    }
  }
  //bug fix 537
  for (int i = 0; i < 4; i++ )
    ACCreg[i] = 0;
  for (int i = 0; i < 8; i++ )
    Addreg[i] = 0;
  return;
}

static BOOL Has_VarWN_idx(WN *stmt) {
  Is_True(WN_operator(stmt) == OPR_STID, ("statement should be stid "));
  if (WN_class(stmt) == CLASS_PREG) {
    if (WN_operator(stmt) == OPR_STID) {
       return TRUE;
    }
  } else if (WN_class(stmt) == CLASS_VAR) {
     return TRUE;
  }
  return FALSE;
}
           			
/*
 *  Pair <preg_num/st_idx,  special_reg_tn> 
 *  get pregnum or st_idx of variable 
 */
static INT Get_VarWN_idx (WN *stmt) {

  Is_True(WN_operator(stmt) == OPR_STID || WN_operator(stmt) == OPR_LDID, ("stid or ldid"));

  if (WN_class(stmt) == CLASS_PREG) {
    if (WN_operator(stmt) == OPR_STID )
      return WN_store_offset(stmt);
    else if (WN_operator(stmt) == OPR_LDID)
      return WN_load_offset(stmt);
  } else if (WN_class(stmt) == CLASS_VAR) {
    return (-WN_st_idx(stmt));
  } else if (WN_class(stmt) == CLASS_BLOCK) {
    return WN_st_idx(stmt); // bug fix 634
  } else {
    Is_True(0, ("NYI"));
  }
}

/*  if rhs of statement is c3 intrnsic op, 
 *  we save the result variable into global variable AddPregN/AccPregN and map it with special register later
 */
void Set_IntrnOP_Result (WN *stid) {
  if (WN_operator(WN_kid0(stid)) == OPR_INTRINSIC_OP ) {
    INTRINSIC id = (INTRINSIC) WN_intrinsic (WN_kid0(stid));
    //  only deal with c3 intrinsic op
    if (id >= INTRN_C3_INTRINSIC_BEGIN && id <= INTRN_C3_INTRINSIC_END) {
      if (id == INTRN_C3_PTR)
        AddPregN = Get_VarWN_idx(stid);
      else
        AccPregN = Get_VarWN_idx(stid);
    }
  }
  return; 
}

/*
 *  Get immediate value of wn 
 *  if wn is an const , return WN_const_val(wn)
 *  else if WN is preg num, get the value of " home wn"
 *  some parameter of c3 intrnsic ops are const value 
 *  in WOPT phase, some const value will be translated to a "preg variable" by LPRE
 *  in CG phase, we get the "home wn" of preg variable for generating right op operand
 */
TN *Get_Liternal_TN  (WN *intrncall, INT kid_id, INT size) {
  Is_Param_Valid(WN_kid(intrncall, kid_id), (intrncall, "argument %d is null", kid_id+1));

  WN *wn = WN_kid0(WN_kid(intrncall, kid_id)); 
  INT val=0;

  if (WN_operator(wn) == OPR_INTCONST) {
    val = WN_const_val(wn);
  } else {
    Is_True(WN_class(wn) == CLASS_PREG, ("WN should be PREG variable"));
    WN *home = Preg_Home(WN_load_offset(wn));
    Is_Param_Valid(home && WN_operator(home) == OPR_INTCONST,
                  (intrncall, "arguments %d should be immediate", kid_id+1));
    val = WN_const_val(home);
  }
  return Gen_Literal_TN(val, size);
}

void Is_IMM_Valid (WN *intrncall, INT kid_id, TN *imm_tn, IMM_Type imm_type)
{
  FmtAssert(TN_has_value(imm_tn), ("Is_IMM_Valid: Not IMM TN"));
  INT value = TN_value(imm_tn);
  BOOL valid;
  const char *str;
  switch (imm_type) {
    case UIMM1 : valid = ((value >= 0) && (value <= 1)); str="UIMM1"; break; 
    case UIMM2 : valid = ((value >= 0) && (value <= 3)); str="UIMM2"; break; 
    case UIMM3  : valid = ((value >= 0) && (value <= 7)); str="UIMM3"; break;
    case IMM3  : valid = ((value >= -4) && (value <= 3)); str="IMM3"; break;
    case UIMM4  : valid = ((value >= 0) && (value <= 15)); str="UIMM4"; break;
    case UIMM5 : valid = ((value >= 0) && (value <= 31)); str="UIMM5"; break;    
    case IMM5  : valid = ((value >= -16) && (value <= 15)); str="UIMM5"; break;
    case UIMM9 : valid = ((value >= 0) && (value <= 511)); str="UIMM9"; break;
    case IMM9  : valid = ((value >= -256) && (value <= 255)); str="IMM9"; break;
    case IMM10  : valid = ((value >= -512) && (value <= 511)); str="IMM10"; break;
    case UIMM16: valid = ((value >= 0) && (value <= 65535)); str="UIMM16"; break;
    case IMM16 : valid = ((value >= -32768) && (value <= 32767)); str="IMM16"; break;
  }
  Is_Param_Valid(valid, (intrncall, "argument %d is not in range(%s)", kid_id+1, str));
  return;
}

/* get variable index from TN*/
static INT TN_To_Index(TN *tn) {
  if (TN_is_symbol(tn)) {
    Is_True(TN_var(tn), ("symbol tn should have var"));
    return (-TN_var(tn)->st_idx);
  }
  else  // need some condition
    return TN_To_PREG(tn);
}

/* called by EXP_COPY
 * if src_tn is mapped to a special register
 * do mapping tgt_tn to the same special register
 */
void Copy_Tn_MapInfo(TN *src_tn, TN *tgt_tn) {
  if ( TN_register_class(tgt_tn) == TN_register_class(src_tn)) {
    INT32 srcpreg = TN_To_Index(src_tn);
    if ((var2acc.size() > 0) && var2acc[srcpreg]) {
      INT32 tgtpreg = TN_To_Index(tgt_tn);
      var2acc[tgtpreg] = var2acc[srcpreg];
      if (dotrace) {	
        fprintf(TFile, "EXP_COPY::");	
        Print_Acc(var2acc[srcpreg]);	
      }
    } 
    else if ((var2addr.size() > 0) && var2addr[srcpreg]) {
      INT32 tgtpreg = TN_To_Index(tgt_tn);
      var2addr[tgtpreg] = var2addr[srcpreg];
      if (dotrace) {		
        fprintf(TFile, "EXP_COPY::");	
        Print_Addr(var2addr[srcpreg]);	
      }
    }
  }
}

/*called by HANDLE_STID
 * if src_tn is mapped to a special register
 * do mapping tgt_tn to the same special register
 */
void Copy_Preg_MapInfo(WN *stid) {
    WN *ldid = WN_kid0(stid);
    if (WN_operator(ldid) != OPR_LDID ) {
      return;   
    }
    Is_True(WN_operator(ldid) == OPR_LDID, ("ldid"));
    if (!(WN_class(ldid) == CLASS_PREG) && !(WN_class(ldid) == CLASS_VAR)) {
      return;
    }
    INT32 srcpreg = Get_VarWN_idx(ldid);
    // fix bug 232: st of stid is mapped to acc
    INT32 tgtpreg; 
    if (Has_VarWN_idx(stid)) {
      tgtpreg = Get_VarWN_idx(stid);
      if (var2acc[tgtpreg]) {
        var2acc[srcpreg] = var2acc[tgtpreg];
        if (dotrace) {
          fprintf(TFile, "HANDLE_STID::");
          Print_Acc(var2acc[tgtpreg]);
        }
      } else if (var2addr[tgtpreg]) {
        var2addr[srcpreg] = var2addr[tgtpreg];
        if (dotrace) {
          fprintf(TFile, "HANDLE_STID::");
          Print_Addr(var2addr[tgtpreg]);
        }
      } else if (var2acc[srcpreg]) {
        tgtpreg = Get_VarWN_idx(stid);
        var2acc[tgtpreg] = var2acc[srcpreg];
        if (dotrace) {	
          fprintf(TFile, "HANDLE_STID::");	
          Print_Acc(var2acc[srcpreg]);	
        }
      } else if (var2addr[srcpreg]) {
        tgtpreg = Get_VarWN_idx(stid);
        var2addr[tgtpreg] = var2addr[srcpreg];
        if (dotrace) {		
          fprintf(TFile, "HANDLE_STID::");	
          Print_Addr(var2addr[srcpreg]);	
        }
      }
   }
}

static INT32 Get_ParmVaridx_Intrncall(WN *intrncall, int kidnum=0) {
  
  Is_Param_Valid(WN_kid(intrncall, kidnum), (intrncall, "argument %d is null", kidnum+1));
  WN *para_wn = WN_kid0(WN_kid(intrncall, kidnum));
  OPERATOR opr = WN_operator(para_wn) ;
  if (opr== OPR_LDID) {
    return Get_VarWN_idx(para_wn);
  } else if (opr == OPR_INTRINSIC_OP) {
    INTRINSIC id = (INTRINSIC) WN_intrinsic (para_wn);
    if ((id >= INTRN_C3_INTRINSIC_BEGIN)  && (id <= INTRN_C3_INTRINSIC_END)) {
      if (id == INTRN_C3_PTR) 
        return Get_ParmVaridx_Intrncall(para_wn, 1);
      else 
        return Get_ParmVaridx_Intrncall(para_wn);
    } else {
      Is_True(0, ("WN must be c3 intrinsic "));
    }
     
  }  
  return 0;
  
}

/*  deal with special case: the result of intrncall is pointer value
    *p = intrinsic_c3_init_acc(*p) 
    the whirl is

     I4INTCONST 0 (0x0)
  I4PARM 2 T<4,.predef_I4,4> #  by_value 
 I4INTRINSIC_CALL <877,INTRN_C3_INIT_ACC> 126 # flags 0x7e
  I4I4LDID 2 <1,2,.preg_I4> T<4,.predef_I4,4> # $r2
 I4STID 72 <1,2,.preg_I4> T<4,.predef_I4,4> # __comma
  I4I4LDID 72 <1,2,.preg_I4> T<4,.predef_I4,4> # __comma
  U4U4LDID 85 <1,4,.preg_U4> T<48,anon_ptr.,4> # L_tdist
 I4ISTORE 0 T<48,anon_ptr.,4>
 ...
   U4U4LDID 85 <1,4,.preg_U4> T<48,anon_ptr.,4> # L_tdist
  I4I4ILOAD 0 T<4,.predef_I4,4> T<48,anon_ptr.,4>
 I4STID 78 <1,2,.preg_I4> T<4,.predef_I4,4> # <preg>            -> map to acctn
*/
static INT32 Get_ResultEqVarIdx ( WN *istore) {
  Is_True(WN_operator(istore) == OPR_ISTORE , ("operator must be istore")) ;

  WN *next = WN_next(istore);
  INT32 varidx = Get_VarWN_idx(WN_kid1(istore));
  WN_OFFSET  addroff = WN_offset(istore);            

  while(next) {
    if ((WN_operator(next) == OPR_STID) && 
        (WN_operator(WN_kid0(next)) == OPR_ILOAD)) {
      WN *load = WN_kid0(next);
      if ((varidx == Get_VarWN_idx(WN_kid0(load)))  && (addroff == WN_offset(load)))
        return Get_VarWN_idx(next);	
      }
      next = WN_next(next);
    }

    if (!next) {
      if (WN_operator(WN_kid0(istore)) == OPR_LDID) {
        return Get_VarWN_idx(WN_kid0(istore));
      } else {
        DevWarn("init special register is undefined");
        return -1;
      }
    }

}

static INT32 Get_Resultidx_Intrncall (WN *intrncall) {
  	
  // get st of result variable now
  //     parm
  //  intrinsic c3 call
  //     ldid r2
  //  stid preg  __comma   <- temp equivalence  variable
  //  ...
  //     ldid __comma
  //  stid variable          <- get the st of variable map to acc
  WN *next= WN_next(intrncall); // get stid from $r2 to __comma or other st
  WN *retval = WN_kid0(next);     // must be $r2

  Is_Param_Valid(next && (WN_operator(next) == OPR_STID), 
                  (intrncall, "intrnsic should be saved"));


  Is_Param_Valid((WN_operator(retval) == OPR_LDID) && (ST_class(WN_st(retval)) == CLASS_PREG) && 
          (WN_offset(retval) == First_Int_Preg_Return_Offset),
          (intrncall, "intrnsic should be saved"));

  //    ldid r2 
  // stid tmp_eq   <- temp equivalence  variable
  INT32 tmp_idx =  Get_VarWN_idx(next);

  if (strcmp(ST_name(WN_st(next)) , "__comma") != 0) {
    next = WN_next(next);
  }
  
  while (next) {
    if (WN_operator(next) == OPR_STID  && 
        (WN_operator(WN_kid0(next))==OPR_LDID) && 
	(tmp_idx == Get_VarWN_idx(WN_kid0(next)))) {
	  PREG_NUM tmppn= Get_VarWN_idx(next);
	  if (tmppn > 11 || tmppn < 4) // skip argument register tn
           break;
    }		
    if (WN_operator(next) == OPR_ISTORE && 
        (WN_operator(WN_kid0(next)) == OPR_LDID)  && 
        (tmp_idx == Get_VarWN_idx(WN_kid0(next)))) {
      return Get_ResultEqVarIdx(next);
    }		  
    next = WN_next(next);
  }
  
  if (!next) {
    return tmp_idx;
  }
  
  return  Get_VarWN_idx(next);
 
}

/*Get  a valid acctn, if status array Accreg[i]=0, "Acc[i]_TN" is the valid acc register*/
static TN *Get_New_AccTN (WN *intrncall) {
  TN *acctn=NULL;
  int i;
  for(i = prev_alloc_AccIndex; i < (prev_alloc_AccIndex + CG_Max_Accreg); i++) {
    if (ACCreg[i%CG_Max_Accreg] == 0) {
      switch (i%CG_Max_Accreg) {
        case 0 : acctn = Acc0_TN ; ACCreg[0]=1; break;
        case 1 : acctn = Acc1_TN ; ACCreg[1]=1; break;
        case 2 : acctn = Acc2_TN ; ACCreg[2]=1; break;
        case 3 : acctn = Acc3_TN;  ACCreg[3]=1; break;
        default: Is_True(0,("illegal acc register tn"));
      }
    }
    if (acctn) break;
   }

   if (i == (prev_alloc_AccIndex + CG_Max_Accreg)) {
     Is_Param_Valid(FALSE, (intrncall, "max acc register numbers are %d\n", CG_Max_Accreg));
     Is_True(FALSE, ("No valid new acc register, max acc register numbers is %d\n", CG_Max_Accreg));
   }
   if (CG_round_spreg)
     prev_alloc_AccIndex = (i+1)%CG_Max_Accreg;
   return acctn;
}

static TN *Create_Var2Acc_Map(WN *intrncall, INT32 varidx) {
  INT i;	
  TN *acctn = NULL;
  if (var2acc.size() > 0)
    acctn = var2acc[varidx];
	
  Is_Param_Valid(!acctn, (intrncall, "variable has acquired an acc register and did't free it"));	
  acctn = Get_New_AccTN(intrncall);	
  var2acc[varidx] = acctn;  

  if (dotrace) {
    fprintf(TFile, "Create_Var2Acc_Map::");
    Print_Acc(acctn);
  }
  return acctn;	
}

/*dual acc tn must be even */
static TN *Get_New_DualAccTN (WN *intrncall) {
  TN *acctn=NULL;
  int i;
  if (prev_alloc_AccIndex %2 !=0 ) {
    prev_alloc_AccIndex = (prev_alloc_AccIndex+1)%CG_Max_Accreg;
  }
  for(i = prev_alloc_AccIndex; i < (prev_alloc_AccIndex + CG_Max_Accreg); i=i+2) {
    if ((ACCreg[i%CG_Max_Accreg] == 0)  && (ACCreg[(i+1)%CG_Max_Accreg] == 0)) {
      switch (i%CG_Max_Accreg) {
        case 0 : acctn = Acc0_TN ; ACCreg[0]=1; ACCreg[1]=1;break;
        case 2 : acctn = Acc2_TN ; ACCreg[2]=1; ACCreg[3]=1; break;
        default: Is_True(0,("Get_New_DualAccTN::illegal acc register tn"));
      }
    }
    if (acctn) break;
  }

  if (!acctn || (CG_Max_Accreg < 2) || (CG_Max_Accreg == 3 && ACCreg[3]==1))
  {
    Is_Param_Valid(FALSE, (intrncall, "max acc register numbers are %d\n", CG_Max_Accreg));
    Is_True(FALSE, ("NO valid dual acc registers, max acc register numbers are %d\n", CG_Max_Accreg));
  }
  
  if (CG_round_spreg)
    prev_alloc_AccIndex = (i+2)%CG_Max_Accreg;
 
  return acctn;
}

static TN *Create_Var2DualAcc_Map(WN* intrncall, INT32 varidx) {
  INT i;	
  TN *acctn = NULL;
  if(var2acc.size() > 0)
    acctn = var2acc[varidx];

  Is_Param_Valid(!acctn, (intrncall, "variable has acquired an acc register and didn't free it"));	

  acctn = Get_New_DualAccTN(intrncall);
  var2acc[varidx] = acctn; 

  if (dotrace) {
    fprintf(TFile, "Create_Var2DAcc_Map::");
    Print_Acc(acctn);
  }
  return acctn;	
}


static TN *Get_New_AddrTN (WN *intrncall) {
  TN *addtn=NULL;
  int i;
  for(i = prev_alloc_AddrIndex; i < (prev_alloc_AddrIndex + CG_Max_Addreg); i++) {
    if (Addreg[i%CG_Max_Addreg] == 0) {
      switch (i%CG_Max_Addreg) {
        case 0 : addtn = Addr0_TN ; Addreg[0]=1; break;
        case 1 : addtn = Addr1_TN ; Addreg[1]=1; break;
        case 2 : addtn = Addr2_TN ; Addreg[2]=1; break;
        case 3 : addtn = Addr3_TN;  Addreg[3]=1; break;
        case 4 : addtn = Addr4_TN;  Addreg[4]=1; break;
        case 5 : addtn = Addr5_TN;  Addreg[5]=1; break;
        case 6 : addtn = Addr6_TN;  Addreg[6]=1; break;
        case 7 : addtn = Addr7_TN;  Addreg[7]=1; break;
        default: Is_True(0,("illegal address register tn"));
      }
    }
    if (addtn) break;
  }
  
  if (i == (prev_alloc_AddrIndex + CG_Max_Addreg)) {
    Is_Param_Valid(FALSE, (intrncall, "max address register numbers is %d\n", CG_Max_Addreg));
    Is_True(FALSE, ("No valid new address register, max address register numbers is %d\n", CG_Max_Addreg));
  }
  
  if (CG_round_spreg)
    prev_alloc_AddrIndex = (i+1)%CG_Max_Addreg;
  return addtn;
}

static TN * Get_AddrSize_Reg(TN *addrtn) {
  TN *addr_size = NULL;

  if (addrtn == Addr0_TN) 
    addr_size = Addrsize0_TN;
  else if (addrtn == Addr1_TN)
    addr_size = Addrsize1_TN;
  else if (addrtn == Addr2_TN)
    addr_size = Addrsize2_TN;
  else if (addrtn == Addr3_TN)
    addr_size = Addrsize3_TN;
  else if (addrtn == Addr4_TN)
    addr_size = Addrsize4_TN;
  else if (addrtn == Addr5_TN)
    addr_size = Addrsize5_TN;
  else if (addrtn == Addr6_TN)
    addr_size = Addrsize6_TN;
  else if (addrtn == Addr7_TN)
    addr_size = Addrsize7_TN;
  return addr_size;
}

static TN *Create_Var2Add_Map(WN *intrncall, INT32 varidx) {
  INT i;	
  TN *addrtn = NULL;
  if(var2addr.size() > 0)
    addrtn = var2addr[varidx];

  Is_Param_Valid(!addrtn, (intrncall, "addrtn should be null"));	
  addrtn = Get_New_AddrTN(intrncall);
  var2addr[varidx] = addrtn;
  if(dotrace) {
    fprintf(TFile, "Create_Var2Add_Map::");
    Print_Addr(addrtn);
  }
  return addrtn;	
}


static inline void Set_Var_AccTN ( INT32 varidx, TN *acctn) {
  var2acc[varidx] = acctn;
  if (dotrace) {
    Print_Acc(acctn);
  }	
}

static inline void Set_Var_AddrTN ( INT32 varidx, TN *addtn) {
  var2addr[varidx] = addtn;
  if (dotrace) {
    Print_Addr(addtn);
  }
}


static TN *Get_Acc_from_Varidx(INT32 varidx) {
  if ((var2acc.size() > 0) && var2acc[varidx]) {
    return var2acc[varidx];
  } else
    return NULL;
}

static TN *Get_Addr_from_Varidx(INT32 varidx) {
  if ((var2addr.size() > 0) && var2addr[varidx]) {
    return var2addr[varidx];
  } 
  else
    return NULL;
}

static void Erase_AccTN (TN *acctn) {
  if (acctn == Acc0_TN) 
    ACCreg[0] = 0;
  else if (acctn == Acc1_TN) 
    ACCreg[1] = 0;
  else if (acctn == Acc2_TN)
    ACCreg[2] = 0;
  else if (acctn == Acc3_TN)
    ACCreg[3] = 0;
  else 
    Is_True(0, ("tn is not a valie acc register tn"));
  
  return;
}


/* erase an acc register dedicate tn*/
static void Erase_Var2Acc_Map(INT32 varidx) {
  TN *acctn = Get_Acc_from_Varidx(varidx);
  Is_True(acctn, ("acctn are not used or has been freed before "));
  
  if(dotrace) {
    fprintf(TFile, "Erase_Var2Acc_Map:: ");
    Print_Acc(acctn);
  }
  Erase_AccTN(acctn);
  // erase var2acc related with acctn
  typedef std::map<INT32, TN*> Var2Acc;
  Var2Acc::iterator it = var2acc.begin(); 
  Var2Acc::iterator tempit;
  for(;it != var2acc.end();) 
  { 
    tempit = it;
    it++;
    if ((*tempit).second == acctn) {
      var2acc.erase((*tempit).first);
    }
  }   
  return;
}

/*
 * return 2nd acc tn of dual opertion
 */
static TN *Get_DualAccTN (TN *acc_tn) {
   TN *dacc_tn ;
   if (acc_tn == Acc0_TN) 
     dacc_tn = Acc1_TN;
   else if (acc_tn == Acc2_TN)
     dacc_tn = Acc3_TN;
   else if (acc_tn == Acc3_TN || acc_tn == Acc1_TN)
     Is_True(0, ("first dual acc register should be even index"));
   else {
     Is_True(0, ("dual acc register is not valid"));
   }
   return dacc_tn;	
}

/* erase a acc register dedicate tn*/
static void Erase_Var2DualAcc_Map(WN *intrncall, INT32 varidx) {
  TN *acctn = Get_Acc_from_Varidx(varidx);
  Is_Param_Valid(acctn, (intrncall, "dual acctn are not used or have been free before"));
  TN *dacctn = Get_DualAccTN(acctn);
 
  if(dotrace) {
    fprintf(TFile, "Erase_Var2DualAcc_Map ");
    Print_Acc(acctn);
    fprintf(TFile, "Erase_Var2DualAcc_Map ");
    Print_Acc(dacctn);
  }
  Erase_AccTN(acctn);
  Erase_AccTN(dacctn);
  // erase var2acc related with acctn
  typedef std::map<INT32, TN*> Var2Acc;
  Var2Acc::iterator it = var2acc.begin(); 
  Var2Acc::iterator tempit;
  for(;it != var2acc.end();) 
  { 
    tempit = it;
    it++;
    if (((*tempit).second == acctn) || ((*tempit).second == dacctn)) {
      var2acc.erase((*tempit).first);
    }
  }   
  return;
}


static void Erase_AddrTN (TN *addrtn) {

  if (addrtn == Addr0_TN) 
    Addreg[0] = 0;
  else if (addrtn == Addr1_TN) 
    Addreg[1] = 0;
  else if (addrtn == Addr2_TN)
    Addreg[2] = 0;
  else if (addrtn == Addr3_TN)
    Addreg[3] = 0;
  else if (addrtn == Addr4_TN)
    Addreg[4] = 0;
  else if (addrtn == Addr5_TN)
    Addreg[5] = 0;
  else if (addrtn == Addr6_TN)
    Addreg[6] = 0;
  else if (addrtn == Addr7_TN)
    Addreg[7] = 0;
  else 
    Is_True(0, ("addrtn is not a valid address register tn"));
  return;
}

/* erase a address register dedicate tn*/
static void Erase_Var2Addr_Map(INT32 varidx) {
  TN *addtn = Get_Addr_from_Varidx(varidx);
  Is_True(addtn, ("address register is not used or has been free before"));
  
  if (dotrace) {
    fprintf(TFile, "Erase_Var2Addr_Map:: ");
    Print_Addr(addtn);
  }
  Erase_AddrTN(addtn);	

  // erase var2acc related with acctn
  typedef std::map<INT32, TN*> Var2Addr;
  Var2Addr::iterator it = var2addr.begin(); 
  Var2Addr::iterator tempit;
  for(;it != var2addr.end();) 
  {
    tempit = it;
    it++;
    if ((*tempit).second == addtn && (*tempit).first == varidx) {
      var2addr.erase((*tempit).first);
    }
  }   
  return;
}

/*
 *  result = SL1_acquire_acc (rs1);         
 *  result : acquire a new acc register for result
 *  rs1 : init the value of acc
 */

TN *Expand_C3_INIT_ACC (WN *intrncall, TN *result, OPS *ops) {
   
   Is_Param_Valid(WN_kid0(intrncall), (intrncall, "1st argument is null"));
   TN *kid0_tn = Expand_Expr (WN_kid0(intrncall), intrncall, NULL);  // RS1
   TN *shl_tn =  Get_Liternal_TN(intrncall, 1, 4);  // shift left
   Is_IMM_Valid(intrncall, 1, shl_tn, UIMM5);
   INT32 pregn = Get_Resultidx_Intrncall(intrncall);
   TN *acc_tn = Create_Var2Acc_Map(intrncall, pregn);
   Build_OP(TOP_c3_mvtacc, acc_tn, kid0_tn, shl_tn, ops);

   return kid0_tn;	
}

/*
 * rd = SL1_free_acc(a)
 * a: variable acquired an acc register 
 * result: copy value of acc register
 * operand0 map acc register tn
 */
TN *Expand_C3_SAVE_ACC (WN *intrncall, TN *result, OPS *ops) {
   
   Is_Param_Valid(WN_kid0(intrncall) && WN_kid1(intrncall), (intrncall, "arguments are incomplete"));
   TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
   TN *shr_tn = Get_Liternal_TN(intrncall, 1, 4);
   Is_IMM_Valid(intrncall, 1, shr_tn, UIMM5);
   INT32 varidx = Get_ParmVaridx_Intrncall(intrncall);
   TN *acctn = Get_Acc_from_Varidx(varidx);
   Is_Param_Valid(acctn, (intrncall, "variable hasn't acquired acc register")); 
   
   if (!result) {
     result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
   }
   Build_OP(TOP_c3_mvfacc, result, acctn, shr_tn, ops);

   Erase_Var2Acc_Map(varidx);

   return result;	
	
}

TN *Expand_C3_MVFS(WN *intrncall, TN *result, OPS *ops)  {
  	
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *kid0_tn = Expand_Expr (WN_kid0(intrncall), intrncall, NULL);  // acc tn
  TN *shiftnum = Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, shiftnum, UIMM5);
  TN *accid = Get_Liternal_TN(intrncall, 2, 4);  
  INT32 varidx  = Get_ParmVaridx_Intrncall(intrncall);
  TN *acctn = Get_Acc_from_Varidx(varidx);
  Is_Param_Valid(acctn, (intrncall, "1st argument has not mapped to acc register"));
  Is_Param_Valid(TN_has_value(accid), (intrncall, "3rd argument must be immediate integer"));
  if (TN_value(accid) == 1) {
    Is_Param_Valid(Get_DualAccTN(acctn), (intrncall, "1st argument must be mapped to dual acc register")); 
    acctn = Get_DualAccTN(acctn); 
  } 
  Build_OP(TOP_c3_mvfacc, result, acctn, shiftnum, ops);
  return result;
}

TN *Expand_C3_INIT_DACC (WN *intrncall, TN *result, OPS *ops) {
    
   Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) && WN_kid3(intrncall)),
                  (intrncall, "arguments are incomplete"));
   TN *kid0_tn = Expand_Expr (WN_kid0(intrncall), intrncall, NULL);  // RS1
   TN *shl_tn =  Get_Liternal_TN(intrncall, 1, 4);  // shift left
   Is_IMM_Valid(intrncall, 1, shl_tn, UIMM5);
   TN *kid1_tn = Expand_Expr (WN_kid2(intrncall), intrncall, NULL);  // RS2
   TN *shl1_tn =  Get_Liternal_TN(intrncall, 3, 4);  // shift left 2
   Is_IMM_Valid(intrncall, 3, shl1_tn, UIMM5);
   INT32 varidx = Get_Resultidx_Intrncall(intrncall);
   TN *acc_tn = Create_Var2DualAcc_Map(intrncall, varidx);
   TN *dacc_tn =  Get_DualAccTN(acc_tn); 
  
   Build_OP(TOP_c3_mvtacc, acc_tn, kid0_tn, shl_tn, ops);
   Build_OP(TOP_c3_mvtacc, dacc_tn, kid1_tn, shl1_tn, ops);
   
   return kid0_tn;	
}

TN *Expand_C3_SAVE_DACC (WN *intrncall, TN *result, OPS *ops) {
   
   Is_Param_Valid(WN_kid0(intrncall) && WN_kid1(intrncall), (intrncall, "arguments are incomplete"));
   TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
   //opr_tn == 0:copy first acc to result; opr_tn ==1: add dual acc registers with saturation and copy to result 
   TN *opr_tn = Get_Liternal_TN(intrncall, 1, 4); 
   INT32 varidx = Get_ParmVaridx_Intrncall(intrncall);
   TN *acc_tn = Get_Acc_from_Varidx(varidx);
   Is_Param_Valid(acc_tn, (intrncall, "variable has not acquired acc register")); 
   TN *dacc_tn =  Get_DualAccTN(acc_tn); 
   TN *tmp1 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
   TN *tmp2 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
   TN *tmp0 = Gen_Literal_TN(0,4);
   
   Is_Param_Valid(TN_has_value(opr_tn), (intrncall, "2nd arguments must be immediate")); 
   int oprvalue= TN_value(opr_tn);

   if (!result) {
     result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
   }
   if (oprvalue == 1) {    
     Build_OP(TOP_c3_mvfacc, tmp1, acc_tn, tmp0, ops);
     Build_OP(TOP_c3_mvfacc, tmp2, dacc_tn, tmp0, ops);
     Build_OP(TOP_c3_saadds, result, tmp1, tmp2, tmp0, ops);
   } else if (oprvalue == 0) {
     // TODO: this version only copy first acc value to result
     // how to deal withe second acc register?
     Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
   } else {
     Is_Param_Valid(0, (intrncall, "2nd argument is unknown immediate"));
   }
   //enable acc_tn
   Erase_Var2DualAcc_Map(intrncall, varidx);

   return result;	
	
}

/*
 *  result = intrinsic_c3_init_acc (rs1);         
 *  result : map to acc
 *  rs1 : init the value of acc
 */

TN *Expand_INIT_ADDR (WN *intrncall, TN *result, OPS *ops) {

  Is_Param_Valid(WN_kid0(intrncall) && WN_kid1(intrncall), (intrncall, "arguments are incomplete"));
  TN *kid0_tn = Expand_Expr (WN_kid0(intrncall), intrncall, NULL);  // RS1
  TN *address_mode = Get_Liternal_TN(intrncall, 1, 4);    // add_mode
  Is_IMM_Valid(intrncall, 1, address_mode, UIMM5);

  INT32 varidx = Get_Resultidx_Intrncall(intrncall);
  TN *addr_tn = Create_Var2Add_Map(intrncall, varidx);
  TN *tmp_tn = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  Build_OP(TOP_addiu, tmp_tn, kid0_tn, address_mode, ops);
  Build_OP(TOP_c3_mvtaddr, addr_tn, tmp_tn, Gen_Literal_TN(0,4), ops);

  return kid0_tn;	
}

TN *Expand_SAVE_ADDR (WN *intrncall, TN *result, OPS *ops) {
   
  Is_Param_Valid(WN_kid0(intrncall), (intrncall, "arguments are incomplete"));
  TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  INT32 varidx = Get_ParmVaridx_Intrncall(intrncall);
  TN *addrtn = Get_Addr_from_Varidx(varidx);
  Is_Param_Valid(addrtn, (intrncall, "variable has not arquired address register")); 
  if (!result) {
    result = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
  }
  Build_OP(TOP_c3_mvfaddr, result, addrtn, Gen_Literal_TN(0,4), ops);
   //enable acc_tn
  Erase_Var2Addr_Map(varidx);

  return result;	
}

TN *Expand_C3_PTR (WN *intrncall, TN *result, OPS *ops) {
      
  INT32 pregn1  = Get_ParmVaridx_Intrncall(intrncall, 1);
  TN *add1tn = Get_Addr_from_Varidx(pregn1);
  if (add1tn) {
    Set_Var_AddrTN(AddPregN, add1tn);
    if (dotrace) {	
      fprintf(TFile, "Expand_C3_PTR :: ");	 
      Print_Addr(add1tn);	 
    }
  }  
  else {
    TN *rs1 = Expand_Expr(WN_kid1(intrncall), intrncall, NULL); 
    Exp_COPY(result, rs1, ops);
  }	  
  return result;	  
}

TN *Expand_SET_ADDRSIZE (WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)), (intrncall, "arguments are incomplete"));

  TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *step_tn = Expand_Expr(WN_kid0(WN_kid1(intrncall)), WN_kid1(intrncall), NULL); //step

  INT32 varidx = Get_ParmVaridx_Intrncall(intrncall);
  TN *addrtn = Get_Addr_from_Varidx(varidx);
  Is_Param_Valid(addrtn, (intrncall, "1st argument has not arquired address register"));
  Is_Param_Valid(TN_value(step_tn), (intrncall, "2nd argument should be larger than zero"));

  TN *tmp_tn = Gen_Register_TN (ISA_REGISTER_CLASS_integer, 4);
  TN *addrs_tn = Get_AddrSize_Reg(addrtn);
  Expand_Add(tmp_tn, Zero_TN, step_tn, MTYPE_I4, ops);
  Build_OP(TOP_c3_mvtadds, addrs_tn, addrtn, tmp_tn,  Gen_Literal_TN(0,4), addrtn, ops);

  return result;
}

TN *Expand_Set_CircBuf(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) && WN_kid(intrncall, 3) && WN_kid(intrncall, 4)),
          (intrncall, "arguments are incomplete"));
  //TN  *cbuf_b, *cbuf_e;
  TN *src_begin = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *src_end = Expand_Expr(WN_kid1(intrncall), intrncall, NULL);
  TN *circbuf_id = Get_Liternal_TN(intrncall, 2, 4);   // circular buffer id
  Is_IMM_Valid(intrncall, 2, circbuf_id, UIMM2);
  TN *cbuf_b = Get_Liternal_TN(intrncall, 3, 4); // begin address
  TN *cbuf_e = Get_Liternal_TN(intrncall, 4, 4);  // end address
  TN *value0 = Gen_Literal_TN(0, 4);
  Is_Param_Valid(TN_has_value(circbuf_id) && TN_has_value(cbuf_b) && TN_has_value(cbuf_e), 
    (intrncall, "3nd/4th/5th arguments must be immediate"));

  TN *begin_addr = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
  TN *end_addr = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  Expand_Add(begin_addr, Zero_TN, cbuf_b, MTYPE_I4, ops);
  Expand_Add(end_addr, Zero_TN, cbuf_e, MTYPE_I4, ops);

  Build_OP(TOP_sw, src_begin, begin_addr, value0, ops);
  Set_OP_volatile(OPS_last(ops));
  Build_OP(TOP_sw, src_end, end_addr, value0, ops);
  Set_OP_volatile(OPS_last(ops));
}


TN* Expand_Float64_Const(WN* intrncall, TN* result,  BOOL Is_high,  OPS *ops)
{
// check if the value has been stored into a preg.
// check if the parameter is a const. give a assertion if not. 
// following format is expected
//      F8CONST/F8LDID
//    F8PARM
//  I4INTRINSIC_OP
  WN* kid0 = WN_kid0(intrncall); 
  if(!kid0) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d: Parameter is NULL when handling CVT64",  SRCPOS_linenum(current_srcpos)); 
  }

  WN* const_val = WN_kid0(kid0);
  
#if defined (EMULATE_FLOAT_POINT)

  FmtAssert(WN_rtype(const_val) == MTYPE_F8, ("Expand_Float64_Const: Unexpected Type"));

  TN * const_tn = Expand_Expr(const_val, kid0, NULL);
  
  INTRINSIC id = WN_intrinsic(intrncall); 
  if (id == INTRN_CVT64_LOW)
  {
  	Exp_COPY(result, const_tn, ops); 
  }
  else
  {
    extern TN* Get_TN_Pair(TN* key);

	  TN * const_tn_high = Get_TN_Pair(const_tn);
	  FmtAssert(const_tn_high, ("Expand_Float64_Const: Get const Tn high failed"));

	  Exp_COPY(result, const_tn_high, ops); 
  }

#else

  if(WN_operator(const_val) != OPR_CONST) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d: constant expected when handling CVT64",  SRCPOS_linenum(current_srcpos)); 
  }
  ST* sym = WN_st(const_val); 
  TCON tcon = STC_val(sym);
  INTRINSIC id = WN_intrinsic(intrncall); 
  INT32 val  = (id == INTRN_CVT64_LOW) ?  TCON_v0(tcon) : TCON_v1(tcon);  
  if(val == 0) 
     Exp_COPY(result, Zero_TN, ops); 
  else {
     Build_OP (TOP_lui, result, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
     Build_OP(TOP_ori, result, result, Gen_Literal_TN(val & 0xffff, 4), ops);
  }
  
#endif
  
  return result; 

}


TN* Expand_LONGLONG_Const(WN* intrncall, TN* result,  BOOL Is_high,  OPS *ops)
{
// check if the value has been stored into a preg.
// check if the parameter is a const. give a assertion if not. 
// following format is expected
//      I8CONST/I8LDID
//    I8PARM
//  I4INTRINSIC_OP
  WN* kid0 = WN_kid0(intrncall); 
  if(!kid0) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d: Parameter is NULL when handling LONLONG CVT64",  SRCPOS_linenum(current_srcpos)); 
  }
  
  WN* const_val = WN_kid0(kid0); 

#if defined (EMULATE_LONGLONG)

  FmtAssert((WN_rtype(const_val) == MTYPE_I8) || (WN_rtype(const_val) == MTYPE_U8), 
  ("Expand_LONGLONG_Const: Unexpected Type"));

  TN * const_tn = Expand_Expr(const_val, kid0, NULL);
  
  INTRINSIC id = WN_intrinsic(intrncall); 
  if (id == INTRN_LONGLONG_CVT64_LOW)
  {
  	Exp_COPY(result, const_tn, ops); 
  }
  else
  {
    extern TN* Get_TN_Pair(TN* key);

	  TN * const_tn_high = Get_TN_Pair(const_tn);
	  FmtAssert(const_tn_high, ("Expand_LONGLONG_Const: Get const Tn high failed"));

	  Exp_COPY(result, const_tn_high, ops); 
  }
  
#else
  if(WN_operator(const_val) != OPR_INTCONST) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d: constant expected when handling CVT64",  SRCPOS_linenum(current_srcpos)); 
  }
  
  INTRINSIC id = WN_intrinsic(intrncall); 
  INT32 val  = (id == INTRN_LONGLONG_CVT64_LOW) ? (WN_const_val(const_val) & 0xffffffff) :( (WN_const_val(const_val) >> 32) & 0xffffffff);  
  if(val == 0) 
     Exp_COPY(result, Zero_TN, ops); 
  else {
     Build_OP (TOP_lui, result, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
     Build_OP(TOP_ori, result, result, Gen_Literal_TN(val & 0xffff, 4), ops);
  }
#endif

  return result; 
}


TN* Expand_Float32_Const(WN* intrncall, TN* result,  OPS *ops)
{
// check if the value has been stored into a preg.
// check if the parameter is a const. give a assertion if not. 
// following format is expected
//      F4CONST/F4LDID
//    F4PARM
//  I4INTRINSIC_OP
  WN* kid0 = WN_kid0(intrncall); 
  if(!kid0) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d:  Parameter is NULL when handling CVT64", SRCPOS_linenum(current_srcpos)); 
  }

  WN* const_val = WN_kid0(kid0); 
  
#if defined (EMULATE_FLOAT_POINT)

  FmtAssert(WN_rtype(const_val) == MTYPE_F4, ("Expand_Float32_Const: Unexpected Type"));
  TN * const_tn = Expand_Expr(const_val, kid0, NULL);
  Exp_COPY(result, const_tn, ops);

#else

  if(WN_operator(const_val) != OPR_CONST) {
     Print_Src_Line(current_srcpos, stderr); 
     Fail_FmtAssertion("line %d: constant expected when handling CVT32",  SRCPOS_linenum(current_srcpos)); 
  }

  ST* sym = WN_st(const_val); 
  TCON tcon = STC_val(sym);
  INTRINSIC id = WN_intrinsic(intrncall); 
  INT32 val  = TCON_v0(tcon);  
  if(val == 0) 
     Exp_COPY(result, Zero_TN, ops); 
  else {
     Build_OP (TOP_lui, result, Gen_Literal_TN((val >> 16)&0xffff, 4), ops);
     Build_OP(TOP_ori, result, result, Gen_Literal_TN(val & 0xffff, 4), ops);
  }
#endif

  return result; 
}

TN *Expand_C3_aadda(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)
                 && WN_kid(intrncall, 4)), (intrncall, "arguments are incomplete"));

  TN *accd = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);  // accd
  TN *acm = Get_Liternal_TN(intrncall, 1, 4);  // acm
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *accs = Expand_Expr(WN_kid2(intrncall), intrncall, NULL);  // accs
  TN *m =  Get_Liternal_TN(intrncall, 3, 4);
  Is_IMM_Valid(intrncall, 3, m, UIMM3);
 
  TN *paired = Get_Liternal_TN(intrncall, 4, 4);  // get paired dual acc
  UINT flag= 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *accd_tn = Get_Acc_from_Varidx(v1);
  INT32 v2  = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *accs_tn = Get_Acc_from_Varidx(v2);

  Is_Param_Valid((TN_has_value(paired) && (TN_value(paired) == 0 || TN_value(paired) == 1 )), 
                 (intrncall, "5th argument should be 0 or 1"));
  if (TN_value(paired) == 1){
    accs_tn = Get_DualAccTN(accd_tn);
    Is_Param_Valid(accs_tn, (intrncall, "first acc should acquire dual acc"));
  } else {
    v2 = Get_ParmVaridx_Intrncall(intrncall, 2);
    accs_tn = Get_Acc_from_Varidx(v2);
  }

  if (!accd_tn) {
    accd_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, accd_tn, accd, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, accd_tn);
  }
  if (!accs_tn) {
    accs_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, accs_tn, accs, tmp0, ops);
    flag |=2;
  }
  Build_OP(TOP_c3_aadda, accd_tn, acm, accs_tn, m, accd_tn, ops);
  
  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc,  result, accd_tn, tmp0, ops);
    Erase_AccTN(accd_tn);
  } 
  if (flag & 2) {
    Erase_AccTN(accs_tn);
  }
  return result;
}

TN *Expand_C3_bitr(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall)),
          (intrncall, "arguments are incomplete"));

  TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); //rs1
  TN *uimm5_tn =  Get_Liternal_TN(intrncall, 1, 4); // imm5
  Is_IMM_Valid(intrncall, 1, uimm5_tn, UIMM5);
  
  TN *mode_tn =  Get_Liternal_TN(intrncall, 2, 4);  // mode
  Is_IMM_Valid(intrncall, 2, mode_tn, UIMM4);

  Build_OP(TOP_c3_bitr, result, kid0_tn, uimm5_tn, mode_tn, ops);
  return result;
}

TN *Expand_C3_cs(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall)),
          (intrncall, "arguments are incomplete"));

  TN *rs0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // rs1
  TN *rs1_tn = Expand_Expr(WN_kid1(intrncall), intrncall, NULL); // rs2
  TN *mode_tn =  Get_Liternal_TN(intrncall, 2, 4);  // mode
  Is_IMM_Valid(intrncall, 2, mode_tn, UIMM2);

  Build_OP(TOP_c3_cs, result, HI_TN, rs0_tn, rs1_tn, mode_tn, HI_TN, ops);
  return result;
}

INT32 Get_Oper_Value (WN *intrncall, TN *oper, INT para_no) {
    INT32 value = -1;
    if (TN_has_value(oper)) {
      value = TN_value(oper);
      Is_Param_Valid((value == 1 || value == 0), (intrncall, "oper(arguments %d) should be 0 or 1", para_no));
    } else {
      Is_Param_Valid(0, (intrncall, "oper(arguments %d) should be constant", para_no));
    }
    return value;	
}

TN *Expand_C3_Mode0(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_True((const_parm_nums < 3 ), ("const nums < 3"));
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)), (intrncall, "arguments are incomplete"));
  TN *kid0_tn = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // rs1
  TN *kid1_tn = Expand_Expr(WN_kid1(intrncall), intrncall, NULL); // rs2
  TN *uimm5[3];
  TN *oper;
  INT32 value = -1;
  INT i=0;
  if (has_oper) {
    oper = Get_Liternal_TN(intrncall, 2+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 3+const_parm_nums);
  }
  while (const_parm_nums > 0) {
    uimm5[i] = Get_Liternal_TN(intrncall, 2+i, 4);
    Is_IMM_Valid(intrncall, 2+i, uimm5[i], UIMM5);
    const_parm_nums--;
    i++;
  }
  switch (top) {
    case TOP_c3_dadd: top = value ==1 ? TOP_c3_dsub : top; break;
    case TOP_c3_saadds: top = value == 1 ? TOP_c3_sasubs: top; break;
    case TOP_c3_saaddsh: top = value == 1 ? TOP_c3_sasubsh: top; break;
    case TOP_c3_samulsh:  break;
    default: Is_True(0, ("unknown TOP"));
  }
  
  switch (i) {
    case 0: Build_OP(top, result, kid0_tn, kid1_tn, ops); break;
    case 1: Build_OP(top, result, kid0_tn, kid1_tn, uimm5[0], ops); break;
    default:  Is_True(0, ("now only use 0 or 1 uimm5"));
  }
  return result;
}


// mode 1 : TOP acc, acm, rs1, rs2, mode, N, M , oper(add/sub)
TN *Expand_C3_Mode1(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops)  {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
                 (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4); //
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *rs1 =  Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *rs2 =  Expand_Expr(WN_kid3(intrncall), intrncall, NULL);
  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 4+i, 4);
    const_parm_nums--;
    i++;
  }
  TN *oper =  Get_Liternal_TN(intrncall, 4+i, 4);
  INT32 value = -1;
  value = Get_Oper_Value(intrncall, oper, 5 + i);

  switch (top) {
    case TOP_c3_mac:   
      Is_IMM_Valid(intrncall, 4, const_parm[0], UIMM1);
      top = value == 1 ? TOP_c3_macn : top; break;
    case TOP_c3_mula:  top = value == 1 ? TOP_c3_mulan : top; break;
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    if (top == TOP_c3_mac || top == TOP_c3_macn) 
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  }
  if ((top == TOP_c3_mula) || (top == TOP_c3_mulan)) {
    switch (i) {
      case 0: Build_OP(top, acc_tn, acm, rs1, rs2, ops); break;
      case 1: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], ops); break;
      case 2: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], const_parm[1], ops); break;
      case 3: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], const_parm[1], const_parm[2], ops); break;
      default:  Is_True(0, ("const num should <=3 "));
    }
  } else {
    switch (i) {
      case 0: Build_OP(top, acc_tn, acm, rs1, rs2, acc_tn, ops); break;
      case 1: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], acc_tn, ops); break;
      case 2: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], const_parm[1], acc_tn, ops); break;
      case 3: Build_OP(top, acc_tn, acm, rs1, rs2, const_parm[0], const_parm[1], const_parm[2], acc_tn, ops); break;
      default:  Is_True(0, ("const num should <=3 "));
    }
  }
  if (flag == 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }

  return result;
}

// mode2: acc, dacc,  acm, rs1, rs2, mode, N, M , oper(add/sub), acc, dacc 
TN *Expand_C3_Mode2(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
                 (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4); //
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);      
  TN *rs1 =  Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *rs2 =  Expand_Expr(WN_kid3(intrncall), intrncall, NULL);
  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 4+i, 4);
    const_parm_nums--;
    i++;
  }
  TN *oper =  Get_Liternal_TN(intrncall, 4+i, 4);
  INT32 value = -1;
  
  value = Get_Oper_Value(intrncall, oper, 5 + i);  
  switch (top) {
    case TOP_c3_dmac: {
      Is_IMM_Valid(intrncall, 4, const_parm[0], UIMM2);
      Is_IMM_Valid(intrncall, 5, const_parm[1], UIMM1);
      Is_IMM_Valid(intrncall, 6, const_parm[2], UIMM1);
      top = value == 1 ? TOP_c3_dmacn : top; break;
    }
    case TOP_c3_dmula: 
      Is_IMM_Valid(intrncall, 4, const_parm[0], UIMM2);
      Is_IMM_Valid(intrncall, 5, const_parm[1], UIMM1);
      top = value == 1 ? TOP_c3_dmulan : top; break;
    default: Is_True(0, ("unkown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  TN *acc1_tn;
  if (!acc_tn) {
    acc_tn = Get_New_DualAccTN(intrncall);
    acc1_tn =  Get_DualAccTN(acc_tn);
    if (top == TOP_c3_dmac || top == TOP_c3_dmacn) {
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
      Build_OP(TOP_c3_mvtacc, acc1_tn, Zero_TN, tmp0, ops);
    }
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
    acc1_tn =  Get_DualAccTN(acc_tn);
  }
  switch (i) {
    case 0: Build_OP(top, acc_tn, acc1_tn, acm, rs1, rs2, acc_tn, acc1_tn, ops); break;
    case 1: Build_OP(top, acc_tn, acc1_tn, acm, rs1, rs2, const_parm[0], acc_tn, acc1_tn, ops); break;
    case 2: Build_OP(top, acc_tn, acc1_tn, acm, rs1, rs2, const_parm[0], const_parm[1], acc_tn, acc1_tn, ops); break;
    case 3: Build_OP(top, acc_tn, acc1_tn, acm, rs1, rs2, const_parm[0], const_parm[1], const_parm[2], acc_tn, acc1_tn, ops); break;
    default:  Is_True(0, ("const num should <=3 "));
  }

  if (flag == 1) {
    TN *tmp1 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp2 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    Build_OP(TOP_c3_mvfacc,  tmp1, acc_tn, tmp0, ops);
    Build_OP(TOP_c3_mvfacc,  tmp2, acc1_tn, tmp0, ops);
    Build_OP(TOP_c3_saadds,  result, tmp1, tmp2, tmp0, ops);
    Erase_AccTN(acc_tn);
    Erase_AccTN(acc1_tn);
  }

}

// mode 3: acc, acm, as1, am1, as2, am2, M,
TN *Expand_C3_Mode3(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)
                 && WN_kid(intrncall, 4) && WN_kid(intrncall, 5)), (intrncall, "arguments are incomplete"));

  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4); 
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *as1 =  Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 3, 4); 
  Is_IMM_Valid(intrncall, 3, am1, UIMM3);
  TN *as2 =  Expand_Expr(WN_kid(intrncall, 4), intrncall, NULL);
  TN *am2 = Get_Liternal_TN(intrncall, 5, 4);
  Is_IMM_Valid(intrncall, 5, am2, UIMM3);

  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1; 
  
  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 6+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 7 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 6+i, 4);
    const_parm_nums--;
    i++;
  }
 
  switch (top) {
    case TOP_c3_mac_a:  
      Is_IMM_Valid(intrncall, 6, const_parm[0], UIMM1);
      top = value == 1 ? TOP_c3_macn_a : top; 
      break;
    case TOP_c3_mula_a:  
      break;
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *add1_tn = Get_Addr_from_Varidx(v2);
  INT32 v3 = Get_ParmVaridx_Intrncall(intrncall, 4); 
  TN *add2_tn = Get_Addr_from_Varidx(v3);

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    if (top != TOP_c3_mula_a)
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  }
  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 2;
  }  
  if (!add2_tn) {
    add2_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add2_tn, as2, tmp0, ops);
    flag |= 4;
  }

  if (top == TOP_c3_mula_a) {
    switch (i) {
      case 0: Build_OP(top, acc_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, ops); break;
      case 1: Build_OP(top, acc_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], ops); break;
      default:  Is_True(0, ("const num should <=1 "));
    }
  } else {
    switch (i) {
      case 0: Build_OP(top, acc_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, acc_tn, ops); break;
      case 1: Build_OP(top, acc_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], acc_tn, ops); break;
      default:  Is_True(0, ("const num should <=1 "));
    }
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall); 

  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }
  if (flag & 2) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  if (flag & 4) {
    Build_OP(TOP_c3_mvfaddr, as2, add2_tn, tmp0, ops); 
    Erase_AddrTN(add2_tn);
  }
  return result;

}

// Mode 4: mac.ar: acc, acm, rs1, as2, am2, M
TN *Expand_C3_Mode4(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)
                 && WN_kid(intrncall, 4)), (intrncall, "arguments are incomplete"));

  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *rs1 = Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *as1 =  Expand_Expr(WN_kid(intrncall, 3), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 4, 4);
  Is_IMM_Valid(intrncall, 4, am1, UIMM3);

  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 5 + const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 6 + const_parm_nums);
  } 
  
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 5+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_mac_ar:  
      Is_IMM_Valid(intrncall, 5, const_parm[0], UIMM1);
      top = value == 1 ? TOP_c3_macn_ar : top; break;
    case TOP_c3_mula_ar:  
      break;
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 3);
  TN *add1_tn = Get_Addr_from_Varidx(v2);

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    if (top != TOP_c3_mula_ar)
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  }
  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 2;
  }

  switch (i) {
    case 0: 
    { 
      if (top == TOP_c3_mula_ar) 
        Build_OP(top, acc_tn, add1_tn, acm, rs1, add1_tn, am1, ops); 
      else
        Build_OP(top, acc_tn, add1_tn, acm, rs1, add1_tn, am1, acc_tn, ops);
      break;
    }
    case 1: 
    {
      if (top == TOP_c3_mula_ar)
         Build_OP(top, acc_tn, add1_tn, acm, rs1, add1_tn, am1, const_parm[0], ops); 
      else 
         Build_OP(top, acc_tn, add1_tn, acm, rs1, add1_tn, am1, const_parm[0], acc_tn, ops); 
      break;
    }
    default:  Is_True(0, ("const num should <=1 "));
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);

  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }
  if (flag & 2) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  return result;

}

// mode 5: acc, acm, rs1, imm9, M,
TN *Expand_C3_Mode5(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) &&  WN_kid2(intrncall) ), (intrncall, "arguments are incomplete"));

  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *rs1 =  Expand_Expr(WN_kid2(intrncall), intrncall, NULL);

  TN *const_parm[4];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 3+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 4 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 3+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_mac_i:  {
      Is_IMM_Valid(intrncall, 3, const_parm[0], IMM9);
      Is_IMM_Valid(intrncall, 4, const_parm[1], UIMM1);
      top = value == 1 ? TOP_c3_macn_i : top; break;
    }
    case TOP_c3_shlata_i: {
      Is_IMM_Valid(intrncall, 3, const_parm[0], UIMM5);
      Is_IMM_Valid(intrncall, 4, const_parm[1], UIMM1);
      top = value == 1 ? TOP_c3_shrata_i : top; break;
    }
    case TOP_c3_mula_i: {
      Is_IMM_Valid(intrncall, 3, const_parm[0], IMM9);
      break;
    }
    case TOP_c3_sadda:  {
      Is_IMM_Valid(intrncall, 3, const_parm[0], UIMM4);
      Is_IMM_Valid(intrncall, 4, const_parm[1], UIMM2);
      Is_IMM_Valid(intrncall, 5, const_parm[2], UIMM1);
      Is_IMM_Valid(intrncall, 6, const_parm[3], UIMM1);
      break;
    }
    case TOP_c3_shav: {
      Is_IMM_Valid(intrncall, 3, const_parm[0], UIMM1);
      break;
    } 
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    if (top != TOP_c3_mula_i) 
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  }

  if ((top == TOP_c3_mula_i) || (top == TOP_c3_shlata_i) || (top == TOP_c3_shrata_i)) {
   // acc_tn is not operand
    switch (i) {
      case 0: Build_OP(top, acc_tn, acm, rs1, ops); break;
      case 1: Build_OP(top, acc_tn, acm, rs1, const_parm[0], ops); break;
      case 2: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], ops); break;
      case 3: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], const_parm[2], ops); break;
      case 4: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], const_parm[2], const_parm[3], ops); break;
      default:  Is_True(0, ("const num should <=4 "));
    }
  } else { 
    // same res
    switch (i) {
      case 0: Build_OP(top, acc_tn, acm, rs1, acc_tn, ops); break;
      case 1: Build_OP(top, acc_tn, acm, rs1, const_parm[0], acc_tn, ops); break;
      case 2: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], acc_tn, ops); break;
      case 3: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], const_parm[2], acc_tn, ops); break;
      case 4: Build_OP(top, acc_tn, acm, rs1, const_parm[0], const_parm[1], const_parm[2], const_parm[3], acc_tn, ops); break;
      default:  Is_True(0, ("const num should <=4 "));
    }
  }

  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }
  
  return result;
}

// Mode6: c3.round / c3.shla.i / c3.shra.i
TN *Expand_C3_Mode6(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)), (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 2+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 3 + const_parm_nums);
  }

  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 2+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_round: 
      Is_IMM_Valid(intrncall, 2, const_parm[0], UIMM2);
      Is_IMM_Valid(intrncall, 3, const_parm[1], UIMM1);
      break;
    case TOP_c3_shla_i: 
      Is_IMM_Valid(intrncall, 2, const_parm[0], UIMM5);
      Is_IMM_Valid(intrncall, 3, const_parm[1], UIMM1);
      top = value == 1 ? TOP_c3_shra_i : top ;  break;
    default: Is_True(0, ("unknown TOP"));
  }


  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  }

  switch (i) {
      case 0: Build_OP(top, acc_tn, acm, acc_tn, ops); break;
      case 1: Build_OP(top, acc_tn, acm, const_parm[0], acc_tn, ops); break;
      case 2: Build_OP(top, acc_tn, acm, const_parm[0], const_parm[1], acc_tn, ops); break;
      case 3: Build_OP(top, acc_tn, acm, const_parm[0], const_parm[1], const_parm[2], acc_tn, ops); break;
      default:  Is_True(0, ("const num should <=3 "));
  }
 
  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }

}


// mode 7: rd, as1, am1, as2, am2, N, M,
TN *Expand_C3_Mode7(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {

  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)), 
                 (intrncall, "arguments are incomplete"));
  
  TN *as1 =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, am1, UIMM3);
  TN *as2 =  Expand_Expr(WN_kid(intrncall, 2), intrncall, NULL);
  TN *am2 = Get_Liternal_TN(intrncall, 3, 4);
  Is_IMM_Valid(intrncall, 1, am1, UIMM3);

  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 4+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 5 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 4+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_saadd_a: top = value == 1 ? TOP_c3_sasub_a : top;  break;
    case TOP_c3_saaddh_a: top = value == 1 ? TOP_c3_sasubh_a : top;  break;
    case TOP_c3_samulh_a: break; 
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 0);
  TN *add1_tn = Get_Addr_from_Varidx(v2);
  INT32 v3 = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *add2_tn = Get_Addr_from_Varidx(v3);

  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 1;
  }
  if (!add2_tn) {
    add2_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add2_tn, as2, tmp0, ops);
    flag |= 2;
  }

  switch (i) {
    case 0: Build_OP(top, result, add1_tn, add2_tn, add1_tn, am1, add2_tn, am2, ops); break;
    case 1: Build_OP(top, result, add1_tn, add2_tn, add1_tn, am1, add2_tn, am2, const_parm[0], ops); break;
    case 2: Build_OP(top, result, add1_tn, add2_tn, add1_tn, am1, add2_tn, am2, const_parm[0], const_parm[1], ops); break;
    default:  Is_True(0, ("const num should <= 2 "));
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);

  if (flag & 1) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  if (flag & 2) {
    Build_OP(TOP_c3_mvfaddr, as2, add2_tn, tmp0, ops);
    Erase_AddrTN(add2_tn);
  }
  return result;

}

// mode 8: dacc, acm, as1, am1, as2, am2, M,
TN *Expand_C3_Mode8(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)
                 && WN_kid(intrncall, 4) && WN_kid(intrncall, 5)), (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *as1 =  Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 3, 4);
  Is_IMM_Valid(intrncall, 3, am1, UIMM3);
  TN *as2 =  Expand_Expr(WN_kid(intrncall, 4), intrncall, NULL);
  TN *am2 = Get_Liternal_TN(intrncall, 5, 4);
  Is_IMM_Valid(intrncall, 5, am2, UIMM3);

  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 6+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 7 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 6+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_dmac_a: 
      Is_IMM_Valid(intrncall, 6, const_parm[0], UIMM1);
      top = value == 1 ? TOP_c3_dmacn_a : top; 
      break;
    case TOP_c3_dmula_a: top = value == 1 ? TOP_c3_dmulan_a : top; break;
    default: Is_True(0, ("unknown TOP"));
  }

  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *add1_tn = Get_Addr_from_Varidx(v2);
  INT32 v3 = Get_ParmVaridx_Intrncall(intrncall, 4);
  TN *add2_tn = Get_Addr_from_Varidx(v3);
  TN *acc1_tn;

  if (!acc_tn) {
    acc_tn = Get_New_DualAccTN(intrncall);
    acc1_tn =  Get_DualAccTN(acc_tn);
    if (top == TOP_c3_dmac_a || top == TOP_c3_dmacn_a) {
      Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
      Build_OP(TOP_c3_mvtacc, acc1_tn, Zero_TN, tmp0, ops);
    }
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
    acc1_tn =  Get_DualAccTN(acc_tn);
  }
  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 2;
  }
  if (!add2_tn) {
    add2_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add2_tn, as2, tmp0, ops);
    flag |= 4;
  }

  if (top == TOP_c3_dmula_a || top == TOP_c3_dmulan_a) {
    switch (i) {
      case 0:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, ops);  break;
      case 1:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], ops);  break;
      case 2:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], const_parm[1], ops);  break;
      default:  Is_True(0, ("const num should <=2 "));
    }
  } else {
    switch (i) {
     case 0:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, acc_tn, acc1_tn, ops);  break;
     case 1:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], acc_tn, acc1_tn, ops);  break;
     case 2:  Build_OP(top, acc_tn, acc1_tn, add1_tn, add2_tn, acm, add1_tn, am1, add2_tn, am2, const_parm[0], const_parm[1], acc_tn, acc1_tn, ops);  break;
     default:  Is_True(0, ("const num should <=2 "));
   }
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);

  if (flag & 1) {
    TN *tmp1 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    TN *tmp2 = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
    Build_OP(TOP_c3_mvfacc,  tmp1, acc_tn, tmp0, ops);
    Build_OP(TOP_c3_mvfacc,  tmp2, acc1_tn, tmp0, ops);
    Build_OP(TOP_c3_saadds,  result, tmp1, tmp2, tmp0, ops);
    Erase_AccTN(acc_tn);
    Erase_AccTN(acc1_tn);
  }
  if (flag & 2) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  if (flag & 4) {
    Build_OP(TOP_c3_mvfaddr, as2, add2_tn, tmp0, ops);
    Erase_AddrTN(add2_tn);
  }
  return result;

}

// mode9 : rs1, imm, imm
TN *Expand_C3_Mode9(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall)), (intrncall, "arguments are incomplete"));

  TN *rs1 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 1+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 2 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 1+i, 4);
    const_parm_nums--;
    i++;
  }

  switch (top) {
    case TOP_c3_dshll_i: {
      Is_IMM_Valid(intrncall, 1, const_parm[0], UIMM5);
      Is_IMM_Valid(intrncall, 2, const_parm[1], UIMM5);
      top = value == 1 ? TOP_c3_dshrl_i : top; break;
    }
    case TOP_c3_revb: {
      Is_IMM_Valid(intrncall, 1, const_parm[0], UIMM5);
      break;
    }
    default: Is_True(0, ("unknown TOP"));
  }
 
  switch (i) {
    case 0: Build_OP(top, result, rs1, ops);
    case 1: Build_OP(top, result, rs1, const_parm[0], ops); break;
    case 2: Build_OP(top, result, rs1, const_parm[0], const_parm[1], ops); break;
    default: Is_True(0, ("const parm number < 3"));
  }

  return result;
}

// mode 10: rd as1, am1, dtyp (c3.ld/c3.fftld)
TN *Expand_C3_Mode10(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *as1  =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *am1  = Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, am1, UIMM3);
  TN *dtyp = Get_Liternal_TN(intrncall, 2, 4);
  Is_IMM_Valid(intrncall, 2, dtyp, UIMM3);

  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 3+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 4 + const_parm_nums);
  }
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 3+i, 4);
    const_parm_nums--;
    i++;
  }

  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 0);
  TN *add1_tn = Get_Addr_from_Varidx(v2);

  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 1;
  }
  
  switch (top) {
    case TOP_c3_ld: top = value == 1 ? TOP_c3_fftld : top; break;
    default: Is_True(0, ("unknown TOP"));
  }

  switch (i) {
    case 0:  Build_OP(top, result, add1_tn, add1_tn, tmp0, am1, dtyp, ops);  break;
    case 1:  Build_OP(top, result, add1_tn, add1_tn, tmp0, am1, dtyp, const_parm[0], ops);  break;
    default:  Is_True(0, ("const num should <=1 "));
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);
  
  if (flag & 1) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  
  return result;
}

// mode 11: rd as1, am1, dtyp (c3.st/c3.fftst)
TN *Expand_C3_Mode11(TOP top, WN *intrncall, TN *result, UINT32 const_parm_nums, OPS *ops, BOOL has_oper = TRUE) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
          (intrncall, "arguments are incomplete"));
  
  TN *rs1 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *as1 = Expand_Expr(WN_kid1(intrncall), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 2, 4);
  Is_IMM_Valid(intrncall, 2, am1, UIMM3);
  TN *dtyp = Get_Liternal_TN(intrncall, 3, 4);
  Is_IMM_Valid(intrncall, 3, dtyp, UIMM3);


  TN *const_parm[3];
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 i = 0;
  TN *oper;
  INT32 value = -1;

  if (has_oper) {
    oper =  Get_Liternal_TN(intrncall, 4+const_parm_nums, 4);
    value = Get_Oper_Value(intrncall, oper, 5 + const_parm_nums);
  }
  
  while(const_parm_nums > 0) {
    const_parm[i] = Get_Liternal_TN(intrncall, 4+i, 4);
    const_parm_nums--;
    i++;
  }

  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 1);
  TN *add1_tn = Get_Addr_from_Varidx(v2);

  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1, tmp0, ops);
    flag |= 1;
  }

  switch (top) {
    case TOP_c3_st: top = value == 1 ? TOP_c3_fftst : top; break;
    default: Is_True(0, ("unknown TOP"));
  }

  switch (i) {
    case 0:  Build_OP(top, add1_tn, rs1, add1_tn, tmp0, am1, dtyp, ops);  break;
    case 1:  Build_OP(top, add1_tn, rs1, add1_tn, tmp0, am1, dtyp, const_parm[0], ops);  break;
    default:  Is_True(0, ("const num should <=1 "));
  }
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);

  if (flag & 1) {
    Build_OP(TOP_c3_mvfaddr, as1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }

  return result;
}

// rd, acc, acm, imm5
TN *Expand_C3_shlafa_i(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm =  Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *uimm5 =  Get_Liternal_TN(intrncall, 2, 4);
  Is_IMM_Valid(intrncall, 2, uimm5, UIMM5);
  TN *oper = Get_Liternal_TN(intrncall, 3, 4);
  INT32 value = -1;
  UINT32 flag = 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  value = Get_Oper_Value(intrncall, oper, 4);
  
  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  
  TOP top = value == 1 ? TOP_c3_shrafa_i : TOP_c3_shlafa_i;

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  }
  Build_OP(top, result, acc_tn, acm, uimm5, ops);

  if (flag & 1) {
    Erase_AccTN(acc_tn);
  }
  return result;  
}

// c3.ffe
TN *Expand_C3_FFE(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *op1 =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *op2 =  Expand_Expr(WN_kid1(intrncall), intrncall, NULL);
  TN *mode =  Get_Liternal_TN(intrncall, 2, 4);
  Is_IMM_Valid(intrncall, 2, mode, UIMM5);
  Build_OP(TOP_c3_ffe, op1, op1, op2, mode, ops);
  return op1;
}

//c3.lead rd, acc
TN *Expand_C3_lead(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid(WN_kid0(intrncall), (intrncall, "arguments are incomplete"));
  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 flag = -1;
  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  }

  Build_OP(TOP_c3_lead, result, acc_tn, ops);

  if (flag & 1) {
    Erase_AccTN(acc_tn);
  }
  return result;
}


//c3.revb rd, rs1, imm5
TN *Expand_C3_revb(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *op1 =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *mode =  Get_Liternal_TN(intrncall, 1, 4);
  Build_OP(TOP_c3_revb, op1, op1, mode, ops);
  return op1;
}

// c3.sadda.a  acc, acm, as1, am1, uimm4, mode , N, M
TN *Expand_C3_saadda_a(WN *intrncall, TN *result, OPS *ops) {

  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)
                 && WN_kid(intrncall, 4) && WN_kid(intrncall, 5) && WN_kid(intrncall, 6) && WN_kid(intrncall, 7)),
                 (intrncall, "arguments are incomplete"));

  TN *acc_kid0 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // acc
  TN *acm = Get_Liternal_TN(intrncall, 1, 4);
  Is_IMM_Valid(intrncall, 1, acm, UIMM3);
  TN *as1_kid1 = Expand_Expr(WN_kid2(intrncall), intrncall, NULL);
  TN *am1 = Get_Liternal_TN(intrncall, 3, 4); 
  Is_IMM_Valid(intrncall, 3, am1, UIMM3);

  TN *uimm4 = Get_Liternal_TN(intrncall, 4, 4);
  TN *mode = Get_Liternal_TN(intrncall, 5, 4);
  TN *n = Get_Liternal_TN(intrncall, 6, 4);
  TN *m = Get_Liternal_TN(intrncall, 7, 4);
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *acc_tn = Get_Acc_from_Varidx(v1);
  INT32 v2 = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *add1_tn = Get_Addr_from_Varidx(v2);
  UINT32 flag = 0;

  if (!acc_tn) {
    acc_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, acc_tn, acc_kid0, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, acc_tn);
  } 
  if (!add1_tn) {
    add1_tn = Get_New_AddrTN(intrncall);
    Build_OP(TOP_c3_mvtaddr, add1_tn, as1_kid1, tmp0, ops);
    flag |= 2;
  }  

  Build_OP(TOP_c3_sadda_a, acc_tn, add1_tn, acm, add1_tn, am1, uimm4, mode, n, m, acc_tn, ops);
  // map to whirl node for memory operation
  Set_OP_To_WN_Map(intrncall);

  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc, result, acc_tn, tmp0, ops);
    Erase_AccTN(acc_tn);
  }

  if (flag & 2) {
    Build_OP(TOP_c3_mvfaddr, as1_kid1, add1_tn, tmp0, ops);
    Erase_AddrTN(add1_tn);
  }
  return result;
}

//c3.subc hi, rd, rs1, rs2
TN *Expand_C3_subc(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *op1 =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *op2 =  Expand_Expr(WN_kid1(intrncall), intrncall, NULL);
  TN *tmp0 = Gen_Literal_TN(0, 4);
  TN *tmp = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  Build_OP(TOP_c3_subc, HI_TN, op1, op1, op2, HI_TN, ops);
  Exp_COPY(result, op1, ops);
  return result;
}

// c3.nega 
TN *Expand_C3_nega(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
          (intrncall, "arguments are incomplete"));

  TN *accd = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);  // accd
  TN *acm  = Get_Liternal_TN(intrncall, 1, 4);  // acm
  TN *accs = Expand_Expr(WN_kid2(intrncall), intrncall, NULL);  // accs
  TN *m    = Get_Liternal_TN(intrncall, 3, 4);

  UINT flag= 0;
  TN *tmp0 = Gen_Literal_TN(0, 4);
  INT32 v1 = Get_ParmVaridx_Intrncall(intrncall);
  TN *accd_tn = Get_Acc_from_Varidx(v1);
  INT32 v2  = Get_ParmVaridx_Intrncall(intrncall, 2);
  TN *accs_tn = Get_Acc_from_Varidx(v2);

  if (!accd_tn) {
    accd_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, accd_tn, accd, tmp0, ops);
    flag = 1;
  } else {
    Set_Var_AccTN(AccPregN, accd_tn);
  }
  if (!accs_tn) {
    accs_tn = Get_New_AccTN(intrncall);
    Build_OP(TOP_c3_mvtacc, accs_tn, accs, tmp0, ops);
    flag |=2;
  }
  Build_OP(TOP_c3_nega, accd_tn, acm, accs_tn, m, ops);

  if (flag & 1) {
    Build_OP(TOP_c3_mvfacc,  result, accd_tn, tmp0, ops);
    Erase_AccTN(accd_tn);
  }
  if (flag & 2) {
    Erase_AccTN(accs_tn);
  }
  return result;
}

// c3.muls/c3.mulus
TN *Expand_C3_mul(TOP top, WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall) && WN_kid2(intrncall) &&  WN_kid3(intrncall)),
          (intrncall, "arguments are incomplete"));
  TN *op1   =  Expand_Expr(WN_kid0(intrncall), intrncall, NULL); // rs1
  TN *op2   =  Expand_Expr(WN_kid1(intrncall), intrncall, NULL); // rs2
  TN *uimm5 = Get_Liternal_TN(intrncall, 2, 4);  // shl
  Is_IMM_Valid(intrncall, 2, uimm5, UIMM5);
  TN *hi    = Get_Liternal_TN(intrncall, 3, 4);  // hi or low
  INT32 value = -1;
  if (TN_has_value(hi)) {
    value = TN_value(hi);
  } else {
    Is_Param_Valid(0, (intrncall, "hi_bit(argument 4) should be 0/1"));
  }
  
  TN *tmp0 = Gen_Literal_TN(0, 4);
  TN *tmp = Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);

  Build_OP(top, HI_TN, tmp, op1, op2, uimm5, ops);
  if (value == 1)
    Build_OP(TOP_c3_mvfs, result, HI_TN, tmp0, ops);
  else 
    Exp_COPY(result, tmp, ops);

  return result;
}


TN *Expand_Init_HI(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)), (intrncall, "arguments are incomplete"));
  TN *rs1 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL);
  TN *uimm5 = Get_Liternal_TN(intrncall, 1, 4);  // shl
  Is_IMM_Valid(intrncall, 1, uimm5, UIMM5);

  TOP top = TOP_c3_mvts;
  Build_OP(top, HI_TN, rs1, uimm5, ops);
  return result;

}

TN *Expand_Copy_HI(WN *intrncall, TN *result, OPS *ops) {
  Is_Param_Valid((WN_kid0(intrncall) && WN_kid1(intrncall)), (intrncall, "arguments are incomplete"));
  TN *rs1 = Expand_Expr(WN_kid0(intrncall), intrncall, NULL); 
  TN *uimm5 = Get_Liternal_TN(intrncall, 1, 4);  // shr
  Is_IMM_Valid(intrncall, 1, uimm5, UIMM5);
  TOP top = TOP_c3_mvfs;
  Build_OP(top, result, HI_TN, uimm5, ops);
  return result;
}

