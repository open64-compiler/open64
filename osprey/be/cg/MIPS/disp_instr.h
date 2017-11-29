#ifndef disp_instr_INCLUDED
#define disp_instr_INCLUDED

#include "bb.h"


     
class Repl_Size16_Instr {

private:
   OP* _cur_op;
   
public:
  Repl_Size16_Instr (OP* op) { _cur_op = op; }
  ~Repl_Size16_Instr () {};
  
  void Set_OP (OP *op) { _cur_op = op; }
  OP* Get_OP (void) { return _cur_op;}
  
  BOOL Replace_ResEqOper_OP (BOOL either);
  BOOL Replace_Restricted_Imm_OP (void);
  BOOL Replace_LdSt_OP (void);
  BOOL Replace_Direct_OP (void);
  BOOL Replace_NOP (void);
  BOOL Replace_ABS(void);
  BOOL Replace_ADDI (void);
  BOOL Replace_LDU(void);
  BOOL Replace_BR(void);

  inline void Replace_CurOP(OP *newop); 
  inline BOOL Replaceable_TOP_Imm( TOP top, INT32 imm);
  inline BOOL Tn_Is_Reloc_TN(TN *tn);
  void Replace_OP (void);
};

extern void Replace_Size16_Instr (void);
extern void Guarantee_Paired_instr16 (void);
extern void Replace_Size16_Instr(BB *bb) ;
extern void Check_QuadWord_Boundary(void);
extern void Check_QuadWord_Boundary_Absolute(void) ;
extern void Check_Br16(int first);
typedef struct MvtcInfor {
    UINT32 cr_idx;
    UINT32 pc;	
  } MVTCInfor;
#endif // disp_instr_INCLUDED

