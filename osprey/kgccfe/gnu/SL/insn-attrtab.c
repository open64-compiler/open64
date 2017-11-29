/* Generated automatically by the program `genattrtab'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "regs.h"
#include "real.h"
#include "output.h"
#include "insn-attr.h"
#include "toplev.h"
#include "flags.h"
#include "function.h"

#define operands recog_data.operand

extern int insn_current_length PARAMS ((rtx));
int
insn_current_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      extract_constrain_insn_cached (insn);
      if ((abs ((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - ((insn_current_reference_address (insn)) + (4)))) < (131072))
        {
	  return 4;
        }
      else if ((flag_pic && ! TARGET_EMBEDDED_PIC) != (0))
        {
	  return 24 /* 0x18 */;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 279:
      extract_constrain_insn_cached (insn);
      if (((flag_pic && ! TARGET_EMBEDDED_PIC) == (0)) || ((abs ((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[0]) == LABEL_REF ? XEXP (operands[0], 0) : operands[0])) : 0) - ((insn_current_reference_address (insn)) + (4)))) < (131072)))
        {
	  return 4;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_variable_length_p PARAMS ((rtx));
int
insn_variable_length_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 279:
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_default_length PARAMS ((rtx));
int
insn_default_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 188:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 187:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 8;
        }
      else if (which_alternative == 2)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 3)
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 16 /* 0x10 */;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return 8;
        }
      else
        {
	  return 4;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (which_alternative == 1)
        {
	  return 8;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return 4;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return 8;
        }
      else if (which_alternative == 7)
        {
	  return 4;
        }
      else if (which_alternative == 8)
        {
	  return 8;
        }
      else if (which_alternative == 9)
        {
	  return 4;
        }
      else
        {
	  return 4;
        }

    case 185:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 16 /* 0x10 */;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 8;
        }
      else if (which_alternative == 10)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 11)
        {
	  return 8;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 184:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 4;
        }
      else if (which_alternative == 10)
        {
	  return 8;
        }
      else if (which_alternative == 11)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 4)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 8;
	    }
	  else
	    {
	      return 12 /* 0xc */;
	    }
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if (which_alternative == 7)
        {
	  return 4;
        }
      else if (which_alternative == 8)
        {
	  return 8;
        }
      else
        {
	  return 4;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 4;
        }
      else
        {
	  return 4;
        }

    case 169:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 4;
        }
      else if (which_alternative == 10)
        {
	  return 8;
        }
      else if (which_alternative == 11)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 4)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 8;
	    }
	  else
	    {
	      return 12 /* 0xc */;
	    }
        }
      else if (which_alternative == 5)
        {
	  if (m16_usym8_4 (operands[1], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 6)
        {
	  return 4;
        }
      else if (which_alternative == 7)
        {
	  return 8;
        }
      else if (which_alternative == 8)
        {
	  return 4;
        }
      else if (which_alternative == 9)
        {
	  return 8;
        }
      else
        {
	  return 4;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))
        {
	  return 4;
        }
      else if (which_alternative == 10)
        {
	  return 8;
        }
      else if ((which_alternative == 11) || (which_alternative == 12))
        {
	  return 4;
        }
      else if (which_alternative == 13)
        {
	  return 8;
        }
      else if ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || (which_alternative == 21))))))))
        {
	  return 4;
        }
      else if (which_alternative == 22)
        {
	  return 8;
        }
      else if ((which_alternative == 23) || (which_alternative == 24))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 4)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 8;
	    }
	  else
	    {
	      return 12 /* 0xc */;
	    }
        }
      else if (which_alternative == 5)
        {
	  if (m16_usym5_4 (operands[1], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 6)
        {
	  return 4;
        }
      else if (which_alternative == 7)
        {
	  return 8;
        }
      else if (which_alternative == 8)
        {
	  return 4;
        }
      else if (which_alternative == 9)
        {
	  return 8;
        }
      else
        {
	  return 4;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 4;
        }
      else if ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17))))))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (which_alternative == 6)
        {
	  return 8;
        }
      else if ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))
        {
	  return 4;
        }
      else if (which_alternative == 10)
        {
	  return 8;
        }
      else if ((which_alternative == 11) || (which_alternative == 12))
        {
	  return 4;
        }
      else if (which_alternative == 13)
        {
	  return 8;
        }
      else if ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16)))
        {
	  return 4;
        }
      else if ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || (which_alternative == 22))))))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 161:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 8;
        }
      else if (which_alternative == 4)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 5)
        {
	  return 8;
        }
      else if (which_alternative == 6)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 7)
        {
	  return 8;
        }
      else if (which_alternative == 8)
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 8;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else if (which_alternative == 1)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 2)
        {
	  return 8;
        }
      else if (which_alternative == 3)
        {
	  return 16 /* 0x10 */;
        }
      else if (which_alternative == 4)
        {
	  return 8;
        }
      else if (which_alternative == 5)
        {
	  return 16 /* 0x10 */;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || (which_alternative == 13))))))))
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 2)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 338:
    case 337:
    case 335:
    case 333:
    case 332:
    case 331:
    case 313:
    case 311:
    case 310:
    case 306:
    case 304:
    case 303:
    case 302:
    case 298:
    case 296:
    case 295:
    case 285:
    case 280:
    case 230:
    case 229:
    case 213:
    case 205:
    case 198:
    case 121:
    case 120:
    case 119:
    case 110:
    case 33:
    case 31:
      return 8;

    case 341:
    case 340:
      return 40 /* 0x28 */;

    case 339:
    case 189:
    case 89:
    case 86:
    case 85:
    case 72:
    case 71:
    case 36:
      return 12 /* 0xc */;

    case 300:
      return 1;

    case 292:
      return 20 /* 0x14 */;

    case 289:
      return 0;

    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 288:
    case 287:
    case 90:
      return 24 /* 0x18 */;

    case 336:
    case 334:
    case 214:
    case 206:
    case 199:
    case 92:
    case 22:
    case 8:
    case 279:
      return 16 /* 0x10 */;

    case 260:
    case 258:
    case 248:
    case 246:
      extract_insn_cached (insn);
      if (m16_uimm8_m1_1 (operands[2], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 256:
    case 254:
    case 244:
    case 242:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else
        {
	  if (m16_uimm8_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }

    case 216:
    case 210:
    case 208:
    case 203:
    case 201:
    case 196:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }

    case 212:
    case 204:
    case 197:
      return 48 /* 0x30 */;

    case 211:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 8;
	    }
	  else
	    {
	      return 12 /* 0xc */;
	    }
        }
      else
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 12 /* 0xc */;
	    }
	  else
	    {
	      return 16 /* 0x10 */;
	    }
        }

    case 193:
    case 192:
    case 191:
    case 190:
      return 80 /* 0x50 */;

    case 165:
      extract_constrain_insn_cached (insn);
      if (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 159:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 156:
    case 155:
    case 154:
    case 153:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 8;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 146:
    case 144:
      return 36 /* 0x24 */;

    case 118:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  return 4;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 117:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  return 4;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 116:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  return 8;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 114:
      extract_constrain_insn_cached (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 111:
    case 108:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (which_alternative == 1)
        {
	  if (m16_uimm8_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else
        {
	  return 4;
        }

    case 109:
      extract_constrain_insn_cached (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 106:
      extract_constrain_insn_cached (insn);
      if ((mips_isa) >= (3))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 105:
      extract_constrain_insn_cached (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 101:
      extract_constrain_insn_cached (insn);
      if ((mips_isa) >= (3))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 100:
      extract_constrain_insn_cached (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 97:
      extract_constrain_insn_cached (insn);
      if ((mips_isa) >= (3))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 42:
      extract_constrain_insn_cached (insn);
      if ((GENERATE_MULT3_DI) != (0))
        {
	  return 4;
        }
      else
        {
	  return 12 /* 0xc */;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (which_alternative == 1)
        {
	  return 8;
        }
      else
        {
	  return 8;
        }

    case 29:
    case 21:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (m16_nsimm8_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_nsimm4_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else
        {
	  return 4;
        }

    case 27:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (m16_nsimm5_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_nsimm4_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else
        {
	  return 4;
        }

    case 26:
      extract_insn_cached (insn);
      if (m16_nuimm5_4 (operands[0], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 25:
    case 19:
      extract_insn_cached (insn);
      if (m16_nsimm8_8 (operands[0], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 23:
    case 9:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 12 /* 0xc */;
        }
      else if (which_alternative == 1)
        {
	  return 8;
        }
      else
        {
	  return 16 /* 0x10 */;
        }

    case 20:
      extract_insn_cached (insn);
      if (m16_nuimm8_4 (operands[1], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 15:
    case 7:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (m16_simm8_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_simm4_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else
        {
	  return 4;
        }

    case 13:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  if (m16_simm5_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_simm4_1 (operands[2], VOIDmode))
	    {
	      return 4;
	    }
	  else
	    {
	      return 8;
	    }
        }
      else
        {
	  return 4;
        }

    case 12:
      extract_insn_cached (insn);
      if (m16_uimm5_4 (operands[0], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 11:
    case 5:
      extract_insn_cached (insn);
      if (m16_simm8_8 (operands[0], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case 6:
      extract_insn_cached (insn);
      if (m16_uimm8_4 (operands[1], VOIDmode))
        {
	  return 4;
        }
      else
        {
	  return 8;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

extern int bypass_p PARAMS ((rtx));
int
bypass_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern int insn_default_latency PARAMS ((rtx));
int
insn_default_latency (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 0;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 3;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 6;
        }
      else
        {
	  return 0;
        }

    case 187:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 5))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 6))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 5))
        {
	  return 9;
        }
      else
        {
	  return 0;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 185:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 9;
        }
      else
        {
	  return 0;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 9;
        }
      else
        {
	  return 0;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if (((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || ((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5500)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if (((which_alternative == 9) && (((mips_cpu_attr) == (CPU_SR71000)))) || ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 6) || (which_alternative == 7))) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 1))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 173:
    case 172:
    case 171:
    case 170:
      if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 3;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 9;
        }
      else
        {
	  return 0;
        }

    case 169:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))) || ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 6;
        }
      else
        {
	  return 0;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23))))))) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 10) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 10)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 10) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16))))) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21))))) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 161:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 9) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 9)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 9) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) || (which_alternative == 1)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 9) || (which_alternative == 12))) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 1))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) || (which_alternative == 1)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 189:
    case 157:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 150:
    case 149:
    case 147:
    case 146:
    case 145:
    case 144:
    case 143:
    case 142:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 6;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 0;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 155:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 3;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case 152:
    case 151:
    case 148:
    case 115:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 6;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 0;
        }

    case 95:
    case 94:
    case 88:
    case 87:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 84:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 61 /* 0x3d */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 60 /* 0x3c */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 48 /* 0x30 */;
        }
      else
        {
	  return 0;
        }

    case 83:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 121 /* 0x79 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 118 /* 0x76 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 240 /* 0xf0 */;
        }
      else
        {
	  return 0;
        }

    case 82:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 30 /* 0x1e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 62 /* 0x3e */;
        }
      else
        {
	  return 0;
        }

    case 81:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 72 /* 0x48 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 59 /* 0x3b */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 122 /* 0x7a */;
        }
      else
        {
	  return 0;
        }

    case 80:
    case 78:
    case 76:
    case 74:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 74 /* 0x4a */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 73 /* 0x49 */;
        }
      else
        {
	  return 0;
        }

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 41 /* 0x29 */;
        }
      else
        {
	  return 0;
        }

    case 66:
    case 64:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 30 /* 0x1e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 0;
        }

    case 65:
    case 63:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 72 /* 0x48 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 59 /* 0x3b */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 120 /* 0x78 */;
        }
      else
        {
	  return 0;
        }

    case 62:
    case 60:
    case 58:
    case 56:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 9;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 61:
    case 59:
    case 57:
    case 55:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 0;
        }

    case 54:
    case 53:
    case 52:
    case 46:
      if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 51:
    case 50:
    case 42:
    case 41:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 6;
        }
      else
        {
	  return 0;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0)) || ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 2)) || ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 2)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 2))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 2))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 49:
    case 48:
    case 47:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 3;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 33:
    case 32:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 31:
    case 30:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 6;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 0;
        }

    case 17:
    case 3:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 16:
    case 2:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      if ((((mips_cpu_attr) == (CPU_R5400))) || ((((mips_cpu_attr) == (CPU_R5500))) || (((mips_cpu_attr) == (CPU_SR71000)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    }
}

extern int insn_alts PARAMS ((rtx));
int
insn_alts (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 187:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 6))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))) || ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 6))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 5))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 0;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 185:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && ((((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))) || (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 0;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 3) || (which_alternative == 4)) || ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 1) || (which_alternative == 2)) || ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 64 /* 0x40 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 0;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || (((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || (((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5500)))) || ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 4;
        }
      else if (((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))) || (((which_alternative == 9) && (((mips_cpu_attr) == (CPU_SR71000)))) || ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 6) || (which_alternative == 7))) || ((which_alternative != 0) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 8)) || ((which_alternative == 6) || (which_alternative == 7))) || ((which_alternative != 0) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 177:
    case 176:
    case 175:
    case 174:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 32 /* 0x20 */;
        }
      else
        {
	  return 0;
        }

    case 173:
    case 172:
    case 171:
    case 170:
      if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 0;
        }

    case 169:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))) || (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && ((((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))) || (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))) || (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 32 /* 0x20 */;
        }
      else
        {
	  return 0;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23))))))) || (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23))))))) || (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 10) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 10) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative == 8) || (which_alternative == 9)))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 10) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16)))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16))))) || (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16)))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16))))) || (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16)))))))))))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21))))) || (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((((which_alternative == 0) || (which_alternative == 7)) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21))))) || (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 161:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 9) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 9) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && (((which_alternative == 5) || (which_alternative == 6)) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative == 9) || (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 9) || (which_alternative == 12))) || (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) || (which_alternative == 1))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative == 0) || ((which_alternative == 9) || (which_alternative == 12))) || (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) || (which_alternative == 1))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) || (which_alternative == 1)))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 194:
    case 192:
    case 190:
    case 165:
    case 159:
    case 156:
    case 155:
    case 154:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 80:
    case 79:
    case 78:
    case 77:
    case 76:
    case 75:
    case 74:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 54:
    case 53:
    case 52:
    case 46:
      if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0)) || ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 2)) || ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 2)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 2))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 2))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 51:
    case 50:
    case 49:
    case 48:
    case 47:
    case 45:
    case 44:
    case 43:
    case 42:
    case 41:
    case 39:
    case 36:
    case 35:
    case 34:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 4;
        }
      else
        {
	  return 0;
        }

    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 152:
    case 151:
    case 150:
    case 149:
    case 148:
    case 147:
    case 146:
    case 145:
    case 144:
    case 143:
    case 142:
    case 115:
    case 95:
    case 94:
    case 88:
    case 87:
    case 84:
    case 83:
    case 82:
    case 81:
    case 66:
    case 65:
    case 64:
    case 63:
    case 62:
    case 61:
    case 60:
    case 59:
    case 58:
    case 57:
    case 56:
    case 55:
    case 33:
    case 32:
    case 31:
    case 30:
    case 17:
    case 16:
    case 3:
    case 2:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 2;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 294:
    case 293:
    case 289:
    case 193:
    case 191:
    case 90:
    case 89:
    case 86:
    case 85:
    case 72:
    case 71:
    case 1:
    case 0:
      if ((((mips_cpu_attr) == (CPU_R5400))) || ((((mips_cpu_attr) == (CPU_R5500))) || (((mips_cpu_attr) == (CPU_SR71000)))))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    default:
      if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 8;
        }
      else
        {
	  return 0;
        }

    }
}

extern int internal_dfa_insn_code PARAMS ((rtx));
int
internal_dfa_insn_code (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 22 /* 0x16 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 48 /* 0x30 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 77 /* 0x4d */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 28 /* 0x1c */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 55 /* 0x37 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 187:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 5))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 5))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && (which_alternative != 6)))))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 5))
        {
	  return 62 /* 0x3e */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 185:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 62 /* 0x3e */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 58 /* 0x3a */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 59 /* 0x3b */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 62 /* 0x3e */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 5;
        }
      else if ((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((which_alternative == 9) && (((mips_cpu_attr) == (CPU_R5500))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((which_alternative == 9) && (((mips_cpu_attr) == (CPU_SR71000))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 1))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 1))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || (which_alternative == 3)))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 8)))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 61 /* 0x3d */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 1))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 177:
    case 176:
    case 175:
    case 174:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 30 /* 0x1e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 59 /* 0x3b */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 173:
    case 172:
    case 171:
    case 170:
      if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 29 /* 0x1d */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 58 /* 0x3a */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 169:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 61 /* 0x3d */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 318:
    case 168:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 34 /* 0x22 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 84 /* 0x54 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 61 /* 0x3d */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 10))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 10))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 10))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 61 /* 0x3d */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || (which_alternative == 7)))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 61 /* 0x3d */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 161:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 9))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 9))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 9))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 1))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 30 /* 0x1e */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 31 /* 0x1f */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 32 /* 0x20 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 33 /* 0x21 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 1))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 57 /* 0x39 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 60 /* 0x3c */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 61 /* 0x3d */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 63 /* 0x3f */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 1))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 330:
    case 329:
    case 328:
    case 327:
    case 326:
    case 325:
    case 324:
    case 323:
    case 322:
    case 321:
    case 320:
    case 319:
    case 189:
    case 157:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 31 /* 0x1f */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 194:
    case 192:
    case 190:
    case 165:
    case 159:
    case 156:
    case 154:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 3;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 30 /* 0x1e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 57 /* 0x39 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 150:
    case 149:
    case 147:
    case 146:
    case 145:
    case 144:
    case 143:
    case 142:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 50 /* 0x32 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 78 /* 0x4e */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0))
        {
	  return 29 /* 0x1d */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 0))
        {
	  return 34 /* 0x22 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 56 /* 0x38 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 155:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 29 /* 0x1d */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 56 /* 0x38 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 152:
    case 151:
    case 148:
    case 115:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 49 /* 0x31 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 78 /* 0x4e */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 95:
    case 94:
    case 88:
    case 87:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 47 /* 0x2f */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 76 /* 0x4c */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 84:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 24 /* 0x18 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 51 /* 0x33 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 81 /* 0x51 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 83:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 25 /* 0x19 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 52 /* 0x34 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 82 /* 0x52 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 82:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 45 /* 0x2d */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 79 /* 0x4f */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 81:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 20 /* 0x14 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 46 /* 0x2e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 80 /* 0x50 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 80:
    case 78:
    case 76:
    case 74:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 13 /* 0xd */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 39 /* 0x27 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 68 /* 0x44 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 12 /* 0xc */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 38 /* 0x26 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 67 /* 0x43 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 66:
    case 64:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 45 /* 0x2d */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 74 /* 0x4a */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 65:
    case 63:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 20 /* 0x14 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 46 /* 0x2e */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 75 /* 0x4b */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 62:
    case 60:
    case 58:
    case 56:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 43 /* 0x2b */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 72 /* 0x48 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 61:
    case 59:
    case 57:
    case 55:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 18 /* 0x12 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 44 /* 0x2c */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 73 /* 0x49 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 54:
    case 53:
    case 52:
    case 46:
      if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 65 /* 0x41 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 51:
    case 50:
    case 42:
    case 41:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 36 /* 0x24 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 66 /* 0x42 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative != 0))
        {
	  return 26 /* 0x1a */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative != 0))
        {
	  return 53 /* 0x35 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 0))
        {
	  return 65 /* 0x41 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 0))
        {
	  return 83 /* 0x53 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 2))
        {
	  return 26 /* 0x1a */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5500))) && (which_alternative == 2))
        {
	  return 53 /* 0x35 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative != 2))
        {
	  return 65 /* 0x41 */;
        }
      else if ((((mips_cpu_attr) == (CPU_SR71000))) && (which_alternative == 2))
        {
	  return 83 /* 0x53 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 49:
    case 48:
    case 47:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 9;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 35 /* 0x23 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 65 /* 0x41 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 33:
    case 32:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 15 /* 0xf */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 41 /* 0x29 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 72 /* 0x48 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 31:
    case 30:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 73 /* 0x49 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 17:
    case 3:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 40 /* 0x28 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 70 /* 0x46 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 16:
    case 2:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 40 /* 0x28 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 71 /* 0x47 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 294:
    case 293:
    case 289:
    case 72:
    case 71:
    case 1:
    case 0:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 0;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 27 /* 0x1b */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 54 /* 0x36 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 193:
    case 191:
    case 90:
    case 89:
    case 86:
    case 85:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 26 /* 0x1a */;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 53 /* 0x35 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 83 /* 0x53 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    default:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5500)))
        {
	  return 34 /* 0x22 */;
        }
      else if (((mips_cpu_attr) == (CPU_SR71000)))
        {
	  return 64 /* 0x40 */;
        }
      else
        {
	  return 86 /* 0x56 */;
        }

    }
}

extern int result_ready_cost PARAMS ((rtx));
int
result_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R6000)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 187:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || (which_alternative == 5))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 2)) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || (which_alternative == 2)) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || (which_alternative == 3)) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 2) || (which_alternative == 3)) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 185:
    case 169:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if (((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 167:
    case 164:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 181:
    case 179:
    case 161:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((which_alternative == 9) || (which_alternative == 12)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 3;
        }
      else if ((which_alternative != 0) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 173:
    case 172:
    case 171:
    case 170:
    case 155:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 95:
    case 94:
    case 88:
    case 87:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 84:
    case 82:
      if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))
        {
	  return 54 /* 0x36 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 31 /* 0x1f */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else
        {
	  return 1;
        }

    case 83:
    case 81:
      if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))
        {
	  return 112 /* 0x70 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 60 /* 0x3c */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 1;
        }

    case 80:
    case 78:
    case 76:
    case 74:
      if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 69 /* 0x45 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 68 /* 0x44 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  return 67 /* 0x43 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R4650))))))
        {
	  return 38 /* 0x26 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  return 69 /* 0x45 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))
        {
	  return 38 /* 0x26 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 37 /* 0x25 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 36 /* 0x24 */;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))
        {
	  return 35 /* 0x23 */;
        }
      else
        {
	  return 1;
        }

    case 66:
    case 64:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 32 /* 0x20 */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))
        {
	  return 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 1;
        }

    case 65:
    case 63:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 61 /* 0x3d */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 1;
        }

    case 51:
    case 50:
    case 42:
    case 41:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 9;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((which_alternative == 0) && ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((which_alternative == 0) && ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600)))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative == 0) && ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 5;
        }
      else if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 2) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600)))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 5;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 54:
    case 53:
    case 52:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 33:
    case 32:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 8;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R6000))) || (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 31:
    case 30:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))
        {
	  return 8;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 6;
        }
      else
        {
	  return 5;
        }

    case 17:
    case 16:
    case 3:
    case 2:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R6000))) || (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

extern int divide_unit_ready_cost PARAMS ((rtx));
int
divide_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 84:
    case 82:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 31 /* 0x1f */;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 54 /* 0x36 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 83:
    case 81:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 66:
    case 64:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 32 /* 0x20 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 12 /* 0xc */;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 23 /* 0x17 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 65:
    case 63:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 61 /* 0x3d */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 19 /* 0x13 */;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 112 /* 0x70 */;

    }
}

extern int mult_unit_ready_cost PARAMS ((rtx));
int
mult_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 33:
    case 32:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 4;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && (! (((mips_cpu_attr) == (CPU_R4600))))))
        {
	  return 7;
        }
      else
        {
	  return 8;
        }

    case 31:
    case 30:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 5;
        }
      else
        {
	  return 8;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 8;

    }
}

extern int adder_unit_ready_cost PARAMS ((rtx));
int
adder_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
      if ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000)))))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 1;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 3;
        }
      else
        {
	  return 4;
        }

    case 95:
    case 94:
    case 88:
    case 87:
      if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R5000))) || ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600)))))))
        {
	  return 1;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 17:
    case 16:
    case 3:
    case 2:
      if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

extern int imuldiv_unit_ready_cost PARAMS ((rtx));
int
imuldiv_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))))) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))) && (((((mips_cpu_attr) == (CPU_R4300))) || ((((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))) || (((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19)))) && (((((mips_cpu_attr) == (CPU_R4300))) || ((((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))) || (((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 181:
    case 179:
    case 161:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8))) && (((((mips_cpu_attr) == (CPU_R4300))) || (((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R5000)))) || ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))) || ((((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))) || ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 95:
    case 94:
    case 88:
    case 87:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 80:
    case 78:
    case 76:
    case 74:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 68 /* 0x44 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))
        {
	  return 67 /* 0x43 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 35 /* 0x23 */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R4000)))))
        {
	  return 38 /* 0x26 */;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 37 /* 0x25 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))
        {
	  return 35 /* 0x23 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 35 /* 0x23 */;
        }
      else if (! (((mips_cpu_attr) == (CPU_R4000))))
        {
	  return 38 /* 0x26 */;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 51:
    case 50:
    case 42:
    case 41:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 8;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 9;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 5;
        }
      else if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R4000))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 5;
        }
      else if ((which_alternative == 0) && ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))
        {
	  return 1;
        }
      else if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 4;
        }
      else if ((which_alternative == 0) && ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((which_alternative == 0) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 5;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4000))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 5;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100)))))
        {
	  return 1;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 4;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((which_alternative != 2) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 54:
    case 53:
    case 52:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) || (((mips_cpu_attr) == (CPU_R4100))))
        {
	  return 1;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 17 /* 0x11 */;
        }

    case 33:
    case 32:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 5;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 31:
    case 30:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 8;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 17:
    case 16:
    case 3:
    case 2:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 3;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 69 /* 0x45 */;

    }
}

extern unsigned int imuldiv_unit_blockage_range PARAMS ((rtx));
unsigned int
imuldiv_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65605 /* min 1, max 69 */;

    }
}

extern int memory_unit_ready_cost PARAMS ((rtx));
int
memory_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 187:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 3) || (which_alternative == 4)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) && ((which_alternative != 5) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9)))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 8) && (which_alternative != 9))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (which_alternative == 4)) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 185:
    case 169:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 4) || (which_alternative == 5)) || ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 4) && (which_alternative != 5)) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25))))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22))))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 24) || (which_alternative == 25)))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 24) && (which_alternative != 25)))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 167:
    case 164:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16)))))))))))))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 14) && ((which_alternative != 15) && (which_alternative != 16))))))))))))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) && (((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23))))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20))))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 12) || ((which_alternative == 13) || ((which_alternative == 22) || (which_alternative == 23)))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) && (((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 12) && ((which_alternative != 13) && ((which_alternative != 22) && (which_alternative != 23)))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 181:
    case 179:
    case 161:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4120))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14))))))) || (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 10) && (which_alternative != 11)))) && (((which_alternative != 9) && (which_alternative != 12)) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 194:
    case 192:
    case 190:
    case 177:
    case 176:
    case 175:
    case 174:
    case 165:
    case 159:
    case 156:
    case 154:
      return 1;

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if (((((mips_cpu_attr) == (CPU_R5000))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4300))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4120))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4100))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4650))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4600))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R3900))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R3000))) && (which_alternative == 0)) || ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 173:
    case 172:
    case 171:
    case 170:
    case 155:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      if ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 3;

    }
}

extern int function_units_used PARAMS ((rtx));
int
function_units_used (insn)
     rtx insn;
{
  enum attr_mode attr_mode = get_attr_mode (insn);
  enum attr_type attr_type = get_attr_type (insn);
  unsigned long accum = 0;

  accum |= ((((attr_type == TYPE_LOAD) && ((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))) || ((attr_type == TYPE_STORE) || (attr_type == TYPE_XFER))) ? (1) : (0));
  accum |= (((attr_type == TYPE_HILO) || ((((attr_type == TYPE_IMUL) || (attr_type == TYPE_IMADD)) && (((((((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((attr_mode == MODE_SI) && ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))) || ((attr_mode == MODE_DI) && ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))) || ((attr_mode == MODE_SI) && ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5000)))))) || (((attr_type == TYPE_IDIV) && (((((((((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4120))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R4600)))) || (((mips_cpu_attr) == (CPU_R4650)))) || (((mips_cpu_attr) == (CPU_R4000)))) || ((attr_mode == MODE_SI) && ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))) || ((attr_mode == MODE_DI) && ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120)))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5000)))))) || (((attr_type == TYPE_FADD) && (((mips_cpu_attr) == (CPU_R4300)))) || ((((attr_type == TYPE_FCMP) || ((attr_type == TYPE_FABS) || (attr_type == TYPE_FNEG))) && (((mips_cpu_attr) == (CPU_R4300)))) || (((attr_type == TYPE_FMUL) && (((attr_mode == MODE_SF) && (((mips_cpu_attr) == (CPU_R4300)))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R4300)))))) || (((attr_type == TYPE_FDIV) && ((attr_type == TYPE_FSQRT) || (attr_type == TYPE_FRSQRT))) && (((attr_mode == MODE_SF) && (((mips_cpu_attr) == (CPU_R4300)))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R4300)))))))))))) ? (2) : (0));
  accum |= ((((attr_type == TYPE_FCMP) && (((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R6000)))))) || (((mips_cpu_attr) == (CPU_R5000))))) || (((attr_type == TYPE_FADD) && (((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || (((mips_cpu_attr) == (CPU_R4300))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R6000))))) || (((attr_type == TYPE_FABS) || (attr_type == TYPE_FNEG)) && ((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) ? (4) : (0));
  accum |= ((((attr_type == TYPE_FDIV) && ((((((attr_mode == MODE_SF) && (((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R6000)))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))) || (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4300))))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R6000))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))))) || (((attr_type == TYPE_FSQRT) || (attr_type == TYPE_FRSQRT)) && (((((attr_mode == MODE_SF) && (((! ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))) || (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R5000))))))) ? (16) : (0));
  accum |= (((attr_type == TYPE_FMUL) && (((((attr_mode == MODE_SF) && ((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000)))))) || (((mips_cpu_attr) == (CPU_R6000)))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000))))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R6000)))))) ? (8) : (0));

  if (accum && accum == (accum & -accum))
    {
      int i;
      for (i = 0; accum >>= 1; ++i) continue;
      accum = i;
    }
  else
    accum = ~accum;
  return accum;
}

extern int num_delay_slots PARAMS ((rtx));
int
num_delay_slots (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 279:
      return 1;

    case 532:
    case 531:
    case 530:
    case 529:
    case 280:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

extern enum attr_can_delay get_attr_can_delay PARAMS ((rtx));
enum attr_can_delay
get_attr_can_delay (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 260:
    case 258:
    case 248:
    case 246:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 216:
    case 210:
    case 208:
    case 203:
    case 201:
    case 196:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 187:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 5) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 7) && ((mips16) == (0))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 8)))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 185:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && (which_alternative == 0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 184:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5)))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 9) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative != 3) && (which_alternative != 5))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 173:
    case 172:
    case 171:
    case 170:
      extract_constrain_insn_cached (insn);
      if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && ((mips16) == (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 169:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 16) && ((which_alternative != 17) && ((which_alternative != 18) && (which_alternative != 19))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || ((which_alternative == 23) || (which_alternative == 24)))))))))))))))))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 165:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if (((which_alternative != 10) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 16) && (which_alternative != 17))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && (which_alternative != 2)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      extract_constrain_insn_cached (insn);
      if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (((mips16) == (0)) && (which_alternative == 0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 118:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((mips16) == (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 117:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((mips16) == (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 114:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 109:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 256:
    case 254:
    case 244:
    case 242:
    case 111:
    case 108:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 106:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((mips_isa) >= (3)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 105:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 101:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((mips_isa) >= (3)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 100:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 97:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((mips_isa) >= (3)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 42:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && ((GENERATE_MULT3_DI) != (0)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 159:
    case 38:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (which_alternative == 0))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (which_alternative != 2))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 26:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_nuimm5_4 (operands[0], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 20:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_nuimm8_4 (operands[1], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 25:
    case 19:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_nsimm8_8 (operands[0], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 12:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_uimm5_4 (operands[0], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 29:
    case 27:
    case 21:
    case 15:
    case 13:
    case 7:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (get_attr_length (insn) == 4))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 6:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_uimm8_4 (operands[1], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 11:
    case 5:
      extract_constrain_insn_cached (insn);
      if (((mips16) == (0)) && (m16_simm8_8 (operands[0], VOIDmode)))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 341:
    case 340:
    case 339:
    case 338:
    case 337:
    case 336:
    case 335:
    case 334:
    case 333:
    case 332:
    case 331:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 295:
    case 292:
    case 291:
    case 290:
    case 289:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
    case 214:
    case 213:
    case 212:
    case 211:
    case 206:
    case 205:
    case 204:
    case 199:
    case 198:
    case 197:
    case 193:
    case 192:
    case 191:
    case 190:
    case 189:
    case 188:
    case 161:
    case 160:
    case 156:
    case 155:
    case 154:
    case 153:
    case 146:
    case 144:
    case 121:
    case 120:
    case 119:
    case 116:
    case 110:
    case 92:
    case 90:
    case 89:
    case 86:
    case 85:
    case 72:
    case 71:
    case 36:
    case 33:
    case 31:
    case 23:
    case 22:
    case 9:
    case 8:
    case 168:
      return CAN_DELAY_NO;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  return CAN_DELAY_YES;
        }
      else
        {
	  return CAN_DELAY_NO;
        }

    }
}

extern enum attr_dslot get_attr_dslot PARAMS ((rtx));
enum attr_dslot
get_attr_dslot (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 187:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 5) || (((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 6))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || (which_alternative == 2)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) || (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))) || (((which_alternative == 2) || (which_alternative == 3)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 185:
    case 169:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))) || ((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 167:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 20) || (which_alternative == 23)))))) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19))))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 21) || (which_alternative == 22)))))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 164:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 10) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 6) || (which_alternative == 7))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 13) || (which_alternative == 16)))) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12))))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 14) || (which_alternative == 15)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 8) || ((which_alternative == 11) || ((which_alternative == 18) || (which_alternative == 21)))) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17))))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 19) || (which_alternative == 20)))))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 161:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 9) || ((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 5) || (which_alternative == 6))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if ((((which_alternative == 9) || (which_alternative == 12)) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))) || (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 10) || (which_alternative == 11)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 0) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 317:
    case 316:
    case 315:
    case 314:
    case 295:
    case 211:
    case 173:
    case 172:
    case 171:
    case 170:
    case 155:
    case 153:
    case 141:
    case 140:
    case 139:
    case 138:
    case 137:
    case 136:
    case 135:
    case 133:
    case 131:
    case 129:
    case 127:
    case 125:
      extract_constrain_insn_cached (insn);
      if (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 532:
    case 531:
    case 530:
    case 529:
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 280:
    case 279:
    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      return DSLOT_YES;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return DSLOT_NO;

    }
}

extern enum attr_mode get_attr_mode PARAMS ((rtx));
enum attr_mode
get_attr_mode (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 261:
    case 262:
    case 263:
    case 264:
    case 265:
    case 266:
    case 267:
    case 268:
    case 269:
    case 270:
    case 271:
    case 272:
    case 273:
    case 274:
    case 275:
    case 276:
    case 277:
    case 278:
      return MODE_FPSW;

    case 2:
    case 16:
    case 30:
    case 31:
    case 55:
    case 57:
    case 59:
    case 61:
    case 63:
    case 65:
    case 81:
    case 83:
    case 87:
    case 94:
    case 142:
    case 143:
    case 144:
    case 145:
    case 146:
    case 147:
    case 149:
    case 150:
    case 172:
    case 173:
    case 176:
    case 177:
    case 185:
    case 186:
    case 187:
    case 188:
    case 328:
    case 329:
    case 330:
    case 336:
      return MODE_DF;

    case 3:
    case 17:
    case 32:
    case 33:
    case 56:
    case 58:
    case 60:
    case 62:
    case 64:
    case 66:
    case 82:
    case 84:
    case 88:
    case 95:
    case 115:
    case 148:
    case 151:
    case 152:
    case 170:
    case 171:
    case 174:
    case 175:
    case 182:
    case 183:
    case 184:
    case 325:
    case 326:
    case 327:
    case 335:
      return MODE_SF;

    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 22:
    case 23:
    case 24:
    case 25:
    case 26:
    case 27:
    case 28:
    case 41:
    case 42:
    case 50:
    case 51:
    case 74:
    case 76:
    case 78:
    case 80:
    case 86:
    case 90:
    case 92:
    case 93:
    case 97:
    case 100:
    case 101:
    case 102:
    case 105:
    case 106:
    case 109:
    case 110:
    case 111:
    case 112:
    case 114:
    case 125:
    case 128:
    case 129:
    case 134:
    case 135:
    case 136:
    case 137:
    case 141:
    case 159:
    case 160:
    case 161:
    case 162:
    case 163:
    case 164:
    case 189:
    case 198:
    case 199:
    case 200:
    case 201:
    case 204:
    case 205:
    case 206:
    case 207:
    case 208:
    case 212:
    case 213:
    case 214:
    case 215:
    case 216:
    case 218:
    case 233:
    case 234:
    case 236:
    case 239:
    case 240:
    case 243:
    case 244:
    case 247:
    case 248:
    case 251:
    case 252:
    case 255:
    case 256:
    case 259:
    case 260:
    case 322:
    case 323:
    case 324:
    case 334:
    case 339:
    case 341:
      return MODE_DI;

    case 117:
    case 124:
    case 130:
    case 131:
    case 178:
    case 179:
    case 332:
    case 337:
      return MODE_HI;

    case 118:
    case 180:
    case 181:
    case 331:
      return MODE_QI;

    case 168:
    case 190:
    case 191:
    case 192:
    case 193:
    case 194:
    case 219:
    case 220:
    case 221:
    case 222:
    case 223:
    case 224:
    case 225:
    case 226:
    case 227:
    case 228:
    case 229:
    case 230:
    case 279:
    case 280:
    case 281:
    case 282:
    case 283:
    case 284:
    case 285:
    case 286:
    case 287:
    case 288:
    case 289:
    case 290:
    case 291:
    case 292:
    case 296:
    case 297:
    case 298:
    case 299:
    case 300:
    case 301:
    case 302:
    case 303:
    case 304:
    case 305:
    case 306:
    case 307:
    case 308:
    case 309:
    case 310:
    case 311:
    case 312:
    case 313:
    case 318:
    case 529:
    case 530:
    case 531:
    case 532:
      return MODE_NONE;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 0:
    case 1:
    case 71:
    case 72:
    case 157:
    case 293:
    case 294:
    case 295:
    case 314:
    case 315:
    case 316:
    case 317:
      return MODE_UNKNOWN;

    default:
      return MODE_SI;

    }
}

extern enum attr_type get_attr_type PARAMS ((rtx));
enum attr_type
get_attr_type (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 187:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 6)
        {
	  return TYPE_LOAD;
        }
      else
        {
	  return TYPE_MOVE;
        }

    case 186:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 8) || (which_alternative == 9))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_MOVE;
        }

    case 188:
    case 184:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 183:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 182:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 185:
    case 169:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 166:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 12) || (which_alternative == 13))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 14) || (which_alternative == 15))
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || (which_alternative == 19))))
        {
	  return TYPE_HILO;
        }
      else if (which_alternative == 20)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 21) || (which_alternative == 22))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 23)
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 24)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 167:
    case 164:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 8) || (which_alternative == 9))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 163:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 7) || (which_alternative == 8))
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return TYPE_HILO;
        }
      else if (which_alternative == 13)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 14) || (which_alternative == 15))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 16)
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 17)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 162:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 7)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 12) || (which_alternative == 13))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17))))
        {
	  return TYPE_HILO;
        }
      else if (which_alternative == 18)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 19) || (which_alternative == 20))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 21)
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 22)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 181:
    case 179:
    case 161:
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 7) || (which_alternative == 8))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 160:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))
        {
	  return TYPE_HILO;
        }
      else if (which_alternative == 9)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 10) || (which_alternative == 11))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 12)
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 13)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 134:
    case 132:
    case 130:
    case 128:
    case 126:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_ARITH;
        }
      else
        {
	  return TYPE_LOAD;
        }

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if (which_alternative != 2)
        {
	  return TYPE_IMADD;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 38:
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return TYPE_IMADD;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_MULTI;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 168:
    case 318:
      return TYPE_NOP;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 85:
    case 86:
    case 89:
    case 90:
    case 191:
    case 193:
      return TYPE_MULTI;

    case 83:
    case 84:
      return TYPE_FRSQRT;

    case 81:
    case 82:
      return TYPE_FSQRT;

    case 115:
    case 142:
    case 143:
    case 144:
    case 145:
    case 146:
    case 147:
    case 148:
    case 149:
    case 150:
    case 151:
    case 152:
      return TYPE_FCVT;

    case 261:
    case 262:
    case 263:
    case 264:
    case 265:
    case 266:
    case 267:
    case 268:
    case 269:
    case 270:
    case 271:
    case 272:
    case 273:
    case 274:
    case 275:
    case 276:
    case 277:
    case 278:
      return TYPE_FCMP;

    case 94:
    case 95:
      return TYPE_FNEG;

    case 87:
    case 88:
      return TYPE_FABS;

    case 63:
    case 64:
    case 65:
    case 66:
      return TYPE_FDIV;

    case 55:
    case 56:
    case 57:
    case 58:
    case 59:
    case 60:
    case 61:
    case 62:
      return TYPE_FMADD;

    case 30:
    case 31:
    case 32:
    case 33:
      return TYPE_FMUL;

    case 2:
    case 3:
    case 16:
    case 17:
      return TYPE_FADD;

    case 67:
    case 68:
    case 69:
    case 70:
    case 73:
    case 74:
    case 75:
    case 76:
    case 77:
    case 78:
    case 79:
    case 80:
      return TYPE_IDIV;

    case 46:
    case 52:
    case 53:
    case 54:
      return TYPE_IMADD;

    case 34:
    case 35:
    case 36:
    case 39:
    case 41:
    case 42:
    case 43:
    case 44:
    case 45:
    case 47:
    case 48:
    case 49:
    case 50:
    case 51:
      return TYPE_IMUL;

    case 8:
    case 9:
    case 10:
    case 22:
    case 23:
    case 24:
    case 92:
    case 97:
    case 100:
    case 101:
    case 105:
    case 106:
    case 109:
    case 110:
    case 114:
    case 116:
    case 117:
    case 118:
    case 119:
    case 120:
    case 121:
    case 122:
    case 123:
    case 124:
    case 197:
    case 198:
    case 199:
    case 204:
    case 205:
    case 206:
    case 212:
    case 213:
    case 214:
      return TYPE_DARITH;

    case 157:
    case 189:
    case 319:
    case 320:
    case 321:
    case 322:
    case 323:
    case 324:
    case 325:
    case 326:
    case 327:
    case 328:
    case 329:
    case 330:
      return TYPE_MOVE;

    case 154:
    case 156:
    case 159:
    case 165:
    case 174:
    case 175:
    case 176:
    case 177:
    case 190:
    case 192:
    case 194:
      return TYPE_STORE;

    case 125:
    case 127:
    case 129:
    case 131:
    case 133:
    case 135:
    case 136:
    case 137:
    case 138:
    case 139:
    case 140:
    case 141:
    case 153:
    case 155:
    case 170:
    case 171:
    case 172:
    case 173:
    case 211:
    case 295:
    case 314:
    case 315:
    case 316:
    case 317:
      return TYPE_LOAD;

    case 292:
    case 296:
    case 297:
    case 298:
    case 299:
    case 300:
    case 301:
    case 302:
    case 303:
    case 304:
    case 305:
    case 306:
    case 307:
    case 308:
    case 309:
    case 310:
    case 311:
    case 312:
    case 313:
      return TYPE_CALL;

    case 279:
    case 281:
    case 282:
    case 283:
    case 284:
    case 285:
    case 286:
    case 287:
    case 288:
    case 290:
    case 291:
      return TYPE_JUMP;

    case 219:
    case 220:
    case 221:
    case 222:
    case 223:
    case 224:
    case 225:
    case 226:
    case 227:
    case 228:
    case 229:
    case 230:
    case 280:
    case 529:
    case 530:
    case 531:
    case 532:
      return TYPE_BRANCH;

    case 0:
    case 1:
    case 71:
    case 72:
    case 289:
    case 293:
    case 294:
    case 331:
    case 332:
    case 333:
    case 334:
    case 335:
    case 336:
    case 337:
    case 338:
    case 339:
      return TYPE_UNKNOWN;

    default:
      return TYPE_ARITH;

    }
}

int
eligible_for_delay (delay_insn, slot, candidate_insn, flags)
     rtx delay_insn ATTRIBUTE_UNUSED;
     int slot;
     rtx candidate_insn;
     int flags ATTRIBUTE_UNUSED;
{
  rtx insn;

  if (slot >= 1)
    abort ();

  insn = delay_insn;
  switch (recog_memoized (insn))
    {
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  slot += 3 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 279:
      slot += 2 * 1;
      break;
      break;

    case 532:
    case 531:
    case 530:
    case 529:
    case 280:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  slot += 1 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      slot += 0 * 1;
      break;
      break;

    }

  if (slot < 1)
    abort ();

  insn = candidate_insn;
  switch (slot)
    {
    case 3:
      switch (recog_memoized (insn))
	{
        case 260:
        case 258:
        case 248:
        case 246:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 216:
        case 210:
        case 208:
        case 203:
        case 201:
        case 196:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 187:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 5) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 7) && ((mips16) == (0))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 186:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 185:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 184:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 183:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 182:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 181:
        case 179:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 180:
        case 178:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 317:
        case 316:
        case 315:
        case 314:
        case 173:
        case 172:
        case 171:
        case 170:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 167:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 166:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 16) && ((which_alternative != 17) && ((which_alternative != 18) && (which_alternative != 19))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || ((which_alternative == 23) || (which_alternative == 24)))))))))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 164:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 163:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 162:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 16) && (which_alternative != 17))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 134:
        case 132:
        case 130:
        case 128:
        case 126:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && (which_alternative != 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 141:
        case 140:
        case 139:
        case 138:
        case 137:
        case 136:
        case 135:
        case 133:
        case 131:
        case 129:
        case 127:
        case 125:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 118:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 117:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 114:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 109:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 256:
        case 254:
        case 244:
        case 242:
        case 111:
        case 108:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 106:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 105:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 101:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 100:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 97:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((GENERATE_MULT3_DI) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
        case 38:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 40:
        case 37:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 26:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 20:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 25:
        case 19:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nsimm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 12:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 532:
        case 531:
        case 530:
        case 529:
        case 341:
        case 340:
        case 339:
        case 338:
        case 337:
        case 336:
        case 335:
        case 334:
        case 333:
        case 332:
        case 331:
        case 313:
        case 312:
        case 311:
        case 310:
        case 309:
        case 308:
        case 307:
        case 306:
        case 305:
        case 304:
        case 303:
        case 302:
        case 301:
        case 300:
        case 299:
        case 298:
        case 297:
        case 296:
        case 295:
        case 292:
        case 291:
        case 290:
        case 289:
        case 288:
        case 287:
        case 286:
        case 285:
        case 284:
        case 283:
        case 282:
        case 281:
        case 280:
        case 279:
        case 278:
        case 277:
        case 276:
        case 275:
        case 274:
        case 273:
        case 272:
        case 271:
        case 270:
        case 269:
        case 268:
        case 267:
        case 266:
        case 265:
        case 264:
        case 263:
        case 262:
        case 261:
        case 230:
        case 229:
        case 228:
        case 227:
        case 226:
        case 225:
        case 224:
        case 223:
        case 222:
        case 221:
        case 220:
        case 219:
        case 214:
        case 213:
        case 212:
        case 211:
        case 206:
        case 205:
        case 204:
        case 199:
        case 198:
        case 197:
        case 193:
        case 192:
        case 191:
        case 190:
        case 189:
        case 188:
        case 168:
        case 161:
        case 160:
        case 156:
        case 155:
        case 154:
        case 153:
        case 146:
        case 144:
        case 121:
        case 120:
        case 119:
        case 116:
        case 110:
        case 92:
        case 90:
        case 89:
        case 86:
        case 85:
        case 72:
        case 71:
        case 36:
        case 33:
        case 31:
        case 23:
        case 22:
        case 9:
        case 8:
	  return 0;

        case 29:
        case 27:
        case 21:
        case 15:
        case 13:
        case 7:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (get_attr_length (insn) == 4))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 6:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 11:
        case 5:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_simm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  extract_constrain_insn_cached (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

      }
    case 2:
      switch (recog_memoized (insn))
	{
        case 260:
        case 258:
        case 248:
        case 246:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 216:
        case 210:
        case 208:
        case 203:
        case 201:
        case 196:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 187:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 5) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 7) && ((mips16) == (0))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 186:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 185:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 184:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 183:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 182:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 181:
        case 179:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 180:
        case 178:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 317:
        case 316:
        case 315:
        case 314:
        case 173:
        case 172:
        case 171:
        case 170:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 167:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 166:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 16) && ((which_alternative != 17) && ((which_alternative != 18) && (which_alternative != 19))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || ((which_alternative == 23) || (which_alternative == 24)))))))))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 164:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 163:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 162:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 16) && (which_alternative != 17))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 134:
        case 132:
        case 130:
        case 128:
        case 126:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && (which_alternative != 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 141:
        case 140:
        case 139:
        case 138:
        case 137:
        case 136:
        case 135:
        case 133:
        case 131:
        case 129:
        case 127:
        case 125:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 118:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 117:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 114:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 109:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 256:
        case 254:
        case 244:
        case 242:
        case 111:
        case 108:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 106:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 105:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 101:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 100:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 97:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((GENERATE_MULT3_DI) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
        case 38:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 40:
        case 37:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 26:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 20:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 25:
        case 19:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nsimm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 12:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 532:
        case 531:
        case 530:
        case 529:
        case 341:
        case 340:
        case 339:
        case 338:
        case 337:
        case 336:
        case 335:
        case 334:
        case 333:
        case 332:
        case 331:
        case 313:
        case 312:
        case 311:
        case 310:
        case 309:
        case 308:
        case 307:
        case 306:
        case 305:
        case 304:
        case 303:
        case 302:
        case 301:
        case 300:
        case 299:
        case 298:
        case 297:
        case 296:
        case 295:
        case 292:
        case 291:
        case 290:
        case 289:
        case 288:
        case 287:
        case 286:
        case 285:
        case 284:
        case 283:
        case 282:
        case 281:
        case 280:
        case 279:
        case 278:
        case 277:
        case 276:
        case 275:
        case 274:
        case 273:
        case 272:
        case 271:
        case 270:
        case 269:
        case 268:
        case 267:
        case 266:
        case 265:
        case 264:
        case 263:
        case 262:
        case 261:
        case 230:
        case 229:
        case 228:
        case 227:
        case 226:
        case 225:
        case 224:
        case 223:
        case 222:
        case 221:
        case 220:
        case 219:
        case 214:
        case 213:
        case 212:
        case 211:
        case 206:
        case 205:
        case 204:
        case 199:
        case 198:
        case 197:
        case 193:
        case 192:
        case 191:
        case 190:
        case 189:
        case 188:
        case 168:
        case 161:
        case 160:
        case 156:
        case 155:
        case 154:
        case 153:
        case 146:
        case 144:
        case 121:
        case 120:
        case 119:
        case 116:
        case 110:
        case 92:
        case 90:
        case 89:
        case 86:
        case 85:
        case 72:
        case 71:
        case 36:
        case 33:
        case 31:
        case 23:
        case 22:
        case 9:
        case 8:
	  return 0;

        case 29:
        case 27:
        case 21:
        case 15:
        case 13:
        case 7:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (get_attr_length (insn) == 4))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 6:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 11:
        case 5:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_simm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  extract_constrain_insn_cached (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

      }
    case 1:
      switch (recog_memoized (insn))
	{
        case 260:
        case 258:
        case 248:
        case 246:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 216:
        case 210:
        case 208:
        case 203:
        case 201:
        case 196:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 187:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 5) && (((which_alternative != 1) && ((which_alternative != 2) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 7) && ((mips16) == (0))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 186:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 185:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 184:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 3) && (which_alternative != 4))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 183:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 182:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 181:
        case 179:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 180:
        case 178:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 317:
        case 316:
        case 315:
        case 314:
        case 173:
        case 172:
        case 171:
        case 170:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || (which_alternative == 1)) || (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))) && (((mips16) == (0)) && ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 167:
	  extract_constrain_insn_cached (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 166:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 16) && ((which_alternative != 17) && ((which_alternative != 18) && (which_alternative != 19))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || ((which_alternative == 23) || (which_alternative == 24)))))))))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 164:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 163:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 162:
	  extract_constrain_insn_cached (insn);
	  if (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 16) && (which_alternative != 17))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((mips16) == (0)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 134:
        case 132:
        case 130:
        case 128:
        case 126:
	  extract_constrain_insn_cached (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (((mips16) == (0)) && (which_alternative != 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 141:
        case 140:
        case 139:
        case 138:
        case 137:
        case 136:
        case 135:
        case 133:
        case 131:
        case 129:
        case 127:
        case 125:
	  extract_constrain_insn_cached (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (((mips16) == (0)) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 118:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 117:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 114:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 109:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 256:
        case 254:
        case 244:
        case 242:
        case 111:
        case 108:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 106:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 105:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 101:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 100:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 97:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && ((GENERATE_MULT3_DI) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
        case 38:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 40:
        case 37:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 26:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 20:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nuimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 25:
        case 19:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_nsimm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 12:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 532:
        case 531:
        case 530:
        case 529:
        case 341:
        case 340:
        case 339:
        case 338:
        case 337:
        case 336:
        case 335:
        case 334:
        case 333:
        case 332:
        case 331:
        case 313:
        case 312:
        case 311:
        case 310:
        case 309:
        case 308:
        case 307:
        case 306:
        case 305:
        case 304:
        case 303:
        case 302:
        case 301:
        case 300:
        case 299:
        case 298:
        case 297:
        case 296:
        case 295:
        case 292:
        case 291:
        case 290:
        case 289:
        case 288:
        case 287:
        case 286:
        case 285:
        case 284:
        case 283:
        case 282:
        case 281:
        case 280:
        case 279:
        case 278:
        case 277:
        case 276:
        case 275:
        case 274:
        case 273:
        case 272:
        case 271:
        case 270:
        case 269:
        case 268:
        case 267:
        case 266:
        case 265:
        case 264:
        case 263:
        case 262:
        case 261:
        case 230:
        case 229:
        case 228:
        case 227:
        case 226:
        case 225:
        case 224:
        case 223:
        case 222:
        case 221:
        case 220:
        case 219:
        case 214:
        case 213:
        case 212:
        case 211:
        case 206:
        case 205:
        case 204:
        case 199:
        case 198:
        case 197:
        case 193:
        case 192:
        case 191:
        case 190:
        case 189:
        case 188:
        case 168:
        case 161:
        case 160:
        case 156:
        case 155:
        case 154:
        case 153:
        case 146:
        case 144:
        case 121:
        case 120:
        case 119:
        case 116:
        case 110:
        case 92:
        case 90:
        case 89:
        case 86:
        case 85:
        case 72:
        case 71:
        case 36:
        case 33:
        case 31:
        case 23:
        case 22:
        case 9:
        case 8:
	  return 0;

        case 29:
        case 27:
        case 21:
        case 15:
        case 13:
        case 7:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (get_attr_length (insn) == 4))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 6:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_uimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 11:
        case 5:
	  extract_constrain_insn_cached (insn);
	  if (((mips16) == (0)) && (m16_simm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  extract_constrain_insn_cached (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

      }
    default:
      abort ();
    }
}

int
eligible_for_annul_false (delay_insn, slot, candidate_insn, flags)
     rtx delay_insn ATTRIBUTE_UNUSED;
     int slot;
     rtx candidate_insn;
     int flags ATTRIBUTE_UNUSED;
{
  rtx insn;

  if (slot >= 1)
    abort ();

  insn = delay_insn;
  switch (recog_memoized (insn))
    {
    case 313:
    case 312:
    case 311:
    case 310:
    case 309:
    case 308:
    case 307:
    case 306:
    case 305:
    case 304:
    case 303:
    case 302:
    case 301:
    case 300:
    case 299:
    case 298:
    case 297:
    case 296:
    case 292:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  slot += 3 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case 291:
    case 290:
    case 288:
    case 287:
    case 286:
    case 285:
    case 284:
    case 283:
    case 282:
    case 281:
    case 279:
      slot += 2 * 1;
      break;
      break;

    case 532:
    case 531:
    case 530:
    case 529:
    case 280:
    case 230:
    case 229:
    case 228:
    case 227:
    case 226:
    case 225:
    case 224:
    case 223:
    case 222:
    case 221:
    case 220:
    case 219:
      extract_constrain_insn_cached (insn);
      if ((mips16) == (0))
        {
	  slot += 1 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      slot += 0 * 1;
      break;
      break;

    }

  if (slot < 1)
    abort ();

  insn = candidate_insn;
  switch (slot)
    {
    case 3:
      switch (recog_memoized (insn))
	{
        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 0;

      }
    case 2:
      switch (recog_memoized (insn))
	{
        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 0;

      }
    case 1:
      switch (recog_memoized (insn))
	{
        case 260:
        case 258:
        case 248:
        case 246:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 216:
        case 210:
        case 208:
        case 203:
        case 201:
        case 196:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 187:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative == 7))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 186:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 184:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) || (((which_alternative == 1) || (which_alternative == 2)) || (((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5)))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 183:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 182:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 181:
        case 179:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative != 9) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 180:
        case 178:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 317:
        case 316:
        case 315:
        case 314:
        case 173:
        case 172:
        case 171:
        case 170:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))) && (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 167:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative == 3) || (((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 166:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 20) && (which_alternative != 23)))))) && ((which_alternative != 16) && ((which_alternative != 17) && ((which_alternative != 18) && (which_alternative != 19))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 21) && (which_alternative != 22)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || ((which_alternative == 18) || ((which_alternative == 19) || ((which_alternative == 20) || ((which_alternative == 21) || ((which_alternative == 23) || (which_alternative == 24)))))))))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((unsigned HOST_WIDE_INT) INTVAL (operands[0])) < (1024)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 164:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative != 10) && ((((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (get_attr_length (insn) == 4)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 163:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 13) && (which_alternative != 16)))) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 14) && (which_alternative != 15)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 162:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 8) && ((which_alternative != 11) && ((which_alternative != 18) && (which_alternative != 21)))) && ((which_alternative != 14) && ((which_alternative != 15) && ((which_alternative != 16) && (which_alternative != 17))))) && (((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 19) && (which_alternative != 20)))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 3) || ((which_alternative == 5) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 14) || ((which_alternative == 15) || (which_alternative == 16))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 134:
        case 132:
        case 130:
        case 128:
        case 126:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (which_alternative != 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 141:
        case 140:
        case 139:
        case 138:
        case 137:
        case 136:
        case 135:
        case 133:
        case 131:
        case 129:
        case 127:
        case 125:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 118:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 117:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 114:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 109:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 256:
        case 254:
        case 244:
        case 242:
        case 111:
        case 108:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 106:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 105:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 101:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 100:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 97:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((GENERATE_MULT3_DI) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 185:
        case 159:
        case 38:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 40:
        case 37:
	  extract_constrain_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 26:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nuimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 20:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nuimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 25:
        case 19:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nsimm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 12:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 532:
        case 531:
        case 530:
        case 529:
        case 341:
        case 340:
        case 339:
        case 338:
        case 337:
        case 336:
        case 335:
        case 334:
        case 333:
        case 332:
        case 331:
        case 313:
        case 312:
        case 311:
        case 310:
        case 309:
        case 308:
        case 307:
        case 306:
        case 305:
        case 304:
        case 303:
        case 302:
        case 301:
        case 300:
        case 299:
        case 298:
        case 297:
        case 296:
        case 295:
        case 292:
        case 291:
        case 290:
        case 289:
        case 288:
        case 287:
        case 286:
        case 285:
        case 284:
        case 283:
        case 282:
        case 281:
        case 280:
        case 279:
        case 278:
        case 277:
        case 276:
        case 275:
        case 274:
        case 273:
        case 272:
        case 271:
        case 270:
        case 269:
        case 268:
        case 267:
        case 266:
        case 265:
        case 264:
        case 263:
        case 262:
        case 261:
        case 230:
        case 229:
        case 228:
        case 227:
        case 226:
        case 225:
        case 224:
        case 223:
        case 222:
        case 221:
        case 220:
        case 219:
        case 214:
        case 213:
        case 212:
        case 211:
        case 206:
        case 205:
        case 204:
        case 199:
        case 198:
        case 197:
        case 193:
        case 192:
        case 191:
        case 190:
        case 189:
        case 188:
        case 161:
        case 160:
        case 156:
        case 155:
        case 154:
        case 153:
        case 146:
        case 144:
        case 121:
        case 120:
        case 119:
        case 116:
        case 110:
        case 92:
        case 90:
        case 89:
        case 86:
        case 85:
        case 72:
        case 71:
        case 36:
        case 33:
        case 31:
        case 23:
        case 22:
        case 9:
        case 8:
	  return 0;

        case 29:
        case 27:
        case 21:
        case 15:
        case 13:
        case 7:
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (get_attr_length (insn) == 4))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 6:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 11:
        case 5:
	  extract_insn_cached (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_simm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  if (((GENERATE_BRANCHLIKELY) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

      }
    default:
      abort ();
    }
}

static int imuldiv_unit_blockage PARAMS ((rtx, rtx));
static int
imuldiv_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 167:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 166:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 164:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 163:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 162:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 161:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 160:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 95:
    case 94:
    case 88:
    case 87:
      casenum = 23 /* 0x17 */;
      break;

    case 80:
    case 78:
    case 76:
    case 74:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 13 /* 0xd */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 13 /* 0xd */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 51:
    case 50:
    case 42:
    case 41:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 7;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 9;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 10 /* 0xa */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 38:
      extract_constrain_insn_cached (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 54:
    case 53:
    case 52:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 33:
    case 32:
      casenum = 24 /* 0x18 */;
      break;

    case 31:
    case 30:
      casenum = 25 /* 0x19 */;
      break;

    case 17:
    case 16:
    case 3:
    case 2:
      casenum = 22 /* 0x16 */;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 27 /* 0x1b */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 3;

    case 1:
      return 17 /* 0x11 */;

    case 2:
      return 5;

    case 3:
      return 12 /* 0xc */;

    case 4:
      return 10 /* 0xa */;

    case 5:
      return 4;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 5;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 38 /* 0x26 */;

    case 12:
      return 35 /* 0x23 */;

    case 13:
      return 42 /* 0x2a */;

    case 14:
      return 36 /* 0x24 */;

    case 15:
      return 69 /* 0x45 */;

    case 16:
      return 35 /* 0x23 */;

    case 17:
      return 67 /* 0x43 */;

    case 18:
      return 37 /* 0x25 */;

    case 19:
      return 69 /* 0x45 */;

    case 20:
      return 36 /* 0x24 */;

    case 21:
      return 68 /* 0x44 */;

    case 22:
      return 3;

    case 23:
      return 1;

    case 24:
      return 5;

    case 25:
      return 8;

    case 27:
      return 58 /* 0x3a */;

    default:
      abort ();
    }
}

static int imuldiv_unit_conflict_cost PARAMS ((rtx, rtx));
static int
imuldiv_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 181:
    case 179:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 180:
    case 178:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 167:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 166:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 164:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 163:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 162:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 161:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 160:
      extract_constrain_insn_cached (insn);
      casenum = 0;
      break;

    case 278:
    case 277:
    case 276:
    case 275:
    case 274:
    case 273:
    case 272:
    case 271:
    case 270:
    case 269:
    case 268:
    case 267:
    case 266:
    case 265:
    case 264:
    case 263:
    case 262:
    case 261:
    case 95:
    case 94:
    case 88:
    case 87:
      casenum = 23 /* 0x17 */;
      break;

    case 80:
    case 78:
    case 76:
    case 74:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 13 /* 0xd */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 79:
    case 77:
    case 75:
    case 73:
    case 70:
    case 69:
    case 68:
    case 67:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 11 /* 0xb */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 12 /* 0xc */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 13 /* 0xd */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 51:
    case 50:
    case 42:
    case 41:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 7;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 9;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 10 /* 0xa */;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 38:
      extract_constrain_insn_cached (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 40:
    case 37:
      extract_constrain_insn_cached (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 54:
    case 53:
    case 52:
    case 49:
    case 48:
    case 47:
    case 46:
    case 45:
    case 44:
    case 43:
    case 39:
    case 36:
    case 35:
    case 34:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4120)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) || (((mips_cpu_attr) == (CPU_R4120))))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else
        {
	  casenum = 27 /* 0x1b */;
        }
      break;

    case 33:
    case 32:
      casenum = 24 /* 0x18 */;
      break;

    case 31:
    case 30:
      casenum = 25 /* 0x19 */;
      break;

    case 17:
    case 16:
    case 3:
    case 2:
      casenum = 22 /* 0x16 */;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 27 /* 0x1b */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 3;

    case 1:
      return 17 /* 0x11 */;

    case 2:
      return 5;

    case 3:
      return 12 /* 0xc */;

    case 4:
      return 10 /* 0xa */;

    case 5:
      return 4;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 5;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 38 /* 0x26 */;

    case 12:
      return 35 /* 0x23 */;

    case 13:
      return 42 /* 0x2a */;

    case 14:
      return 36 /* 0x24 */;

    case 15:
      return 69 /* 0x45 */;

    case 16:
      return 35 /* 0x23 */;

    case 17:
      return 67 /* 0x43 */;

    case 18:
      return 37 /* 0x25 */;

    case 19:
      return 69 /* 0x45 */;

    case 20:
      return 36 /* 0x24 */;

    case 21:
      return 68 /* 0x44 */;

    case 22:
      return 3;

    case 23:
      return 1;

    case 24:
      return 5;

    case 25:
      return 8;

    case 27:
      return 58 /* 0x3a */;

    default:
      abort ();
    }
}

const struct function_unit_desc function_units[] = {
  {"memory", 1, 1, 0, 1, 1, memory_unit_ready_cost, 0, 1, 0, 0}, 
  {"imuldiv", 2, 1, 0, 0, 69, imuldiv_unit_ready_cost, imuldiv_unit_conflict_cost, 69, imuldiv_unit_blockage_range, imuldiv_unit_blockage}, 
  {"adder", 4, 1, 1, 1, 1, adder_unit_ready_cost, 0, 1, 0, 0}, 
  {"mult", 8, 1, 1, 1, 1, mult_unit_ready_cost, 0, 1, 0, 0}, 
  {"divide", 16, 1, 1, 1, 1, divide_unit_ready_cost, 0, 1, 0, 0}, 
};


int max_dfa_issue_rate = 6;
/* Vector translating external insn codes to internal ones.*/
static const unsigned char vr54_translate[] ATTRIBUTE_UNUSED = {
    0,     1,     2,     2,     2,     1,     1,     1,     1,     1,
    1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     0,     4,     4,     4,
    4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
    4,     4,     4,     4,     4,     5};

/* Vector for state transitions.  */
static const unsigned char vr54_transitions[] ATTRIBUTE_UNUSED = {
    4,    10,     7,     1,     0,     0,    12,     5,     2,    12,
    1,     0,    12,     3,    12,    12,     2,     0,    12,     4,
   12,    12,     3,     0,    12,    12,    12,    12,     4,     0,
   12,     6,     3,    12,     5,     0,    12,    12,     4,    12,
    6,     0,    12,     8,    12,     2,     7,     0,    12,     9,
   12,     3,     8,     0,    12,    12,    12,     4,     9,     0,
   12,    11,     8,     5,    10,     0,    12,    12,     9,     6,
   11,     0};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char vr54_state_alts[] ATTRIBUTE_UNUSED = {
    1,     2,     1,     1,     1,     1,     0,     2,     1,     0,
    1,     1,     0,     2,     0,     0,     1,     1,     0,     1,
    0,     0,     1,     1,     0,     0,     0,     0,     1,     1,
    0,     1,     1,     0,     1,     1,     0,     0,     1,     0,
    1,     1,     0,     2,     0,     1,     1,     1,     0,     1,
    0,     1,     1,     1,     0,     0,     0,     1,     1,     1,
    0,     1,     1,     1,     1,     1,     0,     0,     1,     1,
    1,     1};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char vr54_min_issue_delay[] ATTRIBUTE_UNUSED = {
    2,    75,    44,   242,    77,    40,   163,   136,    48};

/* Vector for locked state flags.  */
static const unsigned char vr54_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char vr55_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     1,     2,     3,
    3,     4,     4,     4,     4,     5,     5,     5,     5,     5,
    6,     5,     5,     5,     5,     5,     5,     6,     6,     6,
    6,     5,     5,     1,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     7};

/* Vector for state transitions.  */
static const unsigned char vr55_transitions[] ATTRIBUTE_UNUSED = {
    0,     6,    47,    45,    37,    25,     1,     0,     1,    48,
   24,    22,    14,     2,    48,     0,     2,    48,    13,    11,
    3,    48,    48,     0,     3,    48,    10,     8,     4,    48,
   48,     0,     4,    48,     7,     5,    48,    48,    48,     0,
    5,    48,     6,    48,    48,    48,    48,     0,     6,    48,
   48,    48,    48,    48,    48,     0,     7,    48,    48,     6,
   48,    48,    48,     0,     8,    48,     9,    48,     5,    48,
   48,     0,     9,    48,    48,    48,     6,    48,    48,     0,
   10,    48,    48,     9,     7,    48,    48,     0,    11,    48,
   12,    48,     8,    48,    48,     0,    12,    48,    48,    48,
    9,    48,    48,     0,    13,    48,    48,    12,    10,    48,
   48,     0,    14,    48,    21,    19,    15,     3,    48,     0,
   15,    48,    18,    16,    48,     4,    48,     0,    16,    48,
   17,    48,    48,     5,    48,     0,    17,    48,    48,    48,
   48,     6,    48,     0,    18,    48,    48,    17,    48,     7,
   48,     0,    19,    48,    20,    48,    16,     8,    48,     0,
   20,    48,    48,    48,    17,     9,    48,     0,    21,    48,
   48,    20,    18,    10,    48,     0,    22,    48,    23,    48,
   19,    11,    48,     0,    23,    48,    48,    48,    20,    12,
   48,     0,    24,    48,    48,    23,    21,    13,    48,     0,
   25,    48,    36,    34,    26,    48,     2,     0,    26,    48,
   33,    31,    27,    48,     3,     0,    27,    48,    30,    28,
   48,    48,     4,     0,    28,    48,    29,    48,    48,    48,
    5,     0,    29,    48,    48,    48,    48,    48,     6,     0,
   30,    48,    48,    29,    48,    48,     7,     0,    31,    48,
   32,    48,    28,    48,     8,     0,    32,    48,    48,    48,
   29,    48,     9,     0,    33,    48,    48,    32,    30,    48,
   10,     0,    34,    48,    35,    48,    31,    48,    11,     0,
   35,    48,    48,    48,    32,    48,    12,     0,    36,    48,
   48,    35,    33,    48,    13,     0,    37,    48,    44,    42,
   38,    26,    14,     0,    38,    48,    41,    39,    48,    27,
   15,     0,    39,    48,    40,    48,    48,    28,    16,     0,
   40,    48,    48,    48,    48,    29,    17,     0,    41,    48,
   48,    40,    48,    30,    18,     0,    42,    48,    43,    48,
   39,    31,    19,     0,    43,    48,    48,    48,    40,    32,
   20,     0,    44,    48,    48,    43,    41,    33,    21,     0,
   45,    48,    46,    48,    42,    34,    22,     0,    46,    48,
   48,    48,    43,    35,    23,     0,    47,    48,    48,    46,
   44,    36,    24,     0};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char vr55_state_alts[] ATTRIBUTE_UNUSED = {
    1,     1,     1,     1,     2,     1,     1,     1,     1,     0,
    1,     1,     2,     1,     0,     1,     1,     0,     1,     1,
    2,     0,     0,     1,     1,     0,     1,     1,     1,     0,
    0,     1,     1,     0,     1,     1,     0,     0,     0,     1,
    1,     0,     1,     0,     0,     0,     0,     1,     1,     0,
    0,     0,     0,     0,     0,     1,     1,     0,     0,     1,
    0,     0,     0,     1,     1,     0,     1,     0,     1,     0,
    0,     1,     1,     0,     0,     0,     1,     0,     0,     1,
    1,     0,     0,     1,     1,     0,     0,     1,     1,     0,
    1,     0,     2,     0,     0,     1,     1,     0,     0,     0,
    2,     0,     0,     1,     1,     0,     0,     1,     2,     0,
    0,     1,     1,     0,     1,     1,     1,     1,     0,     1,
    1,     0,     1,     1,     0,     1,     0,     1,     1,     0,
    1,     0,     0,     1,     0,     1,     1,     0,     0,     0,
    0,     1,     0,     1,     1,     0,     0,     1,     0,     1,
    0,     1,     1,     0,     1,     0,     1,     1,     0,     1,
    1,     0,     0,     0,     1,     1,     0,     1,     1,     0,
    0,     1,     1,     1,     0,     1,     1,     0,     1,     0,
    2,     1,     0,     1,     1,     0,     0,     0,     2,     1,
    0,     1,     1,     0,     0,     1,     2,     1,     0,     1,
    1,     0,     1,     1,     2,     0,     1,     1,     1,     0,
    1,     1,     1,     0,     1,     1,     1,     0,     1,     1,
    0,     0,     1,     1,     1,     0,     1,     0,     0,     0,
    1,     1,     1,     0,     0,     0,     0,     0,     1,     1,
    1,     0,     0,     1,     0,     0,     1,     1,     1,     0,
    1,     0,     1,     0,     1,     1,     1,     0,     0,     0,
    1,     0,     1,     1,     1,     0,     0,     1,     1,     0,
    1,     1,     1,     0,     1,     0,     2,     0,     1,     1,
    1,     0,     0,     0,     2,     0,     1,     1,     1,     0,
    0,     1,     2,     0,     1,     1,     1,     0,     1,     1,
    1,     1,     1,     1,     1,     0,     1,     1,     0,     1,
    1,     1,     1,     0,     1,     0,     0,     1,     1,     1,
    1,     0,     0,     0,     0,     1,     1,     1,     1,     0,
    0,     1,     0,     1,     1,     1,     1,     0,     1,     0,
    1,     1,     1,     1,     1,     0,     0,     0,     1,     1,
    1,     1,     1,     0,     0,     1,     1,     1,     1,     1,
    1,     0,     1,     0,     2,     1,     1,     1,     1,     0,
    0,     0,     2,     1,     1,     1,     1,     0,     0,     1,
    2,     1,     1,     1};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char vr55_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,    66,    70,    70,    78,    94,   126,   110,    86,   118,
  102,    86,   118,   102,    66,    74,    90,   122,   106,    82,
  114,    98,    82,   114,    98,    68,    68,    76,    92,   124,
  108,    84,   116,   100,    84,   116,   100,    64,    72,    88,
  120,   104,    80,   112,    96,    80,   112,    96};

/* Vector for locked state flags.  */
static const unsigned char vr55_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_cpu_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     1,     2,     2,     2,     3,     4,
    2,     4,     3,     2,     2,     2,     2,     2,     2,     2,
    5,     5,     5,     5,     5,     5,     5,     5,     5,     5,
    5,     5,     5,     1,     2,     6};

/* Vector for state transitions.  */
static const unsigned char sr71_cpu_transitions[] ATTRIBUTE_UNUSED = {
    0,    19,    12,   158,   155,     1,     0,     1,   160,    13,
  152,   154,   153,     2,     2,   160,    64,   152,   151,     3,
    0,     3,   160,    65,   160,   160,   160,     4,     4,   160,
   75,   150,    10,     5,     0,     5,   160,     6,   160,   160,
  160,     2,     6,   160,     7,   160,   160,   160,     2,     7,
  160,     8,   160,   160,   160,     2,     8,   160,     9,   160,
  160,   160,     2,     9,   160,   160,   160,   160,   160,     2,
   10,   160,    76,   160,   160,   160,    11,    11,   160,    73,
  149,   148,   147,    12,    12,   160,    45,   145,    42,    13,
    0,    13,   160,    39,    31,    23,    14,     2,    14,   160,
   20,   160,   160,   160,    15,    15,   160,    16,   160,   160,
  160,     0,    16,   160,    17,   160,   160,   160,     0,    17,
  160,    18,   160,   160,   160,     0,    18,   160,    19,   160,
  160,   160,     0,    19,   160,   160,   160,   160,   160,     0,
   20,   160,    21,   160,   160,   160,    15,    21,   160,    22,
  160,   160,   160,    15,    22,   160,   160,   160,   160,   160,
   15,    23,   160,    29,   160,   160,   160,    24,    24,   160,
   25,   160,   160,   160,    12,    25,   160,    26,   160,   160,
  160,    12,    26,   160,    27,   160,   160,   160,    12,    27,
  160,    28,   160,   160,   160,    12,    28,   160,   160,   160,
  160,   160,    12,    29,   160,    30,   160,   160,   160,    24,
   30,   160,   160,   160,   160,   160,    24,    31,   160,    32,
  160,   160,   160,    34,    32,   160,    33,   160,   160,   160,
   34,    33,   160,   160,   160,   160,   160,    34,    34,   160,
   35,   160,   160,   160,    13,    35,   160,    36,   160,   160,
  160,    13,    36,   160,    37,   160,   160,   160,    13,    37,
  160,    38,   160,   160,   160,    13,    38,   160,   160,   160,
  160,   160,    13,    39,   160,    40,    32,    29,    20,     2,
   40,   160,    41,    33,    30,    21,     2,    41,   160,   160,
  160,   160,    22,     2,    42,   160,    46,   144,    43,    23,
   11,    43,   160,    47,   160,   160,   160,    44,    44,   160,
  140,   160,   160,   160,    45,    45,   160,   138,    55,    46,
   39,     0,    46,   160,    54,    48,    47,    29,    11,    47,
  160,   160,   160,   160,   160,    44,    48,   160,   160,   160,
  160,   160,    49,    49,   160,    50,   160,   160,   160,    39,
   50,   160,    51,   160,   160,   160,    39,    51,   160,    52,
  160,   160,   160,    39,    52,   160,    53,   160,   160,   160,
   39,    53,   160,   160,   160,   160,   160,    39,    54,   160,
  160,   160,   160,    30,    11,    55,   160,   137,   131,    48,
   32,    56,    56,   160,    94,   130,    62,    57,    13,    57,
  160,    58,   160,   160,   160,     6,    58,   160,    59,   160,
  160,   160,     6,    59,   160,    60,   160,   160,   160,     6,
   60,   160,    61,   160,   160,   160,     6,    61,   160,   160,
  160,   160,   160,     6,    62,   160,    95,   160,   160,   160,
   63,    63,   160,   111,   160,   160,   160,    64,    64,   160,
  127,    31,    69,    65,     0,    65,   160,    66,   160,   160,
  160,     4,    66,   160,    67,   160,   160,   160,     4,    67,
  160,    68,   160,   160,   160,     4,    68,   160,   160,   160,
  160,   160,     4,    69,   160,   125,   160,   160,   160,    70,
   70,   160,    89,   124,    72,    71,    12,    71,   160,    63,
  160,   160,   160,    64,    72,   160,    90,   160,   160,   160,
   73,    73,   160,   121,   118,    88,    74,    12,    74,   160,
   85,   160,   160,   160,    75,    75,   160,    82,    79,    76,
    6,     0,    76,   160,    77,   160,   160,   160,    11,    77,
  160,    78,   160,   160,   160,    11,    78,   160,   160,   160,
  160,   160,    11,    79,   160,    80,   160,   160,   160,    56,
   80,   160,    81,   160,   160,   160,    56,    81,   160,   160,
  160,   160,   160,    56,    82,   160,    83,    80,    77,     7,
    0,    83,   160,    84,    81,    78,     8,     0,    84,   160,
  160,   160,   160,     9,     0,    85,   160,    86,   160,   160,
  160,    75,    86,   160,    87,   160,   160,   160,    75,    87,
  160,   160,   160,   160,   160,    75,    88,   160,   116,   160,
  160,   160,    89,    89,   160,   110,    93,    90,    63,    12,
   90,   160,    91,   160,   160,   160,    73,    91,   160,    92,
  160,   160,   160,    73,    92,   160,   160,   160,   160,   160,
   73,    93,   160,   108,   160,   160,   160,    94,    94,   160,
  105,    98,    95,    58,    13,    95,   160,    96,   160,   160,
  160,    63,    96,   160,    97,   160,   160,   160,    63,    97,
  160,   160,   160,   160,   160,    63,    98,   160,   103,   160,
  160,   160,    99,    99,   160,   100,   160,   160,   160,    65,
  100,   160,   101,   160,   160,   160,    65,   101,   160,   102,
  160,   160,   160,    65,   102,   160,   160,   160,   160,   160,
   65,   103,   160,   104,   160,   160,   160,    99,   104,   160,
  160,   160,   160,   160,    99,   105,   160,   106,   103,    96,
   59,    13,   106,   160,   107,   104,    97,    60,    13,   107,
  160,   160,   160,   160,    61,    13,   108,   160,   109,   160,
  160,   160,    94,   109,   160,   160,   160,   160,   160,    94,
  110,   160,   114,   108,    91,   111,    12,   111,   160,   112,
  160,   160,   160,    64,   112,   160,   113,   160,   160,   160,
   64,   113,   160,   160,   160,   160,   160,    64,   114,   160,
  115,   109,    92,   112,    12,   115,   160,   160,   160,   160,
  113,    12,   116,   160,   117,   160,   160,   160,    89,   117,
  160,   160,   160,   160,   160,    89,   118,   160,   119,   160,
  160,   160,    35,   119,   160,   120,   160,   160,   160,    35,
  120,   160,   160,   160,   160,   160,    35,   121,   160,   122,
  119,   116,    85,    12,   122,   160,   123,   120,   117,    86,
   12,   123,   160,   160,   160,   160,    87,    12,   124,   160,
   93,   160,   160,   160,    94,   125,   160,   126,   160,   160,
  160,    70,   126,   160,   160,   160,   160,   160,    70,   127,
  160,   128,    32,   125,    66,     0,   128,   160,   129,    33,
  126,    67,     0,   129,   160,   160,   160,   160,    68,     0,
  130,   160,    98,   160,   160,   160,    99,   131,   160,   160,
  160,   160,   160,   132,   132,   160,   133,   160,   160,   160,
   20,   133,   160,   134,   160,   160,   160,    20,   134,   160,
  135,   160,   160,   160,    20,   135,   160,   136,   160,   160,
  160,    20,   136,   160,   160,   160,   160,   160,    20,   137,
  160,   160,   160,   160,    33,    56,   138,   160,   139,   137,
   54,    40,     0,   139,   160,   160,   160,   160,    41,     0,
  140,   160,   141,   160,   160,   160,    45,   141,   160,   142,
  160,   160,   160,    45,   142,   160,   143,   160,   160,   160,
   45,   143,   160,   160,   160,   160,   160,    45,   144,   160,
   48,   160,   160,   160,    49,   145,   160,    55,   146,   144,
   31,    56,   146,   160,   131,   160,   160,   160,   132,   147,
  160,    74,   160,   160,   160,    75,   148,   160,    88,   160,
  160,   160,    89,   149,   160,   118,   160,   160,   160,    35,
  150,   160,    79,   160,   160,   160,    56,   151,   160,    69,
  160,   160,   160,    70,   152,   160,    31,   160,   160,   160,
   34,   153,   160,    14,   160,   160,   160,    15,   154,   160,
   23,   160,   160,   160,    24,   155,   160,    42,   157,   156,
  154,    11,   156,   160,    43,   160,   160,   160,    44,   157,
  160,   144,   160,   160,   160,    49,   158,   160,   145,   159,
  157,   152,    56,   159,   160,   146,   160,   160,   160,   132,
};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char sr71_cpu_state_alts[] ATTRIBUTE_UNUSED = {
    1,     1,     4,    64,    32,     2,     1,     1,     0,     4,
   32,    16,     1,     1,     1,     0,     4,    32,    16,     1,
    1,     1,     0,     4,     0,     0,     0,     1,     1,     0,
    4,    32,    16,     1,     1,     1,     0,     4,     0,     0,
    0,     1,     1,     0,     3,     0,     0,     0,     1,     1,
    0,     2,     0,     0,     0,     1,     1,     0,     1,     0,
    0,     0,     1,     1,     0,     0,     0,     0,     0,     1,
    1,     0,     3,     0,     0,     0,     1,     1,     0,     4,
   32,    16,     1,     1,     1,     0,     3,    48,    24,     2,
    1,     1,     0,     3,    24,    12,     1,     1,     1,     0,
    3,     0,     0,     0,     1,     1,     0,     4,     0,     0,
    0,     1,     1,     0,     3,     0,     0,     0,     1,     1,
    0,     2,     0,     0,     0,     1,     1,     0,     1,     0,
    0,     0,     1,     1,     0,     0,     0,     0,     0,     1,
    1,     0,     2,     0,     0,     0,     1,     1,     0,     1,
    0,     0,     0,     1,     1,     0,     0,     0,     0,     0,
    1,     1,     0,     2,     0,     0,     0,     1,     1,     0,
    4,     0,     0,     0,     1,     1,     0,     3,     0,     0,
    0,     1,     1,     0,     2,     0,     0,     0,     1,     1,
    0,     1,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     0,     1,     0,     0,     0,     1,
    1,     0,     0,     0,     0,     0,     1,     1,     0,     2,
    0,     0,     0,     1,     1,     0,     1,     0,     0,     0,
    1,     1,     0,     0,     0,     0,     0,     1,     1,     0,
    4,     0,     0,     0,     1,     1,     0,     3,     0,     0,
    0,     1,     1,     0,     2,     0,     0,     0,     1,     1,
    0,     1,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     0,     2,    16,     8,     1,     1,
    1,     0,     1,     8,     4,     1,     1,     1,     0,     0,
    0,     0,     1,     1,     1,     0,     2,    12,     6,     1,
    1,     1,     0,     1,     0,     0,     0,     1,     1,     0,
    4,     0,     0,     0,     1,     1,     0,     2,    32,    16,
    2,     1,     1,     0,     1,     6,     3,     1,     1,     1,
    0,     0,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     0,     4,     0,     0,     0,     1,
    1,     0,     3,     0,     0,     0,     1,     1,     0,     2,
    0,     0,     0,     1,     1,     0,     1,     0,     0,     0,
    1,     1,     0,     0,     0,     0,     0,     1,     1,     0,
    0,     0,     0,     1,     1,     1,     0,     1,     3,     3,
    1,     1,     1,     0,     4,    16,    16,     1,     1,     1,
    0,     4,     0,     0,     0,     1,     1,     0,     3,     0,
    0,     0,     1,     1,     0,     2,     0,     0,     0,     1,
    1,     0,     1,     0,     0,     0,     1,     1,     0,     0,
    0,     0,     0,     1,     1,     0,     3,     0,     0,     0,
    1,     1,     0,     3,     0,     0,     0,     1,     1,     0,
    3,    24,    12,     1,     1,     1,     0,     3,     0,     0,
    0,     1,     1,     0,     2,     0,     0,     0,     1,     1,
    0,     1,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     0,     2,     0,     0,     0,     1,
    1,     0,     4,    32,    16,     1,     1,     1,     0,     4,
    0,     0,     0,     1,     1,     0,     3,     0,     0,     0,
    1,     1,     0,     3,    24,    12,     1,     1,     1,     0,
    3,     0,     0,     0,     1,     1,     0,     3,    24,    12,
    1,     1,     1,     0,     2,     0,     0,     0,     1,     1,
    0,     1,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     0,     2,     0,     0,     0,     1,
    1,     0,     1,     0,     0,     0,     1,     1,     0,     0,
    0,     0,     0,     1,     1,     0,     2,    16,     8,     1,
    1,     1,     0,     1,     8,     4,     1,     1,     1,     0,
    0,     0,     0,     1,     1,     1,     0,     2,     0,     0,
    0,     1,     1,     0,     1,     0,     0,     0,     1,     1,
    0,     0,     0,     0,     0,     1,     1,     0,     2,     0,
    0,     0,     1,     1,     0,     3,    24,    12,     1,     1,
    1,     0,     2,     0,     0,     0,     1,     1,     0,     1,
    0,     0,     0,     1,     1,     0,     0,     0,     0,     0,
    1,     1,     0,     2,     0,     0,     0,     1,     1,     0,
    3,    12,    12,     1,     1,     1,     0,     2,     0,     0,
    0,     1,     1,     0,     1,     0,     0,     0,     1,     1,
    0,     0,     0,     0,     0,     1,     1,     0,     2,     0,
    0,     0,     1,     1,     0,     3,     0,     0,     0,     1,
    1,     0,     2,     0,     0,     0,     1,     1,     0,     1,
    0,     0,     0,     1,     1,     0,     0,     0,     0,     0,
    1,     1,     0,     1,     0,     0,     0,     1,     1,     0,
    0,     0,     0,     0,     1,     1,     0,     2,     8,     8,
    1,     1,     1,     0,     1,     4,     4,     1,     1,     1,
    0,     0,     0,     0,     1,     1,     1,     0,     1,     0,
    0,     0,     1,     1,     0,     0,     0,     0,     0,     1,
    1,     0,     2,    16,     8,     1,     1,     1,     0,     2,
    0,     0,     0,     1,     1,     0,     1,     0,     0,     0,
    1,     1,     0,     0,     0,     0,     0,     1,     1,     0,
    1,     8,     4,     1,     1,     1,     0,     0,     0,     0,
    1,     1,     1,     0,     1,     0,     0,     0,     1,     1,
    0,     0,     0,     0,     0,     1,     1,     0,     2,     0,
    0,     0,     1,     1,     0,     1,     0,     0,     0,     1,
    1,     0,     0,     0,     0,     0,     1,     1,     0,     2,
   16,     8,     1,     1,     1,     0,     1,     8,     4,     1,
    1,     1,     0,     0,     0,     0,     1,     1,     1,     0,
    3,     0,     0,     0,     1,     1,     0,     1,     0,     0,
    0,     1,     1,     0,     0,     0,     0,     0,     1,     1,
    0,     2,    16,     8,     1,     1,     1,     0,     1,     8,
    4,     1,     1,     1,     0,     0,     0,     0,     1,     1,
    1,     0,     3,     0,     0,     0,     1,     1,     0,     0,
    0,     0,     0,     1,     1,     0,     4,     0,     0,     0,
    1,     1,     0,     3,     0,     0,     0,     1,     1,     0,
    2,     0,     0,     0,     1,     1,     0,     1,     0,     0,
    0,     1,     1,     0,     0,     0,     0,     0,     1,     1,
    0,     0,     0,     0,     1,     1,     1,     0,     1,    16,
    8,     2,     1,     1,     0,     0,     0,     0,     2,     1,
    1,     0,     3,     0,     0,     0,     1,     1,     0,     2,
    0,     0,     0,     1,     1,     0,     1,     0,     0,     0,
    1,     1,     0,     0,     0,     0,     0,     1,     1,     0,
    1,     0,     0,     0,     1,     1,     0,     2,     6,     6,
    1,     1,     1,     0,     1,     0,     0,     0,     1,     1,
    0,     4,     0,     0,     0,     1,     1,     0,     3,     0,
    0,     0,     1,     1,     0,     3,     0,     0,     0,     1,
    1,     0,     3,     0,     0,     0,     1,     1,     0,     3,
    0,     0,     0,     1,     1,     0,     3,     0,     0,     0,
    1,     1,     0,     4,     0,     0,     0,     1,     1,     0,
    3,     0,     0,     0,     1,     1,     0,     3,    18,     9,
    1,     1,     1,     0,     2,     0,     0,     0,     1,     1,
    0,     2,     0,     0,     0,     1,     1,     0,     3,     9,
    9,     1,     1,     1,     0,     2,     0,     0,     0,     1,
};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_cpu_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,    32,     0,     0,     1,     0,     0,
    0,    32,    17,    16,     1,     0,     0,     0,    32,    17,
   16,     2,     1,    17,     0,    32,    17,    16,     2,     1,
   17,     0,    33,    17,    16,     3,     1,    17,     0,    32,
    0,     0,     1,     0,     0,     0,    32,     0,     0,     2,
    2,    34,     0,    16,    17,    16,     1,     1,    17,     0,
   16,    17,    16,     1,     1,    17,     0,    17,    17,    16,
    2,     2,    34,     0,    32,    34,    32,     2,    18,    34,
    0,    48,    34,    32,     2,     1,    17,     0,    32,    17,
   16,     2,     1,    17,     0,    32,    17,    16,     2,    17,
   17,     0,    48,    34,    32,     3,    18,    34,     0,    64,
   34,    32,     4,     2,    34,     0,    65,    34,    32,     3,
    1,    17,     0,    48,    17,    16,     3,     1,    17,     0,
   48,    17,    16,     3,    17,    17,     0,    32,     0,     0,
    2,     0,     0,     0,    33,    17,     0,     3,     0,     0,
    0,    48,    34,    32,     2,     1,    17,     0,    16,     0,
    0,     3,     0,     0,     0,    49,    34,    32,     4,    18,
   34,     0,    48,    17,    16,     3,     1,    17,     0,    48,
   17,    16,     3,     1,    17,     0,    49,    17,    16,     3,
   17,    16,     0,    64,     0,     0,     3,     0,     0,     0,
   48,    34,    32,     3,     2,    34,     0,    48,    34,    32,
    3,     2,    34,     0,    49,    34,    32,     3,     2,    34,
    0,    32,    17,    16,     1,     0,     0,     0,    32,    17,
   16,     2,     1,    17,     0,    32,    17,    16,     2,    17,
   17,     0,    48,    17,    16,     2,     0,     0,     0,    32,
   17,    16,     3,     1,    17,     0,    32,     0,     0,     2,
    1,    17,     0,    16,     0,     0,     3,     1,    17,     0,
   48,    17,    16,     3,    17,    17,     0,    64,    17,    16,
    4,     1,    17,     0,    65,    17,    16,     1,     0,     0,
    0,    16,     0,     0,     1,    17,    16,     0,    32,    17,
   16,     2,     1,    17,     0,    33,    17,    16,     3,     1,
   17,     0,    32,     0,     0,     3,     1,    17,     0,    48,
   17,    16,     3,    17,    17,     0,    64,    17,    16,     3,
    0,     0,     0,    48,    34,    32,     3,     2,    34,     0,
   49,    34,    32,     4,     3,    51,     0,    48,    34,    32,
    3,     2,    34,     0,    48,    34,    32,     3,    18,    34,
    0,    64,    51,    48,     4,    19,    51,     0,    48,     0,
    0,     3,     0,     0,     0,    49,    17,     0,     4,     1,
   17,     0,    65,    17,    16,     2,     0,     0,     0,    32,
   17,    16,     2,     1,    17,     0,    33,    17,    16,     2,
    0,     0,     0,    33,    17,     0,     3,     1,    17,     0,
   49,    17,    16,     4,     2,    34,     0,    64,    34,    32,
    4,    18,    34,     0,    32,     0,     0,     2,     0,     0,
    0,    33,    17,     0,     4,     1,    17,     0,    48,    17,
   16,     3,    17,    17,     0,    16,     0,     0,     1,     0,
    0,     0,    17,    17,     0,     4,     3,    51,     0,    65,
   68,    64,     3,     3,    51,     0,    48,    51,    48,     3,
    3,    51,     0,    48,    51,    48,     3,    19,    51,     0,
   65,    17,     0,     1,     0,     0,     0,    17,    17,     0,
    2,     1,    17,     0,    32,    17,    16,     2,     1,    17,
    0,    33,    17,    16,     4,     2,    34,     0,    64,     0,
    0,     4,     4,    68,     0,    32,    17,    16,     3,     1,
   17,     0,    64,    34,    32,     4,     1,    17,     0,    48,
   17,    16,     4,     2,    34,     0,    32,    34,    32,     3,
    2,    34,     0,    48,     0,     0,     3,     2,    34,     0,
   64,    34,    32,     4,     0,     0,     0,    64,    68,    64,
};

/* Vector for locked state flags.  */
static const unsigned char sr71_cpu_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_cpu1_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     1,     2,     2,     3,     3,
    4,     3,     3,     4,     4,     5,     5,     5,     5,     4,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     4,     6};

/* Vector for state transitions.  */
static const unsigned char sr71_cpu1_transitions[] ATTRIBUTE_UNUSED = {
    0,     1,    22,    75,    46,    78,     0,     1,    80,     2,
   70,    47,    73,     0,     2,    80,    80,     3,    40,    44,
    0,     3,    80,    80,    80,     4,    27,     6,     4,    80,
   80,    80,     5,    80,     6,     5,    80,    80,    80,    80,
   80,     6,     6,     7,    12,    65,    29,    68,    22,     7,
   80,     8,    60,    30,    63,    22,     8,    80,    80,     9,
   20,    57,    22,     9,    80,    80,    80,    10,    17,    12,
   10,    80,    80,    80,    11,    80,    12,    11,    80,    80,
   80,    80,    80,    12,    12,     8,    80,    13,    19,    56,
   22,    13,     9,    80,    80,    14,    16,    12,    14,    10,
   80,    80,    15,    80,    12,    15,    11,    80,    80,    80,
   80,    12,    16,    17,    80,    80,    55,    80,    19,    17,
   80,    80,    80,    18,    80,    19,    18,    80,    80,    80,
   80,    80,    19,    19,    20,    80,    14,    37,    80,    22,
   20,    80,    80,    10,    21,    80,    22,    21,    80,    80,
   11,    80,    80,    22,    22,     2,    80,    23,    39,    43,
    0,    23,     3,    80,    80,    24,    26,     6,    24,     4,
   80,    80,    25,    80,     6,    25,     5,    80,    80,    80,
   80,     6,    26,    27,    80,    80,    38,    80,    29,    27,
   80,    80,    80,    28,    80,    29,    28,    80,    80,    80,
   80,    80,    29,    29,    30,    19,    34,    36,    80,    22,
   30,    80,    20,    31,    33,    80,    22,    31,    80,    10,
   80,    32,    80,    12,    32,    80,    11,    80,    80,    80,
   12,    33,    80,    21,    32,    80,    80,    22,    34,    31,
   14,    80,    35,    80,    12,    35,    32,    15,    80,    80,
   80,    12,    36,    33,    37,    35,    80,    80,    22,    37,
   21,    80,    15,    80,    80,    22,    38,    28,    80,    80,
   80,    80,    29,    39,    40,    80,    24,    42,    80,     0,
   40,    80,    80,     4,    41,    80,     0,    41,    80,    80,
    5,    80,    80,     0,    42,    41,    80,    25,    80,    80,
    0,    43,    44,    80,    26,    54,    80,    46,    44,    80,
   80,    27,    45,    80,    46,    45,    80,    80,    28,    80,
   80,    46,    46,    47,    39,    51,    53,    80,     0,    47,
   80,    40,    48,    50,    80,     0,    48,    80,     4,    80,
   49,    80,     6,    49,    80,     5,    80,    80,    80,     6,
   50,    80,    41,    49,    80,    80,     0,    51,    48,    24,
   80,    52,    80,     6,    52,    49,    25,    80,    80,    80,
    6,    53,    50,    42,    52,    80,    80,     0,    54,    45,
   80,    38,    80,    80,    46,    55,    18,    80,    80,    80,
   80,    19,    56,    57,    80,    16,    59,    80,    39,    57,
   80,    80,    17,    58,    80,    39,    58,    80,    80,    18,
   80,    80,    39,    59,    58,    80,    55,    80,    80,    39,
   60,    80,     9,    80,    31,    61,    12,    61,    80,    17,
   80,    62,    80,    19,    62,    80,    18,    80,    80,    80,
   19,    63,    80,    57,    61,    64,    80,    39,    64,    80,
   58,    62,    80,    80,    39,    65,    60,    13,    80,    34,
   66,    12,    66,    61,    16,    80,    67,    80,    19,    67,
   62,    55,    80,    80,    80,    19,    68,    63,    56,    66,
   69,    80,    39,    69,    64,    59,    67,    80,    80,    39,
   70,    80,     3,    80,    48,    71,     6,    71,    80,    27,
   80,    72,    80,    29,    72,    80,    28,    80,    80,    80,
   29,    73,    80,    44,    71,    74,    80,    46,    74,    80,
   45,    72,    80,    80,    46,    75,    70,    23,    80,    51,
   76,     6,    76,    71,    26,    80,    77,    80,    29,    77,
   72,    38,    80,    80,    80,    29,    78,    73,    43,    76,
   79,    80,    46,    79,    74,    54,    77,    80,    80,    46,
};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char sr71_cpu1_state_alts[] ATTRIBUTE_UNUSED = {
    1,     4,     4,    64,     8,     4,     1,     1,     0,     4,
   64,     8,     4,     1,     1,     0,     0,    64,     8,     4,
    1,     1,     0,     0,     0,     8,     4,     1,     1,     0,
    0,     0,     4,     0,     1,     1,     0,     0,     0,     0,
    0,     1,     1,     4,     4,    64,     8,     4,     1,     1,
    0,     4,    64,     8,     4,     1,     1,     0,     0,    64,
    8,     4,     1,     1,     0,     0,     0,     8,     4,     1,
    1,     0,     0,     0,     4,     0,     1,     1,     0,     0,
    0,     0,     0,     1,     1,     4,     0,    64,     8,     4,
    1,     1,     4,     0,     0,     8,     4,     1,     1,     4,
    0,     0,     4,     0,     1,     1,     4,     0,     0,     0,
    0,     1,     1,     4,     0,     0,     4,     0,     1,     1,
    0,     0,     0,     4,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     4,     0,    64,     4,     0,     1,
    1,     0,     0,    64,     4,     0,     1,     1,     0,     0,
   64,     0,     0,     1,     1,     4,     0,    64,     8,     4,
    1,     1,     4,     0,     0,     8,     4,     1,     1,     4,
    0,     0,     4,     0,     1,     1,     4,     0,     0,     0,
    0,     1,     1,     4,     0,     0,     4,     0,     1,     1,
    0,     0,     0,     4,     0,     1,     1,     0,     0,     0,
    0,     0,     1,     1,     4,     4,    64,     4,     0,     1,
    1,     0,     4,    64,     4,     0,     1,     1,     0,     4,
    0,     4,     0,     1,     1,     0,     4,     0,     0,     0,
    1,     1,     0,     4,    64,     0,     0,     1,     1,     4,
    4,     0,     4,     0,     1,     1,     4,     4,     0,     0,
    0,     1,     1,     4,     4,    64,     0,     0,     1,     1,
    4,     0,    64,     0,     0,     1,     1,     4,     0,     0,
    0,     0,     1,     1,     4,     0,    64,     4,     0,     1,
    1,     0,     0,    64,     4,     0,     1,     1,     0,     0,
   64,     0,     0,     1,     1,     4,     0,    64,     0,     0,
    1,     1,     4,     0,    64,     4,     0,     1,     1,     0,
    0,    64,     4,     0,     1,     1,     0,     0,    64,     0,
    0,     1,     1,     4,     4,    64,     4,     0,     1,     1,
    0,     4,    64,     4,     0,     1,     1,     0,     4,     0,
    4,     0,     1,     1,     0,     4,     0,     0,     0,     1,
    1,     0,     4,    64,     0,     0,     1,     1,     4,     4,
    0,     4,     0,     1,     1,     4,     4,     0,     0,     0,
    1,     1,     4,     4,    64,     0,     0,     1,     1,     4,
    0,    64,     0,     0,     1,     1,     4,     0,     0,     0,
    0,     1,     1,     4,     0,    64,     4,     0,     1,     1,
    0,     0,    64,     4,     0,     1,     1,     0,     0,    64,
    0,     0,     1,     1,     4,     0,    64,     0,     0,     1,
    1,     0,     4,     0,     8,     4,     1,     1,     0,     4,
    0,     4,     0,     1,     1,     0,     4,     0,     0,     0,
    1,     1,     0,     4,    64,     4,     0,     1,     1,     0,
    4,    64,     0,     0,     1,     1,     4,     4,     0,     8,
    4,     1,     1,     4,     4,     0,     4,     0,     1,     1,
    4,     4,     0,     0,     0,     1,     1,     4,     4,    64,
    4,     0,     1,     1,     4,     4,    64,     0,     0,     1,
    1,     0,     4,     0,     8,     4,     1,     1,     0,     4,
    0,     4,     0,     1,     1,     0,     4,     0,     0,     0,
    1,     1,     0,     4,    64,     4,     0,     1,     1,     0,
    4,    64,     0,     0,     1,     1,     4,     4,     0,     8,
    4,     1,     1,     4,     4,     0,     4,     0,     1,     1,
    4,     4,     0,     0,     0,     1,     1,     4,     4,    64,
    4,     0,     1,     1,     4,     4,    64,     0,     0,     1,
};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_cpu1_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,    16,     0,     0,     1,    16,     0,
    0,    17,    16,     0,     1,    17,     1,     0,    17,    17,
   16,     0,     0,     0,     0,    16,     0,     0,     1,    64,
    0,     0,    20,    16,     0,     1,    65,     1,     0,    20,
   17,    16,     0,    48,     0,     0,     3,    16,     0,     0,
   65,     1,     0,     4,    17,    16,     0,    49,     2,     0,
   19,    16,    32,     1,    49,    18,     0,     2,     0,    16,
    1,    32,     1,     0,    18,     1,    16,     0,    16,     0,
    0,     1,    16,     0,     0,    17,     1,     0,     1,    17,
   16,     0,    17,     2,     0,    17,    16,    32,     1,    17,
   18,     0,     0,     0,    16,     1,     0,     1,     0,    16,
   16,    16,     1,     1,    17,     0,    16,     1,    16,     0,
    1,     1,     0,     0,    17,    16,     0,     0,    17,     0,
    2,     1,    16,     0,    17,    18,     0,     1,     0,    16,
    1,    16,     1,     0,    17,     1,    16,     0,    16,    17,
    0,     1,     0,    32,     1,    16,     2,     0,    17,     1,
   32,     0,     0,     1,     0,    16,     0,    16,     1,     1,
    1,     0,    16,    17,    16,     1,     0,    17,     0,     0,
   16,    16,     0,     1,    17,     0,     0,     1,    16,     0,
   16,    18,     0,     3,    17,    32,     0,    32,     2,     0,
   18,     0,    32,     1,    32,    18,     0,     2,     1,    32,
    1,     1,     0,     0,    16,    16,    32,     1,     1,    18,
    0,    16,     0,    32,     1,     0,    18,     0,     0,    16,
    0,     0,     1,     2,     0,     0,    17,    32,     0,     0,
    2,     0,     0,     1,    32,     1,     1,     0,     0,    16,
   16,    32,     1,     1,    18,     0,    16,     0,    32,     1,
    0,    18,     0,     0,    16,     0,     0,     1,     2,     0,
    0,    17,    32,     0,     0,     2,     0,     0,     1,    32,
};

/* Vector for locked state flags.  */
static const unsigned char sr71_cpu1_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_cp1_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     1,     2,
    0,     2,     1,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     3};

/* Vector for state transitions.  */
static const unsigned char sr71_cp1_transitions[] ATTRIBUTE_UNUSED = {
    0,     1,     3,     0,     1,     4,     2,     0,     2,     4,
    4,     0,     3,     2,     4,     0};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char sr71_cp1_state_alts[] ATTRIBUTE_UNUSED = {
    1,    64,    32,     1,     1,     0,    32,     1,     1,     0,
    0,     1,     1,    64,     0,     1};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_cp1_min_issue_delay[] ATTRIBUTE_UNUSED = {
    4,    98};

/* Vector for locked state flags.  */
static const unsigned char sr71_cp1_dead_lock[] = {
    0,     0,     0,     0};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_cp2_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1,     1,     1,     2,     3,     3,     1,     1,     4,     3,
    3,     3,     3,     0,     0,     5};

/* Vector for state transitions.  */
static const unsigned char sr71_cp2_transitions[] ATTRIBUTE_UNUSED = {
    0,     1,     3,     2,     5,     0,     1,     8,     8,     8,
    8,     2,     2,     8,     8,     8,     8,     0,     3,     8,
    8,     8,     8,     4,     4,     8,     8,     8,     8,     5,
    5,     8,     8,     8,     8,     6,     6,     8,     8,     8,
    8,     7,     7,     8,     8,     8,     8,     1};


#if AUTOMATON_STATE_ALTS
/* Vector for state insn alternatives.  */
static const unsigned char sr71_cp2_state_alts[] ATTRIBUTE_UNUSED = {
    1,     2,     2,     2,     2,     1,     1,     0,     0,     0,
    0,     1,     1,     0,     0,     0,     0,     1,     1,     0,
    0,     0,     0,     1,     1,     0,     0,     0,     0,     1,
    1,     0,     0,     0,     0,     1,     1,     0,     0,     0,
    0,     1,     1,     0,     0,     0,     0,     1};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_cp2_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     2,    34,    32,     1,    17,    16,     7,
  119,   112,     6,   102,    96,     5,    85,    80,     4,    68,
   64,     3,    51,    48};

/* Vector for locked state flags.  */
static const unsigned char sr71_cp2_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_fextra_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     1,     2,     0,     0,     0,     3,
    4,     5,     6,     0,     0,     7};

/* Comb vector for state transitions.  */
static const unsigned char sr71_fextra_transitions[] ATTRIBUTE_UNUSED = {
    0,   179,   121,   177,   119,   191,     1,     0,     1,     2,
    3,     4,     5,     6,     7,     2,     3,     4,     5,     6,
    7,     8,     8,     9,    10,    11,    12,    13,    14,     9,
   10,    11,    12,    13,    14,    15,    15,    16,    17,    18,
   19,    20,    21,    16,    17,    18,    19,    20,    21,    22,
   22,    23,    24,    25,    26,    27,    28,    23,    24,    25,
   26,    27,    28,    29,    29,    30,    31,    32,    33,    34,
   35,    30,    31,    32,    33,    34,    35,    36,    36,    37,
   38,    39,    40,    41,    42,    37,    38,    39,    40,    41,
   42,    43,    43,    44,    45,    46,    47,    48,    49,    44,
   45,    46,    47,    48,    49,    50,    50,    51,    52,    53,
   54,    55,    56,    51,    52,    53,    54,    55,    56,    57,
   57,    58,    59,    60,    61,    62,    63,    58,    59,    60,
   61,    62,    63,    64,    64,    65,    66,    67,    68,    69,
   70,    65,    66,    67,    68,    69,    70,    71,    71,    72,
   73,    74,    75,    76,    77,    72,    73,    74,    75,    76,
   77,    78,    78,    79,    80,    81,    82,    83,    84,    79,
   80,    81,    82,    83,    84,    85,    85,    86,    87,    88,
   89,    90,    91,    86,    87,    88,    89,    90,    91,    92,
   92,    93,    94,    95,    96,    97,    98,    93,    94,    95,
   96,    97,    98,    99,    99,   100,   101,   102,   103,   104,
  105,   100,   101,   102,   103,   104,   105,   106,   106,   107,
  108,   109,   110,   111,   112,   107,   108,   109,   110,   111,
  112,   113,   113,   114,   115,   116,   117,   118,   119,   114,
  115,   116,   117,   118,   119,   120,   120,   121,   122,   123,
  124,   125,   126,   121,   122,   123,   124,   125,   126,   127,
  127,   128,   129,   130,   131,   132,   133,   128,   129,   130,
  131,   132,   133,   134,   134,   135,   136,   137,   138,   139,
  140,   135,   136,   137,   138,   139,   140,   141,   141,   142,
  143,   144,   145,   146,   147,   142,   143,   144,   145,   146,
  147,   148,   148,   149,   150,   151,   152,   153,   154,   149,
  150,   151,   152,   153,   154,   155,   155,   156,   157,   158,
  159,   160,   161,   156,   157,   158,   159,   160,   161,   162,
  162,   163,   164,   165,   166,   167,   168,   163,   164,   165,
  166,   167,   168,   169,   169,   170,   171,   172,   173,   174,
  175,   170,   171,   172,   173,   174,   175,   176,   176,   177,
  178,   179,   180,   181,   182,   177,   178,   179,   180,   181,
  182,   183,   183,   184,   185,   186,   187,   188,   189,   184,
  185,   186,   187,   188,   189,   190,   190,   191,   192,   193,
  194,   195,   196,   191,   192,   193,   194,   195,   196,   197,
  197,   198,   199,   200,   201,   202,   203,   198,   199,   200,
  201,   202,   203,   204,   204,   205,   206,   207,   208,   209,
  210,   205,   206,   207,   208,   209,   210,   211,   211,   212,
  213,   214,   215,   216,   217,   212,   213,   214,   215,   216,
  217,   218,   218,   219,   220,   221,   222,   223,   224,   219,
  220,   221,   222,   223,   224,   225,   225,   226,   227,   228,
  229,   230,   230,   226,   227,   228,   229,     0};

/* Check vector for state transitions.  */
static const unsigned char sr71_fextra_check[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     1,     2,
    3,     4,     5,     6,     7,     1,     2,     3,     4,     5,
    6,     7,     8,     9,    10,    11,    12,    13,    14,     8,
    9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
   19,    20,    21,    15,    16,    17,    18,    19,    20,    21,
   22,    23,    24,    25,    26,    27,    28,    22,    23,    24,
   25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
   35,    29,    30,    31,    32,    33,    34,    35,    36,    37,
   38,    39,    40,    41,    42,    36,    37,    38,    39,    40,
   41,    42,    43,    44,    45,    46,    47,    48,    49,    43,
   44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
   54,    55,    56,    50,    51,    52,    53,    54,    55,    56,
   57,    58,    59,    60,    61,    62,    63,    57,    58,    59,
   60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
   70,    64,    65,    66,    67,    68,    69,    70,    71,    72,
   73,    74,    75,    76,    77,    71,    72,    73,    74,    75,
   76,    77,    78,    79,    80,    81,    82,    83,    84,    78,
   79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
   89,    90,    91,    85,    86,    87,    88,    89,    90,    91,
   92,    93,    94,    95,    96,    97,    98,    92,    93,    94,
   95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
  105,    99,   100,   101,   102,   103,   104,   105,   106,   107,
  108,   109,   110,   111,   112,   106,   107,   108,   109,   110,
  111,   112,   113,   114,   115,   116,   117,   118,   119,   113,
  114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
  124,   125,   126,   120,   121,   122,   123,   124,   125,   126,
  127,   128,   129,   130,   131,   132,   133,   127,   128,   129,
  130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
  140,   134,   135,   136,   137,   138,   139,   140,   141,   142,
  143,   144,   145,   146,   147,   141,   142,   143,   144,   145,
  146,   147,   148,   149,   150,   151,   152,   153,   154,   148,
  149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
  159,   160,   161,   155,   156,   157,   158,   159,   160,   161,
  162,   163,   164,   165,   166,   167,   168,   162,   163,   164,
  165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
  175,   169,   170,   171,   172,   173,   174,   175,   176,   177,
  178,   179,   180,   181,   182,   176,   177,   178,   179,   180,
  181,   182,   183,   184,   185,   186,   187,   188,   189,   183,
  184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
  194,   195,   196,   190,   191,   192,   193,   194,   195,   196,
  197,   198,   199,   200,   201,   202,   203,   197,   198,   199,
  200,   201,   202,   203,   204,   205,   206,   207,   208,   209,
  210,   204,   205,   206,   207,   208,   209,   210,   211,   212,
  213,   214,   215,   216,   217,   211,   212,   213,   214,   215,
  216,   217,   218,   219,   220,   221,   222,   223,   224,   218,
  219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
  229,   230,   230,   225,   226,   227,   228,   229};

/* Base vector for state transitions.  */
static const unsigned short sr71_fextra_base[] = {
    0,     8,     9,    10,    11,    12,    13,    14,    22,    23,
   24,    25,    26,    27,    28,    36,    37,    38,    39,    40,
   41,    42,    50,    51,    52,    53,    54,    55,    56,    64,
   65,    66,    67,    68,    69,    70,    78,    79,    80,    81,
   82,    83,    84,    92,    93,    94,    95,    96,    97,    98,
  106,   107,   108,   109,   110,   111,   112,   120,   121,   122,
  123,   124,   125,   126,   134,   135,   136,   137,   138,   139,
  140,   148,   149,   150,   151,   152,   153,   154,   162,   163,
  164,   165,   166,   167,   168,   176,   177,   178,   179,   180,
  181,   182,   190,   191,   192,   193,   194,   195,   196,   204,
  205,   206,   207,   208,   209,   210,   218,   219,   220,   221,
  222,   223,   224,   232,   233,   234,   235,   236,   237,   238,
  246,   247,   248,   249,   250,   251,   252,   260,   261,   262,
  263,   264,   265,   266,   274,   275,   276,   277,   278,   279,
  280,   288,   289,   290,   291,   292,   293,   294,   302,   303,
  304,   305,   306,   307,   308,   316,   317,   318,   319,   320,
  321,   322,   330,   331,   332,   333,   334,   335,   336,   344,
  345,   346,   347,   348,   349,   350,   358,   359,   360,   361,
  362,   363,   364,   372,   373,   374,   375,   376,   377,   378,
  386,   387,   388,   389,   390,   391,   392,   400,   401,   402,
  403,   404,   405,   406,   414,   415,   416,   417,   418,   419,
  420,   428,   429,   430,   431,   432,   433,   434,   442,   443,
  444,   445,   446,   447,   448,   456,   457,   458,   459,   460,
};


#if AUTOMATON_STATE_ALTS
/* Comb vector for state insn alternatives.  */
static const unsigned char sr71_fextra_state_alts[] ATTRIBUTE_UNUSED = {
    1,     2,     2,     2,     2,     2,     2,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     0,     0,     1,     1,     1,     1,     1};

/* Check vector for state insn alternatives.  */
static const unsigned char sr71_fextra_check_state_alts[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     1,     2,
    3,     4,     5,     6,     7,     1,     2,     3,     4,     5,
    6,     7,     8,     9,    10,    11,    12,    13,    14,     8,
    9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
   19,    20,    21,    15,    16,    17,    18,    19,    20,    21,
   22,    23,    24,    25,    26,    27,    28,    22,    23,    24,
   25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
   35,    29,    30,    31,    32,    33,    34,    35,    36,    37,
   38,    39,    40,    41,    42,    36,    37,    38,    39,    40,
   41,    42,    43,    44,    45,    46,    47,    48,    49,    43,
   44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
   54,    55,    56,    50,    51,    52,    53,    54,    55,    56,
   57,    58,    59,    60,    61,    62,    63,    57,    58,    59,
   60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
   70,    64,    65,    66,    67,    68,    69,    70,    71,    72,
   73,    74,    75,    76,    77,    71,    72,    73,    74,    75,
   76,    77,    78,    79,    80,    81,    82,    83,    84,    78,
   79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
   89,    90,    91,    85,    86,    87,    88,    89,    90,    91,
   92,    93,    94,    95,    96,    97,    98,    92,    93,    94,
   95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
  105,    99,   100,   101,   102,   103,   104,   105,   106,   107,
  108,   109,   110,   111,   112,   106,   107,   108,   109,   110,
  111,   112,   113,   114,   115,   116,   117,   118,   119,   113,
  114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
  124,   125,   126,   120,   121,   122,   123,   124,   125,   126,
  127,   128,   129,   130,   131,   132,   133,   127,   128,   129,
  130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
  140,   134,   135,   136,   137,   138,   139,   140,   141,   142,
  143,   144,   145,   146,   147,   141,   142,   143,   144,   145,
  146,   147,   148,   149,   150,   151,   152,   153,   154,   148,
  149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
  159,   160,   161,   155,   156,   157,   158,   159,   160,   161,
  162,   163,   164,   165,   166,   167,   168,   162,   163,   164,
  165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
  175,   169,   170,   171,   172,   173,   174,   175,   176,   177,
  178,   179,   180,   181,   182,   176,   177,   178,   179,   180,
  181,   182,   183,   184,   185,   186,   187,   188,   189,   183,
  184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
  194,   195,   196,   190,   191,   192,   193,   194,   195,   196,
  197,   198,   199,   200,   201,   202,   203,   197,   198,   199,
  200,   201,   202,   203,   204,   205,   206,   207,   208,   209,
  210,   204,   205,   206,   207,   208,   209,   210,   211,   212,
  213,   214,   215,   216,   217,   211,   212,   213,   214,   215,
  216,   217,   218,   219,   220,   221,   222,   223,   224,   218,
  219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
  229,   230,   230,   225,   226,   227,   228,   229};

/* Base vector for state insn alternatives.  */
static const unsigned short sr71_fextra_base_state_alts[] = {
    0,     8,     9,    10,    11,    12,    13,    14,    22,    23,
   24,    25,    26,    27,    28,    36,    37,    38,    39,    40,
   41,    42,    50,    51,    52,    53,    54,    55,    56,    64,
   65,    66,    67,    68,    69,    70,    78,    79,    80,    81,
   82,    83,    84,    92,    93,    94,    95,    96,    97,    98,
  106,   107,   108,   109,   110,   111,   112,   120,   121,   122,
  123,   124,   125,   126,   134,   135,   136,   137,   138,   139,
  140,   148,   149,   150,   151,   152,   153,   154,   162,   163,
  164,   165,   166,   167,   168,   176,   177,   178,   179,   180,
  181,   182,   190,   191,   192,   193,   194,   195,   196,   204,
  205,   206,   207,   208,   209,   210,   218,   219,   220,   221,
  222,   223,   224,   232,   233,   234,   235,   236,   237,   238,
  246,   247,   248,   249,   250,   251,   252,   260,   261,   262,
  263,   264,   265,   266,   274,   275,   276,   277,   278,   279,
  280,   288,   289,   290,   291,   292,   293,   294,   302,   303,
  304,   305,   306,   307,   308,   316,   317,   318,   319,   320,
  321,   322,   330,   331,   332,   333,   334,   335,   336,   344,
  345,   346,   347,   348,   349,   350,   358,   359,   360,   361,
  362,   363,   364,   372,   373,   374,   375,   376,   377,   378,
  386,   387,   388,   389,   390,   391,   392,   400,   401,   402,
  403,   404,   405,   406,   414,   415,   416,   417,   418,   419,
  420,   428,   429,   430,   431,   432,   433,   434,   442,   443,
  444,   445,   446,   447,   448,   456,   457,   458,   459,   460,
};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_fextra_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,   229,
  229,   229,   229,   229,   229,     0,     0,   228,   228,   228,
  228,   228,   228,     0,     0,   227,   227,   227,   227,   227,
  227,     0,     0,   226,   226,   226,   226,   226,   226,     0,
    0,   225,   225,   225,   225,   225,   225,     0,     0,   224,
  224,   224,   224,   224,   224,     0,     0,   223,   223,   223,
  223,   223,   223,     0,     0,   222,   222,   222,   222,   222,
  222,     0,     0,   221,   221,   221,   221,   221,   221,     0,
    0,   220,   220,   220,   220,   220,   220,     0,     0,   219,
  219,   219,   219,   219,   219,     0,     0,   218,   218,   218,
  218,   218,   218,     0,     0,   217,   217,   217,   217,   217,
  217,     0,     0,   216,   216,   216,   216,   216,   216,     0,
    0,   215,   215,   215,   215,   215,   215,     0,     0,   214,
  214,   214,   214,   214,   214,     0,     0,   213,   213,   213,
  213,   213,   213,     0,     0,   212,   212,   212,   212,   212,
  212,     0,     0,   211,   211,   211,   211,   211,   211,     0,
    0,   210,   210,   210,   210,   210,   210,     0,     0,   209,
  209,   209,   209,   209,   209,     0,     0,   208,   208,   208,
  208,   208,   208,     0,     0,   207,   207,   207,   207,   207,
  207,     0,     0,   206,   206,   206,   206,   206,   206,     0,
    0,   205,   205,   205,   205,   205,   205,     0,     0,   204,
  204,   204,   204,   204,   204,     0,     0,   203,   203,   203,
  203,   203,   203,     0,     0,   202,   202,   202,   202,   202,
  202,     0,     0,   201,   201,   201,   201,   201,   201,     0,
    0,   200,   200,   200,   200,   200,   200,     0,     0,   199,
  199,   199,   199,   199,   199,     0,     0,   198,   198,   198,
  198,   198,   198,     0,     0,   197,   197,   197,   197,   197,
  197,     0,     0,   196,   196,   196,   196,   196,   196,     0,
    0,   195,   195,   195,   195,   195,   195,     0,     0,   194,
  194,   194,   194,   194,   194,     0,     0,   193,   193,   193,
  193,   193,   193,     0,     0,   192,   192,   192,   192,   192,
  192,     0,     0,   191,   191,   191,   191,   191,   191,     0,
    0,   190,   190,   190,   190,   190,   190,     0,     0,   189,
  189,   189,   189,   189,   189,     0,     0,   188,   188,   188,
  188,   188,   188,     0,     0,   187,   187,   187,   187,   187,
  187,     0,     0,   186,   186,   186,   186,   186,   186,     0,
    0,   185,   185,   185,   185,   185,   185,     0,     0,   184,
  184,   184,   184,   184,   184,     0,     0,   183,   183,   183,
  183,   183,   183,     0,     0,   182,   182,   182,   182,   182,
  182,     0,     0,   181,   181,   181,   181,   181,   181,     0,
    0,   180,   180,   180,   180,   180,   180,     0,     0,   179,
  179,   179,   179,   179,   179,     0,     0,   178,   178,   178,
  178,   178,   178,     0,     0,   177,   177,   177,   177,   177,
  177,     0,     0,   176,   176,   176,   176,   176,   176,     0,
    0,   175,   175,   175,   175,   175,   175,     0,     0,   174,
  174,   174,   174,   174,   174,     0,     0,   173,   173,   173,
  173,   173,   173,     0,     0,   172,   172,   172,   172,   172,
  172,     0,     0,   171,   171,   171,   171,   171,   171,     0,
    0,   170,   170,   170,   170,   170,   170,     0,     0,   169,
  169,   169,   169,   169,   169,     0,     0,   168,   168,   168,
  168,   168,   168,     0,     0,   167,   167,   167,   167,   167,
  167,     0,     0,   166,   166,   166,   166,   166,   166,     0,
    0,   165,   165,   165,   165,   165,   165,     0,     0,   164,
  164,   164,   164,   164,   164,     0,     0,   163,   163,   163,
  163,   163,   163,     0,     0,   162,   162,   162,   162,   162,
  162,     0,     0,   161,   161,   161,   161,   161,   161,     0,
    0,   160,   160,   160,   160,   160,   160,     0,     0,   159,
  159,   159,   159,   159,   159,     0,     0,   158,   158,   158,
  158,   158,   158,     0,     0,   157,   157,   157,   157,   157,
  157,     0,     0,   156,   156,   156,   156,   156,   156,     0,
    0,   155,   155,   155,   155,   155,   155,     0,     0,   154,
  154,   154,   154,   154,   154,     0,     0,   153,   153,   153,
  153,   153,   153,     0,     0,   152,   152,   152,   152,   152,
  152,     0,     0,   151,   151,   151,   151,   151,   151,     0,
    0,   150,   150,   150,   150,   150,   150,     0,     0,   149,
  149,   149,   149,   149,   149,     0,     0,   148,   148,   148,
  148,   148,   148,     0,     0,   147,   147,   147,   147,   147,
  147,     0,     0,   146,   146,   146,   146,   146,   146,     0,
    0,   145,   145,   145,   145,   145,   145,     0,     0,   144,
  144,   144,   144,   144,   144,     0,     0,   143,   143,   143,
  143,   143,   143,     0,     0,   142,   142,   142,   142,   142,
  142,     0,     0,   141,   141,   141,   141,   141,   141,     0,
    0,   140,   140,   140,   140,   140,   140,     0,     0,   139,
  139,   139,   139,   139,   139,     0,     0,   138,   138,   138,
  138,   138,   138,     0,     0,   137,   137,   137,   137,   137,
  137,     0,     0,   136,   136,   136,   136,   136,   136,     0,
    0,   135,   135,   135,   135,   135,   135,     0,     0,   134,
  134,   134,   134,   134,   134,     0,     0,   133,   133,   133,
  133,   133,   133,     0,     0,   132,   132,   132,   132,   132,
  132,     0,     0,   131,   131,   131,   131,   131,   131,     0,
    0,   130,   130,   130,   130,   130,   130,     0,     0,   129,
  129,   129,   129,   129,   129,     0,     0,   128,   128,   128,
  128,   128,   128,     0,     0,   127,   127,   127,   127,   127,
  127,     0,     0,   126,   126,   126,   126,   126,   126,     0,
    0,   125,   125,   125,   125,   125,   125,     0,     0,   124,
  124,   124,   124,   124,   124,     0,     0,   123,   123,   123,
  123,   123,   123,     0,     0,   122,   122,   122,   122,   122,
  122,     0,     0,   121,   121,   121,   121,   121,   121,     0,
    0,   120,   120,   120,   120,   120,   120,     0,     0,   119,
  119,   119,   119,   119,   119,     0,     0,   118,   118,   118,
  118,   118,   118,     0,     0,   117,   117,   117,   117,   117,
  117,     0,     0,   116,   116,   116,   116,   116,   116,     0,
    0,   115,   115,   115,   115,   115,   115,     0,     0,   114,
  114,   114,   114,   114,   114,     0,     0,   113,   113,   113,
  113,   113,   113,     0,     0,   112,   112,   112,   112,   112,
  112,     0,     0,   111,   111,   111,   111,   111,   111,     0,
    0,   110,   110,   110,   110,   110,   110,     0,     0,   109,
  109,   109,   109,   109,   109,     0,     0,   108,   108,   108,
  108,   108,   108,     0,     0,   107,   107,   107,   107,   107,
  107,     0,     0,   106,   106,   106,   106,   106,   106,     0,
    0,   105,   105,   105,   105,   105,   105,     0,     0,   104,
  104,   104,   104,   104,   104,     0,     0,   103,   103,   103,
  103,   103,   103,     0,     0,   102,   102,   102,   102,   102,
  102,     0,     0,   101,   101,   101,   101,   101,   101,     0,
    0,   100,   100,   100,   100,   100,   100,     0,     0,    99,
   99,    99,    99,    99,    99,     0,     0,    98,    98,    98,
   98,    98,    98,     0,     0,    97,    97,    97,    97,    97,
   97,     0,     0,    96,    96,    96,    96,    96,    96,     0,
    0,    95,    95,    95,    95,    95,    95,     0,     0,    94,
   94,    94,    94,    94,    94,     0,     0,    93,    93,    93,
   93,    93,    93,     0,     0,    92,    92,    92,    92,    92,
   92,     0,     0,    91,    91,    91,    91,    91,    91,     0,
    0,    90,    90,    90,    90,    90,    90,     0,     0,    89,
   89,    89,    89,    89,    89,     0,     0,    88,    88,    88,
   88,    88,    88,     0,     0,    87,    87,    87,    87,    87,
   87,     0,     0,    86,    86,    86,    86,    86,    86,     0,
    0,    85,    85,    85,    85,    85,    85,     0,     0,    84,
   84,    84,    84,    84,    84,     0,     0,    83,    83,    83,
   83,    83,    83,     0,     0,    82,    82,    82,    82,    82,
   82,     0,     0,    81,    81,    81,    81,    81,    81,     0,
    0,    80,    80,    80,    80,    80,    80,     0,     0,    79,
   79,    79,    79,    79,    79,     0,     0,    78,    78,    78,
   78,    78,    78,     0,     0,    77,    77,    77,    77,    77,
   77,     0,     0,    76,    76,    76,    76,    76,    76,     0,
    0,    75,    75,    75,    75,    75,    75,     0,     0,    74,
   74,    74,    74,    74,    74,     0,     0,    73,    73,    73,
   73,    73,    73,     0,     0,    72,    72,    72,    72,    72,
   72,     0,     0,    71,    71,    71,    71,    71,    71,     0,
    0,    70,    70,    70,    70,    70,    70,     0,     0,    69,
   69,    69,    69,    69,    69,     0,     0,    68,    68,    68,
   68,    68,    68,     0,     0,    67,    67,    67,    67,    67,
   67,     0,     0,    66,    66,    66,    66,    66,    66,     0,
    0,    65,    65,    65,    65,    65,    65,     0,     0,    64,
   64,    64,    64,    64,    64,     0,     0,    63,    63,    63,
   63,    63,    63,     0,     0,    62,    62,    62,    62,    62,
   62,     0,     0,    61,    61,    61,    61,    61,    61,     0,
    0,    60,    60,    60,    60,    60,    60,     0,     0,    59,
   59,    59,    59,    59,    59,     0,     0,    58,    58,    58,
   58,    58,    58,     0,     0,    57,    57,    57,    57,    57,
   57,     0,     0,    56,    56,    56,    56,    56,    56,     0,
    0,    55,    55,    55,    55,    55,    55,     0,     0,    54,
   54,    54,    54,    54,    54,     0,     0,    53,    53,    53,
   53,    53,    53,     0,     0,    52,    52,    52,    52,    52,
   52,     0,     0,    51,    51,    51,    51,    51,    51,     0,
    0,    50,    50,    50,    50,    50,    50,     0,     0,    49,
   49,    49,    49,    49,    49,     0,     0,    48,    48,    48,
   48,    48,    48,     0,     0,    47,    47,    47,    47,    47,
   47,     0,     0,    46,    46,    46,    46,    46,    46,     0,
    0,    45,    45,    45,    45,    45,    45,     0,     0,    44,
   44,    44,    44,    44,    44,     0,     0,    43,    43,    43,
   43,    43,    43,     0,     0,    42,    42,    42,    42,    42,
   42,     0,     0,    41,    41,    41,    41,    41,    41,     0,
    0,    40,    40,    40,    40,    40,    40,     0,     0,    39,
   39,    39,    39,    39,    39,     0,     0,    38,    38,    38,
   38,    38,    38,     0,     0,    37,    37,    37,    37,    37,
   37,     0,     0,    36,    36,    36,    36,    36,    36,     0,
    0,    35,    35,    35,    35,    35,    35,     0,     0,    34,
   34,    34,    34,    34,    34,     0,     0,    33,    33,    33,
   33,    33,    33,     0,     0,    32,    32,    32,    32,    32,
   32,     0,     0,    31,    31,    31,    31,    31,    31,     0,
    0,    30,    30,    30,    30,    30,    30,     0,     0,    29,
   29,    29,    29,    29,    29,     0,     0,    28,    28,    28,
   28,    28,    28,     0,     0,    27,    27,    27,    27,    27,
   27,     0,     0,    26,    26,    26,    26,    26,    26,     0,
    0,    25,    25,    25,    25,    25,    25,     0,     0,    24,
   24,    24,    24,    24,    24,     0,     0,    23,    23,    23,
   23,    23,    23,     0,     0,    22,    22,    22,    22,    22,
   22,     0,     0,    21,    21,    21,    21,    21,    21,     0,
    0,    20,    20,    20,    20,    20,    20,     0,     0,    19,
   19,    19,    19,    19,    19,     0,     0,    18,    18,    18,
   18,    18,    18,     0,     0,    17,    17,    17,    17,    17,
   17,     0,     0,    16,    16,    16,    16,    16,    16,     0,
    0,    15,    15,    15,    15,    15,    15,     0,     0,    14,
   14,    14,    14,    14,    14,     0,     0,    13,    13,    13,
   13,    13,    13,     0,     0,    12,    12,    12,    12,    12,
   12,     0,     0,    11,    11,    11,    11,    11,    11,     0,
    0,    10,    10,    10,    10,    10,    10,     0,     0,     9,
    9,     9,     9,     9,     9,     0,     0,     8,     8,     8,
    8,     8,     8,     0,     0,     7,     7,     7,     7,     7,
    7,     0,     0,     6,     6,     6,     6,     6,     6,     0,
    0,     5,     5,     5,     5,     5,     5,     0,     0,     4,
    4,     4,     4,     4,     4,     0,     0,     3,     3,     3,
    3,     3,     3,     0,     0,     2,     2,     2,     2,     2,
    2,     0,     0,     1,     1,     1,     1,     1,     1,     0,
};

/* Vector for locked state flags.  */
static const unsigned char sr71_fextra_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
};

/* Vector translating external insn codes to internal ones.*/
static const unsigned char sr71_imacc_translate[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     1,     2,     3,     4,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     5};

/* Comb vector for state transitions.  */
static const unsigned char sr71_imacc_transitions[] ATTRIBUTE_UNUSED = {
    0,     1,     2,     4,    39,     0,     1,     2,     3,     4,
    5,     0,     3,     1,     5,     6,     6,     7,     8,     9,
   10,     7,     8,     9,    10,    11,    11,    12,    13,    14,
   15,    12,    13,    14,    15,    16,    16,    17,    18,    19,
   20,    17,    18,    19,    20,    21,    21,    22,    23,    24,
   25,    22,    23,    24,    25,    26,    26,    27,    28,    29,
   30,    27,    28,    29,    30,    31,    31,    32,    33,    34,
   35,    32,    33,    34,    35,    36,    36,    37,    38,    39,
   40,    37,    38,     2,    40,    41,    41,    42,    43,    44,
   45,    42,    43,    44,    45,    46,    46,    47,    48,    49,
   50,    47,    48,    49,    50,    51,    51,    52,    53,    54,
   55,    52,    53,    54,    55,    56,    56,    57,    58,    59,
   60,    57,    58,    59,    60,    61,    61,    62,    63,    64,
   65,    62,    63,    64,    65,    66,    66,    67,    68,    69,
   70,    67,    68,    69,    70,     4};

/* Check vector for state transitions.  */
static const unsigned char sr71_imacc_check[] = {
    0,     0,     0,     0,     0,     0,     1,     2,     3,     4,
    5,     1,     2,     3,     4,     5,     6,     7,     8,     9,
   10,     6,     7,     8,     9,    10,    11,    12,    13,    14,
   15,    11,    12,    13,    14,    15,    16,    17,    18,    19,
   20,    16,    17,    18,    19,    20,    21,    22,    23,    24,
   25,    21,    22,    23,    24,    25,    26,    27,    28,    29,
   30,    26,    27,    28,    29,    30,    31,    32,    33,    34,
   35,    31,    32,    33,    34,    35,    36,    37,    38,    39,
   40,    36,    37,    38,    39,    40,    41,    42,    43,    44,
   45,    41,    42,    43,    44,    45,    46,    47,    48,    49,
   50,    46,    47,    48,    49,    50,    51,    52,    53,    54,
   55,    51,    52,    53,    54,    55,    56,    57,    58,    59,
   60,    56,    57,    58,    59,    60,    61,    62,    63,    64,
   65,    61,    62,    63,    64,    65,    66,    67,    68,    69,
   70,    66,    67,    68,    69,    70};

/* Base vector for state transitions.  */
static const unsigned char sr71_imacc_base[] = {
    0,     6,     7,     8,     9,    10,    16,    17,    18,    19,
   20,    26,    27,    28,    29,    30,    36,    37,    38,    39,
   40,    46,    47,    48,    49,    50,    56,    57,    58,    59,
   60,    66,    67,    68,    69,    70,    76,    77,    78,    79,
   80,    86,    87,    88,    89,    90,    96,    97,    98,    99,
  100,   106,   107,   108,   109,   110,   116,   117,   118,   119,
  120,   126,   127,   128,   129,   130,   136,   137,   138,   139,
  140};


#if AUTOMATON_STATE_ALTS
/* Comb vector for state insn alternatives.  */
static const unsigned char sr71_imacc_state_alts[] ATTRIBUTE_UNUSED = {
    1,     4,     4,     4,     4,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1};

/* Check vector for state insn alternatives.  */
static const unsigned char sr71_imacc_check_state_alts[] = {
    0,     0,     0,     0,     0,     0,     1,     2,     3,     4,
    5,     1,     2,     3,     4,     5,     6,     7,     8,     9,
   10,     6,     7,     8,     9,    10,    11,    12,    13,    14,
   15,    11,    12,    13,    14,    15,    16,    17,    18,    19,
   20,    16,    17,    18,    19,    20,    21,    22,    23,    24,
   25,    21,    22,    23,    24,    25,    26,    27,    28,    29,
   30,    26,    27,    28,    29,    30,    31,    32,    33,    34,
   35,    31,    32,    33,    34,    35,    36,    37,    38,    39,
   40,    36,    37,    38,    39,    40,    41,    42,    43,    44,
   45,    41,    42,    43,    44,    45,    46,    47,    48,    49,
   50,    46,    47,    48,    49,    50,    51,    52,    53,    54,
   55,    51,    52,    53,    54,    55,    56,    57,    58,    59,
   60,    56,    57,    58,    59,    60,    61,    62,    63,    64,
   65,    61,    62,    63,    64,    65,    66,    67,    68,    69,
   70,    66,    67,    68,    69,    70};

/* Base vector for state insn alternatives.  */
static const unsigned char sr71_imacc_base_state_alts[] = {
    0,     6,     7,     8,     9,    10,    16,    17,    18,    19,
   20,    26,    27,    28,    29,    30,    36,    37,    38,    39,
   40,    46,    47,    48,    49,    50,    56,    57,    58,    59,
   60,    66,    67,    68,    69,    70,    76,    77,    78,    79,
   80,    86,    87,    88,    89,    90,    96,    97,    98,    99,
  100,   106,   107,   108,   109,   110,   116,   117,   118,   119,
  120,   126,   127,   128,   129,   130,   136,   137,   138,   139,
  140};


#endif /* #if AUTOMATON_STATE_ALTS */

/* Vector of min issue delay of insns.*/
static const unsigned char sr71_imacc_min_issue_delay[] ATTRIBUTE_UNUSED = {
    0,     0,     0,     0,     0,     0,     0,     1,     1,     1,
    1,     0,     0,     3,     3,     3,     3,     0,     0,     2,
    2,     2,     2,     0,     0,    38,    38,    38,    38,     0,
    0,    37,    37,    37,    37,     0,     0,    36,    36,    36,
   36,     0,     0,    35,    35,    35,    35,     0,     0,    34,
   34,    34,    34,     0,     0,    33,    33,    33,    33,     0,
    0,    32,    32,    32,    32,     0,     0,    31,    31,    31,
   31,     0,     0,    30,    30,    30,    30,     0,     0,    29,
   29,    29,    29,     0,     0,    28,    28,    28,    28,     0,
    0,    27,    27,    27,    27,     0,     0,    26,    26,    26,
   26,     0,     0,    25,    25,    25,    25,     0,     0,    24,
   24,    24,    24,     0,     0,    23,    23,    23,    23,     0,
    0,    22,    22,    22,    22,     0,     0,    21,    21,    21,
   21,     0,     0,    20,    20,    20,    20,     0,     0,    19,
   19,    19,    19,     0,     0,    18,    18,    18,    18,     0,
    0,    17,    17,    17,    17,     0,     0,    16,    16,    16,
   16,     0,     0,    15,    15,    15,    15,     0,     0,    14,
   14,    14,    14,     0,     0,    13,    13,    13,    13,     0,
    0,    12,    12,    12,    12,     0,     0,    11,    11,    11,
   11,     0,     0,    10,    10,    10,    10,     0,     0,     9,
    9,     9,     9,     0,     0,     8,     8,     8,     8,     0,
    0,     7,     7,     7,     7,     0,     0,     6,     6,     6,
    6,     0,     0,     5,     5,     5,     5,     0,     0,     4,
    4,     4,     4,     0,     0,    70,    70,    70,    70,     0,
    0,    69,    69,    69,    69,     0,     0,    68,    68,    68,
   68,     0,     0,    67,    67,    67,    67,     0,     0,    66,
   66,    66,    66,     0,     0,    65,    65,    65,    65,     0,
    0,    64,    64,    64,    64,     0,     0,    63,    63,    63,
   63,     0,     0,    62,    62,    62,    62,     0,     0,    61,
   61,    61,    61,     0,     0,    60,    60,    60,    60,     0,
    0,    59,    59,    59,    59,     0,     0,    58,    58,    58,
   58,     0,     0,    57,    57,    57,    57,     0,     0,    56,
   56,    56,    56,     0,     0,    55,    55,    55,    55,     0,
    0,    54,    54,    54,    54,     0,     0,    53,    53,    53,
   53,     0,     0,    52,    52,    52,    52,     0,     0,    51,
   51,    51,    51,     0,     0,    50,    50,    50,    50,     0,
    0,    49,    49,    49,    49,     0,     0,    48,    48,    48,
   48,     0,     0,    47,    47,    47,    47,     0,     0,    46,
   46,    46,    46,     0,     0,    45,    45,    45,    45,     0,
    0,    44,    44,    44,    44,     0,     0,    43,    43,    43,
   43,     0,     0,    42,    42,    42,    42,     0,     0,    41,
   41,    41,    41,     0,     0,    40,    40,    40,    40,     0,
    0,    39,    39,    39,    39,     0};

/* Vector for locked state flags.  */
static const unsigned char sr71_imacc_dead_lock[] = {
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0};


#define DFA__ADVANCE_CYCLE 85

struct DFA_chip
{
  unsigned char vr54_automaton_state;
  unsigned char vr55_automaton_state;
  unsigned char sr71_cpu_automaton_state;
  unsigned char sr71_cpu1_automaton_state;
  unsigned char sr71_cp1_automaton_state;
  unsigned char sr71_cp2_automaton_state;
  unsigned char sr71_fextra_automaton_state;
  unsigned char sr71_imacc_automaton_state;
};


int max_insn_queue_index = 255;

static int internal_min_issue_delay PARAMS ((int, struct DFA_chip *));
static int
internal_min_issue_delay (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip  ATTRIBUTE_UNUSED;
{
  int temp ATTRIBUTE_UNUSED;
  int res = -1;

  switch (insn_code)
    {
    case 0: /* ir_vr54_unknown */
    case 1: /* ir_vr54_branch */
    case 2: /* ir_vr54_load */
    case 3: /* ir_vr54_store */
    case 4: /* ir_vr54_fstore */
    case 5: /* ir_vr54_move */
    case 6: /* ir_vr54_xfer */
    case 7: /* ir_vr54_hilo */
    case 8: /* ir_vr54_arith */
    case 9: /* ir_vr54_imul_si */
    case 10: /* ir_vr54_imul_di */
    case 11: /* ir_vr54_imadd_si */
    case 12: /* ir_vr54_idiv_si */
    case 13: /* ir_vr54_idiv_di */
    case 14: /* ir_vr54_fadd */
    case 15: /* ir_vr54_fmul_sf */
    case 16: /* ir_vr54_fmul_df */
    case 17: /* ir_vr54_fmadd_sf */
    case 18: /* ir_vr54_fmadd_df */
    case 19: /* ir_vr54_fdiv_sf */
    case 20: /* ir_vr54_fdiv_df */
    case 21: /* ir_vr54_fabs */
    case 22: /* ir_vr54_fcmp */
    case 23: /* ir_vr54_fcvt */
    case 24: /* ir_vr54_frsqrt_sf */
    case 25: /* ir_vr54_frsqrt_df */
    case 26: /* ir_vr54_multi */

      temp = vr54_min_issue_delay [(vr54_translate [insn_code] + chip->vr54_automaton_state * 6) / 8];
      temp = (temp >> (8 - (vr54_translate [insn_code] % 8 + 1) * 1)) & 1;
      res = temp;
      break;

    case 27: /* ir_vr55_unknown */
    case 28: /* ir_vr55_branch */
    case 29: /* ir_vr55_load */
    case 30: /* ir_vr55_store */
    case 31: /* ir_vr55_move */
    case 32: /* ir_vr55_xfer */
    case 33: /* ir_vr55_hilo */
    case 34: /* ir_vr55_arith */
    case 35: /* ir_vr55_imul_si */
    case 36: /* ir_vr55_imul_di */
    case 37: /* ir_vr55_imadd_si */
    case 38: /* ir_vr55_idiv_si */
    case 39: /* ir_vr55_idiv_di */
    case 40: /* ir_vr55_fadd */
    case 41: /* ir_vr55_fmul_sf */
    case 42: /* ir_vr55_fmul_df */
    case 43: /* ir_vr55_fmadd_sf */
    case 44: /* ir_vr55_fmadd_df */
    case 45: /* ir_vr55_fdiv_sf */
    case 46: /* ir_vr55_fdiv_df */
    case 47: /* ir_vr55_fabs */
    case 48: /* ir_vr55_fcmp */
    case 49: /* ir_vr55_fcvt_sf */
    case 50: /* ir_vr55_fcvt_df */
    case 51: /* ir_vr55_frsqrt_sf */
    case 52: /* ir_vr55_frsqrt_df */
    case 53: /* ir_vr55_multi */

      temp = vr55_min_issue_delay [(vr55_translate [insn_code] + chip->vr55_automaton_state * 8) / 8];
      temp = (temp >> (8 - (vr55_translate [insn_code] % 8 + 1) * 1)) & 1;
      res = temp;
      break;

    case 54: /* ir_sr70_unknown */
    case 83: /* ir_sr70_multi */

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      res = temp;
      break;

    case 55: /* ir_sr70_branch */
    case 56: /* ir_sr70_load */
    case 57: /* ir_sr70_store */
    case 60: /* ir_sr70_move */
    case 63: /* ir_sr70_hilo */
    case 64: /* ir_sr70_arith */
    case 69: /* ir_sr70_icmp */
    case 84: /* ir_sr70_nop */

      temp = sr71_cpu1_min_issue_delay [(sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu1_translate [insn_code] % 2 + 1) * 4)) & 15;
      res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
      break;

    case 58: /* ir_sr70_fload */
    case 59: /* ir_sr70_fstore */
    case 61: /* ir_sr70_xfer_from */
    case 62: /* ir_sr70_xfer_to */

      temp = sr71_cp1_min_issue_delay [(sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4) / 8];
      temp = (temp >> (8 - (sr71_cp1_translate [insn_code] % 8 + 1) * 1)) & 1;
      res = temp;

      temp = sr71_cpu1_min_issue_delay [(sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu1_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
      break;

    case 65: /* ir_sr70_imul_si */
    case 66: /* ir_sr70_imul_di */
    case 67: /* ir_sr70_idiv_si */
    case 68: /* ir_sr70_idiv_di */

      temp = sr71_imacc_min_issue_delay [sr71_imacc_translate [insn_code] + chip->sr71_imacc_automaton_state * 6];
      res = temp;

      temp = sr71_cpu1_min_issue_delay [(sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu1_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
      break;

    case 70: /* ir_sr70_fadd_sf */
    case 71: /* ir_sr70_fadd_df */
    case 72: /* ir_sr70_fmul_sf */
    case 73: /* ir_sr70_fmul_df */
    case 76: /* ir_sr70_fabs */
    case 77: /* ir_sr70_fcmp */
    case 78: /* ir_sr70_fcvt */

      temp = sr71_cp2_min_issue_delay [(sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6) / 2];
      temp = (temp >> (8 - (sr71_cp2_translate [insn_code] % 2 + 1) * 4)) & 15;
      res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
      break;

    case 74: /* ir_sr70_fdiv_sf */
    case 75: /* ir_sr70_fdiv_df */
    case 79: /* ir_sr70_fsqrt_sf */
    case 80: /* ir_sr70_fsqrt_df */
    case 81: /* ir_sr70_frsqrt_sf */
    case 82: /* ir_sr70_frsqrt_df */

      temp = sr71_fextra_min_issue_delay [sr71_fextra_translate [insn_code] + chip->sr71_fextra_automaton_state * 8];
      res = temp;

      temp = sr71_cp2_min_issue_delay [(sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6) / 2];
      temp = (temp >> (8 - (sr71_cp2_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;
      break;

    case 85: /* $advance_cycle */

      temp = sr71_imacc_min_issue_delay [sr71_imacc_translate [insn_code] + chip->sr71_imacc_automaton_state * 6];
      res = temp;

      temp = sr71_fextra_min_issue_delay [sr71_fextra_translate [insn_code] + chip->sr71_fextra_automaton_state * 8];
      if (temp > res)
        res = temp;

      temp = sr71_cp2_min_issue_delay [(sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6) / 2];
      temp = (temp >> (8 - (sr71_cp2_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = sr71_cp1_min_issue_delay [(sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4) / 8];
      temp = (temp >> (8 - (sr71_cp1_translate [insn_code] % 8 + 1) * 1)) & 1;
      if (temp > res)
        res = temp;

      temp = sr71_cpu1_min_issue_delay [(sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu1_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = sr71_cpu_min_issue_delay [(sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7) / 2];
      temp = (temp >> (8 - (sr71_cpu_translate [insn_code] % 2 + 1) * 4)) & 15;
      if (temp > res)
        res = temp;

      temp = vr55_min_issue_delay [(vr55_translate [insn_code] + chip->vr55_automaton_state * 8) / 8];
      temp = (temp >> (8 - (vr55_translate [insn_code] % 8 + 1) * 1)) & 1;
      if (temp > res)
        res = temp;

      temp = vr54_min_issue_delay [(vr54_translate [insn_code] + chip->vr54_automaton_state * 6) / 8];
      temp = (temp >> (8 - (vr54_translate [insn_code] % 8 + 1) * 1)) & 1;
      if (temp > res)
        res = temp;
      break;


    default:
      res = -1;
      break;
    }
  return res;
}

static int internal_state_transition PARAMS ((int, struct DFA_chip *));
static int
internal_state_transition (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip  ATTRIBUTE_UNUSED;
{
  int temp ATTRIBUTE_UNUSED;

  switch (insn_code)
    {
    case 0: /* ir_vr54_unknown */
    case 1: /* ir_vr54_branch */
    case 2: /* ir_vr54_load */
    case 3: /* ir_vr54_store */
    case 4: /* ir_vr54_fstore */
    case 5: /* ir_vr54_move */
    case 6: /* ir_vr54_xfer */
    case 7: /* ir_vr54_hilo */
    case 8: /* ir_vr54_arith */
    case 9: /* ir_vr54_imul_si */
    case 10: /* ir_vr54_imul_di */
    case 11: /* ir_vr54_imadd_si */
    case 12: /* ir_vr54_idiv_si */
    case 13: /* ir_vr54_idiv_di */
    case 14: /* ir_vr54_fadd */
    case 15: /* ir_vr54_fmul_sf */
    case 16: /* ir_vr54_fmul_df */
    case 17: /* ir_vr54_fmadd_sf */
    case 18: /* ir_vr54_fmadd_df */
    case 19: /* ir_vr54_fdiv_sf */
    case 20: /* ir_vr54_fdiv_df */
    case 21: /* ir_vr54_fabs */
    case 22: /* ir_vr54_fcmp */
    case 23: /* ir_vr54_fcvt */
    case 24: /* ir_vr54_frsqrt_sf */
    case 25: /* ir_vr54_frsqrt_df */
    case 26: /* ir_vr54_multi */
      {

        temp = vr54_transitions [vr54_translate [insn_code] + chip->vr54_automaton_state * 6];
        if (temp >= 12)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->vr54_automaton_state = temp;
        return -1;
      }
    case 27: /* ir_vr55_unknown */
    case 28: /* ir_vr55_branch */
    case 29: /* ir_vr55_load */
    case 30: /* ir_vr55_store */
    case 31: /* ir_vr55_move */
    case 32: /* ir_vr55_xfer */
    case 33: /* ir_vr55_hilo */
    case 34: /* ir_vr55_arith */
    case 35: /* ir_vr55_imul_si */
    case 36: /* ir_vr55_imul_di */
    case 37: /* ir_vr55_imadd_si */
    case 38: /* ir_vr55_idiv_si */
    case 39: /* ir_vr55_idiv_di */
    case 40: /* ir_vr55_fadd */
    case 41: /* ir_vr55_fmul_sf */
    case 42: /* ir_vr55_fmul_df */
    case 43: /* ir_vr55_fmadd_sf */
    case 44: /* ir_vr55_fmadd_df */
    case 45: /* ir_vr55_fdiv_sf */
    case 46: /* ir_vr55_fdiv_df */
    case 47: /* ir_vr55_fabs */
    case 48: /* ir_vr55_fcmp */
    case 49: /* ir_vr55_fcvt_sf */
    case 50: /* ir_vr55_fcvt_df */
    case 51: /* ir_vr55_frsqrt_sf */
    case 52: /* ir_vr55_frsqrt_df */
    case 53: /* ir_vr55_multi */
      {

        temp = vr55_transitions [vr55_translate [insn_code] + chip->vr55_automaton_state * 8];
        if (temp >= 48)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->vr55_automaton_state = temp;
        return -1;
      }
    case 54: /* ir_sr70_unknown */
    case 83: /* ir_sr70_multi */
      {

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        return -1;
      }
    case 55: /* ir_sr70_branch */
    case 56: /* ir_sr70_load */
    case 57: /* ir_sr70_store */
    case 60: /* ir_sr70_move */
    case 63: /* ir_sr70_hilo */
    case 64: /* ir_sr70_arith */
    case 69: /* ir_sr70_icmp */
    case 84: /* ir_sr70_nop */
      {
        unsigned char _sr71_cpu1_automaton_state;

        temp = sr71_cpu1_transitions [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        if (temp >= 80)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cpu1_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        chip->sr71_cpu1_automaton_state = _sr71_cpu1_automaton_state;
        return -1;
      }
    case 58: /* ir_sr70_fload */
    case 59: /* ir_sr70_fstore */
    case 61: /* ir_sr70_xfer_from */
    case 62: /* ir_sr70_xfer_to */
      {
        unsigned char _sr71_cp1_automaton_state;
        unsigned char _sr71_cpu1_automaton_state;

        temp = sr71_cp1_transitions [sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4];
        if (temp >= 4)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cp1_automaton_state = temp;

        temp = sr71_cpu1_transitions [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        if (temp >= 80)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cpu1_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        chip->sr71_cp1_automaton_state = _sr71_cp1_automaton_state;
        chip->sr71_cpu1_automaton_state = _sr71_cpu1_automaton_state;
        return -1;
      }
    case 65: /* ir_sr70_imul_si */
    case 66: /* ir_sr70_imul_di */
    case 67: /* ir_sr70_idiv_si */
    case 68: /* ir_sr70_idiv_di */
      {
        unsigned char _sr71_imacc_automaton_state;
        unsigned char _sr71_cpu1_automaton_state;

        temp = sr71_imacc_base [chip->sr71_imacc_automaton_state] + sr71_imacc_translate [insn_code];
        if (sr71_imacc_check [temp] != chip->sr71_imacc_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_imacc_automaton_state = sr71_imacc_transitions [temp];

        temp = sr71_cpu1_transitions [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        if (temp >= 80)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cpu1_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        chip->sr71_imacc_automaton_state = _sr71_imacc_automaton_state;
        chip->sr71_cpu1_automaton_state = _sr71_cpu1_automaton_state;
        return -1;
      }
    case 70: /* ir_sr70_fadd_sf */
    case 71: /* ir_sr70_fadd_df */
    case 72: /* ir_sr70_fmul_sf */
    case 73: /* ir_sr70_fmul_df */
    case 76: /* ir_sr70_fabs */
    case 77: /* ir_sr70_fcmp */
    case 78: /* ir_sr70_fcvt */
      {
        unsigned char _sr71_cp2_automaton_state;

        temp = sr71_cp2_transitions [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        if (temp >= 8)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cp2_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        chip->sr71_cp2_automaton_state = _sr71_cp2_automaton_state;
        return -1;
      }
    case 74: /* ir_sr70_fdiv_sf */
    case 75: /* ir_sr70_fdiv_df */
    case 79: /* ir_sr70_fsqrt_sf */
    case 80: /* ir_sr70_fsqrt_df */
    case 81: /* ir_sr70_frsqrt_sf */
    case 82: /* ir_sr70_frsqrt_df */
      {
        unsigned char _sr71_fextra_automaton_state;
        unsigned char _sr71_cp2_automaton_state;

        temp = sr71_fextra_base [chip->sr71_fextra_automaton_state] + sr71_fextra_translate [insn_code];
        if (sr71_fextra_check [temp] != chip->sr71_fextra_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_fextra_automaton_state = sr71_fextra_transitions [temp];

        temp = sr71_cp2_transitions [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        if (temp >= 8)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cp2_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->sr71_cpu_automaton_state = temp;
        chip->sr71_fextra_automaton_state = _sr71_fextra_automaton_state;
        chip->sr71_cp2_automaton_state = _sr71_cp2_automaton_state;
        return -1;
      }
    case 85: /* $advance_cycle */
      {
        unsigned char _sr71_imacc_automaton_state;
        unsigned char _sr71_fextra_automaton_state;
        unsigned char _sr71_cp2_automaton_state;
        unsigned char _sr71_cp1_automaton_state;
        unsigned char _sr71_cpu1_automaton_state;
        unsigned char _sr71_cpu_automaton_state;
        unsigned char _vr55_automaton_state;

        temp = sr71_imacc_base [chip->sr71_imacc_automaton_state] + sr71_imacc_translate [insn_code];
        if (sr71_imacc_check [temp] != chip->sr71_imacc_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_imacc_automaton_state = sr71_imacc_transitions [temp];

        temp = sr71_fextra_base [chip->sr71_fextra_automaton_state] + sr71_fextra_translate [insn_code];
        if (sr71_fextra_check [temp] != chip->sr71_fextra_automaton_state)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_fextra_automaton_state = sr71_fextra_transitions [temp];

        temp = sr71_cp2_transitions [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        if (temp >= 8)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cp2_automaton_state = temp;

        temp = sr71_cp1_transitions [sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4];
        if (temp >= 4)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cp1_automaton_state = temp;

        temp = sr71_cpu1_transitions [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        if (temp >= 80)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cpu1_automaton_state = temp;

        temp = sr71_cpu_transitions [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        if (temp >= 160)
          return internal_min_issue_delay (insn_code, chip);
        else
          _sr71_cpu_automaton_state = temp;

        temp = vr55_transitions [vr55_translate [insn_code] + chip->vr55_automaton_state * 8];
        if (temp >= 48)
          return internal_min_issue_delay (insn_code, chip);
        else
          _vr55_automaton_state = temp;

        temp = vr54_transitions [vr54_translate [insn_code] + chip->vr54_automaton_state * 6];
        if (temp >= 12)
          return internal_min_issue_delay (insn_code, chip);
        else
          chip->vr54_automaton_state = temp;
        chip->sr71_imacc_automaton_state = _sr71_imacc_automaton_state;
        chip->sr71_fextra_automaton_state = _sr71_fextra_automaton_state;
        chip->sr71_cp2_automaton_state = _sr71_cp2_automaton_state;
        chip->sr71_cp1_automaton_state = _sr71_cp1_automaton_state;
        chip->sr71_cpu1_automaton_state = _sr71_cpu1_automaton_state;
        chip->sr71_cpu_automaton_state = _sr71_cpu_automaton_state;
        chip->vr55_automaton_state = _vr55_automaton_state;
        return -1;
      }

    default:
      return -1;
    }
}


static int *dfa_insn_codes;

static int dfa_insn_codes_length;

#ifdef __GNUC__
__inline__
#endif
static int dfa_insn_code PARAMS ((rtx));
static int
dfa_insn_code (insn)
	rtx insn;
{
  int insn_code;
  int temp;

  if (INSN_UID (insn) >= dfa_insn_codes_length)
    {
      temp = dfa_insn_codes_length;
      dfa_insn_codes_length = 2 * INSN_UID (insn);
      dfa_insn_codes = xrealloc (dfa_insn_codes, dfa_insn_codes_length * sizeof (int));
      for (; temp < dfa_insn_codes_length; temp++)
        dfa_insn_codes [temp] = -1;
    }
  if ((insn_code = dfa_insn_codes [INSN_UID (insn)]) < 0)
    {
      insn_code = internal_dfa_insn_code (insn);
      dfa_insn_codes [INSN_UID (insn)] = insn_code;
    }
  return insn_code;
}

int
state_transition (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return -1;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_state_transition (insn_code, state);
}


#if AUTOMATON_STATE_ALTS

static int internal_state_alts PARAMS ((int, struct DFA_chip *));
static int
internal_state_alts (insn_code, chip)
	int insn_code;
	struct DFA_chip *chip;
{
  int res;

  switch (insn_code)
    {
    case 0: /* ir_vr54_unknown */
    case 1: /* ir_vr54_branch */
    case 2: /* ir_vr54_load */
    case 3: /* ir_vr54_store */
    case 4: /* ir_vr54_fstore */
    case 5: /* ir_vr54_move */
    case 6: /* ir_vr54_xfer */
    case 7: /* ir_vr54_hilo */
    case 8: /* ir_vr54_arith */
    case 9: /* ir_vr54_imul_si */
    case 10: /* ir_vr54_imul_di */
    case 11: /* ir_vr54_imadd_si */
    case 12: /* ir_vr54_idiv_si */
    case 13: /* ir_vr54_idiv_di */
    case 14: /* ir_vr54_fadd */
    case 15: /* ir_vr54_fmul_sf */
    case 16: /* ir_vr54_fmul_df */
    case 17: /* ir_vr54_fmadd_sf */
    case 18: /* ir_vr54_fmadd_df */
    case 19: /* ir_vr54_fdiv_sf */
    case 20: /* ir_vr54_fdiv_df */
    case 21: /* ir_vr54_fabs */
    case 22: /* ir_vr54_fcmp */
    case 23: /* ir_vr54_fcvt */
    case 24: /* ir_vr54_frsqrt_sf */
    case 25: /* ir_vr54_frsqrt_df */
    case 26: /* ir_vr54_multi */
      {

        res = vr54_state_alts [vr54_translate [insn_code] + chip->vr54_automaton_state * 6];
        break;
      }

    case 27: /* ir_vr55_unknown */
    case 28: /* ir_vr55_branch */
    case 29: /* ir_vr55_load */
    case 30: /* ir_vr55_store */
    case 31: /* ir_vr55_move */
    case 32: /* ir_vr55_xfer */
    case 33: /* ir_vr55_hilo */
    case 34: /* ir_vr55_arith */
    case 35: /* ir_vr55_imul_si */
    case 36: /* ir_vr55_imul_di */
    case 37: /* ir_vr55_imadd_si */
    case 38: /* ir_vr55_idiv_si */
    case 39: /* ir_vr55_idiv_di */
    case 40: /* ir_vr55_fadd */
    case 41: /* ir_vr55_fmul_sf */
    case 42: /* ir_vr55_fmul_df */
    case 43: /* ir_vr55_fmadd_sf */
    case 44: /* ir_vr55_fmadd_df */
    case 45: /* ir_vr55_fdiv_sf */
    case 46: /* ir_vr55_fdiv_df */
    case 47: /* ir_vr55_fabs */
    case 48: /* ir_vr55_fcmp */
    case 49: /* ir_vr55_fcvt_sf */
    case 50: /* ir_vr55_fcvt_df */
    case 51: /* ir_vr55_frsqrt_sf */
    case 52: /* ir_vr55_frsqrt_df */
    case 53: /* ir_vr55_multi */
      {

        res = vr55_state_alts [vr55_translate [insn_code] + chip->vr55_automaton_state * 8];
        break;
      }

    case 54: /* ir_sr70_unknown */
    case 83: /* ir_sr70_multi */
      {

        res = sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 55: /* ir_sr70_branch */
    case 56: /* ir_sr70_load */
    case 57: /* ir_sr70_store */
    case 60: /* ir_sr70_move */
    case 63: /* ir_sr70_hilo */
    case 64: /* ir_sr70_arith */
    case 69: /* ir_sr70_icmp */
    case 84: /* ir_sr70_nop */
      {

        res = sr71_cpu1_state_alts [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 58: /* ir_sr70_fload */
    case 59: /* ir_sr70_fstore */
    case 61: /* ir_sr70_xfer_from */
    case 62: /* ir_sr70_xfer_to */
      {

        res = sr71_cp1_state_alts [sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4];
        res += sr71_cpu1_state_alts [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 65: /* ir_sr70_imul_si */
    case 66: /* ir_sr70_imul_di */
    case 67: /* ir_sr70_idiv_si */
    case 68: /* ir_sr70_idiv_di */
      {
        int temp;

        temp = sr71_imacc_base_state_alts [chip->sr71_imacc_automaton_state] + sr71_imacc_translate [insn_code];
        if (sr71_imacc_check_state_alts [temp] != chip->sr71_imacc_automaton_state)
          return 0;
        else
          res = sr71_imacc_state_alts [temp];
        res += sr71_cpu1_state_alts [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 70: /* ir_sr70_fadd_sf */
    case 71: /* ir_sr70_fadd_df */
    case 72: /* ir_sr70_fmul_sf */
    case 73: /* ir_sr70_fmul_df */
    case 76: /* ir_sr70_fabs */
    case 77: /* ir_sr70_fcmp */
    case 78: /* ir_sr70_fcvt */
      {

        res = sr71_cp2_state_alts [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 74: /* ir_sr70_fdiv_sf */
    case 75: /* ir_sr70_fdiv_df */
    case 79: /* ir_sr70_fsqrt_sf */
    case 80: /* ir_sr70_fsqrt_df */
    case 81: /* ir_sr70_frsqrt_sf */
    case 82: /* ir_sr70_frsqrt_df */
      {
        int temp;

        temp = sr71_fextra_base_state_alts [chip->sr71_fextra_automaton_state] + sr71_fextra_translate [insn_code];
        if (sr71_fextra_check_state_alts [temp] != chip->sr71_fextra_automaton_state)
          return 0;
        else
          res = sr71_fextra_state_alts [temp];
        res += sr71_cp2_state_alts [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        break;
      }

    case 85: /* $advance_cycle */
      {
        int temp;

        temp = sr71_imacc_base_state_alts [chip->sr71_imacc_automaton_state] + sr71_imacc_translate [insn_code];
        if (sr71_imacc_check_state_alts [temp] != chip->sr71_imacc_automaton_state)
          return 0;
        else
          res = sr71_imacc_state_alts [temp];

        temp = sr71_fextra_base_state_alts [chip->sr71_fextra_automaton_state] + sr71_fextra_translate [insn_code];
        if (sr71_fextra_check_state_alts [temp] != chip->sr71_fextra_automaton_state)
          return 0;
        else
          res += sr71_fextra_state_alts [temp];
        res += sr71_cp2_state_alts [sr71_cp2_translate [insn_code] + chip->sr71_cp2_automaton_state * 6];
        res += sr71_cp1_state_alts [sr71_cp1_translate [insn_code] + chip->sr71_cp1_automaton_state * 4];
        res += sr71_cpu1_state_alts [sr71_cpu1_translate [insn_code] + chip->sr71_cpu1_automaton_state * 7];
        res += sr71_cpu_state_alts [sr71_cpu_translate [insn_code] + chip->sr71_cpu_automaton_state * 7];
        res += vr55_state_alts [vr55_translate [insn_code] + chip->vr55_automaton_state * 8];
        res += vr54_state_alts [vr54_translate [insn_code] + chip->vr54_automaton_state * 6];
        break;
      }


    default:
      res = 0;
      break;
    }
  return res;
}

int
state_alts (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_state_alts (insn_code, state);
}


#endif /* #if AUTOMATON_STATE_ALTS */

int
min_issue_delay (state, insn)
	state_t state;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;

  return internal_min_issue_delay (insn_code, state);
}

static int internal_state_dead_lock_p PARAMS ((struct DFA_chip *));
static int
internal_state_dead_lock_p (chip)
	struct DFA_chip *chip;
{
  if (vr54_dead_lock [chip->vr54_automaton_state])
    return 1/* TRUE */;
  if (vr55_dead_lock [chip->vr55_automaton_state])
    return 1/* TRUE */;
  if (sr71_cpu_dead_lock [chip->sr71_cpu_automaton_state])
    return 1/* TRUE */;
  if (sr71_cpu1_dead_lock [chip->sr71_cpu1_automaton_state])
    return 1/* TRUE */;
  if (sr71_cp1_dead_lock [chip->sr71_cp1_automaton_state])
    return 1/* TRUE */;
  if (sr71_cp2_dead_lock [chip->sr71_cp2_automaton_state])
    return 1/* TRUE */;
  if (sr71_fextra_dead_lock [chip->sr71_fextra_automaton_state])
    return 1/* TRUE */;
  if (sr71_imacc_dead_lock [chip->sr71_imacc_automaton_state])
    return 1/* TRUE */;
  return 0/* FALSE */;
}

int
state_dead_lock_p (state)
	state_t state;
{
  return internal_state_dead_lock_p (state);
}

int
state_size ()
{
  return sizeof (struct DFA_chip);
}

static void internal_reset PARAMS ((struct DFA_chip *));
static void
internal_reset (chip)
	struct DFA_chip *chip;
{
  memset (chip, 0, sizeof (struct DFA_chip));
}

void
state_reset (state)
	 state_t state;
{
  internal_reset (state);
}

int
min_insn_conflict_delay (state, insn, insn2)
	state_t state;
	rtx insn;
	rtx insn2;
{
  struct DFA_chip DFA_chip;
  int insn_code, insn2_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;


  if (insn2 != 0)
    {
      insn2_code = dfa_insn_code (insn2);
      if (insn2_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn2_code = DFA__ADVANCE_CYCLE;

  memcpy (&DFA_chip, state, sizeof (DFA_chip));
  internal_reset (&DFA_chip);
  if (internal_state_transition (insn_code, &DFA_chip) > 0)
    abort ();
  return internal_min_issue_delay (insn2_code, &DFA_chip);
}

static int internal_insn_latency PARAMS ((int, int, rtx, rtx));
static int
internal_insn_latency (insn_code, insn2_code, insn, insn2)
	int insn_code;
	int insn2_code;
	rtx insn ATTRIBUTE_UNUSED;
	rtx insn2 ATTRIBUTE_UNUSED;
{
  switch (insn_code)
    {
    case 0:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 1:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 2:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 3:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 4:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 5:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 6:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 7:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 8:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 9:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 10:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 11:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 12:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 42 : 0);
    case 13:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 74 : 0);
    case 14:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 15:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 5 : 0);
    case 16:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 17:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 9 : 0);
    case 18:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 10 : 0);
    case 19:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 42 : 0);
    case 20:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 72 : 0);
    case 21:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 22:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 23:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 24:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 61 : 0);
    case 25:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 121 : 0);
    case 26:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 27:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 28:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 29:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 30:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 31:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 32:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 33:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 34:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 35:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 36:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 37:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 3 : 0);
    case 38:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 42 : 0);
    case 39:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 74 : 0);
    case 40:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 41:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 5 : 0);
    case 42:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 43:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 9 : 0);
    case 44:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 10 : 0);
    case 45:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 30 : 0);
    case 46:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 59 : 0);
    case 47:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 48:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 49:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 50:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 51:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 60 : 0);
    case 52:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 118 : 0);
    case 53:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 54:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 55:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 56:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 2 : 0);
    case 57:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 58:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 9 : 0);
    case 59:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 60:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 61:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 62:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 9 : 0);
    case 63:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 64:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 65:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 66:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 6 : 0);
    case 67:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 41 : 0);
    case 68:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 73 : 0);
    case 69:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 70:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 8 : 0);
    case 71:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 10 : 0);
    case 72:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 8 : 0);
    case 73:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 16 : 0);
    case 74:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 60 : 0);
    case 75:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 120 : 0);
    case 76:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 4 : 0);
    case 77:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 10 : 0);
    case 78:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 12 : 0);
    case 79:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 62 : 0);
    case 80:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 122 : 0);
    case 81:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 48 : 0);
    case 82:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 240 : 0);
    case 83:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 84:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 1 : 0);
    case 85:
      return (insn2_code != DFA__ADVANCE_CYCLE ? 0 : 0);
    default:
      return 0;
    }
}

int
insn_latency (insn, insn2)
	rtx insn;
	rtx insn2;
{
  int insn_code, insn2_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn_code = DFA__ADVANCE_CYCLE;


  if (insn2 != 0)
    {
      insn2_code = dfa_insn_code (insn2);
      if (insn2_code > DFA__ADVANCE_CYCLE)
        return 0;
    }
  else
    insn2_code = DFA__ADVANCE_CYCLE;

  return internal_insn_latency (insn_code, insn2_code, insn, insn2);
}

void
print_reservation (f, insn)
	FILE *f;
	rtx insn;
{
  int insn_code;

  if (insn != 0)
    {
      insn_code = dfa_insn_code (insn);
      if (insn_code > DFA__ADVANCE_CYCLE)
        {
          fprintf (f, "nothing");
          return;
        }
    }
  else
    {
      fprintf (f, "nothing");
      return;
    }
  switch (insn_code)
    {
    case 0:
      fprintf (f, "(vr54_dp0+vr54_dp1+vr54_mem+vr54_mac)");
      break;
    case 1:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 2:
      fprintf (f, "vr54_mem");
      break;
    case 3:
      fprintf (f, "vr54_mem");
      break;
    case 4:
      fprintf (f, "vr54_mem");
      break;
    case 5:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 6:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 7:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 8:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 9:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 10:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 11:
      fprintf (f, "vr54_mac");
      break;
    case 12:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 13:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 14:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 15:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 16:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 17:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 18:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 19:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 20:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 21:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 22:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 23:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 24:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 25:
      fprintf (f, "vr54_dp0|vr54_dp1");
      break;
    case 26:
      fprintf (f, "(vr54_dp0+vr54_dp1+vr54_mem+vr54_mac)");
      break;
    case 27:
      fprintf (f, "(vr55_dp0+vr55_dp1+vr55_mem+vr55_mac+vr55_fp+vr55_bru)");
      break;
    case 28:
      fprintf (f, "vr55_bru");
      break;
    case 29:
      fprintf (f, "vr55_mem");
      break;
    case 30:
      fprintf (f, "vr55_mem");
      break;
    case 31:
      fprintf (f, "vr55_dp0|vr55_dp1");
      break;
    case 32:
      fprintf (f, "vr55_dp0|vr55_dp1");
      break;
    case 33:
      fprintf (f, "vr55_dp0|vr55_dp1");
      break;
    case 34:
      fprintf (f, "vr55_dp0|vr55_dp1");
      break;
    case 35:
      fprintf (f, "vr55_mac");
      break;
    case 36:
      fprintf (f, "vr55_mac");
      break;
    case 37:
      fprintf (f, "vr55_mac");
      break;
    case 38:
      fprintf (f, "vr55_mac");
      break;
    case 39:
      fprintf (f, "vr55_mac");
      break;
    case 40:
      fprintf (f, "vr55_fp");
      break;
    case 41:
      fprintf (f, "vr55_mac");
      break;
    case 42:
      fprintf (f, "vr55_mac");
      break;
    case 43:
      fprintf (f, "vr55_mac");
      break;
    case 44:
      fprintf (f, "vr55_mac");
      break;
    case 45:
      fprintf (f, "vr55_mac");
      break;
    case 46:
      fprintf (f, "vr55_mac");
      break;
    case 47:
      fprintf (f, "vr55_fp");
      break;
    case 48:
      fprintf (f, "vr55_fp");
      break;
    case 49:
      fprintf (f, "vr55_fp");
      break;
    case 50:
      fprintf (f, "vr55_fp");
      break;
    case 51:
      fprintf (f, "vr55_mac");
      break;
    case 52:
      fprintf (f, "vr55_mac");
      break;
    case 53:
      fprintf (f, "(vr55_dp0+vr55_dp1+vr55_mem+vr55_mac+vr55_fp+vr55_bru)");
      break;
    case 54:
      fprintf (f, "serial_dispatch");
      break;
    case 55:
      fprintf (f, "ri_branch");
      break;
    case 56:
      fprintf (f, "ri_mem");
      break;
    case 57:
      fprintf (f, "ri_mem");
      break;
    case 58:
      fprintf (f, "(cpu_iss+cp1_iss),(ri_mem+rf_ldmem)");
      break;
    case 59:
      fprintf (f, "(cpu_iss+cp1_iss),(fpu_mov+ri_mem)");
      break;
    case 60:
      fprintf (f, "ri_insns");
      break;
    case 61:
      fprintf (f, "(cpu_iss+cp1_iss),(fpu_mov+ri_mem)");
      break;
    case 62:
      fprintf (f, "(cpu_iss+cp1_iss),(ri_mem+rf_ldmem)");
      break;
    case 63:
      fprintf (f, "ri_insns");
      break;
    case 64:
      fprintf (f, "ri_insns");
      break;
    case 65:
      fprintf (f, "ri_alux,ipu_alux,ipu_macc_iter");
      break;
    case 66:
      fprintf (f, "ri_alux,ipu_alux,ipu_macc_iter*3");
      break;
    case 67:
      fprintf (f, "ri_alux,ipu_alux,ipu_macc_iter*38");
      break;
    case 68:
      fprintf (f, "ri_alux,ipu_alux,ipu_macc_iter*70");
      break;
    case 69:
      fprintf (f, "ri_insns");
      break;
    case 70:
      fprintf (f, "rf_insn,fpu_fpu");
      break;
    case 71:
      fprintf (f, "rf_insn,fpu_fpu");
      break;
    case 72:
      fprintf (f, "rf_insn,fpu_fpu");
      break;
    case 73:
      fprintf (f, "rf_insn,fpu_fpu*6");
      break;
    case 74:
      fprintf (f, "(rf_multi1+fpu_iter*51)");
      break;
    case 75:
      fprintf (f, "(rf_multi1+fpu_iter*109)");
      break;
    case 76:
      fprintf (f, "rf_insn,fpu_fpu");
      break;
    case 77:
      fprintf (f, "rf_insn,fpu_fpu");
      break;
    case 78:
      fprintf (f, "rf_insn,fpu_fpu*4");
      break;
    case 79:
      fprintf (f, "(rf_multi1+fpu_iter*53)");
      break;
    case 80:
      fprintf (f, "(rf_multi1+fpu_iter*111)");
      break;
    case 81:
      fprintf (f, "(rf_multi1+fpu_iter*39)");
      break;
    case 82:
      fprintf (f, "(rf_multi1+fpu_iter*229)");
      break;
    case 83:
      fprintf (f, "serial_dispatch");
      break;
    case 84:
      fprintf (f, "ri_insns");
      break;
    default:
      fprintf (f, "nothing");
    }
}

void
dfa_start ()
{
  int i;

  dfa_insn_codes_length = get_max_uid ();
  dfa_insn_codes = (int *) xmalloc (dfa_insn_codes_length * sizeof (int));
  for (i = 0; i < dfa_insn_codes_length; i++)
    dfa_insn_codes [i] = -1;
}

void
dfa_finish ()
{
  free (dfa_insn_codes);
}

int
const_num_delay_slots (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    default:
      return 1;
    }
}

int length_unit_log = 0;
