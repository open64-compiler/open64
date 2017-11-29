/* Generated automatically by the program `genextract'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "toplev.h"

static rtx junk ATTRIBUTE_UNUSED;
void
insn_extract (insn)
     rtx insn;
{
  rtx *ro = recog_data.operand;
  rtx **ro_loc = recog_data.operand_loc;
  rtx pat = PATTERN (insn);
  int i ATTRIBUTE_UNUSED;

  memset (ro, 0, sizeof (*ro) * MAX_RECOG_OPERANDS);
  memset (ro_loc, 0, sizeof (*ro_loc) * MAX_RECOG_OPERANDS);
  switch (INSN_CODE (insn))
    {
    case -1:
      fatal_insn_not_found (insn);

    case 532:
    case 531:
    case 530:
    case 529:
      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
          ro[i] = *(ro_loc[i] = &XVECEXP (pat, 0, i));
      break;

    case 330:  /* *mips.md:10869 */
    case 327:  /* *mips.md:10823 */
    case 324:  /* *mips.md:10777 */
    case 321:  /* *mips.md:10731 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 2));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 329:  /* *mips.md:10854 */
    case 328:  /* *mips.md:10839 */
    case 326:  /* *mips.md:10808 */
    case 325:  /* *mips.md:10793 */
    case 323:  /* *mips.md:10762 */
    case 322:  /* *mips.md:10747 */
    case 320:  /* *mips.md:10716 */
    case 319:  /* *mips.md:10701 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 317:  /* prefetch_di */
    case 315:  /* prefetch_si */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      ro[2] = *(ro_loc[2] = &XEXP (pat, 2));
      break;

    case 316:  /* prefetch_di_address */
    case 314:  /* prefetch_si_address */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      ro[2] = *(ro_loc[2] = &XEXP (pat, 2));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 0), 1));
      break;

    case 313:  /* call_value_multiple_internal2 */
    case 312:  /* call_value_multiple_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 311:  /* call_value_internal4b */
    case 310:  /* call_value_internal4a */
    case 309:  /* call_value_internal3c */
    case 308:  /* call_value_internal3b */
    case 307:  /* call_value_internal3a */
    case 306:  /* call_value_internal2 */
    case 305:  /* call_value_internal1 */
    case 304:  /* *mips.md:10399 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 303:  /* call_internal4b */
    case 302:  /* call_internal4a */
    case 301:  /* call_internal3c */
    case 300:  /* call_internal3b */
    case 299:  /* call_internal3a */
    case 298:  /* call_internal2 */
    case 297:  /* call_internal1 */
    case 296:  /* *mips.md:10194 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 294:  /* eh_set_lr_di */
    case 293:  /* eh_set_lr_si */
      ro[0] = *(ro_loc[0] = &XVECEXP (XVECEXP (pat, 0, 0), 0, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 292:  /* get_fnaddr */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      break;

    case 291:  /* return_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      break;

    case 288:  /* casesi_internal_di */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 287:  /* casesi_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 286:  /* *mips.md:9828 */
    case 285:  /* *mips.md:9797 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0);
      recog_data.dup_num[0] = 1;
      break;

    case 284:  /* tablejump_internal2 */
    case 283:  /* tablejump_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 0), 0));
      break;

    case 282:  /* indirect_jump_internal2 */
    case 281:  /* indirect_jump_internal1 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 1));
      break;

    case 280:  /* *mips.md:9633 */
    case 279:  /* jump */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 230:  /* *mips.md:8168 */
    case 229:  /* *mips.md:8139 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 2));
      break;

    case 228:  /* branch_equality_di_inverted */
    case 227:  /* branch_equality_inverted */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 2), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 226:  /* branch_equality_di */
    case 225:  /* branch_equality */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 224:  /* branch_zero_di_inverted */
    case 222:  /* branch_zero_inverted */
    case 220:  /* branch_fp_inverted */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 2), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 223:  /* branch_zero_di */
    case 221:  /* branch_zero */
    case 219:  /* branch_fp */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 194:  /* movstrsi_internal3 */
    case 193:  /* *mips.md:6612 */
    case 192:  /* movstrsi_internal2 */
    case 191:  /* *mips.md:6531 */
    case 190:  /* movstrsi_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XVECEXP (pat, 0, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 5), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 6), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 189:  /* loadgp */
      ro[0] = *(ro_loc[0] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0, 1));
      break;

    case 177:  /* *mips.md:6136 */
    case 176:  /* *mips.md:6127 */
    case 175:  /* *mips.md:6118 */
    case 174:  /* *mips.md:6109 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 1));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 0), 0), 1));
      break;

    case 336:  /* consttable_df */
    case 335:  /* consttable_sf */
    case 334:  /* consttable_di */
    case 333:  /* consttable_si */
    case 332:  /* consttable_hi */
    case 331:  /* consttable_qi */
    case 168:  /* hilo_delay */
      ro[0] = *(ro_loc[0] = &XVECEXP (pat, 0, 0));
      break;

    case 165:  /* *mips.md:5699 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XEXP (pat, 0), 0), 1));
      break;

    case 341:  /* leadi */
    case 340:  /* leasi */
    case 188:  /* *mips.md:6450 */
    case 187:  /* movdf_internal2 */
    case 186:  /* movdf_internal1a */
    case 185:  /* movdf_internal1 */
    case 184:  /* *mips.md:6389 */
    case 183:  /* movsf_internal2 */
    case 182:  /* movsf_internal1 */
    case 181:  /* *mips.md:6296 */
    case 180:  /* movqi_internal */
    case 179:  /* *mips.md:6186 */
    case 178:  /* movhi_internal */
    case 169:  /* movcc */
    case 167:  /* *mips.md:5738 */
    case 166:  /* movsi_internal */
    case 164:  /* *movdi_internal2_mips16 */
    case 162:  /* movdi_internal2 */
    case 161:  /* *mips.md:5274 */
    case 160:  /* movdi_internal */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (pat, 1));
      break;

    case 159:  /* *mips.md:5248 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      break;

    case 156:  /* movdi_usd */
    case 155:  /* movdi_uld */
    case 154:  /* movsi_usw */
    case 153:  /* movsi_ulw */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XVECEXP (XEXP (pat, 1), 0, 0));
      break;

    case 135:  /* *paradoxical_extendhidi2 */
    case 124:  /* *mips.md:4052 */
    case 123:  /* *mips.md:4043 */
    case 122:  /* *mips.md:4034 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      break;

    case 114:  /* *nordi3 */
    case 113:  /* *norsi3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 146:  /* fix_truncsfsi2_macro */
    case 144:  /* fix_truncdfsi2_macro */
    case 92:  /* negdi2_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 90:  /* ffsdi2 */
    case 89:  /* ffssi2 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 84:  /* *mips.md:3230 */
    case 83:  /* *mips.md:3221 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      break;

    case 236:  /* sne_di_zero */
    case 235:  /* sne_si_zero */
    case 234:  /* *mips.md:8539 */
    case 233:  /* seq_di_zero */
    case 232:  /* *mips.md:8521 */
    case 231:  /* seq_si_zero */
    case 163:  /* *movdi_internal2_extend */
    case 157:  /* high */
    case 152:  /* floatdisf2 */
    case 151:  /* floatsisf2 */
    case 150:  /* floatdidf2 */
    case 149:  /* floatsidf2 */
    case 148:  /* fix_truncsfdi2 */
    case 147:  /* fix_truncdfdi2 */
    case 145:  /* fix_truncsfsi2_insn */
    case 143:  /* fix_truncdfsi2_insn */
    case 142:  /* extendsfdf2 */
    case 141:  /* extendqidi2_insn */
    case 140:  /* extendqisi2_insn */
    case 139:  /* extendqihi2_internal */
    case 138:  /* extendhisi2_internal */
    case 137:  /* extendhidi2_internal */
    case 136:  /* *mips.md:4311 */
    case 134:  /* *mips.md:4278 */
    case 133:  /* *mips.md:4253 */
    case 132:  /* *mips.md:4238 */
    case 131:  /* *mips.md:4213 */
    case 130:  /* *mips.md:4198 */
    case 129:  /* *mips.md:4172 */
    case 128:  /* *mips.md:4157 */
    case 127:  /* *mips.md:4132 */
    case 126:  /* *mips.md:4117 */
    case 125:  /* zero_extendsidi2_internal */
    case 118:  /* truncdiqi2 */
    case 117:  /* truncdihi2 */
    case 116:  /* truncdisi2 */
    case 115:  /* truncdfsf2 */
    case 97:  /* one_cmpldi2 */
    case 96:  /* one_cmplsi2 */
    case 95:  /* negsf2 */
    case 94:  /* negdf2 */
    case 93:  /* negdi2_internal_2 */
    case 91:  /* negsi2 */
    case 88:  /* abssf2 */
    case 87:  /* absdf2 */
    case 86:  /* absdi2 */
    case 85:  /* abssi2 */
    case 82:  /* sqrtsf2 */
    case 81:  /* sqrtdf2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      break;

    case 72:  /* div_trap_mips16 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (XVECEXP (pat, 0, 0), 1));
      break;

    case 71:  /* div_trap_normal */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 1));
      ro[2] = *(ro_loc[2] = &XEXP (pat, 1));
      break;

    case 70:  /* udivmoddi4_internal */
    case 69:  /* udivmodsi4_internal */
    case 68:  /* divmoddi4_internal */
    case 67:  /* divmodsi4_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 0);
      recog_data.dup_num[0] = 1;
      recog_data.dup_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 1), 1), 1);
      recog_data.dup_num[1] = 2;
      break;

    case 62:  /* *mips.md:2572 */
    case 61:  /* *mips.md:2562 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 1), 1));
      break;

    case 60:  /* *mips.md:2552 */
    case 59:  /* *mips.md:2542 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (pat, 1), 0), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 58:  /* *mips.md:2531 */
    case 57:  /* *mips.md:2521 */
    case 56:  /* *mips.md:2511 */
    case 55:  /* *mips.md:2501 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 54:  /* *mul_acc_64bit_di */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1);
      recog_data.dup_num[0] = 0;
      break;

    case 53:  /* *mul_acc_di */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1);
      recog_data.dup_num[0] = 0;
      break;

    case 52:  /* madsi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      recog_data.dup_loc[0] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1);
      recog_data.dup_num[0] = 0;
      break;

    case 51:  /* umuldi3_highpart */
    case 50:  /* smuldi3_highpart */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 49:  /* *xmulsi3_neg_highpart_mulhi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[8] = *(ro_loc[8] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 48:  /* xmulsi3_highpart_mulhi */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[8] = *(ro_loc[8] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 47:  /* xmulsi3_highpart_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 46:  /* *msac_di */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 45:  /* *muls_di */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 44:  /* mulsidi3_64bit */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 43:  /* mulsidi3_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 39:  /* *muls */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 40:  /* *msac */
    case 38:  /* *mul_sub_si */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 0));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 37:  /* *mul_acc_si */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[6] = *(ro_loc[6] = &XEXP (XVECEXP (pat, 0, 3), 0));
      ro[7] = *(ro_loc[7] = &XEXP (XVECEXP (pat, 0, 4), 0));
      break;

    case 80:  /* umoddi3_internal */
    case 79:  /* umodsi3_internal */
    case 78:  /* udivdi3_internal */
    case 77:  /* udivsi3_internal */
    case 76:  /* moddi3_internal */
    case 75:  /* modsi3_internal */
    case 74:  /* divdi3_internal */
    case 73:  /* divsi3_internal */
    case 41:  /* muldi3_internal */
    case 35:  /* mulsi3_internal */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      break;

    case 42:  /* muldi3_internal2 */
    case 36:  /* mulsi3_r4000 */
    case 34:  /* mulsi3_mult3 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      ro[4] = *(ro_loc[4] = &XEXP (XVECEXP (pat, 0, 2), 0));
      ro[5] = *(ro_loc[5] = &XEXP (XVECEXP (pat, 0, 3), 0));
      break;

    case 173:  /* *mips.md:6100 */
    case 172:  /* *mips.md:6091 */
    case 171:  /* *mips.md:6082 */
    case 170:  /* *mips.md:6073 */
    case 121:  /* *mips.md:4009 */
    case 120:  /* *mips.md:3983 */
    case 119:  /* *mips.md:3959 */
    case 29:  /* *mips.md:1655 */
    case 28:  /* subsi3_internal_2 */
    case 15:  /* *mips.md:1157 */
    case 14:  /* addsi3_internal_2 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XEXP (pat, 1), 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XEXP (pat, 1), 0), 1));
      break;

    case 214:  /* lshrdi3_internal3 */
    case 213:  /* lshrdi3_internal2 */
    case 212:  /* lshrdi3_internal */
    case 206:  /* ashrdi3_internal3 */
    case 205:  /* ashrdi3_internal2 */
    case 204:  /* ashrdi3_internal */
    case 199:  /* ashldi3_internal3 */
    case 198:  /* ashldi3_internal2 */
    case 197:  /* ashldi3_internal */
    case 23:  /* subdi3_internal_2 */
    case 22:  /* subdi3_internal */
    case 9:  /* adddi3_internal_2 */
    case 8:  /* adddi3_internal_1 */
      ro[0] = *(ro_loc[0] = &XEXP (XVECEXP (pat, 0, 0), 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 1));
      ro[3] = *(ro_loc[3] = &XEXP (XVECEXP (pat, 0, 1), 0));
      break;

    case 26:  /* *mips.md:1534 */
    case 20:  /* *mips.md:1247 */
    case 12:  /* *mips.md:1023 */
    case 6:  /* *mips.md:683 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 25:  /* *mips.md:1521 */
    case 19:  /* *mips.md:1234 */
    case 11:  /* *mips.md:1011 */
    case 5:  /* *mips.md:671 */
      ro[0] = *(ro_loc[0] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 278:  /* sge_sf */
    case 277:  /* sgt_sf */
    case 276:  /* sle_sf */
    case 275:  /* slt_sf */
    case 274:  /* seq_sf */
    case 273:  /* sunle_sf */
    case 272:  /* suneq_sf */
    case 271:  /* sunlt_sf */
    case 270:  /* sunordered_sf */
    case 269:  /* sge_df */
    case 268:  /* sgt_df */
    case 267:  /* sle_df */
    case 266:  /* slt_df */
    case 265:  /* seq_df */
    case 264:  /* sunle_df */
    case 263:  /* suneq_df */
    case 262:  /* sunlt_df */
    case 261:  /* sunordered_df */
    case 260:  /* *mips.md:9301 */
    case 259:  /* sleu_di_const */
    case 258:  /* *mips.md:9272 */
    case 257:  /* sleu_si_const */
    case 256:  /* *mips.md:9219 */
    case 255:  /* sltu_di */
    case 254:  /* *mips.md:9196 */
    case 253:  /* sltu_si */
    case 252:  /* *mips.md:9085 */
    case 251:  /* sgtu_di */
    case 250:  /* *mips.md:9067 */
    case 249:  /* sgtu_si */
    case 248:  /* *mips.md:8969 */
    case 247:  /* sle_di_const */
    case 246:  /* *mips.md:8940 */
    case 245:  /* sle_si_const */
    case 244:  /* *mips.md:8887 */
    case 243:  /* slt_di */
    case 242:  /* *mips.md:8864 */
    case 241:  /* slt_si */
    case 240:  /* *mips.md:8753 */
    case 239:  /* sgt_di */
    case 238:  /* *mips.md:8735 */
    case 237:  /* sgt_si */
    case 218:  /* rotrdi3 */
    case 217:  /* rotrsi3 */
    case 216:  /* *mips.md:7713 */
    case 215:  /* lshrdi3_internal4 */
    case 211:  /* *mips.md:7457 */
    case 210:  /* lshrsi3_internal2 */
    case 209:  /* lshrsi3_internal1 */
    case 208:  /* *mips.md:7329 */
    case 207:  /* ashrdi3_internal4 */
    case 203:  /* ashrsi3_internal2 */
    case 202:  /* ashrsi3_internal1 */
    case 201:  /* *mips.md:6975 */
    case 200:  /* ashldi3_internal4 */
    case 196:  /* ashlsi3_internal2 */
    case 195:  /* ashlsi3_internal1 */
    case 158:  /* low */
    case 112:  /* xordi3_immed */
    case 111:  /* *mips.md:3814 */
    case 110:  /* *mips.md:3804 */
    case 109:  /* *mips.md:3786 */
    case 108:  /* *mips.md:3759 */
    case 107:  /* *mips.md:3748 */
    case 106:  /* *mips.md:3709 */
    case 105:  /* *mips.md:3691 */
    case 104:  /* *mips.md:3672 */
    case 103:  /* *mips.md:3661 */
    case 102:  /* anddi3_internal1 */
    case 101:  /* *mips.md:3604 */
    case 100:  /* *mips.md:3586 */
    case 99:  /* *mips.md:3563 */
    case 98:  /* *mips.md:3552 */
    case 66:  /* *mips.md:2617 */
    case 65:  /* *mips.md:2608 */
    case 64:  /* divsf3 */
    case 63:  /* divdf3 */
    case 33:  /* mulsf3_r4300 */
    case 32:  /* mulsf3_internal */
    case 31:  /* muldf3_r4300 */
    case 30:  /* muldf3_internal */
    case 27:  /* *mips.md:1547 */
    case 24:  /* subdi3_internal_3 */
    case 21:  /* *mips.md:1261 */
    case 18:  /* subsi3_internal */
    case 17:  /* subsf3 */
    case 16:  /* subdf3 */
    case 13:  /* *mips.md:1035 */
    case 10:  /* adddi3_internal_3 */
    case 7:  /* *mips.md:695 */
    case 4:  /* addsi3_internal */
    case 3:  /* addsf3 */
    case 2:  /* adddf3 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 1), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 1), 1));
      break;

    case 1:  /* *mips.md:583 */
      ro[0] = *(ro_loc[0] = &XEXP (pat, 0));
      ro[1] = *(ro_loc[1] = &XEXP (XEXP (pat, 0), 0));
      ro[2] = *(ro_loc[2] = &XEXP (XEXP (pat, 0), 1));
      break;

    case 339:  /* align_8 */
    case 338:  /* align_4 */
    case 337:  /* align_2 */
    case 318:  /* nop */
    case 295:  /* exception_receiver */
    case 290:  /* return */
    case 289:  /* blockage */
    case 0:  /* trap */
      break;

    default:
      abort ();
    }
}
