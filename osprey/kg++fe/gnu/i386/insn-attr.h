/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Generated automatically by the program `genattr'
   from the machine description file `md'.  */

#ifndef GCC_INSN_ATTR_H
#define GCC_INSN_ATTR_H

#define HAVE_ATTR_alternative
#define get_attr_alternative(insn) which_alternative
#define HAVE_ATTR_cpu
enum attr_cpu {CPU_I386, CPU_I486, CPU_PENTIUM, CPU_PENTIUMPRO, CPU_K6, CPU_ATHLON, CPU_PENTIUM4};
extern enum attr_cpu get_attr_cpu PARAMS ((void));

#define HAVE_ATTR_type
enum attr_type {TYPE_OTHER, TYPE_MULTI, TYPE_ALU, TYPE_ALU1, TYPE_NEGNOT, TYPE_IMOV, TYPE_IMOVX, TYPE_LEA, TYPE_INCDEC, TYPE_ISHIFT, TYPE_ISHIFT1, TYPE_ROTATE, TYPE_ROTATE1, TYPE_IMUL, TYPE_IDIV, TYPE_ICMP, TYPE_TEST, TYPE_IBR, TYPE_SETCC, TYPE_ICMOV, TYPE_PUSH, TYPE_POP, TYPE_CALL, TYPE_CALLV, TYPE_STR, TYPE_CLD, TYPE_FMOV, TYPE_FOP, TYPE_FSGN, TYPE_FMUL, TYPE_FDIV, TYPE_FPSPC, TYPE_FCMOV, TYPE_FCMP, TYPE_FXCH, TYPE_FISTP, TYPE_SSELOG, TYPE_SSEIADD, TYPE_SSEISHFT, TYPE_SSEIMUL, TYPE_SSE, TYPE_SSEMOV, TYPE_SSEADD, TYPE_SSEMUL, TYPE_SSECMP, TYPE_SSECVT, TYPE_SSEDIV, TYPE_MMX, TYPE_MMXMOV, TYPE_MMXADD, TYPE_MMXMUL, TYPE_MMXCMP, TYPE_MMXCVT, TYPE_MMXSHFT};
extern enum attr_type get_attr_type PARAMS ((rtx));

#define HAVE_ATTR_mode
enum attr_mode {MODE_UNKNOWN, MODE_NONE, MODE_QI, MODE_HI, MODE_SI, MODE_DI, MODE_UNKNOWNFP, MODE_SF, MODE_DF, MODE_XF, MODE_TI, MODE_V4SF, MODE_V2DF, MODE_V2SF};
extern enum attr_mode get_attr_mode PARAMS ((rtx));

#define HAVE_ATTR_unit
enum attr_unit {UNIT_INTEGER, UNIT_I387, UNIT_SSE, UNIT_MMX, UNIT_UNKNOWN};
extern enum attr_unit get_attr_unit PARAMS ((rtx));

#define HAVE_ATTR_length_immediate
extern int get_attr_length_immediate PARAMS ((rtx));
#define HAVE_ATTR_length_address
extern int get_attr_length_address PARAMS ((rtx));
#define HAVE_ATTR_prefix_data16
extern int get_attr_prefix_data16 PARAMS ((rtx));
#define HAVE_ATTR_prefix_rep
extern int get_attr_prefix_rep PARAMS ((rtx));
#define HAVE_ATTR_prefix_0f
extern int get_attr_prefix_0f PARAMS ((rtx));
#define HAVE_ATTR_modrm
extern int get_attr_modrm PARAMS ((rtx));
#define HAVE_ATTR_length
extern int get_attr_length PARAMS ((rtx));
extern void shorten_branches PARAMS ((rtx));
extern int insn_default_length PARAMS ((rtx));
extern int insn_variable_length_p PARAMS ((rtx));
extern int insn_current_length PARAMS ((rtx));

#include "insn-addr.h"

#define HAVE_ATTR_memory
enum attr_memory {MEMORY_NONE, MEMORY_LOAD, MEMORY_STORE, MEMORY_BOTH, MEMORY_UNKNOWN};
extern enum attr_memory get_attr_memory PARAMS ((rtx));

#define HAVE_ATTR_imm_disp
enum attr_imm_disp {IMM_DISP_FALSE, IMM_DISP_TRUE, IMM_DISP_UNKNOWN};
extern enum attr_imm_disp get_attr_imm_disp PARAMS ((rtx));

#define HAVE_ATTR_fp_int_src
enum attr_fp_int_src {FP_INT_SRC_FALSE, FP_INT_SRC_TRUE};
extern enum attr_fp_int_src get_attr_fp_int_src PARAMS ((rtx));

#define HAVE_ATTR_pent_prefix
enum attr_pent_prefix {PENT_PREFIX_FALSE, PENT_PREFIX_TRUE};
extern enum attr_pent_prefix get_attr_pent_prefix PARAMS ((rtx));

#define HAVE_ATTR_pent_pair
enum attr_pent_pair {PENT_PAIR_UV, PENT_PAIR_PU, PENT_PAIR_PV, PENT_PAIR_NP};
extern enum attr_pent_pair get_attr_pent_pair PARAMS ((rtx));

#define HAVE_ATTR_ppro_uops
enum attr_ppro_uops {PPRO_UOPS_ONE, PPRO_UOPS_FEW, PPRO_UOPS_MANY};
extern enum attr_ppro_uops get_attr_ppro_uops PARAMS ((rtx));

#define HAVE_ATTR_athlon_decode
enum attr_athlon_decode {ATHLON_DECODE_DIRECT, ATHLON_DECODE_VECTOR};
extern enum attr_athlon_decode get_attr_athlon_decode PARAMS ((rtx));

#define HAVE_ATTR_athlon_fpunits
enum attr_athlon_fpunits {ATHLON_FPUNITS_NONE, ATHLON_FPUNITS_STORE, ATHLON_FPUNITS_MUL, ATHLON_FPUNITS_ADD, ATHLON_FPUNITS_MULADD, ATHLON_FPUNITS_ANY};
extern enum attr_athlon_fpunits get_attr_athlon_fpunits PARAMS ((rtx));

#define TRADITIONAL_PIPELINE_INTERFACE 1
#define DFA_PIPELINE_INTERFACE 1
#define INSN_SCHEDULING

extern int result_ready_cost PARAMS ((rtx));
extern int function_units_used PARAMS ((rtx));

extern const struct function_unit_desc
{
  const char *const name;
  const int bitmask;
  const int multiplicity;
  const int simultaneity;
  const int default_cost;
  const int max_issue_delay;
  int (*const ready_cost_function) PARAMS ((rtx));
  int (*const conflict_cost_function) PARAMS ((rtx, rtx));
  const int max_blockage;
  unsigned int (*const blockage_range_function) PARAMS ((rtx));
  int (*const blockage_function) PARAMS ((rtx, rtx));
} function_units[];

#define FUNCTION_UNITS_SIZE 21
#define MIN_MULTIPLICITY 1
#define MAX_MULTIPLICITY 3
#define MIN_SIMULTANEITY 0
#define MAX_SIMULTANEITY 1
#define MIN_READY_COST 1
#define MAX_READY_COST 100
#define MIN_ISSUE_DELAY 1
#define MAX_ISSUE_DELAY 56
#define MIN_BLOCKAGE 1
#define MAX_BLOCKAGE 99
#define BLOCKAGE_BITS 8
#define INSN_QUEUE_SIZE 128

/* DFA based pipeline interface.  */
#ifndef AUTOMATON_STATE_ALTS
#define AUTOMATON_STATE_ALTS 0
#endif

#ifndef CPU_UNITS_QUERY
#define CPU_UNITS_QUERY 0
#endif

extern int max_dfa_issue_rate;

/* The following macro value is calculated from the
   automaton based pipeline description and is equal to
   maximal number of all insns described in constructions
   `define_insn_reservation' which can be issued on the
   same processor cycle. */
#define MAX_DFA_ISSUE_RATE max_dfa_issue_rate

/* Insn latency time defined in define_insn_reservation. */
extern int insn_default_latency PARAMS ((rtx));

/* Return nonzero if there is a bypass for given insn
   which is a data producer.  */
extern int bypass_p PARAMS ((rtx));

/* Insn latency time on data consumed by the 2nd insn.
   Use the function if bypass_p returns nonzero for
   the 1st insn. */
extern int insn_latency PARAMS ((rtx, rtx));

/* The following function returns number of alternative
   reservations of given insn.  It may be used for better
   insns scheduling heuristics. */
extern int insn_alts PARAMS ((rtx));

/* Maximal possible number of insns waiting results being
   produced by insns whose execution is not finished. */
extern int max_insn_queue_index;

/* Pointer to data describing current state of DFA.  */
typedef void *state_t;

/* Size of the data in bytes.  */
extern int state_size PARAMS ((void));

/* Initiate given DFA state, i.e. Set up the state
   as all functional units were not reserved.  */
extern void state_reset PARAMS ((state_t));
/* The following function returns negative value if given
   insn can be issued in processor state described by given
   DFA state.  In this case, the DFA state is changed to
   reflect the current and future reservations by given
   insn.  Otherwise the function returns minimal time
   delay to issue the insn.  This delay may be zero
   for superscalar or VLIW processors.  If the second
   parameter is NULL the function changes given DFA state
   as new processor cycle started.  */
extern int state_transition PARAMS ((state_t, rtx));

#if AUTOMATON_STATE_ALTS
/* The following function returns number of possible
   alternative reservations of given insn in given
   DFA state.  It may be used for better insns scheduling
   heuristics.  By default the function is defined if
   macro AUTOMATON_STATE_ALTS is defined because its
   implementation may require much memory.  */
extern int state_alts PARAMS ((state_t, rtx));
#endif

extern int min_issue_delay PARAMS ((state_t, rtx));
/* The following function returns nonzero if no one insn
   can be issued in current DFA state. */
extern int state_dead_lock_p PARAMS ((state_t));
/* The function returns minimal delay of issue of the 2nd
   insn after issuing the 1st insn in given DFA state.
   The 1st insn should be issued in given state (i.e.
    state_transition should return negative value for
    the insn and the state).  Data dependencies between
    the insns are ignored by the function.  */
extern int min_insn_conflict_delay PARAMS ((state_t, rtx, rtx));
/* The following function outputs reservations for given
   insn as they are described in the corresponding
   define_insn_reservation.  */
extern void print_reservation PARAMS ((FILE *, rtx));

#if CPU_UNITS_QUERY
/* The following function returns code of functional unit
   with given name (see define_cpu_unit). */
extern int get_cpu_unit_code PARAMS ((const char *));
/* The following function returns nonzero if functional
   unit with given code is currently reserved in given
   DFA state.  */
extern int cpu_unit_reservation_p PARAMS ((state_t, int));
#endif

/* Initiate and finish work with DFA.  They should be
   called as the first and the last interface
   functions.  */
extern void dfa_start PARAMS ((void));
extern void dfa_finish PARAMS ((void));

#define ATTR_FLAG_forward	0x1
#define ATTR_FLAG_backward	0x2
#define ATTR_FLAG_likely	0x4
#define ATTR_FLAG_very_likely	0x8
#define ATTR_FLAG_unlikely	0x10
#define ATTR_FLAG_very_unlikely	0x20

#endif /* GCC_INSN_ATTR_H */
