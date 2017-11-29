/* Generated automatically by the program `genattr'
   from the machine description file `md'.  */

#ifndef GCC_INSN_ATTR_H
#define GCC_INSN_ATTR_H

#define HAVE_ATTR_alternative
#define get_attr_alternative(insn) which_alternative
#define HAVE_ATTR_itanium_class
enum attr_itanium_class {ITANIUM_CLASS_UNKNOWN, ITANIUM_CLASS_IGNORE, ITANIUM_CLASS_STOP_BIT, ITANIUM_CLASS_BR, ITANIUM_CLASS_FCMP, ITANIUM_CLASS_FCVTFX, ITANIUM_CLASS_FLD, ITANIUM_CLASS_FMAC, ITANIUM_CLASS_FMISC, ITANIUM_CLASS_FRAR_I, ITANIUM_CLASS_FRAR_M, ITANIUM_CLASS_FRBR, ITANIUM_CLASS_FRFR, ITANIUM_CLASS_FRPR, ITANIUM_CLASS_IALU, ITANIUM_CLASS_ICMP, ITANIUM_CLASS_ILOG, ITANIUM_CLASS_ISHF, ITANIUM_CLASS_LD, ITANIUM_CLASS_CHK_S, ITANIUM_CLASS_LONG_I, ITANIUM_CLASS_MMMUL, ITANIUM_CLASS_MMSHF, ITANIUM_CLASS_MMSHFI, ITANIUM_CLASS_RSE_M, ITANIUM_CLASS_SCALL, ITANIUM_CLASS_SEM, ITANIUM_CLASS_STF, ITANIUM_CLASS_ST, ITANIUM_CLASS_SYST_M0, ITANIUM_CLASS_SYST_M, ITANIUM_CLASS_TBIT, ITANIUM_CLASS_TOAR_I, ITANIUM_CLASS_TOAR_M, ITANIUM_CLASS_TOBR, ITANIUM_CLASS_TOFR, ITANIUM_CLASS_TOPR, ITANIUM_CLASS_XMPY, ITANIUM_CLASS_XTD, ITANIUM_CLASS_NOP_B, ITANIUM_CLASS_NOP_F, ITANIUM_CLASS_NOP_I, ITANIUM_CLASS_NOP_M, ITANIUM_CLASS_NOP_X, ITANIUM_CLASS_LFETCH};
extern enum attr_itanium_class get_attr_itanium_class PARAMS ((rtx));

#define HAVE_ATTR_type
enum attr_type {TYPE_UNKNOWN, TYPE_A, TYPE_I, TYPE_M, TYPE_F, TYPE_B, TYPE_L, TYPE_X, TYPE_S};
extern enum attr_type get_attr_type PARAMS ((rtx));

#define HAVE_ATTR_itanium_requires_unit0
enum attr_itanium_requires_unit0 {ITANIUM_REQUIRES_UNIT0_NO, ITANIUM_REQUIRES_UNIT0_YES};
extern enum attr_itanium_requires_unit0 get_attr_itanium_requires_unit0 PARAMS ((rtx));

#define HAVE_ATTR_predicable
enum attr_predicable {PREDICABLE_NO, PREDICABLE_YES};
extern enum attr_predicable get_attr_predicable PARAMS ((rtx));

#define TRADITIONAL_PIPELINE_INTERFACE 1
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

#define FUNCTION_UNITS_SIZE 2
#define MIN_MULTIPLICITY 1
#define MAX_MULTIPLICITY 6
#define MIN_SIMULTANEITY 1
#define MAX_SIMULTANEITY 1
#define MIN_READY_COST 1
#define MAX_READY_COST 13
#define MIN_ISSUE_DELAY 1
#define MAX_ISSUE_DELAY 1
#define MIN_BLOCKAGE 1
#define MAX_BLOCKAGE 13
#define BLOCKAGE_BITS 5
#define INSN_QUEUE_SIZE 16

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
