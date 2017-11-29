/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

/*
  sas, a stand-alone scheduler, which will perform local instruction scheduling
  and software pipelining.

  02-04-2003
*/

#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "cg_loop.h"
#include "gra_live.h"
#include "freq.h"
#include "findloops.h"
#include "register.h"
#include "ti_res_res.h"
#include "ti_res_count.h"
#include "tracing.h"
#include "config_asm.h"
#include "note.h"
#include "cgexp.h"
#include "lra.h"
#include "wn_util.h"
#include "cg_swp_options.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <math.h>

#include "cg_sas.h"

#define TN_has_register(t)  ( TN_register(t) != REGISTER_UNDEFINED )

/* Emit a loop note to the .s file.
 */
void Emit_KEY_SWP_Note( BB* bb, FILE* file )
{
  const ANNOTATION* ant = ANNOT_Get( BB_annotations(bb), ANNOT_ROTATING_KERNEL );
  const ROTATING_KERNEL_INFO* info = ANNOT_rotating_kernel(ant);
  char prefix[20];

  if( ROTATING_KERNEL_INFO_succeeded(info) ){
    const int Kmin = ROTATING_KERNEL_INFO_min_ii(info);
    sprintf( prefix, "%s<swps> ", ASM_CMNT_LINE );
    fprintf( file, "%s\n", prefix );
    fprintf( file, "%s%3d cycles per 1 iteration in steady state\n",
	     prefix, ROTATING_KERNEL_INFO_ii(info) );
    fprintf( file, "%s%3d pipeline stages\n", 
	     prefix, ROTATING_KERNEL_INFO_stage_count(info) );
    fprintf( file, "%s%3d %s of the kernel\n",
	     prefix, Kmin, Kmin == 1 ? "copy" : "copies" );
    fprintf( file, "%s\n", prefix );

    sprintf( prefix, "%s<swps>      ", ASM_CMNT_LINE );

    ISA_REGISTER_CLASS rc;    
    FOR_ALL_ISA_REGISTER_CLASS( rc ){
      const ISA_REGISTER_CLASS_INFO* icinfo = ISA_REGISTER_CLASS_Info( rc );
      const REGISTER_SET tmp = ROTATING_KERNEL_INFO_live_in(info)[rc];
      const int size = REGISTER_SET_Size( tmp );
      if( size > 0 ){
	fprintf( file, "%s%d %s registers are required\n",
		 prefix, size, ISA_REGISTER_CLASS_INFO_Name( icinfo ) );
      }
    }

    fprintf( file, "%s\n", prefix );

    const int mii = std::max( ROTATING_KERNEL_INFO_res_min_ii(info),
			 ROTATING_KERNEL_INFO_rec_min_ii(info) );

    fprintf( file, "%smin %d cycles required by resources\n",
	     prefix, ROTATING_KERNEL_INFO_res_min_ii(info) );
    fprintf( file, "%smin %d cycles required by recurrences\n",
	     prefix, ROTATING_KERNEL_INFO_rec_min_ii(info) );
    fprintf( file, "%smin %d cycles required by resources/recurrence\n", 
	     prefix, mii );
    fprintf( file, "%smin %d cycles (actual %d cycles) required to schedule one iteration\n",
	     prefix, ROTATING_KERNEL_INFO_min_sched_len(info),
	     ROTATING_KERNEL_INFO_sched_len(info) );
    fprintf( file, "%s\n", prefix );
    TI_RES_COUNT_Emit_Note( prefix, file, ROTATING_KERNEL_INFO_res_counts(info), 
			    ROTATING_KERNEL_INFO_ii(info) );
    fprintf( file, "%s\n", prefix );

  } else {
    sprintf( prefix, "%s<swpf> ", ASM_CMNT_LINE );
    fprintf( file, "%s\n", prefix );
    char* failure_msg = NULL;

    switch( ROTATING_KERNEL_INFO_failure_code(info) ){
    case MODULO_SCHED_FAILED:
      failure_msg = "unable to find a modulo schedule";
      break;
    case REGISTER_ALLOC_FAILED:
      failure_msg = "not enough registers";
      break;
    case ONE_STAGE_COUNT_ONLY:
      failure_msg = "loop has only one stage count";
      break;
    case SKIP_FDIV_SQRT:
      failure_msg = "loop has fdiv or sqrt operation";
      break;
    default:
      //Is_True(FALSE, ("unknown SWP RETURN CODE."));
      failure_msg = "unknown failure code";
    }

    fprintf( file, "%s %s\n", prefix, failure_msg );
    fprintf( file, "%s\n", prefix );
  }
}


#define is_power_of_two(i) (((i) & ((i)-1)) == 0)

extern ARC* new_arc( CG_DEP_KIND, OP*, OP*, UINT8, UINT8, BOOL );
extern void Exp_COPY( TN*, TN*, OPS* );
extern void Emit_SWP_Note( BB*, FILE* );
extern void Unroll_Make_Remainder_Loop( CG_LOOP&, INT32 );
extern void Unroll_Do_Loop_guard( LOOP_DESCR*, LOOPINFO*, TN* );
extern void append_to_prolog( BB* );
extern void Unroll_Do_Loop( CG_LOOP&, UINT32 );
extern void Fix_Backpatches( CG_LOOP&, bool );
extern void extend_prolog();
extern void extend_epilog( LOOP_DESCR* );

typedef struct {
  uint16_t ntimes;
  bool const_trip;
} NOTE_SWP_HEAD;
  

/* array op_info(op) is indexed thru OP_map_idx(op)
   Since op_info is indexed thru OP_map_idx(op), which will be changed by
   BB_Append_Op(). So we should be careful after Reorder_Kernel() is finished. */

struct OP_info {
  OP* op;
  OP* last_use;
  OP* addr_op;        // the op which will affect the current ofst
  bool is_rv;         // it is a reduction variable
  int indx;           // a consecutive order for sas
  int cycle;          // at which cycle this op is issued by sas
  int order;          // in what order an op is scheduled relative to others by sas.
  int8_t stage;       // at which stage
  uint8_t omega[OP_MAX_FIXED_OPNDS];

  void Init( OP* opr, bool swp ) {
    indx = cycle = order = stage = -1;
    op = opr;
    is_rv = false;
    addr_op = last_use = NULL;

    if( swp ){
      for( int opnd = 0; opnd < OP_opnds(op); opnd++ ){
	TN* tn = OP_opnd( op, opnd );
	omega[opnd] = OP_omega( op, opnd );
      }
    }
  }

}* op_info = NULL;

static int OP_info_size = 0;

#define OP_INFO(op)        ( &op_info[OP_map_idx(op)] )
#define OP_info_init(op,s) ( (OP_INFO(op))->Init(op,s) )
#define OP_info_op(op)     ( (OP_INFO(op))->op )
#define OP_info_verify(op) ( Is_True( (OP_INFO(op))->op == (op), ("") ) )
#define OP_info_indx(op)   ( (OP_INFO(op))->indx )
#define OP_info_cycle(op)  ( (OP_INFO(op))->cycle )
#define OP_info_order(op)  ( (OP_INFO(op))->order )
#define OP_info_stage(op)  ( (OP_INFO(op))->stage )
#define OP_info_last_use(op)   ( (OP_INFO(op))->last_use )
#define OP_info_omega(op,i)    ( (OP_INFO(op))->omega[i] )
#define OP_info_reset()        (bzero( op_info, sizeof(op_info[0])*OP_info_size ));
#define OP_info_is_rv(op)      ( (OP_INFO(op))->is_rv )
#define OP_info_addr_op(op)    ( (OP_INFO(op))->addr_op )


inline static void OP_info_move( OP* from, OP* to )
{
  int to_idx = OP_map_idx( to );
  int from_idx = OP_map_idx( from );

  Is_True( to_idx < OP_info_size, ("") );
  Is_True( op_info[to_idx].op == NULL, ("") );

  op_info[to_idx] = op_info[from_idx];
  op_info[to_idx].op = to;
  op_info[from_idx].op = NULL;
}

#ifndef cg_swp_INCLUDED
// Define an ordering for CLASS_REG_PAIR.
//   - required by STL map.
//
inline bool operator<(CLASS_REG_PAIR x, CLASS_REG_PAIR y) {
  return CLASS_REG_PAIR_class_n_reg(x) <  CLASS_REG_PAIR_class_n_reg(y); 
}

inline bool operator==(CLASS_REG_PAIR x, CLASS_REG_PAIR y) {
  return memcmp(&x, &y, sizeof(CLASS_REG_PAIR)) == 0;
}
#endif

// The number of bbs passed to cg_sas.
static unsigned int bb_count = 0;
static const char* ddg_file = tempnam( "/tmp/", "key_sch" );
static const char* sched_file = tempnam( "/tmp/", "q" );
static char func_name[128];
static const bool dump_ddg = false;

inline static uint16_t log2( uint32_t n )
{
  uint16_t result = 0;
  Is_True( n > 0, ("") );

  while( ( 1 << result ) <= n ){
    result++;
  }

  return result-1;
}


static void preconditioning_head_note_handler( NOTE_ACTION action, NOTE_INFO* info,
					       FILE *file )
{
  const NOTE_SWP_HEAD* info_u = (const NOTE_SWP_HEAD*)info;
  const int ntimes = info_u->ntimes;
  const bool const_trip = info_u->const_trip;

  switch( action ){
  case NOTE_PRINT_TO_FILE:
    if( const_trip ){
      fprintf(file,
	      "%s<loop> Preconditioning loop (%d iteration%s)\n",
	      ASM_CMNT_LINE, ntimes, ntimes == 1 ? "" : "s");
    } else {
      fprintf(file,
	      "%s<loop> Preconditioning loop (at most %d iteration%s)\n",
	      ASM_CMNT_LINE, ntimes-1, ntimes-1 == 1 ? "" : "s");
    }
    if (ntimes == 0)
      DevWarn( "Found Preconditioning head note with ntimes = 0" );
    break;
  case NOTE_PRINT_TO_ANL_FILE:
    /* ignore for now */
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file, "preconditioning_head_note_handler");
    break;
  default:
    Is_True(FALSE, ("Didn't recognize action"));
  }
}


/* Handler for NOTE_SWP_HEAD note. */
static void prolog_note_handler( NOTE_ACTION action, NOTE_INFO* info, FILE *file )
{
  const NOTE_SWP_HEAD* info_u = (const NOTE_SWP_HEAD*)info;
  const int ntimes = info_u->ntimes;
  const bool const_trip = info_u->const_trip;

  switch( action ){
  case NOTE_PRINT_TO_FILE:
    if( const_trip ){
      fprintf( file,
	       "%s<loop> SWP Prolog (%d stage%s)\n",
	       ASM_CMNT_LINE, ntimes, ntimes == 1 ? "" : "s" );
    } else {
      fprintf( file,
	       "%s<loop> SWP Prolog (at most %d stage%s)\n",
	       ASM_CMNT_LINE, ntimes-1, ntimes-1 == 1 ? "" : "s" );
    }

    break;

  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf( file, "prolog_note_handler" );
    break;

  default:
    Is_True( FALSE, ("Didn't recognize action") );
  }
}


static int sort_by_scycle( const void* a, const void* b )
{
  OP* op1 = *(OP**)a;
  OP* op2 = *(OP**)b;

  if( OP_info_cycle( op1 ) < OP_info_cycle( op2 ) )
    return -1;

  if( OP_info_cycle( op1 ) > OP_info_cycle( op2 ) )
    return 1;

  // Given the same cycles, tie-breaking with stages.
  if( OP_info_stage( op1 ) > OP_info_stage( op2 ) )
    return -1;

  if( OP_info_stage( op1 ) < OP_info_stage( op2 ) )
    return 1;

  // Given the same stages, tie-breaking with orders.
  if( OP_info_order( op1 ) < OP_info_order( op2 ) )
    return -1;

  return 1;
}


void KEY_SCH::Emit_TN( char* buf, int size, TN* tn ) const
{
  char *result = buf;

  FmtAssert( tn != NULL, ("") );

  if( TN_is_constant(tn) ){
    if( TN_has_value(tn) ){
      buf += sprintf( buf, "(0x%llx)", TN_value(tn) );
      if (TN_size(tn) == 4 && 
	  TN_value(tn) >> 32 != 0 &&
	  TN_value(tn) >> 31 != -1)
	buf += sprintf( buf, "!!! TN_value=0x%llx is too big to fit in a word",
			TN_value(tn) );
    }
    else if (TN_is_enum(tn)) {
      buf += sprintf( buf, "(enum:%s)", ISA_ECV_Name(TN_enum(tn)) );
    }
    else if ( TN_is_label(tn) ) {
      LABEL_IDX lab = TN_label(tn);
      const char *name = LABEL_name(lab);
      int64_t offset = TN_offset(tn);
      if ( offset == 0 ) {
	buf += sprintf( buf, "(lab:%s)", name );
      }
      else {
	buf += sprintf( buf, "(lab:%s+%lld)", name, offset );
      }
    } 
    else if ( TN_is_tag(tn) ) {
      LABEL_IDX lab = TN_label(tn);
      const char *name = LABEL_name(lab);
      buf += sprintf( buf, "(tag:%s)", name );
    }
    else if ( TN_is_symbol(tn) ) {
      ST *var = TN_var(tn);
      buf += sprintf( buf, "(sym" );
      char* p = buf;
      if (ST_class(var) == CLASS_CONST)
	buf += sprintf( buf, ":%s)", Targ_Print(NULL, ST_tcon_val(var)));
      else
	buf += sprintf( buf, ":%s%+lld)", ST_name(var), TN_offset(tn) );

      // clean up something for trigger.lex
      while( p < buf - 1 ){
	if( *p == '(' )
	  *p = '[';
	else if( *p == ')' )
	  *p = ']';
	p++;
      }
    } 
    else {
      FmtAssert( false, ("") );
    }

  } else {  /* register TN */
    if( TN_is_global_reg(tn) ){
      buf += sprintf( buf, "GTN%d", TN_number(tn) );
    } else {
      buf += sprintf( buf, "TN%d", TN_number(tn) );
    }

    if( TN_register(tn) != REGISTER_UNDEFINED ){
      if (TN_register(tn) <= REGISTER_CLASS_last_register(TN_register_class(tn))) {
	buf += sprintf(buf, "(%s)", 
		       REGISTER_name(TN_register_class(tn), TN_register(tn)));
      } else {
	buf += sprintf( buf, "(%d,%d)", TN_register_class(tn), TN_register(tn) );
      }
    }

    if( TN_is_save_reg(tn) ){
      buf += sprintf( buf, "(sv:%s)", 
		      REGISTER_name(TN_save_rclass(tn), TN_save_reg(tn)) );
    }
  }
  
  FmtAssert( buf - result < size, ("") );
}


void KEY_SCH::Emit_Src_DDG() const
{
  OP* op;
  char buf[1024];
  int size = sizeof(buf);
  FILE* t = fopen( ddg_file, dump_ddg ? "a" : "w" );
  FmtAssert( t != NULL, ("Emit_Src_DDG: fail to open %s",ddg_file) );

  fprintf( t, "\n{\n" );

  //fprintf( t, "\tbb:  %s:%s:%d\n", "file.cxx", func_name, BB_id(kernel) );
  fprintf( t, "\tbb:  %s:%s:%d\n", Obj_File_Name, func_name, bb_count );
  fprintf( t, "\tops: %d\n", nOps );
  if( perform_swp )
    fprintf( t, "\tswp\n" );
  else
    fprintf( t, "\tlist\n" );

  FOR_ALL_BB_OPs( kernel, op ){
    FmtAssert( BB_id(OP_bb(op)) == BB_id(kernel), ("") );

    // Print OP No SrcLine

    bool cg_loop_op = Is_CG_LOOP_Op(op);

    fprintf( t, "\top%d: ", OP_info_indx(op) );

    for( int i = 0; i < OP_results(op); i++ ){
      Emit_TN( buf, size, OP_result(op,i) );
      fprintf( t , "%s ", buf );
    }

    fprintf(t, "= ");
    fprintf(t, "%s ", TOP_Name(OP_code(op)));
    for( int i=0; i<OP_opnds(op); i++ ){
      TN *tn = OP_opnd(op,i);
      Emit_TN( buf, size, tn );
      fprintf( t, "%s ", buf );
      if ( cg_loop_op ) {
	int omega = TN_is_symbol(tn) ? OP_restore_omega(op) : OP_omega(op,i);
	if (omega) fprintf(t, "[%d]", omega);
      }
      //if (OP_Defs_TN(op, tn)) fprintf(t, "<defopnd>");
      fprintf(t, " ");
    }
    
    fprintf( t, " {" );
    
    // Print succs

    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* succ_op = ARC_succ(arc);
      uint16_t succ_id = OP_info_indx(succ_op);
      CG_DEP_KIND kind = ARC_kind(arc);
      
      fprintf( t, "\n\t\top%d: ", succ_id );
      
      if( kind == CG_DEP_REGIN )
	fprintf( t, "raw opnd(%d) ", ARC_opnd(arc) );
      else if( kind == CG_DEP_REGANTI )
	fprintf( t, "war opnd(%d) ", ARC_opnd(arc) );
      else if ( kind == CG_DEP_REGOUT )
	fprintf( t, "waw " );
      else
	fprintf( t, "misc " );
      
      fprintf( t, "latency(%d) omega(%d) ", ARC_latency(arc), ARC_omega(arc) );
    }
    
    if( OP_succs(op) != NULL )
      fprintf( t, "\n\t" );

    fprintf( t, "}\n" );
  }

  fprintf( t, "}\n" );  
  fclose( t );
}


void KEY_SCH::Schedule_DDG()
{
  static char* solver = NULL;

  if( solver == NULL ){
    (void*)solver = (char*)malloc( sizeof(solver[0]) * 256 );

    if( CG_Path == NULL ){
      char* path = getenv( "LD_LIBRARY_PATH" );
      Is_True( path != NULL, ("environment variable LD_LIBRARY_PATH is not set") );
      strcpy( solver, path );
      path = strchr( solver, ':' );
      if( path == NULL )
	strcat( solver, "/sas" );
      else
	strcpy( path, "/sas" );

    } else {
      sprintf( solver, "%s/sas", CG_Path );
    }
  }

  pid_t childpid;

  /* Call the solver. */
  if( ( childpid = fork() ) == 0 ){
    close(0);    close(1);
    //close(2);
    execlp( solver, solver, ddg_file, "-o", sched_file,(char*)0 );
    FmtAssert( FALSE, ("KEY_SCH: fail to execute %s", solver) );

  } else {
    // The mother process.
    int status;
    FmtAssert( childpid > 0, ("") );

    if( waitpid( childpid, &status, 0 ) != childpid ){
      FmtAssert( false, ("") );
    }

    if( status != 0 ){
      Gen_Kernel_Fail_Info( MODULO_SCHED_FAILED );
      Is_True( false, ("KEY_SAS FAILED to SCHEDULE %s", ddg_file) );
    }
  }
}


void KEY_SCH::GetLine( FILE* f, char* line )
{
  int len = 0, C = EOF;

  while( ( C = fgetc( f ) ) != EOF ){
    char c = (char)C;
    if( c == '\n' || c == '\r' ){
      break;
    }

    if( c != ' ' && c != '\t' ){
      line[len++] = c;
    }
  }

  is_eof = C == EOF;

  line[len] = '\0';
}


void KEY_SCH::Collect_Sched_Info()
{
  char line[1024];
  FILE* f = fopen( sched_file, "r" );
  FmtAssert( f != NULL, ("") );
  OP* op = BB_first_op( kernel );
  int indx = 0;
  uint32_t big = 0;

  is_eof = false;
  min_sched_len = sched_len = sc = res_mii = rec_mii = mii = -1;

  while( !is_eof ){
    char* p = NULL;

    GetLine( f, line );
    //puts( line );

    if( perform_swp &&
	mii < 0     &&
	( p = strstr( line, "swp:" ) ) != NULL ){
      p = strstr( p, "mii" );
      if( p == NULL )
	continue;

      mii = atoi( &p[3] );
      FmtAssert( mii > 0, ("") );
      p = strstr( p, "res_mii" );
      res_mii = atoi( &p[7] );
      p = strstr( p, "rec_mii" );
      rec_mii = atoi( &p[7] );

      continue;
    }

    if( !( p = strstr( line, "cycle" ) ) )
      continue;
    if( p[5] > '9' || p[5] < '0' )
      continue;

    OP_info_cycle( op ) = atoi( &p[5] );
    sched_len = MAX( sched_len, OP_info_cycle(op) );
    if( !OP_br( op ) ){
      min_sched_len = MAX( min_sched_len, OP_info_cycle(op) );
    }

    if( perform_swp ){
      OP_info_stage( op ) = OP_info_cycle( op ) / mii;

      if( OP_info_stage( op ) > 0 &&
	  !OP_br( op ) ){
	for( int opnd = 0; opnd < OP_opnds(op); opnd++ ){
	  TN* tn = OP_opnd( op, opnd );
	  if( TN_is_register(tn) &&
	      !TN_is_const_reg(tn) ){
	    int new_omega = OP_info_omega( op, opnd ) + OP_info_stage( op );
	    /* Don't use Set_OP_omega() here, since kernel will be used in
	       Kernel_Preconditioning() later. */
	    OP_info_omega( op, opnd ) = new_omega;
	  }
	}
      }
    }

    p = strstr( p, "order" );
    FmtAssert( p != NULL, ("") );
    OP_info_order( op ) = atoi( &p[5] );
    FmtAssert( OP_info_indx(op) == indx, ("") );
    op = OP_next( op );
    indx++;
  }

  if( perform_swp ){
    sched_len++;
    min_sched_len++;
    sc = (int)ceil( (double)sched_len / mii );
    if( sc < 2 ){
      Gen_Kernel_Fail_Info( ONE_STAGE_COUNT_ONLY );
    }
  }

  FmtAssert( indx == nOps, ("") );

  fclose( f );
}


void KEY_SCH::Reorder_Kernel( ARRAY_ELEMENT_CMP_FUNC cmp_func )
{
  OP* op = NULL;

  VECTOR_Reset( schedule );

  int indx = 0;
  FOR_ALL_BB_OPs( kernel, op ){
    // Make sure we still have the "same" kernel.
    OP_info_verify( op );
    Is_True( OP_info_indx( op ) == indx, ("") );

    VECTOR_Add_Element( schedule, op );
    indx++;
  }

  VECTOR_Sort( schedule, cmp_func );

  // DDG info will be gone after this call.
  Is_True( BB_length(kernel) == nOps, ("") );
  BB_Remove_All( kernel );

  for( int i = 0; i < nOps; i++ ){
    OP* op = (OP*)VECTOR_element( schedule, i );
    int old_map_idx = OP_map_idx( op );

    OP_info_order( op ) = i;
    OP_scycle( op ) = OP_info_cycle( op );
    BB_Append_Op( kernel, op );    // BB_Append_Op(op) will change OP_map_idx(op).

    if( old_map_idx != OP_map_idx(op) ){
      Is_True( OP_map_idx(op) < OP_info_size, ("") );
      struct OP_info tmp = op_info[OP_map_idx(op)];

      op_info[OP_map_idx(op)] = op_info[old_map_idx];
      op_info[old_map_idx] = tmp;

      if( tmp.op != NULL ){
	(tmp.op)->map_idx = old_map_idx;
      }
    }
  }

  // For safety, we need to find a way to remove the ddg!!!

  return;
}


/* Convert sequence
      addr = addr' + ofst
      ... = (addr)
   into
      addr = addr' + ofst
      ... = (addr')ofst
   Also add a war data dependency between them if addr = addr'.
   Benefit: more freedom to schedule ops, and the latency between
   addiu and ld will be long is addiu is scheduled at EX
*/
void KEY_SCH::Handle_Ldst_Addiu()
{
  /* Since KEY_SCH is called very late, Ldst_Addiu has been taken care
     of by earlier hb scheduling phase. Also, after register allocation,
     any dag transformation could lead to a loop. */
  return;

  OP* mem_op = NULL;

  FOR_ALL_BB_OPs_REV( kernel, mem_op ){
    // Don't need to worry about store.
    if( !OP_load( mem_op ) )
      continue;

    /* Check if the memory OP has an offset field (i.e., it is not an 
       indexed load/store/prefx. */
    //INT offset_opndnum = Memory_OP_Offset_Opndnum( mem_op );
    const INT ofst_idx = TOP_Find_Operand_Use( OP_code(mem_op), OU_offset );
    if( !TN_has_value( OP_opnd( mem_op, ofst_idx ) ) )
      continue;

    const INT base_idx = TOP_Find_Operand_Use( OP_code(mem_op), OU_base );
    OP* addr_op = NULL;
    ARC* addr_arc = NULL;

    for( ARC_LIST* arcs = OP_preds(mem_op); arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* op = ARC_pred( arc );

      if( ARC_kind(arc) == CG_DEP_REGIN ){
	if( ARC_opnd(arc) == base_idx &&
	    CGTARG_Is_OP_Addr_Incr( op ) ){
	  addr_op = op;
	  addr_arc = arc;
	}
	break;
      }
    }

    if( addr_op == NULL )
      continue;

    // Update the offset of the memory op.
    const INT64 addiu_const = TN_value( OP_opnd( addr_op, 1 ) );
    const INT64 ld_const = TN_value( OP_opnd( mem_op, ofst_idx ) ) + addiu_const;

    if( !TOP_Can_Have_Immediate( ld_const, OP_code(mem_op) ) )
      continue;

    for( ARC_LIST* arcs = OP_succs(addr_op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC* arc = ARC_LIST_first(arcs);
      FmtAssert( ARC_kind(arc) != CG_DEP_REGOUT, ("") );
    }

    // Now, update the new ofst for the mem_op.
    TN* old_ofst_tn = OP_opnd( mem_op, ofst_idx );
    TN* new_ofst_tn = Gen_Literal_TN( ld_const, TN_size( old_ofst_tn ) );

    Set_OP_opnd( mem_op, ofst_idx, new_ofst_tn );

    // Remove the original data dependency arc.
    CG_DEP_Detach_Arc( addr_arc );

    // Clone an arc between op -> addr_op to op -> mem_op.
    for( ARC_LIST* arcs = OP_preds(addr_op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC* arc = ARC_LIST_first(arcs);
      if( ARC_kind(arc) == CG_DEP_REGIN ){
	new_arc( CG_DEP_REGIN, ARC_pred(arc), mem_op,
		 ARC_omega(arc), base_idx, FALSE );
	break;
      }
    }
    
    // Add an arc between mem_op -> addr_op if necessary.
    if( OP_opnd( mem_op, base_idx ) == OP_result( addr_op, 0 ) ){
      new_arc( CG_DEP_REGOUT, mem_op, addr_op, 0, 0, FALSE );
      //CG_DEP_Trace_Graph( kernel );
    }
  }
}


/* Given the long live range of an induction varible, it should be
   detached from the ddg to avoid spending too many registers on it.
   And the ofst of all the memory op should be re-computed after PKE
   are generated.
*/
void KEY_SCH::Construct_addr_vector()
{
  OP* addr_op = NULL;

  FOR_ALL_BB_OPs( kernel, addr_op ){
    // Identify LDST/ADDIU instructions with non-relocatable offsets.
    if( !CGTARG_Is_OP_Addr_Incr( addr_op ) )
      continue;

    Is_True( OP_code(addr_op) == TOP_addiu, ("") );

    ARC_LIST* arcs = NULL;

    for( arcs = OP_preds(addr_op); arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* pred_op = ARC_pred(arc);
      if( pred_op != addr_op )
	break;
    }

    if( arcs != NULL )
      continue;
	
    OP_info_is_rv( addr_op ) = true;

    for( arcs = OP_succs(addr_op); arcs != NULL; ){
      ARC* arc = ARC_LIST_first(arcs);
      OP* succ_op = ARC_succ(arc);

      arcs = ARC_LIST_rest(arcs);

      if( ARC_kind(arc) != CG_DEP_REGIN ){
	FmtAssert( false, ("") );
	continue;
      }

      if( succ_op == addr_op ){
	;

      } else if( OP_memory( succ_op ) ){
	const int ofst_idx = TOP_Find_Operand_Use( OP_code(succ_op), OU_offset );
	const int base_idx = TOP_Find_Operand_Use( OP_code(succ_op), OU_base );

	/* We have to preserve the order here, since the base might not be
	   defined out side of kernel. */
	if( OP_info_omega( succ_op, base_idx ) == 0 )
	  continue;

	if( !TN_has_value( OP_opnd( succ_op, ofst_idx ) ) ||
	    ARC_opnd(arc) != base_idx )
	  continue;

	const INT64 addiu_const = TN_value( OP_opnd( addr_op, 1 ) );
	// Assuming omega will not be larger than 8.
	const INT64 ofst_const =
	  TN_value( OP_opnd( succ_op, ofst_idx ) ) - addiu_const * 8;

	if( !TOP_Can_Have_Immediate( ofst_const, OP_code(succ_op) ) )
	  continue;

	OP_info_addr_op( succ_op ) = addr_op;

      } else if( OP_br( succ_op ) ){
	;
      } else
	continue;

      // Detach addr_op from its successor.
      CG_DEP_Detach_Arc( arc );
    }

    // Detach addr_op from its predecessors.

    for( ARC_LIST* arcs = OP_preds(addr_op); arcs != NULL; ){
      ARC* arc = ARC_LIST_first(arcs);
      /* If so, we need to adjust Adjust_ofst() or reorder
	 related ops. */
      Is_True( ARC_kind(arc) != CG_DEP_REGANTI, ("") );
      arcs = ARC_LIST_rest(arcs);
      CG_DEP_Detach_Arc( arc );
    }
  }
}


void KEY_SCH::Adjust_incr( OP* addr_op )
{
  const TN* old_incr_tn = OP_opnd( addr_op, 1 );
  const INT64 incr_const = TN_value( old_incr_tn ) * Kmin;

  TN* new_incr_tn = Gen_Literal_TN( incr_const, TN_size( old_incr_tn ) );
  Set_OP_opnd( addr_op, 1, new_incr_tn );
}


/* Adjust the ofst of a memory op according to addr_op and unrolling.
 */
void KEY_SCH::Adjust_ofst( const OP* addr_op, OP* mem_op ) const
{
  // If it is not an add, then the value of addiu_const should be negative.
  const INT64 addiu_const = TN_value( OP_opnd( addr_op, 1 ) );
  const int base_idx = TOP_Find_Operand_Use( OP_code(mem_op), OU_base );

  /* Don't use OP_precedes, which is not the original relative order
     inside kernel. Also, reverse magnitude if addr_op is a sub. */

  const int ofst_idx = TOP_Find_Operand_Use( OP_code(mem_op), OU_offset );
  const TN* old_ofst_tn = OP_opnd( mem_op, ofst_idx );

  /* <omega> gives the number of times that <addiu_const> has been
     updated between the peer <add_op> and the current <mem_op>. */
  int omega = 0;
  const int unrollings = Kmin + sc - 1;

  int start = OP_info_cycle( addr_op ) + ( 1 + OP_unrolling( mem_op ) ) * mii;

  for( int unrolling = OP_unrolling( mem_op) + 1;
       unrolling < unrollings;
       unrolling++ ){
    if( start >= OP_scycle( mem_op) )
      break;

    omega++;
    start += mii;
  }

  if( OP_info_order( addr_op ) < OP_info_order( mem_op ) ){
    if( OP_info_indx( addr_op ) > OP_info_indx( mem_op ) )
      omega++;

  } else {
    if( OP_info_indx( addr_op ) < OP_info_indx( mem_op ) )
      omega--;
  }

  // It is assumed that addr_op will be the last scheduled one.

  if( omega == 0 )
    return;

  const INT64 ofst_const = TN_value( old_ofst_tn ) - addiu_const * omega;
  Is_True( TOP_Can_Have_Immediate( ofst_const, OP_code(mem_op) ), ("") );
  TN* new_ofst_tn = Gen_Literal_TN( ofst_const, TN_size( old_ofst_tn ) );

  // Now, update the new ofst for the mem_op.
  Set_OP_opnd( mem_op, ofst_idx, new_ofst_tn );

  if( trace ){
    fprintf( TFile, "old: %lld, new: %lld\n",
	     TN_value(old_ofst_tn), TN_value(new_ofst_tn) );
    fprintf( TFile, "offset changed:" );
    Print_OP_No_SrcLine( mem_op );
  }
}


void KEY_SCH::Schedule_Kernel()
{
  // First, allocate data array.
  int max_indx = 0;
  OP* op;

  FOR_ALL_BB_OPs( kernel, op ){
    max_indx = std::max( max_indx, OP_map_idx(op) );
  }

  OP_info_size = max_indx + 1 + 1;  // for the new br_op at Preconditioning
  (void*)op_info = (OP_info *)MEM_POOL_Alloc( mem_pool, ( sizeof(op_info[0]) * OP_info_size ) );
  OP_info_reset();
  max_indx = 0;
  FOR_ALL_BB_OPs( kernel, op ){
    OP_info_init( op, perform_swp );
    OP_info_indx(op) = max_indx++;
  }

  nOps = max_indx;
  FmtAssert( nOps == BB_length(kernel), ("") );

  schedule = VECTOR_Init( nOps, mem_pool );

  if( perform_swp )
    Construct_addr_vector();
  else
    Handle_Ldst_Addiu();

  Emit_Src_DDG();

  Schedule_DDG();

  Collect_Sched_Info();

  bb_count++;

  if( unlink( ddg_file ) != 0 ||
      unlink( sched_file ) != 0 ){
    FmtAssert( false, ("") );
  }
}


KEY_SCH::KEY_SCH( BB* bb,  MEM_POOL* pool )
  : kernel( bb ), mem_pool( pool ), perform_swp( false )
{
  success = true;

  if( BB_entry( kernel ) ){
    ANNOTATION* ant = ANNOT_Get( BB_annotations(kernel), ANNOT_ENTRYINFO );
    ENTRYINFO*  ent = ANNOT_entryinfo( ant );
    strcpy( func_name, ST_name( ENTRYINFO_name(ent) ) );
  }

  if( func_name[0] == '\0' ){
    strcpy( func_name, "unknown" );
  }

  Schedule_Kernel();
  Reorder_Kernel( sort_by_scycle );
}


void KEY_SCH::Assign_Register( TN* tn )
{
  Is_True( TN_is_register( tn ) && !TN_has_register( tn ), ("") );

  if( reg_allocation.find(tn) != reg_allocation.end() )
    return;

  CLASS_REG_PAIR rp;
  ISA_REGISTER_CLASS rc = TN_register_class(tn);
  /* Try to get one that doesn't need to be saved. */
  REGISTER r = REGISTER_SET_Choose_Intersection( avail_reg_set[rc],
						 REGISTER_CLASS_caller_saves(rc) );
  if( r == REGISTER_UNDEFINED ){
    r = REGISTER_SET_Choose( avail_reg_set[rc] );
    if( r == REGISTER_UNDEFINED ){
      FmtAssert( false, ("SWP_KEY: run out of registers unexpectedly.") );
      return;
    }
  }

  avail_reg_set[rc] = REGISTER_SET_Difference1( avail_reg_set[rc], r );
  Set_CLASS_REG_PAIR( rp, rc, r );
  reg_allocation[tn] = rp;
    
  Is_True( CLASS_REG_PAIR_rclass(rp) >= ISA_REGISTER_CLASS_MIN &&
	   CLASS_REG_PAIR_rclass(rp) <= ISA_REGISTER_CLASS_MAX,
	   ("SWP_KEY: invalid register class %d\n", CLASS_REG_PAIR_rclass(rp)));

  Set_TN_class_reg( tn, rp );
  Set_TN_is_dedicated( tn );
  Is_True( reg2tn_map.find(rp) == reg2tn_map.end(), ("") );
  reg2tn_map[rp] = tn;

  if( trace ){
    fPrint_TN( TFile, "KEY_SWP REG ALLOC: %s\n", tn );
  }
}


void KEY_SCH::Add_Glue( TN* result, TN* opnd, BB* bb, GLUE_DIRECTION dir )
{
  if( result == opnd )
    return;

  OPS ops = OPS_EMPTY;
  Exp_COPY( result, opnd, &ops );

  OP* op = NULL;

  FOR_ALL_OPS_OPs( &ops, op ){
    Set_OP_glue( op );
  }

  if( trace ){
    fprintf( TFile, "Add_Glue: " );
    Print_OPS_No_SrcLines( &ops );
  }

  if( dir == APPEND )
    BB_Append_Ops( bb, &ops );
  else
    BB_Prepend_Ops( bb, &ops );
}


void KEY_SCH::register_allocation_init()
{
  bool enough_reg = true;
  // The number of bbs can be solved by cg_sas.
  static unsigned int swp_count = 0;
  // Construct avail_reg_set.
  ISA_REGISTER_CLASS rc;
  int reg_num_required[ISA_REGISTER_CLASS_MAX+1];

  FOR_ALL_ISA_REGISTER_CLASS( rc ){
    avail_reg_set[rc] = REGISTER_CLASS_allocatable( rc );
    reg_num_required[rc] = 0;
  }

  // Construct tn_invariants.
  TN_SET* tn_defs = TN_SET_Create_Empty( Last_TN + 1, mem_pool );
  TN_SET* tn_uses = TN_SET_Create_Empty( Last_TN + 1, mem_pool );

  tn_live_ins = TN_SET_Create_Empty( Last_TN + 1, mem_pool );

  OP* op = NULL;

  const TN_SET* live_outs = BB_live_in( epilog );

  FOR_ALL_BB_OPs( kernel, op ){
    for( INT j = 0; j < OP_opnds(op); j++ ){
      TN* tn = OP_opnd(op, j);
      if( TN_is_register(tn) && !TN_is_dedicated(tn) ){
	tn_uses = TN_SET_Union1D( tn_uses, tn, mem_pool );
	if( OP_omega( op, j ) > 0 && 
	    !TN_SET_MemberP( tn_defs, tn ) ){
	  tn_live_ins = TN_SET_Union1D( tn_live_ins, tn, mem_pool );
	}
      }
    }

    for( INT i = 0; i < OP_results(op); i++ ){
      TN* tn = OP_result(op, i);
      if( TN_is_register(tn) && !TN_is_dedicated(tn) ){
	tn_defs = TN_SET_Union1D( tn_defs, tn, mem_pool );
      }
    }
  }  

  // Identify loop invariants.
  tn_invariants = TN_SET_Difference( tn_uses, tn_defs, mem_pool );
  tn_live_ins   = TN_SET_Difference( tn_live_ins, tn_invariants, mem_pool );
  tn_live_outs  = TN_SET_Difference( tn_defs, BB_live_in(epilog), mem_pool );

  /* Get rid of dedicated registers from tn_invariants set, and
     count do we have enough registers for glue copies. */
  for( TN* tn = TN_SET_Choose(tn_invariants);
       tn != TN_SET_CHOOSE_FAILURE;
       tn = TN_SET_Choose_Next(tn_invariants,tn) ){
    if( TN_is_dedicated( tn ) ){
      Is_True( false, ("step thru it") );
      tn_invariants = TN_SET_Difference1( tn_invariants, tn, mem_pool );
    } else {
      const ISA_REGISTER_CLASS rc = TN_register_class(tn);
      reg_num_required[rc]++;
    }
  }

  if( trace ){
    if( BS_Size( tn_live_ins ) > 0 ){
      fprintf( TFile, "%d live_ins: GTN", BS_Size( tn_live_ins ) );
      TN_SET_Print( tn_live_ins, TFile );
      fprintf( TFile, "\n" );
    }

    if( BS_Size( tn_invariants ) > 0 ){
      fprintf( TFile, "%d invariants: GTN", BS_Size( tn_invariants ) );
      TN_SET_Print( tn_invariants, TFile );
      fprintf( TFile, "\n" );
    }

    if( BS_Size( tn_live_outs ) > 0 ){
      fprintf( TFile, "%d live_outs: GTN", BS_Size( tn_live_outs ) );
      TN_SET_Print( tn_live_outs, TFile );
      fprintf( TFile, "\n" );
    }
  }

  /* Do we have enough registers for unrolled kernel?
     It must be consistent with tn_renaming(). */
  FOR_ALL_BB_OPs( kernel, op ){
    for( int i = 0; i < OP_results(op); i++ ){
      TN* result = OP_result( op, i );
      if( TN_is_dedicated( result )      ||
	  OP_info_last_use( op ) == NULL ||
	  TN_SET_MemberP( tn_invariants, result ) )
	continue;

      const ISA_REGISTER_CLASS rc = TN_register_class( result );
      // Compute the live range of result.
      const int start = OP_info_cycle( op );
      const int end = OP_info_cycle( OP_info_last_use( op ) );

      /* Find the smallest k_min s.t. k_min covers the live range
	 of op, and ( Kmin % k_min ) == 0. */
      int k_min = 1 + ( end - start - 1 ) / mii;

      while( ( Kmin % k_min ) != 0 ){
	k_min++;
      }

      reg_num_required[rc] += k_min;
    }
  }

  FOR_ALL_ISA_REGISTER_CLASS( rc ){
    if( reg_num_required[rc] == 0 )
      continue;

    int avail_reg_num = REGISTER_SET_Size( avail_reg_set[rc] );

    if( trace ){
      const ISA_REGISTER_CLASS_INFO* icinfo = ISA_REGISTER_CLASS_Info( rc );
      const char* rcname = ISA_REGISTER_CLASS_INFO_Name( icinfo );
      fprintf( TFile, "(BB:%d, rc:%s)\tAvail:%d\tRequired:%d",
	       BB_id(kernel), rcname, avail_reg_num, reg_num_required[rc] );

      if( avail_reg_num < reg_num_required[rc] ){
	fprintf( TFile, " (%d more registers are needed)",
		 reg_num_required[rc] - avail_reg_num );
      }

      fprintf( TFile, "\n" );
    }

    if( avail_reg_num < reg_num_required[rc] ){
      enough_reg = false;
    }
  }  

  if( !enough_reg )
    Gen_Kernel_Fail_Info( REGISTER_ALLOC_FAILED );
  else
    swp_count++;

  return;
}


TN* KEY_SCH::New_live_out_tn( TN* live_out )
{
  // Should we do something like CG_LOOP_DEF to find the last def of op?
  TN* new_live_out = CG_LOOP_Backpatch_Find_Body_TN( prolog, live_out, NULL );

  if( new_live_out != NULL ){
    CG_LOOP_Backpatch_Add( epilog, live_out, new_live_out, 0 );
  }

  return new_live_out;
}


TN* KEY_SCH::New_live_in_tn( TN* live_in )
{
  TN* new_live_in = CG_LOOP_Backpatch_Find_Body_TN( prolog, live_in, NULL );

  if( new_live_in == NULL ){
    new_live_in = Dup_TN( live_in );
    CG_LOOP_Backpatch_Add( prolog, live_in, new_live_in, 0 );

    if( TN_SET_MemberP( tn_invariants, live_in ) ){
      CG_LOOP_Backpatch_Add( epilog, live_in, new_live_in, 0 );      
    }

    if( trace ){
      if( TN_SET_MemberP( tn_invariants, live_in ) )
	fPrint_TN( TFile, "KEY_SWP: invariant %s ", live_in );
      else
	fPrint_TN( TFile, "KEY_SWP: live_in %s ", live_in );

      fPrint_TN( TFile, "is renamed as %s\n", new_live_in );
    }

    Assign_Register( new_live_in );
  }

  return new_live_in;
}


// Perform TN renaming for invariants.
void KEY_SCH::rename_invariants()
{
  OP* op = NULL;

  FOR_ALL_BB_OPs( kernel, op ){
    for( int i = 0; i < OP_opnds(op); i++ ){
      TN* opnd = OP_opnd( op, i );
      if( !TN_is_register( opnd ) ||
	  TN_is_dedicated( opnd ) )
	continue;

      if( TN_SET_MemberP( tn_invariants, opnd ) ||
	  TN_SET_MemberP( tn_live_ins, opnd ) ){
	TN* new_body_tn = New_live_in_tn( opnd );
	Set_OP_opnd( op, i, new_body_tn );
      }
    }

    // handle the results.
    for( int i = 0; i < OP_results(op); i++ ){
      TN* result = OP_result( op, i );
      if( TN_is_dedicated( result ) )
	continue;

      if( TN_SET_MemberP( tn_live_outs, result ) ){
	TN* new_result_tn = New_live_out_tn( result );

	if( new_result_tn != NULL )
	  Set_OP_result( op, i, new_result_tn );
      }
    }
  }
}


void KEY_SCH::Gen_Kernel_Fail_Info( SWP_FAILURE_CODE failure_code )
{
  // Generate SWP ROTATING KERNEL Annotation
  ROTATING_KERNEL_INFO* info = TYPE_PU_ALLOC( ROTATING_KERNEL_INFO );

  bzero( info, sizeof(info[0]) );
  ROTATING_KERNEL_INFO_succeeded(info) = false;
  ROTATING_KERNEL_INFO_failure_code(info) = failure_code;
  BB_Add_Annotation( kernel, ANNOT_ROTATING_KERNEL, (void*)info );
  Reset_BB_rotating_kernel( kernel );

  success = false;
}


void KEY_SCH::Gen_Kernel_Info()
{
  OP* op = NULL;

  // Generate SWP ROTATING KERNEL Annotation
  ROTATING_KERNEL_INFO* info = TYPE_PU_ALLOC( ROTATING_KERNEL_INFO );
  bzero( info, sizeof(info[0]) );

  ISA_REGISTER_CLASS rc;

  FOR_ALL_ISA_REGISTER_CLASS( rc ){
    REGISTER_SET tmp = REGISTER_CLASS_allocatable(rc);
    tmp = REGISTER_SET_Difference( tmp, avail_reg_set[rc] );
    ROTATING_KERNEL_INFO_live_in(info)[rc] = tmp;
    ROTATING_KERNEL_INFO_kill(info)[rc] = tmp;
  }

  // Regenerate SWP resource statistics
  TI_RES_COUNT* res_counts = TI_RES_COUNT_Alloc( MEM_pu_pool_ptr );

  // Sum resources from each OP's resource usage.
  for( op = BB_first_op( kernel ); op != NULL; op = OP_next( op ) ){
    if( OP_scycle( op ) < mii )
      TI_RES_COUNT_Add_Op_Resources( res_counts, OP_code(op) );
  }

  // Save SWP statistics
  ROTATING_KERNEL_INFO_succeeded(info) = success;
  ROTATING_KERNEL_INFO_ii(info) = mii;
  ROTATING_KERNEL_INFO_stage_count(info) = sc;
  //ROTATING_KERNEL_INFO_min_ii(info) = max( res_mii, rec_mii );
  ROTATING_KERNEL_INFO_min_ii(info) = Kmin;
  ROTATING_KERNEL_INFO_res_min_ii(info) = res_mii;
  ROTATING_KERNEL_INFO_rec_min_ii(info) = rec_mii;
  ROTATING_KERNEL_INFO_sched_len(info) = sched_len;
  ROTATING_KERNEL_INFO_min_sched_len(info) = min_sched_len;
  ROTATING_KERNEL_INFO_res_counts(info) = res_counts;

  FOR_ALL_BB_OPs( glue_prolog, op ){
    if( OP_glue(op) ){
      for( INT k = 0; k < OP_results(op); k++ ){
	TN* tn = OP_result( op, k );
	if( !TN_is_const_reg(tn) )
	  ROTATING_KERNEL_INFO_copyin(info).push_back(tn);
      }
    }
  }

  FOR_ALL_BB_OPs( glue_epilog, op ){
    if( OP_glue(op) ){
      for( INT j = 0; j < OP_opnds(op); j++ ){
	TN* tn = OP_opnd( op, j );
	if( !TN_is_const_reg(tn) )
	  ROTATING_KERNEL_INFO_copyout(info).push_back(tn);
      }
    }
  }

  BB_Add_Annotation( kernel, ANNOT_ROTATING_KERNEL, (void *)info );

  if( trace ){
    Emit_SWP_Note( kernel, TFile );
  }
}


/* The TNs used in the SWP region are used in the
   SWP region, used as results in prolog glue copies, or used 
   as operands in the epilog copies.  Those TNs are not used elsewhere.
   This is to relieve GRA from worrying about live ranges that span the 
   boundary of a SWP region.  A consequence is that any <tn_invariants>
   must be assigned a new TN inside the SWP loop, and a glue
   copy is introduced to copy the value of old TN into the new one.
   Similar requirements for other <tn_live_ins> and <tn_live_outs> are
   enforced by the conversion of backpatches into glue copies.
*/
void KEY_SCH::tn_renaming( const int unrollings )
{
  OP* op = NULL;

  tn_renames = TN_MAP_Create();

  FOR_ALL_BB_OPs( kernel, op ){
    for( int i = 0; i < OP_results(op); i++ ){
      TN* result = OP_result( op, i );
      
      if( TN_MAP_Get( tn_renames, result ) != NULL )
	continue;

      TN** entry = TYPE_MEM_POOL_ALLOC_N( TN*, mem_pool, unrollings );
      TN_MAP_Set( tn_renames, result, entry );

      if( TN_is_dedicated( result ) ||
	  OP_info_last_use( op ) == NULL ){
	for( int unrolling = 0; unrolling < unrollings; unrolling++ ){
	  entry[unrolling] = result;
	}

      } else {
	entry[0] = result;

	// Compute the live range of result.
	const int start = OP_info_cycle( op );
	const int end = OP_info_cycle( OP_info_last_use( op ) );

	/* Find the smallest k_min s.t. k_min covers the live range
	   of op, and ( Kmin % k_min ) == 0. */
	int k_min = 1 + ( end - start - 1 ) / mii;

	while( ( Kmin % k_min ) != 0 ){
	  k_min++;
	}
      
	for( int unrolling = 1; unrolling < unrollings; unrolling++ ){
	  TN* tn = result;

	  if( unrolling < k_min ){
	    //int scycle = OP_info_cycle( op ) + unrolling * mii;
	    //if( scycle > start && scycle < end )
	    tn = Dup_TN( result );

	  } else {
	    int pos = unrolling % k_min;
	    tn = entry[pos];
	  }

	  entry[unrolling] = tn;
	}
      }
      
      // Perform register allocation for TNs.
      for( int unrolling = 0; unrolling < unrollings; unrolling++ ){
	TN* tn = entry[unrolling];
	if( !TN_has_register( tn ) )
	  Assign_Register( tn );
      }
    }
  }
}


/* At current unrolling, get the valid tn, which could be
   defined in previous unrollings, or in the current unrolling.
*/
TN* KEY_SCH::rename_tn( TN* tn, const int unrolling, const int omega ) const
{
  if( TN_is_register(tn) ){
    const int indx = unrolling - omega;
    Is_True( indx >= -1, ("") );

    if( indx == -1 ){
      return tn;
    }

    TN** entry = (TN**)TN_MAP_Get( tn_renames, tn );
    return ( entry == NULL ? tn : entry[indx] );
  }

  return tn;
}


static int sort_swp_kernel( const void* a, const void* b )
{
  OP* op1 = *(OP**)a;
  OP* op2 = *(OP**)b;

  if( OP_scycle( op1 ) < OP_scycle( op2 ) )
    return -1;

  if( OP_scycle( op1 ) > OP_scycle( op2 ) )
    return 1;

  /* Given they are scheduled at the same cycle,
     tie-breaking with OP_info_order. */

  if( OP_unrolling( op1 ) < OP_unrolling( op2 ) )
    return -1;

  if( OP_unrolling( op1 ) > OP_unrolling( op2 ) )
    return 1;

  if( OP_info_order( op1 ) < OP_info_order( op2 ) )
    return -1;

  return 1;
}


static void Create_Region( BB* bb )
{
  RID* r = RID_Create(New_Region_Id(), 0, NULL);
  RID_has_reg_alloc_Set(r);
  RID_level(r) = RL_CG;
  RID_type(r) = RID_TYPE_swp;
  RID_bounds_exist(r) = REGION_BOUND_UNKNOWN;
  RID_has_return(r) = REGION_NO_RETURN;
  RID_num_exits(r) = 1;
  RID_is_glue_code(r) = FALSE;        
  RID* parent = BB_rid( bb );
  RID_parent(r) = parent;
  RID_cginfo(r) = NULL;
  if( parent != NULL )
    RID_Add_kid( r, parent );

  BB_rid( bb ) = r;
}


// Also refer to SWP_Emit in cg_swp_emit.cxx.
void KEY_SCH::Gen_PKE( CG_LOOP& cl )
{
  // Create a SWP REGION
  Create_Region( kernel );

  Set_BB_scheduled( kernel );
  Set_BB_reg_alloc( kernel );

  Reorder_Kernel( sort_by_scycle );

  rename_invariants();
  tn_renaming( Kmin + sc - 1 );
  
  if( trace ){
    CG_LOOP_Backpatch_Trace( prolog, NULL );
    CG_LOOP_Backpatch_Trace( epilog, NULL );
    Print_OPS_No_SrcLines( &kernel->ops );
    fprintf( TFile, "\n" );
  }

  // Detach the branch op from kernel first.
  OP* br_op = BB_branch_op( kernel );
  const bool br_op_is_last = BB_last_op(kernel) == br_op;
  BB_Remove_Op( kernel, br_op );

  VECTOR kernel_vec = VECTOR_Init( Kmin * BB_length(kernel), mem_pool );
  const int length = BB_length(kernel) * ( sc - 1 );
  VECTOR prolog_vec = VECTOR_Init( length, mem_pool );
  VECTOR epilog_vec = VECTOR_Init( length, mem_pool );

  // Clear up the kernel first.

  OP* op;
  FOR_ALL_BB_OPs( kernel, op ){
    Set_OP_unrolling( op, 0 );
  }

  // Generate Prolog, Kernel and Epilog here.
  const int unrollings = Kmin + sc - 1;

  for( int unrolling = 0; unrolling < unrollings; unrolling++ ){
    OP* mom = NULL;

    FOR_ALL_BB_OPs( kernel, mom ){
      OP* new_op = Dup_OP( mom );
      CG_LOOP_Init_Op( new_op );
      Copy_WN_For_Memory_OP( new_op, mom );

      Set_OP_unrolling( new_op, unrolling );
      Set_OP_orig_idx( new_op, OP_map_idx(mom) );
      
      for( int opnd = 0; opnd < OP_opnds(mom); opnd++ ){
	TN* new_tn = rename_tn( OP_opnd(mom,opnd), unrolling, OP_omega(mom,opnd) );

	if( TN_is_register(new_tn) && !TN_has_register(new_tn) ){
	  Is_True( false, ("") );
	  Assign_Register( new_tn );
	}

	Set_OP_opnd( new_op, opnd, new_tn );
	Set_OP_omega( new_op, opnd, 0 );
      }

      for( int res = 0; res < OP_results(mom); res++ ){
	TN* new_res = rename_tn( OP_result(mom,res), unrolling, 0 );
	if( TN_is_register(new_res) && !TN_has_register(new_res) ){
	  Is_True( false, ("") );
	  Assign_Register( new_res );
	}

	Set_OP_result( new_op, res, new_res );
      }

      const int scycle = OP_info_cycle( mom ) + unrolling * mii;
      OP_scycle( new_op ) = scycle;

      if( OP_info_addr_op( mom ) != NULL )
	Adjust_ofst( OP_info_addr_op( mom ), new_op );

      // Insert a new_op to prolog, kernel, or epilog.

      if( scycle < ( sc - 1 ) * mii ){
	VECTOR_Add_Element( prolog_vec, new_op );
	Set_OP_unroll_bb( new_op, prolog );

      } else if( scycle < ( sc - 1 + Kmin ) * mii ){
	VECTOR_Add_Element( kernel_vec, new_op );
	Set_OP_unroll_bb( new_op, kernel );
	OP_scycle( new_op ) = scycle - ( sc - 1 ) * mii;
      } else {
	VECTOR_Add_Element( epilog_vec, new_op );
	Set_OP_unroll_bb( new_op, epilog );
	OP_scycle( new_op ) = scycle - ( sc - 1 + Kmin ) * mii;
      }
    }
  }

  for( int opnd = 0; opnd < OP_opnds(br_op); opnd++ ){
    TN* new_tn = rename_tn( OP_opnd(br_op,opnd), unrollings-1, OP_omega(br_op,opnd) );
    Set_OP_opnd( br_op, opnd, new_tn );
    Set_OP_omega( br_op, opnd, 0 );
  }

  OP_scycle( br_op ) = Kmin * mii - 1;
  
  TN_MAP_Delete( tn_renames );

  // Generate copies for prolog-backpatches.
  glue_prolog = prolog;
  for( CG_LOOP_BACKPATCH* bp = CG_LOOP_Backpatch_First(prolog, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp) ){
    TN* non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN* body_tn = CG_LOOP_BACKPATCH_body_tn(bp);

    Add_Glue( body_tn, non_body_tn, glue_prolog, APPEND );
  }

  // Generate copies for epilog-backpatches.
  glue_epilog = epilog;
  for( CG_LOOP_BACKPATCH* bp = CG_LOOP_Backpatch_First(epilog, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp) ){
    TN* body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN* non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp); 

    Add_Glue( non_body_tn, body_tn, glue_epilog, PREPEND );
  }

  extend_prolog();
  prolog = CG_LOOP_prolog;

  extend_epilog( cl.Loop() );
  epilog = CG_LOOP_epilog;


  // Sort the ops inside PKE_vec.

  VECTOR_Sort( prolog_vec, &sort_swp_kernel );
  VECTOR_Sort( kernel_vec, &sort_swp_kernel );
  VECTOR_Sort( epilog_vec, &sort_swp_kernel );

  BB_Remove_All( kernel );
  for( int i = 0; i < VECTOR_count( kernel_vec ); i++ ){
    op = (OP*)VECTOR_element( kernel_vec, i );
    BB_Append_Op( kernel, op );
  }

  // BB_Append_Op will change the OP_map_idx() for each op.
  OP_info_reset();

  if( br_op_is_last )
    BB_Append_Op( kernel, br_op );
  else
    BB_Insert_Op_Before( kernel, BB_last_op(kernel), br_op );

  for( int i = 0; i < VECTOR_count( prolog_vec ); i++ ){  
    op = (OP*)VECTOR_element( prolog_vec, i );
    BB_Append_Op( prolog, op );
  }

  for( int i = VECTOR_count( epilog_vec ) - 1; i >= 0; i-- ){  
    op = (OP*)VECTOR_element( epilog_vec, i );
    BB_Prepend_Op( epilog, op );
  }

  if( trace ){
    Print_OPS_No_SrcLines( &prolog->ops );
    fprintf( TFile, "\n" );
    Print_OPS_No_SrcLines( &kernel->ops );
    fprintf( TFile, "\n" );
    Print_OPS_No_SrcLines( &epilog->ops );
    fprintf( TFile, "\n" );
  }

  // Generate notes for prolog.
  NOTE_SWP_HEAD* note = TYPE_MEM_POOL_ALLOC( NOTE_SWP_HEAD, &MEM_pu_nz_pool );
  note->const_trip = true;
  note->ntimes = sc - 1;
  NOTE_Add_To_BB( prolog, prolog_note_handler, (NOTE_INFO*)note );

  cl.Recompute_Liveness();  
  //cl.EBO_Before_Unrolling();

  Gen_Kernel_Info();

  // Update loop descriptor for kernel.
  const ANNOTATION* annot = ANNOT_Get( BB_annotations(kernel), ANNOT_LOOPINFO );
  LOOPINFO* info = ANNOT_loopinfo(annot);
  const TN* trip_count = LOOPINFO_trip_count_tn(info);

  if( TN_is_constant(trip_count) ){
    int new_trip_count_val = ( TN_value(trip_count) - sc + 1 ) / Kmin;
    LOOPINFO_trip_count_tn(info) =
      Gen_Literal_TN(new_trip_count_val, TN_size(trip_count));
  }
}


/* Peel the first ( <trip_count> % <ntimes> ) trips off the kernel.
 */
void KEY_SCH::Loop_Peeling( const int ntimes, TN* trip_count_tn ) const
{
  if( ntimes < 2 )
    return;

  const INT32 trip_size = TN_size( trip_count_tn );
  const float exit_prob = 1.0 / ntimes;
  const BOOL freqs = FREQ_Frequencies_Computed();
  OPS ops = OPS_EMPTY;

  BB* loop_entry = Gen_BB_Like( prolog );
  Link_Pred_Succ_with_Prob( prolog, loop_entry, exit_prob );

  if( freqs || BB_freq_fb_based(prolog) )
    BB_freq( loop_entry ) += BB_freq(prolog) * exit_prob;
  if( freqs )
    Change_Succ_Prob( prolog, BB_next(prolog), 1.0 - exit_prob );

  // trip_count = trip_count % ntimes

  if( is_power_of_two(ntimes) )
    Exp_OP2( trip_size == 4 ? OPC_U4BAND : OPC_U8BAND,
	     trip_count_tn,
	     trip_count_tn,
	     Gen_Literal_TN(ntimes-1, trip_size),
	     &ops );
  else
    Exp_OP2( trip_size == 4 ? OPC_U4MOD : OPC_U8MOD,
	     trip_count_tn,
	     trip_count_tn,
	     Gen_Literal_TN(ntimes, trip_size),
	     &ops );

  Exp_OP3v( OPC_FALSEBR,
	    NULL,
	    Gen_Label_TN( Gen_Label_For_BB(loop_entry), 0 ),
	    trip_count_tn,
	    Zero_TN,
	    V_BR_I8EQ,
	    &ops );

  BB_Append_Ops( prolog, &ops );

  OP* op = NULL;
  BB* new_body = Gen_BB_Like( kernel );

  OPS_Remove_All( &ops );

  FOR_ALL_BB_OPs( kernel, op ){
    if( !OP_br( op ) ){
      OP* new_op = Dup_OP(op);
      CG_LOOP_Init_Op( new_op );
      Copy_WN_For_Memory_OP( new_op, op );
      BB_Append_Op( new_body, new_op );
    }
  }

  Exp_OP2( trip_size == 4 ? OPC_I4ADD : OPC_I8ADD,
	   trip_count_tn,
	   trip_count_tn,
	   Gen_Literal_TN(-1, trip_size),
	   &ops );

  Exp_OP3v( OPC_TRUEBR,
	    NULL,
	    Gen_Label_TN( Gen_Label_For_BB(new_body), 0 ),
	    trip_count_tn,
	    Zero_TN,
	    V_BR_I8NE,
	    &ops );

  BB_Append_Ops( new_body, &ops);

  INT64 trip_est = ( TN_is_constant(trip_count_tn) ? TN_value(trip_count_tn)
		     : MIN((1 + ntimes) / 2, 2) );

  Link_Pred_Succ_with_Prob( new_body, new_body, 1.0 - exit_prob );
  //Link_Pred_Succ_with_Prob( new_body, remainder_tail, exit_prob );
  //Chain_BBs( new_body, body );

  float body_freq = 0.0;

  if( freqs || BB_freq_fb_based(kernel) ){
    /* BB_freq(body) doesn't yet include edge from prolog. */
    body_freq = BB_freq(CG_LOOP_prolog) * (trip_est - 1);
    if (freqs && trip_est > 0)
      BBLIST_prob(BB_succs(new_body)) = (trip_est - 1.0) / trip_est;
  }

  append_to_prolog( new_body );
  append_to_prolog( loop_entry );

  if( freqs || BB_freq_fb_based(new_body) )
    BB_freq(new_body) = body_freq;    
}


void KEY_SCH::Peeling_For_Unknown_Trip( CG_LOOP& cl, TN* trip_count_tn )
{
  const int trip_size = TN_size(trip_count_tn);
  OPS ops = OPS_EMPTY;
  const BOOL freqs = FREQ_Frequencies_Computed();
  const ANNOTATION* annot = ANNOT_Get( BB_annotations(kernel), ANNOT_LOOPINFO );
  LOOPINFO* info = ANNOT_loopinfo(annot);
  const WN* wn = LOOPINFO_wn(info);

  // Used to wrap up the orignal kernel.
  extend_prolog();
  extend_epilog( cl.Loop() );
  cl.Recompute_Liveness();

  // If trip_cnt < Kmin + sc - 1, then execute the original loop instead.

  Exp_OP3v( OPC_FALSEBR,
	    NULL,
	    Gen_Label_TN( Gen_Label_For_BB(CG_LOOP_prolog), 0 ),
	    trip_count_tn,
	    Gen_Literal_TN( (Kmin+sc-1), trip_size ),
	    V_BR_I8GE,
	    &ops );

  BB_Append_Ops( prolog, &ops );

  OP* op = NULL;
  BB* new_kernel = Gen_BB_Like( kernel );

  FOR_ALL_BB_OPs( kernel, op ){
    OP* new_op = Dup_OP( op );
    if( OP_br( new_op ) ){
      Set_OP_opnd( new_op,
		   Branch_Target_Operand(new_op),
		   Gen_Label_TN( Gen_Label_For_BB(new_kernel), 0 ) );
    }
    CG_LOOP_Init_Op( new_op );
    Copy_WN_For_Memory_OP( new_op, op );
    BB_Append_Op( new_kernel, new_op );
  }

  if( freqs || BB_freq_fb_based(kernel) )
    BB_freq( new_kernel ) = BB_freq( prolog ); 

  Chain_BBs( prolog, new_kernel );
  Link_Pred_Succ_with_Prob( prolog, new_kernel, 0.1F );
  Change_Succ_Prob( prolog, CG_LOOP_prolog, 0.9F );
  Link_Pred_Succ_with_Prob( new_kernel, new_kernel, 
			    (WN_loop_trip_est(wn) - 1.0) / WN_loop_trip_est(wn) );

  BB* goto_bb = Gen_BB_Like( epilog );
  Chain_BBs( new_kernel, goto_bb );
  Add_Goto( goto_bb, epilog );
  Link_Pred_Succ_with_Prob( new_kernel, goto_bb, 1.0 / WN_loop_trip_est(wn) );
  Chain_BBs( goto_bb, CG_LOOP_prolog );

  /* Now we can handle the peeling now. */

  prolog = CG_LOOP_prolog;
  epilog = CG_LOOP_epilog;

  // new_trip_cnt = trip_cnt - ( sc - 1 )

  TN* new_trip_count = Build_TN_Like( trip_count_tn );
  
  OPS_Remove_All( &ops );
  Exp_OP2( trip_size == 4 ? OPC_I4ADD : OPC_I8ADD,
	   new_trip_count,
	   trip_count_tn,
	   Gen_Literal_TN( -(sc-1), trip_size ),
	   &ops );

  BB_Append_Ops( prolog, &ops );

  Loop_Peeling( Kmin, new_trip_count );

  if( trace )
    CG_LOOP_Trace_Loop( cl.Loop(), "**** After Loop Pre-conditioning ****" );


  prolog = CG_LOOP_prolog;
  epilog = CG_LOOP_epilog;
}


void KEY_SCH::Peeling_For_Known_Trip( CG_LOOP& cl, TN* trip_count_tn )
{
  INT64 trips = TN_value( trip_count_tn ) - ( sc - 1 );
  Is_True( trips >= ( Kmin + sc - 1 ), ("") );

  if( ( trips % Kmin ) == 0 )
    return;

  if( ( trips % Kmin ) == 1 ){
    OP* op = NULL;
    BB* new_bb = Gen_BB_Like( prolog );

    FOR_ALL_BB_OPs( kernel, op ){
      if( !OP_br( op ) ){
	OP* new_op = Dup_OP(op);
	Set_OP_orig_idx( new_op, OP_map_idx(op) );
	Set_OP_unroll_bb( new_op, new_bb );
	CG_LOOP_Init_Op( new_op );
	Copy_WN_For_Memory_OP( new_op, op );
	BB_Append_Op( new_bb, new_op );
      }
    }

    if( BB_freq_fb_based( prolog ) )
      Set_BB_freq_fb_based(new_bb);
    
    append_to_prolog( new_bb );
    prolog = CG_LOOP_prolog;

    return;
  }

  trips = TN_value( trip_count_tn ) - ( sc - 1 );
  Set_TN_value( trip_count_tn, trips );

  // If ( sc - 1 ) is too big, we might consider using Loop_Peeling() instead.
  Unroll_Make_Remainder_Loop( cl, Kmin );
  
  trips += ( sc - 1 );
  Set_TN_value( trip_count_tn, trips );
}


/* The kernel must be executed at least ( Kmin*i + (sc-1) ) times.
 */
void KEY_SCH::Loop_Preconditioning( CG_LOOP& cl )
{
  const ANNOTATION* annot = ANNOT_Get( BB_annotations(kernel), ANNOT_LOOPINFO );
  LOOPINFO* info = ANNOT_loopinfo(annot);
  TN* trip_count_tn = LOOPINFO_trip_count_tn(info);

  // The branch op will be removed by Unroll_Make_Remainder_Loop().
  OP* org_br_op = BB_branch_op( kernel );
  const bool br_op_is_last = BB_last_op(kernel) == org_br_op;
  OP* br_op = Dup_OP( org_br_op );

  CG_LOOP_Init_Op( br_op );
  Copy_WN_For_Memory_OP( br_op, org_br_op );
  Set_BB_unrollings( kernel, 0 );

  if( TN_is_constant(trip_count_tn) ){
    Peeling_For_Known_Trip( cl, trip_count_tn );

  } else {
    Peeling_For_Unknown_Trip( cl, trip_count_tn );
  }


  // Otherwise, OP_info will be messed up.
  Is_True( kernel == cl.Loop_header(), ("") );
  prolog = CG_LOOP_prolog;
  epilog = CG_LOOP_epilog;

  if( BB_branch_op( kernel ) == NULL ){
    if( br_op_is_last )
      BB_Append_Op( kernel, br_op );
    else
      BB_Insert_Op_Before( kernel, BB_last_op(kernel), br_op );

    // Migrate the OP_info for org_br_op to br_op.
    OP_info_move( org_br_op, br_op );
  }
}


/* Estimate the number of copies of kernel we need to
   make to avoid live-range overlapping across different
   iterations.
   This routine must be called before kernel is rebuilt due to
   the ddg info.
*/
void KEY_SCH::Compute_Kmin()
{
  OP* op = NULL;

  Kmin = 1;

  FOR_ALL_BB_OPs( kernel, op ){
    int start = OP_info_cycle( op );
    int end = start - 1;

    for( ARC_LIST* arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs) ){
      ARC* arc = ARC_LIST_first(arcs);
      
      if( ARC_kind(arc) == CG_DEP_REGIN ){
	OP* succ_op = ARC_succ(arc);
	int c = OP_info_cycle( succ_op );
	if( c > end ){
	  end = c;
	  OP_info_last_use( op ) = succ_op;
	}
      }
    }

    // Assume that the latency for waw can be 0.
    if( end > start ){
      int copies = 1 + ( end - start - 1 ) / mii;
      Kmin = MAX( Kmin, copies );
    }
  }
}


void KEY_SCH::Loop_Unrolling( CG_LOOP& cl, int ntimes )
{
  Is_True( ntimes > 1, ("") );

  Unroll_Do_Loop( cl, ntimes );

  cl.Recompute_Liveness();
  cl.EBO_Before_Unrolling();
  Fix_Backpatches( cl, trace );

  kernel = cl.Loop_header();
  prolog = CG_LOOP_prolog;
  epilog = CG_LOOP_epilog;

  unrolls *= ntimes;
}


void KEY_SCH::Delete_Backpatches()
{
  for( CG_LOOP_BACKPATCH* bp = CG_LOOP_Backpatch_First(prolog, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp) ){
    CG_LOOP_Backpatch_Delete( prolog, bp );
  }

  for( CG_LOOP_BACKPATCH* bp = CG_LOOP_Backpatch_First(epilog, NULL);
       bp != NULL;
       bp = CG_LOOP_Backpatch_Next(bp) ){
    CG_LOOP_Backpatch_Delete( epilog, bp );
  }
}


KEY_SCH::KEY_SCH( CG_LOOP& cl, BB* _prolog, BB* _epilog, bool trace )
  : prolog( _prolog ), epilog( _epilog ), trace( trace ), perform_swp( true )
{
  OP* op = NULL;

  int max_omega = 0;
  CXX_MEM_POOL sas_local_pool( "sas pool", FALSE );
  mem_pool = sas_local_pool();

  strcpy( func_name, "swp" );
  unrolls = 1;

  /* First, if we encounter an omega > 1, then either perform unrolling,
     or call CG_LOOP_Remove_Notations() to fix it. */

  kernel = cl.Loop_header();

  FOR_ALL_BB_OPs( kernel, op ){

    // Skip loops with have div or sqrt op.
    if( OP_fdiv( op ) || OP_sqrt( op) ){
      Gen_Kernel_Fail_Info( SKIP_FDIV_SQRT );
      return;
    }

    Is_True( OP_results( op ) < 2, ("") );
    for( int opnd = 0; opnd < OP_opnds(op); opnd++ ){
      TN* tn = OP_opnd( op, opnd );
      max_omega = MAX( max_omega, OP_omega( op, opnd ) );
    }
  }


  CG_LOOP_Remove_Notations( cl.Loop(), prolog, epilog );
  Delete_Backpatches();
  cl.Recompute_Liveness();

  if( trace ){
    CG_LOOP_Trace_Loop( cl.Loop(), "**** Before KEY_SWP ****" );
  }

  // Invokes CG_DEP_Compute_Graph, deconstructor deletes graph.
  CYCLIC_DEP_GRAPH cyclic_graph( kernel, mem_pool );
  if( trace )
    CG_DEP_Trace_Graph( kernel );

  success = true;
  Schedule_Kernel();

  if( !success )
    return;

  Compute_Kmin();

  if( trace )
    fprintf( TFile, "KEY_SWP: Kmin = %d sc = %d\n", Kmin, sc );

  // Do not go any further if register allocaion fails.
  register_allocation_init();
  if( !success )
    return;

  prolog = CG_LOOP_prolog;
  epilog = CG_LOOP_epilog;
  glue_prolog = glue_epilog = NULL;

  Loop_Preconditioning( cl );

  // Now we have a kernel which will be executed exactly (Kmin*i + sc - 1) times
  Gen_PKE( cl );

  Is_True( success, ("") );

  // Delete all the loop backpatches here.
  Delete_Backpatches();

  //Print_All_BBs();
  
  if( trace ){
    CG_LOOP_Trace_Loop( cl.Loop(), "**** After KEY_SWP ****" );
  }
}
