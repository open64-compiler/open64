//
// Created by xc5 on 15/7/2018.
//

#ifndef OSPREY_JGEN_GLOBAL_H
#define OSPREY_JGEN_GLOBAL_H

extern int trace_verbose;
// an_error_severity error_threshold ;

/* Static data:	command	line information: */
//extern INT32 Argc;		/* Copy of argc */
//extern char **Argv;		/* Copy of argv */
extern INT32 Source_Arg ;	/* Number of current source arg */
extern char Dash [];

/* Internal flags: */
extern BOOL Echo_Flag ;	/* Echo command	lines */
extern BOOL Delete_IR_File ;	/* Delete SGIR file when done */

// KEY
extern bool Did_Not_Terminate_Region ;


extern gs_t *deferred_function_stack;
extern INT32 deferred_function_i;
extern INT32 deferred_function_max;
extern BOOL flag_keep_inline_functions ;
extern UINT32 WGEN_PU_count ;
extern BOOL finish_alias ;

// keep pu and pu_info
extern PU_Info *PU_Tree_Root;
extern PU_Info *PU_Info_Table     [258];


extern ST      *Return_Address_ST [258];
extern BOOL map_mempool_initialized ;
extern MEM_POOL Map_Mem_Pool;


// to manage the current function
extern gs_t curr_namespace_decl ;

extern int __ctors ;
extern int __dtors ;

extern gs_t *func_decl_stack ;
extern INT func_decl_stack_size ;
extern INT func_decl_stack_top;

extern PU_Info *PU_Tree_Root ;
extern int Reg_Parm_Count ;
extern BOOL SSE_Reg_Parm ;

extern void get_err_tables();
extern void logger(string str);
extern void logger(const char *str);

extern string int2str(int a);
extern string long2str(long a);
extern string ll2str(long long a);
extern string char2str(char a);
extern string double2str(double a);

namespace JGEN{
  class Config{
   public:
    static bool treat_static_as_global;
    static bool need_inliner;
    static BOOL Keep_Zero_length_structs;
    static bool processing_function_prototypes;
    static bool expanding_function_definition;
    static bool do_not_parse_common; //flag_no_common
    static bool lang_cplus;
    static bool lang_oop; // lang_java || lang_cplusˆ
  };
}

#endif //OSPREY_JGEN_GLOBAL_H
