%token LPAREN RPAREN EQUALS END_BASIC_BLOCK COMMA LBRACK RBRACK DUMP
%token AND SUBSET COVER TRUE_T FALSE_T
%token <itype> ITYPE
%token <ival> PRED  INTVALUE
%type <predset> pred_list pred_set
%type <ival> optional_modifier
%start thing
%{
#include "pqstest.h"
#include "pqs_defs.h"
#include "pqsstubs.h"
#include "pqs.h"

#include <map.h>

extern int yylex(void);

PQS_MANAGER pqsm;

typedef TN *TNp;

typedef map<TN_NUM, TNp> tnmap_t;
tnmap_t tnmap;

TN * get_a_tn(TN_NUM t)
{
   tnmap_t::iterator p;
   p = tnmap.find(t);
   if (p == tnmap.end()) {
      TN * new_tn = new TN;
      new_tn->num = t;
      PQS_TN_set_last_definition(new_tn,PQS_IDX_NONE);
      tnmap[t] = new_tn;
   }
   return tnmap[t];
}

static int numinst = 0;

void init_tn0(void)
{
   TN *t=get_a_tn(0);
   PQS_TN_set_last_definition(t,PQS_IDX_TRUE);
   pqsm.PQS_TN_P0 = t;
}





void build_inst(int pq, int p1, int p2, PQS_ITYPE itype, int cnum)
{
   PQS_NODE_IDX idx;
   OP * op = new OP(itype,get_a_tn(pq),get_a_tn(p1),get_a_tn(p2));
   idx = pqsm.PQS_Add_Instruction(op);
   if (cnum == 1) pqsm.PQS_NODE_set_condition_false(idx);
   if (cnum == 2) pqsm.PQS_NODE_set_condition_true(idx);
   ++numinst;
}

void disjoint_query(int p1, int p2)
{
   BOOL answer;
   TN * t1,*t2;
   t1 = get_a_tn(p1);
   t2 = get_a_tn(p2);
   answer = pqsm.PQS_is_disjoint(t1,t2);
   printf("is_disjoint P%d P%d = %d\n",p1,p2,answer);
}

void subset_query(int p1, int p2)
{
   BOOL answer;
   TN * t1,*t2;
   t1 = get_a_tn(p1);
   t2 = get_a_tn(p2);
   answer = pqsm.PQS_is_subset_of(t1,t2);
   printf("is_subset_of {%d} {%d} = %d\n",p1,p2,answer);
}

void subset_query(int p1, PQS_TN_SET *p2)
{
   BOOL answer;
   TN * t1;
   t1 = get_a_tn(p1);
   answer = pqsm.PQS_is_subset_of(t1,*p2);
   printf("is_subset_of {%d} %s = %d\n",p1,p2->Get_Set_String(),answer);
}

void subset_query(PQS_TN_SET *p1, PQS_TN_SET *p2)
{
   BOOL answer;
   answer = pqsm.PQS_is_subset_of(*p1,*p2);
   printf("is_subset_of %s %s = %d\n",p1->Get_Set_String(),p2->Get_Set_String(),answer);
}


void cover_query(PQS_TN_SET *p1, int p2)
{
   printf("covers %s {%d} = ?? \n",p1->Get_Set_String(), p2);
}

main(int argc, char **argv) {
   extern int yyparse(void);
   extern int yydebug;

   init_tn0();
   
   if (argc > 1) {
      yydebug = 1;
   }


   printf("start\n");
   yyparse();
}

extern "C" {
   void yywrap(void){}
}


void yyerror (const char * x) {
   printf ("parse error |%s|\n",x);
}


%}

%union {
   int ival;
   PQS_ITYPE itype;
   PQS_TN_SET *predset;
}

%%

thing: 
   program |
   program {printf("Processed %d instructions\n",numinst);} query_list
;

program: instruction_list END_BASIC_BLOCK

instruction_list:
   instruction |
   instruction_list instruction
;

instruction: 
  LPAREN PRED RPAREN PRED COMMA PRED EQUALS ITYPE optional_modifier
     {build_inst($2,$4,$6,$8,$9);} |
  PRED COMMA PRED EQUALS ITYPE optional_modifier
     {build_inst(0,$1,$3,$5,$6);}  | 
  DUMP
     {pqsm.Print_all();}  
;

optional_modifier:     
      {$$ = 0;} | 
      LBRACK INTVALUE RBRACK {$$ = $2;} |
      LBRACK FALSE_T RBRACK {$$ = 1;}   |
      LBRACK TRUE_T RBRACK  {$$ = 2;}
;


query_list:
     query |
     query_list query
;


query:
     disjoint_query |
     subset_query |
     cover_query
;
     
disjoint_query:
     PRED AND PRED {disjoint_query($1,$3);} ;

subset_query:
     PRED SUBSET PRED         {subset_query($1,$3);} | 
     PRED SUBSET pred_set     {subset_query($1,$3);} | 
     pred_set SUBSET pred_set {subset_query($1,$3);} 
     ;

cover_query:
     pred_set COVER PRED {cover_query($1,$3);} 
;

pred_set:
     LPAREN pred_list RPAREN {$$ = $2;} 
;

pred_list:
     PRED {$$ = new PQS_TN_SET; $$->Insert(get_a_tn($1));} |
     pred_list COMMA PRED {$1->Insert(get_a_tn($3)); $$ = $1;}
;


%%
