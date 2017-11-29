%{
#include "pqstest.h"
#include "pqs_defs.h"
#include "pqsstubs.h"
#include "pqs.h"
#include "pqstest.tab.h"

void intvalue(char *p_in)
{
   int val=0,negate=0;
   char *p;
   p = p_in;
   if (*p == '-') {
      negate = 1;
      ++p;
   }
   while (*p) {
      val = 10*val + (*p - '0');
      ++p;
   }
   if (negate) val = -val;
   yylval.ival = val;
}

%}
D [0-9-]
%%
#.*
"END_BASIC_BLOCK" {return END_BASIC_BLOCK;}
"END" {return END_BASIC_BLOCK;}
"DUMP" {return DUMP;}
"(" {return LPAREN;}
")" {return RPAREN;}
"[" {return LBRACK;}
"]" {return RBRACK;}
"," {return COMMA;}
P{D}+  {intvalue(yytext+1); return PRED;}
{D}+   {intvalue(yytext); return INTVALUE;}
"=" {return EQUALS;}
"NORM" {yylval.itype = PQS_ITYPE_NORM; return ITYPE;}
"UNC" {yylval.itype = PQS_ITYPE_UNC; return ITYPE;}
"OR" {yylval.itype = PQS_ITYPE_OR; return ITYPE;}
"AND" {yylval.itype = PQS_ITYPE_AND; return ITYPE;}
"ORANDCM" {yylval.itype = PQS_ITYPE_ORANDCM; return ITYPE;}
"ORCM" {yylval.itype = PQS_ITYPE_ORCM; return ITYPE;}
"ANDCM" {yylval.itype = PQS_ITYPE_ANDCM; return ITYPE;}
"ANDORCM" {yylval.itype = PQS_ITYPE_ANDORCM; return ITYPE;}
"DIVSQRT" {yylval.itype = PQS_ITYPE_DIVSQRT; return ITYPE;}
[ \t]+ ;
"\n"
"&" {return AND;}
"<" {return SUBSET;}
">" {return COVER;}
"TRUE" {return TRUE_T;}
"FALSE" {return FALSE_T;}
%%
