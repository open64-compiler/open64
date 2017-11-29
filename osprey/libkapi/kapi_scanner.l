/*
 * Copyright (c) 2000, Intel Corporation
 * All rights reserved.
 *
 * WARRANTY DISCLAIMER
 *
 * THESE MATERIALS ARE PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL OR ITS 
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THESE
 * MATERIALS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Intel Corporation is the author of the Materials, and requests that all
 * problem reports or change requests be submitted to it directly at
 * http://developer.intel.com/opensource.
 */


/* static char sccs_id[] = "%W%  %G%  %U%"; */

%{

#include "kapi_internal.h"
#include "kapi_parse.h"
#include "kapi_error.h"
#include "kapi_util.h"
#include "kapi_y.tab.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

extern knobs_t *kapi_pknobsCurrent;
extern int kapi_cLine;      /* Keeps track of the current line number */
extern YYSTYPE yylval, yyval;
void ProcessStr();

%}

comment  	"#"([^\n]*)
delim   	[ \t\f]
newline		[\n]
ws		{delim}+
alpha		[a-zA-Z]
digit		[0-9]
unumber		{digit}*
minus		[\-]
int		{minus}*({digit}+)
float		{minus}*(({int}\.{unumber})|({unumber}\.{int}))
id  		{alpha}({digit}|{alpha}|[_])*
stringg		\"(\\\n|\\.|[^\"\n])*(\"|\n)
%a 3000
%p 3000

%%

{newline}	{ kapi_cLine++; }
{ws}		{ /* do nothing for now */ }
ARRAY		|
Array		|
array		{ return( tokARRAY ); }
BITMASK		|
Bitmask		|
bitmask		{ return( tokBITMASK ); }
EXPECT		|
Expect		|
expect		{ return( tokEXPECT ); }
APPEND		|
Append		|
append		{ return( tokAPPEND ); }
ENUM		|
Enum		|
enum		{ return( tokENUM ); }
LIMIT		|
Limit		|
limit		{ return( tokLIMIT ); }
VARIABLE	|
Variable	|
variable	{ return( tokVARIABLE ); }
ATTRIBUTE	|
Attribute	|
attribute	{ return( tokATTRIBUTE ); }
IMPLICIT	|
Implicit	|
implicit	{ return( tokIMPLICIT ); }
NONE		|
none		|
None		{ return( tokNONE ); }

INT		|
Int		|
int		{ 
		   yylval.pstnA = kapi_pstnLookup( kapi_pknobsCurrent,
                                                   (char *)yytext );
		   return( tokINT ); 
		}
REAL		|
Real		|
real		{ 
		   yylval.pstnA = kapi_pstnLookup( kapi_pknobsCurrent,
                                                   (char *)yytext );
		   return( tokREAL ); 
		}
STRING		|
String		|
string		{ 
		   yylval.pstnA = kapi_pstnLookup( kapi_pknobsCurrent,
                                                   (char *)yytext );
		   return( tokSTRING ); 
		}

TYPE		|
Type		|
type		{ return( tokTYPE ); }
noredefine	|
Noredefine	|
NoRedefine	|
NOREDEFINE	{ return( tokNOREDEFINE ); }
OF		|
Of		|
of		{ return( tokOF ); }

"+"		{
		    yylval.pch = kapi_pchCopy( (char *)yytext );
			return(tokPLUS);
		}
"/"		{
		    yylval.pch = kapi_pchCopy( (char *)yytext );
			return(tokDIV);
		}


"="		{ return( tokEQUALS ); }
","		{ return( tokCOMMA ); }
";"		{ return( tokSEMICOLON ); }
":="		{ return( tokCOLONEQUALS ); }
"+="		{ return( tokPLUSEQUALS ); }
":"		{ return( tokCOLON ); }
"*"		{ return( tokSTAR ); }
".."		{ return( tokDDOT ); }
"."		{ return( tokDOT ); }
"|"		{ return( tokVERTBAR ); }

"["		{ return( tokLSBRACK ); }
"<"		{ return( tokLABRACK ); }
"{"		{ return( tokLCBRACK ); }
"]"		{ return( tokRSBRACK ); }
">"		{ return( tokRABRACK ); }
"}"		{ return( tokRCBRACK ); }
"("		{ return( tokLPAREN ); }
")"		{ return( tokRPAREN ); }


{id}		{ 
		    yylval.pch = kapi_pchCopy( (char *)yytext );
		    return( tokIDENT );
		}
{int}		{ 
		    yylval.iValue = atoi((char *)yytext);
		    return( tokINTCONST );
	        }
{float}		{ 
		    yylval.rValue =  atof((char *)yytext);
		    return( tokREALCONST );
	        }
{stringg}	{ 
		  ProcessStr();
		  yylval.pch = kapi_pchCopy((char *)yytext);
		  return( tokSTRINGCONST );
		}
{comment}	{ }

.		{ 
	          kapi_Error_pch1( kapi_cLine, 1, "Unknown character %s found", (char *)yytext );
		}

%%

/*
 *  Processstr -- uses the global variables yytext and kapi_cLine.
 *   If a new line is detected in the procedure then kapi_cLine is incremented.
 *   yytext is checked to see if it has any \<cr>'s 
 *   and if necessary, are they removed.
 *
 */
void ProcessStr()
{
    int irgch = 0;
    int irgchSave;

    if (yytext[strlen(yytext)-1] != '"') {
	irgchSave = 0;
	if (yytext[strlen(yytext)-1] == '\n')
	    irgchSave = 1;
        yytext[strlen(yytext)-1] = 0;
	kapi_Error( kapi_cLine, 1, "Unclosed string" );
	if (irgchSave)
	    kapi_cLine++;
    }
    else {
	yytext[strlen(yytext)-1] = 0;
    }

    /* delete first char in string */

    while ( yytext[irgch] ) {
       yytext[ irgch ] = yytext[ 1+irgch ];
       irgch++;
    }
    irgch = 0;

    /* remove all \<cr>'s from the string */

    while (yytext[irgch+1]) {
        if ((yytext[irgch+1] == '\n') && (yytext[irgch] == '\\')) {
	    irgchSave = irgch;
	    ++kapi_cLine;
	    while (yytext[irgchSave] = yytext[irgchSave++ + 2])
	        /* nothing */;
        }
	else {
	    ++irgch;
        }
    }
}

/*
 *  yywrap -- is a dummy proc for yylex's use
 *  this procedure must be defined as the default 1 unless other actions
 * are desired when EOF is encountered 
 *
 */
int yywrap()
{
    return(1);
}
