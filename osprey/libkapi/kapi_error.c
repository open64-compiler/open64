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

#include <stdio.h>
#include <assert.h>
#include "kapi_error.h"

int kapi_cntSemanticWarning = 0;
int kapi_cntSyntaxWarning = 0;
int kapi_cntWarnings = 0;

int kapi_cntSemanticError = 0;
int kapi_cntSyntaxError = 0;

void
kapi_Error( int iLine, int fSyntax, char *pch1 )
{
   char *pchError;
   /* int fInBaseLine; unreferenced */

   if ( fSyntax ) {
      kapi_cntSyntaxError++;
      pchError = "Knobsfile Syntax Error";
   } else {
      kapi_cntSemanticError++;
      pchError = "Knobsfile Error";
   }
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: %s\n", pchError, pch1 );
   } else {
      fprintf( stdout, "%s: line %4d: %s\n", pchError, iLine, pch1 );
   }
}

void
kapi_Error_i1( int iLine, int fSyntax, char *pchFormat, int i )
{
   char *pchError;

   if ( fSyntax ) {
      kapi_cntSyntaxError++;
      pchError = "Knobsfile Syntax Error";
   } else {
      kapi_cntSemanticError++;
      pchError = "Knobsfile Error";
   }
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchError );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchError, iLine );
   }
   fprintf( stdout, pchFormat, i );
   fprintf( stdout, "\n" );
}

void
kapi_Error_pch1( int iLine, int fSyntax, char *pchFormat, char *pch1 )
{
   char *pchError;

   if ( fSyntax ) {
      kapi_cntSyntaxError++;
      pchError = "Knobsfile Syntax Error";
   } else {
      kapi_cntSemanticError++;
      pchError = "Knobsfile Error";
   }
   
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchError );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchError, iLine );
   }
   fprintf( stdout, pchFormat, pch1 );
   fprintf( stdout, "\n" );
}

void
kapi_Error_pch2( int iLine, int fSyntax, char *pchFormat, char *pch1, char *pch2 )
{
   char *pchError;

   if ( fSyntax ) {
      kapi_cntSyntaxError++;
      pchError = "Knobsfile Syntax Error";
   } else {
      kapi_cntSemanticError++;
      pchError = "Knobsfile Error";
   }
   
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchError );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchError, iLine );
   }
   fprintf( stdout, pchFormat, pch1, pch2 );
   fprintf( stdout, "\n" );
}

void
kapi_Error_pch3( int iLine, int fSyntax, char *pchFormat, 
                 char *pch1, char *pch2, char *pch3 )
{
   char *pchError;

   if ( fSyntax ) {
      kapi_cntSyntaxError++;
      pchError = "Knobsfile Syntax Error";
   } else {
      kapi_cntSemanticError++;
      pchError = "Knobsfile Error";
   }
   
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchError );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchError, iLine );
   }
   fprintf( stdout, pchFormat, pch1, pch2, pch3 );
   fprintf( stdout, "\n" );
}

void
kapi_Warning( int iLine, int fSyntax, char *pch1 )
{
   char *pchWarning;

   if ( fSyntax ) {
      kapi_cntSyntaxWarning++;
      pchWarning = "Knobsfile Syntax Warning";
   } else {
      kapi_cntSemanticWarning++;
      pchWarning = "Knobsfile Warning";
   }
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: %s\n", pchWarning, pch1 );
   } else {
      fprintf( stdout, "%s: line %4d: %s\n", pchWarning, iLine, pch1 );
   }
}

void
kapi_Warning_i2( int iLine, int fSyntax, char *pchFormat, int i1, int i2 )
{
   char *pchWarning;

   if ( fSyntax ) {
      kapi_cntSyntaxWarning++;
      pchWarning = "Knobsfile Syntax Warning";
   } else {
      kapi_cntSemanticWarning++;
      pchWarning = "Knobsfile Warning";
   }
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchWarning );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchWarning, iLine );
   }
   fprintf( stdout, pchFormat, i1, i2 );
   fprintf( stdout, "\n" );
}

void
kapi_Warning_i1( int iLine, int fSyntax, char *pchFormat, int i )
{
   char *pchWarning;

   if ( fSyntax ) {
      kapi_cntSyntaxWarning++;
      pchWarning = "Knobsfile Syntax Warning";
   } else {
      kapi_cntSemanticWarning++;
      pchWarning = "Knobsfile Warning";
   }
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchWarning );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchWarning, iLine );
   }
   fprintf( stdout, pchFormat, i );
   fprintf( stdout, "\n" );
}

void
kapi_Warning_pch1( int iLine, int fSyntax, char *pchFormat, char *pch1 )
{
   char *pchWarning;

   if ( fSyntax ) {
      kapi_cntSyntaxWarning++;
      pchWarning = "Knobsfile Syntax Warning";
   } else {
      kapi_cntSemanticWarning++;
      pchWarning = "Knobsfile Warning";
   }
   
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchWarning );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchWarning, iLine );
   }
   fprintf( stdout, pchFormat, pch1 );
   fprintf( stdout, "\n" );
}

void
kapi_Warning_pch2( int iLine, int fSyntax, char *pchFormat, 
                   char *pch1, char *pch2 )
{
   char *pchWarning;

   if ( fSyntax ) {
      kapi_cntSyntaxWarning++;
      pchWarning = "Knobsfile Syntax Warning";
   } else {
      kapi_cntSemanticWarning++;
      pchWarning = "Knobsfile Warning";
   }
   
   if ( iLine < 0 ) {
      fprintf( stdout, "%s: ", pchWarning );
   } else {
      fprintf( stdout, "%s: line %4d: ", pchWarning, iLine );
   }
   fprintf( stdout, pchFormat, pch1, pch2 );
   fprintf( stdout, "\n" );
}

