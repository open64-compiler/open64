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

#ifndef _ERT_H_
#define _ERT_H_

extern void kapi_Warning( int iLine, int fSyntax, char *pch1 );
extern void kapi_Warning_i1( int iLine, int fSyntax, char *pchFormat, int i );
extern void kapi_Warning_pch1( int iLine, int fSyntax, char *pchFormat, char *pch );
extern void kapi_Warning_pch2( int iLine, int fSyntax, char *pchFormat, 
                               char *pch1, char *pch2 );

extern void kapi_Error( int iLine, int fSyntax, char *pch1 );
extern void kapi_Error_i1( int iLine, int fSyntax, char *pchFormat, int );
extern void kapi_Error_i2( int iLine, int fSyntax, char *pchFormat, int,  int );
extern void kapi_Error_pch1( int iLine, int fSyntax, char *pchFormat, char * );
extern void kapi_Error_pch2( int iLine, int fSyntax, char *pchFormat, char *, char * );
extern void kapi_Error_pch3( int iLine, int fSyntax, char *pchFormat, char *, char *, char * );

extern int kapi_cntSemanticError;
extern int kapi_cntSyntaxError;

extern int kapi_cntSemanticWarning;
extern int kapi_cntSyntaxWarning;

extern void kapi_error();

#endif
