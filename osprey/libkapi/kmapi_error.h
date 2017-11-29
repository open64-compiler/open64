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

#ifndef _KMAPI_ERROR_H
#define _KMAPI_ERROR_H


extern void kmapi_Error( int fTerminate, char *pch1 );
extern void kmapi_Error_i1( int fTerminate, char *pchFormat, int i );
extern void kmapi_Error_pch1( int fTerminate, char *pchFormat, char *pch );
extern void kmapi_Error_pch2( int fTerminate, char *pchFormat, char *pch1 ,char *pch2);

extern void kmapi_Warning( char *pch1 );
extern void kmapi_Warning_i1( char *pchFormat, int i );
extern void kmapi_Warning_i2( char *pchFormat, int i1, int i2 );
extern void kmapi_Warning_pch1( char *pchFormat, char *pch );

extern int kmapi_err_counter;


#endif
