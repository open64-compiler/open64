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


#ifndef _KAPI_H_
#define _KAPI_H_

#define KAPI_RELEASE 4.0

/* 
   ------- This header contains structures and definitions
   ------- needed to call raw KAPI interface routines.
*/

#define kapi_bv32PORTS_ALL (0xFFFFFFFF)

#include "kapi_bv.h"

/*
For description of the following routines, 
please refer to the kapi.doc file distributed with the release.
*/

/* ----------- General routines ---------------- */

extern void *KAPI_Initialize2( char *pchDelta, char *pchBaseline, 
                               char *pchToolname);
extern void *KAPI_Initialize( FILE *fpDelta, FILE *fpBaseline, 
                              char *pchToolname);
extern void  KAPI_Finalize( void *pConfig );

extern void  KAPI_DumpKnobs( void *pConfig, FILE *fp );


/* ------------------  Generic data query interface  ----------------------- */

extern int     KAPI_VariableCardinality( void *pConfig, char *pchAttribute );

extern int     KAPI_GetEnumVariable( void *pConfig, char *pchAttribute,
                                        int iIndex );
extern char   *KAPI_GetEnumVariableName( void *pConfig, char *pchAttribute,
                                        int iIndex );
extern int     KAPI_GetIntegerVariable( void *pConfig, char *pchAttribute,
                                        int iIndex );
extern double  KAPI_GetDoubleVariable( void *pConfig, char *pchAttribute,
                                        int iIndex  );
extern char   *KAPI_GetStringVariable( void *pConfig, char *pchAttribute,
                                        int iIndex  );
extern bv_t   *KAPI_GetBvVariable( void *pConfig, char *pchAttribute,
                                        int iIndex  );

extern int   KAPI_ArrayIndex( void *pConfig, char *pchArray, char *pchIndex );
extern int   KAPI_EnumIndex( void *pConfig, char *pchType, char *pchEnumName );
extern int   KAPI_EnumCardinality( void *pConfig, char *pchType );
extern char *KAPI_EnumName( void *pConfig, int enumconst, char *pchType );


/* ------------------  Unstructured data query interface  ---------------- */

extern int   KAPI_count4attribute( void *pConfig, char *pchAttribute );
extern char *KAPI_attribute4index( void *pConfig, char *pchAttribute, 
                                   int iIndex );

/* ------------------            Other           ---------------- */

extern int KAPI_error_attribute;
/* Version stuff */

/* Internal Version */
extern double KAPI_GetInternalVersion();
/* Tool version */
extern int KAPI_GetXVersion_MAJOR();
extern int KAPI_GetXVersion_MINOR();
/* API version */
extern int KAPI_GetAPIVersion_MAJOR();
extern int KAPI_GetAPIVersion_MINOR();

#endif

