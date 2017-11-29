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

#include <assert.h>
#include <string.h>
#include "kapi_internal.h"

char *
kapi_pchCopy( char *pch )
{
   if ( pch == NULL ) {
      return( NULL );
   } else {
      return( strdup( pch ) );
   }
}

char
kapi_ut2ch( kapi_ut_t ut )
{
   static char ut2pch[ kapi_nUT ];
   static int fFirst = 1;

   if (fFirst) {
      ut2pch[ kapi_utI ] = 'I';
      ut2pch[ kapi_utF ] = 'F';
      ut2pch[ kapi_utB ] = 'B';
      ut2pch[ kapi_utM ] = 'M';
      fFirst = 0;
   }
   return( ut2pch[ ut ] );
}

char
kapi_syl2ch( kapi_syl_t syl )
{
   static char syl2ch[ kapi_nSYL ];
   static int fFirst = 1;

   if (fFirst) {
      syl2ch[ kapi_sylI ] = 'I';
      syl2ch[ kapi_sylF ] = 'F';
      syl2ch[ kapi_sylB ] = 'B';
      syl2ch[ kapi_sylM ] = 'M';
      syl2ch[ kapi_sylL ] = 'L';
      fFirst = 0;
   }
   return( syl2ch[ syl ] );
}
