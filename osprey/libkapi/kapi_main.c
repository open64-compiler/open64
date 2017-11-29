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

#include <conio.h>

#include <stdlib.h>

#include <string.h>
#include "kapi.h"

#include "kapi_debug.h"

#include "kapi_ia64.h"
#include "kapi_save_source_ia64.h"

static void  Test_bv128Rtns();
static void  Test_bv64Rtns();
static void  Test_bv32Rtns();
//extern int trying123;



void dump_instruction_fus(FILE *fp, void *pknobs)

{

	int num_of_insts,iid,dummy;
	kapi_fu_t fu;

	typedef struct {
		int x;
		int *ix;
		char *gogo;
		int nx[2];
	} xtest;
	typedef struct {
		int nxs;
		xtest *pxtest;
		xtest *ppx[2];
		char *dx;
		xtest x[2];
	} ytest;

	static int yy[]={5,5};


	static xtest xx[]= {{2,yy,"gogo",{0,0}},
						{2,yy,"gogo",{0,0}}};
	static xtest zz[2]= {{2,yy,"gogo",{0,0}},
						{2,yy,"gogo",{0,0}}};

	static ytest ylast={2,
						&(zz[1]),
						{xx,zz},
						"gugunza",
	};

	char *str;
	char *cIdx;
	char buffer[100];
	int idx;
	int iVal;
	//void *trying=&trying123;
	FILE *fpTab=fopen("tryTab.c","w");
	int expressions=KAPI_EnumCardinality(pknobs,"expressions");
	dummy=KAPI_cportMask4ut(pknobs,0,kapi_utI);
	dummy=KAPI_cportMask4ut(pknobs,0,kapi_utM);
	dummy=KAPI_cportMask4ut(pknobs,0,kapi_utF);
	dummy=KAPI_cportMask4ut(pknobs,0,kapi_utB);
	
	
	dummy=KAPI_fuCount(pknobs);
	num_of_insts=KAPI_iidCount(pknobs);
	_strset(buffer,0);
	str="#include \"tryhead.h\"\n\ninst_info_tab_t infoTab[] = {\n";
	fwrite(str,sizeof(char),strlen(str),fp);
	for (iid=0;iid<num_of_insts;iid++)
	{
		_strset(buffer,0);
		str=KAPI_iid2uniqueName(pknobs,iid,dummy);
		fwrite(str,sizeof(char),strlen(str),fp);
		fwrite(",",sizeof(char),1,fp);
		fu=KAPI_iid2fu(pknobs,iid,dummy);
		strcat(buffer,"/* ");
		strcat(buffer,KAPI_fu2fuName(pknobs,fu,dummy));
		strcat(buffer," */");
		fwrite(buffer,sizeof(char),strlen(buffer),fp);
		*buffer='\0';
		_itoa(fu,buffer,10);
		fwrite(buffer,sizeof(char),strlen(buffer),fp);
		fwrite(",",sizeof(char),1,fp);
		fwrite("\n",sizeof(char),1,fp);
	}
	_strset(buffer,0);
	str="0,0\n};\n/*";
	fwrite(str,sizeof(char),strlen(str),fp);
	
	for (idx=0;idx<expressions;idx++)
	{
		*buffer='\0';
		cIdx=KAPI_EnumName(pknobs,idx,"expressions");
		iVal=KAPI_GetIntegerVariable(pknobs,"expr",idx);
		sprintf(buffer,"[%s] = %d\n",cIdx,iVal);
		fwrite(buffer,sizeof(char),strlen(buffer),fp);
	}
	_strset(buffer,0);
	str="*/\n\n";
	fwrite(str,sizeof(char),strlen(str),fp);
	KAPI_save_as_header_all_IA64_info( fp, pknobs , "trying123");
	printf("finished try\n");
	fclose(fp);
	fclose(fpTab);
	//KAPI_fEnableIA64call_from_header(&pknobs,trying,0);
	//_getch();
}


int main( int argc, char *argv[] )
{
   void *pvoid1;
   FILE *fp1=NULL,*fp2=NULL;
   FILE *outp=NULL;
   FILE *Ferr=NULL;

   char *toolname;



   if (( (toolname=strrchr(argv[ 0],'\\')) ==NULL) && 
	   ( (toolname=strrchr(argv[ 0],'/')) ==NULL) )
	   toolname=argv[ 0];

 

	   
   if ( argc < 2 ) {
      printf("%s knobs toolname [delta knobs]\n", toolname );
      exit(1);
   } 

   if ( 5 <= argc )
	   freopen(argv[4],"w",stderr);
   if ( 4 <= argc ) /*delta file exists*/
	fp2 = fopen( argv[ 3 ], "r" );
   fp1 = fopen( argv[ 1 ], "r" );
   outp = fopen( "try.c", "w" );
   if ((fp1==NULL) && (fp2==NULL)) 
   {
	   printf("Could not open knobsfiles!\n");
	   return 2;
   }
   pvoid1 = KAPI_Initialize( fp2, fp1, argv[ 2 ] );
   fclose( fp1 );

   if (NULL!=fp2) fclose( fp2 );
   if ((5 <=argc ) && (NULL!=Ferr)) fclose( Ferr );

   if ( pvoid1 ) {
      int fuLD;
/*
      KDebug_DumpMachineDescription( stdout,  pvoid1, 0 );

      KDebug_DumpInterClusterBypass( stdout, pvoid1 );
      KDebug_DumpClusterDistances( stdout, pvoid1 );
      KDebug_DumpIntraClusterBypass( stdout, pvoid1 );
      KDebug_DumpLatencies( stdout, pvoid1 );
      KDebug_DumpTotalLatency( stdout, pvoid1 );
*/
      fuLD = KAPI_EnumIndex( pvoid1, "fu_t", "fuLD" );
      printf( "fuLD has %d ports\n", 
             KAPI_cportCount4fu( pvoid1, 0, fuLD ) );


	  dump_instruction_fus(outp,pvoid1);
      KAPI_Finalize( pvoid1 );
   } else {
      fprintf( stderr, "KAPI_Initialize failed\n" );
   }


   return 0;
}

static void
Test_bv32Rtns()
{
}

static void
Test_bv64Rtns()
{
   bv64_t bv64Tmp, bv64Tmp2;
   char mpch[ 200 ];
   int i;

   /* test set bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      SETBIT_bv64( bv64Tmp, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp, mpch ) );
   }

   /* test shiftL bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 4 );
      SHL_bv64( bv64Tmp2, bv64Tmp, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp2, mpch ) );
   }

   /* test shiftL bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 20 );
      SHL_bv64( bv64Tmp2, bv64Tmp, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp2, mpch ) );
   }

   /* test shiftL bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 40 );
      SHL_bv64( bv64Tmp2, bv64Tmp, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp2, mpch ) );
   }

   /* test shiftR bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 4 );
      SHL_bv64( bv64Tmp2, bv64Tmp, 60 );
      SHRL_bv64( bv64Tmp, bv64Tmp2, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp, mpch ) );
   }

   /* test shiftR bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 20 );
      SHL_bv64( bv64Tmp2, bv64Tmp, 44 );
      SHRL_bv64( bv64Tmp, bv64Tmp2, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp, mpch ) );
   }

   /* test shiftR bit */
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 40 );
      SHL_bv64( bv64Tmp2, bv64Tmp, 24 );
      SHRL_bv64( bv64Tmp, bv64Tmp2, i );
      printf("Bit %2d = %s\n", i, bv642pch( bv64Tmp, mpch ) );
   }

   /* test extract bit */
   ZERO_bv64( bv64Tmp );
   ONES_bv64( bv64Tmp, 8 );
   SHL_bv64( bv64Tmp2, bv64Tmp, 28 );
   printf("Initial string %s\n", bv642pch( bv64Tmp2, mpch ) );
   for ( i=0; i<=64; i++ ) {
      ZERO_bv64( bv64Tmp );
      ONES_bv64( bv64Tmp, 8 );
      SHL_bv64( bv64Tmp2, bv64Tmp, 28 );
      EXTRACTU_bv64( bv64Tmp, bv64Tmp2, 4, i );
      printf("Extract len=%d pos %2d = %s\n", 4, i, bv642pch( bv64Tmp, mpch ) );
   }
}

static void
Test_bv128Rtns()
{
}
