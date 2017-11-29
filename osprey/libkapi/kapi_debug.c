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
#include "kapi_internal.h"
#include "kapi_parse.h"
#include "kapi_debug.h"
#include "kapi_util.h"
#include "kapi_error.h"
#include "string.h"

static void KDebug_Dcluster0Cutports( knobs_t *pknobs, FILE *fp, stn_t *pstn, int nTab );
void KDebug_DumpPortInfo( FILE *fp, knobs_t *pknobs, int nTab );

void
kapi_indent_line( FILE *fp, int nTab )
{
   int i;

   for ( i=0;i<nTab; i++ ) {
      fprintf( fp, "    " );
   }
}

void
KDebug_DumpMachineDescription( FILE *fp, knobs_t *pknobs, int nTab )
{
   int  iclr;
   /* int iLevel, nencTotal, i; unreferenced??? */
   

   kapi_indent_line( fp, nTab );
   fprintf( fp, "Machine Description\n" );
   fprintf( fp, "\n" );

   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "Machine issue width (bundles)\n" );
   for ( iclr=0; iclr<pknobs->nclr; iclr++ ) {
      kapi_indent_line( fp, nTab+2 );
      fprintf( fp, "cluster %d = %2d\n", iclr, 
                   KAPI_BundleIssueWidth( pknobs, iclr ) );
   }

   kapi_indent_line( fp, nTab );
   fprintf( fp, "\n\nPort/Issue Information:\n" );
   KDebug_DumpPortInfo( fp, pknobs, nTab+1 );
   fprintf( fp, "\n" );

   KDebug_DumpFUTable( fp, pknobs, nTab+1 );
   fprintf( fp, "\n" );

   KDebug_DumpITTable( fp, pknobs, nTab+1 );
   fprintf( fp, "\n" );

   KDebug_DumpS2SLatencyTable( fp, pknobs, nTab+1 );

}

void
KDebug_DumpPortMaps( FILE *fp, knobs_t *pknobs, int nTab )
{
   kapi_port_t port;
   kapi_cluster_t cluster;
   kapi_cport_t cport;
   kapi_ut_t ut;
   kapi_cutport_t cutport;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "Global port information:\n" );
   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "%5s: %8s %6s %4s %8s\n", "Port", "Cluster", "Cport", "Ut", "Cutport" );
   for ( port=0; port<pknobs->nports; port++ ) {
      KAPI_portInfo( pknobs, port, &cluster, &cport, &ut, &cutport );
      kapi_indent_line( fp, nTab+1 );
      fprintf( fp, "%5d: %8d %6d %4c %8d\n", 
               port, cluster, cport, kapi_ut2ch( ut ), cutport );
   }

   kapi_indent_line( fp, nTab );
   fprintf( fp, "Cport information:\n" );
   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "%8s %4s: %5s %6s %8s\n", "Cluster", "Cport", "Port", "Ut", "Cutport" );
   for ( cluster=0; cluster<pknobs->nclr; cluster++ ) {
      for ( cport=0; cport<pknobs->mpclrTable[cluster].ncports; cport++ ) {
         KAPI_cportInfo( pknobs, cluster, cport, &port, &ut, &cutport );
         kapi_indent_line( fp, nTab+1 );
         fprintf( fp, "%8d %4d: %5d %6c %8d\n", cluster, cport, 
                  port, kapi_ut2ch( ut ), cutport );
      }
   }

   kapi_indent_line( fp, nTab );
   fprintf( fp, "Cutport information:\n" );
   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "%8s %4s %4s: %5s %6s\n", "Cluster", "Ut", "Cutport", 
                "Port", "Cport" );
   for ( cluster=0; cluster<pknobs->nclr; cluster++ ) {
      for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
         for ( cutport=0; cutport<pknobs->mpclrTable[cluster].mpncutport[ut]; cutport++ ) {
            KAPI_cutportInfo( pknobs, cluster, ut, cutport, &port, &cport );
            kapi_indent_line( fp, nTab+1 );
            fprintf( fp, "%8d %4c %8d: %5d %6d\n", cluster, kapi_ut2ch( ut ), cutport,
                     port, cport );
         }
      }
   }
}


void
KDebug_DumpPortInfo( FILE *fp, knobs_t *pknobs, int nTab )
{
   int clr;
   kapi_syl_t syl;

   for ( clr=0; clr < pknobs->nclr; clr++ ) {
      kapi_indent_line( fp, nTab+1 );
      fprintf( fp, "Cluster %d (%2d units):  I=%d, M=%d, F=%d, B=%d\n", 
                   clr,
                   KAPI_cportCount( pknobs, clr ),
                   KAPI_cutportCount( pknobs, clr, kapi_utI ),
                   KAPI_cutportCount( pknobs, clr, kapi_utM ),
                   KAPI_cutportCount( pknobs, clr, kapi_utF ),
                   KAPI_cutportCount( pknobs, clr, kapi_utB ) );
   }
   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "All       (%2d units):  I=%d, M=%d, F=%d, B=%d\n", 
                   KAPI_cportCount( pknobs, -1 ),
                   KAPI_cutportCount( pknobs, -1, kapi_utI ),
                   KAPI_cutportCount( pknobs, -1, kapi_utM ),
                   KAPI_cutportCount( pknobs, -1, kapi_utF ),
                   KAPI_cutportCount( pknobs, -1, kapi_utB ) );


   kapi_indent_line( fp, nTab+1 );
   fprintf( fp, "Syllable dispersal:\n" );
   for ( syl=kapi_sylFIRST; syl<=kapi_sylLAST; syl++ ) {
      kapi_indent_line( fp, nTab+2 );
      fprintf( fp, "Can disperse %d %c syllables/clock\n", 
              KAPI_DisperseCount4syl( pknobs, syl ), kapi_syl2ch( syl ) );
   }

   KDebug_DumpPortMaps( fp, pknobs, nTab );
}

void
KDebug_DumpFUTable( FILE *fp, knobs_t *pknobs, int iLevel )
{
   int i,j,clr;
   char pchTemp[ 132 ], *pch;

   kapi_indent_line( fp, iLevel );
   fprintf( fp, "Function unit class (fu) description dump\n" );

   
   kapi_indent_line( fp, iLevel+1 );
   fprintf( fp, "%-15s ", "Class" );
   for ( clr=0; clr<pknobs->nclr; clr++ ) {
      clr_t *pclr;
 
      pclr = &(pknobs->mpclrTable[ clr ]);

      for ( i=0; i<pclr->ncports; i++ ) {
         cportinfo_t *pcportinfo;

         pcportinfo = pclr->mppcportinfoTable[i];
         assert( pcportinfo );
         pch = KAPI_EnumName( pknobs, pcportinfo->ut, "ut_t" );
         assert( pch );
         pchTemp[i] = pch[ 2 ];
      }
      pchTemp[ pclr->ncports ] = '\0';
      fprintf( fp, "%-12s ", pchTemp );
   }

   fprintf( fp, "%6s %-7s\n", "nUnits", "Latency" );



   for ( i=0; i<pknobs->nfuinfoTable; i++ ) {
      char *pchMnemonic;
      fuinfo_t *pfuinfo;

      kapi_indent_line( fp, iLevel+1 );

      pfuinfo = &(pknobs->dmpfuinfoTable[i]);
      pchMnemonic = pfuinfo->pchName;
      fprintf( fp, "%-15s ", KAPI_EnumName( pknobs, pfuinfo->fu, "fu_t" ) );

      for ( clr=0; clr<pknobs->nclr; clr++ ) {
         clr_t *pclr;
 
         pclr = &(pknobs->mpclrTable[ clr ]);

         for ( j=0; j<pclr->ncports; j++ ) {
            int b;
            cportinfo_t *pcportinfo;

            pcportinfo = pclr->mppcportinfoTable[j];
            
            assert( pcportinfo && pcportinfo->bvfuAllowed.pint32Data );
            b = ( 0 != isbvBITSET( &(pcportinfo->bvfuAllowed), i ) );
            pchTemp[ j ] = '0' + b;
         }
         pchTemp[ pclr->ncports ] = '\0';
         fprintf( fp, "%-12s ", pchTemp );
      }

      fprintf( fp, " %6d %s", KAPI_cportCount4fu( pknobs, -1, pfuinfo->fu ), "n/a" );
      fprintf( fp, "\n"  );
   }
}

void
KDebug_DumpITTable( FILE *fp, knobs_t *pknobs, int iLevel )
{
   int i;
   itinfo_t *pitinfo;
   /* char pchTemp[ 132 ]; unreferenced ??? */

   kapi_indent_line( fp, iLevel );
   fprintf( fp, "Instruction type description dump\n" );

   kapi_indent_line( fp, iLevel+1 );
   fprintf( fp, "%-15s %-7s %-9s %-20s\n", "Class", "IT", "Max Occ", "Syllables Allowed" );

   for ( i=0; i<pknobs->nitinfoTable; i++ ) {
      kapi_syl_t syl;

      pitinfo = &(pknobs->dmpitinfoTable[i]);
      kapi_indent_line( fp, iLevel+1 );
      fprintf( fp, "%-15s ", pitinfo->pchitName );
      fprintf( fp, "%-7s ", KAPI_EnumName( pknobs, pitinfo->it, "it_t" ) );
      fprintf( fp, "%9d ", pknobs->dmpitinfoTable[ pitinfo->it ].maxAvail );
      for ( syl=kapi_sylFIRST;syl<=kapi_sylLAST;syl++ ) {
         if ( pitinfo->bv32sylAllowed & ( 1 << syl ) ) {
            fprintf( fp, "%s ", KAPI_EnumName( pknobs, syl, "syl_t" ) );
         }
      }
      fprintf( fp, "\n" );
   }
}

void
KDebug_DumpS2SLatencyTable( FILE *fp, knobs_t *pknobs, int iLevel )
{
}

void
KDebug_Dumpmpit( FILE *fp, knobs_t *pknobs, int mpnitAvail[] )
{

   fprintf( fp, " (itI:%d)", pknobs->dmpitinfoTable[ kapi_itI ].maxAvail );

   fprintf( fp, " (itM:%d)", pknobs->dmpitinfoTable[ kapi_itM ].maxAvail );

   fprintf( fp, " (itA:%d)", pknobs->dmpitinfoTable[ kapi_itA ].maxAvail );

   fprintf( fp, " (itB:%d)", pknobs->dmpitinfoTable[ kapi_itB ].maxAvail );

   fprintf( fp, " (itBl:%d)", pknobs->dmpitinfoTable[ kapi_itBl ].maxAvail );

   fprintf( fp, " (itF:%d)", pknobs->dmpitinfoTable[ kapi_itF ].maxAvail );

   fprintf( fp, " (itL:%d)", pknobs->dmpitinfoTable[ kapi_itL ].maxAvail );
}


void
KDebug_DumpInstTable( FILE *fp, knobs_t *pknobs, int nTab )
{
   int i;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "%d mnemonics in instruction set\n", pknobs->ninstTable );

   kapi_indent_line( fp, nTab );
   fprintf( fp, "%5s   %-25s %-45s %-8s %-5s\n", "iid", "mnemonic", "unique", "fu", "it" );
   for ( i=0; i<pknobs->ninstTable; i++ ) {
      kapi_indent_line( fp, nTab );
      fprintf( fp, "%5d   %-25s %-45s %-8s %-5s\n",
                   i, 
                   pknobs->dmpinstTable[ i ].pchMnemonic,
                   pknobs->dmpinstTable[ i ].pchUniqueName,
                   KAPI_EnumName( pknobs, pknobs->dmpinstTable[ i ].fu, "fu_t" ),
                   KAPI_EnumName( pknobs, pknobs->dmpinstTable[ i ].it, "it_t" ) );
      assert( i == KAPI_uniqueName2iid( pknobs, pknobs->dmpinstTable[ i ].pchUniqueName, 0 ) );
   }
}


void
KDebug_ValnNameList( FILE *fp, valn_t *pvalnList, int nTab )
{
   int i = 0;

   while ( pvalnList ) {
      if ( i ) { fprintf( fp, ", " ); }
      fprintf( fp, "%s", pvalnList->val.pch );
      pvalnList = pvalnList->pvalnNext;
      i++;
   }
}
void
KDebug_StnNameList( FILE *fp, stn_t *pstnList, int nTab )
{
   int i = 0;

   while ( pstnList ) {
      if ( i ) { fprintf( fp, ", " ); }
      fprintf( fp, "%s", pstnList->pchName );
      pstnList = pstnList->pstnNext;
      i++;
   }
}

void
KDebug_DumpType( FILE *fp, stn_t *pstn, int nTab )
{
   kapi_indent_line( fp, nTab );

   assert( pstn->u.tfi.tty == ttyENUM );
   fprintf( fp, "TYPE %s = enum ( ", pstn->pchName );

   KDebug_ValnNameList( fp, pstn->u.tfi.pvalnEnums, nTab );

   fprintf( fp, " )" );

   if ( pstn->u.tfi.tredefStatus == tredefSTATUS_MARKED_NOREDEFINE ) {
      fprintf( fp, " limit <> noredefine" );
   }

   fprintf( fp, ";\n" );

}

void
KDebug_DumpTypeList( FILE *fp, knobs_t *pknobs, int nTab )
{
   stn_t *pstn;
   valn_t *pvalnRun;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "Defined types\n" );

   nTab++;
   pvalnRun = pknobs->pvalnTypeList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      KDebug_DumpType( fp, pstn, nTab );
      pvalnRun = pvalnRun->pvalnNext;
   }
}

void
KDebug_DumpTypeSpec( FILE *fp, tfi_t *ptfi, int nTab )
{
   switch( ptfi->tty ) {
     case ttyINT:
        fprintf( fp, "int" );
        break;
     case ttyREAL:
        fprintf( fp, "real" );
        break;
     case ttySTRING:
        fprintf( fp, "string" );
        break;
     case ttyENUM:
        fprintf( fp, "%s", ptfi->pstnIdent->pchName );
        break;
     case ttyBITMASK:
        fprintf( fp, "bitmask( %s )", ptfi->ptfiBitmaskBaseType->pstnIdent->pchName );
        break;
     case ttyARRAY:
        fprintf( fp, "array[ %s ] of ", 
                     ptfi->ptfiArrayIndexType->pstnIdent->pchName );
        KDebug_DumpTypeSpec( fp, ptfi->ptfiArrayEltType, 0 );
        break;
     default:
        assert( 0 );
        break;
   }
}

void
KDebug_DumpVar( FILE *fp, stn_t *pstn, int nTab )
{
   kapi_indent_line( fp, nTab );

   fprintf( fp, "%s : ", pstn->pchName );

   KDebug_DumpTypeSpec( fp, pstn->u.vfi.ptfi, nTab );

   fprintf( fp, ";\n" );

}

void
KDebug_printval2( FILE *fp, tfi_t *ptfi, valn_t *pvaln, int nTab )
{
   switch( ptfi->tty ) {
     case ttyINT:
        fprintf( fp, "%d", pvaln->val.i );
        break;
     case ttyREAL:
        fprintf( fp, "%8.4f", pvaln->val.r );
        break;
     case ttySTRING:
        fprintf( fp, "\"%s\"", pvaln->val.pch );
        break;
     case ttyENUM:
        fprintf( fp, "%s", pvaln->val.pch );
        break;
     case ttyBITMASK: 
     case ttyARRAY:
     default:
        assert( 0 );
        break;
   }
}
void
KDebug_printval( FILE *fp, tfi_t *ptfi, ed_t *pedIn, int nTab )
{
   switch( ptfi->tty ) {
     case ttyINT:
        fprintf( fp, "%d", pedIn->valhdrValue.pvalnList->val.i );
        break;
     case ttyREAL:
        fprintf( fp, "%8.4f", pedIn->valhdrValue.pvalnList->val.r );
        break;
     case ttySTRING:
        fprintf( fp, "\"%s\"", pedIn->valhdrValue.pvalnList->val.pch );
        break;
     case ttyENUM:
        fprintf( fp, "%s", pedIn->valhdrValue.pvalnList->val.pch );
        break;
     case ttyBITMASK: {
        valn_t *pvalnRun;

        fprintf( fp, "bitmask( " );
        pvalnRun = pedIn->valhdrValue.pvalnList;
        while ( pvalnRun ) {
           fprintf( fp, "%s", pvalnRun->val.pch );
           pvalnRun = pvalnRun->pvalnNext;
           if ( pvalnRun ) { fprintf( fp, ", " ); }
        }
        fprintf( fp, " )" );
        break;
     }
     case ttyARRAY: {
        ed_t *pedElt, *pedGlobal;
        valn_t *pvalnRun;
   
        pvalnRun = ptfi->ptfiArrayIndexType->pvalnEnums;
        pedGlobal = pedLookup4ped( pedIn, "*" );
        while ( pvalnRun ) {
           kapi_indent_line( fp, nTab );
           fprintf( fp, "[ %s ] = ", pvalnRun->val.pch );

           pedElt = pedLookup4ped( pedIn, pvalnRun->val.pch );
           if ( pedElt ) {
               KDebug_printval( fp, ptfi->ptfiArrayEltType, 
                                pedElt, nTab );
           } else if ( pedGlobal ) {
               KDebug_printval( fp, ptfi->ptfiArrayEltType, 
                                pedGlobal, nTab );
           } else {
               fprintf( fp, "UNDEFINED\n" );
           }
           fprintf( fp, "\n" );
           pvalnRun = pvalnRun->pvalnNext;
        }
        break;
     }
     default:
        assert( 0 );
        break;
   }
}

void
KDebug_PrintVarValueLimits( FILE *fp, stn_t *pstn, tfi_t *ptfi, ed_t *pedList )
{
   if ( pedList->valhdrLimit.fNoRedefine ) {
      fprintf( fp, " limit <> noredefine" );
      return;
   } 

   assert( ptfi->tty != ttyARRAY );

   switch ( pedList->valhdrLimit.vals ) {
      case valsRANGE:
         fprintf( fp, " limit <> ( " );
         KDebug_printval2( fp, ptfi, pedList->valhdrLimit.pvalnList, 0 );
         fprintf( fp, " ..  " );
         KDebug_printval2( fp, ptfi, pedList->valhdrLimit.pvalnList->pvalnNext, 0 );
         fprintf( fp, " ) " );
         break;
      case valsLIST: {
         valn_t *pvalnRun;

         fprintf( fp, " limit <> ( " );
         pvalnRun = pedList->valhdrLimit.pvalnList;
         while ( pvalnRun ) {
            KDebug_printval2( fp, ptfi, pvalnRun, 0 );
            pvalnRun = pvalnRun->pvalnNext;
            if ( pvalnRun ) {
               fprintf( fp, ", " );
            }
         }
         fprintf( fp, " ) " );
         break;
      }
      case valsSCALAR:
         KDebug_printval2( fp, ptfi, pedList->valhdrLimit.pvalnList, 0 );
         break;
      case valsNOREDEFINE:
         fprintf( fp, "  limit <> noredefine" );
         break;
      case valsUNSET:
         break;
   }
}

void
KDebug_DumpVarValues( FILE *fp, stn_t *pstn, int nTab )
{
   switch( pstn->u.vfi.ptfi->tty ) {
     case ttyINT:
        kapi_indent_line( fp, nTab );
        fprintf( fp, "%s := ", pstn->pchName );
        if ( pstn->u.vfi.pedList->valhdrValue.pvalnList == NULL ) {
           fprintf( fp, "UNDEFINED" );
        } else {
           KDebug_printval( fp,  pstn->u.vfi.ptfi, pstn->u.vfi.pedList, nTab );
        }

        KDebug_PrintVarValueLimits( fp, pstn, pstn->u.vfi.ptfi,
                                    pstn->u.vfi.pedList );

        fprintf( fp, ";\n" );
        break;
     case ttyREAL:
        kapi_indent_line( fp, nTab );
        fprintf( fp, "%s := ", pstn->pchName );
        if ( pstn->u.vfi.pedList->valhdrValue.pvalnList == NULL ) {
           fprintf( fp, "UNDEFINED" );
        } else {
           KDebug_printval( fp,  pstn->u.vfi.ptfi, pstn->u.vfi.pedList, nTab );
        }

        KDebug_PrintVarValueLimits( fp, pstn, pstn->u.vfi.ptfi,
                                    pstn->u.vfi.pedList );

        fprintf( fp, ";\n" );
        break;
     case ttySTRING:
        kapi_indent_line( fp, nTab );
        fprintf( fp, "%s := ", pstn->pchName );
        if ( pstn->u.vfi.pedList->valhdrValue.pvalnList == NULL ) {
           fprintf( fp, "\"UNDEFINED\"" );
        } else {
           KDebug_printval( fp,  pstn->u.vfi.ptfi, pstn->u.vfi.pedList, nTab );
        }

        KDebug_PrintVarValueLimits( fp, pstn, pstn->u.vfi.ptfi,
                                    pstn->u.vfi.pedList );

        fprintf( fp, ";\n" );
        break;
     case ttyENUM:
        kapi_indent_line( fp, nTab );
        fprintf( fp, "%s := ", pstn->pchName );
        if ( pstn->u.vfi.pedList->valhdrValue.pvalnList == NULL ) {
           fprintf( fp, "UNDEFINED" );
        } else {
           KDebug_printval( fp,  pstn->u.vfi.ptfi, pstn->u.vfi.pedList, nTab );
        }

        KDebug_PrintVarValueLimits( fp, pstn, pstn->u.vfi.ptfi,
                                    pstn->u.vfi.pedList );

        fprintf( fp, ";\n" );
        break;

     case ttyBITMASK:
        kapi_indent_line( fp, nTab );
        fprintf( fp, "%s := ", pstn->pchName );
        if ( pstn->u.vfi.pedList->valhdrValue.pvalnList == NULL ) {
           kapi_indent_line( fp, nTab );
           fprintf( fp, "UNDEFINED" );
        } else {
           KDebug_printval( fp,  pstn->u.vfi.ptfi, pstn->u.vfi.pedList, nTab );
        }

        KDebug_PrintVarValueLimits( fp, pstn, pstn->u.vfi.ptfi,
                                    pstn->u.vfi.pedList );

        fprintf( fp, ";\n" );
        break;
     case ttyARRAY: {
        ed_t *pedIn, *pedElt, *pedGlobal;
        valn_t *pvalnRun;
        tfi_t *ptfi;
   
        ptfi = pstn->u.vfi.ptfi;
        pedIn = pstn->u.vfi.pedList;

        /* if an associative array */
        if ( ptfi->ptfiArrayIndexType->tty == ttySTRING ) {

           while ( pedIn ) {
              kapi_indent_line( fp, nTab );
              fprintf( fp, "%s[ \"%s\" ] := ", 
                       pstn->pchName, (pedIn->pchIndexName+1) );
              if ( pedIn->pchIndexName[0] != '%' ) {
                  kapi_Error_pch2( -1, 0,"Internal error dumping knobs - index %s[ %s ] malformed\n",pstn->pchName, pedIn->pchIndexName );
              }
   
              pedElt = pedIn;
              if ( pedElt ) {
                  KDebug_printval( fp, ptfi->ptfiArrayEltType, 
                                   pedElt, nTab );
                  KDebug_PrintVarValueLimits( fp, pstn, ptfi->ptfiArrayEltType,
                                              pedElt );
              } else {
                  fprintf( fp, "UNDEFINED;\n" );
              }
   
   
              fprintf( fp, ";\n" );
              pedIn = pedIn->pedNext;
           }

        } else {  /* not an associative array */
           pvalnRun = ptfi->ptfiArrayIndexType->pvalnEnums;
           pedGlobal = pedLookup4ped( pedIn, "*" );
           while ( pvalnRun ) {
              kapi_indent_line( fp, nTab );
              fprintf( fp, "%s[ %s ] := ", pstn->pchName, pvalnRun->val.pch );
   
              pedElt = pedLookup4ped( pedIn, pvalnRun->val.pch );
              if ( pedElt ) {
                  KDebug_printval( fp, ptfi->ptfiArrayEltType, 
                                   pedElt, nTab );
                  KDebug_PrintVarValueLimits( fp, pstn, ptfi->ptfiArrayEltType,
                                              pedElt );
              } else if ( pedGlobal ) {
                  KDebug_printval( fp, ptfi->ptfiArrayEltType, 
                                   pedGlobal, nTab );
                  KDebug_PrintVarValueLimits( fp, pstn, ptfi->ptfiArrayEltType,
                                              pedGlobal );
              } else {
                  fprintf( fp, "UNDEFINED;\n" );
              }
   
   
              fprintf( fp, ";\n" );
              pvalnRun = pvalnRun->pvalnNext;
           }
        }
        break;
     }
     default:
        assert( 0 );
        break;
   }
   fprintf( fp, "\n" );
}

void
KDebug_DumpVarValueList( FILE *fp, knobs_t *pknobs, int nTab )
{
   /* int i; unreferenced */
   stn_t *pstn;
   valn_t *pvalnRun;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "# Defined variables' values \n" );

   nTab++;
   pvalnRun = pknobs->pvalnVarList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      KDebug_DumpVar( fp, pstn, nTab );
      KDebug_DumpVarValues( fp, pstn, nTab+1 );
      pvalnRun = pvalnRun->pvalnNext;
   }
}


void
KDebug_DumpVarList( FILE *fp, knobs_t *pknobs, int nTab )
{
   /* int i; unreferenced */
   stn_t *pstn;
   valn_t *pvalnRun;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "# Defined variables\n" );

   nTab++;
   pvalnRun = pknobs->pvalnVarList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      KDebug_DumpVar( fp, pstn, nTab );
      pvalnRun = pvalnRun->pvalnNext;
   }
}

void
KDebug_DumpIdent( FILE *fp, stn_t *pstn, int nTab )
{
   switch ( pstn->ity ) {
     case ityVARNAME:
        KDebug_DumpVar( fp, pstn, nTab );
        break;
     case ityTYPENAME:
        KDebug_DumpType( fp, pstn, nTab );
        break;
     case ityATTRIBUTENAME:
     case ityENUMCONST:
     case ityUNKNOWN:
     default: 
        assert( 0 );
   }
}

void
KDebug_DumpLatencies( FILE *fp, knobs_t *pknobs )
{
   int i, j, cntfu;

   printf( "\n\nCore latencies\n\n" );
   cntfu = KAPI_fuCount( pknobs );
   for ( i=0; i<cntfu; i++ ) {
      int ilat, cntDestOpp;
      char *pch;

      pch = KAPI_EnumName( pknobs, i, "fu_t" );
      cntDestOpp = KAPI_destOppCount( pknobs, i );
      for ( j=0; j<cntDestOpp; j++ ) {
         char *pchOppName;

         ilat = KAPI_CoreLatency( pknobs, i, j );
         if ( ilat >= 0 ) {
            pchOppName = KAPI_destOppName( pknobs, i, j );
            fprintf( fp, "   CoreLatency[ %12s/%-8s ] = %2d\n", pch, pchOppName, ilat );
         }
      }
   }
}

void
KDebug_DumpIntraClusterBypass( FILE *fp, knobs_t *pknobs )
{
   int fuSrc, fuDest, cntSrcDestOpp, cntDestSrcOpp, oppDestSrc, oppSrcDest; 
   int clr, cntclr, cntfu;
   int fPrintedHdr, ilat;
   char *pchFuDest, *pchFuSrc, *pchOppSrcDest, *pchOppDestSrc;

   cntclr = KAPI_clusterCount( pknobs );
   cntfu = KAPI_fuCount( pknobs );
   fprintf( fp, "\n\nDumping bypasses for %d clusters\n", cntclr );

   for ( clr=0; clr<cntclr; clr++ ) {

      for ( fuSrc=0; fuSrc<cntfu; fuSrc++ ) {
         cntSrcDestOpp = KAPI_destOppCount( pknobs, fuSrc );
         pchFuSrc = KAPI_EnumName( pknobs, fuSrc, "fu_t" );

         for ( fuDest=0; fuDest<cntfu; fuDest++ ) {
            fPrintedHdr = 0;
            cntDestSrcOpp = KAPI_srcOppCount( pknobs, fuDest );
            pchFuDest = KAPI_EnumName( pknobs, fuDest, "fu_t" );

            for ( oppSrcDest=0; oppSrcDest<cntSrcDestOpp; oppSrcDest++ ) { 
               for ( oppDestSrc=0; oppDestSrc<cntDestSrcOpp; oppDestSrc++ ) {
                  int nlist, ipapair;
                  papair_t *ppapair, *ppapairList;

                  ppapairList = KAPI_IntraClusterBypassList( pknobs, clr, 
                                               fuSrc, oppSrcDest, -1, -1,
                                               fuDest, oppDestSrc,  -1, -1, &nlist );
                  for ( ipapair=0; ipapair<nlist; ipapair++ ) {
                     ppapair = &(ppapairList[ ipapair ]);
                        
                     ilat = KAPI_IntraClusterBypass( pknobs, clr,
                                    fuSrc, oppSrcDest, ppapair->utSrc, ppapair->cutportSrc,
                                    fuDest, oppDestSrc,  ppapair->utDest, ppapair->cutportDest );
                     if ( ilat > 0  ) {
                        if ( !fPrintedHdr ) {
                           fprintf( fp, "   cluster%d\\%s:%s\n", clr, pchFuSrc, pchFuDest );
                           fPrintedHdr = 1;
                        }
   
                        pchOppSrcDest = KAPI_destOppName(  pknobs, fuSrc, oppSrcDest );
   
                        pchOppDestSrc = KAPI_srcOppName( pknobs, fuDest, oppDestSrc );
   
                        if ( ppapair->cutportDest == -1 && ppapair->cutportSrc == -1 ) {
                           fprintf( fp, "      %s -> %s = %2d\n", 
                                 pchOppSrcDest, pchOppDestSrc, ilat );
                        } else if ( ppapair->cutportDest == -1 ) {
                           fprintf( fp, "      %s/%c%d -> %s = %2d\n", 
                                 pchOppSrcDest, kapi_ut2ch( ppapair->utSrc ), 
                                 ppapair->cutportSrc, pchOppDestSrc, ilat );
                        } else if ( ppapair->cutportSrc == -1 ) {
                           fprintf( fp, "      %s -> %s/%c%d = %2d\n", 
                                 pchOppSrcDest, pchOppDestSrc, 
                                 kapi_ut2ch( ppapair->utDest ), ppapair->cutportDest, 
                                 ilat );
                        } else {
                           fprintf( fp, "      %s/%c%d -> %s/%c%d = %2d\n", 
                                 pchOppSrcDest, kapi_ut2ch( ppapair->utSrc ), 
                                 ppapair->cutportSrc, pchOppDestSrc, 
                                 kapi_ut2ch( ppapair->utDest ), ppapair-> cutportDest, ilat );
                        }
                     }
                  }
               }
            }
         }
      }
   }
}

void
KDebug_DumpInterClusterBypass( FILE *fp, knobs_t *pknobs )
{
   int fuSrc, fuDest, cntSrcDestOpp, cntDestSrcOpp, oppDestSrc, oppSrcDest; 
   int clrSrc, clrDest, cntclr, cntfu;
   int fPrintedHdr, ilat;
   char *pchFuDest, *pchFuSrc, *pchOppSrcDest, *pchOppDestSrc;

   fprintf( fp, "\n\nDumping intercluster bypasses\n");

   cntclr = KAPI_clusterCount( pknobs );
   cntfu = KAPI_fuCount( pknobs );

   for ( clrSrc=0; clrSrc<cntclr; clrSrc++ ) {
      for ( clrDest=0; clrDest<cntclr; clrDest++ ) {

         for ( fuSrc=0; fuSrc<cntfu; fuSrc++ ) {
            cntSrcDestOpp = KAPI_destOppCount( pknobs, fuSrc );
            pchFuSrc = KAPI_EnumName( pknobs, fuSrc, "fu_t" );
   
            for ( fuDest=0; fuDest<cntfu; fuDest++ ) {
               fPrintedHdr = 0;
               cntDestSrcOpp = KAPI_srcOppCount( pknobs, fuDest );
               pchFuDest = KAPI_EnumName( pknobs, fuDest, "fu_t" );
   
               for ( oppSrcDest=0; oppSrcDest<cntSrcDestOpp; oppSrcDest++ ) { 
                  for ( oppDestSrc=0; oppDestSrc<cntDestSrcOpp; oppDestSrc++ ) {
                     int nlist, ipepair;
                     pepair_t *ppepair, *ppepairList;
   
                     ppepairList = KAPI_InterClusterBypassList( pknobs,
                                             clrSrc, fuSrc, oppSrcDest, -1, -1,
                                             clrDest, fuDest, oppDestSrc,  -1, -1, &nlist );
                     for ( ipepair=0; ipepair<nlist; ipepair++ ) {
                        ppepair = &(ppepairList[ ipepair ]);
                           
                        ilat = KAPI_InterClusterBypass( pknobs,
                            clrSrc, fuSrc, oppSrcDest, ppepair->utSrc, ppepair->cutportSrc,
                            clrDest, fuDest, oppDestSrc,  ppepair->utDest, ppepair->cutportDest );
                        if ( ilat > 0  ) {
                           if ( !fPrintedHdr ) {
                              fprintf( fp, "   cluster%d/%s:cluster%d/%s\n", 
                                       clrSrc, pchFuSrc, clrDest, pchFuDest );
                              fPrintedHdr = 1;
                           }
      
                           pchOppSrcDest = KAPI_destOppName(  pknobs, fuSrc, oppSrcDest );
      
                           pchOppDestSrc = KAPI_srcOppName( pknobs, fuDest, oppDestSrc );
      
                           if ( ppepair->cutportDest == -1 && ppepair->cutportSrc == -1 ) {
                              fprintf( fp, "      %s -> %s = %2d\n", 
                                    pchOppSrcDest, pchOppDestSrc, ilat );
                           } else if ( ppepair->cutportDest == -1 ) {
                              fprintf( fp, "      %s/%c%d -> %s = %2d\n", 
                                    pchOppSrcDest, kapi_ut2ch( ppepair->utSrc ), 
                                    ppepair->cutportSrc, pchOppDestSrc, ilat );
                           } else if ( ppepair->cutportSrc == -1 ) {
                              fprintf( fp, "      %s -> %s/%c%d = %2d\n", 
                                    pchOppSrcDest, pchOppDestSrc, 
                                    kapi_ut2ch( ppepair->utDest ), ppepair->cutportDest, 
                                    ilat );
                           } else {
                              fprintf( fp, "      %s/%c%d -> %s/%c%d = %2d\n", 
                                    pchOppSrcDest, kapi_ut2ch( ppepair->utSrc ), 
                                    ppepair->cutportSrc, pchOppDestSrc, 
                                    kapi_ut2ch( ppepair->utDest ), ppepair-> cutportDest, ilat );
}  }  }  }  }  }  }  }  }  }


void
KDebug_DumpTotalLatency( FILE *fp, knobs_t *pknobs )
{
   int fuSrc, fuDest, cntSrcDestOpp, cntDestSrcOpp, oppDestSrc, oppSrcDest; 
   int clr, clrSrc, clrDest, cntclr, cntfu;
   int fPrintedHdr, ilat;
   char *pchFuDest, *pchFuSrc, *pchOppSrcDest, *pchOppDestSrc;

   fprintf( fp, "\n\nDumping total latency\n");

   cntclr = KAPI_clusterCount( pknobs );
   cntfu = KAPI_fuCount( pknobs );

   for ( clrSrc=0; clrSrc<cntclr; clrSrc++ ) {
      for ( clrDest=0; clrDest<cntclr; clrDest++ ) {

         for ( fuSrc=0; fuSrc<cntfu; fuSrc++ ) {
            cntSrcDestOpp = KAPI_destOppCount( pknobs, fuSrc );
            pchFuSrc = KAPI_EnumName( pknobs, fuSrc, "fu_t" );
   
            for ( fuDest=0; fuDest<cntfu; fuDest++ ) {
               fPrintedHdr = 0;
               cntDestSrcOpp = KAPI_srcOppCount( pknobs, fuDest );
               pchFuDest = KAPI_EnumName( pknobs, fuDest, "fu_t" );
   
               for ( oppSrcDest=0; oppSrcDest<cntSrcDestOpp; oppSrcDest++ ) { 
                  for ( oppDestSrc=0; oppDestSrc<cntDestSrcOpp; oppDestSrc++ ) {
                     int nlist, ipepair;
                     pepair_t *ppepair, *ppepairList;
   
                     ppepairList = KAPI_InterClusterBypassList( pknobs,
                                             clrSrc, fuSrc, oppSrcDest, -1, -1,
                                             clrDest, fuDest, oppDestSrc,  -1, -1, &nlist );
                     for ( ipepair=0; ipepair<nlist; ipepair++ ) {
                        ppepair = &(ppepairList[ ipepair ]);
                           
                        ilat = KAPI_TotalLatency( pknobs,
                            clrSrc, fuSrc, oppSrcDest, ppepair->utSrc, ppepair->cutportSrc,
                            clrDest, fuDest, oppDestSrc,  ppepair->utDest, ppepair->cutportDest );
                        if ( ilat > 0  ) {
                           if ( !fPrintedHdr ) {
                              fprintf( fp, "   cluster%d/%s:cluster%d/%s\n", 
                                       clrSrc, pchFuSrc, clrDest, pchFuDest );
                              fPrintedHdr = 1;
                           }
      
                           pchOppSrcDest = KAPI_destOppName(  pknobs, fuSrc, oppSrcDest );
      
                           pchOppDestSrc = KAPI_srcOppName( pknobs, fuDest, oppDestSrc );
      
                           if ( ppepair->cutportDest == -1 && ppepair->cutportSrc == -1 ) {
                              fprintf( fp, "      %s -> %s = %2d\n", 
                                    pchOppSrcDest, pchOppDestSrc, ilat );
                           } else if ( ppepair->cutportDest == -1 ) {
                              fprintf( fp, "      %s/%c%d -> %s = %2d\n", 
                                    pchOppSrcDest, kapi_ut2ch( ppepair->utSrc ), 
                                    ppepair->cutportSrc, pchOppDestSrc, ilat );
                           } else if ( ppepair->cutportSrc == -1 ) {
                              fprintf( fp, "      %s -> %s/%c%d = %2d\n", 
                                    pchOppSrcDest, pchOppDestSrc, 
                                    kapi_ut2ch( ppepair->utDest ), ppepair->cutportDest, 
                                    ilat );
                           } else {
                              fprintf( fp, "      %s/%c%d -> %s/%c%d = %2d\n", 
                                    pchOppSrcDest, kapi_ut2ch( ppepair->utSrc ), 
                                    ppepair->cutportSrc, pchOppDestSrc, 
                                    kapi_ut2ch( ppepair->utDest ), ppepair-> cutportDest, ilat );
   }  }  }  }  }  }  }  }  }
   for ( clr=0; clr<cntclr; clr++ ) {

      for ( fuSrc=0; fuSrc<cntfu; fuSrc++ ) {
         cntSrcDestOpp = KAPI_destOppCount( pknobs, fuSrc );
         pchFuSrc = KAPI_EnumName( pknobs, fuSrc, "fu_t" );

         for ( fuDest=0; fuDest<cntfu; fuDest++ ) {
            fPrintedHdr = 0;
            cntDestSrcOpp = KAPI_srcOppCount( pknobs, fuDest );
            pchFuDest = KAPI_EnumName( pknobs, fuDest, "fu_t" );

            for ( oppSrcDest=0; oppSrcDest<cntSrcDestOpp; oppSrcDest++ ) { 
               for ( oppDestSrc=0; oppDestSrc<cntDestSrcOpp; oppDestSrc++ ) {
                  int nlist, ipapair;
                  papair_t *ppapair, *ppapairList;

                  ppapairList = KAPI_IntraClusterBypassList( pknobs, clr, 
                                               fuSrc, oppSrcDest, -1, -1,
                                               fuDest, oppDestSrc,  -1, -1, &nlist );
                  for ( ipapair=0; ipapair<nlist; ipapair++ ) {
                     ppapair = &(ppapairList[ ipapair ]);
                        
                     ilat = KAPI_TotalLatency( pknobs,
                                clr, fuSrc, oppSrcDest, ppapair->utSrc, ppapair->cutportSrc,
                                clr, fuDest, oppDestSrc,  ppapair->utDest, ppapair->cutportDest );
                     if ( ilat > KAPI_CoreLatency( pknobs, fuSrc, oppSrcDest ) ) {
                        if ( !fPrintedHdr ) {
                           fprintf( fp, "   cluster%d\\ %s:%s (min %d)\n", clr, 
                                    pchFuSrc, pchFuDest,
                                    KAPI_MinIntraClusterTotalLatency( pknobs, clr,
                                            fuSrc, oppSrcDest, fuDest, oppDestSrc)  );
                           fPrintedHdr = 1;
                        }
   
                        pchOppSrcDest = KAPI_destOppName(  pknobs, fuSrc, oppSrcDest );
   
                        pchOppDestSrc = KAPI_srcOppName( pknobs, fuDest, oppDestSrc );
   
                        if ( ppapair->cutportDest == -1 && ppapair->cutportSrc == -1 ) {
                           fprintf( fp, "      %s -> %s = %2d\n", 
                                 pchOppSrcDest, pchOppDestSrc, ilat );
                        } else if ( ppapair->cutportDest == -1 ) {
                           fprintf( fp, "      %s/%c%d -> %s = %2d\n", 
                                 pchOppSrcDest, kapi_ut2ch( ppapair->utSrc ), 
                                 ppapair->cutportSrc, pchOppDestSrc, ilat );
                        } else if ( ppapair->cutportSrc == -1 ) {
                           fprintf( fp, "      %s -> %s/%c%d = %2d\n", 
                                 pchOppSrcDest, pchOppDestSrc, 
                                 kapi_ut2ch( ppapair->utDest ), ppapair->cutportDest, 
                                 ilat );
                        } else {
                           fprintf( fp, "      %s/%c%d -> %s/%c%d = %2d\n", 
                                 pchOppSrcDest, kapi_ut2ch( ppapair->utSrc ), 
                                 ppapair->cutportSrc, pchOppDestSrc, 
                                 kapi_ut2ch( ppapair->utDest ), ppapair-> cutportDest, ilat );
                        }
}  }  }  }  }  }  }  }


void
KDebug_DumpClusterDistances( FILE *fp, knobs_t *pknobs )
{
   int cntclr;
   kapi_cluster_t clusterSrc, clusterDest;

   cntclr = KAPI_clusterCount( pknobs );
   fprintf( fp, "\n\nDumping cluster distances for %d clusters\n", cntclr );

   for ( clusterSrc=0; clusterSrc<cntclr; clusterSrc++ ) {
      for ( clusterDest=0; clusterDest<cntclr; clusterDest++ ) {
         fprintf( fp, "   cluster%d:cluster%d = %d\n", clusterSrc, clusterDest,
             KAPI_ClusterDistance( pknobs, clusterSrc, clusterDest ) );
      }
   }

}

void
KDebug_DumpAttr( FILE *fp, stn_t *pstn, int iLevel )
{
   int i;

   kapi_indent_line( fp, iLevel );
   for ( i=0; i<pstn->u.afih.nAttr; i++ ) {
      fprintf( fp, "%s += \"%s\";\n", pstn->pchName, pstn->u.afih.u.dmppch[ i ] );
   }
}


void
KDebug_DumpKnobs( FILE *fp, knobs_t *pknobs, int nTab )
{
   stn_t *pstn;
   valn_t *pvalnRun;

   kapi_indent_line( fp, nTab );
   fprintf( fp, "#\n# Defined types#\n\n" );


   /* first simple types */
   pvalnRun = pknobs->pvalnTypeList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      if ( pstn->u.tfi.tty == ttyINT || 
          pstn->u.tfi.tty == ttyREAL || 
          pstn->u.tfi.tty == ttyENUM || 
          pstn->u.tfi.tty == ttySTRING ) {

         /* Weed out port_t type definitions */

         if ( 0 != strcmp( pstn->pchName, "port_t" ) ) {
            KDebug_DumpType( fp, pstn, nTab );
         }
      }
      pvalnRun = pvalnRun->pvalnNext;
   }

   /* first then bitmasks types */
   pvalnRun = pknobs->pvalnTypeList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      if ( pstn->u.tfi.tty == ttyBITMASK ) {
         KDebug_DumpType( fp, pstn, nTab );
      }
      pvalnRun = pvalnRun->pvalnNext;
   }

   /* finally arrays */
   pvalnRun = pknobs->pvalnTypeList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      if ( pstn->u.tfi.tty == ttyARRAY ) {

         KDebug_DumpType( fp, pstn, nTab );
      }
      pvalnRun = pvalnRun->pvalnNext;
   }


   kapi_indent_line( fp, nTab );
   fprintf( fp, "#\n# Defined variables#\n\n" );

   pvalnRun = pknobs->pvalnVarList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      fprintf( fp, "" );

      /* KAPI3X weeds out PortMask and PortTypes arrays */

      if ( 0 != strcmp( pstn->pchName, "PortMask" ) 
             && 0 != strcmp( pstn->pchName, "PortTypes" ) ) {
         if ( 0 == strcmp( pstn->pchName, "cluster0Cutports" ) ) {
            KDebug_Dcluster0Cutports(  pknobs, fp, pstn, nTab );
         } else {
            KDebug_DumpVar(       fp, pstn, nTab );
            KDebug_DumpVarValues( fp, pstn, nTab+1 );
         }
      }
      pvalnRun = pvalnRun->pvalnNext;
   }


   kapi_indent_line( fp, nTab );
   fprintf( fp, "#\n# Defined attributes#\n\n" );

   pvalnRun = pknobs->pvalnAttrList;
   while ( pvalnRun ) {
      pstn = kapi_pstnLookup( pknobs, pvalnRun->val.pch );
      KDebug_DumpAttr( fp, pstn, nTab );
      pvalnRun = pvalnRun->pvalnNext;
   }
}

void
KAPI_DumpKnobs( void *pknobs, FILE *fp )
{
    KDebug_DumpKnobs( fp, pknobs, 0 );
}


static void
KDebug_Dcluster0Cutports( knobs_t *pknobs, FILE *fp, stn_t *pstn, int nTab )
{
   kapi_ut_t ut;

   fprintf( fp, "cluster0Cutports : array[ ut_t ] of int;\n" );
   for ( ut=kapi_utFIRST; ut<kapi_nUT; ut++ ) {
      char *pchIndex;

      pchIndex = KAPI_EnumName( pknobs, ut, "ut_t" );   
      fprintf( fp, "    cluster0Cutports[ %s ] := %d;\n", pchIndex, 
               pknobs->mpclrTable[ 0 ].mpncutport[ ut ] );
   }

   fprintf( fp, "\n" );
}
