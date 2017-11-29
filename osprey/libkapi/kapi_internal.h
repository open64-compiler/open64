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


/*
 * General NAMING CONVENTIONS used:
 *
 *   This is a twisted variant of the "hungarian" naming conventions.
 *
 *   Variables used to flag or signal are prefixed with 'f'
 *
 *   Plain integer variables are prefixed with 'i'
 *
 *   Less often used: plain integer variables whose value might represent
 *     a bit mask(bm) or bit vector (bv32, bv64, bv128), a count (c), or length
(l), or
 *     'number of' (n), or size (z), or just a vanilla untyped data (w)
 *     (stands for word) that doesn't fit into
 *     other categories.
 *
 *   New types are created with 2 or 3 letter prefixes and
 *     all variables declared of that type are prefixed with that
 *     three letter prefix.
 *
 *   When declaring a pointer, the variable name is prefixed with one
*     p for each dereference level, so if you have a type called 'ibx',
 *     and you wished to declare a pointer to an ibx, it would be named
*     pibxXXXX, where the XXXX can be used to qualify its specific use.
 *
 *     ibx *pibxXXXX;
 *
 *   The rules for double pointers are similar:
*
 *     ibx **ppibxXXXX;
 *
 *   Arrays are prefixed with mpXXYYYY, where mp stands for 'map'
 *     XX is the type of element in the array, and YYYY is a qualifier that
names
 *     the typed data.
 *     For example, to declare an array of ibx, one would write:
 *
 *     ibx  mpibxFred[ 10 ];
 *
 *     which is a declaration of an arary of 10 ibx's. the word Fred, might
 *     qualify this array as being reserved for "Fred's" ibxs (whatever that
 *     means).
 *
 *     Dynamically allocated arrays implemented as pointers can be declared
 *     as 'dmp' instead of 'mp'
 *
 *     Sometimes no prefix is given if the array merely is a lookup function

*     in which case there is usually a '2' separating two types.
 *
 *   I occasionally apply these rules to #define constants and to functions
 *     that return values.


 *   Generally, I make the name of the type just the prefix code. However,
 *     to avoid confusion with declarations such as:
 *
 *        ibx ibx;   -- which declares a variable ibx, of type ibx
 *
 *     I will prefix types with '_t' to follow the convention set in other
 *    mppsim code.
 *
 *        ibx_t ibx; -- which declares a variable ibx, of type ibx_t
 *
 *
 */


#ifndef _KAPI_INTERNAL_H_
#define _KAPI_INTERNAL_H_

#include "kapi.h"
#include "kapi_ia64.h"
#include "kapi_bv.h"


#ifdef _WIN32
 #define strdup _strdup
 #define fileno _fileno
 #define itoa _itoa
 #include <malloc.h>
 #define alloca _alloca
#endif


/* 
   ------- This header contains structures needed only
   ------- internally to implement the raw HaMM interface 
*/

/* maximum number of register dests/sources per instruction */
#define maxDESTINATIONS 32
#define maxSOURCES 32

#define maxCLR       4
#define maxPORTS    32
#define maxCPORTS   32
#define maxICPORTS   6
#define maxMCPORTS   8
#define maxBCPORTS   6
#define maxFCPORTS   4


/* allows bundles to be configurable (try new bids, etc). */
typedef struct BIDINFO_T {
   kapi_syl_t mpsylType[ nSYLBID ];       /* syllables comprising this bun_t */
   int   mpnsylAvail[ kapi_nSYL ];  /* number of syllables of each type */
   int   mpnitAvail[ kapi_nIT ];    /* number of instructions of each type possible */
   int   fReserved;            /* is this a reserved bid */
   int   isylSbit;             /* 0 == no sbit, 1, 2 legal values */
   char  *pchBID;              /* string rep of expected bid */
} bidinfo_t;


/* syllable information */
typedef struct SYLINFO_T {
   int   mpnutNeeded[ kapi_nUT ];  /* number of syllables of each type */
   kapi_it_t  itMajor;
} sylinfo_t;


#define maxBUNDLE_ISSUE_IMP  4

typedef struct TPINFO_T {
   kapi_bid_t  mpbid[ maxBUNDLE_ISSUE_IMP ];
} tpinfo_t;

#define syl2bv32( syl ) ( 1 << (syl) )

#define itOk4syl( _syl, _it ) \
   ( dmpitinfoTable[ (_it) ].bv32sylAllowed & syl2bv32( _syl ) )


/* ----------- Defines ----------- */

/* +++ uarch unknobable +++ */

#define   bidMII        0x0
#define   bidMI_I       0x1
#define   bidMLI        0x2
#define   bidRESERVED_3 0x3
#define   bidMMI        0x4
#define   bidM_MI       0x5
#define   bidMFI        0x6
#define   bidMMF        0x7
#define   bidMIB        0x8
#define   bidMBB        0x9
#define   bidRESERVED_A 0xA
#define   bidBBB        0xB
#define   bidMMB        0xC
#define   bidRESERVED_D 0xD
#define   bidMFB        0xE
#define   bidRESERVED_F 0xF

#define   bvMII   ( 1 << bidMII )
#define   bvMI_I  ( 1 << bidMI_I )
#define   bvMLI   ( 1 << bidMLI )
#define   bvMMI   ( 1 << bidMMI )
#define   bvM_MI  ( 1 << bidM_MI )
#define   bvMFI   ( 1 << bidMFI )
#define   bvMMF   ( 1 << bidMMF )
#define   bvMIB   ( 1 << bidMIB )
#define   bvMBB   ( 1 << bidMBB )
#define   bvBBB   ( 1 << bidBBB )
#define   bvMMB   ( 1 << bidMMB )
#define   bvMFB   ( 1 << bidMFB )

#define bv32sylI ( 1 << kapi_sylI )
#define bv32sylF ( 1 << kapi_sylF )
#define bv32sylB ( 1 << kapi_sylB )
#define bv32sylM ( 1 << kapi_sylM )
#define bv32sylL ( 1 << kapi_sylL )


/* ----------- Types ----------- */

/* function unit information table entry */
typedef struct _PORTINFO_T {
    kapi_ut_t      ut;
    kapi_cport_t   cport;
    kapi_cutport_t cutport;
    bv_t           bvfuAllowed;
} cportinfo_t;


/* instruction type table */
typedef struct _ITINFO_T {
   kapi_it_t     it;
   int      maxAvail;
   char    *pchitName;
   bv32_t   bv32sylAllowed; 
} itinfo_t;

typedef struct _FUINFO_T {
    kapi_fu_t   fu;
    int     iCoreLatency[ maxDESTINATIONS ];

    int     cntDest;
    char    *mppchDestName[ maxDESTINATIONS ];

    int     cntSrc;
    char    *mppchSrcName[ maxSOURCES ];
	bv32_t bv32InfoBits;

    char   *pchName;
} fuinfo_t;

typedef enum {
	type_single,
	type_multiple
} operand_list_e;

typedef struct operand_match_T {
	operand_list_e iType;
	union
	{
		int pOperand;
		struct operand_match_T *pOperandList;
	} pIndexes[2];
} operand_match_t;


typedef struct  _INST_T {
    char   *pchUniqueName;
    char   *pchMnemonic;
    char   *pchfu;     /* function unit class character representation */
    char   *pchit;     /* it character representation */
    kapi_it_t    it;       /* instruction type */
    kapi_fu_t  fu;     /* function unit restrictions/byp/latency */
	operand_match_t *pExplicitOps;
	operand_match_t *pImplicitOps;
} inst_t;

typedef struct _PORTNODE {
   struct  _PORTNODE  *pportnNext;   
   int           latDefault;
   int     oppSource, oppSink;
   kapi_cport_t   cportSource, cportSink;
} portn_t;

/* ------------- Cluster type ----------------------- */
typedef struct _CLUSTER_T {

   /* cluster number */
   int   idxclr;

   /* number of ports in cluster */
   int          ncports;
   /* 1d array of pointers to structures */
   cportinfo_t  *mppcportinfoTable[ maxCPORTS ];

   /* 2d array of cport structures */
   cportinfo_t  *dmppcportinfoTable[ kapi_nUT ];

   /* number of cutports of each type (indexed by ut_t) */
   int  mpncutport[ kapi_nUT ];

   /* cport to global port mapping */
   int   cport2port[ 32 ];

   /* distance to other clr */
   int   distClr[ maxCLR ];
} clr_t;


typedef struct _EBYPASS_T {
   kapi_cluster_t clusterSrc, clusterDest;
   kapi_fu_t      fuSrc,      fuDest;
   kapi_cutport_t cutportSrc, cutportDest;   
   kapi_ut_t      utSrc,      utDest;   
   int            oppSrc,     oppDest;
   int            iValue;
} eby_t;

typedef struct _ABYPASS_T {
   kapi_cluster_t cluster;
   kapi_fu_t      fuSrc,      fuDest;
   kapi_cutport_t cutportSrc, cutportDest;   
   kapi_ut_t      utSrc,      utDest;   
   int            oppSrc,     oppDest;
   int            iValue;
   char           *pchEntry;
} aby_t;


typedef struct _CACHEPORT_T {
   bv32_t  bv32AccessMode;
} cacheport_t;

typedef struct CACHE_T {
   int  nLines;       /* number of lines in this cache */
   int  nBytesLine;   /* number of bytes/line */
   int  nWays;        /* associativity */

   int  nCachePorts;   /* number of access ports */
   cacheport_t    cacheportInfo[ KAPI_MAX_CACHE_PORTS_IMPL ];
                       /* array indexed by nCachePorts */
   bv32_t  bv32CacheContents;  /* bit mask of KAPI_CACHE_CONTENT_xxx */
   kapi_cache_policy_write_e iWritePolicy;  
   kapi_cache_policy_repl_e iReplPolicy; 
   kapi_cache_policy_alloc_e iAllocPolicy;
   int nCyclesRead;
} cache_t;


typedef enum _CACHE_TYPES_E {
	cache_type_instruction=0,
	cache_type_data,
	cache_type_unified,
	cache_type_enum_size
} cache_types_e;

/* ----------- Knobsfile structure type --------------- */

typedef enum _KNOBS_T_FIELDS_E {
	enum_pfSaveHeaderFlags=0,
	enum_maxBundleIssue,
	enum_maxInstructionIssue,
	enum_fImplicitNone,
    enum_cntLinesDeltaFile,
    enum_cntLinesBaselineFile,
	enum_pchToolname,
	enum_nclr,
	enum_mpclrTable,
	enum_nRawInter,
	enum_dmpebyInterTable,
	enum_nRawIntra,
	enum_dmpabyIntraTable,
	enum_nports,
	enum_mppportinfoTable,
	enum_port2cluster,
	enum_mpnut,
	enum_nfuinfoTable,
	enum_dmpfuinfoTable,
	enum_ninstTable,
	enum_dmpinstTable,
	enum_nitinfoTable,
	enum_dmpitinfoTable,
	enum_nbidinfoTable,
	enum_dmpbidinfoTable,
	enum_nsylinfoTable,
	enum_dmpsylinfoTable,
	enum_nCacheLevels,
	enum_dmpcacheTable,
	enum_dmpstn,
	enum_nstn,
	enum_fRestructuredAttributes,
	enum_pvalnTypeList,
	enum_pvalnVarList,
	enum_pvalnAttrList,
	enum_LAST_KAPI_FIELD
} knobs_fields_enum_t;


typedef struct _KNOBS_T {

   /* misc values */
   int *pfSaveHeaderFlags;
   int maxBundleIssue[ maxCLR ];        
   int maxInstructionIssue[ maxCLR ];        

   int fImplicitNone;   /* no implicit variable/attribute uses */

   int cntLinesDeltaFile;
   int cntLinesBaselineFile;

   char *pchToolname;

   /* Cluster information */
   int   nclr;
   clr_t mpclrTable[maxCLR];

   /* Reprocessed intercluster bypass */
   int nRawInter;
   eby_t *dmpebyInterTable;

   /* Reprocessed intracluster bypass */
   int nRawIntra;
   aby_t *dmpabyIntraTable;

   /* global port information table */
   int          nports;
   cportinfo_t  *mppportinfoTable[ maxPORTS ];

   /* global port to cluster mapping */
   int   port2cluster[ 32 ];

   /* global unit type information table (indexed by ut_t) */
   int  mpnut[ kapi_nUT ];

   /* fu information table (indexed by fu_t) */
   int      nfuinfoTable;
   fuinfo_t *dmpfuinfoTable;

   /* fu operand direction information table (indexed by fu_t) */
   int      nfudirTable;
   int      fudirDestination;
   int      fudirSource;
   int      *dmpfudirTable;

   /* instruction information table (indexed by iid_t) */
   int      ninstTable;
   inst_t  *dmpinstTable;

   /* it information table (indexed by it_t) */
   int      nitinfoTable;
   itinfo_t *dmpitinfoTable;

   /* bundle information (indexed by bid_t) */
   int       nbidinfoTable;
   bidinfo_t *dmpbidinfoTable;     /* info about bundles and syls */

   /* syllable information (indexed by syl_t) */
   int       nsylinfoTable;
   sylinfo_t *dmpsylinfoTable;     /* info about syls */
   
   /* cache information, indexed by cache_level_t */
   int nCacheLevels[cache_type_enum_size]; /* number of levels in each type */
   cache_t *dmpcacheTable[cache_type_enum_size]; /* cache[type][level] */

   /* symbol table information - use pstnHashNext field to traverse */
   struct _SYM_TABLE_NODE *dmpstn; 
   int nstn;

   /* has attributestructured data been translated from LL to array */
   int fRestructuredAttributes;

   /* misc lists - use field to traverse */
   struct _VALUE_NODE *pvalnTypeList;   /* stored as string */
   struct _VALUE_NODE *pvalnVarList;    /* stored as string */
   struct _VALUE_NODE *pvalnAttrList;   /* stored as string */
} knobs_t;


/* ----------- Function Prototypes ----------- */

extern void kapi_InitBidAndSyllable( knobs_t *pknobs );

extern kapi_cport_t cport4utcport( knobs_t *pknobs, 
                                   kapi_cluster_t cluster, kapi_ut_t ut, 
                                   int cportfromut );


/* ----------- Variable Prototypes ----------- */

#define iITANIUM    0
#define iHALEAKALA  1

extern int      kapi_cLine;

#endif

