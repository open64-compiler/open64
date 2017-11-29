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

/* Non MED section begin */

/* static char sccs_id[] = "%W%  %G%  %U%"; */



#ifndef _KAPI_IA64_H_
#define _KAPI_IA64_H_
/* Non MED section end */


/*------------------**
** KAPI Layer 2 API **
**------------------*/


/*
  ------- This header contains structures and definitions
   ------- needed to call raw KAPI interface routines.
   
*/

/* -------------- User visible types ---------------- */

typedef int kapi_iid_t;     /* KAPI internal opcode number */
typedef int kapi_fu_t;          /* function unit class type (instructions) */
typedef int kapi_port_t;    /* global port ids */
typedef int kapi_cport_t;   /* local cluster port ids */
typedef int kapi_cutport_t; /* local cluster ut relative ids */
typedef int kapi_bid_t;     /* bundle id index */
typedef int kapi_cluster_t; /* cluster id */


#define kapi_fuNONE  -1

#define kapi_bv32PORTS_ALL (0xFFFFFFFF)



/* -------------- User visible types ---------------- */

/* Non MED section begin */
#include "kapi.h"
/* Non MED section end */

typedef enum _RANGE_T {
    kapi_rangeEXACT_LATENCY,
    kapi_rangeMIN_LATENCY,
    kapi_rangeMAX_LATENCY
} kapi_range_t;


/* syl numbers must be consecutive! */
#define kapi_nUT        4
#define kapi_utFIRST    0
#define kapi_utLAST     3
typedef enum KAPI_UT_T {
   kapi_utM = 0,
   kapi_utI = 1,
   kapi_utF = 2,
   kapi_utB = 3
} kapi_ut_t;      /* unit type */




/* ------- it_t stuff ------- */

#define   kapi_itNONE  -1
#define   kapi_itFIRST  0
#define   kapi_itLAST   6
#define   kapi_nIT      (kapi_itLAST+1)
typedef enum KAPI_IT_T {
   kapi_itA  = 0,
   kapi_itM  = 1,
   kapi_itB  = 2,
   kapi_itBl = 3,
   kapi_itI  = 4,
   kapi_itF  = 5,
   kapi_itL  = 6
} kapi_it_t;      /* instruction type */



/* ------- syl_t stuff ------- */

/* syl numbers must be consecutive! */
#define kapi_nSYL        5
#define kapi_sylFIRST    0
#define kapi_sylLAST     4
typedef enum KAPI_SYL_T {
   kapi_sylI = 0,
   kapi_sylM = 1,
   kapi_sylF = 2,
   kapi_sylB = 3,
   kapi_sylL = 4
} kapi_syl_t;

/* ------- bid_t stuff ------- */

/* +++ arch unknobable +++ */
#define kapi_nBID     16
#define kapi_lognBID   4
#define nSYLBID        3

#define kapi_bidFIRST      0x0
#define kapi_bidLAST       0xF

typedef struct PAPAIR_T {
   kapi_cluster_t	cluster;
   kapi_fu_t        fuSrc, fuDest;
   kapi_cutport_t	cutportSrc, cutportDest;
   kapi_ut_t		utSrc, utDest;
   int				oppSrc, oppDest;
   int				iValue;
} papair_t;

typedef struct PEPAIR_T {
   kapi_cluster_t	clusterSrc, clusterDest;
   kapi_fu_t        fuSrc, fuDest;
   kapi_cutport_t	cutportSrc, cutportDest;
   kapi_ut_t		utSrc, utDest;
   int              oppSrc, oppDest;
   int              iValue;
} pepair_t;

typedef struct UPORT_T {
   kapi_cluster_t  cluster;
   kapi_ut_t       ut;
   kapi_cutport_t  cutport;
} uport_t;


/* ------- CACHE stuff ------- */
typedef enum KAPI_CACHE_CONTENT_E {
	KAPI_CACHE_CONTENT_INSTRUCTION = 0x1,
	KAPI_CACHE_CONTENT_DATA        = 0x2,
	KAPI_CACHE_CONTENT_OTHER       = 0x4
} kapi_cache_content_e;

typedef enum KAPI_CACHE_TYPES_E {
	KAPI_CACHE_TYPE_INSTRUCTION,
	KAPI_CACHE_TYPE_DATA,
	KAPI_CACHE_TYPE_UNIFIED,
} kapi_cache_types_e;
#define KAPI_NCACHE_TYPES          (KAPI_CACHE_TYPE_UNIFIED + 1)


/* for simplicity */
#define KAPI_MAX_CACHE_PORTS_IMPL            16

/* WB = write back, WT = write through */
typedef enum KAPI_CACHE_POLICY_WRITE_E {
	KAPI_CACHE_POLICY_WRITE_OTHER=0,
	KAPI_CACHE_POLICY_WRITE_WB=1,
	KAPI_CACHE_POLICY_WRITE_WT=2
} kapi_cache_policy_write_e;

/* LRU = least recently used, NRU = not recently used */
typedef enum KAPI_CACHE_POLICY_REPL_E {
KAPI_CACHE_POLICY_REPL_OTHER=0,
KAPI_CACHE_POLICY_REPL_LRU=1,
KAPI_CACHE_POLICY_REPL_NRU=2
} kapi_cache_policy_repl_e;

/* WA = write allocate, NWA = no write allocate */
typedef enum KAPI_CACHE_POLICY_ALLOC_E {
KAPI_CACHE_POLICY_ALLOC_OTHER=0,
KAPI_CACHE_POLICY_ALLOC_WA=1,
KAPI_CACHE_POLICY_ALLOC_NWA=2
} kapi_cache_policy_alloc_e;

/* port access defines */
#define KAPI_ACCESSMODE_OTHER         (1 << 0)
#define KAPI_ACCESSMODE_READ          (1 << 1)
#define KAPI_ACCESSMODE_WRITE         (1 << 2)
#define KAPI_ACCESSMODE_SNOOP         (1 << 3)
#define KAPI_ACCESSMODE_FETCH         (1 << 4)

   /*
      Each port is assumed to be able to load continguous data
      only and from only one request (i.e., data from only one
      load or one store) can be met.  Thus, this information
      does not accurately represent bus widths between levels
      of the cache -- only the number of independent requests
      that can be serviced simultaneously.

      Ports for which both the READ and WRITE bit are set are
      shared R/W ports and only one R or one W per cycle can
      be accomodated.

      There is room for expansion of this structure at a later
      date.  For now, this gives the basic desired information
      while retaining room for extension later.
   */
typedef struct KAPI_CACHEPORT_T {
   bv32_t  bv32AccessMode;
} kapi_cacheport_t;

typedef struct KAPI_CACHE_T {
   int  nLines;       /* number of lines in this cache */
   int  nBytesLine;   /* number of bytes/line */
   int  nWays;        /* associativity */

   int  nCachePorts;   /* number of access ports */
   kapi_cacheport_t    cacheportInfo[ KAPI_MAX_CACHE_PORTS_IMPL ];
                       /* array indexed by nCachePorts */
   bv32_t  bv32CacheContents;  /* bit mask of KAPI_CACHE_CONTENT_xxx */
   kapi_cache_policy_write_e iWritePolicy;  
   kapi_cache_policy_repl_e iReplPolicy; 
   kapi_cache_policy_alloc_e iAllocPolicy;  
   int nCyclesRead;
} kapi_cache_t;


typedef enum
{
	kapi_fu_info_approximate_latency=0, 
	kapi_fu_info_no_latency_info=1,
	kapi_fu_info_no_primary_source=30,
	kapi_fu_info_no_primary_destination=31
} kapi_fu_infobits_e;

typedef enum
{
	kapi_latency_type_none,
	kapi_latency_type_approximate,
	kapi_latency_type_full
} kapi_latency_type_t;

typedef enum 
{
	kapi_op_role_dest,
	kapi_op_role_src
} kapi_operand_role_e;



/* Non MED section begin */

/* -----------------   Iinitialization   ---------------------- */
/* pConfig is a pointer returned from a successful call to KAPI_Initialize.
   This function will initialize the IA64 structures required for working with
   Kapi layer 2.
   */
void *KAPI_ia64_Initialize(void *pConfig);
/* pConfig is the address of the pointer that will contain the structures required for working with
   Kapi layer 2. pHeaderConfig points to a structure saved before with one of save as header functions.
   The function will return 0 if successful */
extern int KAPI_fEnableIA64call_from_header(void **pConfig, void *pHeaderConfig, int iReserved);

/* Non MED section end */


/* Non MED section begin */
#ifdef MED_API
/* Non MED section end */

/*-----------------------------------------------------------------------**
** Initialization:                                                       **
** 	These function will initialize the pointers for work with kapi/kmapi.**
** 	Use the pointer initialized by pLayer2Info for kapi_ia64 calls,      **
** 	and the pointer initialized by pLayer3Info for kmapi calls.          **
**  Return 0 if successful.												 **
** 	Sample:                                                              **
** 		void *pKapi_ia64info;                                            **
** 		void *pKmapi_info;                                               **
**                                                                       **
** 		KAPI_init(&pKapi_ia64info, &pKmapi_info, 0);					 **
** 		KAPI_iidCount(pKapi_ia64info);                                   **
** 		KMAPI_CreateResourceMap(pKmapi_info);                            **
**-----------------------------------------------------------------------*/

int KAPI_init(void **pLayer2Info, void **pLayer3Info, int iReserved);

int KAPI_init_external(void **pLayer2Info, void **pLayer3Info, void *pLayer2Data, void *pLayer3Data);


/* Non MED section begin */
#endif /*MED only API*/
/* Non MED section end */


/* Non MED section begin */
/*-------------------------------------------------------------------------**
**                                                                         **
** For all of the following functions, void *pConfig should be a pointer   **
** that was previously initialized by KAPI_ia64_Initialize.                **
** For further documentation and description of functions, please refer to **
** kapi.doc bundled with the release.                                      **
**-------------------------------------------------------------------------*/
/* Non MED section end */


/*-------------------------------------------------------------------------**
**                                                                         **
** For all of the following functions, void *pConfig should the first      **
** pointer that was previously initialized by KAPI_init (pLayer2Info).	   **
** For further documentation and description of functions, please refer to **
** kapi.doc bundled with the release.                                      **
**-------------------------------------------------------------------------*/

/* -----------------   Instruction set queries   ---------------------- */

/* Count instructions */
extern int        KAPI_iidCount( void *pConfig );

/* translate instruction id (synnonimous to emdb instruction id) to instruction type */
extern kapi_it_t  KAPI_iid2it( void *pConfig, kapi_iid_t iid, int iVariant );
/* translate instruction id to functional unit class */
extern kapi_fu_t  KAPI_iid2fu( void *pConfig, kapi_iid_t iid, int iVariant );

extern char      *KAPI_iid2mnemonic( void *pConfig, kapi_iid_t iid,
                                     int iVariant );
extern char      *KAPI_iid2uniqueName( void *pConfig, kapi_iid_t iid,
                                       int iVariant );
extern kapi_iid_t KAPI_uniqueName2iid( void *pConfig, char *pchName,
                                       int iVariant );

/* translate operand name to operand index for a specific fu */
extern int KAPI_oppGetSource( void *pConfig, kapi_fu_t fu, char *pchOppName );
extern int KAPI_oppGetDest( void *pConfig, kapi_fu_t fu, char *pchOppName );
/* translate operand name to operand index for a specific instruction and operand index */
extern int KAPI_GetOppIndex(void *pConfig, kapi_iid_t iid, 
					 int iIndex, kapi_operand_role_e iRole,
					 char *pchOppName);

/* -----------------   Ports and Clustering   ---------------------- */

/* find the issue width for a specific cluster */
extern int  KAPI_BundleIssueWidth( void *pConfig, kapi_cluster_t cluster );

/* Count dispersal for a syllable */
extern int  KAPI_DisperseCount4syl( void *pConfig, kapi_syl_t syl );
/* Count how many functional unit classes are defined */
extern int  KAPI_fuCount( void *pConfig );
/* Count how many clusters are defined */
extern int  KAPI_clusterCount( void *pConfig );

/* translate kapi class index funcional unit class name, return -1 if not found */
extern char *KAPI_fu2fuName( void *pConfig, kapi_fu_t fu, int iVariant );
/* translate functional unit class name to kapi index, return -1 if not found */
extern int KAPI_fuName2fuIndex( void *pConfig, char *fuName );
/* get fu info bits */
extern bv32_t KAPI_fuGetMiscInfo(void *pConfig, kapi_fu_t fu);
extern kapi_latency_type_t KAPI_fuGetLatencyType(void *pConfig, kapi_fu_t fu);

/* Count operands for a functional unit class */
/* Sources are the consumed operands, destinations are the produced operadns */
extern int KAPI_srcOppCount( void *pConfig, kapi_fu_t fuSrc );
extern int KAPI_destOppCount( void *pConfig, kapi_fu_t fuDest );

/* Get operand name for a specifc operand  & functional unit class */
extern char *KAPI_srcOppName( void *pConfig, kapi_fu_t fuSrc, int opp );
extern char *KAPI_destOppName( void *pConfig, kapi_fu_t fuDest, int opp );

/* Get a cport mask for a functional unit class or unit type */
extern bv32_t  KAPI_cportMask4fu( void *pConfig, kapi_cluster_t cluster,
                                  kapi_fu_t fu );
extern bv32_t  KAPI_cportMask4ut( void *pConfig, kapi_cluster_t cluster,
                                  kapi_ut_t ut );

/* Get a cport mask for a functional unit class or unit type */
extern int  KAPI_cportCount( void *pConfig, kapi_cluster_t cluster );
extern int  KAPI_cportCount4fu( void *pConfig, kapi_cluster_t cluster,
                               kapi_fu_t fu );
extern int  KAPI_cportCount4ut( void *pConfig, kapi_cluster_t cluster,
                               kapi_ut_t ut );
extern int KAPI_cutportCount( void *pConfig, kapi_cluster_t cluster, 
                               kapi_ut_t utIn );


/* Get port/cport/cutport info */
extern void KAPI_portInfo( void *pConfig, kapi_port_t port,
               kapi_cluster_t *pcluster, kapi_cport_t *pcport,
               kapi_ut_t *put, kapi_cutport_t *pcutport );
extern void KAPI_cportInfo( void *pConfig, kapi_cluster_t cluster,
                kapi_cport_t cport,
                kapi_port_t *pport, kapi_ut_t *put, kapi_cutport_t *pcutport );
extern void KAPI_cutportInfo( void *pConfig, kapi_cluster_t cluster,
                  kapi_ut_t ut, kapi_cutport_t cutport,
                  kapi_port_t *pport, kapi_cport_t *pcport );


/* -----------------   Ports and Clustering   ---------------------- */


/* Get core latency of a functional unit */
extern int  KAPI_CoreLatency( void *pConfig, kapi_fu_t fuProd, int oppProd );
/* Get bypass latency between functional units in different clusters */
extern int  KAPI_InterClusterBypass( void *pConfig,
									kapi_cluster_t clusterProd, kapi_fu_t fuProd,
									int oppProd, kapi_ut_t utProd, kapi_cutport_t
									cutportProd,
									kapi_cluster_t clusterCons, kapi_fu_t fuCons,
									int oppCons, kapi_ut_t utCons, kapi_cutport_t
									cutportCons );

/* Get bypass latency between functional units in the same cluster */
extern int  KAPI_IntraClusterBypass( void *pConfig, kapi_cluster_t cluster,
									kapi_fu_t fuProd, int oppProd, kapi_ut_t utProd, kapi_cutport_t
									cutportProd,
									kapi_fu_t fuCons, int oppCons, kapi_ut_t utCons, kapi_cutport_t
									cutportCons );

/* Get distance between 2 clusters */
extern int  KAPI_ClusterDistance( void *pConfig,
					                kapi_cluster_t clusterProd, kapi_cluster_t clusterDest );

/* Get a list of intraclsuter bypasses for specific cutport */
extern papair_t *KAPI_IntraClusterBypassList( void *pConfig,  kapi_cluster_t clr,   
									kapi_fu_t fuProd, int oppProd, kapi_ut_t utProd, kapi_cutport_t cutportProd, 
									kapi_fu_t fuCons, int oppCons, kapi_ut_t utCons, kapi_cutport_t cutportCons, 
									int *pnbypass );
/* Get a list of interclsuter bypasses for specific cutport */
extern pepair_t *KAPI_InterClusterBypassList( void *pConfig,
									kapi_cluster_t clusterProd,
									kapi_fu_t fuProd, int oppProd, kapi_ut_t utProd,
									kapi_cutport_t cutportProd,
									kapi_cluster_t clusterCons,
									kapi_fu_t fuCons, int oppCons, kapi_ut_t utCons,
									kapi_cutport_t cutportCons,
									int *pnbypass );


/* Find the total latency (core+bypasses) from producer to consumer */
extern int  KAPI_TotalLatency( void *pConfig,
									kapi_cluster_t clusterProd, kapi_fu_t fuProd,
									int oppProd, kapi_ut_t utProd,  kapi_cutport_t cutportProd,
									kapi_cluster_t clusterCons, kapi_fu_t fuCons,
									int oppCons,  kapi_ut_t utCons, kapi_cutport_t cutportCons );

/* Find the minimal possible intracluster latency from producer to consumer */
extern int  KAPI_MinIntraClusterTotalLatency( void *pConfig, kapi_cluster_t cluster,
			                        kapi_fu_t fuProd, int oppProd, kapi_fu_t fuCons, int oppCons );

/* Find maximum latency taking into account direct bypasses */
extern int	KAPI_MaxIntraClusterTotalLatency( void *pConfig, kapi_cluster_t cluster,
									kapi_fu_t fuProd, int oppProd, kapi_fu_t fuCons, int oppCons );

/* ------------------  Architectural queries  ------------------------ */



/* Get number of syllables of specific type in a bundle type */
extern int  KAPI_SylCount_bid( void *pConfig, kapi_bid_t bid, kapi_syl_t syl );
/* Get stop bit placement for a bundle type */
extern int  KAPI_SbitPlacement_bid( void *pConfig, kapi_bid_t bid );
/* Is the bundle type reserved? */
extern int  KAPI_isReserved_bid( void *pConfig, kapi_bid_t bid );
/* Get syllable type for each slot in the bundle */
extern void KAPI_SylOrder_bid( void *pConfig, kapi_bid_t bid, kapi_syl_t mpsyl[ nSYLBID ] );
/* Get unit types for a specific syllable type */
extern void KAPI_utCount_syl( void *pConfig, kapi_syl_t syl, kapi_ut_t mput[kapi_nUT] );
/* Get bundle name */
extern char *KAPI_bidName( void *pConfig, kapi_bid_t bid );

/* Non MED section begin */
/* Version stuff */

/* Internal Version */
extern double KAPI_ia64_GetInternalVersion();
/* Tool version */
extern int KAPI_ia64_GetXVersion_MAJOR();
extern int KAPI_ia64_GetXVersion_MINOR();
/* API version */
extern int KAPI_ia64_GetAPIVersion_MAJOR();
extern int KAPI_ia64_GetAPIVersion_MINOR();
/* Non MED section end */

/* Non MED section begin */



/* ------------------  Saving internal structures to files  ---------- */
/* For all of the following:
	fp: File for saving the structure.
	pConfig: pointer to a knobs structure that was initialized with KAPI_Initialize,
			 while the flag for building IA64 structures was turned on.
	pchName: name for the structure saved in the file.
*/
extern int KAPI_save_as_header_all_IA64_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_latency_all_info( FILE *fp, void *pConfig , char *pchName);
extern int KAPI_save_as_header_latency_core_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_cluster_all_info( FILE *fp, void *pConfig , char *pchName);
extern int    KAPI_save_as_header_cluster_distance_info( FILE *fp, void *pConfig , char *pchName);
extern int    KAPI_save_as_header_cluster_intracluster_latency_info( FILE *fp, void *pConfig , char *pchName);
extern int    KAPI_save_as_header_cluster_intercluster_latency_info( FILE *fp, void *pConfig , char *pchName);
extern int    KAPI_save_as_header_cluster_width_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_functional_units_info_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_cport_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_instruction_all_info( FILE *fp, void *pConfig , char *pchName);
extern int  KAPI_save_as_header_instruction_type_info( FILE *fp, void *pConfig , char *pchName);
extern int KAPI_save_as_header_byd_n_syl_info( FILE *fp, void *pConfig , char *pchName);
extern int KAPI_fEnableIA64call_from_header(void **pConfig, void *pHeaderConfig, int iReserved);

/* Non MED section end */


/* -----------------  Cache functions ---------------- */
/*
   Return the number of levels in the cache hierarchy of the
   given type (note that this call does not take a bitmask -- only
   one of the valid cachecontent type values (instruction/data)).
   -1 will be returned for illegal cachecontent values.
*/
extern int KAPI_nCacheHierarchyLevels(void *pConfig, kapi_cache_types_e cachecontent );

/* KAPI_cacheHierarchy - returns a structure that contains information about
   the cache hierarch as indicated in the structure comments.
   *** user is responsible for deallocating! 

   If the value passed for level is greater than the number of
   levels in that type of hierarchy, NULL is returned.

   If an invalid value for cachecontent (as given by the
   KAPI_CACHECONTENT_xxxx enums, then NULL is returned.

   Unified caches have bits set for both I and D contents. I-only
   and D-only have only their corresponding bit set.
   NULL will be returned for illegal content values or illegal level values.
*/
kapi_cache_t *KAPI_CacheHierarcy(void *pConfig, int level, kapi_cache_types_e cachecontent );


/* Non MED section begin */
#ifdef MED_API
/* Non MED section end */

typedef enum {
	VER_X_MAJOR,
	VER_X_MINOR,
	VER_DVLOC,
	VER_KNOBSFILE,
	VER_KAPI_LAYER2,
	VER_KAPI_LAYER3,
	VER_SCHED
} SCHED_VERSION_E;


extern double SCHED_VERSION(SCHED_VERSION_E VersionType);

/* Non MED section begin */
#endif /*MED only API*/
/* Non MED section end */



/* Non MED section begin */
#endif /* of header file */
/* Non MED section end */

