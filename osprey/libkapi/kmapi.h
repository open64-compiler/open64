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
#ifndef _KMAPI_H
#define _KMAPI_H

#ifdef __cplusplus
extern "C" {
#endif

#include "kapi_ia64.h"
/* Non MED section end */

/*-------------------------------------------------------------------------**
**                                                                         **
** For all of the following functions, void *pConfig should the second     **
** pointer that was previously initialized by KAPI_init (pLayer3Info).	   **
** For further documentation and description of functions, please refer to **
** kapi.doc bundled with the release.                                      **
**-------------------------------------------------------------------------*/


/* a resource can be either free or occupied.
   an invalid value can only result from using an invalid resource
   (i.e. one that was not initialized within kmapi) */
typedef enum KAPI_RESOURCE_STATUS_E {
	kmapi_resource_free=0,
	kmapi_resource_occupied=1,
	kmapi_resource_invalid
} kmapi_resource_status_e;

typedef int kmapi_resource_t;

/* Resource map can be viewd as an array holding resource status for each machine resource.
   Machine resource definition may change for each implementation of the uArch */

typedef bv32_t *kmapi_pResourceMap_t;
typedef bv32_t kmapi_ResourceMap_t;

typedef struct {
	int nResources;
	kmapi_ResourceMap_t *dmResourceMaps;
} kmapi_Resource_Array_t;

/* kmapi functions usualy return kmapi_result to indicate success or failure */
typedef enum _KMAPI_RESULT_E {
	kmapi_success=0,
	kmapi_failure,
	kmapi_invalid_input,
	kmapi_internal_error
} kmapi_result;

#define kmapi_BUNDLE_WIDTH 3

typedef struct _KMAPI_BUNDLE_INFO_T {
	int iPlace; /* place of bundle in issue group */
	kapi_bid_t bid; /* bundle type */
	int iSlot; /* slot of instruction */
} kmapi_bundle_info_t;


/* Non MED section begin */

/* Initializations  */
/********************/

/* to initialize kmapi, use a kapi tructure that was initialized previously 
   to allow layer 2 (ia64) calls. 
   Use the pointer returned by the function in further calls to KMAPI functions.
   Will return NULL if unsuccessful.
*/
extern void *KMAPI_initialize(void *pIA64Config);

/* use KMAPI_finalize to clean up all memory used by kmapi */

extern void KMAPI_finalize(void *pKmapiConfig);



/* Saving/Loading header format */
/********************************/
/* Save kmapi info that was initialized by KMAPI_initialize into file fp, using name pchKmapiInfoName.
   If you did not save the kapi information that was used to initialize kmapi separately,
   you can save it now using pchKapiIA64InfoName. Otherwise use NULL for that parameter,
   and enable kmapi calls from header with the ia64 config you saved separately.
   Compile the resulting file to an object, add this object to your project, and use 
   &KmapiInfoName (and &KapiIA64InfoName if the name was not NULL in the saver) with KMAPI_fEnableKmapiCalls_from_header.
 */
kmapi_result KMAPI_save_as_header_all_kmapi_info( FILE *fp, void *pConfig , char *pchKmapiInfoName, char *pchKapiIA64InfoName);

kmapi_result KMAPI_fEnableKmapiCalls_from_header(void **pConfig, void *pKMAPI_HeaderConfig, 
												 void *pKAPI_IA64_HeaderConfig, int iReserved);
kmapi_result KMAPI_fDisableKmapiCalls_from_header(void **pConfig);

/* Non MED section end */


/* Handling resources & resource maps functions */
/************************************************/
/* Create/Destroy/Clear a resource map. pConfig is a pointer to a struct initialized by KMAPI_initialize */
extern kmapi_pResourceMap_t KMAPI_CreateResourceMap(void *pKmapiConfig);
extern void KMAPI_DestroyResourceMap(kmapi_pResourceMap_t mpMap);
extern void KMAPI_SetAllResources(kmapi_pResourceMap_t mpMap, kmapi_resource_status_e status);
extern void KMAPI_CopyResourceMap(kmapi_pResourceMap_t mpMapDst, kmapi_ResourceMap_t MapSrc);
/* set a signle resource in a map to a status */
extern kmapi_result KMAPI_SetResource(kmapi_pResourceMap_t mpMap, kmapi_resource_t iResource, kmapi_resource_status_e status);
/* set all the resources marked in mpMapSrc to status in mpMapDst */
extern kmapi_result KMAPI_SetResourceMap(kmapi_pResourceMap_t mpMapDst, kmapi_pResourceMap_t mpMapSrc, kmapi_resource_status_e status);
/* check the status of a specific resource in a map */
extern kmapi_resource_status_e KMAPI_ResourceStatus(kmapi_pResourceMap_t mpMap, kmapi_resource_t iResource);
/* Functions to extract all the resources from a map one by one. 
	Call getfirst to get the first resource, then getnext with the previous resource as the second parameter to get the next.
	Both will return a positive value if a resource was found, a negative if no resource was found. */
extern kmapi_resource_t KMAPI_GetFirstAllocatedMapResource(kmapi_pResourceMap_t mpMap);
extern kmapi_resource_t KMAPI_GetNextAllocatedMapResource(kmapi_pResourceMap_t mpMap, kmapi_resource_t iPrevResource);
/* Functions checking the map: */



/* Mapping */
/***********/
/* Map an instruction:
mpMap: resource map of current status
pbinfo: bundle info for instruction.
iid: instruction id.
pAllocatedResource: address of map after allocatio. If this is the same as mpMap, 
an update will occur. Otherwise, only the resources that were allocated will be marked as occupied.
Return Value: 
- kmapi_success if allocation succeeded
- kmapi_failure if allocation failed
- kmapi_invalid_input if pConfig contains information that kmapi can't use for mapping,
  or any other parameter contains illeagal info.
- kmapi_internal_error if a fatal error occured.
*/

extern kmapi_result KMAPI_MapInstructionToPort(void *pKmapiConfig, 
												  kmapi_pResourceMap_t mpMap, 
												  kmapi_bundle_info_t *pbinfo, 
												  kapi_iid_t iid, 
												  kmapi_pResourceMap_t pAllocatedResource);

/* convert kmapi resource to kapi equivalents */
extern kmapi_result KMAPI_ResourceInfo(void *pKmapiConfig,kmapi_resource_t iResource,
												  kapi_port_t *pport,
												  kapi_cluster_t *pclr,kapi_cport_t *pcport,
												  kapi_ut_t *put,kapi_cutport_t *pcutport);


typedef struct {
	int nOptions;				/* size of following array */
	bv32_t *dmpOptions;			/* dynamically allocated array of options */
	kapi_cluster_t *dmpClrID;	/* dynamically allocated array of cluster per option */
} kmapi_allocation_option_t;

/* Get a list of possible mapping options for a specific fuClass and instruction type */
extern kmapi_result KMAPI_AllocationOptions4fu(void *pConfig, 
										kapi_fu_t fu, kapi_it_t it,
										kmapi_allocation_option_t *pOptions);

/* Deallocate memory used by the Allocation option struct */
extern kmapi_result KMAPI_ClearAllocationOptions(kmapi_allocation_option_t *pOptions);

/* Get number of possible allocation options for specific syllable */
kmapi_result KMAPI_nAllocationOptions4syl(void *pConfig, kapi_syl_t syl, int *iOptions);
/* Get raw allocation option data from slot (0..Max issue per cycle) and syllable type	*/
/* Deallocation on client responsibility												*/
kmapi_Resource_Array_t *KMAPI_GetRawOptions(void *pConfig, int iIssueSlot, kapi_syl_t iSyl);

/* Split issue */
/***************/
/* To specify possible implicit cycle breaks 
   due to a uArch (such as mmf in Intel(R) Itanium(TM) processor): */
typedef enum _KMAPI_IMPLICIT_BREAK_E {
	kmapi_no_implicit_break=0,
	kmapi_implicit_break_before=1,
	kmapi_implicit_break_after=2,
	kmapi_implicit_break_both=3
} kmapi_implicit_break_t;


/* KMAPI_ImplicitBreakType determines according to the bid & fuClass in each slot, whether
   the issue group will be split implicitly, and how as indicated by the return value.
   bid is assumed to be a valid bundle id, and mpfu should point to an array of valid fuClasses,
   one for each slot (i.e. 3 for eas2.4). */
extern kmapi_implicit_break_t  KMAPI_ImplicitBreakType(void *pConfig,kapi_bid_t bid, kapi_fu_t *mpfu);

/* Non MED section begin */

/* Version stuff */
/***************/

/* Internal Version */
extern double KMAPI_GetInternalVersion();
/* Tool version */
extern int KMAPI_GetXVersion_MAJOR();
extern int KMAPI_GetXVersion_MINOR();
/* API version */
extern int KMAPI_GetAPIVersion_MAJOR();
extern int KMAPI_GetAPIVersion_MINOR();

/* Non MED section end */

/* Non MED section begin */
#ifdef __cplusplus
}
#endif

#endif /* _KMAPI_H */
/* Non MED section end */
