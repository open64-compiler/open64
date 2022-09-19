//-*-c++-*-
// =============================================================================
// =============================================================================
//
// Module: opt_vsa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_vsa.cxx,v $
//
// Revision history:
//  Spring 2018 - Original Version
//
// =============================================================================
//
// Description:
//
//  Configure the -VSA group (included in config.cxx).
//
// =============================================================================
// =============================================================================
/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_vsa.h"


/* ====================================================================
 *
 * Global flag variables which reflect the -VSA group options.
 *
 * ====================================================================
 */

BOOL VSA_Vra = TRUE;
BOOL VSA_Uiv = TRUE;
BOOL VSA_Aob = TRUE;
BOOL VSA_Npd = TRUE;
BOOL VSA_Ubf = TRUE;
BOOL VSA_Uaf = TRUE;
BOOL VSA_Msf = TRUE;
BOOL VSA_Prf = TRUE;
BOOL VSA_Dse = TRUE;
BOOL VSA_Ral = TRUE;
BOOL VSA_Muchi = TRUE;
BOOL VSA_Not_Muchi = TRUE;
BOOL VSA_Hor = TRUE;

BOOL VSA_Output_Json = FALSE;
BOOL VSA_Emit_Whirl = TRUE;

/* List of global variables to turn on and off various optimizations */

/* ====================================================================
 *
 * Descriptor for the -VHO option group.
 *
 * ====================================================================
 */
static OPTION_DESC Options_VSA[] = {
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "uiv_uninit_var",          "uiv",
    FALSE, 0, 0, &VSA_Uiv,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "aob_array_out_of_bound",  "aob",
    FALSE, 0, 0, &VSA_Aob,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "npd_null_ptr_deref",      "npd",
    FALSE, 0, 0, &VSA_Npd,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ubf_unbalanced_free",     "ubf",
    FALSE, 0, 0, &VSA_Ubf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "uaf_use_after_free",      "uaf",
    FALSE, 0, 0, &VSA_Uaf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "msf_missing_free",        "msf",
    FALSE, 0, 0, &VSA_Msf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "dead_assign_val",         "dead",
    FALSE, 0, 0, &VSA_Dse,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "ral_return_addr_lcl",     "ral",
    FALSE, 0, 0, &VSA_Ral,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "prf_missing_free",        "prf",
    FALSE, 0, 0, &VSA_Prf,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vra_value_range",         "vra",
    FALSE, 0, 0, &VSA_Vra,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "hor_heap_object",         "hor",
    FALSE, 0, 0, &VSA_Hor,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "muchi_gbl_ref_at_entry",  "muchi",
    FALSE, 0, 0, &VSA_Muchi,NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "nmuchi_gbl_ref_at_entry", "nmuchi",
    FALSE, 0, 0, &VSA_Not_Muchi,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "vra_value_range",         "vra",
    FALSE, 0, 0, &VSA_Vra,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "json_output_json",        "json",
    FALSE, 0, 0, &VSA_Output_Json,  NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "emit_whirl",              "emit",
    FALSE, 0, 0, &VSA_Emit_Whirl,  NULL },
  
    { OVK_COUNT }		/* List terminator -- must be last */
};


