/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

/* ====================================================================
 * ====================================================================
 *
 *  TN utility routines which include target dependencies.
 *
 *  THIS FILE IS ONLY TO BE INCLUDE BY ../tn.h!!!!
 *
 * ====================================================================
 * ====================================================================
 */

//
// Generate TN to contain a general predicate value
//
inline TN*
Gen_Predicate_TN()
{
  return Gen_Register_TN(ISA_REGISTER_CLASS_integer, 4);
}

inline BOOL TN_is_fcc_register (const TN *tn)
{
  return TN_register_class(tn) == ISA_REGISTER_CLASS_fcc; 
}

inline TN* Hilo_TN (void)
{
  return Build_Dedicated_TN(ISA_REGISTER_CLASS_hilo, REGISTER_MIN, 0);
}
