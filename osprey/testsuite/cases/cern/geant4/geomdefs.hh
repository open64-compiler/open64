//
// ********************************************************************
// * DISCLAIMER                                                       *
// *                                                                  *
// * The following disclaimer summarizes all the specific disclaimers *
// * of contributors to this software. The specific disclaimers,which *
// * govern, are listed with their locations in:                      *
// *   http://cern.ch/geant4/license                                  *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.                                                             *
// *                                                                  *
// * This  code  implementation is the  intellectual property  of the *
// * GEANT4 collaboration.                                            *
// * By copying,  distributing  or modifying the Program (or any work *
// * based  on  the Program)  you indicate  your  acceptance of  this *
// * statement, and all its terms.                                    *
// ********************************************************************
//
//
// $Id: geomdefs.hh,v 1.5 2002/05/15 09:32:10 gcosmo Exp $
// GEANT4 tag $Name: geant4-07-00-patch-01 $
//
// 
// ----------------------------------------------------------------------
// Constants, typedefs, enums for Geometry Section
//
// History:
// 30.06.95 P.Kent

#ifndef GeomDefs_hh
#define GeomDefs_hh

// #include "globals.hh"

// `Infinity' - Distance returned for no intersection etc.
static const G4double kInfinity = 9.0E99;

// Thickness of shapes for Inside function / tracking.
// Should be greater than largest math error from the shape 
// distance calculation routines.
// Tolerance is centred on surface: Inside routine uses a
//                                  tolerance dx +/- kTol/2
// Note: values not `tuned', and because of approximations kRadtolerance and
//       kAngTolerance may not always be used as an exact radius
static const G4double kCarTolerance = 1E-9*mm;
static const G4double kRadTolerance = 1E-9*mm;
static const G4double kAngTolerance = 1E-9*rad;

#endif // Geomdefs.hh
