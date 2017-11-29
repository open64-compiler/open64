#include "G4Types.hh"
#include "G4SIunits.hh"

#include "timing.h"
#include "geomdefs.hh"

enum EInside {kOutside,kSurface,kInside};

const double pi = 3.14159265358979323846;
const double twopi=2*pi;

#include <iostream>
#include <math.h>
#include "Hep3Vector.h"
typedef Hep3Vector G4ThreeVector;

#include "G4String.hh"
#include "G4Tubs.hh"

G4Tubs::G4Tubs( const G4String &pName,
                      G4double pRMin, G4double pRMax,
                      G4double pDz,
                      G4double pSPhi, G4double pDPhi )
  //: G4CSGSolid(pName)
{

  if (pDz>0) // Check z-len
  {
    fDz = pDz ;
  }
  else
  {
    //G4cerr << "ERROR - G4Tubs()::G4Tubs(): " << GetName() << G4endl
    //       << "        Negative Z half-length ! - "
    //       << pDz << G4endl;
    //G4Exception("G4Tubs::G4Tubs()", "InvalidSetup", FatalException,
	//            "Invalid Z half-length");
  }
  if ( pRMin < pRMax && pRMin >= 0 ) // Check radii
  {
    fRMin = pRMin ; 
    fRMax = pRMax ;
  }
  else
  {
    //G4cerr << "ERROR - G4Tubs()::G4Tubs(): " << GetName() << G4endl
    //       << "        Invalid values for radii !" << G4endl
    //       << "        pRMin = " << pRMin << ", pRMax = " << pRMax << G4endl;
    //G4Exception("G4Tubs::G4Tubs()", "InvalidSetup", FatalException,
    //            "Invalid radii.");
  }
  if ( pDPhi >= twopi ) // Check angles
  {
    fDPhi=twopi;
  }
  else
  {
    if ( pDPhi > 0 )
    {
      fDPhi = pDPhi;
    }
    else
    {
      //G4cerr << "ERROR - G4Tubs()::G4Tubs(): " << GetName() << G4endl
      //       << "        Negative delta-Phi ! - "
      //       << pDPhi << G4endl;
      //G4Exception("G4Tubs::G4Tubs()", "InvalidSetup", FatalException,
      //            "Invalid dphi.");
    }
  }
  
  // Ensure fSphi in 0-2PI or -2PI-0 range if shape crosses 0

  fSPhi = pSPhi;

  if ( fSPhi < 0 )
  {
    fSPhi = twopi - /*std::*/fmod(/*std::*/fabs(fSPhi),twopi) ;
  }
  else
  {
    fSPhi = /*std::*/fmod(fSPhi,twopi) ;
  }
  if (fSPhi + fDPhi > twopi )
  {
    fSPhi -= twopi ;
  }
}

EInside G4Tubs::Inside( const G4ThreeVector& p ) const
{
  G4double r2,pPhi,tolRMin,tolRMax;
  EInside in = kOutside ;

  if (/*std::*/fabs(p.z()) <= fDz - kCarTolerance*0.5)
  {
    r2 = p.x()*p.x() + p.y()*p.y() ;

    if (fRMin) tolRMin = fRMin + kRadTolerance*0.5 ;
    else       tolRMin = 0 ;

    tolRMax = fRMax - kRadTolerance*0.5 ;
      
    if (r2 >= tolRMin*tolRMin && r2 <= tolRMax*tolRMax)
    {
      //  if ( fDPhi == twopi || r2 == 0 )  in = kInside ;
      if ( fDPhi == twopi )  in = kInside ;
      else
      {
        // Try inner tolerant phi boundaries (=>inside)
        // if not inside, try outer tolerant phi boundaries

        pPhi = /*std::*/atan2(p.y(),p.x()) ;

        if ( pPhi < -kAngTolerance*0.5 ) pPhi += twopi ; // 0<=pPhi<2pi

        if ( fSPhi >= 0 )
        {
          if ( (/*std::*/abs((int)pPhi) < kAngTolerance*0.5)
            && (/*std::*/fabs(fSPhi + fDPhi - twopi) < kAngTolerance*0.5) )
          { 
            pPhi += twopi ; // 0 <= pPhi < 2pi
          }
          if ( (pPhi >= fSPhi + kAngTolerance*0.5)
            && (pPhi <= fSPhi + fDPhi - kAngTolerance*0.5) )
          {
            in = kInside ;
          }
          else if ( (pPhi >= fSPhi - kAngTolerance*0.5)
                 && (pPhi <= fSPhi + fDPhi + kAngTolerance*0.5) )
          {
            in = kSurface ;
          }
        }
        else  // fSPhi < 0
        {
          if ( (pPhi <= fSPhi + twopi - kAngTolerance*0.5)
            && (pPhi >= fSPhi + fDPhi  + kAngTolerance*0.5) ) ;
          else if ( (pPhi <= fSPhi + twopi + kAngTolerance*0.5)
                 && (pPhi >= fSPhi + fDPhi  - kAngTolerance*0.5) )
          {
            in = kSurface ;
          }
          else
          {
            in = kInside ;
          }
        }                    
      }
    }
    else  // Try generous boundaries
    {
      tolRMin = fRMin - kRadTolerance*0.5 ;
      tolRMax = fRMax + kRadTolerance*0.5 ;

      if ( tolRMin < 0 ) tolRMin = 0 ;

      if ( (r2 >= tolRMin*tolRMin) && (r2 <= tolRMax*tolRMax) )
      {
        if ( fDPhi == twopi || r2 == 0 ) // Continuous in phi or on z-axis
        {
          in = kSurface ;
        }
        else // Try outer tolerant phi boundaries only
        {
          pPhi = /*std::*/atan2(p.y(),p.x()) ;

          if ( pPhi < -kAngTolerance*0.5 ) pPhi += twopi ; // 0<=pPhi<2pi
          if ( fSPhi >= 0 )
          {
            if ( (/*std::*/abs((int)pPhi) < kAngTolerance*0.5)
              && (/*std::*/fabs(fSPhi + fDPhi - twopi) < kAngTolerance*0.5) )
            { 
              pPhi += twopi ; // 0 <= pPhi < 2pi
            }
            if ( (pPhi >= fSPhi - kAngTolerance*0.5)
              && (pPhi <= fSPhi + fDPhi + kAngTolerance*0.5) )
            {
              in = kSurface ;
            }
          }
          else  // fSPhi < 0
          {
            if ( (pPhi <= fSPhi + twopi - kAngTolerance*0.5)
              && (pPhi >= fSPhi + fDPhi  + kAngTolerance*0.5) )  ;
            else
            {
              in = kSurface ;
            }
          }
        }
      }
    }
  }
  else if (/*std::*/fabs(p.z()) <= fDz + kCarTolerance*0.5)
  {                                          // Check within tolerant r limits
    r2      = p.x()*p.x() + p.y()*p.y() ;
    tolRMin = fRMin - kRadTolerance*0.5 ;
    tolRMax = fRMax + kRadTolerance*0.5 ;

    if ( tolRMin < 0 ) tolRMin = 0 ;

    if ( (r2 >= tolRMin*tolRMin) && (r2 <= tolRMax*tolRMax) )
    {
      if (fDPhi == twopi || r2 == 0 ) // Continuous in phi or on z-axis
      {
        in = kSurface ;
      }
      else // Try outer tolerant phi boundaries
      {
        pPhi = /*std::*/atan2(p.y(),p.x()) ;

        if ( pPhi < -kAngTolerance*0.5 ) pPhi += twopi ;   // 0<=pPhi<2pi
        if ( fSPhi >= 0 )
        {
          if ( (/*std::*/abs((int)pPhi) < kAngTolerance*0.5)
            && (/*std::*/fabs(fSPhi + fDPhi - twopi) < kAngTolerance*0.5) )
          { 
            pPhi += twopi ; // 0 <= pPhi < 2pi
          }
          if ( (pPhi >= fSPhi - kAngTolerance*0.5)
            && (pPhi <= fSPhi + fDPhi + kAngTolerance*0.5) )
          {
            in = kSurface;
          }
        }
        else  // fSPhi < 0
        {
          if ( (pPhi <= fSPhi + twopi - kAngTolerance*0.5)
            && (pPhi >= fSPhi + fDPhi  + kAngTolerance*0.5) )  ;
          else
          {
            in = kSurface ;
          }
        }      
      }
    }
  }
  return in ;
}

