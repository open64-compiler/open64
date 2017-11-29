
// J.M. Dana
// dana@ace.ual.es

#include <stdio.h> // Compatibility with
#include <errno.h> // gcc 2.96 
#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <vector>
#include <string>
#include "Rtypes.h"
#include "TMath.h"
#include "TGeoCone.h"
//_____________________________________________________________________________
void TGeoCone::SetConeDimensions(Double_t dz, Double_t rmin1, Double_t rmax1,
                             Double_t rmin2, Double_t rmax2)
{
   if (rmin1>=0) {
      if (rmax1>0) {
         if (rmin1<=rmax1) {
         // normal rmin/rmax
            fRmin1 = rmin1;
            fRmax1 = rmax1;
         } else {
            fRmin1 = rmax1;
            fRmax1 = rmin1;
			// WITHOUT ERRORS !!!
            //Warning("SetConeDimensions", "rmin1>rmax1 Switch rmin1<->rmax1");
            //SetShapeBit(TGeoShape::kGeoBad);
         }
      } else {
         // run-time
         fRmin1 = rmin1;
         fRmax1 = rmax1;
      }
   } else {
      // run-time
      fRmin1 = rmin1;
      fRmax1 = rmax1;
   }
   if (rmin2>=0) {
      if (rmax2>0) {
         if (rmin2<=rmax2) {
         // normal rmin/rmax
            fRmin2 = rmin2;
            fRmax2 = rmax2;
         } else {
            fRmin2 = rmax2;
            fRmax2 = rmin2;
			// WITHOUT ERRORS !!!
            //Warning("SetConeDimensions", "rmin2>rmax2 Switch rmin2<->rmax2");
            //SetShapeBit(TGeoShape::kGeoBad);
         }
      } else {
         // run-time
         fRmin2 = rmin2;
         fRmax2 = rmax2;
      }
   } else {
      // run-time
      fRmin2 = rmin2;
      fRmax2 = rmax2;
   }

   fDz   = dz;
}


//_____________________________________________________________________________
TGeoCone::TGeoCone(Double_t dz, Double_t rmin1, Double_t rmax1,
                   Double_t rmin2, Double_t rmax2)
         //:TGeoBBox(0, 0, 0)
{
// Default constructor specifying minimum and maximum radius
//   SetShapeBit(TGeoShape::kGeoCone);
   SetConeDimensions(dz, rmin1, rmax1, rmin2, rmax2);
//   if ((dz<0) || (rmin1<0) || (rmax1<0) || (rmin2<0) || (rmax2<0)) {
//      SetShapeBit(kGeoRunTimeShape);
//   }
//   else ComputeBBox(); // THIS FUNCTION IS EMPTY !!!
}

//_____________________________________________________________________________
Bool_t TGeoCone::Contains(Double_t *point) const
{
// test if point is inside this cone
   if (TMath::Abs(point[2]) > fDz) return kFALSE;
#ifdef NODIVIDE
   Double_t r2 = (point[0]*point[0]+point[1]*point[1])*fDz*fDz;
   Double_t rl = 0.5*(fRmin2*(point[2]+fDz)+fRmin1*(fDz-point[2]));
   Double_t rh = 0.5*(fRmax2*(point[2]+fDz)+fRmax1*(fDz-point[2]));
#else
   Double_t r2 = point[0]*point[0]+point[1]*point[1];
   Double_t rl = 0.5*(fRmin2*(point[2]+fDz)+fRmin1*(fDz-point[2]))/fDz;
   Double_t rh = 0.5*(fRmax2*(point[2]+fDz)+fRmax1*(fDz-point[2]))/fDz;
#endif
   if ((r2<rl*rl) || (r2>rh*rh)) return kFALSE;
   return kTRUE;
}

