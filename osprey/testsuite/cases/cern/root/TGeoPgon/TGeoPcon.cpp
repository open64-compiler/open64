#include <string.h>
#include "TGeoPcon.h"




//_____________________________________________________________________________
TGeoPcon::TGeoPcon(Double_t phi, Double_t dphi, Int_t nz)//:TGeoBBox(0, 0, 0)
{
// Default constructor
   //SetShapeBit(TGeoShape::kGeoPcon);
   fPhi1 = phi;
   if (fPhi1<0) fPhi1+=360.;
   fDphi = dphi;
   fNz   = nz;
   fRmin = new Double_t [nz];
   fRmax = new Double_t [nz];
   fZ    = new Double_t [nz];

   memset(fRmin, 0, nz*sizeof(Double_t));
   memset(fRmax, 0, nz*sizeof(Double_t));
   memset(fZ,0 , nz*sizeof(Double_t));

}


//_____________________________________________________________________________
void TGeoPcon::DefineSection(Int_t snum, Double_t z, Double_t rmin, Double_t rmax)
{


// Defines z position of a section plane, rmin and rmax at this z. Sections
// should be defined in increasing or decreasing Z order and the last section 
// HAS to be snum = fNz-1
   if ((snum<0) || (snum>=fNz)) return;
   fZ[snum]    = z;
   fRmin[snum] = rmin;
   fRmax[snum] = rmax;


   if (rmin>rmax) 
      //Warning("DefineSection", "Shape %s: invalid rmin=%g rmax=%g", GetName(), rmin, rmax);
   if (snum==(fNz-1)) {
      // Reorder sections in increasing Z order
      if (fZ[0] > fZ[snum]) {
         Int_t iz = 0;
         Int_t izi = fNz-1;
         Double_t temp;
         while (iz<izi) {
            temp = fZ[iz];
            fZ[iz] = fZ[izi];
            fZ[izi] = temp;
            temp = fRmin[iz];
            fRmin[iz] = fRmin[izi];
            fRmin[izi] = temp;
            temp = fRmax[iz];
            fRmax[iz] = fRmax[izi];
            fRmax[izi] = temp;
            iz++;
            izi--;
         }   
      }      
     // ComputeBBox();
   }   
}


