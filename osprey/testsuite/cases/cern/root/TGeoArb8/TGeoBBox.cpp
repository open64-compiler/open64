#include "Rtypes.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"

//_____________________________________________________________________________
void TGeoBBox::SetBoxDimensions(Double_t dx, Double_t dy, Double_t dz, Double_t *origin)
{
// set parameters of box
   fDX = dx;
   fDY = dy;
   fDZ = dz;
   for (Int_t i=0; i<3; i++) {
      if (!origin) {
         fOrigin[i] = 0.0;
      } else {
         fOrigin[i] = origin[i];
      }
   }
   if ((fDX==0) && (fDY==0) && (fDZ==0)) return;
   if ((fDX<0) || (fDY<0) || (fDZ<0)) {
      SetShapeBit(kGeoRunTimeShape);
//      printf("box : %f %f %f\n", fDX, fDY, fDZ);
   }
}     

//_____________________________________________________________________________
TGeoBBox::TGeoBBox(Double_t dx, Double_t dy, Double_t dz, Double_t *origin)
         :TGeoShape("")
{
// Constructor
   SetShapeBit(TGeoShape::kGeoBox);
   SetBoxDimensions(dx, dy, dz, origin);
}

