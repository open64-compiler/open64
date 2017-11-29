#include "TGeoPgon.h"




TGeoPgon::TGeoPgon(Double_t phi, Double_t dphi, Int_t nedges, Int_t nz)
         :TGeoPcon(phi, dphi, nz) 
{
// Default constructor
//   SetShapeBit(TGeoShape::kGeoPgon);
   fNedges = nedges;
 
}

Bool_t TGeoPgon::Contains(Double_t *point) const
{
// test if point is inside this shape
   // check total z range
   if (point[2]<fZ[0]) return kFALSE;
   if (point[2]>fZ[fNz-1]) return kFALSE;
   Double_t divphi = fDphi/fNedges;
   // now check phi
   Double_t phi = TMath::ATan2(point[1], point[0])*TMath::RadToDeg();
   while (phi < fPhi1) phi += 360.0;
   Double_t ddp = phi-fPhi1;
   if (ddp>fDphi) return kFALSE;
   // now find phi division
   Int_t ipsec = TMath::Min(Int_t(ddp/divphi), fNedges-1);
   Double_t ph0 = (fPhi1+divphi*(ipsec+0.5))*TMath::DegToRad();
   // now check projected distance
   Double_t r = point[0]*TMath::Cos(ph0) + point[1]*TMath::Sin(ph0);
   // find in which Z section the point is in
   Int_t iz = TMath::BinarySearch(fNz, fZ, point[2]);
   if (iz==fNz-1) {
      if (r<fRmin[iz]) return kFALSE;
      if (r>fRmax[iz]) return kFALSE;
      return kTRUE;
   }  
   Double_t dz = fZ[iz+1]-fZ[iz];
   Double_t rmin, rmax;
   if (dz<1E-8) {
      // we are at a radius-changing plane 
      rmin = TMath::Min(fRmin[iz], fRmin[iz+1]);
      rmax = TMath::Max(fRmax[iz], fRmax[iz+1]);
      if (r<rmin) return kFALSE;
      if (r>rmax) return kFALSE;
      return kTRUE;
   }   
   // now compute rmin and rmax and test the value of r
   Double_t dzrat = (point[2]-fZ[iz])/dz;
   rmin = fRmin[iz]+dzrat*(fRmin[iz+1]-fRmin[iz]);
   // is the point inside the 'hole' at the center of the volume ?
   if (r < rmin) return kFALSE;
   rmax = fRmax[iz]+dzrat*(fRmax[iz+1]-fRmax[iz]);
   if (r > rmax) return kFALSE;
   return kTRUE;
}


