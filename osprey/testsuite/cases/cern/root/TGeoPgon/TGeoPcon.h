#include "Rtypes.h"

class TGeoPcon //: public TGeoBBox
{
protected:
   // data members
   Int_t                 fNz;    // number of z planes (at least two)
   Double_t              fPhi1;  // lower phi limit 
   Double_t              fDphi;  // phi range
   Double_t             *fRmin;  //[fNz] pointer to array of inner radii 
   Double_t             *fRmax;  //[fNz] pointer to array of outer radii 
   Double_t             *fZ;     //[fNz] pointer to array of Z planes positions 
   
   // methods
public:
   // constructors
   TGeoPcon(Double_t phi, Double_t dphi, Int_t nz);
   virtual void          DefineSection(Int_t snum, Double_t z, Double_t rmin, Double_t rmax);
  // ClassDef(TGeoPcon, 1)         // polycone class 
};

