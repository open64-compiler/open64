#include "TGeoPcon.h"
#include "Rtypes.h"
#include "TMath.h"


 
class TGeoPgon : public TGeoPcon
{
protected:
   // data members
   Int_t                 fNedges;    // number of edges (at least one)
   
 
public:
   // constructors
    TGeoPgon(Double_t phi, Double_t dphi, Int_t nedges, Int_t nz);
   virtual Bool_t        Contains(Double_t *point) const;
};


