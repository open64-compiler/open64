// @(#)root/matrix:$Name:  $:$Id: TMatrixDBase.cxx,v 1.17 2004/10/23 20:19:04 brun Exp $
// Authors: Fons Rademakers, Eddy Offermann   Nov 2003

//#include "TString.h"
#include "TMatrixDBase.h"
//#include "TMatrixFBase.h"
//#include "TVectorD.h"


//______________________________________________________________________________
void TMatrixDBase::DoubleLexSort(Int_t n,Int_t *first,Int_t *second,Double_t *data)
{
  
  const int incs[] = {1,5,19,41,109,209,505,929,2161,3905,8929,16001,INT_MAX};

  Int_t kinc = 0;
  while (incs[kinc] <= n/2)
    kinc++;
  kinc -= 1;

  // incs[kinc] is the greatest value in the sequence that is also <= n/2.
  // If n == {0,1}, kinc == -1 and so no sort will take place.

  for( ; kinc >= 0; kinc--) {
    const Int_t inc = incs[kinc];

    for (Int_t k = inc; k < n; k++) {
      const Double_t tmp = data[k];
      const Int_t fi = first [k];
      const Int_t se = second[k];
      Int_t j;
      for (j = k; j >= inc; j -= inc) {
        if ( fi < first[j-inc] || (fi == first[j-inc] && se < second[j-inc]) ) {
          data  [j] = data  [j-inc];
          first [j] = first [j-inc];
          second[j] = second[j-inc];
        } else
          break;
      }
      data  [j] = tmp;
      first [j] = fi;
      second[j] = se;
    }
  }
}


//______________________________________________________________________________
/*
TMatrixDBase &TMatrixDBase::SetMatrixArray(const Double_t *data,Option_t *option) 
{
  // Copy array data to matrix . It is assumed that array is of size >= fNelems
  // (=)))) fNrows*fNcols
  // option indicates how the data is stored in the array:
  // option =
  //          'F'   : column major (Fortran) m[i][j] = array[i+j*fNrows]
  //          else  : row major    (C)       m[i][j] = array[i*fNcols+j] (default)

 // Assert(IsValid());

 // TString opt = option;
 // opt.ToUpper();

  Double_t *elem = GetMatrixArray();
 // if (opt.Contains("F")) {
    for (Int_t irow = 0; irow < fNrows; irow++) {
      const Int_t off1 = irow*fNcols;
      Int_t off2 = 0;
      for (Int_t icol = 0; icol < fNcols; icol++) {
        elem[off1+icol] = data[off2+irow];
        off2 += fNrows;
      }
    }
  //}
  //else
  //  memcpy(elem,data,fNelems*sizeof(Double_t));

  return *this if  (n <= 0 || !a) return -1;
   Int_t xmin = a[0];
   Long64_t loc = 0;
   for  (Long64_t i = 1; i < n; i++) {
      if (xmin > a[i])  {
         xmin = a[i];
         loc = i;
      }
   }
   return loc;
}

*/
