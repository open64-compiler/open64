#include "Rtypes.h"
//#include "TMath.h"
#include "TMatrixDBase.h"
class TMatrixDSparse : public TMatrixDBase {

protected:

  Int_t    *fRowIndex;  //[fNrowIndex] row index
  Int_t    *fColIndex;  //[fNelems]    column index
  Double_t *fElements;  //[fNelems]

  virtual void Allocate(Int_t nrows,Int_t ncols,Int_t row_lwb = 0,Int_t col_lwb = 0,
                        Int_t init = 0,Int_t nr_nonzeros = 0);

   
public:
   void AMultBt(const TMatrixDSparse &a,const TMatrixDSparse &b,Int_t constr=1);

  TMatrixDSparse(Int_t no_rows,Int_t no_cols);

  TMatrixDSparse(Int_t row_lwb,Int_t row_upb,Int_t col_lwb,Int_t col_upb,Int_t nr_nonzeros,
                 Int_t *row, Int_t *col,Double_t *data);
  virtual TMatrixDBase &SetMatrixArray(Int_t nr,Int_t *row,Int_t *col,Double_t *data);
  TMatrixDSparse &SetSparseIndex  (Int_t nelem_new);

//virtual const Int_t    *GetRowIndexArray() const;
//virtual       Int_t    *GetRowIndexArray();
  inline const Int_t    *GetRowIndexArray() const { return fRowIndex; }//k
                                                   //removed TMatrixDSparse:: from  *TMatrixDSparse::GetRowIndexArray(
inline       Int_t    *GetRowIndexArray()       { return fRowIndex; }

inline const Double_t *GetMatrixArray  () const { return fElements; }
inline       Double_t *GetMatrixArray  ()       { return fElements; }
//inline const Int_t    *TMatrixDSparse::GetRowIndexArray() const { return fRowIndex; }
//inline       Int_t    *TMatrixDSparse::GetRowIndexArray()       { return fRowIndex; }
inline const Int_t    *GetColIndexArray() const { return fColIndex; }
inline       Int_t    *GetColIndexArray()       { return fColIndex; }
};
