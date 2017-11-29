#include "TMath.h"
//#include "TObject.h"

#ifndef INT_MAX
#define INT_MAX 2147483647
#endif
class TMatrixDBase //: public TObject 
{

private:
  Double_t *GetElements();  // This function is now obsolete (and is not implemented) you should use TMatrix::GetMatrixArray().

protected:
//este enum pertenece a la cabecera TObject.h (fBits)
UInt_t         fBits;       //bit field status word
//const    INT_MAX=2147483647;

  Int_t     fNrows;               // number of rows
  Int_t     fNcols;               // number of columns
  Int_t     fRowLwb;              // lower bound of the row index
  Int_t     fColLwb;              // lower bound of the col index
  Int_t     fNelems;              // number of elements in matrix
  Int_t     fNrowIndex;           // length of row index array (= fNrows+1) wich is only used for sparse matrices

  Double_t  fTol;                 // sqrt(epsilon); epsilon is smallest number number so that  1+epsilon  
                                  //  fTol is used in matrix decomposition (like in inversion)

  enum {kSizeMax = 25};           // size data container on stack, see New_m(),Delete_m()
  enum {kWorkMax = 100};          // size of work array's in several routines

  Bool_t    fIsOwner;             //!default kTRUE, when Use array kFALSE

  static  void DoubleLexSort (Int_t n,Int_t *first,Int_t *second,Double_t *data);
  virtual void Allocate      (Int_t nrows,Int_t ncols,Int_t row_lwb = 0,
                              Int_t col_lwb = 0,Int_t init = 0,Int_t nr_nonzero = -1) = 0;

public:
//esto esta definido en cint/include/limit.h

//este enum pertenece a la cabecera TObject.h
   enum {
      kIsOnHeap      = 0x01000000,    // object is on heap
      kNotDeleted    = 0x02000000,    // object has not been deleted
      kZombie        = 0x04000000,    // object ctor failed
      kBitMask       = 0x00ffffff
   };

  enum EMatrixStatusBits {
    kStatus = BIT(14) // set if matrix object is valid
  };

  enum EMatrixCreatorsOp1 { kZero,kUnit,kTransposed,kInverted,kAtA };
  enum EMatrixCreatorsOp2 { kMult,kTransposeMult,kInvMult,kMultTranspose,kPlus,kMinus };

           inline       Int_t     GetRowLwb     () const { return fRowLwb; }
          inline       Int_t     GetRowUpb     () const { return fNrows+fRowLwb-1; }
          inline       Int_t     GetNrows      () const { return fNrows; }
          inline       Int_t     GetColLwb     () const { return fColLwb; }
          inline       Int_t     GetColUpb     () const { return fNcols+fColLwb-1; }
          inline       Int_t     GetNcols      () const { return fNcols; }
          inline       Int_t     GetNoElements () const { return fNelems; }
          inline       Double_t  GetTol        () const { return fTol; }
          //const Double_t GetMatrixArray() const { return fElements; }
          //inline       Double_t *GetMatrixArray()       { return fElements; }

/*
 virtual        const Double_t *GetMatrixArray  () const = 0;
  virtual              Double_t *GetMatrixArray  ()       = 0;
  virtual        const Int_t    *GetRowIndexArray() const = 0;
  virtual              Int_t    *GetRowIndexArray()       = 0;
  virtual        const Int_t    *GetColIndexArray() const = 0;
  virtual              Int_t    *GetColIndexArray()       = 0;

  virtual              TMatrixDBase &SetRowIndexArray(Int_t *data) = 0;
  virtual              TMatrixDBase &SetColIndexArray(Int_t *data) = 0;
 */
// virtual              TMatrixDBase &SetMatrixArray  (const Double_t *data,Option_t *option="");
         // inline       Double_t      SetTol          (Double_t tol);

 // virtual void   Clear      (Option_t *option="") = 0;

  inline  void   Invalidate ()       { SetBit(kStatus); }
  inline  void   MakeValid  ()       { ResetBit(kStatus); }
  inline  Bool_t IsValid    () const { return !TestBit(kStatus); }
  inline  Bool_t IsOwner    () const { return fIsOwner; }
// inline  Double_t NormInf    () const { return RowNorm(); }
//  inline  Double_t Norm1      () const { return ColNorm(); }
 
  Double_t SetTol(Double_t newTol) //k  Double_t TMatrixDBase::SetTol(Double_t newTol)
{
  const Double_t oldTol = fTol;
  if (newTol >= 0.0)
    fTol = newTol;
  return oldTol;
}

  void     SetBit(UInt_t f, Bool_t set){
      if (set)
        SetBit(f);
      else
        ResetBit(f);
    }
   void     SetBit(UInt_t f) { fBits |= f & kBitMask; }
   void     ResetBit(UInt_t f) { fBits &= ~(f & kBitMask); }
   Bool_t   TestBit(UInt_t f) const { return (Bool_t) ((fBits & f) != 0); }
   Int_t    TestBits(UInt_t f) const { return (Int_t) (fBits & f); }
   void     InvertBit(UInt_t f) { fBits ^= f & kBitMask; }



};
