#include <string>
//#include "Riostream.h"
#include "TMatrixDSparse.h"
//#include "TMatrixD.h"

TMatrixDSparse::TMatrixDSparse(Int_t no_rows,Int_t no_cols)
{
  // Space is allocated for row/column indices and data, but the sparse structure
  // information has still to be set !

  Allocate(no_rows,no_cols,0,0,1);
}


TMatrixDSparse::TMatrixDSparse(Int_t row_lwb,Int_t row_upb,Int_t col_lwb,Int_t col_upb,
                               Int_t nr,Int_t *row, Int_t *col,Double_t *data)
{
  // Space is allocated for row/column indices and data. Sparse row/column index
  // structure together with data is coming from the arrays, row, col and data, resp .
//printf(" Valores de nr %d, row %d,col %d",nr,&row,col);

  const Int_t irowmin = TMath::LocMin(nr,row);
  const Int_t irowmax = TMath::LocMax(nr,row);
/* for (int j=0;j<5;j++)
  printf("valor %d es %d\n",j,row[j]);

  
  Int_t irowmin=2000;
  if  (nr <= 0 || !row)
printf(" Error en rowmin");

  else{  
   Int_t xmin = row[0];
   Long64_t loc = 0;
   for  (Long64_t i = 1; i < nr; i++) {
      if (xmin > row[i])  {
         xmin = row[i];
         loc = i;
      }
   }
  irowmin=loc; 
  }

  Int_t irowmax=2000;
   if  (nr <= 0 || !row) 
     printf(" Error en el locmax");
   else{
   Int_t xmax = row[0];
   Long64_t loc = 0;
   for  (Long64_t i = 1; i < nr; i++) {
      if (xmax < row[i])  {
         xmax = row[i];
         loc = i;
      }
// printf(" xmax %d,  i %d" ,xmax,i);

   }
  irowmax=loc;
  }
*/  const Int_t icolmin = TMath::LocMin(nr,col);
  const Int_t icolmax = TMath::LocMax(nr,col);
  
  //Assert(row[irowmin] >= row_lwb && row[irowmax] <= row_upb);
  //Assert(col[icolmin] >= col_lwb && col[icolmax] <= col_upb);
  Allocate(row_upb-row_lwb+1,col_upb-col_lwb+1,row_lwb,col_lwb,1,nr);

  SetMatrixArray(nr,row,col,data);
}


//______________________________________________________________________________
void TMatrixDSparse::Allocate(Int_t no_rows,Int_t no_cols,Int_t row_lwb,Int_t col_lwb,
                              Int_t init,Int_t nr_nonzeros)
{

 
  // Allocate new matrix. Arguments are number of rows, columns, row lowerbound (0 default)
  // and column lowerbound (0 default), 0 initialization flag and number of non-zero 
  // elements (only relevant for sparse format).
  
  if ( (nr_nonzeros > 0 && (no_rows == 0 || no_cols == 0)) ||
       (no_rows < 0 || no_cols < 0 || nr_nonzeros < 0) )
  { 
    //Error("Allocate","no_rows=%d no_cols=%d non_zeros=%d",no_rows,no_cols,nr_nonzeros);
    printf("/nAllocate","no_rows=%d no_cols=%d non_zeros=%d",no_rows,no_cols,nr_nonzeros);
    Invalidate();
    return;
  }

  MakeValid();
  fNrows     = no_rows;
  fNcols     = no_cols;
  fRowLwb    = row_lwb;
  fColLwb    = col_lwb;
  fNrowIndex = fNrows+1;
  fNelems    = nr_nonzeros;
  fIsOwner   = kTRUE;
  fTol       = DBL_EPSILON;
  

  fRowIndex = new Int_t[fNrowIndex];
  if (init)
    memset(fRowIndex,0,fNrowIndex*sizeof(Int_t));

  if (fNelems > 0) {
    fElements = new Double_t[fNelems];
    fColIndex = new Int_t   [fNelems];
    if (init) {
      memset(fElements,0,fNelems*sizeof(Double_t));
      memset(fColIndex,0,fNelems*sizeof(Int_t));
    }
  } else {
    fElements = 0;
    fColIndex = 0;
  }

}

//______________________________________________________________________________
TMatrixDBase &TMatrixDSparse::SetMatrixArray(Int_t nr,Int_t *row,Int_t *col,Double_t *data)
{
  // Copy nr elements from row/col index and data array to matrix . It is assumed
  // that arrays are of size >= nr

  //Assert(IsValid());
  if (nr <= 0) {
    //Error("SetMatrixArray(Int_t,Int_t*,Int_t*,Double_t*","nr <= 0");
    Invalidate();
    return *this;
  }

  const Int_t irowmin = TMath::LocMin(nr,row);
  const Int_t irowmax = TMath::LocMax(nr,row);
  const Int_t icolmin = TMath::LocMin(nr,col);
  const Int_t icolmax = TMath::LocMax(nr,col);

  //Assert(row[irowmin] >= fRowLwb && row[irowmax] <= fRowLwb+fNrows-1);
  //Assert(col[icolmin] >= fColLwb && col[icolmax] <= fColLwb+fNcols-1);

  DoubleLexSort(nr,row,col,data);

  Int_t nr_nonzeros = 0;
  const Double_t *ep        = data;
  const Double_t * const fp = data+nr;

  while (ep < fp)
    if (*ep++ != 0.0) nr_nonzeros++;

  // if nr_nonzeros != fNelems
  if (nr_nonzeros != fNelems) {
    if (fColIndex) { delete [] fColIndex; fColIndex = 0; }
    if (fElements) { delete [] fElements; fElements = 0; }
    fNelems = nr_nonzeros;
    if (fNelems > 0) {
      fColIndex = new Int_t[nr_nonzeros];
      fElements = new Double_t[nr_nonzeros];
    } else {
      fColIndex = 0;
      fElements = 0;
    }
  }

  if (fNelems <= 0)
    return *this;

  fRowIndex[0] = 0;
  Int_t ielem = 0;
  nr_nonzeros = 0;
  for (Int_t irow = 1; irow < fNrows+1; irow++) {
    if (ielem < nr && row[ielem] < irow) {
      while (ielem < nr) {
        if (data[ielem] != 0.0) {
          fColIndex[nr_nonzeros] = col[ielem]-fColLwb;
          fElements[nr_nonzeros] = data[ielem];
          nr_nonzeros++;
        }
        ielem++;
        if (ielem >= nr || row[ielem] != row[ielem-1])
          break;
      }
    }
    fRowIndex[irow] = nr_nonzeros;
  }

  return *this;
}


//______________________________________________________________________________
void TMatrixDSparse::AMultBt(const TMatrixDSparse &a,const TMatrixDSparse &b,Int_t constr)
{
  // General matrix multiplication. Create a matrix C such that C = A * B'.
  // Note, matrix C is allocated for constr=1.
  
  //Assert(a.IsValid());
  //Assert(b.IsValid());

  if (a.GetNcols() != b.GetNcols() || a.GetColLwb() != b.GetColLwb()) {
    //Error("AMultBt","A and B columns incompatible");
printf("Error AMultBt A and B columns incompatible ");
    Invalidate();
    return;
  }

  if (this == &a) {
    //Error("AMultB","this = &a");
printf("Error AMultBt thisB ");

    Invalidate();
    return;
  }     

  if (this == &b) {
    //Error("AMultB","this = &b");
printf("Error AMultB this ");

    Invalidate();
    return;
  }     

  const Int_t * const pRowIndexa = a.GetRowIndexArray();
  const Int_t * const pColIndexa = a.GetColIndexArray();
  const Int_t * const pRowIndexb = b.GetRowIndexArray();
  const Int_t * const pColIndexb = b.GetColIndexArray();

  Int_t *pRowIndexc;
  Int_t *pColIndexc;
  if (constr) {
    // make a best guess of the sparse structure; it will guarantee
    // enough allocated space !

    Int_t nr_nonzero_rowa = 0;
    {
      for (Int_t irowa = 0; irowa < a.GetNrows(); irowa++)
        if (pRowIndexa[irowa] < pRowIndexa[irowa+1])
          nr_nonzero_rowa++;
    }
    Int_t nr_nonzero_rowb = 0;
    {
      for (Int_t irowb = 0; irowb < b.GetNrows(); irowb++)
        if (pRowIndexb[irowb] < pRowIndexb[irowb+1])
          nr_nonzero_rowb++;
    }

    Int_t nc = nr_nonzero_rowa*nr_nonzero_rowb; // best guess
    Allocate(a.GetNrows(),b.GetNrows(),a.GetRowLwb(),b.GetRowLwb(),1,nc);

    pRowIndexc = this->GetRowIndexArray();
    pColIndexc = this->GetColIndexArray();

    pRowIndexc[0] = 0;
    Int_t ielem = 0;
    for (Int_t irowa = 0; irowa < a.GetNrows(); irowa++) {
      pRowIndexc[irowa+1] = pRowIndexc[irowa];
      if (pRowIndexa[irowa] >= pRowIndexa[irowa+1]) continue;
      for (Int_t irowb = 0; irowb < b.GetNrows(); irowb++) {
        if (pRowIndexb[irowb] >= pRowIndexb[irowb+1]) continue;
        pRowIndexc[irowa+1]++;
        pColIndexc[ielem++] = irowb;
      }
    }
  } else {
    pRowIndexc = this->GetRowIndexArray();
    pColIndexc = this->GetColIndexArray();
  }

  const Double_t * const pDataa = a.GetMatrixArray();
  const Double_t * const pDatab = b.GetMatrixArray();
  Double_t * const pDatac = this->GetMatrixArray();
  Int_t shift = 0;
  Int_t indexc_r = 0;
  for (Int_t irowc = 0; irowc < this->GetNrows(); irowc++) {
    const Int_t sIndexc = pRowIndexc[irowc]+shift;
    const Int_t eIndexc = pRowIndexc[irowc+1];
    const Int_t sIndexa = pRowIndexa[irowc];
    const Int_t eIndexa = pRowIndexa[irowc+1];
    for (Int_t indexc = sIndexc; indexc < eIndexc; indexc++) {
      const Int_t icolc = pColIndexc[indexc];
      const Int_t sIndexb = pRowIndexb[icolc];
      const Int_t eIndexb = pRowIndexb[icolc+1];
      Double_t sum = 0.0;
      Int_t indexb = sIndexb;
      for (Int_t indexa = sIndexa; indexa < eIndexa && indexb < eIndexb; indexa++) {
        const Int_t icola = pColIndexa[indexa];
        while (indexb < eIndexb && pColIndexb[indexb] <= icola) {
          if (icola == pColIndexb[indexb]) {
            sum += pDataa[indexa]*pDatab[indexb];
            break;
          }
          indexb++;
        }
      }
      if (!constr)
        pDatac[indexc] = sum;
      else {
        if (sum != 0.0) {
          pRowIndexc[irowc+1]  = indexc_r+1;
          pColIndexc[indexc_r] = icolc;
          pDatac[indexc_r] = sum;
          indexc_r++;
        } else
          shift++;
      }
    }
  }

  if (constr)
    SetSparseIndex(indexc_r);
}



TMatrixDSparse &TMatrixDSparse::SetSparseIndex(Int_t nelems_new)
{
  // Increase/decrease the number of non-zero elements to nelems_new

  if (nelems_new != fNelems) {
    Int_t nr = TMath::Min(nelems_new,fNelems);
    Int_t *oIp = fColIndex;
    fColIndex = new Int_t[nelems_new];
    memmove(fColIndex,oIp,nr*sizeof(Int_t));
    if (oIp) delete [] oIp; 
    Double_t *oDp = fElements;
    fElements = new Double_t[nelems_new];
    memmove(fElements,oDp,nr*sizeof(Double_t));
    if (oDp) delete [] oDp; 
    fNelems = nelems_new;
    if (nelems_new > nr) { 
      memset(fElements+nr,0,(nelems_new-nr)*sizeof(Double_t));
      memset(fColIndex+nr,0,(nelems_new-nr)*sizeof(Int_t));
    } else {
      for (Int_t irow = 0; irow < fNrowIndex; irow++) 
        if (fRowIndex[irow] > nelems_new)
          fRowIndex[irow] = nelems_new;
    }
  }

  return *this;
}

//_______________________________


