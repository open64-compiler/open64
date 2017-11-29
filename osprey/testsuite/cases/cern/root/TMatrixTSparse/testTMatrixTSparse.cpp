//#include <stdio.h> // Compatibility with
//#include <errno.h> // gcc 2.96 
#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <vector>
#include <string>

#include "timing.h"
#include "Rtypes.h"
//#include "TMatrixTSparse.h"

using namespace std;

#include "TMatrixTSparse.cpp" //KEVIN .... program wont' link correctedly when using templates unless i include cxx file instead of .h

#define N 4000000

//function printMatrix
// @ param : TMatrixTSparse
// print only the elements different from 0 and includes the location in the matrix (N_row,N_col)=element

template<class Element>
void printMatrix(TMatrixTSparse<Element> matrix){

  Int_t *fRowIndex=matrix.GetRowIndexArray();
  Int_t *fColIndex=matrix.GetColIndexArray();
  Element * fElements=matrix.GetMatrixArray();
  Int_t fNrows= matrix.GetNrows();
  Int_t fRowLwb= matrix.GetRowLwb();
  Int_t fColLwb= matrix.GetColLwb();
  
  for (Int_t irow = 0; irow < fNrows; irow++) {                        
    const Int_t sIndex = fRowIndex[irow];                              
    const Int_t eIndex = fRowIndex[irow+1];         
            
    for (Int_t index = sIndex; index < eIndex; index++) {              
      const Int_t icol = fColIndex[index];                             
      const Double_t data = fElements[index];                          
      printf("\n data(%d,%d) = %0f",irow+fRowLwb,icol+fColLwb,data);   
    }                                                                  
  }
  printf("\n"); 
} 

template<class Element>
bool checkCorrect(TMatrixTSparse<Element> matrix){
  Int_t *fRowIndex=matrix.GetRowIndexArray();
  Int_t *fColIndex=matrix.GetColIndexArray();
  Element * fElements=matrix.GetMatrixArray();
  Int_t fNrows= matrix.GetNrows();
  Int_t fRowLwb= matrix.GetRowLwb();
  Int_t fColLwb= matrix.GetColLwb();
  double epsilon = 0.000001;
  
  for (Int_t irow = 0; irow < fNrows; irow++) {                        
    const Int_t sIndex = fRowIndex[irow];                              
    const Int_t eIndex = fRowIndex[irow+1];         
            
    for (Int_t index = sIndex; index < eIndex; index++) {              
      const Int_t icol = fColIndex[index];                             
      const Double_t data = fElements[index];                          
      if (data+epsilon < 0.1 || data-epsilon > 0.1)
	return false;
    }                                                                  
  }
  return true;
}

// Debugging function, isn't part of HepMatrix class
template<class Element>
void generateElements(Element *data, int size) {
  int i;

  srand48(clock()+getpid());
  
  for(i=0;i<size+1;i++)
    data[i]=drand48()+1;
}


int main (int argc, char **argv) {

  Timer *time=new Timer();

  Int_t a_row[100];
  Int_t a_col[100];
  int counter=0;
  for (int i=0; i<10;i++)
    {
      for(int k=0; k<10; k++)
	{
	  a_row[counter]=i;
	  a_col[counter]=k;
	  counter++;
	}
    }
  Double_t a_data[100];
  for(int i=0;i<100;i++)
    a_data[i]=0.1;




  TMatrixTSparse<Double_t> amatrix=TMatrixTSparse<Double_t>(0,10,0,10,100,a_row,a_col,a_data);
  TMatrixTSparse<Double_t> bmatrix=TMatrixTSparse<Double_t>(0,10,0,10,100,a_row,a_col,a_data);

  //printMatrix(amatrix);
  //printMatrix(bmatrix);

  TMatrixTSparse<Double_t> prev=TMatrixTSparse<Double_t>(10,10);
  TMatrixTSparse<Double_t> sol=TMatrixTSparse<Double_t>(10,10);
  TMatrixTSparse<Double_t> c=TMatrixTSparse<Double_t>(10,10);
//Remember AMultbT  does A*B', but the parameter B should be B not B'.

 time->start();
 for(int i=0;i<N;i++) {
   c.AMultBt(prev,sol);
   prev=amatrix;
   sol=amatrix;
 }
 
 //printMatrix(c);
 time->getTime();
 cout.precision(5);
 

 cout << endl << "time = " << time->getwTime() << endl;
 
  if (checkCorrect(c))
    cout << "testTMatrixTSparse -- OK" << endl;
  else{
    cout << "testTMatrixTSparse -- FAILURE   (all values in matrix c should be 0.1)" << endl;
    printMatrix(c);
  }
 
 return 0;
}
