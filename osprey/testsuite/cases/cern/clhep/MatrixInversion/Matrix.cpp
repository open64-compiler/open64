#include <vector> 
#include <memory>

#include "GenMatrix.h"
#include "Matrix.h"

HepMatrix::HepMatrix(int p,int q,int init)
   : m(p*q), nrow(p), ncol(q)
{
   size = nrow * ncol;

   if (size > 0) {
      switch(init)
      {
      case 0:
	 break;

      case 1:
	 {
	    if ( ncol == nrow ) {
 	       mIter a = m.begin();
 	       mIter b = m.end();
	       for( ; a<b; a+=(ncol+1)) *a = 1.0;
	    } else {
//	       error("Invalid dimension in HepMatrix(int,int,1).");
	    }
	    break;
	 }
      default:
		  break;
//	 error("Matrix: initialization must be either 0 or 1.");
      }
   }
}

HepMatrix::~HepMatrix(){};

