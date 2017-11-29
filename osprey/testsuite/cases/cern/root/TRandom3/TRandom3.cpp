
// J.M. Dana
// dana@ace.ual.es

#include <stdio.h> // Compatibility with
#include <errno.h> // gcc 2.96 
#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <vector>
#include <string>

#include "timing.h"
#include "Rtypes.h"
#include "TRandom.h"
#include "TRandom3.h"

using namespace std;

#define N	500000000

// ------------------------------------------------------------------

TRandom3::TRandom3(UInt_t seed)
{
//*-*-*-*-*-*-*-*-*-*-*default constructor*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
// If seed is 0, the seed is automatically computed via a TUUID object.
// In this case the seed is guaranteed to be unique in space and time.

   //SetName("Random3");
   //SetTitle("Random number generator: Mersenne Twistor");
   SetSeed(seed);
}

TRandom3::~TRandom3(){}

void TRandom3::SetSeed(UInt_t seed) {
//  Set the random generator sequence
// if seed is 0 (default value) a TUUID is generated and used to fill
// the first 8 integers of the seed array.
// In this case the seed is guaranteed to be unique in space and time.
   
	TRandom::SetSeed(seed);
  fCount624 = 624;
  Int_t i,j;
  if (seed > 0) {
     fMt[0] = fSeed;
     j = 1;
  } else {
     exit(99);
 /*  TUUID uid;
     UChar_t uuid[16];
     uid.GetUUID(uuid);
     for (i=0;i<8;i++) {
        fMt[i] = uuid[2*i]*256 +uuid[2*i+1];
     }
     j = 9; */
  }
  for(i=j; i<624; i++) {
    fMt[i] = (69069 * fMt[i-1]) & 0xffffffff;
  }
}

Double_t TRandom3::Rndm(Int_t i) {
//  Machine independent random number generator.
//  Produces uniformly-distributed floating points in [0,1]
//  Method: Mersenne Twistor

	UInt_t y;

  const Int_t  kM = 397;
  const Int_t  kN = 624;
  const UInt_t kTemperingMaskB =  0x9d2c5680;
  const UInt_t kTemperingMaskC =  0xefc60000;
  const UInt_t kUpperMask =       0x80000000;
  const UInt_t kLowerMask =       0x7fffffff;
  const UInt_t kMatrixA =         0x9908b0df;

  if (fCount624 >= kN) {
    register Int_t i;

    for (i=0; i < kN-kM; i++) {
      y = (fMt[i] & kUpperMask) | (fMt[i+1] & kLowerMask);
      fMt[i] = fMt[i+kM] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
    }

    for (   ; i < kN-1    ; i++) {
      y = (fMt[i] & kUpperMask) | (fMt[i+1] & kLowerMask);
      fMt[i] = fMt[i+kM-kN] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
    }

    y = (fMt[kN-1] & kUpperMask) | (fMt[0] & kLowerMask);
    fMt[kN-1] = fMt[kM-1] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
    fCount624 = 0;
  }

  y = fMt[fCount624++];
  y ^=  (y >> 11);
  y ^= ((y << 7 ) & kTemperingMaskB );
  y ^= ((y << 15) & kTemperingMaskC );
  y ^=  (y >> 18);

  if (y) return ( (Double_t) y * 2.3283064365386963e-10); // * Power(2,-32)
  return Rndm();
}

//______________________________________________________________________________
void TRandom3::RndmArray(Int_t n, Double_t *array)
{
  // Return an array of n random numbers uniformly distributed in ]0,1]
   
   Int_t k = 0;
  
   UInt_t y;

   const Int_t  kM = 397;
   const Int_t  kN = 624;
   const UInt_t kTemperingMaskB =  0x9d2c5680;
   const UInt_t kTemperingMaskC =  0xefc60000;
   const UInt_t kUpperMask =       0x80000000;
   const UInt_t kLowerMask =       0x7fffffff;
   const UInt_t kMatrixA =         0x9908b0df;

   while (k < n) {
      if (fCount624 >= kN) {
         register Int_t i;

         for (i=0; i < kN-kM; i++) {
            y = (fMt[i] & kUpperMask) | (fMt[i+1] & kLowerMask);
            fMt[i] = fMt[i+kM] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
         }

         for (   ; i < kN-1    ; i++) {
            y = (fMt[i] & kUpperMask) | (fMt[i+1] & kLowerMask);
            fMt[i] = fMt[i+kM-kN] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
         }

         y = (fMt[kN-1] & kUpperMask) | (fMt[0] & kLowerMask);
         fMt[kN-1] = fMt[kM-1] ^ (y >> 1) ^ ((y & 0x1) ? kMatrixA : 0x0);
         fCount624 = 0;
      }

      y = fMt[fCount624++];
      y ^=  (y >> 11);
      y ^= ((y << 7 ) & kTemperingMaskB );
      y ^= ((y << 15) & kTemperingMaskC );
      y ^=  (y >> 18);

      if (y) {
         array[k] = Double_t( y * 2.3283064365386963e-10); // * Power(2,-32)
         k++;
      }
   }
}

