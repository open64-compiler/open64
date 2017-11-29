//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TBuffer                                                              //
//                                                                      //
// Buffer base class used for serializing objects.                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include <string>
#include <iostream>
//#include "Bytes.h"
//#include "TROOT.h"
//#include "TFile.h"
#include "TBuffer.h"
//#include "TExMap.h"
//#include "TClass.h"
//#include "TStorage.h"
#include "TMath.h"
//#include "TError.h"
//#include "TObjArray.h"
//#include "TStreamer.h"
//#include "TStreamerInfo.h"
#include "TStreamerElement.h"
using namespace std;	

const UInt_t kNullTag           = 0;
const UInt_t kNewClassTag       = 0xFFFFFFFF;
const UInt_t kClassMask         = 0x80000000;  // OR the class index with this
const UInt_t kByteCountMask     = 0x40000000;  // OR the byte count with this
const UInt_t kMaxMapCount       = 0x3FFFFFFE;  // last valid fMapCount and byte count
const Version_t kByteCountVMask = 0x4000;      // OR the version byte count with this
const Version_t kMaxVersion     = 0x3FFF;      // highest possible version number
const Int_t  kExtraSpace        = 8;   // extra space at end of buffer (used for free block count)
const Int_t  kMapOffset         = 2;   // first 2 map entries are taken by null obj and self obj

Int_t TBuffer::fgMapSize   = kMapSize;

//_____________________________________________________________________________
TBuffer::TBuffer(EMode mode) 
  // :fInfo(0), fInfos(10)
{
   // Create an I/O buffer object. Mode should be either TBuffer::kRead or
   // TBuffer::kWrite. By default the I/O buffer has a size of
   // TBuffer::kInitialSize (1024) bytes.
  fBufSize  = kInitialSize;
  fMode     = mode;
  fVersion  = 0;
  fMapCount = 0;
  fMapSize  = fgMapSize;
  //fMap      = 0;
  //fClassMap = 0;
  //fParent   = 0;
  fDisplacement = 0;

  //SetBit(kIsOwner);

  fBuffer = new char[fBufSize+kExtraSpace];

  fBufCur = fBuffer; 
  fBufMax = fBuffer + fBufSize;
  }
//______________________________________________________________________________
 TBuffer::TBuffer(EMode mode, Int_t bufsiz, void *buf)
{
   // Create an I/O buffer object. Mode should be either TBuffer::kRead or
   // TBuffer::kWrite. By default the I/O buffer has a size of
   // TBuffer::kInitialSize (1024) bytes.

   // Before using the buffer make sure some assumptions are true
/*   Assert(sizeof(Short_t) == 2);
   Assert(sizeof(Int_t) == 4);
#ifdef R__B64
   Assert(sizeof(Long_t) == 8);
#else
   Assert(sizeof(Long_t) == 4);
#endif
   Assert(sizeof(Float_t) == 4);
   Assert(sizeof(Double_t) == 8);
*/
   if (bufsiz < kMinimalSize) bufsiz = kMinimalSize;
   fBufSize  = bufsiz;
   fMode     = mode;
   fVersion  = 0;
   fMapCount = 0;
   fMapSize  = fgMapSize;
   //fReadMap  = 0;
   //printf("Tamano de buf es %d\n",(TBuffer *)buf->Length());
//   printf("Tamano de Double32_t es %d",sizeof(Double32_t));
   /*Double32_t *tDouble = (Double32_t *)buf;

   for(int i=0;i<5;i++){
       printf("Dato %d",tDouble[i]);
   }*/
   
   if (buf){
      fBuffer = (char*) buf;
      //PrintBuffer(10);
   }
   else
     fBuffer = new char[fBufSize+kExtraSpace];
   //fBuffer = (char *) malloc (sizeof (Float_t) * (fBufSize + kExtraSpace));
   fBufCur = fBuffer;
   fBufMax = fBuffer + fBufSize;
}

//______________________________________________________________________________
char *TBuffer::ReAllocChar(char *ovp, size_t size, size_t oldsize)
{
   // Reallocate (i.e. resize) array of chars. Size and oldsize are
   // in number of chars.

   // Needs to be protected by global mutex
  // R__LOCKGUARD(gGlobalMutex);

   static const char *where = "TStorage::ReAllocChar";

   char *vp;
   if (ovp == 0) {
     vp = new char[size];
     if (vp == 0)
          printf("Error inside TStorage::ReAllocChar 1 \n");
        //Fatal(where, kSpaceErr);
     return vp;
   }
   if (oldsize == size)
      return ovp;

   vp = new char[size];
   if (vp == 0)
      printf("Error inside TStorage::ReAllocChar 1 \n");
      //Fatal(where, kSpaceErr);
   if (size > oldsize) {
      memcpy(vp, ovp, oldsize);
      memset((char*)vp+oldsize, 0, size-oldsize);
   } else
      memcpy(vp, ovp, size);
   delete [] ovp;
   return vp;
}


//________________________________________________________________________________

void TBuffer::Expand(Int_t newsize)
{
   // Expand the I/O buffer to newsize bytes.
  cout << "Expanding" << endl;
   Int_t l  = Length();
   fBuffer  = ReAllocChar(fBuffer, newsize+kExtraSpace,
                                    fBufSize+kExtraSpace);
   fBufSize = newsize;
   fBufCur  = fBuffer+1;
   fBufMax  = fBuffer + fBufSize;
}

//______________________________________________________________________________

void TBuffer::ReadFastArrayDouble32(Double_t *d, Int_t n, TStreamerElement *ele)
{
   // Read array of n doubles (written as float) from the I/O buffer.
   // see comments about Double32_t encoding at TBuffer::WriteDouble32

   if (n <= 0 || 4*n > fBufSize){
   printf("Read primer if \n");   
   return;
   }

   if (ele && ele->GetFactor() != 0) {
     printf("Read primer if2 \n");   
     Double_t xmin = ele->GetXmin();
     Double_t factor = ele->GetFactor();
     for (int j=0;j < n; j++) {
       printf("Read for elemento %d \n",j);   
       UInt_t aint;
       *this >> aint;
       d[j] = (Double_t)(aint/factor + xmin);
     }
   } else {
     Float_t afloat;
     //printf("Read primer else \n");   
     for (int i = 0; i < n; i++) {
       //printf("Read for q escribe en buffer %d \n",i);   
       frombuf(fBufCur, &afloat);
       // printf("element is %f \n",afloat);   
       d[i]=afloat;
       //printf("afloat= %f\n", afloat);
      }
   }
}

void TBuffer::WriteFastArrayDouble32(const Double_t *d, Int_t n, TStreamerElement *ele)
{
   // Write array of n doubles (as float) into the I/O buffer.
   // see comments about Double32_t encoding at TBuffer::WriteDouble32

   if (n <= 0) return;

   Int_t l = sizeof(Float_t)*n;
   if (fBufCur + l > fBufMax) Expand(TMath::Max(2*fBufSize, fBufSize+l));

   if (ele && ele->GetFactor()) {
     printf("element used....\n");
      Double_t factor = ele->GetFactor();
      Double_t xmin = ele->GetXmin();
      Double_t xmax = ele->GetXmax();
      for (int j = 0; j < n; j++) {
         Double_t x = d[j];
         if (x < xmin) x = xmin;
         if (x > xmax) x = xmax;
         UInt_t aint = UInt_t(0.5+factor*(x-xmin)); *this << aint;
      }
   } else {
     //Float_t afloat;
     //printf("fBuffer = %x \t vect size = %d\n",fBuffer, n);
     for (int i = 0; i < n; i++){
       //tobuf(blah2, Float_t(d[i]));
       tobuf(fBufCur, Float_t(d[i]));
       //memcpy(&afloat, fBufCur, sizeof(Float_t));
       //printf ("afloat = %f\n", afloat);
       //printf ("Float_t(d[%d]) = %f\n", i,d[i]);
       //printf ("blah[%c]= %f\n", i, blah2[i]);
       //frombuf(fBufCur, fBufCur);
       //printf ("fBufCur[%d]= %f\n", i, fBufCur[i]);
     }
   }
}
/*void TBuffer::PrintBuffer(Int_t n)
{
  Float_t afloat;
  char * blah;
  blah = fBuffer;
  for (int i = 0; i < n; i++)
    {
      memcpy(&afloat, blah, sizeof(Float_t));
      printf("printing afloat = %f\n", afloat);
      blah+=sizeof(Float_t);
    }
} */

