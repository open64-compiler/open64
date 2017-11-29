// @(#)root/base:$Name:  $:$Id: TBuffer.h,v 1.49 2005/08/15 21:20:41 pcanal Exp $
// Author: Fons Rademakers   04/05/96

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//#ifndef ROOT_TBuffer
//#define ROOT_TBuffer


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TBuffer                                                              //
//                                                                      //
// Buffer base class used for serializing objects.                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

/*#ifndef ROOT_TObject
#include "TObject.h"
#endif
#ifndef ROOT_Bytes
#include "Bytes.h"
#endif
*/
#include <vector>
#include "Rtypes.h"
#include "Bytes.h"
#ifdef R__OLDHPACC
namespace std {
   using ::string;
   using ::vector;
}
#endif

/*class TStreamerInfo;
class TStreamerElement;
class TClass;
class TExMap;
*/
class TStreamerElement;

class TBuffer // : public TObject 
{

protected:
//   typedef std::vector<TStreamerInfo*> InfoList_t;

   Bool_t          fMode;          //Read or write mode
   Int_t           fVersion;       //Buffer format version
   Int_t           fBufSize;       //Size of buffer
   char           *fBuffer;        //Buffer used to store objects
   char           *fBufCur;        //Current position in buffer
   char           *fBufMax;        //End of buffer
   Int_t           fMapCount;      //Number of objects or classes in map
   Int_t           fMapSize;       //Default size of map
   Int_t           fDisplacement;  //Value to be added to the map offsets
 //  TExMap         *fMap;           //Map containing object,offset pairs for reading/writing
  // TExMap         *fClassMap;      //Map containing object,class pairs for reading
  // TObject        *fParent;        //Pointer to the buffer parent (file) where buffer is read/written
  // TStreamerInfo  *fInfo;          //Pointer to TStreamerInfo object writing/reading the buffer
  // InfoList_t      fInfos;         //Stack of pointers to the TStreamerInfos

   static Int_t fgMapSize; //Default map size for all TBuffer objects

   // Default ctor
 /*  TBuffer() : TObject(), fMode(0), fVersion(0), fBufSize(0), fBuffer(0),
               fBufCur(0), fBufMax(0), fMapCount(0), fMapSize(0),
               fDisplacement(0), fMap(0), fClassMap(0), fParent(0),
               fInfo(0),  fInfos() {}
*/
 
    TBuffer() :fMode(0), fVersion(0), fBufSize(0), fBuffer(0),fBufCur(0), fBufMax(0),fDisplacement(0){}



   // TBuffer objects cannot be copied or assigned
   TBuffer(const TBuffer &);           // not implemented
   void operator=(const TBuffer &);    // not implemented
   void Expand(Int_t newsize);  // expand buffer to newsize
  
 public:
   enum EMode { kRead = 0, kWrite = 1 };
   enum { kInitialSize = 1024, kMinimalSize = 128 };
   enum { kMapSize = 503 };
   enum { kStreamedMemberWise = BIT(14) }; //added to version number to know if a collection has been stored member-wise
   enum { kNotDecompressed = BIT(15) }; //indicates a weird buffer, used by TBasket
   enum { kIsOwner = BIT(16) };  //if set TBuffer owns fBuffer
   enum { kUser1 = BIT(21), kUser2 = BIT(22), kUser3 = BIT(23)}; //free for user

   
   static char          *ReAllocChar(char *vp, size_t size, size_t oldsize);
   TBuffer(EMode mode);
   TBuffer(EMode mode, Int_t bufsiz, void *buf);//   TBuffer(EMode mode, Int_t bufsiz);
//   virtual   void     ReadDouble32 (Double_t *d, TStreamerElement *ele=0);
//   virtual   void     WriteDouble32(Double_t *d, TStreamerElement *ele=0);
   // void PrintBuffer(Int_t n); //prints out the stuff in the buffer
   char    *Buffer() const { return fBuffer; }
   virtual   void     ReadFastArrayDouble32(Double_t  *d, Int_t n, TStreamerElement *ele=0);
    virtual   void     WriteFastArrayDouble32(const Double_t  *d, Int_t n, TStreamerElement *ele=0);
  Int_t    Length() const { return (Int_t)(fBufCur - fBuffer); }
 /* 
   virtual   TBuffer  &operator>>(Bool_t    &b);
   virtual   TBuffer  &operator>>(Char_t    &c);
   virtual   TBuffer  &operator>>(UChar_t   &c);
   virtual   TBuffer  &operator>>(Short_t   &h);
   virtual   TBuffer  &operator>>(UShort_t  &h);*/
   virtual   TBuffer  &operator>>(Int_t     &i);
   virtual   TBuffer  &operator>>(UInt_t    &i);/*
   virtual   TBuffer  &operator>>(Long_t    &l);
   virtual   TBuffer  &operator>>(ULong_t   &l);
   virtual   TBuffer  &operator>>(Long64_t  &l);
   virtual   TBuffer  &operator>>(ULong64_t &l);
   virtual   TBuffer  &operator>>(Float_t   &f);
   virtual   TBuffer  &operator>>(Double_t  &d);
   virtual   TBuffer  &operator>>(Char_t    *c);

   virtual   TBuffer  &operator<<(Bool_t    b);
   virtual   TBuffer  &operator<<(Char_t    c);
   virtual   TBuffer  &operator<<(UChar_t   c);
   virtual   TBuffer  &operator<<(Short_t   h);
   virtual   TBuffer  &operator<<(UShort_t  h);*/
   virtual   TBuffer  &operator<<(Int_t     i);
   virtual   TBuffer  &operator<<(UInt_t    i);/*
   virtual   TBuffer  &operator<<(Long_t    l);
   virtual   TBuffer  &operator<<(ULong_t   l);
   virtual   TBuffer  &operator<<(Long64_t  l);
   virtual   TBuffer  &operator<<(ULong64_t l);
   virtual   TBuffer  &operator<<(Float_t   f);
   virtual   TBuffer  &operator<<(Double_t  d);
   virtual   TBuffer  &operator<<(const Char_t *c);
*/
   //friend TBuffer  &operator>>(TBuffer &b, TObject *&obj);
   //friend TBuffer  &operator>>(TBuffer &b, const TObject *&obj);
   //friend TBuffer  &operator<<(TBuffer &b, const TObject *obj);

 };
/*
//---------------------- TBuffer inlines ---------------------------------------

//______________________________________________________________________________
*/inline TBuffer &TBuffer::operator<<(Int_t i)
{
   if (fBufCur + sizeof(Int_t) > fBufMax) Expand(2*fBufSize);

   tobuf(fBufCur, i);
   return *this;
}


inline TBuffer &TBuffer::operator>>(Int_t &i)
{
   frombuf(fBufCur, &i);
   return *this;
}
inline TBuffer &TBuffer::operator<<(UInt_t i)
   { return TBuffer::operator<<((Int_t)i); }
//______________________________________________________________________________
inline TBuffer &TBuffer::operator>>(UInt_t &i)
   { return TBuffer::operator>>((Int_t&)i); }
#if defined(R__TEMPLATE_OVERLOAD_BUG)
template <>
#endif

