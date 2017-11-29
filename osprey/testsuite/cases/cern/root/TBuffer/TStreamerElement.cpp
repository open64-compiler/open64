// @(#)root/meta:$Name:  $:$Id: TStreamerElement.cxx,v 1.82 2005/08/25 08:25:25 brun Exp $
// Author: Rene Brun   12/10/2000

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


//#include "TROOT.h"
#include "TStreamerElement.h"
/*#include "TStreamerInfo.h"
#include "TClass.h"
#include "TClassEdit.h"
#include "TBaseClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "TMethodCall.h"
#include "TRealData.h"
#include "TFolder.h"
#include "TRef.h"
#include "Api.h"
#include "TInterpreter.h"
*/
#include <string>
namespace std {} using namespace std;

const Int_t kMaxLen = 1024;
static char gIncludeName[kMaxLen];


//______________________________________________________________________________
TStreamerElement::TStreamerElement()
{
   // Default ctor.

   fType        = 0;
   fSize        = 0;
   fNewType     = 0;
   fArrayDim    = 0;
   fArrayLength = 0;
   //fStreamer    = 0;
   //fMethod      = 0;
   fOffset      = 0;
   //fClassObject = (TClass*)(-1);
   fTObjectOffset = 0;
   fFactor      = 0;
   fXmin        = 0;
   fXmax        = 0;
   for (Int_t i=0;i<5;i++) fMaxIndex[i] = 0;
};

