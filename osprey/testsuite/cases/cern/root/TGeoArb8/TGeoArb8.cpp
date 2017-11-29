#include <iostream>

#include "Rtypes.h"
#include "TMath.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoArb8.h"

//_____________________________________________________________________________
void TGeoArb8::ComputeBBox()
{
// compute bounding box for a Arb8
   Double_t xmin, xmax, ymin, ymax;
   xmin = xmax = fXY[0][0];
   ymin = ymax = fXY[0][1];
   
   for (Int_t i=1; i<8; i++) {
      if (xmin>fXY[i][0]) xmin=fXY[i][0];
      if (xmax<fXY[i][0]) xmax=fXY[i][0];
      if (ymin>fXY[i][1]) ymin=fXY[i][1];
      if (ymax<fXY[i][1]) ymax=fXY[i][1];
   }
   fDX = 0.5*(xmax-xmin);
   fDY = 0.5*(ymax-ymin);
   fDZ = fDz;
   fOrigin[0] = 0.5*(xmax+xmin);
   fOrigin[1] = 0.5*(ymax+ymin);
   fOrigin[2] = 0;
   SetShapeBit(kGeoClosedShape);
}   

//_____________________________________________________________________________
void TGeoArb8::ComputeTwist()
{
// compute tangents of twist angles (angles between projections on XY plane
// of corresponding -dz +dz edges). Called after last point [7] was set.
   Double_t twist[4];
   Bool_t twisted = kFALSE;
   Double_t dx1, dy1, dx2, dy2;
   for (Int_t i=0; i<4; i++) {
      dx1 = fXY[(i+1)%4][0]-fXY[i][0];
      dy1 = fXY[(i+1)%4][1]-fXY[i][1];
      if (dx1==0 && dy1==0) {
         twist[i] = 0;
         continue;
      }   
      dx2 = fXY[4+(i+1)%4][0]-fXY[4+i][0];
      dy2 = fXY[4+(i+1)%4][1]-fXY[4+i][1];
      if (dx2==0 && dy2==0) {
         twist[i] = 0;
         continue;
      }
      twist[i] = dy1*dx2 - dx1*dy2;
      if (TMath::Abs(twist[i])<1E-3) {
         twist[i] = 0;
         continue;
      }
      twist[i] = TMath::Sign(1.,twist[i]);
      twisted = kTRUE;
   }
   if (!twisted) return;
   if (fTwist) delete [] fTwist;
   fTwist = new Double_t[4];
   memcpy(fTwist, &twist[0], 4*sizeof(Double_t));
}


//_____________________________________________________________________________
TGeoArb8::TGeoArb8(Double_t dz, Double_t *vertices)
         :TGeoBBox(0,0,0)
{
// constructor. If the array of vertices is not null, this should be
// in the format : (x0, y0, x1, y1, ... , x7, y7) 
   fDz = dz;
   fTwist = 0;
   SetShapeBit(kGeoArb8); 
   if (vertices) {
      for (Int_t i=0; i<8; i++) {
         fXY[i][0] = vertices[2*i];
         fXY[i][1] = vertices[2*i+1];
      }
      ComputeTwist();
      ComputeBBox();
   } else {
      for (Int_t i=0; i<8; i++) {
         fXY[i][0] = 0.0;
         fXY[i][1] = 0.0;
      }   
   }
}

//_____________________________________________________________________________

Bool_t TGeoArb8::Contains(Double_t *point) const
{
// test if point is inside this sphere
   // first check Z range
   if (TMath::Abs(point[2]) > fDz) return kFALSE;
   // compute intersection between Z plane containing point and the arb8
   Double_t poly[8];
#ifdef ALLUNROLLED
   Double_t x1,y1,x2,y2,cross,x,y;
#endif
//   memset(&poly[0], 0, 8*sizeof(Double_t));
   //SetPlaneVertices(point[2], &poly[0]);
   Double_t cf = 0.5*(fDz-point[2])/fDz;
   Int_t i;
#ifndef ALLUNROLLED
   for (i=0; i<4; i++) {
      poly[2*i]   = fXY[i+4][0]+cf*(fXY[i][0]-fXY[i+4][0]);
      poly[2*i+1] = fXY[i+4][1]+cf*(fXY[i][1]-fXY[i+4][1]);
   }
   return InsidePolygon(point[0],point[1],poly);
#else
   x=point[0]; y=point[1];
   poly[0] = fXY[4][0]  +cf*(fXY[0][0]-fXY[4][0]); 
   poly[1] = fXY[4][1]  +cf*(fXY[0][1]-fXY[4][1]); 
   poly[2] = fXY[5][0]  +cf*(fXY[1][0]-fXY[5][0]); 
   poly[3] = fXY[5][1]  +cf*(fXY[1][1]-fXY[5][1]); 
   poly[4] = fXY[6][0]  +cf*(fXY[2][0]-fXY[6][0]); 
   poly[5] = fXY[6][1]  +cf*(fXY[2][1]-fXY[6][1]); 
   poly[6] = fXY[7][0]  +cf*(fXY[3][0]-fXY[7][0]); 
   poly[7] = fXY[7][1]  +cf*(fXY[3][1]-fXY[7][1]); 
   x1 = poly[0]; y1 = poly[1]; x2 = poly[2]; y2 = poly[3];
   cross = (x-x1)*(y2-y1) - (y-y1)*(x2-x1);
   if (cross<0) return kFALSE;
   x1 = poly[2]; y1 = poly[3]; x2 = poly[4]; y2 = poly[5];
   cross = (x-x1)*(y2-y1) - (y-y1)*(x2-x1);
   if (cross<0) return kFALSE;
   x1 = poly[4]; y1 = poly[5]; x2 = poly[6]; y2 = poly[7];
   cross = (x-x1)*(y2-y1) - (y-y1)*(x2-x1);
   if (cross<0) return kFALSE;
   x1 = poly[6]; y1 = poly[7]; x2 = poly[0]; y2 = poly[1];
   cross = (x-x1)*(y2-y1) - (y-y1)*(x2-x1);
   if (cross<0) return kFALSE;
   return kTRUE;
#endif
}

//_____________________________________________________________________________
Bool_t TGeoArb8::InsidePolygon(Double_t x, Double_t y, Double_t *pts)
{
// Find if a point in XY plane is inside the polygon defines by PTS.
   Int_t i,j;
   Double_t x1,y1,x2,y2;
   Double_t cross;
   for (i=0; i<4; i++) {
      j = (i+1)%4;
      x1 = pts[i<<1];
      y1 = pts[(i<<1)+1];
      x2 = pts[j<<1];
      y2 = pts[(j<<1)+1];
      cross = (x-x1)*(y2-y1)-(y-y1)*(x2-x1);
      if (cross<0) return kFALSE;
   }
   return kTRUE;   
}

