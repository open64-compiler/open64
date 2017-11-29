#include "Rtypes.h"
#include "TGeoShape.h"
	 
TGeoShape::TGeoShape(const char *name)
          //:TNamed(name, "")
{
// Default constructor
   fShapeBits = 0;
   fShapeId   = 0;
   //if (!gGeoManager) {
   //   gGeoManager = new TGeoManager("Geometry", "default geometry");
      // // gROOT->AddGeoManager(gGeoManager);
   //}
   //fShapeId = gGeoManager->GetListOfShapes()->GetSize();
   //gGeoManager->AddShape(this);
}

