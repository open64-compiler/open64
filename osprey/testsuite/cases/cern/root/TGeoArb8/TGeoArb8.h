class TGeoArb8 : public TGeoBBox
{
protected:
   //~ enum EGeoArb8Type {
//~ //      kArb8Trd1 = BIT(25), // trd1 type
//~ //      kArb8Trd2 = BIT(26), // trd2 type
      //~ kArb8Trap = BIT(27), // planar surface trapezoid
      //~ kArb8Tra  = BIT(28)  // general twisted trapezoid
   //~ };
   //~ // data members
   Double_t              fDz;          // half length in Z
   Double_t             *fTwist;       //! [4] tangents of twist angles 
   Double_t              fXY[8][2];    // list of vertices

public:
   //~ // constructors
   //~ TGeoArb8();
   TGeoArb8(Double_t dz, Double_t *vertices=0);
   //~ TGeoArb8(const char *name, Double_t dz, Double_t *vertices=0);
   //~ // destructor
   //~ virtual ~TGeoArb8();
   //~ // methods
   virtual void          ComputeBBox();
   //~ virtual void          ComputeNormal(Double_t *point, Double_t *dir, Double_t *norm);
   void                  ComputeTwist();
   virtual Bool_t        Contains(Double_t *point) const;     
   //~ Double_t              DistToPlane(Double_t *point, Double_t *dir, Int_t ipl, Bool_t in) const;
   //~ virtual Double_t      DistFromInside(Double_t *point, Double_t *dir, Int_t iact=1, 
                                   //~ Double_t step=TGeoShape::Big(), Double_t *safe=0) const;
   //~ virtual Double_t      DistFromOutside(Double_t *point, Double_t *dir, Int_t iact=1, 
                                   //~ Double_t step=TGeoShape::Big(), Double_t *safe=0) const;
   //~ virtual TGeoVolume   *Divide(TGeoVolume *voldiv, const char *divname, Int_t iaxis, Int_t ndiv, 
                                //~ Double_t start, Double_t step);
   //~ virtual Double_t      GetAxisRange(Int_t iaxis, Double_t &xlo, Double_t &xhi) const;
   //~ virtual void          GetBoundingCylinder(Double_t *param) const;
   //~ virtual Int_t         GetByteCount() const {return 100;}
   //~ Double_t              GetDz() const {return fDz;}
   //~ virtual Int_t         GetFittingBox(const TGeoBBox *parambox, TGeoMatrix *mat, Double_t &dx, Double_t &dy, Double_t &dz) const;
   //~ virtual TGeoShape    *GetMakeRuntimeShape(TGeoShape * /*mother*/, TGeoMatrix * /*mat*/) const {return 0;}
   //~ static void           GetPlaneNormal(Double_t *p1, Double_t *p2, Double_t *p3, Double_t *norm);
   //~ virtual Int_t         GetNmeshVertices() const {return 8;}
   //~ Double_t             *GetVertices() {return &fXY[0][0];}
   //~ Double_t              GetTwist(Int_t iseg) const;
   //~ virtual Bool_t        IsCylType() const {return kFALSE;}
   //~ static Bool_t         IsSamePoint(Double_t *p1, Double_t *p2) {return ((p1[0]==p2[0]) && (p1[1]==p2[1]))?kTRUE:kFALSE;}
   static Bool_t         InsidePolygon(Double_t x, Double_t y, Double_t *pts);
   //~ virtual void          InspectShape() const;
   //~ Bool_t                IsTwisted() const {return (fTwist==0)?kFALSE:kTRUE;}
   //~ Double_t              SafetyToFace(Double_t *point, Int_t iseg, Bool_t in) const;
   //~ virtual Double_t      Safety(Double_t *point, Bool_t in=kTRUE) const;
   //~ virtual void          SavePrimitive(ofstream &out, Option_t *option);
   //~ void                  SetPlaneVertices(Double_t zpl, Double_t *vertices) const;
   //~ virtual void          SetVertex(Int_t vnum, Double_t x, Double_t y);
   //~ virtual void          SetDimensions(Double_t *param);
   //~ virtual void          SetPoints(Double_t *points) const;
   //~ virtual void          SetPoints(Float_t *points) const;
   //~ virtual void          Sizeof3D() const;

  //~ ClassDef(TGeoArb8, 1)         // arbitrary trapezoid with 8 vertices
};

