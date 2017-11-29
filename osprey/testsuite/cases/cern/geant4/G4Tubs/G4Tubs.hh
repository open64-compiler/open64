class G4Tubs //: public G4CSGSolid
{
  public:  // with description

    G4Tubs( const G4String& pName,
                  G4double pRMin,
                  G4double pRMax,
                  G4double pDz,
                  G4double pSPhi,
                  G4double pDPhi );
      //~ //
      //~ // Constructs a tubs with the given name and dimensions

     //~ virtual ~G4Tubs();
      //~ //
      //~ // Destructor

    //~ // Accessors
    
    //~ inline G4double GetInnerRadius   () const;
    //~ inline G4double GetOuterRadius   () const;
    //~ inline G4double GetZHalfLength   () const;
    //~ inline G4double GetStartPhiAngle () const;
    //~ inline G4double GetDeltaPhiAngle () const;
    //~ inline G4double GetCubicVolume();


    //~ // Modifiers

    //~ inline void SetInnerRadius   (G4double newRMin);
    //~ inline void SetOuterRadius   (G4double newRMax);
    //~ inline void SetZHalfLength   (G4double newDz);
    //~ inline void SetStartPhiAngle (G4double newSPhi);
    //~ inline void SetDeltaPhiAngle (G4double newDPhi);

    //~ // Methods for solid

    //~ void ComputeDimensions(       G4VPVParameterisation* p,
                            //~ const G4int n,
                            //~ const G4VPhysicalVolume* pRep );

    //~ G4bool CalculateExtent( const EAxis pAxis,
                            //~ const G4VoxelLimits& pVoxelLimit,
                            //~ const G4AffineTransform& pTransform,
                                  //~ G4double& pmin, G4double& pmax ) const;

    EInside Inside( const G4ThreeVector& p ) const;

    //~ G4ThreeVector SurfaceNormal( const G4ThreeVector& p ) const;

    //~ G4double DistanceToIn(const G4ThreeVector& p, const G4ThreeVector& v) const;
    //~ G4double DistanceToIn(const G4ThreeVector& p) const;
    //~ G4double DistanceToOut(const G4ThreeVector& p, const G4ThreeVector& v,
                           //~ const G4bool calcNorm=G4bool(false),
                                 //~ G4bool *validNorm=0, G4ThreeVector *n=0) const;
    //~ G4double DistanceToOut(const G4ThreeVector& p) const;

    //~ G4GeometryType GetEntityType() const;

    //~ std::ostream& StreamInfo( std::ostream& os ) const;

    //~ // Visualisation functions

    //~ void                DescribeYourselfTo ( G4VGraphicsScene& scene ) const;
    //~ G4Polyhedron*       CreatePolyhedron   () const;
    //~ G4NURBS*            CreateNURBS        () const;

  public:  // without description

    //~ //  Older names for access functions

    //~ inline G4double GetRMin() const;
    //~ inline G4double GetRMax() const;
    //~ inline G4double GetDz  () const;
    //~ inline G4double GetSPhi() const;
    //~ inline G4double GetDPhi() const;

  protected:

    //~ G4ThreeVectorList*
    //~ CreateRotatedVertices( const G4AffineTransform& pTransform ) const;
      //~ //
      //~ // Creates the List of transformed vertices in the format required
      //~ // for G4VSolid:: ClipCrossSection and ClipBetweenSections

    G4double fRMin,fRMax,fDz,fSPhi,fDPhi;

    //~ // Used by distanceToOut

    enum ESide {kNull,kRMin,kRMax,kSPhi,kEPhi,kPZ,kMZ};

    //~ // used by normal

    enum ENorm {kNRMin,kNRMax,kNSPhi,kNEPhi,kNZ};

  private:

    //~ G4ThreeVector ApproxSurfaceNormal( const G4ThreeVector& p ) const;
      //~ // Algorithm for SurfaceNormal() following the original
      //~ // specification for points not on the surface
};

