class G4AffineTransform
{

public:

  G4AffineTransform();

public: // with description

  //~ G4AffineTransform(const G4ThreeVector &tlate);
    //~ // Translation only: under t'form translate point at origin by tlate

  //~ G4AffineTransform(const G4RotationMatrix &rot);
    //~ // Rotation only: under t'form rotate by rot

  //~ G4AffineTransform(const G4RotationMatrix &rot,
                    //~ const G4ThreeVector &tlate);
    //~ // Under t'form: rotate by rot then translate by tlate

  //~ G4AffineTransform(const G4RotationMatrix *rot,
                    //~ const G4ThreeVector &tlate);
    //~ // Optionally rotate by *rot then translate by tlate - rot may be null
  
  //~ G4AffineTransform operator * (const G4AffineTransform &tf) const;
    //~ // Compound Transforms:
    //~ //       tf2=tf2*tf1 equivalent to tf2*=tf1
    //~ //       Returns compound transformation of self*tf

  //~ G4AffineTransform& operator *= (const G4AffineTransform &tf);
    //~ // (Modifying) Multiplies self by tf; Returns self reference
    //~ //             ie. A=AB for a*=b


  //~ G4AffineTransform& Product(const G4AffineTransform &tf1,
                             //~ const G4AffineTransform &tf2);
    //~ // 'Products' for avoiding (potential) temporaries:
    //~ //            c.Product(a,b) equivalent to c=a*b
    //~ //            c.InverseProduct(a*b,b ) equivalent to c=a
    //~ // (Modifying) Sets self=tf1*tf2; Returns self reference

  G4AffineTransform& InverseProduct(const G4AffineTransform &tf1,
                                    const G4AffineTransform &tf2);
    //~ // (Modifying) Sets self=tf1*(tf2^-1); Returns self reference

  //~ G4ThreeVector TransformPoint(const G4ThreeVector &vec) const;
    //~ // Transform the specified point: returns vec*rot+tlate

  //~ G4ThreeVector TransformAxis(const G4ThreeVector &axis) const;
    //~ // Transform the specified axis: returns

  //~ void ApplyPointTransform(G4ThreeVector &vec) const;
    //~ // Transform the specified point (in place): sets vec=vec*rot+tlate

  //~ void ApplyAxisTransform(G4ThreeVector &axis) const;
    //~ // Transform the specified axis (in place): sets axis=axis*rot;

  //~ G4AffineTransform Inverse() const;
    //~ // Return inverse of current transform

  //~ G4AffineTransform& Invert();
    //~ // (Modifying) Sets self=inverse of self; Returns self reference

  //~ G4AffineTransform& operator +=(const G4ThreeVector &tlate);
  //~ G4AffineTransform& operator -=(const G4ThreeVector &tlate);
    //~ // (Modifying) Adjust net translation by given vector;
    //~ //             Returns self reference

  //~ G4bool operator == (const G4AffineTransform &tf) const;
  //~ G4bool operator != (const G4AffineTransform &tf) const;

  //~ G4double operator [] (const G4int n) const;

  //~ G4bool IsRotated() const;
    //~ // True if transform includes rotation

  //~ G4bool IsTranslated() const;
    //~ // True if transform includes translation

  //~ G4RotationMatrix NetRotation() const;

  //~ G4ThreeVector NetTranslation() const;

  //~ void SetNetRotation(const G4RotationMatrix &rot);

  //~ void SetNetTranslation(const G4ThreeVector &tlate);

 private:

  //~ G4AffineTransform(const G4double prxx,const G4double prxy,const G4double prxz,
                    //~ const G4double pryx,const G4double pryy,const G4double pryz,
                    //~ const G4double przx,const G4double przy,const G4double przz,
                    //~ const G4double ptx, const G4double pty, const G4double ptz );

 public:
  G4double rxx,rxy,rxz;
  G4double ryx,ryy,ryz;
  G4double rzx,rzy,rzz;
  G4double tx,ty,tz;
};

#include "G4AffineTransform.icc"
