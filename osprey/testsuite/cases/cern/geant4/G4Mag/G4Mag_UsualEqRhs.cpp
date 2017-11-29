
// J.M. Dana
// dana@ace.ual.es

#include <iostream>
#include <math.h>
//#include "G4MagneticField.hh"
#include "G4Mag_EqRhs.hh"
#include "timing.h"

using namespace std;

#define	N	700000000
//#define	N	10

class G4Mag_UsualEqRhs : public G4Mag_EqRhs
{
   public:  // with description

     G4Mag_UsualEqRhs( G4MagneticField* MagField );
    ~G4Mag_UsualEqRhs();
       //~ // Constructor and destructor. No actions.

     void EvaluateRhsGivenB( const G4double y[],
                             const G4double B[3],
                                   G4double dydx[] ) const;
       //~ // Given the value of the magnetic field B, this function 
       //~ // calculates the value of the derivative dydx.

     //~ virtual void SetChargeMomentumMass( G4double particleCharge, // in e+ units
                                         //~ G4double MomentumXc,
                                         //~ G4double mass);
     
  private:

    //~ G4double  fInvCurrentMomentumXc;   // This extra state enables us 
                                    //~ // to save a square root in a
                                    //~ // critical method.
};

void
G4Mag_UsualEqRhs::EvaluateRhsGivenB(const G4double y[],
			             			const G4double B[3],
				           			G4double dydx[] ) const
{
   G4double momentum_mag_square = y[3]*y[3] + y[4]*y[4] + y[5]*y[5];
   G4double inv_momentum_magnitude = 1.0 / std::sqrt( momentum_mag_square );

   G4double cof = FCof()*inv_momentum_magnitude;

   dydx[0] = y[3]*inv_momentum_magnitude;       //  (d/ds)x = Vx/V
   dydx[1] = y[4]*inv_momentum_magnitude;       //  (d/ds)y = Vy/V
   dydx[2] = y[5]*inv_momentum_magnitude;       //  (d/ds)z = Vz/V

   dydx[3] = cof*(y[4]*B[2] - y[5]*B[1]) ;   // Ax = a*(Vy*Bz - Vz*By)
   dydx[4] = cof*(y[5]*B[0] - y[3]*B[2]) ;   // Ay = a*(Vz*Bx - Vx*Bz)
   dydx[5] = cof*(y[3]*B[1] - y[4]*B[0]) ;   // Az = a*(Vx*By - Vy*Bx)

   return ;
}

G4Mag_UsualEqRhs::G4Mag_UsualEqRhs( G4MagneticField* MagField )
  : G4Mag_EqRhs( MagField ) {}
	  
G4Mag_UsualEqRhs::~G4Mag_UsualEqRhs() {}

