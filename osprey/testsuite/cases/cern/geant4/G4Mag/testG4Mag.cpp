
// J.M. Dana
// dana@ace.ual.es

#include <iostream>
#include <math.h>
#include "G4Mag_UsualEqRhs.hh"
#include "timing.h"

using namespace std;

#define	N	700000000

int main(int argc, char **argv) {
	int i;
	double epsilon;
	Timer *time=new Timer();
	G4Mag_UsualEqRhs *mag=new G4Mag_UsualEqRhs(NULL);
	G4double y[6]={1000.3,1000.5,1000.2,1000.1,1000.4,1000.6};
	G4double B[3]={1000.9,1000.6,1000.3};
	G4double dydx[6], sum = 0, correction = 1.0000000001;
	
	//cout << mag->FCof() << endl;
	mag->SetChargeMomentumMass(1.0,0.0,0.0);
	//cout << mag->FCof() << endl;
	
	time->start();
	
	for(i=0;i<N;i++) {
		mag->EvaluateRhsGivenB(y,B,dydx);
		
		B[0] *= correction;
		B[1] *= correction;
		B[2] *= correction;
		y[3] *= correction;
		y[4] *= correction;
		y[5] *= correction;

                if (i%1024 == 0) sum += (dydx[0]+dydx[1]+dydx[2]+dydx[3]+dydx[4]+dydx[5]);
			
	}
			
	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << ". sum = " << sum << endl;

	//cout << dydx[0] << '\t' << dydx[1] << '\t' << dydx[2] << '\t' << dydx[3] << '\t' << dydx[4] << '\t' << dydx[5] << endl;
	//test correctedness
	epsilon = 0.0001;
	//cout << "epsilon= " << epsilon << endl;
	if (dydx[0] < 0.5772 + epsilon && dydx[0] > 0.5772 - epsilon)
	  {
	    epsilon = 0.00001;
	    //cout << "epsilon= " << epsilon << endl;
	    if (dydx[1] < .57737+epsilon && dydx[1] > 0.57737 - epsilon)
	    {
	      epsilon = .0001;
	      //cout << "epsilon= " << epsilon << endl;
	      if (dydx[2] < .57748+epsilon && dydx[2] > .57748- epsilon){
		epsilon = .0001E-11;
		//cout << "epsilon= " << epsilon << endl;
		if (dydx[3] < -1.4873E-11 + epsilon && dydx[3] > -1.4873E-11 - epsilon){
		  //epsilon = .0001E-11;
		  //cout << "epsilon= " << epsilon << endl;
		  if (dydx[4] < 3.2719E-11 + epsilon && dydx[4] > 3.2719E-11 - epsilon){
		    //epsilon = .0001E-11;
		    //cout << "epsilon= " << epsilon << endl;
		    if (dydx[5] < -1.7848E-11 + epsilon && dydx[5] > -1.7848E-11 - epsilon){
		      cout << "testG4Mag_EqRhs -- OK" << endl;
		    }
		    else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[5])" << endl;
		  }
		  else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[4])" << endl;
		}
		else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[3])" << endl;
	      }
	      else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[2])" << endl;
	    }
	    else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[1])" << endl;
	  }
	else cout <<"testG4Mag_EqRhs -- FAILURE   (see dydx[0])" << endl;
	
	
	return 0;
}
