
// J.M. Dana
// dana@ace.ual.es
#include "G4Types.hh"
#include "G4SIunits.hh"

#include "timing.h"
#include "geomdefs.hh"

enum EInside {kOutside,kSurface,kInside};

const double pi = 3.14159265358979323846;
const double twopi=2*pi;

#include <iostream>
#include <math.h>
#include "Hep3Vector.h"
typedef Hep3Vector G4ThreeVector;

#include "G4String.hh"
#include "G4Tubs.hh"

using namespace std;

#define	N	2100000000

int main(int argc, char **argv) {
	int i;
	Timer *time=new Timer();
	G4Tubs *tub=new G4Tubs(*(new G4String("Name")),0.,10500000000.,1049999999.,0.,2*pi);
	G4ThreeVector *vector=new G4ThreeVector();
	int result;
	int zero=0;
	int one=0;
	int two=0;

        G4double myval = 0.0;
	G4double k = -1.0;
	time->start();

	for(i=0;i<N;i++) {
	  k++;
		vector->set(k,k,k);
		result=tub->Inside(*vector);

		if(result==kOutside)
			zero++;
		else if(result==kSurface)
			one++;
		else
			two++;

	}

	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;

	//cout << endl << "ZERO's: " << zero << '\t' << "ONE's: " << one << '\t' << "TWO's: " << two << endl;

	if ((zero != two) || one != 0 ){
	  cout <<"testG4Tubs -- FAILURE   (zeroes and twos should match, ones should be 0)";
	  printf("ZERO\'s:%d   ONE\'s:%d   TWO\'s:%d\n", zero, one, two);
	}
	else
	  cout <<"testG4Tubs -- OK" << endl;
	return 0;
}
