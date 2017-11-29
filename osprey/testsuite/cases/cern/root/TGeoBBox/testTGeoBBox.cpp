
// J.M. Dana
// dana@ace.ual.es

//#include <stdio.h> // Compatibility with
//#include <errno.h> // gcc 2.96 
#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <vector>
#include <string>

#include "timing.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TGeoBBox.h"

using namespace std;

#define N	3500000000

int main (int argc, char **argv) {
	Timer *time=new Timer();
	Double_t *origin= (Double_t*)malloc(sizeof(Double_t)*3);
	TGeoBBox *box=new TGeoBBox(1749999999.0,1749999999.0,1749999999.0, origin);
	Int_t yes=0;
	Int_t no=0;
	Double_t d, point[3]; 
#ifdef ALLINSIDE
        Double_t delta = 0.001;
#else
        Double_t delta = 1.0;
#endif
	
	//cout << "Compilation date: "<< __DATE__ << "," << __TIME__ << endl;
	
	time->start();
	
	d=-delta;
	for(long i=0;i<N;i++) {
                d+=delta;
		point[0]=d;
		point[1]=d;
		point[2]=d;
		
		if(box->Contains(point))
			yes++;
		else
			no++;
	}
		
	time->getTime();
	cout.precision(5);
	cout << endl << "time = "<<time->getwTime()<<endl;
	
	cout << endl << "inside: " << yes << endl << "outside: " << no << endl;

#ifdef ALLINSIDE
        if (yes!=N){
#else
	if (yes!=no){
#endif
	  cout << "testTGeoBBox -- FAILURE   (number of points in&out of shape are not equal)" << endl;
	  cout << endl << "YES: " << yes << endl << "NO: " << no << endl;
	}
	else
	  cout <<"testTGeoBBox -- OK" << endl;
	
	return 0;
}
