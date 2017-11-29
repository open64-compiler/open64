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
#include "TGeoCone.h"

using namespace std;

#define N	1500000000

int main (int argc, char **argv) {
	Timer *time=new Timer();
	//if you want 50/50, set the args to sqrt(2)*N/2  for the 1st,3rd and 5th parameters
	TGeoCone *cone=new TGeoCone(1060660171.0,0.0,1060660171.0,0.0,1060660171.0);
	Int_t yes=0;
	Int_t no=0;
	Double_t d, point[3];
	
	//cout << "Compilation date: "<< __DATE__ << "," << __TIME__ << endl;;
	
	time->start();

	d = -1.0;
	for(int i=0;i<N;i++) {
                d++;
		point[0]=d;
		point[1]=d;
		point[2]=d;
		
		if(cone->Contains(point))
			yes++;
		else
			no++;
	}
		
	time->getTime();
	cout.precision(5);
	cout << endl << "time = "<<time->getwTime()<<endl;
	
	//cout << endl << "YES: " << yes << endl << "NO: " << no << endl;

	if (yes!=no){
	  cout << "testTGeoCone -- FAILURE   (number of points inside and outside of shape are not equal)" << endl;
	  cout << endl << "YES: " << yes << endl << "NO: " << no << endl;
	}
	else
	  cout <<"testTGeoCone -- OK" << endl;
	
	return 0;
}
