
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
#include "TRandom.h"

using namespace std;

#define N	50000000

int main (int argc, char **argv) {
	TRandom *rand=new TRandom();
	Timer *time=new Timer();
	Double_t sum=0;
	Double_t epsilon=0.001, valid = 15.443;
	
	//cout << "Compilation date: "<< __DATE__ << "," << __TIME__ << endl;;
	
	time->start();

	for(int i=0;i<N;i++)
		sum+=rand->Landau();
			

	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;
	sum /= N;
	//cout << endl << sum << endl << endl;
	//cout << "epsilon= " << epsilon << endl;
	if ((sum > valid-epsilon) && (sum < valid + epsilon))
	  {
	    cout <<"testTRandom -- OK" << endl;
	  }
	else{
	  printf("testTRandom -- FAILURE   (result sum is not %d within %d)\n", epsilon, valid);
	  cout << "sum = " << sum << endl;
	  cout << "epsilon= " << epsilon << endl;
	}
	
	return 0;
}
