
// J.M. Dana
// dana@ace.ual.es

//#include <stdio.h> // Compatibility with
//#include <errno.h> // gcc 2.96 
#include <iostream>
#include <sys/time.h>
#include <unistd.h>

#include "timing.h"
#include "Rtypes.h"
#include "TRandom.h"
#include "TRandom3.h"

using namespace std;

#define N	2100000000
#define M	10
#define NN 	N/M

int main (int argc, char **argv) {
	Timer *time=new Timer();
	TRandom3 *rand3=new TRandom3();
	Double_t sum=0.0, epsilon = 0.00001 ,rarray[M];
	
	//cout << "Compilation date: "<< __DATE__ << "," << __TIME__ << endl;;
	
	time->start();

#ifndef RNDMARRAY
	for (int i=0; i<N; i++) sum+= rand3->Rndm();
#else
	for (int i=0; i<NN; i++) { 
                 rand3->RndmArray(M,rarray);
		 for (int j=0; j<M; j++) sum += rarray[j];
                 }
#endif

	time->getTime();
	cout.precision(5);
	cout << "time = " << time->getwTime() << endl;
	
	sum /= N;
	if (sum > 0.5+ epsilon || sum < 0.5 - epsilon){
	  cout << "testTRandom3 -- FAILURE   (result should be " << epsilon << " within 0.5)" << endl;
	  cout << "sum = " << sum << endl;
	}
	else
	  cout << "testTRandom3 -- OK. " << N << " iterations" 
#ifdef RNDMARRAY
          << " with RndmArray."
#endif
          << endl;
	
	return 0;
}
