
// J.M. Dana
// dana@ace.ual.es

#include <iostream>
#include <math.h>
#include <vector>
#include <memory>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

#include "timing.h"
#include "RandomEngine.h"
#include "RanluxEngine.h"

//using namespace std;
using std::cout;
using std::cerr;
using std::endl;

#define	N	150000000

using namespace CLHEP;

int main(int argc, char **argv) {
	int i;
	Timer *time=new Timer();
	RanluxEngine *eng=new RanluxEngine(123456789,
	LUXLEVEL);
	double sum=0;

	time->start();
	
	for(i=0;i<N;i++) 
		sum+=eng->flat();
		
	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;
       
	sum /= N;
	//cout << sum << endl;
	double k = .0001;
	//cout <<"i = " << k << endl;
        if (sum > 0.5+k || sum < 0.5-k){
	    cout <<"testRanluxEngine -- FAILURE   (sum should be " << k << " within 0.5)" << endl;
	    cout << "sum = " << sum << endl;
	}
	  else
	    cout <<"testRanluxEngine -- OK" << endl;
	return 0;
}
