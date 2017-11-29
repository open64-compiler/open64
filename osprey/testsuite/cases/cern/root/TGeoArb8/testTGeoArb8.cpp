
// J.M. Dana
// dana@ace.ual.es

#include "timing.h"
#include "math.h"
#include <iostream>
#include "Rtypes.h"
#include "TMath.h"

#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoArb8.h"

using namespace std;

#define N       500000000

int main(int argc, char **argv) {
    Timer *time=new Timer();
    
    Double_t dz=0.5;
    Double_t vertices[16]={0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0};
    Double_t point[3];
    Int_t trues=0,falses=0;
        
    TGeoArb8 *geo=new TGeoArb8(dz,vertices);
        
    //cout << "Compilation date: "<< __DATE__ << "," << __TIME__ << endl;;
    
    srand48(clock());

    point[0]=0.5;
    point[1]=3/2;
    point[2]=0;
#ifdef ALLINSIDE
    double z = 0.4/(double)N;
#else
    double z = 1.0/(double)N;
#endif
        
    time->start();
    
    for(int i=0;i<N;i++) {
      point[1] -= 2.0 * z;
        if(geo->Contains(point))
            trues++;
	else
            falses++;     			
    }
    
    

    cout.precision(5);
    time->getTime();
    cout << "time = " << time->getwTime()<< endl;        

#ifdef ALLINSIDE
    if (trues!=N){
#else
    if (trues!=falses){
#endif
	  cout << "testTGeoArb8 -- FAILURE   (number of points inside and outside of shape are not equal)" << endl;
	  cout << "True: "<< trues << endl << "False: " << falses << endl << endl;
    }
	else
          cout << "True: "<< trues << endl << "False: " << falses << endl << endl;
	  cout <<"testTGeoArb8 -- OK" << endl;
        
    return 0;
}
