
// J.M. Dana
// dana@ace.ual.es
#define N 600000000

#include <iostream>
#include <math.h>
#include <vector>
#include <memory>
#include "timing.h"
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include "Rotation.h"

using namespace CLHEP;

int main(int argc, char **argv) {
	int i;
	double e = .0001;
	bool pass=true;
	Timer *time=new Timer();
	HepRotation *rot=new HepRotation();
	
	//srand48(clock()+getpid());
	
	//Assigned set numbers to enable testing for correctness
	rot->rxx=0.60114;
	rot->rxy=0.709848;
	rot->rxz=0.343257;
	rot->ryx=0.79411;
	rot->ryy=0.335087;
	rot->ryz=0.434594;
	rot->rzx=0.0872391;
	rot->rzy=0.761866;
	rot->rzz=0.414663;
	
        double yy = 0.5/(double)N/3.1415;
	double k = -1.0;
	time->start();
	
	for(i=0;i<N;i++) {
	  k++;
	  double z = k * yy;
	  rot->rotateX(z);
#ifdef ALL3
	  rot->rotateY(z);		
	  rot->rotateZ(z);		
#endif
	}		
	
	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;

	/*cout << "rot->rxx= " << rot->rxx << endl;
	cout << "rot->rxy= " << rot->rxy << endl;
	cout << "rot->rxz= " << rot->rxz << endl;
	cout << "rot->ryx= " << rot->ryx << endl;
	cout << "rot->ryy= " << rot->ryy << endl;
	cout << "rot->ryz= " << rot->ryz << endl;
	cout << "rot->rzx= " << rot->rzx << endl;
	cout << "rot->rzy= " << rot->rzy << endl;
	cout << "rot->rzz= " << rot->rzz << endl;*/
	
	
	if (abs(rot->rxx-0.60114) > e){
	  cout << "testRotation -- FAILURE  (rot->rxx should be 0.60114)" << endl;
	  pass=false;}
	if (abs(rot->rxy-0.70985) > e){
	  cout << "testRotation -- FAILURE  (rot->rxy should be 0.70985)" << endl;
	  pass=false;}
	if (abs(rot->rxz-0.34326) > e){
	  cout << "testRotation -- FAILURE  (rot->rxz should be 0.34326)" << endl;
	  pass=false;}
	if (abs(rot->ryx+0.26362) > e){
	  cout << "testRotation -- FAILURE  (rot->ryx should be -0.26362)" << endl;
	  pass=false;}
	if (abs(rot->ryy-0.75812) > e){
	  cout << "testRotation -- FAILURE  (rot->ryy should be 0.75812)" << endl;
	  pass=false;}
	if (abs(rot->ryz-0.60039) > e){
	  cout << "testRotation -- FAILURE  (rot->ryz should be 0.60039)" << endl;
	  pass=false;}
	if (abs(rot->rzx+0.46523) > e){
	  cout << "testRotation -- FAILURE  (rot->rzx should be -0.46523)" << endl;
	  pass=false;}
	if (abs(rot->rzy-0.15514) > e){
	  cout << "testRotation -- FAILURE  (rot->rzy should be 0.15514)" << endl;
	  pass=false;}
	if (abs(rot->rzz-0.33019) > e){
	  cout << "testRotation -- FAILURE  (rot->rzz should be 0.33019)" << endl;
	  pass=false;}
	if(pass)
	  cout <<"testRotation -- OK" << endl;
	
	
	return 0;
}
