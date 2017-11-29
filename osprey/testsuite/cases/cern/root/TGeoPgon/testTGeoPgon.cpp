#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <vector>
#include <string>

#include "timing.h"
#include "Rtypes.h"
#include "TMath.h"
#include "TGeoPgon.h"
//#include "TGeoPcon.h"


using namespace std;

#define N	600000000

int main (int argc, char **argv) {
	Timer *time=new Timer();
/*************************************************************************
 * TGeoPgon - a polygone. It has at least 10 parameters :
 *            - the lower phi limit;
 *            - the range in phi;
 *            - the number of edges on each z plane;
 *            - the number of z planes (at least two) where the inner/outer
 *              radii are changing;
 *            - z coordinate, inner and outer radius for each z plane
 *
 *************************************************************************/
/**********************************************************************
*Definition of a complite Poligone with 10 sides and 2 z planes
*	TGeoPgon *pgon=new TGeoPgon(0,360.0,10,2);
*	pgon->DefineSection(0,-30.0,0,50);
*      	pgon->DefineSection(1,30.0,0,50);
*************************************************************************/
/****************************************************************************
*Definition of a cylinder with 
*	TGeoPgon *pgon=new TGeoPgon(0,360.0,0,2);
*	pgon->DefineSection(0,-30.0,0,50);
*     	pgon->DefineSection(1,30.0,0,50);
*************************************************************************/
	TGeoPgon *pgon=new TGeoPgon(0.0,360.0,0,2);
	//pgon->DefineSection(0,0.0,10,50);
      	//pgon->DefineSection(1,30.0,10,50);
	pgon->DefineSection(0,-300000000.0,0,424264069);
	pgon->DefineSection(1,300000000.0,0,424264069);


        Int_t yes=0;
	Int_t no=0;
	Double_t d = 0.0, point[3];
 	Double_t fZ, delta = 300./N; 

	time->start();

	for(int i=0;i<N;i++) {
                d++;
		point[0]=d;
		point[1]=d;
		point[2]=d;

               if(pgon->Contains(point)){
			yes++;
                        
		}else
			no++;
	
        }
		

	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;
	
	//cout << "YES: " << yes << endl << "NO: " << no << endl;

	if (yes!=no){
	  cout << "testTGeoPgon -- FAILURE   (number of points inside and outside of shape are not equal)" << endl;
	  cout << "YES: " << yes << endl << "NO: " << no << endl;
	}
	else
	  cout <<"testTGeoPgon -- OK" << endl;

	return 0;
}
