// J.M. Dana
// dana@ace.ual.es

typedef double G4double;

#include <iostream>
#include <unistd.h>

#include "timing.h"
#include "G4AffineTransform.hh"

using namespace std;

#define N       1000000000

int main(int argc, char **argv) {
        int i;
        Timer *time=new Timer();
        G4AffineTransform *trans=new G4AffineTransform();
        G4AffineTransform *tf1=new G4AffineTransform();
        G4AffineTransform *tf2=new G4AffineTransform();
        G4double epsi = 0.000001;
	//double e = 0.00000001;
	bool pass = true;

        // fill_matrix(tf1);
        // fill_matrix(tf2);

        time->start();

        for(i=0;i<N;i++) {
          trans->InverseProduct(*tf1,*tf2);
          *tf1 = *trans;
        }

        time->getTime();
        cout.precision(5);
        cout << endl << "time = " << time->getwTime() << endl;

        //print_matrix(trans);

	
      	if (trans->rxx != 1.0)
	  {
	  cout << "testG4AffineTransform -- FAILURE   (trans->rxx != 1)" << endl;
	  pass = false;
	  }
      	if (trans->rxy != 0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->rxy != 0)" << endl;
	  pass = false;
	}
	if (trans->rxz != 0)
	  {
	  cout << "testG4AffineTransform -- FAILURE   (trans->rxz != 0)" << endl;
	  pass = false;
	}
	if (trans->ryx != 0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->ryx != 0)" << endl;
	  pass = false;
	}
	if (trans->ryy != 1.0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->ryy != 1)" << endl;
	  pass = false;
	  }
	if (trans->ryz != 0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->ryz != 0)" << endl;
	  pass = false;
	}
	if (trans->rzx != 0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->rzx != 0)" << endl;
	  pass = false;
	}
	if (trans->rzy != 0){
	  cout << "testG4AffineTransform -- FAILURE   (trans->rzy != 0)" << endl;
	  pass = false;
	}
	if (trans->rzz != 1.0)
	{
	  cout << "testG4AffineTransform -- FAILURE   (trans->rzz != 1)" << endl;
	  pass = false;
	}
	if (pass)
	  cout <<"testG4AffineTransform -- OK" << endl;

        return 0;
}

