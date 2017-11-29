// J.M. Dana
// dana@ace.ual.es

// Debugging function, isn't part of HepMatrix class

#define	N	100000002


#include <iostream>
#include <math.h>
#include <vector>
#include <memory>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

#include "timing.h"
#include "GenMatrix.h"
#include "Matrix.h"

//using namespace std;
using std::cout;
using std::endl;

void print_matrix(HepMatrix *matrix) {
	int i,j;
	for(i=0;i<matrix->num_row();i++) {
		for(j=0;j<matrix->num_col();j++)
			printf("%0f\t", matrix->m[j+matrix->num_col()*i]);
		cout << endl;
	}
	
	cout << endl;
}

// Debugging function, isn't part of HepMatrix class
void fill_matrix(HepMatrix *matrix) {
	int i,j;

	srand48(clock()+getpid());
	
	for(i=0;i<matrix->num_row();i++) 
		for(j=0;j<matrix->num_col();j++)
		  matrix->m[j+matrix->num_col()*i]=drand48();
}

int main(int argc, char **argv) {
  int i, j; //k: j is new
	int ifail;
	double epsilon = 0.001; //k
	Timer *time=new Timer();
	HepMatrix *matrix=new HepMatrix(5,5,0);
	HepMatrix *temp = new HepMatrix(5,5,0); //k
	
	fill_matrix(matrix);
	
	for (i = 0; i < matrix->num_row();i++) //k
	  for (j=0;j<matrix->num_col();j++) //k
	    temp->m[j+matrix->num_col()*i]=matrix->m[j+matrix->num_col()*i]; //k

	time->start();
	
	//print_matrix(matrix);
	
	for(i=0;i<N;i++) 
		matrix->invertHaywood5(ifail);
		
	//print_matrix(matrix);
	
	time->getTime();
	cout.precision(5);
	cout << endl << "time = " << time->getwTime() << endl;

	for (i = 0; i < matrix->num_row();i++)
	  for (j=0;j<matrix->num_col();j++)
	    {
	      if (abs (temp->m[j+matrix->num_col()*i]-matrix->m[j+matrix->num_col()*i]) > epsilon){
		cout << "testMatrixInversion -- FAILURE   (matrix has changed)" << endl;
		cout << "original matrix: " << endl;
		print_matrix(matrix);
		cout << "final matrix: " << endl;
		print_matrix(temp);
		return 0;
	      }
	    }
	cout << "testMatrixInversion -- OK" << endl;
	
	return 0;
}
