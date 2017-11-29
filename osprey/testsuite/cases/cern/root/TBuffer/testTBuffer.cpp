#include <iostream>
#include <unistd.h>
#include "TBuffer.h"
#include "timing.h"
#include "Rtypes.h"
#include "TStreamerElement.h"

using namespace std;
#define N 30000000



void printVector(Double32_t *d, int size){
  
  for (Int_t counter= 0; counter < size; counter++)
    cout << "d ["<<counter<<"]="<< d[counter]<< "\t";
  cout << endl;
}

void comparar(Double32_t *d,Double32_t *d2, int size){
  
  for (Int_t counter= 0; counter < size; counter++) 
    cout << "d ["<<counter<<"]="<< d[counter]<<"..."<<d2[counter]<<endl; 
}


// Debugging function, isn't part of HepMatrix class
void generateElements(Double32_t *data, int size) {
  int i;

  srand48(clock()+getpid());

  for(i=0;i<size;i++){
    data[i]=drand48()+1;
     //cout << "data ["<<i<<"]="<< data[i]<<endl; 
  }
}

int main(void){
  
  TStreamerElement *element=new TStreamerElement();
  TBuffer *buf=new TBuffer(TBuffer::kWrite);
  
  int size=100;
  Double32_t vect[size];
  Double32_t vect2[size]; 

  generateElements(vect,size);
  //  printVector(vect, size);
  
  buf->WriteFastArrayDouble32(vect,size);
  

  Timer *time=new Timer();
  time->start();

for (int counter=0;counter <N; counter ++){
  TBuffer *buf2=new TBuffer(TBuffer::kRead,buf->Length(),buf->Buffer());
  buf2->ReadFastArrayDouble32(vect2,size);
}

  
//printVector (vect2, size);
  cout.precision(5);
  time->getTime();
  cout << endl << "time = " <<time->getwTime()<<endl;
  
  Double32_t epsilon = 0.0001;
  for (int i = 0; i < size; i++){
    if (((vect2[i]+epsilon) < vect[i] && (vect2[i] < vect[i])) || ((vect[i] < vect2[i]) && ((vect[i]+epsilon) < vect2[i])))
      {
      cout << "testTBuffer -- FAILURE   (initial and final vectors do not match)" << endl;
      cout << "original vector: " << endl;
      printVector(vect, size);
      cout << "final vector: " << endl;
      printVector (vect2, size);
      return 0;
    }
  }
  cout << "testTBuffer -- OK" << endl;
  

  return 0;
}
