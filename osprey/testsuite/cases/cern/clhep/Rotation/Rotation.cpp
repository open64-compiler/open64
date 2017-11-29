#include <iostream>
#include <math.h>
#include <vector>
#include <memory>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include "Rotation.h"

namespace CLHEP {

HepRotation & HepRotation::rotateX(double a) {
  double c = cos(a);
  double s = sin(a);
  double x = ryx, y = ryy, z = ryz; 
  ryx = c*x - s*rzx;
  ryy = c*y - s*rzy;
  ryz = c*z - s*rzz;
  rzx = s*x + c*rzx;
  rzy = s*y + c*rzy;
  rzz = s*z + c*rzz;
  return *this;
}

HepRotation & HepRotation::rotateY(double a){
  double c = cos(a);
  double s = sin(a);
  double x = rzx, y = rzy, z = rzz; 
  rzx = c*x - s*rxx;
  rzy = c*y - s*rxy;
  rzz = c*z - s*rxz;
  rxx = s*x + c*rxx;
  rxy = s*y + c*rxy;
  rxz = s*z + c*rxz;
  return *this;
}

HepRotation & HepRotation::rotateZ(double a) {
  double c = cos(a);
  double s = sin(a);
  double x = rxx, y = rxy, z = rxz; 
  rxx = c*x - s*ryx;
  rxy = c*y - s*ryy;
  rxz = c*z - s*ryz;
  ryx = s*x + c*ryx;
  ryy = s*y + c*ryy;
  ryz = s*z + c*ryz;
  return *this;
}

}

