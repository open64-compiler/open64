#include <math.h>
 
#include "RandomEngine.h"

namespace CLHEP {

HepRandomEngine::HepRandomEngine() 
: theSeeds(&theSeed),
  exponent_bit_32( pow(2.,32.) )
{
  theSeed = 19780503L;
}

}
