#include <math.h>

#include "RandomEngine.h"
#include "RanluxEngine.h"

namespace CLHEP {

void RanluxEngine::setSeed(long seed, int lux) {

// The initialisation is carried out using a Multiplicative
// Congruential generator using formula constants of L'Ecuyer 
// as described in "A review of pseudorandom number generators"
// (Fred James) published in Computer Physics Communications 60 (1990)
// pages 329-344

  const int ecuyer_a = 53668;
  const int ecuyer_b = 40014;
  const int ecuyer_c = 12211;
  const int ecuyer_d = 2147483563;

  const int lux_levels[5] = {0,24,73,199,365};  

  long int_seed_table[24];
  long next_seed = seed;
  long k_multiple;
  int i;
  
// number of additional random numbers that need to be 'thrown away'
// every 24 numbers is set using luxury level variable.

  theSeed = seed;
  if( (lux > 4)||(lux < 0) ){
     if(lux >= 24){
        nskip = lux - 24;
     }else{
        nskip = lux_levels[3]; // corresponds to default luxury level
     }
  }else{
     luxury = lux;
     nskip = lux_levels[luxury];
  }

   
  for(i = 0;i != 24;i++){
     k_multiple = next_seed / ecuyer_a;
     next_seed = ecuyer_b * (next_seed - k_multiple * ecuyer_a) 
     - k_multiple * ecuyer_c ;
     if(next_seed < 0)next_seed += ecuyer_d;
     int_seed_table[i] = next_seed % int_modulus;
  }     

  for(i = 0;i != 24;i++)
    float_seed_table[i] = int_seed_table[i] * mantissa_bit_24;

  i_lag = 23;
  j_lag = 9;
  carry = 0. ;

  if( float_seed_table[23] == 0. ) carry = mantissa_bit_24;
  
  count24 = 0;
}

void RanluxEngine::setSeeds(const long *seeds, int lux) {

   const int ecuyer_a = 53668;
   const int ecuyer_b = 40014;
   const int ecuyer_c = 12211;
   const int ecuyer_d = 2147483563;

   const int lux_levels[5] = {0,24,73,199,365};
   int i;
   long int_seed_table[24];
   long k_multiple,next_seed;
   const long *seedptr; 

   theSeeds = seeds;
   seedptr  = seeds;
 
   if(seeds == 0){
      setSeed(theSeed,lux);
      theSeeds = &theSeed;
      return;
   }

   theSeed = *seeds;

// number of additional random numbers that need to be 'thrown away'
// every 24 numbers is set using luxury level variable.

  if( (lux > 4)||(lux < 0) ){
     if(lux >= 24){
        nskip = lux - 24;
     }else{
        nskip = lux_levels[3]; // corresponds to default luxury level
     }
  }else{
     luxury = lux;
     nskip = lux_levels[luxury];
  }
      
  for( i = 0;(i != 24)&&(*seedptr != 0);i++){
      int_seed_table[i] = *seedptr % int_modulus;
      seedptr++;
  }		       

  if(i != 24){
     next_seed = int_seed_table[i-1];
     for(;i != 24;i++){
        k_multiple = next_seed / ecuyer_a;
        next_seed = ecuyer_b * (next_seed - k_multiple * ecuyer_a) 
        - k_multiple * ecuyer_c ;
        if(next_seed < 0)next_seed += ecuyer_d;
        int_seed_table[i] = next_seed % int_modulus;
     }          
  }

  for(i = 0;i != 24;i++)
    float_seed_table[i] = int_seed_table[i] * mantissa_bit_24;

  i_lag = 23;
  j_lag = 9;
  carry = 0. ;

  if( float_seed_table[23] == 0. ) carry = mantissa_bit_24;
  
  count24 = 0;
}

RanluxEngine::RanluxEngine(long seed, int lux)
: int_modulus(0x1000000),
  mantissa_bit_24( pow(0.5,24.) ),
  mantissa_bit_12( pow(0.5,12.) )
{
   long seedlist[2]={0,0};

   luxury = lux;
   setSeed(seed, luxury);
   
   // setSeeds() wants a zero terminated array!
   seedlist[0]=theSeed;
   seedlist[1]=0;
   setSeeds(seedlist, luxury);
}

double RanluxEngine::flat() {

  float next_random;
  float uni;
  int i;

  uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
	#ifdef TRACE_IO
	if (flat_trace) {
	  std::cout << "float_seed_table[" << j_lag << "] = "
	  << float_seed_table[j_lag] 
	  << "  float_seed_table[" << i_lag << "] = " << float_seed_table[i_lag]
	  << "  uni = " << uni << "\n";
	  std::cout << float_seed_table[j_lag] 
	            << " - " << float_seed_table[i_lag]
		    << " - " << carry << " = " 
		    << (double)float_seed_table[j_lag] 
		    -  (double) float_seed_table[i_lag] - (double)carry
		    << "\n";
	}
	#endif
  if(uni < 0. ){
     uni += 1.0;
     carry = mantissa_bit_24;
  }else{
     carry = 0.;
  }

  float_seed_table[i_lag] = uni;
  i_lag --;
  j_lag --;
  if(i_lag < 0) i_lag = 23;
  if(j_lag < 0) j_lag = 23;

  if( uni < mantissa_bit_12 ){
     uni += mantissa_bit_24 * float_seed_table[j_lag];
     if( uni == 0) uni = mantissa_bit_24 * mantissa_bit_24;
  }
  next_random = uni;
  count24 ++;

// every 24th number generation, several random numbers are generated
// and wasted depending upon the luxury level.

  if(count24 == 24 ){
     count24 = 0;
         	#ifdef TRACE_IO
		if (flat_trace) {
		  std::cout << "carry = " << carry << "\n"; 
		}
		#endif
     for( i = 0; i != nskip ; i++){
         uni = float_seed_table[j_lag] - float_seed_table[i_lag] - carry;
         if(uni < 0. ){
            uni += 1.0;
            carry = mantissa_bit_24;
         }else{
            carry = 0.;
         }
         float_seed_table[i_lag] = uni;
         	#ifdef TRACE_IO
		if (flat_trace) {
		  double xfst = float_seed_table[i_lag];
		  std::cout << "fst[" << i_lag << "] = " 
			    << DoubConv::d2x(xfst) << "\n";
		}
		#endif
	 i_lag --;
         j_lag --;
         if(i_lag < 0)i_lag = 23;
         if(j_lag < 0) j_lag = 23;
      }
  } 
	#ifdef TRACE_IO
	if (flat_trace) {
	  std::cout << "next_random = " << next_random << "\n";
          // flat_trace = false;
	}
	#endif
  return (double) next_random;
}
}  // namespace CLHEP
