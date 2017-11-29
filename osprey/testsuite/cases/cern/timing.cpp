#include "timing.h"
#include <sys/resource.h>
#include <sys/time.h>
#include <string.h>
#include <stdio.h> // sprintf
#include <stdlib.h>

#if (defined(__ia64__) && defined(__INTEL_COMPILER))
	#include <ia64intrin.h>
#endif

long long Timer::rdtsc() {

#if defined __ia64__
        #ifdef __HP_aCC
#include <machine/sys/inline.h>
         return _Asm_mov_from_ar(_AREG_ITC);
         #undef __GNUC__
        #elif __INTEL_COMPILER
  	return __getReg (_IA64_REG_AR_ITC);
         #undef __GNUC__
	#elif __GNUC__
  	unsigned long result;
    __asm__ __volatile__("mov %0=ar.itc" : "=r"(result) :: "memory");
    while (__builtin_expect ((int) result == -1, 0))
    	__asm__ __volatile__("mov %0=ar.itc" : "=r"(result) :: "memory");
	 	return result;
  #endif
#else // x86 
	long long a;
  asm volatile("rdtsc":"=&A" (a));
  return a;
#endif


   // Microsoft and Intel Windows compilers
   //#elif defined _M_IX86
   //  __asm rdtsc
   //#elif defined _M_AMD64
   //  return __rdtsc ();
   //#elif defined __ia64__
//	return 0;
}
 
Timer::Timer() {
	uTime=sTime=wTime=uTimePrev=sTimePrev=wTimePrev=0.0;	
	cycles=0;
	started=false;
}

bool Timer::start() {
	struct rusage buffer;
	struct timeval tp;
	struct timezone tzp;

	if(started)
		return false;
								
	getrusage(RUSAGE_SELF, &buffer);
	gettimeofday(&tp, &tzp);
	
	cyclesPrev = cycles = rdtsc();
	uTimePrev = uTime = (double) buffer.ru_utime.tv_sec + 1.0e-6 * buffer.ru_utime.tv_usec;
	sTimePrev = sTime = (double) buffer.ru_stime.tv_sec + 1.0e-6 * buffer.ru_stime.tv_usec;
	wTimePrev = wTime = (double) tp.tv_sec + 1.0e-6 * tp.tv_usec;
												
	return (started=true);
}

bool Timer::isStarted() {
	return started;
}
				
char* Timer::getTime() {
	struct rusage buffer;
	struct timeval tp;
	struct timezone tzp;
	char *out=(char*)malloc(sizeof(char)*1000);

	if(!started)
		return NULL;

	wTimePrev=wTime;
	sTimePrev=sTime;
	uTimePrev=uTime;
	cyclesPrev=cycles;
	
	getrusage(RUSAGE_SELF, &buffer);
	gettimeofday(&tp, &tzp);
								
	cycles=rdtsc();
	uTime = (double) buffer.ru_utime.tv_sec + 1.0e-6 * buffer.ru_utime.tv_usec;
	sTime = (double) buffer.ru_stime.tv_sec + 1.0e-6 * buffer.ru_stime.tv_usec;
	wTime = (double) tp.tv_sec + 1.0e-6 * tp.tv_usec;
												
	sprintf(out,"\nWall Time: %.3f\nUser Time: %.3f\nSystem Time: %.3f\nCPU/Wall %.3f %%\nCycles: %lld\nCycles/sec: %g\n",getwTime(),getuTime(),getsTime(),100*(getuTime()+getsTime())/(getwTime()),getCycles(),getCycles()/getwTime());

	return out;
}
				
char* Timer::getTimeAndStop() {
	char *out=getTime();
	
	started=false;
	uTime=sTime=wTime=uTimePrev=sTimePrev=wTimePrev=0.0;
		
	return out;								
}

double Timer::getuTime() {
	return uTime-uTimePrev;
}

double Timer::getsTime() {
	return sTime-sTimePrev;
}

double Timer::getwTime() {
	return wTime-wTimePrev;
}

long long Timer::getCycles() {
	return cycles-cyclesPrev;
}
