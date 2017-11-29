
class Timer {
	private:
		double uTime;	// User Time (ONLY processing time)
		double sTime; // System Time (time used by the operating system for I/O purpouses (in example))
		double wTime; // Wall Time (total time between the first and the last code line)
		double uTimePrev;
		double sTimePrev;
		double wTimePrev;
		long long cycles; // CPU cycles
		long long cyclesPrev;
		bool started;
		long long rdtsc();
	public:
		Timer();
		bool start();
		bool isStarted();
		char *getTime();
		char *getTimeAndStop();
		double getuTime();
		double getsTime();
		double getwTime();
		long long getCycles();
	
};
