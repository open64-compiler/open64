namespace CLHEP {

class RanluxEngine : public HepRandomEngine {

public:

  //RanluxEngine( std::istream& is );
  //RanluxEngine();
  RanluxEngine( long seed, int lux = 3 );
  //RanluxEngine( int rowIndex, int colIndex, int lux );
  //virtual ~RanluxEngine();
  // Constructors and destructor

  //RanluxEngine(const RanluxEngine &p);
  // Copy constructor

  //RanluxEngine & operator = (const RanluxEngine &p);
  // Overloaded assignment operator, to retrieve the engine status.

// Luxury level is set in the same way as the original FORTRAN routine.
//  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
//           and Zaman, very long period, but fails many tests.
//  level 1  (p=48): considerable improvement in quality over level 0,
//           now passes the gap test, but still fails spectral test.
//  level 2  (p=97): passes all known tests, but theoretically still
//           defective.
//  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
//           correlations have very small chance of being observed.
//  level 4  (p=389): highest possible luxury, all 24 bits chaotic.

  double flat();
/*
  // It returns a pseudo random number between 0 and 1,
  // excluding the end points.

  void flatArray (const int size, double* vect);
  // Fills the array "vect" of specified size with flat random values.
*/
  void setSeed(long seed, int lux=3);
  // Sets the state of the algorithm according to seed.

  void setSeeds(const long * seeds, int lux=3);
  // Sets the state of the algorithm according to the zero terminated
  // array of seeds. Only the first seed is used.
/*
  void saveStatus( const char filename[] = "Ranlux.conf" ) const;
  // Saves on file Ranlux.conf the current engine status.

  void restoreStatus( const char filename[] = "Ranlux.conf" );
  // Reads from file Ranlux.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

  int getLuxury() const { return luxury; }
  // Gets the luxury level.

  operator unsigned int(); // 32-bit flat, but slower than double or float

  virtual std::ostream & put (std::ostream & os) const;
  virtual std::istream & get (std::istream & is);
  static  std::string beginTag ( );
  virtual std::istream & getState ( std::istream & is );

  std::string name() const;
  static std::string engineName() {return "RanluxEngine";}

  std::vector<unsigned long> put () const;
  bool get (const std::vector<unsigned long> & v);
  bool getState (const std::vector<unsigned long> & v);
  
  static const unsigned int VECTOR_STATE_SIZE = 31;
*/  
private:

  int nskip, luxury;
  float float_seed_table[24];
  int i_lag,j_lag;  
  float carry;
  int count24;
  const int int_modulus;
  const double mantissa_bit_24;
  const double mantissa_bit_12;
  static int numEngines;
  static int maxIndex;
};

}
