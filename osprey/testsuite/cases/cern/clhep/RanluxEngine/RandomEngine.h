namespace CLHEP {

class HepRandomEngine {

public:
  HepRandomEngine();
/*
  virtual ~HepRandomEngine();
  // Constructor and destructor

  inline bool operator==(const HepRandomEngine& engine);
  inline bool operator!=(const HepRandomEngine& engine);
  // Overloaded operators, ==, !=

  virtual double flat() = 0;
  // Should return a pseudo random number between 0 and 1 
  // (excluding the end points)

  virtual void flatArray(const int size, double* vect) = 0;
  // Fills an array "vect" of specified size with flat random values.

  virtual void setSeed(long seed, int) = 0;
  // Should initialise the status of the algorithm according to seed.

  virtual void setSeeds(const long * seeds, int) = 0;
  // Should initialise the status of the algorithm according to the zero terminated
  // array of seeds. It is allowed to ignore one or many seeds in this array.

  virtual void saveStatus( const char filename[] = "Config.conf") const = 0;
  // Should save on a file specific to the instantiated engine in use
  // the current status.

  virtual void restoreStatus( const char filename[] = "Config.conf" ) = 0;
  // Should read from a file (specific to the instantiated engine in use)
  // and restore the last saved engine configuration.

  virtual void showStatus() const = 0;
  // Should dump the current engine status on the screen.

  virtual std::string name() const = 0;
  // Engine name.

  virtual std::ostream & put (std::ostream & os) const;
  virtual std::istream & get (std::istream & is);
  // Save and restore to/from streams

  static std::string beginTag ( );
  virtual std::istream & getState ( std::istream & is );
  // Helpers for EngineFactory which restores anonymous engine from istream

  static HepRandomEngine* newEngine(std::istream & is);
  // Instantiates on the heap a new engine of type specified by content of is

  static HepRandomEngine* newEngine(const std::vector<unsigned long> & v);
  // Instantiates on the heap a new engine of type specified by content of v

  virtual std::vector<unsigned long> put () const;
  virtual bool get (const std::vector<unsigned long> & v);
  virtual bool getState (const std::vector<unsigned long> & v);
  // Save and restore to/from vectors

  long getSeed() const { return theSeed; }
  // Gets the current seed.

  const long* getSeeds() const { return theSeeds; }
  // Gets the current array of seeds.

  virtual operator double();        // Returns same as flat()
  virtual operator float();         // less precise flat, faster if possible
  virtual operator unsigned int();     // 32-bit int flat, faster if possible

  // The above three conversion operators permit one to retrieve a pseudo-
  // random number as either a double-precision float, a single-precision
  // float, or a 32-bit unsigned integer. The usage, presuming an object
  // of the respective engine class "e", is as follows:

  // Recommended:
  //    float x;
  //    x = float( e );

  // Reasonable:
  //    x = e;

  // Works, but bad practice:
  //    x = 1.5 + e;

  // Won't compile:
  //    x = e + 1.5;
*/
protected:

  long theSeed;
  const long* theSeeds;

  const double exponent_bit_32;
/*  
  static bool checkFile (std::istream & file, 
  		         const std::string & filename, 
  		         const std::string & classname, 
		         const std::string & methodname); 
*/
};

}
