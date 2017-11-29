class G4String : public std::string
{

  typedef std::string std_string;

public: 

  //~ enum caseCompare { exact, ignoreCase };
  //~ enum stripType { leading, trailing, both };

  //~ inline G4String ();
  //~ inline G4String ( char );
  inline G4String ( const char * );
  //~ inline G4String ( const G4String& );
  //~ inline G4String ( const G4SubString& );
  //~ inline G4String ( const std::string & );
  //~ virtual ~G4String () {}

  //~ inline G4String& operator=(const G4String&);
  //~ inline G4String& operator=(const std::string &);
  //~ inline G4String& operator=(const char*);

  //~ inline char operator () (str_size) const; 
  //~ inline char& operator () (str_size);

  //~ inline G4String& operator+=(const G4SubString&);
  //~ inline G4String& operator+=(const char*);
  //~ inline G4String& operator+=(const std::string &);
  //~ inline G4String& operator+=(const char&);
  //~ inline G4bool operator==(const G4String&) const;
  //~ inline G4bool operator==(const char*) const;
  //~ inline G4bool operator!=(const G4String&) const;
  //~ inline G4bool operator!=(const char*) const;

  //~ //inline G4String operator () (unsigned int, unsigned int);
  //~ inline operator const char*() const;
  //~ inline G4SubString operator()(str_size, str_size);

  //~ inline G4int compareTo(const char*, caseCompare mode=exact);
  //~ inline G4int compareTo(const G4String&, caseCompare mode=exact);

  //~ inline G4String& prepend (const char*);
  //~ inline G4String& append (const G4String&);

  //~ inline std::istream& readLine (std::istream&, G4bool skipWhite=true);
  
  //~ inline G4String& replace (unsigned int, unsigned int, 
                             //~ const char*, unsigned int );
  //~ inline G4String& replace(str_size, str_size, const char*);

  //~ inline G4String& remove(str_size);
  //~ inline G4String& remove(str_size, str_size);

  //~ inline G4int first(char) const;
  //~ inline G4int last(char) const;

  //~ inline G4bool contains(std::string) const;
  //~ inline G4bool contains(char) const;

  //~ // stripType = 0 beginning
  //~ // stripType = 1 end
  //~ // stripType = 2 both
  //~ //
  //~ inline G4String strip (G4int stripType=trailing, char c=' ');

  //~ inline void toLower ();
  //~ inline void toUpper ();

  //~ inline G4bool isNull() const;

  //~ inline str_size index (const char*, G4int pos=0) const; 
  //~ inline str_size index (char, G4int pos=0) const; 
  //~ inline str_size index (const G4String&, str_size, str_size, caseCompare) const;

  //~ inline const char* data() const;

  //~ inline G4int strcasecompare(const char*, const char*) const;

  //~ inline unsigned int hash( caseCompare cmp = exact ) const;
  //~ inline unsigned int stlhash() const;

  //~ // useful for supplying hash functions to template hash collection ctors
  //~ //
  //~ static inline unsigned int hash(const G4String&);

};

G4String::G4String ( const char * astring )
 : std_string ( astring ) {}

