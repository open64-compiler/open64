class HepMatrix : public HepGenMatrix {
public:
   //~ inline HepMatrix();
   //~ // Default constructor. Gives 0 x 0 matrix. Another Matrix can be
   //~ // assigned to it.

   //~ HepMatrix(int p, int q);
   //~ // Constructor. Gives an unitialized p x q matrix.
   HepMatrix(int p, int q, int i);
   //~ // Constructor. Gives an initialized p x q matrix. 
   //~ // If i=0, it is initialized to all 0. If i=1, the diagonal elements
   //~ // are set to 1.0.

   //~ HepMatrix(int p, int q, HepRandom &r);
   //~ // Constructor with a Random object.

   //~ HepMatrix(const HepMatrix &m1);
   //~ // Copy constructor.

   //~ HepMatrix(const HepSymMatrix &m1);
   //~ HepMatrix(const HepDiagMatrix &m1);
   //~ HepMatrix(const HepVector &m1);
   //~ // Constructors from SymMatrix, DiagMatrix and Vector.

   virtual ~HepMatrix();
   //~ // Destructor.

   inline virtual int num_row() const;
   //~ // Returns the number of rows.

   inline virtual int num_col() const;
   //~ // Returns the number of columns.

   //~ inline virtual const double & operator()(int row, int col) const;
   //~ inline virtual double & operator()(int row, int col);
   //~ // Read or write a matrix element. 
   //~ // ** Note that the indexing starts from (1,1). **

   //~ HepMatrix & operator *= (double t);
   //~ // Multiply a Matrix by a floating number.

   //~ HepMatrix & operator /= (double t); 
   //~ // Divide a Matrix by a floating number.

   //~ HepMatrix & operator += ( const HepMatrix &m2);
   //~ HepMatrix & operator += ( const HepSymMatrix &m2);
   //~ HepMatrix & operator += ( const HepDiagMatrix &m2);
   //~ HepMatrix & operator += ( const HepVector &m2);
   //~ HepMatrix & operator -= ( const HepMatrix &m2);
   //~ HepMatrix & operator -= ( const HepSymMatrix &m2);
   //~ HepMatrix & operator -= ( const HepDiagMatrix &m2);
   //~ HepMatrix & operator -= ( const HepVector &m2);
   //~ // Add or subtract a Matrix. 
   //~ // When adding/subtracting Vector, Matrix must have num_col of one.

   //~ HepMatrix & operator = ( const HepMatrix &m2);
   //~ HepMatrix & operator = ( const HepSymMatrix &m2);
   //~ HepMatrix & operator = ( const HepDiagMatrix &m2);
   //~ HepMatrix & operator = ( const HepVector &m2);
   //~ HepMatrix & operator = ( const HepRotation &m2);
   //~ // Assignment operators.

   //~ HepMatrix operator- () const;
   //~ // unary minus, ie. flip the sign of each element.

   //~ HepMatrix apply(double (*f)(double, int, int)) const;
   //~ // Apply a function to all elements of the matrix.

   //~ HepMatrix T() const;
   //~ // Returns the transpose of a Matrix.

   //~ HepMatrix sub(int min_row, int max_row, int min_col, int max_col) const;
   //~ // Returns a sub matrix of a Matrix.
   //~ // WARNING: rows and columns are numbered from 1
   //~ void sub(int row, int col, const HepMatrix &m1);
   //~ // Sub matrix of this Matrix is replaced with m1.
   //~ // WARNING: rows and columns are numbered from 1

   //~ friend inline void swap(HepMatrix &m1, HepMatrix &m2);
   //~ // Swap m1 with m2.

   //~ inline HepMatrix inverse(int& ierr) const;
   //~ // Invert a Matrix. Matrix must be square and is not changed.
   //~ // Returns ierr = 0 (zero) when successful, otherwise non-zero.

   //~ virtual void invert(int& ierr);
   //~ // Invert a Matrix. Matrix must be square.
   //~ // N.B. the contents of the matrix are replaced by the inverse.
   //~ // Returns ierr = 0 (zero) when successful, otherwise non-zero. 
   //~ // This method has less overhead then inverse().

   //~ double determinant() const;
   //~ // calculate the determinant of the matrix.

   //~ double trace() const;
   //~ // calculate the trace of the matrix (sum of diagonal elements).

   class HepMatrix_row {
    public:
      //~ inline HepMatrix_row(HepMatrix&,int);
      //~ double & operator[](int);
    private:
      //~ HepMatrix& _a;
      //~ int _r;
   };
   
   class HepMatrix_row_const {
    public:
      //~ inline HepMatrix_row_const (const HepMatrix&,int);
      //~ const double & operator[](int) const;
    private:
      //~ const HepMatrix& _a;
      //~ int _r;
   };
   
   //~ // helper classes for implementing m[i][j]

   //~ inline HepMatrix_row operator[] (int);
   //~ inline const HepMatrix_row_const operator[] (int) const;
   //~ // Read or write a matrix element.
   //~ // While it may not look like it, you simply do m[i][j] to get an
   //~ // element. 
   //~ // ** Note that the indexing starts from [0][0]. **

 protected:
   //~ virtual inline int num_size() const;
 public: /****************** ADDED ****************/
   virtual void invertHaywood4(int& ierr);
   virtual void invertHaywood5(int& ierr);
   virtual void invertHaywood6(int& ierr);

 private:
   //~ friend class HepMatrix_row;
   //~ friend class HepMatrix_row_const;
   //~ friend class HepVector;
   //~ friend class HepSymMatrix;
   //~ friend class HepDiagMatrix;
   //~ // Friend classes.

   //~ friend HepMatrix operator+(const HepMatrix &m1, const HepMatrix &m2);
   //~ friend HepMatrix operator-(const HepMatrix &m1, const HepMatrix &m2);
   //~ friend HepMatrix operator*(const HepMatrix &m1, const HepMatrix &m2);
   //~ friend HepMatrix operator*(const HepMatrix &m1, const HepSymMatrix &m2);
   //~ friend HepMatrix operator*(const HepMatrix &m1, const HepDiagMatrix &m2);
   //~ friend HepMatrix operator*(const HepSymMatrix &m1, const HepMatrix &m2);
   //~ friend HepMatrix operator*(const HepDiagMatrix &m1, const HepMatrix &m2);
   //~ friend HepMatrix operator*(const HepVector &m1, const HepMatrix &m2);
   //~ friend HepVector operator*(const HepMatrix &m1, const HepVector &m2);
   //~ friend HepMatrix operator*(const HepSymMatrix &m1, const HepSymMatrix &m2);
   //~ // Multiply a Matrix by a Matrix or Vector.

   //~ friend HepVector solve(const HepMatrix &, const HepVector &);
   //~ // solve the system of linear eq
   //~ friend HepVector qr_solve(HepMatrix *, const HepVector &);
   //~ friend HepMatrix qr_solve(HepMatrix *, const HepMatrix &b);
   //~ friend void tridiagonal(HepSymMatrix *a,HepMatrix *hsm);
   //~ friend void row_house(HepMatrix *,const HepMatrix &, double,
			 //~ int, int, int, int);
   //~ friend void row_house(HepMatrix *,const HepVector &, double,
			 //~ int, int);
   //~ friend void back_solve(const HepMatrix &R, HepVector *b);
   //~ friend void back_solve(const HepMatrix &R, HepMatrix *b);
   //~ friend void col_givens(HepMatrix *A, double c,
			  //~ double s, int k1, int k2, 
			  //~ int rowmin, int rowmax);
   //~ //    Does a column Givens update.
   //~ friend void row_givens(HepMatrix *A, double c,
			  //~ double s, int k1, int k2, 
			  //~ int colmin, int colmax);
   //~ friend void col_house(HepMatrix *,const HepMatrix &, double,
			 //~ int, int, int, int);
   //~ friend HepVector house(const HepMatrix &a,int row,int col);
   //~ friend void house_with_update(HepMatrix *a,int row,int col);
   //~ friend void house_with_update(HepMatrix *a,HepMatrix *v,int row,int col);
   //~ friend void house_with_update2(HepSymMatrix *a,HepMatrix *v,
				  //~ int row,int col); 

   //~ int dfact_matrix(double &det, int *ir);
   //~ // factorize the matrix. If successful, the return code is 0. On
   //~ // return, det is the determinant and ir[] is row-interchange
   //~ // matrix. See CERNLIB's DFACT routine.

   //~ int dfinv_matrix(int *ir);
   //~ // invert the matrix. See CERNLIB DFINV.

// #ifdef DISABLE_ALLOC
	public : /********* ADDED *************/
   std::vector<double > m;
// #else
//   std::vector<double,Alloc<double,25> > m;
// #endif
   int nrow, ncol;
   int size;
};


inline int HepMatrix::num_row() const { return nrow;}

inline int HepMatrix::num_col() const  { return ncol;}

