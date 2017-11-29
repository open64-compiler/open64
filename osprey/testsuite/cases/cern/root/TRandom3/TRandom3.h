class TRandom3 : public TRandom {

private:
   UInt_t   fMt[624];
   Int_t    fCount624;

public:
   TRandom3(UInt_t seed=65539);
   virtual ~TRandom3();
   virtual  Double_t  Rndm(Int_t i=0);
	/*
   virtual  void      RndmArray(Int_t n, Float_t *array);
	*/
   virtual  void      RndmArray(Int_t n, Double_t *array);
   virtual  void      SetSeed(UInt_t seed=0);

   //ClassDef(TRandom3,2)  //Random number generator: Mersenne Twistor
};

