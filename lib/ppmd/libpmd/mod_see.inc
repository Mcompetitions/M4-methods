#ifndef MOD_SEE_INCLUDED
#define MOD_SEE_INCLUDED

enum { 
  INT_BITS    = 7, 
  PERIOD_BITS = 7, 
  TOT_BITS    = INT_BITS + PERIOD_BITS,
  INTERVAL    = 1 << INT_BITS, 
  BIN_SCALE   = 1 << TOT_BITS, 
  ROUND       = 16
};

// SEE-contexts for PPM-contexts with masked symbols
struct SEE2_CONTEXT { 
  word Summ;
  byte Shift;
  byte Count;

  void init( uint InitVal ) { 
    Shift = PERIOD_BITS-4;
    Summ  = InitVal << Shift; 
    Count = 7; 
  }

  uint getMean() {
    return Summ >> Shift;
  }

  void update() { 
    if( --Count==0 ) setShift_rare(); 
  }

  void setShift_rare() {
    uint i = Summ >> Shift;
    i = PERIOD_BITS - (i>40) - (i>280) - (i>1020);
    if( i<Shift ) { Summ >>= 1; Shift--; } else
    if( i>Shift ) { Summ <<= 1; Shift++; }
    Count = 5 << Shift;
  }
};

#endif // MOD_SEE_INCLUDED