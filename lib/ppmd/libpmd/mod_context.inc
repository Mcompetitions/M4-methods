#ifndef MOD_CONTEXT_INCLUDED
#define MOD_CONTEXT_INCLUDED

enum {
  MAX_FREQ    = 124,
  O_BOUND     = 9 
};

struct PPM_CONTEXT;

struct STATE {
  byte Symbol;
  byte Freq;
  uint iSuccessor;
};

PPM_CONTEXT* getSucc( STATE* This ) { 
  return (PPM_CONTEXT*)Indx2Ptr( This->iSuccessor ); 
}


void SWAP( STATE& s1, STATE& s2 ) {
  word t1       = (word&)s1;
  uint t2       = s1.iSuccessor;
  (word&)s1     = (word&)s2;
  s1.iSuccessor = s2.iSuccessor;
  (word&)s2     = t1;                        
  s2.iSuccessor = t2;
}

struct PPM_CONTEXT {

  byte NumStats;
  byte Flags;
  word SummFreq;
  uint iStats;
  uint iSuffix;

  STATE& oneState() const { return (STATE&) SummFreq; }
};

STATE* getStats( PPM_CONTEXT* This ) { return (STATE*)Indx2Ptr(This->iStats); }

PPM_CONTEXT* suff( PPM_CONTEXT* This ) { return (PPM_CONTEXT*)Indx2Ptr(This->iSuffix); }

#endif // MOD_CONTEXT_INCLUDED