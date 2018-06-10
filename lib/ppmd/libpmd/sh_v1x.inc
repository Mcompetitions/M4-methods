#ifndef SH_V1X_INCLUDED
#define SH_V1X_INCLUDED

template< int ProcMode >
struct Rangecoder_SH1x : Coroutine {

  enum {
    SCALElog = 15,
    SCALE    = 1<<SCALElog
  };

  enum {
    NUM   = 4,
    sTOP  = 0x01000000U,
    gTOP  = 0x00010000U,
    Thres = 0xFF000000U,
    Threg = 0x00FF0000U
  };

//  int   ProcMode; // 0=encode, 1=decode;
  union {
    struct {
      uint  low;  
      uint  Carry;
    };
    qword lowc;
    uint  code; 
  };
  uint  FFNum;
  uint  Cache;
  uint  range;

  void rc_Process( uint cumFreq, uint freq, uint totFreq ) {
    uint tmp = cumFreq*range;
    if( ProcMode ) code-=tmp; else lowc+=tmp;
    range *= freq;
    Renorm();
  }


  void rc_Arrange( uint totFreq ) {
    range /= totFreq;
  }

  uint rc_GetFreq( uint totFreq ) {
    return code/range;
  }

  void Renorm( void ) {
    if( ProcMode ) {
 //     while( range<sTOP ) range<<=8, (code<<=8)+=get();
      if( range<gTOP ) range<<=16, (code<<=16)+=(get()<<8)+get(); 
      else if( range<sTOP ) range<<=8, (code<<=8)+=get();
    } else {
 //     while( range<sTOP ) range<<=8, ShiftLow();
      if( range<gTOP ) range<<=16, ShiftLow2(); 
      else if( range<sTOP ) range<<=8, ShiftLow();
    }
  }

//  NOINLINE
  void rc_BProcess( uint freq, int& bit ) { 

 //   uint rnew = (qword(range)*(freq<<(32-SCALElog)))>>32;
 //   uint rnew = ( range>=16*sTOP ) ? freq*(range>>SCALElog) : freq*(range>>(SCALElog-4))>>4;
    uint rnew = (range>>SCALElog)*freq;

 //   if( ProcMode ) bit = 1 + uint((qword(code)-rnew)>>32);
    if( ProcMode ) bit = (code>=rnew);

    range = ((range-rnew-rnew)&(-bit)) + rnew;
    rnew &= -bit;

    if( ProcMode ) code -= rnew; else lowc += rnew;

    Renorm();
  }

  void ShiftLow( void ) {
    if( low<Thres || Carry ) {
      put( Cache+Carry );
      for (;FFNum != 0;FFNum--) put( Carry-1 ); // (Carry-1)&255;
      Cache = low>>24;
      Carry = 0;
    } else FFNum++;
    low<<=8;
  }

  void ShiftLow2( void ) {
    if( low<Thres || Carry ) {
      put( Cache+Carry );
      for (;FFNum != 0;FFNum--) put( Carry-1 ); // (Carry-1)&255;
      Cache = low>>24;
      Carry = 0;
    } else FFNum++;
    low &= sTOP-1;
    if( low<Threg ) {
      put( Cache );
      for(; FFNum!=0; FFNum-- ) put( 0xFF ); // (Carry-1)&255;
      Cache = low>>16;
    } else FFNum++;
    low<<=16;
  }

  void rcInit( void ) { 
    range = 0xFFFFFFFF;
    low   = 0;
    FFNum = 0;
    Carry = 0;    
    Cache = 0;
  }
  
  void rc_Init( void ) {
    rcInit();
    if( ProcMode==1 ) {
      for(int _=0; _<NUM+1; _++) (code<<=8)+=get(); 
    }
  }

  void rc_Quit( void ) {
    if( ProcMode==0 ) {
      for(int _=0; _<NUM+1; _++) ShiftLow(); 
    }
  }

};

#endif // SH_V1X_INCLUDED