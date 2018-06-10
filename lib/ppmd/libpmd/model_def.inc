#ifndef MODEL_DEF_INCLUDED
#define MODEL_DEF_INCLUDED

//#pragma pack(1)

const int ORealMAX=256;

#include "sh_v1x.inc"

static signed char EscCoef[12] = { 16, -10, 1, 51, 14, 89, 23, 35, 64, 26, -42, 43  };

// Tabulated escapes for exponential symbol distribution
static const byte ExpEscape[16]={ 51,43,18,12,11,9,8,7,6,5,4,3,3,2,2,2 };


template< int ProcMode >
struct Model: Rangecoder_SH1x<ProcMode> {

  typedef Rangecoder_SH1x<ProcMode> Base;

  using Base::rc_BProcess;
  using Base::rc_Arrange;
  using Base::rc_GetFreq;
  using Base::rc_Process;
  using Base::rc_Init;
  using Base::rc_Quit;
  using Base::get;
  using Base::put;
  using Base::f_quit;

  typedef byte* pbyte;
  byte* HeapStart;
  uint   Ptr2Indx( void* p ) { return pbyte(p)-HeapStart; }
  void*  Indx2Ptr(uint indx) { return indx + HeapStart; }

  enum{ 
    UNIT_SIZE=12, 
    N1=4, N2=4, N3=4, N4=(128+3-1*N1-2*N2-3*N3)/4,
    N_INDEXES=N1+N2+N3+N4 
  };

  #include "alloc_node.inc"

  uint  GlueCount;
  uint  GlueCount1;
  uint  SubAllocatorSize;
  byte* pText;
  byte* UnitsStart;
  byte* LoUnit;
  byte* HiUnit;
  byte* AuxUnit;

  #include "alloc_units.inc"

  #include "ppmd_init.inc"
  #include "mod_context.inc"

  int _MaxOrder, _CutOff, _MMAX;
  uint _filesize;
  int OrderFall;

  STATE* FoundState; // found next state transition
  PPM_CONTEXT* MaxContext;

  uint EscCount;
  uint CharMask[256];

  int  BSumm;
  int  RunLength;
  int  InitRL;

  #include "mod_see.inc"

  int  NumMasked;

  #include "mod_rescale.inc"
  #include "mod_cutoff.inc"

  #include "ppmd_flush.inc"
  #include "ppmd_update.inc"

  #include "ppmd_proc0.inc"
  #include "ppmd_proc1.inc"
  #include "ppmd_proc2.inc"


  uint Init( uint MaxOrder, uint MMAX, uint CutOff, uint filesize ) {
    _MaxOrder = MaxOrder;
    _CutOff = CutOff;
    _MMAX = MMAX;
    _filesize = filesize;

    PPMD_STARTUP();

    //f_quit=0; coro_init();

    if( !StartSubAllocator( _MMAX ) ) return 1;

    StartModelRare();

//printf( "f_DEC=%i ord=%i mem=%i cutoff=%i\n", ProcMode, _MaxOrder, _MMAX, _CutOff );

    return 0;
  }

  void Quit( void ) {
    StopSubAllocator();
  }

  void do_process( void ) {
    uint c,i;

    if( ProcMode==0 ) {
      for( c=24; c!=-8; c-=8 ) put( byte(_filesize>>c) );
    } else {
      for( c=0,i=24; i!=-8; i-=8 ) c |= get()<<i;
      _filesize=c;
    }

    rc_Init();

    for( i=0; i<_filesize; i++ ) {
      c = 0;
      if( ProcMode==0 ) c=get();
      c = ProcessByte( c ); 
      if( ProcMode==1 ) put(c);
    }

    f_quit=1;
    rc_Quit(); 

/*
    while( f_quit==0 ) {
      c = 0;

      if( ProcMode==0 ) { c = get(); if( f_quit ) break; }

      c = ProcessByte( c ); 

      if( ProcMode==1 ) { if( c!=-1 ) put(c); else f_quit=1; }
    }

    if( ProcMode==0 ) {
      ProcessByte( -1 );
      rc_Quit(); 
    }
*/
    yield(this,0);
  }

  #include "ppmd_byte.inc"

};

//#pragma pack()

//typedef Model<0> Model0;
//typedef Model<1> Model1;

#endif // MODEL_DEF_INCLUDED