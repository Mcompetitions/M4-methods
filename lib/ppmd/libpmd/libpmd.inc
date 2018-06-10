#ifndef LIBPMD_INCLUDED
#define LIBPMD_INCLUDED

//namespace {

#include "model_def.inc"

struct pmd_codec : Coroutine {

  struct DecWrap : Model<1> {};
  struct EncWrap : Model<0> {};

  #define Max(x,y) ((x)>(y)?(x):(y))
  enum {
    MSize0 = sizeof(EncWrap),
    MSize1 = sizeof(DecWrap),
    MSize2 = Max( MSize0, MSize1 ) - sizeof(Coroutine)
  };
  #undef Max

  ALIGN(32)
  byte pad[ MSize2 ];

  uint f_DEC;

  uint ppmd_order;
  uint ppmd_memory;
  uint ppmd_restore; // flag
  uint ppmd_filesize;

//  uint Init( int MaxOrder, int CutOff, int MMAX ) {

  uint Init( uint _mode, uint* _args, uint f_noflush=1 ) { 
    DecWrap& M1 = *(DecWrap*)this;
    EncWrap& M0 = *(EncWrap*)this;
    uint r=0;
    if( f_noflush ) coro_init(), f_quit=0;
    f_DEC = _mode;
    ppmd_order = _args[0];
    ppmd_memory = _args[1];
    ppmd_restore = _args[2];
    ppmd_filesize = _args[3];

    r = f_DEC ? M1.Init(ppmd_order,ppmd_memory,ppmd_restore,ppmd_filesize)
              : M0.Init(ppmd_order,ppmd_memory,ppmd_restore,ppmd_filesize);
    return r;
  }

  void Flush( uint _mode, uint* _args ) {
    Quit();
    Init(_mode,_args,0);
  }

  void Quit( void ) {
    DecWrap& M1 = *(DecWrap*)this;
    EncWrap& M0 = *(EncWrap*)this;
    f_DEC ? M1.Quit() : M0.Quit();
  }

  uint GetUsedMemory() {
    DecWrap& M1 = *(DecWrap*)this;
    EncWrap& M0 = *(EncWrap*)this;
    return f_DEC ? M1.GetUsedMemory() : M0.GetUsedMemory();
  }

  NOINLINE
  void do_process( void ) {

    DecWrap& M1 = *(DecWrap*)this;
    EncWrap& M0 = *(EncWrap*)this;

    f_DEC ? M1.do_process() : M0.do_process();

    yield(this,0);
  }

};


//#include "pmd_api_.inc"

//}

#endif // LIBPMD_INCLUDED