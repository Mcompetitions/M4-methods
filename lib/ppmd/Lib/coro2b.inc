#ifndef CORO2B_INCLUDED
#define CORO2B_INCLUDED

/*
#if (defined __GNUC__) || (defined __INTEL_COMPILER)

  #ifdef X64

  //#include <setjmp.h>
  #include "my_setjmp2_x64.h"

  #else

  //#include <setjmp.h>
  //#include "my_setjmp.h"
  //#include "my_setjmp2.h"
  #include "my_setjmp2a.h"

  #endif

#else 

  #include <setjmp.h>

#endif
*/

#include <setjmp.h>

void yield( void* p, int value );

struct Coroutine {

  volatile byte* inpptr;
  volatile byte* inpbeg;
  volatile byte* inpend;
  volatile byte* outptr;
  volatile byte* outbeg;
  volatile byte* outend;
  
  volatile char* stkptrH;
  volatile char* stkptrL;

  volatile uint  state;
  volatile uint  f_quit;

  typedef void (Coroutine::*t_do_process)( void );

  ALIGN(32) jmp_buf PointA;
  ALIGN(32) jmp_buf PointB;

#ifndef STKPAD
  enum{ STKPAD=4096*4 }; // coroutine stack size
#endif
#ifndef STKPAD0
  enum{ STKPAD0=1<<16 }; // stack padding from frontend to coroutine
#endif
  ALIGN(32) byte stk[STKPAD];

  NOINLINE
  static void call_do_process0( Coroutine* that, t_do_process p_do_process ) {
    // call_do_process0 needs to be an actual separate function to allocate stack pad in its frame
    byte stktmp[STKPAD0]; that->stkptrH = (char*)stktmp;
    // do_process also needs a separate stack frame, to avoid merging stktmp into it, but ptr call is ok
    (that->*p_do_process)();
    // do_process ends with yield(0) (can't normally return to changed frontend stack)
    // so tell compiler that this point can't be reached
    __assume(0);
  }

  NOINLINE
  static uint coro_call0( Coroutine* that, t_do_process p_do_process ) {
    if( setjmp(that->PointA)==0 ) {
      if( that->state ) {
        memcpy( (char*)that->stkptrL, that->stk, that->stkptrH-that->stkptrL );
        longjmp(that->PointB,1); 
        __assume(0);
      }
      call_do_process0(that,p_do_process);
      __assume(0);
    }
    return that->state;
  }

  template <typename T> 
  uint coro_call( T* that ) {
    return coro_call0(that,(t_do_process)&T::do_process);
  }

  void chkinp( void ) { if( __builtin_expect(inpptr>=inpend,0) ) yield(this,1); }

  void chkout( uint d=0 ) { if( __builtin_expect(outptr>=outend-d,0) ) yield(this,2); }

  uint get( void ) { 
    //chkinp(); 
    if( __builtin_expect(inpptr>=inpend,0) ) {
      do {
        if( f_quit ) return uint(-1);
        yield(this,1);
      } while( inpptr>=inpend );
    }
    return *inpptr++; 
  }

  void put( uint c ) { *outptr++ = c; chkout(); }

  byte get0( void ) { return *inpptr++; }
  void put0( uint c ) { *outptr++ = c; }

  void coro_init( void ) {
    inpptr=inpbeg=inpend = 0;
    outptr=outbeg=outend = 0;
    state = 0; f_quit=0;
  }

  uint getinplen() { return inpend-inpptr; }
  uint getoutlen() { return outend-outptr; }

  uint getinpleft() { return inpend-inpptr; } //-V524
  uint getinpsize() { return inpptr-inpbeg; }
  uint getoutleft() { return outend-outptr; } //-V524
  uint getoutsize() { return outptr-outbeg; }

  void addinp( byte* inp,uint inplen ) {
    inpbeg = inpptr = inp;
    inpend = &inp[inplen];
  }

  void addout( byte* out,uint outlen ) {
    outbeg = outptr = out;
    outend = &out[outlen];
  }
/*
  #define ptr inpptr
  #include "coro2_getstr.inc"
  #undef ptr
  #define ptr outptr
  #include "coro2_putstr.inc"
  #undef ptr
*/
};

NOINLINE
void yield( void* p, int value ) { 
  Coroutine& q = *(Coroutine*)p;
  char curtmp; q.stkptrL=(&curtmp)-16;
  if( setjmp(q.PointB)==0 ) { 
    q.state=value; 
    memcpy( q.stk, (char*)q.stkptrL, q.stkptrH-q.stkptrL );
    longjmp(q.PointA,1); 
    __assume(0);
  }
}

#endif // CORO2B_INCLUDED