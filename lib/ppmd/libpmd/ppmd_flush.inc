#ifndef PPMD_FLUSH_INCLUDED
#define PPMD_FLUSH_INCLUDED

NOINLINE
void StartModelRare( void ) {
  int i, k, s;
  byte i2f[25];

  memset( CharMask, 0, sizeof(CharMask) );
  EscCount=1;

  // we are in solid mode
  if( _MaxOrder<2 ) {
    OrderFall = _MaxOrder;
    for( PPM_CONTEXT* pc=MaxContext; pc->iSuffix!=0; pc=suff(pc) ) OrderFall--;
    return;
  }

  OrderFall = _MaxOrder;

  InitSubAllocator();

  InitRL = -( (_MaxOrder<13) ? _MaxOrder : 13 );
  RunLength = InitRL;

  // alloc and init order0 context
  MaxContext = (PPM_CONTEXT*)AllocContext();
  MaxContext->NumStats = 255;
  MaxContext->SummFreq = 255+2;
  MaxContext->iStats   = Ptr2Indx(AllocUnits(256/2));
  MaxContext->Flags    = 0;
  MaxContext->iSuffix  = 0;
  PrevSuccess          = 0;

  for( i=0; i<256; i++ ) {
    getStats(MaxContext)[i].Symbol     = i; 
    getStats(MaxContext)[i].Freq       = 1;
    getStats(MaxContext)[i].iSuccessor = 0;
  }

  // _InitSEE
  if( 1 ) {
    // a freq for quant?
    for( k=i=0; i<25; i2f[i++]=k+1 ) while( QTable[k]==i ) k++;

    // bin SEE init
    for( k=0; k<64; k++ ) {
      for( s=i=0; i<6; i++ ) s += EscCoef[2*i+((k>>i)&1)];
      s = 128*CLAMP( s, 32, 256-32 );
      for( i=0; i<25; i++ ) BinSumm[i][k] = BIN_SCALE - s/i2f[i];
    }

    // masked SEE init
    for( i=0; i<23; i++ ) for( k=0; k<32; k++ ) SEE2Cont[i][k].init(8*i+5);
  }
}


// model flush
NOINLINE
void RestoreModelRare( void ) {
  STATE* p; 
  pText = HeapStart;
  PPM_CONTEXT* pc = saved_pc;

  // from maxorder down, while there 2 symbols and 2nd symbol has a text pointer
  for(;; MaxContext=suff(MaxContext) ) {
   if( (MaxContext->NumStats==1) && (MaxContext!=pc) ) {
     p = getStats(MaxContext);
     if( (byte*)(getSucc(p+1))>=UnitsStart ) break;
   } else break;
    // turn a context with 2 symbols into a context with 1 symbol
    MaxContext->Flags = (MaxContext->Flags & 0x10) + 0x08*(p->Symbol>=0x40);
    p[0].Freq = (p[0].Freq+1) >> 1;
    MaxContext->oneState() = p[0];
    MaxContext->NumStats=0;
    FreeUnits( p, 1 );
  }

  // go all the way down
  while( MaxContext->iSuffix ) MaxContext=suff(MaxContext);

  AuxUnit = UnitsStart;

  ExpandTextArea();

  // free up 25% of memory
  do {
    PrepareTextArea();
    cutOff( MaxContext[0], 0, _MaxOrder ); // MaxContext is a tree root here, order0
    ExpandTextArea();
  } while( GetUsedMemory()>3*(SubAllocatorSize>>2) );

  GlueCount = GlueCount1 = 0;
  OrderFall = _MaxOrder;
}

#endif // PPMD_FLUSH_INCLUDED