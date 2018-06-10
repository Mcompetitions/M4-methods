#ifndef PPMD_PROC1_INCLUDED
#define PPMD_PROC1_INCLUDED

// encode in unmasked (maxorder) context
void processSymbol1( PPM_CONTEXT& q, int symbol ) {
  STATE* p = getStats(&q);

  int cnum  = q.NumStats;
  int i     = p[0].Symbol;
  int low   = 0;
  int freq  = p[0].Freq;
  int total = q.SummFreq;
  int flag;
//  int mode;
  int count;

  rc_Arrange( total );
  if( ProcMode ) {
    count = rc_GetFreq( total );
    flag  = count<freq;
  } else {
    flag  = i==symbol;
  }

  if( flag ) {

//    mode = 2;
    PrevSuccess = 0;//(2*freq>1*total);
    p[0].Freq  += 4;
    q.SummFreq += 4;

  } else {

    PrevSuccess = 0;

    for( low=freq,i=1; i<=cnum; i++ ) {
      freq = p[i].Freq;
      flag = ProcMode ? low+freq>count : p[i].Symbol==symbol;
      if( flag ) break;
      low += freq;
    }

//    mode = 2+1+flag;

    if( flag ) {
      p[i].Freq  += 4;
      q.SummFreq += 4;
      if( p[i].Freq > p[i-1].Freq ) SWAP( p[i], p[i-1] ), i--;
      p = &p[i];
    } else {
      if( q.iSuffix ) PrefetchData( suff(&q) );
      freq = total-low;
      NumMasked = cnum;
      for( i=0; i<=cnum; i++ ) CharMask[p[i].Symbol]=EscCount;
      p = NULL;
    }
  }

  rc_Process( low, freq, total );

  FoundState = p;
  if( p && (p[0].Freq>MAX_FREQ) ) FoundState=rescale(q,OrderFall,FoundState);
}

#endif // PPMD_PROC1_INCLUDED