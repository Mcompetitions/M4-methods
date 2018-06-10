#ifndef PPMD_PROC2_INCLUDED
#define PPMD_PROC2_INCLUDED

SEE2_CONTEXT SEE2Cont[23][32];
SEE2_CONTEXT DummySEE2Cont;

// encode in masked context
void processSymbol2( PPM_CONTEXT& q, int symbol ) {
  byte px[256];
  STATE* p = getStats(&q);

  int c;
  int count;
  int low;
  int see_freq;
  int freq;
  int cnum = q.NumStats;

  SEE2_CONTEXT* psee2c;
  if( cnum != 0xFF ) {
    psee2c = SEE2Cont[ QTable[cnum+3]-4 ];
    psee2c+= (q.SummFreq > 10*(cnum+1));
    psee2c+= 2*(2*cnum < suff(&q)->NumStats+NumMasked) + q.Flags;
    see_freq = psee2c->getMean()+1;
//    if( see_freq==0 ) psee2c->Summ+=1, see_freq=1;
  } else { 
    psee2c = &DummySEE2Cont;
    see_freq = 1; 
  }

  int flag=0,pj,pl;

  int i,j;
  for( i=0,j=0,low=0; i<=cnum; i++ ) {
    c = p[i].Symbol; 
    if( CharMask[c]!=EscCount ) {
      CharMask[c]=EscCount;
      low += p[i].Freq;
      if( ProcMode ) 
        px[j++] = i;
      else
        if( c==symbol ) flag=1,j=i,pl=low;
    }
  }

  int Total = see_freq + low;

  rc_Arrange( Total );
  if( ProcMode ) {
    count = rc_GetFreq( Total );
    flag = count<low;
  }

  if( flag ) {
    if( ProcMode ) {
      for( low=0, i=0; (low+=p[j=px[i]].Freq)<=count; i++ );
    } else {
      low = pl;
    }
    p+=j;

    freq = p[0].Freq;

    if( see_freq>2 ) psee2c->Summ -= see_freq;
    psee2c->update();

    FoundState = p;
    p[0].Freq  += 4;
    q.SummFreq += 4; if( p[0].Freq > MAX_FREQ ) FoundState=rescale(q,OrderFall,FoundState);
    RunLength = InitRL;
    EscCount++;

  } else {

    low = Total;
    freq = see_freq;

    NumMasked  = cnum;
    psee2c->Summ += Total-see_freq;

  }

  rc_Process( low-freq, freq, Total );

}

#endif // PPMD_PROC2_INCLUDED