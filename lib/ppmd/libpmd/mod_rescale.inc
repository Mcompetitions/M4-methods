#ifndef MODEL_RESCALE_INCLUDED
#define MODEL_RESCALE_INCLUDED

STATE* rescale( PPM_CONTEXT& q, int OrderFall, STATE* FoundState ) {
  STATE tmp; STATE* p; STATE* p1;

  q.Flags &= 0x14;

  // move the current node to rank0
  p1 = getStats(&q);
  tmp = FoundState[0];
  for( p=FoundState; p!=p1; p-- ) p[0]=p[-1];
  p1[0] = tmp;

  int of = (OrderFall != 0);
  int a, i;
  int f0 = p->Freq;
  int sf = q.SummFreq;
  int EscFreq = sf-f0;
  q.SummFreq = p->Freq = (f0+of)>>1;

  // sort symbols by freqs
  for( i=0; i<q.NumStats; i++ ) {
    p++;
    a = p->Freq;
    EscFreq  -= a;
    a = (a+of)>>1;
    p->Freq = a; 
    q.SummFreq += a;
    if( a ) q.Flags |= 0x08*(p->Symbol>=0x40);
    if( a > p[-1].Freq ) {
      tmp = p[0];
      for( p1=p; tmp.Freq>p1[-1].Freq; p1-- ) p1[0]=p1[-1];
      p1[0] = tmp;
    }
  }

  // remove the zero freq nodes
  if( p->Freq==0 ) {
    for( i=0; p->Freq==0; i++,p-- );
    EscFreq += i;
    a = (q.NumStats+2) >> 1;
    if( (q.NumStats-=i)==0 ) {
      tmp = getStats(&q)[0];
      tmp.Freq = Min( MAX_FREQ/3, (2*tmp.Freq+EscFreq-1)/EscFreq );
      q.Flags &= 0x18;
      FreeUnits( getStats(&q), a );
      q.oneState() = tmp;
      FoundState = &q.oneState();
      return FoundState;
    }
    q.iStats = Ptr2Indx( ShrinkUnits(getStats(&q),a,(q.NumStats+2)>>1) );
  }

  // some weird magic
  q.SummFreq += (EscFreq+1) >> 1;
  if( OrderFall || (q.Flags & 0x04)==0 ) {
    a = (sf-=EscFreq) - f0;
    a = CLAMP( uint( ( f0*q.SummFreq - sf*getStats(&q)->Freq + a-1 ) / a ), 2U, MAX_FREQ/2U-18U );
  } else {
    a = 2;
  }

  (FoundState=getStats(&q))->Freq += a;
  q.SummFreq += a;
  q.Flags |= 0x04;

  return FoundState;
}

#endif // MODEL_RESCALE_INCLUDED