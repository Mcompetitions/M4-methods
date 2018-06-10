#ifndef MOD_CUTOFF_INCLUDED
#define MOD_CUTOFF_INCLUDED

void AuxCutOff( STATE* p, int Order, int MaxOrder ) {
  if( Order<MaxOrder ) {
    PrefetchData( getSucc(p) );
    p->iSuccessor = cutOff( getSucc(p)[0], Order+1,MaxOrder);
  } else {
    p->iSuccessor=0;
  }
}

uint cutOff( PPM_CONTEXT& q, int Order, int MaxOrder ) {
  int i, tmp, EscFreq, Scale;
  STATE* p;
  STATE* p0;

  // for binary context, just cut off the successors
  if( q.NumStats==0 ) {

    int flag = 1;
    p = &q.oneState();
    if( (byte*)getSucc(p) >= UnitsStart ) {
      AuxCutOff( p, Order, MaxOrder );
      if( p->iSuccessor || Order<O_BOUND ) flag=0;
    }
    if( flag ) {
      FreeUnit( &q );
      return 0;
    }

  } else {

    tmp = (q.NumStats+2)>>1;
    p0 = (STATE*)MoveUnitsUp(getStats(&q),tmp);
    q.iStats = Ptr2Indx(p0);

    // cut the branches with links to text
    for( i=q.NumStats, p=&p0[i]; p>=p0; p-- ) {
      if( (byte*)getSucc(p) < UnitsStart ) {
        p[0].iSuccessor=0;
        SWAP( p[0], p0[i--] );
      } else AuxCutOff( p, Order, MaxOrder );
    }

    // if something was cut
    if( i!=q.NumStats && Order>0 ) {
      q.NumStats = i;
      p = p0;
      if( i<0 ) { 
        FreeUnits( p, tmp );
        FreeUnit( &q );
        return 0;
      } 
      if( i==0 ) {
        q.Flags = (q.Flags & 0x10) + 0x08*(p[0].Symbol>=0x40);
        p[0].Freq = 1+(2*(p[0].Freq-1))/(q.SummFreq-p[0].Freq);
        q.oneState() = p[0];
        FreeUnits( p, tmp );
      } else {
        p = (STATE*)ShrinkUnits( p0, tmp, (i+2)>>1 );
        q.iStats = Ptr2Indx(p);
        Scale = (q.SummFreq>16*i); // av.freq > 16
        q.Flags = (q.Flags & (0x10+0x04*Scale));
        if( Scale ) {
          EscFreq = q.SummFreq;
          q.SummFreq = 0;
          for( i=0; i<=q.NumStats; i++ ) {
            EscFreq  -= p[i].Freq;
            p[i].Freq = (p[i].Freq+1)>>1;
            q.SummFreq += p[i].Freq;
            q.Flags |= 0x08*(p[i].Symbol>=0x40);
          };
          EscFreq = (EscFreq+1)>>1;
          q.SummFreq += EscFreq;
        } else {
          for( i=0; i<=q.NumStats; i++ ) q.Flags |= 0x08*(p[i].Symbol>=0x40);
        }
      }
    }

  }

  if( (byte*)&q==UnitsStart ) {
    // if this is a root, copy it
    UnitsCpy( AuxUnit, &q, 1 );
    return Ptr2Indx(AuxUnit);
  } else {
    // if suffix is root, switch the pointer
    if( (byte*)suff(&q)==UnitsStart ) q.iSuffix=Ptr2Indx(AuxUnit);
  }

  return Ptr2Indx(&q);
}

#endif // MOD_CUTOFF_INCLUDED