#ifndef PPMD_UPDATE_INCLUDED
#define PPMD_UPDATE_INCLUDED

PPM_CONTEXT* saved_pc;

PPM_CONTEXT* UpdateModel( PPM_CONTEXT* MinContext ) {
  byte Flag, sym, FSymbol;
  uint ns1, ns, cf, sf, s0, FFreq;
  uint iSuccessor, iFSuccessor;
  PPM_CONTEXT* pc;
  STATE* p = NULL;

  FSymbol     = FoundState->Symbol;
  FFreq       = FoundState->Freq;
  iFSuccessor = FoundState->iSuccessor;

  // partial update for the suffix context
  if( MinContext->iSuffix ) {
    pc = suff(MinContext);
    // is it binary?
    if( pc[0].NumStats ) {
      p = getStats(pc);
      if( p[0].Symbol!=FSymbol ) {
        for( p++; p[0].Symbol!=FSymbol; p++ );
        if( p[0].Freq >= p[-1].Freq ) SWAP( p[0], p[-1] ), p--;
      }
      if( p[0].Freq<MAX_FREQ-3 ) {
        cf = 2 + (FFreq<28);
        p[0].Freq += cf;
        pc[0].SummFreq += cf;
      }
    } else { 
      p = &(pc[0].oneState());
      p[0].Freq += (p[0].Freq<14); 
    }
  }

  pc=MaxContext;

  // try increasing the order
  if( !OrderFall && iFSuccessor ) {
    FoundState->iSuccessor = CreateSuccessors( 1, p, MinContext );
    if( !FoundState->iSuccessor ) { saved_pc=pc; return 0; };
    MaxContext = getSucc(FoundState);
    return MaxContext;
  }

  *pText++ = FSymbol;
  iSuccessor = Ptr2Indx(pText);
  if( pText>=UnitsStart ) { saved_pc=pc; return 0; };

  if( iFSuccessor ) {
    if( (byte*)Indx2Ptr(iFSuccessor) < UnitsStart )
     iFSuccessor = CreateSuccessors( 0, p, MinContext );
    else
     PrefetchData( Indx2Ptr(iFSuccessor) );
  } else
    iFSuccessor = ReduceOrder( p, MinContext );

  if( !iFSuccessor ) { saved_pc=pc; return 0; };

  if( !--OrderFall ) {
    iSuccessor = iFSuccessor;
    pText -= (MaxContext!=MinContext);
  }

  s0 = MinContext->SummFreq - FFreq;
  ns = MinContext->NumStats;
  Flag = 0x08*(FSymbol>=0x40);
  for( pc=MaxContext; pc!=MinContext; pc=suff(pc) ) {
    ns1 = pc[0].NumStats;
    // non-binary context?
    if( ns1 ) {
      // realloc table with alphabet size is odd
      if( ns1&1 ) {
        p = (STATE*)ExpandUnits( getStats(pc),(ns1+1)>>1 );
        if( !p ) { saved_pc=pc; return 0; };
        pc[0].iStats = Ptr2Indx(p);
      }
      // increase escape freq (more for larger alphabet)
      pc[0].SummFreq += QTable[ns+4] >> 3;
    } else {
      // escaped binary context
      p = (STATE*)AllocUnits(1);
      if( !p ) { saved_pc=pc; return 0; };
      p[0] = pc[0].oneState();
      pc[0].iStats = Ptr2Indx(p);
      p[0].Freq = (p[0].Freq<=MAX_FREQ/3) ? (2*p[0].Freq-1) : (MAX_FREQ-15);
      // update escape
      pc[0].SummFreq = p[0].Freq + (ns>1) + ExpEscape[QTable[BSumm>>8]]; //-V602
    }

    // inheritance
    cf = (FFreq-1)*(5 + pc[0].SummFreq); 
    sf = s0 + pc[0].SummFreq;
    // this is a weighted rescaling of symbol's freq into a new context (cf/sf)
    if( cf<=3*sf ) {
      // if the new freq is too small the we increase the escape freq too
      cf = 1 + (2*cf>sf) + (2*cf>3*sf);
      pc[0].SummFreq += 4;
    } else {
      cf = 5 + (cf>5*sf) + (cf>6*sf) + (cf>8*sf) + (cf>10*sf) + (cf>12*sf);
      pc[0].SummFreq += cf;
    }

    p = getStats(pc) + (++pc[0].NumStats);  
    p[0].iSuccessor = iSuccessor;
    p[0].Symbol = FSymbol;
    p[0].Freq   = cf;
    pc[0].Flags |= Flag; // flag if last added symbol was >=0x40
  }

  MaxContext = (PPM_CONTEXT*)Indx2Ptr(iFSuccessor);
  return MaxContext;
}


uint CreateSuccessors( uint Skip, STATE* p, PPM_CONTEXT* pc ) {
  byte tmp;
  uint cf, s0;
  STATE*  ps[MAX_O];
  STATE** pps=ps;

  byte sym = FoundState->Symbol;
  uint iUpBranch = FoundState->iSuccessor;

  if( !Skip ) {
    *pps++ = FoundState;
    if( !pc[0].iSuffix ) goto NO_LOOP;
  }

  if( p ) { pc = suff(pc); goto LOOP_ENTRY; }

  do {
    pc = suff(pc);

    // increment current symbol's freq in lower order contexts
    // more partial updates?
    if( pc[0].NumStats ) {
      // find sym node
      for( p=getStats(pc); p[0].Symbol!=sym; p++ );
      // increment freq if limit allows
      tmp = 2*(p[0].Freq<MAX_FREQ-1);
      p[0].Freq += tmp;
      pc[0].SummFreq += tmp;
    } else {
      // binary context
      p = &(pc[0].oneState());
      p[0].Freq += (!suff(pc)->NumStats & (p[0].Freq<16));
    }

LOOP_ENTRY:
    if( p[0].iSuccessor!=iUpBranch ) {
      pc = getSucc(p);
      break;
    }
    *pps++ = p;
  } while ( pc[0].iSuffix );

NO_LOOP:
  if( pps==ps ) return Ptr2Indx(pc);

  // fetch a following symbol from the text buffer
  PPM_CONTEXT ct;
  ct.NumStats = 0;
  ct.Flags = 0x10*(sym>=0x40);
  sym = *(byte*)Indx2Ptr(iUpBranch);
  ct.oneState().iSuccessor = Ptr2Indx((byte*)Indx2Ptr(iUpBranch)+1);
  ct.oneState().Symbol = sym;
  ct.Flags |= 0x08*(sym>=0x40);

  // pc is MinContext, the context used for encoding
  if( pc[0].NumStats ) {
    for( p=getStats(pc); p[0].Symbol!=sym; p++ );
    cf = p[0].Freq - 1;
    s0 = pc[0].SummFreq - pc[0].NumStats - cf;
    cf = 1 + ((2*cf<s0) ? (12*cf>s0) : 2+cf/s0);
    ct.oneState().Freq = Min<uint>( 7, cf );
  } else {
    ct.oneState().Freq = pc[0].oneState().Freq;
  }

  // attach the new node to all orders
  do {
    PPM_CONTEXT* pc1 = (PPM_CONTEXT*)AllocContext();
    if( !pc1 ) return 0;
    ((uint*)pc1)[0] = ((uint*)&ct)[0];
    ((uint*)pc1)[1] = ((uint*)&ct)[1];
    pc1->iSuffix = Ptr2Indx(pc);
    pc = pc1; pps--;
    pps[0][0].iSuccessor = Ptr2Indx(pc);
  } while( pps!=ps );

  return Ptr2Indx(pc);
}


uint ReduceOrder( STATE* p, PPM_CONTEXT* pc ) {
  byte tmp;
  STATE* p1;
  PPM_CONTEXT* pc1=pc;
  FoundState->iSuccessor = Ptr2Indx(pText);
  byte sym = FoundState->Symbol;
  uint iUpBranch = FoundState->iSuccessor;
  OrderFall++;

  if( p ) { pc=suff(pc); goto LOOP_ENTRY; }

  while(1) {
    if( !pc->iSuffix ) return Ptr2Indx(pc);
    pc = suff(pc);

    if( pc->NumStats ) {
      for( p=getStats(pc); p[0].Symbol!=sym; p++ );
      tmp = 2*(p->Freq<MAX_FREQ-3);
      p->Freq += tmp;
      pc->SummFreq += tmp;
    } else { 
      p = &(pc->oneState());
      p->Freq += (p->Freq<11);
    }

LOOP_ENTRY:
    if( p->iSuccessor ) break;
    p->iSuccessor = iUpBranch;
    OrderFall++;
  }

  if( p->iSuccessor<=iUpBranch ) {
    p1 = FoundState;
    FoundState = p;
    p->iSuccessor = CreateSuccessors(0,0,pc);
    FoundState = p1;
  }

  if( OrderFall==1 && pc1==MaxContext ) {
    FoundState->iSuccessor = p->iSuccessor;
    pText--;
  }

  return p->iSuccessor;
}

#endif // PPMD_UPDATE_INCLUDED