#ifndef PPM_BYTE_INCLUDED
#define PPM_BYTE_INCLUDED

uint ProcessByte( uint c ) {

  PPM_CONTEXT* MinContext = MaxContext;
  if( MinContext->NumStats ) {
    processSymbol1(   MinContext[0], c );
  } else {
    processBinSymbol( MinContext[0], c );
  }

  while( !FoundState ) {
    do {
//      if( !MinContext->iSuffix ) { return -1; };
      OrderFall++;
      MinContext = suff(MinContext);
    } while( MinContext->NumStats==NumMasked );
    processSymbol2( MinContext[0], c );
  }

  if( ProcMode ) c = FoundState->Symbol;

  PPM_CONTEXT* p;
  if( (OrderFall!=0) || ((byte*)getSucc(FoundState)<UnitsStart) ) {
    p = UpdateModel( MinContext );
    if( p ) MaxContext = p;
  } else {
    p = MaxContext = getSucc(FoundState);
  }

  if( p==0 ) {
    if( _CutOff ) {
      RestoreModelRare();
    } else {
      StartModelRare();
    }
  }

  return c;
}

#endif // PPM_BYTE_INCLUDED