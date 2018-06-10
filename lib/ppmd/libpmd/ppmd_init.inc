#ifndef PPMD_INIT_INCLUDED
#define PPMD_INIT_INCLUDED

static const int MAX_O=ORealMAX;  // maximum allowed model order

//enum { FALSE=0,TRUE=1 };

template <class T> 
  T CLAMP( const T& X, const T& LoX, const T& HiX ) { return (X >= LoX)?((X <= HiX)?(X):(HiX)):(LoX); }

template <class T>
  void SWAP( T& t1, T& t2 ) { T tmp=t1; t1=t2; t2=tmp; }

void PrefetchData(void* Addr) {
  byte Prefetchbyte = *(volatile byte*)Addr;
}

enum { 
  UP_FREQ     = 5
};

byte Indx2Units[N_INDEXES];
byte Units2Indx[128]; // constants

byte NS2BSIndx[256];
byte QTable[260]; 


// constants initialization
void PPMD_STARTUP( void ) {
  int i, k, m, Step;

  for( i=0,k=1; i<N1         ;i++,k+=1 ) Indx2Units[i]=k;
  for( k++;     i<N1+N2      ;i++,k+=2 ) Indx2Units[i]=k;
  for( k++;     i<N1+N2+N3   ;i++,k+=3 ) Indx2Units[i]=k;
  for( k++;     i<N1+N2+N3+N4;i++,k+=4 ) Indx2Units[i]=k;

  for( k=0,i=0; k<128; k++ ) {
    i += Indx2Units[i]<k+1;
    Units2Indx[k]=i;
  }

  NS2BSIndx[0] = 2*0; //-V525
  NS2BSIndx[1] = 2*1;
  NS2BSIndx[2] = 2*1;
  memset(NS2BSIndx+3,  2*2, 26);             
  memset(NS2BSIndx+29, 2*3, 256-29);

  for( i=0; i<UP_FREQ; i++ ) QTable[i]=i;

  for( m=i=UP_FREQ, k=Step=1; i<260; i++ ) {
    QTable[i] = m;
    if( !--k ) k = ++Step, m++;
  }
}

#endif // PPMD_INIT_INCLUDED