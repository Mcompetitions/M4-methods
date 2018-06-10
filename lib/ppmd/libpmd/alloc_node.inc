#ifndef ALLOC_NODE_INCLUDED
#define ALLOC_NODE_INCLUDED

struct _MEM_BLK { 
  uint Stamp;
  uint NextIndx;
  uint NU; 
};


struct BLK_NODE {
  uint Stamp;
  uint NextIndx;
  int avail() const { return (NextIndx!=0); }
};


BLK_NODE* getNext( BLK_NODE* This ) { 
  return (BLK_NODE*)Indx2Ptr(This->NextIndx); 
}

void setNext( BLK_NODE* This, BLK_NODE* p ) { 
  This->NextIndx = Ptr2Indx(p); 
}

void link( BLK_NODE* This, BLK_NODE* p ) { 
  p->NextIndx = This->NextIndx; 
  setNext( This, p ); 
}

void unlink( BLK_NODE* This ) { 
  This->NextIndx = getNext(This)->NextIndx; 
}

void* remove( BLK_NODE* This ) {
  BLK_NODE* p = getNext(This); 
  unlink(This);
  This->Stamp--;
  return p;
}

void insert( BLK_NODE* This, void* pv, int NU ) {
  BLK_NODE* p = (BLK_NODE*)pv;
  link(This,p);
  p->Stamp = ~uint(0);
  ((_MEM_BLK&)*p).NU = NU;
  This->Stamp++;
}


struct MEM_BLK : public BLK_NODE {
  uint NU; 
};

typedef BLK_NODE* pBLK_NODE;

typedef MEM_BLK* pMEM_BLK;

BLK_NODE BList[N_INDEXES+1];

#endif // ALLOC_NODE_INCLUDED