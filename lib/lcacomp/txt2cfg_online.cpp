/* 
 *  Copyright (c) 2011-2012 Shirou Maruyama
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above Copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above Copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 */

#include "txt2cfg_online.h"

//#define DEBUG
#define INLINE __inline

#define INIT_RLBUF_LEN (256*1024) //initial size of buffer for rule[] 
#define RLBUF_RESIZE_FACTOR (1.5) //resize factor of buffer for rule[].
#define INIT_PRIMES_INDEX (13) //initial value of primes[].
#define HASH_LOAD_FACTOR (1.0) //load factor of hash entry.
#define INPUT_BUFFER_SIZE (32*1024)   // size of input buffer.
#define MAX_LEN_QUE (8)   // maximum length of queue.
#define MOD(X) ((X) % MAX_LEN_QUE) //compute index of que[].
#define hash_val(BUF_LEN, A, B) (((A)*(B))%BUF_LEN)
//#define hash_val(BUF_LEN, A, B) ((((A)<<16)|(B)>>16)%BUF_LEN)
//#define hash_val(BUF_LEN, A, B) ((A+(A<<16)^B)%BUF_LEN)

typedef struct Queue {
  CODE w[MAX_LEN_QUE];
  uint bpos;
  uint epos;
  uint num;
  struct Queue *next;
} QU;

//Inner function declaration.
static QU   *createQueue       ();
static void destructQueues     (QU *q);
static void enQueue            (QU *q, CODE c);
static CODE deQueue            (QU *q);
static CODE refQueue           (QU *q, uint i);
static bool isRepetition       (QU *q, uint i);
static bool isMinimal          (QU *q, uint i);
static uint computeLCAd        (CODE i, CODE j);
static bool isMaximal          (QU *q, uint i);
static bool isPair             (QU *q);
static void resizeDict         (DICT *d);
static void rehash             (DICT *d);
static CODE addRule2Dictionary (DICT *d, CODE left, CODE right);
static CODE searchRule         (DICT *d, CODE left, CODE right);
static CODE reverseAccess      (DICT *d, CODE left, CODE right);
static DICT *createDictionary  ();
static void grammarTrans_rec   (DICT *d, QU *q, CODE c);

static
QU *createQueue() {
  uint i;
  QU *q = new QU;

  for (i = 0; i < MAX_LEN_QUE; i++) {
    q->w[i] = DUMMY_CODE;
  }
  q->bpos = 0;
  q->epos = 0;
  q->num = 1;
  q->next = NULL;

  return q;
}

static INLINE
void enQueue(QU *q, CODE c)
{
  q->epos = MOD(q->epos+1);
  q->w[q->epos] = c;
  q->num++;
}

static INLINE
CODE deQueue(QU *q)
{
  register CODE x = q->w[q->bpos];

  q->bpos = MOD(q->bpos+1);
  q->num--;
  return x;
}

static INLINE
CODE refQueue(QU *q, uint i)
{
  return q->w[MOD(q->bpos+i)];
}

static
void destructQueues(QU *q)
{
  if (q == NULL) {
    return;
  }
  destructQueues(q->next);
  delete q;
}

static INLINE
bool isRepetition(QU *q, uint i)
{
  register CODE w1 = refQueue(q, i);
  register CODE w2 = refQueue(q, i+1);

  if (w1 == w2) {
    return true;
  }
  return false;
}

static INLINE
bool isMinimal(QU *q, uint i)
{
  register CODE w0 = refQueue(q, i-1);
  register CODE w1 = refQueue(q, i);
  register CODE w2 = refQueue(q, i+1);

  if ((w0 > w1) && (w1 < w2)) {
    return true;
  } else {
    return false;
  }
}

static INLINE
uint computeLCAd(CODE i, CODE j)
{
  register uint Ni, Nj;
  register uint x;

  Ni = 2*i - 1;
  Nj = 2*j - 1;
  x = Ni ^ Nj;
  x = (uint)floor(LOG2(x));
  return x;
}

static INLINE
bool isMaximal(QU *q, uint i)
{
  register CODE w0 = refQueue(q, i-1);
  register CODE w1 = refQueue(q, i);
  register CODE w2 = refQueue(q, i+1);
  register CODE w3 = refQueue(q, i+2);

  if (!(w0 < w1 && w1 < w2 && w2 < w3) && 
      !(w0 > w1 && w1 > w2 && w2 > w3))
    {
      return false;
    }
  
  if (computeLCAd(w1,w2) > computeLCAd(w0,w1) &&
      computeLCAd(w1,w2) > computeLCAd(w2,w3)) 
    {
      return true;
    } 
  else
    {
      return false;
    }
}

static INLINE
bool isPair(QU *q) {
  if (isRepetition(q, 1)) {
    return true;
  }
  else if (isRepetition(q, 2)) {
    return false;
  }
  else if (isRepetition(q, 3)) {
    return true;
  }
  else if (isMinimal(q, 1) || isMaximal(q, 1)) {
    return true;
  }
  else if (isMinimal(q, 2) || isMaximal(q, 2)) {
    return false;
  }
  return true;
}

static INLINE
void rehash(DICT *d)
{
  uint i;
  uint h;
  CODE temp;

  if ((d->hebuf_len = primes[++d->p_idx]) == 0) {
    puts("size of hash table is overflow.");
    exit(1);
  }
  d->h_entry = (CODE*)realloc(d->h_entry, d->hebuf_len*sizeof(CODE));
  if (d->h_entry == NULL) {
    puts("Memory reallocate error (h_entry) at rehash.");
    exit(1);
  }
  for (i = 0; i < d->hebuf_len; i++) {
    d->h_entry[i] = (uint)DUMMY_CODE;
  }
  for (i = CHAR_SIZE+1; i < d->num_rules; i++) {
    d->h_list[i] = DUMMY_CODE;
  }
  for (i = d->num_rules-1; i > CHAR_SIZE; i--) {
    h = hash_val(d->hebuf_len, d->rule[i].left, d->rule[i].right);
    temp = d->h_entry[h];
    d->h_entry[h] = i;
    if (temp != DUMMY_CODE) {
      d->h_list[i] = temp;
    }
  }
}

static INLINE
void resizeDict(DICT *d)
{
  d->rlbuf_len *= RLBUF_RESIZE_FACTOR;
  d->rule = (RULE*)realloc(d->rule, d->rlbuf_len*sizeof(RULE));
  if (d->rule == NULL) {
    puts("Memory reallocate error (rule) at resizeDict.");
    exit(1);
  }
  d->h_list = (CODE*)realloc(d->h_list, d->rlbuf_len*sizeof(CODE));
  if (d->h_list == NULL) {
    puts("Memory reallocate error (h_list) at resizeDict.");
    exit(1);
  } 
}

static INLINE
CODE addRule2Dictionary(DICT *d, CODE left, CODE right)
{
  CODE new_key = d->num_rules++;
  CODE temp;
  uint h;

  if (d->num_rules > d->rlbuf_len) {
    resizeDict(d);
  }
  if (d->num_rules > (uint)(d->hebuf_len*HASH_LOAD_FACTOR)) {
    rehash(d);
  }

  d->rule[new_key].left  = left;
  d->rule[new_key].right = right;

  if (new_key > DUMMY_CODE) {
    h = hash_val(d->hebuf_len, left, right);
    temp = d->h_entry[h];
    d->h_entry[h] = new_key;
    if (temp != DUMMY_CODE) {
      d->h_list[new_key] = temp;
    }
    else {
      d->h_list[new_key] = DUMMY_CODE;
    }
  }
  return new_key;
}

static INLINE
CODE searchRule(DICT *d, CODE left, CODE right)
{
  register CODE key;
  register uint h;

  h = hash_val(d->hebuf_len, left, right);
  key = d->h_entry[h];
  while (key != DUMMY_CODE) {
    if (d->rule[key].left == left && d->rule[key].right == right) {
      return key;
    } else {
      key = d->h_list[key];
    }
  }
  return DUMMY_CODE;
}

static INLINE
CODE reverseAccess(DICT *d, CODE left, CODE right)
{
  register CODE R;

  if ((R = searchRule(d, left, right)) == DUMMY_CODE) {
    R = addRule2Dictionary(d, left, right);
  }
  return R;
}

static
DICT *createDictionary()
{
  uint i;
  DICT *d;

  d = new DICT();
  d->rlbuf_len = INIT_RLBUF_LEN;
  d->p_idx = INIT_PRIMES_INDEX;
  d->hebuf_len = primes[d->p_idx];

  d->rule = new RULE[d->rlbuf_len]();
  if (d->rule == NULL) {
    puts("Memory allocate error (rule) at createDictionary.");
    exit(1);
  }
  d->h_entry = new CODE[d->hebuf_len]();
  if (d->h_entry == NULL) {
    puts("Memory allocate error (h_entry) at createDictionary.");
    exit(1);
  }
  for (i = 0; i < d->hebuf_len; i++) {
    d->h_entry[i] = DUMMY_CODE;
  }

  d->h_list = new CODE[d->rlbuf_len];
  if (d->h_list == NULL) {
    puts("Memory allocate error (h_list) at createDictionary.");
    exit(1);
  }
  for (i = 0; i < d->rlbuf_len; i++) {
    d->h_list[i] = DUMMY_CODE;
  }
  for (i = 0; i < CHAR_SIZE; i++) {
    addRule2Dictionary(d, i, DUMMY_CODE);
  }
  addRule2Dictionary(d, DUMMY_CODE, DUMMY_CODE);
  return d;
}

static INLINE
void grammarTrans_rec(DICT *d, QU *p, CODE c)
{
  QU *q;
  CODE v, x1, x2, x3;

  if (p->next == NULL) {
    q = p->next = createQueue();
  }
  else {
    q = p->next;
  }

  enQueue(q, c);
  if (q->num == MAX_LEN_QUE) {
    if (isPair(q) == true) {
      deQueue(q);
      x1 = deQueue(q); x2 = refQueue(q, 0);
      v = reverseAccess(d, x1, x2);
      grammarTrans_rec(d, q, v);
    }
    else {
      deQueue(q);
      x1 = deQueue(q);
      grammarTrans_rec(d, q, x1);
      x2 = deQueue(q); x3 = refQueue(q, 0);
      v = reverseAccess(d, x2, x3);
      grammarTrans_rec(d, q, v);
    }
  }
}

DICT *GrammarTrans_LCA(std::stringstream &ist)
{
  uchar *w, *w_top;
  uint lg;
  QU *dummy_que, *que;
  CODE v, x1, x2;
  DICT *d;
  long cnt = 0;

  dummy_que = createQueue();
  d = createDictionary();

  //printf("Grammar Transforming ...\n");
  w_top = new uchar[INPUT_BUFFER_SIZE];
  while (true) {
    ist.read(reinterpret_cast<char *>(w_top), sizeof(uchar)*INPUT_BUFFER_SIZE);
    lg = ist.gcount();
    if (!(lg > 0)) {
      break;
    }
    cnt += lg;
    w = w_top;
    do {
      grammarTrans_rec(d, dummy_que, *w++);
    } while (--lg > 0);
    //printf("\r");
    //printf("[ %12ld ] bytes -> [ %10d ] rules.", cnt, d->num_rules);
    //fflush(stdout);
  }
  d->txt_len = cnt;

  que = dummy_que->next;
  while (que->next != NULL || que->num > 2) {
    deQueue(que);
    while (que->num > 1) {
      x1 = deQueue(que); x2 = deQueue(que);
      v  = reverseAccess(d, x1, x2);
      grammarTrans_rec(d, que, v);
    }
    if (que->num == 1) {
      x1 = deQueue(que);
      grammarTrans_rec(d, que, x1);
    }
    que = que->next;
  }

  //printf("\r");
  //printf("[ %12ld ] bytes -> [ %10d ] rules.\n", cnt, d->num_rules);
  delete[] w_top;
  destructQueues(dummy_que);
  return d;
}

void OutputGeneratedCFG(DICT *d, FILE *output)
{
  fwrite(&d->txt_len, sizeof(uint), 1, output);
  fwrite(&d->num_rules, sizeof(uint), 1, output);
  fwrite(d->rule+CHAR_SIZE+1, sizeof(RULE), 
	 d->num_rules-(CHAR_SIZE+1), output);
}

void DestructDict(DICT *d)
{
  if (d == NULL) return;
  if (d->rule    != NULL) delete[] d->rule;
  if (d->h_entry != NULL) delete[] d->h_entry;
  if (d->h_list  != NULL) delete[] d->h_list;
  delete d;
}
