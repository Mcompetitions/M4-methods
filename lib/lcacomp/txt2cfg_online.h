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

#ifndef TXT2CFG_ONLINE_H
#define TXT2CFG_ONLINE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <iostream>
#include <sstream>
#include "lcacommon.h"

typedef struct Dictionary {
  uint txt_len;
  uint num_rules;
  RULE *rule;
  CODE *h_entry;
  CODE *h_list;
  uint rlbuf_len;
  uint p_idx;
  uint hebuf_len;
} DICT;

// function prototype declarations
DICT *GrammarTrans_LCA  (std::stringstream &ist);
void OutputGeneratedCFG (DICT *d, FILE *output);
void DestructDict       (DICT *d);

#endif /* TXT2CFG_ONLINE_H */
