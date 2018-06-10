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

#include "cfg2enc.h"

#define INLINE __inline

static void encodeCFG_rec(CODE code, EDICT *ed, BITOUT *output);

static void putLeaf(uint num_code, CODE lv_code, BITOUT *output);

static void putParen(uchar b, BITOUT *output);

static INLINE
uint bits(uint n) {
    uint b = 0;
    while (n) {
        b++;
        n >>= 1;
    }
    return b;
}

static INLINE
void putLeaf(uint num_code, uint lvcode, BITOUT *output) {
    uint bits_len = bits(num_code);
    writeBits(output, lvcode, bits_len);
}

static INLINE
void putParen(uchar b, BITOUT *output) {
    if (b == OP) {
        writeBits(output, OP, 1);
    } else {
        writeBits(output, CP, 1);
    }
}

static
void encodeCFG_rec(uint code, EDICT *ed, BITOUT *output) {
    if (ed->tcode[code] == DUMMY_CODE) {
        encodeCFG_rec(ed->rule[code].left, ed, output);
        encodeCFG_rec(ed->rule[code].right, ed, output);
        ed->tcode[code] = ++ed->newcode;
        putParen(CP, output);
    } else {
        putParen(OP, output);
        if (code < CHAR_SIZE) {
            putLeaf(ed->newcode, code, output);
        } else {
            putLeaf(ed->newcode, ed->tcode[code], output);
        }
    }
}

void EncodeCFG(EDICT *ed, std::stringstream &ost) {
    BITOUT *bitout;
    //printf("Encoding CFG ... ");
    ost.flush();
    //fflush(stdout);
    ed->newcode = CHAR_SIZE;
    ost.write((const char *) &ed->txt_len, sizeof(uint));
    ost.write((const char *) &ed->num_rules, sizeof(uint));
    bitout = createBitout(ost);
    encodeCFG_rec(ed->start, ed, bitout);
    putParen(CP, bitout);
    flushBitout(bitout);
    deleteBitout(bitout);
    //printf("Done!\n");
}

EDICT *ReadCFG(std::stringstream &ist) {
    uint i;
    uint num_rules, txt_len;
    EDICT *ed;
    RULE *rule;
    CODE *tcode;

    ist.read(reinterpret_cast<char *>(&txt_len), sizeof(uint));
    ist.read(reinterpret_cast<char *>(&num_rules), sizeof(uint));
    rule = new RULE[num_rules];

    //printf("num_rules = %d\n", num_rules);

    for (i = 0; i <= CHAR_SIZE; i++) {
        rule[i].left = (CODE) i;
        rule[i].right = DUMMY_CODE;
    }

    ist.read(reinterpret_cast<char *>(rule + CHAR_SIZE + 1), sizeof(RULE) * (num_rules - (CHAR_SIZE + 1)));

    tcode = new CODE[num_rules];
    for (i = 0; i <= CHAR_SIZE; i++) {
        tcode[i] = i;
    }
    for (i = CHAR_SIZE + 1; i < num_rules; i++) {
        tcode[i] = DUMMY_CODE;
    }

    ed = new EDICT;
    ed->txt_len = txt_len;
    ed->num_rules = num_rules;
    ed->start = num_rules - 1;
    ed->rule = rule;
    ed->tcode = tcode;
    ed->newcode = CHAR_SIZE;
    return ed;
}

void DestructEDict(EDICT *ed) {
    if (ed == NULL) return;
    if (ed->rule != NULL) delete[] ed->rule;
    if (ed->tcode != NULL) delete[] ed->tcode;
    delete ed;
}
