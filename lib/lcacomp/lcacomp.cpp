#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstream>
#include "txt2cfg_online.h"
#include "cfg2enc.h"
#include "lcacomp.h"

EDICT *convertDict(DICT *dict) {
    EDICT *edict = new EDICT;
    uint i;
    edict->txt_len = dict->txt_len;
    edict->start = dict->num_rules - 1;
    edict->rule = dict->rule;
    edict->num_rules = dict->num_rules;
    edict->tcode = dict->h_list;
    edict->newcode = CHAR_SIZE;

    for (i = 0; i <= CHAR_SIZE; i++) {
        edict->tcode[i] = i;
    }
    for (i = CHAR_SIZE + 1; i < dict->num_rules; i++) {
        edict->tcode[i] = DUMMY_CODE;
    }

    delete[] dict->h_entry;
    delete dict;
    return edict;
}

int compression(std::stringstream &ist, std::stringstream &ost) {
    DICT *dict;
    EDICT *edict;

    dict = GrammarTrans_LCA(ist);
    edict = convertDict(dict);

    EncodeCFG(edict, ost);
    DestructEDict(edict);

    return 1;
}

size_t Lcacomp::lcacomp_compress(const unsigned char *src, size_t size) {
    std::stringstream ist(std::ios_base::in | std::ios_base::out | std::ios::binary);
    ist.write(reinterpret_cast<const char *>(src), size);
    std::stringstream ost;
    compression(ist, ost);

    return ost.str().size();
}