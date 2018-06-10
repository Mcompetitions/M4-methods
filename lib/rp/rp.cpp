/*
 *  This file is part of Re-Pair.
 *  Copyright (c) by
 *  Nicola Prezza <nicola.prezza@gmail.com>, Philip Bille, and Inge Li Gørtz
 *
 *   Re-Pair is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.

 *   Re-Pair is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details (<http://www.gnu.org/licenses/>).
 *
 *
 * rp.cpp
 *
 *  Created on: Jan 11, 2017
 *      Author: nico
 */

#include <chrono>
#include <string>
#include <iostream>
#include <vector>
#include <set>
#include <stack>
#include <sstream>
#include <memory>
#include <cmath>

#include <lf_queue.hpp>

#include <fstream>
#include <memory.h>

#include "internal/packed_gamma_file3.hpp"
#include "internal/skippable_text.hpp"
#include "internal/text_positions.hpp"
#include "internal/hf_queue.hpp"
#include "rp.h"

namespace Rp {
    using namespace std;

//high/low frequency text type
    using text_t = skippable_text32_t;
    using TP_t = text_positions32_t;
    using hf_q_t = hf_queue32_t;
    using lf_q_t = lf_queue32_t;
    using itype = uint32_t;

    using cpair = hf_q_t::cpair;

    // All global variables from the original program are placed in this structure to enable multithreading.
    struct Globals {
        //next free dictionary symbol
        itype X = 0;
        itype last_freq = 0;
        itype n_distinct_freqs = 0;

        vector<itype> A; //alphabet (mapping int->ascii)
        vector<pair<itype, itype> > G; //grammar
        vector<itype> T_vec;// compressed text
    };

    void new_high_frequency_queue(hf_q_t &, TP_t &, text_t &, uint64_t);

    template<typename queue_t>
    void synchronize(queue_t &, TP_t &, text_t &, cpair);

    template<typename queue_t>
    void synchro_or_remove_pair(queue_t &, TP_t &, text_t &, cpair);

    /*
 * bit-width of x
 */
    uint64_t wd(uint64_t);

    /*
 * return frequency of replaced pair
 */
    template<typename queue_t>
    uint64_t substitution_round(queue_t &, TP_t &, text_t &, std::unique_ptr<Globals> &);

    void compute_repair(const unsigned char *, itype, std::unique_ptr<Globals> &);

    void decompress(vector<itype> &, vector<pair<itype, itype> > &, vector<itype> &, ofstream &);

    size_t rp_compress(const unsigned char *, size_t);
} // of Rp

/*
 * Given (empty) queue, text positions, text, and minimum frequency: insert in Q all pairs with frequency at least min_freq.
 *
 * assumptions: TP is sorted by character pairs, Q is void
 *
 */
void Rp::new_high_frequency_queue(hf_q_t &Q, TP_t &TP, text_t &T, uint64_t min_freq) {
    itype j = 0; //current position on TP
    itype n = TP.size();

    int old_perc = 0;
    int perc;

    itype n_pairs = 0;

    /*
     * step 1: count number of high-freq pairs
     */
    while (j < n) {
        itype k = 1; //current pair frequency
        while (j < TP.size() - 1 &&
               T.pair_starting_at(TP[j]) != T.blank_pair() &&
               T.pair_starting_at(TP[j]) == T.pair_starting_at(TP[j + 1])) {
            j++;
            k++;
        }

        if (k >= min_freq) {
            n_pairs++;
        }

        j++;
    }

    //largest possible dictionary symbol
    itype max_d = 256 + T.size() / min_freq;

    //create new queue. Capacity is number of pairs / min_frequency
    Q.init(max_d, min_freq);

    /*
     * step 2. Fill queue
     */
    j = 0;
    while (j < n) {
        itype P_ab = j; //starting position in TP of pair
        itype k = 1; //current pair frequency
        cpair ab = T.blank_pair();

        while (j < TP.size() - 1 &&
               T.pair_starting_at(TP[j]) != T.blank_pair() &&
               T.pair_starting_at(TP[j]) == T.pair_starting_at(TP[j + 1])) {
            ab = T.pair_starting_at(TP[j]);
            j++;
            k++;
        }

        if (k >= min_freq) {
            Q.insert({ab, P_ab, k, k});
        }

        j++;
    }
}


/*
 * synchronize queue in range corresponding to pair AB.
 */
template<typename queue_t>
void Rp::synchronize(queue_t &Q, TP_t &TP, text_t &T, cpair AB) {

    //variables associated with AB
    assert(Q.contains(AB));
    auto q_el = Q[AB];
    itype P_AB = q_el.P_ab;
    itype L_AB = q_el.L_ab;
    itype F_AB = q_el.F_ab;

    itype freq_AB = 0;//number of pairs AB seen inside the interval. Computed inside this function

    assert(P_AB + L_AB <= TP.size());
    //sort sub-array corresponding to AB
    TP.cluster(P_AB, P_AB + L_AB);
    assert(TP.is_clustered(P_AB, P_AB + L_AB));

    //scan TP[P_AB,...,P_AB+L_AB-1] and detect new pairs
    itype j = P_AB;//current position in TP
    while (j < P_AB + L_AB) {

        itype p = j; //starting position of current pair in TP
        itype k = 1; //current pair frequency

        cpair XY = T.pair_starting_at(TP[j]);

        while (j < (P_AB + L_AB) - 1 &&
               XY != T.blank_pair() &&
               XY == T.pair_starting_at(TP[j + 1])) {
            j++;
            k++;
        }

        freq_AB = XY == AB ? k : freq_AB;

        if (k >= Q.minimum_frequency()) {

            //if the pair is not AB and it is a high-frequency pair, insert it in queue
            if (XY != AB) {

                assert(XY != T.blank_pair());

                assert(not Q.contains(XY));

                Q.insert({XY, p, k, k});

                assert(TP.contains_only(p, p + k, XY));

            } else if (XY == AB) { //the pair is AB and is already in the queue: update its frequency

                assert(Q.contains(AB));
                Q.update({AB, p, k, k});

                assert(TP.contains_only(p, p + k, AB));

            }

        }

        j++;

    }

    assert(Q.contains(AB));

    //it could be that now AB's frequency is too small: delete it
    if (freq_AB < Q.minimum_frequency()) {

        Q.remove(AB);

    }

    assert(not Q.contains(AB) || Q[AB].F_ab == Q[AB].L_ab);

}


/*
 * look at F_ab and L_ab. Cases:
 *
 * 1. F_ab <= L_ab/2 and F_ab >= min_freq: synchronize pair. There could be new high-freq pairs in ab's list
 * 2. F_ab <= L_ab/2 and F_ab < min_freq: as above. This because there could be new high-freq pairs in ab's list.
 * 3. F_ab > L_ab/2 and F_ab >= min_freq: do nothing
 * 4. F_ab > L_ab/2 and F_ab < min_freq: remove ab. ab's list cannot contain high-freq pairs, so it is safe to lose references to these pairs.
 *
 */
template<typename queue_t>
void Rp::synchro_or_remove_pair(queue_t &Q, TP_t &TP, text_t &T, cpair ab) {

    assert(Q.contains(ab));

    auto q_el = Q[ab];
    itype F_ab = q_el.F_ab;
    itype L_ab = q_el.L_ab;

    if (F_ab <= L_ab / 2) {

        synchronize<queue_t>(Q, TP, T, ab);

    } else {

        if (F_ab < Q.minimum_frequency()) {
            Q.remove(ab);
        }
    }
}

uint64_t Rp::wd(uint64_t x) {
    auto w = 64 - __builtin_clzll(uint64_t(x));
    return x == 0 ? 1 : w;
}

const std::pair <Rp::itype, Rp::itype> nullpair = {~Rp::itype(0), ~Rp::itype(0)};

template<typename queue_t>
uint64_t Rp::substitution_round(queue_t &Q, TP_t &TP, text_t &T, std::unique_ptr<Globals> &g) {

    using ctype = text_t::char_type;

    //compute max
    cpair AB = Q.max();

    g->G.push_back(AB);

    assert(Q.contains(AB));
    assert(Q[AB].F_ab >= Q.minimum_frequency());

    //extract P_AB and L_AB
    auto q_el = Q[AB];
    itype F_AB = q_el.F_ab;
    itype P_AB = q_el.P_ab;
    itype L_AB = q_el.L_ab;

    uint64_t f_replaced = F_AB;

    g->n_distinct_freqs += (F_AB != g->last_freq);
    g->last_freq = F_AB;

    for (itype j = P_AB; j < P_AB + L_AB; ++j) {

        itype i = TP[j];

        if (T.pair_starting_at(i) == AB) {
            ctype A = AB.first;
            ctype B = AB.second;

            //the context of AB is xABy. We now extract AB's context:
            cpair xA = T.pair_ending_at(i);
            cpair By = T.next_pair(i);

            assert(xA == T.blank_pair() or xA.second == A);
            assert(By == T.blank_pair() or By.first == B);

            //note: xA and By could be blank pairs if this AB was the first/last pair in the text

            //perform replacement
            T.replace(i, g->X);

            assert(By == T.blank_pair() || T.pair_starting_at(i) == cpair(g->X, By.second));

            if (Q.contains(xA) && xA != AB) {
                Q.decrease(xA);
            }

            if (Q.contains(By) && By != AB) {
                Q.decrease(By);
            }
        }
    }

    /*
     * re-scan text positions associated to AB and synchronize if needed
     */
    for (itype j = P_AB; j < P_AB + L_AB; ++j) {

        itype i = TP[j];

        assert(T.pair_starting_at(i) != AB); //we replaced all ABs ...

        if (T[i] == g->X) {

            //the context of X is xXy. We now extract X's left (x) and right (y) contexts:
            cpair xX = T.pair_ending_at(i);
            cpair Xy = T.pair_starting_at(i);

            ctype A = AB.first;
            ctype B = AB.second;

            //careful: x and y could be = X. in this case, before the replacements this xX was equal to ABAB -> a BA disappeared
            ctype x = xX.first == g->X ? B : xX.first;
            ctype y = Xy.second == g->X ? A : Xy.second;

            //these are the pairs that disappeared
            cpair xA = xX == T.blank_pair() ? xX : cpair {x, A};
            cpair By = Xy == T.blank_pair() ? Xy : cpair {B, y};

            if (Q.contains(By) && By != AB) {

                synchro_or_remove_pair<queue_t>(Q, TP, T, By);

            }

            if (Q.contains(xA) && xA != AB) {

                synchro_or_remove_pair<queue_t>(Q, TP, T, xA);

            }

        }

    }

    assert(Q.contains(AB));
    synchronize<queue_t>(Q, TP, T, AB); //automatically removes AB since new AB's frequency is 0
    assert(not Q.contains(AB));

    //advance next free dictionary symbol
    g->X++;

    return f_replaced;
}


void Rp::compute_repair(const unsigned char *ifs, itype n, std::unique_ptr<Globals> &g) {
    /*
     * tradeoff between low-frequency and high-freq phase:
     *
     * - High-freq phase will use n^(2 - 2*alpha) words of memory and process approximately n^(1-alpha) pairs
     *   alpha should satisfy 0.5 < alpha < 1
     *
     * - Low-freq phase will use n^alpha words of memory
     *
     */
    double alpha = 0.66; // = 2/3

    /*
     * in the low-frequency pair processing phase, insert at most n/B elements in the hash
     */
    uint64_t B = 50;

    //itype n = ifs.str().size();
    itype sigma = 0; //alphabet size

    /*
     * Pairs with frequency greater than or equal to min_high_frequency are inserted in high-freq queue
     */
    itype min_high_frequency = 0;
    itype lf_queue_capacity = 0;

    /*
     * (1) INITIALIZE DATA STRUCTURES
     */

    const itype null = ~itype(0);

    vector<itype> char_to_int(256, null);

    assert(std::pow(n, alpha) < static_cast<double>(std::numeric_limits<itype>::max()));
    min_high_frequency = std::pow(n, alpha);// n^(alpha)

    min_high_frequency = min_high_frequency < 2 ? 2 : min_high_frequency;

    itype width = 64 - __builtin_clzll(uint64_t(n));

    //largest possible high-frequency dictionary symbol
    //at most max_d <= n high-freq dictionary symbols can be created; given that min. freq of
    //a high-freq dictionary symbol is f=min_high_frequency and that every new dictionary symbol
    //introduces a blank in the text, we have the inequality max_d * f <= n  <=> max_d <= n/f
    itype max_d = 256 + n / min_high_frequency;

    //initialize text and text positions
    text_t T(n);

    itype j = 0;

    unsigned char c;

    //while(ifs.get(c)){
    for (size_t i = 0; i < n; ++i) {
        c = ifs[i];
        if (char_to_int[uint8_t(c)] == null) {

            char_to_int[uint8_t(c)] = sigma;
            g->A.push_back(uint8_t(c));
            sigma++;

        }

        T.set(j++, char_to_int[uint8_t(c)]);

    }

    TP_t TP(&T, min_high_frequency);

    //next free dictionary symbol = sigma
    g->X = sigma;

    hf_q_t HFQ;
    new_high_frequency_queue(HFQ, TP, T, min_high_frequency);

    int last_perc = -1;
    uint64_t F = 0;//largest freq

    while (HFQ.max() != HFQ.nullpair()) {

        auto f = substitution_round<hf_q_t>(HFQ, TP, T, g);

        if (last_perc == -1) {

            F = f;

            last_perc = 0;

        } else {

            int perc = 100 - (100 * f) / F;

            if (perc > last_perc + 4) {
                last_perc = perc;
            }

        }

    }

    //T.compact(); //remove blank positions
    TP.fill_with_text_positions(); //store here all remaining text positions

    TP.cluster(); //cluster text positions by character pairs

    /*
     * scan sorted array of text positions and count frequencies
     *
     * in this phase, all pairs have frequency < min_high_frequency
     *
     * after counting, frequencies[f] is the number of pairs with frequency equal to f
     *
     */
    uint64_t n_lf_pairs = 0; //number of low-frequency pairs

    uint64_t f = 1;
    for (uint64_t i = 1; i < TP.size(); ++i) {

        if (T.pair_starting_at(TP[i]) == T.pair_starting_at(TP[i - 1])) {
            f++;
        } else {
            f = 1;
            n_lf_pairs++;
        }
    }

    lf_q_t LFQ(min_high_frequency - 1);

    f = 1;

    using el_t = lf_q_t::el_type;

    for (uint64_t i = 1; i < TP.size(); ++i) {

        if (T.pair_starting_at(TP[i]) == T.pair_starting_at(TP[i - 1])) {
            f++;
        } else {
            if (f > 1) {

                cpair ab = T.pair_starting_at(TP[i - 1]);

                assert(i >= f);
                itype P_ab = i - f;

                itype L_ab = f;

                itype F_ab = f;

                el_t el = {ab, P_ab, L_ab, F_ab};

                LFQ.insert(el);

            }
            f = 1;
        }
    }

    pair<itype, itype> replaced = {0, 0};

    last_perc = -1;
    uint64_t tl = T.number_of_non_blank_characters();

    while (LFQ.max() != LFQ.nullpair()) {

        auto f = substitution_round<lf_q_t>(LFQ, TP, T, g);

        int perc = 100 - (100 * T.number_of_non_blank_characters()) / tl;

        if (perc > last_perc + 4) {

            last_perc = perc;
        }
    }

    for (itype i = 0; i < T.size(); ++i) {
        if (not T.is_blank(i)) g->T_vec.push_back(T[i]);
    }
}

void Rp::decompress(vector<itype> &A, vector<pair<itype, itype> > &G, vector<itype> &Tc, ofstream &ofs) {

    std::stack<itype> S;

    string buffer;
    int buf_size = 1000000;//1 MB buffer

    /*
     * decompress Tc symbols one by one
     */
    for (itype i = 0; i < Tc.size(); ++i) {

        S.push(Tc[i]);

        while (!S.empty()) {

            itype X = S.top(); //get symbol
            S.pop();//remove top

            if (X < A.size()) {

                unsigned char c = A[X];

                buffer.push_back(c);

                if (buffer.size() == buf_size) {

                    ofs.write(buffer.c_str(), buffer.size());
                    buffer = string();

                }

            } else {

                //expand rule: X -> ab
                auto ab = G[X - A.size()];

                S.push(ab.second);
                S.push(ab.first);

            }

        }

    }

    if (buffer.size() > 0) ofs.write(buffer.c_str(), buffer.size());

}

size_t Rp::rp_compress(const unsigned char *src, size_t size) {
    std::unique_ptr<Globals> g(new Globals);

    compute_repair(src, size, g);

    packed_gamma_file3<> out_file;

    return out_file.compress_and_store(g->A, g->G, g->T_vec);
}
