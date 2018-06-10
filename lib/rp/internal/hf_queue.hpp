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
 * hf_queue_v2.hpp
 *
 *  Created on: Jan 12, 2017
 *      Author: nico
 *
 *  High-frequency pairs queue implemented by direct-addressing pairs
 *
 *  Supported operations:
 *
 *  operator[ab]: return triple <P_ab, L_ab, F_ab> relative to pair ab
 *  max()/min(): return pair ab with max/min F_ab
 *  remove(ab): delete pair ab from queue
 *  contains(ab): true iff ab is in the queue
 *  size(): current queue size
 *  decrease(ab): decrease by 1 ab's frequency F_ab. This function removes ab if its frequency goes below the queue's min frequency
 *  insert(list_el), where list_el = <ab, P_ab, L_ab, F_ab> is a linked list element
 *
 *
 */

#include <ll_vec.hpp>
#include <ll_el.hpp>
#include <pair_hash.hpp>

#ifndef INTERNAL_HF_QUEUE_HPP_
#define INTERNAL_HF_QUEUE_HPP_

namespace Rp
{
  
using namespace std;

/*
 * template on linked list type and integer type
 */
template<typename el_type = ll_el32_t, typename itype = uint32_t, typename ctype = uint32_t>
class hf_queue{

public:

	using triple_t = triple<itype>;

	typedef pair_hash<triple_t,itype,ctype> hash_t;

	using cpair = pair<ctype,ctype>;

	/*
	 * default constructor. Note that object must be created with the other constructor in order to be
	 * usable (using object built with this constructor causes failed assertions)
	 */
	hf_queue(){

		min_freq = 0;

	}

	hf_queue(itype max_alphabet_size, itype min_freq) {

		assert(min_freq>1);

		this->min_freq = min_freq;

		H = hash_t(max_alphabet_size, triple_t());

	}

	void init(itype max_alphabet_size, itype min_freq) {

		assert(min_freq>1);

		this->min_freq = min_freq;

		H.init(max_alphabet_size, triple_t());

	}

	itype minimum_frequency(){
		return min_freq;
	}

	/*
	 * return triple <P_ab, L_ab, F_ab> relative to pair ab
	 * complexity: O(1)
	 */
	triple_t operator[](cpair ab){

		assert(ab != NULLPAIR);
		assert(contains(ab));

		return H[ab];

	}

	/*
	 * return pair with maximum frequency
	 */
	cpair max(){

		if(current_size==0) return NULLPAIR;

		itype max_F = 0;
		cpair max_pair = NULLPAIR;

		for(cpair p : pairs_in_hash){

			if(contains(p)){

				auto f = H[p].F_ab;

				if(f > max_F){

					max_F = f;
					max_pair = p;

				}

			}

		}

		assert(max_pair != NULLPAIR);
		return max_pair;

	}

	void remove(cpair ab){

		assert(contains(ab));
		assert(H[ab] != H.null_el());

		H.erase(ab);

		assert(not contains(ab));

		current_size--;

	}

	bool contains(cpair ab){

		assert(H.count(NULLPAIR) == 0);

		return H.count(ab) == 1;

	}

	/*
	 * current number of pairs in the queue
	 */
	itype size(){

		return current_size;

	}

	/*
	 * decrease by 1 F_ab. Does not remove pair!
	 *
	 */
	void decrease(cpair ab){

		assert(contains(ab));
		assert(H[ab] != H.null_el());

		assert(H[ab].F_ab > 0);

		H[ab].F_ab--;

	}

	void insert(el_type el){

		cpair ab = el.ab;

		assert(not contains(ab));
		assert(el.F_ab >= min_freq);

		H.insert({ab,triple_t {el.P_ab, el.L_ab, el.F_ab}});

		pairs_in_hash.push_back(ab);

		current_size++;
		peak_size = current_size > peak_size ? current_size : peak_size;


		//there is at least one pair in the queue (ab), so MAX and MIN must be defined
		assert(max() != NULLPAIR);
		assert(contains(max()));
		assert(H[ab].P_ab == el.P_ab);
		assert(H[ab].L_ab == el.L_ab);
		assert(H[ab].F_ab == el.F_ab);
		assert(contains(ab));

	}

	/*
	 * el must be already in the queue. update values for el
	 */
	void update(el_type el){

		cpair ab = el.ab;

		assert(contains(ab));
		assert(el.F_ab >= min_freq);

		H[ab].P_ab = el.P_ab;
		H[ab].L_ab = el.L_ab;
		H[ab].F_ab = el.F_ab;

		//there is at least one pair in the queue (ab), so MAX and MIN must be defined
		assert(max() != NULLPAIR);
		assert(contains(max()));

	}

	cpair nullpair(){
		return NULLPAIR;
	}

	/*
	 * return max number of stored pairs at any point in time
	 */
	itype peak(){
		return peak_size;
	}

private:

	itype min_freq;

	hash_t H;
	vector<cpair> pairs_in_hash;

	itype current_size=0;
	itype peak_size = 0;

	const itype null = ~itype(0);

	const cpair NULLPAIR = {null,null};

};

typedef hf_queue<ll_el32_t, uint32_t, uint32_t> hf_queue32_t;
typedef hf_queue<ll_el64_t, uint64_t, uint64_t> hf_queue64_t;

}

#endif /* INTERNAL_HF_QUEUE_HPP_ */
