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
 * lf_queue.hpp
 *
 *  Created on: Jan 16, 2017
 *      Author: nico
 *
 *  Low-frequency pairs queue
 *
 *  This queue is a pair Q = <F,H> of structures, where:
 *
 *  - F: is a doubly-linked frequency vector indexing all possible pair frequencies smaller than
 *    a pre-defoined quantity. Each F's entry (frequency) is associated to a linked list containing all pairs
 *    with that frequency
 *
 *  - H: sigma x sigma -> int is a hash table pointing at elements in B
 *
 *  Supported operations (all amortized constant time)
 *
 *  operator[ab]: return triple <P_ab, L_ab, F_ab> relative to pair ab
 *  max(): return pair ab with max F_ab
 *  remove(ab): delete pair ab from queue
 *  contains(ab): true iff ab is in the queue
 *  size(): current queue size
 *  decrease(ab): decrease by 1 ab's frequency F_ab. This function removes ab if its frequency goes below the queue's min frequency
 *  insert(list_el), where list_el = <ab, P_ab, L_ab, F_ab> is a linked list element
 *
 *
 */

#ifndef INTERNAL_LF_QUEUE_HPP_
#define INTERNAL_LF_QUEUE_HPP_

#include <ll_vec.hpp>
#include <unordered_map>
#include <ll_el.hpp>
#include <algorithm>

namespace Rp
{

using namespace std;

/*
 * template on linked list type and integer type
 */
template<typename el_t = ll_el32_t>
class lf_queue{

public:

	using itype = typename el_t::int_type;
	using ctype = typename el_t::char_type;
	using el_type = el_t;

	using cpair = pair<ctype,ctype>;
	using ipair = pair<itype,itype>;

	struct h_el_t{

		itype P_ab;
		itype L_ab;
		itype F_ab;

	};

	//value of hash elements: pair <frequency, offset>. The element is accessed as F[frequency].list[offset]
	using hash_t = std::unordered_map<cpair, h_el_t>;
	//using hash_t = dense_hash_map<cpair, h_el_t>;

	using int_type = itype;
	using char_type = ctype;

	using triple_t = triple<itype>;

	/*
	 * default constructor. Note that object must be created with the other constructor in order to be
	 * usable (using object built with this constructor causes failed assertions)
	 */
	lf_queue(){

		max_size = 0;
		max_freq = 0;

	}

	lf_queue(itype max_freq) {

		assert(max_freq>0);

		this->max_freq = max_freq;
		//this->max_size = max_size;
		this->max_size = ~(itype(0)); //for now, unlimited max size

		//F = vector<ll_type>(max_freq+1);
		F = vector<vector<cpair> >(max_freq+1,vector<cpair>(0));
		F_idx = vector<itype>(max_freq+1,0);
		F_size = vector<itype>(max_freq+1,0);
		is_sorted = vector<bool>(max_freq+1,false);

		MAX = max_freq;

	}

	/*
	 * return triple <P_ab, L_ab, F_ab> relative to pair ab
	 * complexity: O(1)
	 */
	triple_t operator[](cpair ab){

		assert(max_size>0);
		assert(contains(ab));

		auto e = H[ab];
		return {e.P_ab, e.L_ab, e.F_ab};

	}

	triple_t at(cpair ab){

		return operator[](ab);

	}



	itype get_max_freq(){

		max();
		return MAX;

	}

	/*
	 * return pair with highest frequency. If there are only pairs with freq < 2, return null pair.
	 */
	cpair max(){

		if(MAX<2) return NULLPAIR;

		assert(max_size>0);
		assert(MAX<F.size());
		assert(MAX>1);

		cpair ab = NULLPAIR;

		while(ab == NULLPAIR and MAX > 1){

			assert(F_idx[MAX] <= F[MAX].size());

			if(F_idx[MAX] < F[MAX].size()){

				//if this is the first element of this list, sort the list
				if(F_idx[MAX] == 0 and not is_sorted[MAX]){

					std::sort(F[MAX].begin(),F[MAX].end(),
							[](const cpair & a, const cpair & b) -> bool
							{
							    return std::max(a.first,a.second) < std::max(b.first,b.second);
							});

					is_sorted[MAX] = true;

				}

				//if the pair is in the hash with this frequency, we found the MAX.
				cpair ab1 = F[MAX][F_idx[MAX]];

				if(contains(ab1) && at(ab1).F_ab == MAX){

					ab = ab1;

					assert(contains(ab));
					assert(ab != NULLPAIR);

				}else{//else: increment index in the list (pair is not in the hash OR it is but with a different frequency)

					F_idx[MAX]++;

				}

			}else{

				MAX--;

			}

		}

		if(MAX<2) return NULLPAIR;

		assert(ab != NULLPAIR);
		assert(contains(ab));
		assert(at(ab).F_ab == MAX);

		return ab;

	}

	void remove(cpair ab){

		assert(contains(ab));
		assert(max_size>0);

		h_el_t el = H[ab];
		H.erase(ab);//remove pair from hash

		assert(F_size[el.F_ab]>0);
		F_size[el.F_ab]--;

		assert(current_size>0);
		current_size--;

		//if more than half of B's entries are empty, compact B.
		if(F_size[el.F_ab] < F[el.F_ab].size()/2)
			compact_ll(el.F_ab);

	}

	bool contains(cpair ab){

		assert(max_size>0);

		return ab == NULLPAIR ? false : H.count(ab) == 1;

	}

	/*
	 * decrease by 1 F_ab
	 */
	void decrease(cpair ab){

		assert(contains(ab));
		assert(max_size>0);

		h_el_t & el = H[ab];

		assert(el.F_ab > 0);
		assert(el.F_ab < F.size());
		assert(F_size[el.F_ab]>0);

		F_size[el.F_ab]--;
		el.F_ab--; //decrease frequency
		F[el.F_ab].push_back(ab); //now insert the element in its list
		F_size[el.F_ab]++;

		assert(contains(ab));

		//if more than half of entries of old frequency are empty, compact list
		if(F_size[el.F_ab+1] < F[el.F_ab+1].size()/2)
			compact_ll(el.F_ab+1);

	}

	/*
	 * insert an element in the queue.
	 * Warning: this function assumes that element's frequency e.F_ab is
	 * already recorded in the internal linked lists
	 *
	 */
	void insert(el_t el){

		itype F_ab = el.F_ab;

		//must insert only pairs with frequency smaller than MAX
		assert(F_ab <= MAX);
		assert(F_ab < F.size());
		assert(not contains(el.ab));
		assert(max_size>0);

		F[F_ab].push_back(el.ab); //insert ab in its list
		F_size[F_ab]++;
		H.insert({el.ab,{el.P_ab,el.L_ab,F_ab}}); //insert ab in the hash

		current_size++;
		peak_size = current_size > peak_size ? current_size : peak_size;

		assert(F_ab >= 2);
		assert(at(el.ab).F_ab == F_ab);

	}

	itype minimum_frequency(){
		return 2;
	}

	/*
	 * el must be already in the queue. update P_ab and L_ab value for el
	 *
	 * note that el.F_ab must be the same of the frequency stored in the queue
	 *
	 */
	void update(el_t el){

		assert(contains(el.ab));
		assert(max_size>0);
		assert(el.F_ab < F.size());

		auto & e = H[el.ab];
		e.P_ab = el.P_ab;
		e.L_ab = el.L_ab;

		assert(e.F_ab == el.F_ab);
		assert(H[el.ab].P_ab == el.P_ab);
		assert(H[el.ab].L_ab == el.L_ab);
		assert(H[el.ab].F_ab == el.F_ab);
		assert(el.F_ab >= 2);

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

	/*
	 * compact memory used by list F[f]
	 */
	void compact_ll(itype f){

		assert(max_size>0);

		vector<cpair> new_list;

		for(auto ab : F[f]){

			if(contains(ab) && at(ab).F_ab == f){

				new_list.push_back(ab);

			}

		}

		assert(F_size[f] == new_list.size());

		F[f] = new_list;
		F_idx[f] = 0;

	}

	itype max_size = 0;
	itype max_freq = 0;

	itype current_size=0;
	itype peak_size = 0;

	itype MAX = 0;

	//vector<ll_type> F;

	vector<vector<cpair> > F;
	vector<itype> F_idx;
	vector<itype> F_size;
	vector<bool> is_sorted;

	hash_t H;

	const itype null = ~ctype(0);
	const cpair NULLPAIR = {null,null};
	const cpair VOIDPAIR = {null-1,null-1};

};

typedef lf_queue<ll_el32_t> lf_queue32_t;
typedef lf_queue<ll_el64_t> lf_queue64_t;

} // of Rp
  
#endif /* INTERNAL_LF_QUEUE_HPP_ */
