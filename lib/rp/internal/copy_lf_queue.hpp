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
 *  max()/min(): return pair ab with max/min F_ab
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

namespace Rp {
	using namespace std;

	template<typename ll_type = ll_vec32_t>
	struct f_vec_el {

		using itype = typename ll_type::int_type;

		const static itype null = ~itype(0);

		//each F's element is associated to a frequency f: F[f] = this element

		//linked list's pointers
		itype prev = null;
		itype next = null;

		//list corresponding to frequency f
		ll_type list;

	};

/*
 * template on linked list type and integer type
 */
	template<typename ll_type = ll_vec32_t>
	class lf_queue {

		using itype = typename ll_type::int_type;
		using ctype = typename ll_type::char_type;

		using cpair = pair<ctype, ctype>;
		using ipair = pair<itype, itype>;

//value of hash elements: pair <frequency, offset>. The element is accessed as F[frequency].list[offset]
		using hash_t = std::unordered_map<cpair, ipair>;

	public:

		using int_type = itype;
		using char_type = ctype;

		using triple_t = triple<itype>;
		using el_type = typename ll_type::el_type;

		/*
         * default constructor. Note that object must be created with the other constructor in order to be
         * usable (using object built with this constructor causes failed assertions)
         */
		lf_queue() {

			max_size = 0;
			max_freq = 0;

		}

		lf_queue(itype max_size, itype max_freq) {

			assert(max_freq > 0);

			this->max_freq = max_freq;
			this->max_size = max_size;

			F = vector<f_vec_el<ll_type> >(max_freq + 1);
			H = hash_t(max_size * 2);

		}

		/*
         * return triple <P_ab, L_ab, F_ab> relative to pair ab
         * complexity: O(1)
         */
		triple_t operator[](cpair ab) {

			assert(check_hash_consistency());

			assert(max_size > 0);
			assert(H[ab].second < F[H[ab].first].list.capacity());

			assert(contains(ab));

			assert(check_hash_consistency());

			auto coord = H[ab];

			auto freq = coord.first;
			auto offset = coord.second;

			assert(freq < F.size());
			assert(offset < F[freq].list.capacity());
			assert(not F[freq].list[offset].is_null());

			auto e = F[freq].list[offset];

			assert(e.F_ab == freq);

			return {e.P_ab, e.L_ab, e.F_ab};

		}

		cpair min() {

			assert(check_hash_consistency());

			assert(max_size > 0);
			assert(MIN < F.size());
			assert(F[MIN].list.size() > 0);

			auto ab = F[MIN].list.head();

			assert(H[ab].second < F[H[ab].first].list.capacity());
			assert(contains(ab));
			assert(check_hash_consistency());

			return ab;

		}

		cpair max() {

			assert(check_hash_consistency());

			assert(max_size > 0);
			assert(MAX < F.size());
			assert(F[MAX].next == null);
			assert(F[MAX].list.size() > 0);

			auto ab = F[MAX].list.head();

			assert(H[ab].second < F[H[ab].first].list.capacity());
			assert(contains(ab));

			assert(F[H[ab].first].list[H[ab].second].ab == ab);
			assert(not F[H[ab].first].list[H[ab].second].is_null());

			assert(check_hash_consistency());

			return ab;

		}

		void remove(cpair ab) {

			assert(check_hash_consistency());

			assert(contains(ab));
			assert(max_size > 0);
			assert(H[ab].second < F[H[ab].first].list.capacity());

			assert(MIN < F.size());
			assert(MAX < F.size());

			auto coord = H[ab];

			auto freq = coord.first;
			auto offset = coord.second;

			assert(freq < F.size());
			assert(offset < F[freq].list.capacity());
			assert(not F[freq].list[offset].is_null());
			assert(F[freq].list[offset].ab == ab);

			assert(check_hash_consistency());

			F[freq].list.remove(offset);
			//remove pair from hash
			H.erase(ab);

			assert(check_hash_consistency());

			//if more than half of B's entries are empty, compact B.
			if (F[freq].list.size() < F[freq].list.capacity() / 2) compact_ll(freq);

			//if the list is now empty
			if (F[freq].list.size() == 0) {

				auto prev = F[freq].prev;
				auto next = F[freq].next;

				if (prev == null) {

					//this frequency must be the first one (smallest)
					assert(MIN == freq);
					MIN = next;

				} else {

					assert(F[prev].next == freq);
					F[prev].next = next;

				}

				if (next == null) {

					//this freq must be the last one (largest)
					assert(MAX == freq);
					MAX = prev;

				} else {

					assert(F[next].prev == freq);
					F[next].prev = prev;

				}

			}

			assert(check_hash_consistency());

			assert(n > 0);

			//decrease size
			n--;

			assert(check_hash_consistency());

		}

		bool contains(cpair ab) {

			//assert(check_hash_consistency());

			assert(max_size > 0);
			//assert(H.count(ab) == 0 or H[ab].second < F[H[ab].first].list.capacity());

			return H.count(ab) == 1;

		}

		itype size() {

			assert(check_hash_consistency());

			assert(max_size > 0);

			return n;

		}

		/*
         * decrease by 1 F_ab
         */
		void decrease(cpair ab) {

			assert(check_hash_consistency());

			assert(contains(ab));
			assert(max_size > 0);
			assert(H[ab].second < F[H[ab].first].list.capacity());

			auto coord = H[ab];

			auto freq = coord.first;
			auto offset = coord.second;

			assert(freq > 1);
			assert(freq < F.size());
			assert(offset < F[freq].list.capacity());
			assert(not F[freq].list[offset].is_null());

			//copy element
			el_type e = el_type(F[freq].list[offset]);

			assert(ab == e.ab);
			assert(e.F_ab == freq);

			//store temporarily prev and next
			auto prev = F[freq].prev;
			auto next = F[freq].next;

			//remove pair ab from the queue as its frequency changed
			remove(ab);

			assert(not contains(ab));

			//decrease frequency
			e.F_ab--;

			assert(check_hash_consistency());

			//if ab's frequency becomes equal to 1, do not re-insert ab in the queue
			if (e.F_ab == 1) return;

			assert(check_hash_consistency());

			//now re-insert ab with his new frequency in the queue
			if (F[e.F_ab].list.size() == 0) {

				//there were no pairs with this frequency: add e.F_ab to linked list of frequencies

				if (F[freq].list.size() == 0) {

					//after deleting ab, its list disappeared: link e.F_ab to prev and next
					F[e.F_ab].prev = prev;
					F[e.F_ab].next = next;

					if (prev != null) {

						F[prev].next = e.F_ab;

					} else {

						//freq and freq-1 do not exist now and freq's prev was null:
						//this means that the MIN must be next (possibly,next == null)
						assert(MIN == next);

						//the new minimum is e.F_ab
						MIN = e.F_ab;

					}

					if (next != null) {

						F[next].prev = e.F_ab;

					} else {

						//freq and freq-1 do not exist now and freq's next was null:
						//this means that the MAX must be prev (possibly,prev == null)
						assert(MAX == prev);

						//the nex max is e.F_ab
						MAX = e.F_ab;

					}

				} else {

					//after deleting ab, its frequency remained: link e.F_ab to prev and freq
					F[e.F_ab].prev = prev;
					F[e.F_ab].next = freq;

					if (prev != null) {

						F[prev].next = e.F_ab;

					} else {

						//freq-1 does not exist, and prev = null: this means that
						//freq must be the minimum
						assert(MIN == freq);

						//now e.F_ab is the new minimum
						MIN = e.F_ab;

					}

					F[freq].prev = e.F_ab;

				}

			}//else: e.F_ab is already in the frequency linked list: just add e to its list

			//now insert the element in its list
			auto off = F[e.F_ab].list.insert(e);

			assert(off < F[e.F_ab].list.capacity());
			assert(not contains(ab));

			//update hash with new coordinates of the pair ab
			H.insert({ab, {e.F_ab, off}});

			//increase size counter ( it was decreased inside remove() )
			n++;

			assert(H[ab].second < F[H[ab].first].list.capacity());

			assert(check_hash_consistency());

		}

		/*
         * insert an element in the queue.
         * Warning: this function assumes that element's frequency e.F_ab is
         * already recorded in the internal linked lists
         *
         */
		void insert(el_type el) {

			assert(check_hash_consistency());

			itype f = el.F_ab;
			cpair ab = el.ab;

			assert(f < F.size());
			assert(not contains(ab));
			assert(max_size > 0);

			//if f does not have a predecessor, then f must be the minimum
			assert(F[f].prev != null or f == MIN);
			//if f does not have a successor, then f must be the maximum
			assert(F[f].next != null or f == MAX);

			assert(check_hash_consistency());

			//insert ab in its list
			auto off = F[f].list.insert(el);
			assert(off < F[f].list.capacity());

			assert(check_hash_consistency());

			//insert ab in the hash
			H.insert({ab, {f, off}});

			assert(check_hash_consistency());

			n++;

			assert(H[ab].second < F[H[ab].first].list.capacity());
			assert(n <= max_size);

			assert(check_hash_consistency());

		}

		/*
         * add a frequency at the end of the frequency's linked lists.
         * This frequency must be larger than all frequencies in the list
         * (or must be the first inserted frequency)
         */
		void push_back_frequency(itype f) {

			assert(check_hash_consistency());

			assert(f > 1);

			//either this is the first inserted frequency or it is larger than MIN
			assert(MIN == null or f > MIN);

			//either this is the first inserted frequency or it is larger than MAX
			assert(MIN == null or f > MAX);

			//this frequency must not have already been inserted
			assert(F[f].prev == null);
			assert(F[f].next == null);

			if (MIN == null) {

				//first frequency
				assert(MAX == null);

				MIN = f;
				MAX = f;

			} else {

				assert(MAX != null);

				F[MAX].next = f;
				F[f].prev = MAX;

				MAX = f;

			}

			assert(check_hash_consistency());

		}

		itype minimum_frequency() {
			return 2;
		}

		/*
         * el must be already in the queue. update values for el
         */
		void update(el_type el) {

			cpair ab = el.ab;

			assert(contains(ab));

			assert(check_hash_consistency());

			assert(contains(ab));
			assert(max_size > 0);
			assert(H[ab].second < F[H[ab].first].list.capacity());

			auto coord = H[ab];

			auto freq = coord.first;
			auto offset = coord.second;

			assert(freq == el.F_ab);

			assert(freq > 1);
			assert(freq < F.size());
			assert(offset < F[freq].list.capacity());
			assert(not F[freq].list[offset].is_null());

			//copy element
			F[freq].list[offset].L_ab = el.L_ab;

			assert(contains(min()));
			assert(contains(max()));

		}

	private:

		/*
         * compact memory used by linked list F[f] and re-compute
         * pair's indexes
         */
		void compact_ll(itype f) {

			assert(check_hash_consistency());

			assert(max_size > 0);

			F[f].list.compact();

			for (itype i = 0; i < F[f].list.size(); ++i) {

				assert(not F[f].list[i].is_null());

				auto ab = F[f].list[i].ab;

				assert(contains(ab));

				H[ab] = {f, i};

				assert(H[ab].second < F[H[ab].first].list.capacity());

			}

			assert(check_hash_consistency());

		}

		bool check_hash_consistency() {

			//decomment this line to enable the check
			//be aware that this check introduces a lot of overhead,
			//so run it only on small datasets
			return true;

			for (auto el : H) {

				auto p = el.first;
				if (H[p].second >= F[H[p].first].list.capacity()) {

					cout << "Failed pair: " << (char) p.first << (char) p.second << endl;
					cout << H[p].second << " / " << F[H[p].first].list.capacity() << endl;

					return false;

				}

			}

			return true;

		}

		//number of elements in the queue
		itype n = 0;

		itype max_size = 0;
		itype max_freq = 0;

		//min and max frequencies of pairs in the queue
		itype MIN = null;
		itype MAX = null;

		vector <f_vec_el<ll_type>> F;
		hash_t H;

		const static itype null = ~itype(0);

	};

	typedef lf_queue<ll_vec32_t> lf_queue32_t;
	typedef lf_queue<ll_vec64_t> lf_queue64_t;
} // of Rp

#endif /* INTERNAL_LF_QUEUE_HPP_ */

