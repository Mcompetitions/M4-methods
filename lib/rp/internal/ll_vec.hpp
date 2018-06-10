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
 * ll_vec.hpp
 *
 *  Created on: Jan 11, 2017
 *      Author: nico
 *
 *  A doubly-linked list of elements of some type t. Memory allocation is managed internally: all elements are stored contiguously.
 *  Type t must define internally a comparator <. The list supports the following operations:
 *
 *      pop(): remove and return the first element from the list. Complexity: O(1)
 *      popmax(): remove and return the max element from the list (uses the comparator). Complexity: linear
 *      min()/max(): return pair ab with the lowest/largest frequency F_ab
 *      insert(x) insert x on top of the list. Return a pointer (integer) to x. The pointer is relative and refers
 *                to allocated slots in the list. Complexity: O(1)
 *                if there isn't enough allocated space, we increase the allocated space by 50%: alloc' <- alloc * 3/2. In this case,
 *                relative pointers to elements in the list remain unchanged.
 *      operator[i] return object at position i. return NULL if no object is stored at position i. Complexity: O(1)
 *      remove(i) remove object at position i. Position i will contain NULL. Complexity: O(1)
 *      alloc() return number of allocated slots. Complexity: O(1)
 *      size() return number of slots actually filled with objects. Complexity: O(1)
 *      compact(): remove all NULL positions. Note that pointers to list's objects
 *                 change after this operation, so a scan is needed to retrieve the new pointers.
 *
 */

#include <vector>
#include "stdint.h"
#include <ll_el.hpp>
#include <cassert>


#ifndef INTERNAL_LL_VEC_HPP_
#define INTERNAL_LL_VEC_HPP_

namespace Rp
{

using namespace std;

template<typename t = ll_el32_t>
class ll_vec{

using itype = typename t::int_type;
using ctype = typename t::char_type;

using cpair = pair<ctype,ctype>;

public:

	using el_type = t;
	using int_type = itype;
	using char_type = ctype;

	/*
	 * constructor: initialize empty list, allocate memory for 1 element.
	 */
	ll_vec(){

		V = vector<t>(1);

		next_el = vector<itype>(1);
		prev_el = vector<itype>(1);

		next_el[0] = null;
		prev_el[0] = null;

		first_el = null;
		first_empty = 0;

		n = 0;

	}


	/*
	 * return reference to element stored at position i (could be null)
	 */
	t & operator[](itype i){

		assert(i < capacity());
		return V[i];

	}

	/*
	 * extract head of the list and return its corresponding pair
	 */
	cpair head(){

		assert(size()>0);
		assert(first_el != null);

		if(size()==0){

			cout << "error in ll_vec 105: size() == 0" << endl;exit(0);

		}

		t el = V[first_el];

		assert(not el.is_null());

		return el.ab;

	}

	/*
	 * linear-scan the array and retrieve pair with smallest F_ab
	 * if array is empty, return null pair
	 */
	cpair min_pair(){

		if(size()==0) return {null,null};

		assert(first_el != null);

		itype curr = first_el;
		itype next = next_el[curr];

		//minimum is the first at the beginning
		itype curr_m = curr;
		itype next_m = next;

		while(next != null){

			assert(not V[curr].is_null());

			//jump to next element
			curr = next;
			next = next_el[next];

			if(V[curr] < V[curr_m]){

				//new max

				curr_m = curr;
				next_m = next;

			}

		}

		//this is the min element
		t el = V[curr_m];

		assert(not el.is_null());

		return el.ab;

	}

	/*
	 * linear-scan the array and retrieve pair with largest F_ab
	 * if array is empty, return null pair
	 */
	cpair max_pair(){

		if(size()==0) return {null,null};

		assert(first_el != null);

		itype curr = first_el;
		itype next = next_el[curr];

		//minimum is the first at the beginning
		itype curr_m = curr;
		itype next_m = next;

		while(next != null){

			assert(not V[curr].is_null());

			//jump to next element
			curr = next;
			next = next_el[next];

			if(V[curr_m] < V[curr]){

				//new max

				curr_m = curr;
				next_m = next;

			}

		}

		//this is the min element
		t el = V[curr_m];

		assert(not el.is_null());

		return el.ab;

	}

	/*
	 * remove element at position i. note: i must not contain null
	 */
	void remove(itype i){

		assert(i<V.size());
		assert(size()>0);
		assert(not V[i].is_null());
		assert(n==0 or not V[first_el].is_null());
		assert(first_el == null || prev_el[first_el] == null);

		//insert null in this position
		V[i] = t();

		auto prev = prev_el[i]; //save temporarily previous element
		auto next = next_el[i]; //save temporarily next element

		//this position is now empty, so now it points to next empty position
		next_el[i] = first_empty;
		//first empty position becomes this position
		first_empty = i;

		assert(first_el == null || prev_el[first_el] == null);

		if(prev == null){

			assert(next != null || n==1);

			//in this case, i was the first element
			assert(first_el == i);

			first_el = next;

		}else{

			//if there is a predecessor, i cannot be the first element
			assert(first_el != i);

			//i has a predecessor
			next_el[prev] = next;

		}

		if(next != null)
			prev_el[next] = prev;

		assert(first_el == null || prev_el[first_el] == null);

		//decrease size;
		n--;

		first_el = n==0 ? null : first_el;

		assert(n>0 or first_el == null);
		assert(n==0 or not V[first_el].is_null());
		assert(first_el == null || prev_el[first_el] == null);


	}

	/*
	 * insert new element x. return (relative) pointer to position where x is stored.
	 */
	itype insert(t x){

		assert(n==0 or not V[first_el].is_null());
		assert(first_el == null || prev_el[first_el] == null);

		if(first_empty == null){ //if there is no space left

			//V must be full
			assert(size() == V.size());

			//re-allocate memory

			itype old_size = n;

			//increase size at least by 1
			itype new_size = n + (n/2==0?1:n/2);

			V.resize(new_size);
			V.shrink_to_fit();

			assert(V.size()==V.capacity());

			first_empty = old_size;

			assert(V[first_empty].is_null());

			next_el.resize(new_size);
			next_el.shrink_to_fit();

			prev_el.resize(new_size);
			prev_el.shrink_to_fit();

			for(itype i = old_size;i<new_size;++i){

				next_el[i] = i+1 == new_size ? null : i+1;
				prev_el[i] = i == old_size ? null : i-1;

			}

		}

		assert(first_empty != null);
		assert(V[first_empty].is_null());

		//now there is enough space to insert elements

		//modify pointers of empty elements
		auto insert_pos = first_empty;
		first_empty = next_el[insert_pos];

		assert(first_empty == null || V[first_empty].is_null());
		assert(V[insert_pos].is_null());

		//insert x
		V[insert_pos] = x;

		//modify pointers of full positions
		next_el[insert_pos] = first_el;
		prev_el[insert_pos] = null;

		if(first_el != null)
			prev_el[first_el] = insert_pos;

		first_el = insert_pos;

		n++;

		assert(insert_pos < V.size());

		assert(V[insert_pos].P_ab == x.P_ab);
		assert(V[insert_pos].L_ab == x.L_ab);
		assert(V[insert_pos].F_ab == x.F_ab);

		assert(operator[](insert_pos).P_ab == x.P_ab);
		assert(operator[](insert_pos).L_ab == x.L_ab);
		assert(operator[](insert_pos).F_ab == x.F_ab);

		assert(n==0 or not V[first_el].is_null());
		assert(first_el == null || prev_el[first_el] == null);

		return insert_pos;

	}

	/*
	 * return number of elements stored in the list
	 */
	itype size(){

		return n;

	}

	/*
	 * return number of allocated slots
	 */
	itype capacity(){

		assert(V.size() == 0 or V.capacity() == V.size());

		return V.size();

	}

	/*
	 * remove null positions, re-allocate memory. If size() == 0, leaves only 1 empty position.
	 */
	void compact(){

		if(n==0){

			V = vector<t>(1);

			next_el = vector<itype>(1);
			prev_el = vector<itype>(1);

			next_el[0] = null;
			prev_el[0] = null;

			first_el = null;
			first_empty = 0;

			n = 0;

		}else{

			{
				vector<t> tmp(n);

				auto cur_pos = first_el;

				itype i = 0;

				while(cur_pos != null){

					assert(i<n);

					tmp[i++] = V[cur_pos];
					cur_pos = next_el[cur_pos];

				}

				assert(i==n);

				V = vector<t>(tmp);

			}

			first_empty = null;
			first_el = 0;

			//shrink capacities of pointer vectors

			next_el.resize(n);
			std::vector<itype>(next_el).swap(next_el);

			prev_el.resize(n);
			std::vector<itype>(prev_el).swap(prev_el);

			assert(next_el.capacity() == next_el.size());
			assert(prev_el.capacity() == prev_el.size());

			for(itype i=0;i<n;++i){

				prev_el[i] = i==0 ? null : i-1;
				next_el[i] = i==n-1 ? null : i+1;

			}

		}

		assert(n == 0 or capacity()==size());

	}

private:

	vector<t> V;

	//elements are doubly chained: this permits to retrieve in constant time
	//empty spaces and elements from the set
	vector<itype> next_el;
	vector<itype> prev_el; //not used for null positions

	//this value is reserved to indicate NULL elements/pointers
	const itype null = ~itype(0);

	//number of elements stored
	itype n;

	itype first_el;
	itype first_empty;

};

typedef ll_vec<ll_el32_t> ll_vec32_t;
typedef ll_vec<ll_el64_t> ll_vec64_t;

} // of Rp

#endif /* INTERNAL_LL_VEC_HPP_ */
