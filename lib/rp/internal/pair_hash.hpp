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
 * pair_hash.hpp
 *
 *  Created on: Jan 25, 2017
 *      Author: nico
 *
 * direct-access hash H : sigma x sigma -> el_type
 *
 *
 */

#ifndef INTERNAL_PAIR_HASH_HPP_
#define INTERNAL_PAIR_HASH_HPP_

#include <vector>
#include <cassert>

namespace Rp
{

using namespace std;

template<typename el_type = uint32_t, typename itype = uint32_t, typename ctype = uint32_t>
class pair_hash{

	using cpair = pair<ctype,ctype>;

public:

	pair_hash(){};

	/*
	 * build a hash of size max_alphabet_size^2
	 */
	pair_hash(itype max_alphabet_size, el_type null_el){

		this->null = null_el;

		H = vector<vector<el_type> >(max_alphabet_size,vector<el_type>(max_alphabet_size,null_el));

	}

	void init(itype max_alphabet_size, el_type null_el){

		this->null = null_el;

		H = vector<vector<el_type> >(max_alphabet_size,vector<el_type>(max_alphabet_size,null_el));

	}

	el_type & operator[](cpair ab){

		assert(H.size()>0);
		assert(contains(ab));
		return H[ab.first][ab.second];

	}

	bool contains(cpair ab){

		assert(H.size()>0);
		return count(ab);

	}

	bool count(cpair ab){

		assert(H.size()>0);

		if(ab==nullpair) return false;

		assert(ab.first < H.size());
		assert(ab.second < H.size());

		return H[ab.first][ab.second] != null;

	}

	void insert(pair<cpair,el_type> p){

		assert(H.size()>0);

		cpair ab = p.first;
		el_type i = p.second;

		assert(ab != nullpair);

		assert(not contains(ab));
		assert(ab.first < H.size());
		assert(ab.second < H.size());

		H[ab.first][ab.second] = i;

	}

	void assign(pair<cpair,el_type> p){

		assert(H.size()>0);

		cpair ab = p.first;
		el_type i = p.second;

		assert(ab != nullpair);

		assert(contains(ab));
		assert(ab.first < H.size());
		assert(ab.second < H.size());

		H[ab.first][ab.second] = i;

	}

	void erase(cpair ab){

		assert(H.size()>0);
		assert(contains(ab));

		H[ab.first][ab.second] = null;

	}

	el_type null_el(){
		return null;
	}

private:

	el_type null = el_type();
	const ctype blank = ~itype(0);

	const cpair nullpair = {blank,blank};

	vector<vector<el_type> > H;


};

typedef pair_hash<uint32_t,uint32_t,uint32_t> pair_hash32_t;
typedef pair_hash<uint64_t,uint64_t,uint64_t> pair_hash64_t;

} // of Rp

#endif /* INTERNAL_PAIR_HASH_HPP_ */
