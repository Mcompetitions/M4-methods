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
 * ll_el.hpp
 *
 *  Created on: Jan 11, 2017
 *      Author: nico
 *
 *  a linked list element: quadruple <ab, P_ab, L_ab, F_ab>
 *
 */

#include <cassert>
#include <functional>

#ifndef INTERNAL_LL_EL_HPP_
#define INTERNAL_LL_EL_HPP_
/*
 * define hash functions for pairs of (32-bits/64-bits) integers
 */
namespace std {

template <>
struct hash<pair<uint32_t,uint32_t> >{

	std::size_t operator()(const pair<uint32_t,uint32_t>& k) const{

		return hash<uint32_t>()(k.first) ^ hash<uint32_t>()(k.second);

	}

};

template <>
struct hash<pair<uint64_t,uint64_t> >{

	std::size_t operator()(const pair<uint64_t,uint64_t>& k) const{

		return hash<uint64_t>()(k.first) ^ hash<uint64_t>()(k.second);

	}

};

} // of std

namespace Rp {

using namespace std;

template<typename itype = uint32_t>
struct triple{

public:

	triple(){}

	triple(itype P_ab, 	itype L_ab,	itype F_ab){

		this->P_ab = P_ab;
		this->L_ab = L_ab;
		this->F_ab = F_ab;

	}

	bool operator==(triple<itype> t){

		return t.P_ab == P_ab and t.L_ab == L_ab and t.F_ab == F_ab;

	}

	bool operator!=(triple<itype> t){

		return not operator==(t);

	}

	itype P_ab=0;
	itype L_ab=0;
	itype F_ab=0;

};

/*
 * template on the char type and on the integer type
 */
template<typename ctype = uint32_t, typename itype = uint32_t>
class ll_el{

public:

using int_type = itype;
using char_type = ctype;

using cpair = pair<ctype,ctype>;

	ll_el(){}

	ll_el(cpair ab){

		this->ab = ab;

		F_ab = 0;
		L_ab = 0;

	}

	ll_el(cpair ab, itype P_ab, itype L_ab, itype F_ab){

		this->ab = ab;
		this->P_ab = P_ab;
		this->L_ab = L_ab;
		this->F_ab = F_ab;

	}

	bool operator<(ll_el &e1) const {
		return F_ab < e1.F_ab;
	}

	bool is_null(){
		return F_ab==null;
	}

	//this value is reserved to indicate NULL elements/pointers
	const static itype null = ~itype(0);

	cpair ab;
	itype P_ab = 0;
	itype L_ab = 0;
	itype F_ab = null;

};

typedef ll_el<uint32_t,uint32_t> ll_el32_t;
typedef ll_el<uint64_t,uint64_t> ll_el64_t;

} // of Rp

#endif /* INTERNAL_LL_EL_HPP_ */
