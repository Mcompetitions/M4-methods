/*
 * packed_gamma_file3.hpp
 *
 *  Created on: Feb 15, 2017
 *      Author: nico
 *
 *  read/write integers in packed form in/from a file
 *
 */

#ifndef INTERNAL_PACKED_GAMMA_FILE3_HPP_
#define INTERNAL_PACKED_GAMMA_FILE3_HPP_

#include <cassert>
#include <vector>
#include <sstream>


#endif /* INTERNAL_PACKED_GAMMA_FILE3_HPP_ */

namespace Rp
{
using namespace std;

template<typename itype = uint32_t, uint64_t block_size = 6>
class packed_gamma_file3{

public:

	/*
	 * constructor
	 *
	 * input: file name and flag indicating whether file is used in write mode (true) or read mode (false)
	 *
	 */
	packed_gamma_file3(bool write = true) {

		this->write = write;

		if(write){

		  //out = ofstream(filename, std::ios::binary);
		  //out = std::cout;

		}else{

		  //in = ifstream(filename, std::ios::binary);
		  //in = std::cin;

			//read file into the bitvector bits
			fill_bits();
			fill_buffer();

		}

		lower_bound_bitsize = 0;
		actual_bitsize = 0;

	}

	/*
	 * append integer x in packed form to the file
	 */
	void push_back(uint64_t x){

		assert(write);

		buffer.push_back(x);

		if(buffer.size()%block_size == 0){

			uint8_t max_bitsize = 0;

			for(uint64_t i=buffer.size()-block_size;i<buffer.size();++i){

				max_bitsize = std::max(max_bitsize,wd(buffer[i]));

			}

			blocks_bitsizes.push_back(max_bitsize);

		}

		lower_bound_bitsize += wd(x);


	}

	/*
	 * read next integer from file.
	 * After EOF is reached, returns always 0
	 */
	uint64_t read(){

		assert(not write);

		if(eof()) return 0;

		return buffer[idx_in_buf++];

	}

	/*
	 * return true if there are no more characters to be read from the file
	 */
	bool eof(){

		assert(not write);

		return idx_in_buf >= buffer.size();

	}

	/*
	 * close file: must be called when there are no more integers to be written.
	 * this function compresses the content of buffer and saves it to file
	 */
	void close(size_t *out_size) {

		assert(write);

		//in this case we did not compute last bitsize
		if(buffer.size()%block_size != 0){

			uint8_t max_bitsize = 0;

			for(uint64_t i=(buffer.size()/block_size)*block_size;i<buffer.size();++i){

				max_bitsize = std::max(max_bitsize,wd(buffer[i]));

			}

			blocks_bitsizes.push_back(max_bitsize);

		}

		flush_to_file(out_size);

		//out.close();

	}

	uint64_t written_bytes(){

		return actual_bitsize/8;

	}

	uint64_t lower_bound_bytes(){

		return lower_bound_bitsize/8;

	}

	/*
	 * return % of overhead of the number of bits saved to file w.r.t.
	 * the cumulated bitlengths of the stored integers
	 */
	double overhead(){

		return 100*double(actual_bitsize-lower_bound_bitsize)/lower_bound_bitsize;

	}

	/*
	 * input: alphabet encoding A (ascii -> integers in {0,...,|A|-1}), grammar G (pairs) and compressed text T
	 *
	 * compress G and T and store them to file
	 *
	 * compression techniques:
	 *
	 * 	- the maximums of G's pairs form M increasing sequences; we compress them with gamma encoding
	 * 	- |G| bits to remember who was the max in each pair
	 * 	- M starting values for the M increasing sequences
	 * 	- gap-encoded bitvector to store the beginnings of the M increasing sequences
	 * 	- the minimums are stored without compression
	 *
	 * 	- TOTAL SIZE: A log A + G log G + G + 2M log (G/M) + G log M bits
	 *
	 */
	size_t compress_and_store(vector<itype> & A, vector<pair<itype,itype> > & G, vector<itype> & T){
		// Instead of store compressed data, simply compute it's size.
		size_t out_size = 0;

		//store A
		push_back(A.size());
		for(auto a : A) push_back(a);

		//store G
		vector<itype> deltas; //deltas between the pair's maximums
		vector<itype> starting_values; //starting values of each increasing sequence
		vector<itype> deltas_starting_points; //starting values of each increasing sequence
		vector<itype> deltas_minimums; //maximums - minimums
		vector<bool> max_first; //the max is the first in the pair. If false, the min is the first in the pair

		uint64_t last_max = 0;

		uint64_t last_incr_seq = 0; //index of last pair that started an increasing sequence
		uint64_t i = 0;//pair number

		for(auto ab: G){

			max_first.push_back(ab.first >= ab.second);

			uint64_t max = std::max(ab.first,ab.second);
			uint64_t delta_min = max - std::min(ab.first,ab.second);

			deltas_minimums.push_back(delta_min);

			if(max>=last_max){

				deltas.push_back(max-last_max);

			}else{

				//i-th pair starts a new increasing seq
				starting_values.push_back(max);
				deltas_starting_points.push_back(i-last_incr_seq);
				last_incr_seq = i;

			}

			last_max = max;
			i++;

		}

		push_back(deltas.size()); for(auto x:deltas) push_back(x);
		push_back(starting_values.size());for(auto x:starting_values) push_back(x);
		push_back(deltas_starting_points.size());for(auto x:deltas_starting_points) push_back(x);

		push_back(deltas_minimums.size());for(auto x:deltas_minimums) push_back(x);
		push_back(max_first.size());for(auto x:max_first) push_back(x);

		//store T
		push_back(T.size());
		for(auto a : T) push_back(a);

		close(&out_size);

		auto wr = written_bytes()*8;

		/*
		 * statistics
		 */

		uint64_t g = G.size();
		uint64_t t = T.size();
		uint64_t s = A.size();
		uint64_t log_s = (64 - __builtin_clzll(uint64_t(A.size())));
		uint64_t log_g = (64 - __builtin_clzll(uint64_t(g)));
		uint64_t log_gs = (64 - __builtin_clzll(uint64_t(g+A.size())));
		double min_bits_rule = (double(log_g)+0.557);

		auto bits_per_rule = double(wr - log_gs*t)/g;

		double inf_min = g*min_bits_rule + t*log_gs + s*log_s;

		return out_size;
	}


	/*
	 * inverse of function compress_and_store_2(...)
	 *
	 * input arrays are overwritten with content of file
	 *
	 */
	void read_and_decompress(vector<itype> & A, vector<pair<itype,itype> > & G, vector<itype> & T){

		//empty arrays
		A = {};
		G = {};
		T = {};

		//read A
		assert(not eof());
		auto A_size = read();
		//fill A
		for(int i=0;i<A_size;++i) A.push_back(read());

		//store G
		vector<itype> deltas; //deltas between the pair's maximums
		vector<itype> starting_values; //starting values of each increasing sequence
		vector<itype> deltas_starting_points; //starting values of each increasing sequence
		vector<itype> deltas_minimums; //maximums - minimums
		vector<bool> max_first; //the max is the first in the pair. If false, the min is the first in the pair

		uint64_t last_max = 0;

		uint64_t last_incr_seq = 0; //index of last pair that started an increasing sequence
		uint64_t i = 0;//pair number

		uint64_t size;
		size = read(); for(uint64_t i=0;i<size;++i) deltas.push_back(read());
		size = read(); for(uint64_t i=0;i<size;++i) starting_values.push_back(read());
		size = read(); for(uint64_t i=0;i<size;++i) deltas_starting_points.push_back(read());
		size = read(); for(uint64_t i=0;i<size;++i) deltas_minimums.push_back(read());
		size = read(); for(uint64_t i=0;i<size;++i) max_first.push_back(read());
		size = read(); for(uint64_t i=0;i<size;++i) T.push_back(read());

		//now retrieve G from the above vectors

		uint64_t idx_in_deltas=0;
		uint64_t idx_in_starting_values=0;
		uint64_t idx_in_deltas_starting_points=0;

		uint64_t last_delta_starting_point = 0;

		for(uint64_t i = 0;i<max_first.size();++i){

			uint64_t max;
			uint64_t min;

			if(idx_in_deltas_starting_points < deltas_starting_points.size() && i == last_delta_starting_point + deltas_starting_points[idx_in_deltas_starting_points]){

				//starts a new increasing seq

				last_delta_starting_point = i;
				max = starting_values[idx_in_starting_values++];
				idx_in_deltas_starting_points++;

			}else{

				max = last_max + deltas[idx_in_deltas++];

			}

			last_max = max;
			min = max - deltas_minimums[i];

			if(max_first[i])
				G.push_back({max,min});
			else
				G.push_back({min,max});

		}

	}


private:

	void flush_to_file(size_t *out_size){

		//first, write number of integers in the file
		flush_gamma_integer(buffer.size());

		auto bitsizes = blocks_bitsizes;//store copy

		assert(blocks_bitsizes.size()==(buffer.size()/block_size) + (buffer.size()%block_size != 0));
 		//delta-encode bitsizes
		delta_encode(blocks_bitsizes);
		//run-length encode the deltas
		auto R = run_length_encode(blocks_bitsizes);
		//most of the runs have length 1, so run-length encode lengths of runs
		auto R2 = run_length_encode(R.first);

		//store R's size to file (number of runs)
		flush_gamma_integer(R.second.size());

		//store R run heads to file
		for(auto x:R.second) flush_gamma_integer(x);

		//store R2's size to file (number of runs)
		flush_gamma_integer(R2.second.size());

		//store R2 run lengths to file
		for(auto x:R2.first) flush_gamma_integer(x);

		//store R2 run heads to file
		for(auto x:R2.second) flush_gamma_integer(x);

		//flush all integers using bitsize of their block
		for(uint64_t i=0;i<buffer.size();++i) flush_binary_integer(buffer[i],bitsizes[i/block_size]);

		flush_bits(out_size);

	}


	/*
	 * replace content of V with the deltas of its consecutive elements,
	 * encode each delta with function f (so that final values are
	 * all > 0)
	 */
	void delta_encode(vector<itype> & V){

		int x = 0;//last integer

		for(uint64_t i=0;i<V.size();++i){

			int temp = V[i];
			V[i] = f(int(V[i])-x);
			x = temp;

		}

	}

	/*
	 * inverse of delta_encode
	 */
	void delta_decode(vector<itype> & V){

		assert(V.size()>0);
		assert(f_1(V[0]) > 0);//first delta cannot be negative

		V[0] = f_1(V[0]);

		for(uint64_t i=1;i<V.size();++i){

			//result cannot be <= 0
			assert(int(V[i-1]) + f_1(V[i])>0);

			V[i] = int(V[i-1]) + f_1(V[i]);

		}

	}

	/*
	 * input: vector of integers
	 * output: two vectors storing lengths of runs and run heads
	 */
	pair<vector<itype>, vector<itype> > run_length_encode(vector<itype> & V){

		pair<vector<itype>, vector<itype> > R;
		itype l = 1;
		itype n = 0;

		for(itype i=0;i<V.size();++i){

			if(i == V.size()-1 || V[i] != V[i+1]){

				R.first.push_back(l);
				R.second.push_back(V[i]);

				n+=l;

				l = 1;

			}else{

				++l;

			}

		}

		assert(n == V.size());

		return R;

	}

	vector<itype> run_length_decode(vector<itype> & L, vector<itype> & H){

		assert(L.size() == H.size());

		vector<itype> V;

		for(uint64_t i = 0;i<L.size();++i){

			for(uint64_t j=0;j<L[i];++j){

				V.push_back(H[i]);

			}

		}

		return V;

	}

	/*
	 * read from bits next gamma code and advance idx_in_bits accordingly
	 */
	uint64_t read_next_gamma(){

		//if next bits are not a full gamma code, read more bits from file
		assert(is_next_code_complete());

		auto len = next_gamma_length();

		//prefix of zeros
		auto prefix = (len - 1)/2;

		idx_in_bits += prefix;

		return read_next_int(prefix+1);

	}

	/*
	 * read from bits next w-bits integer and advance idx_in_bits accordingly
	 */
	uint64_t read_next_int(uint64_t w){

		assert(idx_in_bits + w <= bits.size());

		uint64_t x = 0;

		for(int i = 0;i<w;++i){

			x |= ( uint64_t(bits[idx_in_bits++]) << ((w-i)-1) );

		}

		return x;

	}

	/*
	 * read all bytes from file and store their bits in bitvector bits
	 */
	void fill_bits(){

		//first of all, delete from bits all bits that have already been read

		assert(idx_in_bits==0);

		while(not std::cin.eof()){

			uint8_t x;
			std::cin.read((char*)&x,1);

			//push back bits of x in vector bits
			for(int j=0;j<8;++j) bits.push_back( (x>>(7-j)) & uint8_t(1) );

		}

	}

	/*
	 * decode the content of bits and store the integers to buffer
	 */
	void fill_buffer(){

		assert(buffer.size()==0);

		//number of integers stored in the file
		uint64_t n = read_next_gamma();

		//number of stored bit-sizes
		uint64_t n_blocks = (n/block_size) + (n%block_size!=0);

		//number of R's runs
		uint64_t R_size = read_next_gamma();

		vector<itype> R_heads;
		for(itype r = 0;r<R_size;++r) R_heads.push_back(read_next_gamma());

		//number of R2's runs
		itype R2_size = read_next_gamma();

		vector<itype> R2_lengths;
		for(itype r = 0;r<R2_size;++r) R2_lengths.push_back(read_next_gamma());

		vector<itype> R2_heads;
		for(itype r = 0;r<R2_size;++r) R2_heads.push_back(read_next_gamma());

		vector<itype> R_lengths = run_length_decode(R2_lengths,R2_heads);

		vector<itype> V = run_length_decode(R_lengths,R_heads);

		assert(V.size() == n_blocks);

		delta_decode(V);

		//now read n integers
		for(itype i=0;i<n;++i){

			buffer.push_back(read_next_int(V[i/block_size]));

		}

	}

	/*
	 * return length of the next gamma code
	 */
	uint64_t next_gamma_length(){

		uint64_t i = idx_in_bits;

		//count how many 0 are prefixing bits[idx_in_bits, ...]
		while(i<bits.size() && bits[i]==0)	i++;

		auto zeros = i - idx_in_bits;

		return 2*zeros + 1;

	}

	/*
	 * is the code prefixing bits[idx_in_bits, ...] a complete gamma code?
	 */
	bool is_next_code_complete(){

		return (bits.size() - idx_in_bits) >= next_gamma_length();

	}

	/*
	 * flush to bitvector bits the bit-representation of gamma(x)
	 */
	void flush_gamma_integer(uint64_t x){

		for(auto b:gamma(x)) bits.push_back(b);

	}

	/*
	 * flush to bitvector bits the bit-representation of gamma(x)
	 */
	void flush_binary_integer(uint64_t x, uint64_t w = 0){

		for(auto b:binary(x,w)) bits.push_back(b);

	}


	//flush bits to file. A padding of 0s is added so that the size is a multiple of 8
	void flush_bits(size_t *out_size) {
	  // std::cout << "Bits without padding: " << bits.size() << '\n';
		//add padding
		while(bits.size()%8!=0) bits.push_back(0);

		int n = bits.size();
		int i = 0;
		uint8_t x = 0;

		for(auto b : bits){

			x |= (uint8_t(b) << (7-i));

			if(i==7){ //we just pushed last bit of a Byte
				*out_size +=1;
				x = 0;

				actual_bitsize += 8;

			}

			i = (i+1)%8;

		}

	}

	/*
	 * bit-width of x
	 */
	uint8_t wd(uint64_t x){

		auto w = 64 - __builtin_clzll(uint64_t(x));

		return x == 0 ? 1 : w;

	}

	/*
	 * return gamma encoding of x
	 */
	vector<bool> gamma(uint64_t x){

		assert(x>0);

		auto w = wd(x);

		vector<bool> code;

		//append w-1 zeros
		for(int i=0;i<w-1;++i) code.push_back(false);

		//append binary code of x using w bits
		for(bool b : binary(x,w)) code.push_back(b);

		return code;

	}

	/*
	 * return w-bits binary code of x. If w is not specified, the bitsize of x is used.
	 */
	vector<bool> binary(uint64_t x, uint64_t w = 0){

		assert(w==0 || w>= wd(x));

		w = w == 0 ? wd(x) : w;

		vector<bool> code;

		for(int i=0;i<w;++i) code.push_back( (x>>((w-i)-1)) & uint64_t(1) );

		return code;

	}

	/*
	 * maps small signed integers to small nonzero unsigned integers:
	 *
	 * 0  -> 1
	 * -1 -> 2
	 * 1  -> 3
	 * -2 -> 4
	 * 2  -> 5
	 * -3 -> 6
	 * 3  -> 7
	 *
	 * ...
	 *
	 */
	uint16_t f(int x){

		return x>=0 ? 2*x+1 : (-x)*2;

	}

	/*
	 * inverse of function f
	 */
	int f_1(uint16_t x){

		return x%2 == 1 ? (x-1)/2 : -int(x)/2;

	}


	bool write;

	/*
	 * counters storing number of bits used for the 3 grammar components
	 * these counters are filled after calling close()
	 */
	uint64_t bits_for_grammar = 0;
	uint64_t bits_for_text = 0;
	uint64_t bits_for_alphabet = 0;

	vector<bool> bits;//bits to be flushed to file

	uint64_t idx_in_bits = 0;//index in vector bits

	vector<itype> buffer;
	uint64_t idx_in_buf = 0;//current index in buffer while reading

	vector<itype> blocks_bitsizes;//store bitsize of largest integer in each block


	bool end_of_file = false;

  /*istream in;
    ostream out;*/

	/*
	 * stores accumulated bitlength of all integers stored in the file
	 */
	uint64_t lower_bound_bitsize = 0;

	/*
	 * stores accumulated bitlength of all integers stored in the file
	 */
	uint64_t actual_bitsize = 0;

};

} // of Rp
