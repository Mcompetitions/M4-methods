/*
 * packed_gamma_file.hpp
 *
 *  Created on: Feb 15, 2017
 *      Author: nico
 *
 *  read/write integers in packed form in/from a file
 *
 */

#ifndef INTERNAL_PACKED_GAMMA_FILE_HPP_
#define INTERNAL_PACKED_GAMMA_FILE_HPP_

#include <cassert>
#include <vector>
#include <fstream>

#endif /* INTERNAL_PACKED_GAMMA_FILE_HPP_ */

namespace Rp
{
using namespace std;

template<uint64_t block_size = 3>
class packed_gamma_file{

public:

	/*
	 * constructor
	 *
	 * input: file name and flag indicating whether file is used in write mode (true) or read mode (false)
	 *
	 */
	packed_gamma_file(string filename, bool write = true){

		this->write = write;

		if(write){

			out = ofstream(filename, std::ios::binary);

		}else{

			in = ifstream(filename, std::ios::binary);

		}

		lower_bound_bitsize = 0;
		actual_bitsize = 0;

	}

	/*
	 * append integer x in packed form to the file
	 */
	void push_back(uint64_t x){

		assert(write);

		if(buffer.size() == block_size){

			//buffer full: flush to file
			flush_buffer();
			assert(buffer.size() == 0);

		}

		buffer.push_back(x);

		lower_bound_bitsize += wd(x);

	}

	/*
	 * read next integer from file.
	 * After EOF is reached, returns always 0
	 */
	uint64_t read(){

		assert(not write);

		if(eof()) return 0;

		assert(idx_in_buf <= buffer.size());

		if(idx_in_buf == buffer.size()){

			//if there are still integers to be read from the file, fill buffer
			fill_buffer();

		}

		//here buffer contains some integers
		assert(idx_in_buf < buffer.size());

		//return next integer and advance counter in buffer
		auto x = buffer[idx_in_buf++];

		return x;

	}

	/*
	 * return true if there are no more characters to be read from the file
	 */
	bool eof(){

		assert(not write);

		return end_of_file and idx_in_buf >= buffer.size();

	}

	/*
	 * close file: must be called when there are no more integers to be written
	 */
	void close(){

		assert(write);
		assert(buffer.size()<=block_size);

		//remaining integers that need to be written (possibly 0)
		uint64_t remaining = buffer.size();

		uint64_t END = 65;//this denotes the end of file (larger than bitsize(uint64_t))

		//push back EOF code
		for(bool b : gamma(END)) bits.push_back(b);

		//push back 64 bits with size of buffer
		for(bool b : binary(remaining,64)) bits.push_back(b);

		//push back remaining integers in buffer (using 64 bits each)
		for(uint64_t x : buffer)
			for(bool b : binary(x,64))
				bits.push_back(b);

		//pad bits with 0s until we reach a multiple of 8
		//in this way, we are sure that flush_bits() will flush
		//to file all bits
		while(bits.size()%8 != 0) bits.push_back(false);

		flush_bits();

		assert(bits.size()==0);

		out.close();

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
	void compress_and_store_2(vector<uint64_t> & A, vector<pair<uint64_t,uint64_t> > & G, vector<uint64_t> & T){

		//store A
		push_back(A.size());
		for(auto a : A) push_back(a);

		//store G
		vector<uint64_t> deltas; //deltas between the pair's maximums
		vector<uint64_t> starting_values; //starting values of each increasing sequence
		vector<uint64_t> deltas_starting_points; //starting values of each increasing sequence
		vector<uint64_t> deltas_minimums; //maximums - minimums
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

		auto w1 = written_bytes()*8;
		push_back(deltas.size()); for(auto x:deltas) push_back(x);
		push_back(starting_values.size());for(auto x:starting_values) push_back(x);
		push_back(deltas_starting_points.size());for(auto x:deltas_starting_points) push_back(x);

		auto w2 = written_bytes()*8;
		push_back(deltas_minimums.size());for(auto x:deltas_minimums) push_back(x);
		push_back(max_first.size());for(auto x:max_first) push_back(x);

		auto w3 = written_bytes()*8;
		//store T
		push_back(T.size());
		for(auto a : T) push_back(a);

		close();

		auto w4 = written_bytes()*8;

		/*
		 * statistics
		 */

		uint64_t g = G.size();
		auto bits_per_rule = double(w3-w1)/g;
		auto bits_per_first_rule = double(w2-w1)/g;
		auto bits_per_second_rule = double(w3-w2)/g;
		auto bits_per_text_ch = double(w4-w3)/T.size();

		// cout << "Compressed file size : " << w4/8 << " Bytes" << endl;
		// cout << "Grammar size : g = " << g << " rules" << endl;
		// cout << "log_2 g = " << (64 - __builtin_clzll(uint64_t(g))) << endl;
		// cout << bits_per_text_ch << " bits per compressed file character (" << T.size() << " characters)" << endl;
		// cout << bits_per_rule << " bits per grammar rule, distributed in:" << endl;
		// cout << " " << bits_per_first_rule << " bits per sorted components (M = " << starting_values.size() << " increasing subsequences)" << endl;
		// cout << " " << bits_per_second_rule << " bits per unsorted components" << endl;

	}


	/*
	 * inverse of function compress_and_store_2(...)
	 *
	 * input arrays are overwritten with content of file
	 *
	 */
	void read_and_decompress_2(vector<uint64_t> & A, vector<pair<uint64_t,uint64_t> > & G, vector<uint64_t> & T){

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
		vector<uint64_t> deltas; //deltas between the pair's maximums
		vector<uint64_t> starting_values; //starting values of each increasing sequence
		vector<uint64_t> deltas_starting_points; //starting values of each increasing sequence
		vector<uint64_t> deltas_minimums; //maximums - minimums
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



	/*
	 * input: alphabet encoding A (ascii -> integers in {0,...,|A|-1}), grammar G (pairs) and compressed text T
	 *
	 * store A, G, T to file without compression
	 *
	 * TOTAL SIZE: A log A + 2 G log G
	 *
	 */
	void compress_and_store_1(vector<uint64_t> & A, vector<pair<uint64_t,uint64_t> > & G, vector<uint64_t> & T){

		//store A
		push_back(A.size());
		for(auto a : A) push_back(a);

		auto w1 = written_bytes()*8;

		//Store G
		push_back(G.size());
		for(auto ab : G){

			push_back(ab.first);
			push_back(ab.second);

		}

		auto w2 = written_bytes()*8;

		//store T
		push_back(T.size());
		for(auto a : T) push_back(a);

		close();

		auto w3 = written_bytes()*8;

		/*
		 * statistics
		 */

		uint64_t g = G.size();
		auto bits_per_rule = double(w2-w1)/g;
		auto bits_per_text_ch = double(w3-w2)/T.size();

		// cout << "Compressed file size : " << w3/8 << " Bytes" << endl;
		// cout << "Grammar size : " << g << " rules" << endl;
		// cout << bits_per_rule << " bits per grammar rule." << endl;
		// cout << bits_per_text_ch << " bits per compressed file character (" << T.size() << " characters)" << endl;

	}

private:

	void fill_buffer(){

		assert(not end_of_file);

		//empty buffer
		buffer = {};

		uint64_t w = read_next_gamma();

		//65 indicates EOF (i.e. last buffer)
		assert(w<=65);

		if(w==65){

			//end of file reached: this is the last buffer we fill
			end_of_file = true;

			uint64_t bufsize = read_next_int(64);

			for(uint64_t i = 0;i<bufsize;++i) buffer.push_back(read_next_int(64));

		}else{

			//w is the bitsize of following block_size integers
			for(uint64_t i = 0;i<block_size;++i) buffer.push_back(read_next_int(w));

		}

		idx_in_buf = 0;

	}

	/*
	 * read from bits next gamma code and advance idx_in_bits accordingly
	 */
	uint64_t read_next_gamma(){

		//if next bits are not a full gamma code, read more bits from file
		if(not is_next_code_complete()) fill_bits();

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

		if(idx_in_bits + w > bits.size()) fill_bits();

		uint64_t x = 0;

		for(int i = 0;i<w;++i){

			x |= ( uint64_t(bits[idx_in_bits++]) << ((w-i)-1) );

		}

		return x;

	}

	/*
	 * read at most bytes Bytes from file and push back content in vector bits
	 */
	void fill_bits(uint64_t bytes = 256){

		//first of all, delete from bits all bits that have already been read

		{

			vector<bool> bits1;

			for(uint64_t i=idx_in_bits;i<bits.size();++i) bits1.push_back(bits[i]);

			bits = bits1;

			idx_in_bits = 0;

		}

		uint64_t i = 0;//read bytes

		while(i<bytes and not in.eof()){

			uint8_t x;
			in.read((char*)&x,1);

			//push back bits of x in vector bits
			for(int j=0;j<8;++j) bits.push_back( (x>>(7-j)) & uint8_t(1) );

			++i;

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

	void flush_buffer(){

		assert(buffer.size() == block_size);

		uint64_t w = 0;

		//detect max width of integers in buffer
		for(auto x:buffer) w = std::max(w,wd(x));

		//cout << w << " ";

		assert(w>0);

		//push back gamma code of w
		for(bool b : gamma(w)) bits.push_back(b);

		//push back w-bits codes of integers in buffer
		for(auto x:buffer)
			for(auto b:binary(x,w))
				bits.push_back(b);

		flush_bits();

		//empty buffer
		buffer = vector<uint64_t>();

	}

	//flush bits to file (bits.size()%8 bits remain in the array after this call)
	void flush_bits(){

		int n = bits.size();
		int i = 0;
		uint8_t x = 0;

		for(auto b : bits){

			x |= (uint8_t(b) << (7-i));

			if(i==7){ //we just pushed last bit of a Byte

				out.write((char*)&x,1);
				x = 0;

				actual_bitsize += 8;

			}

			i = (i+1)%8;

		}

		auto bits1 = vector<bool>();

		//move remainder at the beginning of bits
		for(int i = (n/8)*8; i<n;++i) bits1.push_back(bits[i]);

		bits = bits1;

	}

	/*
	 * bit-width of x
	 */
	uint64_t wd(uint64_t x){

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

	bool write;

	vector<bool> bits;//bits to be flushed to file

	uint64_t idx_in_bits = 0;//index in vector bits

	vector<uint64_t> buffer;
	uint64_t idx_in_buf = 0;//current index in buffer while reading

	bool end_of_file = false;

	ifstream in;
	ofstream out;

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
