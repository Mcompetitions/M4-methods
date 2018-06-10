/*
 * packed_file.hpp
 *
 *  Created on: Feb 15, 2017
 *      Author: nico
 *
 *  read/write integers in packed form in/from a file
 *
 */

#ifndef INTERNAL_PACKED_FILE_HPP_
#define INTERNAL_PACKED_FILE_HPP_

#include <cassert>
#include <vector>
#include <fstream>

#endif /* INTERNAL_PACKED_FILE_HPP_ */

namespace Rp
{
using namespace std;

template<uint64_t buffer_size = 64>
class packed_file{

public:

	/*
	 * constructor
	 *
	 * input: file name and flag indicating wether file is used in write mode (true) or read mode (false)
	 *
	 */
	packed_file(string filename, bool write = true){

		this->write = write;

		if(write){

			out = ofstream(filename, std::ios::binary);

		}else{

			in = ifstream(filename, std::ios::binary);

		}

		//empty buffer
		buffer = vector<uint64_t>(0);

	}

	/*
	 * append integer x in packed form to the file
	 */
	void push_back(uint64_t x){

		assert(write);
		assert(buffer.size() <= buffer_size);

		information_content += wd(x);

		if(buffer.size() == buffer_size) flush_buffer();

		buffer.push_back(x);

	}

	/*
	 * read next integer from file.
	 * After EOF is reached, returns always 0
	 */
	uint64_t read(){

		assert(not write);

		if(eof()) return 0;

		if(idx_in_buf == buffer.size()){

			uint8_t first_byte;

			in.read((char*)&first_byte,1);

			if(first_byte != 0){

				//case 1: read whole buffer

				extract_from_file(first_byte,buffer_size);

			}else{

				//first byte followed by 64-bit length and 8-bit width

				end_of_file = true;

				uint64_t len;
				uint8_t width;

				in.read((char*)&len,sizeof(len));
				in.read((char*)&width,sizeof(width));

				assert(len>0);

				extract_from_file(width,len);

			}

			idx_in_buf = 0;

		}

		return buffer[idx_in_buf++];

	}

	/*
	 * return true if there are no more characters to be read from the file
	 */
	bool eof(){

		assert(not write);

		return end_of_file and (idx_in_buf == buffer.size());

	}

	/*
	 * close file: must be called when there are no more integers to be written
	 */
	void close(){

		assert(write);
		assert(buffer.size()>0);

		uint64_t last_size = buffer.size();

		auto Z = uint8_t(0);

		//this indicates that the next 64 bits contain size of last block
		out.write((char*)&Z,1);
		out.write((char*)&last_size,sizeof(uint64_t));

		flush_buffer();

		out.close();

		//cout << "Closing file. Cumulated bitlengths = " << information_content/8 << " Bytes" << endl;

	}

	uint64_t get_information_content(){

		return information_content;

	}

private:

	/*
	 * extract m packed integer of width w from file in
	 */
	void extract_from_file(uint64_t w, uint64_t m){

		buffer = vector<uint64_t>(0);

		uint64_t n_bits = w*m;
		uint64_t n_words = (n_bits / 64) + (n_bits % 64 != 0);

		vector<uint64_t> packed(n_words,0);

		in.read((char*)packed.data(),n_words*sizeof(uint64_t));

		uint64_t j = 0;//next bit-read position
		for(uint64_t k = 0; k<m;++k){//read k integers

			uint64_t x = 0;//next integer

			uint8_t bit_pos = 0;

			//we write buffer elements from left to right; every buffer element
			//is written in binary from right to left
			for(int i=0;i<w;++i){

				uint64_t word = j / 64;
				uint64_t off = j % 64;

				uint64_t bit = packed[word] >> ((63-off)&uint64_t(1));

				x |= (bit << bit_pos);

				bit_pos++;
				j++;

			}

			buffer.push_back(x);

		}

	}


	//flush buffer to file
	void flush_buffer(){

		uint64_t width = 0;

		//detect max width
		for(auto y : buffer) width = std::max(wd(y), width);

		assert(width <= 64);
		assert(width > 0);

		uint8_t W = uint8_t(width);

		//write W
		out.write((char*)&W,1);

		uint64_t n_bits = width*buffer_size;
		uint64_t n_words = (n_bits / 64) + (n_bits % 64 != 0);

		vector<uint64_t> packed(n_words,0);

		uint64_t j = 0;//next bit-append position
		for(uint64_t y:buffer){

			assert(wd(y) <= width);

			//we write buffer elements from left to right; every buffer element
			//is written in binary from right to left
			for(int i=0;i<width;++i){

				uint64_t bit = y & uint64_t(1);
				uint64_t word = j / 64;
				uint64_t off = j % 64;

				assert(word < packed.size());

				packed[word] |= (bit << (63-off));

				y >> 1;
				j++;

			}

		}

		out.write((char*)packed.data(),n_words*sizeof(uint64_t));

		buffer = vector<uint64_t>(0);//empty buffer

	}

	/*
	 * bit-width of x
	 */
	uint64_t wd(uint64_t x){

		auto w = 64 - __builtin_clzll(uint64_t(x));

		return x == 0 ? 1 : w;

	}

	bool write;

	vector<uint64_t> buffer;
	uint64_t idx_in_buf = 0;//current index in buffer while reading

	ifstream in;
	ofstream out;

	bool end_of_file = false;

	/*
	 * stores accumulated bitlength of all integers stored in the file
	 */
	uint64_t information_content = 0;

};

} // of Rp
