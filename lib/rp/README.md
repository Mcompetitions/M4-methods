rp: a space-efficient compressor based on the Re-Pair grammar
===============
Author: Nicola Prezza (nicola.prezza@gmail.com)

From the paper: Bille P, Gørtz IL, Prezza N. Space-Efficient Re-Pair Compression. Data Compression Conference 2017, Snowbird (UT).

### Description

The tool 'rp' computes the Re-Pair grammar of a input ASCII file using roughly 6n Bytes of RAM during execution, where n is the file length. Running time is linear. The final grammar is furthermore compressed in order to produce a very small compressed file. The tool compresses particularly well extremely repetitive files: for compression rates >5000x, the output file is usually much smaller than that produced by 7-Zip. Running time of rp is, however, one order of magnitude higher than that of 7-Zip.

### Download

To clone the repository, call

> git clone http://github.com/nicolaprezza/Re-Pair

### Compile

The tool has been tested under linux compiling with gcc 6.2.0

You can use use cmake to generate the Makefile. Create a build folder in the main Re-Pair folder:

> mkdir build

execute cmake:

> cd build; cmake ..

and compile:

> make

### Run

After compiling, run 

>  ./rp c input.txt

This command produces the compressed file input.txt.rp. To decompress, run

>  ./rp d input.txt.rp

This command produces the decompressed file input.txt

