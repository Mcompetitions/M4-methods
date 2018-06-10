        1. Short DESCRIPTION.
    PPMd program is  file-to-file compressor, it  is written for  embedding in
user  programs  mainly  and  it  is  not  intended  for  immediate  use. I was
interested in speed and performance  improvements of abstract PPM model  [1-6]
only, without tuning it to  particular data types, therefore compressor  works
good  enough  for  texts,  but  it  is  not  so  good for nonhomogeneous files
(executables) and for  noisy analog data  (sounds, pictures etc.).  Program is
very memory  consuming, You  can choose  balance between  execution speed  and
memory economy,  on one  hand, and  compression performance,  on another hand,
with the help of model order selection option (-o).
	Methods of restoration of model correctness at memory insufficiency:
    '-r0 - restart model  from scratch'.  This  method is not optimal  for any
type of data  sources, but it  works fast and  efficient in average,  so it is
the recommended method.
    '-r1 - cut off model'. This method is optimal for quasistationary  sources
when the period  of stationarity is  much larger than  period between cutoffs.
As a rule, it  gives better results, but  it is slower than  other methods and
it is unstable against fragmentation of  memory heap at high model orders  and
low memory.
    '-r2 - freeze model'. This method is optimal for stationary sources  (show
me such source when You will find  it ;-)). It is fast and efficient  for such
sources.

        2. Distribution CONTENTS.
    read_me.txt  - this file;
    PPMd.h, PPMdType.h - header files;
    Coder.hpp, SubAlloc.hpp, Model.cpp, PPMd.cpp - code sources;
    makefile.gmk - makefile for GnuC v.2.95.2 (tested for DJGPP v.2.03 only);
    makefile.imk - makefile for IntelC v.4.0;
    makefile.mak - makefile for BorlandC v.5.01;
    PPMd.exe     - compressor itself (2.097bpb on Calgary corpus);
    PPMonstr.exe - fat and sleepy compressor (for taking PPMd down only,
1.963bpb on Calgary corpus);

        3. LEGAL stuff.
    You can not misattribute authorship on algorithm or code sources,  You can
not patent algorithm or its parts, all other things are allowed and welcomed.
    Dmitry Subbotin  and me  have authorship  rights on  code sources.  Dmitry
Subbotin owns authorship rights on  his variation of rangecoder algorithm  and
I own authorship rights  on my variation of  PPM algorithm. This variation  is
named PPMII (PPM with Information Inheritance).
    PPMonstr  program  is  distributed  for  experiments and noncommercial use
only.

        4. DIFFERENCES between variants.
        Jun 13, 1999  var.A
    Initial release;
        Jun 30, 1999  var.B
    Arithmetic coder was changed to newer version;
    Simplified LOE was tested (Model1.cpp file);
    Some small improvements were done;
        Aug 22, 1999  var.C
    Rudimentary SEE was added;
    Some small improvements were done;
        Oct  6, 1999  var.D
    Inherited probabilities (IPs) were added;
    Memory requirements were reduced a bit;
    Small improvements were continued;
        Dec  3, 1999  var.E
    Program  name  was  changed  from  PPM,  escape  method D (PPMD) to PPM by
Dmitry (PPMd). Pronounce correctly! ;-)
    Bug in ARI_FLUSH_ENCODER was crushed;
    Model1.cpp file was removed from package due to LOE gives no gain;
        Apr  7, 2000  var.F(inal?)
    Michael   Schindler`s   rangecoder   implementation   was   replaced  with
'carryless rangecoder' by  Dmitry Subbotin.   Now, PPMd is  pure public domain
program;
    CC rate - 2.123/2.089bpb;
        Nov 26, 2000  var.G(rand final)
    Memory requirements were reduced;
    CC rate - 2.121/2.056bpb;
        Apr 21, 2001  var.H(ard run to final)
    Memory requirements were reduced a bit;
    CC rate - 2.104/2.041bpb;
        Apr 28, 2002  var.I(t is final too)
    References to papers were corrected;
    CC rate - 2.097/1.963bpb;
        Apr 30, 2002  var.I rev.1
    One  defect  in  PPMonstr  was  fixed,  this  revision  of PPMonstr is not
compatible with previous one;

        5. REFERENCES.
    [1] Excellent  introductory  review  T.Bell,  I.H.Witten, J.G.Cleary
'MODELING FOR TEXT COMPRESSION'. Russian translation is placed at
http://cotty.mebius.net/compress/ru/modeling.txt;
    [2] Very descriptive M.R.Nelson`s COMP-2 program (PPMd is based on it).
COMP-2 is  in  wuarchive.wustl.edu:/mirrors/msdos/ddjmag/ddj9102.zip  (inner
zip file nelson.zip);
    [3] P.G.Howard PhD thesis 'The Design and Analysis of Efficient Lossless
Data Compression Systems', is available in
ftp.cs.brown.edu/pub/techreports/93/cs93-28.ps.Z;
    [4] S.Bunton PhD thesis 'On-Line Stochastic Processes in Data
Compression', is available in
ftp.cs.washington.edu/tr/1997/03/UW-CSE-97-03-02.PS.Z;

PPMII algorithm:
    [5] D.Shkarin 'Improving the efficiency of PPM algorithm', in Russian,
http://sochi.net.ru/~maxime/doc/PracticalPPM.ps.gz;
	[6] D.Shkarin 'PPM: one step to practicality', in English,
http://www.dogma.net/DataCompression/Miscellaneous/PPMII_DCC02.pdf

    AUTHOR SHALL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, SPECIAL,  INCIDENTAL,
OR CONSEQUENTIAL  DAMAGES ARISING  OUT OF  ANY USE  OF THIS  SOFTWARE. YOU USE
THIS PROGRAM AT YOUR OWN RISK.

                                        Dancy compression!
                                        Dmitry Shkarin
                                        E-mail: dmitry.shkarin@mtu-net.ru
