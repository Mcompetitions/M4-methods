#!/bin/bash
set -euo pipefail
mkdir -p Data
cd Data/
wget https://www.m4.unic.ac.cy/wp-content/uploads/2017/12/M4DataSet.zip
unzip M4DataSet.zip
rm M4DataSet.zip
