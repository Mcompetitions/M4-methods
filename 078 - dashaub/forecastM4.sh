#!/bin/bash

set -euo pipefail

cd ~/m4

Rscript --no-save ~/m4/forecastM4.R "$@"
echo 'Point forecasts and prediction intervals created successfully!'
echo 'Pressing enter will exit the container and erase all results.'
read
exit
