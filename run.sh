#!/bin/bash

mkdir figures
mkdir output_data
cd src
gfortran -c environment_settings.f03
gfortran -c rad_decay_class.f03
gfortran -c rad_decay_chain.f03
gfortran -c integrator.f03
gfortran -c test.f03
gfortran *.o -o radtest.exe
./radtest.exe
rm *.o *.mod 

cd ../plotting
python plot.py
