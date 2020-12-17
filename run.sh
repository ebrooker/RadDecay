#!/bin/bash


if [ ! -d "./figures" ]; then
	mkdir figures
fi

if [ ! -d "./output_data" ]; then
	mkdir output_data
fi

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
