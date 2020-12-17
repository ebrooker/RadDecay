# RadDecay
A simple object-oriented Fortran 2003 radioactive decay chain solver


## Notes

This is a very simple radioactive decay chain solver implemented using the object-oriented features of modern Fortran. The program works by holding each isotope in the decay chain as an instance of the `Isotope` class. Each `Isotope` instance is stored in an instance of the `DecayChain` class that manages the system of isotopes and the interconnections within the decay chain. There is an `Integrator` class built-in for testing the the evolution of the decay chain.

This code only supports a single decay chain for right now as this was originally just meant to be an exercise in using OOP features in Fortran to solve a numerical ODE problem.

## Tutorial

Usage is simple at the moment, pull the entire GitHub repository (input/output directories included). In a BASH terminal, just use:
`$ ./run.sh`
and the code should execute. Creates `output_data/` and `figures/` directories if they don't exist. Compiles Fortran source, then executes code, creates plot of result.

The test.f03 file that comes in `src/` is the current program driver. There is a sample Ni56 decay chain input provided in `input_data/`

To change the decay chain inputs, go to the `input_data/` directory. There will be two files, `abundances.dat` and `isotopes.dat`. In the abundances file, list the parent isotope first and all children isotopes in order in the decay chain for the first column, for the second column give the fractional abundance of the isotope at the beginning of the time evolution (a value between 0 and 1, all values in this column should add up to 1, but is not required). For the isotopes file, each row gives the parent isotope, the type of radioactive decay (for future features), the mass number, neutron number, half-life, and immediate child isotope. Provide this information for every isotope in the decay chain. For the final child isotope (the stable isotope) list `none` for the decay type and 0.0 for the half-life; leave the child isotope column blank for this row.

## Visualization

Comes with a Python script, `plotting/plot.py` that reads the output data, `output_data/decay.dat` and produces a PDF figure, `figures/rad_decay_class.pdf`.

## Currently Broken

Nothing that I'm aware of...

## Future

I hope to expand this simple program into something more complex that can hold a general mapping of multiple decay chains and solve large systems of radioactive decay processes. I also plan to adapt the program to be more friendly when interfacing with integration subroutines (maybe even make my own simple integration library for it to call).

## To Users

If you have any suggestions or want to make contributions, be sure to contact me!
