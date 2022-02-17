This directory contains the data used in the original experiments.
It is linked in experiment_generator.pl.

If you wish to run experiments on your own data, format your data as Prolog facts like the example files,
and set the appropriate paths in experiment_generator.pl

for pattern_collection.sh in particular, it should be noted that 
it runs the program in parallel, for optimal performance you should
set the 'parts' variable to an appropriate number (default=20)
