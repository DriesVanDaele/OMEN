This directory contains the data used in the original experiments.
It is linked in experiment_generator.pl.

If you wish to run experiments on your own data, format your data as Prolog facts like the example files,
and set the appropriate paths in experiment_generator.pl

for pattern_collection.sh in particular, it should be noted that 
it runs the program in parallel, for optimal performance you should
set the 'parts' variable to an appropriate number (default=20)

If the original experiment is too computationally costly to replicate, you can still compute an approximation 
by modifying the threshold/1 fact in experiment_generator.pl
e.g. replace  threshold(10**(-6)). with threshold(10**(-5)). in order to explore the network less thoroughly.
The probability specified in the argument is the probability at which paths are no longer further considered for extension.
