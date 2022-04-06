# OMEN: Network-based Driver Gene Identification using Mutual Exclusivity

## Requirements

Linux (tested on Ubuntu 18.04)

* SWI-Prolog version >=7, on your path as 'swipl'
* Yap version >=6.2.2, on your path as 'yap' (tested on Yap 6.2.2)
* GNU Parallel, on your path as 'parallel'
* cgcp from https://github.com/dh-tran/cgcp in order to cluster a probabilistic network into modules
* bash for executing shell scripts

## Usage

### Data format:

* CADD probabilities are provided as facts of gene_patient_probability/3 (args: Gene, Patient, Probability)
and coverage data is specified in gene_coverage_helper/2 (args: Gene, Coverage_Score).
* The interaction network is defined by the predicate undirected_interaction/2 (args: Node, Node)

Note that both the interaction network and the coverage data need to contain a header as seen in the example data files:
- example_data/tokheim_pancancer_somatic_coverage_ranks.pl
- example_data/network.pl

### Execution:

OMEN runs through a sequence of distinct steps.
Each step can be called individually through a number of shell scripts as discussed in section Note below.

In case you choose to interact through the shell scripts, your data (CADD, coverage, network)
has to initially be linked in experiment_parameters.pl (the example data is used by default)

Alternatively, you can also interact through a Python interface (main.yp) . The Python interface will generate this
experiment_parameters.pl file thus all you have to do when using the Python interface is run main.py
using the appropriate arguments as outlined in that file.


## Note

By default, pattern_collection.sh creates a directory called 'output' within the working directory
Within this directory a subdirectory will be created for each run of pattern_collection.sh.
It is within that directory that the output of pattern collection will be archived. 
(For convenience, a copy of these files will also appear in the working directory)

Subsequent scripts (evaluate_paths.sh,...) should be called with inputs drawn from that directory as indicated below.
In turn their output will be stored there as well. The python interface main.py performs this chaining automatically.

```


Usage collecting patterns:
./pattern_collection.sh <numberOfThreads> <experimentFile> <outputFile>
    <numberOfThreads>    : Number of CPU threads to use simultaneously
    <experimentFile>     : Path to which a copy of the experiment file generated during pattern collection is written (ease of use feature)
    <outputFile>         : Path to which a copy of the output file generaetd during pattern collection is written (ease of use feature)
    
Usage explicit scoring of paths:
./evaluate_paths.sh <experimentFile> <outputFile>
    <experimentFile>     : Path of the experiment file generated during pattern collection (e.g. new_experiment_[...].pl)
    <outputFile>         : Path of the scored pattern file generated during pattern collection (e.g. output)

Usage explicit filtering of paths:
./filter_patterns.sh <scoredPatternFile> <patternQualityThreshold>
    <scoredPatternFile>         : Path of the output file generated during path evaluation
    <patternQualityThreshold>   : float in [0,1] representing minimum pattern quality

Usage probabilistic network generation:
./probabilistic_network.sh <filteredPatternFile>
    <filteredPatternFile>       : Path of the output file generated during path filtering

Usage gene ranking:
./ranking.sh <probabilisticNetwork> <network>
    <probabilisticNetwork>      : Path of the output file generated during probabilistic network generation
    <network>                   : Path of the file containing the interaction network

Usage clustering data generations (network, must-link, cannot-link):
./generate_clustering_data.sh <filteredPatternFile> <N>
    <filteredPatternFile>       : Path of the output file generated during path filtering
    <N>                         : N is an integer, the top-N nodes that should be clustered
```
