# OMEN: Network-based Driver Gene Identification using Mutual Exclusivity

## Requirements

Linux (tested on Ubuntu 18.04)

* SWI-Prolog version >7, on your path as 'swipl'
* yap version >6.2.2, on your path as 'yap'
* GNU Parallel, on your path as 'parallel'
* cgcp from https://github.com/dh-tran/cgcp in order to cluster a probabilistic network into modules
* bash for executing shell scripts

## Usage

Data file names are linked in experiment_generator.pl

Data format:

* CADD probabilities are provided as facts of gene_patient_probability/3 (args: Gene, Patient, Probability)
and coverage data is specified in data/2.
* The interaction network is defined by the predicate interaction/2 (args: Node, Node)

Your own data can be loaded by modifying the first 6 lines of experiment_generator.pl

## Note

By default, pattern_collection.sh creates a directory called 'output' within the working directory
Within this directory a subdirectory will be created for each run of pattern_collection.sh.
It is within that directory that the output of pattern collection will be stored.
Subsequent scripts (evaluate_paths.sh,...) should be called with inputs drawn from that directory as indicated below.
In turn their output will be stored there as well.

```


Usage collecting patterns:
./pattern_collection.sh

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
./ranking.sh <probabilisticNetwork>
    <probabilisticNetwork>      : Path of the output file generated during probabilistic network generation

Usage clustering data generations (network, must-link, cannot-link):
./generate_clustering_data.sh <filteredPatternFile> <N>
    <filteredPatternFile>       : Path of the output file generated during path filtering
    <N>                         : N is an integer, the top-N nodes that should be clustered
```
