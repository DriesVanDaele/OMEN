# FAME: Network-based Driver Gene Identification using Mutual Exclusivity

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

```
Usage collecting patterns:
./pattern_collection.sh

Usage explicit scoring of paths:
./evaluate_paths.sh <experimentFile> <outputFile>
    <experimentFile>     : Path of the output file generated during pattern collection
    <outputFile>         : Path of the output file to store scored patterns

Usage explicit filtering of paths:
./filter_paths.sh <scoredPatternFile> <patternQualityThreshold>
    <scoredPatternFile          : Path of the output file generated during path evaluation
    <patternQualityThreshold>   : float in [0,1] representing minimum pattern quality
```


