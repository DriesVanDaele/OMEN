import argparse
import subprocess
import sys

#
# A straightforward interface to interacting with OMEN. By default all parameters have been set to repeat the original experiment
# thus given that all required software has been installed (see README.md), the original experiment can be repeated by calling
#
# $ python3 main.py
#
# (if the execution time takes too long, the original threshold probability of 10**(-6) can be changed to e.g. 2*10**(-5))
# Note also the importance of setting a sensible value for the parameter -cores (default=7) as much of the computational
# load can be parallellized
#
# Using the provided parameters, experiments on your own data can be performed. Note however that for the data:
# (cadd_file, coverage_file and network_file) you should provide the required set of predicates in its proper format as outlined in README.md
#
# Modifying the path_definition_file is provided as a possibility, but should be only considered by experts, as this file is effectively
# a core component of OMEN.
#

# PARSE ARGUMENTS

parser = argparse.ArgumentParser(description="calculate X to the power of Y")
# NOTE that valid filepaths should not contain single quotes
parser.add_argument("cadd_file", type=str, nargs='?', help="CADD probabilities .pl filepath", default='example_data/tokheim_pancancer_somatic_CADD.pl')
parser.add_argument("coverage_file", type=str, nargs='?', help="Coverage .pl filepath", default='example_data/tokheim_pancancer_somatic_coverage_ranks.pl')
parser.add_argument("network_file", type=str, nargs='?', help="Network filepath", default='example_data/network.pl')

parser.add_argument("-pattern_quality_threshold", nargs='?', type=float, help="float specifying the quality a pattern has to exceed to be considered", default=0.67)
parser.add_argument("-threshold_probability", nargs='?', type=str, help="math expression for the SLP threshold", default='10**(-6)')
parser.add_argument("-alpha", type=float, nargs='?', help="value between 0 and 1 managing trade-off between the mutex term and the gene_freq term", default=0.8)
parser.add_argument("-path_definition_file", type=str, nargs='?', help="file specifying the path distribution", default='example_data/path_definition.dslp')

parser.add_argument("-cores", type=str, nargs='?', help="number of CPU threads to employ", default=7)

args = parser.parse_args()
original_stdout = sys.stdout


# GENERATE PARAMETERS FILE FOR PATTERN COLLECTION 
with open('experiment_parameters.pl', 'w') as f:
    sys.stdout = f
    print("data('{}', '{}').".format(args.cadd_file, args.coverage_file))
    print("network_file('{}').".format(args.network_file))
    print("threshold({}).".format(eval(args.threshold_probability)))
    print("pattern_quality_metric_threshold({}).".format(args.pattern_quality_threshold))
    print("mcda(weighted_sum, [{}, 0]).".format(args.alpha))
    print("path_definition_file('{}').".format(args.path_definition_file))
    sys.stdout = original_stdout


# PERFORM PATTERN COLLECTION
subprocess.call(['./pattern_collection.sh', str(args.cores), 'experiment_file_1', 'output_file_1'])
# EVALUATE PATHS
subprocess.call(['./evaluate_paths.sh', 'experiment_file_1', 'output_file_1'])
# FILTER PATTERNS
subprocess.call(['./filter_patterns.sh', 'output_file_1.evaluated', str(args.pattern_quality_threshold)])
# GENERATE PROBABILISTIC NETWORK
subprocess.call(['./probabilistic_network.sh', 'output_file_1.evaluated.filtered_' + str(args.pattern_quality_threshold)])
# GENERATE RANKING
subprocess.call(['./ranking.sh', 'output_file_1.evaluated.filtered_' + str(args.pattern_quality_threshold) + '.probabilistic_network', args.network_file])

