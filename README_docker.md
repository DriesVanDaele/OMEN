# The Docker file included here runs the pattern collection phase of the original experiment performed by OMEN
# You can run experiments using your own data by modifying the appropriate facts in  experiment_generator.pl :
#
# the first 6 lines of experiment_generator.pl can be adjusted to choose your own data and settings:
#
# data('example_data/tokheim_pancancer_somatic_CADD.pl', 'example_data/tokheim_pancancer_somatic_coverage_ranks.pl').
# network_file('example_data/network.pl').
# threshold(10**(-6)).
# pattern_quality_metric_threshold(0.67).
# mcda(weighted_sum, [0.8, 0]).
# path_definition_file('example_data/path_definition.dslp').
#
# Note that this is just one of multiple phases listed in README.md


# build docker

$ sudo docker build -t omen .

assuming your working directory is FAME

-------------

# run docker

$ sudo docker run --rm  --name omen --mount type=bind,source=/home/user/temp_output,target=/generated_files/output omen

adjust host path as needed.
output directory to store directory containing the pattern output file



