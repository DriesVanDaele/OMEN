data('example_data/tokheim_pancancer_somatic_CADD.pl', 'example_data/tokheim_pancancer_somatic_coverage_ranks.pl').
network_file('example_data/network.pl').
threshold(10**(-6)).
pattern_quality_metric_threshold(0.67).
mcda(weighted_sum, [0.8, 0]).
path_definition_file('example_data/path_definition.dslp').
