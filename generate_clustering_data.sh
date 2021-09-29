#!/bin/bash

# generate graph file, cannot links file, and must links file
echo "Generating graph file, cannot links file, and must links file."
network_file='example_data/network.pl'
cadd_file='example_data/tokheim_pancancer_somatic_CADD.pl'
filtered_patterns_file=$1
number_of_nodes=$2

node_file=$(mktemp)
head -n ${number_of_nodes} ${filtered_patterns_file}'.probabilistic_network.desc' > "$node_file"
swipl -g "init_go('${network_file}', '${cadd_file}', '${filtered_patterns_file}', '${node_file}'), halt" generate_clustering_data.pl
graph_file=${node_file}'_1'
cannot_links_file=${node_file}'_1_cannot_links'
must_links_file=${node_file}'_1_must_links'
