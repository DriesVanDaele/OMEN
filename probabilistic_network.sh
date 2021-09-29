#!/bin/bash

# specify the automatically generated experiment directory
filtered_path_file="$1"

swipl -g "evaluate('${filtered_path_file}'),
         halt" probabilistic_network_generation.pl
