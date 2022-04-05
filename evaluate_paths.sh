#!/bin/bash

experiment_file=$1
output_file=$2

swipl -g "go('${experiment_file}', '${output_file}'), halt" evaluate_paths.pl

