#!/bin/bash

experiment_file=$1
output_file=$2

swipl -G30g -g "go('${experiment_file}', '${output_file}'), halt" evaluate_paths.pl

