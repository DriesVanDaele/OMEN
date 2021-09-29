#!/bin/bash
evaluated_output_file=$1
pattern_quality_threshold=$2

swipl -G30g -g "go('${evaluated_output_file}', ${pattern_quality_threshold}), halt" filter_patterns.pl
