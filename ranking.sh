#!/bin/bash

probabilistic_network="$1"

swipl -g "evaluate('${probabilistic_network}'),
         halt" probabilistic_network_ranking_desc.pl
