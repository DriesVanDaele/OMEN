#!/bin/bash

probabilistic_network="$1"
network="$2"

swipl -g "consult('${network}'), evaluate('${probabilistic_network}'), halt" probabilistic_network_ranking_desc.pl
