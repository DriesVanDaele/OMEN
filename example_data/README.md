This directory contains the data used in the original experiments.

Note that when loading your own data it should be appropriately formatted.
That is
1) define the predicates:
- gene_patient_probability/3
- gene_coverage_helper/2
- undirected_interaction/2
2) include headers when appropriate (see the example data)
- the coverage file has a header defining gene_coverage/2 that has to be included
- the network file has a header defining subnetwork/2 and interaction/2 that has to be included
