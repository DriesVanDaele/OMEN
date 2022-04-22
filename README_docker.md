# Note

The Docker file included here runs ONLY the pattern collection phase of the original experiment performed by OMEN.
Note that this is just one of multiple phases listed in README.md.
By specifying a different ENTRYPOINT, the Dockerfile can be modified to execute all subsequent steps in the process.

(Each of these steps are listed in main.py and can be executed using that interface
given that the software as specified in README.md is available. The purpose of this Dockerfile is solely 
to give an idea of what a proper software environment should look like to replicate our experiments)
 
Since the Dockerfile calls the shell scripts directly, you can run experiments using your own data by modifying the appropriate facts in  experiment_parameters.pl. 
Its facts can be adjusted to set your own data and choose your own settings (the default ones correspond to the original experiment discussed in the paper):

data('example_data/tokheim_pancancer_somatic_CADD.pl', 'example_data/tokheim_pancancer_somatic_coverage_ranks.pl').  
network_file('example_data/network.pl').  
threshold(10**(-6)).  
pattern_quality_metric_threshold(0.67).  
mcda(weighted_sum, [0.8, 0]).  
path_definition_file('example_data/path_definition.dslp').  

# build docker

$ sudo docker build -t omen .

assuming your working directory contains OMEN

-------------

# run docker

$ sudo docker run --rm  --name omen --mount type=bind,source=/home/user/temp_output,target=/generated_files/output omen

adjust host path as needed.
output directory to store directory containing the pattern output file



