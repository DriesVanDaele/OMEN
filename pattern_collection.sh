#!/bin/bash

# Since this manipulates a lot of data in the working directory,
# it's imperative that only a single instance of this script is
# running at any time.
shopt -s extglob

# generate the experiment files
parameterFiles=($(swipl -g "generate_experiment(Experiment_File), write(Experiment_File), halt" -l experiment_generator.pl | tr -d '[]' | tr ',' ' '))

# The number of pieces in which to split each experiment.
# This number has to be at least as high (ideally equal) as the 
# number of cores you wish to employ simultaneously
parts=20

# the archival directory storing the output and the experiment file
archivedir=output

for element in "${parameterFiles[@]}";
do
    # clean the working directory
    #rm -f *.pl.split*

    # split the experiment into $parts pieces
    swipl -g "split(${parts}, '${element}'), halt" -l experiment_splitter.pl
    split_output_files=()

    # generate a file containing the commands to be executed
    for split_file in ${element}.split+([0-9]);
    do
        split_output_files+=(\'"${split_file}".output\')
        test="'$split_file'"
        echo "consult(experiment_framework), generate_experiment("${test}"), halt" >> argfile
    done

    # run the commands in parallel
    #parallel --gnu --ungroup --joblog joblog --workdir $workdir --sshloginfile nodes2 --delay 1 "yap -z" :::: argfile
    parallel --progress --gnu --ungroup --delay 1 "yap -z " :::: argfile

    # merge the output into a single file
    split_output_files_list=$(IFS=,; echo "[${split_output_files[*]}]")
    # for some reason YAP ends up hanging on the findall/3, instead, stick to swipl
    swipl -G30g -g "consult(experiment_merger), merge(${split_output_files_list}, new_final_output_file), halt"
    #yap -z "consult(experiment_merger), merge(${split_output_files_list}), halt" > new_final_output_file
  
    # generate a random directory to write the output in
    unique_directory=$(head -c 500 /dev/urandom | LC_ALL=C tr -dc 'a-zA-Z0-9' | fold -w 20 | head -n 1)
    mkdir $archivedir/$unique_directory
    mv new_final_output_file $archivedir/${unique_directory}/output
    mv $element $archivedir/${unique_directory}/

    # clean the working directory
    ##mkdir ${unique_directory}
    for split_file in  ${element}.split+([0-9]);
    do
	echo 'not removing file'
        ##mv $split_file  ${unique_directory}/
        ##mv ${split_file}.output ${unique_directory}/

        #rm $split_file
        #rm ${split_file}.output
    done
    #rm argfile
done

shopt -u extglob
