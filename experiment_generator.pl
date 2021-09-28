data('example_data/tokheim_pancancer_somatic_CADD.pl', 'example_data/tokheim_pancancer_somatic_coverage_ranks.pl').
network_file('../data/network.pl').
threshold(10**(-6)).
pattern_quality_metric_threshold(0.5).
mcda(weighted_sum, [0.8, 0]).
path_definition_file('path_definition.dslp').
   
experiment_query(solution_3_genes(mcda(Type, [alpha(Alpha), beta(Beta)]), Pattern_Quality_Threshold, Selection), Selection, Type, Alpha, Beta, Pattern_Quality_Threshold).

generate_experiment(Experiment_Files) :-
    findall(Generator_File,
    ((data(Gene_Patient_Probability_File, Gene_Function_File),
     path_definition_file(Path_Definition_File),
     network_file(Network_File),
     threshold(Threshold0), 
     Threshold is Threshold0,
     atom_number(Threshold_Atom, Threshold),

     pattern_quality_metric_threshold(Metric_Quality_Threshold0), 
     Metric_Quality_Threshold is Metric_Quality_Threshold0,
     atom_number(Metric_Quality_Threshold_Atom, Metric_Quality_Threshold),

     mcda(Type, [MCDA_Alpha0, MCDA_Beta0]), 
     MCDA_Alpha is MCDA_Alpha0,
     MCDA_Beta is MCDA_Beta0,
     atom_number(MCDA_Alpha_Atom, MCDA_Alpha),
     atom_number(MCDA_Beta_Atom, MCDA_Beta),

     experiment_query(Experiment_Query, _, Type, MCDA_Alpha, MCDA_Beta, Metric_Quality_Threshold),

     functor(Experiment_Query, Query_Name, Query_Arity),
     term_to_atom(Query_Name-Query_Arity, Experiment_Query_Atom),
     file_base_name(Gene_Patient_Probability_File, G_P_P_Base),
     file_base_name(Gene_Function_File, G_F_Base),
     file_base_name(Path_Definition_File, P_D_Base),
     file_base_name(Network_File, N_Base),
     atomic_list_concat([G_P_P_Base, G_F_Base, N_Base, Threshold_Atom, Metric_Quality_Threshold_Atom, MCDA_Alpha_Atom, MCDA_Beta_Atom, P_D_Base, Experiment_Query_Atom], File_Base),
     atom_concat(new_experiment_, File_Base, Generator_Base),
     atom_concat(Generator_Base, '.pl', Generator_File)
   ), 
   ( open(Generator_File, write, A),
     format(A, '~q.~n', [gene_patient_probability_file(Gene_Patient_Probability_File)]),
     format(A, '~q.~n', [path_definition_file(Path_Definition_File)]),
     format(A, '~q.~n', [gene_function_file(Gene_Function_File)]),
     format(A, '~q.~n', [network_file(Network_File)]),
     format(A, '~q.~n', [threshold(Threshold)]),
     format(A, '~q.~n', [pattern_quality_metric_threshold(Metric_Quality_Threshold)]),
     format(A, '~q.~n', [mcda(Type, [MCDA_Alpha, MCDA_Beta])]),
     format(A, '~q.~n', [experiment_query(Experiment_Query)]),
     close(A)
   )),
   Experiment_Files).
