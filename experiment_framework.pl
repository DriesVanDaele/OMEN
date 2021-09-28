:- use_module(library(system)).

:- dynamic gene_patient_probability_file/1.
:- dynamic path_definition_file/1.
:- dynamic gene_function_file/1.
:- dynamic network_file/1.
:- dynamic output_file/1.
:- dynamic selection/2.
:- dynamic threshold/1.
:- dynamic mcda/2.
:- dynamic pattern_quality_metric_threshold/1.
:- dynamic experiment_query/2.

generate_experiment(File) :-
    consult(File),
    use_module(dslp),
    use_module(library(apply)),
    use_module(library(lists)),
    path_definition_file(Path_Definition_File),
    use_probabilistic_module(Path_Definition_File),
    gene_patient_probability_file(Gene_Patient_Probability_File),
    gene_function_file(Gene_Function_File),
    network_file(Network_File),
    threshold(Threshold),
    mcda(Type, [Alpha, Beta]),
    pattern_quality_metric_threshold(Metric_Quality_Threshold),
    selection(Gene_Subset, Total_Number_Of_Genes),
    experiment_query(Experiment_Query),
    tmpnam(Genes_Selection_File),
    open(Genes_Selection_File, write, Temp_Stream),
    format(Temp_Stream, '~q.~n', [selection(Gene_Subset, Total_Number_Of_Genes)]),
    close(Temp_Stream),
    query_prolog((consult_background(Gene_Patient_Probability_File, Gene_Function_File, Network_File, Genes_Selection_File), Experiment_Query), false, Threshold, 1, Results),
    ( output_file(Output_File) -> true ; atom_concat(File, '.output', Output_File) ),
    open(Output_File, write, S),
    format(S, '~q.~n', [gene_patient_probability_file(Gene_Patient_Probability_File)]),
    format(S, '~q.~n', [gene_function_file(Gene_Function_File)]),
    format(S, '~q.~n', [network_file(Network_File)]),
    format(S, '~q.~n', [threshold(Threshold)]),
    format(S, '~q.~n', [pattern_quality_metric_threshold(Metric_Quality_Threshold)]),
    format(S, '~q.~n', [mcda(Type, [Alpha, Beta])]),
    format(S, '~q.~n', [experiment_query(Experiment_Query)]),
    forall(member(P-(_,Experiment_Query_Sel), Results),
           (Experiment_Query_Sel =.. [_, _, _, Solution],
            format(S, '~q.~n', [P-solution(Solution)]))),
    close(S).
