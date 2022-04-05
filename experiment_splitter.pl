:- set_prolog_flag(stack_limit, 30000000000).

split(N, Parameter_File) :-
    consult(Parameter_File),
    gene_patient_probability_file(Gene_Patient_Probability_File),
    path_definition_file(Path_Definition_File),
    gene_function_file(Gene_Function_File),
    network_file(Network_File),
    threshold(Threshold),
    pattern_quality_metric_threshold(Pattern_Quality_Metric_Threshold),
    mcda(Type, Alpha),
    experiment_query(Query),
    consult(Gene_Patient_Probability_File),
    genes(Gene_Set),
    length(Gene_Set, Total_Number_Of_Genes),
    split_n_parts(Gene_Set, N, Gene_Subsets),
    length(Gene_Subsets, N_Parts),
    forall((between(1, N_Parts, Index), nth1(Index, Gene_Subsets, Gene_Subset)),
           (atom_concat(Parameter_File, '.split', Split_Parameter_File0),
            atom_number(Index_Atom, Index),
            atom_concat(Split_Parameter_File0, Index_Atom, Split_Parameter_File),
            open(Split_Parameter_File, write, A),
            format(A, '~q.~n', [gene_patient_probability_file(Gene_Patient_Probability_File)]),
            format(A, '~q.~n', [path_definition_file(Path_Definition_File)]),
            format(A, '~q.~n', [gene_function_file(Gene_Function_File)]),
            format(A, '~q.~n', [network_file(Network_File)]),
	    format(A, '~q.~n', [threshold(Threshold)]),
	    format(A, '~q.~n', [pattern_quality_metric_threshold(Pattern_Quality_Metric_Threshold)]),
	    format(A, '~q.~n', [mcda(Type, Alpha)]),
            format(A, '~q.~n', [selection(Gene_Subset, Total_Number_Of_Genes)]),
            format(A, '~q.~n', [experiment_query(Query)]),
	    close(A))).

genes(Gene_Set) :-
    findall(Gene, gene_patient_probability(Gene, _, _), Genes),
    sort(Genes, Gene_Set).

split_n_parts(List, N, Sublists) :-
    length(List, M),
    Standard_Sublist_Length is floor(M/N),
    split_n_eles(List, Standard_Sublist_Length, Sublists).

split_n_eles(List, N, [List]) :-
    length(List, L),
    L < 2*N.
split_n_eles(List, N, [Sublist | Sublists]) :-
    length(List, L),
    L >= 2*N,
    length(Sublist, N),
    append(Sublist, Remainder, List),
    split_n_eles(Remainder, N, Sublists).
