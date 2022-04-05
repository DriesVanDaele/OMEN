%:- set_prolog_stack(global, limit(220 000 000 000)).
%:- set_prolog_stack(trail,  limit(50*10**9)).
%:- set_prolog_stack(local,  limit(50*10**9)).
:- set_prolog_flag(stack_limit, 30000000000).

:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic gene_patient_probability_file/1.
:- dynamic gene_function_file/1.

go(Experiment_File, File) :-
    consult(Experiment_File),
    gene_patient_probability_file(Gene_Patient_Probability_File),
    gene_function_file(Gene_Frequency_File),
    mcda(Type, [Alpha, Beta]),
    consult_background(Gene_Patient_Probability_File, Gene_Frequency_File),
    apply_threshold(mcda(Type, [alpha(Alpha), beta(Beta)]), File).

consult_background(Gene_Patient_Probability_File, Gene_Frequency_File) :-
    consult(Gene_Patient_Probability_File),
    consult(Gene_Frequency_File).

split(N, Data, Binned_Data) :-
    findall([], between(1, N, _), Bins),
    splitting_bins(Data, Bins, Binned_Data).

splitting_bins([], Bins, Bins).
splitting_bins([H | T], [Bin | Bins_Acc], Bins) :-
    Bin1 = [H | Bin],
    append(Bins_Acc, [Bin1], Bins_Acc1),
    splitting_bins(T, Bins_Acc1, Bins).

evaluate_pattern(Data, Metric_Info, Selection_Set) :-
    findall(Weight-(S-Selection),
	    (member(Weight-Selection, Data),
	     evaluation_metric(Metric_Info, Selection, S)),
	    Selection_Set).

apply_threshold(Metric_Info, File) :-
    current_prolog_flag(cpu_count, Number_Of_CPUs),
    Available_CPUs is Number_Of_CPUs - 3,
    consult(File),
    findall('-'(Weight, Selection), '-'(Weight, solution(Selection)), Data),
    split(Available_CPUs, Data, Split_Data),
    findall(evaluate_pattern(D, Metric_Info, Selection_Set),
	    member(D, Split_Data),
	    Goals),
    concurrent(Available_CPUs, Goals, []),
    maplist(arg(3), Goals, Selection_Sets),
    atom_concat(File, '.evaluated', Filtered_File),
    findall(Selected, 
	    (member(Selection_Set, Selection_Sets),
	     member(Selected, Selection_Set)),
	    Weight_Selection),
    sort(Weight_Selection, Normalized_Weight_Selection_Sorted),
    open(Filtered_File, write, S),
    forall(member(Selected, Normalized_Weight_Selection_Sorted),
	   format(S, '~q.~n', [Selected])),
    close(S).

normalize2(Xs, Ys) :-
    sumlist(Xs, T),
    ( T > 0 ->
          maplist(divide_by(T), Xs, Ys)
    ;
          Xs = Ys
    ).

divide_by(X, Y, Z) :-
    Z is Y / X.

evaluation_metric(Metric_Info, Genes, Score) :-
    build_matrix(Genes, Matrix),
    total_score(Genes, Matrix, Metric_Info, Score).

build_matrix(Genes, matrix(Matrix)) :-
    findall(G-Pa-Pr,
            (member(G, Genes),
             gene_patient_probability(G, Pa, Pr)),
            GPaPrs),
    findall(P, member(_-P-_, GPaPrs), Ps),
    sort(Ps, Patient_Set),
    findall(Scores, 
            (member(Patient, Patient_Set), 
             numbers(Genes, GPaPrs, Patient, Scores)),
            Matrix).

mutual_exclusivity_score(matrix(Samples), Result) :-
    maplist(mutual_exclusivity, Samples, Scores),
    length(Samples, N),
    sumlist(Scores, Total_Score),
    Result is Total_Score / N.

mutual_exclusivity(Variables, Mutual_Exclusivity) :-
    findall(Mutual_Exclusivity_Term,
            (select(Variable, Variables, Remaining_Variables),
             maplist(complement, Remaining_Variables, Complement_Remaining_Variables),
             product(Complement_Remaining_Variables, Mutual_Exclusivity_Term0),
             Mutual_Exclusivity_Term is Variable * Mutual_Exclusivity_Term0),
            Mutual_Exclusivity_Terms),
    sumlist(Mutual_Exclusivity_Terms, Mutual_Exclusivity).

complement(X, Y) :-
    Y is 1 - X.

% fails on the empty list
product(Xs, P) :-
    length(Xs, L),
    L > 0,
    product_helper(Xs, 1, P).

product_helper([], Acc, P) :-
    P is Acc.
product_helper([X | Xs], Acc, P) :-
    product_helper(Xs, X * Acc, P).


total_score(Genes, X, mcda(Type, Parameters), Y) :-
    pattern_frequency(Genes, Pattern_Frequency),
    X = matrix(A),
    transpose(A, Transposed_Matrix),
    maplist(sumlist, Transposed_Matrix, Gene_Mutation_Counts),
    normalize2(Gene_Mutation_Counts, Gene_Mutation_Distribution),
    entropy_based_genes_metric(Gene_Mutation_Distribution, Entropy_Term),
    mutual_exclusivity_score(X, Y0),

    ( Type == weighted_product ->
        memberchk(alpha(Alpha), Parameters),
        Y is Y0**Alpha * Pattern_Frequency**(1 - Alpha)
    ;    
      Type == weighted_sum ->
        memberchk(alpha(Alpha), Parameters),
        memberchk(beta(Beta), Parameters),
        Y is Alpha * Y0 + (1 - Alpha) * (Beta * Entropy_Term + (1-Beta) * Pattern_Frequency)
    ;
      throw(invalid_metric)
    ).

pattern_frequency(Genes, P) :-
    maplist(gene_mutsigcv, Genes, Probabilities),
    sumlist(Probabilities, P0),
    length(Probabilities, N),
    P is P0 / N.

default_probability(0).

numbers([], _, _, []).
numbers([G | Genes], GPaPrs, Patient, [P | Probabilities]) :-
    ( memberchk(G-Patient-P, GPaPrs) ->
        true
    ;
        default_probability(Default_P),
        Default_P = P
    ),
    numbers(Genes, GPaPrs, Patient, Probabilities).

transpose([], []).
transpose([A|C], B) :-
    foldl(A, B, [A|C], _).

foldl(List1, List2, V0, V) :-
    foldl_(List1, List2, V0, V).

foldl_([], [], V, V).
foldl_([H1|T1], [H2|T2], V0, V) :-
    transpose_(H1, H2, V0, V1),
    foldl_(T1, T2, V1, V).


transpose_(_, Fs, Lists0, Lists) :-
    maplist(list_first_rest, Lists0, Fs, Lists).

list_first_rest([L|Ls], L, Ls).


entropy_based_genes_metric(Genes, Result) :-
    genes_to_distribution(Genes, Distribution, N),
    ( Distribution = [] *->
        Result = 0
    ;
        entropy(Distribution, Entropy),
	Uniform_Distribution_P is 1 / N,
	length(Uniform_Distribution, N),
	maplist(=(Uniform_Distribution_P), Uniform_Distribution),
	entropy(Uniform_Distribution, Max_Entropy),
	Result is Entropy / Max_Entropy
    ).

genes_to_distribution(Gene_Scores, Distribution, N) :-
    length(Gene_Scores, N),
    include(=\=(0), Gene_Scores, Distribution0),
    normalize2(Distribution0, Distribution).

entropy(Values, Entropy) :-
    maplist(entropy_term, Values, Entropy_Terms),
    sumlist(Entropy_Terms, Entropy0),
    Entropy is -1 * Entropy0.

entropy_term(X, Y) :-
    A is log(X),
    B is log(2),
    C is A/B,
    Y is X * C.
