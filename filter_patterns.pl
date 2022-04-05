%:- set_prolog_stack(global, limit(220 000 000 000)).
%:- set_prolog_stack(trail,  limit(50*10**9)).
%:- set_prolog_stack(local,  limit(50*10**9)).
:- set_prolog_flag(stack_limit, 30000000000).

:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic gene_patient_probability_file/1.
:- dynamic gene_function_file/1.


go_relative(Evaluated_Output_File, Keep_Fraction) :-
    consult(Evaluated_Output_File),
    findall(Key, '-'(_Weight, '-'(Key, _)), Keys),
    msort(Keys, Key_Set),
    length(Key_Set, L),
    Index is L - floor(Keep_Fraction * L),
    nth0(Index, Key_Set, Threshold),% nth0 or nth1?
    apply_threshold(Threshold, Evaluated_Output_File, Filtered_File),
    format('written to ~q.~n', [Filtered_File]).

go(Evaluated_Output_File, Pattern_Quality_Threshold) :-
    consult(Evaluated_Output_File),
    apply_threshold(Pattern_Quality_Threshold, Evaluated_Output_File, Filtered_File),
    format('written to ~q.~n', [Filtered_File]).

apply_threshold(Pattern_Quality_Threshold, File, Filtered_File) :-
    findall('-'(Weight, Selection),
	    ('-'(Weight, '-'(S, Selection)),
	     S >= Pattern_Quality_Threshold),
	    Weight_Selection),
    atom_concat(File, '.filtered_', Filtered_File0),
    atom_concat(Filtered_File0, Pattern_Quality_Threshold, Filtered_File),
    open(Filtered_File, write, S),
    pairs_keys_values(Weight_Selection, Weights, Selections),
    normalize2(Weights, Normalized_Weights),
    pairs_keys_values(Normalized_Weight_Selection, Normalized_Weights, Selections),
    sort(Normalized_Weight_Selection, Normalized_Weight_Selection_Sorted),
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
