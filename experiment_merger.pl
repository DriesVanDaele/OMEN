:- consult(dslp).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- multifile gene_patient_probability_file/1.
:- multifile gene_function_file/1.
:- multifile network_file/1.
:- multifile output_file/1.
:- multifile selection/2.
:- multifile threshold/1.
:- multifile mcda/2.
:- multifile pattern_quality_metric_threshold/1.
:- multifile experiment_query/2.
:- multifile experiment_query/1.
:- multifile '-'/2.


% assume that the files that have to be merged were passed to yap, and their data is in the kb.
merge(Files, Output_File) :-
    forall(member(File, Files), consult(File)),
    findall(Weight-solution(Pattern), Weight-solution(Pattern), Weighted_Answers),
    dslp:postprocessing(Weighted_Answers, true, Results),
    open(Output_File, write, S),
    forall(member(Result, Results), format(S, '~q.~n', Result)),
    close(S).
