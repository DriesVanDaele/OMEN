:- module(dslp, [use_probabilistic_module/1,
		 query_prolog/5,
		 sample/3,
		 sample/4,
		 query_general/3,
		 query_specific/3,
		 query_specific/4,
		 query/5,
		 op(1200, yfx, ####),
		 op(1200, xfy, #),
		 op(1100, xfy, ##),
		 op(1200, xfy, ###)]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(pairs)).
:- use_module(library(apply)).

sample(Query, N, Threshold, Scaling_Factor) :-
    dslp_sampling(Query, N, Threshold, Scaling_Factor, Weighted_Answers),
    postprocessing(Weighted_Answers, false, Processed_Weighted_Answers),
    forall(member(A, Processed_Weighted_Answers),
           print_message(informational, weighted_answer(A))).

sample(Query, N, Threshold) :-
    dslp_sampling(Query, N, Threshold, 1, Weighted_Answers),
    postprocessing(Weighted_Answers, true, Processed_Weighted_Answers),
    forall(member(A, Processed_Weighted_Answers),
           print_message(informational, weighted_answer(A))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query predicates providing an interface to the inference system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query_prolog(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, Results) :-
    dslp_inference(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, Weighted_Answers),
    postprocessing(Weighted_Answers, false, Results).

query_general(Query, Threshold, Scaling_Factor) :-
    query(Query, false, Threshold, Scaling_Factor, true).

query_specific(Query, Threshold, Scaling_Factor) :-
    query(Query, true, Threshold, Scaling_Factor, true).

query_specific(Query, Threshold, Scaling_Factor, Is_Normalized) :-
    query(Query, true, Threshold, Scaling_Factor, Is_Normalized).

query(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, Is_Normalized) :-
    dslp_inference(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, Weighted_Answers),
    postprocessing(Weighted_Answers, Is_Normalized, Processed_Weighted_Answers),
    forall(member(A, Processed_Weighted_Answers),
           print_message(informational, weighted_answer(A))).

print_top_bottom(Top, Bottom, Xs) :-
    length(Best, Top),
    length(Worst, Bottom),
    append(Worst, _, Xs),
    append(_, Best, Xs),
    reverse(Worst, Ascending_Worseness),
    reverse(Best, Descending_Bestness),
    forall(member(X, Descending_Bestness),
           print_message(informational, weighted_answer(X))),
    forall(member(X, Ascending_Worseness),
           print_message(informational, weighted_answer(X))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog DCG for data presentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile prolog:message/3.

prolog:message(weighted_answer(X-Y)) -->
    [ '~w-~w.'-[X, Y]].

prolog:message(downstream_path(Path)) -->
    [ 'downstream(~w).~n'-[Path]].

prolog:message(upstream_path(Path)) -->
    [ 'upstream(~w).~n'-[Path]].

prolog:message(interesting_path(Path)) -->
    [ 'interesting_path(~w).~n'-[Path]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% message_hook/3 definition for user manipulation of the output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile user:message_hook/3.

path_to_quoted_path([], []).
path_to_quoted_path([H | T], [Quoted_H | Quoted_T]) :-
    atom_concat('\"', H, Partially_Quoted),
    atom_concat(Partially_Quoted, '\"', Quoted_H),
    path_to_quoted_path(T, Quoted_T).


user:message_hook(weighted_answer(Probability-walk_over_all_paths(_, Path)), _, Lines) :-
    path_to_quoted_path(Path, Quoted_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Path, Probability]]).

user:message_hook(weighted_answer(Probability-walk_over_all_paths(Path)), _, Lines) :-
    path_to_quoted_path(Path, Quoted_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Path, Probability]]).




% the one used by Lund
user:message_hook(weighted_answer(Probability-walk_over_all_paths_correct(_, Path)), _, Lines) :-
    Path = [upstream_path(Upstream_Path), downstream_path(Downstream_Path)],
    reverse(Upstream_Path, Reversed_Upstream_Path),
    path_to_quoted_path(Reversed_Upstream_Path, Quoted_Reversed_Upstream_Path),
    path_to_quoted_path(Downstream_Path, Quoted_Downstream_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Reversed_Upstream_Path, Probability]]),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Downstream_Path, Probability]]).

trace_json(edge(Movement, Gene1, Gene2, Type, Direction), Clean_Trace) :-
    path_to_quoted_path([Movement, Gene1, Gene2, Type, Direction], Clean_Trace).

user:message_hook(weighted_answer(Probability-walk_over_all_paths_correct2(_, Path, Trace)), _, Lines) :-
    maplist(trace_json, Trace, Clean_Trace),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w },'-[Clean_Trace, Probability]]).

user:message_hook(weighted_answer(Probability-walk_over_all_paths_correct(Path)), _, Lines) :-
    Path = [upstream_path(Upstream_Path), downstream_path(Downstream_Path)],
    reverse(Upstream_Path, Reversed_Upstream_Path),
    path_to_quoted_path(Reversed_Upstream_Path, Quoted_Reversed_Upstream_Path),
    path_to_quoted_path(Downstream_Path, Quoted_Downstream_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w },'-[Quoted_Reversed_Upstream_Path, Probability]]),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w },'-[Quoted_Downstream_Path, Probability]]).


user:message_hook(weighted_answer(Probability-connect_mutations(Gene_Percentile, Path)), _, Lines) :-
    write(user_error, Path),
    path_to_quoted_path(Path, Quoted_Path),
    write(user_error, Quoted_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Path, Probability]]).

user:message_hook(weighted_answer(Probability-explore_neighbourhood(_, Path)), _, Lines) :-
    path_to_quoted_path(Path, Quoted_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Path, Probability]]).

user:message_hook(weighted_answer(Probability-explore_neighbourhood(Path)), _, Lines) :-
    path_to_quoted_path(Path, Quoted_Path),
    print_message_lines(user_output, '', ['{ "path": ~w, "probability": ~w},'-[Quoted_Path, Probability]]).

user:message_hook(upstream_path(Path), _, Lines) :-
    true.

user:message_hook(downstream_path(Path), _, Lines) :-
    true.

user:message_hook(interesting_path(Path), _, Lines) :-
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	dSLP core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_probabilistic_module(File) :-
    atom_concat(File, '.internal_representation', DSLP_Module),
    parse_file(File, DSLP_Module),
    use_module(DSLP_Module).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates used for presenting the output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

postprocessing([], _, []).
postprocessing(Weighted_Answers, Performs_Normalization, Final_Answers) :-
    (  forall(member(X, Weighted_Answers), ground(X)) ->
       transpose_pairs(Weighted_Answers, Transposed_Weighted_Answers),
       pairs_keys_values(Transposed_Weighted_Answers, A, B),
       pairs_keys_values(Ordered_Weighted_Answers, B, A),
       writeln(compressing_go),
       compress(Ordered_Weighted_Answers, Unsorted_Results),
       msort(Unsorted_Results, Results)
    ;
       pairs_values(Weighted_Answers, Answers),
       sort(Answers, Answer_Set),
       findall(P-Answer_Substitution,
               (member(Answer_Substitution, Answer_Set),
                findall(Weight,
                        member(Weight-Answer_Substitution, Weighted_Answers),
                        Ls),
                sumlist(Ls, P)
               ),
               Results)
    ),
    (  Performs_Normalization ->
       pairs_keys(Results, Weights),
       pairs_values(Results, Answers),
       normalize(Weights, Probabilities),
       pairs_keys_values(Intermediate_Results, Probabilities, Answers)
    ;
       Intermediate_Results = Results
    ),
    sort(Intermediate_Results, Final_Answers).

compress([X], [X]).
compress([P1-Answer1, P2-Answer2 | Rest], Compressed) :-
    (  Answer1 == Answer2 ->
       P is P1 + P2,
       compress([P-Answer1 | Rest], Compressed)
    ;
       Compressed = [P1-Answer1 | Compressed_Rest],
       compress([P2-Answer2 | Rest], Compressed_Rest)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sampling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dslp_sampling(Query, N, Threshold, Scaling_Factor, Weighted_Answers) :-
    findall(P-Query,
            (between(1, N, _), stochastic_stepping_play(Query, Threshold, Scaling_Factor, _, 1, P)),
            Weighted_Answers).

stochastic_stepping_play(X, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P) :-
    (  X == true ->
       P0 = P,
       Scaling_Factor = Scaling_Factor_0
    ;  Threshold_Probability > P0 ->
           fail
    ;
       (  conjunction(X, (H,T)) ->
          stochastic_stepping_play(H, Threshold_Probability, Scaling_Factor_0, Scaling_Factor_1, P0, P1),
          stochastic_stepping_play(T, Threshold_Probability, Scaling_Factor_1, Scaling_Factor, P1, P)
       ;
          disjunction(X, Test, X1, X2) ->
              (Test ->
                   stochastic_stepping_play(X1, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P)
              ;
                   stochastic_stepping_play(X2, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P)
              )
       ;
          (  is_probabilistic(X, Predicate) ->
                 one_stochastic_step(X, Probability, Y),
                 (  ###(Predicate, Scaling_Factor_1) ->
                    conj_append(Y, dslp_set_scaling_factor(Scaling_Factor_0), New_Y)
                 ;
                    Scaling_Factor_1 = Scaling_Factor_0,
                    New_Y = Y
                 ),
                 P1 is P0 * Probability * Scaling_Factor_0,
                 stochastic_stepping_play(New_Y, Threshold_Probability, Scaling_Factor_1, Scaling_Factor, P1, P)
          ;
             (  X = dslp_set_scaling_factor(Scaling_Factor) ->
                true
             ;
                call(X),
                Scaling_Factor = Scaling_Factor_0
             ),
             P0 = P
          )
       )
    ).


one_stochastic_step(Goal, P, Chosen_Body) :-
    weighted_alternatives(Goal, Cumulative_Distribution, Bodies),
    choose(Goal, Cumulative_Distribution, Bodies, P, (Goal-Chosen_Body)).



weighted_alternatives(Goal, Cumulative_Distribution, Reversed_Options) :-
    findall(Probability-(Goal-Body),
            one_step(Goal, Probability, Body),
            Probability_Body_Pairs),
    pairs_keys_values(Probability_Body_Pairs, Weights, Options),
    weights(Weights, [Y | Ys]),
    maplist(divide_by(Y), [Y | Ys], Cumulative_Distribution),
    reverse(Options, Reversed_Options).

choose(Goal, Cumulative_Distribution, Options, Option_Probability, Option) :-
    random(P),
    match(Goal, Cumulative_Distribution, Options, P, Option_Probability, Option).

match(Goal, [P], [(Goal-Y)], _, P, (Goal-Y)).
match(Goal, [X1, X2 | Intervals_As_List], [(Head-Y)  | Options], Probability, P, Option) :-
    ( (Probability =< X1, Probability > X2) ->
          P is X1 - X2,
          Option = (Head-Y),
          Goal = Head
    ;
          match(Goal, [X2 | Intervals_As_List], Options, Probability, P, Option)
    ).

weights([], []).
weights([X | Xs], Cumulative_Weights) :-
    weights(Xs, [X], Cumulative_Weights).

weights([], Cumulative_Weights, Cumulative_Weights).
weights([X | Xs], [Base | Rest], Cumulative_Weights) :-
    X_Cumulative is X + Base,
    weights(Xs, [X_Cumulative, Base | Rest], Cumulative_Weights).

divide_by(Y, X, Z) :-
    Z is X / Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inference
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dslp_inference(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, Weighted_Answers) :-
    findall(P-Query,
            stepping_play(Query, Has_Sorted_Clauses, Threshold, Scaling_Factor, _, 1, P),
            Weighted_Answers).

stepping_play(X, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P) :-
    (  X == true ->
       P0 = P,
       Scaling_Factor = Scaling_Factor_0
    ;  
       (  conjunction(X, (H,T)) ->
          stepping_play(H, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_0, Scaling_Factor_1, P0, P1),
          stepping_play(T, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_1, Scaling_Factor, P1, P)
       ;
          disjunction(X, Test, X1, X2) ->
              (Test ->
	          stepping_play(X1, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P)
              ;
                  stepping_play(X2, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_0, Scaling_Factor, P0, P)
              )
       ;
          (  is_probabilistic(X, Predicate) ->
                 (Threshold_Probability > P0 ->
                     fail
                 ;
	         
  		    ( Presumptuous_Evaluation ->
		      one_presumptuous_step(X, P0, Scaling_Factor_0, Threshold_Probability, Y, P1)
		    ;
		      one_step(X, Probability, Y),
		      P1 is P0 * Probability * Scaling_Factor_0
		    ),
		    (  ###(Predicate, Scaling_Factor_1) ->
		      conj_append(Y, dslp_set_scaling_factor(Scaling_Factor_0), New_Y)
		    ;
		      Scaling_Factor_1 = Scaling_Factor_0,
		      New_Y = Y
		    ),
		    stepping_play(New_Y, Presumptuous_Evaluation, Threshold_Probability, Scaling_Factor_1, Scaling_Factor, P1, P)
                  )
          ;
             (  X = dslp_set_scaling_factor(Scaling_Factor) ->
                true
             ;  
                call(X),
                Scaling_Factor = Scaling_Factor_0
             ),
             P0 = P
          )
       )
    ).

is_probabilistic(Goal, Name/Arity) :-
    functor(Goal, Name, Arity),
    probabilistic_predicate(Name/Arity).

one_step(Goal, Probability, true) :-
    (Goal #### Probability_Expression),
    probability_at_call_time(Probability_Expression, Probability).

one_step(Goal, Probability, Body) :-
    ((Goal :- Body) #### Probability_Expression),
    probability_at_call_time(Probability_Expression, Probability).

one_presumptuous_step(Goal, P0, Scaling_Factor, Threshold_Probability, true, P1) :-
    (Goal #### Probability_Expression),
    is_stopping_probability(Probability_Expression, Is_Stopping_Probability),
    probability_at_call_time(Probability_Expression, Probability),
    P1 is P0 * Probability * Scaling_Factor,
    ( Threshold_Probability > P1 ->
          ( Is_Stopping_Probability ->
              fail
          ;
              !, fail
          )
     ;
          true
    ).
one_presumptuous_step(Goal, P0, Scaling_Factor, Threshold_Probability, Body, P1) :-
    ((Goal :- Body) #### Probability_Expression),
    is_stopping_probability(Probability_Expression, Is_Stopping_Probability),
    probability_at_call_time(Probability_Expression, Probability),
    P1 is P0 * Probability * Scaling_Factor,
    ( Threshold_Probability > P1 ->
          ( Is_Stopping_Probability ->
              fail
           ;
              !, fail
          )
     ;
          true
    ).

is_stopping_probability(Probabilistic_Expression, Is_Stopping_Probability) :-
    ( Probabilistic_Expression = ##(A, _), var(A) ->
         Is_Stopping_Probability = true
    ;
         Is_Stopping_Probability = false
    ).

% Expression should be deterministic, the cut is there solely for good measure
probability_at_call_time(Probability_Expression, Probability) :-
    (  Probability_Expression = ##(Probabilistic_Formula, Expression) ->
       call(Expression), !,
       Probability is Probabilistic_Formula
    ;
       Probability is Probability_Expression
    ).

conj_append(X, Ys, Ys) :-
    X == true, !.
conj_append(X, Ys, (X, Ys)):-
    X \== true,
    \+ conjunction(X), !.
conj_append((X1, Xs), Ys, (X1, Zs)):-
    conj_append(Xs, Ys, Zs).


conjunction(X, (H,T)) :-
    \+ var(X),
    X = (H,T).

conjunction(X) :-
    conjunction(X, _).

disjunction(X, Test, X1, X2) :-
    X = (Test -> X1 ; X2).

normalize(Weights, Probabilities) :-
    sumlist(Weights, Total_Weight),
    maplist(divide_by(Total_Weight), Weights, Probabilities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Program parsing (pre-processing)

probabilistic_infix_operator(#).

parse_file(Input_File, Output_File) :-
        style_check(-discontiguous),
        open(Input_File, read, Input_Stream),
        read_and_process_terms(Input_Stream, Clauses, Directives, Probabilistic_Predicates, Nonprobabilistic_Predicates),
        close(Input_Stream),
        sort(Nonprobabilistic_Predicates, Nonprobabilistic_Predicate_Set),
        sort(Probabilistic_Predicates, Probabilistic_Predicate_Set),
        open(Output_File, write, Stream),
        append([probabilistic_predicate/1, op(1200, yfx, ####), (####)/2, op(1200, xfy, ###), op(1100, xfy, ##)],
               Nonprobabilistic_Predicate_Set,
               Nonprob_Predicate_Set),
        write_module_declaration(Stream, Output_File, Nonprob_Predicate_Set),
        write_goals(Stream, Directives),
        forall(member(Probabilistic_Predicate, Probabilistic_Predicate_Set),
               format(Stream, 'probabilistic_predicate(~k).~n', [Probabilistic_Predicate])),
        write_clauses(Stream, Clauses),
        close(Stream).

write_module_declaration(Stream, Module_Name, Predicates) :-
    format(Stream, ':- module(~q, ~w).~n~n', [Module_Name, Predicates]).

write_goals(Stream, Directives) :-
    forall(member(Directive, Directives),
           format(Stream, '~q.~n', [Directive])).

write_clauses(Stream, Clauses) :-
    forall(member(Clause, Clauses),
           format(Stream, '~k.~n', [Clause])).

read_and_process_terms(Input_Stream, Clauses, Directives, Probabilistic_Predicates, Nonprobabilistic_Predicates) :-
        read_term(Input_Stream, Term, []),
        ( Term == end_of_file ->
              Directives = [],
              Clauses = [],
              Probabilistic_Predicates = [],
              Nonprobabilistic_Predicates = [],
              !
        ;
          is_goal(Term) ->
              Directives = [Term | Remaining_Directives],
              read_and_process_terms(Input_Stream, Clauses, Remaining_Directives, Probabilistic_Predicates, Nonprobabilistic_Predicates)
        ;
            dslp_clause_translation(Term, New_Term, Mode),
            ( Mode = probabilistic ->
                clause_to_predicate(New_Term, Predicate),
                Probabilistic_Predicates = [Predicate | Remaining_Probabilistic_Predicates],
                Remaining_Nonprobabilistic_Predicates = Nonprobabilistic_Predicates

             ;
                ( functor(New_Term, (:-), 2) ->
                    arg(1, New_Term, Actual_Term)
                ;
                    Actual_Term = New_Term
                ),
                functor(Actual_Term, Name, Arity),
                Nonprobabilistic_Predicates = [Name/Arity | Remaining_Nonprobabilistic_Predicates],
                Remaining_Probabilistic_Predicates = Probabilistic_Predicates
            ),
            Clauses = [New_Term | Remaining_Clauses],
            read_and_process_terms(Input_Stream, Remaining_Clauses, Directives, Remaining_Probabilistic_Predicates, Remaining_Nonprobabilistic_Predicates)
        ).

is_goal((:- _)).

dslp_clause_translation(External_Clause, Internal_Clause, Mode) :-
        ( functor(External_Clause, Name, 2), probabilistic_infix_operator(Name) ->
            arg(1, External_Clause, Probability_Expression),
            arg(2, External_Clause, Logic_Clause),
            Internal_Clause = (Logic_Clause #### Probability_Expression),
            Mode = probabilistic
        ;
            Internal_Clause = External_Clause,
            Mode = nonprobabilistic
        ).

clause_to_predicate(((X :- _) #### _), Name/Arity) :-
        functor(X, Name, Arity),
        !.
clause_to_predicate((X #### _), Name/Arity) :-
        functor(X, Name, Arity).
