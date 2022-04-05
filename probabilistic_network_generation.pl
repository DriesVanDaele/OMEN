%:- set_prolog_stack(trail,  limit(200*10**9)).
%:- set_prolog_stack(local,  limit(200*10**9)).
%:- set_prolog_stack(global, limit(200*10**9)).
:- set_prolog_flag(stack_limit, 30000000000).

:- use_module(library(csv)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- consult('example_data/network.pl').
:- consult(probabilistic_network).

evaluate(Answer_File) :-
    consult(Answer_File),
    findall(X-Y, X-Y, Answer0),
    Answer = Answer0,
    findall(row(P, Gene), probabilistic_network_nodes(Answer, Gene, P), Probabilistic_Network),
    atom_concat(Answer_File, '.probabilistic_network', Probabilistic_Network_File),
    csv_write_file(Probabilistic_Network_File, Probabilistic_Network).
