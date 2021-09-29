:- set_prolog_stack(trail,  limit(200*10**9)).
:- set_prolog_stack(local,  limit(200*10**9)).
:- set_prolog_stack(global, limit(200*10**9)).

:- use_module(library(csv)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- consult('example_data/network.pl').
:- consult(probabilistic_network).

evaluate(Probabilistic_Network_File) :-
    csv_read_file(Probabilistic_Network_File, Rows),
    findall(P-G, member(row(P, G), Rows), Probabilistic_Network0),
    keysort(Probabilistic_Network0, Best0),
    reverse(Best0, Best),
    findall(row(Positive), member(_-Positive, Best), Desc_Ranking),
    atom_concat(Probabilistic_Network_File, '.desc', Desc_Ranking_File),
    csv_write_file(Desc_Ranking_File, Desc_Ranking).
