:- set_prolog_stack(global, limit(100 000 000 000)).
:- set_prolog_stack(trail,  limit(20 000 000 000)).
:- set_prolog_stack(local,  limit(2 000 000 000)).

:- consult(connected_components).
:- consult(probabilistic_network).
:- use_module(library(apply)).

use_module(library(csv)).
edgeList(undirected_interaction(A, B), [A, B]).

init_go(Network_File, CADD_File, Weighted_Paths_File, Node_File) :-
    consult(Network_File),
    consult(CADD_File),
    go(Weighted_Paths_File, Node_File).

go(Weighted_Paths_File, Node_File) :-
    csv_read_file(Node_File, Rows),
    findall(Gene, member(row(Gene), Rows), Genes),
    nodes_connected_components(Genes, Connected_Components),
    findall(Connected_Component, 
            (member(Connected_Component, Connected_Components),
             length(Connected_Component, L),
             L > 20),
            Large_Components), 
    open(connected_components, write, S),
    forall(member(Connected_Component, Connected_Components), format(S, '~w~n', [Connected_Component])),
    close(S),
    generate_behrouz_files(Node_File, Weighted_Paths_File, Large_Components, []).

generate_behrouz_files(Node_File, Weighted_Paths_File, Large_Components, Additional_Edges) :-
    forall(nth1(Index, Large_Components, Nodes),
           (subnetwork(Nodes, Edges0),
	    append(Edges0, Additional_Edges, Edges1),
            maplist(edgeList, Edges1, Edges),
            atomic_list_concat([Node_File, '_', Index], Graph),
            atomic_list_concat([Graph, '_cannot_links'], Graph_Cannot_Links),
            atomic_list_concat([Graph, '_must_links'], Graph_Must_Links),
	    generate_csv_files(Weighted_Paths_File, Nodes, Edges, Graph, Graph_Cannot_Links, Graph_Must_Links))).

generate_csv_files(Weighted_Paths_File, Node_Set, Edges, Graph_File, Cannot_Links_File, Must_Links_File) :-
    findall(row(X, Y), (member([A, B], Edges), sort([A, B], [X0, Y0]), double_quote(X0, X), double_quote(Y0, Y)), Rows),
    my_csv_write_file(Graph_File, [row(node1, node2)| Rows]),
    findall(row(X1, X2, W), (genes_cannot_links(Node_Set, W-X10-X20), double_quote(X10, X1), double_quote(X20, X2)), Cannot_Link_Rows),
    my_csv_write_file(Cannot_Links_File, [row(node1, node2, weight) | Cannot_Link_Rows]),
    findall(row(X1, X2, W), (genes_must_links(Weighted_Paths_File, Node_Set, W-X10-X20), double_quote(X10, X1), double_quote(X20, X2)), Must_Link_Rows),
    my_csv_write_file(Must_Links_File, [row(node1, node2, weight) | Must_Link_Rows]).

my_csv_write_file(File, Rows) :-
    open(File, write, S),
    forall((member(Row, Rows), Row =.. [_ | Arguments], atomic_list_concat(Arguments, ',', Content)), format(S, '~w~n', [Content])),
    close(S).

double_quote(X, Y) :-
    atomic_list_concat(['"', X, '"'], Y).

genes_cannot_links(Xs, S-X1-X2) :-
    findall(S-P-X1-X2, 
            (member(X1, Xs),
             member(X2, Xs),
             \+ compare(>, X1, X2),
             X1 \== X2,
             gene_patient_probability(X1, P, S1),
             gene_patient_probability(X2, P, S2),
             S is S1 * S2),
            Output1),
    bagof(S1,
          P^member(S1-P-X1-X2, Output1),
          Ss),
          halting(Ss, S0),
          complement(S0, S).

genes_must_links(Weighted_Paths_File, Xs, S-X1-X2) :-
    consult(Weighted_Paths_File),
    findall(X-Y, X-Y, Answer),
    findall(P-Pair, probabilistic_network_node_pairs(Answer, Pair, P), Probabilistic_Network0),
    keysort(Probabilistic_Network0, PN_Pairs),
    member(X1, Xs),
    member(X2, Xs),
    compare(<, X1, X2),
    member(S-[X1, X2], PN_Pairs).

complement(X, Y) :-
    Y is 1 - X.


halting(Probabilities, Halting_Probability) :-
    maplist(complement, Probabilities, Complement_Probabilities),
    product(Complement_Probabilities, Halting_Probability).


product(Xs, P) :-
    length(Xs, L),
    L > 0,
    product_helper(Xs, 1, P).

product_helper([], Acc, P) :-
    P is Acc.
product_helper([X | Xs], Acc, P) :-
    product_helper(Xs, X * Acc, P).
