component_edges(Connected_Component, Edge_Selection, Relevant_Edges) :-
    findall([G1, G2], 
            (member([G1, G2], Edge_Selection),
	     member(G1, Connected_Component),
	     member(G2, Connected_Component)),
            Relevant_Edges).

connected_components(Node_Selection, Connected_Components) :-
    nodes_connected_components(Node_Selection, Connected_Components).

connected_components(Node_Selection, Edge_Selection, Connected_Components) :-
    nodes_connected_components_edge_set(Node_Selection, Connected_Components, Edge_Selection).

% probabilistic_network_nodes(+Normalized_Patterns, -Gene, -Total_Score)
probabilistic_network_nodes(Normalized_Patterns, Gene, Total_Score) :-
    findall(X1/A, (member(X1-X2, Normalized_Patterns), member(A, X2)), As),
    bagof(Score, member(Score/Gene, As), Scores),
    sumlist(Scores, Total_Score).

probabilistic_network_edges(Normalized_Patterns, Edge, Total_Score) :-
    findall(X1/A, (member(X1-X2, Normalized_Patterns), member_edge(A, X2)), As),
    bagof(Score, member(Score/Edge, As), Scores),
    sumlist(Scores, Total_Score).

probabilistic_network_node_pairs(Normalized_Patterns, Edge, Total_Score) :-
    findall(X1/A, (member(X1-X2, Normalized_Patterns), member_node_pair(A, X2)), As),
    bagof(Score, member(Score/Edge, As), Scores),
    sumlist(Scores, Total_Score).

member_edge(Edge, [X, Y | _]) :-
    sort([X, Y], Edge).
member_edge(Edge, [_ | Xs]) :-
    member_edge(Edge, Xs).

% ?- findall(Node_Pair, member_node_pair(Node_Pair, [1, 5, 3, 2]), Node_Pairs), writeln(Node_Pairs).
% Node_Pairs = [[1, 5], [1, 3], [1, 2], [3, 5], [2, 5], [2, 3]].
member_node_pair(Node_Pair, [X | Ys]) :-
    member(Y, Ys),
    sort([X, Y], Node_Pair).
member_node_pair(Node_Pair, [_ | Xs]) :-
    member_node_pair(Node_Pair, Xs).
