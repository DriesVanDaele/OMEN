:- dynamic undirected_interaction/2.

:- use_module(library(lists)).

edges_connected_components(Edges, Connected_Components) :-
    edges_connected_components(Edges, [], Connected_Components).

nodes_connected_components(Nodes, Connected_Components) :-
    nodes_connected_components(Nodes, [], Connected_Components).

% this is the one that uses all edges as edge-set
nodes_connected_components([], Connected_Components, Connected_Components).
nodes_connected_components([Node | Nodes], Partial_Components0, Connected_Components) :-
    node_connected_to_components(Node, Partial_Components0, Partial_Components),
    nodes_connected_components(Nodes, Partial_Components, Connected_Components).


nodes_connected_components_edge_set(Nodes, Connected_Components, Edge_Set) :-
    nodes_connected_components_edge_set(Nodes, [], Connected_Components, Edge_Set).

% find the connected components consisting of the given nodes, 
% where nodes are considered to belong to the same component if they are connected
% through edges in the edge set.
nodes_connected_components_edge_set([], Connected_Components, Connected_Components, _Edge_Set).
nodes_connected_components_edge_set([Node | Nodes], Partial_Components0, Connected_Components, Edge_Set) :-
    node_connected_to_components_edge_set(Node, Partial_Components0, Partial_Components, Edge_Set),
    nodes_connected_components_edge_set(Nodes, Partial_Components, Connected_Components, Edge_Set).


edges_connected_components([], Connected_Components, Connected_Components).
edges_connected_components([Edge | Edges], Partial_Components0, Connected_Components) :-
    edge_connected_to_components(Edge, Partial_Components0, Partial_Components),
    edges_connected_components(Edges, Partial_Components, Connected_Components).


% Assumption:
% The given components are not connected to eachother
%
% We distinguish 3 cases:
% 1) Node is not connected to any component in Components0
% 2) Node is connected to exactly one component in Components0
% 3) Node is connected to more than one component in Components0
%
% if 1) holds then Node becomes the start of a new component.
% if 2) holds then Node is added to its component in Components0
% if 3) holds then Node and all its connected components are merged.
%
% assume a component consists of a set of nodes (and represented as an
% unordered list that might contain duplicates
% Examples:
% ?- node_connected_to_components('AR', [['TP53', 'SP1', 'RNF4'], ['FAM46A', 'RNF14', 'UBE2D1']], [['AR', 'FAM46A', 'RNF14', 'RNF4', 'SP1', 'TP53', 'UBE2D1']]).
% ?- node_connected_to_components('AR', [['TP53'], ['FAM46A', 'RNF14', 'UBE2D1']], [['AR', 'FAM46A', 'RNF14', 'UBE2D1'], ['TP53']]).
% ?- node_connected_to_components('AR', [], [['AR']]).
node_connected_to_components(Node, Components0, [New_Component | Components1]) :-
    findall(C, (member(C, Components0), node_belongs_to_component(Node, C, _Explanation)), Cs),
    ( length(Cs, 0) ->
          New_Component = [Node]
    ;
          flatten(Cs, New_Component0),
          sort([Node | New_Component0], New_Component)
    ),
    findall(C, (member(C, Components0), \+ node_belongs_to_component(Node, C, _)), Components1).

node_belongs_to_component(Node, Component, Node) :-
    memberchk(Node, Component), !.
node_belongs_to_component(Node, Component, Node0) :-
    member(Node0, Component),
    ( undirected_interaction(Node, Node0)
    ; undirected_interaction(Node0, Node)
    ), !.

node_connected_to_components_edge_set(Node, Components0, [New_Component | Components1], Edge_Set) :-
    findall(C, (member(C, Components0), node_belongs_to_component_edge_set(Node, C, _Explanation, Edge_Set)), Cs),
    ( length(Cs, 0) ->
          New_Component = [Node]
    ;
          flatten(Cs, New_Component0),
          sort([Node | New_Component0], New_Component)
    ),
    findall(C, (member(C, Components0), \+ node_belongs_to_component_edge_set(Node, C, _, Edge_Set)), Components1).

node_belongs_to_component_edge_set(Node, Component, Node, _Edge_Set) :-
    memberchk(Node, Component), !.
node_belongs_to_component_edge_set(Node, Component, Node0, Edge_Set) :-
    member(Node0, Component),
    ( member([Node, Node0], Edge_Set)
    ; member([Node0, Node], Edge_Set)
    ), !.

edge_connected_to_components(Edge, Components0, [New_Component | Components1]) :-
    findall(C, (member(C, Components0), edge_belongs_to_component(Edge, C, _Explanation)), Cs),
    ( length(Cs, 0) ->
          New_Component = [Edge]
    ;
          flatten(Cs, New_Component0),
          sort([Edge | New_Component0], New_Component)
    ),
    findall(C, (member(C, Components0), \+ edge_belongs_to_component(Edge, C, _)), Components1).

edge_belongs_to_component(Edge, Component, Edge) :-
    memberchk(Edge, Component), !.
edge_belongs_to_component(Edge, Component, _Edge0) :-
    member([A, B], Component),
    ( Edge = [A, _]
    ; Edge = [B, _]
    ; Edge = [_, A]
    ; Edge = [_, B]
    ), !.
