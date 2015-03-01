% -*- Mode: Prolog -*-
% Updated code to reflect missing max_integer and min_integer values
% (recent SWI Prolog releases support unlimited precision arithmetic)
% The rather arbitrary values 9999 and -9999 are used instead.
%                                                         20060302 RM
%
% Changed parititioning algorithm to produce incomplete matchings in
% order to save time and (especially) heap space requirements. A small
% modification to min_uncovered_weight was necessary for this to
% work; the predicate now fails instead of succeeding with maxint if
% no uncovered weight is found. Also, to prevent partial solutions we
% keep need to remember the number of columns in the full graph.
%                                                         20050923 RM
%
% Added partitioning solution for binding the k-best solutions using
% the Hungariann method. A heap containing problem-solution pairs is
% maintained for this.                                    20050922 RM
%
% Added the Hungarian method for finding minimum weight perfect
% matchings in bipartite graphs. Haven't implemented partitioning for
% finding the k-best solutions yet.                       20050920 RM
%
% This module contains Richard A. O'Keefe's warshall/2 predicate from
% the DEC-10 Prolog Library modified to work when the graph is a
% 234-tree instead of a list.                             20030605 RM

:- module(graph, [delete_vertex/3,
                  delete_edge/4,
		  delete_edges/3,
		  insert_vertex/4,
		  insert_edge/4,
		  put_edge/4,
		  put_edges/3,
		  reverse_graph/2,
		  dfs_tree/2,
		  remove_weights/2,
		  add_destinations/2,
		  add_weights/3,
		  gabow_strong/2,
                  rt_closure/2,
	          reflexive/2,
		  warshall/2,
		  floyd_warshall/2,
		  regin/4,
		  kbest/4,
		  kbest/5,
		  hungarian/2,
		  hungarian/3,
		  hungarian/4,
		  hungarian_max/2,
		  hungarian_max/3,
		  random_matching/3,
		  graph2dot/2,
		  graph2dot/1]).

:- use_module(ordset,  [ord_union/3,
	                ord_intersect/3,
	                ord_member/2,
			ord_key_member/3,
			ord_key_select/4,
			ord_select/3,
	                ord_insert/3,
			ord_delete/3]).
:- use_module(tree234, [btree_get/3,
	                btree_get_replace/5,
			btree_remove/4,
	                btree_insert/4,
			btree_member/3,
			btree_keys/2,
			btree_map_values/3,
	                list_to_btree/2]).
:- use_module(heap,    [empty_heap/1,
	                add_to_heap/4,
			get_from_heap/4]).
:- use_module(list_utils, [strip_values/2]).

% = regin(+Graph, +Columns, -Vital, -Removed)
%
% Implements Regin's algorithm for the all_different constraint on
% a Graph with Columns, computing the Vital edges - which are part
% of every matching - and the Removed edges - which are part of no
% matching.
%
% We start by computing the matching, which should be total, the we
% compute a graph G4 which contains all edges in the matching and
% the reverse of the edges not in the matching. We then compute the
% strong components of this graph, which will correspond to
% groups of coherent alternatives for the current matching.
% Finally, we compute the Vital and Removed edges using the matching
% and the components.

regin(G0, MC, VI, RE) :-
	hungarian(G0, MC, M, _W),
	remove_weights(G0, G1),
	delete_edges(M, G1, G2),
	reverse_graph(G2, G3),
	put_edges(M, G3, G4),
	add_destinations(G4, G),
	btree_to_list(G, L),
	/* there are no free vertices in the graph given that M is a total */
        /* matching, so we skip the depth first search from a free vertex */ 
        /* as described in Regin's paper */
	gabow_strong(G, SC),
	components_to_btree(SC, empty, T),
	remove_edges(L, T, M, VI, [], RE, []).

% = components_to_btree(+ListOfComponents, +BTreeIn, -BTreeOut)
%
% construct a BTree matching ever vertex to its corresponding
% component, which is represented as the list of its vertices.
% It might be more time and space efficient to use components
% *numbers* instead - as in the original description of the
% algorithm - especially if the components are large.
% Note the the strong components algorithm could be modified
% to produce these structures directly.

components_to_btree([], T, T).
components_to_btree([C|Cs], T0, T) :-
	component_to_btree(C, C, T0, T1),
	components_to_btree(Cs, T1, T).

% = components_to_btree(+ListOfVertices, +Component, +BTreeIn, -BTreeOut)
%
% add the complete Component as the value in the BTree for each of its
% vertices in the list.

component_to_btree([], _, T, T).
component_to_btree([A|As], C, T0, T) :-
	btree_put(T0, A, C, T1),
	component_to_btree(As, C, T1, T).

% = remove_edges(+Graph, +Components, +Matching, +VitalIn, -VitalOut,
%                                                +RemoveIn, -RemoveOut)
%
% using the Matching and the Components, decide which edges of Graph
% are Vital and which edges should be Removed. For every vertex V
% of Graph with edges Es, we look at those edges which are not in V's
% component: if they are part of the matching these edges are vital,
% if not, they should be removed.

remove_edges([], _, _, VI, VI, RE, RE).
remove_edges([V-Es|Vs], SC, M, VI0, VI, RE0, RE) :-
	btree_get(SC, V, VC),
	ord_subtract(Es, VC, Unused),
	remove_edges1(Unused, V, M, VI0, VI1, RE0, RE1),
	remove_edges(Vs, SC, M, VI1, VI, RE1, RE).

remove_edges1([], _, _, VI, VI, RE, RE).
remove_edges1([U|Us], V, M, VI0, VI, RE0, RE) :-
    (
	/* if edge V-U, which connects two different strong components */
        /* is a member of the matching it is a vital edge, otherwise we */
        /* remove it */
	ord_key_member(V, M, U)
    ->
	VI0 = [V-U|VI1],
	RE0 = RE1
    ;
	VI0 = VI1,
	RE0 = [V-U|RE1]
    ),
	remove_edges1(Us, V, M, VI1, VI, RE1, RE).

% = dfs_tree(+Graph, -Tree)
%
% true if Tree is the depth first search tree of Graph.

dfs_tree(G, Tree) :-
	btree_keys(G, Keys),
	dfs_tree(Keys, G, [], _, Tree).

% dfs_tree(+Unvisited, +Graph, +VisitedIn, -VisitedOut, ?Tree)

dfs_tree([], _, A, A, []).
dfs_tree([K|Us0], G, A0, A, [T|Ts]) :-
	dfs_tree1(K, G, A0, A1, Us0, Us, T),
	dfs_tree(Us, G, A1, A, Ts).

dfs_tree1(K, G, A0, A, Us0, Us, node(K,Ds)) :-
	% mark K as visited
	ord_insert(A0, K, A1),
	ord_delete(Us0, K, Us1),
	% find K's daughters
	btree_get(G, K, Vs0),
	% remove the daughters we've already visited
	ord_subtract(Vs0, A1, Vs),
	dfs_tree2(Vs, G, A1, A, Us1, Us, Ds).

dfs_tree2([], _, A, A, U, U, []).
dfs_tree2([K|Vs], G, A0, A, U0, U, [D|Ds]) :-
	dfs_tree1(K, G, A0, A1, U0, U1, D),
	dfs_tree2(Vs, G, A1, A, U1, U, Ds).

% = gabow_strong(+Graph, -StrongComponentList)
%
% true if StrongComponentList is the list of strong components in
% Graph, computed using Gabow's algorithm.
%
% as a first step, we collect the vertices of the graph - which are
% the Keys of the 234-tree and pass these on along to
% gabow_strong1/4with an empty accululator which will contain the
% strong components SC.

gabow_strong(G, SC) :-
	btree_keys(G, Keys),
	gabow_strong1(Keys, G, [], SC).

% = gabow_strong1(+Unvisited, +Graph, +ComponentsIn, -ComponentsOut)
%
% performs depth first search on the first unvisited node while
% updating the list of Unvisited nodes and the Components found,
% ending when all nodes have been visited.

gabow_strong1([], _, SC, SC).
gabow_strong1([K|Unv0], G, SC0, SC) :-
	% = perform DFS on the first unvisited vertex
	gabow_dfs(K, Unv0, Unv, G, empty, _, [], [], [], [], 0, _, SC0, SC1),
	gabow_strong1(Unv, G, SC1, SC).

% = gabow_dfs(+Vertex, +UnvI, -Unv0, +Graph, +AuxI, -AuxO, +PathI, -PathO,
%             +BI, -BO, +LI, -LO, +SCI, -SC0
%
% perform Gabow's depth first search algorithm. We'll need to update
% quite a few of the arguments:
%
% Aux is an auxiliary 234-tree which keeps track of the stack position
% of the items in the Path stack, allowing us to verify not only
% whether a vertex is on the path, but also at which position
%
% Path is the search path, implemented as a stack of vertex identifiers
%
% B is Gabow's second stack containing the indices of items of the Path
% stack which are low points.
%
% L is the length of the path
%
% SC is the list of strong components.
%
% For every node, we insert it in the auxiliary stack, add it to the
% path stack and its path stack position to stack B, then visit the
% daughters.
%
% If after having visited all descendants of this node it's on top of
% stack B, we pop all items up until this one from the path stack and
% add them to the next strong component.

gabow_dfs(K, Unv0, Unv, G, PG0, PG, Path0, Path, B0, B, L0, L, SC0, SC) :-
	btree_insert(PG0, K, L0, PG1),
	btree_get(G, K, Daughters),
	L1 is L0 + 1,
	map_gabow_dfs(Daughters, Unv0, Unv, G, PG1, PG, [K|Path0], Path1, [L0|B0], B1, L1, L2, SC0, SC1),	
    (
	B1 = [L0|B]
    ->
	gabow_pop_component(Path1, L2, L0, [], Comp, Path),
	SC = [Comp|SC1],
	L = L0
    ;
	Path = Path1,
	B = B1,
	SC = SC1,
	L = L2
    ).

% map_gabow_dfs
%
% Visit the child nodes of a vertex. There are three cases to consider:
% 1. child C is already on the path at position P, in which case we pop
%    all items which occur after P.
% 2. child C is already a member of another component, in which case
%    we do nothing, since we can't reach C from that component.
% 3. in all other cases, we perform a recursive depth first search on C.

map_gabow_dfs([], Unv, Unv, _, PG, PG, Path, Path, B, B, L, L, SC, SC).
map_gabow_dfs([V|Vs], Unv0, Unv, G, PG0, PG, Path0, Path, B0, B, L0, L, SC0, SC) :-
    (
	/* if V is already on the path we pop from B */
	btree_get(PG0, V, DV)
    ->
	Unv2 = Unv0,
	PG1 = PG0,
	Path1 = Path0,
	L1 = L0,
	SC1 = SC0,
	gabow_pop_b(B0, DV, B2)
    ;
	B1 = B0,
        (
	    /* if V is unvisited, we perform DFS */
	    ord_select(V, Unv0, Unv1)
	->
	    gabow_dfs(V, Unv1, Unv2, G, PG0, PG1, Path0, Path1, B1, B2, L0, L1, SC0, SC1)
        ;
	    /* V is member of another component, we do nothing */
	    Unv2 = Unv0,
	    PG1 = PG0,
            Path1 = Path0,
	    L1 = L0,
            B2 = B1,
	    SC1 = SC0
        )
    ),
	map_gabow_dfs(Vs, Unv2, Unv, G, PG1, PG, Path1, Path, B2, B, L1, L, SC1, SC).

% gabow_pop_b(+BStack, +Item, -BStack)
%
% update BStack popping all elements bigger than Item

gabow_pop_b([], _, []).
gabow_pop_b([B|Bs0], C, Bs) :-
    (
	B =< C
    ->
	Bs = [B|Bs0]
    ;
	gabow_pop_b(Bs0, C, Bs)
    ).

% gabow_pop_component(+Path, +Begin, +End, +Cmp, -Cmp, -Path)
%
% pop all items from Begin to End from the Path stack and insert
% them into an ordered component Cmp.

gabow_pop_component([], _, _, Cmp, Cmp, []).
gabow_pop_component([A|As0], L0, C, Cmp0, Cmp, Bs) :-
    (
	C < L0
    ->
	ord_insert(Cmp0, A, Cmp1),
	L is L0 - 1,
	gabow_pop_component(As0, L, C, Cmp1, Cmp, Bs)
    ;
	Cmp = Cmp0,
	Bs = [A|As0]
    ).

% = add_destinations(+Graph, -Graph)
%
% add vertices which are marked as daughters of other vertices
% in the graph as new vertices.

add_destinations(G0, G) :-
	/* get all source vertices S and destination vertices D0 */
	btree_foldl(G0, graph:source_dest_vertices, []-[], S-D0),
	/* D constains all destinations which are not vertices */
	ord_subtract(D0, S, D),
	/* so we add all of them */
	add_destinations(D, G0, G).

add_destinations([], G, G).
add_destinations([D|Ds], G0, G) :-
	insert_vertex(G0, D, [], G1),
	add_destinations(Ds, G1, G).

source_dest_vertices(K, V, S0-D0, S-D) :-
	ord_insert(S0, K, S),
	ord_union(D0, V, D).

% = insert_vertex(+Graph, +Vertex, +Daughters, -Graph).
%
% add Vertex-Daughters to Graph
% fails if Vertex alreads exists in Graph.

insert_vertex(G0, V, L, G) :-
	btree_insert(G0, V, L, G).

% = delete_vertex(+Graph, +Vertex, -Graph).
%
% delete Vertex from Graph.
% fails if Vertex doesn't exist in Graph.

delete_vertex(G0, V, G) :-
	btree_remove(G0, V, _, G).

% = delete_edges(+ListOfEdges, +Graph, -Graph)
%
% delete all Source-Target edges from Graph
% fails if any of the edges doesn't exist.

delete_edges([], G, G).
delete_edges([S-T|Es], G0, G) :-
	delete_edge(G0, S, T, G1),
	delete_edges(Es, G1, G).

% = delete_edges(+ListOfEdges, +Graph, -Graph)
%
% delete Source-Target edge from Graph
% fails if there is no such edge.

delete_edge(G0, S, T, G) :-
	btree_get_replace(G0, S, Es0, Es, G),
	ord_select(T, Es0, Es).

% = reverse_graph(+Graph, -Reverse)
%
% true if Graph and Reverse correspond to the same graph with all edge
% directions reversed.

reverse_graph(G0, G) :-
	delete_all_edges(G0, G1),
	reverse_graph(G0, G1, G).

reverse_graph(empty, G, G).
reverse_graph(two(K,V,T0,T1), G0, G) :-
	reverse_graph(T0, G0, G1),
	put_edges(V, K, G1, G2),
	reverse_graph(T1, G2, G).
reverse_graph(three(K0,V0,K1,V1,T0,T1,T2), G0, G) :-
	reverse_graph(T0, G0, G1),
	put_edges(V0, K0, G1, G2),
	reverse_graph(T1, G2, G3),
	put_edges(V1, K1, G3, G4),
	reverse_graph(T2, G4, G).
reverse_graph(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), G0, G) :-
	reverse_graph(T0, G0, G1),
	put_edges(V0, K0, G1, G2),
	reverse_graph(T1, G2, G3),
	put_edges(V1, K1, G3, G4),
	reverse_graph(T2, G4, G5),
	put_edges(V2, K2, G5, G6),
	reverse_graph(T3, G6, G).

% = delete_all_edges(+Graph, -Graph)
%
% remove all edges from the Graph while keeping all vertices.

delete_all_edges(empty, empty).
delete_all_edges(two(K,_,T0,T1), two(K,[],T2,T3)) :-
	delete_all_edges(T0, T2),
	delete_all_edges(T1, T3).
delete_all_edges(three(K0,_,K1,_,T0,T1,T2), three(K0,[],K1,[],T3,T4,T5)) :-
	delete_all_edges(T0, T3),
	delete_all_edges(T1, T4),
	delete_all_edges(T2, T5).
delete_all_edges(four(K0,_,K1,_,K2,_,T0,T1,T2,T3), four(K0,[],K1,[],K2,[],T4,T5,T6,T7)) :-
	delete_all_edges(T0, T4),
	delete_all_edges(T1, T5),
	delete_all_edges(T2, T6),
	delete_all_edges(T3, T7).

% = put_edges(+EdgeList, +InGraph, -OutGraph)
%
% true if OutGraph is InGraph with all (new) edges in EdgeList added 
% to it.

put_edges([], G, G).
put_edges([S-T|Es], G0, G) :-
	put_edge(G0, S, T, G1),
	put_edges(Es, G1, G).

% = put_edges(+Sources, +Target, +InGraph, -OutGraph)

put_edges([], _, G, G).
put_edges([V|Vs], K, G0, G) :-
	put_edge(G0, V, K, G1),
	put_edges(Vs, K, G1, G).

% = put_edge(+InGraph, +Source, +Target, -OutGraph)
%
% true if OutGraph is InGraph with the edge Source-Target added.
% adds Target if Source already exists.

put_edge(G0, S, T, G) :-
    (
	btree_get_replace(G0, S, Ts0, Ts, G)
    ->
	ord_insert(Ts0, T, Ts)
    ;
	btree_insert(G0, S, [T], G)
    ).

% = remove_weights(+WeighedGraph, -UnweighedGraph)
%
% change a weighed graph into un unweighed graph by removing all weights.

remove_weights(empty, empty).
remove_weights(two(K,V0,T0,T1), two(K,V1,T2,T3)) :-
	remove_weights(T0, T2),
	strip_values(V0,V1),
	remove_weights(T1, T3).
remove_weights(three(K0,V0,K1,V1,T0,T1,T2), three(K0,V2,K1,V3,T3,T4,T5)) :-
	remove_weights(T0, T3),
	strip_values(V0, V2),
	remove_weights(T1, T4),
	strip_values(V1, V3),
	remove_weights(T2, T5).
remove_weights(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	remove_weights(T0, T4),
	strip_values(V0, V3),
	remove_weights(T1, T5),
	strip_values(V1, V4),
	remove_weights(T2, T6),
	strip_values(V2, V5),
	remove_weights(T3, T7).	

% = add_weights(+WeighedGraph, :Pred, -UnweighedGraph)
%
% change a unweighed graph into un unweighed graph by computing and
% adding weights.

add_weights(empty, _, empty).
add_weights(two(K,V0,T0,T1), WF, two(K,V1,T2,T3)) :-
	add_weights(T0, WF, T2),
	call(WF, V0, V1, K),
	add_weights(T1, WF, T3).
add_weights(three(K0,V0,K1,V1,T0,T1,T2), WF, three(K0,V2,K1,V3,T3,T4,T5)) :-
	add_weights(T0, WF, T3),
	call(WF, V0, V2, K0),
	add_weights(T1, WF, T4),
	call(WF, V1, V3, K1),
	add_weights(T2, WF, T5).
add_weights(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), WF, four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	add_weights(T0, WF, T4),
	call(WF, V0, V3, K0),
	add_weights(T1, WF, T5),
	call(WF, V1, V4, K1),
	add_weights(T2, WF, T6),
	call(WF, V2, V5, K2),
	add_weights(T3, WF, T7).	

% = kbest(+K, +Graph, -Matching, -Weight)
%
% true if Graph has a Matching of Weight and this solution is less
% than or equal to the k-best solution.
% simply computes the best solution and puts it on a fresh heap, then
% iterates over the solutions on the heap.

kbest(K, Graph, Matching, Weight) :-
	K > 0,
	columns(Graph, MC),
	hungarian(Graph, MC, Matching0, Weight0),
	empty_heap(H0),
	add_to_heap(H0, Weight0, Graph-Matching0, H),
	kbest1(H, K, MC, Matching, Weight).

kbest(K, Graph, MC, Matching, Weight) :-
	K > 0,
	hungarian(Graph, MC, Matching0, Weight0),
	empty_heap(H0),
	add_to_heap(H0, Weight0, Graph-Matching0, H),
	kbest1(H, K, MC, Matching, Weight).


% = kbest1(+Heap, +K, -Matching, -Weight)
%
% true if Heap contains problem-solution pair <= K with solution
% Matching and of Weight.
% gets the current best solution from the heap and if we are still
% interested in further solutions, partitions the problem for which
% we just found the best solution into disjoint subproblems all
% different from the current one and without overlapping solutions.

kbest1(H0, K0, MC, Matching, Weight) :-
	get_from_heap(H0, Weight0, Graph0-Matching0, H1),
    (
	Matching = Matching0,
	Weight = Weight0
    ;
	/* do we still need more solutions? */
	K0 > 1,
	K is K0 - 1,
	partition(Matching0, Graph0, MC, H1, H),
	kbest1(H, K, MC, Matching, Weight)
    ).

% = partition(+Matching, +Graph, +Heap, +Heap)
%
% update Graph in such a way that Matching is no longer a
% minimum-weight solution, either by demanding that the first pair be
% absent from the next solution or by demanding it is present but
% that one of the following pairs is different.
% if a new solution is found, it is added to the heap.

partition([], _, _, H, H).
partition([I-J|Is], G0, MC, H0, H) :-
	btree_get_replace(G0, I, V0, V1, G1),
	copy_term(G1-V1, G2-V2),
	partition_list(V0, J, V1, V2),
	partition_graph(G2, I, J, G),
    (
	hungarian(G1, MC, M, W)
    ->
	add_to_heap(H0, W, G1-M, H1)
    ;
	H1 = H0
    ),
	partition(Is, G, MC, H1, H).

partition_graph(empty, _, _, empty).
partition_graph(two(K0,V0,T0,T1), I, J, two(K0,V1,T2,T3)) :-
	partition_graph(T0, I, J, T2),
    (
	K0 = I
    ->
	V0 = V1
    ;
	partition_list(V0, J, V1, _)
    ),
	partition_graph(T1, I, J, T3).
partition_graph(three(K0,V0,K1,V1,T0,T1,T2), I, J, 
	        three(K0,V2,K1,V3,T3,T4,T5)) :-
	partition_graph(T0, I, J, T3),
    (
	K0 = I
    ->
	V0 = V2
    ;
	partition_list(V0, J, V2, _)
    ),
	partition_graph(T1, I, J, T4),
    (
	K1 = I
    ->
	V1 = V3
    ;
	partition_list(V1, J, V3, _)
    ),
	partition_graph(T2, I, J, T5).
partition_graph(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), I, J, 
	        four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	partition_graph(T0, I, J, T4),
    (
	K0 = I
    ->
	V0 = V3
    ;
	partition_list(V0, J, V3, _)
    ),
	partition_graph(T1, I, J, T5),
    (
	K1 = I
    ->
	V1 = V4
    ;
	partition_list(V1, J, V4, _)
    ),
	partition_graph(T2, I, J, T6),
    (
	K2 = I
    ->
	V2 = V5
    ;
	partition_list(V2, J, V5, _)
    ),
	partition_graph(T3, I, J, T7).

partition_list(As, J, Bs, Cs) :-
    (
	ord_key_select(J, As, V, Bs)
    ->
	Cs = [J-V]
    ;
	Bs = As,
	Cs = []
    ).

% = hungarian(+Graph, -Matching)
%
% giving a complete, weighted, bipartite Graph, we compute a
% minimum-weight total matching using the Kuhn-Munkres algorithm
% (also known as the Hungarian Method).

hungarian(Graph, Matching) :-
	hungarian(Graph, Matching, _Weight).

% = hungarian(+Graph, -Matching, -Weight)
%
% giving a complete, weighted, bipartite Graph, we compute a
% total matching with minimum weight Weight using the Kuhn-Munkres
% algorithm (also known as the Hungarian Method).

hungarian(Graph0, Matching, Weight) :-
	columns(Graph0, MC),
	subtract_smallest(Graph0, Graph1),
	hungarian2(Graph1, empty, Mask0, [], _, [], _),
	hungarian(Graph1, _Graph, Mask0, Mask, [], _CC, [], _RC, MC),
	compute_matching(Mask, Graph0, 0, Weight, Matching, []).

hungarian(Graph0, MC, Matching, Weight) :-
	subtract_smallest(Graph0, Graph1),
	hungarian2(Graph1, empty, Mask0, [], _, [], _),
	hungarian(Graph1, _Graph, Mask0, Mask, [], _CC, [], _RC, MC),
	compute_matching(Mask, Graph0, 0, Weight, Matching, []).


hungarian(Graph0, Graph, Mask0, Mask, CC0, CC, RC0, RC, MC) :-
	hungarian3(Mask0, CC0, CC1),
    (
	/* have we covered all columns? */
	length(CC1, MC)
    ->
	Mask = Mask0
    ;
	hungarian4(Graph0, Graph, Mask0, Mask, CC1, CC, RC0, RC, MC)
    ).

% = columns(+Graph, ?NumCols)
%
% true if NumCols is the number of columns in Graph; assumes all rows
% have the same number of columns

columns(empty, 0).
columns(two(_,V,_,_), L) :-
	length(V, L).
columns(three(_,V,_,_,_,_,_), L) :-
	length(V, L).
columns(four(_,V,_,_,_,_,_,_,_,_), L) :-
	length(V, L).

% = compute_matching(+Mask, +Graph, +Weight, -Weight, +List, -List)
%
% from the Mask, compute the matching as a list, together with the
% total weight of the matching.

compute_matching(empty, _, W, W) -->
	[].
compute_matching(two(I,V,T0,T1), G, W0, W) -->
	compute_matching(T0, G, W0, W1),
	compute_matching1(G, I, V, W1, W2),
	compute_matching(T1, G, W2, W).
compute_matching(three(I0,V0,I1,V1,T0,T1,T2), G, W0, W) -->
	compute_matching(T0, G, W0, W1),
	compute_matching1(G, I0, V0, W1, W2),
	compute_matching(T1, G, W2, W3),
	compute_matching1(G, I1, V1, W3, W4),
	compute_matching(T2, G, W4, W).
compute_matching(four(I0,V0,I1,V1,I2,V2,T0,T1,T2,T3), G, W0, W) -->
	compute_matching(T0, G, W0, W1),
	compute_matching1(G, I0, V0, W1, W2),
	compute_matching(T1, G, W2, W3),
	compute_matching1(G, I1, V1, W3, W4),
	compute_matching(T2, G, W4, W5),
	compute_matching1(G, I2, V2, W5, W6),
	compute_matching(T3, G, W6, W).

% compute_matching1(+Graph, +I, +V, +Weight, -Weight, +List, -List)
%
% find the item I is matched to together with the weight of this link
% and update the total Weight and the List of linked items
%
% assumes V contains just a single item

compute_matching1(G, I, [J-1], W0, W, [I-J|L], L) :-
	btree_get(G, I, E),
	ord_key_member(J, E, W1),
	W is W0+W1.	

% = hungarian2(+Graph, +Mask, -Mask, +RC, -RC, +CC, +CC)
%
% star every uncovered 0 and cover its row and column, ie. there will be
% at most one starred zero in each row and in each column.
% if there is one in each column, we are done, if not, we need to add
% more zeros

hungarian2(empty, M, M, R, R, C, C).
hungarian2(two(K,V0,T0,T1), M0, M, R0, R, C0, C) :-
	hungarian2(T0, M0, M1, R0, R1, C0, C1),
	hungarian2b(V0, M1, M2, K, R1, R2, C1, C2),
	hungarian2(T1, M2, M, R2, R, C2, C).
hungarian2(three(K0,V0,K1,V1,T0,T1,T2), M0, M, R0, R, C0, C) :-
	hungarian2(T0, M0, M1, R0, R1, C0, C1),
	hungarian2b(V0, M1, M2, K0, R1, R2, C1, C2),
	hungarian2(T1, M2, M3, R2, R3, C2, C3),
	hungarian2b(V1, M3, M4, K1, R3, R4, C3, C4),
	hungarian2(T2, M4, M, R4, R, C4, C).
hungarian2(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), M0, M, R0, R, C0, C) :-
	hungarian2(T0, M0, M1, R0, R1, C0, C1),
	hungarian2b(V0, M1, M2, K0, R1, R2, C1, C2),
	hungarian2(T1, M2, M3, R2, R3, C2, C3),
	hungarian2b(V1, M3, M4, K1, R3, R4, C3, C4),
	hungarian2(T2, M4, M5, R4, R5, C4, C5),
	hungarian2b(V2, M5, M6, K2, R5, R6, C5, C6),
	hungarian2(T3, M6, M, R6, R, C6, C).

hungarian2b([], M, M, _, R, R, C, C).
hungarian2b([J-V0|As], M0, M, I, R0, R, C0, C) :-
    (
	V0 =:= 0,
        \+ ord_member(J, C0),
	\+ ord_member(I, R0)
    ->
	insert_edge(M0, I, J-1, M1),
	ord_insert(C0, J, C1),
	ord_insert(R0, I, R1)
    ;
	M0 = M1,
	C1 = C0,
	R1 = R0
    ),
	hungarian2b(As, M1, M, I, R1, R, C1, C).

% = hungarian3(+Mask, +CC, -CC)
%
% cover the columns of starred zeros.
% didn't we just do this and then throw the results away? RM

hungarian3(empty, C, C).
hungarian3(two(_,V0,T0,T1), C0, C) :-
	hungarian3(T0, C0, C1),
	hungarian3b(V0, C1, C2),
	hungarian3(T1, C2, C).
hungarian3(three(_,V0,_,V1,T0,T1,T2), C0, C) :-
	hungarian3(T0, C0, C1),
	hungarian3b(V0, C1, C2),
	hungarian3(T1, C2, C3),
	hungarian3b(V1, C3, C4),
	hungarian3(T2, C4, C).
hungarian3(four(_,V0,_,V1,_,V2,T0,T1,T2,T3), C0, C) :-
	hungarian3(T0, C0, C1),
	hungarian3b(V0, C1, C2),
	hungarian3(T1, C2, C3),
	hungarian3b(V1, C3, C4),
	hungarian3(T2, C4, C5),
	hungarian3b(V2, C5, C6),
	hungarian3(T3, C6, C).

hungarian3b([], C, C).
hungarian3b([K-V0|As], C0, C) :-
    (
	V0 =:= 1
    ->
	ord_insert(C0, K, C1)
    ;
	C1 = C0
    ),
	hungarian3b(As, C1, C).

% = hungarian4(+Graph, +Mask, -Mask, +RC, -RC, +CC, -CC)
%
% find uncovered zeros in the cost matrix and prime them, then check
% if there is a starred zero in the row of the zero which has just
% been primed.
% if there are no uncovered zeros, add some by reducing the weights
% of the uncovered elements by the smallest amount possible

hungarian4(G0, G, M0, M, R0, R, C0, C, MC) :-
	find_uncovered_zero(G0, R0, C0, I, J),
	!,
	/* prime the uncovered zero */
	add_edge(M0, I, J-2, M1),
	btree_get(M1, I, V),
    (
	/* does this column contain a starred zero? */
	member(S-1, V)
    ->
	ord_select(S, R0, R1),
	ord_insert(C0, I, C1),
	hungarian4(G0, G, M1, M, R1, R, C1, C, MC)
    ;
	hungarian5(G0, G, J, M1, M, R0, C0, [I-J], MC)
    ).

hungarian4(G0, G, M0, M, R0, R, C0, C, MC) :-
	hungarian6(G0, G1, R0, C0),
	hungarian4(G1, G, M0, M, R0, R, C0, C, MC).

find_uncovered_zero(G, R, C, I, J) :-
	btree_member(G, I, V),
	\+ ord_member(I, C),
	member(J-0, V),
	\+ ord_member(J, R).

% = hungarian5
%
% follow stars and primes to find and augmenting path. unstar all
% starred zeros and star each primed zero along this path. finally,
% erase any remaining primes
% is it possible to avoid the second pass (star_primes/unstar_stars)
% and do everything in one go?                                   RM

hungarian5(G0, G, J, M0, M, R, C, L0, MC) :-
    (
	find_star_in_col(M0, J, S)
    ->
	find_prime_in_row(M0, S, P),
	L = [S-P,S-J|L0],
	hungarian5(G0, G, P, M0, M, R, C, L, MC)
    ;
	star_primes(L0, M0, M1),
	erase_primes(M1, M2),
	hungarian(G0, G, M2, M, [], _CC, [], _RC, MC)
    ).

find_star_in_col(M, I, S) :-
	btree_member(M, S, V),
	member(I-1, V).

find_prime_in_row(M, J, P) :-
	btree_get(M, J, V),
	member(P-2, V).

star_primes([I-J|Ps], M0, M) :-
	btree_get_replace(M0, I, V0, V, M1),
	ord_select(J-2, V0, V1),
	ord_insert(V1, J-1, V),
	unstar_stars(Ps, M1, M).

unstar_stars([], M, M).
unstar_stars([I-J|Ps], M0, M) :-
	btree_get_replace(M0, I, V0, V, M1),
	ord_select(J-1, V0, V),
	star_primes(Ps, M1, M).

erase_primes(empty, empty).
erase_primes(two(K,V0,T0,T1), two(K,V,T2,T3)) :-
	erase_primes(T0, T2),
	erase_primes1(V0, V),
	erase_primes(T1, T3).
erase_primes(three(K0,V0,K1,V1,T0,T1,T2), three(K0,V2,K1,V3,T3,T4,T5)) :-
	erase_primes(T0, T3),
	erase_primes1(V0, V2),
	erase_primes(T1, T4),
	erase_primes1(V1, V3),
	erase_primes(T2, T5).
erase_primes(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), 
             four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	erase_primes(T0, T4),
	erase_primes1(V0, V3),
	erase_primes(T1, T5),
	erase_primes1(V1, V4),
	erase_primes(T2, T6),
	erase_primes1(V2, V5),
	erase_primes(T3, T7).

erase_primes1([], []).
erase_primes1([K-V|As], Bs0) :-
    (
	V = 2
    ->
	Bs = Bs0
    ;
	Bs0 = [K-V|Bs]
    ),
	erase_primes1(As, Bs).

% = hungarian6
%
% find the smallest uncovered value in the cost matrix, then subtract
% it from all elements in an uncovered column and add them to all
% elements in a covered row.

hungarian6(G0, G, R, C) :-
	min_uncovered_weight(G0, R, C, W),
	add_covered_row(C, G0, G1, W),
	subtract_uncovered_column(G1, R, W, G).

add_covered_row([], G, G, _).
add_covered_row([R|Rs], G0, G, W) :-
	add_covered_row1(G0, R, W, G1),
	add_covered_row(Rs, G1, G, W).

add_covered_row1(G0, R, W, G) :-
	btree_get_replace(G0, R, V0, V, G),
	add_covered_row2(V0, W, V).

add_covered_row2([], _, []).
add_covered_row2([K-V0|Vs0], W, [K-V|Vs]) :-
	V is V0 + W,
	add_covered_row2(Vs0, W, Vs).

subtract_uncovered_column(empty, _, _, empty).
subtract_uncovered_column(two(K,V0,T0,T1), R, W, two(K,V,T2,T3)) :-
	subtract_uncovered_column(T0, R, W, T2),
	subtract_uncovered_column1(V0, R, W, V),
	subtract_uncovered_column(T1, R, W, T3).

subtract_uncovered_column(three(K0,V0,K1,V1,T0,T1,T2), R, W, 
                          three(K0,V2,K1,V3,T3,T4,T5)) :-
	subtract_uncovered_column(T0, R, W, T3),
	subtract_uncovered_column1(V0, R, W, V2),
	subtract_uncovered_column(T1, R, W, T4),
	subtract_uncovered_column1(V1, R, W, V3),
	subtract_uncovered_column(T2, R, W, T5).
subtract_uncovered_column(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), R, W, 
                          four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	subtract_uncovered_column(T0, R, W, T4),
	subtract_uncovered_column1(V0, R, W, V3),
	subtract_uncovered_column(T1, R, W, T5),
	subtract_uncovered_column1(V1, R, W, V4),
	subtract_uncovered_column(T2, R, W, T6),
	subtract_uncovered_column1(V2, R, W, V5),
	subtract_uncovered_column(T3, R, W, T7).

subtract_uncovered_column1([], _, _, []).
subtract_uncovered_column1([K-V0|As], R, W, [K-V|Bs]) :-
    (
	ord_member(K, R)
    ->
	V = V0
    ;
	V is V0 - W
    ),
	subtract_uncovered_column1(As, R, W, Bs).

value_subtract(empty, _, empty).
value_subtract(two(K,V0,T0,T1), W, two(K,V,T2,T3)) :-
	value_subtract(T0, W, T2),
	value_subtract1(V0, W, V),
	value_subtract(T1, W, T3).

value_subtract(three(K0,V0,K1,V1,T0,T1,T2), W, 
                          three(K0,V2,K1,V3,T3,T4,T5)) :-
	value_subtract(T0, W, T3),
	value_subtract1(V0, W, V2),
	value_subtract(T1, W, T4),
	value_subtract1(V1, W, V3),
	value_subtract(T2, W, T5).
value_subtract(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), W, 
                          four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	value_subtract(T0, W, T4),
	value_subtract1(V0, W, V3),
	value_subtract(T1, W, T5),
	value_subtract1(V1, W, V4),
	value_subtract(T2, W, T6),
	value_subtract1(V2, W, V5),
	value_subtract(T3, W, T7).

value_subtract1([], _, []).
value_subtract1([K-V0|As], W, [K-V|Bs]) :-
	V is W - V0,
	value_subtract1(As, W, Bs).


% = min_uncovered_weight(+Graph, +RC, +CC, -Weight)
%
% true if Weight is the minimum weight in Graph not in the covered
% rows RC or the covered columns CC
% fails if all weights are covered.

min_uncovered_weight(G, R, C, W) :-
	min_uncovered_weight(G, R, C, 99999, W),
	/* did we find an uncovered weight */
	W =\= 99999.

min_uncovered_weight(empty, _, _, W, W).
min_uncovered_weight(two(K,V,T0,T1), R, C, W0, W) :-
	min_uncovered_weight(T0, R, C, W0, W1),
    (
	ord_member(K, C)
    ->
	W2 = W1
    ;
	min_uncovered_weight1(V, R, W1, W2)
    ),
	min_uncovered_weight(T1, R, C, W2, W).
min_uncovered_weight(three(K0,V0,K1,V1,T0,T1,T2), R, C, W0, W) :-
	min_uncovered_weight(T0, R, C, W0, W1),
    (
	ord_member(K0, C)
    ->
	W2 = W1
    ;
	min_uncovered_weight1(V0, R, W1, W2)
    ),
	min_uncovered_weight(T1, R, C, W2, W3),
    (
	ord_member(K1, C)
    ->
	W4 = W3
    ;
	min_uncovered_weight1(V1, R, W3, W4)
    ),
	min_uncovered_weight(T2, R, C, W4, W).
min_uncovered_weight(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), R, C, W0, W) :-
	min_uncovered_weight(T0, R, C, W0, W1),
    (
	ord_member(K0, C)
    ->
	W2 = W1
    ;
	min_uncovered_weight1(V0, R, W1, W2)
    ),
	min_uncovered_weight(T1, R, C, W2, W3),
    (
	ord_member(K1, C)
    ->
	W4 = W3
    ;
	min_uncovered_weight1(V1, R, W3, W4)
    ),
	min_uncovered_weight(T2, R, C, W4, W5),
    (
	ord_member(K2, C)
    ->
	W6 = W5
    ;
	min_uncovered_weight1(V2, R, W5, W6)
    ),
	min_uncovered_weight(T3, R, C, W6, W).

min_uncovered_weight1([], _, W, W).
min_uncovered_weight1([K-V|As], R, W0, W) :-
    (
	ord_member(K, R)
    ->
	W1 = W0
    ;
	W1 is min(V, W0)
    ),
	min_uncovered_weight1(As, R, W1, W).

% = subtract_smallest(+Graph, -Graph)
%
% subtract the smallest element of each row from all elements of this row.

subtract_smallest(empty, empty).
subtract_smallest(two(K,V0,T0,T1), two(K,V,T2,T3)) :-
	subtract_smallest(T0, T2),
	subtract_smallest1(V0, V),
	subtract_smallest(T1, T3).
subtract_smallest(three(K0,V0,K1,V1,T0,T1,T2), three(K0,V2,K1,V3,T3,T4,T5)) :-
	subtract_smallest(T0, T3),
	subtract_smallest1(V0, V2),
	subtract_smallest(T1, T4),
	subtract_smallest1(V1, V3),
	subtract_smallest(T2, T5).
subtract_smallest(four(K0,V0,K1,V1,K2,V2,T0,T1,T2,T3), 
	          four(K0,V3,K1,V4,K2,V5,T4,T5,T6,T7)) :-
	subtract_smallest(T0, T4),
	subtract_smallest1(V0, V3),
	subtract_smallest(T1, T5),
	subtract_smallest1(V1, V4),
	subtract_smallest(T2, T6),
	subtract_smallest1(V2, V5),
	subtract_smallest(T3, T7).

subtract_smallest1(L0, L) :-
	min_weight(L0, W),
	subtract_smallest1(L0, W, L).

subtract_smallest1([], _, []).
subtract_smallest1([K-V0|As], W, [K-V|Bs]) :-
	V is V0 - W,
	subtract_smallest1(As, W, Bs).

% = max_weight(+Graph, ?Weight)
%
% compute the maximum Weight in a weighted Graph.

max_weight(G, W) :-
	max_weight(G, -99999, W).

max_weight(empty, W, W).
max_weight(two(_,V,T0,T1), W0, W) :-
	max_weight(T0, W0, W1),
	max_weight1(V, W1, W2),
	max_weight(T1, W2, W).
max_weight(three(_,V0,_,V1,T0,T1,T2), W0, W) :-
	max_weight(T0, W0, W1),
	max_weight1(V0, W1, W2),
	max_weight(T1, W2, W3),
	max_weight1(V1, W3, W4),
	max_weight(T2, W4, W).
max_weight(four(_,V0,_,V1,_,V2,T0,T1,T2,T3), W0, W) :-
	max_weight(T0, W0, W1),
	max_weight1(V0, W1, W2),
	max_weight(T1, W2, W3),
	max_weight1(V1, W3, W4),
	max_weight(T2, W4, W5),
	max_weight1(V2, W5, W6),
	max_weight(T3, W6, W).

max_weight1([], W, W).
max_weight1([_-W0|Xs], W1, W) :-
	W2 is max(W0, W1),
	max_weight1(Xs, W2, W).

min_weight(L, W) :-
	min_weight1(L, 99999, W).

min_weight1([], W, W).
min_weight1([_-W0|Xs], W1, W) :-
	W2 is min(W0, W1),
	min_weight1(Xs, W2, W).

% = hungarian_max(Graph, Matching)
%
% finds a maximum weight perfect matching for a total bipartite
% graph by simply subtracting all weights in the graph from the
% maximum weight and then finding the miniumum weight matching.

hungarian_max(Graph0, Matching) :-
	max_weight(Graph0, Max),
	value_subtract(Graph0, Max, Graph),
	hungarian(Graph, Matching, _).

hungarian_max(Graph0, Matching, Weight) :-
	max_weight(Graph0, Max),
	value_subtract(Graph0, Max, Graph1),
	columns(Graph1, MC),
	subtract_smallest(Graph1, Graph2),
	hungarian2(Graph2, empty, Mask0, [], _, [], _),
	hungarian(Graph2, _Graph, Mask0, Mask, [], _CC, [], _RC, MC),
	compute_matching(Mask, Graph0, 0, Weight, Matching, []).

% = random_matching(+Rows/Columns, +MaxValue, -Graph)
%
% create a random complete bipartite graph Graph with weights less
% than MaxValue and Rows/Columns rows and columns

random_matching(C, M, G) :-
	C2 is C*2,
	random_matching(0, C, C2, M, empty, G).

random_matching(C0, C, C2, M, G0, G) :-
    (
	C0 < C
    ->
	btree_insert(G0, C0, L, G1),
	random_matching(C, C2, M, L, []),
	C1 is C0+1,
	random_matching(C1, C, C2, M, G1, G)
    ;
	G = G0
    ).

random_matching(R0, R, M, L0, L) :-
    (
	R0 < R
    ->
	W is random(M),
	L0 = [R0-W|L1],
	R1 is R0+1,
	random_matching(R1, R, M, L1, L)
    ;
	L0 = L
    ).

% = some simple test graphs

%% test_ha(M, W) :-
%% 	list_to_btree([1-[3-1,4-2],2-[3-3,4-1]], T),
%% 	kbest(4, T, M, W).

%% test_h0(M, W) :-
%% 	L = [1-[4-14,5-81,6-78],
%% 	     2-[4-54,5-95,6-28],
%% 	     3-[4-67,5-33,6-51]],
%% 	list_to_btree(L, T),
%% 	kbest(256, T, M, W).

%% test_h1(M, W) :-
%% 	L = [1-[6-23,7-27,8-42,9-55,10-97],
%% 	     2-[6-14,7-94,8-5,9-77,10-22],
%% 	     3-[6-53,7-3,8-88,9-89,10-7],
%% 	     4-[6-99,7-52,8-14,9-1,10-63],
%% 	     5-[6-50,7-100,8-34,9-41,10-92]],
%% 	list_to_btree(L, T),
%% 	kbest(256, T, M, W).

%% test_h2(M, W) :-
%% 	L = [1-[4-1,5-1,6-1],
%% 	     2-[4-1,5-1,6-1],
%% 	     3-[4-1,5-1,6-1]],
%% 	list_to_btree(L, T),
%% 	kbest(6, T, M, W).

%% test_h(M, W) :-
%% 	L = [1-[4-1,5-2,6-3],
%% 	     2-[4-2,5-4,6-6],
%% 	     3-[4-3,5-6,6-9]],
%% 	list_to_btree(L, T),
%% 	kbest(6, T, M, W).

insert_edge(Graph0, From, To, Graph) :-
	btree_insert(Graph0, From, [To], Graph).

add_edge(Graph0, From, To, Graph) :-
    (
	btree_get_replace(Graph0, From, Tos0, Tos, Graph)
    ->
	ord_insert(Tos0, To, Tos)
    ;
	btree_insert(Graph0, From, [To], Graph)
    ).

% = maximum_matching(+Graph, -Matching)
%

%maximum_matching(two(V,E,T0,T1), Matching) :-
%	maximum_matching(Graph, [], _, [V], _, Matching).

%maximum_matching(

% = rt_closure(+Graph, -Closure)
%
% true if Closure is the transitive, reflexive closure of Graph.

rt_closure(Graph, Closure) :-
	warshall(Graph, Closure0),
	reflexive(Closure0, Closure).

% = reflexive(+Graph, -Closure)
%
% true if Closure is the reflexive closure of Graph.

reflexive(empty, empty).
reflexive(two(V, E0, T0, T1), two(V, E, T2, T3)) :-
	ord_insert(E0, V, E),
	reflexive(T0, T2),
	reflexive(T1, T3).
reflexive(three(V0, E0, V1, E1, T0, T1, T2),
          three(V0, E2, V1, E3, T3, T4, T5)) :-
	ord_insert(E0, V0, E2),
	ord_insert(E1, V1, E3),
	reflexive(T0, T3),
	reflexive(T1, T4),
	reflexive(T2, T5).
reflexive(four(V0, E0, V1, E1, V2, E2, T0, T1, T2, T3),
          four(V0, E3, V1, E4, V2, E5, T4, T5, T6, T7)) :-
	ord_insert(E0, V0, E3),
	ord_insert(E1, V1, E4),
	ord_insert(E2, V2, E5),
	reflexive(T0, T4),
	reflexive(T1, T5),
	reflexive(T2, T6),
	reflexive(T3, T7).

% = warshall(+Graph, -Closure)
%
% computes the transitive closure of Graph, represented as a 234tree
% which vertices as the keys and ordered lists of destination vertices
% as the edges.
%
% Basically just a translation of Richard A. O'Keefe's warshall/2
% predicate from the DEC 10 Prolog library.

warshall(Graph, Closure) :-
	warshall(Graph, Graph, Closure).

warshall(empty, Closure, Closure).
warshall(two(V, _, T0, T1), E0, Closure) :-
	warshall(T0, E0, E1),
	btree_get(E1, V, Y),	%  Y := E(V)
	warshall(E1, V, Y, E2),
	warshall(T1, E2, Closure).
warshall(three(V0, _, V1, _, T0, T1, T2), E0, Closure) :-
	warshall(T0, E0, E1),
	btree_get(E1, V0, Y0),	%  Y0 := E(V0)
	warshall(E1, V0, Y0, E2),
	warshall(T1, E2, E3),
	btree_get(E3, V1, Y1),
	warshall(E3, V1, Y1, E4),
	warshall(T2, E4, Closure).
warshall(four(V0, _, V1, _, V2, _, T0, T1, T2, T3), E0, Closure) :-
	warshall(T0, E0, E1),
	btree_get(E1, V0, Y0),	%  Y := E(v)
	warshall(E1, V0, Y0, E2),
	warshall(T1, E2, E3),
	btree_get(E3, V1, Y1),
	warshall(E3, V1, Y1, E4),
	warshall(T2, E4, E5),
	btree_get(E5, V2, Y2),
	warshall(E5, V2, Y2, E6),
	warshall(T3, E6, Closure).

warshall(empty, _, _, empty).
warshall(two(X,Neibs,T0,T1), V, Y, two(X,NewNeibs,T2,T3)) :-
    (
	ord_member(V, Neibs)
    ->
	ord_union(Neibs, Y, NewNeibs)
    ;
	NewNeibs = Neibs
    ),
	warshall(T0, V, Y, T2),
	warshall(T1, V, Y, T3).
warshall(three(X,NeibsX,Y,NeibsY,T0,T1,T2), V, Z,
	 three(X,NewNeibsX,Y,NewNeibsY,T3,T4,T5)) :-
    (
	ord_member(V, NeibsX)
    ->
	ord_union(NeibsX, Z, NewNeibsX)
    ;
	NewNeibsX = NeibsX
    ),
    (
	ord_member(V, NeibsY)
    ->
	ord_union(NeibsY, Z, NewNeibsY)
    ;
	NewNeibsY = NeibsY
    ),
	warshall(T0, V, Z, T3),
	warshall(T1, V, Z, T4),
	warshall(T2, V, Z, T5).
warshall(four(X,NeibsX,Y,NeibsY,Z,NeibsZ,T0,T1,T2,T3), V, W,
	 four(X,NewNeibsX,Y,NewNeibsY,Z,NewNeibsZ,T4,T5,T6,T7)) :-
    (
	ord_member(V, NeibsX)
    ->
	ord_union(NeibsX, W, NewNeibsX)
    ;
	NewNeibsX = NeibsX
    ),
    (
	ord_member(V, NeibsY)
    ->
	ord_union(NeibsY, W, NewNeibsY)
    ;
	NewNeibsY = NeibsY
    ),
    (
	ord_member(V, NeibsZ)
    ->
	ord_union(NeibsZ, W, NewNeibsZ)
    ;
	NewNeibsZ = NeibsZ
    ),

	warshall(T0, V, W, T4),
	warshall(T1, V, W, T5),
	warshall(T2, V, W, T6),
	warshall(T3, V, W, T7).

% = floyd_warshall(+Graph, -Closure)
%
% computes the transitive closure of Graph, represented as a 234tree
% which vertices as the keys and ordered lists of destination vertices
% as the edges.
%
% Basically just an adaptation of Richard A. O'Keefe's warshall/2
% predicate from the DEC 10 Prolog library.

floyd_warshall(Graph, Closure) :-
	floyd_warshall(Graph, Graph, Closure).

floyd_warshall(empty, Closure, Closure).
floyd_warshall(two(V, _, T0, T1), E0, Closure) :-
	floyd_warshall(T0, E0, E1),
	btree_get(E1, V, Y),	%  Y := E(V)
	floyd_warshall(E1, V, Y, E2),
	floyd_warshall(T1, E2, Closure).
floyd_warshall(three(V0, _, V1, _, T0, T1, T2), E0, Closure) :-
	floyd_warshall(T0, E0, E1),
	btree_get(E1, V0, Y0),	%  Y0 := E(V0)
	floyd_warshall(E1, V0, Y0, E2),
	floyd_warshall(T1, E2, E3),
	btree_get(E3, V1, Y1),
	floyd_warshall(E3, V1, Y1, E4),
	floyd_warshall(T2, E4, Closure).
floyd_warshall(four(V0, _, V1, _, V2, _, T0, T1, T2, T3), E0, Closure) :-
	floyd_warshall(T0, E0, E1),
	btree_get(E1, V0, Y0),	%  Y := E(v)
	floyd_warshall(E1, V0, Y0, E2),
	floyd_warshall(T1, E2, E3),
	btree_get(E3, V1, Y1),
	floyd_warshall(E3, V1, Y1, E4),
	floyd_warshall(T2, E4, E5),
	btree_get(E5, V2, Y2),
	floyd_warshall(E5, V2, Y2, E6),
	floyd_warshall(T3, E6, Closure).

floyd_warshall(empty, _, _, empty).
floyd_warshall(two(X,Neibs,T0,T1), V, Y, two(X,NewNeibs,T2,T3)) :-
    (
	ord_key_member(V, Neibs, K)
    ->
	ord_pair_union(Neibs, Y, K, NewNeibs)
    ;
	NewNeibs = Neibs
    ),
	floyd_warshall(T0, V, Y, T2),
	floyd_warshall(T1, V, Y, T3).
floyd_warshall(three(X,NeibsX,Y,NeibsY,T0,T1,T2), V, Z,
	       three(X,NewNeibsX,Y,NewNeibsY,T3,T4,T5)) :-
    (
	ord_key_member(V, NeibsX, K1)
    ->
	ord_pair_union(NeibsX, Z, K1, NewNeibsX)
    ;
	NewNeibsX = NeibsX
    ),
    (
	ord_key_member(V, NeibsY, K2)
    ->
	ord_pair_union(NeibsY, Z, K2, NewNeibsY)
    ;
	NewNeibsY = NeibsY
    ),
	floyd_warshall(T0, V, Z, T3),
	floyd_warshall(T1, V, Z, T4),
	floyd_warshall(T2, V, Z, T5).
floyd_warshall(four(X,NeibsX,Y,NeibsY,Z,NeibsZ,T0,T1,T2,T3), V, W,
	       four(X,NewNeibsX,Y,NewNeibsY,Z,NewNeibsZ,T4,T5,T6,T7)) :-
    (
	ord_key_member(V, NeibsX, K1)
    ->
	ord_pair_union(NeibsX, W, K1, NewNeibsX)
    ;
	NewNeibsX = NeibsX
    ),
    (
	ord_key_member(V, NeibsY, K2)
    ->
	ord_pair_union(NeibsY, W, K2, NewNeibsY)
    ;
	NewNeibsY = NeibsY
    ),
    (
	ord_key_member(V, NeibsZ, K3)
    ->
	ord_pair_union(NeibsZ, W, K3, NewNeibsZ)
    ;
	NewNeibsZ = NeibsZ
    ),

	floyd_warshall(T0, V, W, T4),
	floyd_warshall(T1, V, W, T5),
	floyd_warshall(T2, V, W, T6),
	floyd_warshall(T3, V, W, T7).

% ord_pair_union(+OrdKVSetA, +OrdKVSetB, +ValueC, ?OrdKVSet) 
%
% true if OrdKVSet is an ordered set of Key-Value pairs which is
% the union of OrdKVSetA and OrdKVSetB. In the case of equal keys
% with ValueA in OrdKVSetA and ValueB in OrdKVSetB, the corresponding
% Value in OrdKVSet will be the intersection of ValueA with the 
% intersection of ValueB and ValueC.
% Based on the ord_union/3 predicate from the DEC-10 Prolog library.

ord_pair_union(Set1, Set2, K, Union) :-
	map_ord_union(Set2, K, Set3),
	ord_pair_union0(Set1, Set3, Union).

map_ord_union([], _, []).
map_ord_union([Head1-D1|Tail1], D2, [Head1-D|Tail2]) :-
	ord_union(D1, D2, D),
	map_ord_union(Tail1, D2, Tail2).

ord_pair_union0([], Set2, Set2).
ord_pair_union0([Head1-D1|Tail1], Set2, Union) :-
	ord_pair_union1(Set2, Head1, D1, Tail1, Union).

ord_pair_union1([], Head1, D1, Tail1, [Head1-D1|Tail1]).
ord_pair_union1([Head2-D2|Tail2], Head1, D1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_pair_union3(Order, Head1, D1, Tail1, Head2, D2, Tail2, Union).

ord_pair_union2([], Head2, D2, Tail2, [Head2-D2|Tail2]).
ord_pair_union2([Head1-D1|Tail1], Head2, D2, Tail2, Union) :-
	compare(Order, Head1, Head2),
	ord_pair_union3(Order, Head1, D1, Tail1, Head2, D2, Tail2, Union).

ord_pair_union3(=, Head,  D1, Tail1, _,  D2,   Tail2, [Head-D|Union]) :-
	ord_intersect(D1, D2, D),
	ord_pair_union0(Tail1, Tail2, Union).
ord_pair_union3(<, Head1, D1, Tail1, Head2, D2, Tail2, [Head1-D1|Union]) :-
	ord_pair_union2(Tail1, Head2, D2, Tail2, Union).
ord_pair_union3(>, Head1, D1, Tail1, Head2, D2, Tail2, [Head2-D2|Union]) :-
	ord_pair_union1(Tail2, Head1, D1, Tail1, Union).

% = graph2dot(+Graph, +FileName)
%
% creates a dot file FileName portraying Graph

graph2dot(G, File) :-
	telling(Out),
	tell(File),
	graph2dot(G),
	told,
	tell(Out).

% = graph2dot(+Graph)
%
% send a dot representation of Graph to the current output stream

graph2dot(G) :-
	format('digraph "graph" {~2n', []),
	graph2dot1(G),
	format('}~n', []).

graph2dot1(empty).
graph2dot1(two(K, V, T0, T1)) :-
	graph2dot1(T0),
	format('~w;~n', [K]),
	print_vertices(V, K),
	graph2dot1(T1).
graph2dot1(three(K0, V0, K1, V1, T0, T1, T2)) :-
	graph2dot1(T0),
	format('~w;~n', [K0]),
	print_vertices(V0, K0),
	graph2dot1(T1),
	format('~w;~n', [K1]),
	print_vertices(V1, K1),
	graph2dot1(T2).
graph2dot1(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3)) :-
	graph2dot1(T0),
	format('~w;~n', [K0]),
	print_vertices(V0, K0),
	graph2dot1(T1),
	format('~w;~n', [K1]),
	print_vertices(V1, K1),
	graph2dot1(T2),
	format('~w;~n', [K2]),
	print_vertices(V2, K2),
	graph2dot1(T3).

print_vertices([], _) :-
	nl.
print_vertices([V|Vs], R) :-
    (
	V = K-W
    ->
	format('~w -> ~w [label="', [R, K]),
	print_weights(W),
	format('"];~n', [])
    ;
	format('~w -> ~w; ~n', [R, V])
    ),
	print_vertices(Vs, R).

print_weights([]).
print_weights([W|Ws]) :-
	format('~w\\n', [W]),
	print_weights(Ws).
