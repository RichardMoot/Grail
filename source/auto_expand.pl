% -*- Mode: Prolog -*-

:- module(auto_expand,  [auto_expand/6]).

:- use_module(list_utils, [last/2]).
:- use_module(tree234, [btree_get_replace/5]).
:- use_module(ordset, [ord_subtract/3]).

% This is an experimental module, which replaces the generate-and-test way of applying
% the structural rules by a more goal-directed version, which uses the goal word order
% we are trying to derive in order to guide to derivation.

% The current module is created for

auto_expand(monde, Root, InComponent, OutComponent, Leaves, Yields) :-
	auto_expand_monde(Root, InComponent, OutComponent, Leaves, Yields).

auto_expand_monde(Root, Cmp, Cmp, _Leaves0, Yields) :-
	setof(Yield, simple_yield(Root, Cmp, Yield, []), Yields)

% auto_expand_monde(Root, InComponent, OutComponent, Leaves0, Yields) :-
% 	strip_values(Leaves0, Leaves),
%     (
% 	component_to_tree(Root, InComponent, InTree)
%     ->
%         get_node_extremities(InTree, Leaves, empty, Extr),
%         wrap_to_yield(InTree, OutTree, Extr),
%         tree_to_component(OutTree, OutComponent),
%         setof(Yield, simple_yield(Root, OutComponent, Yield, []), Yields)     
%     ;
%         setof(Yield, simple_yield(Root, InComponent, Yield, []), Yields)
%     ).

% get_node_extremities(Tree, Ls, T0, T) :-
% 	get_node_extremities(Tree, _, _, Ls, T0, T).

% get_node_extremities(N0-Tree, L, R, Ls, T0, T) :-
% 	get_node_extremities1(Tree, L, R, Ls, T0, T1),
% 	btree_insert(T1, N0, L-R, T).

% get_node_extremities1(leaf(A), A, B, Ls, T, T) :-
% 	find_next(A, Ls, B).
% get_node_extremities1(p(_,A,B), L, R, Ls, T0, T) :-
% 	get_node_extremities(A, L, _R0, Ls, T0, T1),
% 	get_node_extremities(B, _L0, R, Ls, T1, T).
% get_node_extremities1(dia(_,A), L, R, Ls, T0, T) :-
% 	get_node_extremities(A, L, R, Ls, T0, T).


% find_next(A, Leaves, B) :-
%      (
%           append(_, [A,B|_], Leaves)
%      ->
%           true
%      ;
%           B = end
%      ).

% %wrap_to_yield(Tree, Tree, _).

% wrap_to_yield(N-Tree0, M-Tree, Ext) :-
% 	wrap_to_yield(Tree0, N, Tree, M, Ext).

% wrap_to_yield(p(1,I0-TI0,J0-TJ0), N, Tree, M, Ext) :-
% 	!,
% 	wrap_to_yield(TI0, I0, TI, I, Ext),
% 	wrap_to_yield(TJ0, J0, TJ, J, Ext),
% 	btree_get(Ext, I, _LI-RI),
% 	btree_get(Ext, J, LJ-_RJ),
%     (
%         RI @>= LJ
%     ->
%         M = N,
%         Tree = p(0,I-TI,J-TJ)
%     ;
%         wrap_recursive(TI, I, TJ, J, N, Tree, M, Ext)
%     ).
% wrap_to_yield(p(0,I0-TI0,J0-TJ0), N, p(0,I-TI,J-TJ), N, Ext) :-
% 	wrap_to_yield(TI0, I0, TI, I, Ext),
% 	wrap_to_yield(TJ0, J0, TJ, J, Ext).
% wrap_to_yield(dia(K,I0-TI0), N, dia(K,I-TI), N, Ext) :-
% 	wrap_to_yield(TI0, I0, TI, I, Ext).
% wrap_to_yield(leaf(A), N, leaf(A), N, _).

% % wrap_recursive(+Tree1, +Root1, +WrapTree, +WrapRoot, +ParentRoot, -ResultTree, -ResultRoot, +Ext)

% wrap_recursive(p(0,K0-TK0,L-TL), I, TJ, J, N, p(0,M-Tree,L-TL), N, Ext) :-
% 	!,
% 	/* TODO: fold in wrap_to_yield */
% 	wrap_to_yield(p(1,K0-TK0,J-TJ), I, Tree, M, Ext).
% wrap_recursive(dia(X,K-TK), I, TJ, J, N, dia(X,M-Tree), N, Ext) :-
% 	!,
% 	wrap_recursive(TK, K, TJ, J, I, Tree, M, Ext).
% wrap_recursive(A, I, TJ, J, N, p(0,I-A,J-TJ), N, _Ext).


%wrap(N-p(1,I-A0,J-B), N-A) :-
%	wrap1(I, A0, J, B).

% insert_wrap_nodes([], Component, Component, _, _, Root, _, Yields) :-
%         setof(Yield, simple_yield(Root, Component, Yield, []), Yields).
% insert_wrap_nodes([W-Ys|Rest], C0, C, YV, VY, Root, Leaves, Yields) :-
% 	get_lefts_rights(Ys, Leaves, Lefts, Rights),
% 	insert_lefts(Lefts, W, Root, VY, C0, C1),
% 	insert_rights(Rights, W, Root, VY, C1, C2),
% 	insert_wrap_nodes(Rest, C2, C, YV, VY, Root, Leaves, Yields).


% insert_nodes([], [], [], [], _, _, _, C, C).
% insert_nodes(Ls0, Ls, Rs0, Rs, N, VY, Leaves, C0, C) :-
% 	btree_get_replace(C0, N, Ts0, Ts, C),
% 	list_daughters(Ts0, [], Ds0),
% 	ord_subtract(Ds0, Leaves, Ds),
% 	insert_nodes(Ds, Ls0, Ls, Rs0, Rs, VY, Ts0, Ts).

% insert_nodes([], Ls, Ls, Rs, Rs, _, Ts, Ts).
% insert_nodes([D|Ds], Ls0, Ls, Rs0, Rs, VY, Ts0, Ts) :-
% 	btree_get(VY, D, Ys),
% 	Ys = [Left|_],
% 	last(Ys, Right),
%     (
%         select(Ls0, Left, Ls1)
%     ->
%         findall(X, I^member(p(I,X,D), Ts0), Xs),
%         add_tensors_l(Xs, D, Ts0, Ts1)
%     ;
%         Ts1 = Ts0
%     ),
%     (
%         select(Rs0, Right, Rs1)
%     ->
%         findall(X, I^member(p(I,D,X), Ts0), Xs),
%         add_tensors_r(Xs, D, Ts1, Ts2)
%     ;
%         Ts2 = Ts1
%     ),
%         insert_nodes(Ds, Ls1, Ls, Rs1, Rs, VYs, Ts2, Ts).

% add_tensors([], _, Ts, Ts).
% add_tensors([X|Xs], D, [p(0,X,D)|Ts0], Ts) :-
% 	add_tensors(Xs, D, Ts0, Ts).
     
% insert_right([], _, _, C, C).

% insert_left_node(N, W, L, VY, C0, C) :-
	
% 	insert_left_node_list(Ts0, Ts, [], W, L, VY).

% insert_left_node_list([], Ts, Ts, _, _, _).
% insert_left_node_list([T|Ts0], Us0, Us, W, L, VY) :-
% 	insert_left_node_tensor(T, Us0, Us1, W, L, VY),
% 	insert_left_node_list(Ts0, Us1, Us, W, L, VY).

% insert_left_node_tensor(dia(I,A), [dia(I,A)|Us], Us, _, _, _).
% insert_left_node_tensor(p(I,A,B), [p(I,A,B)|Us0], Us, W, L, VY) :-
	

% get_lefts_rights([], _, [], []).
% get_lefts_rights([Y|Ys], Leaves, Ls0, Rs0) :-
% 	last(Y, Last),
% 	Y = [First|_],
%    (
%         append(_, [Left,First|_], Leaves)
%    ->
%         Ls0 = [Left|Ls]
%    ;
%         Ls0 = Ls
%    ),
%    (
%         append(_, [Last,Right|_], Leaves)
%    ->
%         Rs0 = [Right|Rs]
%    ;
%         Rs0 = Rs
%    ),
%         get_lefts_rights(Ys, Leaves, Ls, Rs).

% delete_wrap_nodes(Component, Root, NewComponent, LNs, Wraps0, Wraps) :-
% 	btree_get_replace(Component, Root, Tensors0, Tensors, Component1),
% 	delete_wrap_nodes_t(Tensors0, [], Tensors, Component, Wraps0, Wraps1),
% 	list_daughters(Tensors, [], Daughters0),
% 	ord_subtract(Daughters0, LNs, Daughters),
% 	delete_wrap_nodes_list(Daughters, Component1, NewComponent, LNs, Wraps1, Wraps).

% delete_wrap_nodes_list([], C, C, _, W, W).
% delete_wrap_nodes_list([D|Ds], C0, C, LNs, W0, W) :-
% 	delete_wrap_nodes(C0, D, C1, LNs, W0, W1),
% 	delete_wrap_nodes_list(Ds, C1, C, LNs, W1, W).

% delete_wrap_nodes_t([], Ts, Ts, _, Ws, Ws).
% delete_wrap_nodes_t([T|Ts0], Us0, Us, Component, Ws0, Ws) :-
%    (
%         T = p(1,X,Y)
%    ->
%         setof(Yield, simple_yield(Y, Component, Yield, []), Yields),
%         btree_get(Component, X, Ts1),
%         delete_wrap_nodes_t(Ts1, Us0, Us1, Component, [Y-Yields|Ws0], Ws1)
%    ;
%         Ws1 = Ws0,
%         Us1 = [T|Us0]
%     ),
%         delete_wrap_nodes_t(Ts0, Us1, Us, Component, Ws1, Ws).


% = component_to_tree(+RootNode, +Component, -Tree)
%
% converts Component into a Tree, starting with RootNode.
% fails if the forest rooted at RootNode is not a tree.
% adds node numbering information to the tree in order
% to make the reverse operation tree_to_component/2 possible

component_to_tree(Root, Component, Root-Tree) :-
	btree_get(Component, Root, [D]),
	!,
	component_to_tree1(D, Component, Tree).
component_to_tree(A, _, A-leaf(A)).

component_to_tree1(dia(I,A), Component, dia(I,Tree)) :-
	component_to_tree(A, Component, Tree).
component_to_tree1(p(I,A,B), Component, p(I,T0,T1)) :-
	component_to_tree(A, Component, T0),
	component_to_tree(B, Component, T1).


% = number_tree(+Tree, -NumberedTree)
%
% add number information to a tree in order to make
% the tree_to_component operation possible.

number_tree(Tree, 0-NTree) :-
	number_tree(Tree, NTree, 1, _N).

number_tree(dia(I,A0), dia(I,N0-A), N0, N) :-
	!,
	N1 is N0 + 1,
	number_tree(A0, A, N1, N).
number_tree(p(I,A0,B0), p(I,N0-A,N1-B), N0, N) :-
	!,
	N1 is N0 + 1,
	N2 is N1 + 1,
	number_tree(A0, A, N2, N3),
	number_tree(B0, B, N3, N).
number_tree(Tree0, N0-Tree, N0, N) :-
	N1 is N0 +1,
	number_tree1(Tree0, Tree, N1, N).

% = tree_to_component(+Tree, -Component)
%
% convert Tree, which is a tree with node information,
% to Component.

tree_to_component(N-Tree, Component) :-
	tree_to_component1(Tree, N, empty, Component).

tree_to_component1(dia(I,D-Tree), N, C0, C) :-
	!,
	btree_insert(C0, N, p(I,D), C1),
	tree_to_component1(Tree, D, C1, C).
tree_to_component1(p(I,D1-T1,D2-T2), N, C0, C) :-
	!,
	btree_insert(C0, N, [p(I,D1,D2)], C1),
	tree_to_component1(T1, D1, C1, C2),
	tree_to_component1(T2, D2, C2, C).
tree_to_component1(leaf(_), _, C, C) :-
	!.
tree_to_component1(X, _, _, _) :-
	functor(X, F, A),
	arg(2, X, Arg1),
	format('~w ~w ~w ~w', [X, F, A, Arg1]).
