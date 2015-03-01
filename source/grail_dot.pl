% -*- Mode: Prolog -*-

:- module(grail_dot, [reportray_graph/0,
	               portray_graph/8,
		       portray_graph/10,
	               portray_graph_c/8,
		       portray_graph_c/10,
		       erase_dot_files/0,
		       show_lookup/3,
		       dot_structural_rules/1,
		       create_leaves/4,
		       portray_structural_rules/5]).

:- use_module(options, [get_option/2,
			set_option/2,
			option_true/1,
			option_false/1]).

% ===================================================================
% =                    User Definable Parameters                    =
% ===================================================================

% = word_mode(+Mode)
%
% if Mode is set to `wordonly', the vertices corresponding to a lexical
% anchor will have the lexical word substituted for its vertex
% number, if Mode is set to `word' both the lexical anchor and the
% vertex number will be displayed. Otherwise, only the vertex number
% is displayed.

word_mode(word).

% options to alter the record shape and color of both internal and
% external nodes. Note that when a node is both external and internal
% (in the case of an atomic lexical entry or goal formula before axiom
% links are made) it will be portrayed as external.

external_shape(record).
external_color(black).

internal_shape(record).
internal_color(white).

% = graph_format(+Mode)

% if Mode unifies with `dot', extra invisible edges are added and the
% to help dot display the graph.

graph_format(dot).

% = edge_direction(+Dir)

% if Dir unifies with `reversed', the direction of the edges of a par
% link is top-down, ie. from the hypotheses to the conclusions. If
% not, layout uses the `real' edge direction, making sure we produce
% a DAG.

edge_direction(reversed).

% ===================================================================
% =                        Cleaning Up Files                        =
% ===================================================================

erase_dot_files :-
    (
	option_true(gv)
    ->
	'$DOTPREFIX'(Prefix),
	'$DOTFILE'(Num),
	erase_dot_files(Prefix, 0, Num)
    ;
	true
    ).

erase_dot_files(Prefix, Num0, Num) :-
	concat_atom([Prefix, Num0, '.ps'], PSFile),
	delete_file_if_exists(PSFile),
    (
	Num0 = Num
    ->
	true
    ;
	Num0 < Num
    ->
	Num1 is Num0 + 1,
	erase_dot_files(Prefix, Num1, Num)
    ).

% ===================================================================
% =                    Graph Display Predicates                     =
% ===================================================================

reportray_graph :-
    (
	user:'$GRAPH'(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp)	
    ->
	portray_graph(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp)	
    ;
	user:'$GRAPH'(Xs, Rs, Gr, String, Rt, Ls, N0, Mp)
    ->
	portray_graph(Xs, Rs, Gr, String, Rt, Ls, N0, Mp)
    ;
	true
    ).

portray_graph_header(String, BgColor, Color) :-
	format('digraph "~s" {~2n\c
                bgcolor = ~w;~2n\c
	        node[color = ~w, fontcolor = ~w, shape = plaintext];~2n\c
	        edge[color = ~w, labelfontcolor = ~w]~2n', [String, BgColor, Color, Color, Color, Color]).

portray_graph_c(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp) :-
	get_option(constraint, Constr),
	set_option(constraint, true),
	portray_graph(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp),
	set_option(constraint, Constr).

portray_graph(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp) :-
	retractall(user:'$GRAPH'(_,_,_,_,_,_,_,_)),
	retractall(user:'$GRAPH'(_,_,_,_,_,_,_,_,_,_)),
	assert(user:'$GRAPH'(Pos, Neg0, Xs, Rs, Gr, String, Rt, Ls, N0, Mp)),
	telling(Stream),
	update_dotfile(Num0),
	user:'$DOTPREFIX'(Prefix),
	concat_atom([Prefix, Num0, '.dot'], File),
	format(log, 'Writing to file ~w.~n', [File]),
	current_prolog_flag(encoding, ENC),
	set_prolog_flag(encoding, utf8),
	tell(File),
	get_option(graph_color, Color),
	get_option(graph_bgcolor, BgColor),
	portray_graph_header(String, BgColor, Color),
    (
	get_option(graph_mode, abstract)
    ->
	true
    ;
	internal_shape(ISh),
	internal_color(ICol),
	external_shape(ESh),
	external_color(ECol),
	portray_leaves(Ls, ESh, ECol, Neg0, Neg),
	portray_posat(Pos, Rt, ISh, ICol, ESh, ECol),
	portray_negat(Neg, ISh, ICol)
    ),
	get_option(new_color, NewColor),
	portray_vertices(Gr, Color, NewColor, N0, N1),
	get_option(active_color, AColor),
	get_option(active_bgcolor, ABgColor),
	portray_graph1(Xs, AColor, ABgColor, N1, N2, Mp),
	portray_graph2(Rs, Color, BgColor, N2, _,  Mp),
	portray_root(Gr, Rt, Pos, ESh, ECol),
	format('~n}~n', []),
	told,
	set_prolog_flag(encoding, ENC),
	tell(Stream),
	layout(Num0).

portray_graph_c(Xs, Rs, Gr, String, Rt, Ls, N0, Mp) :-
	get_option(constraint, Constr),
	set_option(constraint, true),
	portray_graph(Xs, Rs, Gr, String, Rt, Ls, N0, Mp),
	set_option(constraint, Constr).

portray_graph(Xs, Rs, Gr, String, Rt, Ls, N0, Mp) :-
	retractall(interface:'$GRAPH'(_,_,_,_,_,_,_,_)),
	retractall(interface:'$GRAPH'(_,_,_,_,_,_,_,_,_,_)),
	assert('$GRAPH'(Xs, Rs, Gr, String, Rt, Ls, N0, Mp)),
	telling(Stream),
	update_dotfile(Num0),
	user:'$DOTPREFIX'(Prefix),
	concat_atom([Prefix, Num0, '.dot'], File),
	format(log, 'Writing to file ~w.~n', [File]),
	current_prolog_flag(encoding, ENC),
	set_prolog_flag(encoding, utf8),
	tell(File),
	get_option(graph_color, Color),
	get_option(graph_bgcolor, BgColor),
	external_shape(ESh),
	external_color(ECol),
	portray_graph_header(String, BgColor, Color),
    (
	get_option(graph_mode, abstract)
    ->
	true
    ;
	portray_leaves(Ls, ESh, ECol)
    ),
	get_option(new_color, NewColor),
	portray_vertices(Gr, Color, NewColor, N0, N1),
	get_option(active_color, AColor),
	get_option(active_bgcolor, ABgColor),
	portray_graph1(Xs, AColor, ABgColor, N1, N2, Mp),
	portray_graph2(Rs, Color, BgColor, N2, _,  Mp),
	portray_root(Gr, Rt, [], ESh, ECol),
	format('~n}~n', []),
	told,
	set_prolog_flag(encoding, ENC),
	tell(Stream),
	layout(Num0).

% = Portraying the lookup

flatten_lookup([], [], WL, WL, FL, FL).
flatten_lookup([W|Ws], [F|Fs], WL0, WL, FL0, FL) :-
    (
       F = [_|_]
    ->
       create_leaves(F, 0, W, Leaves0),
       append(WL0, Leaves0, WL1),
       append(FL0, F, FL1)
    ;
       append(WL0, [W], WL1),
       append(FL0, [F], FL1)
    ),
    flatten_lookup(Ws, Fs, WL1, WL, FL1, FL).

% = 

create_leaves([], _, _, []).
create_leaves([_F|Fs], N0, W, [A|Ls]) :-
    (
        Fs = []
    ->
        A = W,
        Ls = []
    ;
	N is N0 + 1,
	concat_atom([W, N], ' ', A),
	create_leaves1(Fs, N, W, Ls)
    ).

create_leaves1([], _, _, []).
create_leaves1([_F|Fs], N0, W, [A|Ls]) :-
	N is N0 + 1,
	concat_atom([W, N], ' ', A),
	create_leaves1(Fs, N, W, Ls).

show_lookup(Ws, Fs, Formula) :-
	flatten_lookup(Ws, Fs, [], ListOfWords, [], ListOfFormulas),
	unfold(ListOfFormulas, Formula, _Sem, N, Root, Leaves0,
	       Neg, Pos, C, P0, Mp0),
	merge_lists(ListOfWords, Leaves0, Leaves),
	all_waiting_components(Neg, Pos, P0, Avoid, Mp0, Mp1),
	active_pars(P0, [], Active, Rest, Avoid, Mp1, Mp),
	portray_graph_c(Pos, Neg, Active, Rest, C, "Lookup Graph", Root, Leaves, N, Mp).

% = Portraying the structural rules

dot_structural_rules(Rules) :-
	tell('structural_rules.dot'),
	get_option(graph_color, Color),
	get_option(graph_bgcolor, BgColor),
	get_option(active_color, AColor),
	portray_graph_header("Structural Rules", BgColor, Color),
	format('  compound=true;~n', []),
	portray_structural_rules(Rules, Color, AColor, 0, _),
	format('}~n', []),
	told.

portray_structural_rules([], _, _, N, N).
portray_structural_rules([conversion(X,Y,Z)|Rest], Color, AColor, N0, N) :-
	portray_structural_rule(X, Y, Z, Color, AColor, N0, N1),
	portray_structural_rules(Rest, Color, AColor, N1, N).

portray_structural_rule(LHS, RHS, Name, Color, AColor, N0, N) :-
	get_max_var(LHS, 0, M0),
	get_max_var(RHS, M0, M1), % shouldn't change
	M2 is M1 + 1,
	format('~n  subgraph cluster_l~w {~2n    color=~w~2n', [N0,AColor]),
	portray_tensor_term(LHS, Color, 1, M2, M3, N0, N1),
	format('  }~n', []),
	format('~n  subgraph cluster_r~w {~2n    color=~w~2n', [N1,AColor]),
	portray_tensor_term(RHS, Color, 1, M3, _M, N1, N),
	format('  }~n', []),
	ML is M3 - 1,	
	format('~n{ rank = same;~n  ~w [label="~p"];~n  ~w [label="~p"];~n', [N0, '$VAR'(ML), N1, '$VAR'(ML)]),
	format('  ~w -> ~w [rank=same,ltail="cluster_l~w",lhead="cluster_r~w",label="~w"];~n}~n', [N0,N1,N0,N1,Name]).

get_max_var('$VAR'(V), N0, N) :-
	N is max(N0, V).
get_max_var(dia(_,A), N0, N) :-
	get_max_var(A, N0, N).
get_max_var(p(_,A,B), N0, N) :-
	get_max_var(A, N0, N1),
	get_max_var(B, N1, N).

portray_tensor_term('$VAR'(V), _, _, M, M, N0, N) :-
	N is N0 + 1,
	format('~w [label="~p"];~n', [N0, '$VAR'(V)]).	
portray_tensor_term(p(I,A,B), Col, IsTree, M0, M, N0, N) :-
	N1 is N0 + 1,
	portray_tensor_term(A, Col, IsTree, M0, M1, N1, N2),
	N3 is N2 + 1,
	portray_tensor_term(B, Col, IsTree, M1, M2, N3, N4),
	NC = N0,
	NA = N1,
	NB = N3,
	M is M2 + 1,
	format('~w [label="~p"];~n', [NC, '$VAR'(M2)]),
    (
	IsTree =:= 0	
    ->
	N5 is N4 + 1,
	format('{rank=same; ~w -> ~w -> ~w [style=invis]};~n', [NA, N4, NB]),
	format('~w [label="", style=invis, width=.1];~n', [N4]),
	format('~w -> ~w [style=invis];~n', [N4,NC])
    ;
	N5 = N4
    ),
	N is N5 + 1,
	portray_mode(I, PI),
	portray_tentacles(T1, T2, T3),
	format('~w [color = ~w, fontcolor= ~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N5, Col, Col,PI]),
    (
	edge_direction(reversed)	
    ->
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none];~n', [NA, N5, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none];~n', [NB, N5, Col, Col, T2]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none, weight = 10];~n', [N5, NC, Col, Col, T3])
    ;
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none];~n', [N5, NA, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none];~n', [N5, NB, Col, Col, T2]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none, weight = 10, dir=back];~n', [NC, N5, Col, Col, T3])
    ).

portray_tensor_term(dia(I,A), Col, IsTree, M0, M, N0, N) :-
	N1 is N0 + 1,
	portray_tensor_term(A, Col, IsTree, M0, M1, N1, N2),
	NA = N1,
	NB = N0,
	N is N2 + 1,
	M is M1 + 1,
	format('~w [label="~p"];~n', [NB, '$VAR'(M1)]),
	portray_mode(I, PI),
	portray_tentacles(T1, T2),
	format('~w [color=~w, fontcolor=~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N2, Col, Col,PI]),
    (
	edge_direction(reversed)	
    ->
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", weight = 5, arrowhead=none];~n', [NA, N2, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", weight = 10, arrowhead=none];~n', [N2, NB, Col, Col, T2])
    ;
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", weight = 5, arrowhead=none];~n', [N2, NA, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", weight = 10, dir=back, arrowhead=none];~n', [NB, N2, Col, Col, T2])
    ).

% ===

portray_root(Gr, Rt, Pos, ESh, ECol) :-
    (
	Rt < 0
    ->
	true
    ;
	get_option(graph_mode, abstract)
    ->
	(
	    btree_get(Gr, Rt, cmp(_, RtCmp))
	->
	    btree_foldl(RtCmp, user:count_items, 0, Items)
	;
	    Items=0
	),
	format('~n~w [shape=record, label="{ ~w | ~w }"];~n', [Rt,Rt,Items])
    ;
	(
	    word_mode(word)
	->
	    (
		member(_-at(Rt,_,_,_,_,_), Pos)
	    ->
	        true
	    ;
	        format('~n~w [label=', [Rt]),
	        portray_record([Rt, 'Goal']),
   	        format(', shape=~w, color=~w, height=0.4];~n', [ESh,ECol])
	    )
	;
	    word_mode(wordonly)
        ->
	    format('~n~w [label="Goal", shape=rectangle, peripheries=2, height=0.4];~n', [Rt])
	;
	    format('~n~w [label="~w", shape=rectangle, peripheries=2, height=0.4];~n', [Rt, Rt])
	)
    ).


portray_posat([], _, _, _, _, _).
portray_posat([F-at(N,_,_,Vars,_,_)|As], Rt, ISh, ICol, ESh, ECol) :-
	atom_name(F, Vars, Name),
	atom_font_color(F, FontColor),
    (
	N = Rt
    ->
	format('~w [shape=~w, color=~w, fontcolor=~w, label=', [N,ESh,ECol,FontColor]),
	portray_record([Name,N,'Goal']),
        format(', height=0.4];~n', [])
    ;
	format('~w [shape=~w, color=~w, fontcolor=~w, label=', [N,ISh,ICol,FontColor]),
	portray_record([Name,N]),
        format(', height=0.4];~n', [])
    ),
	portray_posat(As, Rt, ISh, ICol, ESh, ECol).

portray_negat([], _, _).
portray_negat([F-at(N,_,_,Vars,_,_)|As], ISh, ICol) :-
	atom_name(F, Vars, Name),
	atom_font_color(F, FontColor),
	format('~w [shape=~w, color=~w, fontcolor=~w, label=', [N,ISh,ICol,FontColor]),
	portray_record([N,Name]),
	format(', height=0.4];~n', []),
	portray_negat(As, ISh, ICol).

atom_name(s, [_,_,X], Y) :-
	!,
    (
        var(X)
    ->
        Y = s
    ;
        atom(X)
    ->
        concat_atom([s,X], '_', Y)
    ;
        Y = s
    ).
atom_name(pp, [_,_,X], Y) :-
	!,
    (
        var(X)
    ->
        Y = pp
    ;
        atom(X)
    ->
        concat_atom([pp,X], '_', Y)
    ;
        Y = pp
    ).
atom_name(np, [_,_,X], Y) :-
	!,
    (
        var(X)
    ->
        Y = np
    ;
        atom(X)
    ->
        concat_atom([np,X], '_', Y)
    ;
        Y = np
    ).
atom_name(np, [_,_,X,V], Y) :-
	!,
    (
        var(X)
    ->
        Y0 = np
    ;
        atom(X)
    ->
        concat_atom([np,X], '_', Y)
    ;
        Y0 = np
    ),
    (
        var(V)
    ->
        Y = Y0
    ;
        atom(V)
    ->
        concat_atom([Y0,V], '_', Y)
    ;
        Y = Y0
    ).
atom_name(np, [_,_,X,V,W], Y) :-
	!,
    (
        var(X)
    ->
        Y0 = np
    ;
        atom(X)
    ->
        concat_atom([np,X], '_', Y)
    ;
        Y0 = np
    ),
    (
        var(V)
    ->
        Y1 = Y0
    ;
        atom(V)
    ->
        concat_atom([Y0,V], '_', Y1)
    ;
        Y1 = Y0
    ),
    (
        var(W)
    ->
        Y = Y1
    ;
        atom(W)
    ->
        concat_atom([Y1,W], '_', Y)
    ;
        Y = Y1
    ).
atom_name(n, [_,_,X], Y) :-
	!,
    (
        var(X)
    ->
        Y = n
    ;
        X = p
    ->
        Y = np
    ;
        atom(X)
    ->
        concat_atom([n,X], '_', Y)
    ;
        Y = n
    ).
atom_name(A, _, A).

portray_leaf(W, L) :-
	portray_record([W,L]).

portray_leaves([], _, _).
portray_leaves([L-W|Ls], ESh, ECol) :-
    (
	word_mode(word)
    ->
	format('~w [shape=~w, color=~w, label=', [L,ESh,ECol]),
	portray_record([W,L]),
	format(', height=0.4];~n', [])
    ;
        word_mode(wordonly)
    ->
	format('~w [shape=rectangle, label="~w", height=0.4];~n', [L,W])
    ;
	format('~w [shape=rectangle, height=0.4];~n', [L])
    ),
	portray_leaves(Ls, ESh, ECol).

portray_leaves([], _, _, Neg, Neg).
portray_leaves([L-W|Ls], ESh, ECol, Neg0, Neg) :-
    (
	word_mode(word)
    ->
	(
	    select(At-at(L,_,_,_,_,_), Neg0, Neg1)
	->
	    atom_font_color(At, FontColor),
	    format('~w [shape=~w, color=~w, fontcolor=~w, label=', [L,ESh,ECol,FontColor]),
            portray_record([W,L,At]),
            format(', height=0.4];~n', [])
	;   
	    Neg1 = Neg0,
	    format('~w [shape=~w, color=~w, label=', [L,ESh,ECol]),
	    portray_record([W,L]),
	    format(', height=0.4];~n', [])
	)
    ;
	Neg1 = Neg0,
        word_mode(wordonly)
    ->
	format('~w [shape=rectangle, label="~w", height=0.4];~n', [L,W])
    ;
	format('~w [shape=rectangle, height=0.4];~n', [L])
    ),
	portray_leaves(Ls, ESh, ECol, Neg1, Neg).


atom_font_color(Atom, FontColor) :-
    (
	Atom ='*'
    ->
	get_option(active_color, FontColor)
    ;
	get_option(graph_color, FontColor)
    ).

portray_vertices(Gr, Col, NCol, N0, N) :-
	btree_to_list(Gr, List),
	portray_vertices1(List, Col, NCol, N0, N).

portray_vertices1([], _, _, N, N) :-
	nl.
portray_vertices1([R-cmp(New, Cmp)|Rest], Col, NCol, N0, N) :-
	get_option(graph_mode, GraphMode),
    (
	GraphMode == abstract
    ->
	btree_foldl(Cmp, user:count_items, 0, Ct),
	format('~w [shape = Mrecord, label = "{~w | ~w}"];~n', [R, R, Ct]),
	portray_vertices1(Rest, Col, NCol, N0, N)
    ;
	GraphMode == distributed
    ->
	format('subgraph cluster~w {~2nstyle=dotted;~ncolor=blue;~2n', [R]),
	btree_subtract(New, Cmp, Old),
	btree_to_list(Cmp, List),
	portray_comp_leaves(List, [], [], Col),
	btree_to_list(Old, OldL),
	portray_component2(OldL, Col, empty, Tp0, empty, Bt0, N0, N1),
	btree_to_list(New, NewL),
	portray_component2(NewL, NCol, Tp0, Tp, Bt0, Bt, N1, N2),
	portray_invisilinks(Tp, Bt),
	format('~n{rank = sink ~w}~2n}~2n', [R]),
	portray_vertices1(Rest, Col, NCol, N2, N)
    ;
	btree_subtract(New, Cmp, Old),
	portray_component(Old, Col, R, N0, N1),
	portray_component(New, NCol, R, N1, N2),
	portray_vertices1(Rest, Col, NCol, N2, N)
    ).

portray_record(List0) :-
    (
	edge_direction(reversed)
    ->
	List = List0
    ;
	reverse(List0, List)
    ),
	portray_record1(List).

portray_record1([]) :-
	write('"{ }"').
portray_record1([X|Xs]) :-
	write('"{'),
	portray_record2(Xs, X).

portray_record2([], X) :-
	format(' ~p }"', X).
portray_record2([X|Xs], Y) :-
	format(' ~p |', Y),
	portray_record2(Xs, X).

portray_component(Cmp, Col, K, N0, N) :-
	btree_to_list(Cmp, List),
	format('node [color=~w, shape=plaintext];~2n', [Col]),
	tree_component(Cmp, IsTree),
	portray_component1(List, Col, IsTree, N0, N),
	/* portray the root even if the component is empty */
	format('~w;~n', [K]),
	format('node [shape=plaintext];~2n', []).

portray_invisilinks(Tp, Bt) :-
	btree_to_list(Tp, TpList),
	portray_invisilinks1(TpList, Bt).

portray_invisilinks1([], _).
portray_invisilinks1([K-VT|Rest], Bt) :-
    (
	btree_get(Bt, K, VB)
    ->
	portray_invisilinks2(VT, VB)
    ;
	true
    ),
	portray_invisilinks1(Rest, Bt).

portray_invisilinks2([], _).
portray_invisilinks2([T|Ts], Bs) :-
	portray_invisilinks3(Bs, T),
	portray_invisilinks2(Ts, Bs).

portray_invisilinks3([], _).
portray_invisilinks3([B|Bs], T) :-
	format('~w -> ~w [weight=0,style=invis];~n', [T, B]),
	portray_invisilinks3(Bs, T).

portray_comp_leaves([], All, Int, Col) :-
	ord_subtract(All, Int, Leaves),
	format('{rank=source ', []),
	portray_comp_leaves1(Leaves, Col),
	format('}~n', []).
portray_comp_leaves([R-Ts|Rs], All0, Int0, Col) :-
	ord_insert(Int0, R, Int),
	portray_comp_leaves2(Ts, All0, All),
	portray_comp_leaves(Rs, All, Int, Col).

portray_comp_leaves2([], All, All).
portray_comp_leaves2([T|Ts], All0, All) :-
	portray_comp_leaves3(T, All0, All1),
	portray_comp_leaves2(Ts, All1, All).

portray_comp_leaves3(dia(_,A), All0, All) :-
	ord_insert(All0, A, All).
portray_comp_leaves3(p(_,A,B), All0, All) :-
	ord_insert(All0, A, All1),
	ord_insert(All1, B, All).


portray_comp_leaves1([], _).
portray_comp_leaves1([A|As], Col) :-
	portray_comp_leaves1a(As, A, Col).

portray_comp_leaves1a([], A, _Col) :-
	format('~w [style = invis]~n', [A]).

portray_comp_leaves1a([B|Bs], A, Col) :-
	format('~w -> ', [A]),
	portray_comp_leaves1a(Bs, B, Col).

portray_component2([], _, Tp, Tp, Bt, Bt, N, N).
portray_component2([R-Ts|Rs], Col, Tp0, Tp, Bt0, Bt, N0, N) :-
	portray_distri_tensors(Ts, R, Col, Tp0, Tp1, Bt0, Bt1, N0, N1),
	portray_component2(Rs, Col, Tp1, Tp, Bt1, Bt, N1, N).

portray_distri_tensors([], _, _, Tp, Tp, Bt, Bt, N, N).
portray_distri_tensors([T|Ts], R, Col, Tp0, Tp, Bt0, Bt, N0, N) :-
	portray_distri_tensor(T, R, Col, Tp0, Tp1, Bt0, Bt1, N0, N1),
	portray_distri_tensors(Ts, R, Col, Tp1, Tp, Bt1, Bt, N1, N).

portray_component1([], _, _, N, N).
portray_component1([R-Ts|Rs], Col, IsTree, N0, N) :-
	portray_tensors(Ts, R, Col, IsTree, N0, N1),
	portray_component1(Rs, Col, IsTree, N1, N).

portray_tensors([], _, _, _, N, N).
portray_tensors([T|Ts], R, Col, IsTree, N0, N) :-
	portray_tensor(T, R, Col, IsTree, N0, N1),
	portray_tensors(Ts, R, Col, IsTree, N1, N).

portray_distri_tensor(p(I,A,B), C, Col, Tp0, Tp, Bt0, Bt, N0, N) :-
	N1 is N0+1,
	N2 is N1+1, % A
	N3 is N2+1, % B
	N4 is N3+1, % C
	N is N4+1,
	tree234:btree_append_value(Tp0, C, N4, Tp),
	tree234:btree_append_value(Bt0, A, N2, Bt1),
	tree234:btree_append_value(Bt1, B, N3, Bt),
	format('~w [label="~w"];~n', [N2, A]),
	format('~w [label="~w"];~n', [N3, B]),
	format('~w [label="~w"];~n', [N4, C]),
	format('{rank=same ~w -> ~w -> ~w [style=invis]}~n', [N2, N1, N3]),
	portray_mode(I, PI),
	format('~w [color = ~w, fontcolor= ~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col,PI]),
	format('~w [label="", style=invis, width=.1];~n', [N1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="1", arrowhead=none];~n', [N2, N0,Col,Col]),
	format('~w -> ~w [style=invis];~n', [N1, N0]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="2", arrowhead=none];~n', [N3, N0, Col,Col]),
	format('~w -> ~w [color=~w];~n', [N0, N4, Col]).

portray_distri_tensor(dia(I,A), B, Col, Tp0, Tp, Bt0, Bt, N0, N) :-
	N1 is N0+1,
	N2 is N1+1,
	N is N2+1,
	tree234:btree_append_value(Tp0, B, N2, Tp),
	tree234:btree_append_value(Bt0, A, N1, Bt),
	format('~w [label="~w"];~n', [N1, A]),
	format('~w [label="~w"];~n', [N2, B]),
	portray_mode(I, PI),
	format('~w [color=~w, fontcolor=~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col,PI]),
	format('~w -> ~w [color=~w, arrowhead=none];~n', [N1, N0, Col]),
	format('~w -> ~w [color=~w];~n', [N0, N2, Col]).


portray_tensor(p(I,A,B), C, Col, IsTree, N0, N) :-
	N1 is N0+1,
    (
	IsTree =:= 0
    ->
	N is N1+1,
	format('{rank=same ~w -> ~w -> ~w [style=invis]};~n', [A, N1, B]),
	format('~w [label="", style=invis, width=.1];~n', [N1]),
	format('~w -> ~w [style=invis];~n', [N1, N0])
    ;
	N = N1
    ),
	portray_mode(I, PI),
	portray_tentacles(T1, T2, T3),
	format('~w [color = ~w, fontcolor= ~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col,PI]),
    (
	edge_direction(reversed)	
    ->
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none];~n', [A, N0, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none];~n', [B, N0, Col, Col, T2]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none, weight = 10];~n', [N0, C, Col, Col, T3])
    ;
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none];~n', [N0, A, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", arrowhead=none];~n', [N0, B, Col, Col, T2]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", arrowhead=none, weight = 10, dir=back];~n', [C, N0, Col, Col, T3])
    ).

portray_tensor(dia(I,A), B, Col, _IsTree, N0, N) :-
	N is N0+1,
	portray_mode(I, PI),
	portray_tentacles(T1, T2),
	format('~w [color=~w, fontcolor=~w, shape = circle, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col,PI]),
    (
	edge_direction(reversed)	
    ->
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", weight = 5, arrowhead=none];~n', [A, N0, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", weight = 10, arrowhead=none];~n', [N0, B, Col, Col, T2])
    ;
	format('~w -> ~w [color=~w, labelfontcolor=~w, taillabel="~w", weight = 5, arrowhead=none];~n', [N0, A, Col, Col, T1]),
	format('~w -> ~w [color=~w, labelfontcolor=~w, headlabel="~w", weight = 10, dir=back, arrowhead=none];~n', [B, N0, Col, Col, T2])
    ).

portray_graph1([], _, _, N, N, _).
portray_graph1([_-Ys|Xs], Col, BgCol, N0, N, Mp) :-
	portray_graph2(Ys, Col, BgCol, N0, N1, Mp),
	portray_graph1(Xs, Col, BgCol, N1, N, Mp).

portray_graph2([], _, _, N, N, _).
portray_graph2([Y|Ys], Col, BgCol, N0, N, Mp) :-
	portray_graph3(Y, Col, BgCol, N0, N1, Mp),
	portray_graph2(Ys, Col, BgCol, N1, N, Mp).

portray_graph3(p(I,A,B,C), Col, BgCol, N0, N, Mp) :-
    (
	get_option(graph_mode, abstract)
    ->
	aref(A, Mp, CA),
	aref(B, Mp, CB),
	aref(C, Mp, CC)
    ;
	CA = A,
	CB = B,
	CC = C
    ),
	N is N0+1,
	portray_mode(I, PI),
	portray_tentacles(T1, T2, T3),
	format('~w [shape = circle, style = filled, color = ~w, fillcolor = ~w, fontcolor = ~w, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col, BgCol, PI]),
    (
	edge_direction(reversed)
    ->
        /* edge directions reversed to manipulate dot */
	format('~w -> ~w [taillabel="~w",arrowhead=none, color=~w];~n', [N0, CB, T1, Col]),
	format('~w -> ~w [taillabel="~w",arrowhead=none, color=~w];~n', [N0, CC, T2, Col]),
	format('~w -> ~w [headlabel="~w",dir=back, color=~w];~n', [CA, N0, T3, Col])
    ;
        /* `real' edge directions */
	format('~w -> ~w [headlabel="~w",arrowhead=none, color=~w];~n', [CB, N0, T1, Col]),
	format('~w -> ~w [headlabel="~w",arrowhead=none, color=~w];~n', [CC, N0, T2, Col]),
	format('~w -> ~w [taillabel="~w",dir=forward, color=~w];~n', [N0, CA, T3, Col])
    ).
portray_graph3(dl(I,A,B,C), Col, BgCol, N0, N, Mp) :-
    (
	get_option(graph_mode, abstract)
    ->
	aref(A, Mp, CA),
	aref(B, Mp, CB),
	aref(C, Mp, CC)
    ;
	CA = A,
	CB = B,
	CC = C
    ),
	portray_mode(I, PI),
	portray_tentacles(T1, T2, T3),
	format('~w [shape = circle, style = filled, color = ~w, fillcolor = ~w, fontcolor = ~w, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col, BgCol, PI]),
	format('~w -> ~w [headlabel="~w",arrowhead=none, color=~w];~n', [CA, N0, T3, Col]),
	format('~w -> ~w [taillabel="~w", color=~w];~n', [N0, CC, T2, Col]),
	get_option(constraint, Constr),
    (   
	graph_format(dot)
    ->
	N1 is N0+1,
	N is N1+1,
        /* invisible node to align the two conclusions */
       (
	   Constr = false
       ->
	   format('{rank=same ~w -> ~w [style=invis]}~n', [N1, CC]),
	   format('~w [label="",width=.1,style=invis]~n', [N1]),
	   format('~w -> ~w [style=invis];~n', [N0, N1])
       ;
	   format('{rank=same ~w -> ~w -> ~w [style=invis]}~n', [CB, N1, CC]),
	   format('~w [label="",width=.1,style=invis]~n', [N1])
       )
    ;
	N is N0+1
    ),
    (
	edge_direction(reversed)
    ->
        /* edge direction reversed to manipulate dot */
	format('~w -> ~w [tailport=w, headport=w, taillabel="~w", arrowhead=none, color=~w, constraint=~w];~n', [N0, CB, T1, Col, Constr])
    ;
	format('~w -> ~w [tailport=w, headport=w, headlabel="~w", arrowhead=none, color=~w, constraint=~w];~n', [CB, N0, T1, Col, Constr])
    ).
portray_graph3(dr(I,A,B,C), Col, BgCol, N0, N, Mp) :-
    (
	get_option(graph_mode, abstract)
    ->
	aref(A, Mp, CA),
	aref(B, Mp, CB),
	aref(C, Mp, CC)
    ;
	CA = A,
	CB = B,
	CC = C
    ),
	portray_mode(I, PI),
	portray_tentacles(T1, T2, T3),
	format('~w [shape = circle, style = filled, color = ~w, fillcolor = ~w, fontcolor = ~w, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col, BgCol, PI]),
	format('~w -> ~w [headlabel="~w",arrowhead=none, color=~w];~n', [CA, N0, T3, Col]),
	format('~w -> ~w [taillabel="~w",color=~w];~n', [N0, CB, T1, Col]),
	get_option(constraint, Constr),
    (   
	graph_format(dot)
    ->
	N1 is N0+1,
	N is N1+1,
        /* invisible node to align the two conclusions */
       (
	   Constr = false
       -> 
	   format('{rank=same ~w -> ~w [style=invis]}~n', [CB, N1]),
	   format('~w [label="",width=.1,style=invis]~n', [N1]),
	   format('~w -> ~w [style=invis];~n', [N0, N1])
       ;
	   format('{rank=same ~w -> ~w -> ~w [style=invis]}~n', [CB, N1, CC]),
	   format('~w [label="",width=.1,style=invis]~n', [N1])
       )
    ;
	N is N0+1
    ),
    (
	edge_direction(reversed)
    ->
        /* edge direction reversed to manipulate dot */
	format('~w -> ~w [tailport=e, headport=e, taillabel="~w",arrowhead=none, color=~w, constraint=~w];~n', [N0, CC, T2, Col, Constr])
    ;
	format('~w -> ~w [tailport=e, headport=e, headlabel="~w",arrowhead=none, color=~w, constraint=~w];~n', [CC, N0, T2, Col, Constr])
    ).
portray_graph3(dia(I,A,B), Col, BgCol, N0, N, Mp) :-
    (
	get_option(graph_mode, abstract)
    ->
	aref(A, Mp, CA),
	aref(B, Mp, CB)
    ;
	CA = A,
	CB = B
    ),
	N is N0+1,
	portray_mode(I, PI),
	portray_tentacles(T1, T2),
	format('~w [shape = circle, style = filled, color = ~w, fillcolor = ~w, fontcolor = ~w, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col, BgCol, PI]),
    (
	edge_direction(reversed)
    ->
        /* edge directions reversed to manipulate dot */
	format('~w -> ~w [taillabel="~w", arrowhead=none, color=~w];~n', [N0, CB, T1, Col]),
	format('~w -> ~w [headlabel="~w", color=~w dir=back];~n', [CA, N0, T2, Col])
    ;
	format('~w -> ~w [headlabel="~w", arrowhead=none, color=~w];~n', [CB, N0, T1, Col]),
	format('~w -> ~w [taillabel="~w", color=~w dir=forward];~n', [N0, CA, T2, Col])
    ).
portray_graph3(box(I,A,B), Col, BgCol, N0, N, Mp) :-
    (
	get_option(graph_mode, abstract)
    ->
	aref(A, Mp, CA),
	aref(B, Mp, CB)
    ;
	CA = A,
	CB = B
    ),
	N is N0+1,
	portray_mode(I, PI),
	portray_tentacles(T1, T2),
	format('~w [shape = circle, style = filled, color = ~w, fillcolor = ~w, fontcolor = ~w, fontsize=12, width=0.4, height=0.4, fixedsize=true, label="~w"];~n', [N0, Col, Col, BgCol, PI]),
        /* same directions in both cases */
	format('~w -> ~w [headlabel="~w", arrowhead=none, color=~w];~n', [CA, N0, T2, Col]),
	format('~w -> ~w [taillabel="~w", color=~w];~n', [N0, CB, T1, Col]).

portray_mode(I, PI) :-
    (
	option_true(uni_modal),
	option_false(explicit_modes)
    ->
	PI = ''
    ;
	PI = I
    ).

portray_tentacles(T1, T2) :-
    (
	option_false(tentacle_labels)
    ->
	T1 = '',
	T2 = ''
    ;
	option_true(zero_root)
    ->
	T1 = 1,
	T2 = 0
    ;
	T1 = 1,
	T2 = 2
    ).

portray_tentacles(T1, T2, T3) :-
    (
	option_false(tentacle_labels)
    ->
	T1 = '',
	T2 = '',
	T3 = ''
    ;
	option_true(zero_root)
    ->
	T1 = 1,
	T2 = 2,
	T3 = 0
    ;
	T1 = 1,
	T2 = 2,
	T3 = 3
    ).

% = tree_component(+Component, ?IsTree)
%
% computes if the number of nodes which have more than one binary
% link leaving from it in the current component. If this number is
% 0, invisible nodes will be added to the graph to help dot visualize
% it. If the predicate graph_format(Mode) has Mode not equal to `tree'
% no invisible nodes will be added.

tree_component(Cmp, IsTree) :-
    (
	graph_format(dot)
    ->
	btree_foldl(Cmp, grail_dot:is_tree, 0, IsTree)
    ;
	IsTree = 1
    ).

is_tree(_, V, I0, I) :-
	binary_items(V, 0, I1),
    (
	I1 = 0
    ->
	I = I0
    ;
	I is I0+(I1-1)
    ).


binary_items([], N, N).
binary_items([X|Xs], N0, N) :-
    (
	X = p(_, _, _)
    ->
	N1 is N0+1
    ;
	N1 = N0
    ),
	binary_items(Xs, N1, N).
