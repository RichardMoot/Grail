% -*- Mode: Prolog -*-
% = pretty printer

portray('Goal') :-
	write('Goal').

% = quotes

portray('\"') :-
	!,
	write('\\\"').

% = formulas

portray(p(I,A,B)) :-
	!,
	format('( ~p *~p ~p )', [A,I,B]).
portray(dl(I,A,B)) :-
	!,
	format('( ~p \\~p ~p )', [A,I,B]).
portray(dr(I,A,B)) :-
	!,
	format('( ~p /~p ~p )', [A,I,B]).
portray(dia(I,A)) :-
	!,
	format('<>~p ~p', [I,A]).
portray(box(I,A)) :-
	!,
	format('[]~p ~p', [I,A]).
portray(lit(s(A))) :-
	var(A),
	!,
	print(s).
portray(lit(n(A))) :-
	var(A),
	!,
	print(n).
portray(lit(A)) :-
	!,
	print(A).
portray(lit(n(p))) :-
	!,
	print(np).
portray(lit(n(_))) :-
	!,
	print(n).

% = semantics

portray(lambda(X,Y)) :- 
	!,
	format('^~p.~p', [X,Y]).

portray(appl(appl(appl(F,Z),Y),X)) :-
	atom(F),
	!,
	format('~p(~p,~p,~p)', [F,X,Y,Z]).
portray(appl(appl(F,Y),X)) :-
	atom(F),
	!,
	format('~p(~p,~p)', [F,X,Y]).
portray(appl(X,Y)) :-
	!,
	format('~p(~p)', [X,Y]).

portray(pair(X,Y)) :- 
	!,
	format('<~p,~p>', [X,Y]).
portray(pi1(X)) :- 
	!,
	format('pi1(~p)', [X]).
portray(pi2(X)) :- 
	!,
	format('pi2(~p)', [X]).

portray(bool(X,Y,Z)) :- 
	!,
	format('(~p ~p ~p)', [X,Y,Z]).

portray(quant(Q,X,T)) :- 
	!,
	format('~p ~p[~p])', [Q,X,T]).
portray(qualia(C,F,T,A)) :-
	!,
	format('{const:~p,form:~p,telic:~p,agentive:~p}', [C, F, T, A]).

% = axiom link heap items

portray(N-x(P,F,I,_,L,_)) :-
	!,
	format('(~w) ~p~p(~p)  ~p', [N,P,F,I,L]).

% = atomic formulas

portray(Form-at(Node,_Sem,_AtomDist,_FirstOrder,_UMDLIn,_UMDLOut)) :-
	!,
	format('~p(~p)', [Form, Node]).

% = some selected predicates

portray(contract(Ps,Qs,_,C0,_,_,_,_,_)) :-
	!,
	keysort(C0, C),
	write('contract('),
	portray_list(Ps),
	portray_list(Qs),
	portray_list(C),
	write(')'),
	nl.

portray(replace(A,B,C,D,_,_,_,_,_,_)) :-
	!,
	format('~w := ~w~n', [B, C]),
	portray_list(A),
	portray_list(D),
	nl.

portray(expand_component(A0,A1,_,_,_,_,_,_,_)) :-
	!,
    (
        var(A1)
    ->
        keysort(A0,A)
    ;
        keysort(A1,A)
    ),
        write('expand_component('),
        portray_list(A),
        write(')'),
        nl.

/*
portray(X-[]) :-
	!,
	format('[~w]~n', [X]).
portray(X-[p(I,A,B)|Rest]) :-
	!,
	print(X-Rest),
	format('~n  ~w~n  \|~n  ~w~n / \\~n~w   ~w~n', [X,I,A,B]).
portray(X-p(I,A,B)) :-
	!,
	format('~n  ~w~n  \|~n  ~w~n / \\~n~w   ~w~n', [X,I,A,B]).
portray(X-[dia(I,A)|Rest]) :-
	!,
	print(X-Rest),
	format('~n  ~w~n  \|~n  ~w~n  \|~n  ~w~n', [X,I,A]).
portray(X-dia(I,A)) :-
	!,
	format('~n  ~w~n  \|~n  ~w~n  \|~n  ~w~n', [X,I,A]).
*/

% = components

portray(cmp(_,B)) :-
	!,
	print(B).

% = arrays

portray(array(A,B)) :-
	!,
	portray_array(array(A,B)).

% = balanced trees

portray(empty) :-
	!,
	portray_btreef(empty).
portray(tree(A,B,C,D,E)) :-
	!,
	portray_btreef(tree(A,B,C,D,E)).
portray(black(A,B,C,D)) :-
	!,
	portray_btreef(black(A,B,C,D)).
portray(red(A,B,C,D)) :-
	!,
	portray_btreef(red(A,B,C,D)).
portray(two(A,B,C,D)) :-
	!,
	portray_btreef(two(A,B,C,D)).
portray(three(A,B,C,D,E,F,G)) :-
	!,
	portray_btreef(three(A,B,C,D,E,F,G)).
portray(four(A,B,C,D,E,F,G,H,I,J)) :-
	!,
	portray_btreef(four(A,B,C,D,E,F,G,H,I,J)).

% = print routines for various data structures.

% = portray_list(+List)
% 
% prints the elements of List one item per line

portray_list(List) :-
	portray_list(List, user_output).

portray_list([], Stream) :-
	format(Stream, '[]~n', []).
portray_list([X|Xs], Stream) :-
	format(Stream, '[~n', []),
	portray_list(Xs, X, Stream).

portray_list([], X, Stream) :-
	format(Stream, ' ~p~n]~n', [X]).
portray_list([X|Xs], Y, Stream) :-
	format(Stream, ' ~p,~n', [Y]),
	portray_list(Xs, X, Stream).

% = portray_assoc(+Assoc)
%
% prints the elements of Assoc, in increasing order, one item per
% line.

portray_assoc(A) :-
	format('{~n', []),
	portray_assoc1(A),
	format('}~n', []).

portray_assoc1(t).
portray_assoc1(t(K,V,_,L,R)) :-
	portray_assoc1(L),
	format(' ~p~n', [K-V]),
	portray_assoc1(R).

% = portray_btreef(+Btree)
%
% prints the elements of Assoc, in increasing order, without line
% breaks.

portray_btree(Tree) :-
	portray_btree(Tree, user_ouput).

portray_btree(A, Stream) :-
	format(Stream, '{~n', []),
	portray_btree1(A, Stream),
	format(Stream, ' }~n', []).

portray_btree1('*VAR*', Stream) :- 
        !,
	print(Stream, '*VAR*').
portray_btree1(empty, _).
portray_btree1(tree(K,V,_,L,R), Stream) :-
	portray_btree1(L, Stream),
	format(Stream, ' ~p~n',[K-V]),
	portray_btree1(R, Stream).
portray_btree1(red(K,V,L,R), Stream) :-
	portray_btree1(L, Stream),
	format(Stream, ' ~p~n',[K-V]),
	portray_btree1(R, Stream).
portray_btree1(black(K,V,L,R), Stream) :-
	portray_btree1(L, Stream),
	format(Stream, ' ~p~n', [K-V]),
	portray_btree1(R, Stream).
portray_btree1(two(K,V,L,R), Stream) :-
	portray_btree1(L, Stream),
	format(Stream, ' ~p~n', [K-V]),
	portray_btree1(R, Stream).
portray_btree1(three(A,B,C,D,E,F,G), Stream) :-
	portray_btree1(E, Stream),
	format(Stream, ' ~p~n', [A-B]),
	portray_btree1(F, Stream),
	format(Stream, ' ~p~n', [C-D]),
	portray_btree1(G, Stream).
portray_btree1(four(A,B,C,D,E,F,G,H,I,J), Stream) :-
	portray_btree1(G, Stream),
	format(Stream, ' ~p~n', [A-B]),
	portray_btree1(H, Stream),
	format(Stream, ' ~p~n', [C-D]),
	portray_btree1(I, Stream),
	format(Stream, ' ~p~n', [E-F]),
	portray_btree1(J, Stream).

% = portray_btreef(+Btree)
%
% prints the elements of Assoc, in increasing order, without line
% breaks.

portray_btreef(Tree) :-
	portray_btreef(Tree, user_output).

portray_btreef(A, Stream) :-
	format(Stream, '{', []),
	portray_btree1f(A, Stream),
	format(Stream, ' }', []).

portray_btree1f('*VAR*', Stream) :- 
        !,
	print(Stream, '*VAR*').
portray_btree1f(empty, _).
portray_btree1f(tree(K,V,_,L,R), Stream) :-
	portray_btree1f(L, Stream),
	format(Stream, ' ~p', [K-V]),
	portray_btree1f(R, Stream).
portray_btree1f(red(K,V,L,R), Stream) :-
	portray_btree1f(L, Stream),
	format(Stream, ' ~p', [K-V]),
	portray_btree1f(R, Stream).
portray_btree1f(black(K,V,L,R), Stream) :-
	portray_btree1f(L, Stream),
	format(Stream, ' ~p', [K-V]),
	portray_btree1f(R, Stream).
portray_btree1f(two(K,V,L,R), Stream) :-
	portray_btree1f(L, Stream),
	format(Stream, ' ~p', [K-V]),
	portray_btree1f(R, Stream).
portray_btree1f(three(A,B,C,D,E,F,G), Stream) :-
	portray_btree1f(E, Stream),
	format(Stream, ' ~p', [A-B]),
	portray_btree1f(F, Stream),
	format(Stream, ' ~p', [C-D]),
	portray_btree1f(G, Stream).
portray_btree1f(four(A,B,C,D,E,F,G,H,I,J), Stream) :-
	portray_btree1f(G, Stream),
	format(Stream, ' ~p', [A-B]),
	portray_btree1f(H, Stream),
	format(Stream, ' ~p', [C-D]),
	portray_btree1f(I, Stream),
	format(Stream, ' ~p', [E-F]),
	portray_btree1f(J, Stream).

% = portray_array(+Array)

portray_array(array($(A0,A1,A2,A3),Size)) :-
	N is Size-2,
	write('< '),
	portray_subarray(A0, 0, N, 0),
	portray_subarray(A1, 1, N, 0),
	portray_subarray(A2, 2, N, 0),
	portray_subarray(A3, 3, N, 0),
	write('>').
portray_array(array($(A0,A1,A2,A3,A4,A5,A6,A7),Size)) :-
	N is Size-3,
	write('< '),
	portray_subarray(A0, 0, N, 0),
	portray_subarray(A1, 1, N, 0),
	portray_subarray(A2, 2, N, 0),
	portray_subarray(A3, 3, N, 0),
	portray_subarray(A4, 4, N, 0),
	portray_subarray(A5, 5, N, 0),
	portray_subarray(A6, 6, N, 0),
	portray_subarray(A7, 7, N, 0),
	write('>').
portray_array(array($(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),Size)) :-
	N is Size-4,
	write('< '),
	portray_subarray( A0,  0, N, 0),
	portray_subarray( A1,  1, N, 0),
	portray_subarray( A2,  2, N, 0),
	portray_subarray( A3,  3, N, 0),
	portray_subarray( A4,  4, N, 0),
	portray_subarray( A5,  5, N, 0),
	portray_subarray( A6,  6, N, 0),
	portray_subarray( A7,  7, N, 0),
	portray_subarray( A8,  8, N, 0),
	portray_subarray( A9,  9, N, 0),
	portray_subarray(A10, 10, N, 0),
	portray_subarray(A11, 11, N, 0),
	portray_subarray(A12, 12, N, 0),
	portray_subarray(A13, 13, N, 0),
	portray_subarray(A14, 14, N, 0),
	portray_subarray(A15, 15, N, 0),
	write('>').

portray_subarray($, _, _, _) :-
	!.
portray_subarray($(A0,A1,A2,A3), K, N, M) :-
	N > 0,
	!,
	N1 is N-2,
	M1 is (K+M) << 2,
	portray_subarray(A0, 0, N1, M1),
	portray_subarray(A1, 1, N1, M1),
	portray_subarray(A2, 2, N1, M1),
	portray_subarray(A3, 3, N1, M1).
portray_subarray($(A0,A1,A2,A3,A4,A5,A6,A7), K, N, M) :-
	N > 0,
	!,
	N1 is N-3,
	M1 is (K+M) << 3,
	portray_subarray(A0, 0, N1, M1),
	portray_subarray(A1, 1, N1, M1),
	portray_subarray(A2, 2, N1, M1),
	portray_subarray(A3, 3, N1, M1),
	portray_subarray(A4, 4, N1, M1),
	portray_subarray(A5, 5, N1, M1),
	portray_subarray(A6, 6, N1, M1),
	portray_subarray(A7, 7, N1, M1).
portray_subarray($(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), K, N, M) :-
	N > 0,
	!,
	N1 is N-4,
	M1 is (K+M) << 4,
	portray_subarray( A0,  0, N1, M1),
	portray_subarray( A1,  1, N1, M1),
	portray_subarray( A2,  2, N1, M1),
	portray_subarray( A3,  3, N1, M1),
	portray_subarray( A4,  4, N1, M1),
	portray_subarray( A5,  5, N1, M1),
	portray_subarray( A6,  6, N1, M1),
	portray_subarray( A7,  7, N1, M1),
	portray_subarray( A8,  8, N1, M1),
	portray_subarray( A9,  9, N1, M1),
	portray_subarray(A10, 10, N1, M1),
	portray_subarray(A11, 11, N1, M1),
	portray_subarray(A12, 12, N1, M1),
	portray_subarray(A13, 13, N1, M1),
	portray_subarray(A14, 14, N1, M1),
	portray_subarray(A15, 15, N1, M1).
portray_subarray(Item, K, 0, M) :-
        N is K+M,
	format('~p-~p ', [N, Item]).  

