% -*- Mode: Prolog -*-

% This graph plots the maximum length of the unary connective prefix (in 
% balanced pairs of connectives) against the total number of different
% sequences (second column) and the total number of non-equivalent
% sequences (third column) where every sequence B such that A |- B and
% B |- A are both derivable have been reduced (see below for the
% simplification strategy used).
%
%  0          1       1
%  1          3       3
%  2          9       7
%  3         29      15
%  4         99      29
%  5        351      55
%  6      1.275      99
%  7      4.707     173 
%  8     17.577     295
%  9     66.197     491
% 10    250.953     799
% 11    956.385   1.279
% 12  3.660.541   2.013
% 13 14.061.141   3.121

% simplification now works by reducing sequences of the form
%
% a^n b^n a^n  ->  a^n
%
% where a is either dia or box and b is the other connector.
% This removes all sequences which are (a priori) equivalent to
% a simpler sequence.

/* The following predicates are directly copied from g3.in */

add_uni0(lit(A,L), lit(A,L,T,U), T, U, U).
add_uni0(dia(0,A0), dia(0,A), T, [l(0)|U1], U) :-
	add_uni0(A0, A, T, U1, U).
add_uni0(box(0,A0), box(0,A), T, [m(0)|U1], U) :-
	add_uni0(A0, A, T, U1, U).
add_uni0(dl(I,A0,B0), dl(I,A,B), _, _, _) :-
	add_uni1(A0, A, L, L),
	add_uni0(B0, B, M, M, _).
add_uni0(dr(I,A0,B0), dr(I,A,B), _, _, _) :-
	add_uni0(A0, A, M, M, _),
	add_uni1(B0, B, L, L).
add_uni0(p(I,A0,B0), p(I,A,B), _, _, _) :-
	add_uni0(A0, A, L, L, _),
	add_uni0(B0, B, M, M, _).

add_uni1(lit(A,L), lit(A,L,U0,U), U0, U).
add_uni1(dia(0,A0), dia(0,A), U0, U) :-
	add_uni1(A0, A, [m(0)|U0], U).
add_uni1(box(0,A0), box(0,A), U0, U) :-
	add_uni1(A0, A, [r(0)|U0], U).
add_uni1(dl(I,A0,B0), dl(I,A,B), _, _) :-
	add_uni0(A0, A, M, M, _),
	add_uni1(B0, B, L, L).
add_uni1(dr(I,A0,B0), dr(I,A,B), _, _) :-
	add_uni1(A0, A, L, L),
	add_uni0(B0, B, M, M, _).
add_uni1(p(I,A0,B0), p(I,A,B), _, _) :-
	add_uni1(A0, A, L, L),
	add_uni1(B0, B, L, L).

parse_uni -->
	[].
parse_uni -->
	[l(0)],
	parse_uni,
	[m(0)],
	parse_uni.
parse_uni -->
	[m(0)],
	parse_uni,
	[r(0)],
	parse_uni.

/* end of copied code, new code follows */

unary_derivations(N, E) :-
	all_inert_unary(N, LU),
%	findall(S-T, prove_pairs(LU, S, T), L),
	length(LU, Len),
	format('Vertices: ~w~n', [Len]),
	elim_equiv(LU, M, E),
%	vertices_edges_to_ugraph([], L, Gr0),
%	reduce(Gr0, Gr),
	length(M, LenR),
	print_graph(M),
	format('Reduced : ~w~n', [LenR]).

% = elim_equiv(+In, -Out, -Equi)
%
% true if Out contains all the formulas of In minus those which are
% equivalent to a simpler formula. Equi will be a list of pairs
% of equivalent formulas.

elim_equiv(L0, L, E) :-
	select(P, L0, L1),
	select(Q, L1, L2),
	add_uni0(P, P0, VP0, VP0, _),
	add_uni0(Q, Q0, VQ0, VQ0, _),
	add_uni1(P, P1, VP1, VP1),
	add_uni1(Q, Q1, VQ1, VQ1),
	positions(P0, M0, M1),
	positions(Q1, M1, M),
	M = [],
	positions(Q0, N0, N1),
	positions(P1, N1, N),
	N = [],
	parse_uni(M0, M),
	parse_uni(N0, N),
	!,
	write('.'),
	flush_output,
	simplest(P, Q, R, S),
	E = [R-S|E0],
	elim_equiv([R|L2], L, E0).
elim_equiv(L, L, []) :-
	nl.

simplest(P, Q, R, S) :-
	count_unary(P, 0, NP),
	count_unary(Q, 0, NQ),
    (
	NP < NQ
    ->
	R = P,
	S = Q
    ;
	R = Q,
	S = P
    ).

count_unary(lit(_,_), N, N).
count_unary(dia(_,A), N0, N) :-
	N1 is N0 + 1,
	count_unary(A, N1, N).
count_unary(box(_,A), N0, N) :-
	N1 is N0 + 1,
	count_unary(A, N1, N).

prove_pairs(L, S, T) :-
	member(P, L),
	member(Q, L),
	P \== Q,
	add_uni0(P, PU, VP, VP, _),
	add_uni1(Q, QU, VQ, VQ),
	positions(PU, M0, M1),
	positions(QU, M1, M),
	M = [],
    (
	parse_uni(M0, M)
    ->
	S = P,
	T = Q
    ).

unary_derivations(N) :-
	telling(Stream),
	tell('deriv.dot'),
	write('digraph "deriv" {'),
	nl,
	print_layers(0, N),
	tell(Stream),
	simplify_unary(N, L0),
	elim_equiv(L0, L, Ed),
	length(L, LenR),
	format('Reduced : ~w~n', [LenR]),
	telling(Stream),
	tell('equiv.dot'),
	write('digraph "equiv" {'),
	nl,	
	write_edges(Ed),
	write('}'),
	nl,
	told,
	tell('deriv.dot'),
	write_nodes(L),
	tell(Stream),
	prove_pairs(L),
	fail.
unary_derivations(_) :-
	telling(Stream),
	tell('deriv.dot'),
	write('}'),
	nl,
	told,
	shell('/Applications/Graphviz.app/Contents/MacOS/tred deriv.dot > tred.dot'),
	tell(Stream).

prove_pairs(L) :-
	member(P, L),
	member(Q, L),
	P \== Q,
	add_uni0(P, PU, VP, VP, _),
	add_uni1(Q, QU, VQ, VQ),
	positions(PU, M0, M1),
	positions(QU, M1, M),
	M = [],
   (
	parse_uni(M0, M)
    ->
	telling(Stream),
	tell('deriv.dot'),
	write('   "'),
	print_form(P),
	write('" -> "'),
	print_form(Q),
	write('"'),
	write_max_layer(P,Q),
	write(';'),
	nl,
	tell(Stream)
    ).

positions(dia(_,A), M0, M) :-
	positions(A, M0, M).
positions(box(_,A), M0, M) :-
	positions(A, M0, M).
positions(lit(_, _, M0, M), M0, M).

% = 

test_equiv(F0, F) :-
	test_equiv1(F0, F1),
	!,
	test_equiv(F1, F).
test_equiv(F, F).

test_equiv1(F0, F) :-
	form_list(F0, L),
	append(A, [B,C,D|_], L),
	B >= C,
	D >= C,
	sum_length(A, 0, N),
	RS is N + C,
	TL is C*2,
	reduce_equiv(F0, RS, TL, F).

sum_length([], L, L).
sum_length([N|Ns], L0, L) :-
	L1 is L0 + N,
	sum_length(Ns, L1, L).

reduce_equiv(F0, RS0, C, F) :-
    (
	RS0 > 0
    ->
	RS is RS0 - 1,
	reduce_equiv1(F0, F, G0, G),
	reduce_equiv(G0, RS, C, G)
    ;
	reduce_equiv2(F0, C, F)
    ).


reduce_equiv2(F0, C0, F) :-
    (
	C0 =:= 0
   ->
	F = F0
    ;
	reduce_equiv3(F0, F1),
	C is C0 - 1,
	reduce_equiv2(F1, C, F)
    ).

reduce_equiv3(dia(0, A), A).
reduce_equiv3(box(0, A), A).

reduce_equiv1(dia(0, A0), dia(0, A), A0, A).
reduce_equiv1(box(0, A0), box(0, A), A0, A).

form_list(lit(_, _), []).
form_list(dia(0, A), L) :-
	form_list_dia(A, 1, L).
form_list(box(0, A), L) :-
	form_list_box(A, 1, L).

form_list_dia(lit(_, _), N, [N]).
form_list_dia(dia(0, A), N0, L) :-
	N is N0 + 1,
	form_list_dia(A, N, L).
form_list_dia(box(0, A), N, [N|L]) :-
	form_list_box(A, 1, L).

form_list_box(lit(_, _), N, [N]).
form_list_box(dia(0, A), N, [N|L]) :-
	form_list_dia(A, 1, L).
form_list_box(box(0, A), N0, L) :-
	N is N0 + 1,
	form_list_box(A, N, L).


simplify_unary(N, A) :-
	all_inert_unary(N, L),
	length(L, Len),
	format('Vertices: ~w~n', [Len]),
	replace_all(L, M),
	sort(M, A),
	length(A, LenR),
	format('Trivial : ~w~n', [LenR]).

replace_all([], []).
replace_all([A|As], [B|Bs]) :-
	replace(A, B),
    (
	A == B
    ->
%	format('~@ =:= ~@~n', [print_form(A),print_form(B)])
	format('~w =:= ~w~n', [A,B])
    ;
%	format('~@ <-> ~@~n', [print_form(A),print_form(B)])
	format('~w <-> ~w~n', [A,B])
    ),
	replace_all(As, Bs).

replace(A0, A) :-
	replace1(A0, A1),
	!,
	replace(A1, A).
%replace(dia(I,A0), dia(I,A)) :-
%	!,
%	replace(A0, A).
%replace(box(I,A0), box(I,A)) :-
%	!,
%	replace(A0, A).
replace(A, A).

replace1(dia(0,box(0,dia(0,A))), dia(0,A)) :-
	!.
replace1(box(0,dia(0,box(0,A))), box(0,A)) :-
	!.
replace1(dia(0,dia(0,box(0,box(0,dia(0,dia(0,A)))))), dia(0,dia(0,A))) :-
	!.
replace1(box(0,box(0,dia(0,dia(0,box(0,box(0,A)))))), box(0,box(0,A))) :-
	!.
replace1(dia(0,dia(0,dia(0,box(0,box(0,box(0,dia(0,dia(0,dia(0,A))))))))), dia(0,dia(0,dia(0,A)))) :-
	!.
replace1(box(0,box(0,box(0,dia(0,dia(0,dia(0,box(0,box(0,box(0,A))))))))), box(0,box(0,box(0,A)))) :-
	!.
replace1(dia(I,A0), dia(I,A)) :-
	!,
	replace1(A0, A).
replace1(box(I,A0), box(I,A)) :-
	!,
	replace1(A0, A).


all_inert_unary(N0, L) :-
    (
	N0 =:= 0
    ->
	all_inert_unary1(N0, L)
    ;
	N0 > 0,
	N is N0 - 1,
	all_inert_unary(N, L0),
	all_inert_unary1(N0, L1),
	append(L0, L1, L)
    ).

% = all_inert_unary(+N, -List) 
%
% true if List is a list containing all inert balanced sequences of length N
% of unary modes followed by a single atomic formula

all_inert_unary1(N, L) :-
	setof(F, inert_form(N, N, F), L).

inert_form(D, B, F) :-
	compare(OD, D, 0),
	compare(OB, B, 0),
	inert_form(OD, D, OB, B, F0),
	test_equiv(F0, F),
	F == F0.

% inert_form(D, B, F) :-
%	compare(OD, D, 0),
%	compare(OB, B, 0),
%	inert_form(OD, D, OB, B, F).


inert_form(=, _D, OB, B, F) :-
	inert_form1(OB, B, F).
inert_form(>, D, OB, B, F) :-
	inert_form3(OB, B, D, F).

inert_form1(=, _B, lit(a,[])).
inert_form1(>, B0, box(0,F)) :-
	B is B0 - 1,
	compare(OB, B, 0),
	inert_form1(OB, B, F).

inert_form2(=, _B, lit(a,[])).
inert_form2(>, D0, dia(0,F)) :-
	D is D0 - 1,
	compare(OD, D, 0),
	inert_form2(OD, D, F).

inert_form3(=, _B, D0, dia(0,F)) :-
	D is D0 - 1,
	compare(OD, D, 0),
	inert_form2(OD, D, F).
inert_form3(>, B0, D0, F) :-
    (
	B = B0,
	D is D0 - 1,
	F = dia(0,F0)
    ;
	B is B0 - 1,
	D = D0,
	F = box(0,F0)
    ),
	inert_form(D, B, F0).

% = output predicates

print_form(lit(_, _)) :-
	write(a).
print_form(dia(_,A)) :-
	write('<>'),
	print_form(A).
print_form(box(_,A)) :-
	write('[]'),
	print_form(A).

write_max_layer(P, Q) :-
	count_unary(P, 0, NP0),
	NP is NP0 // 2,
	count_unary(Q, 0, NQ0),
	NQ is NQ0 // 2,
	Max is max(NP,NQ)+1,
	format(' [layer="~w:all"]', [Max]).

print_layers(N0, N) :-
	format('~n   layers="', []),
	print_layers1(N0, N),
	format('";~2n', []).

print_layers1(N0, N) :-
    (
	N0 = N
    ->
	N1 is N0 + 1,
	format('~w', [N1])
    ;
	N1 is N0 + 1,
	format('~w:', [N1]),
	print_layers1(N1, N)
    ).

write_nodes([]) :-
	nl.
write_nodes([N|Ns]) :-
	count_unary(N, 0, NC0),
	NC is (NC0 // 2) + 1,
	format('   "~@" [layer="~w:all"]; ~n', [print_form(N), NC]),
	write_nodes(Ns).

write_edges([]) :-
	nl.
write_edges([A-B|Es]) :-
	format('    "~@" -> "~@" [dir=both]; ~n', [print_form(A),print_form(B)]),
	write_edges(Es).
