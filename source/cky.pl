
test(T) :-
	parse([a,b,c], T).

parse(Words, Table) :-
	lookup(Words, 0, N, Lookup),
	initialize_table(Lookup, N, Table),
	cky(2, N, Table).

initialize_table(Lookup, N, Table) :-
	functor(Table, table, N),
	arg(1, Table, Lookup),
	fill_table(2, N, Table).

fill_table(N0, N, Table) :-
	fill_table1(N0, N, Table),
    (
        N0 >= N
    ->
        true
    ;
        N1 is N0 + 1,
        fill_table(N1, N, Table)
    ).

fill_table1(N0, N, Table) :-
	arg(N0, Table, L),
	fill_table2(L, 0, N0, N).

fill_table2([t(N0, N2, [])|L], N0, N2, N) :-
   (
        N0 + N2 >= N
   ->
        L = []
   ;
        N1 is N0 + 1,
        fill_table2(L, N1, N2, N)
   ).
    
cky(N0, N, Table) :-
	V0 = 1,
	V1 is N0 - V0,
	cky(V0, V1, N0, N, Table).

cky(V0, V1, N0, N, Table) :-
	arg(V0, Table, L0),
	arg(V1, Table, L1),
	true.
	

lookup([], N, N, []).
lookup([W|Ws0], N0, N, [t(N0, N1, L)|L0]) :-
	N1 is N0 + 1,
	findall(F-lex, S^lex(W,F,S), L),
	lookup(Ws0, N1, N, L0).


lex(a, dr(0,a,a), a).
lex(b, a, b).
lex(c, dl(0,a,a), c).