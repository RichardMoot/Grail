% -*- Mode: Prolog -*-

:- module(list_utils, [my_member/2,
		       merge_lists/3,
		       select1/3,
		       split_odds_evens/3,
		       split_a_b_cs/4,
		       split_a_b_c_ds/5,
		       select_all/3,
		       delete_all/3,
		       insert_nth0/4,
		       strip_values/2,         %  KeySet -> MultiSet
   	               strip_keys/2,           %  KeySet -> List
		       last/2
		       ]).


% = strip_values(+OrdKeyList, ?OrdMSet)
%
% change a ordered list of Key-Value pairs into a list containing only
% the keys. These keys are ordered, but may contain duplicates.

strip_values([], []).
strip_values([K-_|As], [K|Bs]) :-
	strip_values(As, Bs).


% = strip_keys(+ListOfKeyValuePairs, -ListOfKeys)
%
% strips aways the keys of a list of key-value pairs and returns the
% list of values in the same order (the result is *not* an ordset but
% a list).
 
strip_keys([], []).
strip_keys([_-V|As], [V|Bs]) :-
	strip_keys(As, Bs).


% the library predicate member/2, but changed to use first argument
% indexing to prevent leaving a choice point.

my_member(X, [Y|Ys]) :-
	my_member(Ys, Y, X).

my_member([], X, X).
my_member([Y|Ys], _, X) :-
	my_member(Ys, Y, X).

merge_lists(_, [], []) :-
	!.
merge_lists([], Ls, Ls).
merge_lists([W|Ws], [N-_|Ls0], [N-W|Ls]) :-
	merge_lists(Ws, Ls0, Ls).

% the library predicate select/3, but returning only the first solution

select1(X, [X|Xs], Xs) :-
	!.
select1(X, [Y|Ys], [Y|Zs]) :-
	select1(X, Ys, Zs).


% = split_odds_evens(+List, ?Odds, ?Evens)
%
% splits List into two lists containing the Odd and Even elements
% respecively. See `The Craft of Prolog' page 211.

split_odds_evens([], [], []).
split_odds_evens([O|L], [O|Os], Es) :-
	split_evens_odds(L, Os, Es).

split_evens_odds([], [], []).
split_evens_odds([E|L], Os, [E|Es]) :-
	split_odds_evens(L, Os, Es).

% = split_a_b_cs(+List, ?As, ?Bs, ?Cs)
%
% a generalization of split_odds_evens to three list, list As containing
% each element 1+(3k), list Bs containing each element 2+(3k) and list
% Cs each element 3+(3k) (when counting of list elements starts at 1).

split_a_b_cs([], [], [], []).
split_a_b_cs([A|Ds], [A|As], Bs, Cs) :-
	split_b_cs(Ds, As, Bs, Cs).

split_b_cs([], [], [], []).
split_b_cs([B|Ds], As, [B|Bs], Cs) :-
	split_cs(Ds, As, Bs, Cs).

split_cs([], [], [],[]).
split_cs([C|Ds], As, Bs, [C|Cs]) :-
	split_a_b_cs(Ds, As, Bs, Cs).



% = split_a_b_c_ds(+List, ?As, ?Bs, ?Cs, ?Ds)
%
% a generalization of split_odds_evens to fours list, list As containing
% each element 1+(4k), list Bs containing each element 2+(4k), list Cs
% each element 3+(4k) and list Ds each element 4+(4k) (when counting of
% list elements starts at 1).

split_a_b_c_ds([], [], [], [], []).
split_a_b_c_ds([A|Es], [A|As], Bs, Cs, Ds) :-
	split_b_c_ds(Es, As, Bs, Cs, Ds).

split_b_c_ds([], [], [], [], []).
split_b_c_ds([B|Es], As, [B|Bs], Cs, Ds) :-
	split_c_ds(Es, As, Bs, Cs, Ds).

split_c_ds([], [], [], [], []).
split_c_ds([C|Es], As, Bs, [C|Cs], Ds) :-
	split_ds(Es, As, Bs, Cs, Ds).

split_ds([D|Es], As, Bs, Cs, [D|Ds]) :-
	split_a_b_c_ds(Es, As, Bs, Cs, Ds).

% = select_all

select_all([], Ys, Ys).
select_all([X|Xs], Ys0, Ys) :-
	select(X, Ys0, Ys1),
	select_all(Xs, Ys1, Ys).

% = delete_all

delete_all([], Ys, Ys).
delete_all([X|Xs], Ys0, Ys) :-
	delete(Ys0, X, Ys1),
	delete_all(Xs, Ys1, Ys).

delete([], _, []).
delete([X|Xs], Y, Zs) :-
    (
       X == Y
    ->
       delete(Xs, Y, Zs)
    ;
       Zs = [X|Zs0],
       delete(Xs, Y, Zs0)
    ).

% =

last([X|Xs], Y) :-
	last(Xs, X, Y).

last([], X, X).
last([X|Xs], _, Y) :-
	last(Xs, X, Y).

insert_nth0(N0, El, List0, Result) :-
    (
         N0 =< 0
    ->
         Result = [El|List0]
    ;
         N is N0 - 1,
         List0 = [A|As],
         Result = [A|Bs],
         insert_nth0(N, El, As, Bs)
    ).