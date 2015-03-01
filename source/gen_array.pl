% -*- Mode: Prolog -*-
% gen_array
%
% This is a small Prolog program for automatically generating log2^n arrays
% as Prolog modules. 

array_test(N, A) :-
	new_array(A0),
	array_test(N, A0, A).

array_test(N0, A0, A) :-
    (
	N0 < 0
    ->
	A = A0
    ;
	aset(N0, A0, N0, A1),
	N1 is N0-1,
	array_test(N1, A1, A)
    ).

split_integer(N, B, L) :-
	M is (1<<B)-1,
	split_integer(N, B, M, [], L).

split_integer(N, B, M, L0, L) :-
	N1 is N/\M,
	N2 is N>>B,
    (
	N2 =:= 0
    ->
	L = [N1|L0]
    ;
	L1 = [N1|L0],
	split_integer(N2, B, M, L1, L)
    ).


array_item([], L, Array, Item) :-
	sub_array(L, Array, Item),
	not_undef(Item).
array_item([L|Ls], L0, Array0, Item) :-
	sub_array(L0, Array0, Array),
	array_item(Ls, L, Array, Item).

gen_array(B) :-
	B > 0,
	N is 1<<B,
	Mask is N-1,
	format(atom(File), 'log~warray.pl', [N]),
	tell(File),
	format('% -*- Mode: Prolog -*-~n', []),
	format('% ~`=t~32|~n% =~tLog ~w Array Library~t~31|=~n% ~`=t~32|~2n', [N]),
        format('% Copyright (C) 2003-2009 Richard Moot (Richard.Moot@labri.fr)~n', []),
        format('% Based on original code by David H. D. Warren and Fernando Pereira.~2n', []),
        format('% This library is free software; you can redistribute it and/or~n', []),
        format('% modify it under the terms of the GNU Lesser General Public~n', []),
        format('% License as published by the Free Software Foundation; either~n', []),
        format('% version 2.1 of the License, or (at your option) any later version.~2n', []),
        format('% This library is distributed in the hope that it will be useful,~n', []),
        format('% but WITHOUT ANY WARRANTY; without even the implied warranty of~n', []),
        format('% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU~n', []),
        format('% Lesser General Public License for more details.~2n', []),
        format('% You should have received a copy of the GNU Lesser General Public~n', []),
        format('% License along with this library; if not, write to the Free Software~n', []),
        format('% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA~2n', []),
        format('% ====================================================================~2n', []),
        format('% This file is very heavily inspired by the file logarr.pl from the~n', []),
        format('% Public Domain DEC 10 Prolog Library, which was programmed by~n', []),
        format('% David H. D. Warren and Fernando Pereira.~2n', []),
        format('% There should be little difference between the code here and the~n', []),
        format('% original for client programs, with the exception that undefined~n', []),
        format('% items are Prolog variables instead of the constant $.~2n', []),
        format('% The main reason for coding this was that I liked to have a tree~n', []),
        format('% implementation of arrays where the item position was constructed~n', []),
        format('% bottom-up and the item obtained by unifying the constructed skeleton~n', []),
        format('% with the full aray. For the log4 arrays this appears to use slightly~n', []),
        format('% more logical inferences than the original code. This choice also~n', []),
        format('% forced the use of Prolog variables as `undefined'' value, as it is~n', []),
        format('% possible for the aset/4 operation to extend an empty branch in the~n', []),
        format('% tree and we do not want to unification to fail in this case.~2n', []),
        format('% The current file uses log~w arrays, where an array has up to ~w~n', [N, N]),
        format('% subarrays.~2n', []),
        format('% As in the original Warren/Pereira code, an array is representad as~n', []),
        format('% array(Tree, Size) where Tree is an array with maximum capacity~n', []),
        format('% 2^Size - 1. Since we use log~w arrays, only multiples of ~w are valid~n', [N, B]),
        format('% values of Size.~2n', []),
	format(':- module(log~warr, [~n', [N]),
	format('        new_array/1,~n', []),
	format('        is_array/1,~n', []),
	format('        aref/3,~n', []),
	format('        arepl/5,~n', []),
	format('        arefa/3,~n', []),
	format('        arefl/3,~n', []),
	format('        aset/4,~n', []),
	format('        map_array/3,~n', []),
	format('        array_to_list/2,~n', []),
	format('        list_to_array/2~n', []),
	format('    ]).~2n', []),
	/* new_array */
        format('% = new_array(-Array)~2n', []),
        format('% initializes Array as an empty array.~2n', []),
	format('new_array(array(', []),
	print_dontcare_array(N),
	format(',~w)).~2n', B),
	/* is_array */
        format('% = is_array(+Term)~2n', []),
        format('% true if Term represents an array.~2n', []),
	format('is_array(array(_,_)).~2n', []),
	/* aref */
        format('% = aref(+Index, +Array, ?Item)~2n', []),
        format('% true if the value of Array[Index] is Item.~n', []),
        format('% if no item is associated with Index in Array, this predicate fails.~2n', []),
	format('aref(Index, array(Array,Size), Item) :-~n', []),
	format('        Index < 1<<Size,~n', []),
	format('        N1 is Index/\\~w,~n', [Mask]),
	format('        N2 is Index>>~w,~n', [B]),
	format('        sub_array(N1, Array0, Item),~n', []),
	format('        find_item(Size, N2, Array0, Array),~n', []),
	format('        nonvar(Item).~2n', []),
	/* arefa */
        format('% = arefa(+Index, +Array, ?Item)~2n', []),
        format('% true if the value of Array[Index] is Item.~n', []),
        format('% if no item is associated with Index in Array, this predicate returns~n', []),
        format('% a new array.~2n', []),
	format('arefa(Index, array(Array,Size), Item) :-~n', []),
	format('        Index < 1<<Size,~n', []),
	format('        N1 is Index/\\~w,~n', [Mask]),
	format('        N2 is Index>>~w,~n', [B]),
	format('        sub_array(N1, Array0, Item0),~n', []),
	format('        find_item(Size, N2, Array0, Array),~n', []),
	format('    (~n', []),
	format('        var(Item0)~n', []),
	format('   ->~n', []),
	format('        new_array(Item)~n', []),
	format('   ;~n', []),
	format('        Item = Item0~n', []),
	format('   ).~2n', []),
	/* arefl */
        format('% = arefa(+Index, +Array, ?Item)~2n', []),
        format('% true if the value of Array[Index] is Item.~n', []),
        format('% if no item is associated with Index in Array, this predicate returns~n', []),
        format('% the empty list.~2n', []),
	format('arefl(Index, array(Array,Size), Item) :-~n', []),
	format('        Index < 1<<Size,~n', []),
	format('        N1 is Index/\\~w,~n', [Mask]),
	format('        N2 is Index>>~w,~n', [B]),
	format('        sub_array(N1, Array0, Item0),~n', []),
	format('        find_item(Size, N2, Array0, Array),~n', []),
	format('    (~n', []),
	format('        var(Item0)~n', []),
	format('   ->~n', []),
	format('        Item = []~n', []),
	format('   ;~n', []),
	format('        Item = Item0~n', []),
	format('   ).~2n', []),
        format('% = find_item(+Index, +Size, ?Array, ?Value)~2n', []),
        format('% constructs a dummy array Array bottom-up with Value marked, so~n', []),
        format('% it can unify against the array in which we are searching.~2n', []),
	format('find_item(S0, N, A0, A) :-~n', []),
	format('    (~n', []),
	format('        S0 =:= ~w~n', [B]),
	format('    ->~n', []),
	format('        A0 = A~n', []),
	format('    ;~n', []),
	format('        S is S0-~w,~n', [B]),
	format('        N1 is N/\\~w,~n', [Mask]),
	format('        N2 is N>>~w,~n', [B]),
	format('        sub_array(N1, A1, A0),~n', []), 
	format('        find_item(S, N2, A1, A)~n', []),
	format('   ).~2n', []),
	print_subarray(N),
	nl,
	format('% = aset(+Index, +Array, +Item, -NewArray)~2n', []),
        format('% true if NewArray is Array, with the exception that NewArray[Index]~n', []),
        format('% is set to Item.~2n', []),
	format('aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-~n', []),
	format('        enlarge_array(Index, Size0, Array0, Size, Array1),~n', []),
	format('        N1 is Index/\\~w,~n', [Mask]),
	format('        N2 is Index>>~w,~n', [B]),
	format('        sub_array(N1, Array2, NA0, _, Item),~n', []),
	format('        replace_item(Size, N2, Array2, Array1, NA0, Array).~2n', []),
        format('% = arepl(+Index, +Array, -NewArray, -OldValue, +NewValue)~2n', []),
        format('% true if Array and NewArray differ only in the Value assigned~n', []),
        format('% to Index.~2n', []),
	format('arepl(Index, array(Array0,Size), array(Array,Size), Item0, Item) :-~n', []),
	format('        Index < 1<<Size,~n', []),
	format('        N1 is Index/\\~w,~n', [Mask]),
	format('        N2 is Index>>~w,~n', [B]),
	format('        sub_array(N1, Array1, NewArray1, Item0, Item),~n', []),
	format('        replace_item(Size, N2, Array1, Array0, NewArray1, Array).~2n', []),
	format('replace_item(S0, N, A0, A, NA0, NA) :-~n', []),
	format('    (~n', []),
	format('        S0 =:= ~w~n', [B]),
	format('    ->~n', []),
	format('        A0 = A,~n', []),
	format('        NA0 = NA~n', []),
	format('    ;~n', []),
	format('        S is S0-~w,~n', [B]),
	format('        N1 is N/\\~w,~n', [Mask]),
	format('        N2 is N>>~w,~n', [B]),
	format('        sub_array(N1, A1, NA1, A0, NA0),~n', []),
	format('        replace_item(S, N2, A1, A, NA1, NA)~n', []),
	format('    ).~2n', []),
	print_subarray2(N),
	nl,
	format('% = enlarge_array(+Index, +Size0, +Array0, -Size, -Array)~2n', []),
        format('% enlarges Array0 of Size to Array of Size such that Array is large enough~n% for Index.~2n', []),
	format('enlarge_array(I, Size0, Array0, Size, Array) :-~n', []),
	format('    (~n', []),
	format('        I < 1<<Size0~n', []),
	format('    ->~n', []),
	format('        Array = Array0,~n', []),
	format('        Size = Size0~n', []),
	format('    ;~n', []),
	format('        Size1 is Size0 + ~w,~n', [B]),
	format('        enlarge_array(I, Size1, $(Array0', []),
	print_dontcare_array(1, N),
	format(', Size, Array)~n', []),
	format('    ).~2n', []),
	format('% = array_to_list(+Array, -List)~2n', []),
        format('% true if List contains all Index-Item pairs of Array in increasing order.~2n', []),
	format('array_to_list(array(', []),
	print_a_array(N),
	format(',Size), L0) :-~n', []),
	format('        N is Size-~w,~n', [B]),
	print_sa_to_list(0, N, '0', [], 'N'),
	format('.~2n', []),
	format('subarray_to_list(K, N0, M0, Item, L0, L) :-~n', []),
	format('    (~n', []),
	format('        var(Item)~n', []),
	format('    ->~n', []),
	format('        L = L0~n', []),
	format('    ;~n', []),
	format('        N0 = 0~n', []),
	format('    ->~n', []),
	format('        N is K+M0,~n', []),
	format('        L0 = [N-Item|L]~n', []),
	format('    ;~n', []),
	format('        Item = ', []),
	print_a_array(N),
	format(',~n', []),
	format('        N is N0-~w,~n', [B]),
	format('        M is (K+M0) << ~w,~n', [B]),
	print_sa_to_list(0, N, 'M', 'L', 'N'),
	format('~n    ).~2n', []),
	format('% = list_to_array(+List, +Array)~2n', []),
        format('% true if List is a list of Index-Item pairs and Array is the~n% corresponding array.~2n', []),
	format('list_to_array(List, Array) :-~n', []),
	format('        new_array(Array0),~n', []),
	format('        list_to_array(List, Array0, Array).~2n'),
	format('list_to_array([], Array, Array).~n', []),
	format('list_to_array([Index-Element|Rest], Array0, Array) :-~n', []),
	format('        aset(Index, Array0, Element, Array1),~n', []),
	format('        list_to_array(Rest, Array1, Array).~2n', []),
	format('% = map_array(+Array0, :Pred, -Array)~2n', []),
        format('% true if Array0 and Array have the same Indices and Pred(Item0,Item)~n% holds for every Item0 of Array0 and Item of Array.~2n', []), 
	format('map_array(array(Array0, Sz), Pred, array(Array, Sz)) :-~n', []),
	format('        map_array1(Sz, Array0, Pred, Array).~2n', []),
	format('map_array1(S0, ', []),
	print_a_array(N),
	format(', Pred, ', []),
	print_b_array(N),
	format(') :-~n', []),
	format('    (~n', []), 
	format('        S0 =:= ~w~n', [B]),
	format('    ->~n', []),
	print_mcs(0, N),
	format('    ;~n', []),
	format('        S is S0 - ~w,~n', [B]),
	print_mas(0, N),
	format('    ).~2n'),
	format('map_call(Pred, Item0, Item) :-~n', []),
	format('   (~n', []),
	format('        var(Item0)~n', []),
	format('   ->~n', []),
	format('        Item = Item0~n', []),
	format('    ;~n', []),
	format('        call(Pred, Item0, Item)~n', []),
	format('    ).~2n', []),
	format('map_array2(S, Array0, Pred, Array) :-~n', []),
	format('    (~n', []),
	format('        var(Array0)~n', []),
	format('    ->~n', []),
	format('        Array = Array0~n', []),
	format('    ;~n', []),
	format('        map_array1(S, Array0, Pred, Array)~n', []),
	format('    ).~2n', []),
	told.

print_mcs(N0, N) :-
	N1 is N0+1,
    (
	N1 = N
    ->
	format('        map_call(Pred, A~w, B~w)~n', [N0, N0])
    ;
	format('        map_call(Pred, A~w, B~w),~n', [N0, N0]),
        print_mcs(N1, N)
    ).

print_mas(N0, N) :-
	N1 is N0+1,
    (
	N1 = N
    ->
	format('        map_array2(S, A~w, Pred, B~w)~n', [N0, N0])
    ;
	format('        map_array2(S, A~w, Pred, B~w),~n', [N0, N0]),
        print_mas(N1, N)
    ).

print_sa_to_list(N0, N, X, Y, Z) :-
	N1 is N0+1,
    (
	N1 = N
    ->
	format('        subarray_to_list(~w, ~w, ~w, A~w, L~w, ~w)', [N0, Z, X, N0, N0, Y])
    ;
	format('        subarray_to_list(~w, ~w, ~w, A~w, L~w, L~w),~n', [N0, Z, X, N0, N0,N1]),
	print_sa_to_list(N1, N, X, Y, Z)
    ).

print_dontcare_array(N) :-
	format('$(_', []),
	print_dontcare_array(1, N).
print_dontcare_array(N0, N) :-
    (
	N0 < N
    ->
	format(',_', []),
	N1 is N0+1,
	print_dontcare_array(N1, N)
    ;
	format(')', [])
    ).

print_a_array(N) :-
	format('$(A0', []),
	print_a_array(1, N, 26).
print_a_array(N0, N, A0) :-
    (
	N0 < N
    ->
	format(',~p', ['$VAR'(A0)]),
	A is A0+26,
	N1 is N0+1,
	print_a_array(N1, N, A)
    ;
	format(')', [])
    ).

print_b_array(N) :-
	format('$(B0', []),
	print_b_array(1, N, 27).
print_b_array(N0, N, A0) :-
    (
	N0 < N
    ->
	format(',~p', ['$VAR'(A0)]),
	A is A0+26,
	N1 is N0+1,
	print_b_array(N1, N, A)
    ;
	format(')', [])
    ).


print_subarray(N) :-
	print_subarray(N, 0).

print_subarray(N, M0) :-
    (
 	M0 < N
    ->
	M1 is M0+1,
	print_subarray_clause(N, M0),
	print_subarray(N, M1)
    ;
	true
    ).

print_subarray_clause(N, M) :-
	M < N,
    (
	M = 0
    ->
	format('sub_array(~w, $(A', [M])
    ;
	format('sub_array(~w, $(_', [M])
    ),
	print_subarray_clause1(1, N, M).

print_subarray_clause1(N0, N, M) :-
	N1 is N0+1,
    (
	N0 >= N
    ->
	format('), A).~n', [])
    ;
	(
	    N0 = M
	->
	    format(',A', [])
	;
	    format(',_', [])
	),
	print_subarray_clause1(N1, N, M)
    ).

print_subarray2(N) :-
	print_subarray2(N, 0).

print_subarray2(N, M0) :-
    (
 	M0 < N
    ->
	M1 is M0+1,
	print_subarray2_clause(N, M0),
	print_subarray2(N, M1)
    ;
	true
    ).

print_subarray2_clause(N, M) :-
	M < N,
	format('sub_array(~w, $(~p', [M,'$VAR'(0)]),
	print_subarray2_clause1(1, N, M).


print_subarray2_clause1(N0, N, M) :-
	N1 is N0+1,
    (
	N0 >= N
    ->
	print_subarray2_clause0(N, M)
    ;
	format(',~p', ['$VAR'(N0)]),
	print_subarray2_clause1(N1, N, M)
    ).


print_subarray2_clause0(N, M) :-
    (
	M = 0
    ->
	format('), $(~p', ['$VAR'(N)])
    ;
	format('), $(~p', ['$VAR'(0)])
    ),
	print_subarray2_clause2(1, N, M).


print_subarray2_clause2(N0, N, M) :-
	N1 is N0+1,
    (
	N0 >= N
    ->
	format('), ~p, ~p).~n', ['$VAR'(M), '$VAR'(N)])
    ;
	(
	    N0 = M
	->
	    format(',~p', ['$VAR'(N)])
	;
	    format(',~p', ['$VAR'(N0)])
	),
	print_subarray2_clause2(N1, N, M)
    ).
