% -*- Mode: Prolog -*-
% Log 16 Array Library
% Copyright (C) 2003 Richard Moot (Richard.Moot@labri.fr)
% Based on original code by David H. D. Warren and Fernando Pereira.

% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.

% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

% ====================================================================

% This file is very heavily inspired by the file logarr.pl from the
% Public Domain DEC 10 Prolog Library, which was programmed by
% David H. D. Warren and Fernando Pereira.

% There should be little difference between the code here and the
% original for client programs, with the exception that undefined
% items are Prolog variables instead of the constant $.

% The main reason for coding this was that I liked to have a tree
% implementation of arrays where the item position was constructed
% bottom-up and the item obtained by unifying the constructed skeleton
% with the full aray. For the log4 arrays this appears to use slightly
% more logical inferences than the original code. This choice also
% forced the use of Prolog variables as `undefined' value, as it is
% possible for the aset/4 operation to extend an empty branch in the
% tree and we do not want unification to fail in this case.

% The current file uses log16 arrays, where an array has up to 16
% subarrays.

% As in the original Warren/Pereira code, an array is representad as
% array(Tree, Size) where Tree is an array with maximum capacity
% 2^Size - 1. Since we use log16 arrays, only multiples of 4 are valid
% values of Size.

:- module(log16arr, [
        new_array/1,
        is_array/1,
        aref/3,
        arepl/5,
        arefa/3,
        arefl/3,
        aset/4,
        map_array/3,
        array_to_list/2,
        list_to_array/2
    ]).

% = new_array(-Array)

% initializes Array as an empty array.

new_array(array($(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),4)).

% = is_array(+Term)

% true if Term represents an array.

is_array(array(_,_)).

% = aref(+Index, +Array, ?Item)

% true if the value of Array[Index] is Item.
% if no item is associated with Index in Array, this predicate fails.

aref(Index, array(Array,Size), Item) :-
        Index < 1<<Size,
        N1 is Index/\15,
        N2 is Index>>4,
        sub_array(N1, Array0, Item),
        find_item(Size, N2, Array0, Array),
        nonvar(Item).

% = arefa(+Index, +Array, ?Item)

% true if the value of Array[Index] is Item.
% if no item is associated with Index in Array, this predicate returns
% a new array.

arefa(Index, array(Array,Size), Item) :-
        Index < 1<<Size,
        N1 is Index/\15,
        N2 is Index>>4,
        sub_array(N1, Array0, Item0),
        find_item(Size, N2, Array0, Array),
    (
        var(Item0)
   ->
        new_array(Item)
   ;
        Item = Item0
   ).

% = arefa(+Index, +Array, ?Item)

% true if the value of Array[Index] is Item.
% if no item is associated with Index in Array, this predicate returns
% the empty list.

arefl(Index, array(Array,Size), Item) :-
        Index < 1<<Size,
        N1 is Index/\15,
        N2 is Index>>4,
        sub_array(N1, Array0, Item0),
        find_item(Size, N2, Array0, Array),
    (
        var(Item0)
   ->
        Item = []
   ;
        Item = Item0
   ).

% = find_item(+Index, +Size, ?Array, ?Value)

% constructs a dummy array Array bottom-up with Value marked, so
% it can unify against the array in which we are searching.

find_item(S0, N, A0, A) :-
    (
        S0 =:= 4
    ->
        A0 = A
    ;
        S is S0-4,
        N1 is N/\15,
        N2 is N>>4,
        sub_array(N1, A1, A0),
        find_item(S, N2, A1, A)
   ).

sub_array(0, $(A,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), A).
sub_array(1, $(_,A,_,_,_,_,_,_,_,_,_,_,_,_,_,_), A).
sub_array(2, $(_,_,A,_,_,_,_,_,_,_,_,_,_,_,_,_), A).
sub_array(3, $(_,_,_,A,_,_,_,_,_,_,_,_,_,_,_,_), A).
sub_array(4, $(_,_,_,_,A,_,_,_,_,_,_,_,_,_,_,_), A).
sub_array(5, $(_,_,_,_,_,A,_,_,_,_,_,_,_,_,_,_), A).
sub_array(6, $(_,_,_,_,_,_,A,_,_,_,_,_,_,_,_,_), A).
sub_array(7, $(_,_,_,_,_,_,_,A,_,_,_,_,_,_,_,_), A).
sub_array(8, $(_,_,_,_,_,_,_,_,A,_,_,_,_,_,_,_), A).
sub_array(9, $(_,_,_,_,_,_,_,_,_,A,_,_,_,_,_,_), A).
sub_array(10, $(_,_,_,_,_,_,_,_,_,_,A,_,_,_,_,_), A).
sub_array(11, $(_,_,_,_,_,_,_,_,_,_,_,A,_,_,_,_), A).
sub_array(12, $(_,_,_,_,_,_,_,_,_,_,_,_,A,_,_,_), A).
sub_array(13, $(_,_,_,_,_,_,_,_,_,_,_,_,_,A,_,_), A).
sub_array(14, $(_,_,_,_,_,_,_,_,_,_,_,_,_,_,A,_), A).
sub_array(15, $(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,A), A).

% = aset(+Index, +Array, +Item, -NewArray)

% true if NewArray is Array, with the exception that NewArray[Index]
% is set to Item.

aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-
        enlarge_array(Index, Size0, Array0, Size, Array1),
        N1 is Index/\15,
        N2 is Index>>4,
        sub_array(N1, Array2, NA0, _, Item),
        replace_item(Size, N2, Array2, Array1, NA0, Array).

% = arepl(+Index, +Array, -NewArray, -OldValue, +NewValue)

% true if Array and NewArray differ only in the Value assigned
% to Index.

arepl(Index, array(Array0,Size), array(Array,Size), Item0, Item) :-
        Index < 1<<Size,
        N1 is Index/\15,
        N2 is Index>>4,
        sub_array(N1, Array1, NewArray1, Item0, Item),
        replace_item(Size, N2, Array1, Array0, NewArray1, Array).

replace_item(S0, N, A0, A, NA0, NA) :-
    (
        S0 =:= 4
    ->
        A0 = A,
        NA0 = NA
    ;
        S is S0-4,
        N1 is N/\15,
        N2 is N>>4,
        sub_array(N1, A1, NA1, A0, NA0),
        replace_item(S, N2, A1, A, NA1, NA)
    ).

sub_array(0, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(Q,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), A, Q).
sub_array(1, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,Q,C,D,E,F,G,H,I,J,K,L,M,N,O,P), B, Q).
sub_array(2, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,Q,D,E,F,G,H,I,J,K,L,M,N,O,P), C, Q).
sub_array(3, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,Q,E,F,G,H,I,J,K,L,M,N,O,P), D, Q).
sub_array(4, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,Q,F,G,H,I,J,K,L,M,N,O,P), E, Q).
sub_array(5, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,Q,G,H,I,J,K,L,M,N,O,P), F, Q).
sub_array(6, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,Q,H,I,J,K,L,M,N,O,P), G, Q).
sub_array(7, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,Q,I,J,K,L,M,N,O,P), H, Q).
sub_array(8, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,Q,J,K,L,M,N,O,P), I, Q).
sub_array(9, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,Q,K,L,M,N,O,P), J, Q).
sub_array(10, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,Q,L,M,N,O,P), K, Q).
sub_array(11, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,K,Q,M,N,O,P), L, Q).
sub_array(12, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,K,L,Q,N,O,P), M, Q).
sub_array(13, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,K,L,M,Q,O,P), N, Q).
sub_array(14, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,P), O, Q).
sub_array(15, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q), P, Q).

% = enlarge_array(+Index, +Size0, +Array0, -Size, -Array)

% enlarges Array0 of Size to Array of Size such that Array is large enough
% for Index.

enlarge_array(I, Size0, Array0, Size, Array) :-
    (
        I < 1<<Size0
    ->
        Array = Array0,
        Size = Size0
    ;
        Size1 is Size0 + 4,
        enlarge_array(I, Size1, $(Array0,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), Size, Array)
    ).

% = array_to_list(+Array, -List)

% true if List contains all Index-Item pairs of Array in increasing order.

array_to_list(array($(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),Size), L0) :-
        N is Size-4,
        subarray_to_list(0, N, 0, A0, L0, L1),
        subarray_to_list(1, N, 0, A1, L1, L2),
        subarray_to_list(2, N, 0, A2, L2, L3),
        subarray_to_list(3, N, 0, A3, L3, L4),
        subarray_to_list(4, N, 0, A4, L4, L5),
        subarray_to_list(5, N, 0, A5, L5, L6),
        subarray_to_list(6, N, 0, A6, L6, L7),
        subarray_to_list(7, N, 0, A7, L7, L8),
        subarray_to_list(8, N, 0, A8, L8, L9),
        subarray_to_list(9, N, 0, A9, L9, L10),
        subarray_to_list(10, N, 0, A10, L10, L11),
        subarray_to_list(11, N, 0, A11, L11, L12),
        subarray_to_list(12, N, 0, A12, L12, L13),
        subarray_to_list(13, N, 0, A13, L13, L14),
        subarray_to_list(14, N, 0, A14, L14, L15),
        subarray_to_list(15, N, 0, A15, L15, []).

subarray_to_list(K, N0, M0, Item, L0, L) :-
    (
        var(Item)
    ->
        L = L0
    ;
        N0 = 0
    ->
        N is K+M0,
        L0 = [N-Item|L]
    ;
        Item = $(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),
        N is N0-4,
        M is (K+M0) << 4,
        subarray_to_list(0, N, M, A0, L0, L1),
        subarray_to_list(1, N, M, A1, L1, L2),
        subarray_to_list(2, N, M, A2, L2, L3),
        subarray_to_list(3, N, M, A3, L3, L4),
        subarray_to_list(4, N, M, A4, L4, L5),
        subarray_to_list(5, N, M, A5, L5, L6),
        subarray_to_list(6, N, M, A6, L6, L7),
        subarray_to_list(7, N, M, A7, L7, L8),
        subarray_to_list(8, N, M, A8, L8, L9),
        subarray_to_list(9, N, M, A9, L9, L10),
        subarray_to_list(10, N, M, A10, L10, L11),
        subarray_to_list(11, N, M, A11, L11, L12),
        subarray_to_list(12, N, M, A12, L12, L13),
        subarray_to_list(13, N, M, A13, L13, L14),
        subarray_to_list(14, N, M, A14, L14, L15),
        subarray_to_list(15, N, M, A15, L15, L)
    ).

% = list_to_array(+List, +Array)

% true if List is a list of Index-Item pairs and Array is the
% corresponding array.

list_to_array(List, Array) :-
        new_array(Array0),
        list_to_array(List, Array0, Array).

list_to_array([], Array, Array).
list_to_array([Index-Element|Rest], Array0, Array) :-
        aset(Index, Array0, Element, Array1),
        list_to_array(Rest, Array1, Array).

% = map_array(+Array0, :Pred, -Array)

% true if Array0 and Array have the same Indices and Pred(Item0,Item)
% holds for every Item0 of Array0 and Item of Array.

map_array(array(Array0, Sz), Pred, array(Array, Sz)) :-
        map_array1(Sz, Array0, Pred, Array).

map_array1(S0, $(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), Pred, $(B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15)) :-
    (
        S0 =:= 4
    ->
        map_call(Pred, A0, B0),
        map_call(Pred, A1, B1),
        map_call(Pred, A2, B2),
        map_call(Pred, A3, B3),
        map_call(Pred, A4, B4),
        map_call(Pred, A5, B5),
        map_call(Pred, A6, B6),
        map_call(Pred, A7, B7),
        map_call(Pred, A8, B8),
        map_call(Pred, A9, B9),
        map_call(Pred, A10, B10),
        map_call(Pred, A11, B11),
        map_call(Pred, A12, B12),
        map_call(Pred, A13, B13),
        map_call(Pred, A14, B14),
        map_call(Pred, A15, B15)
    ;
        S is S0 - 4,
        map_array2(S, A0, Pred, B0),
        map_array2(S, A1, Pred, B1),
        map_array2(S, A2, Pred, B2),
        map_array2(S, A3, Pred, B3),
        map_array2(S, A4, Pred, B4),
        map_array2(S, A5, Pred, B5),
        map_array2(S, A6, Pred, B6),
        map_array2(S, A7, Pred, B7),
        map_array2(S, A8, Pred, B8),
        map_array2(S, A9, Pred, B9),
        map_array2(S, A10, Pred, B10),
        map_array2(S, A11, Pred, B11),
        map_array2(S, A12, Pred, B12),
        map_array2(S, A13, Pred, B13),
        map_array2(S, A14, Pred, B14),
        map_array2(S, A15, Pred, B15)
    ).

map_call(Pred, Item0, Item) :-
   (
        var(Item0)
   ->
        Item = Item0
    ;
        call(Pred, Item0, Item)
    ).

map_array2(S, Array0, Pred, Array) :-
    (
        var(Array0)
    ->
        Array = Array0
    ;
        map_array1(S, Array0, Pred, Array)
    ).

