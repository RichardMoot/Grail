% -*- Mode: Prolog -*-
% Array Hack - Library for constant time access arrays in Prolog
% Copyright (C) 2003 Richard Moot (Richard.Moot@labri.fr)

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

% ===================================
% =          array_hack.pl          =
% ===================================

% This is a rather ugly hack for people who *really* need to have
% constant time read/write access. It uses the non-logical Prolog
% constructs arg/3 and argset/3. Be warned that argset/3 is not
% available in most Prologs and that your Prolog may have
% limitations on the arity of Prolog terms, which would translate
% into a limitation on array size.

% Arrays are represented as '$array'(Size, Items) where Size is the
% number of items in the array. Items is of the form '$arrayitems'/C
% of arity C, which is the maximum capacity of the array. Whenever
% the Size would exceed the capacity, the capacity of the array is
% enlarged to the smallest power of 2 which is larger than Size and
% the old items are copied to this new array.

% NB Because the arrays are implemented using arg/3 and argset/3,
%    the first item in the array is 1, not 0.


% 20030813 First version.
% 20030814 Added a predicate array_map_values/3 which calls a
%          specified function on all elements of the array and
%          returns the results in a new array.

:- module(array_hack, [
  	new_array/1,
	new_array/2,
	is_array/1,
	aref/3,
	arepl/5,
	aset/4,
	array_to_list/2,
	list_to_array/2,
	map_array/3
    ]).

% = new_array_size(+Size)
%
% Internal predicate specifying the size of an array generated
% by new_array/1.

new_array_size(1).

% = new_array(-Array)
%
% produces an Array of the default size, where all array values
% are Prolog variables.

new_array('$array'(Sz,Array)) :-
	new_array_size(Sz),
	Cp is 2^ceil(log(Sz)/log(2)),
	functor(Array, '$arrayitems', Cp).

% = new_array(+Size, -Array)
%
% produces an Array of Size, where all array values are Prolog
% variables.

new_array(Sz, '$array'(Sz,Array)) :-
	Cp is 2^ceil(log(Sz)/log(2)),
	functor(Array, '$arrayitems', Cp).

% = is_array(+Array)
%
% true if Array is an array in our representation.

is_array('$array'(Sz,Items)) :-
	functor(Items, '$arrayitems', Cp),
	Cp is 2^ceil(log(Sz)/log(2)).

% = aref(+Key, +Array, ?Value)
%
% true if item Key in Array is associated with Value. Fails if Key is
% zero or out of bounds.

aref(Item, '$array'(_,ArrayItems), Value) :-
	arg(Item, ArrayItems, Value).

arepl(Item, '$array'(Sz,ArrayItems), '$array'(Sz,ArrayItems), Value0, Value) :-
	arg(Item, ArrayItems, Value0),
	setarg(Item, ArrayItems, Value).

% = aset(+Key, !?Array, ?Value, +NewArray)
%
% true if NewArray is the same array as Array, except that Key
% is now associated with Value. If necessary, NewArray is
% enlarged to the required size to accommodate the new Key
% and the items from Array are copied to NewArray.

% Be aware that unless this copy operation takes place, Array
% will be modified as well, as a side-effect to setarg/3.

% Note that this predicate will fail for Key =:= 0.

aset(Item, '$array'(Sz0,ArrayItems0), Value, '$array'(Sz,ArrayItems)) :-
    (
	Item > Sz0
    ->
	Sz = Item,
	enlarge_array(ArrayItems0, Item, ArrayItems)
    ;
	Sz = Sz0,
	ArrayItems = ArrayItems0
    ),
	setarg(Item, ArrayItems, Value).

enlarge_array(AI0, Item, AI) :-
	Cp is 2^ceil(log(Item)/log(2)),
	functor(AI0, _, Cp0),
    (
	Cp > Cp0
    ->
	functor(AI, '$arrayitems', Cp),
	copy_arrayitems(1, Cp0, AI0, AI)
    ;
	AI = AI0
    ).

% = copy_arrayitems(+First, +Last, +ArrayItems, -ArrayItemCopy)
%
% copies the elemenst from First to Last of Array to ArrayCopy.
% Depends on the caller to ensure the 

copy_arrayitems(N0, N, AI0, AI) :-
	arg(N0, AI0, Arg),
	arg(N0, AI, Arg),
    (
	N0 <  N
    ->
	N1 is N0+1,
	copy_arrayitems(N1, N, AI0, AI)
    ;
	true
    ).

% = array_to_list(+Array, -List)
%
% true if List contains all values of Array.

array_to_list('$array'(Sz,Items), List) :-
	array_to_list(1, Sz, Items, List).

array_to_list(N0, N, Items, [I|List0]) :-
	arg(N0, Items, I),
    (
	N0 < N
    ->
	N1 is N0+1,
	array_to_list(N1, N, Items, List0)
    ;
	List0 = []
    ).

% = list_to_array(+List, -Array)
%
% true if Array contains the values of List.

list_to_array(List, '$array'(Sz,Items)) :-
	length(List, Sz),
	Cp is 2^ceil(log(Sz)/log(2)),
	functor(Items, '$arrayitems', Cp),
	list_to_array1(List, 1, Items).

list_to_array1([], _, _).
list_to_array1([X|Xs], N0, Items) :-
	arg(N0, Items, X),
	N1 is N0+1,
	list_to_array1(Xs, N1, Items).

% = map_array(+Array, :Pred, ?NewArray)
%
% true if Pred(V1,V2) holds for all V1 in Array and all corresponding
% V2 is NewArray.

map_array('$array'(Sz,Items0), Pred, '$array'(Sz, Items)) :-
	functor(Items0, F, A),
	functor(Items, F, A),
	map_array1(1, Sz, Items0, Pred, Items).

map_array1(N0, N, AI0, Pred, AI) :-
	arg(N0, AI0, A0),
	user:call(Pred, A0, A),
	arg(N0, AI, A),
    (
	N0 < N
    ->
	N1 is N0+1,
	map_array1(N1, N, AI0, Pred, AI)
    ;
	true
    ).
