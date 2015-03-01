% -*- Mode: Prolog -*-
% Disjoint Set Library
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

% Implementation of the disjoint set functions from Chapter 22
% of Cormen, Leiserson and Rivest's "Introduction to Algorithms".

% In this version, it is essential for the link operation to produce a
% link from the first to the second item, so union by rank is
% impossible. The path compression heuristic is applied, however.

% Makes use of the logarithmic array library.

:- module(disjset,     [ds_init/1,
	                ds_new/3,
	                ds_find/4,
			ds_add/4,
			ds_link/4,
	                ds_union/4]).

:- use_module(log16arr,  [new_array/1,
                          aref/3,
	 		  arepl/5,
	                  aset/4]).

% = ds_init(-DS)
%
% true if DS is a new disjoint set data structure.
% initializes array P.

ds_init(P) :-
	new_array(P).

% = ds_new(+DSA, +Item, ?DSB)
%
% true if DSB is the same as DSA with Item added as a new set
% containing only itself.

ds_new(P0, X, P) :-
	aset(X, P0, X, P).

% = ds_add(+DSA, +Item, +Class, ?DSB)
%
% true if DSB represents the disjoint sets of DSA with Item
% added to Class.

ds_add(P0, X, Y, P) :-
	aset(X, P0, Y, P).

% = ds_link(+DSA, +Class1, +Class2, ?DSB)
%
% true if DSB is DSA, with the addition of a link from Class1
% to Class2

ds_link(P0, X, Y, P) :-
	aset(X, P0, Y, P).

% = ds_find(+DSA, +Item, ?Class, -DSB)
%
% true if Class is the representative of Item in DSA and DSB is the
% path compressed version of DSA, where every item on the path to
% Class will now have a direct link to Class.

ds_find(P0, X, Y, P) :-
	arepl(X, P0, P1, PX, Y),
    (
	X =:= PX
    ->
	Y = PX,
	P1 = P
    ;
	ds_find(P1, PX, Y, P)
    ).

% = ds_union(+DSA, +Item1, +Item2, -DSB)
%
% true if DSB is DSA where the sets to which Item1 and Item2 belong
% have been joined.

ds_union(P0, X, Y, P) :-
	ds_find(P0, X, CX, P1),
	ds_find(P1, Y, CY, P2),
	ds_link(P2, CX, CY, P).

% Disjoint set example from page 442 of Cormen ea.

%cormen_example(DS) :-
%	ds_init(DS0),
%	ds_new(DS0, 1, DS1),
%	ds_new(DS1, 2, DS2),
%	ds_new(DS2, 3, DS3),
%	ds_new(DS3, 4, DS4),
%	ds_new(DS4, 5, DS5),
%	ds_new(DS5, 6, DS6),
%	ds_new(DS6, 7, DS7),
%	ds_new(DS7, 8, DS8),
%	ds_new(DS8, 9, DS9),
%	ds_new(DS9, 10, DS10),
%	ds_union(DS10, 2, 4, DS11),
%	ds_union(DS11, 5, 7, DS12),
%	ds_union(DS12, 1, 3, DS13),
%	ds_union(DS13, 8, 9, DS14),
%	ds_union(DS14, 1, 2, DS15),
%	ds_union(DS15, 5, 6, DS16),
%	ds_union(DS16, 2, 3, DS).
