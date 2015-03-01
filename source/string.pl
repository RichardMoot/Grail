:- module(string_utils, [
		atom_to_lower/2,
		atom_to_upper/2,
		atom_capitalize/2,
		to_lower/2,
		to_upper/2,
		capitalize/2,
		suffix/2,
		prefix/2,
		atom_suffix/2,
		atom_prefix/2]).

suffix(Suffix, String) :-
	append(_, Suffix, String).

prefix([], _).
prefix([A|As], [A|Bs]) :-
	prefix(As, Bs).

atom_to_lower(Atom, Lower) :-
	name(Atom, List0),
	to_lower(List0, List),
	name(Lower, List).

atom_to_upper(Atom, Lower) :-
	name(Atom, List0),
	to_upper(List0, List),
	name(Lower, List).

atom_capitalize(Atom, Lower) :-
	name(Atom, List0),
	capitalize(List0, List),
	name(Lower, List).


to_lower([], []).
to_lower([Char0|Chars0], [Char|Chars]) :-
	char_type(Char, to_lower(Char0)),
	to_lower(Chars0, Chars).

to_upper([], []).
to_upper([Char0|Chars0], [Char|Chars]) :-
	char_type(Char, to_upper(Char0)),
	to_upper(Chars0, Chars).

capitalize([], []).
capitalize([Char0|Chars0], [Char|Chars]) :-
	char_type(Char, to_upper(Char0)),
	to_lower(Chars0, Chars).
