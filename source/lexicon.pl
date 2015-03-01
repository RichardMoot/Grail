% -*- Mode: Prolog -*-

:- module(lexicon, [
         lookup/4,
	 cmd_lookup/5,
	 cmd_lookup_sem/5,
	 cmd_lookup_sem/7,
         get_item_semantics/3,
         get_item_semantics/5,
	 macro_expand/2,
	 macro_reduce/2,
	 is_formula/1]).

:- use_module(grail_dot,   [show_lookup/3]).

:- dynamic(user:lex/3).
:- dynamic(user:macro/2).
:- dynamic(user:sequence_semantics/10).

lookup(Words, Formulas, Goal, ExpandedGoal) :-
	macro_expand(Goal, ExpandedGoal),
	global_lookup(Words, GFormulas),
	show_lookup(Words, GFormulas, ExpandedGoal),
	% save the lexical lookup in a file "lookup.dot" for
	% inspection by the user
	shell('cp -f tmp.dot lookup.dot'),
	layout_lookup,
	enumerate(GFormulas, Formulas).

enumerate([], []).
enumerate([F|Fs], [F0|Ls]) :-
	member(F0, F),
	enumerate(Fs, Ls).

% = global_lookup(+Words, -Formulas, +Goal, -ExpandedGoal)
%
% true if Words are assigned Formulas in the lexicon, after macro
% expansion and Goal is equal to ExpandedGoal after macro expansion.

global_lookup(Words, Formulas) :-
	find_missing_words(Words),
	global_lookup1(Words, Formulas, []).

global_lookup1([], Fs, Fs).
global_lookup1([W|Ws], [Lookups|Fs0], Fs) :-
	findall(F-S, (lex(W,F0,S), macro_expand(F0,F)), Lookups),
	global_lookup1(Ws, Fs0, Fs).

%

cmd_lookup(Words, FAtoms, Formulas, Goal, ExpandedGoal) :-
	macro_expand(Goal, ExpandedGoal),
	parse_forms(FAtoms, GFormulas),
	show_lookup(Words, GFormulas, ExpandedGoal),
	shell('cp -f tmp.dot lookup.dot'),
	layout_lookup,
	enumerate(GFormulas, Formulas).

%

cmd_lookup_sem(Words, FAtoms, Formulas, Goal, ExpandedGoal) :-
	macro_expand(Goal, ExpandedGoal),
	parse_forms(FAtoms, GFormulas),
	add_fake_semantics(GFormulas, GFormulasS),
	show_lookup(Words, GFormulasS, ExpandedGoal),
	shell('cp -f tmp.dot lookup.dot'),
	layout_lookup,
	enumerate(GFormulas, Formulas0),
	add_semantics(Words, Formulas0, Formulas).

%

cmd_lookup_sem(Words, POSs, Lemmas, FAtoms, Formulas, Goal, ExpandedGoal) :-
	macro_expand(Goal, ExpandedGoal),
	parse_forms(FAtoms, GFormulas),
	add_fake_semantics(GFormulas, GFormulasS),
	show_lookup(Words, GFormulasS, ExpandedGoal),
	shell('cp -f tmp.dot lookup.dot'),
	layout_lookup,
	enumerate(GFormulas, Formulas0),
	add_semantics(Words, POSs, Lemmas, Formulas0, Formulas).

parse_forms([], []).
parse_forms([A|As], [F|Fs]) :-
     (
        atom(A)
     ->
	concat_atom(L, '^', A),
	map_atom_to_term(L, F)
     ;
        macro_expand(A, B),
        F = [B]
     ),
	parse_forms(As, Fs).

map_atom_to_term([], []).
map_atom_to_term([A|As], [F|Fs]) :-
	atom_to_term(A, F0, _),
	macro_expand(F0, F),
	map_atom_to_term(As, Fs).

add_semantics([], [], []).
add_semantics([W|Ws0], [F|Fs0], [S|Ss0]) :-
	sequence_semantics([W|Ws0], Ws, _, _, _, _, [F|Fs0], Fs, [S|Ss0], Ss),
	!,
	add_semantics(Ws, Fs, Ss).
add_semantics([W|Ws], [F|Fs], [F-S|Ss]) :-
	get_item_semantics(W, F, S),
	add_semantics(Ws, Fs, Ss).

add_semantics([], [], [], [], []).
add_semantics([W|Ws0], [P|Ps0], [L|Ls0], [F|Fs0], [S|Ss0]) :-
	sequence_semantics([W|Ws0], Ws, [P|Ps0], Ps, [L|Ls0], Ls, [F|Fs0], Fs, [S|Ss0], Ss),
	!,
	add_semantics(Ws, Ps, Ls, Fs, Ss).
add_semantics([W|Ws], [P|Ps], [L|Ls], [F|Fs], [F-S|Ss]) :-
	get_item_semantics(W, P, L, F, S),
	add_semantics(Ws, Ps, Ls, Fs, Ss).

% = get_item_semantics
%
% This predicate does lexical lookup for an automatically extracted
% grammar.

get_item_semantics(Word, Formula, Semantics) :-
        lex(Word, Formula0, Semantics),
	macro_expand(Formula0, Formula),
	!.
get_item_semantics(Word, Formula, Semantics) :-
        default_semantics(Word, Formula, Semantics),
	!.
get_item_semantics(Word, Formula, Word) :-
	format(log, 'MIS_SEM_FORM: ~p~n', [Formula]),
	format(log, 'MIS_SEM_FORM_WORD: ~p ~w~n', [Formula, Word]).


get_item_semantics(Word, _POS, _Lemma, Formula, Semantics) :-
        lex(Word, Formula0, Semantics),
	macro_expand(Formula0, Formula),
	!.
get_item_semantics(_Word, POS, Lemma, Formula, Semantics) :-
        default_semantics(Lemma, POS, Formula, Semantics),
	!.
get_item_semantics(_Word, _POS, Lemma, Formula, Semantics) :-
        default_semantics(Lemma, Formula, Semantics),
	!.
get_item_semantics(Word, _POS, _Lemma, Formula, Semantics) :-
        default_semantics(Word, Formula, Semantics),
	!.
get_item_semantics(Word, POS, Lemma, Formula, Lemma) :-
	format(log, 'MIS_SEM_FORM: ~p~n', [Formula]),
	format(log, 'MIS_SEM_FORM_WORD: ~p ~w~n', [Formula, Word]),
	format(log, 'MIS_SEM_ALL: ~w-~w-~w ~p~n', [Word, POS, Lemma, Formula]).

% = find_missing_words(+ListOfWords)
%
% check if all words in ListOfWords have an assignment in the lexicon.

find_missing_words([]).
find_missing_words([W|Ws]) :-
    (
	lex(W, _, _)
    ->
	true
    ;
	find_lexical_match(W, F, S),
	assert(lex(W,F,S))
    ),
	find_missing_words(Ws).

% = find_lexical_match(+Word, ?Formula, +Semantics)
%
% true if Formula is in the lexicon for a word closely resembling Word,
% using SWI-Prolog's dwim_match/2.

find_lexical_match(Word, Formula, Sem) :-
	setof(LW, LS^A^lex(LW,A,LS), List),
	find_lexical_match(List, [], Word, Formula, Sem).

find_lexical_match([], Rs, Word, Formula, Sem) :-
	ask_replace(Rs, Word, SW),
	lex(SW, F0, Sem),
	macro_expand(F0, Formula).
find_lexical_match([L|Ls], Rs0, Word, Formula, Sem) :-
    (
	dwim_match(L, Word)
    ->
	Rs = [L|Rs0]
    ;
	Rs = Rs0
    ),
	find_lexical_match(Ls, Rs, Word, Formula, Sem).

ask_replace(L, W, Answer) :-
	new(D, dialog('Word not in lexicon')),
	concat_atom(['"', W, '" not in lexicon!\nSelect a replacement or press cancel\nand edit you lexicon.'], Text),
	send(D, append, new(_, text(Text,@default,@default))),
	append_button_list(L, D),
	send(D, append, button(cancel, message(D, return, '* cancel *'))),
	get(D, confirm, Answer),
	send(D, free).

append_button_list([], _).
append_button_list([B|Bs], D) :-
	send(D, append, button(B, message(D, return, B))),
	append_button_list(Bs, D).

% = macro_expand(+Formula, -ExpandedFormula)
%
% true if Formula expands to ExpandedFormula using the macro/2
% definitions and no further macro's apply to ExpandedFormula.
%
% This is basically a just a repeat loop. Note that the macro
% mechanism is very powerful and responsibility of termination and
% confluence are on the user.

macro_expand(S0, S) :-
	translate_formula(S0, S1),
	macro_expand1(S1, S).

translate_formula(S0, S) :-
	user:translate_form(S0, S1),
	!,
	translate_formula(S1, S).
translate_formula(S, S).

macro_expand1(S0, S) :-
     apply_macro(S0, S1),
     !,
     macro_expand1(S1, S).
macro_expand1(S, S).

% = macro_reduce(+Formula, -ReducedFormula)
%
% inverse of macro_expand/2; true if ReducedFormula can be obtained
% from Formula by using the macro's in reverse order, producing more
% compact ReducedFormula.

macro_reduce(S0, S) :-
     apply_macro(S1, S0),
     !,
     macro_reduce(S1, S).
macro_reduce(S, S).

% = apply_macro(+Formula1, -Formula2)
%
% true if Formula2 differs from Formula1 only in the application of a
% single macro/2 step or in the replacement of a Prolog atom A by
% lit(A).

apply_macro(S0, S) :-
     macro(S0, S).
apply_macro(S, lit(S)) :-
     atomic(S).
apply_macro(dia(I,S0), dia(I,S)) :-
     apply_macro(S0, S).
apply_macro(box(I,S0), box(I,S)) :-
     apply_macro(S0, S).
apply_macro(p(I,R0,S), p(I,R,S)) :-
     apply_macro(R0, R).
apply_macro(p(I,R,S0), p(I,R,S)) :-
     apply_macro(S0, S).
apply_macro(dl(I,R0,S), dl(I,R,S)) :-
     apply_macro(R0, R).
apply_macro(dl(I,R,S0), dl(I,R,S)) :-
     apply_macro(S0, S).
apply_macro(dr(I,R0,S), dr(I,R,S)) :-
     apply_macro(R0, R).
apply_macro(dr(I,R,S0), dr(I,R,S)) :-
     apply_macro(S0, S).

% = is_formula(+Term)
%
% true if Term is a formula in the format as accepted by Grail.

is_formula(lit(_)).
is_formula(dl(_,A,B)) :-
	is_formula(A),
	is_formula(B).
is_formula(dr(_,A,B)) :-
	is_formula(A),
	is_formula(B).
is_formula(p(_,A,B)) :-
	is_formula(A),
	is_formula(B).
is_formula(dia(_,A)) :-
	is_formula(A).
is_formula(box(_,A)) :-
	is_formula(A).

add_fake_semantics([], []).
add_fake_semantics([F|Fs], [G|Gs]) :-
	add_fake_semantics1(F, G),
	add_fake_semantics(Fs, Gs).

add_fake_semantics1([], []).
add_fake_semantics1([F|Fs], [F-_|Gs]) :-
	add_fake_semantics1(Fs, Gs).