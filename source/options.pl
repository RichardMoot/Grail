% -*- Mode: Prolog -*-

:- module(options, [create_options/0,
		    set_option/2,
		    set_option/4,
		    set_option_true/1,
		    set_option_false/1,
		    get_option/2,
		    option_true/1,
		    option_false/1]).

:- dynamic graph_mode/1, graph_color/1, graph_bgcolor/1, new_color/1, active_color/1, active_bgcolor/1.
:- dynamic graph_layout_command/1, constraint/1, explicit_modes/1, uni_modal/1, tentacle_labels/1.
:- dynamic zero_root/1, link_mode/1, latex/1, kbest_term/1, par_mode/1, paper_size/1, fxy/1.
:- dynamic collapse_lambda/1, expl_brackets/1, gv/1, stats/1, query/1, zero_heap/1.
:- dynamic lex_by_type/1, regin/1, xpce/0.

xpce.

graph_mode(full).
graph_color(black).
graph_bgcolor(white).
new_color(gray60).
active_color(blue).
active_bgcolor(white).
graph_layout_command('dot -Tps 2 ').
constraint(0).
explicit_modes(0).
uni_modal(1).
tentacle_labels(1).
zero_root(0).
link_mode(xpce).
kbest_term('N').
par_mode(xpce).
paper_size(a3paper).
fxy(1).
collapse_lambda(1).
expl_brackets(0).
gv(0).
stats(0).
query(0).
zero_heap(0).
lex_by_type(0).
regin(1).

% ===================================================================
% =                             Options                             =
% ===================================================================

create_options :-
    (
	'/usr/texbin/pdflatex' == 'NULL'
    ->
	LaTeX = 0
    ;
	LaTeX = 1
    ),
    retractall(latex(_)),
    assert(latex(LaTeX)).

set_option(Option, Value) :-
	functor(GenTerm, Option, 1),
	functor(NewTerm, Option, 1),
	arg(1, NewTerm, Value),
	retractall(GenTerm),
	assert(NewTerm).

% = get_option(+OptionName, ?Value)

get_option(Option, Value) :-
	call(Option, Value).

option_true(Option) :-
	call(Option, 1).
option_false(Option) :-
	call(Option, 0).

set_option_true(Option) :-
	set_option(Option, 1).
set_option_false(Option) :-
	set_option(Option, 0).

set_option(Option, Value, Menu, MenuItem) :-
    (
        xpce
    ->
	send(Menu, clear_selection),
	send(Menu, selected, MenuItem, @on)
    ;
        true
    ),
    	set_option(Option, Value).
