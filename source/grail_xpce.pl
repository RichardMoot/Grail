% -*- Mode: Prolog -*-

:- module(grail_xpce,    [user_link/4,
	                  user_select_par/3,
			  create_main_window/1,
			  create_lex_window/1,
			  create_stats_window/1,
			  open_main_window/0,
			  open_lex_window/0,
			  open_stats_window/0,
			  update_lex_window/0,
			  main_loop/0,
		          kbest_dialog/1]).

:- use_module(grail_dot, [create_leaves/4,
                          portray_graph_c/10]).
:- use_module(graph,     [regin/4,
	                  insert_vertex/4]).
:- use_module(lexicon,   [macro_expand/2]).
:- use_module(options,   [get_option/2,
			  set_option/2,
			  set_option/4]).
:- use_module(library(pce)).
:- use_module(library(autowin)).
:- use_module(library(tabular)).
:- use_module(library(pce_util)).
:- use_module(library(pce_image)).
:- use_module(library(pce_tick_box)).
:- use_module(library(hyper)).
:- use_module(library('doc/load')).
:- use_module(library('plot/barchart')).
:- use_module(doc(html)).
:- use_module(doc(url_fetch)).
:- use_module(library(sgml)).
:- use_module(doc(browser)).

:- pce_autoload(finder, library(find_file)).
:- pce_autoload(tool_bar, library(toolbar)).

% = images

resource(fail_img,      image, image('previous.xpm')).
resource(abort_img,     image, image('stop.xpm')).
resource(next_img,      image, image('next.xpm')).
resource(play_img,      image, image('play.xpm')).
resource(ffwd_img,      image, image('ffwd.xpm')).
resource(tensor_down,   image, image('tensor_down.xpm')).
resource(tensor_left,   image, image('tensor_left.xpm')).
resource(tensor_right,  image, image('tensor_right.xpm')).
resource(u_tensor_up,   image, image('u_tensor_up.xpm')).
resource(u_tensor_down, image, image('u_tensor_down.xpm')).
resource(par_up,        image, image('par_up.xpm')).
resource(par_left,      image, image('par_left.xpm')).
resource(par_right,     image, image('par_right.xpm')).
resource(u_par_down,    image, image('u_par_down.xpm')).
resource(u_par_up,      image, image('u_par_up.xpm')).
resource(undo,          image, image('undo.xpm')).


:- pce_global(@finder, new(finder)).
:- pce_global(@g, create_main_window).
:- pce_global(@lex, create_lex_window).
:- pce_global(@help, create_help_window).
:- pce_global(@statistics, create_stats_window).
:- pce_global(@dl_button, dl_button).
:- pce_global(@p_button, p_button).
:- pce_global(@dr_button, dr_button).
:- pce_global(@dia_button, dia_button).
:- pce_global(@box_button, box_button).
:- pce_global(@undo_button, undo_button).

:- dynamic 'edit formula'/2, 'shown lex'/1.

% ===================================================================
% =                         Main Window                             =
% ===================================================================

create_main_window(G) :-
        new(G, frame('Grail 3')),
	send(G, done_message, message(@lex, uncreate)),
	send(G, append, new(EB, browser)),
	/* single click on browser item */
	send(G, attribute, example_browser, EB),
	send(EB, select_message(message(@prolog, show_dictitem, @arg1))),
	/* double click on browser item */
	send(EB, open_message(and(message(@prolog, show_dictitem, @arg1),
                                   message(@prolog, example, @arg1?key)))),
	/*  keybindings for the browser 
         * return : same as double click on current item
         * up/down: update the phrase and goal */
	send(EB, key_binding, new(KB, key_binding(@nil, list_browser))),
	send(KB, function, 'RET', message(@prolog, run_example)),
	send(KB, function, cursor_up, and(message(EB,previous_line),message(@prolog, show_dictitem, EB?selection))),
	send(KB, function, cursor_down, and(message(EB,next_line),message(@prolog, show_dictitem, EB?selection))),
	send(new(MD, dialog), above(EB)),
	send(MD, right_side, EB?right_side),
	send(new(TI, dialog), below(EB)),
	send(TI, append, new(TEXT, text_item(phrase))),
	send(G, attribute, input_sentence, TEXT),
	send(TEXT, label_font, italic),
	send(TI, gap, size(4,2)),
	send(TI, append, new(FORM, text_item(goal)), right),
	send(G, attribute, goal_formula, FORM),
	send(TEXT, advance, next),
	send(FORM, value_width, 72),
	send(FORM, label_font, italic),
	send(TI, right_side, EB?right_side),
	send(new(BUTTONS, dialog), below(TI)),
	send(G, attribute, control_buttons, BUTTONS),
	send(MD, gap, size(0,0)),
	send(MD, append, new(MB, menu_bar)),
	send(MB, append, new(FILE, popup(file)), alignment:=left),
	send(MB, append, new(WINDOW, popup(window)), alignment:=left),
	send(MB, append, new(OPTM, popup(options)), alignment:=left),
    (
        xpce_html
    ->
	send(MB, append, new(HELP, popup(help)), alignment:=right),
	send_list(HELP, append, [menu_item('About Grail 3...', message(@prolog, about_grail)), menu_item('Type-Logical Grammars...', message(@prolog, about_tlg)), menu_item('Designing Grammars...', message(@prolog, about_design)), menu_item('Interactive Parsing...', message(@prolog, about_parsing))]),
	send(HELP, append, new(GRAMHELP, menu_item('About This Grammar...', message(@prolog, about_grammar)))),
	send(@help, attribute, gram_help, GRAMHELP),
	send(GRAMHELP, active, @off)
    ;
         true
    ),
	new(GRAPHMODE, popup('Graph mode')),
	send(GRAPHMODE, show_current, @on),
	send(GRAPHMODE, multiple_selection, @off),
	send(OPTM, append, GRAPHMODE),
	send_list(GRAPHMODE, append, [
           menu_item(full, and(message(@prolog, set_option, graph_mode, full, GRAPHMODE, full),message(@prolog,reportray_graph))),
	   menu_item(distributed, and(message(@prolog, set_option, graph_mode, distributed, GRAPHMODE, distributed),message(@prolog,reportray_graph))),
	   menu_item(abstract, and(message(@prolog, set_option, graph_mode, abstract, GRAPHMODE, abstract),message(@prolog, reportray_graph)))]),
	send(GRAPHMODE, selected, full, @on),
	new(LAYOUT, popup('Layout')),
	send(LAYOUT, show_current, @on),
	send(OPTM, append, LAYOUT),
	send_list(LAYOUT, append, [
           menu_item(dot, and(message(@prolog, set_option, graph_layout_command, 'dot -Tps2 ', LAYOUT, dot),message(@prolog, layout))),
	   menu_item(neato, and(message(@prolog, set_option, graph_layout_command, 'neato -Goverlap=false -Gsplines=true -Gsep=.1 -Tps2 ', LAYOUT, neato),message(@prolog, layout))),
	   menu_item(circo, and(message(@prolog, set_option, graph_layout_command, 'circo -Tps2 ', LAYOUT, circo),message(@prolog, layout)))]),
	send(LAYOUT, selected, dot, @on),
	new(@linkm, popup('Link mode')),
	send(OPTM, append, @linkm),
	send(@linkm, show_current, @on),
	send_list(@linkm, append, [
           menu_item(auto, message(@prolog, set_option, link_mode, auto, @linkm, auto)),
	   menu_item(partial, message(@prolog, set_option, link_mode, partial, @linkm, partial)),
           menu_item(kbest, message(@prolog, set_option, link_mode, kbest, @linkm, kbest)),
           menu_item(manual, message(@prolog, set_option, link_mode, xpce, @linkm, manual))]),
	send(@linkm, selected, manual, @on),
	new(@term, popup('k-best value')),
	send(OPTM, append, @term),
	send(@term, show_current, @on),
	send_list(@term, append, [
            menu_item(1, message(@prolog, set_option, kbest_term, '1', @term, 1)), 
            menu_item(5, message(@prolog, set_option, kbest_term, '5', @term, 5)), 
            menu_item(10, message(@prolog, set_option, kbest_term, '10', @term, 10)), 
            menu_item(n, message(@prolog, set_option, kbest_term, 'N', @term, n)), 
            menu_item(n2, message(@prolog, set_option, kbest_term, 'N^2', @term, n2)), 
            menu_item(n3, message(@prolog, set_option, kbest_term, 'N^3', @term, n3)),
	    menu_item(custom, message(@prolog, kbest_dialog, @term))]),
	send(@term, selected, n, @on),
	new(@parm, popup('Par mode')),
	send(@parm, show_current, @on),
	send(OPTM, append, @parm),
	send_list(@parm, append, [
           menu_item(auto, message(@prolog, set_option, par_mode, auto, @parm, auto)),
           menu_item(manual, message(@prolog, set_option, par_mode, xpce, @parm, manual))]),
        send(@parm, selected, manual, @on),
	send_list(WINDOW, append, [
            menu_item('Lexicon Window', message(@prolog, open_lex_window)),
            menu_item('Statistics Window', message(@prolog, open_stats_window), end_group := on)]),
    (
	producing_postscript
    ->
	send_list(WINDOW, append, [
            menu_item('Parse Window (Postscript)', message(@prolog, new_postscript_window, tmp)),
            menu_item('Lookup Window (Postscript)', message(@prolog, new_postscript_window, lookup)),
            menu_item('Structural Rules Window (Postscript)', message(@prolog, new_postscript_window, structural_rules), end_group := on) ])
    ;
	true
    ),
    (
	producing_pdf
    ->
        new(@paperm, popup('LaTeX Paper Size')),
	send(@paperm, show_current, @on),
        send_list(@paperm, append, [
		menu_item('A0 paper', and(message(@prolog, set_option, paper_size, a0paper, @paperm, 'A0 paper'),message(@prolog, update_sem_pdf))),
		menu_item('A1 paper', and(message(@prolog, set_option, paper_size, a1paper, @paperm, 'A1 paper'),message(@prolog, update_sem_pdf))),
		menu_item('A2 paper', and(message(@prolog, set_option, paper_size, a2paper, @paperm, 'A2 paper'),message(@prolog, update_sem_pdf))),
		menu_item('A3 paper', and(message(@prolog, set_option, paper_size, a3paper, @paperm, 'A3 paper'),message(@prolog, update_sem_pdf))),
		menu_item('A4 paper', and(message(@prolog, set_option, paper_size, a4paper, @paperm, 'A4 paper'),message(@prolog, update_sem_pdf)))]),
        send(@paperm, selected, 'A3 paper', @on),
	send_list(WINDOW, append, [
	    @paperm,
	    menu_item('Parse all example phrases (LaTeX/pdf)', message(@prolog, open_latex_examples_window)),
	    menu_item('Semantics Window (LaTeX/pdf)', message(@prolog, open_semantics_window), end_group := on),
	    menu_item('Sentences Window (LaTeX/pdf)', message(@prolog, open_latex_sentences_window)),
	    menu_item('Lexicon Window (LaTeX/pdf)', message(@prolog, open_latex_lexicon_window)),
	    menu_item('Structural Rules Window (LaTeX/pdf)', message(@prolog, open_latex_structural_rules_window)),
	    menu_item('Grammar Window (LaTeX/pdf)', message(@prolog, open_latex_grammar_window), end_group := on) ])
    ;
	true
    ),

	send_list(FILE, append, [
            new(LMI, menu_item('Load Grammar...', message(@prolog, load_fragment))),
	    menu_item('Reload Grammar', message(@prolog, reload_fragment), accelerator := 'Control-r'),
	    menu_item('Save Grammar...', message(@prolog, save_fragment), end_group := on), 
	    menu_item(quit, message(G, return, @nil))]),
	send(G, attribute, load_menu_item, LMI),
%	new(HELP, popup(help)),
%	send(HELP, alignment, right),
%	send(MB, append, HELP),
	send(BUTTONS, append, new(PARSE, button(parse, message(@prolog, run_example)))),
	send(BUTTONS, attribute, parse_button, PARSE),
	send(BUTTONS, append, new(LEXBUT, button(lexicon, message(@prolog, open_lex_window)))),
	send(BUTTONS, attribute, lex_button, LEXBUT),
	send(BUTTONS, append, new(LOAD, button(load, message(@prolog, load_fragment)))),
	send(BUTTONS, attribute, load_button, LOAD),
	send(BUTTONS, append, new(QUIT, button(quit, message(G, return, @nil)))),
	send(BUTTONS, attribute, quit_button, QUIT).

open_main_window :-
	send(@g, open).

main_loop :-
	get(@g, confirm, _),
	send(@g, destroy).

% ===================================================================
% =                      Statistics Window                          =
% ===================================================================

create_stats_window(W) :-
        new(W, auto_sized_picture('Statistics')),
        send(W, display, new(@stats, bar_chart(vertical, 0, 50, 200, 10))).

open_stats_window :-
    (
	option_true(stats)
    ->
	send(@statistics, open)
    ;
	true
    ).

kbest_dialog(T) :-
	new(D, dialog('Custom Expression')),
	get_option(kbest_term, Old),
	send(D, append, new(ExprItem, text_item(expression))),
	send(ExprItem, label_font, italic),
	send(ExprItem, selection, Old),
	send(D, append, button(ok, message(D, return, ExprItem?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm, AExpr),
	free(D),
	AExpr \== @nil,
	atom_to_expr(AExpr, Expr, 4),
	_ is Expr,
	send(T, clear_selection),
	send(T, selected, custom, @on),
	set_option(kbest_term, AExpr).

% ===================================================================
% =                        Lexicon Window                           =
% ===================================================================

create_lex_window(L) :-
        new(L, frame('Lexicon')),
	send(L, append, new(LB, browser)),
	send(L, attribute, browser, LB),
	/* single click on browser item */
	send(LB, select_message(message(@prolog, show_lexitem, @arg1?key))),
	/* cursor buttons */
	send(LB, key_binding, new(KB, key_binding(@nil, list_browser))),
	send(KB, function, cursor_up, and(message(LB,previous_line),message(@prolog, show_lexitem, LB?selection?key))),
	send(KB, function, cursor_down, and(message(LB,next_line),message(@prolog, show_lexitem, LB?selection?key))),
	send(new(LD, dialog), above(LB)),
	send(LD, gap, size(0,0)),
	send(LD, append, new(LM, menu_bar)),
	send(LM, append, new(CLOSE, popup(window))),
	send(CLOSE, append, menu_item(close, message(L, uncreate))),
	send(LM, append, new(EDIT, popup(edit))),
	send(EDIT, append, menu_item(new_entry, message(@prolog, new_lexitem))),
	send(EDIT, append, menu_item(copy_entry, message(@prolog, copy_lexitem))),
	send(EDIT, append, menu_item(delete_entry, message(@prolog, delete_lexitem))),
	send(EDIT, append, menu_item(store_entry, message(@prolog, store_lexitem))),
	send(new(WD, dialog), below(LB)),
	send(WD, append, new(WORD, text_item(word))),
	send(WORD, label_font, italic),
	send(WD, gap, size(4,2)),
	send(WORD, message, message(@prolog, new_lexitem)),
	send(L, attribute, lex_word, WORD),
	send(WORD, recogniser, new(KB1, key_binding(@nil,argument))),
	send(KB1, function, 'RET', message(@prolog, new_lexitem)),
	send(new(FDB, dialog), below(WD)),
	send(FDB, gap, size(4,2)),
	send(FDB, append, new(INDEX, text_item(mode))),
	send(FDB, append, new(FORM, text_item(formula)), right),
	send(INDEX, label_font, italic),
	send(FORM, label_font, italic),
	send(FORM, message, message(@prolog, insert_basic)),
	send(FORM, recogniser, new(KB2, key_binding(@nil,argument))),
	send(KB2, function, 'RET', message(@prolog, insert_basic)),
	send(L, attribute, form_index, INDEX),
	send(L, attribute, lex_formula, FORM),
	send(FORM, value_width, 72),
	send(INDEX, value_width, 36),
	send(new(FD, dialog), below(FDB)),
	send(FD, gap, size(4,2)),
	neg_toolbar(TB),
%	deactivate_toolbar,
	send(FD, append, TB),
	findall(W, user:lex(W,_F,_S), WL0),
	sort(WL0, WL),
	fill_lex_window(WL, LB).

portray_toolbar(+) :-
	portray_pos_toolbar.
portray_toolbar(-) :-
	portray_neg_toolbar.

portray_pos_toolbar :-
	send(@dl_button, label, resource(par_right)),
	send(@p_button, label, resource(tensor_down)),
	send(@dr_button, label, resource(par_left)),
	send(@dia_button, label, resource(u_tensor_down)),
	send(@box_button, label, resource(u_par_down)).

portray_neg_toolbar :-
	send(@dl_button, label, resource(tensor_right)),
	send(@p_button, label, resource(par_up)),
	send(@dr_button, label, resource(tensor_left)),
	send(@dia_button, label, resource(u_par_up)),
	send(@box_button, label, resource(u_tensor_up)).


neg_toolbar(TB) :-
	new(TB, tool_bar),
	send_list(TB, append, [@dl_button, @p_button, @dr_button, gap, @dia_button, @box_button, gap, @undo_button]).

dl_button(B) :-
	new(B, tool_button(message(@prolog, insert_lexitem, dl),resource(tensor_right), dl)).
p_button(B) :-
	new(B, tool_button(message(@prolog, insert_lexitem, p), resource(par_up), p)).
dr_button(B) :-
	new(B, tool_button(message(@prolog, insert_lexitem, dr), resource(tensor_left), dr)).
dia_button(B) :-
	new(B, tool_button(message(@prolog, insert_lexitem, dia), resource(u_par_up), dia)).
box_button(B) :-
	new(B, tool_button(message(@prolog, insert_lexitem, box), resource(u_tensor_up), box)).
undo_button(B) :-
	new(B, tool_button(message(@prolog, undo_insert), resource(undo), 'Undo Last Insert')).

open_lex_window :-
	send(@lex, open),
	send(@lex, expose),
	update_valuesets.

update_lex_window :-
	get(@lex, attribute, browser, LB),
	send(LB, clear),
	findall(W, user:lex(W,_F,_S), WL0),
	sort(WL0, WL),
	fill_lex_window(WL, LB).

fill_lex_window([], _).
fill_lex_window([W|Ws], LB) :-
	new(D, dict_item(W, W, @default, @default)),
	send(LB, append, D),
	fill_lex_window(Ws, LB).

% ===================================================================
% =                     View/Edit Lexicon                           =
% ===================================================================

show_lexitem(Word) :-
	retractall('shown lex'(_)),
	retractall('edit formula'(_, _)),
	name(Word, String),
	get(@lex, lex_word, TI),
	send(TI, selection, Word),
	findall(F-S, (user:lex(Word,F0,S),macro_expand(F0,F)), ListOfFormulas),
	length(ListOfFormulas, Length),
    (
	Length > 1
    ->
	create_leaves(0, Length, Word, Leaves0)
    ;
	Leaves0 = [Word]
    ),
	assert('shown lex'(ListOfFormulas)),
	unfold_antecedent(ListOfFormulas, 1, X, Leaves1, 0, _, 0, _, 
                    Neg, [], Pos, [], C0, [], Par, [], Mp, []),
	list_to_array(Mp, AMp),
	list_to_btree(C0, C),
	merge_lists(Leaves0, Leaves1, Leaves),
	portray_graph_c(Pos, Neg, [], Par, C, String, -1, Leaves, X, AMp).

delete_lexitem :-
	get(@lex, attribute, browser, LB),
	get(LB, selection, DictItem),
	get(DictItem, key, Word),
	retractall(user:lex(Word, _, _)),
	update_lex_window.

copy_lexitem :-
	user:'shown lex'(ListOfFormulas),
	get(@lex, lex_word, TI),
	get(TI, selection, Word),
	assert_lex_list(ListOfFormulas, Word),
	update_lex_window,
	get(@lex, attribute, browser, LB),
	send(LB, select, Word),
	send(LB, normalise, Word),
	show_lexitem(Word).

assert_lex_list([], _).
assert_lex_list([F-_|Fs], W) :-
	assert(user:lex(W, F, W)),
	assert_lex_list(Fs, W).

portray_formula(Word, Form0, Vs) :-
	starry_list(Vs),
	macro_expand(Form0, Form1),
	name(Word, String),
	add_first_order(Form1, Form2, 0, [0,1]),
	add_uni0(Form2, Form, UL, UL),
	unfold0(Form, _Sem, 1, X, 1, R, 1, _Di1, Neg, [], Pos, [],
                   C0, [], Cs0, [], Par, [], Mp, []),
	list_to_btree(C0, C),
	list_to_btree([R-cmp(C,C)|Cs0], Comp),
	list_to_array(Mp, AMp),
	portray_graph_c(Pos, Neg, [], Par, Comp, String, -1, [1-Word], X, AMp).

starry_list([]).
starry_list([lit(*)|Vs]) :-
	starry_list1(Vs).

starry_list1([]).
starry_list1([lit(-)|Vs]) :-
	starry_list1(Vs).

ask_store :-
	new(D, dialog('Store lexical entry')),
	send(D, append, new(_, text('Entry complete! Do you want to store it',@default,@default))),
	send(D, append, button(ok, message(D, return, ok))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(D, default_button, ok),
	get(D, confirm, Answer),
    (
	Answer = ok
    ->
	store_lexitem
    ;
	true
    ),
	send(D, free).

store_lexitem :-
	get(@lex, lex_word, TI),
	get(TI, selection, Word),
	'edit formula'(F, L),
    (
	length(L,0)
    ->
	assert(user:lex(Word, F, Word)),
	update_lex_window,
	update_valuesets
%	deactivate_toolbar
    ;
	format('Incomplete lexical entry!~n', [])
    ).

new_lexitem :-
	get(@lex, lex_word, TI),
	get(TI, selection, Word),
	retractall('edit formula'(_, _)),
	assert('edit formula'(X, [X])),
%	activate_toolbar,
	portray_formula(Word, X, [X]).

insert_basic :-
	get(@lex, lex_word, TI),
	get(TI, selection, W),
	get(@lex, lex_formula, FI),
	get(FI, selection, F1),
	macro_expand(F1, F),
	'edit formula'(F0, [F|Xs]),
	retractall('edit formula'(_ , _)),
	assert('edit formula'(F0, Xs)),
    (
	Xs = []
    ->
	portray_formula(W, F0, Xs),
	portray_neg_toolbar,
	ask_store
    ;
	Xs = [X|_],
	compute_polarity0(F0, X, P),
	portray_toolbar(P),
	portray_formula(W, F0, Xs)
    ).

insert_lexitem(Item) :-
	get(@lex, lex_word, TI),
	get(TI, selection, W),
	get(@lex, form_index, INDEX),
	get(INDEX, selection, I0),
	/* used only to unquote integers */
	atom_to_term(I0, I, []),
	'edit formula'(F0, [X|Xs0]),
	insert_lexitem1(Item, I, F0, X, Xs0, Xs),
	retractall('edit formula'(_ , _)),
	assert('edit formula'(F0, Xs)),
	Xs = [Y|_],
	compute_polarity0(F0, Y, P),
	portray_toolbar(P),
	portray_formula(W, F0, Xs).

insert_lexitem1(dia, I, _, dia(I,A), Xs, [A|Xs]).
insert_lexitem1(box, I, _, box(I,A), Xs, [A|Xs]).
insert_lexitem1(p, I, _, p(I,A,B), Xs, [A,B|Xs]).
insert_lexitem1(dl, I, F0, X, Xs0, Xs) :- 
	compute_polarity0(F0, X, Pol),
	X = dl(I, A, B),
	add_polar_items(Pol, A, B, Xs0, Xs).
insert_lexitem1(dr, I, F0, X, Xs0, Xs) :- 
	compute_polarity0(F0, X, Pol),
	X = dr(I, B, A),
	add_polar_items(Pol, A, B, Xs0, Xs).

add_polar_items(-, A, B, Xs, [B,A|Xs]).
add_polar_items(+, A, B, Xs, [A,B|Xs]).

% = compute the polarity of a variable inside a negative formula

compute_polarity0(X, Y, -) :-
	var(X),
	!,
	X == Y.
compute_polarity0(dia(_, X), Y, P) :-
	compute_polarity0(X, Y, P).
compute_polarity0(box(_, X), Y, P) :-
	compute_polarity0(X, Y, P).
compute_polarity0(dl(_,X,Y), Z, P) :-
    (
	compute_polarity1(X, Z, P)
    ->
	true
    ;
	compute_polarity0(Y, Z, P)
    ).
compute_polarity0(dr(_,X,Y), Z, P) :-
    (
	compute_polarity0(X, Z, P)
    ->
	true
    ;
	compute_polarity1(Y, Z, P)
    ).
compute_polarity0(p(_,X,Y), Z, P) :-
    (
	compute_polarity0(X, Z, P)
    ->
	true
    ;
	compute_polarity0(Y, Z, P)
    ).

% = compute the polarity of a variable inside a positive formula

compute_polarity1(X, Y, +) :-
	var(X),
	!,
	X == Y.
compute_polarity1(dia(_, X), Y, P) :-
	compute_polarity1(X, Y, P).
compute_polarity1(box(_, X), Y, P) :-
	compute_polarity1(X, Y, P).
compute_polarity1(dl(_,X,Y), Z, P) :-
    (
	compute_polarity0(X, Z, P)
    ->
	true
    ;
	compute_polarity1(Y, Z, P)
    ).
compute_polarity1(dr(_,X,Y), Z, P) :-
    (
	compute_polarity1(X, Z, P)
    ->
	true
    ;
	compute_polarity0(Y, Z, P)
    ).
compute_polarity1(p(_,X,Y), Z, P) :-
    (
	compute_polarity1(X, Z, P)
    ->
	true
    ;
	compute_polarity1(Y, Z, P)
    ).

undo_insert :-
	'edit formula'(F0, Xs0),
	undo_last_insert(F0, F, Xs0, Xs),
	retractall('edit formula'(_, _)),
	assert('edit formula'(F, Xs)),
	get(@lex, lex_word, TI),
	get(TI, selection, Word),
	portray_formula(Word, F, Xs).


undo_last_insert('is a variable', _, _, _) :-
	!,
	fail.
undo_last_insert(lit(_),    X, Xs, [X|Xs]).
undo_last_insert(dia(I,A),  X, Xs, Ys) :-
	(
	    A = *
	->
	    Xs = [_|Xs0],
	    Ys = [X|Xs0]
	;
	    X = dia(I,A1),
	    undo_last_insert(A, A1, Xs, Ys)
	).
undo_last_insert(box(I,A),  X, Xs, Ys) :-
	(
	    A = *
	->
	    Xs = [_|Xs0],
	    Ys = [X|Xs0]
	;
	    X = box(I,A1),
	    undo_last_insert(A, A1, Xs, Ys)
	).
undo_last_insert(p(I,A,B),  X, Xs, Ys) :-
	(
	    A = *,
	    B = *
	->
	    Xs = [_,_|Xs0],
	    Ys = [X|Xs0]
	;
	    X = p(I,A1,B),
	    undo_last_insert(A, A1, Xs, Ys)
	->
	    true
	;
	    X = p(I,A,B1),
	    undo_last_insert(B, B1, Xs, Ys)
	).
undo_last_insert(dl(I,A,B),  X, Xs, Ys) :-
	(
	    A = *,
	    B = *
	->
	    Xs = [_,_|Xs0],
	    Ys = [X|Xs0]
	;
	    X = dl(I,A1,B),
	    undo_last_insert(A, A1, Xs, Ys)
	->
	    true
	;
	    X = dl(I,A,B1),
	    undo_last_insert(B, B1, Xs, Ys)
	).
undo_last_insert(dr(I,B,A),  X, Xs, Ys) :-
	(
	    A = *,
	    B = *
	->
	    Xs = [_,_|Xs0],
	    Ys = [X|Xs0]
	;
	    X = dr(I,B,A1),
	    undo_last_insert(A, A1, Xs, Ys)
	->
	    true
	;
	    X = dr(I,B1,A),
	    undo_last_insert(B, B1, Xs, Ys)
	).

% ===================================================================
% =                          Axiom Linking                          =
% ===================================================================

user_link(List, Form, Neg, Pos) :-
	get_option(link_mode, Mode),
    (
	Mode = auto
    ->
	List = [_-x(Pol,Form,N,_,Bs0,_)|_],
	member(Form-M, Bs0)
    ;
	portray_atoms(Mode, List, Neg, Pos),
	read_atom_input1(Mode, List, Neg, Links, Form, Pol),
	inverse_pol(Pol, IPol),
	read_atom_input2(Mode, Links, Pos, IPol)
    ),
	neg_pos_atom(Pol, N, M, Neg, Pos).

read_atom_input1(xpce, List, Neg, Links, Form, Pol) :-
	read_atom_input1_xpce(List, Neg, Links, Form, Pol).
read_atom_input1(partial, List, Neg, Links, Form, Pol) :-
	read_atom_input1_xpce(List, Neg, Links, Form, Pol).
read_atom_input1(console, List, Neg, Links, Form, Pol) :-
	read_atom_input1_keyb(List, Neg, Links, Form, Pol).


read_atom_input2(xpce, Links, Pos, IPol) :-
	read_atom_input2_xpce(Links, Pos, IPol).
read_atom_input2(partial, Links, Pos, IPol) :-
	read_atom_input2_xpce(Links, Pos, IPol).
read_atom_input2(console, Links, Pos, IPol) :-
	read_atom_input2_keyb(Links, Pos, IPol).


neg_pos_atom(+, A, B, A, B).
neg_pos_atom(-, A, B, B, A).

read_atom_input1_xpce(As, N, Bs, Form, Pol) :-
	member(_-x(Pol,Form, N, _, Bs, _), As),
	!.
read_atom_input2_xpce([F-ND|As], M, _Pol) :-
	memberchk(F-M, [F-ND|As]).

read_atom_input1_keyb(As, N, Bs, Form, Pol) :-
	As = [_-x(_,_,ND,_,_,_)|_],
	concat_atom(['[', ND, '] ?'], '', Prompt),
	prompt1(Prompt),
	read_integer(N0, ND),
    (
	member(_-x(Pol,Form,N0,_,Bs,_), As)
    ->
	N = N0
    ;
	read_atom_input1_keyb(As, N, Bs, Form, Pol)
    ).

read_atom_input2_keyb([F-ND|As], M, Pol) :-
	portray_pol_atoms([F-ND|As], Pol),
	concat_atom(['[', ND, '] ?'], '', Prompt),
	prompt1(Prompt),
	read_integer(N0, ND),
    (
	select(F-N0, [F-ND|As], Bs)
    ->
	(
	    M = N0
	;
	    read_atom_input2_keyb(Bs, M, Pol)
	)
    ;
	read_atom_input2_keyb([F-ND|As], M, Pol)
    ).

inverse_pol(+, -).
inverse_pol(-, +).

portray_pol_atoms([], _) :-
	nl.
portray_pol_atoms([F-N|Rest], Pol) :-
	portray_atom(N, F, Pol),
	portray_pol_atoms(Rest, Pol).

portray_atom(A, F, P) :-
	format('~w~w(~w) ', [P, F, A]).

% = portray_atoms(+Mode, +Heap, +Answer)
%
% portray the atoms to the console or xpce, depending on Mode

portray_atoms(xpce, List, Neg, Pos) :-
	portray_atoms_xpce(List, Neg, Pos).
portray_atoms(partial, List, Neg, Pos) :-
	portray_atoms_xpce(List, Neg, Pos).
portray_atoms(console, List, _Neg, _Pos) :-
	portray_atoms_text(List).

% = portray_atoms_text(+Heap)
%
% portrays the atoms in Heap to the screen

portray_atoms_text([]) :-
	nl.
portray_atoms_text([_-x(Pol,F,N,_,_,_)|Rest]) :-
	portray_atom_text(N, F, Pol),
	portray_atoms_text(Rest).

portray_atom_text(A, F, P) :-
	format('~w~w(~w) ', [P, F, A]).

% = portray_atoms_xpce(+Heap, +Anser)

portray_atoms_xpce(L, Neg, Pos) :-
	findall(C, member(_A-x(_B,C,_D,_E,_F,_G),L), Atoms0),
	sort(Atoms0, Atoms),
	length(Atoms, Len),
	W is ceil(sqrt(Len)),
        new(D, dialog('Atom Selection')),
	send(D, hor_stretch, 100),
	send(D, ver_stretch, 100),
	send(D, hor_shrink, 0),
	send(D, ver_shrink, 0),
	send(D, resize_message, message(D, layout, @arg2)),
	new(BUTTONS, tool_bar),
	send_list(BUTTONS, append,
	  [ 
            tool_button(message(D, return, fail), resource(fail_img), 'Fail - Abandon current linking and retry alternatives'),
	    tool_button(message(D, return, default), resource(play_img), 'Default - Grail performs its preferred axiom connection'),
	    tool_button(message(D, return, auto), resource(ffwd_img), 'Fast-forward to next solution'),
            tool_button(message(D, return, next), resource(next_img), 'Next - Abandon proof attempts for this lexical lookup and try the next one'),
	    gap,
	    gap,
            tool_button(message(D, return, abort), resource(abort_img), 'Stop - Abort all proof attempts for current sentence')
	  ]),
	new(F, auto_sized_picture),
	get(F, tile, TILE),
	send(TILE, can_resize, @on),
	send(TILE, enforce, @on),
	send(F, hor_stretch, 100),
	send(F, ver_stretch, 100),
	send(F, hor_shrink, 100),
	send(F, ver_shrink, 100),
	send(F, display, new(T, tabular)),
	send(D, append, BUTTONS),
	send(D, append, F),
	L = [_-x(P,_,DefaultX,_,[_-DefaultY|_],_)|_],
	polarity_xy(P, DefaultX, DefaultY, DefX, DefY),
	portray_atoms_xpce(Atoms, L, 1, W, DefX, DefY, D, T),
	get(T, size, size(XT,YT)),
	MAXX = 700,
	MAXY = 500,
	ADD = 6,
	ADD_SCROLL = 28,
    (
	XT > MAXX
    ->
	(
	    YT > MAXY
	->
	    send(F, scrollbars, both),
	    WX is MAXX + ADD_SCROLL,
	    WY is MAXY + ADD_SCROLL
	;
	    send(F, scrollbars, horizontal),
	    WX is MAXX + ADD,
	    WY is YT + ADD_SCROLL
	)
    ;
	(
	    YT > MAXY
	->
	    send(F, scrollbars, vertical),
	    WX is XT + ADD_SCROLL,
	    WY is MAXY + ADD
	;
	    WX is XT + ADD,
	    WY is YT + ADD
	)
    ),
	send(TILE, width, WX),
	send(TILE, height, WY),
	send(D, fit),
	send(D, open),
	get(D, confirm, Answer),
	send(D, free),
    (
	Answer = default
    ->
	continue_linking(DefX, DefY, Neg, Pos, L, Heap)
    ;
	Answer = auto
    ->
	get_option(link_mode, LinkMode),
	retractall(current_link_mode(_)),
	assert(current_link_mode(LinkMode)),
	set_option(link_mode, auto),
	continue_linking(DefX, DefY, Neg, Pos, L, Heap)
    ;
	Answer = fail
    ->
	fail
    ;
	Answer = next
    ->
	throw(next)
    ;
	Answer = abort
    ->
	throw(aborted)
    ;
	/* one of the link buttons has been selected */
	atom_to_term(Answer, Neg0-Pos0, []),
	continue_linking(Neg0, Pos0, Neg, Pos, L, Heap)
   ).

continue_linking(Neg, Pos, Neg, Pos, _, _).
continue_linking(Neg0, Pos0, Neg, Pos, L, Heap) :-
	/* when backtracking, apply Regin's algorithm to the elements */
        /* still on the heap, eliminating items which have become */
        /* impossible given that some possibilties have already been */
        /* considered */
	reduce_heap(L, Neg0, Pos0, Heap),
	portray_atoms_xpce(Heap, Neg, Pos).

polarity_xy(-, Y, X, X, Y).
polarity_xy(+, X, Y, X, Y).

reduce_heap(H0, N, P, H) :-
	reduce_heap1(H0, N, P, H1),
	keysort(H1, H2),
	regin_heap(H2, H).

regin_heap(H0, H) :-
	heap_to_graph(H0, empty, G, 0, MC),
	regin(G, MC, VI, RE),
	update_heap(VI, RE, H0, H).

heap_to_graph([], G, G, C, C).
heap_to_graph([_-x(P,_,I,_,L,_)|Hs], G0, G, C0, C) :-
	heap_to_graph1(P, I, L, G0, G1, C0, C1),
	heap_to_graph(Hs, G1, G, C1, C).

heap_to_graph1(-, I, _, G0, G, C, C) :-
	insert_vertex(G0, I, [], G).
heap_to_graph1(+, I, L0, G0, G, C0, C) :-
	C is C0 + 1,
	list_to_vertices(L0, L1),
	sort(L1, L),
	insert_vertex(G0, I, L, G).

list_to_vertices([], []).
list_to_vertices([_-I|Is], [I-1|Js]) :-
	list_to_vertices(Is, Js).

reduce_heap1([], _, _, []).
reduce_heap1([A0-x(B,C,D,Di,E0,F0)|H0], Neg, Pos, [A-x(B,C,D,Di,E,F)|H]) :-
	reduce_heap_item(B, C, D, Neg, Pos, A0, A, E0, E, F0, F),
	reduce_heap1(H0, Neg, Pos, H).

reduce_heap_item(+, At, N, Neg, Pos, A0, A, E0, E, F0, F) :-
	remove_from_rest(F0, At-Neg, F1),
	remove_from_rest(F1, At-Pos, F),
    (
        N = Neg,
	ord_select(At-Pos, E0, E)
    ->
	A is A0 - 1,
	A > 0
    ;
	A = A0,
	E = E0
    ).
reduce_heap_item(-, At, N, Neg, Pos, A0, A, E0, E, F0, F) :-
	remove_from_rest(F0, At-Neg, F1),
	remove_from_rest(F1, At-Pos, F),
    (
	N = Pos,
	ord_select(At-Neg, E0, E)
    ->
	A is A0 - 1,
	A > 0
    ;
	A = A0,
	E = E0
    ).

remove_from_rest(As, FN, Bs) :-
    (
	select(FN, As, Bs)
    ->
	true
    ;
	Bs = As
    ).

% portray_atoms_xpce(+AtomsForms, +Heap, +N0, ?N, +DefX, +DefY, +Dialog, +Table)

portray_atoms_xpce([], _, _, _, _, _, _, _).
portray_atoms_xpce([A|As], L, N0, N, DefX, DefY, Dialog, T) :-
	portray_atom_xpce(A, L, DefX, DefY, Dialog, T),
    (
	N0 = N
    ->
	send(T, next_row),
	N1 = 1
    ;
	N1 is N0+1
    ),
	portray_atoms_xpce(As, L, N1, N, DefX, DefY, Dialog, T).

portray_atom_xpce(A, L, DefX, DefY, Dialog, P) :-
	findall(pos(B,C,D), member(_E1-x(+,A,B,Di,C,D),L), PosList0),
	findall(neg(B,C,D), member(_E2-x(-,A,B,Di,C,D),L), NegList0),
	sort(PosList0, PosList),
	sort(NegList0, NegList),
	new(F, picture),
	send(F, display, new(T, tabular)),
	send(P, append, F),
        send(T, border, 1),
	send(T, cell_spacing, -1),
	send(T, rules, all),
        send(T, frame, box),
	portray_header_xpce(PosList, Ps, A, T),
	portray_fields_xpce(NegList, Ps, A, DefX, DefY, Dialog, T),
	get(T, size, size(XT,YT)),
	get(F, size, size(XF,YF)),
	send(F, size, size(XT,YT)),
	format(log, 'Picture: ~wx~w~nTabular:~wx~w~n', [XF,YF,XT,XT]).

portray_fields_xpce([], _, _, _, _, _, _).
portray_fields_xpce([neg(B,C,_D)|Ns], Ps, A, DefX, DefY, Dialog, T) :-
%	send(T, append, text(B, font := bold)),
	send(T, append, new(Cell, table_cell(text(B)))),
	send(Cell, halign, right),
	send(Cell, valign, center),
	portray_field_items(C, B, Ps, DefX, DefY, Dialog, T),
	portray_fields_xpce(Ns, Ps, A, DefX, DefY, Dialog, T).

portray_field_items([], _, L, _, _, _, T) :-
	portray_empty_fields(L, T).
portray_field_items([I-N|Is0], A, [N0|Ps], DefX, DefY, Dialog, T) :-
    (
	N0 = N
    ->
	Is = Is0,
        concat_atom([N,A], '-', Txt),
	new(B, button(Txt, message(Dialog, return, Txt))),
	send(B, show_focus_border, @off),
%	send(B, label_width, 25),
	send(B, label_format, center),
	send(B, alignment, center),
	default_button(N, A, DefX, DefY, Def),
	send(B, default_button, Def)
    ;
	Is = [I-N|Is0],
	new(B, text(' '))
    ),
	send(T, append, B),
	portray_field_items(Is, A, Ps, DefX, DefY, Dialog, T).

default_button(X0, Y0, X, Y, R) :-
    (
	X = X0,
	Y = Y0
    ->
	R = @on
    ;
	R = @off
    ).

portray_empty_fields([], T) :-
	send(T, next_row).
portray_empty_fields([_|Ps], T) :-
	new(B, text(' ')),
	send(T, append, B),
	portray_empty_fields(Ps, T).


portray_header_xpce(P, Ps, A0, T) :-
	portray_atom_name(A0, A),
%	send(T, append, text(A, font:=bold)),
	send(T, append, new(C, table_cell(text(A)))),
	send(C, halign, center),
	send(C, valign, center),
	portray_header_xpce1(P, Ps, T).

portray_header_xpce1([], [], T) :-
	send(T, next_row).
portray_header_xpce1([A|As], [P|Ps], T) :-
	portray_pos_atom_xpce(A, P, T),
	portray_header_xpce1(As, Ps, T).

portray_pos_atom_xpce(pos(A,_,_), A, T) :-
%	send(T, append, text(A, font:=bold)).
	send(T, append, new(C, table_cell(text(A)))),
	send(C, halign, center),
	send(C, valign, center).

portray_atom_name('$VAR'(N), A) :-
	!,
	A0 is 65+(N mod 26),
	name(A1, [A0]),
	N1 is N // 26,
    (
	N1 = 0
    ->
	NS = ''
    ;
	NS = N1
    ),
	concat_atom([A1,NS], A).
portray_atom_name(A, A).

% ===================================================================
% =                          Par Selection                          =
% ===================================================================

user_select_par([Par0|Pars0], Par, Pars) :-
	get_option(par_mode, Mode),
    (
	Mode = auto
    ->
	Par = Par0,
	Pars = Pars0
    ;
	Mode = manual
    ->
	portray_pars(Pars0, Par0),
	active_root(Par0, Rt0),
	concat_atom(['[', Rt0, '] ?'], '', Prompt),
	prompt1(Prompt),
	read_integer(Int, Rt0),
	select_par([Par0|Pars0], Int, Par, Pars)
    ;
	select_par_xpce([Par0|Pars0], Par, Pars)
    ).

select_par_xpce([Par0|Pars0], Par, Pars) :-
	new(D, dialog('Select contraction')),
	send(D, append, new(_, text('Select the root node of the par link you want\n to contract, or "cancel" to abort',@default,@default))),
	portray_pars_xpce(Pars0, Par0, D),
	send(D, append, button(cancel, message(D, return, cancel))),
	get(D, confirm, Answer),
    (
	Answer = cancel
    ->
	send(D, free),
	fail
    ;
	send(D, free),
	select_par([Par0|Pars0], Answer, Par, Pars)
    ).

portray_pars_xpce([], Par, D) :-
	active_root(Par, Root),
	send(D, append, button(Root, message(D, return, Root))).
portray_pars_xpce([Par0|Pars], Par, D) :-
	active_root(Par, Root),
	send(D, append, button(Root, message(D, return, Root))),
	portray_pars_xpce(Pars, Par0, D).

portray_pars([], Par) :-
	print(Par),
	active_root(Par, Root),
	format(' [~w]~n', [Root]).

portray_pars([Par0|Pars], Par) :-
	print(Par),
	active_root(Par, Root),
	format(' [~w], ', [Root]),
	portray_pars(Pars, Par0).

select_par([Par0|Pars0], Int, Par, Pars) :-
	active_root(Par0, Root),
    (
	Root = Int
    ->
	Par = Par0,
	Pars = Pars0
    ;
	Pars = [Par0|Pars1],
	select_par(Pars0, Int, Par, Pars1)
    ).

% ===================================================================
% =                           Terminal IO                           =
% ===================================================================

% = read_integer(-Integer, +Default)
%
% reads Integer from the standard input. Unifies Integer with Default
% if the user only presses <Enter>

read_integer(N, ND) :-
	get0(C),
    (
	C = 10
    ->
	N = ND
    ;
	read_integer(C, 0, N)
    ).

read_integer(C, N0, N) :-
    (
	C = 10
    ->
	N = N0
    ;
	C > 47,
	C < 58
    ->
	N1 is (N0*10)+(C-48),
        get0(C1),
	read_integer(C1, N1, N)
    ;
	get0(C1),
	read_integer(C1, N0, N)
    ).
