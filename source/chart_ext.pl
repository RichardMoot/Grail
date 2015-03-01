% -*- Mode: Prolog -*-

:- module(chart, [grail_chart/1,
		  chart_parse_all/0,
		  chart_parse_all/1,
		  chart_parse/2,
		  export/0,
		  prob_parse/2]).

:- use_module(lexicon, [macro_expand/2,get_item_semantics/5]).
:- use_module(heap, [empty_heap/1,add_to_heap/4,get_from_heap/4]).
:- use_module(prob_lex, [list_atom_term/2,list_atom_term/3,remove_brackets/2]).

:- dynamic sentence_length/1, unparsed/3.
:- dynamic word/5, stored/6, grail_heap/2.

% = Chart parse library.
%
% This library contains code adapted from the following paper:
%
% Stuart M. Shieber, Yves Schabes and Fernando C. N. Pereira (1995)
% `Principles and Implementation of Deductive Parsing', Journal of
% Logic Programming 24(1-2):3-36
%
% though with some notable additions and modifications.

% Chart items are of the form
%
%   item(Formula, Left, Right, Data)
%
% where Formula is a multimodal formula, Left and Right are
% string positions representing the leftmost and the
% rightmost part of the string which was recongized as
% being of type Formula.
%
% Item data is a structure containing the following information.
%
%   data(Pros, Sem, Prob, A, B, C)
%
% where Pros is the prosodic structure (representing the
% words of the string in tree form), Sem is the semantics
% of the Formula, Prob is its probabiblity,

% = export chart contents to the file 'stored.txt'

export :-
	shell('rm stored.txt', _),
	tell('stored.txt'),
	listing(stored),
	told.

% = parse_all sentences.

chart_parse_all :-
	shell('rm chart*.dot', _),
	retractall(unparsed(_,_,_)),
	new_output_file(grail_log, log),
	print_grail_semantics_header,
	chart_parse_all(1, 5000, 0, 0).

chart_parse_all(DL) :-
	shell('rm chart*.dot', _),
	retractall(unparsed(_,_,_)),
	new_output_file(grail_log, log),
	print_grail_semantics_header,
	chart_parse_all(1, DL, 0, 0).

chart_parse_all(N0, DL, Fail0, Limit0) :-
	clause(user:sent(N0,_),_),
	!,
	format('~nStarting: ~w~n', [N0]),
    (
	call_with_depth_limit(user:sent(N0, Result), DL, DepthLimit)
    ->
        (
	   DepthLimit = depth_limit_exceeded
	->
	   Limit is Limit0 + 1,
	   Fail = Fail0,
	   assert('unparsed'(N0,Length,depth_limit)),
	   format('~nDepth limit exceeded: ~w~n', [N0])
	 ;
	   Limit = Limit0,
           Fail = Fail0,
	   print_grail_semantics(Result),
	   format('~nSuccess: ~w (~w)~n', [N0,DepthLimit])
	)
    ;
	print_chart(N0),
	sentence_length(Length),
	assert('unparsed'(N0,Length,fail)),
        Limit = Limit0,
	Fail is Fail0 +1,
	format('~nFailure: ~w~n', [N0])
    ),
	N1 is N0 + 1,
	chart_parse_all(N1, DL, Fail, Limit).
chart_parse_all(N0, _, F, L) :-
	/* N0 is the first sentence for which no clause is defined */
	N is N0 - 1,
	Success is (N - F) - L,
	SPercentage is (100*Success)/N,
	FPercentage is (100*F)/N,
	LPercentage is (100*L)/N,
	format('Finished parsing~n~w total sentences~n~w sentences succeeded (~w %)~n~w sentences failed (~w %)~n~w resource limits (~w %)~n', [N,Success,SPercentage,F,FPercentage,L,LPercentage]),
	print_grail_semantics_tail,
	close(log).

% = print_chart(+SentNo)
%
% output the chart contexts to a GraphViz file

print_chart(SentNo) :-
	concat_atom(['chart',SentNo,'.dot'], FileName),
	open(FileName, write, Stream),
	print_chart1(Stream),
	close(Stream).
print_chart1(Stream) :-
	format(Stream, 'digraph "chart" {~n', []),
	stored(_, _, I, J, F, _),
	format(Stream, '~w -> ~w [label="~p"]~n', [I,J,F]),
	fail.
print_chart1(Stream) :-
	format(Stream, '}~n', []).

% = chart_to_heap(Ex)
%
% conver chart contents to a heap to be passed to Grail with
% the goal of handing an incomplete parse over to Grail.
% TODO: complete

chart_to_heap(Ex) :-
	findall(P-X, chart_prob(P,X), List),
	empty_heap(Heap0),
	add_list_to_heap(List, Heap0, Heap),
	assert(grail_heap(Ex, Heap)).

add_list_to_heap([], Heap, Heap).
add_list_to_heap([P-X|Xs], Heap0, Heap) :-
	add_to_heap(Heap0, P, X, Heap1),
	add_list_to_heap(Xs, Heap1, Heap).

parse_best_span(N0, I, J) :-
	parse_best_span(I, J, L, []),
	format('~w ~w', [N0, L]).

chart_prob(D-Prob,t(I,J,Atom,F,Sem)) :-
	stored(_, _, I, J, F, data(Pros, Sem, Prob, [], [], [])),
	D is J - I,
	pros_to_atom(Pros, Atom).

pros_to_atom(Pros, Atom) :-
	pros_to_list(Pros, List, []),
	concat_atom(List, '_', Atom).

pros_to_list(p(_,P1,P2), L0, L) :-
	!,
	pros_to_list(P1, L0, L1),
	pros_to_list(P2, L1, L).
pros_to_list(P, [P|L], L).

% = startsymbol(+Start, +Semantics)
%
% true if Start is a valid category for spanning the entire chart.
% Semantics is the semantic term for existantial closure and other "final" operations to produce proper DRSs

startsymbol(lit(txt), lambda(X,X)).
startsymbol(lit(s), lambda(S,merge(drs([event(E)],[]),appl(S,E)))).
startsymbol(lit(s(_)), lambda(S,merge(drs([event(E)],[]),appl(S,E)))).
startsymbol(lit(np(_,_,_)), lambda(P,appl(P,lambda(_V,drs([],[]))))).
startsymbol(lit(cs), lambda(S,merge(drs([event(E)],[]),appl(S,E)))).
startsymbol(lit(n), lambda(N,merge(drs([variable(X)],[]),appl(N,X)))).
startsymbol(dl(0,lit(np(_,_,_)),lit(s(_))), lambda(_NP,lambda(S,merge(drs([event(E)],[]),appl(S,E))))).
startsymbol(dl(0,lit(n),lit(n)), lambda(X,X)).
startsymbol(dr(0,lit(s(X)),lit(s(X))), lambda(X,X)).

grail_chart(L) :-	
	print_grail_semantics_header,
	/* separate words from forula assignments */
	split_a_b_c_ds(L, Ws0, POSs0, Lemmas0, Fs0),
	/* turn POS tags from atoms into terms */
	list_atom_term(POSs0, POSs),
	list_atom_term(Lemmas0, Lemmas),
	/* separate formulas from probabilities */
	list_atom_term(Fs0, FPs, []),
	/* compute sentence string */
	remove_brackets(Ws0, Ws),
	reset_global_counters,
	init_chart,
	empty_heap(Heap),
	create_chart(Ws, POSs, Lemmas, FPs, 0, Heap, Chart, []),
    (
	chart_parse(Chart, Semantics)
    ->
        print_grail_semantics(Semantics),
        print_grail_semantics_tail
    ;
        true
    ),
        /* output final chart for debugging */
        export,
	finished_dialog,
	halt.


print_grail_semantics_header :-
	open_semantics_files,
	get(@options, paper_size, PaperSize),
	latex_header(sem, PaperSize).

print_grail_semantics(Sem) :-
	numbervars(Sem, 0, _),
	reduce_sem(Sem, RSem),
	format('Semantics   : ~p~n', [Sem]),
	format('Reduced Sem : ~p~n', [RSem]),
	format(log, '~n% = Semantics~2n ~W~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(log, '% = Reduced Semantics~2n~W~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '~n% = Semantics~2n ~W.~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '% = Reduced Semantics~2n~W.~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem, '~n\\begin{multline}~n', []),
	latex_semantics(Sem, Formula, sem),
	format(sem, '\\rightarrow_{\\beta}\\\\ ', []),
	latex_semantics(RSem, Formula, sem),
	format(sem, '~n\\end{multline}~2n', []).

print_grail_semantics_tail :-
	latex_tail(sem),
	close(sem),
	pdflatex_semantics.
	
add_heap_to_chart(H0) -->
	{get_from_heap(H0, _Key, Datum, H)},
	!,
	[Datum],
	add_heap_to_chart(H).
add_heap_to_chart(_) -->
	[].

% create_chart(+WordList, +POSList, +LemmaList, +FormulaProbList, +WordNo, Heap)

create_chart([], [], [], [], N, H) -->
	{retractall(sentence_length(_)),
	 assert(sentence_length(N))},
	add_heap_to_chart(H).
% skip final punctuation if its formula is "boring"
create_chart([_], [pun], [_], [FP], N, H) -->
	{ boring(FP) ,
	  retractall(sentence_length(_)),
	  assert(sentence_length(N)) },
	add_heap_to_chart(H).
create_chart([W|Ws], [P|Ps], [L|Ls], [FP|FPs], N0, H0) -->
	{ N is N0 + 1,
	  assert(word(W,P,L,N0,N))},
	append_item_and_update_heap(FP, W, P, L, N0, N, H0, H),
	create_chart(Ws, Ps, Ls, FPs, N, H).
	
prob_parse(List, Result) :-
	empty_heap(Heap),
	list_to_chart(List, 0, Heap, Chart, []),
	chart_parse(Chart, Result).

list_to_chart([], N, H, As0, As) :-
	retractall(sentence_length(_)),
	assert(sentence_length(N)),
	add_heap_to_chart(H, As0, As).
list_to_chart([si(_, pun, _, FPs)], N, H, As0, As) :-
	boring(FPs),
	!,
	retractall(sentence_length(_)),
	assert(sentence_length(N)),
	add_heap_to_chart(H, As0, As).
list_to_chart([si(W,Pos,Lemma,FPs)|Ws], N0, H0, As0, As) :-
	N1 is N0 + 1,
	assert(word(W,Pos,Lemma,N0,N1)),
	append_item_and_update_heap(FPs, W, Pos, Lemma, N0, N1, H0, H, As0, As1),
	list_to_chart(Ws, N1, H, As1, As).

get_semantics(item(_, _, _, Data), Sem) :-
	get_data_semantics(Data, Sem).

get_data_semantics(data(_, Sem, _, _, _, _), Sem).
		   
boring([]).
boring([Item-_|FPs]) :-
	boring_item(Item),
	!,
	boring(FPs).
boring([Item,_|FPs]) :-
	boring_item(Item),
	boring(FPs).

boring_item(let).
boring_item(dl(0,_,lit(txt))).

append_items([], _, _, _, _, _) -->
	[].
append_items([F0-P|FPs], W, Pos, Lemma, N0, N1) -->
	!,
	{create_item(F0, P, W, Pos, Lemma, N0, N1, Item)},
	[Item],
	append_items(FPs, W, Pos, Lemma, N0, N1).
append_items([F0,P|FPs], W, Pos, Lemma, N0, N1) -->
	{create_item(F0, P, W, Pos, Lemma, N0, N1, Item)},
	[Item],
	append_items(FPs, W, Pos, Lemma, N0, N1).

append_item_and_update_heap([], _, _, _, _, _, H, H) -->
	[].
append_item_and_update_heap([F0-P|FPs], W, Pos, Lemma, N0, N1, H0, H) -->
	!,
	{create_item(F0, P, W, Pos, Lemma, N0, N1, Item)},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, H0, H)}.
append_item_and_update_heap([F0,P|FPs], W, Pos, Lemma, N0, N1, H0, H) -->
	{create_item(F0, P, W, Pos, Lemma, N0, N1, Item)},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, H0, H)}.


update_heap([], _, _, _, _, _, _, H, H).
update_heap([F0-P|FPs], PMax, W, Pos, Lemma, N0, N1, H0, H) :-
	create_item(F0, P, W, Pos, Lemma, N0, N1, Item),
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, H1, H).
update_heap([F0,P|FPs], PMax, W, Pos, Lemma, N0, N1, H0, H) :-
	create_item(F0, P, W, Pos, Lemma, N0, N1, Item),
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, H1, H).


create_item(F0, P, W, Pos, Lemma, N0, N1, item(F, N0, N1, Data)) :-
	macro_expand(F0, F),
	get_item_semantics(W, Pos, Lemma, F, Sem),
	create_data(W, F, P, Sem, N0, N1, Data).
	

create_initial(W, F, N0, N1, Data) :-
	lex(W, F, Sem),
	create_data(W, F, 0.5, Sem, N0, N1, Data).
	       
chart_parse(Axioms, Sem) :-
	init_agenda(Axioms, Agenda),
	exhaust(Agenda),
	final_item(Goal, Sem),
	item_in_chart(Goal),
       	increase_global_counter('$SOLUTION').

init_agenda(Axioms, Agenda) :-
	empty_agenda(Empty),
	add_items_to_agenda(Axioms, Empty, Agenda).

exhaust(Empty) :-
	is_empty_agenda(Empty),
	!.
exhaust(Agenda0) :-
	pop_agenda(Agenda0, Index, Agenda1),
	add_item_to_chart(Index),
	add_consequences_to_agenda(Index, Agenda1, Agenda),
	exhaust(Agenda).

add_consequences_to_agenda(Index, Agenda0, Agenda) :-
	findall(Consequence,
		consequence(Index, Consequence),
		Consequences),
	add_items_to_agenda(Consequences, Agenda0, Agenda).

consequence(Index, Consequent) :-
	index_to_item(Index, Trigger),
	matching_rule(Trigger, RuleName, Others, Consequent, SideConds),
	items_in_chart(Others, Index),
	hold(SideConds),
	notify_consequence(RuleName, Trigger, Others, SideConds, Consequent).

items_in_chart([], _Index).
items_in_chart([Antecedent|Antecedents], Index) :-
	item_in_chart(Antecedent, Index),
	items_in_chart(Antecedents, Index).

hold([]).
hold([Cond|Conds]) :-
	call(Cond),
	hold(Conds).

matching_rule(Trigger, RuleName, Others, Consequent, SideConds) :-
	inference(RuleName, Antecedent, Consequent, SideConds),
	select(Trigger, Antecedent, Others).

:- dynamic stored/6.
:- dynamic key_index/2.

% depracated declaration, will disappear from soon! RM

:- index(stored(1,1,1,1,0,0)).

item_stored(Item, Index) :-
	Item = item(Formula, I, J, Data),
	calculate_key_index(Formula, Key, Index),
	stored(Index, Key, I, J, Formula, Data).

% = calculate_key_index(+Formula, +Index, -Key)
%
% true if Key is a unique hashkey for Formula. Since
% hashkeys are only defined for ground terms, this will
% return a variable (matching all keys) if Formula is not
% ground.

calculate_key_index(Formula0, Key, Index) :-
	simplify_formula(Formula0, Formula),
	ground(Formula),
	!,
        simplified_formula_to_key(Formula, Key),
        key_index(Key, Index).
calculate_key_index(_, _, _).


similar_item(Item, item(Formula, I, J, Data), IndexofSimilar) :-
	Item = item(Formula, I, J, _),
	simplify_formula(Formula, SForm),
	simplified_formula_to_key(SForm, Key),
	key_index(Key, IndexofSimilar),
	stored(IndexofSimilar, Key, I, J, Formula, Data).
subsumed_item(Item) :-
	similar_item(Item, OtherItem, IndexofSimilar),
	subsumes_item(OtherItem, Item, IndexofSimilar).

subsumes_item(item(F0, I0, J0, Data0), item(F, I, J, Data1), IndexofSimilar) :-
	subsumes_chk(F0, F),
	subsumes_chk(I0, I),
	subsumes_chk(J0, J),
	subsumes_data(Data0, Data1, O),
	keep_most_probable(O, IndexofSimilar, F0, F, I0, I, J0, J, Data0).


subsumes_data(data(_,_,Prob0,A0,B0,C0),data(_,_,Prob,A,B,C), O) :-
	subsumes_chk(A0, A),
	subsumes_chk(B0, B),
	subsumes_chk(C0, C),
	compare(O, Prob0, Prob).

% TODO: test
% If the old value is *identical* to the new one, but the probability is lower,
% then erase the old value and fail the subsumption test. Otherwise, succeed.

keep_most_probable(<, IndexofSimilar, F0, F, I0, I, J0, J, Data) :-
	F0 == F,
	I0 == I,
	J0 == J,
	!,
	retractall(stored(IndexofSimilar, _, I, J, F, Data)),
	fail.
keep_most_probable(_, _, _, _, _, _, _, _, _).
	

init_chart :-
	reset_global_counters,
	retractall(word(_,_,_,_,_)),
	retractall(grail_heap(_,_)),
	retractall(stored(_,_,_,_,_,_)),
	retractall(key_index(_,_)).

item_in_chart(Item, RefIndex) :-
	item_stored(Item, ItemIndex),
    (
        ItemIndex =< RefIndex
    ->
        true
    ;
        !,
        fail
    ).

item_in_chart(Item) :-
	item_stored(Item, _).

add_item_to_chart(Index) :-
	notify_chart_addition(Index).

is_empty_agenda(queue(Front, Back)) :-
	Front >= Back.

empty_agenda(queue(0,0)).

pop_agenda(queue(Front,Back), Front, queue(NewFront, Back)) :-
	Front < Back,
	NewFront is Front + 1.

add_item_to_agenda(Item, queue(Front,Back), queue(Front, NewBack)) :-
	Item = item(F, I, J, Data),
	notify_agenda_addition(Item),
    (
        \+ subsumed_item(Item)
    ->
        simplify_formula(F, SF),
        simplified_formula_to_key(SF, Key),
        assertz(stored(Back, Key, I, J, F, Data)),
        assert(key_index(Key, Back)),
        NewBack is Back + 1
    ;
        NewBack = Back
    ).

add_items_to_agenda([], Agenda, Agenda).
add_items_to_agenda([Item|Items], Agenda0, Agenda) :-
	add_item_to_agenda(Item, Agenda0, Agenda1),
	add_items_to_agenda(Items, Agenda1, Agenda).

index_to_item(Index, item(F, I, J, Sem)) :-
	stored(Index, _, I, J, F, Sem).

final_item(item(Start,0,Length,D), appl(SemI, Sem)) :-
	sentence_length(Length),
	has_empty_stack(D),
	get_data_semantics(D, Sem),
	startsymbol(Start, SemI).

simplified_formula_to_key(SimplifiedFormula, Key) :-
	term_hash(SimplifiedFormula, Key).

% = application rules

inference(dr, [item(dr(M,X,Y), I, J, Data1), item(Y, J, K, Data2)],
	       item(X, I, K, Data),
               [application_r(M, Data1, Data2, Data),check_wrap(M, Data)]).
inference(dl, [item(Y, I, J, Data1), item(dl(M,Y,X), J, K, Data2)],
	       item(X, I, K, Data),
               [application_l(M, Data2, Data1, Data)]).


% = rules for skipping interpunction symbols

inference(let, [item(X, I, J, Data),item(lit(let), J, K, _)],
	       item(X, I, K, Data),
	       [J is I+1]).  % prevent "attachment ambiguity"
inference(let, [item(lit(let), 0, I, _),item(X, I, J, Data)],
	       item(X, 0, J, Data),
	       []).

% = wrapping rules

inference(wr, [item(X, I, J, Data1), item(dl(1,V,W), J, K, Data2)],
	       item(X, I, K, Data),
               [J is I+1,wrap(dl(1,V,W), Data1, Data2, Data)]).
%inference(wl, [item(dl(1,V,W), I, J, Data1), item(X, J, K, Data2)],
%	       item(X, I, K, Data),
%               [J is I+1,wrap(dl(1,V,W), Data1, Data2, Data)]).
inference(wpop, [item(X, I, J, Data0)],
	        item(Y, I, J, Data),
	       [pop(dl(1,X,Y), Data0, Data)]).
inference(wpop_vp, [item(dl(0,lit(np(_,_,_)),lit(s(_))), I, J, Data0)],
	        item(dl(1,s,s), I, J, Data),
	       [pop_vp(Data0, Data)]).

% = special case for reported speech of the form "SENT, a dit NP"

inference(a_dit, [item(dr(0,dr(0,lit(s(X)),lit(np(_,_,_))),dl(0,lit(np(A,B,C)),lit(s(ppart)))), I, J, Data1),
		  item(dl(1,lit(s(X)),dl(0,lit(np(A,B,C)),lit(s(ppart)))), J, K, Data2)],
	          item(dr(0,dl(1,lit(s(X)),lit(s(X))),lit(np(A,B,C))), I, K, Data),
	         [a_dit(Data1, Data2, Data)]).
%inference(say, [item(dl(1,lit(s(S)),dr(0,X,Y)), I, J, Data1), item(Y, J, K, Data2)],
%	        item(dl(1,lit(s(S)),X), I, K, Data,

% = right-extraction rules

inference(e_start, [item(_,_,K,data(_,_,_,_,[B|Bs],_)),item(dr(0,X,Y), I, J, Data0)],
		   item(X, I, J, Data),
		   [K=<I,X\=lit(pp(_)),X\=dl(1,lit(s(_)),lit(s(_))),X\=dr(0,lit(s(_)),lit(s(_))),memberchk(Y,[B|Bs]),start_extraction(Y, Data0, Data)]).
inference(e_end, [item(dr(0,X,dr(0,Y,dia(1,box(1,Z)))), I, J, Data0), item(Y, J, K, Data1)],
	         item(X, I, K, Data),
	         [end_extraction(Z, Data0, Data1, Data)]).

% data(Sem, Prob, Set)

check_wrap(dl(0,lit(np(_,_,_)),lit(s(_))), Data) :-
	!,
	Data = data(_, _, _, [], _, _).
check_wrap(lit(s(_)), Data) :-
	!,
	Data = data(_, _, _, [], _, _).
check_wrap(_, _).

has_empty_stack(data(_, _, _, [], [], [])).



application_l(M, data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
	         data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
	         data(p(M,Pros2,Pros1), appl(Sem1,Sem2), Prob, SetA, SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	append(SetB0, SetB1, SetB),
	append(SetC0, SetC1, SetC).
application_r(M, data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
	         data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
	         data(p(M,Pros1,Pros2), appl(Sem1,Sem2), Prob, SetA, SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	append(SetB0, SetB1, SetB),
	append(SetC0, SetC1, SetC).

a_dit(data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
      data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
      data(p(0,Pros1,Pros2), lambda(NP, lambda(S, appl(appl(Sem1,appl(Sem2,S)),NP))), Prob, SetA, SetB, SetC)) :-
        Prob is Prob1 * Prob2,
      	append(SetA0, SetA1, SetA),
	append(SetB0, SetB1, SetB),
        append(SetC0, SetC1, SetC).

% = wrapping

wrap(dl(1,V,W), data(Pros1, Sem, Prob1, SetA0, SetB0, SetC0),
                   data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
                   data(p(1,Pros1,Pros2), Sem, Prob, [dl(1,V,W)-Sem2|SetA], SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	append(SetB0, SetB1, SetB),
	append(SetC0, SetC1, SetC).

push(Item, data(Pros, Sem, Prob, SetA, SetB, SetC), data(Pros, Sem, Prob, [Item|SetA], SetB, SetC)).

pop(X, data(Pros, Sem, Prob, SetA0, SetB, SetC), data(Pros, appl(Sem0,Sem), Prob, SetA, SetB, SetC)) :-
	select(X-Sem0, SetA0, SetA),
	!.
pop_vp(data(Pros, Sem, Prob, SetA0, SetB, SetC), data(Pros, lambda(X,appl(Sem0,appl(Sem,X))), Prob, SetA, SetB, SetC)) :-
	select(dl(1,s,s)-Sem0, SetA0, SetA).

% = extraction

start_extraction(Y, data(Pros, Sem, Prob, SetA, SetB, SetC), data(Pros, appl(Sem,X), Prob, SetA, SetB, [Y-X|SetC])).

end_extraction(Y, data(Pros0, Sem0, Prob0, SetA0, SetB0, SetC0),
                  data(Pros1, Sem1, Prob1, SetA1, SetB1, SetC1),
	          data(p(0,Pros0,Pros1), appl(Sem0,lambda(X,Sem1)), Prob, SetA, SetB, SetC)) :-
	select(Y-X, SetC1, SetC2),
	select(X, SetB0, SetB2),
	!,
	Prob is Prob0 * Prob1,
	append(SetA0, SetA1, SetA),
	append(SetB1, SetB2, SetB),
	append(SetC0, SetC2, SetC).

:- dynamic verbose/0.

verbose.

notify_consequence(RuleName, Trigger, Others, SideConds, Consequent) :-
    (
        verbose
    ->
        format(' ~p:~n     trigger: ~p~n', [RuleName, Trigger]),
        format('     others: ~p~n', [Others]),
        format('     side conds: ~p~n', [SideConds]),
        format('     cons: ~p~n', [Consequent])
    ;
        true
    ).

notify_agenda_addition(Item) :-
    (
         verbose
    ->
         format('~NAdding to agenda: <-> ~p~n', [Item])
    ;
         print('.')
    ).

notify_chart_addition(Index) :-
    (
        verbose
    ->
	index_to_item(Index, item(Formula,I,J,Data)),
        simplify_formula(Formula, SForm),
        simplified_formula_to_key(SForm, Key),
        format('~NAdding to chart: <~p-~p-~p> ~p-~p~n', [Key,I,J,Formula,Data])
    ;
        print(':')
    ).


create_data(Pros, Form, Prob, Sem, _I, _J, data(Pros, Sem, Prob, [], Set, [])) :-
	find_extractions(Form, Set, []).


find_all_extractions([], Set, Set).
find_all_extractions([X|Xs], Set0, Set) :-
	find_extractions(X, Set0, Set1),
	find_all_extractions(Xs, Set1, Set).

find_extractions(dr(0,X,dia(I,box(I,Y))), [Y|Set0], Set) :-
	!,
	find_extractions(X, Set0, Set).
find_extractions(dr(_,X,Y), Set0, Set) :-
	!,
	find_extractions(X, Set0, Set1),
	find_extractions(Y, Set1, Set).
find_extractions(dl(_,Y,X), Set0, Set) :-
	!,
	find_extractions(Y, Set0, Set1),
	find_extractions(X, Set1, Set).
find_extractions(_, Set, Set).

find_extractions_neg(dr(_,X,Y), Set0, Set) :-
	!,
	find_extractions(X, Set0, Set1),
	find_extractions(Y, Set1, Set).
find_extractions_neg(dl(_,Y,X), Set0, Set) :-
	!,
	find_extractions(Y, Set0, Set1),
	find_extractions(X, Set1, Set).
find_extractions_neg(_, Set, Set).

% macro(pp_dans, lit(pp(dans))).
% macro(pp_vers, lit(pp(vers))).
% macro(pp_entre, lit(pp(entre))).
% macro(pp_comme, lit(pp(comme))).
% macro(pp_contre, lit(pp(contre))).
% macro(pp_avec, lit(pp(avec))).
% macro(pp_sans, lit(pp(sans))).
% macro(pp_sous, lit(pp(sous))).
% macro(pp_de, lit(pp(de))).
% macro(pp_a, lit(pp(a))).
% macro(pp, lit(pp(_))).

% macro(s_inf, lit(s(inf))).
% macro(s_deinf, lit(s(deinf))).
% macro(s_ainf, lit(s(ainf))).
% macro(s_ppres, lit(s(ppres))).
% macro(s_ppart, lit(s(ppart))).
% macro(dr(0,s,s), dr(0,lit(s(X)),lit(s(X)))).
% macro(dl(1,s,s), dl(1,lit(s(X)),lit(s(X)))).
% macro(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np)))),dr(0,dl(0,np,lit(s(X))),dr(0,dl(0,np,lit(s(X))),dia(1,box(1,np))))).
% macro(dr(0,dr(0,dl(0,np,s),dl(0,np,s)),n),dr(0,dr(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(X)))),n)).
% macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)),dr(0,dl(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(X)))),dl(0,np,lit(s(_))))).
% macro(dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(X))))).
% macro(dr(0,s,dia(1,box(1,np))), dr(0,lit(s(main)),dia(1,box(1,np)))).
% macro(s, lit(s(_))).

%user:translate_form(dr(0,dl(0,pp,pp),pp),dr(0,dl(0,lit(pp(X)),lit(pp(X))),lit(pp(_)))).
%user:translate_form(dl(0,np,s),dl(0,np,lit(s(main)))).
%user:translate_form(dr(0,dl(0,np,s),np),dr(0,dl(0,np,lit(s(main))),np)).
%user:translate_form(dr(0,dl(0,np,s),dl(0,np,s_inf)),dr(0,dl(0,np,lit(s(main))),dl(0,np,lit(s(inf))))).
%user:translate_form(dr(0,dl(0,dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,dr(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(Y)))),dr(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(Y))))),dr(0,dl(0,np,lit(s(X))),dl(0,np,lit(s(Y)))))).
%user:translate_form(dr(0,dl(0,dr(0,pp,np),dl(0,n,n)),dr(0,s,dia(1,box(1,pp)))),dr(0,dl(0,dr(0,lit(pp(P)),np),dl(0,n,n)),dr(0,s,dia(1,box(1,lit(pp(P))))))).
%user:translate_form(s, lit(s(_))).
%user:translate_form(np, lit(np(_,_,_))).

simplify_formula(X, X) :-
	var(X),
	!.
simplify_formula(lit(s(pass)), s_pass) :- !
simplify_formula(lit(s(ppart)), s_ppart) :- !
simplify_formula(lit(s(ppres)), s_ppres) :- !
simplify_formula(lit(s(inf)), s_inf) :- !
simplify_formula(lit(s(ainf)), s_ainf) :- !
simplify_formula(lit(s(deinf)), s_deinf) :- !
simplify_formula(lit(s(_)), s) :- !.
simplify_formula(lit(pp(_)), pp) :- !.
simplify_formula(lit(np(_,_,_)), np) :- !.
simplify_formula(lit(A), A).
simplify_formula(dl(I,A0,B0), dl(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dr(I,A0,B0), dr(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(p(I,A0,B0), p(I,A,B)) :-
	simplify_formula(A0, A),
	simplify_formula(B0, B).
simplify_formula(dia(I,A0), dia(I,A)) :-
	simplify_formula(A0, A).
simplify_formula(box(I,A0), box(I,A)) :-
	simplify_formula(A0, A).
