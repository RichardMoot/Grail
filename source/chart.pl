% -*- Mode: Prolog -*-

:- module(chart, [grail_chart/1,
		  chart_parse_all/0,
		  chart_parse_all/1,
		  chart_parse_all_dl/1,
		  chart_parse_all/2,
		  chart_parse/2,
		  export/0,
		  prob_parse/2]).

:- use_module(lexicon, [macro_expand/2,get_item_semantics/5]).
:- use_module(heap, [empty_heap/1,add_to_heap/4,get_from_heap/4]).
:- use_module(prob_lex, [list_atom_term/2,list_atom_term/3,remove_brackets/2]).
:- use_module(sem_utils, [substitute_sem/3]).
:- use_module(latex, [latex_proof/2,latex_header/1,latex_tail/1]).
:- use_module(options, [get_option/2]).

:- dynamic sentence_length/1, total_formulas/1, unparsed/3, parsed/2.
:- dynamic word/5, stored/6, max_queue_size/1, grail_heap/2, justify/2.

default_depth_limit(25000).
output_natural_deduction_proofs(true).

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
	listing(justification),
	told.

% = parse all sentences.

chart_parse_all :-
	default_depth_limit(DLimit),
	chart_parse_all(1, DLimit).

% = parse all sentences with a given depth limit DL

chart_parse_all_dl(DL) :-
	chart_parse_all(1, DL).

% = parse all sentences starting with SentNo

chart_parse_all(SentNo) :-
	default_depth_limit(DLimit),
	chart_parse_all(SentNo, DLimit).

chart_parse_all(SentNo, DL) :-
	shell('rm chart*.dot', _),
	retractall(unparsed(_,_,_)),
	new_output_file(grail_log, log),
	new_output_file(unparsed, unparsed),
	new_output_file(parse_logs, plog),
	new_output_file('proof.tex', proof),
	new_output_file('proofs.pl', pl_proof),
	set_global_counter('$CHART_CURRENT', 0),
	set_global_counter('$CHART_FAIL', 0),
	set_global_counter('$CHART_LIMIT', 0),
	set_global_counter('$CHART_ALL', 0),
	print_grail_semantics_header,
	chart_parse_all0(SentNo, DL),
	/* cleanup after failure-driven loop has parsed all sentences */
	'$CHART_ALL'(ALL),
	'$CHART_FAIL'(FAIL),
	'$CHART_LIMIT'(LIMIT),
	retractall('$CHART_ALL'(_)),
	retractall('$CHART_FAIL'(_)),
	retractall('$CHART_LIMIT'(_)),
	Success is (ALL - FAIL) - LIMIT,
	SPercentage is (100*Success)/ALL,
	FPercentage is (100*FAIL)/ALL,
	LPercentage is (100*LIMIT)/ALL,
	format('Finished parsing~n~w total sentences~n~w sentences succeeded (~w %)~n~w sentences failed (~w %)~n~w resource limits (~w %)~n', [ALL,Success,SPercentage,FAIL,FPercentage,LIMIT,LPercentage]),
	print_grail_semantics_tail,
	close(unparsed),
	close(proof),
	close(pl_proof),
	close(plog),
	close(log).

chart_parse_all0(N, DL) :-
	clause(user:sent(N0,_),_),
    (
        /* fail for sentence numbers smaller than N */
        N0 >= N
    ->
        increase_global_counter('$CHART_ALL'),
        set_global_counter('$CHART_CURRENT', N0)
    ),
	format('~nStarting: ~w~n', [N0]),
	statistics(process_cputime, CPU0),
	statistics(inferences, Inferences0),
    (
	call_with_depth_limit(user:sent(N0, Result), DL, DepthLimit)
    ->
        (
	   DepthLimit = depth_limit_exceeded
	->
	   try_recover_chart_semantics(N0, DL, Inferences0, CPU0)
	 ;
	   print_statistics('S', N0, DepthLimit, Inferences0, CPU0),
	   print_grail_semantics(Result),
	   assert(parsed(N0, Result)),
	   format('~nSuccess: ~w (~w)~n', [N0,DepthLimit])
	)
    ;
        print_statistics('F', N0, DL, Inferences0, CPU0),
	print_chart(N0),
	assert('unparsed'(N0,Length,fail)),
        portray_clause(unparsed, unparsed(N0,Length,fail)),
        increase_global_counter('$CHART_FAIL'),
	format('~nFailure: ~w~n', [N0])
    ),
        fail.
chart_parse_all0(_, _).

print_statistics(State, N0, DepthLimit, Inferences0, CPU0) :-
	   statistics(inferences, Inferences1),
	   statistics(process_cputime, CPU1),
	   CPU is CPU1 - CPU0,
	   Inferences is Inferences1 - Inferences0,
	   sentence_length(Length),
	   total_formulas(TotalForms),
	   max_queue_size(MaxQ),
	   format(plog, '~w\t~w\t~w\t~w\t~w\t~2f\t~w\t~w~n', [N0, State, Length, TotalForms, DepthLimit, CPU, Inferences, MaxQ]).

print_sed_command(Stream) :-
	findall(X, parsed(X,_), L),
	write(Stream, 'sed -e\''),
	print_sed_command(L, Stream),
	write(Stream, '\''),
	nl(Stream).

print_sed_command([], _).
print_sed_command([X|Xs], Stream) :-
	print_sed_command(Xs, X, Stream).

print_sed_command([], X, Stream) :-
	format(Stream, '~wd', [X]).
print_sed_command([X|Xs], X0, Stream) :-
	format(Stream, '~wd;', [X0]),
	print_sed_command(Xs, X, Stream).

try_recover_chart_semantics(N0, DL, Inferences, CPU) :-
	final_item(Goal, Sem),
	item_in_chart(Goal, Index),
	compute_proof(Index),
	!,
       	increase_global_counter('$SOLUTION'),
	assert(parsed(N0, Sem)),
	print_grail_semantics(Sem),
	format('~nSuccess: ~w (MAX)~n', [N0]),
        print_statistics('S', N0, DL, Inferences, CPU).
try_recover_chart_semantics(N0, DL, Inferences, CPU) :-
	increase_global_counter('$CHART_LIMIT'),
	assert('unparsed'(N0,Length,depth_limit)),
        portray_clause(unparsed, unparsed(N0,Length,depth_limit)),
	format('~nDepth limit exceeded: ~w~n', [N0]),
        print_statistics('L', N0, DL, Inferences, CPU).

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
startsymbol(lit(n), lambda(N,merge(drs([variable(X)],[]),appl(N,X)))).
startsymbol(dl(0,lit(np(_,_,_)),lit(s(_))), lambda(VP,merge(drs([event(E),variable(X)],[appl(generic,X)]),appl(appl(VP,lambda(P,appl(P,X))),E)))).
startsymbol(dl(0,lit(n),lit(n)), lambda(ADJ,merge(drs([variable(X)],[]),appl(appl(ADJ,lambda(_,drs([],[]))),X)))).
startsymbol(dr(0,lit(s),lit(s)), lambda(ADV,merge(drs([event(E)],[bool(E,=,'event?')]),appl(appl(ADV,lambda(_,drs([],[]))),E)))).
startsymbol(dr(0,lit(s(_)),lit(s(_))), lambda(ADV,merge(drs([event(E)],[bool(E,=,'event?')]),appl(appl(ADV,lambda(_,drs([],[]))),E)))).
startsymbol(lit(let), lambda(_,drs([],[]))).

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
	init_chart,
	empty_heap(Heap),
	create_chart(Ws, POSs, Lemmas, FPs, 0, Heap, 0, _MaxVar, SemInfo, [], Chart, []),
    (
	chart_parse(Chart, Semantics0)
    ->
        chart_semantics(SemInfo, Semantics0, Semantics),
        print_grail_semantics(Semantics),
        print_grail_semantics_tail
    ;
        true
    ),
        /* output final chart for debugging */
        export,
	finished_dialog,
	halt.

chart_semantics(SemInfo, Semantics0, Semantics) :-
	compute_semantics(SemInfo, Subst),
        substitute_sem(Subst, Semantics0, Semantics).	

compute_semantics([], []).
compute_semantics([IN-t(W,PosTT,Lemma,F)|Rest0], [IN-Sem|Rest]) :-
	get_item_semantics(W, PosTT, Lemma, F, Sem),
	compute_semantics(Rest0, Rest).

print_grail_semantics_header :-
	open_semantics_files,
	get_option(paper_size, PaperSize),
	latex_header(sem, PaperSize).

print_grail_semantics(Sem) :-
	numbervars(Sem, 0, _),
	reduce_sem(Sem, RSem),
	format('~nSemantics   : ~p~n', [Sem]),
	format('Reduced Sem : ~p~n', [RSem]),
	format(log, '~n% = Semantics~2n ~W~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(log, '% = Reduced Semantics~2n~W~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '~n% = Semantics~2n ~W.~2n', [Sem,[numbervars(true),quoted(true)]]),
	format(sem_pl, '% = Reduced Semantics~2n~W.~2n', [RSem,[numbervars(true),quoted(true)]]),
	format(sem, '~n\\begin{multline}~n', []),
   (
        user:display_unreduced_semantics(yes)
   ->
	latex_semantics(Sem, Formula, sem),
	format(sem, '\\rightarrow_{\\beta}\\\\ ', [])
   ;
        true
   ),
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

create_chart([], [], [], [], N, H, V, V, S, S) -->
	{retractall(sentence_length(_)),
	 assert(sentence_length(N))},
	add_heap_to_chart(H).
% skip final punctuation if its formula is "boring"
create_chart([_], [PUN], [_], [FP], N, H, V, V, S, S) -->
	{ is_punct(PUN),
          boring(FP) ,
	  retractall(sentence_length(_)),
	  assert(sentence_length(N)) },
        !,
	add_heap_to_chart(H).
create_chart([W|Ws], [P|Ps], [L|Ls], [FP|FPs], N0, H0, V0, V, S0, S) -->
	{ N is N0 + 1,
	  assert(word(W,P,L,N0,N))},
	append_item_and_update_heap(FP, W, P, L, N0, N, V0, V1, S0, S1, H0, H),
	create_chart(Ws, Ps, Ls, FPs, N, H, V1, V, S1, S).
	
prob_parse(List, Result) :-
	check_log_stream,
	init_chart,
	empty_heap(Heap),
	list_to_chart(List, 0, Heap, Chart, [], 0, _V, SemInfo, []),
	chart_parse(Chart, Result0),
	chart_semantics(SemInfo, Result0, Result).

lemma_sequence([], N, N).
lemma_sequence([L|Ls], N0, N) :-
	word(_, _, L, N0, N1),
	lemma_sequence(Ls, N1, N).


list_to_chart([], N, H, As0, As, V, V, S, S) :-
	retractall(sentence_length(_)),
	assert(sentence_length(N)),
	add_heap_to_chart(H, As0, As).
list_to_chart([si(_, PUN, _, FPs)], N, H, As0, As, V, V, S, S) :-
	is_punct(PUN),
	boring(FPs),
	!,
	retractall(sentence_length(_)),
	assert(sentence_length(N)),
	add_heap_to_chart(H, As0, As).
list_to_chart([si(W,Pos,Lemma,FPs)|Ws], N0, H0, As0, As, V0, V, S0, S) :-
	N1 is N0 + 1,
	assert(word(W,Pos,Lemma,N0,N1)),
	append_item_and_update_heap(FPs, W, Pos, Lemma, N0, N1, V0, V1, S0, S1, H0, H, As0, As1),
	list_to_chart(Ws, N1, H, As1, As, V1, V, S1, S).

get_semantics(item(_, _, _, Data), Sem) :-
	get_data_semantics(Data, Sem).

get_data_semantics(data(_, Sem, _, _, _, _), Sem).

is_punct(ponct).
is_punct(pun).
is_punct(ponct-pun).

boring([]).
boring([Item-_|FPs]) :-
	boring_item(Item),
	!,
	boring(FPs).
boring([Item,_|FPs]) :-
	boring_item(Item),
	boring(FPs).

boring_item(let).
boring_item(dl(0,_,txt)).
boring_item(dl(0,_,lit(txt))).

append_items([], _, _, _, _, _, IN, IN, S, S) -->
	[].
append_items([F0-P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S) -->
	!,
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, IN1, S0, S1, Item)},
	[Item],
	append_items(FPs, W, Pos, Lemma, N0, N1, IN1, IN, S1, S).
append_items([F0,P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S) -->
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	 IN1 is IN0 + 1},
	[Item],
	append_items(FPs, W, Pos, Lemma, N0, N1, IN1, IN, S1, S).

append_item_and_update_heap([], _, _, _, _, _, _, S, S, H, H) -->
	[].
append_item_and_update_heap([F0-P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) -->
	!,
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	 IN1 is IN0 + 1},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H0, H)}.
append_item_and_update_heap([F0,P|FPs], W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) -->
	{create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	 IN1 is IN0 + 1},
	[Item],
	{update_heap(FPs, P, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H0, H)}.

enrich_formula(par, _, dr(0,lit(pp(par)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(par, _, dr(0,lit(pp(par)),lit(n))) :-
	!.
enrich_formula(pour, _, dr(0,lit(pp(pour)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(pour, _, dr(0,lit(pp(pour)),lit(n))) :-
	!.
enrich_formula(contre, _, dr(0,lit(pp(contre)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(contre, _, dr(0,lit(pp(contre)),lit(n))) :-
	!.
enrich_formula(sous, _, dr(0,lit(pp(sous)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(sous, _, dr(0,lit(pp(sous)),lit(n))) :-
	!.
enrich_formula(sur, _, dr(0,lit(pp(sur)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(sur, _, dr(0,lit(pp(sur)),lit(n))) :-
	!.
enrich_formula(en, _, dr(0,lit(pp(en)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(en, _, dr(0,lit(pp(en)),lit(n))) :-
	!.
enrich_formula(de, _, dr(0,lit(pp(de)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(de, _, dr(0,lit(pp(de)),lit(n))) :-
	!.
enrich_formula(avant, _, dr(0,lit(pp(avant)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(avant, _, dr(0,lit(pp(avant)),lit(n))) :-
	!.
enrich_formula(après, _, dr(0,lit(pp(après)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(après, _, dr(0,lit(pp(après)),lit(n))) :-
	!.
enrich_formula(L, _, dr(0,lit(pp(L)),lit(np(acc,_,_)))) :-
	!.
enrich_formula(L, _, dr(0,lit(pp(L)),lit(n))) :-
	!.
enrich_formula(_, _, _).

correct_formula(ver:impe, dr(0, lit(s(S)),lit(np(nom,A,B))), dr(0, lit(s(S)), lit(np(acc,A,B)))) :-
	!.
correct_formula(ver:impe, dr(0, dr(0, lit(s(S)),lit(np(nom,A,B))), lit(pp(P))), dr(0, dr(0, lit(s(S)), lit(np(acc,A,B))), lit(pp(P)))) :-
	!.
correct_formula(ver:impe, dr(0, dr(0, lit(s(S)), lit(s(Q))), lit(np(nom,A,B))), dr(0, dr(0, lit(s(S)), lit(s(Q))), lit(np(acc,A,B)))) :-
	!.
correct_formula(_, F, F).

update_heap([], _, _, _, _, _, _, IN, IN, S, S, H, H).
update_heap([F0-P|FPs], PMax, W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) :-
	!,
	create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	IN1 is IN0 + 1,
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H1, H).
update_heap([F0,P|FPs], PMax, W, Pos, Lemma, N0, N1, IN0, IN, S0, S, H0, H) :-
	create_item(F0, P, W, Pos, Lemma, N0, N1, IN0, S0, S1, Item),
	IN1 is IN0 + 1,
	Key is PMax/P,
	add_to_heap(H0, Key, Item, H1),
	update_heap(FPs, PMax, W, Pos, Lemma, N0, N1, IN1, IN, S1, S, H1, H).

create_item(F0, P, W, Pos, Lemma, N0, N1, IN, [IN-t(W,PosTT,Lemma,F)|Ss], Ss, item(F, N0, N1, Data)) :-
	macro_expand(F0, F1),
	get_pos_tt(Pos, PosTT),
	enrich_formula(Lemma, PosTT, F1),
	correct_formula(PosTT, F1, F),
	create_data(W, F, P, '$VAR'(IN), N0, N1, Data).


get_pos_tt(Pos, PosTT) :-
      (
         Pos = _Melt0-Pos0:Sub
      ->
         PosTT = Pos0:Sub
      ;
         Pos = _Melt1-PosTT
      ->
         true
      ;
         PosTT = Pos
      ).

% = 

chart_parse(Axioms, Sem) :-
	init_agenda(Axioms, Agenda),
	exhaust(Agenda),
    (
        /* succeed for empty sentences (eg. interpunction only) */
        Agenda = queue(0,0)
    ->
        Sem = drs([],[])
    ;
	check_solution(Sem)
    ).

check_solution(Sem) :-
	final_item(Goal, Sem),
	item_in_chart(Goal, Index),
	compute_proof(Index),
       	increase_global_counter('$SOLUTION').
	

init_agenda(Axioms, Agenda) :-
	empty_agenda(Empty),
	add_axioms_to_agenda(Axioms, Empty, Agenda),
	Agenda = queue(_, TotalForms),
	retractall(total_formulas(_)),
	assert(total_formulas(TotalForms)).

exhaust(queue(Front, Back)) :-
	Front >= Back,
	!,
	retractall(max_queue_size(_)),
	assert(max_queue_size(Front)).
% = iterative deepening style exhaustion
% RM: Commented out for now, doesn't appear to do anything.
%exhaust(queue(Front, Max)) :-
%	Front > 499,
%	X is Front mod 500,
%	X =:= 0,
%	final_item(Goal, _Sem),
%	item_in_chart(Goal),
%	!,
%	retractall(max_queue_size(_)),
%	assert(max_queue_size(Max)).
exhaust(Agenda0) :-
	pop_agenda(Agenda0, Index, Agenda1),
	add_item_to_chart(Index),
	add_consequences_to_agenda(Index, Agenda1, Agenda),
	exhaust(Agenda).

add_consequences_to_agenda(Index, Agenda0, Agenda) :-
	findall(Consequence-Justification,
		consequence(Index, Consequence, Justification),
		Consequences),
	add_items_to_agenda(Consequences, Agenda0, Agenda).

consequence(Index, Consequent, Justification) :-
	index_to_item(Index, Trigger),
	matching_rule(Trigger, RuleName, Others, Consequent, SideConds),
	items_in_chart(Others, Index, Indices),
	hold(SideConds),
	Justification =.. [RuleName,Index|Indices],
	notify_consequence(RuleName, Trigger, Others, SideConds, Consequent).

items_in_chart([], _MaxIndex, []).
items_in_chart([Antecedent|Antecedents], MaxIndex, [Index|Indices]) :-
	item_in_chart(Antecedent, MaxIndex, Index),
	items_in_chart(Antecedents, MaxIndex, Indices).

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

%:- index(stored(1,1,1,1,0,0)).

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
subsumed_item(Item, Front) :-
	similar_item(Item, OtherItem, IndexofSimilar),
	subsumes_item(OtherItem, Item, IndexofSimilar, Front).

subsumes_item(item(F0, I0, J0, Data0), item(F, I, J, Data1), IndexofSimilar, Front) :-
	subsumes_chk(F0, F),
	subsumes_chk(I0, I),
	subsumes_chk(J0, J),
	subsumes_data(Data0, Data1, O),
	keep_most_probable(O, IndexofSimilar, F0, F, I0, I, J0, J, Data0, Data1, Front).


subsumes_data(data(_,_,Prob0,A0,B0,C0),data(_,_,Prob,A,B,C), O) :-
	subsumes_chk(A0, A),
	subsumes_chk(B0, B),
	subsumes_chk(C0, C),
	compare(O, Prob0, Prob).

% TODO: test
% If the old value is *identical* to the new one, but the probability is lower,
% then erase the old value and fail the subsumption test. Otherwise, succeed.

keep_most_probable(<, IndexofSimilar, F0, F, I0, I, J0, J, Data, BetterData, Front) :-
	F0 == F,
	I0 == I,
	J0 == J,
	!,
	Data = data(_,Sem0,Prob0,_,_,_),
	BetterData = data(_,Sem1,Prob,_,_,_),
	numbervars(Sem0, 0, _),
	reduce_sem(Sem0, RSem0),
	numbervars(Sem1, 0, _),
	reduce_sem(Sem1, RSem1),
	/* delete the previous item */
	retract(stored(IndexofSimilar, H, I, J, F, Data)),
   (
        verbose
   ->
	format('REPLACED (~w < ~w): ~w~nDATA:~p~nBETTER DATA:~p~n', [Prob0, Prob, IndexofSimilar,RSem0,RSem1])
   ;
        true
   ),
        /* if the old item was on the agenda, replace it with the new item */
   (
        IndexofSimilar >= Front
   ->
	assertz(stored(IndexofSimilar, H, I, J, F, BetterData)),
        fail
  ;
        /* add the new item to the agenda */
        true
   ).
keep_most_probable(=, _, _, _, _, _, _, _, _, _, _).
keep_most_probable(>, _, _, _, _, _, _, _, _, _, _).
	

init_chart :-
	reset_global_counters,
	retractall(justification(_,_)),
	retractall(word(_,_,_,_,_)),
	retractall(grail_heap(_,_)),
	retractall(stored(_,_,_,_,_,_)),
	retractall(key_index(_,_)).

item_in_chart(Item, RefIndex, ItemIndex) :-
	item_stored(Item, ItemIndex),
    (
        ItemIndex =< RefIndex
    ->
        true
    ;
        !,
        fail
    ).

item_in_chart(Item, ItemIndex) :-
	item_stored(Item, ItemIndex).

item_in_chart(Item) :-
	item_stored(Item, _).

add_item_to_chart(Index) :-
	notify_chart_addition(Index).

empty_agenda(queue(0,0)).

pop_agenda(queue(Front,Back), Front, queue(NewFront, Back)) :-
	Front < Back,
	NewFront is Front + 1.

% = update_data
%
% add extraction information to Bs

update_data(data(Pros, Sem, Prob, As, Bs, Cs), _, _, _, data(Pros, Sem, Prob, As, Bs, Cs)).


add_item_to_agenda(Item0, Justification, queue(Front,Back), queue(Front, NewBack)) :-
	Item0 = item(F, I, J, Data0),
        update_data(Data0, I, J, F, Data),
	Item = item(F, I, J, Data),
	notify_agenda_addition(Item),
    (
        \+ subsumed_item(Item, Front)
    ->
        simplify_formula(F, SF),
        simplified_formula_to_key(SF, Key),
        assertz(stored(Back, Key, I, J, F, Data)),
        assert(key_index(Key, Back)),
        assertz(justification(Back, Justification)),
        NewBack is Back + 1
    ;
        NewBack = Back
    ),
        notify_agenda_size(Front, NewBack).

add_items_to_agenda([], Agenda, Agenda).
add_items_to_agenda([Item-Justification|Items], Agenda0, Agenda) :-
	!,
	add_item_to_agenda(Item, Justification, Agenda0, Agenda1),
	add_items_to_agenda(Items, Agenda1, Agenda).

add_axioms_to_agenda([], Agenda, Agenda).
add_axioms_to_agenda([Item|Items], Agenda0, Agenda) :-
	add_item_to_agenda(Item, axiom, Agenda0, Agenda1),
	add_axioms_to_agenda(Items, Agenda1, Agenda).


index_to_item(Index, item(F, I, J, Sem)) :-
	stored(Index, _, I, J, F, Sem).

final_item(item(Start,0,Length,D), appl(SemI, Sem)) :-
	sentence_length(Length),
	has_empty_stack(D),
	get_data_semantics(D, Sem),
	startsymbol(Start, SemI).

simplified_formula_to_key(SimplifiedFormula, Key) :-
	term_hash(SimplifiedFormula, Key).


% = compute_proof

new_proof_index(I, J, K) :-
	proof_index(I0),
	I is I0 + 1,
	retractall(proof_index(_)),
	assert(proof_index(I)),
	assert(proof_index_right(I, J, K)).


compute_proof(Index) :-
     (
	 output_natural_deduction_proofs(true)
     ->
         compute_proof1(Index)
     ;
         true
     ).
	      
compute_proof1(Index) :-
	check_proof_stream,
	check_prolog_proof_stream,
	retractall(proof_index(_)),
	assert(proof_index(-1)),
	retractall(proof_index_right(_,_,_)),
	retractall(proof_hole(_,_,_,_,_)),
	justification(Index, Just),
	Just =.. [Rule|Args],
	stored(Index, _, L, R, Formula, _),
	compute_proof(Rule, Args, L, R, _Ant, Formula, Proof),
	!,
	portray_clause(pl_proof, (proof(Proof) :- true)),
	format(proof, '~n\\begin{multline}~n', []),
	latex_proof(Proof, proof),
	format(proof, '~n\\end{multline}~2n', []).
compute_proof1(Index) :-
    (
	current_predicate('$CHART_CURRENT'/1)
    ->
        '$CHART_CURRENT'(CUR)
    ;
        CUR = 1
    ),
	format(proof, '% FAILED to compute proof for index ~w (~w)~n', [CUR,Index]),
	format(log, 'FAILED to compute proof for index ~w (~w)~n', [CUR,Index]),
	format('FAILED to compute proof for index ~w (~w)~n', [CUR,Index]).

compute_proof(axiom, [], L, R, leaf(L,R,W,POS,Lemma), F, rule(ax, leaf(L,R,W,POS,Lemma), F, [])) :-
	word(W, POS, Lemma, L, R).
compute_proof(dr, [A,B], I, K, p(0,I,K,WL,WR), F, rule(dr, p(0,I,K,WL,WR), F, [Left,Right])) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
	justification(A, JustA),
	justification(B, JustB),
	JustA =.. [RuleA|ArgsA],
	JustB =.. [RuleB|ArgsB],
    (
        I = AL, AR = BL, AR = J, K = BR
    ->
        FA = dr(_,F,FB),
        compute_proof(RuleA, ArgsA, I, J, WL, FA, Left),
        compute_proof(RuleB, ArgsB, J, K, WR, FB, Right)
    ;
        I = BL, BR = AL, BR = J, K = AR
    ->
        FB = dr(_,F,FA),
        compute_proof(RuleB, ArgsB, I, J, WL, FB, Left),
        compute_proof(RuleA, ArgsA, J, K, WR, FA, Right)     
    ).
compute_proof(dl, [A,B], I, K, p(0,I,K,WL,WR), F, rule(dl, p(0,I,K,WL,WR), F, [Left,Right])) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
	justification(A, JustA),
	justification(B, JustB),
	JustA =.. [RuleA|ArgsA],
	JustB =.. [RuleB|ArgsB],
    (
        I = AL, AR = BL, AR = J, K = BR
    ->
        FB = dl(_,FA,F),
        compute_proof(RuleA, ArgsA, I, J, WL, FA, Left),
        compute_proof(RuleB, ArgsB, J, K, WR, FB, Right)
    ;
        I = BL, BR = AL, BR = J, K = AR
    ->
        FA = dl(_,FB,F),
        compute_proof(RuleB, ArgsB, I, J, WL, FB, Left),
        compute_proof(RuleA, ArgsA, J, K, WR, FA, Right)     
    ).
compute_proof(let, [A,B], _I, _K, W, F, Proof) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
    (
        FA = lit(let)
    ->
        F = FB,
        justification(A, JustA),
        justification(B, JustB),
        JustA =.. [RuleA|ArgsA],
        JustB =.. [RuleB|ArgsB],
        compute_proof(RuleA, ArgsA, AL, AR, _, FA, _),
        compute_proof(RuleB, ArgsB, BL, BR, W, F, Proof)
    ;
        FB = lit(let)
    ->
        F = FA,
        justification(A, JustA),
        justification(B, JustB),
        JustA =.. [RuleA|ArgsA],
        JustB =.. [RuleB|ArgsB],
        compute_proof(RuleB, ArgsB, BL, BR, _, FB, _),
	compute_proof(RuleA, ArgsA, AL, AR, W, F, Proof)
    ).
compute_proof(wr, [A,B], I, K, W, F, Proof) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
    (
        /* A is right of and ajactent to B */
        BL = I, AR = K, BR = AL %, BR = J
    ->
        F = FB,
        justification(A, JustA),
        justification(B, JustB),
        JustA =.. [RuleA|ArgsA],
        JustB =.. [RuleB|ArgsB],
        compute_proof(RuleA, ArgsA, AL, AR, WA, FA, ProofA),
        compute_proof(RuleB, ArgsB, BL, BR, W, F, Proof),
        assert(proof_hole(AL, AR, WA, FA, ProofA))
    ;
        AL = I, BR = K, AR = BL %, AR = J
    ->
        F = FA,
        justification(A, JustA),
        justification(B, JustB),
        JustA =.. [RuleA|ArgsA],
        JustB =.. [RuleB|ArgsB],
	compute_proof(RuleA, ArgsA, AL, AR, W, F, Proof),
        compute_proof(RuleB, ArgsB, BL, BR, WB, FB, ProofB),
        assert(proof_hole(BL, BR, WB, FB, ProofB))
    ).
compute_proof(wpop_vp, [A], I, K, W, dl(0,lit(np(N1,N2,N3)),lit(s(S))), rule(dli(VI), W, dl(0,lit(np(N1,N2,N3)),lit(s(S))), [rule(dl, p(0,I,K,hyp(I,I,'$VAR'(VI)),W), lit(s(S)), [rule(dl, p(0,I,K,hyp(I,I,'$VAR'(VI)),WA), lit(s(S)), [rule(hyp(VI), hyp(I,I,'$VAR'(VI)), lit(np(N1,N2,N3)), []),ProofA]), ProofH])])) :-
	new_proof_index(VI, K, I),
	stored(A, _, I, K, dl(0,lit(np(N1,N2,N3)),lit(s(S))), _),
	justification(A, JustA),
	JustA =.. [RuleA|ArgsA],
	compute_proof(RuleA, ArgsA, I, K, WA, dl(0,lit(np(N1,N2,N3)),lit(s(S))), ProofA),
	proof_hole(I0, K0, WB, dl(1,lit(s(S)),lit(s(S))), ProofH),
	I0 >= I,
	K0 =< K,
	insert_pros(WA, I0, K0, WB, W).

compute_proof(wpop, [A], I, K, W, FA, rule(dl, W, FB, [ProofA, ProofH])) :-
	stored(A, _, I, K, FA, _),
	justification(A, JustA),
	JustA =.. [RuleA|ArgsA],
	compute_proof(RuleA, ArgsA, I, K, WA, FA, ProofA),
	proof_hole(I0, K0, WB, dl(1,FA,FB), ProofH),
	I0 >= I,
	K0 =< K,
	insert_pros(WA, I0, K0, WB, W).


compute_proof(e_start, [A,B], I, K, p(0, I, K, W, hyp(K,K,'$VAR'(V))), F, rule(dr, p(0,I,K,W,hyp(K,K,'$VAR'(V))), F, [Proof,rule(hyp(V), hyp(K,K,'$VAR'(V)), Arg, [])])) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
    (
        AL = I, AR = K
    ->
        FA = dr(0,F,Arg),
        BR = Ind,
	new_proof_index(V, Ind, K),
        justification(A, JustA),
        JustA =.. [RuleA|ArgsA],
        compute_proof(RuleA, ArgsA, I, K, W, FA, Proof)
    ;
        BL = I, BR = K
    ->
        FB = dr(0,F,Arg),
        AR = Ind,
	new_proof_index(V, Ind, K),
        justification(B, JustB),
        JustB =.. [RuleB|ArgsB],
        compute_proof(RuleB, ArgsB, I, K, W, FB, Proof)
    ).

compute_proof(e_end, [A,B], I, K, p(0,I,K,WL,WR), F, rule(dr,p(0,I,K,WL,WR), F, [Left,rule(dri(PIJ), WR, FR, [Right])])) :-
	stored(A, _, AL, AR, FA, _),
	stored(B, _, BL, BR, FB, _),
	justification(A, JustA),
	justification(B, JustB),
	JustA =.. [RuleA|ArgsA],
	JustB =.. [RuleB|ArgsB],
    (
        AL = I, AR = BL, AR = J, BR = K
    ->
        FR = dr(0,FB,dia(Ind,box(Ind,_))),
        FA = dr(0,F,FR),
        compute_proof(RuleA, ArgsA, I, J, WL, FA, Left),
        compute_proof(RuleB, ArgsB, J, K, WR0, FB, Right),
        proof_index_right(PIJ, J, PI),
        valid_extraction(Ind, PI, K),
        retract_hypothesis(PIJ, PI, WR0, WR)
    ;
        I = BL, BR = AL, BR = J, K = AR
     ->
        FR = dr(0,FA,dia(Ind,box(Ind,_))),
        FB = dr(0,F,FR),
        compute_proof(RuleB, ArgsB, I, J, WL, FB, Left),
        compute_proof(RuleA, ArgsA, J, K, WR0, FA, Right),      
        proof_index_right(PIJ, J, PI),
        valid_extraction(Ind, PI, K),
        retract_hypothesis(PIJ, PI, WR0, WR)
    ).


valid_extraction(0, R, R).
valid_extraction(1, _, _).

insert_pros(leaf(L,R,W,P,Lem), LWB, RWB, WB, p(0,L,RWB,leaf(L,R,W,P,Lem),WB)) :-
	LWB >= R.
insert_pros(p(0,L,R,A,B), C_L, C_R, C, Inserted) :-
        left_most(B, B_L),
     (
        C_R >= R
     ->
        Inserted = p(0,L,C_R,p(0,L,R,A,B),C)
     ;
        C_L =< B_L
     ->
        Inserted = p(0,L,R,NA,B),
        insert_pros(A, C_L, C_R, C, NA)
     ;
        /* B_L < C_L */
        Inserted = p(0,L,R,A,NB),
        insert_pros(B, C_L, C_R, C, NB)
     ).
      
insert_pros(LR0-W, LR, WB, p(0,LR0-W,WB)) :-
	LR0 =< LR.
insert_pros(p(0,A0,B0), LWB, WB, Inserted) :-
	right_most(B0, RB),
     (
        LWB >= RB
    ->
        Inserted=p(0,p(0,A0,B0),WB)
     ;
	right_most(A0, R),
    (
        LWB >= R
    ->
        Inserted = p(0,p(0,A0,WB),B0)
    ;
        LWB < R
    ->
        Inserted = p(0,A,B0),
        insert_pros(A0, LWB, WB, A)
    ;
        left_most(B0, L),
     (
        LWB < L
     ->
        Inserted = p(0,p(0,A0,WB),B0)
     ;
        Inserted = p(0,A0,B),
        insert_pros(B0, LWB, WB, B)
     ))).

right_most(hyp(_,R,_), R).
right_most(leaf(_,R,_,_,_), R).
right_most(p(_,_,R,_,_), R).

left_most(hyp(L,_,_), L).
left_most(leaf(L,_,_,_,_), L).
left_most(p(_,L,_,_,_), L).


retract_hypothesis(I, R, p(0, _, R, V, hyp(_,R,'$VAR'(I))), V) :-
	!.
retract_hypothesis(I, RI, p(0, L, R, V0, W0), Result) :-
	right_most(V0, VR),
    (
        RI =< VR
    ->
        Result = p(0, L, R, V, W0),
        retract_hypothesis(I, RI, V0, V)
    ;
        Result = p(0, L, R, V0, W),
        retract_hypothesis(I, RI, W0, W)
    ).
     
% = application rules

inference(dr, [item(dr(M,X,Y), I, J, Data1), item(Y, J, K, Data2)],
	       item(X, I, K, Data),
               [application_r(M, Data1, Data2, Data),check_wrap(Y, Data2), valid_extraction(Data, I)]).

inference(dl, [item(Y, I, J, Data1), item(dl(M,Y,X), J, K, Data2)],
	       item(X, I, K, Data),
               [check_islands(Y,Data2),application_l(M, Data2, Data1, Data),valid_extraction(Data, I)]).


% = rules for skipping interpunction symbols

inference(let, [item(X, I, J, Data1),item(lit(let), J, K, Data2)],
	         item(X, I, K, Data),
	         [J is I+1, combine_let(r,Data1,Data2,Data)]).  % prevent "attachment ambiguity"
inference(let, [item(lit(let), 0, I, Data2),item(X, I, J, Data1)],
	         item(X, 0, J, Data),
	         [combine_let(l,Data1,Data2,Data)]).

% = wrapping rules

% push the item on the stack

inference(wr, [item(X, I, J, Data1), item(dl(1,V,W), J, K, Data2)],
	       item(X, I, K, Data),
              [J is I+1, wrap(dl(1,V,W), J, K, Data1, Data2, Data)]).

inference(wpop, [item(X, I0, J0, Data0)],
	         item(Y, I, J, Data),
	        [pop(dl(1,X,Y), I0, J0, I, J, Data0, Data)]).

% special rule to allow an s-modifier to modify a vp

inference(wpop_vp, [item(dl(0,lit(np(A,B,C)),lit(s(S))), I0, J0, Data0)],
	            item(dl(0,lit(np(A,B,C)),lit(s(S))), I, J, Data),
	           [pop_vp(I0, J0, I, J, Data0, Data)]).

inference(wpop_vpi, [item(dl(0,lit(np(A,B,C)),lit(s(S))), J, K, Data0),item(dl(1,lit(s(S)),lit(s(S))),I,J,Data1)],
	             item(dl(0,lit(np(A,B,C)),lit(s(S))), I, K, Data),
	            [adv_vp(Data1, Data0, Data)]).

% = special case for reported speech of the form "SENT, a dit NP"

inference(a_dit, [item(dr(0,dr(0,lit(s(X)),lit(np(A,B,C))),dl(0,lit(np(A,B,C)),lit(s(INFL)))), I, J, Data1),
		  item(dl(1,lit(s(X)),dl(0,lit(np(A,B,C)),lit(s(INFL)))), J, K, Data2)],
	          item(dr(0,dl(1,lit(s(X)),lit(s(X))),lit(np(A,B,C))), I, K, Data),
	         [a_dit(Data1, Data2, Data)]).
%inference(a_np_dit, [item(dr(0,lit(s(X)),dl(0,lit(np(A,B,C)),lit(s(INFL)))), I, J, Data1),
%		     item(dl(1,lit(s(X)),dl(0,lit(np(A,B,C)),lit(s(INFL)))), J, K, Data2)]
%	             item(dl(1,lit(s(X)),lit(s(X))), I, J, Data),
%	         [a_np_dit(Data1, Data2, Data)]).

% = right-extraction rules

inference(e_start, [item(dr(0,_,dr(0,_,dia(Ind,box(Ind,Y)))),_,K,_),item(dr(0,X,Y), I, J, Data0)],
		    item(X, I, J, Data),
		   [K=<I,no_island_violation(Ind,X,Y),start_extraction(Y, J, K, Data0, Data)]).
inference(e_end, [item(dr(0,X,dr(0,Y,dia(Ind,box(Ind,Z)))), I, J, Data0), item(Y, J, K, Data1)],
	          item(X, I, K, Data),
	         [check_extraction(Ind,K0,K),end_extraction(Z, K0, J, Data0, Data1, Data),valid_extraction(Data, I)]).

inference(e_start_l, [item(dl(0,dr(0,_,dia(0,box(0,Y))),_),K,_,_),item(dr(0,X,Y), I, J, Data0)],
		      item(X, I, J, Data),
		     [J=<K,no_island_violation(0,X,Y),start_extraction_l(Y, J, K, Data0, Data)]).
inference(e_end_l, [item(dl(0,dr(0,Y,dia(0,box(0,Z))),X), J, K, Data0), item(Y, I, J, Data1)],
	            item(X, I, K, Data),
	           [end_extraction_l(Z, J, J, Data0, Data1, Data),valid_extraction(Data, I)]).

%inference(gapping, [item(dr(0,dl(0,dr(0,lit(s(S)),dia(1,box(1,B))),dr(0,lit(s(S)),dia(1,box(1,B)))),dr(0,lit(s(S)),dia(1,box(1,B))))
%
%inference(p_conj, [item(dr(0,dl(0,p(0,A,B),p(A,dia(0,box(0,B)))),p(A,B)), I, J, Data0),item(B, I1, I, Data1), item(A, I0, I1, Data2), item(A, J, J0, Data3), item(B, J0, J1, Data4)]
%	           item(A, I0, Mid, DataA)
%	          []).

no_island_violation(1, Formula, Gap) :-
	island_violation(Formula, Gap),
	!,
	fail.
no_island_violation(_, _, _).

island_violation(lit(pp(_)), lit(np(_,_,_))).
island_violation(dl(1,lit(s(_)),lit(s(_))), lit(np(_,_,_))).
island_violation(dr(0,lit(s(_)),lit(s(_))), lit(np(_,_,_))).

check_extraction(0, K, K).
check_extraction(1, _, _).

% np island constraint

check_islands(lit(np), Data) :-
	!,
	Data = data(_, _, _, _, [], []).
check_islands(lit(pp(_)), Data) :-
	!,
	Data = data(_, _, _, _, [], []).
check_islands(_, _).

% = wrapped adjective modifiers must be bound in the embedding np

check_np_wrap([]).
check_np_wrap([X|Xs]) :-
	X \= dl(1,dl(0,n,n),dl(0,n,n)),
	check_np_wrap(Xs).

check_wrap(dl(0,lit(np(_,_,_)),lit(s(_))), Data) :-
	!,
	Data = data(_, _, _, [], _, _).
check_wrap(lit(s(_)), Data) :-
	!,
	Data = data(_, _, _, [], _, _).
check_wrap(_, _).

has_empty_stack(data(_, _, _, [], [], [])).

%dit_mod(data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
%	data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
%	data(p(0,Pros1, Pros2), 
combine_let(Dir, data(Pros0, Sem, Prob1, SetA0, SetB0, SetC0),
	         data(Pros1   , _  , Prob2, SetA1, SetB1, SetC1),
	         data(Pros, Sem, Prob, SetA, SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	combine_pros(Dir, Pros0, Pros1, Pros),
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).

combine_pros(l, Pros0, Pros1, p(0,Pros1,Pros0)).
combine_pros(r, Pros0, Pros1, p(0,Pros0,Pros1)).

application_l(M, data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
	         data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
	         data(p(M,Pros2,Pros1), appl(Sem1,Sem2), Prob, SetA, SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).
application_r(M, data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
	         data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
	         data(p(M,Pros1,Pros2), appl(Sem1,Sem2), Prob, SetA, SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).

a_dit(data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
      data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
      data(p(0,Pros1,Pros2), lambda(NP, lambda(S, appl(appl(Sem1,appl(Sem2,S)),NP))), Prob, SetA, SetB, SetC)) :-
        Prob is Prob1 * Prob2,
      	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
        ord_key_union_i(SetC0, SetC1, SetC).

a_np_dit(data(Pros1, Sem1, Prob1, SetA0, SetB0, SetC0),
         data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
         data(p(0,Pros1,Pros2), lambda(NP, lambda(S, appl(appl(Sem1,appl(Sem2,S)),NP))), Prob, SetA, SetB, SetC)) :-
        Prob is Prob1 * Prob2,
      	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
        ord_key_union_i(SetC0, SetC1, SetC).


% = wrapping
%
% "wrap" infixes are pused onto SetA

wrap(dl(1,V,W), I, J, data(Pros1, Sem, Prob1, SetA0, SetB0, SetC0),
                      data(Pros2, Sem2, Prob2, SetA1, SetB1, SetC1),
                      data(p(1,Pros1,Pros2), Sem, Prob, [t(I,J,dl(1,V,W),Sem2)|SetA], SetB, SetC)) :-
	Prob is Prob1 * Prob2,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).

pop(X, I1, J1, I, J, data(Pros, Sem, Prob, SetA0, SetB, SetC), data(Pros, appl(Sem0,Sem), Prob, SetA, SetB, SetC)) :-
	select(t(I0,J0,X,Sem0), SetA0, SetA),
	/* verify the wrapped constituent is a substing of the current string */
	verify_wrap(I1, I0, J0, J1, I, J),
	!.

pop_vp(I1, J1, I, J, data(Pros, Sem, Prob, SetA0, SetB, SetC), data(Pros, lambda(X,appl(Sem0,appl(Sem,X))), Prob, SetA, SetB, SetC)) :-
	select(t(I0,J0,dl(1,lit(s(S)),lit(s(S))),Sem0), SetA0, SetA),
	verify_wrap(I1, I0, J0, J1, I, J),
	!.

adv_vp(data(Pros0, Sem0, Prob0, SetA0, SetB0, SetC0),
       data(Pros1, Sem1, Prob1, SetA1, SetB1, SetC1),
       data(p(0,Pros0, Pros1), lambda(X,appl(Sem0,appl(Sem1,X))), Prob, SetA, SetB, SetC)) :-
	Prob is Prob0 * Prob1,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).
	

% I = I1 --- I0 --- J0 --- J1 = J

verify_wrap(I, I0, J0, J, I, J) :-
	I0 >= I,
	J0 =< J,
	!.
% I = I0 --- J0 = I1 -- J1 = J
verify_wrap(I1, I0, J0, J1, I, J) :-
	trace,
	format('I0:~w I1: ~w J0: ~w J1: ~w', [I0,I1,J0,J1]),
	I0 = I,
	J0 = I1,
	J1 = J.
	
% = extraction

% = start_extraction(+ExtractedFormula, RightEdgeOfFormula, RightEdgeOfIntroduction, Data1 Data2)
%
% ExtractedFormula: formula extracted
% RightEdgeOfFormula: string position where the formula has been inserted
% RightEdgeOfIntroduction: string position where the higher-order formula authorizing the introduction ends
%
% SetC has entries of the form IntroRightEdge-r(Formula,FormRightEdge,SemVar)
% with meaning Formula-SemVar has been used at string position J-J (FormRightEdge)

start_extraction(Y, J, K, data(Pros, Sem, Prob, SetA, SetB, SetC0), data(Pros, appl(Sem,X), Prob, SetA, SetB, SetC)) :-
	ord_key_insert_i(SetC0, K, t(Y,J,X), SetC).

% SetC = 

start_extraction_l(Y, J, K, data(Pros, Sem, Prob, SetA, SetB0, SetC), data(Pros, appl(Sem,X), Prob, SetA, SetB, SetC)) :-
	ord_key_insert_i(SetB0, K, t(Y,J,X), SetB).

end_extraction_l(Y, J, K, data(Pros0, Sem0, Prob0, SetA0, SetB0, SetC0),
                       data(Pros1, Sem1, Prob1, SetA1, SetB1, SetC1),
	               data(p(0,Pros1,Pros0), appl(Sem1,lambda(X,Sem0)), Prob, SetA, SetB, SetC)) :-
	select(K-t(Y,J,X), SetB1, SetB2),
	!,
	Prob is Prob0 * Prob1,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB2, SetB),
	ord_key_union_i(SetC0, SetC1, SetC).


end_extraction(Y, J, K, data(Pros0, Sem0, Prob0, SetA0, SetB0, SetC0),
                  data(Pros1, Sem1, Prob1, SetA1, SetB1, SetC1),
	          data(p(0,Pros0,Pros1), appl(Sem0,lambda(X,Sem1)), Prob, SetA, SetB, SetC)) :-
	select(K-t(Y,J,X), SetC1, SetC2),
	!,
	Prob is Prob0 * Prob1,
	append(SetA0, SetA1, SetA),
	ord_key_union_i(SetB0, SetB1, SetB),
	ord_key_union_i(SetC0, SetC2, SetC).

valid_extraction(data(_, _, _, _, _, SetC), K) :-
	valid_item(SetC, K).

valid_item([], _).
valid_item([K-_|_], K0) :-
	K0 >= K.

:- dynamic verbose/0.

%verbose.

notify_agenda_size(Head, Tail) :-
    (
       verbose
    ->
       format('Agenda size: ~w - ~w', [Head, Tail])
    ;
       true
    ).

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
         Item = item(Formula,I,J,Data),
         simplify_formula(Formula, SForm),
         format('~NAdding to agenda: <-> ~p-~p ~p ~@~n', [I,J,SForm,print_stacks(Data)])
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
        format('~NAdding to chart: <~p-~p-~p> ~p ~@~n', [Key,I,J,SForm,print_stacks(Data)])
    ;
        print(':')
    ).


print_stacks(data(_,_,_,[],[],[])) :-
	!.
print_stacks(data(_,_,_,As,Bs,Cs)) :-
	format('{~p ~p ~p}', [As,Bs,Cs]).

create_data(Pros, _Form, Prob, Sem, _I, _J, data(Pros, Sem, Prob, [], [], [])).


simplify_formula(X, X) :-
	var(X),
	!.
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
simplify_formula(dia(_,A0), dia(1,A)) :-
	simplify_formula(A0, A).
simplify_formula(box(_,A0), box(1,A)) :-
	simplify_formula(A0, A).

% = ord_key_insert_i(+OrdSet, +Key, +Data, -OrdSet)
%
% as ord_insert/3, but assumes the elements of OrdSet are Key-Value
% pairs; uses *inverse* ordering                                     RM

ord_key_insert_i([], Key, Data, [Key-Data]).
ord_key_insert_i([Key-Data|Tail], Key0, Data0, Rest) :-
	compare(Order, Key0, Key),
	ord_key_insert_i(Order, Tail, Key0, Data0, Key, Data, Rest).

ord_key_insert_i(>, Tail, Key0, Data0, Key, Data, [Key0-Data0,Key-Data|Tail]).
ord_key_insert_i(=, Rest, Key, Data0, Key, _Data, [Key-Data0|Rest]).
ord_key_insert_i(<, Tail, Key0, Data0, Key, Data, [Key-Data|Rest]) :-
	ord_key_insert_i(Tail, Key0, Data0, Rest).


% = ord_key_union(+Map1, +Map2, ?Map3)
%
% as ord_union/3, but for ordered sets of Key-Value pairs, where Value
% is itself an ordered set. If Map1 and Map2 contain the same Key,
% Map3 will contain the ord_union of the two values.                RM

ord_key_union_i([], Set2, Set2).
ord_key_union_i([H1-V1|T1], Set2, Union) :-
	ord_key_union_2_i(Set2, H1, V1, T1, Union).

ord_key_union_2_i([], H1, V1, T1, [H1-V1|T1]).
ord_key_union_2_i([H2-V2|T2], H1, V1, T1, Union) :-
	compare(Order, H1, H2),
	ord_key_union_3_i(Order, H1, V1, T1, H2, V2, T2, Union).

ord_key_union_3_i(>, H1, V1, T1, H2, V2, T2, [H1-V1|Union]) :-
	ord_key_union_2_i(T1, H2, V2, T2, Union).
% NOTE: commenting out the equality restricts lexical entries to have only
% a single extraction each, since the gapping analysis will require special
% treatment in any case, this simple solution seems justified
%ord_key_union_3_i(=, H1, V1, T1, H2, V2, T2, [H1-V1,H2-V2|Union]) :-
%	V1 \== V2,
%	ord_key_union_i(T1, T2, Union).
ord_key_union_3_i(<, H1, V1, T1, H2, V2, T2, [H2-V2|Union]) :-
	ord_key_union_2_i(T2, H1, V1, T1, Union).


% = streams

check_log_stream :-
    (
	is_stream(log)
    ->
        true
    ;
        new_output_file(grail_log, log)
    ).

check_proof_stream :-
    (
	is_stream(proof)
    ->
        true
    ;
        new_output_file('proof.tex', proof)
    ).

check_prolog_proof_stream :-
   (
       is_stream(pl_proof)
   ->
       true
   ;
       new_output_file('proofs.pl', pl_proof)
   ).