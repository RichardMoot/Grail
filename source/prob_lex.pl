% -*- Mode: Prolog -*-

:- module(prob_lex, [prob_parse/1,
		     parse_pos_lemma/1,
		     list_atom_term/2,
		     list_atom_term/3,
		     remove_brackets/2]).

:- use_module(tree234,    [btree_get_replace/5,
                           btree_insert/4,
			   btree_to_list/2]).
:- use_module(lexicon,    [macro_expand/2,
			   cmd_lookup_sem/5,
			   cmd_lookup_sem/7]).
:- use_module(heap,       [empty_heap/1,
			   heap_size/2,
			   get_from_heap/4,
			   add_to_heap/4]).
:- use_module(list_utils, [split_odds_evens/3,
			   split_a_b_c_ds/5]).
:- use_module(options,    [get_option/2]).

:- set_prolog_flag(character_escapes, true).

% = prob_parse_pos_lemma(+ListOfArguments)
%
% 

parse_pos_lemma(L) :-
	catch(parse_pos_lemma1(L), Exc, close_latex_sem),
    (
         Exc = next
    ->
         true
    ;
         Exc = aborted
    ->
         true
    ;
         throw(Exc)
    ).

parse_pos_lemma1(L) :-	
	open_semantics_files,
	get_option(paper_size, PaperSize),
	latex_header(sem, PaperSize),
	/* separate words from formula assignments */
	split_a_b_c_ds(L, Ws0, POSs0, Lemmas0, Fs0),
	/* turn POS tags from atoms into terms */
	list_atom_term(POSs0, POSs1),
	list_atom_term(Lemmas0, Lemmas1),
	/* separate formulas from probabilities */
	list_atom_term(Fs0, R, []),
	/* compute sentence string */
	remove_brackets(Ws0, Ws1),
	reset_global_counters,
        /* try the first solution */
	best_prob(R, 1, NProb, Formulas0),
	/* and add the alternatives to the heap */
	Prob is 1 - NProb,
	empty_heap(H0),
	add_partitions_to_heap(R, H0, H),
	find_goal_formula(Formulas0, Formulas, Ws1, Ws, POSs1, POSs, Lemmas1, Lemmas, Goals),
	concat_atom(Ws, ' ', Atom),
	name(Atom, String0),
	backslash_quotes(String0, String),
	parse_pos_lemma(Goals, H, String, Ws1, Ws, POSs1, POSs, Lemmas1, Lemmas, Formulas, Prob),
	fail.

parse_pos_lemma1(_) :-
	close_latex_sem.

parse_pos_lemma([], H, String, Ws, _, POSs, _, Lemmas, _, _, _) :-
	get_best_and_update(H, String, Ws, POSs, Lemmas).
parse_pos_lemma([G|Gs], H, String, Ws0, Ws, POSs0, POSs, Lemmas0, Lemmas, Formulas, Prob) :-
    (
        format(log, 'Probability: ~w~n', [Prob]),
        format(user_error, 'Probability: ~w~n', [Prob]),
        member(Goal0, [G|Gs]),
	cmd_lookup_sem(Ws, POSs, Lemmas, Formulas, SemForms, Goal0, Goal),
	catch(prove(SemForms, Goal, Ws, String, 0), aborted, close_latex_sem),
        fail
    ;
        get_best_and_update(H, String, Ws0, POSs0, Lemmas0)
    ).

get_best_and_update(H0, String, Words0, POSs0, Lemmas0) :-
	heap_size(H0, Size),
    (
        Size =:= 0
    ->
        true
    ;
	get_from_heap(H0, NProb, Fs, H1),
        best_formulas(Fs, Formulas0),
        find_goal_formula(Formulas0, Formulas, Words0, Words, POSs0, POSs, Lemmas0, Lemmas, Goals),
        parse_best(Goals, Formulas, Fs, NProb, H1, String, Words0, Words, POSs0, POSs, Lemmas0, Lemmas)
    ).

parse_best([], _, Fs, _, H0, String, Words, _, POSs, _, Lemmas, _) :-
	add_partitions_to_heap(Fs, H0, H),
	get_best_and_update(H, String, Words, POSs, Lemmas).
parse_best([G|Gs], Formulas, Fs, NProb, H0, String, Words0, Words, POSs0, POSs, Lemmas0, Lemmas) :-
        Prob is 1 - NProb,
	add_partitions_to_heap(Fs, H0, H),
   (
        format(log, 'Probability: ~w~n', [Prob]),
        format(user_error, 'Probability: ~w~n', [Prob]),
        member(Goal0, [G|Gs]),
	cmd_lookup_sem(Words, POSs, Lemmas, Formulas, SemForms, Goal0, Goal),
	prove(SemForms, Goal, Words, String, 0),
        fail
   ;
        /* try next parse */
        get_best_and_update(H, String, Words0, POSs0, Lemmas0)
   ).


% = prob_parse(+ListOfArguments)
%
% 

prob_parse(L) :-
	catch(prob_parse1(L), Exc, close_latex_sem),
    (
         Exc = next
    ->
         true
    ;
         Exc = aborted
    ->
         true
    ;
         throw(Exc)
    ).


prob_parse1(L) :-	
	open_semantics_files,
	get_option(paper_size, PaperSize),
	latex_header(sem, PaperSize),
	/* separate words from forula assignments */
	split_odds_evens(L, Ws0, Fs0),
	/* separate formulas from probabilities */
	list_atom_term(Fs0, R, []),
	/* compute sentence string */
	remove_brackets(Ws0, Ws1),
	reset_global_counters,
        /* try the first solution */
	best_prob(R, 1, NProb, Formulas0),
	/* and add the alternatives to the heap */
	Prob is 1 - NProb,
	empty_heap(H0),
	add_partitions_to_heap(R, H0, H),
	find_goal_formula(Formulas0, Formulas, Ws1, Ws, Goals),
	concat_atom(Ws, ' ', Atom),
	name(Atom, String0),
	backslash_quotes(String0, String),
	prob_parse(Goals, H, Ws, String, Formulas, Prob),
	fail.

prob_parse1(_) :-
	close_latex_sem.

prob_parse([], H, Ws, String, _, _) :-
	get_best_and_update(H, Ws, String).
prob_parse([G|Gs], H, Ws, String, Formulas, Prob) :-
    (
        format(log, 'Probability: ~w~n', [Prob]),
        format(user_error, 'Probability: ~w~n', [Prob]),
        member(Goal0, [G|Gs]),
	cmd_lookup_sem(Ws, Formulas, SemForms, Goal0, Goal),
	catch(prove(SemForms, Goal, Ws, String, 0), aborted, close_latex_sem),
        fail
    ;
        get_best_and_update(H, Ws, String)
    ).

get_best_and_update(H0, Words0, String) :-
	heap_size(H0, Size),
    (
        Size =:= 0
    ->
        true
    ;
	get_from_heap(H0, NProb, Fs, H1),
        best_formulas(Fs, Formulas0),
        find_goal_formula(Formulas0, Formulas, Words0, Words, Goals),
        parse_best(Goals, Formulas, Fs, NProb, H1, Words0, Words, String)
    ).

parse_best([], _, Fs, _, H0, Words, _, String) :-
	add_partitions_to_heap(Fs, H0, H),
	get_best_and_update(H, Words, String).
parse_best([G|Gs], Formulas, Fs, NProb, H0, Words0, Words, String) :-
        Prob is 1 - NProb,
	add_partitions_to_heap(Fs, H0, H),
   (
        format(log, 'Probability: ~w~n', [Prob]),
        format(user_error, 'Probability: ~w~n', [Prob]),
        member(Goal0, [G|Gs]),
	cmd_lookup_sem(Words, Formulas, SemForms, Goal0, Goal),
	prove(SemForms, Goal, Words, String, 0),
        fail
   ;
        /* try next parse */
        get_best_and_update(H, Words0, String)
   ).

best_formulas([], []).
best_formulas([[F0,_|_]|Ds], [F|Fs]) :-
	macro_expand(F0, F),
	best_formulas(Ds, Fs).

% as best_formulas, but returns the (negated) probability of the
% best sequence as well.

best_prob([], N0, N, []) :-
	N is 1 - N0.
best_prob([[F0,P|_]|Ds], N0, N, [F|Fs]) :-
	macro_expand(F0, F),
	N1 is N0 * P,
	best_prob(Ds, N1, N, Fs).

% = partition(+Lookups, -CompatibleLookup)
%
% partition a set of possible lookups into a set of mutually exclusive
% compatible lookups, by backtracking through the different possibilities.
%
% For n words and the possible formula assignmments and for each prefix k
% of the set of lookups for 1<k<n, we commit to lookups for the first k-1
% words and remove the first lookup for item k, while keeping the lookups
% from k+1 to n unchanged.
% 
% Now each of these items will be different from eachother (since they
% differ at least in the lexical assignment of k and all of these items
% together enumerate all possibilities.

partition([[_,_,A,B|Cs]|Ds], [[A,B|Cs]|Ds]).
partition([[A,B|_]|Ds0], [[A,B]|Ds]) :-
	partition(Ds0, Ds).

% = add_partitions_to_heap
%
% finds all possibile partitions using partition/2 and then adds them
% to the heap with the corresponding probability of the lexical lookup
% as its key.

add_partitions_to_heap(Fs, H0, H) :-
	findall(P, partition(Fs, P), List),
	add_list_to_heap(List, H0, H).

add_list_to_heap([], H, H).
add_list_to_heap([F|Fs], H0, H) :-
	best_prob(F, 1, P, _),
	add_to_heap(H0, P, F, H1),
        add_list_to_heap(Fs, H1, H).

% =

list_atom_term([], []).
list_atom_term([A|As], [T|Ts]) :-
	atom_to_term_catch(A, T0),
   (
        T0 = {T}
   ->
        true
   ;
        T = T0
   ),
	list_atom_term(As, Ts).

% = list_atom_term
%
% converts an list of input atoms which are all of the form
% (F1-P1)-...-Fn)-Pn to a list of lists of the form [F1,P1,...,Fn,Pn]

list_atom_term([], Rs, Rs).
list_atom_term([A|As], [R|R0], Rs) :-
	atom_to_term_catch(A, T),
	separate_probabilities(T, [], R),
	list_atom_term(As, R0, Rs).

separate_probabilities(Fs-P, R0, R) :-
	!,
	separate_probabilities1(Fs, [P|R0], R).
separate_probabilities(F0, R, [F|R]) :-
	macro_expand(F0, F).

separate_probabilities1(Fs-F0, R0, R) :-
	!,
	macro_expand(F0, F),
	separate_probabilities(Fs, [F|R0], R).
separate_probabilities1(F0, R, [F|R]) :-
	macro_expand(F0, F).

atom_to_term_catch(Atom, Term) :-
	catch(atom_to_term_bindings(Atom, Term), _Error, handle_syntax_error(Atom,Term)).

atom_to_term_bindings(Atom, Term) :-
	atom_to_term(Atom, Term, Bindings),
	bind_variables(Bindings).

bind_variables([]).
bind_variables([X=X|Rest]) :-
	bind_variables(Rest).

handle_syntax_error(Atom, Term) :-
     (
        var(Atom)
     ->
        Term = '$VAR'(_)
     ;
        Term = Atom
     ).

% = find_goal_formula(+ListOfFormulas, -FormulasWithoutInterpunction, -SetOfGoals)
%
% true if SetOfGoals is an appropriate set of goals for the given list
% of formulas, simply by computing the count check of the list of formulas
% and looking up the set of goals by means of the predicate goal_formula/5.
%
% Makes the assuption that the number of atomic formulas is quite limited -
% the five following are counted: s, np/n (no distinction is made between
% these), cp, pp and txt, versions of s, np/n and pp with one argument are
% also supported but counted along with their main category. A message is
% printed if any unknow (atomic) formulas are found.

find_goal_formula(Formulas0, Formulas, Words0, Words, Goals) :-
	remove_interpunction(Formulas0, Formulas, Words0, Words),
	count_check_all(Formulas, empty, Tree),
	btree_to_list(Tree, List0),
	remove_zeros(List0, List),
	goal_formula(List, Goals).	

remove_interpunction([], [], [], []).
remove_interpunction([X|Xs], Ys, [W|Ws], Zs) :-
    ( 
        X = lit(let)
    ->
        remove_interpunction(Xs, Ys, Ws, Zs)
    ;
        Ys = [X|Ys0],
        Zs = [W|Zs0],
        remove_interpunction(Xs, Ys0, Ws, Zs0)
    ).

% = find_goal_formula(+ListOfFormulas, -FormulasWithoutInterpunction, -SetOfGoals)
%
% true if SetOfGoals is an appropriate set of goals for the given list
% of formulas, simply by computing the count check of the list of formulas
% and looking up the set of goals by means of the predicate goal_formula/5.
%
% Makes the assuption that the number of atomic formulas is quite limited -
% the five following are counted: s, np/n (no distinction is made between
% these), cp, pp and txt, versions of s, np/n and pp with one argument are
% also supported but counted along with their main category. A message is
% printed if any unknow (atomic) formulas are found.

find_goal_formula(Formulas0, Formulas, Words0, Words, POS0, POS, Lemmas0, Lemmas, Goals) :-
	remove_interpunction(Formulas0, Formulas, Words0, Words, POS0, POS, Lemmas0, Lemmas),
	count_check_all(Formulas, empty, Tree),
	btree_to_list(Tree, List0),
	remove_zeros(List0, List),
	goal_formula(List, Goals).	

remove_interpunction([], [], [], [], [], [], [], []).
remove_interpunction([X|Xs], Ys, [W|Ws], Zs, [P|Ps], Qs, [L|Ls], Ms) :-
    ( 
        X = lit(let)
    ->
        remove_interpunction(Xs, Ys, Ws, Zs, Ps, Qs, Ls, Ms)
    ;
        Ys = [X|Ys0],
        Zs = [W|Zs0],
        Qs = [P|Qs0],
        Ms = [L|Ms0],
        remove_interpunction(Xs, Ys0, Ws, Zs0, Ps, Qs0, Ls, Ms0)
    ).


% = count_check_all(+ListOfFormulas, +S, -S, +NP, -NP, +Let, -Let,
%                   +PP, -PP, +Txt, -Txt, +Unknowns, -Unknowns)
%
% counts the positive against the negative occurences of each of the
% atomic formulas, while keeping track of the unknown formulas which
% are encountered along the way.

count_check_all([], T, T).
count_check_all([H|Hs], T0, T) :-
	count0(H, T0, T1),
	count_check_all(Hs, T1, T).

% = count the negative occurrences of the different atomic formulas

count0(dia(_,P), T0, T) :-
	count0(P, T0, T).
count0(box(_,P), T0, T) :-
	count0(P, T0, T).
count0(p(_,P,Q), T0, T) :-
	count0(P, T0, T1),
	count0(Q, T1, T).
count0(dl(_,P,Q), T0, T) :-
	count1(P, T0, T1),
	count0(Q, T1, T).
count0(dr(_,P,Q), T0, T) :-
	count0(P, T0, T1),
	count1(Q, T1, T).
count0(lit(X), T0, T) :-
	functor(X, F, _A),
     (
        btree_get_replace(T0, F, N0, N, T)
     ->
        N is N0 + 1
     ;
        btree_insert(T0, F, 1, T)
     ).

% = count the positive occurrences of the different atomic formulas

count1(dia(_,P), T0, T) :-
	count1(P, T0, T).
count1(box(_,P), T0, T) :-
	count1(P, T0, T).
count1(p(_,P,Q), T0, T) :-
	!,
	count1(P, T0, T1),
	count1(Q, T1, T).
count1(dl(_,P,Q), T0, T) :-
	!,
	count0(P, T0, T1),
	count1(Q, T1, T).
count1(dr(_,P,Q), T0, T) :-
	!,
	count1(P, T0, T1),
	count0(Q, T1, T).
count1(lit(X), T0, T) :-
	functor(X, F, _A),
     (
        btree_get_replace(T0, F, N0, N, T)
     ->
        N is N0 - 1
     ;
        btree_insert(T0, F, -1, T)
     ).

% = goal_formula(+Scount, +NPcount, +CPcount, +PPcount, +TXTcount, -GoalSet)
%
% lists the set of Goal formulas corresponding to different count values of
% the atomic formulas.

goal_formula([], [dr(0,lit(s),lit(s)),dl(0,lit(n),lit(n))]) :-
	!.
goal_formula([s-1], [lit(s)]) :-
	!.
goal_formula([cs-1], [lit(cs)]) :-
	!.
goal_formula([np-(-1),s-1], [dl(0,lit(np),lit(s))]) :-
	!.
goal_formula([np-1], [lit(np)]) :-
	!.
goal_formula([pp-1], [lit(pp)]) :-
	!.
goal_formula([txt-1], [lit(txt)]) :-
	!.
goal_formula(List, []) :-
	format(user_error, 'No goal formula found for:~n ~w~n', [List]).

remove_zeros([], []).
remove_zeros([At-N|As], Bs0) :-
    (
        N =:= 0
    ->
        Bs0 = Bs
    ;
        Bs0 = [At-N|Bs]
    ),
        remove_zeros(As, Bs).

print_supertags(Words, PFormulas, Goal) :-
	strip_probabilities(PFormulas, Formulas, _Probabilities),
	show_lookup(Words, Formulas, Goal),
	shell('cp -f tmp.dot supertags.dot'),
	layout_lookup.

remove_brackets([], []).
remove_brackets([A|As], [B|Bs]) :-
	name(A, S0),
	remove_brackets1(S0, S),
	name(B, S),
	remove_brackets(As, Bs).

remove_brackets1([A|As], Bs) :-
    (
        A = 123
    ->
        remove_brackets2(As, Bs)
    ;
        remove_brackets2(As, A, Bs)
    ).

remove_brackets2([], []).
remove_brackets2([A|As], Bs) :-
	remove_brackets2(As, A, Bs).
remove_brackets2([], A, Bs) :-
   (
        A = 125
   ->
        Bs = []
   ;
        Bs = [A]
   ).
remove_brackets2([A|As], B, [B|Bs]) :-
	remove_brackets2(As, A, Bs).
		 
close_latex_sem :-
	latex_tail(sem),
	close(sem),
	pdflatex_semantics.
