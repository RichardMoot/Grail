% -*- Mode: Prolog -*-
% ============================================================
% big_french_dynamics.pl
% ============================================================
% !grail 3.1.1

% This grammar is made for use with the supertagger and the grammar
% extracted from the Paris VII Treebank.

% It contains a mix of macro definitions, structural rules and the
% start of a semantic component, which will be made more and more
% complete over time.

% This versions uses the framework of Montegovian dynamics as
% proposed by Philippe de Groote (2006) "Towards a Montagovian
% account of dynamics", SALT XVI.

:- ensure_loaded(french_numbers).
:- ensure_loaded(french_roles).
:- dynamic hide_lefff_info.

% quote this line if you want to have the explicit Lefff info as part of
% you lambda terms.

hide_lefff_info.

% = event_style(+EventStyle)
%
% the current implementation supports four values for EventStyle
%
% - none      No event variables are used. Adverbs take sentences as argument.
%             eg. passionnement(aimer(Marie, Jean))
% - davidson  The verb semantics has an additional event argument. Adverbs take this event as an argument.
%             eg. aimer(e, Marie, Jean) & passionnement(e)
% - neo       The verb semantics has a single event argument. Other arguments are relative by separate role predicates.
%             eg. aimer(e) & agent(e, Marie) & patient(e, Jean) & passionnement(e) 
% - mixed     This is the option "davidson" but with the additional use of the labels of the "neo" option.
%             eg. aimer(e, agent:Marie, patient:Jean) & passionnement(e)

event_style(davidson).

proper_name(named_variable).

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(cl_r, e).
atomic_type(cl_y, e).
atomic_type(s, t->t).
atomic_type(s(_), t->t).
atomic_type(cs, t->t).
atomic_type(txt, t->t).

custom_first_order(lit(cl(Z)), lit(cl, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(pp(Z)), lit(pp, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(s(Z)), lit(s, [X,Y,Z]), _, [X,Y]). 

expand_term((lex(A,B0,C0):-D0), (lex(A,B,C):-D)) :-
         macro_expand(B0, B),
	 goals_to_list(D0, L0, []),
	 select(convert_semantics(T,C1), L0, L),
	 C1 == C0,
	 !,
	 list_to_goals(L1, D),
	 convert_semantics(C0, B0, C).

goals_to_list(true) -->
	!,
	[].
goals_to_list((A,B)) -->
	!,
	goals_to_list(A),
	goals_to_list(B),
goals_to_list(A) -->
	[A].

list_to_goals([], true).
list_to_goals([A|As], G) :-
	list_to_goals(As, A, G).

list_to_goals([], A, A).
list_to_goals([A|As], A0, (A0, G)) :-
	     list_to_goals(As, A, G).


convert_semantics(C0, _, C) :-
	freeze(C0, empty, Tree),
	translate(
	

% = extraction

% = mode 1 : mixed associativity and mixed commutativity

custom_first_order(dr(0,B0,dia(1,box(1,A0))), dr(0,B,dia(1,box(1,A))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).

% = mode 0 : mixed associativity only

custom_first_order(dia(0,box(0,A0)), dia(0,box(0,A)), P, [X,Y]) :-
	!,
	add_first_order(A0, A, P, [X,Y]).



macro(dr(0,dl(0,dl(0,np,s),np),np), dr(0,dl(0,dl(0,lit(np),lit(s(_))),lit(np)),lit(np))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X)))),dl(0,lit(np),lit(s(_))))).
macro(dr(0,dl(0,np,s),dl(0,np,s)), dr(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).
macro(dl(0,dl(0,np,s),dl(0,np,s)), dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).

macro(pp_pour, lit(pp(pour))).
macro(pp_par, lit(pp(par))).
macro(pp_sur, lit(pp(sur))).
macro(pp_en, lit(pp(en))).
macro(pp_dans, lit(pp(dans))).
macro(pp_vers, lit(pp(vers))).
macro(pp_entre, lit(pp(entre))).
macro(pp_comme, lit(pp(comme))).
macro(pp_contre, lit(pp(contre))).
macro(pp_avec, lit(pp(avec))).
macro(pp_sans, lit(pp(sans))).
macro(pp_sous, lit(pp(sous))).
macro(pp_de, lit(pp(de))).
macro(pp_a, lit(pp(à))).
macro(pp, lit(pp(_))).

macro(s_inf, lit(s(inf))).
macro(s_deinf, lit(s(deinf))).
macro(s_ainf, lit(s(ainf))).
macro(s_ppres, lit(s(ppres))).
macro(s_ppart, lit(s(ppart(_)))).
macro(s_q, lit(s(q))).
macro(s_whq, lit(s(whq))).
macro(s, lit(s(_))).
macro(s_top, lit(s(_))).

macro(cl_3d, lit(cl(d3))).
macro(cl_3a, lit(cl(a3))).
macro(cl_12r, lit(cl(r))).
macro(cl_r, lit(cl(r))).
macro(cl_en, lit(cl(en))).
macro(cl_y, lit(cl(y))).

macro(dl(0,s,s), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s,s), dr(0,lit(s(X)),lit(s(X)))).

% =

continuous(0).

external(_).

% =

% standard extraction postulates for a unary mode 1
% commutativity, mixed commutativity and mixed associativity for mode 1

postulate(p(0,A,zip(1,B)), p(0,zip(1,B),A), 'Cp1').
postulate(p(0,A,p(0,B,zip(1,C))), p(0,p(0,A,B),zip(1,C)), 'MCp1').
postulate(p(0,p(0,A,zip(1,B)),C), p(0,p(0,A,C),zip(1,B)), 'MAp1').

% mixed associativity only for unary mode 0

postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(0,A,B),zip(0,C)), 'MAa1').

special_string(".", '.').
special_string(";", ';').
special_string(":", ':').
special_string(",", ',').
special_string("!", '!').
special_string("?", '?').
special_string("\.", '.').
special_string("\;", ';').
special_string("\:", ':').
special_string("\,", ',').
special_string("\!", '!').
special_string("\?", '?').

gq_a_semantics(lambda(N,lambda(Psi,lambda(E,lambda(Phi,quant(exists,X,appl(appl(appl(N,X),E),lambda(F0,appl(appl(appl(Psi,X),NewContext),Phi)))))))), INFO) :-
	add_to_context(exists, F0, X, INFO, NewContext).
% N         e->s->(s->t)->t
% Psi       e->s->(s->t)->t
% E,F0,F    s
% Phi       s->t
% ((N X) E) (s->t)->t
gq_every_semantics(lambda(N,lambda(Psi,lambda(E,lambda(Phi,bool(quant(forall,X,not(appl(appl(appl(N,X),E),lambda(F0,not(appl(appl(appl(Psi,X),F),lambda(G,erase_context(G)))))))),&,appl(Phi,erase_context(F)))))))) :-
	add_to_context(exists, F0, X, [], F).
wh_rel_semantics(lambda(R,lambda(N,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N,X),E),lambda(F,appl(appl(appl(R,lambda(Psi,appl(Psi,X))),F),Phi))))))))).

% 

translate_sem(Term0, Term) :-
	translate_de_predicates(Term0, Term1),
	calculate_contexts(Term1, Term2),
	resolve_anaphora(Term2, Term).

calculate_contexts(erase_context(X), Y) :-
	!,
	erase_context(X, Y).
calculate_contexts(Atom0, Atom) :-
	atomic(Atom0),
	!,
	Atom = Atom0.
calculate_contexts(Term0, Term) :-
	Term0 =.. List0,
	calculate_contexts1(List0, List),
	Term =.. List.
calculate_contexts1([], []).
calculate_contexts1([X|Xs], [Y|Ys]) :-
	calculate_contexts(X, Y),
	calculate_contexts1(Xs, Ys).


resolve_anaphora(sel(Features,Stack), X) :-
	select_features(Stack, Features, X),
	!.
resolve_anaphora(Atom0, Atom) :-
	atomic(Atom0),
	!,
	Atom = Atom0.
resolve_anaphora(Term0, Term) :-
	Term0 =.. List0,
	resolve_anaphora1(List0, List),
	Term =.. List.

resolve_anaphora1([], []).
resolve_anaphora1([X|Xs], [Y|Ys]) :-
	resolve_anaphora(X, Y),
	resolve_anaphora1(Xs, Ys).

select_features(update(ref(_,X,Fs0),_), Fs, X) :-
	ord_subset(Fs, Fs0),
	!.
select_features(update(_,Ss), Fs, X) :-
	select_features(Ss, Fs, X).

%

translate_de_predicates(Term0, Term) :-
	find_de_predicates(Term0, Term, Pred, [], De, []),
	match_predicates(De, Pred).

instantiate_preds([]).
instantiate_preds([X-P|Ps]) :-
	X = P,
	instantiate_preds(Ps).

match_predicates([], Pred) :-
	instantiate_preds(Pred).
match_predicates([V-appl(appl(de,X),Y)|Rest], Pred0) :-
    (
        select(W-appl(R,Y), Pred0, Pred)
    ->
        V = true,
        atomic_list_concat([R,'_',de], Fun),
        W = appl(appl(Fun,X),Y)
    ;
        Pred = Pred0,
        V = appl(appl(de,X),Y)
    ),
        match_predicates(Rest, Pred).
     
find_de_predicates(appl(appl(de,X),Y), Z, Ps, Ps, [Z-appl(appl(de,X),Y)|De], De) :-
	!.
find_de_predicates(appl(Pred,X), Y, [Y-appl(Pred,X)|Ps], Ps, De, De) :-
	de_predicate(Pred),
	!.
find_de_predicates(Atom0, Atom, Ps, Ps, De, De) :-
	atomic(Atom0),
	!,
	Atom = Atom0.
find_de_predicates(Term0, Term, Ps0, Ps, De0, De) :-
	Term0 =.. List0,
	find_de_predicates1(List0, List, Ps0, Ps, De0, De),
	Term =.. List.

find_de_predicates1([], [], Ps, Ps, De, De).
find_de_predicates1([X|Xs], [Y|Ys], Ps0, Ps, De0, De) :-
	find_de_predicates(X, Y, Ps0, Ps1, De0, De1),
	find_de_predicates1(Xs, Ys, Ps1, Ps, De1, De).

de_predicate(père).
de_predicate(mère).
de_predicate(frère).
de_predicate(soeur).
de_predicate(président).
de_predicate(roi).
de_predicate(reine).
de_predicate(prince).
de_predicate(princesse).
de_predicate(capital).
de_predicate('Président').
de_predicate('Roi').
de_predicate('Reine').

existential_closure_event(Var, Term0, Term) :-
	event_style(Style),
	!,
   (
        Style = none
   ->
        Term = Term0
   ;
        Term = quant(exists,Var,Term0)
   ).

existential_closure_name(Var, Term0, Term) :-
	proper_name(Style),
	!,
   (
        Style = constant
   ->
        Term = Term0
   ;
        Term = quant(exists,Var,Term0)
   ).


handle_tense_aspect(passé_composé, Ev, Phi, Term) :-
	event_style(Style),
	!,
    (
        Style = none
    ->
        Term = Phi
    ;
        passe_compose_semantics(Ev, Term0),
	Term = lambda(X,bool(Term0,&,appl(Phi,X)))
    ).


non_intersective_adverb(possiblement).
non_intersective_adverb(presque).

handle_event_modifier(_Ev, Word, Term0, Term) :-
	event_style(none),
	!,
	Term = appl(Word,Term0).
% we are using some (neo-)Davidsonian event semantics
handle_event_modifier(Ev, Word, Term0, Term) :-
	non_intersective_adverb(Word),
	!,
	Term = exists(Ev1,bool(appl(appl(Word,Ev),Ev1),&,Term0)).
handle_event_modifier(Ev, Word, Term0, bool(appl(Word,Ev),&,Term0)).

handle_event_semantics(Word, _Event, Variables, _Types, Term) :-
	event_style(none),
	!,
	add_arguments(Variables, Word, Term).
handle_event_semantics(Word, Event, Variables, _Types, Term) :-
	event_style(davidson),
	!,
	add_arguments([Event|Variables], Word, Term).
handle_event_semantics(Word, Event, Variables, Types, appl(Term,Event)) :-
	event_style(mixed),
	get_roles(Word, Types, Roles),
	!,
	add_arguments_roles(Variables, Roles, Word, Term).
handle_event_semantics(Word, Event, Variables, Types, Term) :-
	event_style(neo),
	get_roles(Word, Types, Roles),
	!,
	add_roles(Roles, Variables, Event, bool(appl(event,Event),&,appl(Word,Event)), Term).

handle_event_semantics_roles(Word, _Event, Variables, _Roles, Term) :-
	event_style(none),
	!,
	add_arguments(Variables, Word, Term).
handle_event_semantics_roles(Word, Event, Variables, _Roles, Term) :-
	event_style(davidson),
	!,
	add_arguments([Event|Variables], Word, Term).
handle_event_semantics_roles(Word, Event, Variables, Roles, appl(Term,Event)) :-
	event_style(mixed),
	!,
	add_arguments_roles(Variables, Roles, Word, Term).
handle_event_semantics_roles(Word, Event, Variables, Roles, Term) :-
	event_style(neo),
	!,
	add_roles(Roles, Variables, Event, bool(appl(event,Event),&,appl(Word,Event)), Term).


handle_discourse_relation(_Relation, _E1, _E2, Term0, Term) :-
	event_style(none),
	!,
	Term = Term0.
handle_discourse_relation(Relation, E1, E2, Term0, bool(appl(appl(Relation,E2),E1),&,Term0)).


add_arguments_roles([], [], Term, Term).
add_arguments_roles([V|Vs], [R|Rs], Term0, appl(Term,R:V)) :-
	add_arguments_roles(Vs, Rs, Term0, Term).

add_arguments([], Term, Term).
add_arguments([V|Vs], Term0, appl(Term,V)) :-
	add_arguments(Vs, Term0, Term).

add_roles([], [], _, Term, Term).
add_roles([Role|Rs], [Var|Vs], Event, Term0, Term) :-
    (
        Role == null
    ->
        add_roles(Rs, Vs, Event, Term0, Term)
    ;
	add_roles(Rs, Vs, Event, bool(Term0,&,appl(appl(Role,Var),Event)), Term)
    ).

% =====================================
% =           Role lexicon            =
% =====================================

get_roles(Verb, Arguments, Roles) :-
	get_roles1(Verb, Arguments, Roles),
	!.
get_roles(_, Arguments, Roles) :-
	get_default_roles(Arguments, Roles).

get_default_roles([], []).
get_default_roles([_|Xs], [agent|Ys]) :-
	get_default_roles1(Xs, Ys).
get_default_roles1([], []).
get_default_roles1([_|Xs], [patient|Ys]) :-
	get_default_roles2(Xs, Ys).
get_default_roles2([], []).
get_default_roles2([_|Xs], [participant|Ys]) :-
	get_default_roles2(Xs, Ys).

integrate_se(Word, SeWord) :-
	atom_codes(Word, [C|_]),
	is_vowel(C),
	!,
	atomic_list_concat([s,'_', Word], SeWord).
integrate_se(Word, SeWord) :-
	atomic_list_concat([se,'_',Word], SeWord).


%is_vowel(65533). % à
is_vowel(97).  % a
is_vowel(101). % e
is_vowel(105). % i
is_vowel(111). % o
is_vowel(117). % u
is_vowel(121). % y


% =====================================
% =        Tense information          =
% =====================================

pos_time(ver:TENSE, E, Term0, Term) :-
	!,
	tense_time(TENSE, E, Term0, Term).
pos_time(_, []).

tense_time(impf, E, Term, bool(Term,&,appl(past,E))) :-
	!.
tense_time(simp, E, Term, bool(Term,&,appl(past,E))) :-
	!.
tense_time(cond, E, Term, bool(Term,&,appl(possible,E))) :-
	!.
tense_time(futu, E, Term, bool(Term,&,appl(future,E))) :-
	!.
tense_time(_, _-[]).

add_lefff_info(_, _, T, T) :-
	hide_lefff_info,
	!.
add_lefff_info(F, V, T0, T) :-
	add_lefff_info1(F, V, T0, T).
add_lefff_info1(h,   X, T,  bool(T,&,appl(humain,X))).
add_lefff_info1(hm,  X, T, bool(T,&,bool(appl(humain,X),&,appl(masculin,X)))).
add_lefff_info1(hf,  X, T, bool(T,&,bool(appl(humain,X),&,appl(féminin,X)))).
add_lefff_info1(hs,  X, T, bool(T,&,appl(humain,X))).
add_lefff_info1(hms, X, T, bool(T,&,bool(appl(humain,X),&,appl(masculin,X)))).
add_lefff_info1(hfs, X, T, bool(T,&,bool(appl(humain,X),&,appl(féminin,X)))).
add_lefff_info1(hp,  X, T, bool(T,&,appl(humain,X))).
add_lefff_info1(hmp, X, T, bool(T,&,appl(humain,X),&,appl(masculin,X))).
add_lefff_info1(hfp, X, T, bool(T,&,appl(humain,X),&,appl(féminin,X))).
add_lefff_info1(l,   X, T, bool(T,&,appl(lieu,X))).
add_lefff_info1(lm,  X, T, bool(T,&,appl(lieu,X),&,appl(masculin,X))).
add_lefff_info1(lf,  X, T, bool(T,&,appl(lieu,X),&,appl(féminin,X))).
add_lefff_info1(ls,  X, T, bool(T,&,appl(lieu,X))).
add_lefff_info1(lms, X, T, bool(T,&,bool(appl(lieu,X),&,appl(masculin,X)))).
add_lefff_info1(lfs, X, T, bool(T,&,bool(appl(lieu,X),&,appl(féminin,X)))).
add_lefff_info1(lp,  X, T, bool(T,&,appl(lieu,X))).
add_lefff_info1(lmp, X, T, bool(T,&,bool(appl(lieu,X),&,appl(masculin,X)))).
add_lefff_info1(lfp, X, T, bool(T,&,bool(appl(lieu,X),&,appl(féminin,X)))).
add_lefff_info1(m,   X, T, bool(T,&,appl(masculin,X))).
add_lefff_info1(f,   X, T, bool(T,&,appl(féminin,X))).
add_lefff_info1(mp,  X, T, bool(T,&,appl(masculin,X))).
add_lefff_info1(fp,  X, T, bool(T,&,appl(féminin,X))).
add_lefff_info1(ms,  X, T, bool(T,&,appl(masculin,X))).
add_lefff_info1(fs,  X, T, bool(T,&,appl(féminin,X))).

extract_lefff_info(String, List) :-
    (
        var(String)
    ->
        List = []
    ;
	name(String, Chars),
	Chars = [C|Cs],
	add_chars_to_list(C, Cs, List0, []),
        sort(List0, List)
    ).

add_chars_to_list([]) -->
	[].
add_chars_to_list([C|Cs]) -->
	add_chars_to_list(C, Cs).

add_chars_to_list(104, Chars) -->
	!,
	[human],
	add_chars_to_list(Chars).
add_chars_to_list(108, Chars) -->
	!,
	[place],
	add_chars_to_list(Chars).
add_chars_to_list(102, Chars) -->
	!,
	[fem],
	add_chars_to_list(Chars).
add_chars_to_list(109, Chars) -->
	!,
	[masc],
	add_chars_to_list(Chars).
add_chars_to_list(112, Chars) -->
	!,
	[plur],
	add_chars_to_list(Chars).
add_chars_to_list(115, Chars) -->
	!,
	[sing],
	add_chars_to_list(Chars).
add_chars_to_list(_, Chars) -->
	!,
	add_chars_to_list(Chars).

% combine a preposition with another word

combine_prep_word(Prp, Word0, Word) :-
	var(Prp),
	!,
	Word = Word0.
combine_prep_word(_, Word0, Word) :-
	var(Word0),
	!,
	Word = Word0.
combine_prep_word(a, Word, PrepWord) :-
	!,
        atomic_list_concat([Word,'_',à], PrepWord).
combine_prep_word(Prep, Word, PrepWord) :-
        atomic_list_concat([Word,'_',Prep], PrepWord).

% =====================================
% =  Default semantics with POS tag   =
% =====================================

% = numbers

% = nouns
% = NOM

default_semantics(Word, nom, lit(n), lambda(X,lambda(E,lambda(Phi,bool(appl(Word,X),&,appl(Phi,E)))))).
default_semantics(Word, nom:INFO, lit(n), lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))) :-
	add_lefff_info(INFO, X, appl(Word,X), Term).

%default_semantics(Word, nom, lit(np), lambda(Psi, lamda(E, lambda(Phi, lambda(X,lambda(E,lambda(Phi,bool(appl(Word,X),&,appl(Phi,E)))))).


% = NAM

default_semantics(Word, nam, lit(n), lambda(X,lambda(E,lambda(Phi,bool(appl(Word,X),&,appl(Phi,E)))))).
default_semantics(Word, nam:INFO, lit(n), lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))) :-
	add_lefff_info(INFO, X, appl(Word,X), Term).

% = noun phrase - name

default_semantics(Word, nam, lit(np), lambda(Psi,lambda(E,lambda(Phi,Term)))) :-
	proper_name_semantics(Word, X, appl(appl(appl(Psi,X),E),lambda(F,appl(Phi,NewContext))), Term1),
	add_to_context(proper_name, F, X, [], NewContext),
	existential_closure_name(X, Term1, Term).
default_semantics(Word, nam:INFO0, lit(np), lambda(Psi,lambda(E,lambda(Phi,Term)))) :-
	extract_lefff_info(INFO0, INFO),
	proper_name_semantics(Word, X, appl(appl(appl(Psi,X),E),lambda(F,appl(Phi,NewContext))), Term1),
	add_to_context(proper_name, F, X, INFO, NewContext),
	existential_closure_name(X, Term1, Term).

%= verbs

% = auxiliary verbs

default_semantics(avoir, _POS, dr(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(ppart(avoir))))), lambda(VP, lambda(S, lambda(Ev, lambda(E, lambda(Phi, appl(appl(appl(appl(VP,S),Ev),E),Term))))))) :-
	handle_tense_aspect(passé_composé, Ev, Phi, Term).
default_semantics(être, _POS, dr(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(ppart(être))))), lambda(VP, lambda(S, lambda(Ev, lambda(E, lambda(Phi, appl(appl(appl(appl(VP,S),Ev),E),Term))))))) :-
	handle_tense_aspect(passé_composé, Ev, Phi, Term).

% = intransitive

% S   (e->s->(s->t)->t)->s->(s->t)->t
% X   e
% E   s
% Phi s->t
% Ev  s

default_semantics(Word, _POS, dl(_,lit(np),lit(s(_))), lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))) :-
	handle_event_semantics(Word, Ev, [X], [np], Term).

% = intransitive - verb initial

default_semantics(Word, _POS, dr(_,lit(s(_)),lit(np)), lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))) :-
	handle_event_semantics(Word, Ev, [X], [np], Term).

% = transitive - SVO

default_semantics(Word, _POS, dr(_,dl(_,lit(np),lit(s(_))),lit(np)), lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y], [np, np], Term).

default_semantics(Word, _POS, dr(_,dr(_,lit(s(_)),lit(np)),lit(np)), lambda(S,lambda(O,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y], [np, np], Term).


% = transitive - Subj + PP

default_semantics(Word, _POS, dr(_,dl(_,lit(np),lit(s(_))),lit(pp(Prep))), lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))))) :-
	handle_event_semantics(Word, Ev, [X,appl(Prep,Y)], [np, pp(Prep)], Term).
default_semantics(Word, _POS, dr(_,dr(_,lit(s(_)),lit(np)),lit(pp(Prep))), lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))))) :-
	handle_event_semantics(Word, Ev, [X,appl(Prep,Y)], [np, pp(Prep)], Term).

% = transitive - Subj + PP + NP

default_semantics(Word, _POS, dr(0,dr(_,dl(_,lit(np),lit(s(_))),lit(pp(Prep))),lit(np)), lambda(O,lambda(PP,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y,appl(Prep,Z)], [np, np, pp(Prep)], Term).
default_semantics(Word, _POS, dr(0,dr(_,dl(_,lit(np),lit(s(_))),lit(np)),lit(pp(Prep))), lambda(PP,lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y,appl(Prep,Z)], [np, np, pp(Prep)], Term).
default_semantics(Word, _POS, dr(0,dr(_,dr(_,lit(s(_)),lit(np)),lit(pp(Prep))),lit(np)), lambda(O,lambda(PP,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y,appl(Prep,Z)], [np, np, pp(Prep)], Term).
default_semantics(Word, _POS, dr(0,dr(_,dr(_,lit(s(_)),lit(np)),lit(np)),lit(pp(Prep))), lambda(PP,lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))))) :-
	handle_event_semantics(Word, Ev, [X,Y,appl(Prep,Z)], [np, np, pp(Prep)], Term).


% subject raising

%default_semantics(Word, _POS, dl(_,lit(np),lit(s(_))), lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))) :-
%	handle_event_semantics(Word, Ev, [X], [np], Term).

% object raising
% "demande à Marie de venir" ("Marie" is aobj of "demander" and subject of "de venir"

% PP    (e->s->(s->t)->t)->s->(s->t)->t
% S    (e->s->(s->t)->t)->s->(s->t)->t
% Deinf ((e->s->(s->t)->t)->s->(s->t)->t)->s->s->(s->t)->t

%default_semantics(Word, _, dr(0,dr(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(deinf)))),lit(pp(a))), lambda(PP, lambda(Deinf, lambda(S,lambda(Ev,appl(PP,lambda(X,lambda(E,lambda(Phi,bool(Term),&,appl(Deinf,PP)))))))))) :-
%	true.

%default_semantics(Word, _POS, dr(_,dl(_,lit(np),lit(s(_))),lit(np)), lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))))) :-

% = reflexive verbs

%default_semantics(faire, _, dr(0,dl(0,lit(cl(r)),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(inf)))), lambda(VP, lambda(_Refl, lambda(S, lambda(Ev, appl(S,lambda(X,lambda(E,lambda(Phi

default_semantics(Word, _POS, dl(0,lit(cl(r)),dl(0,lit(np),lit(s(_)))), lambda(_Refl,lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	get_roles1(Word, [np, cl_r], [Role, null]),
	!,
	/* Word is a pseude_reflexive verb */
	integrate_se(Word, SeWord),
	handle_event_semantics_roles(SeWord, Ev, [X], [Role], Term).
default_semantics(Word, _POS, dl(0,lit(cl(r)),dl(0,lit(np),lit(s(_)))), lambda(_Refl,lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_semantics(Word, Ev, [X,X], [np,np], Term).

default_semantics(Word, _POS, dr(0,dl(0,lit(cl(r)),dl(0,lit(np),lit(s(_)))),lit(pp(Prep))), lambda(PP,lambda(_Refl,lambda(S,lambda(Ev,appl(S,lambda(X,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))) :-
	get_roles1(Word, [np, cl_r, pp(Prep)], [Role1, null, Role2]),
	!,
	/* Word is a pseude_reflexive verb */
	integrate_se(Word, SeWord),
	handle_event_semantics_roles(SeWord, Ev, [X,appl(Prep,Z)], [Role1, Role2], Term).
default_semantics(Word, _POS, dr(0,dl(0,lit(cl(r)),dl(0,lit(np),lit(s(_)))),lit(pp(Prep))), lambda(PP,lambda(_Refl,lambda(S,lambda(Ev,appl(S,lambda(X,appl(PP,lambda(Z,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))) :-
	/* true reflexive */
	handle_event_semantics(Word, Ev, [X,X,appl(Prep,Z)], [np, np, pp(Prep)], Term).

% = copula verbs
% Adj (e->s->(s->t)->t)->e->s->(s->t)->t
% S   (e->s->(s->t)->t)->s->(s->t)->t
% X   e
% E   s
% Phi s->t

default_semantics(Word, _POS, dr(_,dl(_,lit(np),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_semantics(Word, Ev, [X, lambda(Y,appl(appl(appl(appl(Adj,lambda(_,lambda(E1,lambda(Phi1,appl(Phi1,E1))))),Y),E),Phi))], [np, adj], Term).
default_semantics(Word, _POS, dr(_,dl(_,dl(0,lit(n),lit(n)),lit(s(_))),lit(np)), lambda(S,lambda(Adj,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_semantics(Word, Ev, [X, lambda(Y,appl(appl(appl(appl(Adj,lambda(_,lambda(_,lambda(_,true)))),Y),E),Phi))], [np, adj], Term).
default_semantics(Word, _POS, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))),lit(np)), lambda(S,lambda(Adj,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_semantics(Word, Ev, [X, lambda(Y,appl(appl(appl(appl(Adj,lambda(_,lambda(_,lambda(_,true)))),Y),E),Phi))], [np, adj], Term).
default_semantics(Word, _POS, dr(_,dr(_,lit(s(_)),lit(np)),dl(0,lit(n),lit(n))), lambda(Adj,lambda(S,lambda(Ev,appl(S,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_semantics(Word, Ev, [X, lambda(Y,appl(appl(appl(appl(Adj,lambda(_,lambda(_,lambda(_,true)))),Y),E),Phi))], [np, adj], Term).
	

% = Subj + V + Obj + Adj

default_semantics(Word, _POS, dr(0,dr(_,dl(_,lit(np),lit(s(_))),dl(0,lit(n),lit(n))),lit(np)), lambda(O,lambda(Adj,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))) :-
	handle_event_semantics(Word, Ev, [X, Y, lambda(Z,appl(appl(appl(appl(Adj,lambda(_,lambda(_,lambda(_,true)))),Z),E),Phi))], [np, np, adj], Term).
default_semantics(Word, _POS, dr(0,dr(_,dl(_,lit(np),lit(s(_))),lit(np)),dl(0,lit(n),lit(n))), lambda(Adj,lambda(O,lambda(S,lambda(Ev,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E))))))))))))) :-
	handle_event_semantics(Word, Ev, [X, Y, lambda(Z,appl(appl(appl(appl(Adj,lambda(_,lambda(_,lambda(_,true)))),Z),E),Phi))], [np, np, adj], Term).


% adverbs

% S    s->s->(s->t)->t

default_semantics(Word, _POS, dr(0,lit(s(_)),lit(s(_))), lambda(S, lambda(Ev, lambda(E, lambda(Phi, Term))))) :-
	handle_event_modifier(Ev, Word, appl(appl(appl(S,Ev),E),Phi), Term).
default_semantics(Word, _POS, dl(1,lit(s(_)),lit(s(_))), lambda(S, lambda(Ev, lambda(E, lambda(Phi, Term))))) :-
	handle_event_modifier(Ev, Word, appl(appl(appl(S,Ev),E),Phi), Term).

% adverbial prepositions

% NP  (e->s->(s->t)->t)->s->(s->t)->t
% S   s->(s->t)->s->t

default_semantics(Word, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(np)), lambda(NP, lambda(S, lambda(Ev, appl(NP,lambda(X,lambda(E,lambda(Phi,bool(Term,&,appl(Phi,E)))))))))) :-
	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),_),lambda(_,true)), Term).
% The following works, but not for higher-order assignments
%default_semantics(Word, _POS, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(np)), lambda(NP, lambda(S, lambda(Ev, appl(NP,lambda(X,lambda(E,lambda(Phi,Term)))))))) :-
%	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),E),Phi), Term).
default_semantics(Word, _POS, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(np)), lambda(NP, lambda(S, lambda(Ev, appl(NP,lambda(X,lambda(E,lambda(Phi,Term)))))))) :-
	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),E),Phi), Term).
%default_semantics(Word, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(np)), lambda(NP, lambda(S, lambda(Ev, appl(NP,lambda(X,lambda(E,lambda(Phi,Term)))))))) :-
%	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),E),lambda(E1,appl(), Term).

% N   e->s->(s->t)->s
% Psi e->s->(s->t)->t
% E   s
% Phi s->t
% X   e
% F   s
% update (e->s)->s

% prepositions as noun modifiers

% N1  e->s->(s->t)->t
% N2  e->s->(s->t)->t
% X   e
% E   s
% Phi s->t

default_semantics(Word, prp, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2, lambda(N1, lambda(X, lambda(E, lambda(Phi, appl(appl(appl(N1,X),E),lambda(E1,quant(exists,Y,bool(appl(appl(Word,X),Y),&,appl(appl(appl(N2,Y),update(Y,E1)),Phi))))))))))).
default_semantics(Word, prp:det, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2, lambda(N1, lambda(X, lambda(E, lambda(Phi, appl(appl(appl(N1,X),E),lambda(E1,quant(exists,Y,bool(appl(appl(Word,X),Y),&,appl(appl(appl(N2,Y),update(Y,E1)),Phi))))))))))).

% NP        (e->s->(s->t)->t)->s->(s->t)->t
% N         e->s->(s->t)->t
% X,Y       e
% E,E1,E2   s
% Phi,Phi2  s->t
% (N X)     s->(s->t)->t
% ((N X) E) (s->t)->t
% (NP ... ) (s->t)->t
% (NP Y^T ) (s->t)->t  T:s->(s->t)->t
% (NP Y^(E^T) ) (s->t)->t  T:(s->t)->t
% ? E1, ? E2, ? Phi2

default_semantics(Word, _, dr(0,dl(0,lit(n),lit(n)),lit(np)), lambda(NP, lambda(N, lambda(X, lambda(E, lambda(Phi, appl(appl(appl(N,X),E),lambda(E1,appl(appl(appl(NP,lambda(Y,lambda(E2,lambda(Phi2,bool(appl(appl(Word,X),Y),&,appl(Phi2,E2)))))),E1),Phi))))))))).

% = adj + PP

default_semantics(Word, _, dr(0,dl(0,lit(n),lit(n)),lit(pp(par))), lambda(PP, lambda(N, lambda(X, lambda(E, lambda(Phi, appl(appl(appl(N,X),E),lambda(E1,appl(appl(appl(PP,lambda(Y,lambda(E2,lambda(Phi2,bool(quant(exists,Ev,appl(appl(appl(Word,Ev),X),Y)),&,appl(Phi2,E2)))))),E1),Phi))))))))).

default_semantics(Word, _, dr(0,dl(0,lit(n),lit(n)),lit(pp(Prep))), lambda(PP, lambda(N, lambda(X, lambda(E, lambda(Phi, appl(appl(appl(N,X),E),lambda(E1,appl(appl(appl(PP,lambda(Y,lambda(E2,lambda(Phi2,bool(appl(appl(PrepWord,X),Y),&,appl(Phi2,E2)))))),E1),Phi))))))))) :-
	combine_prep_word(Prep, Word, PrepWord).

% gq_a_semantics(lambda(N,lambda(Psi,lambda(E,lambda(Phi,quant(exists,X,appl(appl(appl(N,X),E),lambda(F,appl(appl(appl(Psi,X),update(X,F)),Phi))))))))).

% N   e->s->(s->t)->s
% S   s->(s->t)->s->t
% Ev  s
% E   s
% Phi s->t

% Prepositions as arguments

default_semantics(Word, prp, dr(_,lit(pp(PRP)),lit(np)), lambda(X,X)) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).
%dr(0,dr(0,s,dl(0,n,n)),np)
% =====================================
% = Default semantics without POS tag =
% =====================================

default_semantics(Word, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(Ev, lambda(E, lambda(Phi,quant(exists,X,Term))))))) :-
	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),E),lambda(F,appl(appl(appl(N,X),update(X,F)),Phi))), Term).
default_semantics(Word, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(Ev, lambda(E, lambda(Phi,quant(exists,X,Term))))))) :-
	handle_event_modifier(Ev, appl(Word,X), appl(appl(appl(S,Ev),E),lambda(F,appl(appl(appl(N,X),update(X,F)),Phi))), Term).

% = Intersective adjectives
% N e->s->(s->t)->t).
% X e
% E s
% Phi s->t
% ((N X) E) (s->t)->t

default_semantics(Word, dl(0,lit(n),lit(n)), lambda(N,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N,X),E),lambda(F,bool(appl(Word,X),&,appl(Phi,F))))))))).

% Intensifier
% Adj (e->s->(s->t)->t))->(e->s->(s->t)->t))
% N e->s->(s->t)->t)
% X e
% E s
% Phi s->t
%default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), lambda(Adj, lambda(N, lambda(X, lambda(E, lambda(Phi, appl(Word,appl(Adj,lambda(_,lambda(_,lambda(_,true))))),X),E),Phi)))).
%default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), lambda(Adj, lambda(_N, lambda(X, lambda(E, lambda(Phi, appl(Word,appl(Adj,lambda(_,lambda(_,lambda(_,true))))),X),E),Phi)))).
%default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, lambda(Phi, bool(appl(appl(appl(N2,X),E),lambda(F,appl(appl(N1,Y),F))),&,bool(appl(appl(Word,Y),X)),&,appl(Phi,E)))))))).
%lambda(X,lambda(E,lambda(Phi
% 
% default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),lit(np)), lambda(N, lambda(NP, lambda(X, lambda(E, lambda(Phi, appl(N2,lambda(Y,appl(N1,lambda(Z,

%default_semantics(Word, nam, lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,pierre),E),lambda(F,appl(Phi,update(pierre,F)))))))).


% ============================================================
% Lexicon
% ============================================================

% = Personal pronouns

lex('Il', lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([masc,sing],E)),E),Phi))))).
lex('Elle', lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([fem,sing],E)),E),Phi))))).
lex(il, lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([masc,sing],E)),E),Phi))))).
lex(elle, lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([fem,sing],E)),E),Phi))))).
lex(le, lit(cl_a3), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([masc,sing],E)),E),Phi))))).
lex(la, lit(cl_a3), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([fem,sing],E)),E),Phi))))).

lex('Ils', lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([masc,plur],E)),E),Phi))))).
lex('Elles', lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([fem,plur],E)),E),Phi))))).
lex(ils, lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([masc,plur],E)),E),Phi))))).
lex(elles, lit(np), lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel([fem,plur],E)),E),Phi))))).

% Indefinites

lex('Certains', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [plur]).
lex('Des', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [plur]).
lex('D\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, []).
lex('Du', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex(des, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [plur]).
lex(du, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex(de, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, []).
lex('d\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, []).

% Existential

lex(un, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex('Un', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex(une, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [fem,sing]).
lex('Une', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [fem,sing]).

%

lex(le, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex('Le', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [masc,sing]).
lex(la, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [fem,sing]).
lex('La', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [fem,sing]).
lex('l\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [sing]).
lex('L\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [sing]).
lex(les, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [plur]).
lex('Les', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem, [plur]).

% Universal

lex(chaque, dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Chaque', dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(tout, dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Tout', dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(toute, dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Toute', dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(tous, dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Tous', dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(toutes, dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Toutes', dr(0,lit(np),lit(n)), Sem) :-
	gq_every_semantics(Sem).

lex(qui, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(0,box(0,lit(np))))), Sem) :-
	wh_rel_semantics(Sem).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(np))))), Sem) :-
	wh_rel_semantics(Sem).
lex(que, dr(0,lit(s(q)),lit(s(_))), lambda(X,X)).

lex('.', dl(0,s,txt), lambda(S,lambda(P,lambda(Ev,appl(appl(appl(S,Ev),nil),P))))).

% = common nouns used as adjectives with a specific sense
% TODO: add more
	
lex(maison, dl(0,n,n), lambda(P,lambda(X,lambda(E,lambda(Phi,bool(appl(fait_maison,X),&,appl(appl(appl(P,X),E),Phi))))))).
lex(accession, dl(0,n,n), lambda(P,lambda(X,lambda(E,lambda(Phi,bool(appl(à_l_accession,X),&,appl(appl(appl(P,X),E),Phi))))))).

% = sentence conjunctions
% S s->(s->t)->s->t
% Ev s
% E s
% Phi s->t


lex('Parce', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(explanation, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(parce, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(explanation, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(parce, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(explanation, Ev, Ev2, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex('Pour', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev, Ev2, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(pour, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev, Ev2, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(pour, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex('Afin', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev, Ev2, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(afin, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev, Ev2, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(afin, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(goal, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex('Alors', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(alors, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(alors, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex('Tandis', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(tandis, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S1,lambda(S2,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(tandis, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(q))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(et, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	existential_closure_event(Ev2,appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term).
lex(ou, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2, lambda(S1, lambda(_Ev, lambda(E, lambda(Phi, bool(bool(Term1,\/,Term2),&,appl(Phi,E)))))))) :-
	existential_closure_event(Ev1,appl(appl(appl(S1,Ev1),E),lambda(F,erase_context(F))), Term1),
	existential_closure_event(Ev2,appl(appl(appl(S2,Ev2),E),lambda(G,erase_context(G))), Term2).
lex(mais, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(contrast, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex('Quand', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(quand, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(quand, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
lex(quand, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(Ev,lambda(E,lambda(Phi,Term)))))) :-
	handle_discourse_relation(parallel, Ev2, Ev, appl(appl(appl(S1,Ev2),E),lambda(E2,appl(appl(appl(S2,Ev),E2),Phi))), Term1),
	existential_closure_event(Ev2, Term1, Term).
%lex(mais, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).
lex(',', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).
lex(';', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).
lex(':', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).
lex('-', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).
lex('!', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(S2,lambda(S1,lambda(E,lambda(Phi,appl(appl(S1,E),lambda(E2,appl(appl(S2,E2),Phi)))))))).

% noun phrase conjunctions
% 
% NP1 (e->s->(s->t)->t)->s->(s->t)->t
% NP2 (e->s->(s->t)->t)->s->(s->t)->t
% Psi e->s->(s->s)->s
% E   s
% Phi s->t

lex(et, dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP2,lambda(NP1,lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(NP1,Psi),E),lambda(E1,appl(appl(appl(NP2,Psi),E1),Phi))))))))).
lex(',', dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP2,lambda(NP1,lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(NP1,Psi),E),lambda(E1,appl(appl(appl(NP2,Psi),E1),Phi))))))))).
lex('-', dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP2,lambda(NP1,lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(NP1,Psi),E),lambda(E1,appl(appl(appl(NP2,Psi),E1),Phi))))))))).
lex(':', dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP2,lambda(NP1,lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(NP1,Psi),E),lambda(E1,appl(appl(appl(NP2,Psi),E1),Phi))))))))).
lex(';', dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP2,lambda(NP1,lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(NP1,Psi),E),lambda(E1,appl(appl(appl(NP2,Psi),E1),Phi))))))))).

% noun conjunctions

% N1  e->s->(s->t)->t
% N2  e->s->(s->t)->t
% X   e
% E   s
% Phi s->t

lex(et, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2,lambda(N1,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N1,X),E),lambda(E1,appl(appl(appl(N2,X),E1),Phi))))))))).
lex(',', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2,lambda(N1,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N1,X),E),lambda(E1,appl(appl(appl(N2,X),E1),Phi))))))))).
lex(':', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2,lambda(N1,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N1,X),E),lambda(E1,appl(appl(appl(N2,X),E1),Phi))))))))).
lex('-', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2,lambda(N1,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N1,X),E),lambda(E1,appl(appl(appl(N2,X),E1),Phi))))))))).
lex(';', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N2,lambda(N1,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N1,X),E),lambda(E1,appl(appl(appl(N2,X),E1),Phi))))))))).

% vp conjunctions

% VP1 ((e->t->(s->t)->t)->s->(s->t)->t)->s->(s->t)->t
% VP2 ((e->t->(s->t)->t)->s->(s->t)->t)->s->(s->t)->t
% NP  (e->s->(s->t)->t)->s->(s->t)->t
%

lex(et, dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))), lambda(VP2,lambda(VP1,lambda(NP,lambda(E,lambda(Phi,appl(appl(appl(VP1,NP),E),lambda(E1,appl(appl(appl(VP2,NP),E1),Phi))))))))).
lex(',', dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))), lambda(VP2,lambda(VP1,lambda(NP,lambda(E,lambda(Phi,appl(appl(appl(VP1,NP),E),lambda(E1,appl(appl(appl(VP2,NP),E1),Phi))))))))).
lex('-', dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))), lambda(VP2,lambda(VP1,lambda(NP,lambda(E,lambda(Phi,appl(appl(appl(VP1,NP),E),lambda(E1,appl(appl(appl(VP2,NP),E1),Phi))))))))).
lex(':', dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))), lambda(VP2,lambda(VP1,lambda(NP,lambda(E,lambda(Phi,appl(appl(appl(VP1,NP),E),lambda(E1,appl(appl(appl(VP2,NP),E1),Phi))))))))).
lex(';', dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))), lambda(VP2,lambda(VP1,lambda(NP,lambda(E,lambda(Phi,appl(appl(appl(VP1,NP),E),lambda(E1,appl(appl(appl(VP2,NP),E1),Phi))))))))).


example_phrase(pos_lemma, 
          "Un homme tres stupide dort .",
          ['Un', det:art, un, dr(0,np,n),
	   'homme', nom, 'homme', n,
	   'très', adv, 'très', dr(0,dl(0,n,n),dl(0,n,n)),
	   'stupide', adj, 'stupide', dl(0,n,n),
	   dort, ver:pres, dormir, dl(0,np,s)],
           lit(s)).
example_phrase(pos_lemma, 
          "Jean se trompe .",
          ['Jean', nam, 'Jean', np,
	   'se', pro:per, 'se', cl_r,
	   'trompe', ver:pres, 'tromper', dl(0,cl_r,dl(0,np,s))],
           lit(s)).
example_phrase(pos_lemma, 
          "Jean s'associe à Marie .",
          ['Jean', nam, 'Jean', np,
	   's\'', pro:per, 'se', cl_r,
	   'associe', ver:pres, 'associer', dr(0,dl(0,cl_r,dl(0,np,s)),pp_a),
	   'à', prp, 'à', dr(0,pp_a,np),
	   'Marie', nam, 'Marie', np],
           lit(s)).
