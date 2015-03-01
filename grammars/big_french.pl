% -*- Mode: Prolog -*-
% ============================================================
% big_french.pl
% ============================================================
% !grail 3.1.1

atomic_type(n, e->t).
atomic_type(np, (e->t)->t).
atomic_type(pp, (e->t)->t).
atomic_type(cl_r12, (e->t)->t).
atomic_type(cl_a3, (e->t)->t).
atomic_type(cl_d3, (e->t)->t).
atomic_type(cl_y, (e->t)->t).
atomic_type(cl_en, (e->t)->t).
atomic_type(cl, (e->t)->t).
atomic_type(cl(_), (e->t)->t).
atomic_type(s, s->t).
atomic_type(s(_), s->t).
atomic_type(cs, s->t).
atomic_type(txt, t).


event_semantics(neo).
discourse_semantics(drt).

% This grammar is made for use with the supertagger and the grammar
% extracted from the Paris VII Treebank.

% It contains a mix of macro definitions, structural rules and the
% start of a semantic component, which will be made more and more
% complete over time.

custom_first_order(lit(pp(Z)), lit(pp, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(s(Z)), lit(s, [X,Y,Z]), _, [X,Y]). 

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
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X)))),dl(0,lit(np),lit(s(X))))).
macro(dr(0,dl(0,np,s),dl(0,np,s)), dr(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).
macro(dl(0,dl(0,np,s),dl(0,np,s)), dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X)))),dl(0,lit(np),lit(s(X))))).

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
macro(pp_a, lit(pp(a))).
macro(pp, lit(pp(_))).

macro(s_inf, lit(s(inf))).
macro(s_deinf, lit(s(deinf))).
macro(s_ainf, lit(s(ainf))).
macro(s_ppres, lit(s(ppres))).
macro(s_ppart, lit(s(ppart))).
macro(s, lit(s(_))).
macro(s_top, lit(s(_))).

macro(cl_3d, lit(cl_d3)).
macro(cl_3a, lit(cl_a3)).
macro(cl_12r, lit(cl_r12)).
macro(cl_en, lit(cl_en)).
macro(cl_y, lit(cl_y)).

macro(dl(0,s,s), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s,s), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_ppart,s_ppart), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s_ppart,s_ppart), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_inf,s_inf), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s_inf,s_inf), dr(0,lit(s(X)),lit(s(X)))).


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

% ====================
% = semantic recipes =
% ====================

semantics(dot, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_semantics(ES, Dis, Sem).

semantics(dot_np, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_np_semantics(ES, Dis, Sem).

semantics(intransitive_verb, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, SubjectRole),
	intransitive_verb_semantics(ES, Dis, Word, Time, SubjectRole, Sem).

semantics(transitive_verb, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, SubjectRole, ObjectRole),
	transitive_verb_semantics(ES, Dis, Word, Time, SubjectRole, ObjectRole, Sem).

semantics(ditransitive_verb_pp, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, Role1, Role2, Role3),
	ditransitive_verb_semantics_pp(ES, Dis, Word, Time, Role1, Role2, Role3, Sem).
semantics(ditransitive_verb_pp_datcl, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, Role3, Role2, Role1),
	ditransitive_verb_semantics_pp_datcl(ES, Dis, Word, Time, Role1, Role2, Role3, Sem).

semantics(reflexive_verb, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, SubjectRole, ObjectRole),
	reflexive_verb_semantics(ES, Dis, Word, Time, SubjectRole, ObjectRole, Sem).

semantics(transitive_verb_pp, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	transitive_verb_semantics_pp(ES, Dis, Word, Time, Sem).

semantics(noun, Word, Sem) :-
	discourse_semantics(Dis),
	noun_semantics(Dis, Word, Sem).

semantics(adjective, Word, Sem) :-
	discourse_semantics(Dis),
	adj_semantics(Dis, Word, Sem).

semantics(transitive_verb, Word, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	role_lexicon(Word, SubjectRole, ObjectRole),
	transitive_verb_semantics(ES, Dis, Word, _-[], SubjectRole, ObjectRole, Sem).


possessive_1p_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(P,X)),appl(Q,X))))).
possessive_2p_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(P,X)),appl(Q,X))))).
possessive_3p_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(P,X)),appl(Q,X))))).

gq_no_semantics(lambda(P,lambda(Q,drs([],[not(merge(drs([variable(X)],[]),merge(appl(Q,X),appl(P,X))))])))).
gq_a_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))).
gq_every_semantics(lambda(P,lambda(Q,drs([],[bool(merge(drs([variable(X)],[]),appl(P,X)),->,appl(Q,X))])))).
gq_the_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(P,X)),appl(Q,X))))).
gq_this_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(P,X)),appl(Q,X))))).

wh_rel_semantics(lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(appl(P,lambda(R,appl(R,X))),_)))))).

adj_semantics(fo, Word, lambda(P, lambda(X, bool(appl(P,X),&,appl(Word,X))))).
adj_semantics(drt, Word, lambda(P,lambda(V, merge(drs([],[appl(Word,V)]),appl(P,V))))).

dot_semantics(none, _, lambda(X,X)).
dot_semantics(classic, Dis, Sem) :-
	dot_semantics1(Dis, Sem).
dot_semantics(neo, Dis, Sem) :-
	dot_semantics1(Dis, Sem).

dot_semantics1(drt, lambda(P, appl(P, _))). 

dot_np_semantics(none, _, lambda(X,X)).
dot_np_semantics(classic, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).
dot_np_semantics(neo, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).

dot_np_semantics1(drt, lambda(P,appl(P,lambda(_V,drs([],[]))))).

noun_semantics(drt, Word, lambda(V,drs([],[appl(Word,V)]))).


intransitive_verb_semantics(none, Dis, Word, Time, _SubjectRole, Sem) :-
	intransitive_verb_semantics_none(Dis, Word, Time, Sem).
intransitive_verb_semantics(classic, Dis, Word, Time, _SubjectRole, Sem) :-
	intransitive_verb_semantics_classic(Dis, Word, Time, Sem).
intransitive_verb_semantics(neo, Dis, Word, Time, SubjectRole, Sem) :-
	intransitive_verb_semantics_neo(Dis, Word, Time, SubjectRole, Sem).

intransitive_verb_semantics_none(drt, Word, _-Time, lambda(P, appl(P,lambda(V, drs([], [appl(Word,V)|Time]))))).
intransitive_verb_semantics_classic(drt, Word, E-Time, lambda(P, lambda(E,appl(P,lambda(V,drs([event(E)], [appl(event,E),appl(appl(Word,V),E)|Time])))))).
intransitive_verb_semantics_neo(drt, Word, E-Time, SubjectRole, lambda(P, lambda(E,appl(P,lambda(V,drs([event(E)], [appl(event,E),appl(Word,E),appl(appl(SubjectRole,V),E)|Time])))))).

transitive_verb_semantics(none, Dis, Word, Time, _SubjectRole, _ObjectRole, Sem) :-
	transitive_verb_semantics_none(Dis, Word, Time, Sem).
transitive_verb_semantics(classic, Dis, Word, Time, _SubjectRole, _ObjectRole, Sem) :-
	transitive_verb_semantics_classic(Dis, Word, Time, Sem).
transitive_verb_semantics(neo, Dis, Word, Time, SubjectRole, ObjectRole, Sem) :-
	transitive_verb_semantics_neo(Dis, Word, Time, SubjectRole, ObjectRole, Sem).

transitive_verb_semantics_none(drt, Word, _-Time, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)|Time])))))))).
transitive_verb_semantics_classic(drt, Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)], [appl(event,E),appl(appl(appl(Word,W),V),E)|Time]))))))))).
transitive_verb_semantics_neo(drt, Word, E-Time, SujectRole, ObjectRole, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(SujectRole,V),E),appl(appl(ObjectRole,W),E)|Time]))))))))).

ditransitive_verb_semantics_pp_datcl(none, Dis, Word, Time, _, _, _, Sem) :-
	ditransitive_verb_semantics_pp_datcl_none(Dis, Word, Time, Sem).
ditransitive_verb_semantics_pp_datcl(classic, Dis, Word, Time, _, _, _, Sem) :-
	ditransitive_verb_semantics_pp_datcl_classic(Dis, Word, Time, Sem).
ditransitive_verb_semantics_pp_datcl(neo, Dis, Word, Time, Role1, Role2, Role3, Sem) :-
	ditransitive_verb_semantics_pp_datcl_neo(Dis, Word, Time, Role1, Role2, Role3, Sem).

ditransitive_verb_semantics_pp_datcl_none(drt, Word, _-Time, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)|Time])))))))).
ditransitive_verb_semantics_pp_datcl_classic(drt, Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)], [appl(event,E),appl(appl(appl(Word,W),V),E)|Time]))))))))).
ditransitive_verb_semantics_pp_datcl_neo(drt, Word, E-Time, Role1, Role2, Role3, lambda(P,lambda(Q,lambda(R,lambda(E,appl(R,lambda(Z,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(Role1,V),E),appl(appl(Role2,W),E),appl(appl(Role3,Z),E)|Time])))))))))))).



ditransitive_verb_semantics_pp(none, Dis, Word, Time, _, _, _, Sem) :-
	ditransitive_verb_semantics_pp_none(Dis, Word, Time, Sem).
ditransitive_verb_semantics_pp(classic, Dis, Word, Time, _, _, _, Sem) :-
	ditransitive_verb_semantics_pp_classic(Dis, Word, Time, Sem).
ditransitive_verb_semantics_pp(neo, Dis, Word, Time, Role1, Role2, Role3, Sem) :-
	ditransitive_verb_semantics_pp_neo(Dis, Word, Time, Role1, Role2, Role3, Sem).

ditransitive_verb_semantics_pp_none(drt, Word, _-Time, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)|Time])))))))).
ditransitive_verb_semantics_pp_classic(drt, Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)], [appl(event,E),appl(appl(appl(Word,W),V),E)|Time]))))))))).
ditransitive_verb_semantics_pp_neo(drt, Word, E-Time, Role1, Role2, Role3, lambda(P,lambda(Q,lambda(R,lambda(E,appl(R,lambda(Z,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(Role3,V),E),appl(appl(Role2,W),E),appl(appl(Role1,Z),E)|Time])))))))))))).

transitive_verb_semantics_pp(none, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_none(Dis, Word, Sem).
transitive_verb_semantics_pp(classic, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_classic(Dis, Word, Sem).
transitive_verb_semantics_pp(neo, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_neo(Dis, Word, Sem).

transitive_verb_semantics_pp_none(drt, Word, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)])))))))).
transitive_verb_semantics_pp_classic(drt, Word, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)], [appl(event,E),appl(appl(appl(Word,W),V),E)]))))))))).
transitive_verb_semantics_pp_neo(drt, Word, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)], [appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(patient,W),E)]))))))))).

reflexive_verb_semantics(none, Dis, Word, Time, _SubjectRole, _ReflRole, Sem) :-
	reflexive_verb_semantics_none(Dis, Word, Time, Sem).
reflexive_verb_semantics(classic, Dis, Word, Time, _SubjectRole, _ReflRole, Sem) :-
	reflexive_verb_semantics_classic(Dis, Word, Time, Sem).
reflexive_verb_semantics(neo, Dis, Word, Time, SubjectRole, ReflRole, Sem) :-
	reflexive_verb_semantics_neo(Dis, Word, Time, SubjectRole, ReflRole, Sem).

reflexive_verb_semantics_none(drt, Word, _-Time, lambda(P,lambda(Q,appl(Q,lambda(_,appl(P,lambda(V, drs([], [appl(appl(Word,V),V)|Time])))))))).
reflexive_verb_semantics_classic(drt, Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(_,appl(P,lambda(V,drs([event(E)], [appl(event,E),appl(appl(appl(Word,V),V),E)|Time]))))))))).
reflexive_verb_semantics_neo(drt, Word, E-Time, SujectRole, ObjectRole, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(_,appl(P,lambda(V,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(SujectRole,V),E),appl(appl(ObjectRole,V),E)|Time]))))))))).


sem_tv_subject_control(Word, lambda(P,lambda(X,lambda(E,merge(drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(patient,F),E)]),merge(appl(X,lambda(Z,drs([],[appl(appl(agent,Z),E)]))),appl(appl(P,X),F))))))).


auxiliary_verb_etre(POS, Rest0, lambda(P,lambda(X,lambda(E,merge(drs([],[appl(event,E),appl(passé,E),appl(terminé,E)|Rest]),merge(appl(X,lambda(Z,drs([],[appl(appl(patient,Z),E)]))),appl(appl(P,lambda(Q,merge(drs([],[bool(V,=,'?')]),appl(Q,V)))),E))))))) :-
     (
          POS = ver:impf
     ->
          Rest = [appl(background,E)|Rest0]
     ;
          Rest = Rest0
     ).
auxiliary_verb_avoir(POS, Rest0, lambda(P,lambda(X,lambda(E,merge(drs([],[appl(event,E),appl(passé,E),appl(terminé,E)|Rest]),merge(appl(X,lambda(Z,drs([],[appl(appl(agent,Z),E)]))),appl(appl(P,X),E))))))) :-
     (
          POS = ver:impf
     ->
          Rest = [appl(background,E)|Rest0]
     ;
          Rest = Rest0
     ).
auxiliary_verb_refl(POS, Rest0, lambda(P,lambda(_,lambda(X,lambda(E,merge(drs([],[appl(event,E),appl(passé,E),appl(terminé,E)|Rest]),merge(appl(X,lambda(Z,drs([],[appl(appl(agent,Z),E),appl(appl(patient,Z),E)]))),appl(appl(P,X),E)))))))) :-
     (
          POS = ver:impf
     ->
          Rest = [appl(background,E)|Rest0]
     ;
          Rest = Rest0
     ).
auxiliary_verb_par(POS, Rest0, lambda(P,lambda(Y,lambda(X,lambda(E,merge(drs([],[appl(event,E),appl(passé,E),appl(terminé,E)|Rest]),merge(appl(X,lambda(Z,appl(Y,lambda(V,drs([],[appl(appl(patient,Z),E),appl(appl(agent,V),E)]))))),appl(appl(P,Y),E)))))))) :-
     (
          POS = ver:impf
     ->
          Rest = [appl(background,E)|Rest0]
     ;
          Rest = Rest0
     ).
auxiliary_verb_acc(POS, Rest0, lambda(P,lambda(Y,lambda(X,lambda(E,merge(drs([],[appl(event,E),appl(passé,E),appl(terminé,E)|Rest]),merge(appl(X,lambda(Z,appl(Y,lambda(V,drs([],[appl(appl(agent,Z),E),appl(appl(patient,V),E)]))))),appl(appl(P,X),E)))))))) :-
     (
          POS = ver:impf
     ->
          Rest = [appl(background,E)|Rest0]
     ;
          Rest = Rest0
     ).


title_semantics(Title, lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(appl(titre,Title),X)]))))))).

transitive_verb_destination(Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(destination,W),E)|Time]))))))))). 

transitive_verb_source(Word, E-Time, lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(source,W),E)|Time]))))))))). 

% =====================================
% =           Role lexicon            =
% =====================================

% = roles for verbs taking a single argument

role_lexicon(tomber, patient) :-
	!.
role_lexicon(_, agent).

% = roles for verbs taking two arguments

role_lexicon(arriver_à, agent, destination) :-
	!.
role_lexicon(arriver, agent, destination) :-
	!.
role_lexicon(partir_de, agent, source) :-
	!.
role_lexicon(partir, agent, source) :-
	!.
role_lexicon(_, agent, patient).

% = roles for verbs taking three arguments

role_lexicon(donner_à, agent, instrument, destination) :-
	!.
role_lexicon(offrir_à, agent, instrument, destination) :-
	!.
role_lexicon(demander_à, agent, instrument, source) :-
	!.
role_lexicon(prendre_de, agent, instrument, source) :-
	!.
role_lexicon(prendre_à, agent, instrument, source) :-
	!.
role_lexicon(mettre, agent, instrument, destination) :-
	!.
role_lexicon(_, agent, instrument, destination).

% =====================================
% =        Tense information          =
% =====================================

pos_time(ver:TENSE, List) :-
	!,
	tense_time(TENSE, List).
pos_time(_, []).

tense_time(impf, E-[appl(past,E)]) :-
	!.
tense_time(simp, E-[appl(past,E)]) :-
	!.
tense_time(cond, E-[appl(possible,E)]) :-
	!.
tense_time(futu, E-[appl(futur,E)]) :-
	!.
tense_time(_, _-[]).


convert_ordinal(Sem, Num) :-
	convert_ordinal1(Sem, Num),
	!.
convert_ordinal(Num, Num).

convert_ordinal1(zéro, 0).
convert_ordinal1(un, 1).
convert_ordinal1(une, 1).
convert_ordinal1(deux, 2).
convert_ordinal1(trois, 3).
convert_ordinal1(quatre, 4).
convert_ordinal1(cinq, 5).
convert_ordinal1(six, 6).
convert_ordinal1(sept, 7).
convert_ordinal1(huit, 8).
convert_ordinal1(neuf, 9).
convert_ordinal1(dix, 10).
convert_ordinal1(onze, 11).
convert_ordinal1(douze, 12).
convert_ordinal1(treize, 13).
convert_ordinal1(quatorze, 14).
convert_ordinal1(quize, 15).
convert_ordinal1(seize, 16).
convert_ordinal1(dix-sept, 17).
convert_ordinal1(dix-huit, 18).
convert_ordinal1(dix-neuf, 19).
convert_ordinal1(vingt, 20).
convert_ordinal1(vingt-et-un, 21).
convert_ordinal1(vingt-deux, 22).
convert_ordinal1(vingt-trois, 23).
convert_ordinal1(vingt-quatre, 24).
convert_ordinal1(vingt-cinq, 25).
convert_ordinal1(vingt-six, 26).
convert_ordinal1(vingt-sept, 27).
convert_ordinal1(vingt-huit, 28).
convert_ordinal1(vingt-neuf, 29).
convert_ordinal1(trente, 30).
convert_ordinal1(trente-et-un, 31).
convert_ordinal1(trente-deux, 32).
convert_ordinal1(trente-trois, 33).
convert_ordinal1(trente-quatre, 34).
convert_ordinal1(trente-cinq, 35).
convert_ordinal1(trente-six, 36).
convert_ordinal1(trente-sept, 37).
convert_ordinal1(trente-huit, 38).
convert_ordinal1(trente-neuf, 39).
convert_ordinal1(quarante, 40).
convert_ordinal1(quarante-et-un, 41).
convert_ordinal1(quarante-deux, 42).
convert_ordinal1(quarante-trois, 43).
convert_ordinal1(quarante-quatre, 44).
convert_ordinal1(quarante-cinq, 45).
convert_ordinal1(quarante-six, 46).
convert_ordinal1(quarante-sept, 47).
convert_ordinal1(quarante-huit, 48).
convert_ordinal1(quarante-neuf, 49).
convert_ordinal1(cinquante, 50).
convert_ordinal1(cinquante-et-un, 51).
convert_ordinal1(cinquante-deux, 52).
convert_ordinal1(cinquante-trois, 53).
convert_ordinal1(cinquante-quatre, 54).
convert_ordinal1(cinquante-cinq, 55).
convert_ordinal1(cinquante-six, 56).
convert_ordinal1(cinquante-sept, 57).
convert_ordinal1(cinquante-huit, 58).
convert_ordinal1(cinquante-neuf, 59).
convert_ordinal1(soixante, 60).
convert_ordinal1(soixante-et-un, 61).
convert_ordinal1(soixante-deux, 62).
convert_ordinal1(soixante-trois, 63).
convert_ordinal1(soixante-quatre, 64).
convert_ordinal1(soixante-cinq, 65).
convert_ordinal1(soixante-six, 66).
convert_ordinal1(soixante-sept, 67).
convert_ordinal1(soixante-huit, 68).
convert_ordinal1(soixante-neuf, 69).
convert_ordinal1(soixante-dix, 70).
convert_ordinal1(soixante-et-onze, 71).
convert_ordinal1(soixante-douze, 72).
convert_ordinal1(soixante-treize, 73).
convert_ordinal1(soixante-quatorze, 74).
convert_ordinal1(soixante-quinze, 75).
convert_ordinal1(soixante-seize, 76).
convert_ordinal1(soixante-dix-sept, 77).
convert_ordinal1(soixante-dix-huit, 78).
convert_ordinal1(soixante-dix-neuf, 79).
convert_ordinal1(quatre-vingts, 80).
convert_ordinal1(quatre-vingts-et-un, 81).
convert_ordinal1(quatre-vingts-deux, 82).
convert_ordinal1(quatre-vingts-trois, 83).
convert_ordinal1(quatre-vingts-quatre, 84).
convert_ordinal1(quatre-vingts-cinq, 85).
convert_ordinal1(quatre-vingts-six, 86).
convert_ordinal1(quatre-vingts-sept, 87).
convert_ordinal1(quatre-vingts-huit, 88).
convert_ordinal1(quatre-vingts-neuf, 89).
convert_ordinal1(quatre-vingts-dix, 90).
convert_ordinal1(quatre-vingts-et-onze, 91).
convert_ordinal1(quatre-vingts-douze, 92).
convert_ordinal1(quatre-vingts-treize, 93).
convert_ordinal1(quatre-vingts-quatorze, 94).
convert_ordinal1(quatre-vingts-quinze, 95).
convert_ordinal1(quatre-vingts-seize, 96).
convert_ordinal1(quatre-vingts-dix-sept, 97).
convert_ordinal1(quatre-vingts-dix-huit, 98).
convert_ordinal1(quatre-vingts-dix-neuf, 99).
convert_ordinal1(cent, 100).
convert_ordinal1(cents, 100).
convert_ordinal1(mille, '1.000').
convert_ordinal1(million, '1.000.000').
convert_ordinal1(millions, '1.000.000').
convert_ordinal1(milliard, '1.000.000.000').
convert_ordinal1(milliards, '1.000.000.000').

convert_cardinal(premier, 1).
convert_cardinal(première, 1).
convert_cardinal(deuxième, 2).
convert_cardinal(troisième, 3).
convert_cardinal(quatrième, 4).
convert_cardinal(cinquième, 5).
convert_cardinal(sixième, 6).
convert_cardinal(septième, 7).
convert_cardinal(huitième, 8).
convert_cardinal(neuvième, 9).
convert_cardinal(dixième, 10).

add_lefff_info(h, X, [appl(humain,X)]).
add_lefff_info(hm, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info(hf, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info(hs, X, [appl(humain,X)]).
add_lefff_info(hms, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info(hfs, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info(hp, X, [appl(humain,X)]).
add_lefff_info(hmp, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info(hfp, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info(l, X, [appl(lieu,X)]).
add_lefff_info(lm, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info(lf, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info(ls, X, [appl(lieu,X)]).
add_lefff_info(lms, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info(lfs, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info(lp, X, [appl(lieu,X)]).
add_lefff_info(lmp, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info(lfp, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info(m, X, [appl(masculin,X)]).
add_lefff_info(f, X, [appl(féminin,X)]).
add_lefff_info(mp, X, [appl(masculin,X)]).
add_lefff_info(fp, X, [appl(féminin,X)]).
add_lefff_info(ms, X, [appl(masculin,X)]).
add_lefff_info(fs, X, [appl(féminin,X)]).

%sequence_semantics([_,_|Ws], Ws, [_,_|Ps], Ps, [L1,L2|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X,X),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(Q,lambda(Z,merge(drs([],[appl(L1,appl(L2,V))]),appl(Q,Z))))|Ss], Ss).
% intensifiers
sequence_semantics([_,et,_|Ws], Ws, [num,kon,num|Ps], Ps, [A,_,B|Ls], Ls, [dr(0,lit(np),lit(n)),dr(0,dl(0,dr(0,lit(np),lit(n)),dr(0,lit(np),lit(n))),dr(0,lit(np),lit(n))),dr(0,lit(np),lit(n))|Fs], Fs, [dr(0,lit(np),lit(n))-lambda(Z,Z),dr(0,dl(0,dr(0,lit(np),lit(n)),dr(0,lit(np),lit(n))),dr(0,lit(np),lit(n)))-lambda(V,V),dr(0,lit(np),lit(n))-lambda(R,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(R,X)),appl(Q,X))))|Ss], Ss) :-
	convert_ordinal(B, 1),
	convert_ordinal(A, N0),
	integer(N0),
	Num is N0 + 1.

sequence_semantics([_,_|Ws], Ws, [_,_|Ps], Ps, [L1,L2|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X,X),dl(0,lit(n),lit(n))-lambda(Q,lambda(Z,merge(drs([],[appl(L1,appl(L2,Z))]),appl(Q,Z))))|Ss], Ss).
sequence_semantics([environ,W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(n),lit(n))-lambda(X,X),dr(0,lit(n),lit(n))-lambda(P,lambda(V, merge(drs([],[bool(appl(num,V),approx,Num)]),appl(P,V))))|Ss], Ss) :-
	convert_ordinal(W, Num).
sequence_semantics([environ,W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np),lit(np)),dr(0,lit(np),lit(n))|Fs], Fs, [dr(0,lit(np),lit(np))-lambda(X,X),dr(0,lit(np),lit(n))-lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),approx,Num)]),appl(P,X)),appl(Q,X))))|Ss], Ss) :-
	convert_ordinal(W, Num).
sequence_semantics(['Environ',W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(n),lit(n))-lambda(X,X),dr(0,lit(n),lit(n))-lambda(P,lambda(V, merge(drs([],[bool(appl(num,V),approx,Num)]),appl(P,V))))|Ss], Ss) :-
	convert_ordinal(W, Num).
sequence_semantics(['Environ',W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np),lit(np)),dr(0,lit(np),lit(n))|Fs], Fs, [dr(0,lit(np),lit(np))-lambda(X,X),dr(0,lit(np),lit(n))-lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),approx,Num)]),appl(P,X)),appl(Q,X))))|Ss], Ss) :-
	convert_ordinal(W, Num).

% =====================================
% =  Default semantics with POS tag   =
% =====================================

% = numbers

default_semantics(W, num, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(appl(rank,V),=,Num)]),appl(P,V))))) :-
	convert_cardinal(W, Num).
default_semantics(W, num, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_ordinal(W, Num).
default_semantics(W, num, lit(n), lambda(X,drs([],[bool(X,=,Num)]))) :-
	convert_ordinal(W, Num).
default_semantics(W, num, lit(np), lambda(P,merge(drs([variable(X)],[bool(X,=,Num)]),appl(P,X)))) :-
	convert_ordinal(W, Num).
default_semantics(W, adj, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_ordinal1(W, Num).
default_semantics(W, adj, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_ordinal1(W, Num).
default_semantics(W, adj, lit(n), lambda(X,drs([],[bool(num(X),=,Num)]))) :-
	convert_ordinal1(W, Num).
default_semantics(W, nom, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_ordinal1(W, Num).
default_semantics(W, nom, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_ordinal1(W, Num).
default_semantics(W, nom, lit(n), lambda(X,drs([],[bool(num(X),=,Num)]))) :-
	convert_ordinal1(W, Num).

% = nouns
% = NOM

default_semantics(W, nom, lit(n), lambda(X,drs([],[appl(W,X)]))).
default_semantics(W, nom:INFO, lit(n), lambda(X,drs([],[appl(W,X)|Rest]))) :-
	add_lefff_info(INFO, X, Rest).

% = NAM

default_semantics(W, nam, lit(n), lambda(X,drs([],[appl(W,X)]))).
default_semantics(W, nam:INFO, lit(n), lambda(X,drs([],[appl(W,X)|Rest]))) :-
	add_lefff_info(INFO, X, Rest).

% = noun phrase - name

default_semantics(Word, nam, lit(np), lambda(P,merge(drs([variable(X)],[appl(appl(nommé,Word),X)]),appl(P,X)))).
default_semantics(Word, nam, dr(0,lit(np),lit(np)), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)]))),appl(P,X))))).
default_semantics(Word, nam:INFO, lit(np), lambda(P,merge(drs([variable(X)],[appl(appl(nommé,Word),X)|Rest]),appl(P,X)))) :-
	add_lefff_info(INFO, X, Rest).
default_semantics(Word, nam:INFO, dr(0,lit(np),lit(np)), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)|Rest]))),appl(P,X))))) :-
	add_lefff_info(INFO, X, Rest).

%= verbs

% = intransitive

default_semantics(W, POS, dl(_,lit(np),lit(s(_))), Sem) :-
	pos_time(POS, Time),
	semantics(intransitive_verb, W, Time, Sem).

% = intransitive - verb initial

default_semantics(W, POS, dr(_,lit(s(_)),lit(np)), Sem) :-
	pos_time(POS, Time),
        semantics(intransitive_verb, W, Time, Sem).

% = transitive - V2

default_semantics(Word, POS, dr(_,dl(_,lit(np),lit(s(_))),lit(np)), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = transitive - verb initial

default_semantics(Word, POS, dr(_,dr(_,lit(s(_)),lit(np)),lit(np)), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = transitive - verb final

default_semantics(Word, POS, dl(_,lit(np),dl(_,lit(np),lit(s(_)))), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = ditransitive - V2

default_semantics(Word, POS, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(np)),lit(np)), Sem) :-
	pos_time(POS, Time),
	semantics(ditransitive_verb, Word, Time, Sem).

% = ditransitive - S V PP O

default_semantics(Word, POS, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(pp(PRP))),lit(np)), Sem) :-
    (
        var(PRP)
    ->
        PW = Word
    ;
        atomic_list_concat([Word,'_',PRP], PW)
    ),
	pos_time(POS, Time),
	semantics(ditransitive_verb_pp, PW, Time, Sem).

% = reflexive

default_semantics(Word, POS, dl(_,lit(cl_r12),dl(_,lit(np),lit(s(_)))), Sem) :-
	pos_time(POS, Time),
	semantics(reflexive_verb, Word, Time, Sem).

default_semantics(Word, POS, dr(0,dl(_,lit(cl_d3),dl(_,lit(np),lit(s(_)))),lit(np)), Sem) :-
	pos_time(POS, Time),
    (
        var(Word)
    ->
        PW = Word
    ;
	atomic_list_concat([Word,'_à'], PW)
    ),
	semantics(ditransitive_verb_pp_datcl, PW, Time, Sem).

% = auxiliary verbs

default_semantics(être, POS, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(POS, [], Sem).
default_semantics(être, POS, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(pp(par))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_par(POS, [], Sem).
default_semantics(être, POS, dr(_,dl(_,lit(cl_r12),dl(_,lit(np),lit(s(_)))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_refl(POS, [], Sem).
default_semantics(être, POS, dr(_,dl(_,lit(cl_r12),dl(_,lit(np),lit(s(_)))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_acc(POS, [], Sem).

default_semantics(avoir, POS, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_avoir(POS, [], Sem).
default_semantics(avoir, POS, dr(_,dl(0,lit(cl_r12),dl(_,lit(np),lit(s(_)))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_refl(POS, [], Sem).
default_semantics(avoir, POS, dr(_,dl(0,lit(cl_a3),dl(_,lit(np),lit(s(_)))),dl(_,lit(np),lit(s(ppart)))), Sem) :-
	auxiliary_verb_acc(POS, [], Sem).

% = verbs with a destination argument

default_semantics(arriver, POS, dr(_,dl(_,lit(np),lit(s(_))),lit(pp(_))), Sem) :-
	pos_time(POS, Time),
   	semantics(transitive_verb, arriver_à, Time, Sem).

% = verbs with a source argument

default_semantics(partir, POS, dr(_,dl(_,lit(np),lit(s(_))),lit(pp(_))), Sem) :-
	pos_time(POS, Time),
   	semantics(transitive_verb, partir_de, Time, Sem).


% = generic auxiliary verbs (default to subject raising)

default_semantics(W, ver:TIME, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(appl(NP,lambda(X,drs([event(E),event(F)],[appl(W,E),appl(appl(agent,X),E),appl(appl(patient,F),E)|Pred]))),appl(appl(VP,NP),F)))))) :-
	tense_time(TIME, E-Pred).
default_semantics(W, ver:TIME, dl(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(appl(NP,lambda(X,drs([event(E),event(F)],[appl(W,E),appl(appl(agent,X),E),appl(appl(patient,F),E)|Pred]),appl(appl(VP,NP),F)))))))) :-
	tense_time(TIME, E-Pred).

% = VP-level adverbs

default_semantics(W, adv, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).
default_semantics(W, adv, dl(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).

% = past and present participles used as adjectives

default_semantics(W, ver:pper, dl(_,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([event(E)],[appl(background,E),appl(W,E),appl(appl(ObjectRole,V),E)]),appl(P,V))))) :-
	role_lexicon(W, _, ObjectRole).
default_semantics(W, ver:pper, dr(_,dl(_,lit(n),lit(n)),lit(pp(par))), lambda(Q,lambda(P,lambda(V,merge(appl(Q,lambda(Z,drs([],[drs([event(E)],[appl(background,E),appl(W,E),appl(appl(ObjectRole,V),E),appl(appl(SubjectRole,Z),E)])]))),appl(P,V)))))) :-
	role_lexicon(W, SubjectRole, ObjectRole).
default_semantics(W, ver:pper, dr(_,dl(_,lit(n),lit(n)),lit(pp(PRP))), lambda(Q,lambda(P,lambda(V,merge(appl(Q,lambda(Z,drs([event(E)],[appl(background,E),appl(PW,E),appl(appl(ArgRole2,V),E),appl(appl(ArgRole3,Z),E)]))),appl(P,V)))))) :-
    (
        var(PRP)
    ->
        PW = W
    ;
        atomic_list_concat([W,'_',PRP], PW)
    ),
	role_lexicon(PW, _, ArgRole2, ArgRole3).

default_semantics(W, ver:ppre, dl(_,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([event(E)],[appl(W,E),appl(appl(agent,V),E)]),appl(P,V))))).
default_semantics(W, ver:ppre, dr(_,dl(_,lit(n),lit(n)),lit(np)), lambda(Q,lambda(P,lambda(V, merge(appl(Q,lambda(Z,drs([event(E)],[appl(W,E),appl(appl(SubjectRole,V),E),appl(appl(ObjectRole,Z),E)]))),appl(P,V)))))) :-
	role_lexicon(W, SubjectRole, ObjectRole).
default_semantics(W, ver:ppre, dr(_,dl(_,lit(n),lit(n)),lit(pp(PRP))), lambda(Q,lambda(P,lambda(V,merge(appl(Q,lambda(Z,drs([event(E)],[appl(background,E),appl(PW,E),appl(appl(ArgRole1,V),E),appl(appl(ArgRole2,Z),E)]))),appl(P,V)))))) :-
    (
        var(PRP)
    ->
        PW = W
    ;
        atomic_list_concat([W,'_',PRP], PW)
    ),
	role_lexicon(PW, ArgRole1, ArgRole2).


% lambda(P, lambda(E,appl(P,lambda(V,drs([event(E)], [appl(event,E),appl(Word,E),appl(appl(SubjectRole,V),E)|Time]))))))
%lex(il, lit(np), lambda(P,merge(drs([],[bool(X,=,'masculin?')]),appl(P,X)))).

% =====================================
% = Default semantics without POS tag =
% =====================================

% = intransitive

default_semantics(W, dl(_,lit(np),lit(s(_))), Sem) :-
	semantics(intransitive_verb, W, _-[], Sem).

% = intransitive - verb initial

default_semantics(W, dr(_,lit(s(_)),lit(np)), Sem) :-
        semantics(intransitive_verb, W, _-[], Sem).

% = transitive - V2

default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),lit(np)), Sem) :-
	semantics(transitive_verb, Word, _-[], Sem).

% = transitive - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),lit(np)),lit(np)), Sem) :-
	semantics(transitive_verb, Word, _-[], Sem).

% = transitive - verb final

default_semantics(Word, dl(_,lit(np),dl(_,lit(np),lit(s(_)))), Sem) :-
	semantics(transitive_verb, Word, _-[], Sem).

% = ditransitive

default_semantics(Word, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(np)),lit(np)), lambda(P, lambda(Q, lambda(R, lambda(E, appl(R,lambda(X,appl(Q,lambda(Y,appl(P,lambda(Z,drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(agent,X),E),appl(appl(patient,Z),E),appl(appl(instrument,Y),E)])))))))))))).

% = ditransitive + (np + pp)

default_semantics(Word, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(pp(Prep))),lit(np)), lambda(P, lambda(Q, lambda(R, lambda(E, appl(R,lambda(X,appl(Q,lambda(Y,appl(P,lambda(Z,drs([event(E)],[appl(event,E),appl(PrepWord,E),appl(appl(agent,X),E),appl(appl(patient,Y),E),appl(appl(instrument,Z),E)])))))))))))) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        atomic_list_concat([Word,'_',Prep], PrepWord)
    ).

% = transitive - sentential complement

default_semantics(Word, dr(_,dl(_,lit(np),lit(s)),lit(cs)), lambda(Q,lambda(P,lambda(E,merge(appl(P,lambda(V,drs([event(E),event(F)], [appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(patient,V),F)]))),appl(Q,F)))))).

default_semantics(Word, dr(_,dl(_,lit(np),lit(s)),lit(s)),  lambda(Q,lambda(P,lambda(E,merge(appl(P,lambda(V,drs([event(E),event(F)], [appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(patient,V),F)]))),appl(Q,F)))))).

% = transitive + pp

default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),lit(pp(Prep))), Sem) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        atomic_list_concat([Word,'_',Prep], PrepWord)
    ),
    	semantics(transitive_verb, PrepWord, Sem).

% = transitive + prep + np

default_semantics(Word, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(pp(Prep)))),lit(np)), Sem) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        atomic_list_concat([Word,'_',Prep], PrepWord)
    ),
	semantics(transitive_verb_pp, PrepWord, Sem).


% = copula  - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),dr(0,lit(n),lit(n))), lit(np)),
		  lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E),variable(W)],[appl(event,E),appl(appl(property,W),E),appl(Word,E),appl(appl(agent,X),E)]),appl(appl(Adj,lambda(_,drs([],[]))),W)))))))).
default_semantics(Word, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), lit(np)),
		  lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E),variable(W)],[appl(event,E),appl(appl(property,W),E),appl(Word,E),appl(appl(agent,X),E)]),appl(appl(Adj,lambda(_,drs([],[]))),W)))))))).

% = copula  - verb second

default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),dr(0,lit(n),lit(n))),
		  lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E),variable(W)],[appl(event,E),appl(appl(property,W),E),appl(Word,E),appl(appl(agent,X),E)]),appl(appl(Adj,lambda(_,drs([],[]))),W)))))))).
		  
default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),dl(0,lit(n),lit(n))), 
		  lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E),variable(W)],[appl(event,E),appl(appl(property,W),E),appl(Word,E),appl(appl(agent,X),E)]),appl(appl(Adj,lambda(_,drs([],[]))),W)))))))).

% ===================
% = subject control =
% ===================

% = destiner + ainf

% = viser + ainf

% = vouloir + inf 
% P: type(np) -> type(inf) (inf)
% X: type(np) (sujet)
% type(np) = (e->t)->t

default_semantics(vouloir, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), Sem) :-
	sem_tv_subject_control(vouloir, Sem).

% = penser + ainf

default_semantics(penser, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(ainf)))), Sem) :-
	sem_tv_subject_control(penser, Sem).

% = penser + inf

default_semantics(penser, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), Sem) :-
	sem_tv_subject_control(penser, Sem).


% = venir + deinf

default_semantics(venir, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(passé_immédiat,E)]),appl(appl(P,X),E)))))).

% = essayer

% = promettre


default_semantics(promettre, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))), lambda(NPO, lambda(TOINF, lambda(NPS, lambda(E, merge(appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(F)],[appl(appl(appl(appl(promised,F),Y),X),E)]))))),appl(appl(TOINF,NPS),F))))))).

% ===================
% = object control =
% ===================

% np = (e->t)->t
% lambda(P          e->s->t
%    lambda(e       s
%       drt(...)))
% lambda(P,lambda(E,merge(drs(['Jean'],[]),appl(appl(P,Jean),E)))).
% appl(NP, lambda(P, lambda(E, appl(appl(NP, P), E)))).
% appl(NP, 
% lambda(P,lambda(E,merge(appl(appl(NP,P),E),drs([],[appl(appl(patient,m),E)])

% = convaincre

% = persuader + np + deinf

default_semantics(persuader, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))),lit(np)), lambda(NPO, lambda(TOINF, lambda(NPS, lambda(E, merge(appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(F)],[appl(appl(appl(appl(persuader,F),Y),X),E)]))))),appl(appl(TOINF,NPO),F))))))).

% = permettre + deinf

default_semantics(permettre, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(permettre,E),appl(appl(instrument,F),E)]),merge(appl(X,lambda(Z,drs([],[appl(appl(agent,Z),E)]))),appl(P,lambda(Q,lambda(F,merge(drs([variable(Y)],[]),appl(Q,Y),F)))))))))).

% = permettre + pp_a + deinf

default_semantics(permettre, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))),lit(pp(a))), lambda(NPO, lambda(DEINF, lambda(NPS, lambda(E, merge(appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(E),event(F)],[appl(event,E),appl(event,F),appl(permettre,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(instrument,F),E)]))))),appl(appl(DEINF,NPO),F))))))).


% = aimer + inf

default_semantics(aimer, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(appl(X,lambda(V,drs([event(E),event(F)],[appl(aimer,E),appl(event,E),appl(appl(agent,V),E),appl(appl(patient,F),E)]))),appl(P,X),F))))).

% = faire + inf

default_semantics(faire, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(faire,E),appl(appl(agent,_Subj),E),appl(appl(patient,F),E)]),appl(appl(P,X),F)))))).

% = laisser + inf

default_semantics(laisser, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(laisser,E),appl(appl(agent,_Subj),E),appl(appl(patient,F),E)]),appl(appl(P,X),F)))))).

% = prepositions - noun modifiers

default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(W,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(np)), lambda(NP, lambda(N, lambda(X, merge(appl(N,X),appl(NP,lambda(Y,drs([],[appl(appl(W,Y),X)])))))))).
default_semantics(W, dr(0,dl(0,lit(np),lit(np)),lit(np)),lambda(P,lambda(Q,lambda(Z,merge(appl(P,lambda(X,appl(Q,lambda(Y,drs([],[appl(appl(W,X),Y)]))))),appl(Q,Z)))))).

% P = lambda(Y,lambda(_,drs([],[appl(appl(W,Y),X)])))
% lambda(Q,lambda(E,merge(merge(drs([Z],[]),drs([],[appl(appl(W,Z),X)]),appl(appl(Q,Z),E)))))
% NP = lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))

% NP modifiers

default_semantics(monsieur, dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(monsieur, Sem).
default_semantics('M', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(monsieur, Sem).
default_semantics('M.', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(monsieur, Sem).
default_semantics('M\.', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(monsieur, Sem).

default_semantics(madame, dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(madame, Sem).
default_semantics('Mme', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(madame, Sem).
default_semantics('Mme.', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(madame, Sem).
default_semantics('Mme\.', dr(_,lit(np),lit(np)), Sem) :-
	title_semantics(madame, Sem).

default_semantics(W, dr(_,lit(np),lit(np)), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).
default_semantics(W, dl(_,lit(np),lit(np)), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).

% = prepositions - arguments

default_semantics(_, dr(_,lit(pp(_)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))).

default_semantics(_, dr(_,lit(pp(_)),lit(np)), lambda(X,X)).

% = noun phrases - except for names

default_semantics(Word, lit(np), lambda(P,merge(drs([variable(X)],[appl(Word,X)]),appl(P,X)))).

% = nouns

default_semantics(W, lit(n), Sem) :-
	semantics(noun, W, Sem).

% = adjective

default_semantics(W, dr(_,lit(n),lit(n)), Sem) :-
	    semantics(adjective, W, Sem).
default_semantics(W, dl(_,lit(n),lit(n)), Sem) :-
	    semantics(adjective, W, Sem).

% = adjective + preposition "inférieur à", etc.

default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),lit(pp(Prep))), lambda(Prp,lambda(N,lambda(X,merge(appl(N,X),appl(Prp,lambda(Y,drs([],[appl(appl(PrepWord,Y),X)])))))))) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        atomic_list_concat([Word,'_',Prep], PrepWord)
    ).

% = generic determiner type (used for example for adjectives in noun phrases without determiner)

default_semantics(W, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[appl(W,X)]),appl(P,X)),appl(Q,X))))).

% = adverbs - sentence modifiers

default_semantics(W, dl(_,lit(s(_)),lit(s(_))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).
default_semantics(W, dr(_,lit(s(_)),lit(s(_))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).

% = adverbs - infinitives with "à" and "de"

default_semantics(de, dr(_,dl(_,lit(np),lit(s(deinf))),dl(_,lit(np),lit(s(inf)))), lambda(X,X)).
default_semantics(à, dr(_,dl(_,lit(np),lit(s(ainf))),dl(_,lit(np),lit(s(inf)))), lambda(X,X)).

% = adverbial prepositions

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(N,X)),appl(S,E)))))).
default_semantics(W, dr(0,dr(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(N,X)),appl(S,E)))))).

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(np)), lambda(NP,lambda(S,lambda(E,merge(appl(NP,lambda(X,drs([],[appl(appl(W,X),E)]))),appl(S,E)))))).

default_semantics(W, dr(0,dr(_,lit(s(_)),lit(s(_))),lit(np)), lambda(NP,lambda(S,lambda(E,merge(appl(NP,lambda(X,drs([],[appl(appl(W,X),E)]))),appl(S,E)))))).

% ============================================================
% Lexicon
% ============================================================


lex('En', dr(0,dr(0,s,s),dl(0,np,s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(pendant,F),E)]))))))).
lex('En', dr(0,dl(1,s,s),dl(0,np,s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(pendant,F),E)]))))))).
lex(en, dr(0,dr(0,s,s),dl(0,np,s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(pendant,F),E)]))))))).
lex(en, dr(0,dl(1,s,s),dl(0,np,s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(pendant,F),E)]))))))).

lex('Pour', dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex('Pour', dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex(pour, dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex(pour, dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).

lex('Sans', dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex('Sans', dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex(sans, dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex(sans, dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).

lex('Avant', dr(0,dr(0,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex('Avant', dr(0,dl(1,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex(avant, dr(0,dr(0,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex(avant, dr(0,dl(1,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex('Avant', dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex('Avant', dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex(avant, dr(0,dr(0,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).
lex(avant, dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(avant,F),E)]))))))).

lex('Afin', dr(0,dr(0,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex('Afin', dr(0,dl(1,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex(afin, dr(0,dr(0,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).
lex(afin, dr(0,dl(1,s,s),dl(0,np,s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(but,F),E)]))))))).

lex(aussi, dl(0,lit(np),lit(np)), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(agent,X),E),bool(E,=,'?event')]))),appl(P,X))))).

% = proper nouns - French

lex('Je', lit(np), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('J', lit(np), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex(je, lit(np), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex(j, lit(np), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).

lex('On', lit(np), lambda(P,merge(drs([X],[]),appl(P,X)))).
lex(on, lit(np), lambda(P,merge(drs([X],[]),appl(P,X)))).

lex('Tu', lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('T', lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(te, lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(tu, lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(t, lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('Vous', lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(vous, lit(np), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).

lex('Il', lit(np), lambda(P,merge(drs([],[bool(X,=,'masculin?')]),appl(P,X)))).
lex('Elle', lit(np), lambda(P,merge(drs([],[bool(X,=,'feminin?')]),appl(P,X)))).
lex(il, lit(np), lambda(P,merge(drs([],[bool(X,=,'masculin?')]),appl(P,X)))).
lex(elle, lit(np), lambda(P,merge(drs([],[bool(X,=,'feminin?')]),appl(P,X)))).
lex('l\'', lit(np), lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))).
lex('l\'', lit(cl_a3), lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))).
lex(lui, lit(np), lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))).
lex(lui, lit(cl_d3), lambda(P,merge(drs([],[bool(X,=,'?')]),appl(P,X)))).
lex(leur, lit(np), lambda(P,merge(drs([],[bool(X,=,'?')],appl(plural,X)),appl(P,X)))).
lex(leur, lit(cl_d3), lambda(P,merge(drs([],[bool(X,=,'?'),appl(plural,X)]),appl(P,X)))).
lex(le, lit(np), lambda(P,merge(drs([],[bool(X,=,'masculin?')]),appl(P,X)))).
lex(le, lit(cl_a3), lambda(P,merge(drs([],[bool(X,=,'masculin?')]),appl(P,X)))).
lex(la, lit(np), lambda(P,merge(drs([],[bool(X,=,'feminin?')]),appl(P,X)))).
lex(la, lit(cl_a3), lambda(P,merge(drs([],[bool(X,=,'feminin?')]),appl(P,X)))).
lex(se, lit(np), lambda(P,appl(P,_))).
lex(se, lit(cl_r12), lambda(P,appl(P,_))).
lex('s\'', lit(np), lambda(P,appl(P,_))).
lex('s\'', lit(cl_r12), lambda(P,appl(P,_))).

lex(ne, dr(0,dl(0,np,s),dl(0,np,s)), lambda(X,X)).
lex(ne, dl(0,dl(0,np,s),dl(0,np,s)), lambda(X,X)).

% Generalized Quantifiers - French

% No

lex(aucun, dr(0,lit(np),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex(aucune, dr(0,lit(np),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex('Aucun', dr(0,lit(np),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex('Aucune', dr(0,lit(np),lit(n)), Sem) :-
	gq_no_semantics(Sem).

% Demonstratives

lex(ce, dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex(cet, dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex(cette, dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Ce', dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Cet', dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Cette', dr(0,lit(np),lit(n)), Sem) :-
	gq_this_semantics(Sem).

% Indefinites

lex('Certains', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Des', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('D\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Du', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(des, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(du, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(de, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('d\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).

% Definites

lex(le, dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('Le', dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(les, dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('Les', dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(la, dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('La', dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('l\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('L\'', dr(0,lit(np),lit(n)), Sem) :-
	gq_the_semantics(Sem).

% Existential

lex(un, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Un', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(une, dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Une', dr(0,lit(np),lit(n)), Sem) :-
	gq_a_semantics(Sem).

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

% Possessives
			
lex(notre, dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).
lex('Notre', dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).
lex(mon, dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).
lex('Mon', dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).
lex(ma, dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).
lex('Ma', dr(0,lit(np),lit(n)), Sem) :-
	possessive_1p_semantics(Sem).

lex(votre, dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).
lex('Votre', dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).
lex(ton, dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).
lex('Ton', dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).
lex(ta, dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).
lex('Ta', dr(0,lit(np),lit(n)), Sem) :-
	possessive_2p_semantics(Sem).

lex(leur, dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex('Leur', dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex(son, dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex('Son', dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex(ses, dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex('Ses', dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex(sa, dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).
lex('Sa', dr(0,lit(np),lit(n)), Sem) :-
	possessive_3p_semantics(Sem).

% Prepositions

lex('d\'', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
lex(du, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
lex(des, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
lex(au, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(à,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
lex(aux, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(à,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).

% plus_de

lex(plus, dr(0,lit(np),lit(pp(de))), lambda(Q,lambda(P,merge(appl(Q,lambda(Y,drs([variable(X)],[bool(X,=,appl(plus_de,Y))]))),appl(P,X))))).

% Discourse connectives

lex(mais, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).

% conjunctions

lex(et, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(expansion,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
lex(et, dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([],[appl(appl(simultanée,F),E)]),appl(appl(P,N),_)),appl(appl(Q,N),E))))))).
% P,Q ((e->t)->t)->s->t
% N (e->t)->t
% E s
lex(',', dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
lex('\,', dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
% P,Q (e->t)->(e->t)
% R e->t
% X e
lex(et, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(P,lambda(Q,lambda(X,merge(appl(P,X),appl(Q,X)))))).
lex(et, dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP1,P),appl(NP2,P)))))).
lex(et, dr(0,dl(0,lit(pp(_)),lit(pp(_))),lit(pp(_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP1,P),appl(NP2,P)))))).

%adj_semantics(drt, Word, lambda(P,lambda(V, merge(drs([],[appl(Word,V)]),appl(P,V))))).


lex(que, dr(0,lit(cs),lit(s(_))), lambda(X,X)).

% = relativization

% VP np -> s
% ((e->t)->t) -> s->t

% NP
% (e->t)->t

% P
% e->t
% lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).

% wh_rel_semantics(lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(appl(P,lambda(R,appl(R,X))),_)))))).

lex(qui, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
%lex(qui, dr(0,dl(0,lit(np),lit(np)),dl(0,lit(np),lit(s(_)))), lambda(VP,lambda(NP,lambda(P,appl(P,X),appl(appl(VP,lambda(R,appl(R,X))),_))))).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(0,box(0,lit(np))))), Sem) :-
	wh_rel_semantics(Sem).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(np))))), Sem) :-
	wh_rel_semantics(Sem).

% = coordination

lex('Si', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(si,   dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(car,  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(mais, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et,   dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).

% = interpunction

lex('.', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('...', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('.', dl(0,lit(np),lit(txt)), Sem) :-
	semantics(dot_np, Sem).
% NP (e->t)->t
% P e->t
%  VP ((e->t)->t)->s->t
lex(',',  dr(0,dl(0,dl(0,lit(np),lit(s(Flex))),lit(np)),lit(np)), lambda(NP,lambda(VP,lambda(P,merge(appl(NP,P),merge(drs([event(E)],Conds),appl(appl(VP,NP),E))))))) :-
    (
        Tense = ppart
    ->
        Conds = [appl(background,E)]
    ;
        Tense = ppres
    ->
        Conds = [appl(continuing,E)]
    ;
        Conds = []
    ).
lex(',', dr(0,dl(0,dl(0,lit(np),lit(s(_))),dl(0,lit(np),lit(s(_)))),dl(0,lit(np),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([],[appl(appl(simultanée,F),E)]),appl(appl(P,N),_)),appl(appl(Q,N),E))))))).
lex(',',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\,', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(';',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\;', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(':',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\:', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('-',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).

lex(y, dr(0,dl(0,np,s),dl(0,np,s)), lambda(P,lambda(Q,lambda(E,merge(drs([variable(X)],[bool(X,=,'endroit?')]),appl(appl(P,Q),E)))))).
lex(pour, dr(0,dl(1,s,s),dl(0,np,s_inf)), lambda(P,lambda(S,lambda(E,merge(merge(drs([event(F)],[appl(appl(but,E),F)]),appl(S,E)),appl(appl(P,lambda(Q,appl(Q,_))),F)))))).

lex(',', dl(0,n,n), lambda(X,X)).
lex(',', dl(0,np,np), lambda(X,X)).
lex(',', dl(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex(',', dr(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex((','), dl(0,n,n), lambda(X,X)).
lex((','), dl(0,np,np), lambda(X,X)).
lex((','), dl(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex((','), dr(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex('\,', dl(0,n,n), lambda(X,X)).
lex('\,', dl(0,np,np), lambda(X,X)).
lex('\,', dl(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex('\,', dr(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex(',', dr(0,dl(0,lit(np),lit(np)),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(N,X),appl(P,X)))))))).
lex('\,', dr(0,dl(0,lit(np),lit(np)),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(N,X),appl(P,X)))))))).
