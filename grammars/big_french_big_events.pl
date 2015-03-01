% -*- Mode: Prolog -*-
% ============================================================
% big_french.pl
% ============================================================
% !grail 3.1.1

atomic_type(n, e->s->t).
atomic_type(np, (e->s->t)->s->t).
atomic_type(pp, (e->s->t)->s->t).
atomic_type(cl, (e->s->t)->s->t).
atomic_type(cl(_), (e->s->t)->s->t).
atomic_type(s, s->t).
atomic_type(s(_), s->t).
atomic_type(cs, s->t).
atomic_type(txt, s->t).


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



macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X)))),dl(0,lit(np),lit(s(X))))).
macro(dr(0,dl(0,np,s),dl(0,np,s)), dr(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).
macro(dl(0,dl(0,np,s),dl(0,np,s)), dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X))))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np),lit(s(X))),dl(0,lit(np),lit(s(X)))),dl(0,lit(np),lit(s(X))))).

macro(pp_pour, lit(pp(pour))).
macro(pp_par, lit(pp(par))).
macro(pp_sur, lit(pp(sur))).
macro(pp_en, lit(pp(en))).
macro(pp_dans, lit(pp(dans))).
macro(pp_comme, lit(pp(comme))).
macro(pp_contre, lit(pp(contre))).
macro(pp_avec, lit(pp(avec))).
macro(pp_dans, lit(pp(dans))).
macro(pp_de, lit(pp(de))).
macro(pp_a, lit(pp(a))).
macro(pp, lit(pp(_))).

macro(s_inf, lit(s(inf))).
macro(s_deinf, lit(s(deinf))).
macro(s_ainf, lit(s(ainf))).
macro(s_ppres, lit(s(ppres))).
macro(s_ppart, lit(s(ppart))).

macro(cl_elle, lit(cl(elle))).
macro(cl_il, lit(cl(il))).
macro(cl_la, lit(cl(la))).
macro(cl_le, lit(cl(le))).
macro(cl_le_lui, lit(cl(le_lui))).
macro(cl_lui, lit(cl(lui))).
macro(cl_en, lit(cl(en))).
macro(cl_se, lit(cl(se))).
macro(cl_y, lit(cl(y))).

macro(dl(0,s,s), dl(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_top,s_top), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s_top,s_top), dr(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s,s), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_ppart,s_ppart), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s_ppart,s_ppart), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_inf,s_inf), dl(0,lit(s(X)),lit(s(X)))).
macro(dr(0,s_inf,s_inf), dr(0,lit(s(X)),lit(s(X)))).
macro(s_ppres, lit(s(ppres))).
macro(s_ppart, lit(s(ppart))).
macro(s_inf, lit(s(inf))).
macro(s_ainf, lit(s(ainf))).
macro(s_deinf, lit(s(deinf))).
macro(s, lit(s(_))).
macro(s_top, lit(s(_))).

% =

continuous(0).

external(0).

% =
% inclusion

postulate(p(1,A,B), p(0,A,B), 'I1_0').
postulate(p(2,A,B), p(0,A,B), 'I2_0').
%postulate(p(3,A,B), p(0,A,B), 'I3_0').
%postulate(p(4,A,B), p(0,A,B), 'I4_0').

%postulate(p(3,A,B), p(4,B,A), 'P3_4').
% recursive

% odd postulates, to be replaced by more basic ones.7
% (goede *4 nota) *3 ( daar *2 van )
% 2-3-4
postulate(p(3,p(4,A,B),p(2,C,D)), p(0,p(0,C,p(0,A,B)),D), 'Mx').
% 2-2
postulate(p(0,p(0,A,p(2,B,C)),p(2,D,E)), p(0,A,p(0,p(0,p(0,B,D),C),E)), 'MC2_2a').
postulate(p(2,A,p(0,p(2,B,C),D)), p(0,B,p(0,A,p(0,C,D))), 'MC2_2b').
postulate(p(2,A,p(2,B,C)), p(0,B,p(0,A,C)), 'MC2_2c').
% 1-4
postulate(p(4,A,p(1,B,C)), p(1,B,p(0,A,C)), 'MC1_4a').
postulate(p(4,A,p(0,B,p(1,C,D))), p(0,B,p(1,C,p(0,A,D))), 'MC1_4b').
postulate(p(4,A,p(0,B,p(0,C,p(1,D,E)))), p(0,D,p(0,B,p(0,C,p(1,A,E)))), 'MC1_4c').
% 2-4
postulate(p(4,p(2,A,B),C), p(0,p(2,A,C),B), 'MC2_4a').
postulate(p(4,p(0,A,p(2,B,C)),D), p(0,A,p(2,p(0,B,D),C)), 'MC2_4b').
postulate(p(4,A,p(0,B,p(2,C,D))), p(0,B,p(2,p(0,A,C),D)), 'MC2_4c').
postulate(p(4,A,p(0,B,p(0,p(2,C,D),E))), p(0,B,p(0,p(2,p(0,C,A),D),E)), 'MC2_4d').
% 2-3
postulate(p(3,p(2,A,B),C), p(2,p(0,A,C),B), 'MC2_3a').
postulate(p(3,p(0,A,p(2,B,C)),D), p(0,A,p(2,p(0,B,D),C)), 'MC2_3b').
postulate(p(3,p(0,A,p(0,p(2,B,C),D)),E), p(0,A,p(0,p(2,p(0,B,E),C),D)), 'MC2_3c'). 
postulate(p(3,A,p(0,B,p(0,C,p(1,D,E)))), p(0,B,p(0,C,p(1,D,p(0,A,E)))), 'MC2_3d').
postulate(p(3,A,p(2,B,C)), p(2,p(0,B,A),C), 'MCA2_3a').
postulate(p(3,p(0,p(2,A,B),C),D), p(0,p(2,p(0,A,D),B),C), 'MC2_3d').
% 1-3
postulate(p(3,p(0,A,p(1,B,C)),D), p(0,A,p(1,B,p(0,D,C))), 'M1_3a').

% non-recursive
% 1-2
postulate(p(2,A,p(1,B,C)), p(0,B,p(0,A,C)), 'MC2_1a').
postulate(p(2,A,p(0,B,p(1,C,D))), p(0,B,p(0,C,p(0,A,D))), 'MC2_1b').

% 4-4
postulate(p(4,p(4,A,B),C), p(4,A,p(0,B,C)), 'MA4_4a').

% standard extraction postulates for a unary mode p

postulate(p(0,A,zip(1,B)), p(0,zip(1,B),A), 'Cp1').
postulate(p(0,A,p(0,B,zip(1,C))), p(0,p(0,A,B),zip(1,C)), 'MCp1').
postulate(p(0,p(0,A,zip(1,B)),C), p(0,p(0,A,C),zip(1,B)), 'MAp1').

%

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
	intransitive_verb_semantics(ES, Dis, Word, Time, Sem).

semantics(transitive_verb, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	transitive_verb_semantics(ES, Dis, Word, Time, Sem).

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



semantics(transitive_verb, Word, Time, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	transitive_verb_semantics(ES, Dis, Word, Time, Sem).


adj_semantics(fo, Word, lambda(P, lambda(X, bool(appl(P,X),&,appl(Word,X))))).
adj_semantics(drt, Word, lambda(P,lambda(V, lambda(E,merge(drs([],[appl(Word,V)]),appl(appl(P,V),E)))))).

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

dot_np_semantics1(drt, lambda(P,appl(appl(P,lambda(_V,lambda(_E1,drs([],[])))),_))).

noun_semantics(drt, Word, lambda(V,Sem)) :-
	event_semantics(ES),
     (
        ES = none
     ->
        Sem = drs([],[appl(Word,V)])
     ;
        Sem = lambda(_,drs([],[appl(Word,V)]))
     ).

intransitive_verb_semantics(none, Dis, Word, Time, Sem) :-
	intransitive_verb_semantics_none(Dis, Word, Time, Sem).
intransitive_verb_semantics(classic, Dis, Word, Time, Sem) :-
	intransitive_verb_semantics_classic(Dis, Word, Time, Sem).
intransitive_verb_semantics(neo, Dis, Word, Time, Sem) :-
	intransitive_verb_semantics_neo(Dis, Word, Time, Sem).

intransitive_verb_semantics_none(drt, Word, _-Time, lambda(P, appl(P,lambda(V, drs([], Conditions))))) :-
	append(Time, [appl(Word,V)], Conditions).
intransitive_verb_semantics_classic(drt, Word, E1-Time, lambda(P, lambda(E,appl(appl(P,lambda(V,lambda(E1,drs([event(E1)], Conditions)))),E)))) :-
	append(Time, [appl(event,E1),appl(appl(Word,V),E1)], Conditions).
intransitive_verb_semantics_neo(drt, Word, E1-Time, lambda(P, lambda(E,appl(appl(P,lambda(V,lambda(E1,drs([event(E1)], Conditions)))),E)))) :-
	append(Time, [appl(event,E1),appl(Word,E1),appl(appl(agent,V),E1)], Conditions).

transitive_verb_semantics(none, Dis, Word, Time, Sem) :-
	transitive_verb_semantics_none(Dis, Word, Time, Sem).
transitive_verb_semantics(classic, Dis, Word, Time, Sem) :-
	transitive_verb_semantics_classic(Dis, Word, Time, Sem).
transitive_verb_semantics(neo, Dis, Word, Sem) :-
	transitive_verb_semantics_neo(Dis, Word, Time, Sem).

transitive_verb_semantics_none(drt, Word, _-Time, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], Conditions)))))))) :-
	append(Time, [appl(appl(Word,W),V)], Conditions).
transitive_verb_semantics_classic(drt, Word, E2-Time, lambda(P,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([event(E2)], Conditions)))),E)))),E))))) :-
		append(Time, [appl(event,E2),appl(appl(appl(Word,W),V),E2)], Conditions).
transitive_verb_semantics_neo(drt, Word, E2-Time, lambda(P,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([event(E2)],Conditions)))),E)))),E))))) :-
		append(Time, [appl(event,E2),appl(Word,E2),appl(appl(agent,V),E),appl(appl(patient,W),E)], Conditions).

transitive_verb_semantics_pp(none, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_none(Dis, Word, Sem).
transitive_verb_semantics_pp(classic, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_classic(Dis, Word, Sem).
transitive_verb_semantics_pp(neo, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_neo(Dis, Word, Sem).

transitive_verb_semantics_pp_none(drt, Word, lambda(P,lambda(_,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)]))))))))).
transitive_verb_semantics_pp_classic(drt, Word, lambda(P,lambda(_,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([event(E2)], [appl(event,E2),appl(appl(appl(Word,W),V),E2)])))),E)))),E)))))).
transitive_verb_semantics_pp_neo(drt, Word, lambda(P,lambda(_,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([event(E2)], [appl(event,E2),appl(Word,E2),appl(appl(agent,V),E),appl(appl(patient,W),E)])))),E)))),E)))))).

% ============================================================
% Default Semantics
% ============================================================


sem_tv_subject_control(Word, lambda(P,lambda(X,lambda(E,merge(drs([],[appl(Word,E),appl(appl(patient,F),E)]),merge(appl(appl(X,lambda(Z,lambda(D,drs([],[appl(appl(agent,Z),D)])))),E),appl(appl(P,X),F)))))))

pos_time(ver:TENSE, List) :-
	!,
	tense_time(TENSE, List).
pos_time(_, []).

tense_time(impf, E-[appl(past,E)]) :-
	!.
tense_time(_, _-[]).



% = intransitive

default_semantics(W, POS, dl(_,lit(np),lit(s(_))), Sem) :-
	pos_time(POS, Time),
	semantics(intransitive_verb, W, Time, Sem).

% = intransitive - verb initial

default_semantics(W, POS, dr(_,lit(s(_)),lit(np)), Sem) :-
	pos_time(POS, Time),
        semantics(intransitive_verb, W, Time, Sem).

%= verbs

% = ditransitive

default_semantics(W, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(np)),lit(np)), lambda(P, lambda(Q, lambda(R, lambda(E, appl(appl(R,lambda(X,lambda(E1,appl(appl(Q,lambda(Y,lambda(E2,appl(appl(P,lambda(Z,lambda(E3,drs([event(E3)],[appl(event,E3),appl(W,E3),appl(appl(agent,X),E3),appl(appl(patient,Z),E3),appl(appl(instrument,Y),E3)])))),E)))),E)))),E)))))).

% = ditransitive + pp_a

default_semantics(W, dr(_,dr(_,dl(_,lit(np),lit(s(_))),lit(pp(a))),lit(np)), lambda(P, lambda(Q, lambda(R, lambda(E, appl(appl(R,lambda(X,lambda(E1,appl(appl(Q,lambda(Y,lambda(E2,appl(appl(P,lambda(Z,lambda(E3,drs([event(E3)],[appl(event,E3),appl(W,E3),appl(appl(agent,X),E3),appl(appl(patient,Y),E3),appl(appl(instrument,Z),E3)])))),E)))),E)))),E)))))).

% = transitive - V2

default_semantics(Word, POS, dr(_,dl(_,lit(np),lit(s(_))),lit(np)), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = transitive - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),lit(np)),lit(np)), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = transitive - verb final

default_semantics(Word, dl(_,lit(np),dl(_,lit(np),lit(s(_)))), Sem) :-
	pos_time(POS, Time),
	semantics(transitive_verb, Word, Time, Sem).

% = transitive - sentential complement

default_semantics(Word, dr(_,dl(_,lit(np),lit(s)),lit(cs)), lambda(Q,lambda(P, lambda(E,merge(appl(appl(P,lambda(V,lambda(E1,drs([event(E1)], [appl(event,E1),appl(Word,E1),appl(appl(agent,V),E1),appl(appl(patient,W),E1)])))),E),appl(Q,W)))))).

default_semantics(Word, dr(_,dl(_,lit(np),lit(s)),lit(s)), lambda(Q,lambda(P, lambda(E,merge(appl(appl(P,lambda(V,lambda(E1,drs([event(E1)], [appl(event,E1),appl(Word,E1),appl(appl(agent,V),E1),appl(appl(patient,W),E1)])))),E),appl(Q,W)))))).

% = transitive + pp

default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),lit(pp(Prep))), Sem) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        concat_atom([Word,'_',Prep], PrepWord)
    ),
    	semantics(transitive_verb, PrepWord, Sem).


% = transitive + prep + np

default_semantics(Word, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(pp(Prep)))),lit(np)), Sem) :-
	concat_atom([Prep,Word], PrepWord),
	semantics(transitive_verb_pp, PrepWord, Sem).


% = copula  - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),dr(0,lit(n),lit(n))), lit(np)),
 lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([event(E),variable(W)],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))).
default_semantics(Word, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), lit(np)),
 lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([event(E),variable(W)],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))).

% = copula  - verb second

default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),dr(0,lit(n),lit(n))), lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([event(E),variable(W)],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))).
default_semantics(Word, dr(_,dl(_,lit(np),lit(s(_))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([event(E),variable(W)],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))).

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
	pos_time(POS, Time),
	semantics(transitive_verb, Word, _-[], Sem).

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

% ===================
% = object control =
% ===================

% np = (e->t)->t
%    = (e->s->t)->s->t
% lambda(P          e->s->t
%    lambda(e       s
%       drt(...)))
% lambda(P,lambda(E,merge(drs(['Jean'],[]),appl(appl(P,Jean),E)))).
% appl(NP, lambda(P, lambda(E, appl(appl(NP, P), E)))).
% appl(NP, 
% lambda(P,lambda(E,merge(appl(appl(NP,P),E),drs([],[appl(appl(patient,m),E)])

% = convaincre

% = persuader + np + deinf

default_semantics(persuader, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))),lit(np)), lambda(NPO,lambda(Inf,lambda(NP,lambda(E,merge(drs([],[appl(persuader_de,E),appl(appl(instrument,F),E)]),merge(merge(appl(appl(NP,lambda(Z,lambda(D,drs([],[appl(appl(agent,Z),D)])))),E),appl(appl(Inf,NPO),F)),appl(appl(NP,lambda(_,lambda(E1,)))))).

% = permettre + deinf

default_semantics(permettre, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(permettre,E),appl(appl(instrument,F),E)]),merge(appl(appl(X,lambda(Z,lambda(D,drs([],[appl(appl(agent,Z),D)])))),E),appl(appl(P,lambda(Q,lambda(E1,merge(drs([variable(Y)],[]),appl(appl(Q,Y),E1))))),F))))))).

% = permettre + pp_a + deinf

default_semantics(permettre, dr(_,dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(deinf)))),lit(pp(a))), lambda(PPA,lambda(Inf,lambda(NP,lambda(E,merge(drs([],[appl(permettre_à,E),appl(appl(instrument,F),E)]),merge(appl(appl(NP,lambda(Z,lambda(D,drs([],[appl(appl(agent,Z),D)])))),E),appl(appl(Inf,PPA),F)))))))).

% = faire + inf

default_semantics(faire, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(faire,E),appl(appl(agent,_Subj),E),appl(appl(patient,F),E)]),appl(appl(P,X),F)))))).

% = laisser + inf

default_semantics(laisse, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(laisser,E),appl(appl(agent,_Subj),E),appl(appl(patient,F),E)]),appl(appl(P,X),F)))))).

% = noun phrase

default_semantics(Word, lit(np), lambda(P,lambda(E,merge(drs([constant(Word)],[]),appl(appl(P,Word),E))))).

% = prepositions - noun modifiers

default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(np)), lambda(NP, lambda(N, lambda(X, lambda(E, merge(appl(appl(N,X),E),appl(appl(NP,lambda(Y,lambda(_,drs([],[appl(appl(W,Y),X)])))),E))))))).
default_semantics(W, dr(0,dl(0,lit(np),lit(np)),lit(np)),lambda(P,lambda(Q,lambda(Z,lambda(E,merge(appl(appl(P,lambda(X,lambda(_,appl(appl(Q,lambda(Y,lambda(_,drs([],[appl(appl(W,X),Y)])))),E)))),E),appl(appl(Q,Z),E))))))).

default_semantics(de, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).
default_semantics(du, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).
default_semantics(des, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).
default_semantics(à, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(à,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).
default_semantics(au, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(à,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).
default_semantics(aux, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, lambda(E, merge(drs([variable(Y)],[appl(appl(à,Y),X)]),merge(appl(appl(N2,X),E),appl(appl(N1,Y),E)))))))).

% P = lambda(Y,lambda(_,drs([],[appl(appl(W,Y),X)])))
% lambda(Q,lambda(E,merge(merge(drs([Z],[]),drs([],[appl(appl(W,Z),X)]),appl(appl(Q,Z),E)))))
% NP = lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))

% NP modifiers

default_semantics(monsieur, dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,monsieur),X)]))))),E))))).
default_semantics('M', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,monsieur),X)]))))),E))))).
default_semantics('M.', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,monsieur),X)]))))),E))))).
default_semantics('M\.', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,monsieur),X)]))))),E))))).

default_semantics(madame, dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,madame),X)]))))),E))))).
default_semantics('Mme', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,madame),X)]))))),E))))).
default_semantics('Mme.', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,madame),X)]))))),E))))).
default_semantics('Mme\.', dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(appl(titre,madame),X)]))))),E))))).

default_semantics(W, dr(_,lit(np),lit(np)), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(W,X)]))))),E))))).

% = prepositions - arguments

default_semantics(sauf, dr(_,lit(pp(_)),lit(n)), lambda(P,lambda(Q,lambda(E,merge(drs([],[not(merge(drs([X],[]),appl(appl(P,X),E)))]),appl(appl(Q,X),E)))))).
default_semantics(_, dr(_,lit(pp(_)),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).

default_semantics(_, dr(_,lit(pp(_)),lit(np)), lambda(X,X)).

% = noun phrases

% = nouns

default_semantics(W, lit(n), Sem) :-
	semantics(noun, W, Sem).

% = adjective

default_semantics(W, dr(_,lit(n),lit(n)), Sem) :-
	    semantics(adjective, W, Sem).
default_semantics(W, dl(_,lit(n),lit(n)), Sem) :-
	    semantics(adjective, W, Sem).

default_semantics(W, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(W,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).


% = adverbs - sentence modifiers

default_semantics(W, dl(_,lit(s(_)),lit(s(_))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).
default_semantics(W, dr(_,lit(s(_)),lit(s(_))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).

% = adverbs - VP modifiers

default_semantics(de, dr(_,dl(_,lit(np),lit(s(deinf))),dl(_,lit(np),lit(s(inf)))), lambda(X,X)).
default_semantics(à, dr(_,dl(_,lit(np),lit(s(ainf))),dl(_,lit(np),lit(s(inf)))), lambda(X,X)).


default_semantics(W, dr(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).
default_semantics(W, dl(_,dl(_,lit(np),lit(s(_))),dl(_,lit(np),lit(s(_)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).


% = adverbial prepositions

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(appl(N,X),E)),appl(S,E)))))).
default_semantics(W, dr(0,dr(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(appl(N,X),E)),appl(S,E)))))).

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(np)), lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(appl(NP,lambda(X,lambda(F,drs([],[appl(appl(W,X),F)])))),E)))))).

default_semantics(W, dl(0,dr(_,lit(s(_)),lit(s(_))),lit(np)), lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(appl(NP,lambda(X,lambda(F,drs([],[appl(appl(W,X),F)])))),E)))))).


% ============================================================
% Lexicon
% ============================================================

lex(ne, dr(0,dl(0,np,s),dl(0,np,s)), lambda(X,X)).
lex(ne, dl(0,dl(0,np,s),dl(0,np,s)), lambda(X,X)).

% Determiners - French

lex(aucun, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[not(merge(drs([variable(X)],[]),merge(appl(appl(Q,X),E),appl(appl(P,X),E))))]))))).
lex(aucune, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[not(merge(drs([variable(X)],[]),merge(appl(appl(Q,X),E),appl(appl(P,X),E))))]))))).
lex('Aucun', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[not(merge(drs([variable(X)],[]),merge(appl(appl(Q,X),E),appl(appl(P,X),E))))]))))).
lex('Aucune', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[not(merge(drs([variable(X)],[]),merge(appl(appl(Q,X),E),appl(appl(P,X),E))))]))))).

lex(ce, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(cet, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(cette, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ce', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Cet', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Cette', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(demonstrative,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Certains', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(le, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Le', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(les, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Les', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(du, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(la, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('La', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('l\'', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('L\'', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[appl(definite,X)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(un, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Un', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(une, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Une', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X)],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(chaque, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Chaque', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(tout, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Tout', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(toute, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Toute', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(tous, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Tous', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(toutes, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Toutes', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([variable(X)],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).

lex(notre, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Notre', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(mon, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Mon', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(ma, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ma', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).

lex(votre, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Votre', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(ton, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ton', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(ta, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ta', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).

lex(leur, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Leur', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(son, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Son', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(sa, dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Sa', dr(0,lit(np),lit(n)), lambda(P,lambda(Q,lambda(E,merge(merge(drs([variable(X),variable(Y)],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).


lex(mais, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(expansion,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(P,X),E),appl(appl(Q,X),_))))))).
lex(et, dr(0,dl(0,lit(np),lit(np)),lit(np)), lambda(NP1,lambda(NP2,lambda(P,lambda(E,merge(appl(appl(NP1,P),E),appl(appl(NP2,P),_))))))).

% = proper nouns
% French
lex('Je', lit(np), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex('J', lit(np), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex(je, lit(np), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex(j, lit(np), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).

lex('On', lit(np), lambda(P,lambda(E,merge(drs([X],[]),appl(appl(P,X),E))))).
lex(on, lit(np), lambda(P,lambda(E,merge(drs([X],[]),appl(appl(P,X),E))))).

lex('Tu', lit(np), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex('T', lit(np), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(te, lit(np), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(tu, lit(np), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(t, lit(np), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).

lex('Il', lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex('Elle', lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(il, lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(elle, lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(elle, lit(cl(elle)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(le, lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(il, lit(cl(il)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(l, lit(cl(le)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(le, lit(cl(le)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(la, lit(np), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(la, lit(cl(la)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).

lex(que, dr(0,lit(cs),lit(s(_))), lambda(X,X)).

% = others

lex('.', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('.', dl(0,lit(np),lit(txt)), Sem) :-
	semantics(dot_np, Sem).

% = relativization

lex(qui, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np),lit(s(_)))), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(Q,X),E),appl(appl(P,lambda(R,appl(R,X))),_))))))).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(0,box(0,lit(np))))), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(Q,X),E),appl(appl(P,lambda(R,appl(R,X))),E))))))).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(np))))), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(Q,X),E),appl(appl(P,lambda(R,appl(R,X))),E))))))).

% = coordination

lex('Si', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(si,   dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(car,  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(mais, dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et,   dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(',',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\,', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(';',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\;', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(':',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('\:', dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('-',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).

%lex(',', dr(0,dl(0,lit(pp(_)),lit(s(S))),lit(s(S))), lambda(S,lambda(PP,lambda(E,merge(appl(appl(PP,lambda(_,lambda(_,drs([],[])))),E),appl(S,E)))))).
%lex('\,', dr(0,dl(0,lit(pp(_)),lit(s(S))),lit(s(S))), lambda(S,lambda(PP,lambda(E,merge(appl(appl(PP,lambda(_,lambda(_,drs([],[])))),E),appl(S,E)))))).

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
lex(',', dr(0,dl(0,lit(np),lit(np)),lit(n)), lambda(N,lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(N,X),F),appl(appl(P,X),F))))),E)))))).
lex('\,', dr(0,dl(0,lit(np),lit(np)),lit(n)), lambda(N,lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(N,X),F),appl(appl(P,X),F))))),E)))))).
