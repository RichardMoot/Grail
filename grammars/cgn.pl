% -*- Mode: Prolog -*-
% ============================================================
% cgn.pl
% ============================================================
% !grail 3.1.1

event_semantics(neo).
discourse_semantics(drt).

% This grammar is made for use with the supertagger and the grammar
% extracted from the Corpus Gesproken Nederlands (CGN, Spoken Dutch
% Corpus).

% It contains a mix of macro definitions, structural rules and the
% start of a semantic component, which will be made more and more
% complete over time.

macro(dr(0,dl(0,n,n),np),dr(0,dl(0,lit(n(X)),lit(n(X))),lit(n(p)))).
macro(dl(0,lit(s),lit(txt)),dl(0,lit(s(_)),lit(txt))).
macro(dl(0,s,txt),dl(0,lit(s(_)),txt)).
macro(dl(0,n,txt),dl(0,lit(n(_)),txt)).
macro(dl(0,np,txt),dl(0,lit(n(p)),txt)).

macro(dr(0,n,n),dr(0,lit(n(X)),lit(n(X)))).
macro(dl(0,n,n),dl(0,lit(n(X)),lit(n(X)))).

macro(dr(0,s,s),dr(0,lit(s(X)),lit(s(X)))).
macro(dr(4,s,s),dr(4,lit(s(X)),lit(s(X)))).
macro(dl(0,s,s),dl(0,lit(s(X)),lit(s(X)))).
macro(dl(3,s,s),dl(3,lit(s(X)),lit(s(X)))).

macro(np, lit(n(p))).
macro(n, lit(n(_))).
macro(n_refl, lit(n(p))).

macro(pp, lit(pp(_))).
macro(pp_aan, lit(pp(aan))).
macro(pp_achter, lit(pp(achter))).
macro(pp_af, lit(pp(af))).
macro(pp_beneden, lit(pp(beneden))).
macro(pp_boven, lit(pp(boven))).
macro(pp_buiten, lit(pp(buiten))).
macro(pp_in, lit(pp(in))).
macro(pp_met, lit(pp(met))).
macro(pp_na, lit(pp(na))).
macro(pp_naar, lit(pp(naar))).
macro(pp_naast, lit(pp(naast))).
macro(pp_op, lit(pp(op))).
macro(pp_onder, lit(pp(onder))).
macro(pp_over, lit(pp(over))).
macro(pp_te, lit(pp(te))).
macro(pp_tegen, lit(pp(tegen))).
macro(pp_tussen, lit(pp(tussen))).
macro(pp_uit, lit(pp(uit))).
macro(pp_van, lit(pp(van))).

macro(pp_de, lit(pp(de))).
macro(pp_a, lit(pp(a))).

macro(lit(s), lit(s(_))).
macro(s, lit(s(_))).
macro(s_whq,lit(s(whq))).
macro(s_whsub,lit(s(whsub))).
macro(s_whrel,lit(s(whrel))).
macro(s_cp,lit(s(cp))).
macro(s_sub,lit(s(sub))).
macro(s_main,lit(s(main))).
macro(s_ppart,lit(s(ppart))).
macro(s_v1,lit(s(v1))).
macro(s_inf,lit(s(inf))).
macro(s_ti,lit(s(ti))).
macro(s_oti,lit(s(oti))).
macro(s_ahi,lit(s(ahi))).



macro(p(0,dr(0,A,B),C), dr(0,A,dl(0,dia(p,box(p,C)),B))).

continuous(0).

external(0).

% =

custom_first_order(lit(n(Z)), lit(n, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(np(Z)), lit(np, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(pp(Z)), lit(pp, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(s(Z)), lit(s, [X,Y,Z]), _, [X,Y]). 

% = extraction

custom_first_order(dl(0,dia(p,box(p,A0)),B0), dl(0,dia(p,box(p,A)),B), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).

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

postulate(p(0,A,zip(p,B)), p(0,zip(p,B),A), 'Cp1').
postulate(p(0,A,p(0,zip(p,B),C)), p(0,zip(p,B),p(0,A,C)), 'MCp1').
postulate(p(0,p(0,zip(p,A),B),C), p(0,zip(p,A),p(0,B,C)), 'MAp1').

%

postulate(p(0,A,p(0,B,zip(a,C))), p(0,p(0,A,B),zip(a,C)), 'MAa1').

special_string(".", '.').
special_string(";", ';').
special_string(":", ':').
special_string(",", ',').
special_string("!", '!').
special_string("?", '?').

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
	dot_semantics(ES, Dis, Sem).

semantics(intransitive_verb, Word, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	intransitive_verb_semantics(ES, Dis, Word, Sem).

semantics(transitive_verb, Word, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	transitive_verb_semantics(ES, Dis, Word, Sem).

semantics(transitive_verb_pp, Word, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	transitive_verb_semantics_pp(ES, Dis, Word, Sem).

semantics(noun, Word, Sem) :-
	discourse_semantics(Dis),
	noun_semantics(Dis, Word, Sem).

semantics(adjective, Word, Sem) :-
	discourse_semantics(Dis),
	adj_semantics(Dis, Word, Sem).

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

dot_np_semantics1(drt, lambda(P, appl(P, _))). 

empty_determiner_semantics(lambda(Q,Sem1), Sem) :-
	event_semantics(ES),
     (
        ES = none
     ->
        Sem1 = merge(merge(drs([X],[]),appl(Sem,X)),appl(Q,X))
     ;
        Sem1 = lambda(E,merge(merge(drs([X],[]),appl(appl(Sem,X),E)),appl(appl(Q,X),E)))
     ).

noun_semantics(drt, Word, lambda(V,Sem)) :-
	event_semantics(ES),
     (
        ES = none
     ->
        Sem = drs([],[appl(Word,V)])
     ;
        Sem = lambda(_,drs([],[appl(Word,V)]))
     ).
      
% empty_determiner_semantics(Sem, Sem0) :-
%      (
%          atomic(Sem0)
%      ->
%          Sem1 = lambda(X,appl(Sem0,X))
%      ;
%          Sem1 = Sem0
%      ),
%      Sem = lambda(P, lambda(E,appl(appl(P,lambda(V,lambda(E1,drs([], [appl(appl(Sem1,V),E1)])))),E))).

intransitive_verb_semantics(none, Dis, Word, Sem) :-
	intransitive_verb_semantics_none(Dis, Word, Sem).
intransitive_verb_semantics(classic, Dis, Word, Sem) :-
	intransitive_verb_semantics_classic(Dis, Word, Sem).
intransitive_verb_semantics(neo, Dis, Word, Sem) :-
	intransitive_verb_semantics_neo(Dis, Word, Sem).

intransitive_verb_semantics_none(drt, Word, lambda(P, appl(P,lambda(V, drs([], [appl(Word,V)]))))).
intransitive_verb_semantics_classic(drt, Word, lambda(P, lambda(E,appl(appl(P,lambda(V,lambda(E1,drs([E1], [appl(event,E1),appl(appl(Word,V),E1)])))),E)))).
intransitive_verb_semantics_neo(drt, Word, lambda(P, lambda(E,appl(appl(P,lambda(V,lambda(E1,drs([E1], [appl(event,E1),appl(Word,E1),appl(agent,V)])))),E)))).

transitive_verb_semantics(none, Dis, Word, Sem) :-
	transitive_verb_semantics_none(Dis, Word, Sem).
transitive_verb_semantics(classic, Dis, Word, Sem) :-
	transitive_verb_semantics_classic(Dis, Word, Sem).
transitive_verb_semantics(neo, Dis, Word, Sem) :-
	transitive_verb_semantics_neo(Dis, Word, Sem).

transitive_verb_semantics_none(drt, Word, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)])))))))).
transitive_verb_semantics_classic(drt, Word, lambda(P,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([E2], [appl(event,E2),appl(appl(appl(Word,W),V),E2)])))),E)))),E))))).
transitive_verb_semantics_neo(drt, Word, lambda(P,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([E2], [appl(event,E2),appl(Word,E2),appl(appl(agent,V),E),appl(appl(patient,W),E)])))),E)))),E))))).

transitive_verb_semantics_pp(none, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_none(Dis, Word, Sem).
transitive_verb_semantics_pp(classic, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_classic(Dis, Word, Sem).
transitive_verb_semantics_pp(neo, Dis, Word, Sem) :-
	transitive_verb_semantics_pp_neo(Dis, Word, Sem).

transitive_verb_semantics_pp_none(drt, Word, lambda(P,lambda(_,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(Word,W),V)]))))))))).
transitive_verb_semantics_pp_classic(drt, Word, lambda(P,lambda(_,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([E2], [appl(event,E2),appl(appl(appl(Word,W),V),E2)])))),E)))),E)))))).
transitive_verb_semantics_pp_neo(drt, Word, lambda(P,lambda(_,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(_E1,appl(appl(P,lambda(W,lambda(E2,drs([E2], [appl(event,E2),appl(Word,E2),appl(appl(agent,V),E),appl(appl(patient,W),E)])))),E)))),E)))))).


% = possessives

lex(mijn, dr(0, lit(n(p)),lit(n(_))), lambda(P,quant(iota,lambda(X,bool(appl(P,X),&,appl(appl(van,X),speaker)))))).
lex(zijn, dr(0, lit(n(p)),lit(n(_))), lambda(P,quant(iota,lambda(X,bool(appl(P,X),&,appl(appl(van,X),_)))))).
lex(haar, dr(0, lit(n(p)),lit(n(_))), lambda(P,quant(iota,lambda(X,bool(appl(P,X),&,appl(appl(van,X),_)))))).

% = determiners - Dutch

lex(de, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('De', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(een, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Een', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(het, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Het', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(alle, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Alle', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(ieder, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Ieder', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(iedere, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Iedere', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).

% Determiners - French

lex(le, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Le', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(la, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('La', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('l\'', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('L\'', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(un, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Un', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(une, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Une', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(chaque, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Chaque', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(tout, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Tout', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(toute, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Toute', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(tous, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Tous', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex(toutes, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).
lex('Toutes', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,drs([],[bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E))]))))).

lex(mon, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Mon', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(ma, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ma', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(orateur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).

lex(ton, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ton', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(ta, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Ta', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),appl(auditeur,Y)]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).

lex(son, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Son', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(sa, dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex('Sa', dr(0, lit(n(p)),lit(n(_))), lambda(P,lambda(Q,lambda(E,merge(merge(drs([X,Y],[appl(appl(appartient,Y),X),bool(Y,=,'?')]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).


lex(mais, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([E,F],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([E,F],[appl(appl(expansion,F),E)]),merge(appl(P,E),appl(Q,F))))))).

% = proper nouns
% French
lex('Je', lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex('J', lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex(je, lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).
lex(j, lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(orateur,X)]),appl(appl(P,X),E))))).

lex('Tu', lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex('T', lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(te, lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(tu, lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).
lex(t, lit(n(p)), lambda(P,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(P,X),E))))).

lex('Il', lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex('Elle', lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(il, lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(elle, lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(elle, lit(cl_elle), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(le, lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(il, lit(cl_il), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(l, lit(cl_le), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(le, lit(cl_le), lambda(P,lambda(E,merge(drs([],[bool(X,=,'masculin?')]),appl(appl(P,X),E))))).
lex(la, lit(n(p)), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).
lex(la, lit(cl_la), lambda(P,lambda(E,merge(drs([],[bool(X,=,'feminin?')]),appl(appl(P,X),E))))).

% = others

lex(dat, dr(0,lit(s(cp)),lit(s(sub))), lambda(X, X)).
lex('.', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('.', dl(0,lit(n(p)),lit(txt)), Sem) :-
	semantics(dot_np, Sem).

% = infinitives headed by "te"

default_semantics(te, dr(_,dl(0,lit(n(p)),lit(s(ti))),dl(0,lit(n(p)),lit(s(inf)))), lambda(X,X)).

% = sentence modifiers

default_semantics(W, dl(_,lit(s(S)),lit(s(S))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).
default_semantics(W, dr(_,lit(s(S)),lit(s(S))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).

% = adverbial prepositions

default_semantics(W, dr(0,dl(_,lit(s(S)),lit(s(S))),lit(n(_))), lambda(X,lambda(P,lambda(E,bool(appl(P,E),&,appl(appl(W,X),E)))))).
default_semantics(W, dr(0,dr(_,lit(s(S)),lit(s(S))),lit(n(_))), lambda(X,lambda(P,lambda(E,bool(appl(P,E),&,appl(appl(W,X),E)))))).

% = prepositions - noun modifiers

default_semantics(W, dr(0,dl(0,lit(n(N1)),lit(n(N2))),lit(n(p))), lambda(X,lambda(P,lambda(Y,bool(appl(P,Y),&,appl(appl(W,Y),X)))))) :-
	var(N1),
	var(N2),
	N1 == N2.


% = prepositions - arguments

default_semantics(_, dr(_,lit(pp(_)),lit(n(_))), lambda(X,X)).

% = noun phrases

% = nouns

default_semantics(W, lit(n(N)), Sem) :-
	var(N),
	semantics(noun, W, Sem).

%= verbs

% = ditransitive

default_semantics(W, dr(_,dr(_,dl(_,lit(n(_S)),lit(s(_))),lit(n(_IO))),lit(n(_DO))), lambda(X, lambda(Y, lambda(Z, lambda(E,bool(bool(appl(W,E),&,appl(appl(agent,Z),E)),&,bool(appl(appl(patient,X),E),&,appl(appl(participant,Y),E)))))))).

% = transitive - V2

default_semantics(Word, dr(_,dl(_,lit(n(p)),lit(s(_))),lit(n(p))), Sem) :-
	semantics(transitive_verb, Word, Sem).

% = transitive - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),lit(n(p))),lit(n(p))), Sem) :-
	semantics(transitive_verb, Word, Sem).

% = transitive - verb final

default_semantics(Word, dl(_,lit(n(p)),dl(_,lit(n(p)),lit(s(_)))), Sem) :-
	semantics(transitive_verb, Word, Sem).

% = transitive - sentential complement

default_semantics(Word, dr(_,dl(_,lit(n(p)),lit(s(_))),lit(s(_))), lambda(Q,lambda(P, lambda(E,merge(appl(appl(P,lambda(V,lambda(E1,drs([E1], [appl(event,E1),appl(Word,E1),appl(agent,V),appl(patient,W)])))),E),appl(Q,W)))))).

% = transitive + pp

default_semantics(Word, dr(_,dl(_,lit(n(p)),lit(s(_))),lit(pp(Prep))), Sem) :-
    (
        var(Prep)
    ->
        PrepWord = Word
    ;
        concat_atom([Prep,Word], PrepWord)
    ),
    	semantics(transitive_verb, PrepWord, Sem).


% = transitive + prep + np

default_semantics(Word, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(Prep)))),lit(n(p))), Sem) :-
	concat_atom([Prep,Word], PrepWord),
	semantics(transitive_verb_pp, PrepWord, Sem).


% = copula  - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),dr(0,lit(n(N0)),lit(n(N1)))), lit(n(p))),
 lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([E,W],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))) :-
	var(N0),
	var(N1),
	N0 == N1.

% = copula  - verb second

default_semantics(Word, dr(_,dl(_,lit(n(p)),lit(s(_))),dr(0,lit(n(N0)),lit(n(N1)))), lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([E,W],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))) :-
	var(N0),
	var(N1),
	N0 == N1.

% = worden + ppart
% passive, the grammatical subject is the patient and and unspecified
% entity is the agent.

default_semantics(worden, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(appl(appl(P,_),E),&,appl(appl(patient,X),E)))))).

% = hebben + ppart

default_semantics(hebben, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,merge(appl(appl(P,X),E),drs([],[appl(finished,E)])))))).

% = zijn + ppart

default_semantics(is, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(appl(appl(P,X),E),&,appl(finished,E)))))).

default_semantics(zijn, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(appl(appl(P,X),E),&,appl(finished,E)))))).

% = willen + inf

default_semantics(willen, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = zullen + inf

default_semantics(zullen, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = kunnen + inf

default_semantics(kunnen, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(kunnen,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = mogen + inf

default_semantics(mogen, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(mogen,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = hopen + ti

default_semantics(willen, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ti)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(hopen,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = verwachten + ti

default_semantics(verwachten, dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(s(ti)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(verwachten,E),&,appl(appl(agent,_Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,X),F))))))).

% = intransitive

default_semantics(W, dl(_,lit(n(p)),lit(s(_))), Sem) :-
	semantics(intransitive_verb, W, Sem).

% = intransitive - verb initial

default_semantics(W, dr(_,lit(s(_)),lit(n(p))), Sem) :-
        semantics(intransitive_verb, W, Sem).

% = adjective

default_semantics(W, dr(_,lit(n(N1)),lit(n(N2))), Sem) :-
	    var(N1),
	    var(N2),
	    N1 == N2,
	    semantics(adjective, W, Sem).

% = conjunctions

default_semantics(_W, dr(_,dl(_,lit(s(S)),lit(s(S))),lit(s(S))), lambda(X,lambda(Y,lambda(E,bool(quant(exists,F,appl(Y,F)),&,appl(X,E)))))).
default_semantics(_W, dr(_,dl(_,lit(n(S)),lit(n(S))),lit(n(S))), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X)))))) :-
	var(S).

% = impersonal pronouns

default_semantics('Het', lit(n(p)), _).
default_semantics(het, lit(n(p)), _).
default_semantics('Er', lit(n(p)), _).
default_semantics(er, lit(n(p)), _).

% = pronouns - anaphora

default_semantics(dit, lit(n(p)), _).
default_semantics(dat, lit(n(p)), _).
default_semantics(het, lit(n(p)), _).
default_semantics(hem, lit(n(p)), _).
default_semantics(hij, lit(n(p)), _).
default_semantics(haar, lit(n(p)), _).
default_semantics(zij, lit(n(p)), _).
default_semantics(ze, lit(n(p)), _).

% = pronouns - first person

default_semantics('Ik', lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).
default_semantics(ik, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).
default_semantics(me, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).
default_semantics(mij, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).

default_semantics(Word, lit(n(p)), lambda(P,lambda(E,merge(drs([Word],[]),appl(appl(P,Word),E))))).

% = root_form

root_form(slaapt, slapen).
root_form(slaap, slapen).

root_form(hield, houden).
root_form(hielden, houden).
root_form(houdt, houden).
root_form(houd, houden).
root_form(hou, houden).
root_form(gehouden, houden).

root_form(heb, hebben).
root_form(had, hebben).
root_form(hadden, hebben).
root_form(hebt, hebben).
root_form(heeft, hebben).

root_form(geweest, is).
%root_form(zijn, is).
root_form(ben, is).
root_form(bent, is).

root_form(wil, willen).
root_form(wilt, willen).
root_form(wilde, willen).
root_form(wilden, willen).
root_form(gewild, willen).

root_form(sluit, sluiten).

root_form(geef, geven).
root_form(geeft, geven).
root_form(gaf, geven).
root_form(gaven, geven).

root_form(word, worden).
root_form(wordt, worden).
root_form(werd, worden).
root_form(werden, worden).
root_form(geworden, worden).

root_form(hoop, hopen).
root_form(hoopt, hopen).
root_form(hoopte, hopen).
root_form(hoopten, hopen).

root_form(verwacht, verwachten).
root_form(verwachtte, verwachten).
root_form(verwachtten, verwachten).

root_form(kan, kunnen).
root_form(kon, kunnen).
root_form(kun, kunnen).

root_form(mag, mogen).
root_form(mocht, mogen).
root_form(mochten, mogen).

root_form(zal, zullen).
root_form(zul, zullen).
root_form(zou, zullen).
root_form(zouden, zullen).

root_form(aangehouden, aanhouden).

root_form('De', de).
root_form('Een', een).
root_form('Er', er).
root_form('Het', het).