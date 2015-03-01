% -*- Mode: Prolog -*-
% ============================================================
% drt_assoc.pl
% ============================================================
% !grail 3.1.1

% = Discourse representation theory

% This grammar implements Reinhard Muskens' ideas on combining the
% Lambek calculus with Discourse Representation Theory in the Lambek
% calculus, while at the same time using a mixed non-associative and
% associative system.


:- abolish(lazy_unpack/1).
:- abolish(lazy_dr/1).
:- abolish(lazy_dl/1).
:- abolish(transparent_dia/1).
:- abolish(transparent/1).
:- abolish(continuous_dia/1).
:- abolish(continuous/1).
:- abolish(external_dia/1).
:- abolish(external/1).
:- abolish(postulate/3).
:- abolish(postulate1/3).
:- abolish(macro/2).
:- abolish(lex/3).
:- abolish(example/2).

:- dynamic lazy_unpack/1,lazy_dr/1,lazy_dl/1.
:- dynamic transparent_dia/1,transparent/1.
:- dynamic continuous_dia/1,continuous/1.
:- dynamic external_dia/1,external/1.
:- dynamic postulate/3,postulate1/3.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(0,A,B),zip(0,C)), 'MA').
postulate(p(0,p(0,A,zip(0,B)),C), p(0,p(0,A,C),zip(0,B)), 'MC').

% = extraction

custom_first_order(dr(0,A0, dia(0,box(0,B0))), dr(0,A,dia(0,box(0,B))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [X,Y]),
	add_first_order(B0, B, P, [Z,Z]).

% = continuity

continuous(0).

% = non internal modes

external(0).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(prep, dr(0,pp,np)).
macro(det, dr(0,np,n)).
macro(refl, dl(0,tv,iv)).
macro(relpro, dr(0,rel,relbody) ).
macro(relbody, dr(0,s,dia(0,box(0,np))) ).
macro(rel, dl(0,n,n) ).
macro(conj(X), dr(0,dl(0,X,X),X)).
macro(adj, dr(0,n,n) ).

% ============================================================
% Lexicon
% ============================================================

%atomic_type(n, e->t).
%atomic_type(np, (e->t)->t).
%atomic_type(s, (s->t)->t).
%atomic_type(txt, t).

% ((e->t)->t)->(s->t)->t
% P (e->t)->t
% Q s->t
% R e->t

iv_semantics(Word, lambda(P,lambda(Q,appl(P,lambda(V,merge(drs([E], [appl(event,E),appl(appl(Word,V),E)]),appl(Q,E))))))).
tv_semantics(Word, lambda(P,lambda(Q,lambda(E,appl(appl(Q,lambda(V,lambda(E1,appl(appl(P,lambda(W,lambda(E2,drs([E2], [appl(event,E2),appl(appl(appl(Word,W),V),E2)])))),E)))),E))))).

np_semantics(Word, Gender, lambda(P,lambda(E,merge(drs([Word],[appl(Gender,Word)]),appl(appl(P,Word),E))))).

adj_semantics(Word, lambda(P,lambda(V, lambda(E,merge(drs([],[appl(Word,V)]),appl(appl(P,V),E)))))).
%adj_semantics(Word, lambda(P,lambda(V, lambda(E,merge(drs([], [appl(Word,V)]),appl(appl(P,V),E)))))).

copula_semantics(Word, lambda(P,lambda(Q,appl(Q,lambda(V,appl(appl(P,lambda(W,lambda(E,drs([E,W],[appl(event,E),appl(property,W),appl(appl(appl(Word,W),V),E)])))),W)))))).

% = lex(Pros,Formula,Sem)

% ((e->s->s->t)->s->s->t)->s->s->t.
lex(and, dr(0,dl(0,relbody,dr(0,s,np)),relbody), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(P,X),E),appl(appl(Q,X),E))))))).
lex(and, conj(np), lambda(N,lambda(M,lambda(P,lambda(E,merge(appl(appl(N,P),E),appl(appl(M,P),E))))))).
lex(whom, relpro, lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(Q,X),E),appl(appl(P,lambda(R,appl(R,X))),_))))))).
lex(who, dr(0,dl(0,n,n),dl(0,np,s)), lambda(P,lambda(Q,lambda(X,lambda(E,merge(appl(appl(Q,X),E),appl(appl(P,lambda(R,appl(R,X))),_))))))).
lex(who, dr(0,dl(0,np,np),dl(0,np,s)), lambda(VP,lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(N,X),F),appl(appl(P,X),F))))),E)))))).

lex(is, dr(0,dl(0,np,s),dr(0,n,n)), Sem) :-
	copula_semantics(is, Sem).
lex(appears, dr(0,dl(0,np,s),dr(0,n,n)), Sem) :-
	copula_semantics(appear, Sem).


lex(mary, np, Sem) :-
	np_semantics(mary, female, Sem).
lex(sue, np, Sem) :-
	np_semantics(sue, female, Sem).
lex(john, np, Sem) :-
	np_semantics(john, male, Sem).
lex(pedro, np, Sem) :-
	np_semantics(pedro, male, Sem).

lex(tall, adj, Sem) :-
	adj_semantics(tall, Sem).
lex(interesting, adj, Sem) :-
	adj_semantics(interesting, Sem).
lex(ugly, adj, Sem) :-
	adj_semantics(ugly, Sem).
lex(pretty, adj, Sem) :-
	adj_semantics(pretty, Sem).
lex(beautiful, adj, Sem) :-
	adj_semantics(beautiful, Sem).
lex(handsome, adj, Sem) :-
	adj_semantics(handsome, Sem).
lex(smart, adj, Sem) :-
	adj_semantics(smart, Sem).
lex(intelligent, adj, Sem) :-
	adj_semantics(intelligent, Sem).

lex(proud, dr(0,np,np), lambda(NP,lambda(P,lambda(E,appl(appl(NP,lambda(X,lambda(F,merge(appl(appl(P,X),F),drs([],[appl(proud,X)]))))),E))))).

lex(quietly, dl(0,s,s), lambda(S,lambda(E,merge(appl(S,E),drs([],[appl(quietly,E)]))))).

lex(a, det, lambda(P,lambda(Q,lambda(E,merge(merge(drs([X],[]),appl(appl(P,X),E)),appl(appl(Q,X),E)))))).
lex(no, det, lambda(P,lambda(Q,lambda(E,drs([],[not(merge(drs([X],[]),merge(appl(appl(Q,X),E),appl(appl(P,X),E))))]))))).
lex(every, det, lambda(P,lambda(Q,lambda(E,bool(merge(drs([X],[]),appl(appl(P,X),E)),->,appl(appl(Q,X),E)))))).

lex(man, n, lambda(V, lambda(_, drs([], [appl(human,V),appl(male,V)])))).
lex(woman, n, lambda(V, lambda(_, drs([], [appl(human,V),appl(female,V)])))).
lex(farmer, n, lambda(V, lambda(_, drs([], [appl(farmer,V),appl(male,V)])))).
lex(donkey, n, lambda(V, lambda(_, drs([], [appl(donkey,V),appl(neuter,V)])))).
lex(story, n, lambda(V, lambda(_, drs([], [appl(story,V),appl(neuter,V)])))).

lex(of, dr(0,dl(0,n,n),np), lambda(NP, lambda(N, lambda(X, lambda(E, merge(appl(appl(N,X),E),appl(appl(NP,lambda(Y,lambda(_,drs([],[appl(appl(of,Y),X)])))),E))))))).

lex(he, np, lambda(P,lambda(E,merge(drs([],[bool(X,=,'male?')]),appl(appl(P,X),E))))).
lex(him, np, lambda(P,lambda(E,merge(drs([],[bool(X,=,'male?')]),appl(appl(P,X),E))))).
lex(she, np, lambda(P,lambda(E,merge(drs([],[bool(X,=,'female?')]),appl(appl(P,X),E))))).
lex(her, np, lambda(P,lambda(E,merge(drs([],[bool(X,=,'female?')]),appl(P,X),E)))).
lex(it, np, lambda(P,lambda(E,merge(drs([],[bool(X,=,'neuter?')]),appl(appl(P,X),E))))).

lex(stinks, iv, Sem) :-
	iv_semantics(stink, Sem).
lex(enters, iv, Sem) :-
	iv_semantics(enter, Sem).
lex(smiles, iv, Sem) :-
	iv_semantics(smile, Sem).
lex(smile, iv, Sem) :-
	iv_semantics(smile, Sem).

lex(adores, tv, Sem) :-
	tv_semantics(adore, Sem).
lex(adore, tv, Sem) :-
	tv_semantics(adore, Sem).
lex(abhors, tv, Sem) :-
	tv_semantics(abhor, Sem).
lex(bores, tv, Sem) :-
	tv_semantics(bore, Sem).
lex(ignores, tv, Sem) :-
	tv_semantics(ignore, Sem).
lex(owns, tv, Sem) :-
	tv_semantics(own, Sem).
lex(beats, tv, Sem) :-
	tv_semantics(beat, Sem).
lex(loves, tv, Sem) :-
	tv_semantics(love, Sem).

lex(but, dl(0,lit(s),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q,lambda(F,merge(drs([E,F],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(if, dr(0,dr(0,s,s),s), lambda(P, lambda(Q, lambda(_,bool(appl(P,_),->,appl(Q,E)))))).
lex('.', dl(0,lit(s),lit(txt)), lambda(P,appl(P,lambda(E,drs([],[]))))).
lex('.', dl(0,lit(txt),lit(txt)), lambda(P,appl(P,_))).
%lex('.', dl(0,lit(s),lit(txt)), lambda(P,appl(P,_))).
lex('.', dl(0,lit(s),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q,lambda(F,merge(drs([E,F],[appl(appl(before,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('.', dl(0,lit(txt),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q,lambda(F,merge(drs([E,F],[appl(appl(before,E),F)]),merge(appl(P,E),appl(Q,F))))))).
%lex('.', dl(0,lit(txt),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q, merge(P,Q)))).

special_string(".", '.').

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example(" Mary enters.", txt).
example(" Mary who enters smiles.", txt).
example("--- Anaphora and Quantifiers ---", xxx).
example(" Proud Mary enters.", txt).
example(" Mary enters quietly.", txt).
example(" A donkey of Mary enters.", txt).
example(" A donkey of a farmer enters.", txt).
example(" Mary enters.", txt).
example(" Sue appears smart but Mary is intelligent.", txt).
example(" Mary is intelligent.", txt).
example(" A tall man enters.", txt).
example(" Mary enters. She smiles.", txt).
example(" John loves Mary. She enters. He smiles.", txt).
example(" A man adores a woman.", txt).
example(" A man adores a woman. He smiles.", txt).
example(" Every man adores a woman.", txt).
example(" A man adores a woman. She abhors him.", txt).
example(" If a man bores a woman she ignores him.", txt).
example("*Every man loves a woman. He smiles.", txt).
example("*No woman adores a donkey. It stinks.", txt).
example(" If a farmer owns a donkey he beats it.", txt).
example("--- Coordination ---", xxx).
example(" John and Mary smile.", txt).
example(" John and a farmer adore Mary.", txt).
example(" John and a farmer adore Mary and Sue.", txt).
example(" John adores and Mary abhors a donkey.", txt).
example("--- Relativization ---", xxx).
example(" A farmer who owns a donkey beats it.", txt).
example(" A man whom Mary loves smiles.", txt).
