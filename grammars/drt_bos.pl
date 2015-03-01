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

atomic_type(n, e->s->s->t).
atomic_type(np, (e->s->s->t)->s->s->t).
atomic_type(s, s->s->t).
atomic_type(txt, s->s->t).

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

% ============================================================
% Lexicon
% ============================================================

:- dynamic boolean_type/1.

boolean_type(s->s->t).

% = lex(Pros,Formula,Sem)

% ((e->s->s->t)->s->s->t)->s->s->t.
lex(and, dr(0,dl(0,relbody,dr(0,s,np)),relbody), lambda(P,lambda(Q,lambda(X,merge(appl(P,X),appl(Q,X)))))).
lex(and, conj(np), lambda(N,lambda(M,lambda(P,merge(appl(N,P),appl(M,P)))))).
lex(whom, relpro, lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(P,lambda(R,appl(R,X)))))))).
lex(who, dr(0,dl(0,n,n),dl(0,np,s)), lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(P,lambda(R,appl(R,X)))))))).
lex(mary, np, lambda(P,merge(drs([X],[bool(X,=,mary)]),appl(P,X)))).
lex(sue, np, lambda(P,merge(drs([X],[bool(X,=,sue)]),appl(P,X)))).
lex(john, np, lambda(P,merge(drs([X],[bool(X,=,john)]),appl(P,X)))).
lex(pedro, np, lambda(P,merge(drs([X],[bool(X,=,pedro)]),appl(P,X)))).
lex(a, det, lambda(P,lambda(Q,merge(merge(drs([X],[]),appl(P,X)),appl(Q,X))))).
lex(no, det, lambda(P,lambda(Q,drs([],[not(merge(drs([X],[]),merge(appl(Q,X),appl(P,X))))])))).
lex(every, det, lambda(P,lambda(Q,bool(merge(drs([X],[]),appl(P,X)),->,appl(Q,X))))).
lex(man, n, lambda(V, drs([], [appl(man,V)]))).
lex(woman, n, lambda(V, drs([], [appl(woman,V)]))).
lex(farmer, n, lambda(V, drs([], [appl(farmer,V)]))).
lex(donkey, n, lambda(V, drs([], [appl(donkey,V)]))).
lex(he, np, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(him, np, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(she, np, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(her, np, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(it, np, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(stinks, iv, lambda(P, appl(P,lambda(V,drs([], [appl(stink,V)]))))).
lex(enters, iv, lambda(P, appl(P,lambda(V, drs([], [appl(enter,V)]))))).
lex(smiles, iv, lambda(P, appl(P,lambda(V, drs([], [appl(smile,V)]))))).
lex(smile, iv, lambda(P, appl(P,lambda(V, drs([], [appl(smile,V)]))))).
lex(adores, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(adore,W),V)])))))))).
lex(adore, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(adore,W),V)])))))))).
lex(abhors, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(abhor,W),V)])))))))).
lex(bores, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(bore,W),V)])))))))).
lex(ignores, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(ignore,W),V)])))))))).
lex(owns, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(own,W),V)])))))))).
lex(beats, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(beat,W),V)])))))))).
lex(loves, tv, lambda(P,lambda(Q,appl(Q,lambda(V,appl(P,lambda(W, drs([], [appl(appl(love,W),V)])))))))).
lex(if, dr(0,dr(0,s,s),s), lambda(P, lambda(Q, bool(P, ->, Q)))).
lex('.', dl(0,lit(s),lit(txt)), lambda(P, P)).
lex('.', dl(0,lit(s),dr(0,lit(txt),lit(txt))), lambda(P,lambda(Q, merge(P,Q)))).
%lex('.', dl(0,lit(txt),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q, merge(P,Q)))).

special_string(".", '.').

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("--- Anaphora and Quantifiers ---", xxx).
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
