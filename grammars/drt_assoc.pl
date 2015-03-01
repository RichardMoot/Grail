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
atomic_type(np, e).
atomic_type(s, s->s->t).
atomic_type(txt, s->s->t).

% = structural postulates

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass'). % ass2

% = transparency

transparent(0).
transparent(1).
transparent(2).

transparent_dia(i).
transparent_dia(l).
transparent_dia(p).
transparent_dia(r).
transparent_dia(t).

% = continuity

continuous(0).
continuous(a).

continuous_dia(xxx).

% = non internal modes

external(_).

external_dia(_).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(iv,dl(a,np,s)).
macro(tv,dr(a,iv,np)).
macro(prep,dr(a,pp,np)).
macro(gq_subj,dr(a,s,iv)).
macro(det_subj,dr(0,gq_subj,n)).
macro(gq_obj,dl(a,dr(a,s,np),s)).
macro(det_obj,dr(0,gq_obj,n)).
macro(refl,dl(a,tv,iv)).
macro(relpro,dr(a,rel,relbody) ).
macro(relbody,dr(a,s,np) ).
macro(rel,dl(a,n,n) ).
macro(conj(X),dr(a,dl(a,X,X),X)).

% ============================================================
% Lexicon
% ============================================================

:- dynamic boolean_type/1.

boolean_type(s->s->t).

% = lex(Pros,Formula,Sem)

lex(mary, gq_subj, lambda(P,merge(drs([X],[appl(mary,X)]),appl(P,X)))).
lex(mary, gq_obj, lambda(P,merge(drs([X],[appl(mary,X)]),appl(P,X)))).
lex(john, gq_subj, lambda(P,merge(drs([X],[appl(john,X)]),appl(P,X)))).
lex(john, gq_obj, lambda(P,merge(drs([X],[appl(john,X)]),appl(P,X)))).
%lex(mary, np, mary).
%lex(john, np, john).
lex(a, det_subj, lambda(P,lambda(Q,merge(merge(drs([X],[]),appl(P,X)),appl(Q,X))))).
lex(a, det_obj, lambda(P,lambda(Q,merge(merge(drs([X],[]),appl(P,X)),appl(Q,X))))).
lex(no, det_subj, lambda(P,lambda(Q,drs([],[not(merge(drs([X],[]),merge(appl(Q,X),appl(P,X))))])))).
lex(no, det_obj, lambda(P,lambda(Q,drs([],[not(merge(drs([X],[]),merge(appl(Q,X),appl(P,X))))])))).
lex(every, det_subj, lambda(P,lambda(Q,drs([],[bool(merge(drs([X],[]),appl(P,X)),->,appl(Q,X))])))).
lex(every, det_obj,  lambda(P,lambda(Q,drs([],[bool(merge(drs([X],[]),appl(P,X)),->,appl(Q,X))])))).
lex(man, n, lambda(V, drs([], [appl(man,V)]))).
lex(child, n, lambda(V, drs([], [appl(child,V)]))).
lex(pizza, n, lambda(V, drs([], [appl(pizza,V)]))).
lex(woman, n, lambda(V, drs([], [appl(woman,V)]))).
lex(farmer, n, lambda(V, drs([], [appl(farmer,V)]))).
lex(donkey, n, lambda(V, drs([], [appl(donkey,V)]))).
lex(he, gq_subj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(him, gq_obj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(she, gq_subj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(her, gq_obj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(it, gq_subj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(it, gq_obj, lambda(P,merge(drs([],[bool(X,=,?)]),appl(P,X)))).
lex(stinks, iv, lambda(V, drs([], [appl(stink,V)]))).
lex(enters, iv, lambda(V, drs([], [appl(enter,V)]))).
lex(smiles, iv, lambda(V, drs([], [appl(smile,V)]))).
lex(ate, tv, lambda(V, lambda(W, drs([], [appl(appl(ate,V),W)])))).
lex(adores, tv, lambda(V, lambda(W, drs([], [appl(appl(adore,V),W)])))).
lex(abhors, tv, lambda(V, lambda(W, drs([], [appl(appl(abhor,V),W)])))).
lex(bores, tv, lambda(V, lambda(W, drs([], [appl(appl(bore,V),W)])))).
lex(ignores, tv, lambda(V, lambda(W, drs([], [appl(appl(ignore,V),W)])))).
lex(owns, tv, lambda(V, lambda(W, drs([], [appl(appl(own,V),W)])))).
lex(beats, tv, lambda(V, lambda(W, drs([], [appl(appl(beat,V),W)])))).
lex(loves, tv, lambda(V, lambda(W, drs([], [appl(appl(love,V),W)])))).
lex(if, dr(0,dr(0,s,s),s), lambda(P, lambda(Q, bool(P, ->, Q)))).
lex('.', dl(0,lit(s),lit(txt)), lambda(P, P)).
lex('.', dl(0,lit(s),dr(0,lit(txt),lit(txt))), lambda(P,lambda(Q, merge(P,Q)))).
%lex('.', dl(0,lit(txt),dr(0,lit(txt),lit(s))), lambda(P,lambda(Q, merge(P,Q)))).

special_string(".", '.').

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("--- Anaphora ---", xxx).
example(" Every child ate a pizza.", txt).
example(" A man enters.", txt).
example(" He smiles.", txt).
example(" A man enters. He smiles.", txt).
example(" Mary enters. She smiles.", txt).
example(" John loves Mary. She enters. He smiles.", txt).
example("--- Quantifiers and Anaphora ---", xxx).
example(" A man adores a woman.", txt).
example(" A man adores a woman. He smiles.", txt).
example(" Every man adores a woman.", txt).
example("*Every man loves a woman. He smiles.", txt).
example(" Every man loves a woman. She smiles.", txt).
example(" A man adores a woman. She abhors him.", txt).
example(" If a man bores a woman she ignores him.", txt).
example("*No woman adores a donkey. It stinks.", txt).
example(" If a farmer owns a donkey he beats it.", txt).
