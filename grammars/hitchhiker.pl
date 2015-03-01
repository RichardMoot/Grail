% -*- Mode: Prolog -*-
% ============================================================
% hitchhiker.pl
% ============================================================
% grail 3.1.1

% This grammar is meant to illustrate the usefulness of a
% combination of an associative mode "a" with a non-associative
% live "0".
% It is designed for use with the Grail tutorial on multimodal
% categorial grammars and structural rules, which can be found
% at

% http://www.labri.fr/perso/moot/tutorial/structural_rules.html

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
:- dynamic postulate/3,postulate1/3,special_string/2.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Postulates
% ============================================================

atomic_type(imp, e->t).
atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).


% = structural postulates

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass1'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass2'). % ass2

% = lazy evaluation

lazy_dl(a).
lazy_dl(0).

lazy_dr(a).
lazy_dr(0).

lazy_unpack(p).

% = transparency

transparent(0).
transparent(a).

transparent_dia(p).

% = continuity

continuous(a).
continuous(0).

% = non internal modes

external(0).
external(a).

external_dia(p).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(iv, dl(a,np,s)).
macro(tv, dr(a,iv,np)).
macro(prep, dr(a,pp,np)).
macro(gq_subj, dr(a,s,iv)).
macro(det_subj, dr(0,gq_subj,n)).
macro(gq_obj, dl(a,dr(a,s,np),s)).
macro(det_obj, dr(0,gq_obj,n)).
macro(refl, dl(0,tv,dl(0,np,s))).
macro(relpro, dr(0,rel,relbody)).
macro(relbody, dr(a,s,np)).
macro(rel, dl(0,n,n)).
macro(conj(A), dr(0,dl(0,A,A),A)).
macro(adj, dr(0,n,n)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(a, det_obj, lambda(A,lambda(B,quant(exists,C,bool(appl(A,C),&,appl(B,C)))))).
lex(a, det_subj, lambda(A,lambda(B,quant(exists,C,bool(appl(A,C),&,appl(B,C)))))).
lex(about, prep, about).
lex(and, conj(iv), lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(and, conj(s), lambda(A,lambda(B,bool(A,&,B)))).
lex(and, conj(dr(a,s,np)), lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(anything, dl(a,dr(a,lit(s),lit(np)),lit(s)), lambda(A,quant(forall,B,appl(A,B)))).
lex(apologize, dr(a,dl(a,lit(np),lit(s)),lit(pp)), apologize).
lex(arthur, lit(np), a).
lex(beeblebrox, dl(0,lit(np),lit(np)), b).
lex(blaster, lit(n), blaster).
lex(book, n, book).
lex(boy, n, boy).
lex(confronts, dr(a,dr(a,iv,pp),np), confront).
lex(dent, dl(0,lit(np),lit(np)), d).
lex(depresses, dr(a,dl(a,lit(np),lit(s)),lit(np)), depress).
lex(donot, dr(a,lit(imp),lit(imp)), not).
lex(drinks, dr(a,dl(a,lit(np),lit(s)),lit(np)), drink).
lex(drink, dr(a,dl(a,lit(np),lit(s)),lit(np)), drink).
lex(drink, n, drink).
lex(every, det_obj, lambda(A,lambda(B,quant(forall,C,bool(appl(A,C),->,appl(B,C)))))).
lex(every, det_subj, lambda(A,lambda(B,quant(forall,C,bool(appl(A,C),->,appl(B,C)))))).
lex(everyone, gq_obj, lambda(A,quant(forall,B,appl(A,B)))).
lex(everyone, gq_subj, lambda(A,quant(forall,B,appl(A,B)))).
lex(everything, dl(a,dr(a,lit(s),lit(np)),lit(s)), lambda(A,quant(forall,B,appl(A,B)))).
lex(everything, dr(a,lit(s),dl(a,lit(np),lit(s))), lambda(A,quant(forall,B,appl(A,B)))).
lex(for, dr(a,lit(pp),lit(np)), for).
lex(ford, lit(np), f).
lex(galactic, dr(0,lit(n),lit(n)), galactic).
lex(galaxy, lit(n), galaxy).
lex(gargle, dr(0,lit(n),lit(n)), gargle).
lex(girl, n, girl).
lex(robot, n, robot).
lex(guide, lit(n), guide).
lex(planet, lit(n), planet).
lex(harmless, dr(0,lit(n),lit(n)), harmless).
lex(hates, tv, hate).
lex(has, tv, have).
lex(herself, refl, lambda(A,lambda(B,appl(appl(A,B),B)))).
lex(himself, refl, lambda(A,lambda(B,appl(appl(A,B),B)))).
lex(hitchhiker, lit(n), hitchhiker).
lex(inconvenience, lit(n), inconvenience).
lex(likes, dr(a,dl(a,lit(np),lit(s)),lit(np)), like).
lex(loves, tv, love).
lex(marvin, lit(np), m).
lex(mcmillian, dl(0,lit(np),lit(np)), m).
lex(mostly, dr(0,dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))), mostly).
lex(needs, dr(a,iv,gq_obj), need).
lex(pan, dr(0,lit(n),lit(n)), pan).
lex(panic, lit(imp), panic).
lex(prefect, dl(0,lit(np),lit(np)), p).
lex(read, tv, read).
lex(s, dl(a,lit(n),dr(a,lit(n),lit(n))), lambda(A,lambda(B,appl(appl(of,A),B)))).
lex(of, dl(a,lit(n),dr(a,lit(n),lit(np))), lambda(A,lambda(B,appl(appl(of,A),B)))).
lex(president, lit(n), president).
lex(somebody, gq_obj, lambda(A,quant(exists,B,appl(A,B)))).
lex(somebody, gq_subj, lambda(A,quant(exists,B,appl(A,B)))).
lex(someone, dl(a,dr(a,lit(s),lit(np)),lit(s)), lambda(A,quant(exists,B,appl(A,B)))).
lex(something, dl(a,dr(a,lit(s),lit(np)),lit(s)), lambda(A,quant(exists,B,appl(A,B)))).
lex(something, dr(a,lit(s),dl(a,lit(np),lit(s))), lambda(A,quant(exists,B,appl(A,B)))).
lex(talks, dr(a,iv,pp), talk).
lex(that, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(today, dl(a,iv,iv), lambda(A,lambda(B,appl(today,appl(A,B))))).
lex(tricia, lit(np), t).
lex(who, dr(0,dl(0,n,n),dl(a,np,s)), lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(who, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(the, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(thinks, dr(a,iv,s), think).
lex(to, dr(a,dl(a,lit(n),lit(n)),lit(np)), to).
lex(towel, lit(n), towel).
lex(trillian, lit(np), t).
lex(we, lit(np), we).
lex(whom, relpro, lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(with, prep, with).
lex(yesterday, dl(a,iv,iv), lambda(A,lambda(B,appl(yesterday,appl(A,B))))).
lex(zaphod, lit(np), z).
lex(is, dr(a,dl(a,lit(np),lit(s)),lit(np)), is).
lex(is, dr(a,dl(a,lit(np),lit(s)),dr(0,lit(n),lit(n))), is).
lex('!', dl(0,imp,s), excl).

% = atomic_formula(Formula)

special_string("'s", s).
special_string("Don't", don_t).
special_string("don't", don_t).
special_string("!", '!').

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example(" --- basic ---", x).

example(" Don't panic!",s).
example(" Marvin depresses Arthur.",s).
example(" Mostly harmless.",adj).
example(" We apologize for the inconvenience.",s).
example(" Zaphod Beeblebrox is the president of the galaxy.",s).
example(" Zaphod drinks a pan-galactic gargle blaster.",s).
example(" Hitchhiker's guide to the galaxy.",n).

example(" --- wh extraction ---", x).

example(" The robot who depresses Arthur.", np).
example(" The robot who Arthur depresses.", np).
example(" The guide that Arthur read.",np).
example(" The guide that Arthur read today.",np).

example(" --- reflexives ---", x).

example(" Marvin hates himself.",s).
example(" Zaphod loves himself.",s).
example(" Zaphod talks about himself.",s).
example(" Trillian thinks Zaphod loves himself.",s).

example(" --- generalized quantifiers ---", x).

example(" Everyone likes someone.", s).
example(" Zaphod likes anything.",s).
example(" A president likes every drink.",s).
example(" Arthur needs a towel.",s).
example(" Everyone needs a towel.",s).
example(" Ford loves and Marvin hates something.",s).


