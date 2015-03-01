% -*- Mode: Prolog -*-
% ============================================================
% dutch.pl
% ============================================================
% !grail 3.1.1

% = Dutch verb clusters

% This grammar gives one of Michael Moortgat's solutions to the
% treatment of Dutch verbs in multimodal categorial grammars.
% It allows verb-initial, verb-second and verb-final sentences
% and gives the right interaction with wh extraction phenomena.

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


postulate(p(1,A,p(0,B,C)), p(0,B,p(1,A,C)), 'P1').
postulate(p(1,zip(0,A),zip(0,B)), zip(0,p(0,A,B)), 'K').

% = lazy evaluation

lazy_unpack(2).
lazy_unpack(i).

% = transparency

transparent(0).
transparent(2).
transparent(x).

transparent_dia(2).
transparent_dia(5).
transparent_dia(e).
transparent_dia(i).

% = continuity

continuous(2).
continuous(x).

continuous_dia(0).
continuous_dia(2).
continuous_dia(5).
continuous_dia(e).
continuous_dia(e1).
continuous_dia(i).
continuous_dia(w).

% = non internal modes

external(0).

external_dia(0).
external_dia(2).
external_dia(5).
external_dia(e).
external_dia(e1).
external_dia(i).
external_dia(w).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(bang(A,B), dia(A,box(A,B))).
macro(iv, dl(1,np,s)).
macro(tv, dl(1,np,iv)).
macro(rel, dl(1,n,n)).
macro(relprosub, dr(1,rel,dl(1,np,sub))).
macro(relpro, dr(1,rel,dl(1,bang(w,np),sub))).
macro(rpro, dr(1,pp,dr(1,pp,np))).
macro(qprom_r, dr(1,qm,box(i,dl(1,bang(w,rpro),s)))).
macro(qpros_r, dr(1,qs,dl(1,bang(w,rpro),box(e,s)))).
macro(relpro_r, dr(1,rel,dl(1,bang(w,rpro),sub))).
macro(qpro_main, dr(1,qm,box(i,dl(1,bang(w,np),s)))).
macro(qpro_sub, dr(1,xcs,dl(1,bang(w,np),box(e,s)))).
macro(dec, box(2,s)).
macro(sub, s).
macro(xcs, dl(x,dl(1,bang(w,cs),s),s)).
macro(tepro, bang(5,te)).
macro(xte, dl(xt,dl(1,dia(w,box(5,te)),s),s)).
macro(prep, dr(1,pp,np)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(boek, n, boek).
lex(meisje, n, meisje).
lex(taarten, n, tarts).
lex(het, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(de, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(koningin, n, queen).
lex(soepschildpad, n, lambda(A,appl(mock,appl(turtle,A)))).
lex(nijlpaarden, n, hippopotami).
lex(tot, prep, to).
lex(met, prep, with).
lex(op, prep, on).
lex(is, box(0,dl(1,ap,iv)), is).
lex(zijn, box(0,dl(1,ap,inf)), be).
lex(bereid, box(0,dl(1,pp,ap)), prepared).
lex(rekent, box(0,dl(1,pp,iv)), counts).
lex(rekenen, box(0,dl(1,pp,inf)), count).
lex(ik, np, i).
lex(fred, np, fred).
lex(henk, np, henk).
lex(marie, np, mary).
lex(alice, np, alice).
lex(jan, np, john).
lex(gek, ap, silly).
lex(vinden, box(0,dl(1,ap,dl(1,np,inf))), find).
lex(vindt, box(0,dl(1,ap,tv)), find).
lex(zal, box(0,dl(0,inf,iv)), will).
lex(wil, box(0,dl(0,inf,iv)), want).
lex(helpen, box(0,dr(1,dl(0,np,inf),inf)), help).
lex(zag, box(0,dr(1,dl(0,np,dl(0,np,s)),inf)), see).
lex(moeten, box(0,dl(0,inf,inf)), must).
lex(kunnen, box(0,dl(0,inf,inf)), can).
lex(willen, box(0,dl(0,inf,inf)), want).
lex(plagen, box(0,dl(1,np,inf)), tease).
lex(stelen, box(0,dl(1,np,inf)), steal).
lex(plaagt, box(0,tv), tease).
lex(slaapt, box(0,iv), sleeps).
lex(slapen, box(0,inf), sleep).
lex(weet, box(0,dl(1,cs,iv)), knows).
lex(weten, box(0,dl(1,cs,inf)), know).
lex(denkt, box(0,dl(1,cs,iv)), thinks).
lex(denken, box(0,dl(1,cs,inf)), think).
lex(als, dr(1,als,sub), if).
lex(of, dr(1,of,sub), whether).
lex(dat, dr(0,dat,sub), that).
lex(dan, dr(1,dan,box(i,s)), then).
lex(voeren, box(0,dl(0,np,inf)), feed).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example(" dat ik Henk Marie de nijlpaarden zag helpen voeren.", dat).
example("*dat ik Henk zag Marie de nijlpaarden helpen voeren.", dat).
example("*dat ik Henk Marie zag de nijlpaarden helpen voeren.", dat).
example("*dat ik Henk zag Marie helpen de nijlpaarden voeren.", dat).
