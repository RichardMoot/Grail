% -*- Mode: Prolog -*-
% ============================================================
% drt.pl
% ============================================================
% !grail 3.1.1

% = Discourse representation theory

% This grammar implements Reinhard Muskens' ideas on combining the
% Lambek calculus with Discourse Representation Theory while at the
% same time using one of Michael Moortgat's solutions for quantifier
% scope.

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

postulate(p(0,A,p(2,B,C)), p(2,B,p(0,A,C)), 'MC').
postulate(p(0,p(2,A,B),C), p(2,A,p(0,B,C)), 'MA').
postulate(p(1,A,p(0,B,C)), p(0,B,p(1,A,C)), 'MC').
postulate(p(1,A,p(0,B,C)), p(0,p(1,A,B),C), 'MA').


postulate(p(0,A,zip(p,B)), p(0,zip(p,B),A), 'Cp').
postulate(p(0,p(0,zip(p,A),B),C), p(0,zip(p,A),p(0,B,C)), 'MAp').
postulate(p(0,A,p(0,zip(p,B),C)), p(0,zip(p,B),p(0,A,C)), 'MCp').

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

continuous_dia(xxx).

% = non internal modes

external(_).

external_dia(_).

% = extraction/infixation

custom_first_order(p(1,dr(1,A0,dl(2,C0,B0)),dl(2,D0,E0)), p(1,dr(1,A,dl(2,C,B)),dl(2,D,E)), P, [X,Z]) :-
	!,
	flip(P, Q),
	gensym_neg(Q, U),
	gensym_pos(P, V),
	gensym_pos(P, W),
	add_first_order(A0, A, P, [V,W]),
	add_first_order(B0, B, Q, [V,W]),
	add_first_order(C0, C, P, [U,Z]),
	add_first_order(D0, D, Q, [U,Z]),
	add_first_order(E0, E, P, [X,Z]).

% = extraction

custom_first_order(dl(0,dia(p,box(p,A0)),B0), dl(0,dia(p,box(p,A)),B), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(q(A,B,C),p(1,dr(1,C,dl(2,A,B)),dl(2,A,A))).
macro(bang(A,B), dia(A,box(A,B))).
macro(rel, box(i,dl(0,n,n))).
macro(relbody, dl(0,bang(p,np),s)).
macro(relpro, dr(0,rel,relbody)).
macro(relpro(A), q(np,A,dr(0,rel,dl(0,bang(p,A),s)))).
macro(relpropp, q(np,np,relpro)).
macro(det, dr(0,gq,n)).
macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(gq, q(np,s,s)).
% macro(v_ex, dl(0,bang(p,tv),s)).
macro(v_ex, q(tv,s,s)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(a, det, lambda(P,pair(lambda(Q,merge(merge(drs([X],[]),appl(P,X)),appl(Q,X))),lambda(R,R)))).
lex(no, det, lambda(P,pair(lambda(Q,drs([],[not(merge(drs([X],[]),merge(appl(Q,X),appl(P,X))))])),lambda(R,R)))).
lex(every, det, lambda(P,pair(lambda(Q,bool(merge(drs([X],[]),appl(P,X)),->,appl(Q,X))),lambda(R,R)))).
lex(man, n, lambda(V, drs([], [appl(man,V)]))).
lex(woman, n, lambda(V, drs([], [appl(woman,V)]))).
lex(farmer, n, lambda(V, drs([], [appl(farmer,V)]))).
lex(donkey, n, lambda(V, drs([], [appl(donkey,V)]))).
lex(he, gq, pair(lambda(P, appl(P, X)),lambda(Y,Y))).
lex(him, gq, pair(lambda(P, appl(P, X)),lambda(Y,Y))).
lex(she, gq, pair(lambda(P, appl(P, X)),lambda(Y,Y))).
lex(her, gq, pair(lambda(P, appl(P, X)),lambda(Y,Y))).
lex(it, gq, pair(lambda(P, appl(P, X)),lambda(Y,Y))).
lex(stinks, iv, lambda(V, drs([], [appl(stink,V)]))).
lex(enters, iv, lambda(V, drs([], [appl(enter,V)]))).
lex(smiles, iv, lambda(V, drs([], [appl(smile,V)]))).
lex(adores, tv, lambda(V, lambda(W, drs([], [appl(appl(adore,V),W)])))).
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

example(" Every man loves a woman.", txt).
example(" No woman adores a donkey. It stinks.", txt).
example(" A man adores a woman. He smiles.", txt).
example(" If a farmer owns a donkey he beats it.", txt).
