% -*- Mode: Prolog -*-
% ============================================================
% clitics.pl
% ============================================================
% !grail 3.1.1

:- encoding(iso_latin_1).

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
% Semantic Types
% ============================================================

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

% ============================================================
% Structural Rules
% ============================================================

postulate(p(0,A,p(0,B,zip(1,C))), p(0,p(0,A,B),zip(1,C)), 'MA').
postulate(p(0,p(0,A,zip(1,B)),C), p(0,p(0,A,C),zip(1,B)), 'MC').

% ============================================================
% Optimization Parameters
% ============================================================

% = inert modes

inert_dia(0).
inert_dia(1).

% = continuous modes

continuous(0).
continuous_dia(0).
continuous_dia(1).
continuous_dia(i).

% = external modes

external(0).
external_dia(i).

% = extraction

custom_first_order(dr(0,A0, dia(1,box(1,B0))), dr(0,A,dia(1,box(1,B))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [X,Y]),
	add_first_order(B0, B, P, [Z,Z]).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(np_bot, box(0,dia(0,np))).
macro(np_top, dia(0,box(0,np))).
macro(np_n, np).
macro(np_a, box(0,dia(0,dia(0,box(0,np))))).
macro(np_d, dia(0,box(0,box(0,dia(0,np))))).

macro(n_bot, box(0,dia(0,n))).
macro(n_top, dia(0,box(0,n))).
macro(n_m, n).
macro(n_f, box(0,dia(0,dia(0,box(0,n))))).
macro(n_p, dia(0,box(0,box(0,dia(0,n))))).

macro(s(3), box(0,box(0,dia(0,dia(0,s))))).
macro(s(2), box(0,dia(0,s))).
macro(s(1), s).
macro(s(0), dia(0,box(0,s))).
macro(cl, s(3)).

macro(up(B,C), dl(0,dia(1,box(1,C)),B)).
macro(clitic(A,B,C), dr(0,A,dr(0,B,dia(1,box(1,C))))).
macro(iv, dl(0,np_n,s(0))).
macro(vp(I), dl(0,np_n,s(I))).
macro(tv, dr(0,vp(0),np_a)).
macro(dtv, dr(0,dr(0,iv,np_d),np_a)).
macro(inf(I), dl(0,np_n,s(I))).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

%lex(plur, dl(0,n_bot,np), plur). 
lex(marie, np_top, marie).
lex(jean, np_top, jean).
lex(pomme, n_f, pomme).
lex(livre, n_m, un_livre).
lex(un, dr(0,np_top,n_m), un).
lex(une, dr(0,np_top,n_f), une).
lex(des, dr(0,np_top,n_p), des).
lex(le, clitic(vp(2),vp(1),np_a), lambda(A,lambda(B,appl(appl(A,le),B)))).
lex(la, clitic(vp(2),vp(1),np_a), lambda(A,lambda(B,appl(appl(A,la),B)))).
%lex(a_jean, np_d, jean).
lex(le, dr(0,np_top,n_m), le).
lex(la, dr(0,np_top,n_f), la).
lex(les, dr(0,np_top,n_p), les).
lex(à, dr(0,np_d,np_bot), a).
lex(au, dr(0,np_d,n_m), lambda(A,appl(a,appl(le,A)))).
lex(lui, clitic(vp(1),vp(0),np_d), lambda(A,lambda(B,appl(appl(A,lui),B)))).
lex(me, clitic(vp(3),vp(2),np_d), lambda(A,lambda(B,appl(appl(A,me),B)))).
lex(lire, dr(0,inf(3),np_a), lire).
lex(manger, dr(0,inf(3),np_a), manger).
lex(dormir, inf(3), dormir).
lex(dort, vp(2), lambda(A,appl(dort,A))).
lex(mange, tv, lambda(A,lambda(B,appl(appl(mange,A),B)))).
lex(donne, dtv, lambda(A,lambda(B,lambda(C,appl(appl(appl(donne,B),A),C))))).
lex(laisse, dr(0,dr(0,iv,dia(i,inf(3))),np_a), lambda(A,lambda(B,lambda(C,appl(appl(laisse,appl(B,A)),C))))).
lex(laisse, dr(0,dr(0,iv,np_d),inf(3)), lambda(A,lambda(B,lambda(C,appl(appl(laisse,appl(A,B)),C))))).
lex(veut, dr(0,iv,dia(i,inf(3))), lambda(A,lambda(B,appl(appl(veut,appl(A,B)),B)))).
lex(voulait, dr(0,iv,dia(i,inf(3))), lambda(A,lambda(B,appl(appl(veut,appl(A,B)),B)))).
lex(acheté, dr(0,vp(3),np_a), achete).
lex(a, dr(0,vp(0),vp(3)), avoir).

% ============================================================
% Tokenization
% ============================================================

% = special_string(String,LexPros)

special_string("l'", le).

% ============================================================
% Example Sentences
% ============================================================

% = example(String,Formula)

example("--- basic ---", null).
example(" Marie mange une pomme.", cl).
example(" Marie la mange.", cl).
example(" Marie donne une pomme à Jean.", cl).
example([32,77,97,114,105,101,32,108,97,32,100,111,110,110,101,32,224,32,74,101,97,110,46], cl).
example([32,77,97,114,105,101,32,108,117,105,32,100,111,110,110,101,32,117,110,101,32,112,111,109,109,101,46], cl).
example([32,77,97,114,105,101,32,108,97,32,108,117,105,32,100,111,110,110,101,46], cl).
example([42,77,97,114,105,101,32,108,117,105,32,108,97,32,100,111,110,110,101,46], cl).
example([32,77,97,114,105,101,32,109,101,32,108,97,32,100,111,110,110,101,46], cl).
example([42,77,97,114,105,101,32,108,97,32,109,101,32,100,111,110,110,101,46], cl).
example("--- avoir ---", null).
example(" Marie l'a acheté.", cl).
example("*Marie a l'acheté.", cl).
example("--- vouloir ---", null).
example([32,77,97,114,105,101,32,118,101,117,116,32,100,111,114,109,105,114,46], cl).
example([32,77,97,114,105,101,32,118,101,117,116,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,46], cl).
example([32,77,97,114,105,101,32,118,101,117,116,32,108,97,32,109,97,110,103,101,114,46], cl).
example([42,77,97,114,105,101,32,108,97,32,118,101,117,116,32,109,97,110,103,101,114,46], cl).
example("--- laisser ---", null).
example([32,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,100,111,114,109,105,114,46], cl).
example([32,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,100,111,114,109,105,114,46], cl).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,100,111,114,109,105,114,46], cl).
example([32,77,97,114,105,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,32,224,32,74,101,97,110,46], cl).
example([42,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,46], cl).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,46], cl).
example([42,77,97,114,105,101,32,108,101,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], cl).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,74,101,97,110,32,117,110,101,32,112,111,109,109,101,46], cl).
example([32,77,97,114,105,101,32,108,117,105,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,46], cl).
example([42,77,97,114,105,101,32,108,97,105,115,115,101,32,74,101,97,110,32,108,97,32,109,97,110,103,101,114,46], cl).
example([32,77,97,114,105,101,32,108,117,105,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], cl).
example([32,77,97,114,105,101,32,108,97,32,108,117,105,32,108,97,105,115,115,101,32,109,97,110,103,101,114,46], cl).
example([32,77,97,114,105,101,32,109,101,32,108,97,32,108,97,105,115,115,101,32,109,97,110,103,101,114,46], cl).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,108,97,32,109,97,110,103,101,114,46], cl).
example([32,77,97,114,105,101,32,109,101,32,108,97,105,115,115,101,32,109,97,110,103,101,114,32,117,110,101,32,112,111,109,109,101,46], cl).
