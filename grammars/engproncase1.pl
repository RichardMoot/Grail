% -*- Mode: Prolog -*-
% ============================================================
% engproncase1.pl
% ============================================================
% !grail 3.1.1

% = English pronoun case 1

% This program treat case distinctions among English
% pronouns using the unary modal operators: pronouns are
% typed Box_i Diamond_i np (with i in {nom,acc}); np-arguments
% are typed according to whether they are compatible with
% nominative or pronominal pronouns; ordinary np's appear
% in these positions automatically, in virtue of the validity
% of the sequent A |- Box_i Diamond_i A, for which the
% sequent np |- Box_i Diamond_i np is a special case.

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


% = non-internal modes

external(0).

% = transparency

% = continuity

continuous(0).
continuous_dia(n).
continuous_dia(a).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(npnom,box(n,dia(n,np))).
macro(npacc,box(a,dia(a,np))).

macro(ivp,dl(0,npnom,s)).
macro(tv,dr(0,ivp,npacc)).
macro(rel,dl(0,n,n)).
macro(comp,dr(0,rel,ivp)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

lex(he,npnom,x).
lex(him,npacc,u).
lex(she,npnom,y).
lex(her,npacc,v).

lex(alice,np,a).
lex(tweedledum,np,td).
lex(sleeps,ivp,sleep).
lex(irritates,tv,irritate).

% ============================================================
% Examples
% ============================================================

example(" Alice sleeps.",s).
example(" She sleeps.",s).
example("*Her sleeps.",s).
example(" Alice irritates Tweedledum.",s).
example(" She irritates him.",s).
example("*Her irritates him.",s).
example("*She irritates he.",s).
example("*Her irritates he.",s).

% ============================================================

