% -*- Mode: Prolog -*-
% ============================================================
% engproncase2.pl
% ============================================================
% !grail 3.1.1

% = English pronoun case 2

% Like the fragment engproncase1.pl, this program treats
% case distinctions among English
% pronouns using the unary modal operators. But here the
% simplest category is assigned to accusative pronouns: they
% are taken to be bare np's. Full np's (names, descriptions,
% quantifiers) are then given the type Diamond Box np, and hence
% are allowed to occur in any environment allowing bare np's.
% Nominative pronouns are typed Box Diamond Diamond Box np. Thus,
% full np's can occur in nominative positions, but accusative pronouns
% cannot.

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

% = inert modes

%inert_dia(0).

% = non-internal modes

external(0).

% = transparency

% = continuity

continuous(0).
continuous_dia(0).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(npnom,box(0,dia(0,dia(0,box(0,np))))).
macro(npfull,dia(0,box(0,np))).

macro(ivp,dl(0,npnom,s)).
macro(tv,dr(0,ivp,np)).
macro(rel,dl(0,n,n)).
macro(comp,dr(0,rel,ivp)).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).

lex(he,npnom,x).
lex(him,np,u).
lex(she,npnom,y).
lex(her,np,v).

lex(alice,npfull,a).
lex(tweedledum,npfull,td).
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

