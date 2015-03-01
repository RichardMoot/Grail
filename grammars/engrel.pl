% ============================================================
% File header
% ============================================================
% !labels v1.0

% ============================================================
% engrel13aug.pl
% This program treats some simple English relative clause constructions.
% With pied-piping.

% ============================================================

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

% postulate 'Pr' moves a fronted A to the end

postulate(p(0,B,p(r,C,A)),p(w,A,p(0,B,C)),'Pr0').

% postulate 'RAssoc10' is a mixed associativity rule

postulate(p(1,A,p(r,B,C)),p(r,p(1,A,B),C),'RAssoc10').

%postulate(p(1,A,p(pp,B,C)),p(r,p(1,A,B),C),'MRAssoc1pp0').


%

postulate(p(1,p(r,A,C),B),p(r,p(1,A,B),C),'MAssCom10').

postulate(p(1,A,B),p(r,A,B),'rto1').

postulate(p(pp,A,B),p(r,A,B),'rtopp').

postulate(p(0,A,B),p(w,A,B),'wto0').

% pp can compose

postulate(p(pp,A,p(pp,B,C)),p(pp,p(pp,A,B),C),'RAssoc-pp').


% postulates for relative `which'


% first as quantifier:

postulate(p(q,A,t),zip(q,A),'diaq2prodq').

postulate(zip(q,A),p(q,A,x-t),'prodq2diaq').

postulate(p(q,B,zip(l,p(pp,A,C))),p(pp,A,p(q,B,C)),'pp-up').

postulate(p(pp,A,p(q,B,C)),p(q,B,zip(l,p(pp,A,C))),'pp-down').

postulate(p(q,A,zip(l,p(w,B,C))),p(w,p(q,A,B),C),'q-up').

postulate(p(w,p(q,A,B),C),p(q,A,zip(l,p(w,B,C))),'q-down').

 


% = non-internal modes

external(0).
external(1).
external(w).
external(pp).
external_dia(_).

% = transparency

% = continuity

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(ivp,dl(0,np,s)).
macro(trans,dr(1,ivp,np)).
macro(rel,dl(1,n,n)).
macro(comp,dr(1,rel,dl(w,np,s))).




% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Syn,Sem).







lex(tweedledum,np,td).
lex(alice,np,alice).
lex(sleeps,dl(0,np,s),sleep).
lex(book,n,book).
lex(irritates,dr(1,dl(0,np,s),np),irritate).

lex(on,dr(pp,onpp,np),on).
lex(depends,dr(1,ivp,onpp),depend).

lex(logic,n,logic).
lex(which,dia(q,dr(q,dl(1,n,n),dl(q,box(q,np),s))),lambda(A,lambda(B,lambda(C,bool(appl(B,C),&,appl(A,C)))))).
lex(that,comp,lambda(A,lambda(B,lambda(C,bool(appl(B,C),&,appl(A,C)))))).
%lex(that,dr(,lambda(A,lambda(B,lambda(C,bool(appl(B,C),&,appl(A,C)))))).

% ============================================================
% Examples
% ============================================================
example(" Alice depends on Tweedledum",s).
example(" On Tweedledum Alice depends",s).
example(" Tweedledum Alice depends on",s).

example(" Logic that Alice depends on",n).
example(" Logic which Alice depends on",n).
example(" Logic on which Alice depends",n).

example(" Alice sleeps",s).
example(" Alice irritates Tweedledum",s).
example(" irritates Alice",ivp).
example(" that irritates Alice.",rel).
example(" book that irritates Alice.",n).

% ============================================================

