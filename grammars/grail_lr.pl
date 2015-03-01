% -*- Mode: Prolog -*-
% ============================================================
% Grail
% ============================================================
% !grail 3.1.1

% ============================================================
% = Structural Conversions
% ============================================================

% ============================================================
% = Macros
% ============================================================

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

:- dynamic macro/2.

macro(iv, dl(>, np, s)).
macro(tv, dr(<, iv, np)).

continuous(<).
continuous(>).

external(<).
external(>).

% ============================================================
% = Lexicon
% ============================================================

:- dynamic lex/3.

lex(arthur, np, arthur).
lex(lancelot, np, lancelot).
lex(bedevere, np, bedevere).
lex(robin, np, robin).
lex(galahad, np, galahad).
lex(roger, np, roger).
lex(assyria, np, assyria).
lex(camelot, np, camelot).
lex(antioch, np, antioch).
lex(maynard, np, maynard).
lex(tim, np, tim).
lex(dennis, np, dennis).
lex(zoot, np, zoot).
lex(dingo, np, dingo).

lex(the, dr(>, np, n), lambda(P,quant(iota,X,appl(P,X)))).
lex(a, dr(>, np, n), a).

lex(sir, dr(>, np, np), sir).
lex(king, dr(>, np, np), king).
lex(brother, dr(>, np, np), brother).

lex(holy, dr(>, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(holy,X))))).
lex(brave, dr(>, n, n), brave).
lex(black, dr(>, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(black,X))))).
lex(silly, dr(>, n, n), silly).
lex(great, dr(>, np, n), great).
lex(terrible, dr(>, n, n), terrible).
lex(favourite, dr(>, n, n), favourite).
lex(unladen, dr(>, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(unladen,X))))).
lex(african, dr(>, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(african,X))))).
lex(european, dr(>, n, n), european).
lex(autonomous, dr(>, n, n), autonomous).
lex(watery, dr(>, n, n), watery).

lex(king, n, king).
lex(knight, n, knight).
lex(knights, n, knights).
lex(grail, n, grail).
lex(beacon, n, beacon).
lex(handgrenade, n, handgrenade).
lex(shrubbery, n, shrubbery).
lex(shrubber, n, shrubber).
lex(witch, n, witch).
lex(newt, n, newt).
lex(name, n, name).
lex(quest, n, quest).
lex(colour, n, colour).
lex(capital, n, captial).
lex(name, n, name).
lex(swallow, n, swallow).
lex(velocity, n, velocity).
lex(britons, n, britons).
lex(enchanter, n, enchanter).
lex(rabbit, n, rabbit).
lex(tart, n, tart).
lex(sword, n, sword).
lex(collective, n, collective).
lex(peril, n, peril).

lex(flies, iv, fly).
lex(slept, iv, sleep).

lex(dismembers, tv, dismember).
lex(dismembered, tv, dismember).
lex(represses, tv, repress).
lex(menaces, tv, menace).
lex(rescues, tv, rescue).
lex(wet, tv, wet).
lex(saw, tv, see).
lex(burn, tv, burn).
lex(spank, tv, spank).
lex(throws, tv, throws).
lex(is, tv, is).
%lex(were, dr(0, dl(0, box(0, dia(0, np)), s), pp), were).
%lex(are, dr(0, dl(0, box(0, dia(0, np)), s), np), are).
lex(was, dr(0, iv, pp), was).
lex(talks, dr(0, iv, pp), talk).

lex(is, dr(<,iv,dr(>,n,n)), is).

lex(say, dr(<, iv, s), say).

lex(of, dr(>,dl(<,n,n),np), lambda(X, lambda(P, lambda(Y, bool(appl(P,Y),&,appl(appl(of,X),Y)))))).

lex(a, dr(>,dr(>,s,iv),n), lambda(P, lambda(Q, quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(an, dr(>,dr(>,s,iv),n), lambda(P, lambda(Q, quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(someone, dr(>,s,iv), lambda(P, quant(exists,X,appl(P,X)))).
lex(everyone, dr(>,s,iv), lambda(P, quant(forall,X,appl(P,X)))).

lex(himself, dl(<,tv,iv), lambda(P,lambda(X,appl(appl(P,X),X)))).

lex(nearly, dr(>,iv,iv), lambda(P,lambda(X,appl(nearly,appl(P,X))))).
lex(quickly, dl(<,iv,iv), lambda(P,lambda(X,appl(quickly,appl(P,X))))).


% ============================================================
% = Tokenization
% ============================================================

% ============================================================
% = Example Phrases
% ============================================================

example(" --- AB ---", x).
example(" Lancelot rescues Galahad.", s).
example(" Arthur represses Dennis.", s).
example(" Arthur is the king of the britons.", s). 
example(" The black knight menaces Arthur.", s).
example(" Arthur dismembers the black knight.", s).

example(" --- NL ---", x).
example(" Someone slept.", s).
example(" Lancelot is brave.", s).
example("*The swallow is unladen african.", s).
example(" Someone throws the holy handgrenade.", s).
example(" An unladen african swallow flies quickly.", s).
example(" Robin nearly wet himself.", s).
example(" Someone wet himself.", s).
