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

macro(iv, dl(0, np, s)).
macro(tv, dr(0, iv, np)).

continuous(0).

external(0).

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

lex(the, dr(0, np, n), lambda(P,quant(iota,X,appl(P,X)))).
lex(a, dr(0, np, n), a).

lex(sir, dr(0, np, np), sir).
lex(king, dr(0, np, np), king).
lex(brother, dr(0, np, np), brother).

lex(holy, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(holy,X))))).
lex(brave, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(brave,X))))).
lex(black, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(black,X))))).
lex(silly, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(silly,X))))).
lex(great, dr(0, np, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(great,X))))).
lex(terrible, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(terrible,X))))).
lex(favourite, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(favourite,X))))).
lex(unladen, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(unladen,X))))).
lex(african, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(african,X))))).
lex(european, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(european,X))))).
lex(autonomous, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(autonomous,X))))).
lex(watery, dr(0, n, n), lambda(P,lambda(X,bool(appl(P,X),&,appl(watery,X))))).

lex(which, dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(X,bool(appl(Q,X),&,appl(P,X)))))).

lex(and, dr(0,dl(0,dr(0,n,n),dr(0,n,n)),dr(0,n,n)), lambda(P, lambda(Q, lambda(R, appl(Q,appl(P,R)))))).

lex(which, dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,np)))), lambda(P,lambda(Q, lambda(X, bool(appl(Q,X),&,appl(P,X)))))).

lex(king, n, king).
lex(knight, n, knight).
lex(knights, n, lambda(X,appl(knights,X))).
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

lex(is, dr(0,iv,dr(0,n,n)), lambda(Adj, lambda(S, appl(appl(is,appl(Adj,lambda(_,true))),S)))).

lex(say, dr(0, iv, s), say).

lex(of, dr(0,dl(0,n,n),np), lambda(X, lambda(P, lambda(Y, bool(appl(P,Y),&,appl(appl(of,X),Y)))))).

lex(a, dr(0,dr(0,s,iv),n), lambda(P, lambda(Q, quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(an, dr(0,dr(0,s,iv),n), lambda(P, lambda(Q, quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(someone, dr(0,s,iv), lambda(P, quant(exists,X,appl(P,X)))).
lex(everyone, dr(0,s,iv), lambda(P, quant(forall,X,appl(P,X)))).

lex(himself, dl(0,tv,iv), lambda(P,lambda(X,appl(appl(P,X),X)))).

lex(nearly, dr(0,iv,iv), lambda(P,lambda(X,appl(nearly,appl(P,X))))).
lex(quickly, dl(0,iv,iv), lambda(P,lambda(X,appl(quickly,appl(P,X))))).

lex(very, dr(0,dr(0,n,n),dr(0,n,n)), lambda(ADJ,lambda(N,lambda(X,bool(appl(N,X),&,appl(appl(very,appl(ADJ,lambda(_,true))),X)))))).

lex(most, dr(0,dr(0,s,iv),n), lambda(P,lambda(Q,bool(count(bool(P,intersect,Q)),gneq,count(bool(P,setminus,Q)))))).

% ============================================================
% = Tokenization
% ============================================================

% ============================================================
% = Example Phrases
% ============================================================

example(" beacon which Galahad saw.", n).
example(" --- AB ---", x).
example(" Lancelot rescues Galahad.", s).
example(" The very brave knight rescues Galahad.", s).
example(" Arthur represses Dennis.", s).
example(" Arthur is the king of the britons.", s). 
example(" The black knight menaces Arthur.", s).
example(" Arthur dismembers the black knight.", s).

example(" --- NL ---", x).
example(" Someone slept.", s).
example(" Most knights slept.", s).
example(" The very brave knight slept.", s).
example(" Lancelot is brave.", s).
example(" Lancelot is very brave.", s).
example("*The swallow is unladen african.", s).
example(" Someone throws the holy handgrenade.", s).
example(" An unladen african swallow flies quickly.", s).
example(" Robin nearly wet himself.", s).
example(" Someone wet himself.", s).
