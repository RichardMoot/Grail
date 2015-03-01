% -*- Mode: Prolog -*-
% ============================================================
% Itipy.pl
% ============================================================
% !grail 3.1.1 

% ============================================================
% Semantic Types
% ============================================================

:- dynamic atomic_type/2.

atomic_type(n_loc, e_loc->t).
atomic_type(n_time, e_time->t).
atomic_type(np_loc, e_loc).
atomic_type(np_time, e_time).
atomic_type(ppas, e->t).
atomic_type(ppres, e->t).
atomic_type(inf, e->t).
atomic_type(text, t).
atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).
atomic_type(pp_loc, (e_loc->t)->(e_loc->t)).
atomic_type(pp_time, (e_time->t)->(e_time->t)).

:- dynamic entity_type/1.

entity_type(e).
entity_type(e_time).
entity_type(e_loc).

% ============================================================
% Optimization Parameters
% ============================================================

% = inert modes

inert(0).

% = continuous modes

continuous(0).
continuous(a).

% = external modes

external(0).
external(a).

% = extraction

custom_first_order(dr(0,A0,dia(0,box(0,B0))), dr(0,A,dia(0,box(0,B))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [X,Y]),
	add_first_order(B0, B, P, [Z,Z]).

% ============================================================
% Structural Rules
% ============================================================

% = structural postulates

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass'). % ass2

postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(0,A,B),zip(0,C)), 'Whr1').
postulate(p(0,p(0,A,zip(0,C)),B), p(0,p(0,A,B),zip(0,C)), 'Whr2').

% ============================================================
% = Macros
% ============================================================

:- dynamic macro/2.

macro(iv, dl(0, np, s)).
macro(tv, dr(0, iv, np)).
macro(dtv, dr(0, tv, np)).
macro(prep, dr(0,pp,np)).
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
macro(npostm, dl(0,n,n)).
macro(prep_n, dr(0,dl(0,n,n),np)). 

% ============================================================
% Lexicon
% ============================================================

% personal pronouns

lex(je, np, appl(ref,i)).
lex(nous, np, appl(ref,we)).

% determiners

lex(toutes, dr(0,np,np), all).

lex(le, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(la, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(les, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(des, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(une, dr(0,np,n), lambda(P,quant(iota,X,appl(P,X)))).
lex(un, dr(0,np,n), lambda(P,quant(iota,X,appl(P,X)))).

% prepositions

lex(la, dr(0,np_time,n_time), lambda(A,quant(iota,B,appl(A,B)))).

lex(à, prep_n, lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(with,X),Z)))))).
lex(à, dr(0,dl(0,n,n),n), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(with,X),Z)))))).
lex(à, dr(0,pp_loc,np_loc), lambda(X, lambda(P, lambda(Z, bool(appl(P,Z),&,appl(at,X)))))).
lex(à, dr(0,dl(0,iv,iv),np_time), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(at,X),Z)))))).
lex(à, dr(0,dl(0,iv,iv),np_loc), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(at,X),Z)))))).

lex(au, dr(0,dl(0,s,s),n), in).

lex(depuis, dr(0,pp_time,np_time), since).

lex(par, prep_n, lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(with,X),Z)))))).
lex(par, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(by,X),Z)))))).

lex(vers, dr(0,dl(0,iv,iv),np_time), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(around,X),Z)))))).

lex(pour, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(for,X),Z)))))).
lex(pour, dr(0,dl(0,iv,iv),inf), lambda(Q,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(Q,Z)))))).

lex(dans, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(in,X),Z)))))).

lex(avec, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(with,X),Z)))))).
lex(avec, dr(0,dr(0,s,s),np), lambda(X,lambda(P,bool(P,&,appl(with,X))))).

lex(après, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(after,X),Z)))))).

lex(dans, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(in,X),Z)))))).

lex(de, dr(0,dl(0,n,n),n), lambda(P,lambda(Q,lambda(X,appl(appl(of,appl(P,X)),appl(Q,X)))))).
lex(de, dr(0,dl(0,n,n),np_loc), lambda(X,lambda(P,lambda(Y,bool(appl(of,X),&,appl(P,Y)))))).
lex(de, dr(0,dl(0,n,n),np), lambda(X,lambda(P,lambda(Y,bool(appl(of,X),&,appl(P,Y)))))).
lex(de, dr(0,dl(0,iv,iv),np), lambda(X,lambda(P,lambda(Y,bool(appl(of,X),&,appl(P,Y)))))).

lex(du, dr(0,dl(0,n_time,n_time),n_time), of).
lex(du, dr(0,dl(0,n,n),n), of).

lex(en, dr(0,dl(0,s,s),np), in).
lex(en, dr(0,dr(0,s,s),ppres), by).

% verbs

lex(fut, dr(0,iv,dl(0,n,n)), was).
lex(arrivâmes, dr(0,iv,pp_loc), arrive).
lex(sert, dr(0,iv,pp_time), operates).
lex(plaint, dr(0,iv,np), complains).
lex(cause, dr(0,dr(0,iv,pp),np), cause).

% auxiliary verbs

lex(ai, dr(0,iv,ppas), have).
lex(suis, dr(0,iv,ppas), have).
lex(fut, dr(0,iv,ppas), was).
lex(avons, dr(0,iv,ppas), have).

% past participles

lex(parti, ppas, left).
lex(vu, dr(0,ppas,np), see).
lex(baigné, dr(0,ppas,np), bathe).
lex(construite, ppas, built).
lex(mis, dr(0,ppas,np_time), take).

lex(atteint, dr(0,ppas,np_loc), reach).
lex(quitté, dr(0,ppas,np_loc), leave).

lex(passé, dr(0,dr(0,ppas,pp_loc),np_time), pass).

% present participles

lex(louvoyant, ppres, tacking).
lex(ramant, ppres, rowing).

% infinitives

lex(arriver, dr(0,inf,pp_loc), arrive).

% adjectives

lex(breve, dr(0,n,n), short).
lex(belle, dl(0,n,n), beautiful).
lex(argéable, dl(0,n,n), agreable).
lex(supérieure, dl(0,n,n), superieure).
lex(contraire, dl(0,n,n), lambda(P,lambda(X,bool(appl(P,X),&,appl(adverse,X))))).
lex(lenticulaire, dl(0,n,n), lenticulaire).
lex(actuel, dl(0,n,n), current).

% numbers

lex(un, dr(0,np_time,n_time), one).
lex(deux, dr(0,np_time,n_time), two).
lex(trois, dr(0,np_time,n_time), three).
lex(quatre, dr(0,np_time,n_time), four).
lex(cinq, dr(0,np_time,n_time), five).
lex(six, dr(0,np_time,n_time), six).
lex(sept, dr(0,np_time,n_time), seven).
lex(huit, dr(0,np_time,n_time), eight).
lex(neuf, dr(0,np_time,n_time), nine).
lex(dix, dr(0,np_time,n_time), ten).
lex(onze, dr(0,np_time,n_time), eleven).
lex(douze, dr(0,np_time,n_time), twelve).

lex(1, dr(0,np_time,n_time), one).
lex(2, dr(0,np_time,n_time), two).
lex(3, dr(0,np_time,n_time), three).
lex(4, dr(0,np_time,n_time), four).
lex(5, dr(0,np_time,n_time), five).
lex(6, dr(0,np_time,n_time), six).
lex(7, dr(0,np_time,n_time), seven).
lex(8, dr(0,np_time,n_time), eight).
lex(9, dr(0,np_time,n_time), nine).
lex(10, dr(0,np_time,n_time), ten).
lex(11, dr(0,np_time,n_time), eleven).
lex(12, dr(0,np_time,n_time), twelve).

lex(xiv, dl(0,n,n), fourteen).
lex(1789, np, 1789).

% noun phrases for times

lex(midi, np_time, noon).
lex(dimanche, np_time, appl(time,appl(day,sunday))).
lex(onze_heures_et_quart, np_time, appl(time,bool(appl(hour,11),&,appl(minute,15)))).

% nouns for times

lex(matin, n_time, morning).
lex(ans, n_time, years).
lex(heures, n_time, hours).
lex(nuit, n_time, night).

% noun phrases for places

lex(cordouan, np_loc, appl(location, cordouan)).
lex(bordeaux, np_loc, appl(location, bordeaux)).
lex(royan, np_loc, appl(location, royan)).

% nouns

lex(base, n, base).
lex(traversée, n, voyage).
lex(bateau, n, boat).
lex(groupe, n, group).
lex(gens, n, people).
lex(phare, n, lighthouse).
lex(pinasse, n, pinasse).
lex(temps, n, temps).
lex(louis, n, louis).
lex(partie, n, partie).
lex(journée, n, day).
lex(compagnie, n, company).
lex(vent, n, wind).
lex(aide, n, help).
lex(marée, n, tide).
lex(parties, n, parts).
lex(vapeur, n, vapor).
lex(dispositif, n, dispositif).
lex(gardien, n, guard).
lex(lampe, n, light).
lex(ennuis, n, problems).

% clitics

lex(me, dr(0,iv,dr(0,iv,dia(0,box(0,np)))), lambda(P,appl(P,appl(ref,i)))).
lex(se, dr(0,iv,dr(0,iv,dia(0,box(0,np)))), lambda(P,appl(P,appl(ref,he)))).
lex(lui, dr(0,iv,dr(0,iv,dia(0,box(0,pp)))), lambda(P,appl(P,appl(ref,he)))).
lex(y, dr(0,iv,dr(0,iv,dia(0,box(0,pp_loc)))), lambda(P,appl(P,appl(ref,there)))).

% adverbs

lex(cependant, dr(0,s,s), however).
lex(seulement, dl(0,iv,iv), only).
lex(plutôt, dr(0,dl(0,n,n),dl(0,n,n)), rather).

%

lex(qui, dr(0,dl(0,n,n),iv), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X)))))).

% sentence end

lex(dot, dl(0,s,text), lambda(X,X)).

% coordinations

lex(comma, conj(dr(0,s,s)), lambda(Q,lambda(P,lambda(X,bool(appl(P,X),&,appl(Q,X)))))).
lex(comma, dr(0,dl(0,s,s),p(0,np,dl(0,s,s))), and).

lex(et, conj(dr(0,s,s)), lambda(Q,lambda(P,lambda(X,bool(appl(P,X),&,appl(Q,X)))))).
lex(et, conj(s), lambda(Q, lambda(P,bool(P,&,Q)))).
lex(et, dr(0,dl(0,s,s),p(0,np,dl(0,n,n))), and).

special_string(",", comma).
special_string(";", semi).
special_string(".", dot).
special_string("j'", je).
special_string("J'", je).
special_string("l'", le).
special_string("L'", le).
special_string("11h1/4", onze_heures_et_quart).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example(" Dimanche", np_time).
example(" Je me suis baigné.", text).
example(" Je suis parti à 11h1/4 avec un groupe de gens pour le phare de Cordouan dans une pinasse.", text).
example(" Vent contraire", n).
example(" En louvoyant, en ramant et avec l'aide de la marée", dr(0,s,s)).
example(" Nous arrivâmes à Cordouan vers trois heures.", text).
example(" En louvoyant, en ramant et avec l'aide de la maree nous arrivâmes à Cordouan vers trois heures.", text).
example(" Cependant la journée fut belle et la compagnie plutôt argéable.", text).
example(" En louvoyant, en ramant et avec l'aide de la maree nous arrivâmes a Cordouan vers trois heures; cependant la journée fut belle et la compagnie plutôt agréable.", text).
example(" J'ai vu toutes les parties du phare.", text).
example(" La base fut construite au temps de Louis XIV, la partie supérieure en 1789.", text).
example(" J'ai vu toutes les parties du phare; la base fut construite au temps de Louis XIV, la partie supérieure en 1789.", text).
example(" Le dispositif lenticulaire actuel sert depuis douze ans.", text).
example(" Le gardien se plaint seulement de la lampe qui lui cause des ennuis.", text).
example(" Nous avons quitté Cordouan vers 5 heures.", text).
example(" Nous avons mis deux heures pour arriver à Royan.", text).
example(" Presque calme; nous avons mis deux heures pour arriver à Royan.", text).
example(" J'y ai passé la nuit.", text).
example(" Je suis parti par le bateau à vapeur.", text).
example(" Je suis parti par le bateau à vapeur à 6 heures du matin.", text).
example(" J'ai atteint Bordeaux à midi après une breve traversée.", text). 
example(" Je suis parti par le bateau à vapeur à 6 heures du matin et j'ai atteint Bordeaux à midi après une breve traversée.", text). 
