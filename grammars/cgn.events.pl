% -*- Mode: Prolog -*-
% ============================================================
% cgn.pl
% ============================================================
% !grail 3.1.1

% This grammar is made for use with the supertagger and the grammar
% extracted from the Corpus Gesproken Nederlands (CGN, Spoken Dutch
% Corpus).

% It contains a mix of macro definitions, structural rules and the
% start of a semantic component, which will be made more and more
% complete over time.

davidsonian(neo).
% davidsonian(classic).

macro(dr(0,dl(0,n,n),np),dr(0,dl(0,lit(n(X)),lit(n(X))),lit(n(p)))).
macro(dl(0,lit(s),lit(txt)),dl(0,lit(s(_)),lit(txt))).
macro(dl(0,s,txt),dl(0,lit(s(_)),txt)).
macro(dl(0,n,txt),dl(0,lit(n(_)),txt)).
macro(dl(0,np,txt),dl(0,lit(n(p)),txt)).

macro(dr(0,n,n),dr(0,lit(n(X)),lit(n(X)))).
macro(dl(0,n,n),dl(0,lit(n(X)),lit(n(X)))).

macro(dr(0,s,s),dr(0,lit(s(X)),lit(s(X)))).
macro(dr(4,s,s),dr(4,lit(s(X)),lit(s(X)))).
macro(dl(0,s,s),dl(0,lit(s(X)),lit(s(X)))).
macro(dl(3,s,s),dl(3,lit(s(X)),lit(s(X)))).

macro(np, lit(n(p))).
macro(n, lit(n(_))).

macro(pp, lit(pp(_))).
macro(pp_aan, lit(pp(aan))).
macro(pp_achter, lit(pp(achter))).
macro(pp_af, lit(pp(af))).
macro(pp_beneden, lit(pp(beneden))).
macro(pp_boven, lit(pp(boven))).
macro(pp_buiten, lit(pp(buiten))).
macro(pp_in, lit(pp(in))).
macro(pp_met, lit(pp(met))).
macro(pp_na, lit(pp(na))).
macro(pp_naar, lit(pp(naar))).
macro(pp_naast, lit(pp(naast))).
macro(pp_op, lit(pp(op))).
macro(pp_onder, lit(pp(onder))).
macro(pp_over, lit(pp(over))).
macro(pp_te, lit(pp(te))).
macro(pp_tegen, lit(pp(tegen))).
macro(pp_tussen, lit(pp(tussen))).
macro(pp_uit, lit(pp(uit))).
macro(pp_van, lit(pp(van))).

macro(lit(s), lit(s(_)).
macro(s, lit(s(_))).
macro(s_whq,lit(s(whq))).
macro(s_whsub,lit(s(whsub))).
macro(s_whrel,lit(s(whrel))).
macro(s_cp,lit(s(cp))).
macro(s_sub,lit(s(sub))).
macro(s_main,lit(s(main))).
macro(s_ppart,lit(s(ppart))).
macro(s_v1,lit(s(v1))).
macro(s_inf,lit(s(inf))).
macro(s_ti,lit(s(ti))).
macro(s_oti,lit(s(oti))).
macro(s_ahi,lit(s(ahi))).



macro(p(0,dr(0,A,B),C), dr(0,A,dl(0,dia(p,box(p,C)),B))).

continuous(0).

external(0).

% =

custom_first_order(lit(n(Z)), lit(n, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(np(Z)), lit(np, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(pp(Z)), lit(pp, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(s(Z)), lit(s, [X,Y,Z]), _, [X,Y]). 

% = extraction

custom_first_order(dl(0,dia(p,box(p,A0)),B0), dl(0,dia(p,box(p,A)),B), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).

% inclusion

postulate(p(1,A,B), p(0,A,B), 'I1_0').
postulate(p(2,A,B), p(0,A,B), 'I2_0').
%postulate(p(3,A,B), p(0,A,B), 'I3_0').
%postulate(p(4,A,B), p(0,A,B), 'I4_0').

%postulate(p(3,A,B), p(4,B,A), 'P3_4').
% recursive

% odd postulates, to be replaced by more basic ones.7
% (goede *4 nota) *3 ( daar *2 van )
% 2-3-4
postulate(p(3,p(4,A,B),p(2,C,D)), p(0,p(0,C,p(0,A,B)),D), 'Mx').
% 2-2
postulate(p(0,p(0,A,p(2,B,C)),p(2,D,E)), p(0,A,p(0,p(0,p(0,B,D),C),E)), 'MC2_2a').
postulate(p(2,A,p(0,p(2,B,C),D)), p(0,B,p(0,A,p(0,C,D))), 'MC2_2b').
postulate(p(2,A,p(2,B,C)), p(0,B,p(0,A,C)), 'MC2_2c').
% 1-4
postulate(p(4,A,p(1,B,C)), p(1,B,p(0,A,C)), 'MC1_4a').
postulate(p(4,A,p(0,B,p(1,C,D))), p(0,B,p(1,C,p(0,A,D))), 'MC1_4b').
postulate(p(4,A,p(0,B,p(0,C,p(1,D,E)))), p(0,D,p(0,B,p(0,C,p(1,A,E)))), 'MC1_4c').
% 2-4
postulate(p(4,p(2,A,B),C), p(0,p(2,A,C),B), 'MC2_4a').
postulate(p(4,p(0,A,p(2,B,C)),D), p(0,A,p(2,p(0,B,D),C)), 'MC2_4b').
postulate(p(4,A,p(0,B,p(2,C,D))), p(0,B,p(2,p(0,A,C),D)), 'MC2_4c').
postulate(p(4,A,p(0,B,p(0,p(2,C,D),E))), p(0,B,p(0,p(2,p(0,C,A),D),E)), 'MC2_4d').
% 2-3
postulate(p(3,p(2,A,B),C), p(2,p(0,A,C),B), 'MC2_3a').
postulate(p(3,p(0,A,p(2,B,C)),D), p(0,A,p(2,p(0,B,D),C)), 'MC2_3b').
postulate(p(3,p(0,A,p(0,p(2,B,C),D)),E), p(0,A,p(0,p(2,p(0,B,E),C),D)), 'MC2_3c'). 
postulate(p(3,A,p(0,B,p(0,C,p(1,D,E)))), p(0,B,p(0,C,p(1,D,p(0,A,E)))), 'MC2_3d').
postulate(p(3,A,p(2,B,C)), p(2,p(0,B,A),C), 'MCA2_3a').
postulate(p(3,p(0,p(2,A,B),C),D), p(0,p(2,p(0,A,D),B),C), 'MC2_3d').
% 1-3
postulate(p(3,p(0,A,p(1,B,C)),D), p(0,A,p(1,B,p(0,D,C))), 'M1_3a').

% non-recursive
% 1-2
postulate(p(2,A,p(1,B,C)), p(0,B,p(0,A,C)), 'MC2_1a').
postulate(p(2,A,p(0,B,p(1,C,D))), p(0,B,p(0,C,p(0,A,D))), 'MC2_1b').

% 4-4
postulate(p(4,p(4,A,B),C), p(4,A,p(0,B,C)), 'MA4_4a').

% standard extraction postulates for a unary mode p

postulate(p(0,A,zip(p,B)), p(0,zip(p,B),A), 'Cp1').
postulate(p(0,A,p(0,zip(p,B),C)), p(0,zip(p,B),p(0,A,C)), 'MCp1').
postulate(p(0,p(0,zip(p,A),B),C), p(0,zip(p,A),p(0,B,C)), 'MAp1').

%

postulate(p(0,A,p(0,B,zip(a,C))), p(0,p(0,A,B),zip(a,C)), 'MAa1').

special_string(".", '.').
special_string(";", ';').
special_string(":", ':').
special_string(",", ',').
special_string("!", '!').
special_string("?", '?').

lex(dat, dr(0,lit(s(cp)),lit(s(sub))), lambda(X, X)).
lex('.', dl(0,lit(s(_)),lit(txt)), lambda(T,quant(exists,E,appl(T,E)))).

% = infinitives headed by "te"

default_semantics(te, dr(_,dl(0,lit(n(p)),lit(s(ti))),dl(0,lit(n(p)),lit(s(inf)))), lambda(X,X)).

% = sentence modifiers

default_semantics(W, dl(_,lit(s(S)),lit(s(S))), lambda(P,lambda(E,bool(appl(P,E),&,appl(W,E))))).
default_semantics(W, dr(_,lit(s(S)),lit(s(S))), lambda(P,lambda(E,bool(appl(P,E),&,appl(W,E))))).

% = adverbial prepositions

default_semantics(W, dr(0,dl(_,lit(s(S)),lit(s(S))),lit(n(N))), lambda(X,lambda(P,lambda(E,bool(appl(P,E),&,appl(appl(W,Object),E)))))) :-
        (
	    N == p
	->
	    Object = X
	;
	    Object = quant(iota,X)
	 ).
default_semantics(W, dr(0,dr(_,lit(s(S)),lit(s(S))),lit(n(N))), lambda(X,lambda(P,lambda(E,bool(appl(P,E),&,appl(appl(W,Object),E)))))) :-
        (
	    N == p
	->
	    Object = X
	;
	    Object = quant(iota,X)
	 ).

% = prepositions - noun modifiers

default_semantics(W, dr(0,dl(0,lit(n(X)),lit(n(Y))),lit(n(p))), lambda(X,lambda(P,lambda(Y,bool(appl(P,Y),&,appl(appl(W,Y),X)))))).

% = prepositions - arguments

default_semantics(_, dr(_,lit(pp(_)),lit(n(_))), lambda(X,X)).

%= verbs

% = ditransitive

default_semantics(W, dr(_,dr(_,dl(_,lit(n(S)),lit(s(_))),lit(n(IO))),lit(n(DO))), lambda(X, lambda(Y, lambda(Z, lambda(E,bool(bool(appl(W,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,DOSem),E),&,appl(appl(participant,IOSem),E)))))))) :-
	(
	    S == p
	->
	    Subj = Z
	;
	    Subj = quant(iota,Z)
        ),
	(
	    DO == p
	->
	    DOSem = X
	;
	    DOSem = quant(iota,X)
        ),
	(
	    IO == p
	->
	    IOSem = Y
	;
	    IOSem = quant(iota,Y)
        ).	

% = transitive

default_semantics(W, dr(_,dl(_,lit(n(S)),lit(s(_))),lit(n(O))), lambda(X,lambda(Y,lambda(E,bool(bool(appl(W,E),&,appl(appl(agent,Subj),E)),&,appl(appl(patient,Obj),E)))))) :-
	(
	    S == p
	->
	    Subj = Y
	;
	    Subj = quant(iota,Y)
        ),
	(
	    O == p
	->
	    Obj = X
	;
	    Obj = quant(iota,X)
        ).
% = transitive - verb final

default_semantics(W, dl(_,lit(n(O)),dl(_,lit(n(S)),lit(s(_)))), lambda(X,lambda(Y,lambda(E,bool(bool(appl(W,E),&,appl(appl(agent,Subj),E)),&,appl(appl(patient,Obj),E)))))) :-
	(
	    S == p
	->
	    Subj = Y
	;
	    Subj = quant(iota,Y)
        ),
	(
	    O == p
	->
	    Obj = X
	;
	    Obj = quant(iota,X)
        ).

% = transitive - sentential complement

default_semantics(W, dr(_,dl(_,lit(n(S)),lit(s(_))),lit(s(_))), lambda(P,lambda(Y,lambda(E,bool(bool(appl(W,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(P,F))))))) :-
	(
	    S == p
	->
	    Subj = Y
	;
	    Subj = quant(iota,Y)
        ).

% = transitive + pp

default_semantics(houden, dr(_,dl(_,lit(n(S)),lit(s(_))),lit(pp(van))), lambda(X,lambda(Y,lambda(E,bool(bool(appl(houden_van,E),&,appl(appl(agent,Subj),E)),&,appl(appl(patient,X),E)))))) :-
	(
	    S == p
	->
	    Subj = Y
	;
	    Subj = quant(iota,Y)
        ).

% = transitive + prep + np

default_semantics(sluiten, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(uit)))),lit(n(p))), lambda(X,lambda(_,lambda(Y,lambda(E,bool(bool(appl(uitsluiten,E),&,appl(appl(agent,Y),E)),&,appl(appl(patient,X),E))))))).


default_semantics(geven, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(op)))),lit(n(p))), lambda(X,lambda(_,lambda(Y,lambda(E,bool(bool(appl(opgeven,E),&,appl(appl(agent,Y),E)),&,appl(appl(patient,X),E))))))).

default_semantics(geven, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(uit)))),lit(n(p))), lambda(X,lambda(_,lambda(Y,lambda(E,bool(bool(appl(uitgeven,E),&,appl(appl(agent,Y),E)),&,appl(appl(patient,X),E))))))).

default_semantics(geven, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(door)))),lit(n(p))), lambda(X,lambda(_,lambda(Y,lambda(E,bool(bool(appl(doorgeven,E),&,appl(appl(agent,Y),E)),&,appl(appl(patient,X),E))))))).

default_semantics(geven, dr(_,dr(_,dl(_,lit(n(p)),lit(s(_))),dl(_,lit(n(p)),lit(pp(op)))),lit(n(p))), lambda(X,lambda(_,lambda(Y,lambda(E,bool(bool(appl(opgeven,E),&,appl(appl(agent,Y),E)),&,appl(appl(patient,X),E))))))).

% = worden + ppart

default_semantics(worden, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(appl(P,_),E),&,appl(appl(patient,X),E)),&,appl(finished,E)))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = hebben + ppart

default_semantics(hebben, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(appl(appl(P,Subj),E),&,appl(finished,E)))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = zijn + ppart

default_semantics(is, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(ppart)))), lambda(P,lambda(X,lambda(E,bool(appl(appl(P,Subj),E),&,appl(finished,E)))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = willen + inf

default_semantics(willen, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,Subj),F))))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = willen + inf

default_semantics(zullen, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,Subj),F))))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = kunnen + inf

default_semantics(kunnen, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,Subj),F))))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = hopen + ti

default_semantics(willen, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(ti)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,Subj),F))))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = verwachten + ti

default_semantics(willen, dr(_,dl(_,lit(n(S)),lit(s(_))),dl(_,lit(n(p)),lit(s(ti)))), lambda(P,lambda(X,lambda(E,bool(bool(appl(willen,E),&,appl(appl(agent,Subj),E)),&,bool(appl(appl(patient,F),E),&,appl(appl(P,Subj),F))))))) :-
	(
	    S == p
	->
	    Subj = X
	;
	    Subj = quant(iota,X)
        ).

% = intransitive

default_semantics(W, dl(_,lit(n(S)),lit(s(_))), lambda(Y,lambda(E,bool(appl(W,E),&,appl(appl(agent,Subj),E))))) :-
	(
	    S == p
	->
	    Subj = Y
	;
	    Subj = quant(iota,Y)
        ).

% = adjective

default_semantics(W, dr(_,lit(n(N1)),lit(n(N2))), lambda(P,lambda(X,bool(appl(P,X),&,appl(W,X))))) :-
	    var(N1),
	    var(N2),
	    N1 == N2.

% = pronouns - anaphora

default_semantics(dit, lit(n(p)), _).
default_semantics(dat, lit(n(p)), _).
default_semantics(het, lit(n(p)), _).
default_semantics(hem, lit(n(p)), _).
default_semantics(hij, lit(n(p)), _).
default_semantics(haar, lit(n(p)), _).
default_semantics(zij, lit(n(p)), _).
default_semantics(ze, lit(n(p)), _).

% = pronouns - first person

default_semantics(ik, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).
default_semantics(me, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).
default_semantics(mij, lit(n(p)), quant(iota,lambda(X,appl(speaker,X)))).

% = root_form

root_form(slaapt, slapen).
root_form(slaap, slapen).

root_form(hield, houden).
root_form(hielden, houden).
root_form(houdt, houden).
root_form(houd, houden).
root_form(hou, houden).
root_form(gehouden, houden).

root_form(heb, hebben).
root_form(had, hebben).
root_form(hadden, hebben).
root_form(hebt, hebben).
root_form(heeft, hebben).

root_form(geweest, is).
root_form(zijn, is).
root_form(ben, is).
root_form(bent, is).

root_form(wil, willen).
root_form(wilt, willen).
root_form(wilde, willen).
root_form(wilden, willen).
root_form(gewild, willen).

root_form(sluit, sluiten).

root_form(geef, geven).
root_form(geeft, geven).
root_form(gaf, geven).
root_form(gaven, geven).

root_form(word, worden).
root_form(wordt, worden).
root_form(werd, worden).
root_form(werden, worden).
root_form(geworden, worden).

root_form(hoop, hopen).
root_form(hoopt, hopen).
root_form(hoopte, hopen).
root_form(hoopten, hopen).

root_form(verwacht, verwachten).
root_form(verwachtte, verwachten).
root_form(verwachtten, verwachten).

root_form(kan, kunnen).
root_form(kon, kunnen).
root_form(kun, kunnen).

root_form(zal, zullen).
root_form(zul, zullen).
root_form(zou, zullen).
root_form(zouden, zullen).

root_form(aangehouden, aanhouden).

root_form('De', de).
root_form('Een', een).
