% -*- Mode: Prolog -*-
% ============================================================
% tag.pl
% ============================================================
% !grail 3.1.1 

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

% ======================================
% =      Optimization Parameters       =
% ======================================
% = External modes

:- dynamic external/1.

external(0).

% = Inert modes

:- dynamic inert/1.

inert(0).

% = Continous modes

:- dynamic continuous/1.

continuous(0).

% = Custom first-order translations

:- dynamic custom_first_order/4.

custom_first_order(dl(0,lit(vp1),p(1,dr(1,dl(0,lit(np),lit(vp)),lit(vp)),lit(vp1))), dl(0,A,p(1,dr(1,dl(0,B,C),D),E)), P, [X,Y]) :-
	!,
	flip(P, Q),
	add_first_order(lit(vp1), A, Q, [W,X]),
	add_first_order(lit(np), B, Q, [V,W1]),
	add_first_order(lit(vp), C, P, [V,W,X,Z]),
	add_first_order(lit(vp), D, Q, [W1,W,Y,Z]),
	add_first_order(lit(vp1), E, P, [W,Y]).
custom_first_order(p(1,dr(1,A0,B0),C0), p(1,dr(1,A,B),C), P, [X, Y]) :-
	!,
	flip(P, Q),
%	gensym_neg(Q, V),
%	gensym_neg(Q, W),
	add_first_order(A0, A, P, [V,W]),
	add_first_order(B0, B, Q, [V,X,Y,W]),
	add_first_order(C0, C, P, [X,Y]).
custom_first_order(dl(0,lit(vp1),dl(0,lit(np),lit(vp))), dl(0,A,dl(0,B,C)), P, [X, Y]) :-
	!,
	flip(P, Q),
%	gensym_neg(Q, V),
%	gensym_neg(Q, X),
	add_first_order(lit(vp1), A, Q, [W,X]),
	add_first_order(lit(np), B, Q, [V,W]),
	add_first_order(lit(vp), C, P, [V,W,X,Y]).

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(p(1,X,p(0,Y,Z)), p(0,p(1,X,Y),Z), 'MA').
postulate(p(1,X,p(0,Y,Z)), p(0,Y,p(1,X,Z)), 'MC').

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(ik, np, i).
lex(henk, np, henk).
lex(cecilia, np, cecilia).
lex(de, dr(0,np,n), lambda(P,quant(iota,X,appl(P,X)))).
lex(nijlpaarden, n, hippos).
lex(zag, p(1,dr(1,dl(0,np,dl(0,np,s)),vp),vp1), pair(see,lambda(X,X))).
lex(helpen, dl(0,vp1,p(1,dr(1,dl(0,np,vp),vp),vp1)), pair(help,lambda(X,X))).
lex(voeren, dl(0,vp1,dl(0,np,vp)), feed).
lex(sliep, p(1,dr(1,dl(0,np,s),vp),vp), slapen).

% vp*(vp\s) |- s
% (s/vp)*vp |- s

lex(sliep, dl(0,np,p(1,dr(1,s,vp),vp)), lambda(X,pair(lambda(V,appl(appl(V,sleep),X)),lambda(Z,Z)))).
lex(rustig, dl(0,vp,vp), lambda(_,quietly)).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("Ik sliep.", s).
example("Ik sliep rustig.", s).
example("ik Cecilia de nijlpaarden zag voeren.", s).
example("ik Henk Cecilia de nijlpaarden zag helpen voeren.", s).