% ============================================================
% Discontinuity
% ============================================================
% !labels v1.0

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
% Structural Rules
% ============================================================

postulate(p(0,p(1,A,zip(1,B)),C), p(1,p(l,A,C),zip(1,B)), 'MC').
postulate(p(0,A,p(1,B,zip(1,C))), p(1,p(r,A,B),zip(1,C)), 'MA').
postulate(p(1,p(r,A,B),C), p(0,A,p(1,B,C)), 'MA').
postulate(p(1,p(l,A,B),C), p(0,p(1,A,C),B), 'MC').
postulate(p(1,zip(l,A),B), p(0,A,B), 'Il').
postulate(p(1,zip(r,A),B), p(0,B,A), 'Ir').
%postulate(p(1,A,zip(2,B)), p(0,A,B), 'Ir').
postulate(p(0,A,zip(1,B)), p(1,zip(l,A),zip(1,B)), 'I').
postulate(p(0,zip(1,A),B), p(1,zip(r,B),zip(1,A)), 'I').
% nd search
postulate(p(1,p(0,A,B),C), p(0,p(1,A,C),B), 'MC').
postulate(p(1,p(0,A,B),C), p(0,A,p(1,B,C)), 'MA').
postulate(p(1,zip(rm,A),B), p(0,B,A), 'rm').

% = continuity

continuous(0).

continuous_dia(1).

% = non internal modes

external(0).

external_dia(_).

% ============================================================
% Macros
% ============================================================

macro(vp, iv).
macro(iv,dl(0,n,s)).
macro(tv,dr(0,iv,n)).
macro(gq, dl(1,dr(1,s,dia(1,box(1,n))),s)).
macro(vpu, box(r,dr(1,iv,iv))).
macro(stv, dr(1,s,tv)).
macro(det, dr(0,gq,cn)).
macro(adv, dl(0,iv,iv)).
macro(relpro, dr(0,dl(0,cn,cn),relbody)).
macro(relbody, box(u,dr(1,s,n))).

custom_first_order(box(u,A0), box(u,A), P, [X,Y]) :-
	!,
	gensym_pos(P, Z),
	add_first_order(A0, A, P, [X,Z,Z,Y]).
custom_first_order(dia(u,A0), dia(u,A), P, [X,Y]) :-
	!,
	gensym_pos(P, Z),
	add_first_order(A0, A, P, [X,Z,Z,Y]).
custom_first_order(box(r,A0), box(u,A), P, [X,Y]) :-
	!,
%	gensym_pos(P, V),
%	gensym_pos(P, W),
	add_first_order(A0, A, P, [X,V,W,Y]).
custom_first_order(dia(r,A0), dia(u,A), P, [X,Y]) :-
	!,
%	gensym_pos(P, Z),
	add_first_order(A0, A, P, [X,V,W,Y]).
custom_first_order(box(rm,A0), box(u,A), P, [X,Y]) :-
	!,
%	gensym_pos(P, V),
%	gensym_pos(P, W),
	add_first_order(A0, A, P, [Z,Z,X,Y]).
custom_first_order(dia(rm,A0), dia(u,A), P, [X,Y]) :-
	!,
%	gensym_pos(P, Z),
	add_first_order(A0, A, P, [Z,Z,X,Y]).
custom_first_order(dl(0,dr(0,dr(1,VPL3,VPL2),VPL1),dr(1,VPL5,VPL4)), dl(0,dr(0,dr(1,VP3,VP2),VP1),dr(1,VP5,VP4)), P, [I,J]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, X3),
	gensym_pos(P, X1),
	gensym_pos(P, X2),
	add_first_order(VPL1, VP1, P, [I,X3]),
	add_first_order(VPL2, VP2, P, [X1,X2]),
	add_first_order(VPL3, VP3, Q, [X0,X3]),
	add_first_order(VPL4, VP4, Q, [X1,X2]),
	add_first_order(VPL5, VP5, P, [X0,J]).
custom_first_order(dl(1,A0,B0), dl(1,A,B), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, V),
	gensym_pos(Q, W),
	add_first_order(A0, A, Q, [X,V,W,Y]),
	add_first_order(B0, B, P, [V,W]).
custom_first_order(dr(1,A0,B0), dr(1,A,B), P, [X,V,W,Y]) :-
	!,
	flip(P,Q),
	add_first_order(A0, A, P, [X,Y]),
	add_first_order(B0, B, Q, [V,W]).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(john, n, j).
lex(jan, n, j).
lex(boeken, n, books).
lex(mary, n, m).
lex(charles, n, c).
lex(cecilia, n, c).
lex(henk, n, h).
lex(cezanne, n, cezanne).
lex(golf, n, golf).
lex(logic, n, logic).
lex(phonetics, n, phonetics).
lex(ten_mil, n, ten_mil).
lex(boy, cn, boy).
lex(book, cn, book).
lex(girl, cn, girl).
lex(dog, cn, dog).
lex(scene, cn, scene).
lex(painting, cn, painting).
lex(nijlpaarden, cn, hippos).
lex(jogs, iv, jog).
lex(sneezes, iv, sneeze).
lex(gave, dr(0,dr(0,iv,pp),n), give).
lex(played, tv, play).
lex(loves, tv, love).
lex(studies, tv, study).
lex(saw, tv, see).
lex(sold, dr(0,iv,p(0,n,pp)), sell).
lex(to, dr(0,pp,n), lambda(X, X)).
lex(for, dr(0,pp,n), lambda(X, X)).
lex(of, dr(0,dl(0,cn,cn),n), of).
lex(by, dr(0,dl(0,n,n),n), by).
lex(today, adv, today).
lex(too, dl(0,s,s), too).
lex(before, dr(0,dl(0,dl(0,n,s),dl(0,n,s)),s), before).
lex(did, dl(0,dr(0,dr(1,vp,vp),vp),dr(1,vp,vp)), lambda(X,lambda(Y, appl(appl(X,Y),Y)))).
lex(that, relpro, lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z)))))). 
lex(which, dl(1,dr(1,n,n),relpro), lambda(X,lambda(Y,lambda(Z,lambda(W,bool(appl(Z,W),&,appl(Y,appl(X,W)))))))). 
lex(who, dr(0,dl(0,n,gq),relbody), lambda(X,lambda(Y,lambda(Z,bool(appl(X,Z),&,appl(Y,Z)))))).
lex(a, det,
	lambda(X,lambda(Y,quant(exists,Z,bool(appl(X,Z),&,appl(Y,Z)))))
	).
lex(every,det,
	lambda(X,lambda(Y,quant(forall,Z,bool(appl(X,Z),->,appl(Y,Z)))))
	).
lex(the,dr(0,n,cn),lambda(P,quant(iota,X,appl(P,X))) ).
lex(someone, gq, lambda(P,quant(exists,X,appl(P,X))) ).
lex(everyone,gq, lambda(P,quant(forall,X,appl(P,X))) ).
lex(left, iv, leave).
lex(thinks, dr(0,iv,s), think).
lex(and, dr(0,dl(0,dia(r,stv),box(r,stv)),dia(u,stv)), lambda(X,lambda(Y,lambda(Z,lambda(W,bool(appl(Z,W),&,appl(Y,appl(X,W)))))))).
lex(and,dr(0,dl(0,s,s),s),lambda(P,lambda(Q,bool(P,&,Q))) ).
lex(dat, dr(0,cp,s)).
lex(de,dr(0,n,cn),lambda(P,quant(iota,X,appl(P,X))) ).
lex(las, dl(0,n,dl(0,n,s)), read).
lex(lezen, box(rm,dl(0,n,dl(0,n,si))), read).
lex(voeren, box(rm,dl(0,n,dl(0,n,si))), feed).
lex(zag, dl(1,dl(0,n,si),dl(0,n,dl(0,n,s))), see).
lex(kan, dl(1,dl(0,n,si),dl(0,n,s)), can).
lex(kunnen, box(rm,dl(1,dl(0,n,si),dl(0,n,si))), can).
lex(helpen, box(rm,dl(1,dl(0,n,si),dl(0,n,dl(0,n,si)))), help).
lex(wil, dl(1,dl(0,n,si),dl(0,n,s)), want).
lex(wil, dr(0,q,dia(u,dr(1,s,dl(1,dl(0,n,si),dl(0,n,s))))), want).
lex(dat, dr(0,cp,s), that).

special_string("$10,000,000", ten_mil).

% ============================================================
% Examples
% ============================================================

example(" --- Quantification ---", x).
example(" John gave every book to Mary.", s).
example(" Mary thinks someone left.", s).
example(" Everyone loves someone.", s).
example(" --- VP Ellipsis ---", x).
example(" John played golf before Mary did.", s).
example(" John played golf and Mary did too.", s).
example(" --- Medial extraction ---", x).
example(" dog that Mary saw today.", cn).
example(" --- Pied piping ---", x).
example(" scene the painting of which by Cezanne John sold for $10,000,000.", cn).
example(" --- Appositive relativistion ---", x).
example(" John, who jogs, sneezes.", s).
example(" --- Gapping ---", x).
example(" John studies logic and Charles phonetics.", s).
example(" --- Dutch word order ---", x).
example(" dat Jan boeken las.", cp).
example(" dat Jan boeken kan lezen.", cp).
example(" dat Jan boeken wil kunnen lezen.", cp).
example(" dat Jan Cecilia Henk de nijlpaarden zag helpen voeren.", cp).
example(" Wil Jan boeken lezen?", q).
example(" Jan wil boeken lezen.", p(0, n, dia(u,dr(1,q,n)))).