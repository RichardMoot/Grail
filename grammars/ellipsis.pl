% -*- Mode: Prolog -*-
% ============================================================
% ellipsis.pl
% ============================================================
% !grail 3.1.1

:- abolish(continuous_dia/1).
:- abolish(continuous/1).
:- abolish(external_dia/1).
:- abolish(external/1).
:- abolish(postulate/3).
:- abolish(postulate1/3).
:- abolish(macro/2).
:- abolish(lex/3).
:- abolish(example/2).

:- dynamic continuous_dia/1, continuous/1.
:- dynamic external_dia/1, external/1.
:- dynamic inert_dia/1, inert/1.
:- dynamic postulate/3.
:- dynamic macro/2, lex/3, example/2.

% = non internal modes


atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

continuous(0).
continuous_dia(0).

external(0).
external(l).
external(r).
%external(1).

external_dia(0).
external_dia(l).
external_dia(r).

%postulate(p(0,p(0,A,zip(0,B)),C), p(0,p(l,A,C),zip(0,B)), 'MC').
%postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(r,A,B),zip(0,C)), 'MA').
postulate(p(0,zip(0,A),B), p(1,A,zip(l,B)), 'Start_l').
postulate(p(0,A,zip(0,B)), p(1,B,zip(r,A)), 'Start_r').
postulate(p(2,A,zip(l,B)), p(0,zip(0,A),B), 'End_l').
postulate(p(2,B,zip(r,A)), p(0,A,zip(0,B)), 'End_r').

postulate(p(0,A,p(1,B,C)), p(1,B,p(r,A,C)), 'MC').
postulate(p(0,p(1,A,B),C), p(1,A,p(l,B,C)), 'MA').

postulate(p(2,B,p(r,A,C)), p(0,A,p(2,B,C)), 'MC').
postulate(p(2,A,p(l,B,C)), p(0,p(2,A,B),C), 'MA').



% custom_first_order(dl(l,dl(1,A0,B0),dl(2,C0,D0)),
% 		   dl(l,dl(1,A1,B1),dl(2,C1,D1)), P, [Y,Z]) :-
% 	!,
% 	flip(P, Q),
% 	add_first_order(A0, A1, P, [V,W]),
% 	add_first_order(B0, B1, Q, [X,Y]),
% 	add_first_order(C0, C1, Q, [V,W]),
% 	add_first_order(D0, D1, P, [X,Z]).

% % = extraction/infixation

% custom_first_order(dr(2,A0,dl(1,C0,B0)), dr(2,A1,dl(1,C1,B1)), P, [X,Y]) :-
% 	!,
% 	flip(P, Q),
% 	gensym_pos(P, V),
% 	gensym_pos(P, W),
% 	add_first_order(A0, A1, P, [V,W]),
% 	add_first_order(B0, B1, Q, [V,W]),
% 	add_first_order(C0, C1, P, [X,Y]).

% % = extraction

% custom_first_order(dl(1,box(0,A0),B0), dl(1,box(0,A1),B1), P, [X,Y]) :-
% 	!,
% 	flip(P, Q),
% 	gensym_pos(Q, Z),
% 	add_first_order(A0, A1, Q, [Z,Z]),
% 	add_first_order(B0, B1, P, [X,Y]).	
	
macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(s_vp, dl(1,box(0,tv),s)).
macro(s_iv, dl(1,box(0,iv),s)).
macro(s_gq, dl(1,box(0,gq),s)).
macro(vpu, dl(1,iv,iv)).
macro(vp1, iv).
macro(q, dia(0,dr(2,s,dl(1,box(0,np),s)))).
macro(gq, dr(0,q,n)).

lex(someone, q, lambda(P,quant(exists,X,bool(appl(person,X),&,appl(P,X))))).
lex(something, q, lambda(P,quant(exists,X,bool(appl(item,X),&,appl(P,X))))).
lex(everyone, q, lambda(P,quant(forall,X,bool(appl(person,X),->,appl(P,X))))).
lex(everything, q, lambda(P,quant(forall,X,bool(appl(item,X),->,appl(P,X))))).

lex(a, gq, lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(every, gq, lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(most, gq, lambda(P,lambda(Q,appl(appl(sub(most,X),appl(P,X)),appl(Q,X))))).

lex(who, dr(0,dl(0,n,n),dl(1,box(0,np),s)), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X)))))).

lex(bagels, n, bagels).
lex(donuts, n, donuts).
lex(man, n, man).
lex(boys, n, boys).
lex(girls, n, girls).
lex(beans, n, beans).
lex(book, n, book).
lex(record, n, record).
%lex(representative, dr(0,n,pp), representative_of).
lex(representative, n, representative).
lex(company, n, company).
lex(samples, n, samples).

lex(the, dr(0,np,n), lambda(P,quant(iota,X,appl(P,X)))).

lex(john, np, john).
lex(mary, np, mary).
lex(pat, np, pat).
lex(charles, np, charles).
lex(kim, np, kim).
lex(logic, np, logic).
lex(phonetics, np, phonetics).
lex(golf, np, golf).

lex(studies, tv, study).
lex(ate, tv, eat).
lex(tested, tv, test).
lex(bought, tv, buy).
lex(liked, tv, like).
lex(cooked, tv, cook).
lex(played, tv, play).

%lex(of, dr(0,pp,np), lambda(X,X)).
%lex(of, dr(0,dl(0,n,n),np), lambda(X,lamnda(P,lambda(Y,bool(appl(P,Y),&,appl(appl(of,X),Y)))))).
lex(of, dr(0,dl(0,n,n),dr(0,s,dl(0,np,s))), lambda(R,lambda(P,lambda(Y,appl(R,lambda(X,bool(appl(P,Y),&,appl(appl(of,X),Y)))))))).

lex(slept, iv, sleep).

lex(gave, dr(0,tv,np), give).

lex(and, dr(0,dl(l,s_vp,dl(2,tv,s)),s_vp), lambda(A,lambda(B,lambda(R,bool(appl(B,R),&,appl(A,R)))))).

lex(did, iv, did).

%lex(did, dl(r,dl(l,vpu,dl(2,vp1,s)),vpu), lambda(X,lambda(Y, appl(appl(X,Y),Y)))).
lex(before, dr(0,dl(0,s,s),s), before).
lex(too, dl(0,s,s), lambda(X,X)).

lex(than, dr(0,dl(l,s_gq,dl(2,box(t,gq),s)),s_gq), lambda(BQ,lambda(BP,lambda(D3,appl(appl(D3,lambda(Y,appl(BQ,lambda(P1,lambda(Q1,bool(appl(P1,Y),&,appl(Q1,Y))))))),lambda(X,appl(BP,lambda(P,lambda(Q,bool(appl(P,X),&,appl(Q,X))))))))))).
lex(more, box(t,gq), more_than).
lex(less, box(t,gq), less_than).
lex(fewer, box(t,gq), fewer_than).

example(" Everyone studies something.", s).
example(" Every representative of a company tested most samples.", s).
example(" John studies logic and Charles phonetics.", s).
example(" Pat ate more bagels than Kim bought donuts.", s).
example(" More boys cooked than girls ate the beans.", s).
example(" Mary gave more girls a book than boys a record.", s).
example(" John played golf before Mary did.", s).
example(" John played golf and Mary did too.", s).
