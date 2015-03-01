% -*- Mode: Prolog -*-
% ============================================================
% semantics.pl
% ============================================================
% !grail 3.1.1 

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(pp, (e->t)->(e->t)).

inert(0).

continuous(0).
continuous(a).

external(0).
external(a).

% ============================================================
% Postulates
% ============================================================

% = structural postulates

postulate(p(a,p(a,A,B),C),p(a,A,p(a,B,C)),'Ass'). % ass1
postulate(p(a,A,p(a,B,C)),p(a,p(a,A,B),C),'Ass'). % ass2

% ============================================================
% = Macros
% ============================================================

:- dynamic macro/2.

macro(iv, dl(a, np, s)).
macro(tv, dr(a, iv, np)).
macro(dtv, dr(0, tv, np)).
macro(prep, dr(a,pp,np)).
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

% = lex(Pros,Formula,Sem)

lex(kid, n, kid).
lex(kids, n, kids).
lex(book, n, book).
lex(magazine, n, magazine).
lex(movie, n, movie).
lex(box, n, box).
lex(man, n, man).
lex(woman, n, woman).
lex(house, n, house).
lex(notes, n, notes).
lex(toy, n, toy).
lex(dog, n, dog).
lex(class, n, class).
lex(adult, n, adult).

lex(white, adj, lambda(P,lambda(X,bool(appl(P,X),&,appl(white,X))))).
lex(tall, adj, lambda(P,lambda(X,appl(tall,appl(P,X))))).

lex(outside, dl(0,n,n), lambda(P,lambda(X,bool(appl(P,X),&,appl(outside,X))))).
lex(inside, dl(0,n,n), lambda(P,lambda(X,bool(appl(P,X),&,appl(inside,X))))).

lex(jo, np, j).
lex(kelly, np, kelly).
lex(leslie, np, leslie).
lex(brett, np, brett).
lex(francis, np, francis).
lex(felix, np, felix).

lex(barks, iv, bark).
lex(ran, iv, run).
lex(jumped, iv, jump).
lex(fell, iv, fall).
lex(ate, iv, eat).
lex(played, iv, play).
lex(studied, iv, study).

% P : (e->t)->(e->t) Y:e->t X:e
lex(is, dr(0,iv,adj), lambda(P,lambda(X,appl(appl(P,lambda(_,true)),X)))).
lex(is, tv, lambda(P, lambda(Q, bool(Q,=,P)))).


lex(hit, tv, hit).
lex(loves, tv, love).
lex(hated, tv, hate).
lex(read, tv, read).
lex(breaks, tv, break).
lex(broke, tv, broke).
lex(likes, tv, like).

lex(sold, dtv, sell).
lex(gave, dtv, give).

% X:e P:e->t Z:e T:e->t
lex(in, prep_n, lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(in,X),Z)))))).
lex(on, prep_n, lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(on,X),Z)))))).
lex(with, dr(a,dl(a,iv,iv),np), lambda(X,lambda(P,lambda(Z,bool(appl(P,Z),&,appl(appl(with,X),Z)))))).

% P:(e->t)->(e->t)->t Q:(e->t)->(e->t)->t R:e->t S:e->t
lex(butnot, conj(det_subj), lambda(P, lambda(Q, lambda(R, lambda(S, bool(appl(appl(Q,R),S),&,not(appl(appl(P,R),S)))))))).
lex(and, conj(n), lambda(P, lambda(Q, lambda(X,bool(appl(Q,X),&,appl(P,X)))))).
lex(and, conj(s), lambda(P, lambda(Q,bool(Q,&,P)))).
lex(and, conj(iv), lambda(P,lambda(Q,lambda(X,bool(appl(Q,X),&,appl(P,X)))))).
lex(and, conj(gq_obj), lambda(P, lambda(Q, lambda(R, bool(appl(Q,R),&,appl(P,R)))))).

lex(or, conj(tv), lambda(P,lambda(Q,lambda(X,lambda(Y,bool(appl(appl(Q,X),Y),\/,appl(appl(P,X),Y))))))).
lex(or, conj(n), lambda(P,lambda(Q,lambda(X,bool(appl(Q,X),\/,appl(P,X)))))).
lex(or, conj(prep_n), lambda(P,lambda(Q,lambda(X,lambda(Y,lambda(Z,bool(appl(appl(appl(Q,X),Y),Z),\/,appl(appl(appl(P,X),Y),Z)))))))).
% P:(e->t)->(e->t) Q:(e->t)->(e->t) R:e->t Z:e
lex(or, conj(npostm), lambda(P,lambda(Q,lambda(R,lambda(Z,bool(appl(appl(Q,R),Z),\/,appl(appl(P,R),Z))))))).
lex(or, conj(dtv), lambda(P,lambda(Q,lambda(X,lambda(Y,lambda(Z,bool(appl(appl(appl(Q,X),Y),Z),\/,appl(appl(appl(P,X),Y),Z)))))))).
% P:(e->t)->(e->t)->t Q:(e->t)->(e->t)->t R:e->t Z:e
lex(or, conj(gq_subj), lambda(P, lambda(Q, lambda(R, bool(appl(Q,R),\/,appl(P,R)))))).

lex(the, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).

lex(every, det_obj, lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(every, det_subj, lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(all, det_subj, lambda(P,lambda(Q,quant(forall,X,bool(appl(P,X),->,appl(Q,X)))))).
lex(some, det_subj, lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(some, det_obj, lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(a, det_subj, lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).
lex(a, det_obj, lambda(P,lambda(Q,quant(exists,X,bool(appl(P,X),&,appl(Q,X)))))).

lex(someone, gq_subj, lambda(P, quant(exists,X,appl(P,X)))).
lex(someone, gq_obj, lambda(P, quant(exists,X,appl(P,X)))).
lex(everyone, gq_subj, lambda(P, quant(forall,X,appl(P,X)))).
lex(everyone, gq_obj, lambda(P, quant(forall,X,appl(P,X)))).
lex(something, gq_subj, lambda(P, quant(exists,X,appl(P,X)))).
lex(something, gq_obj, lambda(P, quant(exists,X,appl(P,X)))).
lex(everything, gq_subj, lambda(P, quant(forall,X,appl(P,X)))).
lex(everything, gq_obj, lambda(P, quant(forall,X,appl(P,X)))).


special_string("but not", butnot).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("--- Coordination ---", x).
example(" Jo ran and Jo jumped.", s).
example(" Jo ran and jumped.", s).
example(" Jo hit Kelly and ran.", s).
example(" Every kid loves or hated the movie.", s).
example(" Jo read the book or magazine.", s).
example(" The kid in or on the box fell.", s).
example(" The kid in the house or outside ran.", s).
example(" Leslie sold or gave Jo the notes.", s).
example("--- Generalized Quantifiers ---", x).
example(" Every white dog barks.", s).
example(" Someone breaks everything.", s).
example(" Some kid broke every toy.", s).
example(" Every kid played with some toy.", s).
example("--- Coordination and Generalized Quantifiers ---", x).
example(" Jo or some tall kid ran.", s).
example(" Every kid or adult ran.", s).
example(" Some but not all kids ran.", s).
example(" Francis likes Felix and every dog.", s).
example(" Every kid in some class studied.", s).
% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, drs([], [appl(K, L), appl(tall, L)])))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [lambda(O, drs([], [appl(human, J), appl(male, J)])), appl(tall, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, merge(drs([], [appl(tall, L)]), appl(appl(K, L), M))))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [appl(tall, J), appl(human, J), appl(male, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, merge(drs([], [appl(tall, L)]), appl(K, L))))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

merge(merge(drs([J], []), merge(drs([], [appl(tall, J)]), lambda(O, drs([], [appl(human, J), appl(male, J)])))), drs([B], [appl(event, B), appl(appl(enter, J), B)])).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, merge(drs([], [appl(tall, L)]), appl(K, L)))), lambda(M, lambda(N, drs([], [appl(human, M), appl(male, M)]))))))).

% = Reduced Semantics

merge(merge(drs([J], []), appl(merge(drs([], [appl(tall, J)]), lambda(N, drs([], [appl(human, J), appl(male, J)]))), B)), drs([B], [appl(event, B), appl(appl(enter, J), B)])).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, merge(drs([], [appl(tall, L)]), appl(appl(K, L), M))))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [appl(tall, J), appl(human, J), appl(male, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, appl(appl(K, L), M)))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [appl(human, J), appl(male, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, appl(appl(K, L), M)))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [appl(human, J), appl(male, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), appl(lambda(G, lambda(H, lambda(I, merge(merge(drs([J], []), appl(appl(G, J), I)), appl(appl(H, J), I))))), appl(lambda(K, lambda(L, lambda(M, appl(appl(K, L), M)))), lambda(N, lambda(O, drs([], [appl(human, N), appl(male, N)]))))))).

% = Reduced Semantics

drs([B, J], [appl(human, J), appl(male, J), appl(event, B), appl(appl(enter, J), B)]).

% = Solution 1 =

% = Semantics

 appl(lambda(A, appl(A, B)), appl(lambda(C, lambda(D, appl(appl(C, lambda(E, lambda(F, drs([F], [appl(event, F), appl(appl(enter, E), F)])))), D))), lambda(G, lambda(H, merge(drs([mary], [appl(female, mary)]), appl(appl(G, mary), H)))))).

% = Reduced Semantics

drs([mary, B], [appl(female, mary), appl(event, B), appl(appl(enter, mary), B)]).

