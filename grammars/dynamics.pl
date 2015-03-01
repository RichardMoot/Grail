% -*- Mode: Prolog -*-
% ============================================================
% dynamics.pl
% ============================================================
% !grail 3.1.1

:- encoding(iso_latin_1).

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
% Semantic Types
% ============================================================

atomic_type(n, e->s->(s->t)->t).
atomic_type(np, (e->s->(s->t)->t)->s->(s->t)->t).
atomic_type(s, s->(s->t)->t).
atomic_type(txt, s->(s->t)->t).

% ============================================================
% = Optimization Parameters                                  =
% ============================================================
% = External modes

:- dynamic external/1.

external(0).

:- dynamic external_dia/1.

% = Continous modes

:- dynamic continuous/1.

continuous(0).

% ============================================================
% = Tokenization                                             =
% ============================================================

special_string(".", '.').

% ============================================================
% Lexicon                                                    =
% ============================================================

% = lex(Pros,Formula,Sem)

lex('.', dl(0,s,txt), lambda(X,X)).
% S  : g->(g->o)->o
% D  : g->(g->o)->o
% E,F: g
% Phi: g->o
lex('.', dl(0,s,dl(0,txt,txt)), lambda(S,lambda(D,lambda(E,lambda(Phi,appl(appl(D,E),lambda(F,appl(appl(S,F),Phi)))))))).
%atomic_type(n, i->g->(g->o)->o).
% X  : i
% E  : g
% Phi: g->o
lex(man, n, lambda(X,lambda(E,lambda(Phi,bool(appl(man,X),&,appl(Phi,E)))))).
lex(woman, n, lambda(X,lambda(E,lambda(Phi,bool(appl(woman,X),&,appl(Phi,E)))))).
lex(farmer, n, lambda(X,lambda(E,lambda(Phi,bool(appl(farmer,X),&,appl(Phi,E)))))).
lex(donkey, n, lambda(X,lambda(E,lambda(Phi,bool(appl(donkey,X),&,appl(Phi,E)))))).
% Psi:    i->g->(g->o)->o
% E:      g
% F:      g
% Phi:    g->o
% update: i->g->g
% sel:    g->i
lex(mary, np, lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,mary),E),lambda(F,appl(Phi,update(mary,F)))))))).
lex(john, np, lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,john),E),lambda(F,appl(Phi,update(john,F)))))))).
lex(he, np, lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel(he,E)),E),Phi))))).
lex(she, np, lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel(she,E)),E),Phi))))).
lex(it, np, lambda(Psi,lambda(E,lambda(Phi,appl(appl(appl(Psi,sel(it,E)),E),Phi))))).
lex(smiles, dl(0,np,s), lambda(S,appl(S,lambda(X,lambda(E,lambda(Phi,bool(appl(smile,X),&,appl(Phi,E)))))))).
lex(loves, dr(0,dl(0,np,s),np), lambda(O,lambda(S,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(appl(appl(love,Y),X),&,appl(Phi,E))))))))))).
lex(owns, dr(0,dl(0,np,s),np), lambda(O,lambda(S,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(appl(appl(own,Y),X),&,appl(Phi,E))))))))))).
lex(beats, dr(0,dl(0,np,s),np), lambda(O,lambda(S,appl(S,lambda(X,appl(O,lambda(Y,lambda(E,lambda(Phi,bool(appl(appl(beat,Y),X),&,appl(Phi,E))))))))))).
lex(a, dr(0,np,n), lambda(N,lambda(Psi,lambda(E,lambda(Phi,quant(exists,X,appl(appl(appl(N,X),E),lambda(F,appl(appl(appl(Psi,X),update(X,F)),Phi))))))))).
lex(every, dr(0,np,n), lambda(N,lambda(Psi,lambda(E,lambda(Phi,bool(quant(forall,X,not(appl(appl(appl(N,X),E),lambda(F,not(appl(appl(appl(Psi,X),update(X,F)),lambda(_,true))))))),&,appl(Phi,E))))))).
lex(who, dr(0,dl(0,n,n),dl(0,np,s)), lambda(R,lambda(N,lambda(X,lambda(E,lambda(Phi,appl(appl(appl(N,X),E),lambda(F,appl(appl(appl(R,lambda(Psi,appl(Psi,X))),F),Phi))))))))).

% ============================================================
% = Example Sentences                                        =
% ============================================================

example(" He loves Mary.", txt).
example(" A man loves Mary.", txt).
example(" Mary beats a man.", txt).
example(" Every man loves Mary.", txt).
example(" Mary beats every man.", txt).
example(" Every man loves a woman.", txt).
example(" A farmer owns a donkey. He beats it.", txt).
example(" John loves Mary. He smiles.", txt).
example(" A man who loves Mary smiles.", txt).
example(" Every farmer who owns a donkey beats it.", txt).
