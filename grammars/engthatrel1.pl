% ============================================================
% Eng that-rel 1
% ============================================================
% !grail v2.0b

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

% modally-controlled permutation

% permutation of hypothetical np

postulate(p(0,B,zip(r,A)),p(0,zip(r,A),B),'relP').

% rightassociativity: 
% hypothetical np adjoins to the right of the right argument

postulate(p(0,A,p(0,B,zip(r,C))),p(0,p(0,A,B),zip(r,C)),'RA').

% mixed commutativity/associativity: 
% hypothetical np adjoins to the right of the left argument

postulate(p(0,p(0,A,zip(r,C)),B),p(0,p(0,A,B),zip(r,C)),'MAC').


% leftassociativity:
% hypothetical np adjoins the left of the leftmmost argument

postulate(p(0,p(0,zip(r,A),B),C),p(0,zip(r,A),p(0,B,C)),'LA').


external(_).

continuous(0).

% = extraction

custom_first_order(dl(0,dia(r,box(r,A0)),B0), dl(0,dia(r,box(r,A)),B), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).


% ============================================================
% Macros
% ============================================================










% =============================================================
% Lexicon
% =============================================================

lex(letter,n,letter).
lex(that,dr(0,dl(0,n,n),dl(0,dia(r,box(r,np)),s)),lambda(A,lambda(B,lambda(C,bool(appl(A,C),&,appl(B,C)))))).
lex(arrived,dl(0,np,s),arrived).
lex(kim,np,k).
lex(wrote,dl(0,np,dr(0,s,np)),wrote).
lex(sandy,np,s).
lex(bo,np,s).
lex(of,dr(0,ofp,np),of).
lex(draft,dr(0,n,ofp),draft).
lex(first,dr(0,n,n),first).
lex(the, dr(0,np,n), lambda(A,quant(iota,B,appl(A,B)))).
lex(sent,dl(0,np,dr(0,dr(0,s,top),np)),sent).
lex(promised,dl(0,np,dr(0,s,inf)),promised).
lex(write,dr(0,vp,np),write).
lex(promise,dr(0,vp,inf),promise).
lex(to,dr(0,top,np),to).
lex(to,dr(0,inf,vp),lambda(A,A)).
lex(remind,dr(0,dr(0,vp,inf),np),remind).
lex(told,dl(0,np,dr(0,dr(0,s,inf),np)),told).
lex(reached,dl(0,np,dr(0,s,np)),reached).

% ============================================================
% Examples
% ============================================================

% = example(String,Formula)

example("letter that arrived",n).
example("letter that Kim wrote",n).
example("letter that Kim wrote the first draft of",n).
example("letter that Kim sent to Sandy",n).
example("letter that Kim sent the first draft of to Sandy",n).
example("letter that Kim promised to write",n).
example("letter that Kim told Sandy to write the first draft of",n).
example("letter that Kim told Sandy to promise to write",n).
example("letter that Kim told Sandy to remind Bo to promise to write",n).
example("letter that reached Kim",n).




