% -*- Mode: Prolog -*-
% ============================================================
% Grail
% ============================================================
% !grail 3.1.1

% ============================================================
% = Structural Conversions
% ============================================================

%postulate(p(0,p(0,A,B),C),p(0,A,p(0,B,C)),'Ass'). % ass1
%postulate(p(0,A,p(0,B,C)),p(0,p(0,A,B),C),'Ass'). % ass2


% ============================================================
% = Macros
% ============================================================

atomic_type(n, e->t).
atomic_type(np, e). 
atomic_type(s, t).
atomic_type(st, t).
atomic_type(pp, (e->t)->(e->t)).
atomic_type(mod,e->e).

:- dynamic macro/2.

macro(iv, dl(0, np, s)).
macro(tv, dr(0, iv, np)).
macro(mod,dr(0,np,np)).
macro(adv,dl(0,iv,iv)).

continuous(0).

external(0).

% ============================================================
% = Lexicon
% ============================================================

:- dynamic lex/3.

% lexical entries for the first derivation
lex(zhejianyifu, dr(0,st,dl(0,mod,s)),zhejianyifu).
lex(buliao,np,buliao).
lex(bucuo,iv,bucuo).

% lexical entries for example (3)
lex(youplclass,dr(0,st,dr(0,s,adv)),youplclass).
lex(we,np,we).
lex(firstpickupyangfan,iv,firstpickupyangfan).

% lexical entries for example (4)
lex(jiu,dr(0,st,dr(0,s,np)),jiu).
lex(wo,np,wo).
lex(xihuanheguizhou,tv,xihuanheguizhou).
lex(de,dr(0,np,np),de).

% ============================================================
% = Tokenization
% ============================================================

% ============================================================
% = Example Phrases
% ============================================================

%example("--- basic ---", null).
example(" zhejianyifu buliao bucuo.", st).
example(" youplclass we firstpickupyangfan.",st).
example(" jiu wo xihuanheguizhou de.",st).