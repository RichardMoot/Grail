% -*- Mode: Prolog -*-
% ============================================================
% big_french_drt.pl
% ============================================================
% !grail 3.1.1

:- ensure_loaded(french_roles).

% This grammar is made for use with the supertagger and the grammar
% extracted from the Paris VII Treebank.

% It contains a mix of macro definitions, structural rules and the
% start of a semantic component, which will be made more and more
% complete over time.

% = output options

% = tense and aspect
%
% A choice between "yes" and "no"

tense_aspect(verkuyl).
%tense_aspect(drs). % DEFAULT
%TENSE_Y tense_aspect(yes).
%TENSE_N tense_aspect(no).

% is the document in a more literary style? Then the passe
% compose will have E < R o S as temporal relation between
% event time E, reference time R and current time S.
% In spoken language (and more informal styles) the passe
% compose often has E o R < S as an alternative (which
% would be the passe simple in a more literary text).

document_style(literary).

% = Lefff features to add to the semantics
%
% - "none" adds no Lefff features at all
% - "basic" adds only "human" and "place" features
% - "all" adds number and gender features as well
%
% Note that only features which can unambiguously decided will
% be added. If a form is ambiguis between masculin and feminin,
% no information will be added (instead of a disjunction)

lefff_info(basic). % DEFAULT
%LEFFF_N lefff_info(none).
%LEFFF_B lefff_info(basic).
%LEFFF_A lefff_info(all).

% these variables have little effect for the moment, since a lot
% of the neo-Davidsonian DRT semantics is hard-coded.

event_semantics(classic).
discourse_semantics(drt).

%auto_expand(monde). 

invisible_mode(0).

atomic_type(n, e->t).
atomic_type(np, (e->t)->t).
atomic_type(pp, (e->t)->t).
atomic_type(cl_r, (e->t)->t).
atomic_type(cl_r12, (e->t)->t).
atomic_type(cl_a3, (e->t)->t).
atomic_type(cl_d3, (e->t)->t).
atomic_type(cl_y, (e->t)->t).
atomic_type(cl_en, (e->t)->t).
atomic_type(cl, (e->t)->t).
atomic_type(cl(_), (e->t)->t).
atomic_type(s, s->t).
atomic_type(s(_), s->t).
atomic_type(cs, s->t).
atomic_type(txt, t).

% np(Case,Expl,Num-Per)
% Case = nom, acc
% Expl = ce, il
% Num-Per

%custom_first_order(dl(1,lit(s(Z)),lit(s(Z))) dl(1, lit(s, [V,W,Z]), lit(s, [V,W,Z])), _, [X,X]).

custom_first_order(lit(np), lit(np, [X,Y,_,_,_]), _, [X,Y]).
custom_first_order(lit(np(U,V,W)), lit(np, [X,Y,U,V,W]), _, [X,Y]). 
custom_first_order(lit(pp), lit(pp, [X,Y,_]), _, [X,Y]). 
custom_first_order(lit(pp(Z)), lit(pp, [X,Y,Z]), _, [X,Y]). 
custom_first_order(lit(s), lit(s, [X,Y,_]), _, [X,Y]). 
custom_first_order(lit(s(Z)), lit(s, [X,Y,Z]), _, [X,Y]). 

% = extraction

% = mode 1 : mixed associativity and mixed commutativity

custom_first_order(dr(0,B0,dia(1,box(1,A0))), dr(0,B,dia(1,box(1,A))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [Z,Z]),
	add_first_order(B0, B, P, [X,Y]).

% = mode 0 : mixed associativity only

custom_first_order(dia(0,box(0,A0)), dia(0,box(0,A)), P, [X,Y]) :-
	!,
	add_first_order(A0, A, P, [X,Y]).

macro(dr(0,dl(0,dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s))))),dr(0,s,box(1,dia(1,dr(0,dl(0,np,s),dl(0,np,s)))))),dr(0,s,dia(1,box(1,dr(0,dl(0,np,s),dl(0,np,s)))))),
      dr(0,dl(0,dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3))))))),dr(0,lit(s(S1)),box(1,dia(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3)))))))),dr(0,lit(s(S1)),dia(1,box(1,dr(0,dl(0,lit(np(nom,_,_)),lit(s(S2))),dl(0,lit(np(nom,_,_)),lit(s(S3))))))))).
macro(dr(0,dl(0,dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,s),dl(0,np,s))),dr(0,dl(0,np,s),dl(0,np,s))),
      dr(0,dl(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T)))),dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(T))))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dl(0,lit(np(nom,_,_)),lit(s(_)))))).
macro(dr(0,dl(0,dr(0,dl(0,np,s),np),dr(0,dl(0,np,s),np)),dr(0,dl(0,np,s),np)),dr(0,dl(0,dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W))),dr(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),lit(np(acc,V,W)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),lit(np(acc,_,_))))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)),dr(0,dl(0,dl(0,lit(np(nom,X,Y)),lit(s(S))),dl(0,lit(np(nom,X,Y)),lit(s(S)))),dl(0,lit(np(nom,_,_)),lit(s(_))))).
macro(dr(0,dl(0,dr(0,pp,np),dl(0,n,n)),dr(0,s,dia(1,box(1,pp)))), dr(0,dl(0,dr(0,lit(pp(P)),np),dl(0,n,n)),dr(0,lit(s(main)),dia(1,box(1,lit(pp(P))))))).
macro(dr(0,dl(0,dl(0,np,s),np),np), dr(0,dl(0,dl(0,lit(np(U,V,W)),lit(s(_))),lit(np(U,V,W))),lit(np(U,V,W)))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np(U,V,W)),lit(s(X))),dl(0,lit(np(U,V,W)),lit(s(X)))),dl(0,lit(np(U,V,W)),lit(s(_))))).
macro(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,np)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(np(acc,_,_))))))).
macro(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(pp(à))))))).
macro(dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_de)))), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dia(1,box(1,lit(pp(de))))))).
macro(dr(0,dl(0,np,s),dl(0,np,s)), dr(0,dl(0,lit(np(U,V,W)),lit(s(X))),dl(0,lit(np(U,V,W)),lit(s(X))))).
macro(dl(0,dl(0,np,s),dl(0,np,s)), dl(0,dl(0,lit(np(U,V,W)),lit(s(X))),dl(0,lit(np(U,V,W)),lit(s(X))))).
macro(dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s)), dr(0,dl(0,dl(0,lit(np(U,V,W)),lit(s(X))),dl(0,lit(np(U,V,W)),lit(s(X)))),dl(0,lit(np(U,V,W)),lit(s(_))))).
macro(dr(0,dl(0,np,np),dr(0,s,dia(1,box(1,np)))),dr(0,dl(0,lit(np(U,V,W)),lit(np(U,V,W))),dr(0,lit(s(_)),dia(1,box(1,lit(np(acc,_,_))))))).
macro(dr(0,s,dia(1,box(1,np))),dr(0,lit(s(_)),dia(1,box(1,lit(np(acc,_,_)))))).
macro(dr(0,dl(0,np,s),np),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_)))).
macro(dr(0,dr(0,s,np),np),dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(np(nom,_,_)))).
macro(dr(0,dl(0,dr(0,s,dia(0,box(0,dl(0,np,s)))),dr(0,s,dl(0,np,s))),dr(0,s,dia(0,box(0,dl(0,np,s))))),dr(0,dl(0,dr(0,lit(s(X)),dia(0,box(0,dl(0,np,lit(s(Y)))))),dr(0,lit(s(X)),dl(0,np,lit(s(Y))))),dr(0,lit(s(X)),dia(0,box(0,dl(0,np,lit(s(Y)))))))).

macro(pp_apres, lit(pp(apres))).
macro(pp_pour, lit(pp(pour))).
macro(pp_par, lit(pp(par))).
macro(pp_sur, lit(pp(sur))).
macro(pp_en, lit(pp(en))).
macro(pp_dans, lit(pp(dans))).
macro(pp_vers, lit(pp(vers))).
macro(pp_entre, lit(pp(entre))).
macro(pp_comme, lit(pp(comme))).
macro(pp_contre, lit(pp(contre))).
macro(pp_avec, lit(pp(avec))).
macro(pp_sans, lit(pp(sans))).
macro(pp_sous, lit(pp(sous))).
macro(pp_de, lit(pp(de))).
macro(pp_à, lit(pp(à))).
macro(pp_a, lit(pp(à))).
macro(pp, lit(pp(_))).

macro(cl_3d, lit(cl_d3)).
macro(cl_3a, lit(cl_a3)).
macro(cl_12r, lit(cl_r12)).
macro(cl_en, lit(cl_en)).
macro(cl_y, lit(cl_y)).

macro(np_nom, lit(np(nom,_,_))).
macro(np_ce, lit(np(nom,ce,3-s))).
macro(np_il, lit(np(nom,il,3-s))).
macro(np_acc, lit(np(acc,_,_))).
macro(np_refl, lit(np(acc,refl,3-s))).
macro(np, lit(np(_,_,_))).

macro(dr(0,dl(0,np,s),dr(0,dl(0,np,s),np)),dr(0,dl(0,lit(np(nom,A,B)),lit(s(X))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(X))),lit(np(acc,_,_))))).
macro(dr(0,dl(0,dr(0,dl(0,np,s),dia(0,box(0,np))),dr(0,dl(0,np,s),np)),dr(0,dl(0,np,s),dia(0,box(0,np)))),
      dr(0,dl(0,dr(0,dl(0,lit(np(nom,A,B)),lit(s(S))),dia(0,box(0,lit(np(acc,_,_))))),dr(0,dl(0,lit(np(nom,A,B)),lit(s(S))),lit(np(acc,_,_)))),dr(0,dl(0,lit(np(nom,_,_)),lit(s(_))),dia(0,box(0,lit(np(acc,_,_))))))).
macro(dr(0,s_q,s),dr(0,lit(s(q)),lit(s(_)))).	  % main or ppres
macro(dr(0,s_whq,s),dr(0,lit(s(whq)),lit(s(_)))). % main or ppres
macro(dl(1,s,s), dl(1,lit(s(X)),lit(s(X)))).
macro(dr(0,dl(0,s,s),np), dr(0,dl(0,lit(s(X)),lit(s(X))),lit(np(_,_,_)))).
macro(dr(0,dl(0,s,s),s), dr(0,dl(0,lit(s(X)),lit(s(X))),lit(s(_)))).
macro(dr(0,dl(0,pp,pp),pp), dr(0,dl(0,lit(pp(X)),lit(pp(X))),lit(pp(_)))).
macro(dr(0,s,s), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_ppart,s_ppart), dl(1,lit(s(X)),lit(s(X)))).
macro(dr(0,s_ppart,s_ppart), dr(0,lit(s(X)),lit(s(X)))).
macro(dl(0,s_inf,s_inf), dl(1,lit(s(X)),lit(s(X)))).
macro(dr(0,s_inf,s_inf), dr(0,lit(s(X)),lit(s(X)))).

macro(s_inf, lit(s(inf(_)))).
macro(s_deinf, lit(s(inf(de)))).
macro(s_ainf, lit(s(inf(a)))).
macro(s_ppres, lit(s(ppres))).
macro(s_ppart, lit(s(ppart))).
macro(s_pass, lit(s(pass))).
macro(cs, lit(s(q))).
macro(s_q, lit(s(q))).
macro(s_whq, lit(s(whq))).
macro(s, lit(s(_))).
macro(s_top, lit(s(_))).

macro(dia(1,box(1,np)), dia(1,box(1,lit(np(acc,_,3-s))))).

translate_form(dr(0,dl(0,cl_r,s),np),dr(0,dl(0,lit(cl_r),lit(s(main))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),np),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(np(acc,_,_)))).
translate_form(dl(0,cl_r,dl(0,np,s)),dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main))))).
translate_form(dr(0,dl(0,np,s),s_q),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q)))).
translate_form(dr(0,dl(0,np,s),s_whq),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(whq)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),dl(0,n,n)),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(_)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp_de),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(de)))).
translate_form(dr(0,dr(0,dl(0,np,s),s_q),pp_a),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(s(q))),lit(pp(à)))).
translate_form(dr(0,dl(0,np,s),np),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,np,s),pp),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(_)))).
translate_form(dr(0,dl(0,np,s),pp_a),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(à)))).
translate_form(dr(0,dl(0,np,s),pp_de),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(de)))).
translate_form(dr(0,dl(0,np,s),pp_par),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(par)))).
translate_form(dl(0,np,s),dl(0,lit(np(nom,_,_)),lit(s(main)))).
translate_form(dr(0,s,np),dr(0,lit(s(main)),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_inf)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_)))))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_ppart)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(ppart))))).
translate_form(dr(0,dr(0,s,np),dl(0,np,s_ppart)),dr(0,dr(0,lit(s(main)),lit(np(nom,_,_))),dl(0,lit(np(_,_,_)),lit(s(ppart))))).
translate_form(dr(0,dl(0,np,s),dl(0,np,s_pass)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(pass))))).
translate_form(dr(0,dr(0,s,pp),np),dr(0,dr(0,lit(s(main)),lit(pp(_))),lit(np(nom,_,_)))).
translate_form(dr(0,dl(0,np,s),dl(0,n,n)),dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(n),lit(n)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),dl(0,cl_r,dl(0,np,s_ppart))),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(ppart)))))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp_a),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(à)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),pp_de),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(pp(de)))).
translate_form(dr(0,dr(0,s,np),np),dr(0,dr(0,lit(s(main)),lit(np(acc,_,_))),lit(np(nom,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_a),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(à))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_par),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(par))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp_de),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(de))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),pp),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),lit(pp(_))),lit(np(acc,_,_)))).
translate_form(dr(0,dl(0,cl_y,dl(0,np,s)),np),dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),np),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(np(acc,_,_)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),pp_a),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(pp(à)))).
translate_form(dr(0,dr(0,dl(0,np,s),dl(0,np,s_inf)),pp),dr(0,dr(0,dl(0,lit(np(nom,_,_)),lit(s(main))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))),lit(pp(_)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),s_q),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(s(q)))).
translate_form(dr(0,dl(0,cl_r,dl(0,np,s)),s_whq),dr(0,dl(0,lit(cl_r),dl(0,lit(np(nom,_,_)),lit(s(main)))),lit(s(whq)))).

translate_form(lit(s),lit(s(_))).
translate_form(lit(np),lit(np(_,_,_))).

% =

continuous(0).
continuous_dia(0).

external(_).

% =

% standard extraction postulates for a unary mode 1
% commutativity, mixed commutativity and mixed associativity for mode 1

postulate(p(0,A,zip(1,B)), p(0,zip(1,B),A), 'Cp1').
postulate(p(0,A,p(0,B,zip(1,C))), p(0,p(0,A,B),zip(1,C)), 'MCp1').
postulate(p(0,p(0,A,zip(1,B)),C), p(0,p(0,A,C),zip(1,B)), 'MAp1').

% mixed associativity only for unary mode 0

postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(0,A,B),zip(0,C)), 'MAa1').
postulate(p(0,p(0,A,B),zip(0,C)), p(0,A,p(0,B,zip(0,C))), 'MAa').

special_string(".", '.').
special_string(";", ';').
special_string(":", ':').
special_string(",", ',').
special_string("!", '!').
special_string("?", '?').

raising_verb(sembler).
raising_verb(paraître).
raising_verb(apparaître).

weather_verb(pleuvoir).
weather_verb(pleuvasser).
weather_verb(pleuvoter).
weather_verb(pleuviner).
weather_verb(pluviner).
weather_verb(neiger).
weather_verb(gèler).

% ====================
% = semantic recipes =
% ====================

semantics(dot, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_semantics(ES, Dis, Sem).

semantics(dot_np, Sem) :-
	event_semantics(ES),
	discourse_semantics(Dis),
	dot_np_semantics(ES, Dis, Sem).

% TODO - verify carefully which parts are presuppositions

possessive_1p_semantics(lambda(P,lambda(Q,presup(merge(drs(Vars,L0),appl(P,X)),appl(Q,X)))), SujF, ObjF) :-
	possessive_s_features_1p(SujF, Y, Vars, [X], L0, L1),
	possessive_o_features(ObjF, X, L1, [appl(appl(de,X),Y)]).
possessive_2p_semantics(lambda(P,lambda(Q,presup(merge(drs(Vars,L0),appl(P,X)),appl(Q,X)))), SujF, ObjF) :-
	possessive_s_features_2p(SujF, Y, Vars, [X], L0, L1),
	possessive_o_features(ObjF, X, L1, [appl(appl(de,X),Y)]).
possessive_3p_semantics(lambda(P,lambda(Q,presup(merge(drs(Vars,L0),appl(P,X)),appl(Q,X)))), SujF, ObjF) :-
	possessive_s_features_3p(SujF, Y, Vars, [X], L0, L1),
	possessive_o_features(ObjF, X, L1, [appl(appl(de,X),Y)]).

possessive_s_features_1p(s, X, [X|Xs], Xs, [appl(orateur,X)|R], R).
possessive_s_features_1p(p, X, [X,Y|Xs], Xs, [bool(Y,=,?),bool(num(Y),>,1),appl(orateur,X),bool(X,atomic_sub,Y)|R], R).

% note that we do not demand that the set of listeners/addressees has more than
% one element in order to avoid making a commitment between polite singular
% "vous" and plurar "vous"

possessive_s_features_2p(s, X, [X|Xs], Xs, [appl(auditeur,X)|R], R).
possessive_s_features_2p(p, X, [X,Y|Xs], Xs, [bool(Y,=,?),appl(auditeur,Y),bool(X,atomic_sub,Y)|R], R).

possessive_s_features_3p(s, X, [X|Xs], Xs, [bool(X,=,'singular?')|R], R).
possessive_s_features_3p(p, X, [X|Xs], Xs, [bool(X,=,?),bool(num(X),>,1)|R], R).


possessive_o_features(p, X, [bool(num(X),>,1)|R], R).
possessive_o_features(f, X, [appl(feminin,X)|R], R).
possessive_o_features(m, _X, R, R). % masculin form can occur because following word starts with vowel (TODO: REFINE)
possessive_o_features(s, _X, R, R).

% do we treat the determiners "un/une/le/la" as having a singleton set as argument or not?

% variables without further specification are entities
% = add comment to treat X as a singleton set
singleton_sets(_, []).
% = remove comment to treat X as a singleton set
%singleton_sets(X, [bool(num(X),=,1)]).

gq_no_semantics(lambda(P,lambda(Q,drs([],[bool(merge(drs([variable(X)],[]),appl(P,X)),->,drs([],[not(appl(Q,X))]))])))).
gq_a_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X)],L),appl(P,X)),appl(Q,X))))) :-
	singleton_sets(X, L).
gq_every_semantics(lambda(P,lambda(Q,drs([],[bool(merge(drs([variable(X)],[]),appl(P,X)),->,appl(Q,X))])))).
% semantics for "every" but which presupposes existence
% TODO: verify if it really presupposes the existence of more than one (P Z)
gq_every_semantics_bis(lambda(P,lambda(Q,presup(merge(drs([variable(Z)],[bool(num(Z),>,1)]),appl(P,Z)),drs([],[bool(merge(drs([],[]),appl(P,Z)),->,appl(Q,Z))]))))).

gq_the_semantics(lambda(P,lambda(Q,presup(merge(drs([variable(X)],L),appl(P,X)),appl(Q,X))))) :-
	singleton_sets(X, L).
% variation for "l'un des"
gq_l_un_des_semantics(lambda(P,lambda(Q,presup(merge(drs([variable(X),variable(Y)],[bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,Y)),appl(Q,X))))).
% variation for "l'un de"
gq_l_un_de_semantics(lambda(P,lambda(Q,presup(merge(drs([variable(X),variable(Y)],[bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,Y)),appl(Q,X))))).
gq_les_semantics(lambda(P,lambda(Q,presup(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
gq_this_semantics(Sem) :-
	gq_the_semantics(Sem).

wh_rel_semantics(lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(appl(P,lambda(R,appl(R,X))),_)))))).

% Adj  (e->t)->(e->t))
% P    (e->t)
% V    e
intensifier_semantics(Word, lambda(Adj, lambda(P, lambda(V, merge(drs([event(L)],[appl(Word,L),drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),V))]),appl(P,V)))))).

dot_semantics(none, _, lambda(X,X)).
dot_semantics(classic, Dis, Sem) :-
	dot_semantics1(Dis, Sem).
dot_semantics(neo, Dis, Sem) :-
	dot_semantics1(Dis, Sem).

dot_semantics1(drt, lambda(P, merge(drs([event(E)],[]),appl(P, E)))). 

dot_np_semantics(none, _, lambda(X,X)).
dot_np_semantics(classic, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).
dot_np_semantics(neo, Dis, Sem) :-
	dot_np_semantics1(Dis, Sem).

dot_np_semantics1(drt, lambda(P,appl(P,lambda(_V,drs([],[]))))).

noun_semantics(drt, Word, lambda(V,drs([],[appl(Word,V)]))).


sem_tv_subject_control(Word, lambda(INF,lambda(NP,lambda(E,appl(NP,lambda(Z,drs([event(F),event(L)],[appl(event,E)|Conds]))))))) :-
	add_roles([agent-Z,theme-L], Word, E, Conds, [drs_label(L,appl(appl(INF,lambda(P,appl(P,Z))),F))]).

auxiliary_verb_etre(POS, _Rest0, lambda(P,lambda(X,lambda(E,merge(appl(appl(P,X),E),drs(EVs,Rest)))))) :-
	past_participle_semantics(POS, [], EVs, E, Rest).

auxiliary_verb_avoir(POS, _Rest0, lambda(P,lambda(X,lambda(E,merge(appl(appl(P,X),E),drs(EVs,Rest)))))) :-
	past_participle_semantics(POS, [], EVs, E, Rest).
auxiliary_verb_se(POS, _Rest0, lambda(VPPC, lambda(C, lambda(NP,lambda(E,merge(appl(appl(appl(VPPC,C),NP),E),drs(EVs,Rest))))))) :-
	past_participle_semantics(POS, [], EVs, E, Rest).	

title_semantics(Title, lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(appl(titre,Title),X)]))))))).

% =====================================
% =           Role lexicon            =
% =====================================


get_roles(Verb, List, Roles) :-
	get_roles1(Verb, List, Roles),
	!.
get_roles(Verb, [A|As], Roles) :-
	default_roles(As, A, Verb, Roles).

default_roles([], np, _, [agent]).
default_roles([np], np, _, [agent,patient]).
default_roles([cl_r], np, _, [agent,patient]).
default_roles([np], cl_r, _, [agent,patient]).
default_roles([np,np], np, _, [agent,patient,theme]).
default_roles([cl_r,np], np, _, [agent,patient,theme]).
default_roles([cl_r,pp(_)], np, _, [agent,patient,theme]).
default_roles([pp(_)], np, _, [agent,theme]).
default_roles([np,pp(_)], np, _, [agent,theme,patient]).
default_roles([pp(_),np], np, _, [agent,patient,theme]).
default_roles([pp(_),cl_r], np, _, [agent,patient,theme]).
default_roles([cl_r,inf(_)], np, _, [agent,patient,theme]).
default_roles([inf(_),cl_r], np, _, [agent,theme,patient]).

% =

clitic_y_word(retrouver, retrouver_à, arg) :-
	!.
clitic_y_word(X, X, mod).

% = roles for verbs taking a single argument

role_lexicon(naître, patient) :-
	!.
role_lexicon(tomber, patient) :-
	!.
role_lexicon(dormir, patient) :-
	!.
role_lexicon(_, agent).

% = roles for verbs taking two arguments
% if it is the subject which moves along the path and the movement is not necessarily agentive, it has the theme role.
role_lexicon(aller, theme, destination) :-
	!.
role_lexicon(aller_à, theme, destination) :-
	!.
role_lexicon(venir_de, theme, source) :-
	!.
role_lexicon(arriver_à, theme, destination) :-
	!.
role_lexicon(parvenir_à, theme, destination) :-
	!.
role_lexicon(revenir_à, theme, destination) :-
	!.
role_lexicon(venir_à, theme, destination) :-
	!.
role_lexicon(arriver, theme, destination) :-
	!.
role_lexicon(partir_de, theme, source) :-
	!.
role_lexicon(partir, theme, source) :-
	!.
role_lexicon(passer, agent, durée) :-
	!.
% object = theme
role_lexicon(affirmer, agent, theme) :-
	!.
role_lexicon(assurer, agent, theme) :-
	!.
role_lexicon(connaître, agent, theme) :-
	!.
role_lexicon(contrôler, agent, theme) :-
	!.
role_lexicon(critiquer, agent, theme) :-
	!.
role_lexicon(croire, agent, theme) :-
	!.
role_lexicon(déduire, agent, theme) :-
	!.
role_lexicon(définir, agent, theme) :-
	!.
role_lexicon(démontrer, agent, theme) :-
	!.
role_lexicon(déplorer, agent, theme) :-
	!.
role_lexicon(desirer, agent, theme) :-
	!.
role_lexicon(dire, agent, theme) :-
	!.
role_lexicon(entendre, agent, theme) :-
	!.
role_lexicon(entreprendre, agent, theme) :-
	!.
role_lexicon(être, agent, theme) :-
	!.
role_lexicon(expliquer, agent, theme) :-
	!.
role_lexicon(faire, agent, theme) :-
	!.
role_lexicon(fasciner, agent, theme) :-
	!.
role_lexicon(filmer, agent, theme) :-
	!.
role_lexicon(honorer, agent, theme) :-
	!.
role_lexicon(imaginer, agent, theme) :-
	!.
role_lexicon(indiquer, agent, theme) :-
	!.
role_lexicon(interdire, agent, theme) :-
	!.
role_lexicon(monopoliser, agent, theme) :-
	!.
role_lexicon(nier, agent, theme) :-
	!.
role_lexicon(penser, agent, theme) :-
	!.
role_lexicon(pouvoir, agent, theme) :-
	!.
role_lexicon(pratiquer, agent, theme) :-
	!.
role_lexicon(prouver, agent, theme) :-
	!.
role_lexicon(reconnaître, agent, theme) :-
	!.
role_lexicon(redouter, agent, theme) :-
	!.
role_lexicon(refuser, agent, theme) :-
	!.
role_lexicon(regarder, agent, theme) :-
	!.
role_lexicon(regretter, agent, theme) :-
	!.
role_lexicon(réglementer, agent, theme) :-
	!.
role_lexicon(résoudre, agent, theme) :-
	!.
role_lexicon(redouter, agent, theme) :-
	!.
role_lexicon(savoir, agent, theme) :-
	!.
role_lexicon(souhaiter, agent, theme) :-
	!.
role_lexicon(suggérer, agent, theme) :-
	!.
role_lexicon(longer, agent, lieu) :-
	!.
role_lexicon(_, agent, patient).

% = roles for verbs taking three arguments

role_lexicon(amener_à, agent, patient, destination) :-
	!.
role_lexicon(donner_à, agent, patient, destination) :-
	!.
role_lexicon(offrir_à, agent, patient, destination) :-
	!.
role_lexicon(rendre_à, agent, patient, destination) :-
	!.
role_lexicon(demander_à, agent, patient, source) :-
	!.
role_lexicon(prendre_de, agent, patient, source) :-
	!.
role_lexicon(prendre_à, agent, patient, source) :-
	!.
role_lexicon(mettre, agent, patient, destination) :-
	!.
role_lexicon(_, agent, patient, theme).

role_lexicon_np_adj(_, agent, theme).
role_lexicon_np_np_adj(_, agent, patient, theme).

role_lexicon_np_loc(entrer, agent, destination) :-
	!.
role_lexicon_np_loc(aller, agent, destination) :-
	!.
role_lexicon_np_loc(être, agent, lieu) :-
	!.
role_lexicon_np_loc(figurer, agent, lieu) :-
	!.
role_lexicon_np_loc(habiter, agent, lieu) :-
	!.
role_lexicon_np_loc(résider, agent, lieu) :-
	!.
role_lexicon_np_loc(_, agent, lieu).

role_lexicon_np_np_loc(mettre, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(placer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(envoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(renvoyer, agent, patient, destination) :-
	!.
role_lexicon_np_np_loc(_, agent, patient, lieu).

combine_se(Word, SeWord) :-
    (
        var(Word)
    ->
        SeWord = Word
    ;
	atom_chars(Word, [F|_]),
    (
	is_vowel(F)
    ->
        atomic_list_concat(['s\'', Word], SeWord)
    ;
	atomic_list_concat([se,'_',Word], SeWord)
    )). 

% rather restricted list of vowels (and no "h", which would need to be listed lexically)

is_vowel(a).
is_vowel(e).
is_vowel(i).
is_vowel(u).
is_vowel(é).

combine_prep_word(Prp, Word0, Word) :-
	var(Prp),
	!,
	Word = Word0.
combine_prep_word(_, Word0, Word) :-
	var(Word0),
	!,
	Word = Word0.
combine_prep_word(a, Word, PrepWord) :-
	!,
        atomic_list_concat([Word,'_',à], PrepWord).
combine_prep_word(Prep, Word, PrepWord) :-
        atomic_list_concat([Word,'_',Prep], PrepWord).

% =====================================
% =        Tense information          =
% =====================================

% pos_time(+VerbForm, -EventList)
% compatibility only, subsumed by pos_time/4, which allows
% the introduction of additional (event) variables in the
% main DRS.

pos_time(VForm, EList) :-
	pos_time(VForm, _, _, EList),
	format(user_error, 'POS_TIME ~w ~w~n', [VForm,EList]).

pos_time(VForm, EVs0, EVs, EList) :-
	tense_aspect(TA),
	!,
	pos_time(TA, VForm, EVs0, EVs, EList).
pos_time(no, _, EVs, EVs, _-[]) :-
	!.
pos_time(none, _, EVs, EVs, _-[]) :-
	!.
pos_time(drs, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_drs(TENSE, EVs0, EVs, EList).
pos_time(verkuyl, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_verkuyl(TENSE, EVs0, EVs, EList).
pos_time(yes, ver:TENSE, EVs0, EVs, EList) :-
	!,
	pos_time_verkuyl(TENSE, EVs0, EVs, EList).
pos_time(_, _, EVs, EVs, _-[]).

% the tense operators "present" and "past" are of type (v->t)->(v->t)
semantics_tense_pres(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(temps(I),overlaps,maintenant)]))))).
semantics_tense_past(lambda(Phi,lambda(I,merge(appl(Phi,I),drs([],[bool(temps(I),<,maintenant)]))))).

% the "aspectual" operators are of type (v->t)->(v->t)
semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,<,J)])))).
%semantics_aspect_post(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(sub(I,a),<,J)])))).
semantics_aspect_post_imm(lambda(Phi, lambda(I, drs([J],[appl(Phi,J),bool(I,abuts,J)])))).
semantics_aspect_perf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(K,<,I)])))).
semantics_aspect_impf(lambda(Phi, lambda(I, drs([K],[appl(Phi,K),bool(I,subseteq,K)])))).
semantics_aspect_anch(lambda(Phi, lambda(J, drs([K],[appl(Phi,K),bool(K,=,'event?'),bool(K,<,J)])))).


pos_time_drs(pres, E-[bool(E,overlaps,maintenant),bool(E,overlaps,ref_time)]) :-
	!.
pos_time_drs(impf, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(ref_time,overlaps,S1),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(simp, EVs, EVs, E-[bool(appl(temps,E),<,maintenant),bool(ref_time,<,maintenant),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(cond, EVs, [event(S1)|EVs], E-[bool(S1,=,'time?'),bool(S1,<,E),bool(ref_time,overlaps,appl(temps,E))]) :-
	!.
pos_time_drs(futu, EVs, EVs, E-[bool(maintenant,<,appl(temps,E)),bool(ref_time,overlaps,maintenant)]) :-
	!.
pos_time_drs(_, EVs, EVs, _-[]).

pos_time_verkuyl(pres, EVs, EVs, E-[bool(appl(temps,E),overlaps,maintenant)]) :-
	!.
pos_time_verkuyl(impf, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),subseteq,appl(temps,E)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,X),overlaps,maintenant)])]) :-
	!.
%pos_time_verkuyl(futu, EVs, EVs, E-[drs([event(X)],[bool(sub(appl(temps,X),a),<,appl(temps,E)),bool(appl(temps,X),overlaps,maintenant)])]) :-
%	!.
pos_time_verkuyl(cond, EVs, EVs, E-[drs([event(X)],[bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
pos_time_verkuyl(simp, EVs, EVs, E-[drs([event(X)],[bool(X,=,'event?'),bool(appl(temps,X),<,appl(temps,E)),bool(appl(temps,E),<,maintenant)])]) :-
	!.
pos_time_verkuyl(_, EVs, EVs, _-[]).


past_participle_semantics(ver:TIME, EVs0, EVs, E, Rest) :-
	tense_aspect(TA),
	past_participle_semantics(TA, TIME, EVs0, EVs, E, Rest).

past_participle_semantics(drs, Time, EVs, EVs, E, Rest) :-
	!,
	past_participle_semantics_drs(Time, E, Rest).
past_participle_semantics(verkuyl, Time, EVs0, EVs, E, Rest) :-
	!,
	past_participle_semantics_verkuyl(Time, EVs0, EVs, E, Rest).
past_participle_semantics(yes, Time, EVs0, EVs, E, Rest) :-
	!,
	past_participle_semantics_verkuyl(Time, EVs0, EVs, E, Rest).
% even when no tense information is wanted, indicate SDRS "background" relation
% for the plus-que-parfait
past_participle_semantics(_, Time, E, Rest) :-
     (
         Time = impf
     ->
         Rest = [appl(background,E)]
     ;
         Rest = []
     ).

past_participle_semantics_drs(impf, E, [bool(appl(temps,E),<,ref_time),bool(ref_time,<,maintenant),appl(background,E)]) :-
	!.
past_participle_semantics_drs(futu, E, [bool(maintenant,<,appl(temps,E)),bool(appl(temps,E),<,ref_time)]) :-
	!.
past_participle_semantics_drs(cond, E, [bool(appl(temps,E),<,ref_time),bool(ref_time,<,maintenant)]) :-
	!.
past_participle_semantics_drs(pres, E, [bool(appl(temps,E),<,ref_time),bool(ref_time,overlaps,maintenant)]) :-
	!.
past_participle_semantics_drs(_, E, [bool(appl(temps,E),<,maintenant)]).

% Verkuyl-style semantics has perfect operator in auxiliary verb
past_participle_semantics_verkuyl(pres, EVs, EVs, E, [drs([event(I)],[bool(appl(temps,E),<,appl(temps,I)),bool(appl(temps,I),overlaps,maintenant)])]) :-
	!.
past_participle_semantics_verkuyl(futu, EVs, EVs, E, [drs([event(X),event(Y)],[bool(appl(temps,E),<,appl(temps,Y)),bool(appl(temps,X),<,appl(temps,Y)),bool(appl(temps,X),overlaps,maintenant)])]) :-
	!.
past_participle_semantics_verkuyl(impf, EVs, [event(Y)|EVs], E, [drs([event(X)],[bool(appl(temps,E),<,appl(temps,Y)),bool(appl(temps,X),subseteq,appl(temps,Y)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
past_participle_semantics_verkuyl(cond, EVs, EVs, E, [drs([event(X),event(Y),event(Z)],[bool(appl(temps,E),<,appl(temps,Z)),bool(appl(temps,Y),subseteq,appl(temps,Z)),bool(appl(temps,X),<,appl(temps,Y)),bool(appl(temps,X),overlaps,maintenant)])]) :-
	!.
past_participle_semantics_verkuyl(simp, EVs, EVs, E, [drs([event(X),event(Y)],[bool(appl(temps,E),<,appl(temps,Y)),bool(X,=,'event?'),bool(appl(temps,Y),subseteq,appl(temps,X)),bool(appl(temps,X),<,maintenant)])]) :-
	!.
past_participle_semantics_verkuyl(_, EVs, EVs, _, []).

% =====================================
% =         Cardinal Numbers          =
% =====================================

convert_cardinal(Sem, Num) :-
	convert_cardinal1(Sem, Num),
	!.
convert_cardinal(Num0, Num) :-
	atom_codes(Num0, List0),
	convert_interpunction(List0, List),
	atom_codes(Num1, List),
	catch(atom_number(Num1, Num),syntax_error,fail).

% = convert_inpterpunction
%
% NOTE: this implementation presupposes that number are represented
% European-style, with '.' regrouping and the ',' denoting fractions.

convert_interpunction([], []).
convert_interpunction([C0|Cs0], Cs) :-
    (
        C0 = 46
    ->
        convert_interpunction(Cs0, Cs)
    ;
        C0 = 44
    ->
        Cs = [46|Cs1],
        convert_interpunction(Cs0, Cs1)
    ;
        Cs = [C0|Cs1],
        convert_interpunction(Cs0, Cs1)
    ).

convert_cardinal1(zéro, 0).
convert_cardinal1(un, 1).
convert_cardinal1(une, 1).
convert_cardinal1(deux, 2).
convert_cardinal1(trois, 3).
convert_cardinal1(quatre, 4).
convert_cardinal1(cinq, 5).
convert_cardinal1(six, 6).
convert_cardinal1(sept, 7).
convert_cardinal1(huit, 8).
convert_cardinal1(neuf, 9).
convert_cardinal1(dix, 10).
convert_cardinal1(onze, 11).
convert_cardinal1(douze, 12).
convert_cardinal1(treize, 13).
convert_cardinal1(quatorze, 14).
convert_cardinal1(quize, 15).
convert_cardinal1(seize, 16).
convert_cardinal1(dix-sept, 17).
convert_cardinal1(dix-huit, 18).
convert_cardinal1(dix-neuf, 19).
convert_cardinal1(vingt, 20).
convert_cardinal1(vingt-et-un, 21).
convert_cardinal1(vingt-deux, 22).
convert_cardinal1(vingt-trois, 23).
convert_cardinal1(vingt-quatre, 24).
convert_cardinal1(vingt-cinq, 25).
convert_cardinal1(vingt-six, 26).
convert_cardinal1(vingt-sept, 27).
convert_cardinal1(vingt-huit, 28).
convert_cardinal1(vingt-neuf, 29).
convert_cardinal1(trente, 30).
convert_cardinal1(trente-et-un, 31).
convert_cardinal1(trente-deux, 32).
convert_cardinal1(trente-trois, 33).
convert_cardinal1(trente-quatre, 34).
convert_cardinal1(trente-cinq, 35).
convert_cardinal1(trente-six, 36).
convert_cardinal1(trente-sept, 37).
convert_cardinal1(trente-huit, 38).
convert_cardinal1(trente-neuf, 39).
convert_cardinal1(quarante, 40).
convert_cardinal1(quarante-et-un, 41).
convert_cardinal1(quarante-deux, 42).
convert_cardinal1(quarante-trois, 43).
convert_cardinal1(quarante-quatre, 44).
convert_cardinal1(quarante-cinq, 45).
convert_cardinal1(quarante-six, 46).
convert_cardinal1(quarante-sept, 47).
convert_cardinal1(quarante-huit, 48).
convert_cardinal1(quarante-neuf, 49).
convert_cardinal1(cinquante, 50).
convert_cardinal1(cinquante-et-un, 51).
convert_cardinal1(cinquante-deux, 52).
convert_cardinal1(cinquante-trois, 53).
convert_cardinal1(cinquante-quatre, 54).
convert_cardinal1(cinquante-cinq, 55).
convert_cardinal1(cinquante-six, 56).
convert_cardinal1(cinquante-sept, 57).
convert_cardinal1(cinquante-huit, 58).
convert_cardinal1(cinquante-neuf, 59).
convert_cardinal1(soixante, 60).
convert_cardinal1(soixante-et-un, 61).
convert_cardinal1(soixante-deux, 62).
convert_cardinal1(soixante-trois, 63).
convert_cardinal1(soixante-quatre, 64).
convert_cardinal1(soixante-cinq, 65).
convert_cardinal1(soixante-six, 66).
convert_cardinal1(soixante-sept, 67).
convert_cardinal1(soixante-huit, 68).
convert_cardinal1(soixante-neuf, 69).
convert_cardinal1(soixante-dix, 70).
convert_cardinal1(soixante-et-onze, 71).
convert_cardinal1(soixante-douze, 72).
convert_cardinal1(soixante-treize, 73).
convert_cardinal1(soixante-quatorze, 74).
convert_cardinal1(soixante-quinze, 75).
convert_cardinal1(soixante-seize, 76).
convert_cardinal1(soixante-dix-sept, 77).
convert_cardinal1(soixante-dix-huit, 78).
convert_cardinal1(soixante-dix-neuf, 79).
convert_cardinal1(quatre-vingts, 80).
convert_cardinal1(quatre-vingts-et-un, 81).
convert_cardinal1(quatre-vingts-deux, 82).
convert_cardinal1(quatre-vingts-trois, 83).
convert_cardinal1(quatre-vingts-quatre, 84).
convert_cardinal1(quatre-vingts-cinq, 85).
convert_cardinal1(quatre-vingts-six, 86).
convert_cardinal1(quatre-vingts-sept, 87).
convert_cardinal1(quatre-vingts-huit, 88).
convert_cardinal1(quatre-vingts-neuf, 89).
convert_cardinal1(quatre-vingts-dix, 90).
convert_cardinal1(quatre-vingts-et-onze, 91).
convert_cardinal1(quatre-vingts-douze, 92).
convert_cardinal1(quatre-vingts-treize, 93).
convert_cardinal1(quatre-vingts-quatorze, 94).
convert_cardinal1(quatre-vingts-quinze, 95).
convert_cardinal1(quatre-vingts-seize, 96).
convert_cardinal1(quatre-vingts-dix-sept, 97).
convert_cardinal1(quatre-vingts-dix-huit, 98).
convert_cardinal1(quatre-vingts-dix-neuf, 99).
convert_cardinal1(cent, 100).
convert_cardinal1(cents, 100).
convert_cardinal1(mille, '1.000').
convert_cardinal1(million, '1.000.000').
convert_cardinal1(millions, '1.000.000').
convert_cardinal1(milliard, '1.000.000.000').
convert_cardinal1(milliards, '1.000.000.000').

convert_approx_cardinal(dizaine, 10).
convert_approx_cardinal(douzaine, 12).
convert_approx_cardinal(vingtaine, 20).
convert_approx_cardinal(centaine, 100).
convert_approx_cardinal(millier, 1000).

convert_approx_cardinal_pl(dizaines, 10).
convert_approx_cardinal_pl(centaines, 100).
convert_approx_cardinal_pl(milliers, 1000).

% =====================================
% =          Ordinal Numbers          =
% =====================================

convert_ordinal(premier, 1).
convert_ordinal(première, 1).
convert_ordinal(deuxième, 2).
convert_ordinal(troisième, 3).
convert_ordinal(quatrième, 4).
convert_ordinal(cinquième, 5).
convert_ordinal(sixième, 6).
convert_ordinal(septième, 7).
convert_ordinal(huitième, 8).
convert_ordinal(neuvième, 9).
convert_ordinal(dixième, 10).

% =====================================
% =         Lefff Information         =
% =====================================

add_lefff_info(Features, Var, List) :-
	lefff_info(Info),
	add_lefff_info(Info, Features, Var, List).

add_lefff_info(none, _, _, []).
add_lefff_info(basic, Features, Var, List) :-
	add_lefff_info1(Features, Var, List).
add_lefff_info(all, Features, Var, List) :-
	add_lefff_info2(Features, Var, List).


add_lefff_info1(h, X, [appl(humain,X)]).
add_lefff_info1(hm, X, [appl(humain,X)]).
add_lefff_info1(hf, X, [appl(humain,X)]).
add_lefff_info1(hs, X, [appl(humain,X)]).
add_lefff_info1(hms, X, [appl(humain,X)]).
add_lefff_info1(hfs, X, [appl(humain,X)]).
add_lefff_info1(hp, X, [appl(humain,X)]).
add_lefff_info1(hmp, X, [appl(humain,X)]).
add_lefff_info1(hfp, X, [appl(humain,X)]).
add_lefff_info1(l, X, [appl(lieu,X)]).
add_lefff_info1(lm, X, [appl(lieu,X)]).
add_lefff_info1(lf, X, [appl(lieu,X)]).
add_lefff_info1(ls, X, [appl(lieu,X)]).
add_lefff_info1(lms, X, [appl(lieu,X)]).
add_lefff_info1(lfs, X, [appl(lieu,X)]).
add_lefff_info1(lp, X, [appl(lieu,X)]).
add_lefff_info1(lmp, X, [appl(lieu,X)]).
add_lefff_info1(lfp, X, [appl(lieu,X)]).
add_lefff_info1(m, _X, []).
add_lefff_info1(f, _X, []).
add_lefff_info1(mp, _X, []).
add_lefff_info1(fp, _X, []).
add_lefff_info1(ms, _X, []).
add_lefff_info1(fs, _X, []).

add_lefff_info2(h, X, [appl(humain,X)]).
add_lefff_info2(hm, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info2(hf, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info2(hs, X, [appl(humain,X)]).
add_lefff_info2(hms, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info2(hfs, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info2(hp, X, [appl(humain,X)]).
add_lefff_info2(hmp, X, [appl(humain,X),appl(masculin,X)]).
add_lefff_info2(hfp, X, [appl(humain,X),appl(féminin,X)]).
add_lefff_info2(l, X, [appl(lieu,X)]).
add_lefff_info2(lm, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info2(lf, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info2(ls, X, [appl(lieu,X)]).
add_lefff_info2(lms, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info2(lfs, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info2(lp, X, [appl(lieu,X)]).
add_lefff_info2(lmp, X, [appl(lieu,X),appl(masculin,X)]).
add_lefff_info2(lfp, X, [appl(lieu,X),appl(féminin,X)]).
add_lefff_info2(m, X, [appl(masculin,X)]).
add_lefff_info2(f, X, [appl(féminin,X)]).
add_lefff_info2(mp, X, [appl(masculin,X)]).
add_lefff_info2(fp, X, [appl(féminin,X)]).
add_lefff_info2(ms, X, [appl(masculin,X)]).
add_lefff_info2(fs, X, [appl(féminin,X)]).


add_info('Pyrénées', X, [appl(lieu,X),appl(mountain_range,X)|L], L) :-
	!.
add_info('Mont-Perdu', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Maladetta', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Maladette', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Vignemale', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Pimené', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Pique', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Crabère', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Maboré', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Aiguillons', X, [appl(lieu,X),appl(mountain,X)|L], L) :-
	!.
add_info('Corunes', X, [appl(lieu,X),appl(lac,X)|L], L) :-
	!.
add_info(cirque, X, [appl(lieu,X)|L], L) :-
	!.
add_info('Cirque', X, [appl(lieu,X)|L], L) :-
	!.
add_info(col, X, [appl(lieu,X)|L], L) :-
	!.
add_info(mont, X, [appl(lieu,X)|L], L) :-
	!.
add_info(monts, X, [appl(lieu,X)|L], L) :-
	!.
add_info('Mont', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Monts', X, [appl(lieu,X)|L], L) :-
	!.
add_info(port, X, [appl(lieu,X)|L], L) :-
	!.
add_info(ports, X, [appl(lieu,X)|L], L) :-
	!.
add_info('Port', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Ports', X, [appl(lieu,X)|L], L) :-
	!.
add_info(fort, X, [appl(lieu,X)|L], L) :-
	!.
add_info(forts, X, [appl(lieu,X)|L], L) :-
	!.
add_info('fort', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Forts', X, [appl(lieu,X)|L], L) :-
	!.
add_info(lac, X, [appl(lieu,X)|L], L) :-
	!.
add_info(lacs, X, [appl(lieu,X)|L], L) :-
	!.
add_info('Lac', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Lacs', X, [appl(lieu,X)|L], L) :-
	!.
add_info('vallée', X, [appl(lieu,X)|L], L) :-
	!.
add_info(vallon, X, [appl(lieu,X)|L], L) :-
	!.
add_info(val, X, [appl(lieu,X)|L], L) :-
	!.
add_info('Vallée', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Vallon', X, [appl(lieu,X)|L], L) :-
	!.
add_info('Val', X, [appl(lieu,X)|L], L) :-
	!.
add_info(_, _, L, L).


convert_quantifier(nul, lambda(P,lambda(Q,drs([],[bool(merge(drs([variable(X)],[]),appl(P,X)),->,not(appl(Q,X)))])))).
convert_quantifier(nulle, lambda(P,lambda(Q,drs([],[bool(merge(drs([variable(X)],[]),appl(P,X)),->,not(appl(Q,X)))])))).
convert_quantifier(quelque, lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))).

convert_quantifier_adj(nulle, lambda(P,lambda(X,drs([],[not(appl(P,X))])))).
convert_quantifier_adj(quelque, lambda(P, lambda(X, appl(P,X)))).

is_temporal_unit(seconde).
is_temporal_unit(minute).
is_temporal_unit(heure).
is_temporal_unit(jour).
is_temporal_unit(semaine).
is_temporal_unit(an).

% = new sequence_semantics

sequence_semantics([PE, X, Y], Pendant, I0, I, dl(1,lit(s(_)),lit(s(_))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(sub(mésure,LTF),appl(temps,E)),=,Num)]))))) :-
	chart:word(PE, _, _, I0, I1),
     (  PE = pendant ; PE = en ),
	chart:word(X, num, LN, I1, I2),
	chart:word(Y, _, LT, I2, I),
	convert_cardinal(LN, Num),
      (
         Y = mois
      ->
	 LTF = mois
      ;
	 is_temporal_unit(LT),
         LTF = LT
      ),
	concat_atom([PE,LT,Num], Pendant).

sequence_semantics([secrétaire, général], secrétaire_général, _, _, lit(n), lambda(X,drs([],[appl('secrétaire-général',X)]))).
sequence_semantics([secrétaire, général], secrétaire_général, _, _, lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[appl('secrétaire-général',X)]),appl(P,X)))).
sequence_semantics([président,de,la,'République'], 'président_de_la_République', _, _, lit(n), lambda(X,drs([],[appl(président_de_la_République,X)]))).
sequence_semantics([président,de,la,'République'], 'président_de_la_République', _, _, lit(np(_,_,_)), lambda(P,merge(drs([event(X)],[appl(président_de_la_République,X)]),appl(P,X)))).
sequence_semantics([W,bon,train], aller_bon_train, I, _, dl(0,lit(np(_,_,_)),lit(s(_))), lambda(NP,lambda(E,appl(NP,lambda(X,drs([],[appl(appl(aller_bon_train,X),E)])))))) :-
	chart:word(W, _, aller, I, _).
sequence_semantics([ETRE,en,train,DE|_INF0], être_en_train_de, I, K, dl(0,lit(np(_,_,_)),lit(s(_))), lambda(NP,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Conds),merge(drs([event(F)],[bool(E,subseteq,F)]),appl(appl(INF,lambda(Prp,appl(Prp,X))),F)))))))) :-
	chart:word(ETRE, ver:TNS, être, I, _),
	( DE = de ; DE = 'd\'' ),
	chart:word(train, _, _, _, J1),
	chart:word(DE, _, _, J1, J),
	chart:stored(_, _, J, K, dl(0,lit(np(_,_,_)),lit(s(inf(_)))), data(_, INF, _, [], [], [])),
	pos_time(ver:TNS, [], EVs, E-Conds).
sequence_semantics(['d\'',ailleurs], d_ailleurs, _, _, dl(1,lit(s(_)),lit(s(_))), lambda(S,lambda(E,merge(drs([],[appl(d_ailleurs,E)]),appl(S,E))))).



sequence_semantics(['Dans',_,_|Ws], Ws, [prp,num,nom|Ps], Ps, [_,N,Tmp|Ls], Ls, [dr(0,dr(0,lit(s(X)),lit(s(X))),lit(np(N1,N2,N3))),dr(0,lit(np(N1,N2,N3)),lit(n)),lit(n)|Fs], Fs, [dr(0,dr(0,lit(s(X)),lit(s(X))),lit(np(N1,N2,N3)))-lambda(_,lambda(S,lambda(E,merge(appl(S,E),drs([event(E2)],[bool(appl(sub(mésure,Tmp),E2),=,Num),bool(E2,abuts,E)]))))),dr(0,lit(np(N1,N2,N3)),lit(n))-_,lit(n)-_|Ss], Ss, dr(0,lit(s(X)),lit(s(X)))-lambda(S,lambda(E,merge(appl(S,E),drs([event(E2)],[bool(appl(sub(mésure,Tmp),E2),=,Num),bool(E2,abuts,E)]))))) :-
	is_temporal_unit(Tmp),
	convert_ordinal(N,Num),
	!,
	format(user_error, 'DANS ~w ~w~n', [N,Tmp]).
								   
% "il est probable que ..."
%sequence_semantics([_,W|Ws], [W|Ws], [pro:per,P|Ps], Ps, [il,L|Ls], Ls, [lit(np(_,_,_)),F|Fs], [F|Fs], [lit(np(_,_,_))-_,S0|Ss0], [S|Ss]) :-
%	process_verb_adjective(P, Ps, W, Ws, L, Ls, F, Fs, S0, Ss0, S,Ss).

sequence_semantics([Q,part|Ws], Ws, [adj,nom|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),lit(n)|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-QS,lit(n)-lambda(X,drs([],[appl(lieu,X)]))|Ss], Ss) :-
	convert_quantifier(Q,QS),
	!,
	write(user_error, 'SEQUENCE: nulle/quelque part').
sequence_semantics([Q,part|Ws], Ws, [adj,nom|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(n),lit(n)),lit(n)|Fs], Fs, [dr(0,lit(n),lit(n))-AS,lit(n)-lambda(X,drs([],[appl(lieu,X)]))|Ss], Ss) :-
	convert_quantifier_adj(Q,AS),
	!,
	write(user_error, 'SEQUENCE: nulle/quelque part').

sequence_semantics([_,et,_|Ws], Ws, [num,kon,num|Ps], Ps, [A,_,B|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),dr(0,dl(0,dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(np(_,_,_)),lit(n))),dr(0,lit(np(_,_,_)),lit(n))),dr(0,lit(np(_,_,_)),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(Z,Z),dr(0,dl(0,dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(np(_,_,_)),lit(n))),dr(0,lit(np(_,_,_)),lit(n)))-lambda(V,V),dr(0,lit(np(_,_,_)),lit(n))-lambda(R,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(R,X)),appl(Q,X))))|Ss], Ss) :-
	convert_cardinal(B, 1),
	convert_cardinal(A, N0),
	!,
	number(N0),
	Num is N0 + 1.
% "fait-n que-(n\n)/s_q" has the presupposition that the que-phrase is true
sequence_semantics([_,_|Ws], Ws, [nom,_|Ps], Ps, [fait,que|Ls], Ls, [lit(n),dr(0,dl(0,lit(n),lit(n)),lit(s(q)))|Fs], Fs, [lit(n)-lambda(X,drs([],[appl(fait,X)])),dr(0,dl(0,lit(n),lit(n)),lit(s(q)))-lambda(SQ,lambda(CN,lambda(Y,presup(merge(drs([event(E)],[]),appl(SQ,E)),merge(appl(CN,Y),drs([event(Lab)],[appl(appl(contenu,Lab),Y),drs_label(Lab,appl(SQ,E))]))))))|Ss], Ss).
% "l'on"
sequence_semantics([LAp,on|Ws], Ws, [det:art,pro:per|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(np(_,_,_)))-lambda(Z,Z),lit(np(_,_,_))-lambda(P,merge(drs([X],[]),appl(P,X)))|Ss], Ss) :-
	is_l_apostrophe(LAp),
	!.
% "l'un des"
sequence_semantics([LAp,Un,des|Ws], Ws, [det:art,_,prp:det|Ps], Ps, [_,_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),lit(n),dr(0,dl(0,lit(n),lit(n)),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-Sem,lit(n)-lambda(_,drs([],[])),dr(0,dl(0,lit(n),lit(n)),lit(n))-lambda(P1,lambda(_,lambda(X,appl(P1,X))))|Ss], Ss) :-
	is_l_apostrophe(LAp),
	is_un_une(Un),
	!,
	gq_l_un_des_semantics(Sem).
% NUM des
% (e->t)->t
sequence_semantics([W,des|Ws], Ws, [num,prp:det|Ps], Ps, [_,_|Ls], Ls, [lit(np(_,_,_)),dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n))|Fs], Fs, [lit(np(_,_,_))-lambda(_,drs([],[])),dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n))-lambda(N1,lambda(_,lambda(P1,presup(merge(drs([variable(X),variable(Y)],[bool(Y,=,?),bool(X,subsetneq,Y),bool(num(X),=,Num)]),appl(N1,Y)),appl(P1,X)))))|Ss], Ss) :-
	convert_cardinal(W,Num),
	!.
% dizaine/douzaine/centaine de
% (e->t)->t
sequence_semantics([W,de|Ws], Ws, [_,prp|Ps], Ps, [_,_|Ls], Ls, [lit(n),dr(0,dl(0,lit(n),lit(n)),lit(n))|Fs], Fs, [lit(n)-lambda(X,drs([],[bool(num(X),approx,Num)])),dr(0,dl(0,lit(n),lit(n)),lit(n))-lambda(N1,lambda(N2,lambda(Y,merge(appl(N1,Y),appl(N2,Y)))))|Ss], Ss) :-
	convert_approx_cardinal(W,Num),
	!.
% 
sequence_semantics([W,de|Ws], Ws, [_,prp|Ps], Ps, [_,_|Ls], Ls, [lit(n),dr(0,dl(0,lit(n),lit(n)),lit(n))|Fs], Fs, [lit(n)-lambda(X,drs([],[bool(num(X),>,Num)])),dr(0,dl(0,lit(n),lit(n)),lit(n))-lambda(N1,lambda(N2,lambda(Y,merge(appl(N1,Y),appl(N2,Y)))))|Ss], Ss) :-
	convert_approx_cardinal_pl(W,Num0),
	!,
	Num is 2*Num0.

% "l'un de" (mes/ses...)
% TODO this is a bit ugly, find a way to do without explicit identity condition for X and Y
sequence_semantics([LAp,Un,de|Ws], Ws, [det:art,_,prp|Ps], Ps, [_,_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),lit(n),dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-Sem,lit(n)-lambda(_,drs([],[])),dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_)))-lambda(P1,lambda(_,lambda(X,appl(P1,lambda(Y,drs([],[bool(X,=,Y)]))))))|Ss], Ss) :-
	is_l_apostrophe(LAp),
	is_un_une(Un),
	!,
	gq_l_un_de_semantics(Sem).
% "l'un"
sequence_semantics([LAp,Un,Next|Ws], [Next|Ws], [det:art,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),lit(n)|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(_,lambda(P1,presup(drs([Y],[bool(Y,=,?),bool(num(Y),>,1)]),merge(drs([X],[bool(X,atomic_sub,Y),appl(Gender,X)]),appl(P1,X))))),lit(n)-lambda(_,drs([],[]))|Ss], Ss) :-
	Next \== de,
	Next \== des,
	is_l_apostrophe(LAp),
	is_un_une(Un),
	!,
    (
        Un = un
    ->
        Gender = masculin
    ;
        Gender = feminin
    ).
     
sequence_semantics([_,_,_|Ws], Ws, [prp,nom,_|Ps], Ps, [par,rapport,_|Ls], Ls, [dr(0,A,lit(n)),dr(0,lit(n),lit(pp(à))),dr(0,lit(pp(à)),lit(n))|Fs], Fs, [dr(0,A,lit(n))-Sem,dr(0,lit(n),lit(pp(à)))-lambda(Y,lambda(X,appl(Y,X))),dr(0,lit(pp(n),lit(n)))-lambda(Z,Z)|Ss], Ss) :-
	default_semantics(par_rapport_à, prp, dr(0,A,lit(n)), Sem).
sequence_semantics([_,_,_|Ws], Ws, [prp,nom,_|Ps], Ps, [par,rapport,_|Ls], Ls, [dr(0,A,lit(n)),dr(0,lit(n),lit(pp(à))),dr(0,lit(pp(à)),lit(np(_,_,_)))|Fs], Fs, [dr(0,A,lit(np(_,_,_)))-Sem,dr(0,lit(np(_,_,_)),lit(pp(à)))-lambda(P,lambda(X,appl(P,X))),dr(0,lit(pp(n),lit(np(_,_,_))))-lambda(Z,Z)|Ss], Ss) :-
	default_semantics(par_rapport_à, prp, dr(0,A,lit(np(_,_,_))), Sem).
sequence_semantics([environ,W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(n),lit(n))-lambda(X,X),dr(0,lit(n),lit(n))-lambda(P,lambda(V, merge(drs([],[bool(appl(num,V),approx,Num)]),appl(P,V))))|Ss], Ss) :-
	convert_cardinal(W, Num).
sequence_semantics([environ,W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,lit(np(_,_,_)),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(np(_,_,_)))-lambda(X,X),dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),approx,Num)]),appl(P,X)),appl(Q,X))))|Ss], Ss) :-
	convert_cardinal(W, Num).
sequence_semantics(['Environ',W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(n),lit(n))-lambda(X,X),dr(0,lit(n),lit(n))-lambda(P,lambda(V, merge(drs([],[bool(appl(num,V),approx,Num)]),appl(P,V))))|Ss], Ss) :-
	convert_cardinal(W, Num).
sequence_semantics(['Environ',W|Ws], Ws, [_,num|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,lit(np(_,_,_)),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(np(_,_,_)))-lambda(X,X),dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),approx,Num)]),appl(P,X)),appl(Q,X))))|Ss], Ss) :-
	convert_cardinal(W, Num).
% days "le 14"
%sequence_semantics([le,W|Ws], Ws, [_,P|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,dl(1,lit(s),lit(s)),lit(n))-lambda(N,lambda(S,lambda(E,merge(appl(N,E),appl(S,E)))))-lambda(P,lambda(Y,merge(drs([],[Condition]),appl(P,Y))))|Ss], Ss) :-
%	is_date_indication(P, W, Y, Condition).
% days "le mercredi"
%sequence_semantics([le,W|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)),lit(n)|Fs], Fs, [dr(0,dl(1,lit(s),lit(s)),lit(n))-lambda(V,V),lit(n)-lambda(P,lambda(Y,merge(drs([variable(X)],[Condition]),appl(P,Y))))|Ss], Ss) :-
%	is_date_indication(P, W, Condition).
%gq_a_semantics(lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))).
% "tous/toutes les"
sequence_semantics([Tout,les|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,lit(np(_,_,_)),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(np(_,_,_)))-lambda(Z,Z),dr(0,lit(np(_,_,_)),lit(n))-Sem|Ss], Ss) :-
	is_tout(Tout),
	!,
	format(user_error, 'SEQUENCE: ~w les~n', [Tout]),
	gq_every_semantics_bis(Sem).
% "Le meilleur/Les meilleurs"
sequence_semantics([Le,Meilleur|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,presup(presup(merge(drs([variable(Y)],[]),appl(P,Y)),drs([event(E1),event(E2)],[not(drs([variable(Z)],[drs_label(E1,appl(P,Y)),drs_label(E2,appl(P,Z)),bool(appl(mésure,appl(bon,E2)),>,appl(mésure,appl(bon,E1)))]))])),appl(Q,Y)))),dr(0,lit(n),lit(n))-lambda(X,X)|Ss], Ss) :-
	is_le_la(Le),
	is_meilleur(Meilleur),
	!.
sequence_semantics([Les,Meilleurs|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,presup(presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(P,Y)),drs([event(E1),event(E2)],[not(drs([variable(Z),variable(Y1)],[bool(Z,empty_intersect,Y),bool(Y1,atomic_sub,Y),drs_label(E1,appl(P,Y)),drs_label(E2,appl(P,Z)),bool(appl(mésure,appl(bon,E2)),>,appl(mésure,appl(bon,E1)))]))])),appl(Q,Y)))),dr(0,lit(n),lit(n))-lambda(X,X)|Ss], Ss) :-
	is_les(Les),
	is_meilleurs(Meilleurs),
	!.
% "Le pire/Les pires"
sequence_semantics([Le,pire|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,presup(presup(merge(drs([variable(Y)],[]),appl(P,Y)),drs([event(E1),event(E2)],[not(drs([variable(Z)],[drs_label(E1,appl(P,Y)),drs_label(E2,appl(P,Z)),bool(appl(mésure,appl(bon,E2)),<,appl(mésure,appl(bon,E1)))]))])),appl(Q,Y)))),dr(0,lit(n),lit(n))-lambda(X,X)|Ss], Ss) :-
	is_le_la(Le),
	!.
sequence_semantics([Les,pires|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(n),lit(n))|Fs], Fs, [dr(0,lit(np(_,_,_)),lit(n))-lambda(P,lambda(Q,presup(presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(P,Y)),drs([event(E1),event(E2)],[not(drs([variable(Z),variable(Y1)],[bool(Z,empty_intersect,Y),bool(Y1,atomic_sub,Y),drs_label(E1,appl(P,Y)),drs_label(E2,appl(P,Z)),bool(appl(mésure,appl(bon,E2)),<,appl(mésure,appl(bon,E1)))]))])),appl(Q,Y)))),dr(0,lit(n),lit(n))-lambda(X,X)|Ss], Ss) :-
	is_les(Les),
	!.
% "Le plus/les plus" use adjectival
% P (e->t)->(e->t)
% Q e->t
sequence_semantics([Le,plus|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(P,lambda(Q,lambda(Y,presup(drs([event(E1),event(E2)],[not(drs([variable(Z)],[drs_label(E1,appl(appl(P,Q),Y)),drs_label(E2,appl(appl(P,Q),Z)),bool(appl(mésure,E2),>,appl(mésure,E1))]))]),appl(Q,Y))))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X1,X1)|Ss], Ss) :-
	is_le_la(Le),
	!.
sequence_semantics([Les,plus|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(P,lambda(Q,lambda(Y,presup(drs([event(E1),event(E2)],[not(drs([variable(Z),variable(Y1)],[bool(Z,empty_intersect,Y),bool(Y1,atomic_sub,Y),drs_label(E1,appl(appl(P,Q),Y1)),drs_label(E2,appl(appl(P,Q),Z)),bool(appl(mésure,E2),>,appl(mésure,E1))]))]),appl(Q,Y))))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X1,X1)|Ss], Ss) :-
	is_les(Les),
	!.
% "Le plus/les plus" use adjectival
% P (e->t)->(e->t)
% Q e->t
sequence_semantics([Le,plus|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dr(0,dl(0,lit(n),lit(n)),lit(pp(de))),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(P,lambda(Q,lambda(Y,presup(drs([event(E1),event(E2)],[not(drs([variable(Z)],[drs_label(E1,appl(appl(P,Q),Y)),drs_label(E2,appl(appl(P,Q),Z)),bool(appl(mésure,E2),>,appl(mésure,E1))]))]),appl(Q,Y))))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X1,X1)|Ss], Ss) :-
	is_le_la(Le),
	!.
% "Le moins/les moins" use adjectival
% P (e->t)->(e->t)
% Q e->t
sequence_semantics([Le,moins|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(P,lambda(Q,lambda(Y,presup(drs([event(E1),event(E2)],[not(drs([variable(Z)],[drs_label(E1,appl(appl(P,Q),Y)),drs_label(E2,appl(appl(P,Q),Z)),bool(appl(mésure,E2),<,appl(mésure,E1))]))]),appl(Q,Y))))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X1,X1)|Ss], Ss) :-
	is_le_la(Le),
	!.
sequence_semantics([Les,moins|Ws], Ws, [_,_|Ps], Ps, [_,_|Ls], Ls, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))|Fs], Fs, [dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(P,lambda(Q,lambda(Y,presup(drs([event(E1),event(E2)],[not(merge(appl(Q,Z),drs([variable(Z),variable(Y1)],[bool(Z,empty_intersect,Y),bool(Y1,atomic_sub,Y),drs_label(E1,appl(appl(P,Q),Y1)),drs_label(E2,appl(appl(P,Q),Z)),bool(appl(mésure,E2),<,appl(mésure,E1))])))]),appl(Q,Y))))),dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n)))-lambda(X1,X1)|Ss], Ss) :-
	is_les(Les),
	!.
% "à Num heure(s)"
% TODO: Fix this
sequence_semantics([à,Num,_|Ws], Ws, [prp,num,_|Ps], Ps, [_,_,heure|Ls], Ls, [dr(0,dl(1,lit(s(_)),lit(s(_))),lit(np(_,_,_))),dr(0,lit(np(_,_,_)),lit(n)),lit(n)|Fs], Fs, [dl(1,lit(s(ST)),lit(s(ST)))-lambda(E,lambda(S,merge(drs([],[bool(appl(temps,E),=,appl(Card,h))]),appl(S,E))))|Ss], Ss) :-
	convert_cardinal(Num, Card),
	!.

% gq_the_semantics(lambda(P,lambda(Q,presup(merge(drs([variable(X),variable(Y)],[bool(Y,=,?),bool(num(Y),=,1),bool(X,in,Y)]),appl(P,X)),appl(Q,X))))).


is_date_indication(num, W, X, appl(appl(jour,Num),X)) :-
	convert_cardinal(W, Num),
	Num > 0,
	Num < 32,
	!.
is_date_indication(_, W, X, appl(appl(jour_de_la_semaine,W),X)) :-
	is_weekday(W).

% = matches the atoms "l'" and "L'"

is_l_apostrophe('l\'').
is_l_apostrophe('L\'').

is_un_une(un).
is_un_une(une).

is_meilleur(meilleur).
is_meilleur(meilleure).

is_meilleurs(meilleurs).
is_meilleurs(meilleures).

is_le_la(le).
is_le_la(la).
is_le_la('Le').
is_le_la('La').

is_les(les).
is_les('Les').


is_tout(tout).
is_tout(tous).
is_tout(toutes).
is_tout(toute).
is_tout('Tout').
is_tout('Tous').
is_tout('Toutes').
is_tout('Toute').

% = factive verbs
% TODO: verify and complete
% NOTE: some of these ("se souvenir" at least) are sensitive
% to the aspect (subjunctive or not) of the "que" complement,
% where subjunctive can prevent factivity in the context of
% negation eg.
%
% "Pierre se souvient pas que Marie soit venue"
%
% (example 5) from
%
% Paul Egré (2005), Question-Embedding and Factivity, Journées
% Sémantique et Modélisation 2005, Paris.
%
% Handling these cases will require a more subtle treatement
% both syntactically (right now the "s(q)" atomic formula
% is indifferent to whether the sentence is in indicative
% or subjunctive mood) and semantically.

% Note: "voir", "entendre", "sentir" are not truly factive,
% since they permit "deduction" of the that-clause. 
%
% 1. Jean a vu que Marie a renversé sa tasse de café
% 2. Jean a vu Marie renverser sa tasse de café
%
% (1) can be said in case Jean has just seen evidence of
% the coffee being spilt (on her desk, for example), whereas
% (2) requires Jean to have seen the actual action of
% spilling the coffee.
%
% However, as a simplification of the actual facts, we will
% treat these verbs as factive.

factive(admettre).
factive(apprendre).
factive(avouer).
factive(cacher).
factive(comprendre).
factive(constater).
factive(decouvrir).
factive(découvrir).
factive(déplorer).
factive(entendre).
factive(montrer).
factive(observer).
factive(oublier).
factive(rappeler).
factive(reconnaître).
factive(regretter).
factive(révéler).
factive(savoir).
factive(sentir).
factive(voir).
factive(se_souvenir).
factive(se_apercevoir).


% = prefixed adjectives which are subsective
% TODO: verify and complete
% QUESTION: are there any which are truly intersective?
% or intersective in some sort of more subtle way?
% TODO: continue verification, but make subsective the
% base case for prefixed adjectives

% non-subsective non-privative and others
% apparente
% chaud (?)
% demi
% double
% financière (?)
% prochain (?) prochain président != président, prochain jour = jour
% précédent
% complex cases:
% autre !
% seul !
% tel !

prefixed_nonsubsective_adjective(apparent).  % meaning B of TLFI is non-subsective
prefixed_nonsubsective_adjective(éventuel).
prefixed_nonsubsective_adjective(possible).
prefixed_nonsubsective_adjective(présumé).
prefixed_nonsubsective_adjective(probable).
prefixed_nonsubsective_adjective(prochain). % ?

% = prefixed adjectives which are privative
% TODO: verify and complete

prefixed_privative_adjective(faux).
prefixed_privative_adjective(imaginaire).
prefixed_privative_adjective(prétendu).

% = postfixed adjectives which are privative
% TODO: verify and complete

postfixed_privative_adjective(fictif).
postfixed_privative_adjective(imaginaire).

% = postfixed adjective which are not intersective but subsective

postfixed_subsective_adjective(bas).
postfixed_subsective_adjective(difficile).
postfixed_subsective_adjective(facile).
postfixed_subsective_adjective(grand).
postfixed_subsective_adjective(habile).
postfixed_subsective_adjective(influent).
postfixed_subsective_adjective(intéressant).
postfixed_subsective_adjective(irrésistible).
postfixed_subsective_adjective(immense).
postfixed_subsective_adjective(jeune).
postfixed_subsective_adjective(juste).
postfixed_subsective_adjective(haut).
postfixed_subsective_adjective(large).
postfixed_subsective_adjective(lent).
postfixed_subsective_adjective(lointain).
postfixed_subsective_adjective(long).
postfixed_subsective_adjective(lourd).
postfixed_subsective_adjective(légendaire).
postfixed_subsective_adjective(léger).
postfixed_subsective_adjective(magnifique).
postfixed_subsective_adjective(maigre).
postfixed_subsective_adjective(mauvais).
postfixed_subsective_adjective(petit).


temporal_adverb(X) :-
	is_weekday(X).
temporal_adverb(hier).
temporal_adverb('aujourd\'hui').
temporal_adverb(demain).

% = weekdays

is_weekday(lundi).
is_weekday(mardi).
is_weekday(mercredi).
is_weekday(jeudi).
is_weekday(vendredi).
is_weekday(samedi).
is_weekday(dimanch).

is_month(janvier).
is_month(février).
is_month(fevrier).
is_month(mars).
is_month(avril).
is_month(mai).
is_month(juin).
is_month(juillet).
is_month(aout).
is_month(août).
is_month(septembre).
is_month(octobre).
is_month(novembre).
is_month(décembre).
is_month(decembre).

is_season(hiver).
is_season(printemps).
is_season(été).
is_season(automne).

is_daypart(nuit).
is_daypart(matin).
is_daypart('après-midi').
is_daypart(après-midi).
is_daypart(soir).

% = add roles

add_roles(L, P, E, R0, R) :-
	event_semantics(prolog),
	!,
	add_roles_predicate(L, P, E, R0, R).

add_roles(L, P, E, R0, R) :-
	event_semantics(classic),
	!,
	R0 = [appl(T,E)|R],
	add_roles_classic(L, P, T).

add_roles(L, P, E, R0, R) :-
	event_semantics(neo),
	!,
	add_roles_neo(L, P, E, R0, R).

add_roles_neo(L, P, E) -->
	[appl(P,E)],
	add_roles_neo_args(L, E).

add_roles_neo_args([], _) -->
	[].
add_roles_neo_args([R-V|Rest], E) -->
	[appl(appl(R,V),E)],
	add_roles_neo_args(Rest, E).

add_roles_classic([], P, P).
add_roles_classic([_-A|Rest], P, appl(T,A)) :-
	add_roles_classic(Rest, P, T).

		  
add_roles_predicate(L0, P, E) -->
	{strip_keys(L0, L),
	T =.. [P,E|L]},
	[T].


% =====================================
% =  Default semantics with POS tag   =
% =====================================


% = months and weekdays

default_semantics(siecle, nom, lit(n), lambda(X,drs([],[appl(appl(temps,siecle),X)]))).
default_semantics(siecle, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,siecle),X)]))).
default_semantics(décennie, nom, lit(n), lambda(X,drs([],[appl(appl(temps,décennie),X)]))).
default_semantics(décennie, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,décennie),X)]))).
default_semantics(an, nom, lit(n), lambda(X,drs([],[appl(appl(temps,an),X)]))).
default_semantics(an, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,an),X)]))).
default_semantics(année, nom, lit(n), lambda(X,drs([],[appl(appl(temps,an),X)]))).
default_semantics(année, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,an),X)]))).
default_semantics(mois, nom, lit(n), lambda(X,drs([],[appl(appl(temps,mois),X)]))).
default_semantics(mois, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,mois),X)]))).
default_semantics(semaine, nom, lit(n), lambda(X,drs([],[appl(appl(temps,semaine),X)]))).
default_semantics(semaine, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,semaine),X)]))).
default_semantics(jour, nom, lit(n), lambda(X,drs([],[appl(appl(temps,jour),X)]))).
default_semantics(jour, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,jour),X)]))).
default_semantics(journée, nom, lit(n), lambda(X,drs([],[appl(appl(temps,jour),X)]))).
default_semantics(journée, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,jour),X)]))).
default_semantics(heure, nom, lit(n), lambda(X,drs([],[appl(appl(temps,heure),X)]))).
default_semantics(heure, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,heure),X)]))).
default_semantics(minute, nom, lit(n), lambda(X,drs([],[appl(appl(temps,minute),X)]))).
default_semantics(minute, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,minute),X)]))).
default_semantics(seconde, nom, lit(n), lambda(X,drs([],[appl(appl(temps,seconde),X)]))).
default_semantics(seconde, nom:_, lit(n), lambda(X,drs([],[appl(appl(temps,seconde),X)]))).

default_semantics(Season, nom, lit(n), lambda(X,drs([],[appl(appl(saison,Season),X)]))) :-
	is_season(Season).
default_semantics(Season, nom:_, lit(n), lambda(X,drs([],[appl(appl(saison,Season),X)]))) :-
	is_season(Season).
default_semantics(Month, nom, lit(n), lambda(X,drs([],[appl(appl(mois,Month),X)]))) :-
	is_month(Month).
default_semantics(Month, nom:_, lit(n), lambda(X,drs([],[appl(appl(mois,Month),X)]))) :-
	is_month(Month).
default_semantics(Weekday, nom, lit(n), lambda(X,drs([],[appl(appl(jour_de_la_semaine,Weekday),X)]))) :-
	is_weekday(Weekday).
default_semantics(Weekday, nom:_, lit(n), lambda(X,drs([],[appl(appl(jour_de_la_semaine,Weekday),X)]))) :-
	is_weekday(Weekday).
default_semantics(Daypart, nom, lit(n), lambda(X,drs([],[appl(appl(partie_de_la_journée,Daypart),X)]))) :-
	is_daypart(Daypart).
default_semantics(Daypart, nom:_, lit(n), lambda(X,drs([],[appl(appl(partie_de_la_journée,Daypart),X)]))) :-
	is_daypart(Daypart).

% = numbers

default_semantics(W, num, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(appl(rank,V),=,Num)]),appl(P,V))))) :-
	convert_ordinal(W, Num).
default_semantics(W, num, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_cardinal(W, Num).
default_semantics(W, num, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_cardinal(W, Num).
default_semantics(W, num, lit(n), lambda(X,drs([],[bool(X,=,Num)]))) :-
	convert_cardinal(W, Num).
default_semantics(W, num, lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[bool(X,=,Num)]),appl(P,X)))) :-
	convert_cardinal(W, Num).
default_semantics(W, num, dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(appl(Word,Num),X)]))))))) :- 
	convert_cardinal(W, Num),
    (
        Num > 1900
    ->
        Word = édition
    ;
        Word = numéro
    ).

default_semantics(W, adj, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_cardinal1(W, Num).
default_semantics(W, adj, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_cardinal1(W, Num).
default_semantics(W, num, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_cardinal1(W, Num).
default_semantics(W, adj, lit(n), lambda(X,drs([],[bool(num(X),=,Num)]))) :-
	convert_cardinal1(W, Num).
default_semantics(W, nom, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),=,Num)]),appl(P,X)),appl(Q,X))))) :-
	convert_cardinal1(W, Num).
default_semantics(W, nom, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[bool(num(V),=,Num)]),appl(P,V))))) :-
	convert_cardinal1(W, Num).
default_semantics(W, nom, lit(n), lambda(X,drs([],[bool(num(X),=,Num)]))) :-
	convert_cardinal1(W, Num).

% = nouns
% = NOM

default_semantics(W, nom, lit(n), lambda(X,drs([],[appl(W,X)|Rest]))) :-
	add_info(W, X, Rest, []).
default_semantics(W, nom:INFO, lit(n), lambda(X,drs([],[appl(W,X)|Rest0]))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(W, X, Rest0, Rest).

% = NAM

default_semantics(Word, nam, lit(n), lambda(X,presup(drs([],[appl(appl(nommé,Word),X)]),drs([],[])))).
default_semantics(Word, nam:INFO, lit(n), lambda(X,presup(drs([],[appl(appl(nommé,Word),X)|Rest0]),drs([],[])))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).

% = noun phrase - NAM (proper name)

default_semantics(Word, nam, lit(np(_,_,_)), lambda(P,presup(drs([variable(X)],[appl(appl(nommé,Word),X)]),appl(P,X)))).
default_semantics(Word, nam, dr(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)]))),appl(P,X))))).
default_semantics(Word, nam, dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)]))),appl(P,X))))).
default_semantics(Word, nam:INFO, lit(np(_,_,_)), lambda(P,presup(drs([variable(X)],[appl(appl(nommé,Word),X)|Rest0]),appl(P,X)))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).
default_semantics(Word, nam:INFO, dr(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,presup(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)|Rest0]))),appl(P,X))))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).
default_semantics(Word, nam:INFO, dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([],[appl(appl(nommé,Word),X)|Rest0]))),appl(P,X))))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).
% names used as epithets
default_semantics(Word, nam, dl(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[appl(appl(nommé,Word),X)|Rest]),appl(P,X))))) :-
	add_info(Word, X, [], Rest).
default_semantics(Word, nam, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[appl(appl(nommé,Word),X)|Rest]),appl(P,X))))) :-
	add_info(Word, X, [], Rest).
default_semantics(Word, nam:INFO, dl(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[appl(appl(nommé,Word),X)|Rest0]),appl(P,X))))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).
default_semantics(Word, nam:INFO, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[appl(appl(nommé,Word),X)|Rest0]),appl(P,X))))) :-
	add_lefff_info(INFO, X, Rest),
	add_info(Word, X, Rest0, Rest).

% = verbs

% "se faire" passive

default_semantics(faire, ver:TIME, dr(_,dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(_)))),dl(0,lit(np(_,_,_)),lit(s(INF)))), lambda(INF,lambda(_SE,lambda(NP,lambda(E,appl(NP,lambda(Y,presup(drs(Es,Tnse),merge([variable(X),event(L)],Conds))))))))) :-
	nonvar(INF),
	INF = inf(_),
	add_roles([agent-X,patient-Y,theme-L], se_faire, E, Conds, [drs_label(L,appl(INF,lambda(Prp,appl(Prp,X))))]),
	pos_time(ver:TIME, [], Es, E-Tnse).														

% passive (tagged as past participle, should correct some POS-tag errors)

default_semantics(Word, ver:pper, dr(0,dl(0,lit(np(_,_,_)),lit(s(ppart))),lit(pp(par))), lambda(SUJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUJ,lambda(X,drs([],[appl(event,E)|Conds]))))))))) :-
	role_lexicon(Word, Role1, Role2),
	add_roles([Role1-X,Role2-Y], Word, E, Conds, []).


% = "être + passive"
% "être" only provide tense information, the passive itself takes care of role information

default_semantics(être, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(PASS)))), lambda(VP,lambda(NP,lambda(E,presup(drs(Es,Cs),appl(appl(VP,NP),E)))))) :-
	PASS == pass,
	pos_time(ver:TIME, [], Es, E-Cs).


default_semantics(V, _POS, dl(0,lit(np(_,_,_)),lit(s(PASS))), lambda(OBJ,lambda(E,appl(OBJ,lambda(Y, drs([variable(X)],[bool(X,=,'context?')|Conds])))))) :-
	PASS == pass,
	role_lexicon(V, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], V, E, Conds, []).
% some verbs with par-complements which are not the subject (normally these should not get the passive supertag)
default_semantics(multiplier, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(PAR))), lambda(PP,lambda(OBJ,lambda(E,appl(PP,lambda(Z,appl(OBJ,lambda(Y,drs([variable(X)],[bool(X,=,'context?')|Conds]))))))))) :-
	PASS == pass,
	PAR == par,
	role_lexicon(multiplier_par, Arg1, Arg2, Arg3),
	add_roles([Arg1-X,Arg2-Y,Arg3-Z], multiplier_par, E, Conds, []).
default_semantics(passer, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(par))), lambda(PP,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(PP,lambda(Y,drs([variable(P)],Conds))))))))) :-
	PASS == pass,
	add_roles([theme-X,path-P], travel, E, Conds, [appl(path,P),appl(moving,X),appl(appl(cross,Y),P)]).
% passive with par-complement subjet
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(PAR))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	PAR == par,
	role_lexicon(V, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], V, E, Conds, []).
% passive with de-complement subjet
default_semantics(suivre, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(DE))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	DE == de,
	role_lexicon(suivre, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], suivre, E, Conds, []).
default_semantics(aimer, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(DE))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	DE == de,
	role_lexicon(aimer, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], aimer, E, Conds, []).
default_semantics(accompagner, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(DE))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	DE == de,
	role_lexicon(accompagner, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], accompagner, E, Conds, []).
default_semantics(apprécier, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(DE))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	DE == de,
	role_lexicon(apprécier, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], apprécier, E, Conds, []).
default_semantics(respecter, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(DE))), lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds))))))))) :-
	PASS == pass,
	DE == de,
	role_lexicon(respecter, SubjRole, ObjRole),
	add_roles([SubjRole-X,ObjRole-Y], respecter, E, Conds, []).
% other prepositions
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(PP))), lambda(PP,lambda(OBJ,lambda(E,appl(PP,lambda(Z,appl(OBJ,lambda(Y,drs([variable(X)],[bool(X,=,'context?')|Conds]))))))))) :-
	PASS == pass,
	combine_prep_word(PP, V, PW),
	role_lexicon(PW, Arg1, Arg2, Arg3),
	add_roles([Arg1-X,Arg2-Y,Arg3-Z], PW, E, Conds, []).
default_semantics(V, _POS, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(PAR))),lit(pp(PP))), lambda(PP,lambda(SUBJ,lambda(OBJ,lambda(E,appl(PP,lambda(Z,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds)))))))))))) :-
	PASS == pass,
	PAR == par,
	combine_prep_word(PP, V, PW),
	role_lexicon(PW, Arg1, Arg2, Arg3),
	add_roles([Arg1-X,Arg2-Y,Arg3-Z], PW, E, Conds, []).
% "était nommé NP"
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(np(_,_,_))), lambda(OBJ2,lambda(OBJ1,lambda(E,appl(OBJ1,lambda(Y,appl(OBJ2,lambda(Z,drs([variable(X)],[bool(X,=,'context?')|Conds]))))))))) :-
	PASS == pass,
	role_lexicon(V, Arg1, Arg2, Arg3),
	add_roles([Arg1-X,Arg2-Y,Arg3-Z], V, E, Conds, []).
% control verbs, object control only (TODO: verify!)
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),dl(0,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,drs([variable(X),event(L)],[bool(X,=,'context?')|Conds]))))))) :-
	PASS == pass,
	combine_prep_word(de, V, PW),	
	add_roles([agent-X,patient-Y,theme-L], PW, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(DEINF,lambda(P,appl(P,Y))),F)))]).
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),dl(0,lit(np(_,_,_)),lit(s(inf(a))))), lambda(AINF,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,drs([variable(X),event(L)],[bool(X,=,'context?')|Conds]))))))) :-
	PASS == pass,
	combine_prep_word(à, V, PW),	
	add_roles([agent-X,patient-Y,theme-L], PW, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(AINF,lambda(P,appl(P,Y))),F)))]).
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),dl(0,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,drs([variable(X),event(L)],[bool(X,=,'context?')|Conds]))))))) :-
	PASS == pass,
	add_roles([agent-X,patient-Y,theme-L], V, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))]).
% adjectival arguments
default_semantics(V, _POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),dl(0,lit(n),lit(n))), lambda(ADJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,drs([variable(X),event(L)],[bool(X,=,'context?')|Conds]))))))) :-
	PASS == pass,
	add_roles([agent-X,patient-Y,theme-L], V, E, Conds, [drs_label(L,appl(appl(ADJ,lambda(_,drs([],[]))),Y))]).
default_semantics(V, _POS, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),lit(pp(par))),dl(0,lit(n),lit(n))), lambda(ADJ,lambda(SUBJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds)))))))))) :-
	PASS == pass,
	add_roles([agent-X,patient-Y,theme-L], V, E, Conds, [drs_label(L,appl(appl(ADJ,lambda(_,drs([],[]))),Y))]).
default_semantics(V, _POS, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(PASS))),dl(0,lit(n),lit(n))),lit(pp(PAR))), lambda(SUBJ,lambda(ADJ,lambda(OBJ,lambda(E,appl(OBJ,lambda(Y,appl(SUBJ,lambda(X,drs([],Conds)))))))))) :-
	PASS == pass,
	PAR == par,
	add_roles([agent-X,patient-Y,theme-L], V, E, Conds, [drs_label(L,appl(appl(ADJ,lambda(_,drs([],[]))),Y))]).


% = weather verbs: pleuvoir/neiger etc. which are all impersonal verbs.

default_semantics(V, POS, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(_P, lambda(E, presup(drs(Es,Time),drs([],[appl(V,E)]))))) :-
	weather_verb(V),
	pos_time(POS, [], Es, E-Time).

% = intransitive "aller"
% TODO: check more examples, I think movement uses are rather limited, if they exist.
default_semantics(aller, POS, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(P,lambda(E,appl(P,lambda(X,drs(Es,[appl(event,E),appl(aller,E),appl(appl(theme,X),E),bool(Y,=,'lieu?'),appl(appl(source,Y),E)|Time])))))) :-
	pos_time(POS, [], Es, E-Time).
default_semantics(aller, POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(pp(à))), lambda(Q,lambda(P,lambda(E,appl(Q,lambda(Y,appl(P,lambda(X,presup(drs(Es,Time),drs([],[appl(event,E)|Conds])))))))))) :-
	add_roles([moving-X,path-Path], travel, E, Conds, [appl(appl(destination,Y),Path)]),
	pos_time(POS, [], Es, E-Time).

% "revenir" presupposes "partir"

default_semantics(revenir, POS, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(P, lambda(E,appl(P,lambda(V,presup(drs([event(F)],[appl(appl(appl(location,V),L),F)]),drs(Es, [appl(event,E),appl(appl(appl(travel,V),Path),E),appl(path,Path),appl(moving,V),app(appl(source,complement(L)),Path),appl(appl(destination,L),Path),bool(L,=,'lieu?')|Time]))))))) :-
	pos_time(POS, [], Es, E-Time).

default_semantics(revenir, POS,  dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(à))), lambda(Q, lambda(P, lambda(E,appl(P,lambda(V,appl(Q,lambda(Lieu,presup(drs([event(F)],[appl(appl(appl(location,V),L),F)]),drs(Es, [appl(event,E),appl(appl(appl(travel,V),Path),E),appl(path,Path),appl(moving,V),app(appl(source,complement(L)),Path),appl(appl(destination,Lieu),Path)|Time])))))))))) :-
	pos_time(POS, [], Es, E-Time).

% intransitive "arriver", resolve implicit destination anaphor
default_semantics(arriver, POS, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(P, lambda(E,appl(P,lambda(V,presup(drs([D],[bool(D,=,'lieu?')]),drs(EVs, [appl(event,E),appl(appl(appl(travel,Path),V),E),appl(moving,V),appl(path,Path),appl(appl(source,complement(D)),Path),appl(appl(destination,D),Path)|Time]))))))) :-
	pos_time(POS, [], EVs, E-Time).
% intransitive "partir", resolve implicit source anaphor
default_semantics(partir, POS, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(P, lambda(E,appl(P,lambda(V,presup(drs([variable(D)],[bool(D,=,'lieu?')]),drs(EVs, [appl(event,E),appl(appl(appl(travel,Path),V),E),appl(moving,V),appl(path,Path),appl(appl(source,D),Path),appl(appl(destination,complement(D)),Path)|Time]))))))) :-
	pos_time(POS, [], EVs, E-Time).

default_semantics(aller, POS, dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(à))), lambda(Q, lambda(P, lambda(E,appl(P,lambda(V,appl(Q,lambda(W,presup(drs([X,OL,T],[appl(orateur,X),appl(appl(temps,T),E),appl(appl(appl(lieu,OL),T),X),bool(OL,neq,W)]),drs(EVs, [appl(event,E),appl(appl(appl(travel,Path),V),E),appl(moving,V),appl(appl(destination,W),Path)|Time])))))))))) :-
	pos_time(POS, [], EVs, E-Time).

% = intransitive --- DEFAULT CASE

default_semantics(Word, ver:TIME, dl(0,lit(np(_,_,_)),lit(s(_))), lambda(NPS,lambda(E,appl(NPS,lambda(X,drs(EVs,Conds)))))) :-
	get_roles(Word, [np], [SRole]),
	add_roles([SRole-X], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = intransitive - verb initial

default_semantics(Word, ver:TIME, dr(0,lit(s(_)),lit(np(_,_,_))), lambda(NPS,lambda(E,appl(NPS,lambda(X,drs(EVs,Conds)))))) :-
	get_roles(Word, [np], [SRole]),
	add_roles([SRole-X], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = transitive - SVO

default_semantics(quitter, POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(P,lambda(Q,lambda(E,appl(Q,lambda(V,appl(P,lambda(W,presup(drs([event(F)],[appl(appl(appl(location,V),W),F)]),drs(EVs,[appl(event,E),appl(appl(appl(travel,Path),V),E),appl(moving,V),appl(path,Path),appl(appl(source,W),Path),appl(appl(destination,complement(W)),Path),bool(appl(temps,F),<,appl(temps,E))|Time])))))))))) :-
	pos_time(POS, [], EVs, E-Time).

% transitive "être"
% the identity between X and Y needs to depend on the temporal information, which is coded as "=_E" where E is the event
default_semantics(être, POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(P,lambda(Q,lambda(E,appl(Q,lambda(Y,appl(P,lambda(X,presup(drs(EVs,Time),drs([],[bool(X,is_at(E),Y)])))))))))) :-
	pos_time(POS, [], EVs, E-Time).

default_semantics(Word, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = transitive - verb initial

default_semantics(Word, ver:TIME, dr(0,dr(0,lit(s(_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NPS,lambda(NPO,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = transitive - verb final

default_semantics(Word, ver:TIME, dl(0,lit(np(_,_,_)),dl(0,lit(np(_,_,_)),lit(s(_)))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = NP V PP

default_semantics(Word, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(pp(Prp))), lambda(PP,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(PP,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,pp(Prp)], [SRole,PRole]),
	combine_prep_word(Prp, Word, PW),
	add_roles([SRole-X,PRole-Y], PW, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

default_semantics(Word, ver:TIME, dr(0,dr(0,lit(s(_)),lit(pp(Prp))),lit(np(_,_,_))), lambda(NPS,lambda(PP,lambda(E,appl(NPS,lambda(X,appl(PP,lambda(Y,drs(EVs,Conds))))))))) :-
	get_roles(Word, [np,pp(Prp)], [SRole,PRole]),
	combine_prep_word(Prp, Word, PW),
	add_roles([SRole-X,PRole-Y], PW, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = ditransitive - SVO

default_semantics(Word, ver:TIME, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NPO,lambda(NPI,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,appl(NPI,lambda(Z,drs(EVs,Conds)))))))))))) :-
	get_roles(Word, [np,np,np], [SRole,ORole,IRole]),
	add_roles([SRole-X,ORole-Y,IRole-Z], Word, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

default_semantics(Word, ver:TIME, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(pp(Prp))),lit(np(_,_,_))), lambda(NPO,lambda(PP,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,appl(PP,lambda(Z,drs(EVs,Conds)))))))))))) :-
	get_roles(Word, [np,np,pp(Prp)], [SRole,ORole,PRole]),
	combine_prep_word(Prp, Word, PW),
	add_roles([SRole-X,ORole-Y,PRole-Z], PW, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

default_semantics(Word, ver:TIME, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),lit(pp(Prp))), lambda(PP,lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,appl(PP,lambda(Z,drs(EVs,Conds)))))))))))) :-
	get_roles(Word, [np,np,pp(Prp)], [SRole,ORole,PRole]),
	combine_prep_word(Prp, Word, PW),
	add_roles([SRole-X,ORole-Y,PRole-Z], PW, E, Conds, Time),
	pos_time(ver:TIME, [], EVs, E-Time).


% = reflexive

default_semantics(Word, ver:TIME, dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(_)))), lambda(_SE, lambda(NP, lambda(E, appl(NP,lambda(X,drs(Es,Conds))))))) :-
	get_roles(Word, [np, cl_r], [SRole, CLRole]),
    (
        CLRole == null
    ->
        combine_se(Word, SeWord),
        add_roles([SRole-X], SeWord, E, Conds, Tnse)
    ;
        add_roles([SRole-X,CLRole-X], Word, E, Conds, Tnse)
    ),
        pos_time(ver:TIME, [], Es, E-Tnse).


default_semantics(Word, ver:TIME, dr(0,dl(0,lit(cl_r),lit(s(_))),lit(np(_,_,_))), lambda(NP, lambda(_SE, lambda(E, appl(NP,lambda(X,drs(Es,Conds))))))) :-
	get_roles(Word, [np, cl_r], [SRole, CLRole]),
    (
        CLRole == null
    ->
        combine_se(Word, SeWord),
        add_roles([SRole-X], SeWord, E, Conds, Tnse)
    ;
        add_roles([SRole-X,CLRole-X], Word, E, Conds, Tnse)
    ),
        pos_time(ver:TIME, [], Es, E-Tnse).
default_semantics(Word, ver:TIME, dl(0,lit(np(_,_,_)),dl(0,lit(cl_r),lit(s(_)))), lambda(NP, lambda(_SE, lambda(E, appl(NP,lambda(X,drs(Es,Conds))))))) :-
	get_roles(Word, [np, cl_r], [SRole, CLRole]),
    (
        CLRole == null
    ->
        combine_se(Word, SeWord),
        add_roles([SRole-X], SeWord, E, Conds, Tnse)
    ;
        add_roles([SRole-X,CLRole-X], Word, E, Conds, Tnse)
    ),
        pos_time(ver:TIME, [], Es, E-Tnse).

% reflexive + NP/PP
default_semantics(Word, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),lit(np(_,_,_))), lambda(Obj, lambda(_SE, lambda(Suj, lambda(E, appl(Suj,lambda(X,appl(Obj,lambda(Y,drs(Es,Conds)))))))))) :-
	get_roles(Word, [np, cl_r, np], [SRole, CLRole, ORole]),
    (
        CLRole == null
    ->
        combine_se(Word, SeWord),
        add_roles([SRole-X,ORole-Y], SeWord, E, Conds, Tnse)
    ;
        add_roles([SRole-X,CLRole-X,ORole-Y], Word, E, Conds, Tnse)
    ),
        pos_time(ver:TIME, [], Es, E-Tnse).

default_semantics(Word, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),lit(pp(PRP))), lambda(PP, lambda(_SE, lambda(Suj, lambda(E, appl(Suj,lambda(X,appl(PP,lambda(Y,drs(Es,Conds)))))))))) :-
	get_roles(Word, [np, cl_r, pp(PRP)], [SRole, CLRole, PRole]),
    (
        CLRole == null
    ->
        combine_se(Word, SeWord),
        add_roles([SRole-X,PRole-Y], SeWord, E, Conds, Tnse)
    ;
        add_roles([SRole-X,CLRole-X,PRole-Y], Word, E, Conds, Tnse)
    ),
        pos_time(ver:TIME, [], Es, E-Tnse).

% reflexive + INF
default_semantics(Word, ver:TIME, dr(_,dl(0,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_Refl, lambda(NPS, lambda(E, appl(NPS,lambda(Y,drs(EVs,Conds)))))))) :-
	( D0 == inf ; D0 == ainf ; D0 == deinf ),
	get_roles(Word, [np, cl_r, inf(_)], [SRole, CLRole, IRole]),
    (
        CLRole == null
    ->
	combine_se(Word, SeWord),
	add_roles([SRole-Y,IRole-L], SeWord, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred])
    ;
        add_roles([SRole-Y,CLRole-Y,IRole-L], Word, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred])
    ),
    	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% reflexive + ADJ

% TODO

% = auxiliary verbs

default_semantics(été, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(POS, [], Sem).
default_semantics(être, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(POS, [], Sem).
default_semantics(être, POS, dr(0,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(_)))),dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(ppart))))), Sem) :-
	auxiliary_verb_se(POS, [], Sem).

% correct error in lemmatiser: "suis" as form of "suivre", but with a syntactic category which can
% only correspond to "etre"
default_semantics(suivre, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(POS, [], Sem).
default_semantics(suivre, POS, dr(0,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(_)))),dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(ppart))))), Sem) :-
	auxiliary_verb_se(POS, [], Sem).
% correct error in lemmatiser: "sommes" as form of "sommer", but with a syntactic category which can
% only correspond to "etre"
default_semantics(sommer, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(POS, [], Sem).
default_semantics(sommer, POS, dr(0,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(_)))),dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(ppart))))), Sem) :-
	auxiliary_verb_se(POS, [], Sem).
% correct formula error: this formula should only occur for forms of "être", do something half-way reasonable if it occurrs elsewhere
default_semantics(Word, ver:POS, dr(0,dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(_)))),dl(0,lit(cl_r),dl(0,lit(np(A,B,C)),lit(s(ppart))))), lambda(VP,lambda(_CL,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs, Conds)))))))) :-
	combine_se(Word, SeWord),
	add_roles([agent-X, theme-X], SeWord, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(appl(VP,lambda(Q1,appl(Q1,X))),lambda(Q2,appl(Q2,X))),F)))|Time]),
	pos_time(ver:POS, [event(L)], EVs, E-Time).
	

default_semantics(avoir, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_avoir(POS, [], Sem).
default_semantics(avoir, POS, dr(_,dr(_,lit(s(_)),lit(np(_,_,_))),dl(_,lit(np(_,_,_)),lit(s(ppart)))), Sem) :-
	auxiliary_verb_avoir(POS, [], Sem).	

% = verbs with a "locative" argument
% that is, the verb takes an argument which is a preposition, but without specifying its type.

default_semantics(Word, POS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(Prep))), lambda(P, lambda(Q,lambda(E,appl(P,lambda(V,appl(Q,lambda(X,drs(EVs, [appl(event,E)|L0]))))))))) :-
	var(Prep),
	!,
	role_lexicon_np_loc(Word, Role1, Role2),
	add_roles([Role1-X,Role2-V], Word, E, L0, Time),
	pos_time(POS, [], EVs, E-Time).
default_semantics(Word, POS, dr(0,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(Prep))),lit(np(_,_,_))), lambda(P, lambda(Q,lambda(R, lambda(E,appl(P,lambda(V,appl(Q,lambda(X,appl(R,lambda(Y,drs(EVs, L0)))))))))))) :-
	var(Prep),
	role_lexicon_np_np_loc(Word, Role1, Role2, Role3),
	add_roles([Role1-Y,Role2-V,Role3-X], Word, E, L0, Time),
	!,
	pos_time(POS, [], EVs, E-Time).

% = verbs with a preposition argument
% contrary to the previous case, this time Prep is not a variable, but a preposition which is explicitly
% named and selected by the verb.

default_semantics(Word, ver:TIME, dr(0,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(Prep))),lit(np(acc,_,_))), lambda(OBJ,lambda(PP,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,appl(PP,lambda(Z,drs(EVs,L0)))))))))))) :-
	combine_prep_word(Prep, Word, PW),
	get_roles(Word, [np,np,pp(Prep)], [SRole,ORole,PRole]),
	add_roles([SRole-X,ORole-Y,PRole-Z], PW, E, L0, Time),
	pos_time(ver:TIME, [], EVs, E-Time).
default_semantics(Word, ver:TIME, dr(0,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),lit(pp(Prep))), lambda(PP,lambda(OBJ,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,appl(PP,lambda(Z,drs(EVs,L0)))))))))))) :-
	combine_prep_word(Prep, Word, PW),
	get_roles(Word, [np,np,pp(Prep)], [SRole,ORole,PRole]),
	add_roles([SRole-X,ORole-Y,PRole-Z], PW, E, L0, Time),
	pos_time(ver:TIME, [], EVs, E-Time).
default_semantics(Word, ver:TIME, dr(0,dr(0,lit(s(_)),lit(pp(Prep))),lit(np(_,_,_))), lambda(SUJ,lambda(PP,lambda(E,appl(SUJ,lambda(X,appl(PP,lambda(Y,drs(EVs,L0))))))))) :-
	combine_prep_word(Prep, Word, PW),
	get_roles(Word, [np,pp(Prep)], [SRole,PRole]),
	add_roles([SRole-X,PRole-Y], PW, E, L0, Time),
	pos_time(ver:TIME, [], EVs, E-Time).

% = arrêter + deinf
% presupposes "X a arrêté de P" presupposes "P(X)"
% (before ref_time)

default_semantics(arrêter, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(NPS,lambda(E,appl(NPS,lambda(X,presup(merge(drs(EVs,[bool(appl(temps,F),<,appl(temps,E))]),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F)),drs([],L0)))))))) :-
	add_roles([agent-X,theme-F], arrêter, E, L0, Time),
	pos_time(ver:TIME, [event(F)], EVs, E-Time).
default_semantics(cesser, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(NPS,lambda(E,appl(NPS,lambda(X,presup(merge(drs(EVs,[bool(appl(temps,F),<,appl(temps,E))]),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F)),drs([],L0)))))))) :-
	add_roles([agent-X,theme-F], cesser, E, L0, Time),
	pos_time(ver:TIME, [event(F)], EVs, E-Time).
default_semantics(finir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(NPS,lambda(E,appl(NPS,lambda(X,presup(merge(drs(EVs,[bool(appl(temps,F),<,appl(temps,E))]),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F)),drs([],L0)))))))) :-
	add_roles([agent-X,theme-F], finir, E, L0, Pred),
	pos_time(ver:TIME, [event(F)], EVs, E-Pred).

% = finir par INF
% semantics is treated as "finally inf"
default_semantics(finir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(par))))), lambda(PARINF,lambda(NPS,lambda(E,merge(appl(appl(PARINF,NPS),E),drs(EVs,[appl(finalement,E)|Pred])))))) :-
	pos_time(ver:TIME, [], EVs, E-Pred).


% = réussir/arriver + ainf
% veridical
% presupposes ainf is difficult (presupposes *trying* as well)

default_semantics(réussir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(AINF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,presup(drs([event(F),event(E1)],[bool(appl(temps,E),leq,appl(temps,F)),bool(appl(temps,E1),leq,appl(temps,E)),appl(difficile,F)|CondsE1]),merge(drs(EVs,[appl(event,E)|Conds]),appl(appl(AINF,lambda(Pd,appl(Pd,Y))),F))))))))) :-
	add_roles([agent-Y,theme-F], réussir_à, E, Conds, Pred),
	add_roles([agent-Y,theme-F], essayer, E1, CondsE1, []),
	pos_time(ver:TIME, [], EVs, E-Pred).
default_semantics(arriver, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(AINF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,presup(drs([event(F),event(E1)],[bool(appl(temps,E),leq,appl(temps,F)),bool(appl(temps,E1),leq,appl(temps,E)),appl(difficile,F)|CondsE1]),merge(drs(EVs,[appl(event,E)|Conds]),appl(appl(AINF,lambda(Pd,appl(Pd,Y))),F))))))))) :-
	add_roles([agent-Y,theme-F], arriver_à, E, Conds, Pred),
	add_roles([agent-Y,theme-F], essayer, E1, CondsE1, []),
	pos_time(ver:TIME, [], EVs, E-Pred).


% = venir + deinf
% immediate past

% TODO: verify Verkuyl-style times here ("venir de" and "aller")

default_semantics(venir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(P,lambda(X,lambda(E,merge(drs(Events,Pred),appl(appl(P,X),E)))))) :-
     (
        TIME = pres
     ->
        Events0 = [],
        Pred0 = [bool(appl(temps,E),<,maintenant)]
      ;
        TIME = impf
     ->
        Events0 = [event(S1)],
        Pred0 = [bool(appl(temps,E),<,S1),bool(S1,=,?),bool(S1,<,maintenant)]
      ;
        Events0 = [],
        Pred0 = [bool(appl(temps,E),<,maintenant)]
      ),
      (
        tense_aspect(none)
      ->
        Events = [],
        Pred = []
      ;
        tense_aspect(drs)
      ->
        RefTime = ref_time,
        Events = Events0,
        Pred = Pred0
      ;
        RefTime = appl(temps,E1),
        Events = [event(E1)|Events0],
        Pred = Pred0
      ).

% = aller + inf
% "immediate" future

% TODO: add Verkuyl-style times here ("aller" +INF)


default_semantics(aller, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(P,lambda(X,lambda(E,merge(DRS,appl(appl(P,X),E)))))) :-
	SINF == inf,
	(
        TIME = pres
     ->
        DRS = drs([],[bool(appl(temps,E),>,maintenant)])
      ;
        TIME = impf
     ->
        DRS = drs([event(S1)],[bool(S1,=,?),bool(appl(temps,S1),<,maintenant),bool(appl(temps,E),>,appl(temps,S1))])
      ;
        DRS = drs([],[])
      ).

% venir with incorporated clitics, legacy code (obsolete with current treatment of clitics)

default_semantics(venir, ver:TIME, dr(_,dl(0,lit(cl_y),dl(_,lit(np(_,_,_)),lit(s(_)))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(P,lambda(CLY,lambda(X,lambda(E,merge(appl(CLY,lambda(Y,drs(EVs,[appl(venir,E),appl(appl(lieu,Y),E)|Pred]))),appl(appl(P,X),E))))))) :-
	SINF == inf,
	pos_time(ver:TIME, [], EVs, E-Pred).
default_semantics(Word, ver:TIME, dr(_,dl(0,lit(cl_d3),dl(_,lit(np(_,_,_)),lit(s(_)))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(P,lambda(CLD,lambda(X,lambda(E,merge(appl(CLD,lambda(Y,drs(EVs,[appl(PW,E),appl(appl(à,Y),E)|Pred]))),appl(appl(P,X),E))))))) :-
	SINF == deinf,
	combine_prep_word(de, Word, PW),
	pos_time(ver:TIME, [], EVs, E-Pred).	
default_semantics(Word, ver:TIME, dr(_,dl(0,lit(cl_d3),dl(_,lit(np(_,_,_)),lit(s(_)))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(P,lambda(CLD,lambda(X,lambda(E,merge(appl(CLD,lambda(Y,drs(EVs,[appl(PW,E),appl(appl(à,Y),E)|Pred]))),appl(appl(P,X),E))))))) :-
	SINF == ainf,
	combine_prep_word(à, Word, PW),
	pos_time(ver:TIME, [], EVs, E-Pred).

% = factive regretter + DEINF
%
% the theme of "regretter" is generally something which happens before (though not necessarily, it is possible to regret a future event, however, these cases are rather unusual)

default_semantics(regretter, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(SINF)))), lambda(INF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,presup(merge(drs([event(F)],Pred),appl(appl(INF,lambda(Prp,appl(Prp,Y))),F)), drs(EVs,[appl(event,E)|Conds])))))))) :-
	SINF == deinf,
	add_roles([agent-Y,theme-F], regretter, E, Conds, [bool(appl(temps,F),leq,appl(temps,E))]),
	pos_time(ver:TIME, [], EVs, E-Pred).

default_semantics(regretter, ver:TIME, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(SINF)))), lambda(INF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,presup(merge(drs([event(F)],Pred),appl(appl(INF,lambda(Prp,appl(Prp,Y))),F)), drs(EVs,[appl(event,E)|Conds])))))))) :-
	SINF == inf,
	add_roles([agent-Y,theme-F], regretter, E, Conds, [bool(appl(temps,F),leq,appl(temps,E))]),
	pos_time(ver:TIME, [], EVs, E-Pred).

% = falloir + INF
% "il faut" undecided about reference of "il" (determined by context).
% Semantics should be: for a person X in this kind of situation (where "this kind of situation" is contextually determined)
% person X should/must do INF.

default_semantics(falloir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(INF, lambda(_, lambda(E, presup(drs(EVs,Pred),drs([event(L)],[appl(event,E)|Conds])))))) :-
	SINF == inf,
	add_roles([theme-L], falloir, E, Conds, [drs_label(L,merge(drs([event(F),variable(Y)],[bool(Y,=,'context?')]),appl(appl(INF,lambda(P,appl(P,Y))),F)))]),
	pos_time(ver:TIME, [], EVs, E-Pred).
% lemma error for "faut"
default_semantics(faillir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(SINF)))), lambda(INF, lambda(_, lambda(E, presup(drs(EVs,Pred),drs([event(L)],[appl(event,E)|Conds])))))) :-
	SINF == inf,
	add_roles([theme-L], falloir, E, Conds, [drs_label(L,merge(drs([event(F),variable(Y)],[bool(Y,=,'context?')]),appl(appl(INF,lambda(P,appl(P,Y))),F)))]),
	pos_time(ver:TIME, [], EVs, E-Pred).

% pousser without object (object is a "generic")
% TODO: generics are of course better treated as a binary relation between boxes
% "Elle signifie être obligé"

default_semantics(aider, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,presup(drs(EVs,Pred),drs([event(L)],[appl(event,E)|Conds])))))))) :-
	add_roles([agent-X,theme-L], aider, E, Conds, [drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y),bool(appl(temps,E),subseteq,appl(temps,F))]),appl(appl(INF,lambda(P,appl(P,Y))),F)))]),
	pos_time(ver:TIME, [], EVs, E-Pred).
default_semantics(appeler, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,presup(drs(EVs,Pred),drs([event(L)],[appl(event,E)|Conds])))))))) :-
	add_roles([agent-X,theme-L], appeler, E, Conds, [drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y),bool(appl(temps,E),<,appl(temps,F))]),appl(appl(INF,lambda(P,appl(P,Y))),F)))]),
	pos_time(ver:TIME, [], EVs, E-Pred).
% agent/subject is non-person only (?)
default_semantics(conduire, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,Conds))))))) :-
	add_roles([agent-X,theme-L], conduire, E, Conds, [drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),<,appl(temps,L))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(conseiller, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,Conds))))))) :-
	add_roles([agent-X,theme-L], conseiller, E, Conds, [drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),<,appl(temps,L))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(exiger, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(exiger,E),appl(event,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(faire, ver:TIME, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(PPA))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(PP, lambda(NP, lambda(E, appl(NP,lambda(X,appl(PP,lambda(Y,drs(EVs,Conds)))))))))) :-
     (  PPA == a ; PPA == à ),
	add_roles([agent-X,patient-Y,theme-L], faire, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(falloir, ver:TIME, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(PPA))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(PP, lambda(_NP, lambda(E, appl(PP,lambda(Y,drs(EVs,Conds)))))))) :-
     (  PPA == a ; PPA == à ),
	add_roles([theme-L], falloir, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
% lemmatization error for "faut"
default_semantics(faillir, ver:TIME, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(PPA))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(PP, lambda(_NP, lambda(E, appl(PP,lambda(Y,drs(EVs,Conds)))))))) :-
     (  PPA == a ; PPA == à ),
	add_roles([theme-L], falloir, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(faire, ver:TIME, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(PP, lambda(NP, lambda(E, appl(NP,lambda(X,appl(PP,lambda(Y,drs(EVs,Conds)))))))))) :-
	add_roles([agent-X,patient-Y,theme-L], faire, E, Conds, [drs_label(L,merge(drs([variable(Y),event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(faire, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,Conds))))))) :-
	add_roles([agent-X,patient-Y,theme-L], faire, E, Conds, [appl(generic,Y),drs_label(L,merge(drs([variable(Y),event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [variable(Y),event(L)], EVs, E-Pred).
% agent/subject is non-person only (?)
default_semantics(impliquer, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(impliquer,E),appl(event,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(inciter, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(inciter,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),<,appl(temps,L))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(imposer, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(imposer,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(inviter, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(inviter,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),<,appl(temps,L))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(laisser, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(laisser,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),subseteq,appl(temps,L))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(laisser, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(laisser,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F))),bool(appl(temps,E),subseteq,appl(temps,L))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(obliger, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(obliger,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(permettre, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(permettre,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(pousser, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(pousser,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(signifier, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(signifier,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(suggérer, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(suggérer,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(recommander, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(de))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(recommander,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% entendre/voir + INF, existential instead of generic
default_semantics(entendre, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(entendre,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(voir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), lambda(INF, lambda(NP, lambda(E, appl(NP,lambda(X,drs(EVs,[appl(event,E),appl(entendre,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))))) :-
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% TODO
% = consister + AINF
% contribuer + AINF
% demander + AINF
% porter + AINF
% rester + AINF
% revenir + AINF
% suffir + AINF
% travailler + AINF (?)
% équivaloir (?)
% parler + DEINF (?)
% proposer + DEINF (?)
% valoir + DEINF

% = il vaut (mieux) iNF

default_semantics(valoir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_NP, lambda(E, drs(EVs,[appl(event,E),appl(valoir,E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))) :-
	D0 == inf,
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(convenir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_NP, lambda(E, drs(EVs,[appl(event,E),appl(convenir,E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))) :-
	( D0 == deinf ; D0 == inf ),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(importer, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_NP, lambda(E, drs(EVs,[appl(event,E),appl(importer,E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))) :-
	( D0 == deinf ; D0 == inf ),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(suffir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_NP, lambda(E, drs(EVs,[appl(event,E),appl(suffir,E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))) :-
	( D0 == ainf ; D0 == inf ),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(suffir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(_NP, lambda(E, drs(EVs,[appl(event,E),appl(suffir,E),appl(appl(theme,L),E),drs_label(L,merge(drs([variable(Y),event(F)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]))))) :-
        ( D0 == deinf ; D0 == inf ),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% RAISING + INF

% = sembler

default_semantics(sembler, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(P,lambda(X,lambda(E,drs(EVs,[appl(event,E)|Conds]))))) :-
	D0 = inf(_),
	add_roles([theme-L], sembler, E, Conds, [drs_label(L,appl(appl(P,X),E))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% = convenir

default_semantics(convenir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(P,lambda(X,lambda(E,drs(EVs,[appl(event,E)|Conds]))))) :-
	D0 = inf(_),
	add_roles([theme-L], convenir, E, Conds, [drs_label(L,appl(appl(P,X),E))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% = paraître

default_semantics(paraître, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(P,lambda(X,lambda(E,drs(EVs,[appl(event,E)|Conds]))))) :-
	D0 = inf(_),
	add_roles([theme-L], paraître, E, Conds, [drs_label(L,appl(appl(P,X),E))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).


% = generic verbs having an infintive group as an argument; default to subject control

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(DEINF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,drs(EVs,[appl(event,E)|Conds]))))))) :-
	D0 == inf(de),
	combine_prep_word(de, Word, PW),
	add_roles([agent-Y,theme-L], PW, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(DEINF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(AINF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,drs(EVs,[appl(event,E)|Conds]))))))) :-
	D0 == inf(a),
	combine_prep_word(à, Word, PW),
	add_roles([agent-Y,theme-L], PW, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(AINF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).
default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,drs(EVs,[appl(event,E)|Conds]))))))) :-
	D0 = inf(_),
	add_roles([agent-Y,theme-L], Word, E, Conds, [drs_label(L,merge(drs([event(F)],[]),appl(appl(INF,lambda(P,appl(P,Y))),F)))|Pred]),
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).

% voir+np+inf, faire+np+inf
% note: the subseteq relation is a bit too strong for "faire", where
% the Allen relations E < F, E m F and E o F should be possible as well
% eg. in the case of indirectly making someone do something
% It is not clear if these cases are truly factive.
default_semantics(Word, ver:TIME, dr(0,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(D0)))),lit(np(_,_,_))), lambda(NPO, lambda(INF, lambda(NPS, lambda(E, appl(NPS,lambda(Y,appl(NPO,lambda(X,merge(drs(EVs,[appl(event,E),appl(Word,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(theme,L),E),drs_label(L,appl(appl(INF,lambda(Prp,appl(Prp,X))),F)),bool(appl(temps,E),subseteq,appl(temps,F))|Pred]),appl(appl(INF,lambda(Prp,appl(Prp,X))),F))))))))))) :-
	D0 == inf,
	pos_time(ver:TIME, [event(L),event(F)], EVs, E-Pred).
% voir+inf+np, faire+inf+np
default_semantics(Word, ver:TIME, dr(0,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),dl(_,lit(np(_,_,_)),lit(s(D0)))), lambda(INF, lambda(NPO, lambda(NPS, lambda(E, appl(NPS,lambda(Y,appl(NPO,lambda(X,merge(drs(EVs,[appl(event,E),appl(Word,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(theme,L),E),drs_label(L,appl(appl(INF,lambda(Prp,appl(Prp,X))),F)),bool(appl(temps,E),subseteq,appl(temps,F))|Pred]),appl(appl(INF,lambda(Prp,appl(Prp,X))),F))))))))))) :-
	D0 == inf,
	pos_time(ver:TIME, [event(L)], EVs, E-Pred).


%  transitive - sentential complement (factive verbs)

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(_))), lambda(SQ,lambda(NP,lambda(E,presup(merge(drs([event(F)],[]),appl(SQ,F)),appl(NP,lambda(X,drs(EVs, [appl(event,E)|Conds])))))))) :-
	factive(Word),
	!,
	add_roles([agent-X,theme-F], Word, E, Conds, List),
	pos_time(ver:TIME, [], EVs, E-List).

default_semantics(souvenir, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),lit(s(QQ))), lambda(SQ,lambda(_,lambda(NP,lambda(E,presup(merge(drs([event(F)],[]),appl(SQ,F)),appl(NP,lambda(X,drs(EVs, Conds))))))))) :-
	QQ == q,
	add_roles([agent-X,theme-L], se_souvenir, E, Conds, [bool(appl(temps,F),<,appl(temps,E)),drs([],[drs_label(L,appl(SQ,F))])|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).
default_semantics(pouvoir, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),lit(s(QQ))), lambda(SQ,lambda(_CLR,lambda(_SU,lambda(E,drs(EVs, Conds)))))) :-
	QQ == q,
	add_roles([theme-L], se_pouvoir, E, Conds, [drs([event(F)],[drs_label(L,appl(SQ,F))])|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).
default_semantics(trouver, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),lit(s(QQ))), lambda(SQ,lambda(_CLR,lambda(_SU,lambda(E,presup(merge(drs([event(F)],[]),appl(SQ,F)),drs(EVs,Conds))))))) :-
	QQ = q,
	add_roles([theme-L], se_trouver, E, Conds, [drs([],[drs_label(L,appl(SQ,F))])|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).
default_semantics(souvenir, ver:TIME, dr(0,dl(_,lit(cl_r),dl(_,lit(np(_,_,_)),lit(s(_)))),dl(0,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(_REFL,lambda(NP,lambda(E,appl(NP,lambda(X,presup(merge(drs([event(F)],[]),appl(appl(DEINF,lambda(Pr,appl(Pr,X))),F)),drs(EVs, Conds))))))))) :-
	add_roles([agent-X,theme-L], se_souvenir_de, E, Conds, [bool(appl(temps,F),<,appl(temps,E)),drs_label(L,appl(appl(DEINF,lambda(Pr,appl(Pr,X))),F))|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).

% = transitive - sentential complement (raising: "il semble que ..." etc.)

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(QQ))), lambda(SQ,lambda(_NP,lambda(E,drs(EVs,[appl(event,E)|Conds]))))) :-
	QQ == q,
	raising_verb(Word),
	add_roles([theme-L], Word, E, Conds, [drs_label(L,appl(SQ,F))|List]),
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).
default_semantics(falloir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(q))), lambda(SQ,lambda(_NP,lambda(E,drs(EVs,[appl(event,E),appl(il_faut,E),appl(appl(theme,L),E),drs_label(L,appl(SQ,F))|List]))))) :-
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).
default_semantics(faillir, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(q))), lambda(SQ,lambda(_NP,lambda(E,drs(EVs,[appl(event,E),appl(il_faut,E),appl(appl(theme,L),E),drs_label(L,appl(SQ,F))|List]))))) :-
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List). % corrects "il faut" as form of "faillir"

% = transitive - sentential complement (non-factive verbs, non-raising verbs, such as "croire")

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(QQ))), lambda(SQ,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(event,E)|Conds]))))))) :-
	QQ == q,
	add_roles([agent-X,theme-L], Word, E, Conds, [drs_label(L,appl(SQ,F))|List]),
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).
default_semantics(dire, ver:TIME, dr(0,dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),lit(s(_)))),lit(s(QQ))), lambda(SQ,lambda(_,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(event,E)|Conds])))))))) :-
	QQ == q,
	add_roles([agent-X,theme-L], se_dire, E, Conds, [drs_label(L,appl(SQ,F))|List]),
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).
default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(_))), lambda(SQ,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(event,E)|Conds]))))))) :-
	add_roles([agent-X,theme-L], Word, E, Conds, [drs_label(L,appl(SQ,F))|List]),
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).

% = adverbially used verbs "dit, souligne, déclare, explique, précise, constate, note, rappelle " etc.

default_semantics(Word, ver:TIME, dr(0,dl(1,lit(s(STp)),lit(s(STp))),lit(np(_,_,_))), lambda(NP,lambda(S,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(Word,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,appl(S,F))|List]))))))) :-
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).

default_semantics(Word, ver:TIME, dl(1,lit(s(STp)),dr(0,lit(s(STp)),lit(np(_,_,_)))), lambda(S,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(Word,E),appl(appl(agent,X),E),appl(appl(theme,L),E),drs_label(L,appl(S,F))|List]))))))) :-
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).

% = vp complements selecting a sentence

default_semantics(Word, ver:TIME, dl(1,lit(s(_)),dl(_,lit(np(_,_,_)),lit(s(_)))), lambda(SQ,lambda(NP,lambda(E,appl(NP,lambda(V,drs(EVs, [appl(event,E),appl(Word,E),appl(appl(agent,V),E),appl(appl(theme,L),E),drs_label(L,appl(SQ,F))|List]))))))) :-
	pos_time(ver:TIME, [event(F),event(L)], EVs, E-List).

% = VP-level adverbs

default_semantics(W, adv, dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(ST))),dl(_,lit(np(N1,N2,N3)),lit(s(ST)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).
default_semantics(W, adv, dl(_,dl(_,lit(np(N1,N2,N3)),lit(s(ST))),dl(_,lit(np(N1,N2,N3)),lit(s(ST)))), lambda(VP, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(VP,NP),E)))))).

default_semantics(W, adv, dr(0,dl(0,lit(cl(X)),dl(0,lit(np(NA,NB,NC)),lit(s(S1)))),dl(0,lit(cl(X)),dl(0,lit(np(NA,NB,NC)),lit(s(S1))))), lambda(VP, lambda(CL, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(appl(VP,CL),NP),E))))))).
default_semantics(W, adv, dr(0,dl(0,lit(cl_r),dl(0,lit(np(NA,NB,NC)),lit(s(S1)))),dl(0,lit(cl_r),dl(0,lit(np(NA,NB,NC)),lit(s(S1))))), lambda(VP, lambda(CL, lambda(NP, lambda(E, merge(drs([],[appl(W,E)]),appl(appl(appl(VP,CL),NP),E))))))).

% = raising verbs with adjectives: sembler, paraître

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Temp),drs([],[appl(event,E)|Conds])))))))) :-
	raising_verb(Word),
	add_roles([theme-L], Word, E, Conds, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))]),
	pos_time(ver:TIME, [event(L)], EVs, E-Temp).
default_semantics(Word, ver:TIME, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(PPA))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(PP,lambda(NP,lambda(E,appl(NP,lambda(X,appl(PP,lambda(Y,presup(drs(EVs,Temp),drs([],[appl(event,E)|Conds]))))))))))) :-
      ( PPA == a ; PPA == à ),
	raising_verb(Word),
	add_roles([experiencer-Y,patient-X,theme-L], Word, E, Conds, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))]),
	pos_time(ver:TIME, [event(L)], EVs, E-Temp).

% = copula 
% eg. rendre [gen(x)] heurex
%     laisse [gen(x)] rêveur

default_semantics(laisser, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,drs(EVs,[appl(event,E),appl(generic,Y)|List]))))))) :-
	add_roles([agent-X,patient-Y,theme-L], laisser, E, List, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))|Tm]),
	pos_time(ver:TIME, [event(L),variable(Y)], EVs, E-Tm).
default_semantics(rendre, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,drs(EVs,[appl(event,E),appl(generic,Y)|List]))))))) :- 
	add_roles([agent-X,patient-Y,theme-L], rendre, E, List, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))|Tm]),
	pos_time(ver:TIME, [event(L),variable(Y)], EVs, E-Tm).

% = copula  - verb initial and subject inversion

default_semantics(être, ver:pres, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), lit(np(nom,_,_))), lambda(NP,lambda(Adj,lambda(E,appl(NP,lambda(X,appl(appl(Adj,lambda(_,drs([],[]))),subb(X,E)))))))).
default_semantics(être, ver:pres, dr(_,dr(_,dl(0,lit(n),lit(n)),lit(s(_))), lit(np(nom,_,_))), lambda(NP,lambda(Adj,lambda(E,appl(NP,lambda(X,appl(appl(Adj,lambda(_,drs([],[]))),subb(X,E)))))))).
default_semantics(être, ver:TIME, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), lit(np(nom,_,_))), lambda(NP,lambda(Adj,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Pres),merge(drs([variable(Y)],[bool(X,is_at(E),Y)]),appl(appl(Adj,lambda(_,drs([],[]))),Y))))))))) :-
	pos_time(ver:TIME, [], EVs, E-Pres).
default_semantics(être, ver:TIME, dr(_,dl(0,dl(0,lit(n),lit(n)),lit(s(_))), lit(np(nom,_,_))), lambda(NP,lambda(Adj,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Pres),merge(drs([variable(Y)],[bool(X,is_at(E),Y)]),appl(appl(Adj,lambda(_,drs([],[]))),Y))))))))) :-
	pos_time(ver:TIME, [], EVs, E-Pres).
default_semantics(Word, ver:TIME, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), lit(np(nom,_,_))), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(event,E)|List]))))))) :-
	role_lexicon_np_adj(Word, SRole, ARole),
	add_roles([SRole-X,ARole-L], Word, E,  List, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))|Tm]),
	pos_time(ver:TIME, [event(L)], EVs, E-Tm).

% = copula  - verb second

default_semantics(être, ver:TNS, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Time),appl(appl(Adj,lambda(_,drs([],[]))),sub(X,E))))))))) :-
	pos_time(ver:TNS, [], EVs, E-Time).	


% "Jean est content que Marie dort."

default_semantics(être, ver:TNS, dr(_,dr(0,dl(_,lit(np(_,_,_)),lit(s(_))),lit(s(SQ))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(S,lambda(NP,lambda(E,appl(NP,lambda(X,presup(drs(EVs,Time),merge(merge(drs([event(F)],[]),appl(S,F)),appl(appl(Adj,lambda(_,drs([],[]))),sub(X,E))))))))))) :-
	SQ == q,
	pos_time(ver:TNS, [], EVs, E-Time).	

% "il est vrai/probable que"
% TODO: distinguish from "il est content que Marie dort." (above) etc. for this we need
% to distinguish anaphoric "il" from expletive pseudo-il

default_semantics(être, ver:TNS, dr(_,dr(0,dl(_,lit(np(_,_IL,_)),lit(s(_))),lit(s(SQ))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(S,lambda(_NP,lambda(E,presup(drs(EVs,Time),merge(merge(drs([event(F)],[]),appl(S,F)),appl(appl(Adj,lambda(_,drs([],[]))),sub(F,E))))))))) :-
%	IL == il,
	SQ == q,
	pos_time(ver:TNS, [], EVs, E-Time).	

default_semantics(Word, ver:TIME, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,drs(EVs,[appl(event,E)|List]))))))) :-
	role_lexicon_np_adj(Word, SRole, ARole),
	add_roles([SRole-X,ARole-L], Word, E,  List, [drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))|Tm]),
	pos_time(ver:TIME, [event(L)], EVs, E-Tm).

% = verbs taking adjective + np

default_semantics(Word, ver:TIME, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(OBJ,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,drs(EVs,[appl(event,E)|Conds])))))))))) :-
	role_lexicon_np_np_adj(Word, SRole, ORole, ARole),
	add_roles([SRole-X,ORole-Y,ARole-L], Word, E, Conds, [drs([],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))])|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).

default_semantics(Word, ver:TIME, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))),lit(np(_,_,_))), lambda(OBJ,lambda(Adj,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,drs(EVs,[appl(event,E)|Conds])))))))))) :-
	role_lexicon_np_np_adj(Word, SRole, ORole, ARole),
	add_roles([SRole-X,ORole-Y,ARole-L], Word, E, Conds, [drs([],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))])|List]),
	pos_time(ver:TIME, [event(L)], EVs, E-List).

% = past and present participles used as adjectives

default_semantics(W, ver:pper, dl(_,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([event(E),variable(X)],[appl(event,E)|Conds]),appl(P,V))))) :-
	role_lexicon(W, SubjectRole, ObjectRole),
	add_roles([SubjectRole-X,ObjectRole-V], W, E, Conds, []).
default_semantics(W, ver:pper, dr(_,dl(_,lit(n),lit(n)),lit(pp(par))), lambda(Q,lambda(P,lambda(V,merge(appl(Q,lambda(Z,drs([event(E)],[appl(event,E)|Conds]))),appl(P,V)))))) :-
	role_lexicon(W, SubjectRole, ObjectRole),
	add_roles([SubjectRole-Z,ObjectRole-V], W, E, Conds, []).
default_semantics(W, ver:pper, dr(_,dl(_,lit(n),lit(n)),lit(pp(PRP))), lambda(Q,lambda(P,lambda(V,merge(appl(Q,lambda(Z,drs([event(E),variable(X)],[appl(event,E),appl(generic,X)|Conds]))),appl(P,V)))))) :-
	combine_prep_word(PRP, W, PW),
	role_lexicon(PW, ArgRole1, ArgRole2, ArgRole3),
	add_roles([ArgRole1-X,ArgRole2-V,ArgRole3-Z], PW, E, Conds, []).
default_semantics(W, ver:ppre, dr(_,dl(_,lit(n),lit(n)),lit(np(_,_,_))), lambda(Q,lambda(P,lambda(V, merge(appl(Q,lambda(Z,drs([event(E),variable(X)],[appl(event,E)|Conds]))),appl(P,V)))))) :-
	role_lexicon(W, SubjectRole, ObjectRole, Arg),
	add_roles([SubjectRole-X,ObjectRole-V,Arg-Z], W, E, Conds, []).
default_semantics(W, ver:ppre, dr(_,dl(_,lit(n),lit(n)),lit(np(_,_,_))), lambda(Q,lambda(P,lambda(V, merge(appl(Q,lambda(Z,drs([event(E),variable(X)],[appl(event,E),appl(generic,X)|Conds]))),appl(P,V)))))) :-
	role_lexicon(W, SubjectRole, ObjectRole, Arg),
	add_roles([SubjectRole-X,ObjectRole-V,Arg-Z], W, E, Conds, []).

% = prepositions - arguments

default_semantics(Word, prp, dr(_,lit(pp(PRP)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).
default_semantics(Word, prp, dr(_,lit(pp(PRP)),lit(np(_,_,_))), lambda(X,X)) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).
default_semantics(Word, prp:det, dr(_,lit(pp(PRP)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).


% de NUM à NUM NOM
%default_semantics(Word, nam, lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[appl(appl(nommé,Word),X)]),appl(P,X)))).

% "de X à "
%default_semantics('à', prp, dr(0,dl(0,dr(0,lit(np(_,_,_)),n),dl(0,dr(0,pp_de,lit(np(_,_,_))),dr(0,pp_a,n))),dr(0,lit(np(_,_,_)),n)), lambda(P,lambda(Q,lambda(_,lambda(X, lambda(R,XXXX)))))).

default_semantics(_, pun, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
default_semantics(_, pun, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),appl(N,X)))))))).
default_semantics(_, pun, dr(0,dl(0,lit(s(Z)),lit(s(Z))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,F),E)]),merge(appl(P,E),appl(Q,F))))))).
default_semantics(_, pun, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(Z))),dl(0,lit(np(_,_,_)),lit(s(Z)))),dl(0,lit(np(_,_,_)),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([],[]),appl(appl(Q,N),E)),appl(appl(P,N),_))))))).
default_semantics(_, pun, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(P,lambda(Q,lambda(X,merge(appl(P,X),appl(Q,X)))))).
default_semantics(_, pun, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,P),appl(NP1,P)))))).
default_semantics(_, pun, dr(0,dl(0,lit(pp(_)),lit(pp(_))),lit(pp(_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP1,P),appl(NP2,P)))))).

default_semantics(_, pun:cit, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
default_semantics(_, pun:cit, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),appl(N,X)))))))).
default_semantics(_, pun:cit, dr(0,dl(0,lit(s(Z)),lit(s(Z))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,F),E)]),merge(appl(P,E),appl(Q,F))))))).
default_semantics(_, pun:cit, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(Z))),dl(0,lit(np(_,_,_)),lit(s(Z)))),dl(0,lit(np(_,_,_)),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([],[]),appl(appl(Q,N),E)),appl(appl(P,N),_))))))).
default_semantics(_, pun:cit, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(P,lambda(Q,lambda(X,merge(appl(P,X),appl(Q,X)))))).
default_semantics(_, pun:cit, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,P),appl(NP1,P)))))).
default_semantics(_, pun:cit, dr(0,dl(0,lit(pp(_)),lit(pp(_))),lit(pp(_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP1,P),appl(NP2,P)))))).


% = adjective

% = adjectives which are subsective

% = subsective prefixed adjectives
% NOTE: a *subsective* adjective is one such that the interpretation of "A N" is a
% a subset of the interpretation of "N", eg. "grand président" is a subset of
% "président" (but not equal to "président(X) & grand(X)" as an intersective
% adjective) unlike "ancien président".
% Subsective is the default (intersective is the default for postfixed adjectives).
% For the nonsubsective cases, we list them separately: we distinguish two more
% case "privative" and (simple) "nonsubsective".

% = privative adjectives
% NOTE: a *privative* adjective is one such that the interpretation of "A N"
% implies that the interpretation of "N" does *not* hold, eg. "faux billet"
% "emploi fictif"

default_semantics(W, adj, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    prefixed_privative_adjective(W).
default_semantics(W, ver:ppre, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    prefixed_privative_adjective(W).
default_semantics(W, ver:pper, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    prefixed_privative_adjective(W).

default_semantics(W, adj, dl(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    postfixed_privative_adjective(W).
default_semantics(W, ver:ppre, dl(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    postfixed_privative_adjective(W).
default_semantics(W, ver:pper, dl(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(W,S)])))) :-
	    postfixed_privative_adjective(W).

% = non-subsective prefixed adjectives

default_semantics(W, adj, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)])))) :-
	prefixed_nonsubsective_adjective(W).
default_semantics(W, ver:ppre, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)])))) :-
	prefixed_nonsubsective_adjective(W).
default_semantics(W, ver:pper, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)])))) :-
	prefixed_nonsubsective_adjective(W).

% = default to subsective

default_semantics(W, adj, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))).
default_semantics(W, ver:ppre, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))).
default_semantics(W, ver:pper, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))).

% = subsective postfixed (eg. "grand")

default_semantics(W, adj, dl(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))) :-
	    postfixed_subsective_adjective(W).
default_semantics(W, ver:ppre, dl(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))) :-
	    postfixed_subsective_adjective(W).
default_semantics(W, ver:pper, dl(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([event(S)],[drs_label(S,appl(P,X)),appl(W,S)]))))) :-
	    postfixed_subsective_adjective(W).

% relativizers

default_semantics(auquel, pro:rel, dr(0, dl(0, lit(n), lit(n)), dr(0,lit(s(_)),dia(1,box(1,lit(pp(Prp)))))), lambda(VP, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(appl(VP,lambda(Q,appl(Q,X))),E),merge(appl(N,X),appl(appl(à,X),E))))))))) :-
     ( Prp == à ; Prp == a ).
default_semantics(auquel, pro:rel, dr(0, dl(0, lit(n), lit(n)), lit(s(_))), lambda(S, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(S,E),merge(appl(N,X),appl(appl(à,X),E)))))))).
default_semantics(auxquels, pro:rel, dr(0, dl(0, lit(n), lit(n)), dr(0,lit(s(_)),dia(1,box(1,lit(pp(Prp)))))), lambda(VP, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(appl(VP,lambda(Q,appl(Q,X))),E),merge(appl(N,X),appl(appl(à,X),E))))))))) :-
     ( Prp == à ; Prp == a ).
default_semantics(auxquels, pro:rel, dr(0, dl(0, lit(n), lit(n)), lit(s(_))), lambda(S, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(S,E),merge(appl(N,X),appl(appl(à,X),E)))))))).
default_semantics(auxquelles, pro:rel, dr(0, dl(0, lit(n), lit(n)), dr(0,lit(s(_)),dia(1,box(1,lit(pp(Prp)))))), lambda(VP, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(appl(VP,lambda(Q,appl(Q,X))),E),merge(appl(N,X),appl(appl(à,X),E))))))))) :-
     ( Prp == à ; Prp == a ).
default_semantics(auxquelles, pro:rel, dr(0, dl(0, lit(n), lit(n)), lit(s(_))), lambda(S, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(S,E),merge(appl(N,X),appl(appl(à,X),E)))))))).

default_semantics(lequel, pro:rel, dr(0, dl(0, dr(0, lit(pp(Prp)), lit(np(_,_,_))), dl(0, lit(n), lit(n))), dr(0,lit(s(_)),dia(1,box(1,lit(pp(Prp)))))), lambda(VP, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(appl(VP,lambda(Q,appl(Q,X))),E),merge(appl(N,X),drs([],[Term]))))))))) :-
     (
         var(Prp)
     ->
         /* default, make sure this doesn't occur ! */
         Term = appl(appl(prep,X),E)
     ;
         Term = appl(appl(Prp,X),E)
     ).
default_semantics(lequel, pro:rel, dr(0, dl(0, dr(0, lit(pp(Prp)), lit(np(_,_,_))), dl(0, lit(n), lit(n))), lit(s(_))), lambda(S, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(S,E),merge(appl(N,X),drs([],[Term]))))))))) :-
     (
         var(Prp)
     ->
         /* default, make sure this doesn't occur ! */
         Term = appl(appl(prep,X),E)
     ;
         Term = appl(appl(Prp,X),E)
     ).

% =====================================
% = Default semantics without POS tag =
% =====================================

default_semantics(W, dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), lambda(Adj, lambda(P,lambda(V,merge(appl(appl(Adj,P),V),drs([event(E)],[appl(W,E),bool(E,=,'event?')])))))).
default_semantics(W, dr(0,dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),lit(pp(Prp))), lambda(PP,lambda(Adj, lambda(P,lambda(V,merge(appl(appl(Adj,P),V),appl(PP,lambda(X,drs([event(E)],[appl(appl(PW,X),E),bool(E,=,'event?')]))))))))) :-
	combine_prep_word(Prp, W, PW).
default_semantics(W, dr(0,dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),lit(np(_,_,_))), lambda(NP,lambda(Adj, lambda(P,lambda(V,merge(appl(appl(Adj,P),V),appl(NP,lambda(X,drs([event(E)],[appl(appl(W,X),E),bool(E,=,'event?')]))))))))).
default_semantics(W, dr(0,dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),lit(n)), lambda(N,lambda(Adj, lambda(P,lambda(V,merge(appl(appl(Adj,P),V),merge(merge(drs([variable(X)],[]),appl(N,X)),drs([event(E)],[appl(appl(W,X),E),bool(E,=,'event?')])))))))).
default_semantics(a, dr(0,dl(0,lit(cl_y),dl(0,lit(np(nom,il,3-s)),dl(1,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))))),lit(np(_,_,_))), lambda(NP,lambda(_Y,lambda(_IL,lambda(Adj, lambda(P,lambda(V,merge(appl(appl(Adj,P),V),appl(NP,lambda(X,drs([event(E)],[appl(appl(il_y_a,X),E),bool(E,=,'event?')]))))))))))).

% = adjective + ainf
% "facile à lire"

default_semantics(W, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(inf(a))))), lambda(AINF,lambda(N,lambda(X,merge(appl(N,X),drs([event(Th)],[appl(appl(PW,Th),X),drs_label(Th,merge(drs([event(E),variable(Y)],[appl(appl(patient,X),E),appl(generic,Y)]),appl(appl(AINF,lambda(P,appl(P,Y))),E)))])))))) :-
	combine_prep_word(à, W, PW).
default_semantics(W, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(inf(de))))), lambda(DEINF,lambda(N,lambda(X,merge(appl(N,X),drs([event(Th)],[appl(appl(PW,Th),X),drs_label(Th,merge(drs([event(E),variable(Y)],[appl(appl(patient,X),E),appl(generic,Y)]),appl(appl(DEINF,lambda(P,appl(P,Y))),E)))])))))) :-
	combine_prep_word(de, W, PW).

% = intransitive

default_semantics(Word, dl(_,lit(np(_,_,_)),lit(s(_))), lambda(NPS,lambda(E,appl(NPS,lambda(X,drs([],Conds)))))) :-
	get_roles(Word, [np], [SRole]),
	add_roles([SRole-X], Word, E, Conds, []).

% = intransitive - verb initial

default_semantics(Word, dr(_,lit(s(_)),lit(np(_,_,_))), lambda(NPS,lambda(E,appl(NPS,lambda(X,drs([],Conds)))))) :-
	get_roles(Word, [np], [SRole]),
	add_roles([SRole-X], Word, E, Conds, []).

% = transitive - SVO

default_semantics(Word, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs([],Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, []).

% = transitive - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NPS,lambda(NPO,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs([],Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, []).

% = transitive - verb final

default_semantics(Word, dl(_,lit(np(_,_,_)),dl(_,lit(np(_,_,_)),lit(s(_)))), lambda(NPO,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(NPO,lambda(Y,drs([],Conds))))))))) :-
	get_roles(Word, [np,np], [SRole,ORole]),
	add_roles([SRole-X,ORole-Y], Word, E, Conds, []).

% = ditransitive

default_semantics(Word, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),lit(np(_,_,_))), lambda(P, lambda(Q, lambda(R, lambda(E, appl(R,lambda(X,appl(Q,lambda(Y,appl(P,lambda(Z,drs([],Conds)))))))))))) :-
	get_roles(Word, [np,np,np], [SRole,ORole,IRole]),
	add_roles([SRole-X,ORole-Y,IRole-Z], Word, E, Conds, []).

% = ditransitive + (np + pp)

default_semantics(Word, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(Prep))),lit(np(_,_,_))), lambda(P, lambda(Q, lambda(R, lambda(E, appl(R,lambda(X,appl(Q,lambda(Y,appl(P,lambda(Z,drs([],Conds)))))))))))) :-
	get_roles(Word, [np,np,pp(Prep)], [SRole,ORole,PRole]),
	combine_prep_word(Prep, Word, PrepWord),
	add_roles([SRole-X,ORole-Y,PRole-Z], PrepWord, E, Conds, []).

% = transitive + pp

default_semantics(Word, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(pp(Prep))), lambda(PP,lambda(NPS,lambda(E,appl(NPS,lambda(X,appl(PP,lambda(Y,drs([],Conds))))))))) :-
	get_roles(Word, [np,pp(Prep)], [SRole,PRole]),
	combine_prep_word(Prep, Word, PrepWord),
	add_roles([SRole-X,PRole-Y], PrepWord, E, Conds, []).

% = transitive + prep + np
% assume POS-tagger error and use the VER:pres tag to calculate the semantics

default_semantics(Word, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(pp(Prep)))),lit(np(_,_,_))), Sem) :-
	default_semantics(Word, ver:pres, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(pp(Prep)))),lit(np(_,_,_))), Sem).

% ===================
% = subject control =
% ===================

% = destiner + ainf

% = viser + ainf

% = vouloir + inf 
% P: type(np) -> type(inf) (inf)
% X: type(np) (sujet)
% type(np) = (e->t)->t

default_semantics(vouloir, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), Sem) :-
	sem_tv_subject_control(vouloir, Sem).

% = penser + ainf

default_semantics(penser, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(a))))), Sem) :-
	sem_tv_subject_control(penser_à, Sem).

% = penser + inf

default_semantics(penser, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(_))))), Sem) :-
	sem_tv_subject_control(penser, Sem).


% = venir + inf

default_semantics(venir, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(_,lit(np(_,_,_)),lit(s(inf(base))))), lambda(P,lambda(X,lambda(E,merge(drs([],[]),appl(appl(P,X),E)))))).

% = essayer

% ===================
% = object control =
% ===================

% np = (e->t)->t
% lambda(P          e->s->t
%    lambda(e       s
%       drt(...)))
% lambda(P,lambda(E,merge(drs(['Jean'],[]),appl(appl(P,Jean),E)))).
% appl(NP, lambda(P, lambda(E, appl(appl(NP, P), E)))).
% appl(NP, 
% lambda(P,lambda(E,merge(appl(appl(NP,P),E),drs([],[appl(appl(patient,m),E)])

% = convaincre

default_semantics(convaincre, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),lit(np(_,_,_))), lambda(NPO, lambda(TOINF, lambda(NPS, lambda(E, appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(L)],[appl(convaincre_de,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(theme,L),E),drs_label(L,appl(appl(TOINF,lambda(Prp,appl(Prp,X))),_))])))))))))).

% = persuader + deinf + np

default_semantics(persuader, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),lit(np(_,_,_))), lambda(NPO, lambda(TOINF, lambda(NPS, lambda(E, appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(L)],[appl(persuader_de,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(theme,L),E),drs_label(L,appl(appl(TOINF,lambda(Prp,appl(Prp,X))),_))])))))))))).

% = pousser + ainf + np

default_semantics(pousser, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(a))))),lit(np(_,_,_))), lambda(NPO, lambda(TOINF, lambda(NPS, lambda(E, appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(L)],[appl(pousser_à,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(theme,L),E),drs_label(L,appl(appl(TOINF,lambda(Prp,appl(Prp,X))),_))])))))))))).

% = permettre + deinf

default_semantics(permettre, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(SType)))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(permettre,E),appl(appl(instrument,F),E)]),merge(appl(X,lambda(Z,drs([],[appl(appl(agent,Z),E)]))),appl(P,lambda(Q,lambda(F,merge(drs([variable(Y)],[]),appl(Q,Y),F)))))))))) :-
	( SType == inf ; SType == deinf ; SType == ainf ).

% = permettre + deinf + pp_a

default_semantics(permettre, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(SType)))),lit(pp(PType))), lambda(NPO, lambda(DEINF, lambda(NPS, lambda(E, merge(appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(F)],[appl(event,E),appl(permettre,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(instrument,F),E),appl(event,F)]))))),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F))))))) :-
	( SType == inf ; SType == deinf ; SType == ainf ),
	( PType == à ; PType == a ).


% = demander + deinf + pp_a

default_semantics(demander, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),lit(pp(à))), lambda(NPO, lambda(DEINF, lambda(NPS, lambda(E, appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(Lab)],[appl(event,E),appl(demander,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(instrument,Lab),E),drs_label(Lab,merge(drs([event(F)],[]),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F)))])))))))))).

% = aider + ainf + np

default_semantics(aider, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(a))))),lit(np(_,_,_))), lambda(NPO, lambda(DEINF, lambda(NPS, lambda(E, appl(NPO,lambda(X,appl(NPS,lambda(Y,drs([event(Lab)],[appl(event,E),appl(aider_à,E),appl(appl(agent,Y),E),appl(appl(patient,X),E),appl(appl(instrument,Lab),E),drs_label(Lab,merge(drs([event(F)],[]),appl(appl(DEINF,lambda(Prp,appl(Prp,X))),F)))])))))))))).

% = aimer + inf

%default_semantics(aimer, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf)))), lambda(P,lambda(X,lambda(E,merge(appl(X,lambda(V,drs([event(E),event(F)],[appl(aimer,E),appl(event,E),appl(appl(agent,V),E),appl(appl(patient,F),E)]))),appl(P,X),F))))).

% = laisser + inf

default_semantics(laisser, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(P,lambda(X,lambda(E,merge(drs([],[appl(laisser,E),appl(appl(agent,_Subj),E),appl(appl(patient,F),E)]),appl(appl(P,X),F)))))).

% = prepositions - noun modifiers

default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(W,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_))), lambda(NP, lambda(N, lambda(X, merge(appl(NP,lambda(Y,drs([],[appl(appl(W,Y),X)]))),appl(N,X)))))).
default_semantics(W, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))),lambda(P,lambda(Q,lambda(Z,merge(appl(P,lambda(X,appl(Q,lambda(Y,drs([],[appl(appl(W,X),Y)]))))),appl(Z,X)))))).

default_semantics(W, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)),lambda(P,lambda(NP,lambda(Q,appl(NP,lambda(Y,merge(merge(drs([variable(X)],[appl(appl(W,X),Y)]),appl(P,X)),appl(Q,X)))))))).

% = prepositions - noun modifiers having preposition as argument (eg. "deux d'entre nous")

default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(pp(Prp))), lambda(NP, lambda(N, lambda(X, merge(appl(NP,lambda(Y,drs([],[appl(appl(Functor,Y),X)]))),appl(N,X)))))) :-
	combine_prep_word(W, Prp, Functor).	
default_semantics(W, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(pp(Prp))),lambda(P,lambda(Q,lambda(Z,merge(appl(P,lambda(X,appl(Q,lambda(Y,drs([],[appl(appl(Functor,X),Y)]))))),appl(Z,X)))))) :-
	combine_prep_word(W, Prp, Functor).

% idiom: "à travers"
% TODO: this is a bit of a hack, change this to a true multi-word expression

default_semantics('A', dr(0,dr(0,dr(0,lit(s(SS)),lit(s(SS))),lit(np(_,_,_))),lit(n)), lambda(_N,lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(Y,drs([],[appl(appl(à_travers,Y),E)]))))))))).
default_semantics('À', dr(0,dr(0,dr(0,lit(s(SS)),lit(s(SS))),lit(np(_,_,_))),lit(n)), lambda(_N,lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(Y,drs([],[appl(appl(à_travers,Y),E)]))))))))).
default_semantics(à, dr(0,dr(0,dr(0,lit(s(SS)),lit(s(SS))),lit(np(_,_,_))),lit(n)), lambda(_N,lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(Y,drs([],[appl(appl(à_travers,Y),E)]))))))))).
default_semantics(à, dr(0,dr(0,dl(1,lit(s(SS)),lit(s(SS))),lit(np(_,_,_))),lit(n)), lambda(_N,lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(Y,drs([],[appl(appl(à_travers,Y),E)]))))))))).

% NP modifiers

default_semantics(monsieur, dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(monsieur, Sem).
default_semantics('M', dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(monsieur, Sem).
default_semantics('M.', dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(monsieur, Sem).

default_semantics(madame, dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(madame, Sem).
default_semantics('Mme', dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(madame, Sem).
default_semantics('Mme.', dr(_,lit(np(_,_,_)),lit(np(_,_,_))), Sem) :-
	title_semantics(madame, Sem).

default_semantics(W, dr(_,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).
default_semantics(W, dl(_,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).

% = prepositions - arguments

default_semantics(Word, dr(_,lit(pp(PRP)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[]),appl(P,X)),appl(Q,X))))) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).

default_semantics(Word, dr(_,lit(pp(PRP)),lit(np(_,_,_))), lambda(X,X)) :-
     (
          var(PRP)
     ->
          PRP = Word
     ;
          true
     ).

% = noun phrases - except for names

default_semantics(Word, lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[appl(Word,X)]),appl(P,X)))).

% = nouns

default_semantics(W, lit(n), lambda(X,drs([],[appl(W,X)]))).

% = adjective

% = postfixed adjective is intersective
default_semantics(Word, dl(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[appl(Word,V)]),appl(P,V))))).
default_semantics(Word, dr(0,lit(n),lit(n)), lambda(P,lambda(V, merge(drs([],[appl(Word,V)]),appl(P,V))))).

% = adjective + preposition "inférieur à", etc.

default_semantics(Word, dr(0,dl(0,lit(n),lit(n)),lit(pp(Prep))), lambda(Prp,lambda(N,lambda(X,merge(appl(N,X),appl(Prp,lambda(Y,drs([],[appl(appl(PrepWord,Y),X)])))))))) :-
	combine_prep_word(Prep, Word, PrepWord).

% = adverb + preposition "lors de", etc.

default_semantics(Word, dr(0,dl(1,lit(s(X)),lit(s(X))),lit(pp(Prep))), lambda(Prp,lambda(S,lambda(E,merge(appl(S,E),appl(Prp,lambda(Y,drs([],[appl(appl(PrepWord,Y),E)])))))))) :-
	combine_prep_word(Prep, Word, PrepWord).
default_semantics(Word, dr(0,dr(0,lit(s(X)),lit(s(X))),lit(pp(Prep))), lambda(Prp,lambda(S,lambda(E,merge(appl(S,E),appl(Prp,lambda(Y,drs([],[appl(appl(PrepWord,Y),E)])))))))) :-
	combine_prep_word(Prep, Word, PrepWord).

% = intensifiers

default_semantics(Word, dr(_,dr(_,lit(n),lit(n)),dr(_,lit(n),lit(n))), Sem) :-
	intensifier_semantics(Word, Sem).
default_semantics(Word, dr(_,dl(_,lit(n),lit(n)),dl(_,lit(n),lit(n))), Sem) :-
	intensifier_semantics(Word, Sem).

% = adverb intensifier

default_semantics(Word, dr(0,dl(1,lit(s(ST)),lit(s(ST))),dl(1,lit(s(ST)),lit(s(ST)))), lambda(Adv, lambda(S, lambda(E, merge(appl(S,E),drs([event(Lab)], [appl(appl(Word,Lab),E),drs_label(Lab,appl(appl(Adv,lambda(_,drs([],[]))),E))])))))).
default_semantics(Word, dr(0,dr(0,lit(s(ST)),lit(s(ST))),dr(0,lit(s(ST)),lit(s(ST)))), lambda(Adv, lambda(S, lambda(E, merge(appl(S,E),drs([event(Lab)], [appl(appl(Word,Lab),E),drs_label(Lab,appl(appl(Adv,lambda(_,drs([],[]))),E))])))))).

% = generic determiner type (used for example for adjectives in noun phrases without determiner)

default_semantics(W, dr(0,lit(np(_,_,_)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[appl(W,X)]),appl(P,X)),appl(Q,X))))).

% = adverbs - sentence modifiers

default_semantics(maintenant, dl(1,lit(s(ST)),lit(s(ST))), lambda(P,lambda(E,merge(appl(P,E),drs([],[bool(appl(temps,E),overlaps,maintenant)]))))).
default_semantics(W, dl(1,lit(s(ST)),lit(s(ST))), lambda(P,lambda(E,merge(appl(P,E),drs([],[bool(appl(temps,E),subseteq,W)]))))) :-
	temporal_adverb(W),
	!.

default_semantics(W, dl(1,lit(s(ST)),lit(s(ST))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).
default_semantics(W, dr(0,lit(s(ST)),lit(s(ST))), lambda(P,lambda(E,merge(appl(P,E),drs([],[appl(W,E)]))))).

% = adverbs - infinitives with "à" and "de"

default_semantics('d\'', dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(SS))),dl(_,lit(np(N1,N2,N3)),lit(s(inf(_))))), lambda(X,X)) :-
	SS = inf(de).
default_semantics(de, dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(SS))),dl(_,lit(np(N1,N2,N3)),lit(s(inf(_))))), lambda(X,X)) :-
	SS = inf(de).
default_semantics(à, dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(SS))),dl(_,lit(np(N1,N2,N3)),lit(s(inf(_))))), lambda(X,X)) :-
	SS = inf(a).
default_semantics(pour, dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(SS))),dl(_,lit(np(N1,N2,N3)),lit(s(inf(_))))), lambda(X,X)) :-
	SS = inf(pour).
default_semantics(par, dr(_,dl(_,lit(np(N1,N2,N3)),lit(s(SS))),dl(_,lit(np(N1,N2,N3)),lit(s(inf(_))))), lambda(X,X)) :-
	SS = inf(par).

% infinitives with "à" and "de" used as adjectives

default_semantics(de, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(N,lambda(X,merge(appl(N,X),drs([event(Lab)],[appl(appl(à,Lab),X),drs_label(Lab,merge(drs([event(E),variable(Y)],[appl(generic,Y),appl(appl(patient,X),E)]),appl(appl(VP,lambda(P,appl(P,Y))),E)))])))))).
default_semantics('d\'', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(N,lambda(X,merge(appl(N,X),drs([event(Lab)],[appl(appl(à,Lab),X),drs_label(Lab,merge(drs([event(E),variable(Y)],[appl(generic,Y),appl(appl(patient,X),E)]),appl(appl(VP,lambda(P,appl(P,Y))),E)))])))))).
default_semantics(à, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(N,lambda(X,merge(appl(N,X),drs([event(Lab)],[appl(appl(à,Lab),X),drs_label(Lab,merge(drs([event(E),variable(Y)],[appl(generic,Y),appl(appl(patient,X),E)]),appl(appl(VP,lambda(P,appl(P,Y))),E)))])))))).

% = copula  - verb initial

default_semantics(Word, dr(_,dr(_,lit(s(_)),dl(0,lit(n),lit(n))), np), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E),event(L)],[appl(event,E),appl(Word,E),appl(appl(SRole,X),E)]),appl(appl(ARole,L),E),drs([],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))])))))))) :-
		  role_lexicon_np_adj(Word, SRole, ARole).
		  
% = copula  - verb second

default_semantics(Word, dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(NP,lambda(E,appl(NP,lambda(X,merge(drs([event(E)],[appl(event,E),appl(Word,E),appl(appl(SRole,X),E),appl(appl(ARole,L),E)]),drs([event(L)],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),X))])))))))) :-
		  role_lexicon_np_adj(Word, SRole, ARole).
default_semantics(Word, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(OBJ,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,merge(drs([event(E),event(L)],[appl(event,E),appl(Word,E),appl(appl(SRole,X),E),appl(appl(ORole,Y),E),appl(appl(ARole,L),E)]),drs([],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))]))))))))))) :-
		  role_lexicon_np_np_adj(Word, SRole, ORole, ARole).
default_semantics(Word, dr(_,dr(_,dl(_,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))),lit(np(_,_,_))), lambda(OBJ,lambda(Adj,lambda(SUJ,lambda(E,appl(SUJ,lambda(X,appl(OBJ,lambda(Y,merge(drs([event(E),event(L)],[appl(event,E),appl(Word,E),appl(appl(SRole,X),E),appl(appl(ORole,Y),E),appl(appl(ARole,L),E)]),drs([],[drs_label(L,appl(appl(Adj,lambda(_,drs([],[]))),Y))]))))))))))) :-
		  role_lexicon_np_np_adj(Word, SRole, ORole, ARole).

default_semantics(de, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([event(L)],[appl(appl(de,L),X),drs_label(L,appl(appl(VP,lambda(P,appl(P,_))),_))])))))))).
default_semantics('d\'', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([event(L)],[appl(appl(de,L),X),drs_label(L,appl(appl(VP,lambda(P,appl(P,_))),_))])))))))). 
default_semantics(à, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),lit(s(inf(_))))), lambda(VP,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([event(L)],[appl(appl(de,L),X),drs_label(L,appl(appl(VP,lambda(P,appl(P,_))),_))])))))))).

% = adverbial prepositions

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(N,X)),appl(S,E)))))).
default_semantics(W, dr(0,dr(_,lit(s(_)),lit(s(_))),lit(n)), lambda(N,lambda(S,lambda(E,merge(merge(drs([variable(X)],[appl(appl(W,X),E)]),appl(N,X)),appl(S,E)))))).

default_semantics(W, dr(0,dl(_,lit(s(_)),lit(s(_))),lit(np(_,_,_))), lambda(NP,lambda(S,lambda(E,merge(appl(NP,lambda(X,drs([],[appl(appl(W,X),E)]))),appl(S,E)))))).

default_semantics(W, dr(0,dr(_,lit(s(_)),lit(s(_))),lit(np(_,_,_))), lambda(NP,lambda(S,lambda(E,merge(appl(NP,lambda(X,drs([],[appl(appl(W,X),E)]))),appl(S,E)))))).

% "beaucoup de" etc. n/pp

default_semantics(Word, dr(0,lit(n),lit(pp(PRP))), lambda(PP, lambda(_,appl(PP,lambda(X,drs([],[appl(PW,X)])))))) :-
	combine_prep_word(PRP, Word, PW).

% "beaucoup de" etc. np/pp

default_semantics(Word, dr(0,lit(np(_,_,_)),lit(pp(PRP))), lambda(PP,lambda(P,appl(PP,lambda(X,merge(appl(P,X),drs([],[appl(PW,X)]))))))) :-
	combine_prep_word(PRP, Word, PW).

default_semantics(Word, dr(0,lit(cs),lit(s(_))), Sem) :-
	Word \== que,
	Word \== 'qu\'',
	Sem = lambda(S,lambda(E,merge(drs([],[appl(Word,E)]),appl(S,E)))).

default_semantics(Word, lit(s(_)), lambda(E,drs([],[appl(Word,E)]))).

% ============================================================
% Lexicon
% ============================================================

lex(plutôt, dr(0,dl(1,s,s),s_q), lambda(S1,lambda(S2,lambda(E,merge(drs([event(L),event(F)],[drs_label(L,appl(S1,F)),appl(appl(plutôt_que,F),E)]),appl(S2,E)))))).

% "y compris"
lex(compris, dr(0,dl(0,cl_y,dl(1,s,s)),dl(1,s,s)), lambda(SS,lambda(_,lambda(S,lambda(E,merge(merge(drs([event(F)],[]),appl(appl(SS,lambda(_,drs([],[bool(F,subseteq,E)]))),F)),appl(S,E))))))).

% "tout" is "tout en VPR" constructions is considered semantically vacuous (admittedly a simplification, but hard to do better!)
lex(tout, dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,dl(0,np,s),dl(0,np,s))), lambda(X,X)).
lex(tout, dr(0,dl(0,dr(0,s,np),dr(0,s,np)),dl(0,dr(0,s,np),dr(0,s,np))), lambda(X,X)).
lex(tout, dr(0,dr(0,dl(0,np,s),dl(0,np,s)),dr(0,dl(0,np,s),dl(0,np,s))), lambda(X,X)).
   
lex(avoir, dr(0,dl(0,lit(np(A1,A2,A3)),lit(s(II))),dl(0,lit(np(A1,A2,A3)),lit(s(PP)))), lambda(PPART,lambda(NP,lambda(E, appl(NP,lambda(X,merge(drs([event(F)],[bool(appl(temps,F),<,appl(temps,E))]),appl(appl(PPART,lambda(Prp,appl(Prp,X))),F)))))))) :-
	II == inf,
	PP == ppart.
lex(en_une_heure, dl(1,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(sub(mésure,heure),appl(temps,E)),=,1)]))))).
lex(pendant_une_heure, dl(1,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(sub(mésure,heure),appl(temps,E)),=,1)]))))).
lex(le_soir, dr(0,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,soir)]))))).
lex(le_31, dr(0,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,appl(jour,31))]))))).
lex(a_six_heures_du_matin, dl(1,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[bool(appl(temps,E),overlaps,'06:00')]))))).
lex(dans_dix_minutes, dr(0,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[drs([event(E1)],[bool(maintenant,abuts,E1),bool(E1,abuts,E),bool(appl(sub(mésure,minutes),E1),=,10)])]))))).
lex(dans_dix_minutes, dl(1,lit(s(SX)),lit(s(SX))), lambda(S,lambda(E,merge(appl(S,E),drs([],[drs([event(E1)],[bool(maintenant,abuts,E1),bool(E1,abuts,E),bool(appl(sub(mésure,minutes),E1),=,10)])]))))).

lex(unique, dl(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([],[not(merge(drs([variable(Y)],[bool(Y,neq,X)]),appl(P,Y)))]))))).
lex(unique, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(appl(P,X),drs([],[not(merge(drs([variable(Y)],[bool(Y,neg,X)]),appl(P,Y)))]))))).


lex(que, dr(0,lit(np(A,B,C)),lit(np(A,B,C))), lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([variable(Y)],[bool(Y,=,'context?'),bool(num(Y),>,1),bool(X,atomic_sub,Y),bool(drs([variable(Z)],[bool(Z,atomic_sub,Y),bool(Z,neq,X)]),->,not(appl(P,Z)))]))))))).

lex('Rien', lit(np(_,_,_)), lambda(Q,drs([],[drs([],[bool(drs([variable(X)],[]),->,not(appl(Q,X)))])]))).
lex(rien, lit(np(_,_,_)), lambda(Q,drs([],[drs([],[bool(drs([variable(X)],[]),->,not(appl(Q,X)))])]))).
%lex(rien, dr(0,dl(0,lit(np(A,B,C)),lit(s(S))),dr(0,dl(0,lit(np(A,B,C)),lit(s(S))),lit(np(_,_,_)))), lambda(TV,lambda(NP,appl(NP,lambda(X,
lex(personne, lit(np(_,_,_)), lambda(Q,drs([],[drs([],[bool(drs([variable(X)],[appl(humain,X)]),->,not(appl(Q,X)))])]))).
lex('Personne', lit(np(_,_,_)), lambda(Q,drs([],[drs([],[bool(drs([variable(X)],[appl(humain,X)]),->,not(appl(Q,X)))])]))).

lex('non-', dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([X],[not(appl(P,X))])))).
% TODO: the semantics of the prefix "quasi-" given here is just the standard semantics for a
% privative adjective, eg. "quasi-total" implies "not(total)" but says nothing about the
% perceived (or implied) closeness to being total. It would be nice to do better than this.
lex('quasi-', dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([variable(X),event(S)],[not(appl(P,X)),drs_label(S,appl(P,X)),appl(quasi,S)])))).

%

lex('-t-elle', dl(1,s,s), lambda(X,X)).
lex('-elle', dl(1,s,s), lambda(X,X)).
lex('-elles', dl(1,s,s), lambda(X,X)).
lex('-t-il', dl(1,s,s), lambda(X,X)).
lex('-il', dl(1,s,s), lambda(X,X)).
lex('-ils', dl(1,s,s), lambda(X,X)).

% ancien/futur
% NOTE: the prefix "ex-" is treated as synonymous with "ancien"

lex('ex-', dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),<,ref_time)])))).
lex(ancien, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),<,ref_time)])))).
lex(ancienne, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),<,ref_time)])))).
lex(anciens, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),<,ref_time)])))).
lex(anciennes, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),<,ref_time)])))).
lex(futur, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),>,ref_time)])))).
lex(future, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),>,ref_time)])))).
lex(futurs, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),>,ref_time)])))).
lex(futures, dr(0,lit(n),lit(n)), lambda(P,lambda(X,drs([event(S)],[drs_label(S,appl(P,X)),bool(appl(temps,S),>,ref_time)])))).

lex(multiples, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(drs([],[bool(num(X),>,c)]),appl(P,X))))).
lex(nombreux, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(drs([],[bool(num(X),>,c)]),appl(P,X))))).
lex(nombreuses, dr(0,lit(n),lit(n)), lambda(P,lambda(X,merge(drs([],[bool(num(X),>,c)]),appl(P,X))))).

lex(mêmes, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[bool(X,=,?)]),appl(P,X))))).
lex(même, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([],[bool(X,=,?)]),appl(P,X))))).

lex(moins, dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SQ,lambda(P,lambda(X,merge(appl(appl(Adj,P),X),drs([event(L1),event(L2)],[drs_label(L1,SQ),drs_label(L2,appl(appl(Adj,P),X)),bool(appl(mésure,L2),<,appl(mésure,L1))]))))))).
lex(plus, dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SQ,lambda(P,lambda(X,merge(appl(appl(Adj,P),X),drs([event(L1),event(L2)],[drs_label(L1,SQ),drs_label(L2,appl(appl(Adj,P),X)),bool(appl(mésure,L2),>,appl(mésure,L1))]))))))).
lex(aussi, dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SQ,lambda(P,lambda(X,merge(appl(appl(Adj,P),X),drs([event(L1),event(L2)],[drs_label(L1,SQ),drs_label(L2,appl(appl(Adj,P),X)),bool(appl(mésure,L2),=,appl(mésure,L1))]))))))).
lex(si, dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SQ,lambda(P,lambda(X,merge(appl(appl(Adj,P),X),drs([event(L1),event(L2),event(F)],[drs_label(L1,appl(SQ,F)),drs_label(L2,appl(appl(Adj,P),X)),bool(appl(mésure,L2),>,c),appl(appl(cause,L1),L2)]))))))).
lex(tellement, dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))), lambda(Adj,lambda(SQ,lambda(P,lambda(X,merge(appl(appl(Adj,P),X),drs([event(L1),event(L2),event(F)],[drs_label(L1,appl(SQ,F)),drs_label(L2,merge(appl(appl(Adj,P),X),drs([],[bool(appl(mésure,L2),>,c)]))),appl(appl(cause,L1),L2)]))))))).

%lex(toujours, dl(1,s,s), lambda(S,lambda(E,drs([],drs([event(X)],[

lex(que, dr(0,lit(s(q)),lit(np(_,_,_))), Sem) :-
	semantics(dot_np, Sem).

% "C' est dans le jardin que Marie dort"
lex(est, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(s(q))),lit(pp(PRP))), lambda(PP, lambda(CS, lambda(_NP, lambda(E,merge(appl(PP,lambda(X,drs([],[appl(appl(PRP,X),E)]))),appl(CS,E))))))).

lex('En', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,appl(temps,F))]))))))).
lex('En', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,appl(temps,F))]))))))).
lex(en, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,appl(temps,F))]))))))).
lex(en, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[bool(appl(temps,E),subseteq,appl(temps,F))]))))))).

lex('Pour', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex('Pour', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex(pour, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex(pour, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex(pour, dr(0,dr(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf)), lambda(VPInf,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(merge(drs([event(F)],[]),appl(appl(VPInf,lambda(Q,appl(Q,X))),F)),drs([],[appl(appl(but,F),E)])))))))))).
lex(pour, dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_inf)), lambda(VPInf,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(merge(drs([event(F)],[]),appl(appl(VPInf,lambda(Q,appl(Q,X))),F)),drs([],[appl(appl(but,F),E)])))))))))).
lex(pour, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).

lex('Sans', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex('Sans', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex(sans, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).
lex(sans, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,merge(drs([variable(X)],[bool(X,=,'?')]),appl(P,X)))),F),merge(appl(S,E),drs([],[appl(appl(sans,F),E)]))))))).


lex('Avant', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex('Avant', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,appl(agent,E))]))))))).
lex(avant, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex(avant, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex('Avant', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex('Avant', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex(avant, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).
lex(avant, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,E),'<',appl(temps,F)),bool(X,=,'context?')]))))))).

lex('Avant', dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(avant, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(avant, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(avant, dr(0,dl(1,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).

lex('Après', dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_inf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(après, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_inf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(après, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_inf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(après, dr(0,dl(1,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_inf)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),<,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).

lex('En', dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),subseteq,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(en, dr(0,dl(0,dl(0,np,s),dl(0,np,s)),dl(0,np,s_ppres)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),subseteq,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(en, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),subseteq,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).
lex(en, dr(0,dl(1,dl(0,lit(np(_,_,_)),lit(s(ST))),dl(0,lit(np(_,_,_)),lit(s(ST)))),dl(0,lit(np(_,_,_)),s_ppres)), lambda(VPD,lambda(VP,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(VP,lambda(P,appl(P,X))),E),merge(drs([event(F)],[bool(appl(temps,E),subseteq,appl(temps,F))]),appl(appl(VPD,lambda(Q,appl(Q,X))),F)))))))))).


lex('Après', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,F),'<',appl(temps,E)),bool(X,=,'context?')]))))))).
lex('Après', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,F),'<',appl(temps,E)),bool(X,=,'context?')]))))))).
lex(après, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,F),'<',appl(temps,E)),bool(X,=,'context?')]))))))).
lex(après, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[bool(appl(temps,F),'<',appl(temps,E)),bool(X,=,'context?')]))))))).


lex('Afin', dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex('Afin', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex(afin, dr(0,dr(0,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).
lex(afin, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_deinf)), lambda(VP,lambda(S,lambda(E,merge(appl(appl(VP,lambda(P,appl(P,X))),F),merge(appl(S,E),drs([variable(X)],[appl(appl(but,F),E),bool(X,=,appl(agent,E))]))))))).

lex(aussi, dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,merge(appl(NP,lambda(X,drs([event(E)],[appl(appl(agent,X),E),bool(E,=,'event?')]))),appl(P,X))))).

% = proper nouns - French

lex('Là', dr(0,lit(s(_)),lit(s(_))), lambda(S,lambda(E,merge(drs([variable(X)],[bool(X,=,'lieu?'),appl(appl(lieu,X),E)]),appl(S,E))))).
lex('là', dr(0,lit(s(_)),lit(s(_))), lambda(S,lambda(E,merge(drs([variable(X)],[bool(X,=,'lieu?'),appl(appl(lieu,X),E)]),appl(S,E))))).
lex('là', dl(1,lit(s(_)),lit(s(_))), lambda(S,lambda(E,merge(drs([variable(X)],[bool(X,=,'lieu?'),appl(appl(lieu,X),E)]),appl(S,E))))).

lex('Je', lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('J', lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('J\'', lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex(je, lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('-je', lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex(j, lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('j\'', lit(np(nom,g,1-s)), lambda(P,merge(drs([X],[appl(orateur,X)]),appl(P,X)))).
lex('Nous', lit(np(_,_,1-p)), lambda(P,merge(drs([variable(X),variable(Y)],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,Y)))).
lex(nous, lit(np(_,_,1-p)), lambda(P,merge(drs([variable(X),variable(Y)],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,Y)))).
lex('-nous', lit(np(_,_,1-p)), lambda(P,merge(drs([variable(X),variable(Y)],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,Y)))).

lex('On', lit(np(nom,g,3-s)), lambda(P,merge(drs([X],[]),appl(P,X)))).
lex(on, lit(np(nom,g,3-s)), lambda(P,merge(drs([X],[]),appl(P,X)))).
lex('-on', lit(np(nom,g,3-s)), lambda(P,merge(drs([X],[]),appl(P,X)))).

lex('Tu', lit(np(nom,g,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('T', lit(np(_,_,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('T\'', lit(np(_,_,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(te, lit(np(_,_,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(tu, lit(np(nom,g,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(toi, lit(np(nom,g,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('-toi', lit(np(nom,g,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(t, lit(np(_,_,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('t\'', lit(np(_,_,2-s)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex('Vous', lit(np(_,_,2-p)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).
lex(vous, lit(np(_,_,2-p)), lambda(P,merge(drs([X],[appl(auditeur,X)]),appl(P,X)))).

lex('Certains', lit(np(_,_,_)), lambda(P,merge(drs([],[bool(num(X),>,1)]),appl(P,X)))).
lex(certains, lit(np(_,_,_)), lambda(P,merge(drs([],[bool(num(X),>,1)]),appl(P,X)))).
lex('Certaines', lit(np(_,_,_)), lambda(P,merge(drs([],[bool(num(X),>,1)]),appl(P,X)))).
lex(certaines, lit(np(_,_,_)), lambda(P,merge(drs([],[bool(num(X),>,1)]),appl(P,X)))).

lex('Il', lit(np(nom,il,3-s)), lambda(P,merge(drs([X],[bool(X,=,'masculin?')]),appl(P,X)))).
lex('Elle', lit(np(_,n,3-s)), lambda(P,merge(drs([X],[bool(X,=,'feminin?')]),appl(P,X)))).
lex(il, lit(np(nom,il,3-s)), lambda(P,merge(drs([X],[bool(X,=,'masculin?')]),appl(P,X)))).
lex('-il', lit(np(nom,il,3-s)), lambda(P,merge(drs([X],[bool(X,=,'masculin?')]),appl(P,X)))).
lex(ils, lit(np(nom,il,3-p)), lambda(P,merge(drs([X,Y],[bool(num(X),>,1),bool(Y,atomic_sub,X),bool(Y,=,'masculin?')]),appl(P,X)))).
lex('-ils', lit(np(nom,il,3-p)), lambda(P,merge(drs([X,Y],[bool(num(X),>,1),bool(Y,atomic_sub,X),bool(Y,=,'masculin?')]),appl(P,X)))).
lex('Ils', lit(np(nom,il,3-p)), lambda(P,merge(drs([X,Y],[bool(num(X),>,1),bool(Y,atomic_sub,X),bool(Y,=,'masculin?')]),appl(P,X)))).
lex(elles, lit(np(nom,il,3-p)), lambda(P,merge(drs([X],[bool(num(X),>,1),bool(X,=,'feminin?')]),appl(P,X)))).
lex('-elles', lit(np(nom,il,3-p)), lambda(P,merge(drs([X],[bool(num(X),>,1),bool(X,=,'feminin?')]),appl(P,X)))).
lex('Elles', lit(np(nom,il,3-p)), lambda(P,merge(drs([X],[bool(num(X),>,1),bool(X,=,'feminin?')]),appl(P,X)))).
lex(elle, lit(np(_,_,3-s)), lambda(P,merge(drs([X],[bool(X,=,'feminin?')]),appl(P,X)))).
lex('-elle', lit(np(_,_,3-s)), lambda(P,merge(drs([X],[bool(X,=,'feminin?')]),appl(P,X)))).
lex('l\'', lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex(lui, lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('-lui', lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex(y, lit(cl_y), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex(leur, lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'?'),appl(plural,X)]),appl(P,X)))).
lex('-leur', lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'?'),appl(plural,X)]),appl(P,X)))).
lex(le, lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'masculin?')]),appl(P,X)))).
lex('-le', lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'masculin?')]),appl(P,X)))).
lex(la, lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'feminin?')]),appl(P,X)))).
lex('-la', lit(np(_,_,_)), lambda(P,merge(drs([X],[bool(X,=,'feminin?')]),appl(P,X)))).
lex(se, lit(np(acc,refl,_)), lambda(P,appl(P,_))).
lex(se, lit(cl_r), lambda(P,appl(P,_))).
lex('s\'', lit(np(acc,refl,3-_)), lambda(P,appl(P,_))).
lex('s\'', lit(cl_r), lambda(P,appl(P,_))).
lex('Se', lit(np(acc,refl,_)), lambda(P,appl(P,_))).
lex('Se', lit(cl_r), lambda(P,appl(P,_))).
lex('S\'', lit(np(acc,refl,3-_)), lambda(P,appl(P,_))).
lex('S\'', lit(cl_r), lambda(P,appl(P,_))).

% TODO: ne+ cesser/oser/pouvoir is negative by itself (without pas)
%  ne + savoir + whq
%  ne + cesser + deinf
%  ne + oser + inf
%  ne + pouvoir + inf
%  ne + deigner + inf
%  ne + bouger
lex('Ne', dr(0,lit(s(Z)),lit(s(Z))), lambda(X,X)).
lex(ne, dr(0,lit(s(Z)),lit(s(Z))), lambda(X,X)).
lex(ne, dr(0,dl(0,lit(np(A,B,C)),lit(s(Z))),dl(0,lit(np(A,B,C)),lit(s(Z)))), lambda(X,X)).
lex(ne, dl(0,dl(0,lit(np(A,B,C)),lit(s(Z))),dl(0,lit(np(A,B,C)),lit(s(Z)))), lambda(X,X)).
lex('n\'', dr(0,dl(0,lit(np(A,B,C)),lit(s(Z))),dl(0,lit(np(A,B,C)),lit(s(Z)))), lambda(X,X)).
lex('n\'', dr(0,lit(s(Z)),lit(s(Z))), lambda(X,X)).
lex('N\'', dr(0,lit(s(Z)),lit(s(Z))), lambda(X,X)).
lex(pas, dr(0,dl(0,lit(np(_,_,_)),lit(s(Z))),dl(0,lit(np(_,_,_)),lit(s(Z)))), lambda(VP,lambda(NP,lambda(E,drs([],[not(appl(appl(VP,NP),E))]))))).
lex(pas, dl(1,lit(s(Z)),lit(s(Z))), lambda(S,lambda(E,drs([],[not(appl(S,E))])))).
lex(pas, dr(0,lit(np(A,B,C)),lit(np(A,B,C))), lambda(NP,lambda(P,drs([],[not(appl(NP,lambda(X,appl(P,X))))])))).

lex(où, dr(0,dl(0,n,n),s), lambda(S,lambda(P,lambda(V,merge(merge(drs([event(E)],[appl(appl(lieu,E),V)]),appl(S,E)),appl(P,V)))))).

% Generalized Quantifiers

lex(ni, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex('Ni', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex(ni, dr(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,drs([],[not(appl(NP,lambda(X,appl(P,X))))])))).
lex('Ni', dr(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(NP,lambda(P,drs([],[not(appl(NP,lambda(X,appl(P,X))))])))).
lex(aucun, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex(aucune, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex('Aucun', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).
lex('Aucune', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_no_semantics(Sem).

lex('Quelqu\'un', lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[]),appl(P,X)))).
lex('quelqu\'un', lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[]),appl(P,X)))).

% Demonstratives

lex(ce, lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex(c, lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('c\'', lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('Ce', lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('-ce', lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('C\'', lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).
lex('C', lit(np(nom,ce,3-s)), lambda(P,merge(drs([X],[bool(X,=,'?')]),appl(P,X)))).

lex(ce, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex(cet, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex(cette, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Ce', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Cet', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).
lex('Cette', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_this_semantics(Sem).

% Indefinites

lex('Plusieurs', dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex('Certains', dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex('Des', dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex(plusieurs, dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex(certains, dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex(des, dr(0,lit(np(_,_,3-p)),lit(n)), lambda(P,lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(P,X)),appl(Q,X))))).
lex('D\'', dr(0,lit(np(_,_,3-_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Du', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(des, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(du, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(de, dr(0,lit(np(_,_,3-_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('d\'', dr(0,lit(np(_,_,3-_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).

% "un/une" used as pronoun
% presuppose a set Y with at least two elements and select a member of the apropriate gender from it.

lex('Un', lit(np(_,_,3-s)), lambda(P1,presup(drs([Y],[bool(Y,=,?),bool(num(Y),>,1)]),merge(drs([X],[bool(X,atomic_sub,Y),appl(masculin,X)]),appl(P1,X))))). 
lex(un, lit(np(_,_,3-s)), lambda(P1,presup(drs([Y],[bool(Y,=,?),bool(num(Y),>,1)]),merge(drs([X],[bool(X,atomic_sub,Y),appl(masculin,X)]),appl(P1,X))))). 
lex('Une', lit(np(_,_,3-s)), lambda(P1,presup(drs([Y],[bool(Y,=,?),bool(num(Y),>,1)]),merge(drs([X],[bool(X,atomic_sub,Y),appl(feminin,X)]),appl(P1,X))))). 
lex(une, lit(np(_,_,3-s)), lambda(P1,presup(drs([Y],[bool(Y,=,?),bool(num(Y),>,1)]),merge(drs([X],[bool(X,atomic_sub,Y),appl(feminin,X)]),appl(P1,X))))). 

% "certains/plusieurs" used as pronoun
% note that "certains" is not treated as an anaphor but as an indefinite.
% eg. "Certains disent que ..."

lex('Certains', lit(np(_,_,3-p)), lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(Q,X))))).
lex(certains, lit(np(_,_,3-p)), lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(Q,X))))).
lex('Plusieurs', lit(np(_,_,3-p)), lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(Q,X))))).
lex(plusieurs, lit(np(_,_,3-p)), lambda(Q,merge(merge(drs([variable(X)],[bool(num(X),>,1)]),appl(Q,X))))).

% TODO: add other indefinites (celui, celle, celles, ceux, ce)


% de la
lex('De', dr(0,lit(np(A,B,C)),lit(np(A,B,C))), lambda(X,X)).
lex(de, dr(0,lit(np(A,B,C)),lit(np(A,B,C))), lambda(X,X)).

% Definites

% = adverbially used determiners "l'été", "le 15 janvier", "le mardi"

lex(le, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex(le, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('Le', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('Le', dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex(la, dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex(la, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('La', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('La', dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('l\'', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('l\'', dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('L\'', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('L\'', dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, merge(merge(drs([X],[]),appl(N,X)),appl(S,E)))))).
lex('Chaque', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, drs([],[bool(merge(drs([X],[]),appl(N,X)),->,appl(S,E))]))))).
lex(chaque, dr(0,dl(1,lit(s(_)),lit(s(_))),lit(n)), lambda(N, lambda(S, lambda(E, drs([],[bool(merge(drs([X],[]),appl(N,X)),->,appl(S,E))]))))).

% eg. "Le plus courageux"
% lex(le, dr(0,lit(np(_,_,3-s)),dl(0,lit(n),lit(n))), Sem) :- CONTINUE
lex(le, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('Le', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(ledit, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('Ledit', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(les, dr(0,lit(np(_,_,3-p)),lit(n)), Sem) :-
	gq_les_semantics(Sem).
lex('Les', dr(0,lit(np(_,_,3-p)),lit(n)), Sem) :-
	gq_les_semantics(Sem).
lex(la, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('La', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(la, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex(ladite, dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('Ladite', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('La', dr(0,lit(np(_,_,3-s)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('l\'', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_the_semantics(Sem).
lex('L\'', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_the_semantics(Sem).

% Existential

lex(un, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Un', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex(une, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).
lex('Une', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_a_semantics(Sem).

% Universal

lex(chaque, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Chaque', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(tout, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Tout', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(toute, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Toute', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(tous, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Tous', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex(toutes, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).
lex('Toutes', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	gq_every_semantics(Sem).

% Possessives
			
lex(notre, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, p, s).
lex('Notre', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, p, s).
lex(mon, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, m).
lex('Mon', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, m).
lex(ma, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, f).
lex('Ma', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, f).
lex(mes, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, p).
lex('Mes', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, s, p).
lex(nos, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, p, p).
lex('Nos', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_1p_semantics(Sem, p, p).

lex(votre, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, p, s).
lex('Votre', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, p, s).
lex(vos, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, p, p).
lex('Vos', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, p, p).
lex(ton, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, m).
lex('Ton', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, m).
lex(ta, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, f).
lex('Ta', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, f).
lex(tes, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, p).
lex('Tes', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_2p_semantics(Sem, s, p).

lex(leur, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, p, s).
lex('Leur', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, p, s).
lex(leurs, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, p, p).
lex('Leurs', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, p, p).
lex(son, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, m).
lex('Son', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, m).
lex(ses, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, p).
lex('Ses', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, p).
lex(sa, dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, f).
lex('Sa', dr(0,lit(np(_,_,_)),lit(n)), Sem) :-
	possessive_3p_semantics(Sem, s, f).


% Prepositions

lex('d\'', dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, merge(drs([variable(Y)],[appl(appl(de,Y),X)]),merge(appl(N2,X),appl(N1,Y))))))).
lex(du, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, presup(merge(drs([variable(Y)],[]),appl(N1,Y)),merge(appl(N2,X),drs([],[appl(appl(de,Y),X)]))))))).
lex(des, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N1,Y)),merge(appl(N2,X),drs([],[appl(appl(de,Y),X)]))))))).
lex(au, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, presup(merge(drs([variable(Y)],[]),appl(N1,Y)),merge(appl(N2,X),drs([],[appl(appl(à,Y),X)]))))))).
lex(aux, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(N1, lambda(N2, lambda(X, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N1,Y)),merge(appl(N2,X),drs([],[appl(appl(à,Y),X)]))))))).
lex('Au', dr(0,dr(0,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(à,Y),E)]))))))).
lex('Aux', dr(0,dr(0,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(à,Y),E)]))))))).
lex('Du', dr(0,dr(0,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(de,Y),E)]))))))).
lex('Des', dr(0,dr(0,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(de,Y),E)]))))))).
lex(au, dr(0,dl(1,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(à,Y),E)]))))))).
lex(aux, dr(0,dl(1,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(à,Y),E)]))))))).
lex(du, dr(0,dl(1,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(de,Y),E)]))))))).
lex(des, dr(0,dl(1,s,s),n), lambda(N, lambda(S, lambda(E, presup(merge(drs([variable(Y)],[bool(num(Y),>,1)]),appl(N,Y)),merge(appl(S,E),drs([],[appl(appl(de,Y),E)]))))))).

%default_semantics(aller, POS, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(pp(à))), lambda(Q,lambda(P,lambda(E,appl(Q,lambda(Y,appl(P,lambda(X,presup(drs(Es,Time),drs([],[appl(event,E)|Conds])))))))))) :-
%	add_roles([moving-X,path-Path], travel, E, Conds, [appl(appl(destination,Y),Path)]),
%	pos_time(POS, [], Es, E-Time).
%default_semantics(W, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))),lambda(P,lambda(Q,lambda(Z,merge(appl(P,lambda(X,appl(Q,lambda(Y,drs([],[appl(appl(W,X),Y)]))))),appl(Z,X)))))).

% Path de - a
% COMPLETE

lex(à, dr(0,dl(0,pp_de,pp_a),np), lambda(NP, lambda(PP_DE, lambda(Z,merge(appl(NP,lambda(X,appl(PP_DE,lambda(Y,drs([E],[bool(E,=,'event?'),appl(appl(source,Y),E)]))))),appl(Z,X)))))).

% plus_de

lex(plus, dr(0,lit(np(_,_,_)),lit(pp(de))), lambda(Q,lambda(P,merge(appl(Q,lambda(Y,drs([variable(X)],[bool(X,=,appl(plus_de,Y))]))),appl(P,X))))).

% Discourse connectives

% conjunctions

lex(et, dr(0,dl(0,lit(s(Z)),lit(s(Z))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dr(0,dl(0,lit(cs),lit(cs)),lit(cs)), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
lex(et, dr(0,dl(0,dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))),dr(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
lex(et, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(Z))),dl(0,lit(np(_,_,_)),lit(s(Z)))),dl(0,lit(np(_,_,_)),lit(s(_)))),lambda(P,lambda(Q,lambda(NP,lambda(E,appl(NP,lambda(X,merge(appl(appl(Q,lambda(X1,appl(X1,X))),E),merge(drs([event(F)],[]),appl(appl(P,lambda(X1,appl(X1,X))),F)))))))))).
lex(et, dr(0,dl(0,dr(0,lit(np(_,_,_)),lit(n)),dr(0,lit(np(_,_,_)),lit(n))),dr(0,lit(np(_,_,_)),lit(n))),lambda(Det1,lambda(Det2,lambda(N,lambda(P,merge(appl(appl(Det1,N),P),appl(appl(Det2,N),P))))))).

lex(ou, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,merge(appl(R,X),drs([],[bool(appl(appl(Q,R),X),\/,appl(appl(P,R),X))]))))))).
lex(ou, dr(0,dl(0,dr(0,lit(n),lit(n)),dr(0,lit(n),lit(n))),dr(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,merge(appl(R,X),drs([],[bool(appl(appl(Q,R),X),\/,appl(appl(P,R),X))]))))))).

lex('Mais', dr(0,lit(s(Z)),lit(s(Z))), lambda(S,lambda(E,merge(drs([event(F)],[appl(appl(contrast,F),E),bool(F,=,?)]),appl(S,E))))).
lex(mais, dl(0,lit(s(Z)),dr(0,lit(s(Z)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(mais, dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).

lex(puis, dl(0,lit(s(_)),dr(0,lit(s(_)),lit(s(_)))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E)],[bool(appl(temps,E),'<',appl(temps,F))]),merge(appl(P,E),appl(Q,F))))))).
lex(puis, dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(np(_,_,_)),lit(s(_)))),dl(0,lit(np(_,_,_)),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([event(F)],[bool(appl(temps,E),'<',appl(temps,F))]),appl(appl(Q,N),E)),appl(appl(P,N),F))))))).
% P,Q ((e->t)->t)->s->t
% N (e->t)->t
% E s
lex(',', dr(0,dl(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),dl(0,lit(n),lit(n))), lambda(P,lambda(Q,lambda(R,lambda(X,appl(appl(P,appl(Q,R)),X)))))).
% P: e->t
% NP: (e->t)->t
% VP: ((e->t)->t)->s->t
lex(',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),appl(N,X)))))))).
lex(',', dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP,lambda(VP,lambda(P,appl(NP,lambda(X,merge(merge(drs([event(F)],[]),appl(appl(VP,lambda(Prp,appl(Prp,X))),F)),appl(P,X)))))))).
% tough call, "satellite" np's have different uses.
lex(',', dr(0, dl(0, s, s), np), lambda(NP,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(_,drs([],[])))))))).
%title_semantics(Title, lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(appl(titre,Title),X)]))))))).
%lambda(P,merge(drs([variable(X)],[appl(appl(nommé,Word),X)]),appl(P,X)))).
%XYZ
% "Topicalized" constructions
% "np , s"
% "np : s"
% "n : s"

lex(',', dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(s(_))), lambda(S, lambda(NP, lambda(E, merge(appl(NP,lambda(X,(drs([],[appl(appl(topic,X),E)])))),appl(S,E)))))). 
lex(':', dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(s(_))), lambda(S, lambda(NP, lambda(E, merge(appl(NP,lambda(X,(drs([],[appl(appl(topic,X),E)])))),appl(S,E)))))). 
lex(',', dr(0,dl(0,lit(n),lit(s(_))),lit(s(_))), lambda(S, lambda(N, lambda(E, presup(merge(drs([variable(X)],[appl(appl(topic,X),E)]),appl(N,X)),appl(S,E)))))). 


lex(':', dr(0,dl(0,lit(n),lit(s(_))),lit(s(_))), lambda(S, lambda(N, lambda(E, presup(merge(drs([variable(X)],[appl(appl(topic,X),E)]),appl(N,X)),appl(S,E)))))). 
lex(':', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,P),appl(NP1,P)))))).
lex(':', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,P),appl(NP1,P)))))).
lex(':', dr(0,lit(s(q)),lit(s(main))), lambda(X,X)).
lex(':', dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_))), lambda(NP,lambda(N, lambda(X, merge(appl(NP,lambda(_,drs([],[]))),appl(N,X)))))).

% P,Q (e->t)->(e->t)
% R e->t
% X e
lex(et, dr(0,dl(0,lit(n),lit(n)),lit(n)), lambda(P,lambda(Q,lambda(X,merge(appl(P,X),appl(Q,X)))))).
lex(et, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP2,P),appl(NP1,P)))))).
lex(et, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),appl(N,X)))))))).
lex(et, dr(0,dl(0,lit(pp(_)),lit(pp(_))),lit(pp(_))), lambda(NP1,lambda(NP2,lambda(P,merge(appl(NP1,P),appl(NP2,P)))))).
lex(ni, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP1,lambda(NP2,lambda(P,merge(drs([],[not(appl(NP1,P))]),appl(NP2,P)))))).

lex(que, dr(0,dl(0,lit(n),lit(n)),lit(s(q))), lambda(SQ,lambda(CN,lambda(Y,merge(appl(CN,Y),drs([event(E),event(Lab)],[appl(appl(contenu,Lab),Y),drs_label(Lab,appl(SQ,E))])))))).
lex(que, dr(0,lit(s(q)),lit(s(main))), lambda(X,X)).
lex('qu\'', dr(0,lit(s(q)),lit(s(main))), lambda(X,X)).

% = relativization

% VP np -> s
% ((e->t)->t) -> s->t

% NP
% (e->t)->t

% P
% e->t
% lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(P,X),drs([],[appl(W,X)]))))))).

% wh_rel_semantics(lambda(P,lambda(Q,lambda(X,merge(appl(Q,X),appl(appl(P,lambda(R,appl(R,X))),_)))))).

lex(qui, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
lex(qui, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),lit(s(_)))), lambda(VP,lambda(NP,lambda(P,merge(appl(NP,P),appl(appl(VP,NP),_)))))).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(0,box(0,lit(np(_,_,_)))))), Sem) :-
	wh_rel_semantics(Sem).
lex(que, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(np(_,_,_)))))), Sem) :-
	wh_rel_semantics(Sem).
lex(lequel, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
lex(laquelle, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
lex(lesquels, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).
lex(lesquelles, dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))), Sem) :-
	wh_rel_semantics(Sem).

lex(que, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,lit(s(_)),dia(1,box(1,lit(np(_,_,_)))))), lambda(VP,lambda(NP,lambda(P,merge(appl(NP,P),appl(appl(VP,NP),_)))))).
lex('qu\'', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,lit(s(_)),dia(1,box(1,lit(np(_,_,_)))))), lambda(VP,lambda(NP,lambda(P,merge(appl(NP,P),appl(appl(VP,NP),_)))))).
lex('qu\'', dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(0,box(0,lit(np(_,_,_)))))), Sem) :-
	wh_rel_semantics(Sem).
lex('qu\'', dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(np(_,_,_)))))), Sem) :-
	wh_rel_semantics(Sem).
lex(dont, dr(0,dl(0,lit(n),lit(n)),dr(0,lit(s(_)),dia(1,box(1,lit(pp(de)))))), Sem) :-
	wh_rel_semantics(Sem).
lex(dont, dr(0,dr(0,dl(0,lit(n),lit(n)),dl(0,lit(np(_,_,_)),lit(s(_)))),lit(np(_,_,_))), lambda(NP, lambda(VP, lambda(N, lambda(X, merge(appl(NP,lambda(Y,drs([],[appl(appl(de,Y),X)]))),merge(appl(appl(VP,NP),_),appl(N,X)))))))).
lex(dont, dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),lit(s(_)))),lit(np(_,_,_))), lambda(NP, lambda(VP, lambda(NP2, lambda(P, merge(appl(NP,lambda(Y,appl(NP2,lambda(X,drs([],[appl(appl(de,Y),X)]))))),merge(appl(appl(VP,NP),_),appl(P,Y)))))))).


lex(où, dr(0, dl(0, dr(0, lit(pp(Prp)), lit(np(_,_,_))), dl(0, lit(n), lit(n))), lit(s(_))), lambda(S, lambda(_PP, lambda(N, lambda(X, merge(drs([event(E)],[]),merge(appl(S,E),merge(appl(N,X),drs([],[Term]))))))))) :-
     (
         var(Prp)
     ->
         /* default to "de" */
         Term = appl(appl(de,X),E)
     ;
         Term = appl(appl(Prp,X),E)
     ).

% "Il y a une alpiniste ..."
lex(a, dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), lambda(NP,lambda(_,lambda(_,lambda(E,appl(NP,lambda(X,drs([event(E)],[appl(appl(existe,X),E)])))))))).
% "Il y a deux ans"
lex(a, dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),dl(1,s,s))),lit(np(_,_,_))), lambda(NP,lambda(_,lambda(_,lambda(S,lambda(E,merge(appl(S,E),appl(NP,lambda(X,drs([],[appl(appl(il_y_a,X),E)])))))))))).
lex(a, dr(0,dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),s),lit(np(_,_,_))), lambda(NP,lambda(S,lambda(_,lambda(_,lambda(E,merge(appl(S,E),appl(NP,lambda(X,drs([],[appl(appl(il_y_a,X),E)])))))))))).

% = coordination

lex('Si', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(si,   dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(si,   dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex('S\'', dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex('s\'',   dr(0,dr(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex('s\'',   dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P, lambda(Q, lambda(_,drs([],[bool(appl(P,_),->,appl(Q,_))]))))).
lex(car,  dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(mais, dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(contrast,F),E)]),merge(appl(P,E),appl(Q,F))))))).
lex(et,   dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).


lex(parce, dr(0,dl(1,lit(s(SS)),lit(s(SS))),lit(s(QQ))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,F),E)]),merge(appl(P,E),appl(Q,F))))))) :-
	QQ == q.
lex(lorsque, dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex(lorsque, dr(0,dl(1,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex('lorsqu\'', dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex('lorsqu\'', dr(0,dl(1,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex('Lorsque', dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex('Lorsqu\'', dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).

lex(quand, dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex(quand, dr(0,dl(1,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).
lex('Quand', dr(0,dr(0,s,s),s), lambda(S1,lambda(S2,lambda(E,merge(merge(drs([event(F)],[]),appl(S1,F)),merge(appl(S2,E),drs([],[bool(appl(temps,E),overlaps,appl(temps,F))]))))))).


% = interpunction

lex('.', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('?', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('...', dl(0,lit(s(_)),lit(txt)), Sem) :-
	semantics(dot, Sem).
lex('.', dl(0,lit(np(_,_,_)),lit(txt)), Sem) :-
	semantics(dot_np, Sem).
lex('.', dl(0,lit(n),lit(txt)), lambda(N,merge(drs([X],[]),appl(N,X)))).

lex(',',  dr(0,dl(0,dl(0,lit(np(_,_,_)),lit(s(Tense))),lit(np(_,_,_))),lit(np(_,_,_))), lambda(NP,lambda(VP,lambda(P,merge(appl(NP,P),merge(drs([event(E)],Conds),appl(appl(VP,NP),E))))))) :-
    (
        Tense = ppart
    ->
        Conds = [appl(background,E)]
    ;
        Tense = ppres
    ->
        Conds = [appl(continuing,E)]
    ;
        Conds = []
    ).
lex(',', dr(0,dl(0,dl(0,lit(np(nom,_,_)),lit(s(S))),dl(0,lit(np(nom,_,_)),lit(s(S)))),dl(0,lit(np(nom,_,_)),lit(s(_)))),lambda(P,lambda(Q,lambda(N,lambda(E,merge(merge(drs([],[appl(appl(simultanée,F),E)]),appl(appl(P,N),F)),appl(appl(Q,N),E))))))).
lex(',',  dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(narration,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(';',  dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex(':',  dr(0,dl(0,lit(s(S)),lit(s(S))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(explanation,E),F)]),merge(appl(P,E),appl(Q,F))))))).
lex('-',  dr(0,dl(0,lit(s(_)),lit(s(_))),lit(s(_))), lambda(P,lambda(Q,lambda(F,merge(drs([event(E),event(F)],[appl(appl(background,E),F)]),merge(appl(P,E),appl(Q,F))))))).


lex(',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,n,n)), lambda(ADJ,lambda(NP,lambda(P,appl(NP,lambda(X,presup(appl(appl(ADJ,lambda(_,drs([],[]))),X),appl(P,X)))))))).

lex(y, dr(0,s,s), lambda(S,lambda(E,merge(drs([variable(X)],[bool(X,=,'lieu?')]),appl(S,E))))).
lex(y, dr(0,s,dr(0,s,dia(1,box(1,pp_a)))), lambda(P,lambda(E,merge(drs([],[bool(Y,=,'lieu?')]),appl(appl(P,lambda(R,appl(R,Y))),E))))).
lex(y, dr(0,dl(0,lit(np(_,_,_)),lit(s(Z))),dl(0,lit(np(_,_,_)),lit(s(Z)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(X,=,'lieu?'),appl(appl(à,X),E)]),appl(appl(P,Q),E)))))).
lex(y, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'?'),appl(appl(à,Y),E)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
%lex(y, dr(0,dl(0,cl_r,dl(0,lit(np(_,_,_)),lit(s(SS)))),dr(0,dl(0,cl_r,dl(0,lit(np(_,_,_)),lit(s(SS)))),dia(1,box(1,pp_a)))), lambda(VPPA,lambda(CLR,lambda(NPS,lambda(E,merge(drs([],[bool(Y,=,'?'),appl(appl(à,Y),E)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).

lex(en, dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), lambda(P,lambda(Q,lambda(E,merge(drs([],[appl(appl(de,X),E),bool(X,=,'?')]),appl(appl(P,Q),E)))))).
lex(en, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_de)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'?')]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).

lex(me, dr(0,dl(0,lit(np(_,_,_)),lit(s(Sent))),dr(0,dl(0,lit(np(_,_,_)),lit(s(Sent))),dia(1,box(1,lit(np(acc,_,_)))))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(orateur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex('m\'', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(orateur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(te, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(auditeur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex('t\'', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(auditeur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(nous, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([X,Y],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,X)))),Q),E)))))).
lex('Nous', dr(0,s,dr(0,s,dia(1,box(1,np)))), lambda(VP,lambda(E,appl(appl(VP,lambda(P,merge(drs([X,Y],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,X)))),E)))).
lex(nous, dr(0,s,dr(0,s,dia(1,box(1,np)))), lambda(VP,lambda(E,appl(appl(VP,lambda(P,merge(drs([X,Y],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(P,X)))),E)))).
lex(vous, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,X)))),Q),E)))))).
lex(le, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'masculin?')]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(la, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'féminin?')]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex('l\'', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'?')]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(les, dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,np)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'?'),bool(num(Y),>,1)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).

lex(me, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(orateur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex('m\'', dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(orateur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(te, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(auditeur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex('t\'', dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([Y],[appl(auditeur,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(nous, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([X,Y],[appl(orateur,X),bool(num(Y),>,1),bool(X,atomic_sub,Y)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,X)))),Q),E)))))).
lex(vous, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([X],[appl(auditeur,X)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,X)))),Q),E)))))).
lex(lui, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'masculin?')]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).
lex(leur, dr(0,dl(0,np,s),dr(0,dl(0,np,s),dia(1,box(1,pp_a)))), lambda(P,lambda(Q,lambda(E,merge(drs([],[bool(Y,=,'?'),bool(num(Y),>,1)]),appl(appl(appl(P,lambda(R,merge(drs([],[]),appl(R,Y)))),Q),E)))))).

% = autre

lex(autre, lit(n), lambda(Y,presup(drs([variable(Z)],[bool(Z,=,'context?')]),drs([],[bool(bool(Y,intersect,Z),=,nil)])))).
lex(autres, lit(n), lambda(X,presup(drs([variable(Z)],[bool(Z,=,'context?')]),drs([],[bool(X,empty_intersect,Z)])))).

% = âgé de

lex(âgés, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(N,X),appl(PP,lambda(Y,drs([],[bool(appl(âge,X),=,Y)])))))))).
lex(âgé, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(N,X),appl(PP,lambda(Y,drs([],[bool(appl(âge,X),=,Y)])))))))).
lex(âgées, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(N,X),appl(PP,lambda(Y,drs([],[bool(appl(âge,X),=,Y)])))))))).
lex(âgée, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(N,X),appl(PP,lambda(Y,drs([],[bool(appl(âge,X),=,Y)])))))))).

% = couvert de

lex(couvert, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex(couverts, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex(couverte, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex(couvertes, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex('Couvert', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex('Couverts', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex('Couverte', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).
lex('Couvertes', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(couvrir,X),Y)]))),appl(N,X)))))).

% = accompagné de

lex(accompagné, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex(accompagnés, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex(accompagnée, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex(accompagnées, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex('Accompagné', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex('Accompagnés', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex('Accompagnée', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).
lex('Accompagnées', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(accompagner,X),Y)]))),appl(N,X)))))).

% = suivi de

lex(suivi, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex(suivis, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex(suivie, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex(suivies, dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex('Suivi', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex('Suivis', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex('Suivie', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).
lex('Suivies', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(suivre,X),Y)]))),appl(N,X)))))).

% = lié à

lex(lié, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex(liés, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex(liée, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex(liées, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex('Lié', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex('Liés', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex('Liée', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).
lex('Liées', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(lié_à,X),Y)]))),appl(N,X)))))).

% = associé à

lex(associé, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex(associés, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex(associée, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex(associées, dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex('Associé', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex('Associés', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex('Associée', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).
lex('Associées', dr(0,dl(0,lit(n),lit(n)),lit(pp(à))), lambda(PP,lambda(N,lambda(X,merge(appl(PP,lambda(Y,drs([],[appl(appl(associé_à,X),Y)]))),appl(N,X)))))).

% Z is the set of alternatives to X
% Y is a member of these alternatives which is not equal to X
% QUESTION: is it useful/necessary to have X as a member of Z as well? (it seems that the currect way, with X not a member of
lex(autre, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z),variable(Y)],[bool(Z,=,?),bool(Y,atomic_sub,Z)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Y)])))))). 
lex(autres, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z),variable(Y)],[bool(Z,=,?),bool(Y,subseteq,Z)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Y)])))))).
lex(différent, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z)],[bool(Z,=,?)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Z)])))))).
lex(différents, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z)],[bool(Z,=,?)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Z)])))))).
lex(différente, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z)],[bool(Z,=,?)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Z)])))))). 
lex(différentes, dr(0,lit(n),lit(n)), lambda(P,lambda(X,presup(merge(drs([variable(Z)],[bool(Z,=,?)]),appl(P,Z)),merge(appl(P,X),drs([],[bool(X,empty_intersect,Z)])))))).
lex(ailleurs, dl(0,lit(n),lit(n)), lambda(P,lambda(X,presup(drs([variable(Z)],[bool(Z,=,?),appl(lieu,Z)]),merge(appl(P,X),drs([],[bool(X,neq,Z)])))))).
lex(ailleurs, dl(1,s,s), lambda(S,lambda(E,presup(drs([variable(Z)],[bool(Z,=,?),appl(lieu,Z)]),merge(appl(S,E),drs([X],[appl(lieu,X),appl(appl(lieu,X),E),bool(X,neq,Z)])))))).

lex('Comment', dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('comment?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).
lex('Comment', dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('comment?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex(comment, dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('comment?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).
lex(comment, dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('comment?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex('Pourquoi', dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('pourquoi?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).
lex('Pourquoi', dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('pourquoi?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex(pourquoi, dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('pourquoi?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).
lex(pourquoi, dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('pourquoi?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex('Où', dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('lieu?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex(où, dr(0,lit(s(whq)),dl(0,np,lit(s(inf(_))))), lambda(INF, lambda(E, drs([],[appl('lieu?',E),drs_label(E,merge(drs([event(F),variable(Y)],[appl(generic,Y)]),appl(appl(INF,lambda(P,appl(P,Y))),F)))])))).
lex('Où', dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('lieu?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).
lex('où', dr(0,lit(s(whq)),lit(s(main))), lambda(S, lambda(E, drs([],[appl('lieu?',E),drs_label(E,merge(drs([event(F)],[]),appl(S,F)))])))).

% ladite ledit

lex(été, dr(0,dl(0,np,s_ppart),dl(0,np,s_pass)), lambda(VP,lambda(NP,lambda(E,presup(drs(Es,Cs),appl(appl(VP,NP),E)))))) :-
	pos_time(ver:pper, [], Es, E-Cs).

lex(été, dr(0,dl(0,np,s(_)),dl(0,np,lit(s(ppart)))), Sem) :-
	auxiliary_verb_etre(ver:pper, [], Sem).

lex(pour, dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), lambda(P,lambda(S,lambda(E,merge(merge(drs([event(F)],[appl(appl(but,E),F)]),appl(S,E)),appl(appl(P,lambda(Q,appl(Q,_))),F)))))).

lex(',', dl(0,n,n), lambda(X,X)).
lex(',', dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(X,X)).
lex(',', dl(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex(',', dr(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex((','), dl(0,n,n), lambda(X,X)).
lex((','), dl(0,lit(np(_,_,_)),lit(np(_,_,_))), lambda(X,X)).
lex((','), dl(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex((','), dr(0,lit(s(S)),lit(s(S))), lambda(X,X)).
lex(',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(N,X),appl(P,X)))))))).
lex('(', dr(0,dl(0,n,n),lit(np(_,_,_))), lambda(NP, lambda(N, lambda(X, merge(appl(N,X),drs([],[appl(NP,lambda(_,drs([],[])))])))))). 
lex('(', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)), lambda(N,lambda(NP,lambda(P,appl(NP,lambda(X,merge(appl(N,X),drs([],[appl(P,X)])))))))).
%default_semantics(W, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_))), lambda(NP, lambda(N, lambda(X, merge(appl(N,X),appl(NP,lambda(Y,drs([],[appl(appl(W,Y),X)])))))))).
%default_semantics(Word, nam:INFO, lit(np(_,_,_)), lambda(P,merge(drs([variable(X)],[appl(appl(nommé,Word),X)|Rest0]),appl(P,X)))) :-

example_phrase(pos_lemma, 
          "Le ciel couvert de nuages dort.",
          ['Les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
	   'ciel', nom, 'ciel', lit(n),
           'couvert', adj, 'couvrir', dr(0,dl(0,lit(n),lit(n)),lit(pp(de))),
	   'de', prp, de, dr(0,lit(pp(de)),lit(n)),
	   nuages, nom, nuage, lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          " J\' aurai quitté Paris dans dix minutes.",
          ['J\'', pro:per, 'je', lit(np(_,_,_)), 
           'aurai', ver:futu, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'quitte', ver:pper, 'quitter', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           'Paris', nam, 'Paris', lit(np(_,_,_)),
	   'dans_dix_minutes', adv, 'dans_dix_minutes', dl(1,lit(s(X)),lit(s(X))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          " J\' avais pris un pomme hier.",
          ['J\'', pro:per, 'je', lit(np(_,_,_)), 
           'avais', ver:impf, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pris', ver:pper, 'pris', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'pomme', nom, 'pomme', n,
	   'hier', adv, 'hier', dl(1,lit(s(X)),lit(s(X))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          " J\' eut pris un pomme hier.",
          ['J\'', pro:per, 'je', lit(np(_,_,_)), 
           'eut', ver:simp, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pris', ver:pper, 'pris', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'pomme', nom, 'pomme', n,
	   'hier', adv, 'hier', dl(1,lit(s(X)),lit(s(X))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean vient de nulle part ailleurs.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
	   'vient', ver:pres, 'venir', dr(0,dl(0,lit(np(_,_,_)),s),lit(pp(de))),
	   de, prp, de, dr(0,lit(pp(de)),lit(n)),
           'nulle', adj, 'nul', dr(0,lit(n),lit(n)),
	   part, nom, part, lit(n),
	   ailleurs, adj, ailleurs, dl(0,lit(n),lit(n)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean vient de quelque part.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
	   'vient', ver:pres, 'venir', dr(0,dl(0,lit(np(_,_,_)),s),lit(pp(de))),
	   de, prp, de, dr(0,lit(pp(de)),lit(n)),
           'quelque', adj, 'quelque', dr(0,lit(n),lit(n)),
	   part, nom, part, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean va nulle part.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
	   'va', ver:pres, 'aller', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
           'nulle', adj, 'nul', dr(0,lit(np(_,_,_)),lit(n)),
	   part, nom, part, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean va quelque part ailleurs.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
	   'va', ver:pres, 'aller', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
           'quelque', adj, 'quelque', dr(0,lit(np(_,_,_)),lit(n)),
	   part, nom, part, lit(n),
	   ailleurs, adj, ailleurs, dl(0,lit(n),lit(n)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma,
	       "Comment dort Jean ?",
	       ['Comment', adv, comment, dr(0,lit(s(whq)),lit(s(_))),
		dort, ver:pres, dormir, dr(0,lit(s(_)),lit(np(_,_,_))),
		'Jean', nam, 'Jean', lit(np(_,_,_)),
                '?', pun, '.', dl(0,s,txt)],
		lit(txt)).

example_phrase(pos_lemma, 
          "Aucun étudiant arrêtait de fumer.",
          ['Aucun', pro:ind, 'aucun', dr(0,lit(np(_,_,_)),lit(n)), 
	   'étudiant', nom, 'étudiant', lit(n),
           'arrêtait', ver:pres, 'arrêter', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),
	   de, prp, de, dr(0,dl(0,lit(np(_,_,_)),lit(s(inf(de)))),dl(0,lit(np(_,_,_)),lit(s(inf)))),
	   fumer, ver:infi, fumer, dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean persuade personne de fumer.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'persuade', ver:pres, 'persuader', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),lit(np(_,_,_))),
	   personne, pro:ind, personne, lit(np(_,_,_)),
	   de, prp, de, dr(0,dl(0,lit(np(_,_,_)),lit(s(inf(de)))),dl(0,lit(np(_,_,_)),lit(s(inf)))),
	   fumer, ver:infi, fumer, dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma,
	       "Il pleut.",
	       ['Il', pro:pre, il, lit(np(_,_,_)),
		'pleut', ver:pres, dl(0,lit(np(_,_,_)),lit(s(_))),
		'.', pun, '.', dl(0,s,txt)],
	       lit(txt)).
example_phrase(pos_lemma,
          "Jean parait dormir.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'parait', ver:pres, 'paraître', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf)))),
           'dormir', ver:infi, 'dormir', dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean quitte Bordeaux.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'quitte', ver:pres, 'quitter', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'Bordeaux', nam, 'Bordeaux', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean ne quitte pas Bordeaux.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)),
	    ne, adv, ne, dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)),
           'quitte', ver:pres, 'quitter', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   pas, adv, pas, dl(1,s,s),
	   'Bordeaux', nam, 'Bordeaux', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma,
          "Jean semble dormir.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'semble', ver:pres, 'sembler', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf)))),
           'dormir', ver:infi, 'dormir', dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean arrive.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'arrive', ver:pres, 'arriver', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean va à Bordeaux.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'va', ver:pres, 'aller', dr(0,dl(0,lit(np(_,_,_)),s),lit(pp(à))),
	   à, prp, à, dr(0,lit(pp(à)),lit(np(_,_,_))),
	   'Bordeaux', nam, 'Bordeaux', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean arrête de fumer.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'arrête', ver:pres, 'arrêter', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),
	   de, prp, de, dr(0,dl(0,lit(np(_,_,_)),lit(s(inf(de)))),dl(0,lit(np(_,_,_)),lit(s(inf)))),
	   fumer, ver:infi, fumer, dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean revient à Bordeaux.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'revient', ver:pres, 'revenir', dr(0,dl(0,lit(np(_,_,_)),s),lit(pp(à))),
	   'à', prp, 'à', dr(0,lit(pp(à)),lit(np(_,_,_))),
	   'Bordeaux', nam, 'Bordeaux', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean vient de partir.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'vient', ver:pres, 'venir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf(de))))),
	   de, prp, de, dr(0,dl(0,lit(np(_,_,_)),lit(s(inf(de)))),dl(0,lit(np(_,_,_)),lit(s(inf)))),
	   partir, ver:infi, partir, dl(0,lit(np(_,_,_)),lit(s(inf))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime un douzaine de films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'un', det:art, 'um', dr(0,lit(np(_,_,_)),lit(n)),
	   douzaine, num, douzaine, lit(n),
	   de, prp, de, dr(0,dl(0,lit(n),lit(n)),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime des dizaines de films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'des', prp:det, 'des', dr(0,lit(np(_,_,_)),lit(n)),
	   dizaines, nom, dizaine, lit(n),
	   de, prp, de, dr(0,dl(0,lit(n),lit(n)),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime quatre des films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'quatre', num, 'quatre', lit(np(_,_,_)),
	   des, prp:det, des, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime que Marie.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'que', adv, 'que', dr(0,lit(np(_,_,_)),lit(np(_,_,_))),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
% TODO: fix combination of "que" and "NUM des"
example_phrase(pos_lemma, 
          "Jean aime que quatre des films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'que', adv, 'que', dr(0,lit(np(_,_,_)),lit(np(_,_,_))),
	   'quatre', num, 'quatre', lit(np(_,_,_)),
	   des, prp:det, des, dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Le meilleur acteur dort.",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
           'meilleur', adj, 'meilleur', dr(0,lit(n),lit(n)),
	   'acteur', nom, 'acteur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Les meilleurs acteur dorment.",
          ['Les', det:art, 'les', dr(0,lit(np(_,_,_)),lit(n)), 
           'meilleurs', adj, 'meilleurs', dr(0,lit(n),lit(n)),
	   'acteurs', nom, 'acteur', lit(n),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Le acteur le plus intéressant dort.",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
	   'acteur', nom, 'acteur', lit(n),
	   'le', det:art, 'le', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), 
           'plus', adv, 'plus', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),
           'intéressant', adj, 'intéressant', dl(0,lit(n),lit(n)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un acteur plus intéressant que Jean dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
	   'acteur', nom, 'acteur', lit(n),
           'plus', adv, 'plus', dr(0,dr(0,dl(0,lit(n),lit(n)),lit(s(q))),dl(0,lit(n),lit(n))),
           'intéressant', adj, 'intéressant', dl(0,lit(n),lit(n)),
	   'que', kon, 'que', dr(0,lit(s(q)),lit(np(_,_,_))), 
	   'Jean', nam, 'Jean', lit(np(_,_,_)), 
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Les acteurs les plus intéressants dorment.",
          ['Les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
	   'acteurs', nom, 'acteur', lit(n),
	   'les', det:art, 'le', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), 
           'plus', adv, 'plus', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),
           'intéressants', adj, 'intéressant', dl(0,lit(n),lit(n)),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Les acteurs les moins intéressants dorment.",
          ['Les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
	   'acteurs', nom, 'acteur', lit(n),
	   'les', det:art, 'le', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), 
           'moins', adv, 'moins', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),
           'intéressants', adj, 'intéressant', dl(0,lit(n),lit(n)),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Les acteurs les moins intéressants dorment.",
          ['Les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
	   'acteurs', nom, 'acteur', lit(n),
	   'les', det:art, 'le', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))), 
           'moins', adv, 'moins', dr(0,dl(0,lit(n),lit(n)),dl(0,lit(n),lit(n))),
           'verts', adj, 'vert', dl(0,lit(n),lit(n)),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un faux fumeur dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
           'faux', adj, 'faux', dr(0,lit(n),lit(n)),
	   'fumeur', nom, 'fumeur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un présumé fumeur dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
           'présumé', adj, 'présumé', dr(0,lit(n),lit(n)),
	   'fumeur', nom, 'fumeur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un ex-fumeur dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
           'ex-', adv, 'ex-', dr(0,lit(n),lit(n)),
	   'fumeur', nom, 'fumeur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un non-fumeur dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
           'non-', adv, 'non-', dr(0,lit(n),lit(n)),
	   'fumeur', nom, 'fumeur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Un gros fumeur dort.",
          ['Un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)), 
           'gros', adj, 'gros', dr(0,lit(n),lit(n)),
	   'fumeur', nom, 'fumeur', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "L\'ancien président dort.",
          ['L\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
           'ancien', adj, 'ancien', dr(0,lit(n),lit(n)),
	   'président', nom, 'président', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Les anciennes ministres dorment.",
          ['Les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
           'anciennes', adj, 'ancien', dr(0,lit(n),lit(n)),
	   'ministres', nom, 'ministre', lit(n),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Tous les anciennes ministres dorment.",
          ['Tous', adv:adv, 'tout', dr(0,lit(np(_,_,_)),lit(np(_,_,_))),
	   'les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
           'anciennes', adj, 'ancien', dr(0,lit(n),lit(n)),
	   'ministres', nom, 'ministre', lit(n),
	   'dorment', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Le futur président dort.",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)), 
           'futur', adj, 'futurx', dr(0,lit(n),lit(n)),
	   'président', nom, 'président', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean sait que Marie dort .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'sait', ver:pres, 'savoir', dr(0,dl(0,lit(np(_,_,_)),s),lit(s(q))),
	   'que', kon, 'que', dr(0,lit(s(q)),s),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime sa fille.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'sa', det:pos, 'son', dr(0,lit(np(_,_,_)),lit(n)),
	   'fille', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime ma fille.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'ma', det:pos, 'mon', dr(0,lit(np(_,_,_)),lit(n)),
	   'fille', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime ses filles.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'ses', det:pos, 'son', dr(0,lit(np(_,_,_)),lit(n)),
	   'filles', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime mes filles.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'mes', det:pos, 'mon', dr(0,lit(np(_,_,_)),lit(n)),
	   'filles', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime notre fille.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'notre', det:pos, 'notre', dr(0,lit(np(_,_,_)),lit(n)),
	   'fille', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime votre fille.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'votre', det:pos, 'votre', dr(0,lit(np(_,_,_)),lit(n)),
	   'fille', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime nos filles.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'nos', det:pos, 'notre', dr(0,lit(np(_,_,_)),lit(n)),
	   'filles', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime vos filles.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'vos', det:pos, 'votre', dr(0,lit(np(_,_,_)),lit(n)),
	   'filles', nom, fille, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Pierre aime l'une.",
          ['Pierre', nam, 'Pierre', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'une', num, un, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime l'autre.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'autre', adj, autre, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime une autre.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'autre', adj, autre, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime les autres.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'autres', adj, autre, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime l'un des films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'un', num, un, lit(n),
	   des, prp:det, des, dr(0,dl(0,lit(n),lit(n)),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime l'un de mes films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'un', num, un, lit(n),
	   de, prp, de, dr(0,dl(0,lit(n),lit(n)),lit(np(_,_,_))),
	   'mes', det:pos, 'mon', dr(0,lit(np(_,_,_)),lit(n)),
	   films, nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime un autre film.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'un', det:art, 'un', dr(0,lit(np(_,_,_)),lit(n)),
	   'autre', adj, autre, dr(0,lit(n),lit(n)),
	   'film', nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime l'autre film.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'autre', adj, autre, dr(0,lit(n),lit(n)),
	   'film', nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime des autres films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'des', det:art, 'des', dr(0,lit(np(_,_,_)),lit(n)),
	   'autres', adj, autre, dr(0,lit(n),lit(n)),
	   'films', nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime les autres films.",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'les', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'autres', adj, autre, dr(0,lit(n),lit(n)),
	   'films', nom, film, lit(n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean se souvient que Marie dort .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)),
	   se, pro:per, se, lit(cl_r),
           'souvient', ver:pres, 'souvenir', dr(0,dl(0,lit(cl_r),dl(0,lit(np(_,_,_)),s)),lit(s(q))),
	   'que', kon, 'que', dr(0,lit(s(q)),s),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean croit que Marie dort .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'croit', ver:pres, 'croire', dr(0,dl(0,lit(np(_,_,_)),s),lit(s(q))),
	   'que', kon, 'que', dr(0,lit(s(q)),s),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Le fait que Marie dort surprend Jean.",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   fait, nom, fait, lit(n),
	   'que', kon, 'que', dr(0,dl(0,lit(n),lit(n)),lit(s(q))),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
	   surprend, ver:pres, surprendre, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),lit(np(_,_,_))),
	   'Jean', nam, 'Jean', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Le idée que Marie dort semble bizarre.",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),lit(n)),
	   'idée', nom, 'idée', lit(n),
	   'que', kon, 'que', dr(0,dl(0,lit(n),lit(n)),lit(s(q))),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
	   semble, ver:pres, sembler, dr(0,dl(0,lit(np(_,_,_)),lit(s(_))),dl(0,lit(n),lit(n))),
	   bizarre, adj, bizarre, dl(0,lit(n),lit(n)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean regret aimer Marie .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'regrette', ver:pres, 'regretter', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),lit(s(inf)))),
	   'aimer', ver:infi, 'aimer', dr(0,dl(0,lit(np(_,_,_)),lit(s(inf))),lit(np(_,_,_))),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


example_phrase(pos_lemma, 
          "! nous parvînmes au sommet du Néthou où nous retrouvâmes intacte la petite pyramide que nous y avions construite .",
          ['nous', pro:per, 'nous', lit(np(_,_,_)), 
           'parvînmes', ver:simp, 'parvenir', dr(0,dl(0,lit(np(_,_,_)),s),pp_a), 
           'au', prp:det, 'au', dr(0,pp_a,n), 
           'sommet', nom, 'sommet', n, 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'Néthou', nam, 'Néthou', n, 
           'où', pro:rel, 'où', dr(0,dl(0,n,n),s), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'retrouvâmes', ver:simp, 'retrouver', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),dl(0,n,n)), 
           'intacte', adj, 'intact', dl(0,n,n), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'petite', adj, 'petit', dr(0,n,n), 
           'pyramide', nom, 'pyramide', n, 
           'que', pro:rel, 'que', dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'avions', ver:impf, 'avoir', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ppart)), 
           'construite', ver:pper, 'construire', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! nous parvînmes au sommet du Néthou où nous retrouvâmes intacte la petite pyramide que nous y avions construite .",
          ['nous', pro:per, 'nous', lit(np(_,_,_)), 
           'parvînmes', ver:simp, 'parvenir', dr(0,dl(0,lit(np(_,_,_)),s),pp_a), 
           'au', prp:det, 'au', dr(0,pp_a,n), 
           'sommet', nom, 'sommet', n, 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'Néthou', nam, 'Néthou', n, 
           'où', pro:rel, 'où', dr(0,dl(0,n,n),s), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'retrouvâmes', ver:simp, 'retrouver', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),dl(0,n,n)), 
           'intacte', adj, 'intact', dl(0,n,n), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'petite', adj, 'petit', dr(0,n,n), 
           'pyramide', nom, 'pyramide', n, 
           'que', pro:rel, 'que', dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'avions', ver:impf, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'construite', ver:pper, 'construire', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! C\' est au cours du printemps 1787 , que Ramond vînt aux Pyrénées .",
          ['C\'', pro:dem, 'ce', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),cs),pp), 
           'au', prp:det, 'au', dr(0,pp_a,n), 
           'cours', nom, 'cours', n, 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'printemps', nom, 'printemps', n, 
           '1787', num, '1787', dl(0,n,n), 
           'que', kon, 'que', dr(0,cs,s), 
           'Ramond', nam, 'Ramond', lit(np(_,_,_)), 
           'vînt', ver:simp, 'venir', dr(0,dl(0,lit(np(_,_,_)),s),pp_a), 
           'aux', prp:det, 'aux', dr(0,pp_a,n), 
           'Pyrénées', nam:lfp, 'Pyrénées', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Une singulière contingence l\' y amena .",
          ['Une', det:art, 'une', dr(0,lit(np(_,_,_)),n), 
           'singulière', adj, 'singulier', dr(0,n,n), 
           'contingence', nom, 'contingence', n, 
           'l\'', pro:per, 'le_la', cl_3a, 
           'y', pro:per, 'y', cl_y, 
           'amena', ver:simp, 'amener', dl(0,cl_y,dl(0,cl_3a,dl(0,lit(np(_,_,_)),s))), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Une singulière contingence l\' y amena .",
          ['Une', det:art, 'une', dr(0,lit(np(_,_,_)),n), 
           'singulière', adj, 'singulier', dr(0,n,n), 
           'contingence', nom, 'contingence', n, 
           'l\'', pro:per, 'le_la', cl_3a, 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), 
           'amena', ver:simp, 'amener', dr(0,dl(0,cl_3a,dl(0,lit(np(_,_,_)),s)),pp_a), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! A peine arrivé au Lac d\' Oncet , il est frappé du « grand caractère » qu\' y revêtent les rochers et les » sommets hérissés »",
          ['arrivé', ver:pper, 'arriver', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_a), 
           'au', prp:det, 'au', dr(0,pp_a,n), 
           'Lac', nam, 'Lac', n, 
           'd\'', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Oncet', nam, 'Oncet', lit(np(_,_,_)), 
           ',', pun, ',', dr(0,dl(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),lit(np(_,_,_))), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'frappé', ver:pper, 'frapper', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_de), 
           'du', prp:det, 'du', dr(0,pp_de,n), 
           'grand', adj, 'grand', dr(0,n,n), 
           'caractère', nom, 'caractère', n, 
           'qu\'', pro:rel, 'que', dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'y', pro:per, 'y', cl_y, 
           'revêtent', ver:pres, 'revêtir', dr(0,dr(0,dl(0,cl_y,s),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'rochers', nom, 'rocher', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'sommets', nom, 'sommet', n, 
           'hérissés', ver:pper, 'hérisser', dl(0,n,n),
	   '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "! A peine arrivé au Lac d\' Oncet , il est frappé du « grand caractère » qu\' y revêtent les rochers et les » sommets hérissés »",
          ['arrivé', ver:pper, 'arriver', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_a), 
           'au', prp:det, 'au', dr(0,pp_a,n), 
           'Lac', nam, 'Lac', n, 
           'd\'', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Oncet', nam, 'Oncet', lit(np(_,_,_)), 
           ',', pun, ',', dr(0,dl(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),lit(np(_,_,_))), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'frappé', ver:pper, 'frapper', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_de), 
           'du', prp:det, 'du', dr(0,pp_de,n), 
           'grand', adj, 'grand', dr(0,n,n), 
           'caractère', nom, 'caractère', n, 
           'qu\'', pro:rel, 'que', dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'y', pro:per, 'y', dr(0,s,s), 
           'revêtent', ver:pres, 'revêtir', dr(0,dr(0,s,lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'rochers', nom, 'rocher', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'sommets', nom, 'sommet', n, 
           'hérissés', ver:pper, 'hérisser', dl(0,n,n),
	   '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Arrivé à Gavarnie , il lui suffit d\' élever ses regards vers le Marbore , pour y distinguer « des glaces » , qui se dégageaient des neiges dont elles étaient entourées .",
          ['Arrivé', ver:pper, 'arriver', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_a), 
           'à', prp, 'à', dr(0,pp_a,lit(np(_,_,_))), 
           'Gavarnie', nam:lm, 'Gavarnie', lit(np(_,_,_)),
	   ',', pun, ',', dr(0,dl(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),lit(np(_,_,_))), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'lui', pro:per, 'lui', cl_3d, 
           'suffit', ver:pres, 'suffire', dr(0,dl(0,cl_3d,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_deinf)), 
           'd\'', prp, 'de', dr(0,dl(0,lit(np(_,_,_)),s_deinf),dl(0,lit(np(_,_,_)),s_inf)), 
           'élever', ver:infi, 'élever', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))), 
           'ses', det:pos, 'ses', dr(0,lit(np(_,_,_)),n), 
           'regards', nom, 'regard', n, 
           'vers', prp, 'vers', dr(0,dl(1,s,s),lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'Marbore', nam, 'Marbore', n, 
           'pour', prp, 'pour', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), 
           'y', pro:per, 'y', cl_y, 
           'distinguer', ver:infi, 'distinguer', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s_inf)),lit(np(_,_,_))), 
           'des', prp:det, 'des', dr(0,lit(np(_,_,_)),n), 
           'glaces', nom, 'glace', n, 
           'qui', pro:rel, 'qui', dr(0,dl(0,n,n),dl(0,lit(np(_,_,_)),s)), 
           'se', pro:per, 'se', cl_12r, 
           'dégageaient', ver:impf, 'dégager', dr(0,dl(0,cl_12r,dl(0,lit(np(_,_,_)),s)),pp_de), 
           'des', prp:det, 'des', dr(0,pp_de,n), 
           'neiges', nom, 'neige', n, 
           'dont', pro:rel, 'dont', dr(0,dl(0,n,n),dr(0,s,dia(1,box(1,pp_de)))), 
           'elles', pro:per, 'elles', lit(np(_,_,_)), 
           'étaient', ver:impf, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'entourées', ver:pper, 'entourer', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_de), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il lui tarde d\' atteindre la vallée d\' Aran , pour comparer les hauteurs qui la couronnent avec celles qu\' il vient d\' observer .",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'lui', pro:per, 'lui', cl_3d, 
           'tarde', ver:pres, 'tarder', dr(0,dl(0,cl_3d,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_deinf)), 
           'd\'', prp, 'de', dr(0,dl(0,lit(np(_,_,_)),s_deinf),dl(0,lit(np(_,_,_)),s_inf)), 
           'atteindre', ver:infi, 'atteindre', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'vallée', nom, 'vallée', n, 
           'd\'', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Aran', nam, 'Aran', lit(np(_,_,_)), 
           'pour', prp, 'pour', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), 
           'comparer', ver:infi, 'comparer', dr(0,dr(0,dl(0,lit(np(_,_,_)),s_inf),pp),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'hauteurs', nom, 'hauteur', n, 
           'qui', pro:rel, 'qui', dr(0,dl(0,n,n),dl(0,lit(np(_,_,_)),s)), 
           'la', pro:per, 'la', cl_3a, 
           'couronnent', ver:pres, 'couronner', dl(0,cl_3a,dl(0,lit(np(_,_,_)),s)), 
           'avec', prp, 'avec', dr(0,pp_avec,lit(np(_,_,_))), 
           'celles', pro:dem, 'celles', lit(np(_,_,_)), 
           'qu\'', pro:rel, 'que', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'vient', ver:pres, 'venir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_deinf)), 
           'd\'', prp, 'de', dr(0,dl(0,lit(np(_,_,_)),s_deinf),dl(0,lit(np(_,_,_)),s_inf)), 
           'observer', ver:infi, 'observer', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il y va directement par les cols et les forts .",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'va', ver:pres, 'aller', dl(0,cl_y,dl(0,lit(np(_,_,_)),s)), 
           'directement', adv, 'directement', dl(1,s,s), 
           'par', prp, 'par', dr(0,dl(1,s,s),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'cols', nom, 'col', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'forts', nom, 'fort', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il y va directement par les cols et les forts .",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), 
           'va', ver:pres, 'aller', dr(0,dl(0,lit(np(_,_,_)),s),pp_a), 
           'directement', adv, 'directement', dl(1,s,s), 
           'par', prp, 'par', dr(0,dl(1,s,s),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'cols', nom, 'col', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'forts', nom, 'fort', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


example_phrase(pos_lemma, 
          "! il se rendait à Barèges , dont les eaux furent toujours « nécessaires à sa santé » et les « âpres montagnes » chères à son coeur .",
          ['il', pro:per, 'il', lit(np(_,_,_)), 
           'se', pro:per, 'se', cl_12r, 
           'rendait', ver:impf, 'rendre', dr(0,dl(0,cl_12r,dl(0,lit(np(_,_,_)),s)),pp_a), 
           'à', prp, 'à', dr(0,pp_a,lit(np(_,_,_))), 
           'Barèges', nam, 'Barèges', lit(np(_,_,_)), 
           'dont', pro:rel, 'dont', dr(0,dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'eaux', nom, 'eau', n, 
           'furent', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,n,n)), 
           'nécessaires', adj, 'nécessaire', dr(0,dl(0,n,n),pp_a), 
           'à', prp, 'à', dr(0,pp_a,lit(np(_,_,_))), 
           'sa', det:pos, 'sa', dr(0,lit(np(_,_,_)),n), 
           'santé', nom, 'santé', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'âpres', adj, 'âpre', dr(0,n,n), 
           'montagnes', nom, 'montagne', n, 
           'chères', adj, 'cher', dr(0,dl(0,n,n),pp_a), 
           'à', prp, 'à', dr(0,pp_a,lit(np(_,_,_))), 
           'son', det:pos, 'son', dr(0,lit(np(_,_,_)),n), 
           'coeur', nom, 'coeur', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il y passait l\' été et les premiers jours de l\' automne ...",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'passait', ver:impf, 'passer', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'été', nom, 'été', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'premiers', num, 'premiers', dr(0,n,n), 
           'jours', nom, 'jours', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'automne', nom, 'automne', n, 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il y passait l\' été et les premiers jours de l\' automne ...",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'passait', ver:impf, 'passer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'été', nom, 'été', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'premiers', num, 'premiers', dr(0,n,n), 
           'jours', nom, 'jours', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'automne', nom, 'automne', n, 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "? J\' ai pris le parti de faire venir de Paris ce que j\' y avais laissé ...",
          ['J\'', pro:per, 'je', lit(np(_,_,_)), 
           'ai', ver:pres, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pris', ver:pper, 'pris', dr(0,dr(0,dl(0,lit(np(_,_,_)),s_ppart),dl(0,lit(np(_,_,_)),s_deinf)),lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'parti', nom, 'parti', n, 
           'de', prp, 'de', dr(0,dl(0,lit(np(_,_,_)),s_deinf),dl(0,lit(np(_,_,_)),s_inf)), 
           'faire', ver:infi, 'faire', dr(0,dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),s_inf)), 
           'venir', ver:infi, 'venir', dr(0,dl(0,lit(np(_,_,_)),s_inf),pp_de), 
           'de', prp, 'de', dr(0,pp_de,lit(np(_,_,_))), 
           'Paris', nam, 'Paris', lit(np(_,_,_)), 
           'ce', pro:dem, 'ce', lit(np(_,_,_)), 
           'que', pro:rel, 'que', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'j\'', pro:per, 'je', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'avais', ver:impf, 'avoir', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ppart)), 
           'laissé', ver:pper, 'laisser', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! A Maladette j\' y fus trompé un moment par rapport à son revêtement , j\' ai pu corriger mon erreur .",
          ['A', prp, 'a', dr(0,dr(0,s,s),lit(np(_,_,_))), 
           'Maladette', nam, 'Maladette', lit(np(_,_,_)), 
           'j\'', pro:per, 'je', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'fus', ver:simp, 'être', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ppart)), 
           'trompé', ver:pper, 'tromper', dl(0,lit(np(_,_,_)),s_ppart), 
           'un', det:art, 'un', dr(0,dl(1,s,s),n), 
           'moment', nom, 'moment', n, 
           'par', prp, 'par', dr(0,dl(1,s,s),n), 
           'rapport', nom, 'rapport', dr(0,n,pp_a), 
           'à', prp, 'à', dr(0,pp_a,lit(np(_,_,_))), 
           'son', det:pos, 'son', dr(0,lit(np(_,_,_)),n), 
           'revêtement', nom, 'revêtement', n, 
           ',', pun, ',', dr(0,dl(0,s,s),s), 
           'j\'', pro:per, 'je', lit(np(_,_,_)), 
           'ai', ver:pres, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pu', ver:pper, 'pu', dr(0,dl(0,lit(np(_,_,_)),s_ppart),dl(0,lit(np(_,_,_)),s_inf)), 
           'corriger', ver:infi, 'corriger', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))), 
           'mon', det:pos, 'mon', dr(0,lit(np(_,_,_)),n), 
           'erreur', nom, 'erreur', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Le lac ne m\' a offert ni plantes ni animaux .",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'lac', nom, 'lac', n, 
           'ne', adv, 'ne', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'm\'', pro:per, 'me', cl_12r, 
           'a', ver:pres, 'avoir', dr(0,dl(0,cl_12r,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ppart)), 
           'offert', ver:pper, 'offrir', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           'ni', kon, 'ni', dr(0,lit(np(_,_,_)),n), 
           'plantes', nom, 'plante', n, 
           'ni', kon, 'ni', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'animaux', nom, 'animal', lit(np(_,_,_)), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! En vain le soleil y répand la plus vive lumière ...",
          ['En', prp, 'en', dr(0,dr(0,s,s),n), 
           'vain', nom, 'vain', n, 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'soleil', nom, 'soleil', n, 
           'y', pro:per, 'y', cl_y, 
           'répand', ver:pres, 'répandre', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'plus', adv, 'plus', dr(0,dr(0,n,n),dr(0,n,n)), 
           'vive', adj, 'vif', dr(0,n,n), 
           'lumière', nom, 'lumière', n, 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Il y a un alpiniste en Ramond qui voudra triompher du Mont-Perdu , en raison même de la difficulté du geste .",
          ['Il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'a', ver:pres, 'avoir', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'alpiniste', nom, 'alpiniste', n, 
           'en', prp, 'en', dr(0,dl(0,n,n),n), 
           'Ramond', nam, 'Ramond', n, 
           'qui', pro:rel, 'qui', dr(0,dl(0,n,n),dl(0,lit(np(_,_,_)),s)), 
           'voudra', ver:futu, 'vouloir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_inf)), 
           'triompher', ver:infi, 'triompher', dr(0,dl(0,lit(np(_,_,_)),s_inf),pp_de), 
           'du', prp:det, 'du', dr(0,pp_de,n), 
           'Mont-Perdu', nam, 'Mont-Perdu', n, 
           'en', prp, 'en', dr(0,dl(1,s,s),n), 
           'raison', nom, 'raison', n, 
           'même', adj, 'même', dl(0,n,n), 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'difficulté', nom, 'difficulté', n, 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'geste', nom, 'geste', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Et s\' il n\' y est pas conduit par « le zèle de la science » , il sera du moins incité à y revenir par cette curiosité passionnée",
          ['Et', kon, 'et', dr(0,s,s), 
           's\'', kon, 'si', dr(0,dr(0,s,s),s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'n\'', adv, 'ne', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'y', pro:per, 'y', cl_y, 
           'est', ver:pres, 'être', dr(0,dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),pp_par),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pas', adv, 'pas', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'conduit', ver:pper, 'conduire', dl(0,lit(np(_,_,_)),s_ppart), 
           'par', prp, 'par', dr(0,pp_par,lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'zèle', nom, 'zèle', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'science', nom, 'science', n, 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'sera', ver:futu, 'être', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),pp_par),dl(0,lit(np(_,_,_)),s_ppart)), 
           'incité', ver:pper, 'inciter', dr(0,dl(0,lit(np(_,_,_)),s_ppart),dl(0,lit(np(_,_,_)),s_ainf)), 
           'à', prp, 'à', dr(0,dl(0,lit(np(_,_,_)),s_ainf),dl(0,lit(np(_,_,_)),s_inf)), 
           'y', pro:per, 'y', cl_y, 
           'revenir', ver:infi, 'revenir', dl(0,cl_y,dl(0,lit(np(_,_,_)),s_inf)), 
           'par', prp, 'par', dr(0,pp_par,lit(np(_,_,_))), 
           'cette', pro:dem, 'cette', dr(0,lit(np(_,_,_)),n), 
           'curiosité', nom, 'curiosité', n, 
           'passionnée', adj, 'passionner', dl(0,n,n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Et s\' il n\' y est pas conduit par « le zèle de la science » , il sera du moins incité à y revenir par cette curiosité passionnée",
          ['Et', kon, 'et', dr(0,s,s), 
           's\'', kon, 'si', dr(0,dr(0,s,s),s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'n\'', adv, 'ne', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), 
           'est', ver:pres, 'être', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),pp_par),dl(0,lit(np(_,_,_)),s_ppart)), 
           'pas', adv, 'pas', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'conduit', ver:pper, 'conduire', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp_a), 
           'par', prp, 'par', dr(0,pp_par,lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'zèle', nom, 'zèle', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'science', nom, 'science', n, 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'sera', ver:futu, 'être', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),pp_par),dl(0,lit(np(_,_,_)),s_ppart)), 
           'incité', ver:pper, 'inciter', dr(0,dl(0,lit(np(_,_,_)),s_ppart),dl(0,lit(np(_,_,_)),s_ainf)), 
           'à', prp, 'à', dr(0,dl(0,lit(np(_,_,_)),s_ainf),dl(0,lit(np(_,_,_)),s_inf)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), 
           'revenir', ver:infi, 'revenir', dr(0,dl(0,lit(np(_,_,_)),s_inf),pp_a), 
           'par', prp, 'par', dr(0,pp_par,lit(np(_,_,_))), 
           'cette', pro:dem, 'cette', dr(0,lit(np(_,_,_)),n), 
           'curiosité', nom, 'curiosité', n, 
           'passionnée', adj, 'passionner', dl(0,n,n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


example_phrase(pos_lemma, 
          "! ils sont les premiers en ces lieux et leur exemple y « amènera la foule » .",
          ['ils', pro:per, 'ils', lit(np(_,_,_)), 
           'sont', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'premiers', num, 'premiers', n, 
           'en', prp, 'en', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'ces', pro:dem, 'ces', dr(0,lit(np(_,_,_)),n), 
           'lieux', nom, 'lieux', n, 
           'et', kon, 'et', dr(0,dl(0,s,s),s), 
           'leur', det:pos, 'leur', dr(0,lit(np(_,_,_)),n), 
           'exemple', nom, 'exemple', n, 
           'y', pro:per, 'y', cl_y, 
           'amènera', ver:futu, 'amener', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'foule', nom, 'foule', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "! ils sont les premiers en ces lieux et leur exemple y « amènera la foule » .",
          ['ils', pro:per, 'ils', lit(np(_,_,_)), 
           'sont', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'premiers', num, 'premiers', n, 
           'en', prp, 'en', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'ces', pro:dem, 'ces', dr(0,lit(np(_,_,_)),n), 
           'lieux', nom, 'lieux', n, 
           'et', kon, 'et', dr(0,dl(0,s,s),s), 
           'leur', det:pos, 'leur', dr(0,lit(np(_,_,_)),n), 
           'exemple', nom, 'exemple', n, 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dr(0,dl(0,lit(np(_,_,_)),s),dia(1,box(1,pp_a)))), 
           'amènera', ver:futu, 'amener', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),pp_a),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'foule', nom, 'foule', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


example_phrase(pos_lemma, 
          "! Dès la rentrée de l\' Ecole centrale de Tarbes , le vingt brumaire , an VII ( 10 novembre 1798 ) ; nous y retrouvons Ramond , au premier plan .",
          ['Dès', prp, 'dès', dr(0,dr(0,s,s),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'rentrée', nom, 'rentrée', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'Ecole', nam, 'Ecole', n, 
           'centrale', adj, 'central', dl(0,n,n), 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Tarbes', nam:lm, 'Tarbes', lit(np(_,_,_)), 
           ',', pun, ',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'vingt', num, 'vingt', dr(0,n,n), 
           'brumaire', nom, 'brumaire', n, 
           ',', pun, ',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),n), 
           'an', nom, 'an', n, 
           'VII', num, 'VII', dl(0,n,n), 
           '(', pun, '(', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           '10', num, '10', dr(0,lit(np(_,_,_)),n), 
           'novembre', nom, 'novembre', n, 
           '1798', num, '1798', dl(0,n,n), 
            'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'retrouvons', ver:pres, 'retrouver', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'Ramond', nam, 'Ramond', lit(np(_,_,_)), 
           'au', prp:det, 'au', dr(0,dl(1,s,s),n), 
           'premier', num, 'premier', dr(0,n,n), 
           'plan', nom, 'plan', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "! Dès la rentrée de l\' Ecole centrale de Tarbes , le vingt brumaire , an VII ( 10 novembre 1798 ) ; nous y retrouvons Ramond , au premier plan .",
          ['Dès', prp, 'dès', dr(0,dr(0,s,s),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'rentrée', nom, 'rentrée', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'Ecole', nam, 'Ecole', n, 
           'centrale', adj, 'central', dl(0,n,n), 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Tarbes', nam:lm, 'Tarbes', lit(np(_,_,_)), 
           ',', pun, ',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'vingt', num, 'vingt', dr(0,n,n), 
           'brumaire', nom, 'brumaire', n, 
           ',', pun, ',', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),n), 
           'an', nom, 'an', n, 
           'VII', num, 'VII', dl(0,n,n), 
           '(', pun, '(', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           '10', num, '10', dr(0,lit(np(_,_,_)),n), 
           'novembre', nom, 'novembre', n, 
           '1798', num, '1798', dl(0,n,n), 
            'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s)), 
           'retrouvons', ver:pres, 'retrouver', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'Ramond', nam, 'Ramond', lit(np(_,_,_)), 
           'au', prp:det, 'au', dr(0,dl(1,s,s),n), 
           'premier', num, 'premier', dr(0,n,n), 
           'plan', nom, 'plan', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "? Ce que nous savons , c\' est que , pendant l\' été de 1799 , Saint-Amans revient aux Pyrénées pour y rejoindre Ramond .",
          ['Ce', pro:dem, 'ce', lit(np(_,_,_)), 
           'que', pro:rel, 'que', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dr(0,s,dia(1,box(1,lit(np(_,_,_)))))), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'savons', ver:pres, 'savoir', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           ',', pun, ',', dr(0,dl(0,lit(np(_,_,_)),s),s), 
           'c\'', pro:dem, 'ce', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),cs), 
           'que', kon, 'que', dr(0,cs,s), 
           'pendant', prp, 'pendant', dr(0,dr(0,s,s),lit(np(_,_,_))), 
           'l\'', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'été', nom, 'été', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           '1799', num, '1799', lit(np(_,_,_)), 
           'Saint-Amans', nam:lm, 'Saint-Amans', lit(np(_,_,_)), 
           'revient', ver:pres, 'revenir', dr(0,dl(0,lit(np(_,_,_)),s),pp_a), 
           'aux', prp:det, 'aux', dr(0,pp_a,n), 
           'Pyrénées', nam:lfp, 'Pyrénées', n, 
           'pour', prp, 'pour', dr(0,dl(1,s,s),dl(0,lit(np(_,_,_)),s_inf)), 
           'y', pro:per, 'y', cl_y, 
           'rejoindre', ver:infi, 'rejoindre', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s_inf)),lit(np(_,_,_))), 
           'Ramond', nam, 'Ramond', lit(np(_,_,_)), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "! Ramond est bien sans aucun doute un amant passionné de la montagne : il y vient passer trois mois par an et il vit dans l\' enchantement .",
          ['Ramond', nam, 'Ramond', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'amant', nom, 'amant', n, 
           'passionné', ver:pper, 'passionner', dl(0,n,n), 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'montagne', nom, 'montagne', n, 
           ':', pun, ':', dr(0,dl(0,s,s),s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'vient', ver:pres, 'venir', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_inf)), 
           'passer', ver:infi, 'passer', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))), 
           'trois', num, 'trois', dr(0,lit(np(_,_,_)),n), 
           'mois', nom, 'mois', n, 
           'par', prp, 'par', dr(0,dl(0,n,n),n), 
           'an', nom, 'an', n, 
           'et', kon, 'et', dr(0,dl(0,s,s),s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'vit', ver:pres, 'vivre', dr(0,dl(0,lit(np(_,_,_)),s),pp), 
           'dans', prp, 'dans', dr(0,pp_dans,lit(np(_,_,_))), 
           'l\'', det:art, 'le_la', dr(0,lit(np(_,_,_)),n), 
           'enchantement', nom, 'enchantement', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "Là se trouve un petit plateau très-herbeux , mais très-incliné .",
          ['Là', adv, 'là', dr(0,s,s), 
           'se', pro:per, 'se', cl_12r, 
           'trouve', ver:pres, 'trouver', dr(0,dl(0,cl_12r,s),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'petit', adj, 'petit', dr(0,n,n), 
           'plateau', nom, 'plateau', n, 
           'très-herbeux', adj, 'très-herbeux', dl(0,n,n), 
           'mais', kon, 'mais', dr(0,dl(0,dl(0,n,n),dl(0,n,n)),dl(0,n,n)), 
           'très-incliné', ver:pper, 'très-incliné', dl(0,n,n), 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


example_phrase(pos_lemma, 
          "Nous y rencontrâmes un troupeau et son berger ...",
          ['Nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'rencontrâmes', ver:simp, 'rencontrer', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),lit(np(_,_,_))), 
           'un', det:art, 'un', dr(0,lit(np(_,_,_)),n), 
           'troupeau', nom, 'troupeau', n, 
           'et', kon, 'et', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),lit(np(_,_,_))), 
           'son', det:pos, 'son', dr(0,lit(np(_,_,_)),n), 
           'berger', nom, 'berger', n, 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "mais il connaissait fort bien le col de Fanlo , qui est désigné ici sous le nom de col de Niscle , et il s\' engagea à nous y conduire le lendemain .",
          ['mais', kon, 'mais', dr(0,s,s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           'connaissait', ver:impf, 'connaître', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'col', nom, 'col', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Fanlo', nam, 'Fanlo', lit(np(_,_,_)), 
           'qui', pro:rel, 'qui', dr(0,dl(0,n,n),dl(0,lit(np(_,_,_)),s)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'désigné', ver:pper, 'désigner', dr(0,dl(0,lit(np(_,_,_)),s_ppart),pp), 
           'sous', prp, 'sous', dr(0,pp_sous,lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'nom', nom, 'nom', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),n), 
           'col', nom, 'col', n, 
           'de', prp, 'de', dr(0,dl(0,n,n),lit(np(_,_,_))), 
           'Niscle', nam, 'Niscle', lit(np(_,_,_)), 
           'et', kon, 'et', dr(0,dl(0,s,s),s), 
           'il', pro:per, 'il', lit(np(_,_,_)), 
           's\'', pro:per, 'se', cl_12r, 
           'engagea', ver:simp, 'engager', dr(0,dl(0,cl_12r,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ainf)), 
           'à', prp, 'à', dr(0,dl(0,lit(np(_,_,_)),s_ainf),dl(0,lit(np(_,_,_)),s_inf)), 
           'nous', pro:per, 'nous', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'conduire', ver:infi, 'conduire', dl(0,cl_y,dl(0,lit(np(_,_,_)),s_inf)), 
           'le', det:art, 'le', dr(0,dl(1,s,s),n), 
           'lendemain', nom, 'lendemain', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "Au reste , ces êtres organiques sont les derniers que j\' aie rencontrés à la cime du Mont-Perdu .",
          ['Au', prp:det, 'Au', dr(0,dr(0,s,s),n), 
           'reste', nom, 'reste', n, 
           'ces', pro:dem, 'ces', dr(0,lit(np(_,_,_)),n), 
           'êtres', nom, 'être', n, 
           'organiques', adj, 'organique', dl(0,n,n), 
           'sont', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'les', det:art, 'les', dr(0,lit(np(_,_,_)),n), 
           'derniers', adj, 'dernier', n, 
           'que', pro:rel, 'que', dr(0,dl(0,n,n),dr(0,s,dia(0,box(0,lit(np(_,_,_)))))), 
           'j\'', pro:per, 'je', lit(np(_,_,_)), 
           'aie', ver:subp, 'avoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_ppart)), 
           'rencontrés', ver:pper, 'rencontrer', dr(0,dl(0,lit(np(_,_,_)),s_ppart),lit(np(_,_,_))), 
           'à', prp, 'à', dr(0,dl(1,s,s),lit(np(_,_,_))), 
           'la', det:art, 'la', dr(0,lit(np(_,_,_)),n), 
           'cime', nom, 'cime', n, 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'Mont-Perdu', nam, 'Mont-Perdu', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "J\' y ai séjourné deux heures ...",
          ['J\'', pro:per, 'je', lit(np(_,_,_)), 
           'y', pro:per, 'y', cl_y, 
           'ai', ver:pres, 'avoir', dr(0,dl(0,cl_y,dl(0,lit(np(_,_,_)),s)),dl(0,lit(np(_,_,_)),s_ppart)), 
           'séjourné', ver:pper, 'séjourner', dl(0,lit(np(_,_,_)),s_ppart), 
           'deux', num, 'deux', dr(0,dl(1,s,s),n), 
           'heures', nom, 'heure', n, 
           '...', pun, '...', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "Le chien dort .",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'chien', nom, 'chien', lit(n),
	   'dort', ver:pres, 'dormir', dl(0,lit(np(_,_,_)),s),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Nous aimons Marie .",
          ['Nous', pro:per, 'nous', lit(np(_,_,_)), 
           'aimons', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))),
	   'Marie', nam, 'Marie', lit(np(_,_,_)),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean semble stupide .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'semble', ver:pres, 'sembler', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,n,n)),
	   'stupide', adj, 'stupide', dl(0,n,n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean est très stupide .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,n,n)),
	   'très', adv, 'très', dr(0,dl(0,n,n),dl(0,n,n)),
	   'stupide', adj, 'stupide', dl(0,n,n),
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Ce qui devrait permettre d' empêcher une augmentation mécanique du chômage .",
          ['Ce', pro:dem, 'ce', lit(np(_,_,_)), 
           'qui', pro:rel, 'qui', dr(0,dl(0,lit(np(_,_,_)),lit(np(_,_,_))),dl(0,lit(np(_,_,_)),s)), 
           'devrait', ver:cond, 'devoir', dr(0,dl(0,lit(np(_,_,_)),s),dl(0,lit(np(_,_,_)),s_inf)), 
           'permettre', ver:infi, 'permettre', dr(0,dl(0,lit(np(_,_,_)),s_inf),dl(0,lit(np(_,_,_)),s_deinf)), 
           'd\'', prp, 'de', dr(0,dl(0,lit(np(_,_,_)),s_deinf),dl(0,lit(np(_,_,_)),s_inf)), 
           'empêcher', ver:infi, 'empêcher', dr(0,dl(0,lit(np(_,_,_)),s_inf),lit(np(_,_,_))), 
           'une', det:art, 'une', dr(0,lit(np(_,_,_)),n), 
           'augmentation', nom, 'augmentation', n, 
           'mécanique', adj, 'mécanique', dl(0,n,n), 
           'du', prp:det, 'du', dr(0,dl(0,n,n),n), 
           'chômage', nom, 'chômage', n, 
           '.', pun, '.', dl(0,lit(np(_,_,_)),txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean aime Marie .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'aime', ver:pres, 'aimer', dr(0,dl(0,lit(np(_,_,_)),s),lit(np(_,_,_))), 
           'Marie', nam, 'Marie', np, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean est dans le merde .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'est', ver:pres, 'être', dr(0,dl(0,lit(np(_,_,_)),s),pp), 
           'dans', prp, 'dans', dr(0,pp_dans,lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'merde', nom, 'merde', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).
example_phrase(pos_lemma, 
          "Jean met Marie dans le merde .",
          ['Jean', nam, 'Jean', lit(np(_,_,_)), 
           'met', ver:pres, 'mettre', dr(0,dr(0,dl(0,lit(np(_,_,_)),s),pp),lit(np(_,_,_))), 
           'Marie', nam, 'Marie', lit(np(_,_,_)), 
           'dans', prp, 'dans', dr(0,pp_dans,lit(np(_,_,_))), 
           'le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'merde', nom, 'merde', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).

example_phrase(pos_lemma, 
          "L'interêt passe de 5,5 à 3,7 milliards .",
          ['Le', det:art, 'le', dr(0,lit(np(_,_,_)),n), 
           'interêt', nom, 'interêt', n, 
           'passse', ver:ppre, 'passer', dr(0,dl(0,lit(np(_,_,_)),s),pp), 
           'de', prp, 'de', dr(0,pp_de,lit(np(_,_,_))), 
           '5,5', num, '5,5', dr(0,lit(np(_,_,_)),n), 
           'à', prp, 'à', dr(0,dl(0,dr(0,lit(np(_,_,_)),n),dl(0,dr(0,pp_de,lit(np(_,_,_))),dr(0,pp_a,n))),dr(0,lit(np(_,_,_)),n)), 
           '3,7', num, '3,7', dr(0,lit(np(_,_,_)),n), 
           'milliards', nom, 'milliard', n, 
           '.', pun, '.', dl(0,s,txt)],
           lit(txt)).


translate_sem(T0,T) :-
	presupposition_projection(T0, T1),
	format(user_error, 'HOOK:~n', [T0]),
	simplify_drss(T1, T).


% TODO: complete

presupposition_projection(DRS, DRS).

%presupposition_projection(DRS0, DRS) :-
%	select_presupposition(DRS0, DRS1).


select_presupposition(presup(X,Y), X, Y).
select_presupposition(drs(X,Y0), P, drs(X, Y)) :-
	select_presupposition_list(Y0, P, Y).


simplify_drss(drs(V,C), DRS) :-
	simplify_drs(C, V, DRS0),
	!,
	simplify_drss(DRS0, DRS).
simplify_drss(drs(V,C0), drs(V, C)) :-
	simplify_sub_drss(C0, C),
	!.
simplify_drss(DRS, DRS).

simplify_sub_drss([C|Cs], [D|Cs]) :-
	simplify_condition(C, D),
	!.
simplify_sub_drss([C|Cs], [C|Ds]) :-
	simplify_sub_drss(Cs, Ds).

simplify_condition(not(drs(V,C)), not(DRS)) :-
	simplify_drs(C, V, DRS).
simplify_condition(bool(drs(V,C),Cn,DRSB),bool(DRS,Cn,DRSB)) :-
	simplify_drs(C, V, DRS).
simplify_condition(bool(DRSB,Cn,drs(V,C)),bool(DRSB,Cn,DRS)) :-
	simplify_drs(C, V, DRS).
simplify_condition(presup(drs(V,C),DRSB),presup(DRS,DRSB)) :-
	simplify_drs(C, V, DRS).
simplify_condition(presup(DRSB,drs(V,C)),presup(DRSB,DRS)) :-
	simplify_drs(C, V, DRS).
simplify_condition(drs_label(L,drs(V,C)),drs_label(L,DRS)) :-
	simplify_drs(C, V, DRS).

% simplify_drs(C0, V0, drs(V,[appl(appl(appl(travel,X),Path),E),appl(moving,X),appl(path,Path),appl(appl(goal,LG),path)|C3])) :-
% 	select_all([appl(event,E),
% 		    appl(aller,E),
% 		    appl(appl(theme,X),E),
% 		    bool(Y,=,'lieu?'),
% 		    appl(appl(source,Y),E),
% 		    appl(appl(à,LG),E)], C0, C1),
% 	!,
%     (
%         select(appl(appl(de,LS),E), C1, C2)
%     ->
%         Source = [appl(appl(source,LS),Path)]
%     ;
%         Source = [],
%         C2 = C1
%     ),
%     (
%         select(appl(appl(par,LM),E), C2, C3)
%     ->
%         Mid = [appl(appl(mid,LM),Path)|Source]
%     ;
%         Mid = Source,
%         C3 = C2
%     ),
%         select(Y, V0, V).

% simplify_drs(C0, V, drs(V,[appl(appl(cross,Path),LM)|C])) :-
% 	select(appl(appl(par,LM),E), C0, C1),
% 	select(appl(appl(appl(travel,_),Path),E), C1, C).


simplify_drs(C0, V, drs(V,[appl(F,E)|C])) :-
	select(appl(F0,E0), C0, C),
	member(appl(event,E0), C),
	format(user_error, 'EVENT: ~n', [E0]),
	replace_event_arg(F0, E0, F, E),
	E0 \== E,
	!.

replace_event_arg(appl(F,sub(E,B)), _, appl(F,B), E) :-
	!.
replace_event_arg(appl(F0,A0), E0, appl(F,A), E) :-
	replace_event_arg(F0, E0, E1, F),
	replace_event_arg(A0, E1, E, A).
