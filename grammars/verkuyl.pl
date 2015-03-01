% -*- Mode: Prolog -*-
% ============================================================
% big_french_drt.pl
% ============================================================
% !grail 3.1.1

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
atomic_type(s, t).
atomic_type(s_inf, e->t).
atomic_type(s_ppart, e->t).
atomic_type(s(_), t).
atomic_type(cs, t).
atomic_type(txt, t).

% =


continuous(0).
continuous_dia(0).

external(_).

% Verkuyl 2008, p. 83

tense(pres, lambda(Phi,quant(exists,I,bool(appl(Phi,I),&,bool(I,overlaps,n))))).
tense(past, lambda(Phi,quant(exists,I,bool(appl(Phi,I),&,bool(I,<,n))))).

post(lambda(Phi, lambda(I, quant(exists,J,bool(appl(Phi,J),&,bool(I,<,J)))))).
post_imm(lambda(Phi, lambda(I, quant(exists,J,bool(appl(Phi,J),&,bool(I,abuts,J)))))).
perf(lambda(Phi, lambda(I, quant(exists,K,bool(appl(Phi,K),&,bool(K,<,I)))))).
impf(lambda(Phi, lambda(I, quant(exists,K,bool(appl(Phi,K),&,bool(I,subseteq,K)))))).
anch(lambda(Phi, lambda(J, quant(exists,K,bool(appl(Phi,K),&,appl(appl('A',J),K)))))).

% Verkuyl 2008, p. 54.

perf1(lambda(Phi,lambda(J1,quant(exists,K,bool(appl(Phi,K),&,bool(K,prec,J1)))))).
imp1(lambda(Phi,lambda(J1,quant(exists,K,bool(appl(Phi,K),&,bool(K,preceq,J1)))))).

% Verkuyl 2008, p. 65.

post1(lambda(Phi,lambda(I1,quant(exists,J,bool(appl(Phi,J),&,bool(sub(I1,a),<,J)))))).
syn1(lambda(Phi,lambda(I1,quant(exists,J,bool(appl(Phi,J),&,bool(I1,simeq,J)))))).



% = French lexicon

lex(marie, np, m).
lex(dort, dl(0,np,s), lambda(X,appl(Pres,appl(Syn,appl(Imp,lambda(E,appl(appl(sleep,X),E))))))) :-
	syn1(Syn),
	imp1(Imp),
	tense(pres, Pres).
lex(dormait, dl(0,np,s), lambda(X,appl(Past,appl(Syn,appl(Imp,lambda(E,appl(appl(sleep,X),E))))))) :-
	syn1(Syn),
	imp1(Imp),
	tense(past, Past).
lex(dormit, dl(0,np,s), lambda(X,appl(Past,appl(Anch,lambda(E,appl(appl(sleep,X),E)))))) :-
	anch(Anch),
	tense(past, Past).
lex(dormira, dl(0,np,s), lambda(X,appl(Pres,appl(Post,appl(Imp,lambda(E,appl(appl(sleep,X),E))))))) :-
	post1(Post),
	imp1(Imp),
	tense(pres, Pres).
lex(dormirait, dl(0,np,s), lambda(X,appl(Past,appl(Post,lambda(E,appl(appl(sleep,X),E)))))) :-
	post1(Post),
	tense(past, Past).
% = past participle
lex(dormi, dl(0,np,s_ppart), lambda(X,appl(Perf,appl(sleep,X)))) :-
	perf1(Perf).
% = past participle
lex(dormir, dl(0,np,s_inf), sleep).
% = auxiliary
lex(a, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Pres, appl(Ppart,NP))))) :-
	tense(pres, Pres).
lex(avait, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Past, appl(Impf, appl(Ppart,NP)))))) :-
	imp1(Impf),
	tense(past, Past).
lex(aura, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Pres, appl(Post, appl(Ppart,NP)))))) :-
	post1(Post),
	tense(pres, Pres).
lex(aurait, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Pres, appl(Post, appl(Impf, appl(Ppart,NP))))))) :-
	post1(Post),
	imp1(Impf),
	tense(pres, Pres).
lex(eut, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Past, appl(Anch, appl(Ppart,NP)))))) :-
	anch(Anch),
	tense(past, Past).
lex(eu, dr(0,dl(0,np,s_ppart),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Perf, appl(Ppart,NP))))) :-
	perf(Perf).
% = aller
lex(va, dr(0,dl(0,np,s),dl(0,np,s_inf)), lambda(Inf,lambda(NP,appl(Pres, appl(Post,appl(Inf,NP)))))) :-
	tense(pres, Pres),
	post_imm(Post).
lex(allait, dr(0,dl(0,np,s),dl(0,np,s_inf)), lambda(Inf,lambda(NP,appl(Past, appl(Impf, appl(Post,appl(Inf,NP))))))) :-
	tense(past, Past),
	imp1(Impf),
	post_imm(Post).

% = English lexicon

lex(mary, np, m).
lex(sleeps, dl(0,np,s), lambda(X,appl(Pres,lambda(E,appl(appl(sleep,X),E))))) :-
	tense(pres, Pres).
lex(slept, dl(0,np,s), lambda(X,appl(Pres,lambda(E,appl(appl(sleep,X),E))))) :-
	tense(past, Pres).
lex(slept, dl(0,np,s), lambda(X,appl(Pres,lambda(E,appl(appl(sleep,X),E))))) :-
	tense(past, Pres).
lex(sleep, dl(0,np,s_inf), lambda(X,lambda(V,appl(appl(sleep,X),V)))).
lex(slept, dl(0,np,s_ppart), lambda(X,lambda(V,appl(appl(sleep,X),V)))).
lex(will, dr(0,dl(0,np,s),dl(0,np,s_inf)), lambda(Inf,lambda(NP,appl(Pres, appl(Post,appl(Inf,NP)))))) :-
	tense(pres, Pres),
	post(Post).
lex(would, dr(0,dl(0,np,s),dl(0,np,s_inf)), lambda(Inf,lambda(NP,appl(Past, appl(Post,appl(Inf,NP)))))) :-
	tense(past, Past),
	post(Post).
lex(have, dr(0,dl(0,np,s_inf),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Perf,appl(Ppart,NP))))) :-
	perf(Perf).
lex(has, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Pres, appl(Perf,appl(Ppart,NP)))))) :-
	tense(pres, Pres),
	perf(Perf).
lex(had, dr(0,dl(0,np,s),dl(0,np,s_ppart)), lambda(Ppart,lambda(NP,appl(Past, appl(Perf,appl(Ppart,NP)))))) :-
	tense(past, Past),
	perf(Perf).
							     
example("--- English ---", xxx).
example("Mary sleeps.", s).
example("Mary slept.", s).
example("Mary has slept.", s).
example("Mary had slept.", s).
example("Mary will sleep.", s).
example("Mary would sleep.", s).
example("Mary will have slept.", s).
example("Mary would have slept.", s).

example("--- French ---", xxx).
example("Marie dort.", s). % 1a
example("Marie dormait.", s). % 1b
example("Marie a dormi.", s). % 2a
example("Marie avait dormi.", s). % 2b

example("Marie dormira.", s). % 3a
example("Marie dormirait.", s). % futur du passé
example("Marie aurait dormi.", s). % 3b
example("Marie aura dormi.", s). % 4a
example("Marie avait dormi.", s). % 4b
example("Marie a eu dormi.", s). % 5a
example("Marie avait eu dormi.", s). % 5b
example("Marie aura eu dormi.", s). % Fa surcomposé
example("Marie aurait eu dormi.", s). % Fadp surcomposé
example("Marie dormit.", s). % 6b
example("Marie eut dormi.", s). % 7b
example("Marie va dormir.", s).	% future périphrastique
example("Marie allait dormir.", s). % future périphrastique du passé