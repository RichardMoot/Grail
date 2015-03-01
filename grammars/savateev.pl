% -*- Mode: Prolog -*-
% Proof Generator For Grail 3 Based on Savateev's (2008) encoding of
% the satisfyability problem in the product-free Lambek calculus.
% Copyright (C) 2008-2009 Richard Moot (Richard.Moot@labri.fr)

% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.

% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

% = Associativity for L

external(a).

continuous(a).

conversion([A-p(a,B,C),C-p(a,D,F)],[E-p(a,B,D),A-p(a,E,F)],[E], 'Ass_l').
conversion([A-p(a,E,F),E-p(a,B,D)],[C-p(a,D,F),A-p(a,B,C)],[C], 'Ass_r').

% a
% 1 clause, 1 variable
% 48 atomic formulas



example_phrase(prove, "A", Ant, Goal) :-
	sat_to_lambek([[1-1]], Ant, Goal).

% ~a
% 1 clause, 1 variable
% 48 atomic formulas



example_phrase(prove, "~A", Ant, Goal) :-
	sat_to_lambek([[1-0]], Ant, Goal).

% a \/ ~b
% 1 clause, 2 variables
% 90 atomic formulas

example_phrase(prove, "A \\/ ~B", Ant, Goal) :-
	sat_to_lambek([[1-1,2-0]], Ant, Goal).

% (~a \/ b) /\ (~b \/ a)
% a = b = 0,1
% 2 clauses, 2 variables
% 150 atomic formulas

example_phrase(prove, "(~A \\/ B) /\\ (~B \\/ A)", Ant, Goal) :-
	sat_to_lambek([[1-0,2-1],[2-0,1-1]], Ant, Goal).

% a /\ ~a
% FAIL
% 2 clauses, 1 variable
% 80 atomic formulas, fails quickly on ACC

example_phrase(prove, "A /\\ ~A", Ant, Goal) :-
	sat_to_lambek([[1-1],[1-0]], Ant, Goal).

% a \/ ~a
% a = 0,1
% 1 clause, 1 variable
% 48 atomic formulas


example_phrase(prove, "A \\/ ~A", Ant, Goal) :-
	sat_to_lambek([[1-1,1-0]], Ant, Goal).

% (a \/ ~a) /\ (b \/ ~b)
% a,b = 0,1
% 2 clauses, 2 variables
% 150 atomic formulas

example_phrase(prove, "(A \\/ ~A) /\\ (B \\/ ~B)", Ant, Goal) :-
	sat_to_lambek([[1-1,1-0],[2-1,2-0]], Ant, Goal).	

% (a \/ b) /\ ~b /\ (~a \/ b)
% FAIL
% 3 clauses, 2 variables
% 210 atomic formulas

example_phrase(prove, "(A \\/ B) /\\ ~B /\\ (~A \\/ B)", Ant, Goal) :-
	sat_to_lambek([[1-1,2-1],[2-0],[1-0,2-1]], Ant, Goal).

% = sat_to_lambek(+Clauses, -Antecedent, -GoalFormula)
%
% true if Antecedent |- GoalFormula is the translation proposed by
% Savateev (2008) which decides propositional satisfiability of 
% Clauses, which is a set of clauses in Conjunction Normal Form.

sat_to_lambek(Clauses, Ant, Goal) :-
	variables(Clauses, N),
	length(Clauses, M),
	reverse(Clauses, ClausesR),
	sat_to_lambek1(N, ClausesR, AntR, M),
	reverse(AntR, Ant),
	savateev_g(N, M, Goal),
	total_count_atoms([Goal|Ant], List0),
	filter_zeros(List0, List),
	portray_list(List),
	length(List0, Len),
	format('Atomic formulas: ~w~n', [Len]),
	max_order([Goal|Ant], Order),
	format('Maximum order: ~w~n', [Order]).	

sat_to_lambek1(N0, Cs, Fs, M) :-
    (
	N0 = 0
    ->
	Fs = []
    ;
	N is N0 - 1,
	Fs = [dl(a,dl(a,FD,FC),F1),FH,dr(a,F0,dl(a,FB,FA))|Fs0],
	savateev_f(N0, M, Cs, 0, F0),
	savateev_f(N0, M, Cs, 1, F1),
	savateev_h(N0, M, FH),
	savateev_a(N0, M, FA),
	savateev_b(N0, M, FB),
	savateev_c(N0, M, FC),
	savateev_d(N0, M, FD),
	sat_to_lambek1(N, Cs, Fs0, M)
    ).


savateev_a(I, J0, F) :-
	savateev_atom(a, I, J0, FAIJ),
	savateev_atom(p, I, J0, FPIJ),
    (
	J0 =:= 0
    ->
	F = dl(a,FAIJ,FPIJ)
    ;
	J1 is J0 - 1,
	savateev_atom(b, I, J0, FBIJ),
	savateev_atom(q, I, J0, FQIJ),
	savateev_a(I, J1, FFA),
	F = dl(a,dr(a,FQIJ,dl(a,dl(a,FBIJ,FAIJ),FFA)),FPIJ)
    ).

savateev_b(I0, M, dl(a,FBIM,FPI1M)) :-
	I1 is I0 - 1,
	savateev_atom(p, I1, M, FPI1M),
	savateev_b1(I0, M, FBIM).

savateev_b1(I0, J0, F) :-
	savateev_atom(a, I0, J0, FAIJ),
    (
	J0 =:= 0
    ->
	F = FAIJ
    ;
	I1 is I0 - 1,
	J1 is J0 - 1,
	savateev_atom(b, I0, J0, FBIJ),
	savateev_atom(p, I1, J1, FPI1J1),
	savateev_atom(q, I1, J0, FQI1J),
	savateev_b1(I0, J1, FFB),
	F = dr(a,FQI1J,dl(a,dl(a,dr(a,FBIJ,FFB),FAIJ),FPI1J1))
    ).

savateev_c(I, J0, F) :-
	savateev_atom(c, I, J0, FCIJ),
	savateev_atom(p, I, J0, FPIJ),
    (
	J0 =:= 0
    ->
	F = dl(a,FCIJ,FPIJ)
    ;
	J1 is J0 - 1,
	savateev_atom(d, I, J0, FDIJ),
	savateev_atom(q, I, J0, FQIJ),
	savateev_c(I, J1, FFC),
	F = dl(a,dr(a,FQIJ,dl(a,dl(a,FDIJ,FCIJ),FFC)),FPIJ)
    ).

savateev_d(I0, M, dl(a,FDIM,FPI1M)) :-
	I1 is I0 - 1,
	savateev_atom(p, I1, M, FPI1M),
	savateev_d1(I0, M, FDIM).

savateev_d1(I0, J0, F) :-
	savateev_atom(c, I0, J0, FCIJ),
    (
	J0 =:= 0
    ->
	F = FCIJ
    ;
	I1 is I0 - 1,
	J1 is J0 - 1,
	savateev_atom(d, I0, J0, FDIJ),
	savateev_atom(p, I1, J1, FPI1J1),
	savateev_atom(q, I1, J0, FQI1J),
	savateev_d1(I0, J1, FFD),
	F = dr(a,FQI1J,dl(a,dl(a,dr(a,FDIJ,FFD),FCIJ),FPI1J1))
    ).


savateev_e(I0, J0, Cs0, T, F) :-
	I1 is I0-1,
	savateev_atom(p, I1, J0, FPI1J),
    (
	J0 =:= 0
    ->
	F = FPI1J
    ;
	Cs0 = [C|Cs],
	J1 is J0-1,
	savateev_atom(p, I0, J1, FPIJ1),	
	savateev_atom(q, I0, J0, FQIJ),	
	savateev_atom(q, I1, J0, FQI1J),	
	savateev_e(I0, J1, Cs, T, FE),
        (
	    subliteral(C, T, I0)
	->
	    F = dr(a,FQIJ,dl(a,dl(a,dr(a,FQI1J,FE),FPI1J),FPIJ1))
	;
	    F = dl(a,dr(a,FQI1J,dr(a,FQIJ,dl(a,FE,FPIJ1))),FPI1J)
	)
    ).

	
savateev_f(N, M, Cs, B, dl(a,FE,FP)) :-
	savateev_atom(p, N, M, FP),
	savateev_e(N, M, Cs, B, FE). 

savateev_g(N, J0, F) :-
	savateev_atom(p, 0, J0, FP0J),
	savateev_atom(p, N, J0, FPNJ),
    (
	J0 =:= 0
    ->
	F = dl(a, FP0J, FPNJ)
    ;
	J is J0 - 1,
	savateev_atom(q, 0, J0, FQ0J),
	savateev_atom(q, N, J0, FQNJ),
	F = dl(a,dr(a,FQNJ,dl(a,dl(a,FQ0J,FP0J),FG)),FPNJ),
	savateev_g(N, J, FG)
    ).


savateev_h(I0, J0, F) :-
	I1 is I0 - 1,
	savateev_atom(p, I1, J0, FPI1J),
	savateev_atom(p, I0, J0, FPIJ),
    (
	J0 =:= 0
    ->
	F = dl(a,FPI1J,FPIJ)
    ;
	J1 is J0 - 1,
	savateev_atom(q, I1, J0, FQI1J),
	savateev_atom(q, I0, J0, FQIJ),
	F = dl(a,dl(a,dr(a,FQI1J,dr(a,FQIJ,FH)),FPI1J),FPIJ),
	savateev_h(I0, J1, FH)
    ).

%
% compute an atomic formula for p_N^M0
%

savateev_atom(A, N, M, lit(Atom)) :-
	concat_atom([A, N, '_', M], Atom).

variables(Cs, V) :-
	variables(Cs, [], Vs),
	coherent(Vs, 1),
	last(Vs, V).

variables([], Vs, Vs).
variables([C|Cs], Vs0, Vs) :-
	variables1(C, Vs0, Vs1),
	variables(Cs, Vs1, Vs).

variables1([], Vs, Vs).
variables1([V-B|Es], Vs0, Vs) :-
    (
	B == 0
    ->
	true
    ;
	B == 1
    ->
	true
    ;
	throw('Values of propositional variables must be 0 or 1')
    ),
	ord_insert(Vs0, V, Vs1),
	variables1(Es, Vs1, Vs).

coherent([], _).
coherent([V0|Vs], V1) :-
    (
	V0 == V1
    ->
	true
    ;
	throw('Propositional variables must be numbered 1,...,n')
    ),
	V is V1+1,
	coherent(Vs, V).

subliteral([L|Ls], T, C) :-
    (
	L = C-T
    ->
	true
    ;
	subliteral(Ls, T, C)
    ).

filter_zeros([], []).
filter_zeros([A-N|As], Bs) :-
    (
	N =:= 0
    ->
	Bs = Bs0
    ;
	Bs = [A-N|Bs0]
    ),
	filter_zeros(As, Bs0).

