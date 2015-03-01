% -*- Mode: Prolog -*-
% ============================================================
% pentus.pl
% ============================================================
% grail 3.1.1

% Proof Generator For Grail 3 Based on Pentus' (2003,2006) encoding of
% the satisfyability problem.
% Copyright (C) 2003-2009 Richard Moot (Richard.Moot@labri.fr)

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
% 24 atomic formulas

example_phrase(prove, "A", Ant, Goal) :-
	sat_to_lambek([[1-1]], Ant, Goal).


% ~a
% 1 clause, 1 variable
% 24 atomic formulas

example_phrase(prove, "~A", Ant, Goal) :-
	sat_to_lambek([[1-0]], Ant, Goal).


% a \/ ~b
% 1 clause, 2 variables
% 44 atomic formulas

example_phrase(prove, "A \\/ ~B", Ant, Goal) :-
	sat_to_lambek([[1-1,2-0]], Ant, Goal).

% (~a \/ b) /\ (~b \/ a)
% a = b = 0,1
% 2 clauses, 2 variables
% 66 atomic formulas

example_phrase(prove, "(~A \\/ B) /\\ (~B \\/ A)", Ant, Goal) :-
	sat_to_lambek([[1-0,2-1],[2-0,1-1]], Ant, Goal).

% a /\ ~a
% FAIL
% 2 clauses, 1 variable
% 36 atomic formulas

example_phrase(prove, "A /\\ ~A", Ant, Goal) :-
	sat_to_lambek([[1-1],[1-0]], Ant, Goal).

% a \/ ~a
% a = 0,1
% 1 clause, 1 variable
% 24 atomic formulas


example_phrase(prove, "A \\/ ~A", Ant, Goal) :-
	sat_to_lambek([[1-1,1-0]], Ant, Goal).

% (a \/ ~a) /\ (b \/ ~b)
% a,b = 0,1
% 2 clauses, 2 variables
% 66 atomic formulas

example_phrase(prove, "(A \\/ ~A) /\\ (B \\/ ~B)", Ant, Goal) :-
	sat_to_lambek([[1-1,1-0],[2-1,2-0]], Ant, Goal).	

% (a \/ b) /\ ~b /\ (~a \/ b)
% FAIL
% 3 clauses, 2 variables
% 88 atomic formulas

example_phrase(prove, "(A \\/ B) /\\ ~B /\\ (~A \\/ B)", Ant, Goal) :-
	sat_to_lambek([[1-1,2-1],[2-0],[1-0,2-1]], Ant, Goal).

% = sat_to_lambek(+Clauses, -Antecedent, -GoalFormula)
%
% true if Antecedent |- GoalFormula is the translation proposed by
% Pentus (2003,2006) which decides propositional satisfiability of 
% Clauses, which is a set of clauses in Conjunction Normal Form.

sat_to_lambek(Clauses, Ant, Goal) :-
	variables(Clauses, V),
	length(Clauses, M),
	reverse(Clauses, ClausesR),
	sat_to_lambek1(V, ClausesR, AntR, M),
	reverse(AntR, Ant),
	pentus_g(V, M, Goal),
	findall(A, list_atomic_formula([Goal|Ant],A), List0),
	sort(List0, List),
	length(List0, Len0),
	length(List, Len),
	format('Atomic formulas: ~w (~w)~n', [Len,Len0]),
	max_order([Goal|Ant], Order),
	format('Maximum order: ~w~n', [Order]).	
	


list_atomic_formula(List, A) :-
	member(F, List),
	atomic_formula1(F, A).

sat_to_lambek1(V0, Cs, Fs, M) :-
    (
	V0 = 0
    ->
	Fs = []
    ;
	V is V0-1,
	Fs = [F|Fs0],
	pentus_f(V0, M, Cs, F),
	sat_to_lambek1(V, Cs, Fs0, M)
    ).
	
pentus_f(N, M, Cs, p(a, p(a, dr(a, F2, F1), F1), dl(a, F1, F3))) :-
	pentus_h(N, M, F1),
	pentus_e(N, M, Cs, 0, F2),
	pentus_e(N, M, Cs, 1, F3). 

pentus_e(I0, J0, Cs0, T, F) :-
	I1 is I0-1,
	pentus_atom(I1, J0, F1),
	pentus_atom(I0, J0, F2),	
    (
	J0 = 0
    ->
	F = dl(a, F1, F2)
    ;
	Cs0 = [C|Cs],
	J1 is J0-1,
	pentus_e(I0, J1, Cs, T, F3),
        (
	    subliteral(C, T, I0)
	->
	    F = p(a, dl(a, F1, F3), F2)
	;
	    F = dl(a, F1, p(a, F3, F2))
	)
    ).


pentus_h(I0, J0, F) :-
	I1 is I0-1,
	pentus_atom(I1, J0, F1),
	pentus_atom(I0, J0, F2),
    (
	J0 = 0
    ->
	F = dl(a, F1, F2)
    ;
	J1 is J0-1,
	F = dl(a, F1, p(a, F3, F2)),
	pentus_h(I0, J1, F3)
    ).


pentus_g(I, J0, F) :-
	pentus_atom(0, J0, F1),
	pentus_atom(I, J0, F2),
    (
	J0 = 0
    ->
	F = dl(a, F1, F2)
    ;
	J1 is J0-1,
	F = p(a, dl(a, F1, F3), F2),
	pentus_g(I, J1, F3)
    ).

%
% compute an atomic formula for p_N^M0
%

pentus_atom(N, M, lit(Atom)) :-
	concat_atom([p, N, '_', M], Atom).

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

