% G3 utility to convert G2 postulates into the new notation.

term_expansion(lex(Word, Form), lex(Word, Form, Word)).

term_expansion(min_lex(Word, Features), lex(Word, Form, Word)) :-
	translate_features(a, Features, Form).

term_expansion(postulate(A0, B0, Name), conversion(As, Bs, Cs, Name)) :-
	convert_to_g3a(A0, R, As, []),
	convert_to_g3b(B0, R, [], Bs, [_|Cs], []),
	length(As,LA),
	length(Bs,LB),
       (
	   LB>LA
       ->
	   format('{Warning: RHS of postulate larger than LHS}~n{Warning: possible nontermination!}~n',[])
       ;
	   true
       ).

translate_features(a, Features, Form) :-
	translate_features_a(Features, Form).
translate_features(b, Features, Form) :-
	translate_features_b(Features, Form).
translate_features(c, Features, Form) :-
	translate_features_c(Features, Form).

translate_features_a([F|Fs], Form) :-
	translate_features_a1(F, Fs, Form).

translate_features_a1(cat(F), Fs, Form) :-
	translate_features_c(Fs, lit(F), Form).
translate_features_a1(sel(F), Fs, dr(l,F0,F)) :-
	translate_features_b(Fs, F0).

translate_features_b([F|Fs], Form) :-
	translate_features_b1(F, Fs, Form).

translate_features_b1(cat(F), Fs, Form) :-
	translate_features_c(Fs, lit(F), Form).
translate_features_b1(sel(F), Fs, dl(r,F,F0)) :-
	translate_features_b(Fs, F0).
translate_features_b1(pls(F), [cat(G)|Fs], box(F,F0)) :-
	translate_features_c(Fs, lit(G), F0).

translate_features_c([], Form, Form).
translate_features_c([F|Fs], Form0, Form) :-
	translate_features_c1(F, Fs, Form0, Form).

translate_features_c1(min(F), Fs, Form0, box(F,Form)) :-
	translate_features_c(Fs, Form0, Form).

convert_to_g3a(A, A, As, As) :-
	var(A),
	!.
convert_to_g3a(zip(I,B0), R1, [R1-dia(I,R2)|As0], As) :-
	convert_to_g3a(B0, R2, As0, As).
convert_to_g3a(p(I,A0,B0), R1, [R1-p(I,R2,R3)|As0], As) :-
	convert_to_g3a(A0, R2, As0, As1),
	convert_to_g3a(B0, R3, As1, As).

convert_to_g3b(B, B, Bs, Bs, Cs, Cs) :-
	var(B),
	!.
convert_to_g3b(zip(I,B0), R1, Bs0, Bs, [R1|Cs0], Cs) :-
	convert_to_g3b(B0, R2, [R1-dia(I,R2)|Bs0], Bs, Cs0, Cs).
convert_to_g3b(p(I,A0,B0), R1, Bs0, Bs, [R1|Cs0], Cs) :-
	convert_to_g3b(A0, R2, [R1-p(I,R2,R3)|Bs0], Bs1, Cs0, Cs1),
	convert_to_g3b(B0, R3, Bs1, Bs, Cs1, Cs).
