% -*- Mode: Prolog -*-
% ==========================================================
% LaTeX output
% ==========================================================

:- module(latex, [
        latex_header/0,
	latex_header/1,
	latex_header/2,
        latex_header_t/1,
	latex_header_t/2,
	latex_header_tab/2,
	latex_tail_tab/1,
	latex_tail/0,
	latex_tail/1,
	latex_tail_t/0,
	latex_tail_t/1,
	latex_section/2,
	latex_sentences/2,
	latex_print_sentence/4,
	latex_lexicon/2,
	latex_structural_rules/2,
	latex_init_ex_counter/1,
	latex_set_ex_counter/2,
	latex_step_ex_counter/1,
        latex_formula/1,          % Formula ->
	latex_semantics/2,        % Semantics x Formula ->
        latex_label/1,            % Label ->
        latex_label/2,            % Label x Stream ->
        latex_label/4,            % Label ->
        latex_label/5,            % Label ->
        latex_formula/2,          % Formula x Stream ->
	latex_semantics/3,        % Semantics x Formula x Stream -> 
        latex_proof/2,            % Proof x Stream ->
	latex_list/2]).           % List x Stream ->

:- use_module(sem_utils, [get_variable_types/3]).
:- use_module(tree234,   [btree_get/3]).
:- use_module(options,   [get_option/2,option_true/1]).

dynamic short_axioms/0.

short_axioms.

% = latex_header.
%
% Output the header of a LaTeX document to standard out. It will
% include the necassary LaTeX packages and define some minor
% commands for use with the main predicates in this package.

latex_header :-
	latex_header(user_output).

% = latex_header(+Stream)
%
% Output the LaTeX header to Stream.

latex_header(Stream) :-
	latex_header(Stream, a2paper).

% = latex_header(+Stream, +PaperSize)
%
% Output the LaTeX header to Stream.

latex_header(Stream, PaperSize) :-
        format(Stream, '\\documentclass[leqno]{article}~2n', []),
	format(Stream, '\\usepackage[~w]{geometry}~n', [PaperSize]),         
	format(Stream, '\\usepackage[utf8]{inputenc}~n', []),         
	format(Stream, '\\usepackage{amssymb}~n', []),         
	format(Stream, '\\usepackage{latexsym}~n', []),         
	format(Stream, '\\usepackage{amsmath}~n', []),         
	format(Stream, '\\usepackage{proof}~n', []),         
	format(Stream, '\\usepackage{moreverb}~n', []),         
	format(Stream, '\\usepackage{array}~n', []),         
	format(Stream, '\\newcommand{\\Boxd}{\\Box^{\\downarrow}}~n', []),
	format(Stream, '\\newcommand{\\bs}{\\backslash}~n', []),         
	format(Stream, '\\newcommand{\\bo}{[}~n', []),         
	format(Stream, '\\newcommand{\\bc}{]}~2n', []),         
        format(Stream, '\\begin{document}~2n', []).

% = latex_tail.
%
% Outputs the tail of the LaTeX document to standard out.

latex_tail :-
	latex_tail(user_output).

% = latex_tail(+Stream).
%
% Outputs the tail of the LaTeX document to Stream.

latex_tail(Stream) :-
	format(Stream, '~n\\end{document}~n', []).

% = latex_header_t(+Tab)
%
% Output the LaTeX header to standard out and opens a tabular environment
% with columns specified by Tab.

latex_header_t(T) :-
	latex_header_t(T, user_output).

% = latex_header_t(+Tab, +Stream)
%
% Output the LaTeX header to Stream and opens a tabular environment
% with columns specified by Tab.

latex_header_t(T, Stream) :-
	latex_header(Stream),
	latex_header_tab(T, Stream).

% = latex_header_tab(+Tab, +Stream)
%
% open a tabular environment at Stream with tabular specification Tab.

latex_header_tab(T, Stream) :-
	format(Stream, '\\begin{tabular}{~w}~n', [T]).

% = latex_tail_t.
%
% Outputs the tail of the LaTeX document to standard out while closing
% a tabular environment.

latex_tail_t :-
	latex_tail(user_output).

% = latex_tail_t(+Stream).
%
% Outputs the tail of the LaTeX document to Stream while closing a
% tabular environment.

latex_tail_t(Stream) :-
	latex_tail_tab(Stream),
	latex_tail(Stream).

% = latex_header_tab(+Tab, +Stream)
%
% closes a tabular environment at Stream with tabular specification Tab.

latex_tail_tab(Stream) :-
	format(Stream, '\\end{tabular}~2n', []).

% = example counters

latex_init_ex_counter(Stream) :-
	format(Stream, '\\newcounter{ex}~n', []),         
	format(Stream, '\\renewcommand\\theequation{\\arabic{ex}\\alph{equation}}~n', []).

latex_set_ex_counter(Stream, N) :-
	format(Stream, '\\setcounter{ex}{~w}~n', [N]).

latex_step_ex_counter(Stream) :-
	format(Stream, '~n\\stepcounter{ex}~n', []),
	format(Stream, '\\setcounter{equation}{0}~2n', []).

% =

latex_section(Section, Stream) :-
	format(Stream, '~2n\\section{~w}~2n', [Section]).

% =

latex_structural_rules([], _).
latex_structural_rules([conversion(X,Y,Z)|Rest], Stream) :-
         write(Stream, '\\ensuremath{'),
         latex_label(X, 1, [], Stream),
         write(Stream, '} & \\ensuremath{'),
         latex_label(Y, 1, [], Stream),
         format(Stream, '} & \\ensuremath{[~p]} \\\\~n', [Z]),
         latex_structural_rules(Rest, Stream).

% =

latex_lexicon([], _).
latex_lexicon([lex(A,B0,C)|Rest], Stream) :-
         write(Stream, '\\noindent \\ensuremath{'),
         latex_label(x-A, 1, [], Stream),
         write(Stream, ' : '),
         macro_expand(B0, B),
         latex_formula(B, Stream),
         write(Stream, ' - '),
         latex_semantics(C, B, Stream),
         write(Stream, '} \\\\'),
	 nl(Stream),
         latex_lexicon(Rest, Stream).

% =

latex_sentences([], _).
latex_sentences([example(Cat,String0,Goal)|Rest], Stream) :-
        example_deriv_status(String0, String, Dash),
	latex_print_sentence(Cat, String, Dash, Goal, Stream),
        latex_sentences(Rest, Stream).

latex_print_sentence(parse, String, Dash, Goal, Stream) :-
	latex_print_sentence(String, Dash, Goal, Stream).
latex_print_sentence(prove, String, Dash, Goal, Stream) :-
	latex_print_sentence(String, Dash, Goal, Stream).
latex_print_sentence(comment, String, _, _, Stream) :-
        format(Stream, '\\\\~n\\quad\\textbf{~s} \\\\~n\\\\~n', [String]).
 
latex_print_sentence(String0, Dash, Goal, Stream) :-
        example_trim_underscores(String0, String),
        format(Stream, ' \\emph{~s} $~w\\ ', [String,Dash]),
        latex_formula(Goal, Stream),
        format(Stream, '$ \\\\ ~n', []).


example_trim_underscores([], []).
example_trim_underscores([X|Xs], [Y|Ys]) :-
    ( 
	X = 95 
    ->
        Y = 32
    ;
        Y = X
    ),
        example_trim_underscores(Xs, Ys).

example_deriv_status([42|R], R, ' \\nvdash ') :-
	!.
example_deriv_status([63|R], R, ' \\vdash_{?} ') :-
	!.
example_deriv_status([32|R], R, ' \\vdash ') :-
	!.
example_deriv_status(R, R, ' \\vdash ').

% =

latex_label(Label) :-
	latex_label(Label, 1, [], user).

latex_label(Label, Stream) :-
	latex_label(Label, 1, [], Stream),
	format(Stream,'\\rule[-.2ex]{0pt}{.9em}', []).

latex_label(L, P1, P2, Con, Stream) :-
        print(P1),
        latex_label(L, 1, Con, Stream),
        print(P2).

% =

latex_label(A, _N, _Con, Stream) :-
	atomic(A),
	!,
        write(Stream, '\\textrm{'),
	write_atom(A, Stream),
        write(Stream, '}').

latex_label(p(I,_L,_R,A,B), N, Con, Stream) :-
        !,
        write_bo(N, Stream),
        binding(A,p(I,A,B),NA),
        latex_label(A, NA, Con, Stream),
        write(Stream, '\\circ_{'),
	write_mode(I, Stream),
	write(Stream, '}'),
        binding(B,p(I,A,B),NB),
        latex_label(B, NB, Con, Stream),
        write_bc(N, Stream).


latex_label(p(I,A,B), N, Con, Stream) :-
        !,
        write_bo(N, Stream),
        binding(A,p(I,A,B),NA),
        latex_label(A, NA, Con, Stream),
        write(Stream, '\\circ_{'),
	write_mode(I, Stream),
	write(Stream, '}'),
        binding(B,p(I,A,B),NB),
        latex_label(B, NB, Con, Stream),
        write_bc(N, Stream).

latex_label(dia(I,A), _, Con, Stream) :-
        !,
        write(Stream, '\\langle '),
        latex_label(A, 1, Con, Stream),
        write(Stream, '\\rangle^{'),
        write_mode(I, Stream),
        write(Stream, '}').

latex_label(leaf(_L,_R,Word,_Pos,_Lemma), _, _, Stream) :-
        write(Stream, '\\textrm{'),
    (
	atom(Word)
    ->
        write_atom(Word, Stream)
    ;
        print(Stream, Word)
    ),
        write(Stream, '}').	

latex_label(hyp(_,_,'$VAR'(N)), _, _, Stream) :-
        !,
        write_pros_var(N, Stream).

latex_label('$VAR'(N), _, _, Stream) :-
        !,
        write_pros_var(N, Stream).

latex_label(_-'$VAR'(N), _, _, Stream) :-
        !,
        write_pros_var(N, Stream).

latex_label(_-A, _, _, Stream) :-
        write(Stream, '\\textrm{'),
    (
	atom(A)
    ->
        write_atom(A, Stream)
    ;
        print(Stream, A)
    ),
        write(Stream, '}').

write_pros_var(N, Stream) :-
        V is N mod 4,
        I is N // 4,
        pros_var_name(V,Name),
        format(Stream, '\\textrm{~p}_{~p}',[Name,I]).

pros_var_name(0,p).
pros_var_name(1,q).
pros_var_name(2,r).
pros_var_name(3,s).

% ===========================================================

latex_formula(T, Stream) :-
	latex_formula(T, 1, Stream),
	format(Stream,'\\rule[-.2ex]{0pt}{.9em}', []).

latex_formula(T) :-
        latex_formula(T, 1, user).

% = latex_formula0(+Type)
% write a type with outer brackets

latex_formula(lit(np(_,_,_)), _, Stream) :- 
	!,
	write_atom(np, Stream).
latex_formula(lit(pp(P)), _, Stream) :- 
	!,
    (
        var(P)
    ->
	write_atom(pp, Stream)
    ;
        P = '$VAR'(_)
    ->
	write_atom(pp, Stream)
    ;
	format(Stream, 'pp_{\\textit{~w}}', [P])
    ).
latex_formula(lit(s(S)), _, Stream) :- 
	!,
    (
        var(S)
    ->
	write_atom(s, Stream)
    ;
        S = '$VAR'(_)
    ->
	write_atom(s, Stream)
    ;
        S = inf(I)
    ->
        write_inf(I, Stream)
    ;
	format(Stream, 's_{\\textit{~w}}', [S])
    ).
latex_formula(lit(cl_r), _, Stream) :-
	!,
	format(Stream, 'cl_{\\textit{r}}', []).
latex_formula(lit(cl_y), _, Stream) :-
	!,
	format(Stream, 'cl_{\\textit{y}}', []).
latex_formula(lit(X), N, Stream) :- 
	!,
	latex_formula(X, N, Stream).

latex_formula(dl(I,A,B), N, Stream) :- 
        !, 
        write_bo(N, Stream),
        binding(A, dl(I,A,B), NA),
        latex_formula(A, NA, Stream),
        write(Stream, ' \\bs_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        binding(B, dl(I,A,B), NB),
        latex_formula(B, NB, Stream),
        write_bc(N, Stream).

latex_formula(dr(I,A,B),N, Stream) :- 
        !, 
        write_bo(N, Stream),
        binding(A, dr(I,A,B), NA),
        latex_formula(A, NA, Stream),
        write(Stream, ' /_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        binding(B, dr(I,A,B), NB),
        latex_formula(B, NB, Stream),
        write_bc(N, Stream).

latex_formula(p(I,A,B), N, Stream) :-
        !,
        write_bo(N, Stream),
        binding(A, p(I,A,B), NA),
        latex_formula(A, NA, Stream),
        write(Stream, ' \\bullet_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        binding(B, p(I,A,B), NB),
        latex_formula(B, NB, Stream),
        write_bc(N, Stream).

latex_formula(dia(I,A), _, Stream) :-
        !,
        write(Stream, '\\Diamond_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        binding(A, dia(I,A), NA),
        latex_formula(A, NA, Stream).

latex_formula(box(I,A), _, Stream) :-
        !,
        write(Stream, '\\Boxd_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        binding(A, box(I,A), NA),
        latex_formula(A, NA, Stream).

/* extra rules for non-atomic macro definitions */

latex_formula(bang(I,A), _, Stream) :-
        !,
        write(Stream, '!_{'),
        write_mode(I, Stream),
        write(Stream, '}'),
        latex_formula(A, 0).

latex_formula(q(A,B,C), _, Stream) :-
        !,
        write(Stream, 'q('),
        latex_formula(A, 1, Stream),
        write(Stream, ','),
        latex_formula(B, 1, Stream),
        write(Stream, ','),
        latex_formula(C, 1, Stream),
        write(Stream, ')').

latex_formula(X,_, Stream) :-
        /* X is an atomic type */ 
    (
	atomic(X) 
    ->
        write_atom(X, Stream)
    ;
        print(Stream, X)
    ).

% write_mode(+Mode, +Stream)

write_mode(I, _) :-
	invisible_mode(I),
	!.
write_mode(one(M), Stream):-
	!,
	write(Stream, M),
	write(Stream, '_{1}').
write_mode(two(M), Stream):-
	!,
	write(Stream, M),
	write(Stream, '_{2}').
write_mode(Atom, Stream):-
	write(Stream, Atom).

write_inf(Inf, Stream) :- 
	!,
    (
        var(Inf)
    ->
	format(Stream, 's_{\\textit{inf}}', [])
    ;
        Inf = '$VAR'(_)
    ->
	format(Stream, 's_{\\textit{inf}}', [])     
    ;
        Inf = base
    ->
	format(Stream, 's_{\\textit{inf}}', [])
    ;
        format(Stream, 's_{\\textit{~winf}}', [Inf])
    ).

% ===========================================================

latex_semantics(Sem, Formula) :-
	get_variable_types(Sem, Formula, Tr),
	latex_semantics(Sem, 1, Tr, user).
latex_semantics(Sem, Formula, Stream) :-
	get_variable_types(Sem, Formula, Tr),
        latex_semantics(Sem, 1, Tr, Stream).

% = Montegovian dynamics

latex_semantics(sel(X,Y), _, Tr, Stream) :-
	!,
	format(Stream, '~@_{~@}(~@)', [latex_sem_constant(sel, Stream),
				       latex_sem_constant(X, Stream),
				       latex_semantics(Y, 1, Tr, Stream)]).
latex_semantics(erase_context(X), _, Tr, Stream) :-
	!,
	format(Stream, '~@(~@)', [latex_sem_constant(erase_context, Stream),
				  latex_semantics(X, 1, Tr, Stream)]).
latex_semantics(update(X,Y), N, Tr, Stream) :-
	!,
        write_bo(N, Stream),
        binding(X, update(X,Y), NX),
        binding(Y, update(X,Y), NY),
	format(Stream, '~@ :: ~@', [latex_semantics(X, NX, Tr, Stream),
				    latex_semantics(Y, NY, Tr, Stream)]),
        write_bc(N, Stream).
latex_semantics(ref(Ind,X,Y), _, Tr, Stream) :-
	!,
	format(Stream, '~@.{~@}^{~@}', [write_atom_italics(Ind, Stream),
					latex_semantics(X, 1, Tr, Stream),
					latex_write_set(Y, Tr, Stream)]).

% = DRT

latex_semantics(drs_label(L,T), _, Tr, Stream) :-
        !,
	format(Stream, '~@ : ~@', [latex_semantics(L, 1, Tr, Stream),
				   latex_semantics(T, 1, Tr, Stream)]).
latex_semantics(drs(V,C), _, Tr, Stream) :-
        !,
        format(Stream, '\\raisebox{2.4ex}{\\,\\setlength{\\tabcolsep}{8pt}\\begin{tabular}[t]{|l|} \\firsthline~n', []),
        write_list_of_referents(V, Tr, Stream),
        format(Stream, ' \\\\ \\hline~n', []),
        write_list_of_conds(C, Tr, Stream),
        format(Stream, ' \\end{tabular}\\,}', []).

latex_semantics(merge(A,B), N, Tr, Stream) :-
        !,
        write_bo(N, Stream),
        binding(A, merge(A,B), NA),
        latex_semantics(A, NA, Tr, Stream),
        write(Stream, ' \\oplus '),
        binding(B, merge(A,B), NB),
        latex_semantics(B, NB, Tr, Stream),
        write_bc(N, Stream).

latex_semantics(presup(A,B), _N, Tr, Stream) :-
        !,
        write(Stream, '\\left \\langle'),
	flatten_presups(presup(A,B), C, PresupList, []),
        latex_semantics_presup_list(PresupList, Tr, Stream),
        write(Stream, ' , '),
        latex_semantics(C, 1, Tr, Stream),
        write(Stream, '\\right \\rangle').
latex_semantics(presupp(A,B), _N, Tr, Stream) :-
        !,
        write(Stream, '\\left \\langle'),
	flatten_presups(presupp(A,B), C, PresupList, []),
        latex_semantics_presup_list(PresupList, Tr, Stream),
        write(Stream, ' , '),
        latex_semantics(C, 1, Tr, Stream),
        write(Stream, '\\right \\rangle').

% = DPL existential quantifier

latex_semantics(exists(X), _, Tr, Stream) :-
        !,
        write(Stream, '\\exists '),
        latex_semantics(X, 1, Tr, Stream).

% = basic operators

latex_semantics(bool(A,is_at(E),B), N, Tr, Stream) :-
	!,
        write_bo(N, Stream),
        binding(A, bool(A,=,B), NA),
        latex_semantics(A, NA, Tr, Stream),
	write(Stream, ' =_{'),
	latex_semantics(E, 1, Tr, Stream),
	write(Stream, ' }'),
        binding(B, bool(A,=,B), NB),
        latex_semantics(B, NB, Tr, Stream),
        write_bc(N, Stream).	

latex_semantics(bool(A,empty_intersect,B), N, Tr, Stream) :-
	!,
        write_bo(N, Stream),
        write_bo(N, Stream),
        binding(A, bool(A,intersection,B), NA),
        latex_semantics(A, NA, Tr, Stream),
        write_conn(intersection, Stream),
        binding(B, bool(A,intersection,B), NB),
        latex_semantics(B, NB, Tr, Stream),
        write_bc(N, Stream),
	write(Stream, '= \\emptyset'),
        write_bc(N, Stream).	
	
latex_semantics(bool(A,C,B), N, Tr, Stream) :-
        !,
        write_bo(N, Stream),
        binding(A, bool(A,C,B), NA),
        latex_semantics(A, NA, Tr, Stream),
        write_conn(C, Stream),
        binding(B, bool(A,C,B), NB),
        latex_semantics(B, NB, Tr, Stream),
        write_bc(N, Stream).

latex_semantics(true, _, _, Stream) :-
	!,
	write(Stream, '\\top ').
latex_semantics(false, _, _, Stream) :-
	!,
	write(Stream, '\\bot ').
latex_semantics(nil, _, _, Stream) :-
	!,
	write(Stream, '\\emptyset ').
latex_semantics(not(X), _, Tr, Stream) :-
        !,
        write(Stream, ' \\neg '),
        binding(X, not(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(quant(Q,X,T), N, Tr, Stream) :-
        !,
        write_bo(N, Stream),
        write_quant(Q, Stream),
        latex_semantics(X, 1, Tr, Stream),
	get_option(collapse_lambda, CL),
        latex_semantics_quant(CL, T, Q, N, Tr, Stream).      

latex_semantics(quant(Q,T), N, Tr, Stream) :-
        !,
        write_bo(N, Stream),
	binding(T, quant(Q,T), NT),
        write_quant(Q, Stream),
        latex_semantics(T, NT, Tr, Stream),
	write_bc(N, Stream).      

latex_semantics(lambda(X,V), N, Tr, Stream) :-
        !,
        write_bo(N, Stream),
        write(Stream, '\\lambda '),
        latex_semantics(X, 1, Tr, Stream),
	get_option(collapse_lambda, CL),
        latex_semantics_lambda(CL, V, N, Tr, Stream).

latex_semantics(appl(F,X), _, Tr, Stream) :-
    (
        option_true(fxy)
    ->
        write_fun_args(F,[X], Tr, Stream),
        !
    ).

latex_semantics(appl(X,Y), _, Tr, Stream) :-
        !,
        write(Stream, '('),
        latex_semantics(X, 1, Tr, Stream),
        write(Stream, ' \\  '),
        latex_semantics(Y, 1, Tr, Stream),
        write(Stream, ')').
latex_semantics(Role:Term, _, Tr, Stream) :-
        !,
        write(Stream, '\\textit{'),
        write_atom(Role, Stream),
        write(Stream, '}:'),
        latex_semantics(Term, 1, Tr, Stream).

latex_semantics(pair(X,Y), _, Tr, Stream) :-
        !,
        write(Stream, ' \\langle '),
        latex_semantics(X, 1, Tr, Stream),
        write(Stream, ' , '),
        latex_semantics(Y, 1, Tr, Stream),
        write(Stream, ' \\rangle ').

latex_semantics(pi1(X), _, Tr, Stream) :-
        !,
        write(Stream, ' \\pi^1'),
        binding(X, pi1(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(pi2(X), _, Tr, Stream) :-
        !,
        write(Stream, ' \\pi^2'),
        binding(X, pi2(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(debox(X), _, Tr, Stream) :-
        !,
        write(Stream, ' {}^{\\vee} '),
        binding(X, debox(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(dedia(X), _, Tr, Stream) :-
        !,
        write(Stream, ' {}^{\\cup} '),
        binding(X, dedia(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(conbox(X), _, Tr, Stream) :-
        !,
        write(Stream, ' {}^{\\wedge} '),
        binding(X, conbox(X), NX),
        latex_semantics(X, NX, Tr, Stream).

latex_semantics(condia(X), _, Tr, Stream) :-
        !,
        write(Stream, ' {}^{\\cap} '),
        binding(X, condia(X), NX),
        latex_semantics(X, NX, Tr, Stream).
latex_semantics(smash(X), _, Tr, Stream) :-
        !,
        write(Stream, ' \\lfloor '),
        latex_semantics(X, 1, Tr, Stream),
	write(Stream, ' \\rfloor ').


latex_semantics(num(X), _, Tr, Stream) :-
        !,
        write(Stream, ' | '),
        latex_semantics(X, 1, Tr, Stream),
        write(Stream, ' | ').
latex_semantics(count(X), _, Tr, Stream) :-
        !,
        write(Stream, ' | '),
        latex_semantics(X, 1, Tr, Stream),
        write(Stream, ' | ').
latex_semantics(sub(X,Y), _, Tr, Stream) :-
	!,
	write(Stream, '{'),
	latex_semantics(X, 1, Tr, Stream),
	write(Stream, '}_{'),
	latex_semantics(Y, 1, Tr, Stream),
	write(Stream, '}').
latex_semantics(sup(X,Y), _, Tr, Stream) :-
	!,
	write(Stream, '{'),
	latex_semantics(X, 1, Tr, Stream),
	write(Stream, '}^{'),
	latex_semantics(Y, 1, Tr, Stream),
	write(Stream, '}').
latex_semantics(complement(X), _, Tr, Stream) :-
	!,
	write(Stream, '{'),
	latex_semantics(X, 1, Tr, Stream),
	write(Stream, '}^{\\complement}').

latex_semantics('$VAR'(N), _, Tr, Stream) :-
        !,
        V is N mod 3,
        I is N // 3,
    (
        btree_get(Tr, N, Type)
    ->
        sem_var_name(Type, V, Name)
    ;
        sem_var_name(V, Name)
    ),
        format(Stream, '~p_{~p}',[Name,I]).
latex_semantics(event('$VAR'(N)), _, _, Stream) :-
	!,
        V is N mod 3,
        I is N // 3,
        sem_var_name(s, V, Name),
        format(Stream, '~p_{~p}',[Name,I]).	

latex_semantics(variable('$VAR'(N)), _, _, Stream) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(e, V, Name),
        format(Stream, '~p_{~p}',[Name,I]).
latex_semantics(set_variable('$VAR'(N)), _, _, Stream) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(e->t, V, Name),
        format(Stream, '~p_{~p}',[Name,I]).

latex_semantics('$VAR'(N), _, _, Stream) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(e, V, Name),
        format(Stream, '~p_{~p}',[Name,I]).

latex_semantics(constant(Const), _, _, Stream) :-
	!,
	/* explicit constant */
	latex_sem_constant(Const, Stream).

latex_semantics(Const, _, _, Stream) :-
	/* treat all unknowns as constants */
	latex_sem_constant(Const, Stream).

latex_sem_constant(Const, Stream) :-
        write(Stream, '\\textbf{'),
    (
	atomic(Const) 
    ->
        write_atom(Const, Stream)
    ;
        print(Stream, Const)
    ),
        write(Stream, '}').



sem_var_name(0, x).
sem_var_name(1, y).
sem_var_name(2, z).

sem_var_name(e, 0, x) :-
	!.
sem_var_name(e, 1, y) :-
	!.
sem_var_name(e, 2, z) :-
	!.
sem_var_name(s, 0, x) :-
	!.
sem_var_name(s, 1, y) :-
	!.
sem_var_name(s, 2, z) :-
	!.
%sem_var_name(s, 0, d) :-
%	!.
%sem_var_name(s, 1, e) :-
%	!.
%sem_var_name(s, 2, f) :-
%	!.
sem_var_name(e->t, 0, 'P') :-
	!.
sem_var_name(e->t, 1, 'Q') :-
	!.
sem_var_name(e->t, 2, 'R') :-
	!.
sem_var_name(e->s->s->t, 0, 'P') :-
	!.
sem_var_name(e->s->s->t, 1, 'Q') :-
	!.
sem_var_name(e->s->s->t, 2, 'R') :-
	!.
sem_var_name((e->t)->t, 0, 'L') :-
	!.
sem_var_name((e->t)->t, 1, 'M') :-
	!.
sem_var_name((e->t)->t, 2, 'N') :-
	!.
sem_var_name((e->s->s->t)->s->s->t, 0, 'L') :-
	!.
sem_var_name((e->s->s->t)->s->s->t, 1, 'M') :-
	!.
sem_var_name((e->s->s->t)->s->s->t, 2, 'N') :-
	!.
sem_var_name(s->t, 0, '\\alpha') :-
	!.
sem_var_name(s->t, 1, '\\beta') :-
	!.
sem_var_name(s->t, 2, '\\gamma') :-
	!.
sem_var_name(_, 0, 'X') :-
	!.
sem_var_name(_, 1, 'Y') :-
	!.
sem_var_name(_, 2, 'Z') :-
	!.

latex_semantics_lambda(0, V, N, Tr, Stream) :-
        write(Stream, '.'),
        binding(V, lambda(_,V), NV),
        latex_semantics(V, NV, Tr, Stream),
	write_bc(N, Stream).
latex_semantics_lambda(1, V, N, Tr, Stream) :-
        latex_semantics_lambda1(V, N, Tr, Stream).

latex_semantics_lambda1(lambda(X,V), N, Tr, Stream) :-
	!,
        latex_semantics(X, 1, Tr, Stream),
	latex_semantics_lambda1(V, N, Tr, Stream).
latex_semantics_lambda1(V, N, Tr, Stream) :-
        write(Stream, '.'),
        binding(V, lambda(_,V), NV),
	latex_semantics(V, NV, Tr, Stream),
	write_bc(N, Stream).

latex_semantics_quant(0, T, Q, N, Tr, Stream) :-
        !,
        binding(T, quant(Q,_,T), NT),
        latex_semantics(T, NT, Tr, Stream),
        write_bc(N, Stream).      
latex_semantics_quant(1, T, Q, N, Tr, Stream) :-
	latex_semantics_quant1(T, Q, N, Tr, Stream).

latex_semantics_quant1(quant(Q,X,T), Q, N, Tr, Stream) :-
	!,
        latex_semantics(X, 1, Tr, Stream),
	latex_semantics_quant1(T, Q, N, Tr, Stream).
latex_semantics_quant1(V, Q, N, Tr, Stream) :-
	write(Stream, '.'),
	binding(V, quant(Q,_,V), NV),
	latex_semantics(V, NV, Tr, Stream),
	write_bc(N, Stream).

flatten_presups(presup(A,B),C) -->
	!,
	flatten_presups(A),
	flatten_presups(B,C).
flatten_presups(presupp(A,B),C) -->
	!,
	flatten_presups(A),
	flatten_presups(B,C).
flatten_presups(C, C) -->
	[].

flatten_presups(presup(A,B)) -->
	!,
	flatten_presups(A),
	flatten_presups(B).
flatten_presups(presupp(A,B)) -->
	!,
	flatten_presups(A),
	flatten_presups(B).
flatten_presups(A) -->
	[A].

latex_semantics_presup_list([A|As], Tr, Stream) :-
        write(Stream, '\\left \\{'),
	latex_semantics_presup_list1(As, A, Tr, Stream).

latex_semantics_presup_list1([], A, Tr, Stream) :-
	latex_semantics(A, 1, Tr, Stream),
        write(Stream, '\\right \\}').	
latex_semantics_presup_list1([A0|As], A, Tr, Stream) :-
	latex_semantics(A, 1, Tr, Stream),
	write(Stream, ' , '),
	latex_semantics_presup_list1(As, A0, Tr, Stream).
	
latex_semantics_presup(presup(A,B), _, Tr, Stream) :-
        !,
        write(Stream, '\\left \\{'),
	latex_semantics_presup1(presup(A,B), Tr, Stream),
        write(Stream, '\\right \\}').
latex_semantics_presup(presupp(A,B), _, Tr, Stream) :-
        !,
        write(Stream, '\\left \\{'),
	latex_semantics_presup1(presup(A,B), Tr, Stream),
        write(Stream, '\\right \\}').
latex_semantics_presup(A, N, Tr, Stream) :-
	latex_semantics(A, N, Tr, Stream).

latex_semantics_presup1(presup(A,B), Tr, Stream) :-
        !,
        latex_semantics_presup1(A, Tr, Stream),
        write(Stream, ' , '),
        latex_semantics_presup1(B, Tr, Stream).
latex_semantics_presup1(presupp(A,B), Tr, Stream) :-
        !,
        latex_semantics_presup1(A, Tr, Stream),
        write(Stream, ' , '),
        latex_semantics_presup1(B, Tr, Stream).
latex_semantics_presup1(A, Tr, Stream) :-
	latex_semantics(A, 1, Tr, Stream).

% =

write_fun_args(appl(X,Y), As, Tr, Stream) :-
        write_fun_args(X, [Y|As], Tr, Stream).

write_fun_args(Fun, As, Tr, Stream) :- 
        atomic_sem(Fun),
        !,
        latex_semantics(Fun, 1, Tr, Stream),
        write(Stream, '('),
        reverse(As, [B|Bs]),
        write_args(Bs, B, Tr, Stream),
        write(Stream, ')').

write_args([], A, Tr, Stream) :-
        latex_semantics(A, 1, Tr, Stream).
write_args([A|As], A0, Tr, Stream) :-
        latex_semantics(A0, 1, Tr, Stream),
        write(Stream, ' , '),
        write_args(As, A, Tr, Stream).

% =

write_list_of_referents(Refs0, Tr, Stream) :-
	sort(Refs0, Refs),
	write_list_of_referents1(Refs, Tr, Stream).

write_list_of_referents1([], _, Stream) :- 
        write(Stream, '$ \\ $').
write_list_of_referents1([V|Vs], Tr, Stream) :-
        write_list_of_referents1(Vs, V, Tr, Stream).

write_list_of_referents1([], V, Tr, Stream) :-
        write(Stream, '$'),
        latex_semantics(V, 1, Tr, Stream),
        write(Stream, '$').
write_list_of_referents1([V|Vs], V0, Tr, Stream) :-
        write(Stream, '$'),
        latex_semantics(V0, 1, Tr, Stream),
        write(Stream, '\\ $'),
        write_list_of_referents1(Vs, V, Tr, Stream).

% =

latex_write_set([], _Tr, Stream) :-
	write(Stream, '\\emptyset').
latex_write_set([X|Xs], Tr, Stream) :-
	write(Stream, '\\{ '),
	latex_write_set1(Xs, X, Tr, Stream).

latex_write_set1([], X, Tr, Stream) :-
	latex_semantics(X, 1, Tr, Stream),
	write(Stream, ' \\}').
latex_write_set1([X|Xs], X0, Tr, Stream) :-
	latex_semantics(X0, 1, Tr, Stream),
	write(Stream, ','),
	latex_write_set1(Xs, X, Tr, Stream).



% =

write_list_of_conds([], _, Stream) :-
        write(Stream, '$ \\ $ \\\\ \\lasthline ').
write_list_of_conds([C|Cs], Tr, Stream) :-
        write_list_of_conds(Cs, C, Tr, Stream).

write_list_of_conds([], C, Tr, Stream) :-
        write(Stream, '$'),
        latex_semantics(C, 1, Tr, Stream),
	/* add some extra space after an embedded DRS as final element */
    (
        C = drs(_,_)
    ->
        DY = '10pt'
    ;
        C = bool(_,->,_)
    ->
        DY = '10pt'
    ;
        C = drs_label(_,_)
    ->
        DY = '10pt'
    ;
        DY = '0pt'
    ),
        format(Stream, '$ \\\\[~w] \\lasthline ',[DY]).

write_list_of_conds([C|Cs], C0, Tr, Stream) :-
	write(Stream, '$'),
        latex_semantics(C0, 1, Tr, Stream),
        format(Stream, '$ \\\\[0pt]~n ',[]),
        write_list_of_conds(Cs, C, Tr, Stream).

atomic_sem(At) :-
	atomic(At),
	!.
%atomic_sem('$VAR'(_)).

write_bo(0, Stream) :- 
	write(Stream, '(').
write_bo(1, _).

write_bc(0, Stream) :- 
	write(Stream, ')').
write_bc(1, _).

binding(T0, T, N) :-
    (
	option_true(expl_brackets) 
    ->
	N=0
    ;
	binds(T0,_,_,Num0),
	binds(T,Ass,Eq,Num),
	bind1(Eq,T0,Ass,Num0,Num,N)
    ),
	!.

binding(_,_,0).

bind1( _,T,T,M ,M,1) :- 
	!.
bind1(=<,_,_,M0,M,N) :- 
    (
	M > M0
    ->
	N = 0
    ;
	N = 1
    ).
bind1( <,_,_,M0,M,N) :- 
    (
	M >= M0
    ->
	N = 0
    ;
	N = 1
    ).

binds(dia(_,_),n,=<,20).
binds(box(_,_),n,=<,20).
binds(zip(_,_),n,=<,20).

binds(p(_,_,_),n,<,8).
binds(dl(_,_,_),n,<,4).
binds(dr(_,_,_),n,<,4).

binds(not(_),n,=<,20).
binds(dedia(_),n,=<,20).
binds(condia(_),n,=<,20).
binds(debox(_),n,=<,20).
binds(conbox(_),n,=<,20).
binds(quant(_,_,_),n,=<,12).
binds(quant(_,_),n,<,20).
binds(lambda(_,_),n,=<,12).
binds(bool(_,&,_),bool(_,&,_),<,8).
binds(bool(_,\/,_),bool(_,\/,_),<,8).
binds(bool(_,->,_),n,<,4).
binds(bool(_,_,_),n,<,4).
binds(merge(_,_),merge(_,_),<,2).
binds(update(_,_),update(_,_),=<,2).

% =

write_quant(exists, Stream) :-
     !,
     write(Stream, ' \\exists ').
write_quant(forall, Stream) :-
     !,
     write(Stream, ' \\forall ').
write_quant(iota, Stream) :-
     !,
     write(Stream, ' \\iota ').
write_quant(X, Stream) :-
     write(Stream, X).

% = binary connectives

% logical connectives

write_conn(and, Stream) :-
     !,
     write(Stream, ' \\wedge ').
write_conn(&, Stream) :-
     !,
     write(Stream, ' \\wedge ').
write_conn(or, Stream) :-
     !,
     write(Stream, ' \\vee ').
write_conn(\/, Stream) :-
     !,
     write(Stream, ' \\vee ').
write_conn(->, Stream) :-
     !,
     write(Stream, ' \\rightarrow ').

% inequalities and comparisons

write_conn(neq, Stream) :-
     !,
     write(Stream, ' \\neq ').
write_conn(approx, Stream) :-
     !,
     write(Stream, ' \\approxeq ').
write_conn(leq, Stream) :-
     !,
     write(Stream, ' \\leq ').
write_conn(geq, Stream) :-
     !,
     write(Stream, ' \\geq ').
write_conn(nleq, Stream) :-
     !,
     write(Stream, ' \\nleq ').
write_conn(lneq, Stream) :-
     !,
     write(Stream, ' \\lneq ').
write_conn(gneq, Stream) :-
     !,
     write(Stream, ' \\gneq ').
write_conn(ngeq, Stream) :-
     !,
     write(Stream, ' \\ngeq ').
write_conn(prec, Stream) :-
     !,
     write(Stream, ' \\prec ').
write_conn(succ, Stream) :-
     !,
     write(Stream, ' \\succ ').
write_conn(preceq, Stream) :-
     !,
     write(Stream, ' \\preceq ').
write_conn(succeq, Stream) :-
     !,
     write(Stream, ' \\succeq ').
write_conn(simeq, Stream) :-
     !,
     write(Stream, ' \\simeq ').
write_conn(eq, Stream) :-
     !,
     write(Stream, ' = ').


% 

% DRS

write_conn(overlaps, Stream) :-
     !,
     write(Stream, ' \\circ ').
write_conn(abuts, Stream) :-
     !,
     write(Stream, ' \\mbox{\\ensuremath{\\supset\\!\\subset}} ').
%     write(Stream, ' \\bowtie ').

% sets

write_conn(in, Stream) :-
     !,
     write(Stream, ' \\in ').
write_conn(not_in, Stream) :-
     !,
     write(Stream, ' \\not \\in ').
write_conn(atomic_sub, Stream) :-
     !,
     write(Stream, ' \\subseteq_{a} ').
write_conn(subseteq, Stream) :-
     !,
     write(Stream, ' \\subseteq ').
write_conn(subset, Stream) :-
     !,
     write(Stream, ' \\subset ').
write_conn(nsubseteq, Stream) :-
     !,
     write(Stream, ' \\nsubseteq ').
write_conn(subsetneq, Stream) :-
     !,
     write(Stream, ' \\subsetneq ').
write_conn(union, Stream) :-
     !,
     write(Stream, ' \\cup ').
write_conn(intersection, Stream) :-
     !,
     write(Stream, ' \\cap ').
write_conn(intersect, Stream) :-
     !,
     write(Stream, ' \\cap ').
write_conn(setminus, Stream) :-
     !,
     write(Stream, ' \\setminus ').

write_conn(X, Stream) :-
     write(Stream, X).

% =

write_atom_italics(At0, Stream) :-
	latex_quote_underscores(At0, At),
	format(Stream, '\\textit{~p}', [At]).

write_atom(At0, Stream) :-
	latex_quote_underscores(At0, At),
    (
        number(At)
    ->
        format(Stream, '$~p$', [At])
    ;
        /* for some reason format/2 (but not write/2) puts a backslash before double quotes, which LaTeX doesn't like */
        /* this solution is a bit of a hack, and should preferable be replaced by something more elegant */
        At = '"'
    ->
        write(Stream, At)
    ;
	print(Stream, At)
    ).

latex_quote_underscores(Word0, Word) :-
	name(Word0, List0),
	latex_quote_underscores1(List0, List),
	name(Word, List).

latex_quote_underscores1([], []).
latex_quote_underscores1([194,178|As], Bs) :-
	/* UTF-8 encoding of squared symbol */
	!,
	latex_quote_underscores1([178|As], Bs).
latex_quote_underscores1([194,179|As], Bs) :-
	/* UTF-8 encoding of cubed symbol */
	!,
	latex_quote_underscores1([179|As], Bs).
latex_quote_underscores1([92,34|As], [34|Bs]) :-
	!,
	latex_quote_underscores1(As, Bs).
latex_quote_underscores1([A|As], Bs0) :-
    (
        /* underscore */
	A = 95
    ->
	Bs0 = [92,95,123,125|Bs]
    ;
        /* percent symbol "%" */
        A = 37
    ->
        Bs0 = [92,37|Bs]
    ;
        /* ampersand symbol "&" */
        A = 38
    ->
        Bs0 = [92,38|Bs]
    ;
       /* degree symbol */
        A = 176
    ->
        Bs0 = [125,94,123,92,99,105,114,99,125,92,109,97,116,104,114,109,123|Bs]
%        Bs0 = [92,101,110,115,117,114,101,109,97,116,104,123,92,44,123,92,99,105,114,99,125,125|Bs]
     ;
        /* squared symbol */
        A = 178
    ->
        Bs0 = [125,94,50,92,109,97,116,104,114,109,123|Bs]
    ;
        /* cubed symbol */
        A = 179
    ->
        Bs0 = [125,94,51,92,109,97,116,104,114,109,123|Bs]
    ;
	Bs0 = [A|Bs]
    ),
	latex_quote_underscores1(As, Bs).

% = latex_list(+List, +Stream)
%
% output list to LaTeX file Stream as a simple enumerate environment.

latex_list(List, Stream) :-
	format(Stream, '\\begin{enumerate}~n', []),
	latex_list1(List, Stream),
	format(Stream, '\\end{enumerate}~n', []).
	
latex_list1([], _).
latex_list1([X|Xs], Stream) :-
	format(Stream, '\\item ~w~n', [X]),
	latex_list1(Xs, Stream).

% = latex_proof(+Proof, +Stream)
%
%

latex_proof(Proof, Stream) :-
	latex_proof(Proof, 0, Stream).

latex_proof(rule(ax, Label, Formula, []), Tab0, Stream) :-
	short_axioms,
	!,
	format(Stream, '\\infer[', []),
	write_rule_name(ax, Stream),
	format(Stream, ']{', []),
	latex_formula(Formula, Stream),
	nl(Stream),
	tab(Stream, Tab0),
	format(Stream, '}{', []),
	latex_label(Label, Stream),
	format(Stream, '}', []).
	
latex_proof(rule(Name, Label, Formula, Premisses), Tab0, Stream) :-
	tab(Stream, Tab0),
	Tab is Tab0 + 5,
	format(Stream, '\\infer[', []),
	write_rule_name(Name, Stream),
	format(Stream, ']{', []),
	latex_label(Label, Stream),
	format(Stream, ' \\vdash ', []),
	latex_formula(Formula, Stream),
	nl(Stream),
	tab(Stream, Tab0),
	format(Stream, '}{', []),
	latex_premisses(Premisses, Tab, Stream),
	nl(Stream),
	tab(Stream, Tab0),
	format(Stream, '}', []).

latex_premisses([], _, _).
latex_premisses([P|Ps], Tab, Stream) :-
	latex_premisses(Ps, P, Tab, Stream).

latex_premisses([], P, Tab, Stream) :-
	latex_proof(P, Tab, Stream).
latex_premisses([P|Ps], P0, Tab, Stream) :-
	latex_proof(P0, Tab, Stream),
	nl(Stream),
	tab(Stream, Tab),
	write(Stream, '&'),
	nl(Stream),
	latex_premisses(Ps, P, Tab, Stream).


write_rule_name(ax, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Lex}\\bc').
write_rule_name(hyp(N0), Stream) :-
	!,
	N is N0 + 1,
	format(Stream, '\\bo\\textit{Hyp}\\bc_{~w}', [N]).
write_rule_name(dri(N0), Stream) :-
	!,
	N is N0 + 1,
	format(Stream, '\\bo/\\textit{I}\\bc_{~w}', [N]).
write_rule_name(drdiaboxi(I), Stream) :-
	!,
	format(Stream, '\\bo/\\Diamond_{~w}\\Box_{~w}\\textit{I}\\bc', [I,I]).
write_rule_name(dldiaboxi(I), Stream) :-
	!,
	format(Stream, '\\bo\\backslash\\Diamond_{~w}\\Box_{~w}\\textit{I}\\bc', [I,I]).
write_rule_name(dli(N0), Stream) :-
	!,
	N is N0 + 1,
	format(Stream, '\\bo\\backslash\\textit{I}\\bc_{~w}', [N]).
write_rule_name(dr, Stream) :-
	!,
	write(Stream, '\\bo/\\textit{E}\\bc').
write_rule_name(dl, Stream) :-
	!,
	write(Stream, '\\bo\\backslash\\textit{E}\\bc').
write_rule_name(axiom, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Lex}\\bc').
write_rule_name(wr, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Wr}\\bc').
write_rule_name(wr_a, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Wr}_a\\bc').
write_rule_name(wpop, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Wpop}\\bc').
write_rule_name(wpop_vp, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Wpop}_{\\textit{vp}}\\bc').
write_rule_name(wpop_vpi, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Wpop}_{\\textit{vpi}}\\bc').
write_rule_name(a_dit, Stream) :-
	!,
	write(Stream, '\\bo\\textit{a\\ dit}\\bc').
write_rule_name(a_np_dit, Stream) :-
	!,
	write(Stream, '\\bo\\textit{a\\ np\\ dit}\\bc').
write_rule_name(e_start, Stream) :-
	!,
	write(Stream, '\\bo\\textit{E start}\\bc').
write_rule_name(ef_start, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Ef start}\\bc').
write_rule_name(ef_start_iv, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Ef start}_{\\texit{iv}}\\bc').
write_rule_name(e_end, Stream) :-
	!,
	write(Stream, '\\bo\\textit{E end}\\bc').
write_rule_name(e_start_l, Stream) :-
	!,
	write(Stream, '\\bo\\textit{E start}_l\\bc').
write_rule_name(e_end_l, Stream) :-
	!,
	write(Stream, '\\bo\\textit{E end}_l\\bc').
write_rule_name(let, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Let}\\bc').

write_rule_name(prod_e, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{E}\\bc').
write_rule_name(prod_i, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{I}\\bc').
write_rule_name(prod_w, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{W}\\bc').
write_rule_name(prod_wl, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{W}_l\\bc').
write_rule_name(prod_c, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{C}\\bc').
write_rule_name(prod_cl, Stream) :-
	!,
	write(Stream, '\\bo\\bullet\\textit{C}\\bc').

write_rule_name(gap_i, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Gap I}\\bc').
write_rule_name(gap_c, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Gap C}\\bc').
write_rule_name(gap_e, Stream) :-
	!,
	write(Stream, '\\bo\\textit{Gap E}\\bc').

write_rule_name(e_end_l_lnr, Stream) :-
	!,
	write(Stream, '\\bo\\textit{LNR E}_l\\bc').
write_rule_name(e_end_r_lnr, Stream) :-
	!,
	write(Stream, '\\bo\\textit{LNR E}_r\\bc').
write_rule_name(c_l_lnr, Stream) :-
	!,
	write(Stream, '\\bo\\textit{LNR C}_l\\bc').
write_rule_name(c_r_lnr, Stream) :-
	!,
	write(Stream, '\\bo\\textit{LNR C}_r\\bc').
% default rule to prevent failure when new rules are added
write_rule_name(Rule0, Stream) :-
	latex_quote_underscores(Rule0, Rule),
	format(Stream, '\\bo\\textit{~w}\\bc', [Rule]).