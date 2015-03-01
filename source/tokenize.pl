% -*- Mode: Prolog -*-

:- module(tokenize, [tokenize_string/2,
	             tokenize_term/2,
		     read_line/2,
		     symbol_char/1,
		     interpunction_char/1,
		     numerical_char/1,
		     trailer_char/1,
		     uppercase_letter/1,
		     lowercase_letter/1,
		     uppercase_to_lowercase_letter/2]).

:- dynamic(user:special_string/2).

% = read_line(-String, +Stream)
% read String from Stream

read_line(Chars, Stream) :-
	get0(Stream, C0),
	read_line(C0, Chars, Stream).

read_line(10, [], _) :- 
	!.
read_line(-1, [], _) :- 
	!.
read_line(C, [C|T], Stream) :-
	get0(Stream, C1),
	read_line(C1, T, Stream).

% = tokenization of input sentence

% = tokenize_string(+String, -ListOfTerms).
%
% skips the first character (if it's defined as trailer_char) and
% passes to tokenize_string1.

tokenize_string([], []).
tokenize_string([C|Cs], Res) :-
    (
	trailer_char(C) 
   -> 
	tokenize_string1(Cs, Res)
    ;
	tokenize_string1([C|Cs], Res)
    ).

% = tokenize_string1(+String, -ListOfTerms)
%
% parse String as ListOfTerms; we consider four cases here:
% (1) a prefix of String is defined as special_string, in which case
%     we add the corresponding output to ListOfTerms and continue with
%     the rest.
% (2) the first character is an interpuntion mark, in which case we
%     remove it and continue with the rest of the string.
% (3) we tokenize a prefix of String as Word (technically just a
%     Prolog Term) and continue with the rest.
% (4) otherwise, we just skip the first character and continue.

tokenize_string1([], []).
tokenize_string1([C|Cs], Res) :-
    (
        special([C|Cs], Ds0, Lex)
    ->
	Res = [Lex|Res0],
	trim_spaces(Ds0, Ds),
	tokenize_string1(Ds, Res0)
    ; 
	interpunction_char(C)
    ->
        trim_spaces(Cs, Ds),
	tokenize_string(Ds, Res)
    ; 
	tokenize_term([C|Cs], Ds, Word) 
    ->
	Res = [Word|Res0],
	tokenize_string1(Ds, Res0)
    ; 
	tokenize_string1(Cs, Res)
    ).

% = term tokenization

% = tokenize_term(+String, -Term).
%
% tokenize a subset of Prolog syntax.
% Unsupported at this moment are: operator definitions, floating point
% numbers, comments and escape sequences.

tokenize_term(String, Term) :-
       tokenize_term(String, Rest, Term),
     ( Rest = [] ->
       true
     ;
       append(Init, Rest, String),
       format('{Syntax Error: Trailing material deleted.~2n~s<here>~s}',[Init,Rest])
     ).

tokenize_term([], [], []).
tokenize_term([C|Cs], Res0, Term) :-
       C=127 ->
         tokenize_term(Cs, Res0, Term)
     ; C<33 ->
         tokenize_term(Cs, Res0, Term)
     ; C=39 ->
       /* quoted term */
         tokenize_quoted_term(Cs, Res, L, []),
         name(Term, L),
        ( Res = [39|Res1] ->
          trim_spaces(Res1, Res0)
        ;
          trim_spaces(Res, Res0),
          portray_missing_material('''', Res, [C|Cs])
         )
     ; C=40 ->
       /* (Term) */
         tokenize_term(Cs, Res, Term),
        ( Res = [41|Res1] ->
          trim_spaces(Res1, Res0)
        ;
          trim_spaces(Res, Res0),
          portray_missing_material(')', Res, [C|Cs])
        )
     ; C=91 ->
       /* list */
         tokenize_list(Cs, Res, Term),
        ( Res = [93|Res1] ->
          trim_spaces(Res1, Res0)
        ;
          trim_spaces(Res, Res0),
          portray_missing_material(']', Res, [C|Cs])
        )
     ; C=34 ->
       /* string */
         tokenize_string(Cs, Res, Term, []),
        ( Res = [34|Res1] ->
          trim_spaces(Res1, Res0)
        ;
          trim_spaces(Res, Res0),
          portray_missing_material('"', Res, [C|Cs])
        )
     ; C=43,
       trim_spaces(Cs, [D|Ds]),
       D>47,
       D<58 ->
       /* positive integer */
          tokenize_number([D|Ds], Res, NumS, []),
          number_chars(Term, NumS),
          trim_spaces(Res, Res0)
     ; C=45,
       trim_spaces(Cs, [D|Ds]),
       D>47,
       D<58 ->
       /* negative integer */
          tokenize_number([D|Ds], Res, NumS, []),
          number_chars(Term, [45|NumS]),
          trim_spaces(Res, Res0)
     ; (C>47,
        C<58) ->
       /* unsigned integer */
          tokenize_number(Cs, Res, NumS, []),
          number_chars(Term, [C|NumS]),
          trim_spaces(Res, Res0)
     ; symbol_char(C) ->
       /* symbols */
          tokenize_symbol(Cs, Res, Ss, []),
          name(Term, [C|Ss]),
          trim_spaces(Res, Res0)
     ; alpha_char(C, C1) ->
          tokenize_word(Cs, Ds, [C1|Ws], Ws, Word),
        ( Ds = [40|Ds0] ->
          trim_spaces(Ds0, Ds1),
          /* functor/arguments */
          tokenize_args(Ds1, Res, TermList, []),
          Term =.. [Word|TermList],
           (Res = [41|Res1] ->
            trim_spaces(Res1, Res0)
           ;
            trim_spaces(Res, Res0),
            portray_missing_material(')', Res, [C|Cs])
           )
        ;
          /* atom */
          Term = Word,
          trim_spaces(Ds,Res0)
        ).

tokenize_word([], [], Ws0, [], Word) :-
       name(Word, Ws0).
tokenize_word([C|Cs], Ds, Ws0, Ws, Word) :-
       special([C|Cs], _, _) ->
         Ws = [],
         name(Word, Ws0),
         Ds = [C|Cs]
     ; alphanum_char(C, C1) ->
          Ws = [C1|Ws1],
          tokenize_word(Cs, Ds, Ws0, Ws1, Word)
     ; Ws = [],
       name(Word, Ws0),
       Ds = [C|Cs].

tokenize_symbol([], [], S, S).
tokenize_symbol([S|Rs0], Rs, Ss0, Ss) :-
       symbol_char(S) ->
          Ss0 = [S|Ss1],
          tokenize_symbol(Rs0, Rs, Ss1, Ss)
     ; Rs = [S|Rs0],
       Ss0 = Ss.

tokenize_number([], [], NumS, NumS).
tokenize_number([C|Cs], Ds, NumS0, NumS) :-
       C>47,
       C<58 ->
         NumS0 = [C|NumS1],
         tokenize_number(Cs, Ds, NumS1, NumS)
     ;
       Ds = [C|Cs],
       NumS0 = NumS.

tokenize_quoted_term([], [], L, L).
tokenize_quoted_term([C|Cs0], Ds, L0, L) :-
       [C|Cs0] = [39,39|Cs] ->
          L0 = [39|L1],
          tokenize_quoted_term(Cs, Ds, L1, L)
     ; C=39 ->
          L0 = L,
          Ds = [C|Cs0]
     ; L0 = [C|L1],
        tokenize_quoted_term(Cs0, Ds, L1, L).

tokenize_string([], [], Term, Term).
tokenize_string([C|Cs0], Ds, T0, T) :-
       [C|Cs0] = [34,34|Cs] ->
         T0 = [34|T1],
         tokenize_string(Cs, Ds, T1, T)
     ; C = 34 ->
         T0 = T,
         Ds = [C|Cs0]
     ; T0 = [C|T1],
       tokenize_string(Cs0, Ds, T1, T).

tokenize_list([], [], []).
tokenize_list([C|Cs], Ds, L) :-
       C = 93 ->
         Ds = [C|Cs],
         L = []
     ; tokenize_term([C|Cs], Res0, Term),
       ( Res0 = [44|Res1] ->
           trim_spaces(Res1, Res),
           L = [Term|L0],
           tokenize_list(Res, Ds, L0)
       ; Res0 = [124|Res1] ->
           trim_spaces(Res1, Res),
           L = [Term|L0],
           tokenize_term(Res, Ds, L0)
       ; Res0 = [93|Res1] ->
           Ds = [93|Res1],
           L = [Term]
       ; Ds = Res0,
         L = [Term]
        ).

tokenize_args([], [], Args, Args).
tokenize_args([C|Cs0], Es, As0, As) :-
       tokenize_term([C|Cs0], Ds, Arg),
     ( Ds = [44|Ds0] ->
       As0 = [Arg|As1],
       trim_spaces(Ds0, Ds1),
       tokenize_args(Ds1, Es, As1, As)
     ;
       Es = Ds,
       As0 = [Arg|As]
     ).

trim_spaces([], []).
trim_spaces([X|Xs], Ys) :-
    (
	X = 127
    ->
        trim_spaces(Xs, Ys)
    ; 
	X < 33
    ->
        trim_spaces(Xs, Ys)
    ; 
	Ys = [X|Xs]
    ). 


portray_missing_material(Char, Bs, Cs) :-
       append(As, Bs, Cs),
       format('{Syntax Error: Missing "~w" inserted.~2n~s<here>~s}', [Char,As,Bs]).


special([X|Xs], Ys, Res) :-
       user:special_string([Z|Zs], Res),
       append([Z|Zs], Ys, [X|Xs]).

alpha_char(X,Y) :-
    (
	lowercase_letter(X)
    ->
        Y = X
    ;
	uppercase_to_lowercase_letter(X, Z)
    ->
	Y = Z
    ).

alphanum_char(X,Y) :-
    (
	/* underline */
	X=95
    ->
        Y=95
    ;
	lowercase_letter(X)
    ->
	Y = X
    ;
	uppercase_to_lowercase_letter(X, Z)
    ->
	Y = Z
    ;
	numerical_char(X)
    ->
	Y = X
    ).

% =


% The Prolog symbol characters. 

symbol_char( 35). % #
symbol_char( 36). % $
symbol_char( 38). % &
symbol_char( 42). % *
symbol_char( 43). % +
symbol_char( 45). % -
symbol_char( 46). % .
symbol_char( 47). % /
symbol_char( 58). % :
symbol_char( 60). % <
symbol_char( 61). % =
symbol_char( 62). % >
symbol_char( 63). % ?
symbol_char( 64). % @
symbol_char( 92). % \
symbol_char( 94). % ^
symbol_char( 96). % `
symbol_char(126). % ~

% Interpunction characters should not be treated
% as Prolog terms when they occur in a sentence.

interpunction_char(33). % !
interpunction_char(34). % "
interpunction_char(39). % '
interpunction_char(45). % -
interpunction_char(46). % .
interpunction_char(58). % :
interpunction_char(59). % ;
interpunction_char(63). % ?
interpunction_char(96). % `

% Lower case letters, with and without accents

lowercase_letter( 97). % a
lowercase_letter( 98). % b
lowercase_letter( 99). % c
lowercase_letter(100). % d
lowercase_letter(101). % e
lowercase_letter(102). % f
lowercase_letter(103). % g
lowercase_letter(104). % h
lowercase_letter(105). % i
lowercase_letter(106). % j
lowercase_letter(107). % k
lowercase_letter(108). % l
lowercase_letter(109). % m
lowercase_letter(110). % n
lowercase_letter(111). % o
lowercase_letter(112). % p
lowercase_letter(113). % q
lowercase_letter(114). % r
lowercase_letter(115). % s
lowercase_letter(116). % t
lowercase_letter(117). % u
lowercase_letter(118). % v
lowercase_letter(119). % w
lowercase_letter(120). % x
lowercase_letter(121). % y
lowercase_letter(122). % z

lowercase_letter(224). % a grave
lowercase_letter(225). % a acute
lowercase_letter(226). % a circ
lowercase_letter(227). % a tilde
lowercase_letter(228). % a uml
lowercase_letter(229). % a ring
lowercase_letter(230). % ae lig
lowercase_letter(231). % c cedil
lowercase_letter(232). % e grave
lowercase_letter(233). % e acute
lowercase_letter(234). % e circ
lowercase_letter(235). % e uml
lowercase_letter(236). % i grave
lowercase_letter(237). % i acute
lowercase_letter(238). % i circ
lowercase_letter(239). % i uml
lowercase_letter(240). % eth
lowercase_letter(241). % n tilde
lowercase_letter(242). % o grave
lowercase_letter(243). % o acute
lowercase_letter(244). % o circ
lowercase_letter(245). % o tilde
lowercase_letter(246). % o uml
lowercase_letter(248). % o slash
lowercase_letter(249). % u grave
lowercase_letter(250). % u acute
lowercase_letter(251). % u circ
lowercase_letter(252). % u uml
lowercase_letter(253). % y acute
lowercase_letter(255). % y uml

% Uppercase letters, with and without accents

uppercase_letter( 65). % A
uppercase_letter( 66). % B
uppercase_letter( 67). % C
uppercase_letter( 68). % D
uppercase_letter( 69). % E
uppercase_letter( 70). % F
uppercase_letter( 71). % G
uppercase_letter( 72). % H
uppercase_letter( 73). % I
uppercase_letter( 74). % J
uppercase_letter( 75). % K
uppercase_letter( 76). % L
uppercase_letter( 77). % M
uppercase_letter( 78). % N
uppercase_letter( 79). % O
uppercase_letter( 80). % P
uppercase_letter( 81). % Q
uppercase_letter( 82). % R
uppercase_letter( 83). % S
uppercase_letter( 84). % T
uppercase_letter( 85). % U
uppercase_letter( 86). % V
uppercase_letter( 87). % W
uppercase_letter( 88). % X
uppercase_letter( 89). % Y
uppercase_letter( 90). % Z

uppercase_letter(192). % A grave
uppercase_letter(193). % A acute
uppercase_letter(194). % A circ
uppercase_letter(195). % A tilde
uppercase_letter(196). % A uml
uppercase_letter(197). % A ring
uppercase_letter(198). % AE lig
uppercase_letter(199). % C cedil
uppercase_letter(200). % E grave
uppercase_letter(201). % E acute
uppercase_letter(202). % E circ
uppercase_letter(203). % E uml
uppercase_letter(204). % I grave
uppercase_letter(205). % I acute
uppercase_letter(206). % I circ
uppercase_letter(207). % I uml
uppercase_letter(208). % ETH
uppercase_letter(209). % N tilde
uppercase_letter(210). % O grave
uppercase_letter(211). % O acute
uppercase_letter(212). % O circ
uppercase_letter(213). % O tilde
uppercase_letter(214). % O uml
uppercase_letter(216). % O slash
uppercase_letter(217). % U grave
uppercase_letter(218). % U acute
uppercase_letter(219). % U circ
uppercase_letter(220). % U uml
uppercase_letter(221). % Y acute

% Upper to lower case conversion

uppercase_to_lowercase_letter( 65,  97). % A a
uppercase_to_lowercase_letter( 66,  98). % B b
uppercase_to_lowercase_letter( 67,  99). % C c
uppercase_to_lowercase_letter( 68, 100). % D d
uppercase_to_lowercase_letter( 69, 101). % E e
uppercase_to_lowercase_letter( 70, 102). % F f
uppercase_to_lowercase_letter( 71, 103). % G g
uppercase_to_lowercase_letter( 72, 104). % H h
uppercase_to_lowercase_letter( 73, 105). % I i
uppercase_to_lowercase_letter( 74, 106). % J j
uppercase_to_lowercase_letter( 75, 107). % K k
uppercase_to_lowercase_letter( 76, 108). % L l
uppercase_to_lowercase_letter( 77, 109). % M m
uppercase_to_lowercase_letter( 78, 110). % N n
uppercase_to_lowercase_letter( 79, 111). % O o
uppercase_to_lowercase_letter( 80, 112). % P p
uppercase_to_lowercase_letter( 81, 113). % Q q
uppercase_to_lowercase_letter( 82, 114). % R r
uppercase_to_lowercase_letter( 83, 115). % S s
uppercase_to_lowercase_letter( 84, 116). % T t
uppercase_to_lowercase_letter( 85, 117). % U u
uppercase_to_lowercase_letter( 86, 118). % V v
uppercase_to_lowercase_letter( 87, 119). % W w
uppercase_to_lowercase_letter( 88, 120). % X x
uppercase_to_lowercase_letter( 89, 121). % Y y
uppercase_to_lowercase_letter( 90, 122). % Z z

uppercase_to_lowercase_letter(192, 224). % A grave
uppercase_to_lowercase_letter(193, 225). % A acute
uppercase_to_lowercase_letter(194, 226). % A circ
uppercase_to_lowercase_letter(195, 227). % A tilde
uppercase_to_lowercase_letter(196, 228). % A uml
uppercase_to_lowercase_letter(197, 229). % A ring
uppercase_to_lowercase_letter(198, 230). % AE lig
uppercase_to_lowercase_letter(199, 231). % C cedil
uppercase_to_lowercase_letter(200, 232). % E grave
uppercase_to_lowercase_letter(201, 233). % E acute
uppercase_to_lowercase_letter(202, 234). % E circ
uppercase_to_lowercase_letter(203, 235). % E uml
uppercase_to_lowercase_letter(204, 236). % I grave
uppercase_to_lowercase_letter(205, 237). % I acute
uppercase_to_lowercase_letter(206, 238). % I circ
uppercase_to_lowercase_letter(207, 239). % I uml
uppercase_to_lowercase_letter(208, 240). % ETH
uppercase_to_lowercase_letter(209, 241). % N tilde
uppercase_to_lowercase_letter(210, 242). % O grave
uppercase_to_lowercase_letter(211, 243). % O acute
uppercase_to_lowercase_letter(212, 244). % O circ
uppercase_to_lowercase_letter(213, 245). % O tilde
uppercase_to_lowercase_letter(214, 246). % O uml
uppercase_to_lowercase_letter(216, 247). % O slash
uppercase_to_lowercase_letter(217, 248). % U grave
uppercase_to_lowercase_letter(218, 249). % U acute
uppercase_to_lowercase_letter(219, 250). % U circ
uppercase_to_lowercase_letter(220, 252). % U uml
uppercase_to_lowercase_letter(221, 253). % Y acute

% numbers

numerical_char(48).  % 0
numerical_char(49).  % 1
numerical_char(50).  % 2
numerical_char(51).  % 3
numerical_char(52).  % 4
numerical_char(53).  % 5
numerical_char(54).  % 6
numerical_char(55).  % 7
numerical_char(56).  % 8
numerical_char(57).  % 9

% Grammaticality indication, ignore at the start of a string.

trailer_char(32). % space
trailer_char(42). % *
trailer_char(63). % ?
