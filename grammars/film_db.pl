% -*- Mode: Prolog -*-
% ============================================================
% film_db.pl
% ============================================================
% !grail 3.1.1

% = Film database interface

% This grammar is meant to illustrate Grail's possiblities for
% interfacing with an external database. Questions are
% transformed into Prolog queries and the solutions to these
% queries are output. Statements are transformed into Prolog
% facts which are added to the database.

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
:- dynamic postulate/3,postulate1/3,special_string/2.
:- dynamic macro/2,lex/3,example/2.

% ============================================================
% Database
% ============================================================

:- dynamic query_db/2,query_db/3,query_db/4.

% ============================================================
% Postulates
% ============================================================

atomic_type(n, e->t).
atomic_type(np, e).
atomic_type(s, t).
atomic_type(cn, n).
atomic_type(q, t).
atomic_type(wh, t).
atomic_type(pp, e).
atomic_type(s_inf, t).

% = structural postulates

postulate(p(0,A,p(0,B,zip(0,C))), p(0,p(0,A,B),zip(0,C)), 'Whr1').
postulate(p(0,p(0,A,zip(0,C)),B), p(0,p(0,A,B),zip(0,C)), 'Whr2').

% = inertness

%inert(0).
%inert_dia(0).

% = continuity

continuous(0).
continuous_dia(0).

% = non internal modes

external(0).

external_dia(0).

% = extraction

custom_first_order(dr(0,A0,dia(0,box(0,B0))), dr(0,A,dia(0,box(0,B))), P, [X,Y]) :-
	!,
	flip(P,Q),
	gensym_pos(Q, Z),
	add_first_order(A0, A, Q, [X,Y]),
	add_first_order(B0, B, P, [Z,Z]).

% ============================================================
% Macros
% ============================================================

% = macro(Form,Replacement)

macro(inf,dl(0,np,lit(s_inf))).
macro(bang(A,B), dia(A,box(A,B))).
macro(iv, dl(0,np,s)).
macro(tv, dr(0,iv,np)).
macro(prep, dr(0,pp,np)).
macro(gq_subj, dr(0,lit(s),dl(0,np,lit(s)))).
macro(gq_subj_inf, dr(0,lit(s_inf),dl(0,np,lit(s_inf)))).
macro(det_subj, dr(0,gq_subj,n)).
macro(gq_obj, dl(0,dr(0,lit(s),np),lit(s))).
macro(det_obj, dr(0,gq_obj,n)).
macro(refl, dl(0,tv,iv)).
macro(relpro, dr(0,rel,relbody)).
macro(relbody, dr(0,s,dia(0,box(0,np)))).
macro(rel, dl(0,n,n)).
macro(conj(A), dr(0,dl(0,A,A),A)).
macro(rncconj(A,B), dr(0,dl(0,dr(0,A,bang(2,B)),dr(0,A,B)),dr(0,A,bang(2,B)))).

% ============================================================
% Lexicon
% ============================================================

% = lex(Pros,Formula,Sem)

lex(the, dr(0,np,n), lambda(P,quant(iota,X,appl(P,X)))).
lex(everybody,gq_subj,lambda(P,quant(forall,X,appl(P,X)))).
lex(somebody,gq_subj,lambda(P,quant(exists,X,appl(P,X)))).
lex(somebody,gq_subj_inf,lambda(P,quant(exists,X,appl(P,X)))).
%lex(somebody,gq_obj,lambda(P,quant(exists,X,appl(P,X)))).
lex(and,conj(lit(s)),lambda(P,lambda(Q,bool(P,&,Q))) ).
lex(and,conj(dl(0,np,lit(s))), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).
lex(and,conj(dr(0,lit(s),np)),lambda(P,lambda(Q,lambda(X,bool(appl(P,X),&,appl(Q,X))))) ).
lex(and,rncconj(dl(0,np,lit(s)),np),lambda(P,lambda(Q,lambda(X,lambda(Y,bool(appl(appl(P,X),Y),&,appl(appl(Q,X),Y)))))) ).
lex(or,conj(lit(s)),lambda(P,lambda(Q,bool(P,\/,Q))) ).
lex(or,conj(dl(0,np,lit(s))), lambda(P,lambda(Q,lambda(X,bool(appl(P,X),\/,appl(Q,X))))) ).
lex(or,conj(dr(0,lit(s),np)),lambda(P,lambda(Q,lambda(X,bool(appl(P,X),\/,appl(Q,X))))) ).
lex(or,conj(dr(0,dl(0,np,lit(s)),np)),lambda(P,lambda(Q,lambda(X,lambda(Y,bool(appl(appl(P,X),Y),\/,appl(appl(Q,X),Y)))))) ).
lex(a, det_subj, lambda(X,lambda(Y,quant(?,Z,bool(appl(X,Z),&,appl(Y,Z)))))).
lex(a, det_obj, lambda(X,lambda(Y,quant(?,Z,bool(appl(X,Z),&,appl(Y,Z)))))).
lex(an, det_subj, lambda(X,lambda(Y,quant(?,Z,bool(appl(X,Z),&,appl(Y,Z)))))).
lex(an, det_obj, lambda(X,lambda(Y,quant(?,Z,bool(appl(X,Z),&,appl(Y,Z)))))).
lex(every, det_subj, lambda(X,lambda(Y,quant(forall,Z,bool(appl(X,Z),->,appl(Y,Z)))))).
lex(every, det_obj,
    lambda(X,lambda(Y,quant(forall,Z,bool(appl(X,Z),->,appl(Y,Z)))))).
lex(didnot, dr(0,iv,inf),lambda(P,lambda(X,not(appl(P,X))))).
lex(didnot, dr(0,q,s_inf),lambda(P,not(P))).
lex(movie, lit(n), movie).
lex(movies, lit(n), movie).
lex(film,lit(n),movie).
lex(producer, lit(n), producer).
lex(character, lit(n), character).
lex(characters, lit(n), character).
lex(actor, lit(n), actor).
lex(actors, lit(n), actor).
lex(actress, lit(n), actor).
lex(actresses, lit(n), actor).
lex(oscar, lit(n), oscar).
lex(direct, dr(0,inf,np), direct).
lex(directed, tv, direct).
lex(appeared, dr(0,iv,pp), lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,B),A),C))))).
lex(appear, dr(0,inf,pp), lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,B),A),C))))).
lex(play, dr(0,inf,pp), lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,B),A),C))))).
lex(played, dr(0,iv,pp),  lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,B),A),C))))).
lex(play, dr(0,dr(0,inf,pp),np), play).
lex(played, dr(0,dr(0,iv,pp),np), play).
lex(play, dr(0,inf,np), lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,A),C),B))))).
lex(played, dr(0,iv,np), lambda(A,lambda(B,quant(?,C,appl(appl(appl(play,A),C),B))))).
lex(win, dr(0,inf,np), win).
lex(won, tv, win).
lex(love, dr(0,inf,np), love).
lex(loves, tv, love).
lex(hate, dr(0,inf,np), hate).
lex(hates, tv, hate).
lex(write, dr(0,inf,np), wrote).
lex(wrote, tv, wrote).
lex(did, dr(0,q,lit(s_inf)), lambda(A,A)).
lex(does, dr(0,q,p(0,np,inf)), lambda(A,appl(snd(A),fst(A)))).
lex(in, dr(0,pp,np), lambda(A,A)).
lex(how, dr(0,dr(0,dr(0,wh,dr(0,q,dia(0,box(0,np)))),n),cn), lambda(A,lambda(B,lambda(C,quant(A,D,bool(appl(B,D),&,appl(C,D))))))).
lex(what, dr(0,wh,dr(0,q,dia(0,box(0,np)))), lambda(A,quant(?,B,appl(A,B)))).
lex(who, dr(0,wh,dr(0,q,dia(0,box(0,np)))), lambda(A,quant(?,B,appl(A,B)))).
lex(which, dr(0,dr(0,wh,dr(0,q,dia(0,box(0,np)))),n), lambda(A,lambda(B,quant(?,C,bool(appl(A,C),&,appl(B,C)))))).
lex(who, dr(0,wh,dl(0,np,s)), lambda(A,quant(?,B,appl(A,B)))).
lex(which, dr(0,dr(0,wh,dl(0,np,s)),n), lambda(A,lambda(B,quant(?,C,bool(appl(A,C),&,appl(B,C)))))).
lex(many, cn, \#).
% movies
lex(good_will_hunting, lit(np), 'Good Will Hunting').
lex(titanic, lit(np), 'Titanic').
lex(romeo_and_juliet, lit(np), 'Romeo and Juliet').
lex(pulp_fiction, lit(np), 'Pulp Fiction').
lex(reservoir_dogs, lit(np), 'Reservoir Dogs').
lex(blade_runner, lit(np), 'Blade Runner').
lex(alien, lit(np), 'Alien').
lex(aliens, lit(np), 'Aliens').
lex(the_godfather, lit(np), 'The Godfather').
lex(seven, lit(np), 'Seven').
lex(se7en, lit(np), 'Seven').
lex(the_godfather2, lit(np), 'The Godfather II').
lex(the_godfather_ii, lit(np), 'The Godfather II').
lex(thelma_and_louise, lit(np), 'Thelma and Louise').
% characters
lex(romeo, lit(np), 'Romeo').
lex(juliet, lit(np), 'Juliet').
lex(jack_dawson, lit(np), 'Jack Dawson').
lex(vincent_vega, lit(np), 'Vincent Vega').
lex(sean_mcguire, lit(np), 'Sean McGuire').
lex(mr_white, lit(np), 'Mr White').
lex(mr_brown, lit(np), 'Mr Brown').
lex(jimmie, lit(np), 'Jimmie').
lex(mia, lit(np), 'Mia').
lex(ellen_ripley, lit(np), 'Ellen Ripley').
lex(ripley, lit(np), 'Ellen Ripley').
lex(bishop, lit(np), 'Bishop').
lex(deckard, lit(np), 'Deckard').
lex(batty, lit(np), 'Roy Batty').
lex(rachael, lit(np), 'Rachael').
lex(dallas, lit(np), 'Dallas').
lex(vito_corleone, lit(np), 'Vito Corleone').
lex(michael_corleone, lit(np), 'Michael Corleone').
lex(tom_hagen, lit(np), 'Tom Hagen').
lex(william_somerset, lit(np), 'William Somerset').
lex(somerset, lit(np), 'William Somerset').
lex(david_mills, lit(np), 'David Mills').
lex(mills, lit(np), 'David Mills').
lex(louise_sawyer, lit(np), 'Louise').
lex(thelma_dickinson, lit(np), 'Thelma').
lex(louise, lit(np), 'Louise').
lex(thelma, lit(np), 'Thelma').
% actors
lex(leonardo_dicaprio, lit(np), 'Leonardo DiCaprio').
lex(robin_williams, lit(np), 'Robin Williams').
lex(john_travolta, lit(np), 'John Travolta').
lex(harvey_keitel, lit(np), 'Harvey Keitel').
lex(lance_henriksen, lit(np), 'Lance Henriksen').
lex(tom_skerritt, lit(np), 'Tom Skerritt').
lex(harrison_ford, lit(np), 'Harrison Ford').
lex(rutger_hauer, lit(np), 'Rutger Hauer').
lex(marlon_brando, lit(np), 'Marlon Brando').
lex(al_pacino, lit(np), 'Al Pacino').
lex(robert_duvall, lit(np), 'Robert Duvall').
lex(morgan_freeman, lit(np), 'Morgan Freeman').
lex(brad_pitt, lit(np), 'Brad Pitt').
lex(robert_de_niro, lit(np), 'Robert De Niro').
lex(uma_thurman, lit(np), 'Uma Thurman').
lex(claire_danes, lit(np), 'Claire Danes').
lex(sigourney_weaver, lit(np), 'Sigourney Weaver').
lex(sean_young, lit(np), 'Sean Young').
lex(susan_sarandon, lit(np), 'Susan Sarandon').
lex(geena_davis, lit(np), 'Geena Davis').
lex(cameron, lit(np), 'James Cameron').
lex(van_sant, lit(np), 'Gus Van Sant').
lex(tarantino, lit(np), 'Quentin Tarantino').
lex(luhrmann, lit(np), 'Baz Luhrmann').
lex(scott, lit(np), 'Ridley Scott').
lex(coppola, lit(np), 'Francis Ford Coppola').
lex(francis_ford_coppola,lit(np),'Francis Ford Coppola').
lex(francis_coppola,lit(np),'Francis Ford Coppola').
lex(fincher, lit(np), 'David Fincher').

lex(nick_nolte, lit(np), 'Nick Nolte').
lex(cape_fear, lit(np), 'Cape Fear').
lex(sam_bowden, lit(np), 'Sam Bowden').
lex(jessica_lange, lit(np), 'Jessica Lange').
lex(leigh_bowden, lit(np), 'Leigh Bowden').
lex(juliette_lewis, lit(np), 'Juliette Lewis').
lex(danielle_bowden, lit(np), 'Danielle Bowden').
lex(kevin_spacey, lit(np), 'Kevin Spacey').
lex(john_doe, lit(np), 'John Doe').
lex(bryan_singer, lit(np), 'Bryan Singer').
lex(the_usual_suspects, lit(np), 'The Usual Suspects').
lex(verbal_kint, lit(np), 'Verbal Kint').
lex(gabriel_byrne, lit(np), 'Gabriel Byrne').
lex(dean_keaton, lit(np), 'Dean Keaton').
lex(dark_city, lit(np), 'Dark City').
lex(alex_proyas, lit(np), 'Alex Proyas').
lex(rufus_sewell, lit(np), 'Rufus Sewell').
lex(john_murdoch, lit(np), 'John Murdoch').
lex(william_hurt, lit(np), 'William Hurt').
lex(frank_bumstead, lit(np), 'Frank Bumstead').
lex(kiefer_sutherland, lit(np), 'Kiefer Sutherland').
lex(doctor_schreber, lit(np), 'Doctor Schreber').
lex(die_hard, lit(np), 'Die Hard').
lex(john_mctiernan, lit(np), 'John McTiernan').
lex(bruce_willis, lit(np), 'Bruce Willis').
lex(john_mcclane, lit(np), 'John McClane').
lex(alan_rickman, lit(np), 'Alan Rickman').
lex(hans_gruber, lit(np), 'Hans Gruber').
lex(silence_of_the_lambs, lit(np), 'Silence of the Lambs').
lex(jonathan_demme, lit(np), 'Jonathan Demme').
lex(anthony_hopkins, lit(np), 'Anthony Hopkins').
lex(hannibal_lecter, lit(np), 'Hannibal Lecter').
lex(jodie_foster, lit(np), 'Jodie Foster').
lex(clarice_starling, lit(np), 'Clarice Starling').
lex(faceoff, lit(np), 'Face/Off').
lex(john_woo, lit(np), 'John Woo').
lex(nicolas_cage, lit(np), 'Nicolas Cage').
lex(castor_troy, lit(np), 'Castor Troy').
lex(john_travolta, lit(np), 'John Travolta').
lex(sean_archer, lit(np), 'Sean Archer').
lex(trainspotting, lit(np), 'Trainspotting').
lex(danny_boyle, lit(np), 'Danny Boyle').
lex(ewan_mcgregor, lit(np), 'Ewan McGregor').
lex(mark_renton, lit(np), 'Mark Renton').
lex(robert_carlyle, lit(np), 'Robert Carlyle').
lex(francis_begbie, lit(np), 'Francis Begbie').
lex(scream, lit(np), 'Scream').
lex(wes_craven, lit(np), 'Wes Craven').
lex(neve_campbell, lit(np), 'Neve Campbell').
lex(sidney_prescott, lit(np), 'Sidney Prescott').
lex(courteney_cox, lit(np), 'Courteney Cox').
lex(gale_weathers, lit(np), 'Gale Weathers').
lex(skeet_ulrich, lit(np), 'Skeet Ulrich').
lex(billy_loomis, lit(np), 'Billy Loomis').
lex(leaving_las_vegas, lit(np), 'Leaving Las Vegas').
lex(mike_figgis, lit(np), 'Mike Figgis').
lex(ben_sanderson, lit(np), 'Ben Sanderson').
lex(elisabeth_shue, lit(np), 'Elisabeth Shue').
lex(sera, lit(np), 'Sera').
lex(fargo, lit(np), 'Fargo').
lex(joel_coen, lit(np), 'Joel Coen').
lex(frances_mcdormand, lit(np), 'Frances McDormand').
lex(marge_gunderson, lit(np), 'Marge Gunderson').
lex(steve_buscemi, lit(np), 'Steve Buscemi').
lex(carl_showalter, lit(np), 'Carl Showalter').
lex(mission_impossible, lit(np), 'Mission Impossible').
lex(brian_de_palma, lit(np), 'Brian De Palma').
lex(tom_cruise, lit(np), 'Tom Cruise').
lex(ethan_hunt, lit(np), 'Ethan Hunt').
lex(jon_voight, lit(np), 'Jon Voight').
lex(jim_phelps, lit(np), 'Jim Phelps').
lex(emmanuelle_beart, lit(np), 'Emmanuelle Beart').
lex(claire_phelps, lit(np), 'Claire Phelps').
lex(twelve_monkeys,lit(np),'Twelve Monkeys').
lex(terry_gilliam,lit(np),'Terry Gilliam').
lex(james_cole,lit(np),'James Cole').
lex(madeleine_stowe,lit(np),'Madeleine Stowe').
lex(kathryn_railly,lit(np),'Kathryn Railly').
lex(jeffrey_goines,lit(np),'Jeffrey Goines').
lex(raiders_of_the_lost_ark, lit(np), 'Raiders of the Lost Ark').
lex(steven_spielberg, lit(np), 'Steven Spielberg').
lex(indiana_jones, lit(np), 'Indiana Jones').
lex(john_rhysdavies, lit(np), 'John Rhys-Davies').
lex(sallah, lit(np), 'Sallah').
lex(karen_allen, lit(np), 'Karen Allen').
lex(marion_ravenwood, lit(np), 'Marion Ravenwood').
lex(the_fugitive, lit(np), 'The Fugitive').
lex(andrew_davis, lit(np), 'Andrew Davis').
lex(richard_kimble, lit(np), 'Richard Kimble').
lex(tommy_lee_jones, lit(np), 'Tommy Lee Jones').
lex(samuel_gerard, lit(np), 'Samuel Gerard').
lex(jeroen_krabbe, lit(np), 'Jeroen Krabbe').
lex(charles_nichols, lit(np), 'Charles Nichols').
lex(men_in_black, lit(np), 'Men in Black').
lex(barry_sonnenfeld, lit(np), 'Barry Sonnenfeld').
lex(kay, lit(np), 'Kay').
lex(will_smith, lit(np), 'Will Smith').
lex(jay, lit(np), 'Jay').
lex(linda_fiorentino, lit(np), 'Linda Fiorentino').
lex(laurel, lit(np), 'Laurel').
lex(gattaca, lit(np), 'Gattaca').
lex(andrew_niccol, lit(np), 'Andrew Niccol').
lex(ethan_hawke, lit(np), 'Ethan Hawke').
lex(vincent_freeman, lit(np), 'Vincent Freeman').
lex(irene_cassini, lit(np), 'Irene Cassini').
lex(mr_pink,lit(np),'Mr Pink').
lex(the_untouchables, lit(np), 'The Untouchables').
lex(kevin_costner, lit(np), 'Kevin Costner').
lex(eliot_ness, lit(np), 'Eliot Ness').
lex(sean_connery, lit(np), 'Sean Connery').
lex(jim_malone, lit(np), 'Jim Malone').
lex(andy_garcia, lit(np), 'Andy Garcia').
lex(george_stone, lit(np), 'George Stone').
lex(al_capone, lit(np), 'Al Capone').
lex(kate_winslet,lit(np),'Kate Winslet').
lex(rose_dewit_bukater,lit(np),'Rose DeWit Bukater').
lex(rose,lit(np),'Rose DeWit Bukater').
lex(highlander,lit(np),'Highlander').
lex(russell_mulcahy,lit(np),'Russell Mulcahy').
lex(christopher_lambert,lit(np),'Christopher Lambert').
lex(connor_macleod,lit(np),'Connor MacLeod').
lex(macleod,lit(np),'Connor MacLeod').
lex(ramirez,lit(np),'Ramirez').
lex(martin_sheen,lit(np),'Martin Sheen').
lex(colonel_kurtz,lit(np),'Colonel Kurtz').
lex(lieutenant_kilgore,lit(np),'Lieutenant Kilgore').
lex(captain_willard,lit(np),'Captain Willard').
lex(wall_street,lit(np),'Wall Street').
lex(oliver_stone,lit(np),'Oliver Stone').
lex(stone,lit(np),'Oliver Stone').
lex(michael_douglas,lit(np),'Michael Douglas').
lex(gordon_gekko,lit(np),'Gordon Gekko').
lex(charlie_sheen,lit(np),'Charlie Sheen').
lex(bud_fox,lit(np),'Bud Fox').
lex(carl_fox,lit(np),'Carl Fox').
lex(chris,lit(np),'Chris').
lex(forest_whitaker,lit(np),'Forest Whitaker').
lex(big_harold,lit(np),'Big Harold').
lex(tom_berenger,lit(np),'Tom Berenger').
lex(sgt_barnes,lit(np),'Sgt. Barnes').
lex(jim_carrey,lit(np),'Jim Carrey').
lex(carrey,lit(np),'Jim Carrey').
lex(peter_weir,lit(np),'Peter Weir').
lex(weir,lit(np),'Peter Weir').
lex(truman,lit(np),'Truman Burbank').
lex(ed_harris,lit(np),'Ed Harris').
lex(christof,lit(np),'Christof').
lex(truman_burbank,lit(np),'Truman Burbank').
lex(truman_show,lit(np),'The Truman Show').
lex(the_truman_show,lit(np),'The Truman Show').
lex(ole_bornedal,lit(np),'Ole Bornedal').
lex(martin_bells,lit(np),'Martin Bells').
lex(thomas_cray,lit(np),'Thomas Cray').
lex(nightwatch,lit(np),'Nightwatch').
lex(peter_sellers,lit(np), 'Peter Sellers').
lex(dr_strangelove,lit(np),'Dr. Strangelove').



% ============================================================
% Examples
% ============================================================

sentence_category(lit(wh), wh_question).
sentence_category(lit(q), yes_no_question).
sentence_category(lit(s), assertion).

% = example(String,Formula)

example(" Somebody directed Alien.", s).
example(" A producer directed Alien.", s).
example(" Every actor loves Alien.", s).
example(" Who directed Aliens?", wh).
example(" Who directed Romeo_and_Juliet?",wh).
example(" Who played in Pulp_Fiction?",wh).
example(" Who appeared in Thelma_and_Louise?",wh).
example(" Who played Vito_Corleone?",wh).
example(" Who played Ripley in Alien?",wh).
example(" Did Tarantino direct Titanic?",q).
example(" Did somebody direct Aliens?",q).
example(" Which movies did Tarantino direct?",wh).
example(" How many movies did Tarantino direct?",wh).
example(" Which actor didn't play in Pulp_Fiction?", wh).
example(" Harvey_Keitel played in Pulp_Fiction.",s).
example(" Which character does Harvey_Keitel play in Pulp_Fiction?",wh).
example(" Did Bryan_Singer play in and direct the_Usual_Suspects?",q).
example(" Who didn't Peter_Sellers play in Dr_Strangelove?",wh).
example(" Claire_Danes played Juliet in Romeo_and_Juliet.",s).
example(" Which actors played in Romeo_and_Juliet?",wh).
example(" Who played Romeo in Romeo_and_Juliet?",wh).
example(" Which movies did Scott direct?",wh).

% = special_string

special_string("didn't",didnot).

% ============================================================
% Database
% ============================================================

query_db(oscar, 'Best Movie').
query_db(oscar, 'Best Original Screenplay').
query_db(oscar, 'Best Adapted Screenplay').
query_db(oscar, 'Best Actor').
query_db(oscar, 'Best Actress').
query_db(oscar, 'Best Supporting Actor').
query_db(oscar, 'Best Supporting Actress').

query_db(character, 'Romeo').
query_db(character, 'Jack Dawson').
query_db(character, 'Sean McGuire').
query_db(character, 'Vincent Vega').
query_db(character, 'Mr White').
query_db(character, 'Winston Wolf').
query_db(character, 'Mia').
query_db(character, 'Juliet').
query_db(character, 'Jimmie').
query_db(character, 'Mr Brown').
query_db(character, 'Dallas').
query_db(character, 'Ellen Ripley').
query_db(character, 'Bishop').
query_db(character, 'Deckard').
query_db(character, 'Rachael').
query_db(character, 'Roy Batty').
query_db(character, 'Vito Corleone').
query_db(character, 'Michael Corleone').
query_db(character, 'Tom Hagen').
query_db(character, 'William Somerset').
query_db(character, 'David Mills').
query_db(character, 'Louise').
query_db(character, 'Thelma').
query_db(character, 'Max Cady').
query_db(character, 'Will Hunting').
query_db(character, 'Sam Bowden').
query_db(character, 'Leigh Bowden').
query_db(character, 'Danielle Bowden').
query_db(character, 'John Doe').
query_db(character, 'Verbal Kint').
query_db(character, 'Dean Keaton').
query_db(character, 'John Murdoch').
query_db(character, 'Frank Bumstead').
query_db(character, 'Doctor Schreber').
query_db(character, 'James Cole').
query_db(character, 'Kathryn Railly').
query_db(character, 'Jeffrey Goines').
query_db(character, 'John McClane').
query_db(character, 'Hans Gruber').
query_db(character, 'Hannibal Lecter').
query_db(character, 'Clarice Starling').
query_db(character, 'Castor Troy').
query_db(character, 'Sean Archer').
query_db(character, 'Mark Renton').
query_db(character, 'Francis Begbie').
query_db(character, 'Sidney Prescott').
query_db(character, 'Gale Weathers').
query_db(character, 'Billy Loomis').
query_db(character, 'Ben Sanderson').
query_db(character, 'Sera').
query_db(character, 'Marge Gunderson').
query_db(character, 'Carl Showalter').
query_db(character, 'Ethan Hunt').
query_db(character, 'Jim Phelps').
query_db(character, 'Claire Phelps').
query_db(character, 'Vincent Freeman').
query_db(character, 'Irene Cassini').
query_db(character, 'Indiana Jones').
query_db(character, 'Sallah').
query_db(character, 'Marion Ravenwood').
query_db(character, 'Richard Kimble').
query_db(character, 'Samuel Gerard').
query_db(character, 'Charles Nichols').
query_db(character, 'Kay').
query_db(character, 'Jay').
query_db(character, 'Laurel').
query_db(character, 'Mr Pink').
query_db(character, 'Eliot Ness').
query_db(character, 'Jim Malone').
query_db(character, 'George Stone').
query_db(character, 'Al Capone').
query_db(character, 'Rose DeWit Bukater').
query_db(character, 'Connor MacLeod').
query_db(character, 'Ramirez').
query_db(character, 'Colonel Kurtz').
query_db(character, 'Captain Willard').
query_db(character, 'Lieutenant Kilgore').
query_db(character, 'Gordon Gekko').
query_db(character, 'Bud Fox').
query_db(character, 'Carl Fox').
query_db(character, 'Chris').
query_db(character, 'Big Harold').
query_db(character, 'Sgt. Barnes').
query_db(character, 'Martin Bells').
query_db(character, 'Thomas Cray').

query_db(movie, 'Nightwatch').
query_db(movie, 'Apocalypse Now').
query_db(movie, 'The Untouchables').
query_db(movie, 'Men in Black').
query_db(movie, 'Raiders of the Lost Ark').
query_db(movie, 'Gattaca').
query_db(movie, 'The Fugitive').
query_db(movie, 'Blade Runner').
query_db(movie, 'Alien').
query_db(movie, 'Aliens').
query_db(movie, 'Titanic').
query_db(movie, 'Good Will Hunting').
query_db(movie, 'Pulp Fiction').
query_db(movie, 'Reservoir Dogs').
query_db(movie, 'Romeo and Juliet').
query_db(movie, 'The Godfather').
query_db(movie, 'The Godfather II').
query_db(movie, 'Seven').
query_db(movie, 'Thelma and Louise').
query_db(movie, 'Cape Fear').
query_db(movie, 'The Usual Suspects').
query_db(movie, 'Dark City').
query_db(movie, 'Die Hard').
query_db(movie, 'Silence of the Lambs').
query_db(movie, 'Face/Off').
query_db(movie, 'Trainspotting').
query_db(movie, 'Scream').
query_db(movie, 'Leaving Las Vegas').
query_db(movie, 'Fargo').
query_db(movie, 'Mission Impossible').
query_db(movie, 'Twelve Monkeys').
query_db(movie, 'Highlander').
query_db(movie, 'Wall Street').
query_db(movie, 'Platoon').

query_db(nice,  'Trainspotting').
query_db(nice,  'Andy Garcia').
query_db(nice,  'Jodie Foster').

query_db(actor, 'Andy Garcia').
query_db(actor, 'Sean Connery').
query_db(actor, 'Kevin Costner').
query_db(actor, 'John Rhys-Davies').
query_db(actor, 'Karen Allen').
query_db(actor, 'Tommy Lee Jones').
query_db(actor, 'Jeroen Krabbe').
query_db(actor, 'Will Smith').
query_db(actor, 'Linda Fiorentino').
query_db(actor, 'Ethan Hawke').
query_db(actor, 'Leonardo DiCaprio').
query_db(actor, 'John Travolta').
query_db(actor, 'Robin Williams').
query_db(actor, 'Harvey Keitel').
query_db(actor, 'Quentin Tarantino').
query_db(actor, 'Lance Henriksen').
query_db(actor, 'Tom Skerritt').
query_db(actor, 'Rutger Hauer').
query_db(actor, 'Harrison Ford').
query_db(actor, 'Marlon Brando').
query_db(actor, 'Al Pacino').
query_db(actor, 'Robert Duvall').
query_db(actor, 'Morgan Freeman').
query_db(actor, 'Brad Pitt').
query_db(actor, 'Robert De Niro').
query_db(actor, 'Claire Danes').
query_db(actor, 'Uma Thurman').
query_db(actor, 'Sigourney Weaver').
query_db(actor, 'Sean Young').
query_db(actor, 'Susan Sarandon').
query_db(actor, 'Geena Davis').
query_db(actor, 'Matt Damon').
query_db(actor, 'Nick Nolte').
query_db(actor, 'Jessica Lange').
query_db(actor, 'Juliette Lewis').
query_db(actor, 'Kevin Spacey').
query_db(actor, 'Gabriel Byrne').
query_db(actor, 'Rufus Sewell').
query_db(actor, 'William Hurt').
query_db(actor, 'Kiefer Sutherland').
query_db(actor, 'Bruce Willis').
query_db(actor, 'Alan Rickman').
query_db(actor, 'Anthony Hopkins').
query_db(actor, 'Jodie Foster').
query_db(actor, 'Nicolas Cage').
query_db(actor, 'Ewan McGregor').
query_db(actor, 'Robert Carlyle').
query_db(actor, 'Neve Campbell').
query_db(actor, 'Courteney Cox').
query_db(actor, 'Skeet Ulrich').
query_db(actor, 'Elisabeth Shue').
query_db(actor, 'Frances McDormand').
query_db(actor, 'Steve Buscemi').
query_db(actor, 'Tom Cruise').
query_db(actor, 'Jon Voight').
query_db(actor, 'Emmanuelle Beart').
query_db(actor, 'Madeleine Stowe').
query_db(actor, 'Christopher Lambert').
query_db(actor, 'Martin Sheen').
query_db(actor, 'Charlie Sheen').
query_db(actor, 'Michael Douglas').
query_db(actor, 'Forest Whitaker').
query_db(actor, 'Tom Berenger').

query_db(director, 'Ole Bornedal').
query_db(director, 'Andrew Niccol').
query_db(director, 'Barry Sonnenfeld').
query_db(director, 'Andrew Davis').
query_db(director, 'Steven Spielberg').
query_db(director, 'James Cameron').
query_db(director, 'Ridley Scott').
query_db(director, 'Gus Van Sant').
query_db(director, 'Baz Luhrmann').
query_db(director, 'Francis Ford Coppola').
query_db(director, 'David Fincher').
query_db(director, 'Martin Scorsese').
query_db(director, 'Bryan Singer').
query_db(director, 'Alex Proyas').
query_db(director, 'John McTiernan').
query_db(director, 'Jonathan Demme').
query_db(director, 'John Woo').
query_db(director, 'Danny Boyle').
query_db(director, 'Wes Craven').
query_db(director, 'Mike Figgis').
query_db(director, 'Brian De Palma').
query_db(director, 'Joel Coen').
query_db(director, 'Terry Gilliam').
query_db(director, 'Russell Mulcahy').
query_db(director, 'Oliver Stone').

query_db(release, 'Blade Runner', 1982).
query_db(release, 'Alien', 1979).
query_db(release, 'Aliens', 1986).
query_db(release, 'Titanic', 1997).
query_db(release, 'Good Will Hunting', 1997).
query_db(release, 'Pulp Fiction', 1994).
query_db(release, 'Reservoir Dogs', 1992).
query_db(release, 'Romeo and Juliet', 1996).
query_db(release, 'The Godfather', 1972).
query_db(release, 'The Godfather II', 1974).
query_db(release, 'Seven', 1995).
query_db(release, 'Thelma and Louise', 1991).
query_db(release, 'Cape Fear', 1991).
query_db(release, 'The Usual Suspects', 1995).
query_db(release, 'Dark City', 1998).
query_db(release, 'Die Hard', 1988).
query_db(release, 'Face/Off', 1997).
query_db(release, 'Silence of the Lambs', 1991).
query_db(release, 'Trainspotting', 1996).
query_db(release, 'Scream', 1996).
query_db(release, 'Leaving Las Vegas', 1995).
query_db(release, 'Fargo', 1996).
query_db(release, 'Mission Impossible', 1996).
query_db(release, 'Twelve Monkeys', 1995).
query_db(release, 'Gattaca', 1997).
query_db(release, 'Raiders of the Lost Ark', 1981).
query_db(release, 'The Fugitive', 1993).
query_db(release, 'Men in Black', 1997).
query_db(release, 'The Untouchables', 1987).
query_db(release, 'Highlander', 1986).
query_db(release, 'Apocalypse Now', 1979).
query_db(release, 'Wall Street', 1987).
query_db(release, 'Platoon', 1986).
query_db(release, 'The Truman Show', 1998).
query_db(release, 'Nightwatch', 1998).

query_db(direct, 'Brian De Palma', 'The Untouchables').
query_db(direct, 'James Cameron', 'Titanic').
query_db(direct, 'James Cameron', 'Aliens').
query_db(direct, 'Ridley Scott', 'Alien').
query_db(direct, 'Ridley Scott', 'Blade Runner').
query_db(direct, 'Ridley Scott', 'Thelma and Louise').
query_db(direct, 'Gus Van Sant', 'Good Will Hunting').
query_db(direct, 'Quentin Tarantino', 'Pulp Fiction').
query_db(direct, 'Quentin Tarantino', 'Reservoir Dogs').
query_db(direct, 'Baz Luhrmann', 'Romeo and Juliet').
query_db(direct, 'Francis Ford Coppola', 'The Godfather').
query_db(direct, 'Francis Ford Coppola', 'The Godfather II').
query_db(direct, 'David Fincher', 'Seven').
query_db(direct, 'Martin Scorsese', 'Cape Fear').
query_db(direct, 'Bryan Singer', 'The Usual Suspects').
query_db(direct, 'Alex Proyas', 'Dark City').
query_db(direct, 'John McTiernan', 'Die Hard').
query_db(direct, 'Jonathan Demme', 'Silence of the Lambs').
query_db(direct, 'John Woo', 'Face/Off').
query_db(direct, 'Danny Boyle', 'Trainspotting').
query_db(direct, 'Wes Craven', 'Scream').
query_db(direct, 'Mike Figgis', 'Leaving Las Vegas').
query_db(direct, 'Joel Coen', 'Fargo').
query_db(direct, 'Brian De Palma', 'Mission Impossible').
query_db(direct, 'Terry Gilliam', 'Twelve Monkeys').
query_db(direct, 'Andrew Niccol', 'Gattaca').
query_db(direct, 'Steven Spielberg', 'Raiders of the Lost Ark').
query_db(direct, 'Andrew Davis', 'The Fugitive').
query_db(direct, 'Barry Sonnenfeld', 'Men in Black').
query_db(direct, 'Russell Mulcahy', 'Highlander').
query_db(direct, 'Francis Ford Coppola', 'Apocalypse Now').
query_db(direct, 'Oliver Stone', 'Wall Street').
query_db(direct, 'Oliver Stone', 'Platoon').
query_db(direct, 'Ole Bornedal', 'Nightwatch').

query_db(play, 'Leonardo DiCaprio', 'Romeo and Juliet', 'Romeo').
query_db(play, 'Leonardo DiCaprio', 'Titanic', 'Jack Dawson').
query_db(play, 'Robin Williams', 'Good Will Hunting', 'Sean McGuire').
query_db(play, 'John Travolta', 'Pulp Fiction', 'Vincent Vega').
query_db(play, 'Harvey Keitel', 'Reservoir Dogs', 'Mr White').
query_db(play, 'Harvey Keitel', 'Pulp Fiction', 'Winston Wolf').
query_db(play, 'Uma Thurman', 'Pulp Fiction', 'Mia').
query_db(play, 'Quentin Tarantino', 'Pulp Fiction', 'Jimmie').
query_db(play, 'Quentin Tarantino', 'Reservoir Dogs', 'Mr Brown').
query_db(play, 'Sigourney Weaver', 'Alien', 'Ellen Ripley').
query_db(play, 'Sigourney Weaver', 'Aliens', 'Ellen Ripley').
query_db(play, 'Lance Henriksen', 'Aliens', 'Bishop').
query_db(play, 'Tom Skerritt', 'Alien', 'Dallas').
query_db(play, 'Harrison Ford', 'Blade Runner', 'Deckard').
query_db(play, 'Rutger Hauer', 'Blade Runner', 'Roy Batty').
query_db(play, 'Sean Young', 'Blade Runner', 'Rachael').
query_db(play, 'Marlon Brando', 'The Godfather', 'Vito Corleone').
query_db(play, 'Al Pacino', 'The Godfather', 'Michael Corleone').
query_db(play, 'Robert Duvall', 'The Godfather', 'Tom Hagen').
query_db(play, 'Morgan Freeman', 'Seven', 'William Somerset').
query_db(play, 'Brad Pitt', 'Seven', 'David Mills').
query_db(play, 'Robert De Niro', 'The Godfather II', 'Vito Corleone').
query_db(play, 'Al Pacino', 'The Godfather II', 'Michael Corleone').
query_db(play, 'Robert Duvall', 'The Godfather II', 'Tom Hagen').
query_db(play, 'Susan Sarandon', 'Thelma and Louise', 'Louise').
query_db(play, 'Geena Davis', 'Thelma and Louise', 'Thelma').
query_db(play, 'Robert De Niro', 'Cape Fear', 'Max Cady').
query_db(play, 'Matt Damon', 'Good Will Hunting', 'Will Hunting').
query_db(play, 'Nick Nolte', 'Cape Fear', 'Sam Bowden').
query_db(play, 'Jessica Lange', 'Cape Fear', 'Leigh Bowden').
query_db(play, 'Juliette Lewis', 'Cape Fear', 'Danielle Bowden').
query_db(play, 'Kevin Spacey', 'Seven', 'John Doe').
query_db(play, 'Kevin Spacey', 'The Usual Suspects', 'Verbal Kint').
query_db(play, 'Gabriel Byrne', 'The Usual Suspects', 'Dean Keaton').
query_db(play, 'Rufus Sewell', 'Dark City', 'John Murdoch').
query_db(play, 'William Hurt', 'Dark City', 'Frank Bumstead').
query_db(play, 'Kiefer Sutherland', 'Dark City', 'Doctor Schreber').
query_db(play, 'Bruce Willis', 'Die Hard', 'John McClane').
query_db(play, 'Alan Rickman', 'Die Hard', 'Hans Gruber').
query_db(play, 'Anthony Hopkins', 'Silence of the Lambs', 'Hannibal Lecter').
query_db(play, 'Jodie Foster', 'Silence of the Lambs', 'Clarice Starling').
query_db(play, 'Nicolas Cage', 'Face/Off', 'Castor Troy').
query_db(play, 'John Travolta', 'Face/Off', 'Sean Archer').
query_db(play, 'Ewan McGregor', 'Trainspotting', 'Mark Renton').
query_db(play, 'Robert Carlyle', 'Trainspotting', 'Francis Begbie').
query_db(play, 'Neve Campbell', 'Scream', 'Sidney Prescott').
query_db(play, 'Courteney Cox', 'Scream', 'Gale Weathers').
query_db(play, 'Skeet Ulrich', 'Scream', 'Billy Loomis').
query_db(play, 'Nicolas Cage', 'Leaving Las Vegas', 'Ben Sanderson').
query_db(play, 'Elisabeth Shue', 'Leaving Las Vegas', 'Sera').
query_db(play, 'Frances McDormand', 'Fargo', 'Marge Gunderson').
query_db(play, 'Steve Buscemi', 'Fargo', 'Carl Showalter').
query_db(play, 'Tom Cruise', 'Mission Impossible', 'Ethan Hunt').
query_db(play, 'Jon Voight', 'Mission Impossible', 'Jim Phelps').
query_db(play, 'Emmanuelle Beart', 'Mission Impossible', 'Claire Phelps').
query_db(play, 'Bruce Willis', 'Twelve Monkeys', 'James Cole').
query_db(play, 'Madeleine Stowe', 'Twelve Monkeys', 'Kathryn Railly').
query_db(play, 'Brad Pitt', 'Twelve Monkeys', 'Jeffrey Goines').
query_db(play, 'Ethan Hawke', 'Gattaca', 'Vincent Freeman').
query_db(play, 'Uma Thurman', 'Gattaca', 'Irene Cassini').
query_db(play, 'Steve Buscemi', 'Reservoir Dogs', 'Mr Pink').
query_db(play, 'Harrison Ford', 'Raiders of the Lost Ark', 'Indiana Jones').
query_db(play, 'John Rhys-Davies', 'Raiders of the Lost Ark', 'Sallah').
query_db(play, 'Karen Allen', 'Raiders of the Lost Ark', 'Marion Ravenwood').
query_db(play, 'Harrison Ford', 'The Fugitive', 'Richard Kimble').
query_db(play, 'Tommy Lee Jones', 'The Fugitive', 'Samuel Gerard').
query_db(play, 'Jeroen Krabbe', 'The Fugitive', 'Charles Nichols').
query_db(play, 'Tommy Lee Jones', 'Men in Black', 'Kay').
query_db(play, 'Will Smith', 'Men in Black', 'Jay').
query_db(play, 'Linda Fiorentino', 'Men in Black', 'Laurel').
query_db(play, 'Kevin Costner', 'The Untouchables', 'Eliot Ness').
query_db(play, 'Sean Connery', 'The Untouchables', 'Jim Malone').
query_db(play, 'Andy Garcia', 'The Untouchables', 'George Stone').
query_db(play, 'Robert De Niro', 'The Untouchables', 'Al Capone').
query_db(play, 'Kate Winslet', 'Titanic', 'Rose DeWit Bukater').
query_db(play, 'Christopher Lambert','Highlander', 'Connor MacLeod').
query_db(play, 'Sean Connery', 'Highlander', 'Ramirez').
query_db(play, 'Marlon Brando', 'Apocalypse Now', 'Colonel Kurtz').
query_db(play, 'Robert Duvall', 'Apocalypse Now', 'Lieutenant Kilgore').
query_db(play, 'Martin Sheen', 'Apocalypse Now', 'Captain Willard').
query_db(play, 'Michael Douglas', 'Wall Street', 'Gordon Gekko').
query_db(play, 'Charlie Sheen', 'Wall Street', 'Bud Fox').
query_db(play, 'Martin Sheen', 'Wall Street', 'Carl Fox').
query_db(play, 'Charlie Sheen', 'Platoon', 'Chris').
query_db(play, 'Forest Whitaker', 'Platoon', 'Big Harold').
query_db(play, 'Tom Berenger', 'Platoon', 'Sgt. Barnes').
query_db(play, 'Ewan McGregor', 'Nightwatch', 'Martin Bells').
query_db(play, 'Nick Nolte', 'Nightwatch', 'Thomas Cray').
