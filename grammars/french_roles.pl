
% = roles

get_roles1(aller, [np], [patient]).  % unaccusative
get_roles1(arriver, [np], [patient]).  % unaccusative
get_roles1(cailler, [np], [patient]).  % unaccusative
get_roles1(cicatriser, [np], [patient]). % unaccusative
get_roles1(coaguler, [np], [patient]). % unaccusative
get_roles1(cristalliser, [np], [patient]). % unaccusative
get_roles1(convenir, [np], [theme]). % unaccusative
get_roles1(descendre, [np], [patient]). % unaccusative
get_roles1(entrer, [np], [patient]). % unaccusative
get_roles1(monter, [np], [patient]). % unaccusative
get_roles1(mourir, [np], [patient]). % unaccusative
get_roles1(naître, [np], [patient]). % unaccusative
get_roles1(partir, [np], [patient]). % unaccusative
get_roles1(passer, [np], [patient]). % unaccusative
get_roles1(rester, [np], [patient]). % unaccusative
get_roles1(rentrer, [np], [patient]). % unaccusative
get_roles1(retourner, [np], [patient]). % unaccusative
get_roles1(sortir, [np], [patient]). % unaccusative
get_roles1(tomber, [np], [patient]).  % unaccusative
get_roles1(venir, [np], [patient]).  % unaccusative

get_roles1(arriver, [np, pp(à)], [patient, destination]). % unaccusative
get_roles1(convenir, [np, pp(à)], [theme, patient]). % unaccusative
get_roles1(dégorger, [np, pp(dans)], [patient, destination]). % unaccusative
get_roles1(partir, [np, pp(à)], [patient, destination]). % unaccusative
get_roles1(partir, [np, pp(pour)], [patient, destination]). % unaccusative
get_roles1(partir, [np, pp(vers)], [patient, destination]). % unaccusative
get_roles1(partir, [np, pp(de)], [patient, source]).
get_roles1(plaire, [np, pp(à)], [theme, patient]). % unaccusative


get_roles1(chiffrer, [np, np, pp(à)], [agent, patient, mésure]).
get_roles1(demander, [np, np, pp(à)], [agent, theme, source]).
get_roles1(donner, [np, np, pp(à)], [agent, theme, destination]).
get_roles1(mettre, [np, np, pp(_)], [agent, theme, destination]).
get_roles1(offrir, [np, np, pp(à)], [agent, theme, destination]).
get_roles1(prendre, [np, np, pp(de)], [agent, theme, source]).

get_roles1(_, [np, adj], [agent, agent_attr]).

get_roles1(_, [np, np, adj], [agent, patient, patient_attr]).

% reflexive objects

get_roles1(abaisser, [np, cl_r], [agent, null]).
get_roles1(abandonner, [np, cl_r], [agent, null]).
get_roles1(abattre, [np, cl_r], [patient, null]). % unaccusative
get_roles1(abîmer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(abreuver, [np, cl_r], [agent, null]).
get_roles1(abriter, [np, cl_r], [agent, null]).
get_roles1(abrutir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(absenter, [np, cl_r], [agent, null]).
get_roles1(abstraire, [np, cl_r], [agent, null]).
get_roles1(abuser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(accélérer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(accentuer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(acclimater, [np, cl_r], [agent, null]).
get_roles1(accoler, [np, cl_r], [agent, null]).
get_roles1(accoler, [np, cl_r], [agent, null]).
get_roles1(accommoder, [np, cl_r], [agent, null]).
get_roles1(accomplir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(accouder, [np, cl_r], [agent, null]).
get_roles1(accoupler, [np, cl_r], [agent, null]).
get_roles1(accoutumer, [np, cl_r], [agent, null]).
get_roles1(accréditer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(accrocher, [np, cl_r], [agent, null]). 
get_roles1(accroupir, [np, cl_r], [agent, null]). 
get_roles1(accumuler, [np, cl_r], [patient, null]). 
get_roles1(accuser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(acharner, [np, cl_r], [agent, null]).
get_roles1(achever, [np, cl_r], [patient, null]). % unaccusative
get_roles1(activer, [np, cl_r], [agent, null]).
get_roles1(adapter, [np, cl_r], [agent, null]).
get_roles1(adoucir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(aérer, [np, cl_r], [agent, null]).
get_roles1(affaisser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(affaler, [np, cl_r], [agent, null]). % (not sure if this might be unaccusative)
get_roles1(affairer, [np, cl_r], [agent, null]). 
get_roles1(affaiblir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(affermir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(affiner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(affoler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(affronter, [np, cl_r], [agent, null]). 
get_roles1(agacer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agencer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agenouiller, [np, cl_r], [agent, null]). 
get_roles1(agglomérer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agglutiner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(aggraver, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agiter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agrandir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agréger, [np, cl_r], [patient, null]). % unaccusative
get_roles1(agripper, [np, cl_r], [agent, null]). 
get_roles1(aigrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(alarmer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(aligner, [np, cl_r], [agent, null]). 
get_roles1(alléger, [np, cl_r], [patient, null]). % unaccusative
get_roles1(allier, [np, cl_r], [agent, null]). 
get_roles1(allonger, [np, cl_r], [patient, null]). % unaccusative
get_roles1(allumer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(alourdir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(altérer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amadouer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amaigrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amasser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(améliorer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amender, [np, cl_r], [agent, null]).
get_roles1(amener, [np, cl_r], [agent, null]).
get_roles1(ameuter, [np, cl_r], [patient, null]). % unaccusative (?)
get_roles1(amincir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amoindrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amollir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amonceler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amortir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amouracher, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amplifier, [np, cl_r], [patient, null]). % unaccusative
get_roles1(amuser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(ancrer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(anéantir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(angoisser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(animer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(annihiler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(annoncer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(apaiser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(aplanir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(aplatir, [np, cl_r], [agent, null]).
get_roles1(appareiller, [np, cl_r], [agent, null]).
get_roles1(apparenter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(appauvrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(appliquer, [np, cl_r], [agent, null]).
get_roles1(apprêter, [np, cl_r], [agent, null]).
get_roles1(apprivoiser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(approcher, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(approfondir, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(arc-bouter, [np, cl_r], [agent, null]).
get_roles1(armer, [np, cl_r], [agent, null]).
get_roles1(arracher, [np, cl_r], [agent, null]).
get_roles1(arranger, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(arrêter, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(arrondir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(asphyxier, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assagir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(arracher, [np, cl_r], [agent, null]).
get_roles1(assembler, [np, cl_r], [agent, null]).
get_roles1(asseoir, [np, cl_r], [agent, null]).
get_roles1(assimiler, [np, cl_r], [agent, null]).
get_roles1(associer, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(assombrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assortir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assoupir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assouplir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assourdir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(assujettir, [np, cl_r], [agent, null]).
get_roles1(assurer, [np, cl_r], [agent, null]).
get_roles1(atrophier, [np, cl_r], [patient, null]). % unaccusative
get_roles1(attabler, [np, cl_r], [agent, null]).
get_roles1(attarder, [np, cl_r], [agent, null]). % is this an unaccusative ?
get_roles1(attendrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(atténuer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(attiédir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(attrouper, [np, cl_r], [agent, null]).
get_roles1(avachir, [np, cl_r], [patient, null]).
get_roles1(avancer, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(avérer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(avilir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(bagarrer, [np, cl_r], [agent, null]).
get_roles1(baigner, [np, cl_r], [agent, null]).
get_roles1(baisser, [np, cl_r], [agent, null]).
get_roles1(balader, [np, cl_r], [agent, null]).
get_roles1(balancer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(barrer, [np, cl_r], [agent, null]).
get_roles1(barricader, [np, cl_r], [agent, null]).
get_roles1(batailler, [np, cl_r], [agent, null]).
get_roles1(battre, [np, cl_r], [agent, null]).
get_roles1(beurrer, [np, cl_r], [agent, null]).
get_roles1(blesser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(blinder, [np, cl_r], [agent, null]).
get_roles1(blottir, [np, cl_r], [agent, null]).
get_roles1(boucher, [np, cl_r], [patient, null]). % unaccusative
get_roles1(bouger, [np, cl_r], [agent, null]).
get_roles1(bousculer, [np, cl_r], [agent, null]).
get_roles1(brancher, [np, cl_r], [agent, null]).
get_roles1(briser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(brouiller, [np, cl_r], [patient, null]). % unaccusative
get_roles1(bûcher, [np, cl_r], [agent, null]).
get_roles1(buter, [np, cl_r], [patient, null]). % unaccusative (unergative possible)
get_roles1(cabrer, [np, cl_r], [agent, null]).
get_roles1(cacher, [np, cl_r], [agent, null]).
get_roles1(cailler, [np, cl_r], [patient, null]).  % unaccusative
get_roles1(caler, [np, cl_r], [agent, null]).
get_roles1(calmer, [np, cl_r], [patient, null]).  % unaccusative
get_roles1(cambrer, [np, cl_r], [agent, null]).
get_roles1(capitonner, [np, cl_r], [agent, null]).
get_roles1(caser, [np, cl_r], [patient, null]). % ? unaccusative
get_roles1(casser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(chamailler, [np, cl_r], [agent, null]).
get_roles1(changer, [np, cl_r], [agent, null]).
get_roles1(chauffer, [np, cl_r], [agent, null]).
get_roles1(chevaucher, [np, cl_r], [patient, null]). % unaccusative
get_roles1(chiffonner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(chipoter, [np, cl_r], [agent, null]). % ?
get_roles1(cicatriser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(civiliser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(claquer, [np, cl_r], [agent, null]).
get_roles1(coaguler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(coaliser, [np, cl_r], [agent, null]).
get_roles1(coiffer, [np, cl_r], [agent, null]).
get_roles1(coincer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(colorer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(commettre, [np, cl_r], [agent, null]).
get_roles1(communiquer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(compliquer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(comporter, [np, cl_r], [agent, null]).
get_roles1(compromettre, [np, cl_r], [agent, null]).
get_roles1(concentrer, [np, cl_r], [agent, null]).
get_roles1(concerter, [np, cl_r], [agent, null]).
get_roles1(concrétiser, [np, cl_r], [agent, null]).
get_roles1(condenser, [np, cl_r], [agent, null]).
get_roles1(conduire, [np, cl_r], [agent, null]).
get_roles1(confédérer, [np, cl_r], [agent, null]).
get_roles1(confesser, [np, cl_r], [agent, null]).
get_roles1(confirmer, [np, cl_r], [agent, null]).
get_roles1(congeler, [np, cl_r], [agent, null]).
get_roles1(congestionner, [np, cl_r], [agent, null]).
get_roles1(consacrer, [np, cl_r], [agent, null]).
get_roles1(consolider, [np, cl_r], [patient, null]). % ? unaccusative
get_roles1(consumer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(contenir, [np, cl_r], [agent, null]).
get_roles1(contracter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(contrarier, [np, cl_r], [patient, null]). % ? unaccusative
get_roles1(convulser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(corner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(corrompre, [np, cl_r], [patient, null]). % unaccusative
get_roles1(corser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(cotiser, [np, cl_r], [agent, null]).
get_roles1(coucher, [np, cl_r], [agent, null]).
% Commented out the following line since a "true reflexive" use
% (eg. "Jean se coupe") seems more likely than the unaccusative
% use "le tissu se coupe" in the sense of "wear out")
% get_roles1(couper, [np, cl_r], [patient, null]). % unaccusative
get_roles1(courber, [np, cl_r], [agent, null]).
get_roles1(couronner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(courroucer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(couvrir, [np, cl_r], [patient, null]). % unaccusative, unergative possible
get_roles1(cramponner, [np, cl_r], [agent, null]).
get_roles1(craqueler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(creuser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(crevasser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(crever, [np, cl_r], [agent, null]).
get_roles1(crisper, [np, cl_r], [agent, null]). % unaccusative
get_roles1(cristalliser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(damner, [np, cl_r], [agent, null]).
get_roles1(débander, [np, cl_r], [patient, null]). % unaccusative
get_roles1(débattre, [np, cl_r], [agent, null]).
get_roles1(débaucher, [np, cl_r], [agent, null]).
get_roles1(débiner, [np, cl_r], [agent, null]).
get_roles1(débloquer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déboiser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déboîter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déborder, [np, cl_r], [agent, null]).
get_roles1(déboucher, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déboutonner, [np, cl_r], [agent, null]).
get_roles1(débrancher, [np, cl_r], [patient, null]).
get_roles1(débrouiller, [np, cl_r], [agent, null]).
get_roles1(décanter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décarcasser, [np, cl_r], [agent, null]).
get_roles1(déchaîner, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déchausser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déchirer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déclarer, [np, cl_r], [patient, null]). % unaccusative (with optional temporal adverb)
get_roles1(déclasser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déclencher, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décoller, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décolorer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décommander, [np, cl_r], [agent, null]).
get_roles1(décomposer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déconsidérer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décontenancer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décontracter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(découdre, [np, cl_r], [patient, null]). % unaccusative
get_roles1(découper, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décourager, [np, cl_r], [patient, null]). % unaccusative
get_roles1(découvrir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(décrasser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déculotter, [np, cl_r], [agent, null]).
get_roles1(dédire, [np, cl_r], [agent, null]).
get_roles1(défaire, [np, cl_r], [patient, null]). % unaccusative
get_roles1(défiler, [np, cl_r], [agent, null]).
get_roles1(défoncer, [np, cl_r], [agent, null]).
get_roles1(déformer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(défouler, [np, cl_r], [agent, null]).
get_roles1(défroisser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(défroquer, [np, cl_r], [agent, null]).
get_roles1(dégager, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégarnir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégeler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déglinguer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégonfler, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégrader, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégriser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(dégrossir, [np, cl_r], [patient, null]). % unaccusative
get_roles1(déhancher, [np, cl_r], [agent, null]).
get_roles1(déjuger, [np, cl_r], [agent, null]).
get_roles1(délabrer, [np, cl_r], [patient, null]). % unaccusative
get_roles1(délasser, [np, cl_r], [patient, null]). % unaccusative
get_roles1(délecter, [np, cl_r], [patient, null]). % unaccusative
get_roles1(délier, [np, cl_r], [patient, null]). % unaccusative

get_roles1(paumer, [np, cl_r], [agent, null]).
get_roles1(pavaner, [np, cl_r], [agent, null]).
get_roles1(pencher, [np, cl_r], [agent, null]).
get_roles1(perdre, [np, cl_r], [agent, null]).
get_roles1(perfectionner, [np, cl_r], [agent, null]).
get_roles1(perpétuer, [np, cl_r], [agent, null]).
get_roles1(pétrifier, [np, cl_r], [agent, null]).
get_roles1(pincer, [np, cl_r], [agent, null]).
get_roles1(pinter, [np, cl_r], [agent, null]).
get_roles1(piquer, [np, cl_r], [agent, null]).
get_roles1(plaindre, [np, cl_r], [agent, null]).
get_roles1(planquer, [np, cl_r], [agent, null]).

get_roles1(tromper, [np, cl_r], [patient, null]).

% reflexive objects and prepositions

get_roles1(abaisser, [np, cl_r, pp(devant)], [agent, null, theme]).
get_roles1(abaisser, [np, cl_r, pp(vers)], [agent, null, destination]). % direction
get_roles1(abaisser, [np, cl_r, pp(a)], [agent, null, theme]).
get_roles1(abaisser, [np, cl_r, a_inf], [agent, null, theme]).
get_roles1(abandonner, [np, cl_r, pp(a)], [agent, null, theme]).
get_roles1(abattre, [np, cl_r, pp(sur)], [patient, null, destination]). % unaccusative
get_roles1(abreuver, [np, cl_r, pp(_)], [agent, null, source]). % static
get_roles1(abriter, [np, cl_r, pp(derrière)], [patient, null, lieu]). % static
get_roles1(abriter, [np, cl_r, pp(sur)], [patient, null, lieu]). % static
get_roles1(absenter, [np, cl_r, pp(de)], [agent, null, lieu]). % static place
get_roles1(absorber, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(abstenir, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(abstenir, [np, cl_r, inf(de)], [agent, null, theme]).
get_roles1(abstraire, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(abuser, [np, cl_r, pp(sur)], [patient, null, theme]). % unaccusative
get_roles1(acclimater, [np, cl_r, pp(a)], [agent, null, theme]).
get_roles1(accoler, [np, cl_r, pp(a)], [agent, null, theme]).
get_roles1(accommoder, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(accommoder, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(accompagner, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(accorder, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(accorder, [np, cl_r, pp(pour)], [agent, null, but]).
get_roles1(accorder, [np, cl_r, pp(en)], [agent, null, theme]).
get_roles1(accorder, [np, cl_r, pp(avec), pp(pour)], [agent, null, patient, but]).
get_roles1(accorder, [np, cl_r, pp(avec), pp(en)], [agent, null, patient, theme]).
get_roles1(accouder, [np, cl_r, pp(_)], [agent, null, instrument]). % static place
get_roles1(accoupler, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(accoutumer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(accrocher, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(accrocher, [np, cl_r, pp(_)], [agent, null, instrument]). % static place
get_roles1(acharner, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(acharner, [np, cl_r, pp(contre)], [agent, null, patient]).
get_roles1(acharner, [np, cl_r, pp(après)], [agent, null, patient]).
get_roles1(acharner, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(acheminer, [np, cl_r, pp(vers)], [agent, null, destination]).
get_roles1(acheminer, [np, cl_r, pp(à)], [agent, null, destination]).
get_roles1(achever, [np, cl_r, pp(sur)], [patient, null, theme]). % unaccusative
get_roles1(achopper, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(acquitter, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(activer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(activer, [np, cl_r, pp(avec)], [agent, null, theme]).
get_roles1(activer, [np, cl_r, pp(_)], [agent, null, lieu]). % static place
get_roles1(adapter, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(additionner, [np, cl_r, pp(à)], [patient, null, theme]).
get_roles1(adonner, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(adosser, [np, cl_r, pp(contre)], [agent, null, endroit]).
get_roles1(adosser, [np, cl_r, pp(à)], [agent, null, endroit]).
get_roles1(adresser, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(affaisser, [np, cl_r, pp(_)], [patient, null, destination]). % unaccusative
get_roles1(affaler, [np, cl_r, pp(_)], [agent, null, destination]). % (not sure if this might be unaccusative)
get_roles1(affairer, [np, cl_r, inf(à)], [agent, null, theme]). 
get_roles1(affairer, [np, cl_r, pp(à)], [agent, null, theme]). 
get_roles1(affairer, [np, cl_r, pp(auprès_de)], [agent, null, theme]). 
get_roles1(affairer, [np, cl_r, pp(_)], [agent, null, lieu]). 
get_roles1(affecter, [np, cl_r, pp(de)], [agent, null, theme]). 
get_roles1(affecter, [np, cl_r, inf(de)], [agent, null, theme]). 
get_roles1(afficher, [np, cl_r, pp(avec)], [agent, null, theme]). 
get_roles1(affilier, [np, cl_r, pp(à)], [agent, null, theme]). 
get_roles1(affliger, [np, cl_r, pp(de)], [agent, null, theme]). 
get_roles1(affoler, [np, cl_r, pp(de)], [agent, null, theme]). 
get_roles1(affronter, [np, cl_r, pp(à)], [agent, null, theme]). 
get_roles1(affronter, [np, cl_r, pp(avec)], [agent, null, patient]). 
get_roles1(agacer, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(agacer, [np, cl_r, inf(de)], [patient, null, theme]). % unaccusative
get_roles1(agencer, [np, cl_r, pp(avec)], [patient, null, theme]). % unaccusative
get_roles1(agenouiller, [np, cl_r, pp(devant)], [agent, null, lieu]). 
get_roles1(agglutiner, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(agir, [np, cl_r, pp(de)], [null, null, theme]). % impersonal verb !
get_roles1(agir, [np, cl_r, inf(de), pp(pour)], [null, null, theme, but]). % impersonal verb !
get_roles1(agir, [np, cl_r, pp(de), pp(pour)], [null, null, theme, but]). % impersonal verb !
get_roles1(agir, [np, cl_r, pp(de), pp(dans)], [null, null, theme, lieu]). % impersonal verb !
get_roles1(agrandir, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(agripper, [np, cl_r, pp(a)], [agent, null, destination]). 
get_roles1(aider, [np, cl_r, pp(de)], [agent, null, patient]). 
get_roles1(ajouter, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(ajuster, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(alarmer, [np, cl_r, pp(de)], [patient, null, theme]).
get_roles1(aligner, [np, cl_r, pp(sur)], [agent, null, theme]). 
get_roles1(alimenter, [np, cl_r, pp(en)], [patient, null, theme]). % unaccusative
get_roles1(alléger, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(allier, [np, cl_r, pp(contre)], [agent, null, theme]). 
get_roles1(allier, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(allier, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(allier, [np, cl_r, pp(avec), pp(contre)], [agent, null, patient]). 
get_roles1(allonger, [np, cl_r, pp(de)], [patient, null, mésure]). % unaccusative
get_roles1(amaigrir, [np, cl_r, pp(de)], [patient, null, mésure]). % unaccusative
get_roles1(amuser, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(amuser, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(amuser, [np, cl_r, inf(à)], [patient, null, theme]). % unaccusative
get_roles1(ancrer, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(angoisser, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(angoisser, [np, cl_r, inf(de)], [patient, null, theme]). % unaccusative
get_roles1(apercevoir, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(apitoyer, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(aplatir, [np, cl_r, pp(devant)], [agent, null, lieu]).
get_roles1(aplatir, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(appareiller, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(apparenter, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(appesantir, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(applaudir, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(appliquer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(apprêter, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(apprivoiser, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(approcher, [np, cl_r, pp(de)], [agent, null, destination]). % unaccusative
get_roles1(appuyer, [np, cl_r, pp(à)], [agent, null, lieu]).
get_roles1(appuyer, [np, cl_r, pp(sur)], [agent, null, lieu]).
get_roles1(arc-bouter, [np, cl_r, pp(à)], [agent, null, lieu]).
get_roles1(arc-bouter, [np, cl_r, pp(sur)], [agent, null, lieu]).
get_roles1(arc-bouter, [np, cl_r, pp(contre)], [agent, null, lieu]).
get_roles1(armer, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(armer, [np, cl_r, pp(de)], [agent, null, patient]).
get_roles1(armer, [np, cl_r, pp(de), pp(contre)], [agent, null, patient, theme]).
get_roles1(arracher, [np, cl_r, pp(de)], [agent, null, source]).
get_roles1(arracher, [np, cl_r, pp(à)], [agent, null, source]).
get_roles1(arranger, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(arranger, [np, cl_r, pp(pour)], [agent, null, but]).
get_roles1(arranger, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(arranger, [np, cl_r, pp(avec), pp(but)], [agent, null, patient, but]).
get_roles1(arrêter, [np, cl_r, pp(à)], [patient, null, destination]). % unaccusative, unergative is possible, I think ?
get_roles1(arrêter, [np, cl_r, pp(sur)], [patient, null, destination]). % unaccusative, unergative is possible, I think ?
get_roles1(articuler, [np, cl_r, pp(avec)], [patient, null, theme]).
get_roles1(articuler, [np, cl_r, pp(sur)], [patient, null, theme]).
get_roles1(articuler, [np, cl_r, pp(_)], [patient, null, destination]).
get_roles1(assembler, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(asseoir, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(assimiler, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(associer, [np, cl_r, pp(avec)], [patient, null, theme]). % unaccusative, unergative is possible
get_roles1(associer, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative, unergative is possible
get_roles1(assortir, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(assortir, [np, cl_r, pp(avec)], [patient, null, theme]). % unaccusative
get_roles1(assouvir, [np, cl_r, pp(de)], [agent, null, patient]).
get_roles1(assujettir, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(assurer, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(attacher, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(attacher, [np, cl_r, pp(_)], [patient, null, lieu]).
get_roles1(attaquer, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(attarder, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(attarder, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(attarder, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(atteler, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(atteler, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(atteler, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(atteler, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(attendrir, [np, cl_r, pp(sur)], [patient, null, theme]). % unaccusative
get_roles1(attrister, [np, cl_r, pp(sur)], [patient, null, theme]). % unaccusative
get_roles1(attrister, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(attrister, [np, cl_r, inf(de)], [patient, null, theme]). % unaccusative
get_roles1(autoriser, [np, cl_r, pp(de)], [agent, null, theme]). % ? deinf
get_roles1(avachir, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(avachir, [np, cl_r, inf(à)], [patient, null, theme]). % unaccusative
get_roles1(avachir, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(avancer, [np, cl_r, pp(vers)], [patient, null, destination]). % unaccusative (unergative possible)
get_roles1(avancer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(avancer, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(aventurer, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(aventurer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(aventurer, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(aveugler, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(aviser, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(aviser, [np, cl_r, inf(de)], [patient, null, theme]). % unaccusative
get_roles1(bagarrer, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(baigner, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(balancer, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(barrer, [np, cl_r, pp(de)], [agent, null, source]).
get_roles1(barricader, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(base, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(batailler, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(battre, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(blesser, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(blinder, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(blottir, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(borner, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(branler, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative ?
get_roles1(braquer, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(brouiller, [np, cl_r, pp(avec)], [patient, null, theme]). % unaccusative ?
get_roles1(buter, [np, cl_r, pp(à)], [patient, null, theme]).
get_roles1(buter, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(cabrer, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(cacher, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(cacher, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(caler, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(calfeutrer, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(camper, [np, cl_r, pp(devant)], [agent, null, lieu]).
get_roles1(cantonner, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(captiver, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative ?
get_roles1(captiver, [np, cl_r, pp(pour)], [patient, null, theme]). % unaccusative ?
get_roles1(carrer, [np, cl_r, pp(_)], [agent, null, lieu]).
get_roles1(chagriner, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative ?
get_roles1(chamailler, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(charger, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(charger, [np, cl_r, inf(de)], [agent, null, theme]).
get_roles1(chevaucher, [np, cl_r, pp(avec)], [patient, null, theme]). % unaccusative
get_roles1(chiffrer, [np, cl_r, pp(à)], [patient, null, mésure]). % unaccusative
get_roles1(circonscrire, [np, cl_r, pp(autour_de)], [patient, null, theme]). % unaccusative
get_roles1(classer, [np, cl_r, pp(_)], [patient, null, theme]).
get_roles1(coaliser, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(coaliser, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(coaliser, [np, cl_r, pp(avec), pp(contre)], [agent, null, patient, theme]).
get_roles1(coller, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(commettre, [np, cl_r, pp(avec)], [agent, null, theme]).
get_roles1(complaire, [np, cl_r, pp(dans)], [patient, null, theme]). % unaccusative
get_roles1(complaire, [np, cl_r, pp(à)], [patient, null, theme]). % unaccusative
get_roles1(complaire, [np, cl_r, inf(à)], [patient, null, theme]). % unaccusative
get_roles1(compliquer, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(comporter, [np, cl_r, adv], [agent, null, theme]). % adv = dl(1,s,s)
get_roles1(composer, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(compromettre, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(compromettre, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(compromettre, [np, cl_r, pp(avec), pp(dans)], [agent, null, patient, theme]).
get_roles1(concentrer, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(concrétiser, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(conduire, [np, cl_r, adv], [agent, null, theme]). % adv = dl(1,s,s)
get_roles1(confédérer, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(confédérer, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(confesser, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(confesser, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(confesser, [np, cl_r, pp(à), pp(de)], [agent, null, patient, theme]).
get_roles1(confier, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(confiner, [np, cl_r, pp(dans)], [agent, null, lieu]).
get_roles1(conformer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(conjurer, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(conjurer, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(conjurer, [np, cl_r, pp(avec), pp(contre)], [agent, null, patient, theme]).
get_roles1(consacrer, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(consumer, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(consumer, [np, cl_r, pp(en)], [agent, null, theme]).
get_roles1(contenter, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(contrebalancer, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(cotiser, [np, cl_r, pp(avec), pp(pour)], [agent, null, patient, but]).
get_roles1(cotiser, [np, cl_r, pp(pour)], [agent, null, but]).
get_roles1(couper, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(courroucer, [np, cl_r, pp(contre)], [patient, null, theme]). % unaccusative
get_roles1(couvrir, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative, unergative possible
get_roles1(cramponner, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(crever, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(crever, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(cristalliser, [np, cl_r, pp(_)], [patient, null, lieu]). % unaccusative
get_roles1(damner, [np, cl_r, pp(pour)], [agent, null, but]).
get_roles1(débattre, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(déchaîner, [np, cl_r, pp(contre)], [agent, null, patient]).
get_roles1(décharger, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(décharger, [np, cl_r, pp(de), pp(sur)], [agent, null, theme, patient]).
get_roles1(décider, [np, cl_r, pp(à)], [agent, null, theme]).
get_roles1(décider, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(décider, [np, cl_r, pp(pour)], [agent, null, theme]).
get_roles1(déclarer, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(déclarer, [np, cl_r, pp(pour)], [agent, null, theme]).
get_roles1(déclarer, [np, cl_r, pp(contre)], [agent, null, theme]).
get_roles1(déclarer, [np, cl_r, en_faveur_de], [agent, null, theme]).
get_roles1(décommander, [np, cl_r, pp(pour)], [agent, null, theme]).
get_roles1(décommander, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(déconsidérer, [np, cl_r, pp(par)], [patient, null, theme]). % unaccusative
get_roles1(découper, [np, cl_r, pp(sur)], [patient, null, theme]). % unaccusative
get_roles1(décourager, [np, cl_r, inf(de)], [patient, null, theme]). % unaccusative
get_roles1(dédire, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(défaire, [np, cl_r, pp(de)], [agent, null, patient]).
get_roles1(défendre, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(défier, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(dégager, [np, cl_r, pp(de)], [patient, null, source]). % unaccusative
get_roles1(dégager, [np_il, cl_r, pp(de)], [null, null, patient]).
get_roles1(dégager, [np_il, cl_r, pp(de), cs], [null, null, patient, theme]).
get_roles1(dégorger, [np, cl_r, pp(dans)], [patient, null, destination]).
get_roles1(dégoûter, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(délecter, [np, cl_r, pp(de)], [patient, null, theme]). % unaccusative
get_roles1(délecter, [np, cl_r, inf(à)], [patient, null, theme]). % unaccusative

% se défendre: P0 (PP<en>) (PM)

get_roles1(inquiéter, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(inscrire, [np, cl_r, pp(_)], [agent, null, destination]).

get_roles1(pencher, [np, cl_r, pp(sur)], [agent, null, theme]).
get_roles1(perdre, [np, cl_r, pp(_)], [agent, null, destination]). % static
get_roles1(perfectionner, [np, cl_r, pp(en)], [agent, null, theme]).
get_roles1(perpétuer, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(piquer, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(plaindre, [np, cl_r, pp(de)], [agent, null, theme]).
get_roles1(plaindre, [np, cl_r, pp(à)], [agent, null, patient]).
get_roles1(plaindre, [np, cl_r, pp(à), pp(de)], [agent, null, patient, theme]).
get_roles1(plaire, [np, cl_r, inf(à)], [agent, null, theme]).
get_roles1(plaire, [np, cl_r, pp(dans)], [agent, null, theme]).
get_roles1(plaire, [np, cl_r, pp(avec)], [agent, null, patient]).
get_roles1(plaire, [np, cl_r, pp(_)], [agent, null, destination]). % static
get_roles1(planquer, [np, cl_r, pp(_)], [agent, null, destination]). % static

get_roles1(trouver, [np, cl_r, pp(_)], [agent, null, lieu]).


get_roles1(aliener, [np, cl_r, np], [patient, null, theme]). % unaccusative
get_roles1(annexer, [np, cl_r, np], [agent, null, patient]).
get_roles1(appeler, [np, cl_r, np], [patient, null, theme]). % unaccusative
get_roles1(approprier, [np, cl_r, np], [agent, null, patient]).
get_roles1(arroger, [np, cl_r, np], [agent, null, patient]).
get_roles1(assimiler, [np, cl_r, np], [agent, null, patient]).
get_roles1(assujettir, [np, cl_r, np], [agent, null, patient]).
get_roles1(attirer, [np, cl_r, np], [patient, null, theme]). % unaccusative
get_roles1(avouer, [np, cl_r, np], [agent, null, theme]).
get_roles1(classer, [np, cl_r, np], [patient, null, theme]).
get_roles1(coltiner, [np, cl_r, np], [agent, null, patient]).
get_roles1(concilier, [np, cl_r, np], [agent, null, patient]).
get_roles1(confesser, [np, cl_r, np], [agent, null, theme]).



get_roles1(affirmer, [np, cl_r, adj], [patient, null, patient_attr]). % unaccusative
get_roles1(annoncer, [np, cl_r, adj], [patient, null, patient_attr]). % unaccusative
get_roles1(avérer, [np, cl_r, adj], [patient, null, patient_attr]). % unaccusative
get_roles1(avouer, [np, cl_r, adj], [agent, null, theme]).
get_roles1(classer, [np, cl_r, adj], [patient, null, theme]).
get_roles1(confesser, [np, cl_r, adj], [agent, null, theme]).
