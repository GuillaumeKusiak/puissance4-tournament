:- module(ia_blipoup, [joue_coup/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

% Plateau du tournoi : liste de 6 lignes (haut -> bas), 7 colonnes, 0 vide, 1/2 joueurs.
% On convertit vers la représentation colonne bas->haut attendue par le moteur minimax existant.

joue_coup(BoardRows, PlayerNum, Column0) :-
    board_to_internal(BoardRows, Plateau),
    player_token(PlayerNum, Joueur),
    once(ia_minimax(Plateau, Joueur, Column1)),
    Column0 is Column1 - 1,
    !.
joue_coup(BoardRows, _PlayerNum, Column0) :-
    % Fallback: choisir la première colonne encore jouable si le minimax échoue.
    nth0(0, BoardRows, TopRow),
    nth0(Column0, TopRow, 0).

board_to_internal(BoardRows, Plateau) :-
    transpose(BoardRows, ColumnsTopDown),
    maplist(column_to_internal, ColumnsTopDown, Plateau).

column_to_internal(ColumnTopDown, ColumnInternal) :-
    maplist(cell_token, ColumnTopDown, TokensTopDown),
    reverse(TokensTopDown, ColumnInternal).

cell_token(0, '_').
cell_token(1, x).
cell_token(2, o).

player_token(1, x).
player_token(2, o).

% IA Minimax adaptée au format de plateau de logique.pl
% Le plateau fourni est une liste de colonnes, chaque colonne étant rangée du haut vers le bas.
% On convertit en représentation bas-vers-haut avant de réutiliser la logique exacte de ia.pl.

nb_colonnes(7).
nb_lignes(6).

profondeur_max(4).
valeur_infinie(100000).

autre_joueur(x, o).
autre_joueur(o, x).

% aiMinimax(+BoardTopDown, +Player, -Move)
aiMinimax(BoardTopDown, Player, Move) :-
    maplist(reverse, BoardTopDown, BoardBottomUp),
    ia_minimax(BoardBottomUp, Player, Move).

ia_minimax(Plateau, Joueur, MeilleurCoup) :-
    profondeur_max(ProfMax),
    valeur_infinie(Inf),
    NegInf is -Inf,
    colonnes_valides_ordonnees(Plateau, Colonnes),
    (Colonnes = [] ->
        MeilleurCoup = 1
    ;
        findall(Score-Col,
        (member(Col, Colonnes),
         jouer(Plateau, Col, Joueur, Plateau2),
         autre_joueur(Joueur, Adversaire),
         ProfRest is ProfMax - 1,
         minimax(Plateau2, Adversaire, Joueur, ProfRest, NegInf, Inf, Score)),
        Coups),
        meilleur_coup(Coups, MeilleurCoup)
    ).

minimax(Plateau, _, JoueurMax, _, _, _, Score) :-
    terminal_score(Plateau, JoueurMax, Score),
    !.
minimax(Plateau, _, JoueurMax, Profondeur, _, _, Score) :-
    Profondeur =< 0,
    evaluer(Plateau, JoueurMax, Score),
    !.
minimax(Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, Beta, Score) :-
    colonnes_valides_ordonnees(Plateau, Coups),
    (Coups = [] ->
        evaluer(Plateau, JoueurMax, Score)
    ; (JoueurCourant == JoueurMax ->
         valeur_infinie(Inf),
         NegInf is -Inf,
         maximiser(Coups, Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, Beta, NegInf, Score)
      ;
         valeur_infinie(Inf),
         minimiser(Coups, Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, Beta, Inf, Score)
      )
    ).

maximiser([], _, _, _, _, _, _, Meilleur, Meilleur).
maximiser([Col|R], Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, Beta, Courant, MeilleurScore) :-
    jouer(Plateau, Col, JoueurCourant, Plateau2),
    autre_joueur(JoueurCourant, Suivant),
    ProfSuiv is Profondeur - 1,
    minimax(Plateau2, Suivant, JoueurMax, ProfSuiv, Alpha, Beta, ScoreCoup),
    NewBest is max(Courant, ScoreCoup),
    NewAlpha is max(Alpha, ScoreCoup),
    (NewAlpha >= Beta ->
        MeilleurScore = NewBest
    ;
        maximiser(R, Plateau, JoueurCourant, JoueurMax, Profondeur, NewAlpha, Beta, NewBest, MeilleurScore)
    ).

minimiser([], _, _, _, _, _, _, Meilleur, Meilleur).
minimiser([Col|R], Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, Beta, Courant, MeilleurScore) :-
    jouer(Plateau, Col, JoueurCourant, Plateau2),
    autre_joueur(JoueurCourant, Suivant),
    ProfSuiv is Profondeur - 1,
    minimax(Plateau2, Suivant, JoueurMax, ProfSuiv, Alpha, Beta, ScoreCoup),
    NewBest is min(Courant, ScoreCoup),
    NewBeta is min(Beta, ScoreCoup),
    (Alpha >= NewBeta ->
        MeilleurScore = NewBest
    ;
        minimiser(R, Plateau, JoueurCourant, JoueurMax, Profondeur, Alpha, NewBeta, NewBest, MeilleurScore)
    ).

terminal_score(Plateau, JoueurMax, Score) :-
    valeur_infinie(Inf),
    (   vainqueur(Plateau, JoueurMax)
    ->  Score is Inf
    ;   autre_joueur(JoueurMax, Adv),
        vainqueur(Plateau, Adv)
    ->  Score is -Inf
    ;   plein(Plateau)
    ->  Score = 0
    ).

meilleur_coup([Score-Col|Reste], MeilleurCol) :-
    centre_colonne(Centre),
    meilleur_coup(Reste, Score, Col, Centre, MeilleurCol).
meilleur_coup([], _, ColActuel, _, ColActuel).
meilleur_coup([Score-Col|Reste], BestScore, BestCol, Centre, MeilleurCol) :-
    (Score > BestScore ->
        NewScore = Score,
        NewCol = Col
    ; Score =:= BestScore ->
        plus_central(Col, BestCol, Centre, NewCol),
        NewScore = BestScore
    ;
        NewScore = BestScore,
        NewCol = BestCol
    ),
    meilleur_coup(Reste, NewScore, NewCol, Centre, MeilleurCol).

plus_central(Col1, Col2, Centre, Choisi) :-
    Diff1 is abs(Col1 - Centre),
    Diff2 is abs(Col2 - Centre),
    (Diff1 < Diff2 ->
        Choisi = Col1
    ; Diff2 < Diff1 ->
        Choisi = Col2
    ; Col1 < Col2 ->
        Choisi = Col1
    ;
        Choisi = Col2
    ).

jouer(Plateau, Colonne, Joueur, NouveauPlateau) :-
    nth1(Colonne, Plateau, Col),
    inserer_dans_colonne(Col, Joueur, NouvelleCol),
    remplacer(Plateau, Colonne, NouvelleCol, NouveauPlateau).

inserer_dans_colonne(['_' | R], Joueur, [Joueur | R]) :- !.
inserer_dans_colonne([C | R], Joueur, [C | R2]) :-
    inserer_dans_colonne(R, Joueur, R2).

remplacer([_ | R], 1, X, [X | R]) :- !.
remplacer([A | R], I, X, [A | R2]) :-
    I2 is I - 1,
    remplacer(R, I2, X, R2).

vainqueur(Plateau, Joueur) :-
    ligne_gagnante(Plateau, Joueur);
    colonne_gagnante(Plateau, Joueur);
    diagonale_gagnante(Plateau, Joueur).

ligne_gagnante(Plateau, Joueur) :-
    transpose(Plateau, Trans),
    member(Ligne, Trans),
    quatre_aligne(Ligne, Joueur).

colonne_gagnante(Plateau, Joueur) :-
    member(Col, Plateau),
    quatre_aligne(Col, Joueur).

diagonale_gagnante(Plateau, Joueur) :-
    diagonales(Plateau, Diags),
    member(D, Diags),
    quatre_aligne(D, Joueur).

quatre_aligne(L, J) :-
    append(_, [J, J, J, J | _], L).

diagonales(Plateau, Diags) :-
    findall(D, diagonale(Plateau, D), D1),
    maplist(reverse, Plateau, Inv),
    findall(D2, diagonale(Inv, D2), D2s),
    append(D1, D2s, Diags).

diagonale(Plateau, D) :-
    transpose(Plateau, T),
    nth1(X, T, _),
    nth1(Y, Plateau, _),
    diagonale_depuis(Plateau, X, Y, D).

diagonale_depuis(Plateau, X, Y, D) :-
    nb_colonnes(NC),
    nb_lignes(NL),
    X =< NC, Y =< NL,
    diagonale_suivante(Plateau, X, Y, D).

diagonale_suivante(_, _, _, []).
diagonale_suivante(Plateau, X, Y, [V|R]) :-
    nth1(X, Plateau, Col),
    nth1(Y, Col, V),
    X2 is X + 1, Y2 is Y + 1,
    diagonale_suivante(Plateau, X2, Y2, R).

coup_valide(Plateau, Colonne) :-
    nth1(Colonne, Plateau, Col),
    memberchk('_', Col).

colonnes_valides(Plateau, Colonnes) :-
    nb_colonnes(NC),
    numlist(1, NC, Toutes),
    include(coup_valide(Plateau), Toutes, Colonnes).

colonnes_valides_ordonnees(Plateau, ColonnesOrdonnees) :-
    colonnes_valides(Plateau, Colonnes),
    ordonner_par_centre(Colonnes, ColonnesOrdonnees).

ordonner_par_centre(Colonnes, Ordonnees) :-
    centre_colonne(Centre),
    map_list_to_pairs(distance_centre(Centre), Colonnes, Paires),
    keysort(Paires, PairesTriees),
    pairs_values(PairesTriees, Ordonnees).

centre_colonne(Centre) :-
    nb_colonnes(NC),
    Centre is (NC // 2) + 1.

distance_centre(Centre, Col, Distance) :-
    Distance is abs(Col - Centre).

plein(Plateau) :-
    \+ (coup_valide(Plateau, _)).

evaluer(Plateau, Joueur, Score) :-
    autre_joueur(Joueur, Adv),
    score_total(Plateau, Joueur, SJ),
    score_total(Plateau, Adv, SA),
    bonus_centre(Plateau, Joueur, BJ),
    bonus_centre(Plateau, Adv, BA),
    Score is SJ - SA + BJ - BA.

bonus_centre(Plateau, Joueur, Bonus) :-
    nb_colonnes(NC),
    Centre is (NC + 1) // 2,
    nth1(Centre, Plateau, ColCentre),
    include(=(Joueur), ColCentre, Jetons),
    length(Jetons, N),
    Bonus is N * 6.

score_total(Plateau, Joueur, Score) :-
    lignes_toutes(Plateau, Lignes),
    maplist(valeur_ligne(Joueur), Lignes, Scores),
    sum_list(Scores, Score).

lignes_toutes(Plateau, Lignes) :-
    transpose(Plateau, Trans),
    diagonales(Plateau, D1),
    maplist(reverse, Plateau, Inv),
    diagonales(Inv, D2),
    append([Plateau, Trans, D1, D2], Lignes).

valeur_ligne(Joueur, Ligne, Score) :-
    sous_listes(Ligne, Sous),
    maplist(valeur_fenetre(Joueur), Sous, Scores),
    sum_list(Scores, Score).

sous_listes(L, Sous) :-
    findall(W, (append(_, Rest, L), append(W, _, Rest), length(W, 4)), Sous).

valeur_fenetre(Joueur, Fenetre, Score) :-
    autre_joueur(Joueur, Adv),
    include(=(Joueur), Fenetre, L1), length(L1, N1),
    include(=(Adv), Fenetre, L2), length(L2, N2),
    include(=('_'), Fenetre, L3), length(L3, N3),
    (N1 = 4 -> Score = 10000;
     N1 = 3, N3 >= 1 -> Score = 500;
     N1 = 2, N3 >= 2 -> Score = 50;
     N2 = 4 -> Score = -9000;
     N2 = 3, N3 >= 1 -> Score = -450;
     N2 = 2, N3 >= 2 -> Score = -40;
     Score = 0).