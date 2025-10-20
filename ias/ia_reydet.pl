% Fichier : morpion.pl
% Version adaptée : Puissance 4 (board 6x7, drop dans colonnes 1..7)
% Les commentaires ci‑dessous expliquent chaque prédicat pour faciliter la lecture.

:- dynamic board/1. % le fait board/1 est modifiable à l'exécution (assert/retract)

% Représentation du plateau
% Board est une liste row‑major de 42 éléments (nth1 indices 1..42)
% Lignes : 0..5 (0 = haut), Colonnes : 0..6 (gauche->droite)
% Chaque case vaut ' ' (vide), 'x' (joueur humain) ou 'o' (IA).

% displayBoard/0
% Affiche le plateau courant (récupéré via board/1) sous forme 6x7.
% Les variables A..G, A1..G1, ... correspondent aux 6 lignes successives.
displayBoard:- 
    board([A,B,C,D,E,F,G,
    A1,B1,C1,D1,E1,F1,G1,
    A2,B2,C2,D2,E2,F2,G2,
    A3,B3,C3,D3,E3,F3,G3,
    A4,B4,C4,D4,E4,F4,G4,
    A5,B5,C5,D5,E5,F5,G5]),
    format('|~w|~w|~w|~w|~w|~w|~w|',[A, B, C, D, E, F, G]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A1, B1, C1, D1, E1, F1, G1]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A2, B2, C2, D2, E2, F2, G2]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A3, B3, C3, D3, E3, F3, G3]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A4, B4, C4, D4, E4, F4, G4]), nl,
    format('|~w|~w|~w|~w|~w|~w|~w|',[A5, B5, C5, D5, E5, F5, G5]), nl.

% index/3
% index(Row, Col, Idx) : transforme coordonnées (Row 0..5, Col 0..6) en indice nth1 (1..42)
index(Row,Col,Idx) :- % Row 0..5, Col 0..6 -> Idx nth1 1..42
    Idx is Row*7 + Col + 1.

% get_cell/4
% get_cell(Board, Row, Col, Val) : récupère la valeur Val à la position (Row,Col) du Board
get_cell(Board, Row, Col, Val) :-
    index(Row, Col, Idx), nth1(Idx, Board, Val).

% replace/5
% replace(List, Index, OldElem, NewElem, NewList) : remplace OldElem par NewElem à la position Index
% (nécessite que la valeur courante corresponde à OldElem)
replace(List, Index, OldElem, NewElem, NewList) :-
   nth1(Index,List,OldElem,Transfer),
   nth1(Index,NewList,NewElem,Transfer).

% drop_in_column/5
% drop_in_column(Board, Column, Player, NewBoard, PlacedIdx)
% Simule le "drop" d'un jeton Player ('x' ou 'o') dans la colonne Column (1..7).
% Trouve la plus basse ligne vide de la colonne et place le jeton.
% Renvoie NewBoard (plateau modifié) et PlacedIdx (indice nth1 de la case remplie).
drop_in_column(Board, Column, Player, NewBoard, PlacedIdx) :-
    between(1,7,Column),                     % colonne valide 1..7
    Col0 is Column - 1,                      % convertit en 0..6 pour index/3
    % on collecte toutes les lignes R où la case (R,Col0) est vide
    findall(R, (between(0,5,R), index(R,Col0,I), nth1(I,Board,' ')), Rs),
    Rs \= [],                                % colonne non pleine
    last(Rs, RowPlaced),                     % la plus basse ligne vide
    index(RowPlaced, Col0, PlacedIdx),
    nth1(PlacedIdx, Board, ' '),             % vérification (doit être vide)
    nth1(PlacedIdx, NewBoard, Player, Rest), % insère Player à la position PlacedIdx
    nth1(PlacedIdx, Board, ' ', Rest).       % réutilise le "Rest" pour reconstruire

% is_column_full/2
% is_column_full(Board, Column) : vrai si la case du sommet (Row 0) de la colonne est occupée
is_column_full(Board, Column) :-
    Col0 is Column - 1,
    index(0, Col0, Idx),
    nth1(Idx, Board, V),
    V \= ' '.

% next_move/3
% next_move(Board, Player, Column) : trouve une colonne Column (1..7) non pleine qui permet
% à Player de gagner immédiatement (après le drop).
% Utilisé pour heuristiques simples (attaque/defense immédiates).
next_move(Board, Player, Column) :-
    between(1,7,Column),
    \+ is_column_full(Board, Column),
    drop_in_column(Board, Column, Player, NewBoard, _),
    four_in_a_row(NewBoard, Player), !.

% four_in_a_row/2
% four_in_a_row(Board, P) : vrai s'il existe 4 P alignés (horiz/vert/diag) sur Board.
% La recherche parcourt des positions de départ qui garantissent de rester dans la grille.
four_in_a_row(Board, P) :-
    % horizontal (DR=0, DC=1) : start Row 0..5 Col 0..3
    ( between(0,5,R), between(0,3,C), check_seq(Board, R, C, 0, 1, P) );
    % vertical (DR=1, DC=0) : start Row 0..2 Col 0..6
    ( between(0,2,R), between(0,6,C), check_seq(Board, R, C, 1, 0, P) );
    % diag down-right (DR=1, DC=1) : start Row 0..2 Col 0..3
    ( between(0,2,R), between(0,3,C), check_seq(Board, R, C, 1, 1, P) );
    % diag up-right (DR=-1, DC=1) : start Row 3..5 Col 0..3
    ( between(3,5,R), between(0,3,C), check_seq(Board, R, C, -1, 1, P) ),
    P \= ' '.  % ignore four empty cells

% check_seq/6 : vérifie 4 cellules consécutives = P à partir de (R,C) suivant (DR,DC)
check_seq(Board, R, C, DR, DC, P) :-
    R0 is R, C0 is C,
    index(R0, C0, I0), nth1(I0, Board, P),
    R1 is R0 + DR, C1 is C0 + DC, index(R1, C1, I1), nth1(I1, Board, P),
    R2 is R1 + DR, C2 is C1 + DC, index(R2, C2, I2), nth1(I2, Board, P),
    R3 is R2 + DR, C3 is C2 + DC, index(R3, C3, I3), nth1(I3, Board, P).

% win/1
% win(Board) : vrai si un joueur (différent de ' ') a 4 en ligne ; affiche le gagnant.
win(Board) :-
    four_in_a_row(Board, P),
    P \= ' ',
    format('Le joueur ~w a gagne !~n', [P]).

% draw/1
% draw(Board) : vrai si le plateau est plein et aucun alignement de 4 n'existe.
draw(Board) :-
    \+ (four_in_a_row(Board, _)),  % pas de gagnant
    \+ member(' ', Board),         % plus de cases vides
    writeln('Egalite !').

% ia/3
% ia(Board, Column, Player) : calcule un coup pour l'IA (retourne une colonne 1..7).
% Implémentation actuelle :
% - construit la liste des colonnes non pleines,
% - choisit aléatoirement l'une d'elles.
% Remarque : des lignes pour next_move (gagner/bloquer) sont présentes mais commentées.
ia(Board, Column, Player) :-
    changePlayer(Player, Opponent),
    (   next_move(Board, Player, Column) -> true
    ;   next_move(Board, Opponent, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

ia_def(Board, Column, Player) :-
    changePlayer(Player, Opponent),
    (   next_move(Board, Opponent, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

ia_att(Board, Column, Player) :-
    (   next_move(Board, Player, Column) -> true
    ;
    findall(C, (between(1,7,C), \+ is_column_full(Board,C)), ValidCols),
    ValidCols \= [],
    length(ValidCols, N),
    random_between(1, N, R),
    nth1(R, ValidCols, Column)
    ) .

% playMove/4
% playMove(Board, Column, NewBoard, Player) : applique le coup Column pour Player
% en retournant le nouveau plateau NewBoard.
playMove(Board,Column,NewBoard,Player) :-
    drop_in_column(Board, Column, Player, NewBoard, _).

% applyIt/2
% Remplace le fait board/1 courant par NewBoard (réflexion de l'état en mémoire).
applyIt(Board, NewBoard):- 
    retract(board(Board)),
    assert(board(NewBoard)).

% changePlayer/2
% changePlayer(Current, Next) : inverse 'x' <-> 'o'
changePlayer('x','o').
changePlayer('o','x').

% play/1 (ia contre ia)
% Boucle principale de jeu : play(Player) exécute un tour pour Player,
% affiche le plateau, demande un coup IA (ia/3), applique le coup, vérifie fin de partie,
% puis passe au joueur suivant en récursion.
% play(Player):-  write('New turn for:'), writeln(Player),
%             board(Board),
%                displayBoard,
%             (   ia(Board, Move,Player),
%                 playMove(Board,Move,NewBoard,Player),
%                 applyIt(Board, NewBoard),
%                 (   win(NewBoard) -> displayBoard ; draw(NewBoard) -> displayBoard
%                 ;   changePlayer(Player, Next),
%                     play(Next)
%                 )
%             ).


% play/1
% Boucle principale modifiée : le joueur 'x' (un coup sur deux) est demandé à l'utilisateur,
% le joueur 'o' est joué par l'IA.
play(Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    displayBoard,
    (   Player == 'x' ->
        % coup décidé par l'utilisateur
        prompt_move(Board, Column),
        playMove(Board, Column, NewBoard, Player)
    ;   % coup décidé par l'IA
        ia(Board, Move, Player),
        playMove(Board, Move, NewBoard, Player)
    ),
    applyIt(Board, NewBoard),
    (   win(NewBoard) -> displayBoard, ask_restart
    ;   draw(NewBoard) -> displayBoard, ask_restart
    ;   changePlayer(Player, Next),
        play(Next)
    ).

% prompt_move(+Board, -Column)
% Lit au clavier une colonne valide (1..7) non pleine ; répète tant que la saisie est invalide.
prompt_move(Board, Column) :-
    repeat,
        write('Entrez une colonne (1..7) pour jouer (ou "q" pour quitter) : '), flush_output,
        read_line_to_string(user_input, S),
        ( S = "q" -> writeln('Quit.'), abort ; true ),
        ( catch(number_string(N, S), _, fail) ->
            ( integer(N), between(1,7,N), \+ is_column_full(Board,N) ->
                Column = N, !    % succès, on coupe le repeat
            ; writeln('Colonne invalide ou pleine. Reessayer.'), fail
            )
        ; writeln('Entree non numérique. Réessayer.'), fail
        ).

% reset/0
% Supprime l'état courant du plateau (utile pour relancer proprement).
reset :-
    retractall(board(_)).

% ask_restart/0
% Propose à l'utilisateur de recommencer une partie après la fin d'une partie.
% Si l'utilisateur répond 'y' ou 'Y' : reset + init (nouvelle partie).
% Sinon : affiche "Fin." et termine (retourne dans le toplevel).
ask_restart :-
    write('Recommencer une nouvelle partie ? (y/n) : '), flush_output,
    read_line_to_string(user_input, S),
    ( member(S, ["y","Y"]) ->
        reset,
        init
    ; writeln('Fin.')
    ).

% joue_coup(+BoardRows, +JoueurRaw, -Column)
% BoardRows : liste de 6 listes de 7 cases (représentation 2D).
% JoueurRaw : 'x' ou 'o' (atom ou string).
% Column : colonne choisie (1..7). Appelle ia/3 et affiche la colonne choisie.
joue_coup(BoardRows, JoueurRaw, Column) :-
    ( atom(JoueurRaw) -> Joueur = JoueurRaw
    ; string(JoueurRaw) -> atom_string(Joueur, JoueurRaw)
    ; Joueur = JoueurRaw ),
    flatten_board_rows(BoardRows, FlatBoard),
    (   ia(FlatBoard, Column, Joueur)
    ->  format('IA choisit colonne ~w.~n', [Column])
    ;   findall(C, (between(1,7,C), \+ is_column_full(FlatBoard,C)), Valids),
        ( Valids = [] ->
            format('Aucun coup valide.~n'), fail
        ; length(Valids,N), random_between(1,N,R), nth1(R,Valids,Column),
          format('IA (fallback) choisit colonne ~w.~n', [Column])
        )
    ).

% flatten_board_rows(+Rows2D, -Flat)
% Vérifie la forme 6x7 puis aplatis en liste 42 éléments.
flatten_board_rows(Rows, Flat) :-
    is_list(Rows), length(Rows, 6),
    maplist(valid_row, Rows, ConvRows),
    append(ConvRows, Flat).

valid_row(Row, Row) :- 
    is_list(Row), length(Row, 7).

% init/0
% Initialise le plateau (42 cases vides) et lance la partie avec 'x' au tour.
init :-
    retractall(board(_)),                % supprime tout ancien état si présent
    length(Board,42), maplist(=(' '), Board), assert(board(Board)),
    play('x').