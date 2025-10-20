:- dynamic board/1.

% --- Gestion du plateau ---
getFreeColumns(Board, FreeCols) :-
    findall(Index, (nth1(Index, Board, Col), member('_', Col)), FreeCols).

simulateMove(Board, ColIndex, Player, SimulatedBoard) :-
    nth1(ColIndex, Board, Col),
    placeInColumn(Col, Player, NewCol),
    replace(Board, ColIndex, NewCol, SimulatedBoard).

placeInColumn(Col, Player, NewCol) :-
    reverse(Col, RevCol),
    placeInReversedColumn(RevCol, Player, NewRevCol),
    reverse(NewRevCol, NewCol).

placeInReversedColumn([H|T], Player, [Player|T]) :- H = '_', !.
placeInReversedColumn([H|T], Player, [H|R]) :-
    placeInReversedColumn(T, Player, R).

replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :-
    N > 1, N1 is N - 1, replace(T, N1, X, R).

changePlayer('x','o').
changePlayer('o','x').

% --- Victoire ---
win(Player, Board) :-
    horizontal_win(Board, Player);
    vertical_win(Board, Player);
    diagonal_asc(Board, Player);
    diagonal_desc(Board, Player).

horizontal_win(Board, Player) :-
    member(Row, Board),
    consecutive_four(Row, Player).

vertical_win(Board, Player) :-
    transpose(Board, T),
    member(Col, T),
    consecutive_four(Col, Player).

diagonal_asc(Board, Player) :-
    between(1,4,ColIndex), between(1,3,RowIndex),
    nth1(ColIndex, Board, C1), nth1(RowIndex, C1, Player),
    Col2 is ColIndex+1, Row2 is RowIndex+1, nth1(Col2, Board, C2), nth1(Row2, C2, Player),
    Col3 is ColIndex+2, Row3 is RowIndex+2, nth1(Col3, Board, C3), nth1(Row3, C3, Player),
    Col4 is ColIndex+3, Row4 is RowIndex+3, nth1(Col4, Board, C4), nth1(Row4, C4, Player).

diagonal_desc(Board, Player) :-
    between(1,4,ColIndex), between(4,6,RowIndex),
    nth1(ColIndex, Board, C1), nth1(RowIndex, C1, Player),
    Col2 is ColIndex+1, Row2 is RowIndex-1, nth1(Col2, Board, C2), nth1(Row2, C2, Player),
    Col3 is ColIndex+2, Row3 is RowIndex-2, nth1(Col3, Board, C3), nth1(Row3, C3, Player),
    Col4 is ColIndex+3, Row4 is RowIndex-3, nth1(Col4, Board, C4), nth1(Row4, C4, Player).

consecutive_four([P,P,P,P|_], P) :- !.
consecutive_four([_|T], P) :- consecutive_four(T, P).

transpose([], []).
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rows]) :-
    maplist(nth1(1), Matrix, Row),
    maplist(tl, Matrix, Rest),
    transpose(Rest, Rows).
tl([_|T], T).

% --- IA stratégique 3 ---
scoreBoard(Board, Player, Score) :-
    scoreAlign(Board, Player, 2, S2),
    scoreAlign(Board, Player, 3, S3),
    scoreAlign(Board, Player, 4, S4),
    Score is S2 + S3*10 + S4*100.

scoreAlign(Board, Player, Len, Count) :-
    findall(1, (member(L, Board), consecutive(L, Player, Len)), Ls),
    length(Ls, Count1),
    transpose(Board, T),
    findall(1, (member(C, T), consecutive(C, Player, Len)), Lc),
    length(Lc, Count2),
    Count is Count1 + Count2.

consecutive(List, Player, Len) :-
    append(_, Sub, List), length(Sub, Len), maplist(=(Player), Sub).

bestScoreMove([_-M], M) :- !.
bestScoreMove([S1-C1,S2-C2|Rest], Best) :-
    (S1 >= S2 -> bestScoreMove([S1-C1|Rest], Best)
               ; bestScoreMove([S2-C2|Rest], Best)).

iaStrategique3(Board, Move, Player) :-
    getFreeColumns(Board, FreeCols),
    ( member(Move, FreeCols),
      simulateMove(Board, Move, Player, B),
      win(Player, B) -> true
    ;
      changePlayer(Player, Opp),
      member(Move, FreeCols),
      simulateMove(Board, Move, Opp, B),
      win(Opp, B) -> true
    ;
      findall(Score-Col,
    (
        member(Col, FreeCols),
        simulateMove(Board, Col, Player, B1),
        changePlayer(Player, Opp),
        getFreeColumns(B1, OppCols),
        findall(S2,
            (member(OC, OppCols),
             simulateMove(B1, OC, Opp, B2),
             scoreBoard(B2, Player, S2)),
            OppScores),
        (OppScores == [] -> Score = 0; min_list(OppScores, Score))
    ),
    Scores),
    bestScoreMove(Scores, Move)
    ).

% --- Fonction finale qui joue un coup ---
joue_coup(Board, Joueur, Col) :-
    iaStrategique3(Board, Col, Joueur),
    format('~w~n', [Col]).
