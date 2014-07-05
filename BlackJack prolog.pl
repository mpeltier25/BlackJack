Cards(2).
Cards(3).
Cards(4).
Cards(5).
Cards(6).
Cards(7).
Cards(8).
Cards(9).
Cards(10).
Cards(11).

Playerturn(TotalPoints) :-
    Playerturn([], TotalPoints).
total([FirstCard|Deck], Current, Final) :-
    NewResult is FirstCard + Current,
    total(Deck, NewResult, Final)

Dealerturn(PlayerPoints) :-
    Dealerturn([], PlayerPoints).

InputRead(Input) :-
    seeing(message),
    see(player),
    write('(true/false)'),
    read(Input),
    seen,
    see(message).
Playerturn(_, Cards, TotalPoints) :-
    writeln('Please specify either true or false'),
    InputRead(Input),
    Playerturn(Input, Cards, TotalPoints).  
Playerturn('false', Cards, TotalPoints) :-
    total(Cards, 0, TotalPoints).

Playerturn('true', Cards, TotalPoints) :-
    NewCards = [2|Cards],
    writeln(NewCards),
    total(NewCards, 0, Result),
    InputRead(Input),
    Playerturn(Input, NewCards, TotalPoints).

Playerturn(InitCards, TotalPoints) :-
    InputRead(Input),
    Playerturn(Input, InitCards, TotalPoints).

GameResults(Cards, Result, PlayerPoints) :-
    (Result > PlayerPoints ->
        true, write('Dealer win');
        Dealerturn(Cards, PlayerPoints)).
    (Result < PlayerPoints ->
        false, write('Player win');
game :-
    Playerturn(PlayerPoints),
    Dealerturn(PlayerPoints).