%----------------------------------------------------------------------------------------------------
%   Name:     Niraj Bhattarai
%	Project:  Five Crowns
%	Class:	  OPL
%   Date:     December 11, 2019
%----------------------------------------------------------------------------------------------------


%----------------------------------------------------------------------------------------------------
%   Predicate Name: loadTestFile
%   Purpose: load data from file
%   Parameters: 
%           Data, stream of data loaded from file
%           FileName, name of the file from which game was loaded
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

loadTestFile(Data, FileName):-
    exists_file(FileName), 
    write('File Name Entered was: '), write(FileName), nl, 
    open(FileName, read, File),
    read(File, Data),
    close(File).

loadTestFile(Data, _):- 
    write('Enter another filename : '), 
    read(NewFileName), 
    loadTestFile(NewData, NewFileName), 
    Data = NewData.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: saveGame
%   Purpose: Save game state to file
%   Parameters: 
%           State, state of the game that will be saved in file
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

saveGame(State):- 
    write('Enter filename: '), 
    read(FileName),
    open(FileName, append, File),
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, _] = State,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer] = NewState,
    write(File, NewState),
    write(File, '.'),
    close(File), 
    halt.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: parseGame
%   Purpose: load game from file
%   Parameters: 
%           State, state of the game that was loaded from file
%           FileName, name of the file from which game is loaded
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

parseGame(State, FileName):-
    loadTestFile([Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, CurrentPlayer], FileName),
    PlayersGoneOut = 0,
    State = [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, CurrentPlayer, PlayersGoneOut].
%----------------------------------------------------------------------------------------------------
%   Predicate Name: printWinner
%   Purpose: outputs winner of the game
%   Parameters: 
%           HumanScore, cumulative score human player received
%           ComputerScore, cumulative score computer player received
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

printWinner(HumanScore, ComputerScore):-
    HumanScore < ComputerScore,
    nl, write(' Human won the game '), nl,
    nl, write('Human Score: '), write(HumanScore), nl,
    nl, write('Computer Score: '), write(ComputerScore), nl.  

printWinner(HumanScore, ComputerScore):-
    HumanScore > ComputerScore,
    nl, write(' Computer won the game '), nl,
    nl, write('Human Score: '), write(HumanScore), nl,
    nl, write('Computer Score: '), write(ComputerScore), nl.  

printWinner(HumanScore, ComputerScore):-
    HumanScore == ComputerScore,
    nl, write(' Game was draw '), nl,
    nl, write('Human Score: '), write(HumanScore), nl,
    nl, write('Computer Score: '), write(ComputerScore), nl.  
%----------------------------------------------------------------------------------------------------
%   Predicate Name: gameEnd
%   Purpose: display winner/loser. goes back to the starting point of the game
%   Parameters: 
%           State, game state before new round was started
%           NewState, state of the game after the new round is started
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

gameEnd(State):-
    write('\n Game has ended \n'),
    printGameDetails(State),
    [_, ComputerScore, _, HumanScore | _] = State,
    printWinner(HumanScore, ComputerScore),
    nl, write('Game Ended'), nl, 
    main.
%----------------------------------------------------------------------------------------------------
%   Predicate Name: startNewRound
%   Purpose: Starts a new round.
%   Parameters: 
%           State, game state before new round was started
%           NewState, state of the game after the new round is started
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

startNewRound(State, NewState):-
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, _] = State,
    append(DrawPile, ComputerHand, CDrawPile),
    append(CDrawPile, HumanHand, HDrawPile),
    append(HDrawPile, DiscardPile, NewDrawPile),
    permutation(NewDrawPile, NewNewDrawPile),
    NewRound is Round + 1, 
    NumberOfCards is NewRound + 2, 
    distributeCards(NumberOfCards, NewNewDrawPile, NewHumanHand, NayaDrawPile),
    distributeCards(NumberOfCards, NayaDrawPile, NewComputerHand, NayaNayaDrawPile),
    distributeCards(1, NayaNayaDrawPile, NewDiscardPile, ODrawPile), 
    NewPlayersGoneOut = 0,
    [NewRound, ComputerScore, NewComputerHand, HumanScore, NewHumanHand, ODrawPile, NewDiscardPile, NextPlayer, NewPlayersGoneOut] = NewState,
    write('-------------------New Round started -------------------'), nl,
    printGameDetails(NewState).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: playRound
%   Purpose: check if the last player's hand qualifies for minimum score
%   Parameters: 
%           State, game state before player has played.
%           NewState, state of the game after the player has played the game
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

playRound(State , _):- 
    [Round,_, _, _, _, _, _, _, PlayersGoneOut] = State ,
    Round==11, 
    PlayersGoneOut==2,
    gameEnd(State). 


playRound(State, NewState):- 
    [_, _, _, _, _, _, _, _, PlayersGoneOut] = State,
    PlayersGoneOut==2,
    startNewRound(State, NewState),
    nl, printGameDetails(NewState), nl.


playRound(State, NewState):-
    [_, _, _, _, _, _, _, CurrentPlayer, _] = State,
    CurrentPlayer == human,
    write("\nCurrent Player is Human\n"),
    humanMove(State, NewState).

playRound(State, NewState):-
    [_, _, _, _, _, _, _, CurrentPlayer, _] = State,
    CurrentPlayer == computer,
    computerMove(State, NewState), nl.

setGoOut(0, 1).
setGoOut(_, 0).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: validateGoOut
%   Purpose: check if the last player's hand qualifies for minimum score
%   Parameters: 
%               State, State before player has played. Gives the last player who played the game.
%           NewState, state of the game after the player has played the game
%           GoOut, True if the last player playing the game can go out
%           Player, the player that can go out
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

validateGoOut(State, NewState, GoOut, Player):- 
    [Round, _, _, _, _, _, _, PrevPlayer, _] = State,
    [_, _, CurrComputerHand, _, _, _, _, _, _] = NewState,
    PrevPlayer == computer,
    genBestMain(CurrComputerHand, Round, [], X, MinScore),
    nl, write('Min Score on computers hand: '), write(MinScore), nl,
    nl, write('Min Branch on computers hand: '), write(X), nl,
    setGoOut(MinScore, GoOut),
    PrevPlayer = Player.

validateGoOut(State, NewState, GoOut, Player):- 
    [Round, _, _, _, _, _, _, PrevPlayer, _] = State,
    [_, _, _, _, CurrHumanHand, _, _, _, _] = NewState,
    PrevPlayer == human,
    genBestMain(CurrHumanHand, Round, [], X, MinScore),
    nl, write('Min Score on humans hand: '), write(MinScore), nl,
    nl, write('Min Branch on humans hand: '), write(X), nl,
    setGoOut(MinScore, GoOut),
    PrevPlayer = Player.
%----------------------------------------------------------------------------------------------------
%   Predicate Name: forceGoOut
%   Purpose: Forces another player to go out if a player has gone out already
%   Parameters: Player, Variable that determines the last player that played
%               State, State before player has played. Gives the last player who played the game.
%           NewState, state of the game after the player has played the game
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

forceGoOut(Player, State, NewState):-
    Player == computer, 
    [Round, ComputerScore, CurrComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = State,
    genBestMain(CurrComputerHand, Round, [], X, MinScore),
    nl, write('Min Score on computers hand: '), write(MinScore), nl,
    nl, write('Min Branch on computers hand: '), write(X), nl,
    NewComputerScore is ComputerScore + MinScore,
    NewPlayersGoneOut is PlayersGoneOut + 1,
    [Round, NewComputerScore, CurrComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, human, NewPlayersGoneOut] = NewState,
    nl,write('---------------------'), write(Player), write(' forced to go out -----------------------'), nl.

forceGoOut(Player, State, NewState):- 
    Player == human, 
    [Round, ComputerScore, ComputerHand, HumanScore, CurrHumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = State,
    genBestMain(CurrHumanHand, Round, [], X, MinScore),
    nl, write('Min Score on humans hand: '), write(MinScore), nl,
    nl, write('Min Branch on humans hand: '), write(X), nl,
    NewHumanScore is HumanScore + MinScore,
    NewPlayersGoneOut is PlayersGoneOut + 1,
    [Round, ComputerScore, ComputerHand, NewHumanScore, CurrHumanHand, DrawPile, DiscardPile, computer, NewPlayersGoneOut] = NewState,
    nl,write('---------------------'), write(Player), write(' forced to go out -----------------------'), nl.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: goOutIfPossible
%   Purpose: Goes out if the card in current players hand can be arranged into runs and books
%   Parameters: State, State before player has played. Gives the last player who played the game.
%           NewState, state of the game after the player has played the game
%           GoneOutState, State  of the game after player has gone out
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

goOutIfPossible(State, NewState, GoneOutState):-
    [_, _, _ , _, _, _, _, PrevPlayer, _] = State,
    [_, _, _ , _, _, _, _, _, PlayersGoneOut] = NewState,
    PlayersGoneOut > 0,
    forceGoOut(PrevPlayer, NewState, GoneOutState),
    nl, printGameDetails(GoneOutState), nl.

goOutIfPossible(State, NewState, GoneOutState):-
    validateGoOut(State, NewState, GoOut, Player),
    nl, write('Go Out : '), write(GoOut), nl,
    goOutIfPossibleHelper(GoOut, NewState, GoneOutState, Player).
% Helper function for goOutIfPossible predicate
goOutIfPossibleHelper(GoOut, NewState, GoneOutState,_):-
    GoOut == 0,
    GoneOutState = NewState. 

goOutIfPossibleHelper(GoOut, NewState, GoneOutState, Player):-
    GoOut == 1,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = NewState,
    NewPlayersGoneOut is PlayersGoneOut + 1,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, NewPlayersGoneOut] = GoneOutState,
    nl,write('---------------------'), write(Player), write(' went out -----------------------'), nl.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: playRoundLoopHelper
%   Purpose: Helper function that let's each player play their turn.
%           Checks if a player can go out after their turn
%   Parameters: State, state of the game before checking player
%           NewState, state of the game after new game has started or player has gone out
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

playRoundLoopHelper(State, NewState):- 
    [PrevRound | _] = State,
    [NewRound | _ ] = NewState, 
    PrevRound \== NewRound, 
    playRoundLoop(NewState).

playRoundLoopHelper(State, NewState):- 
    [PrevRound | _] = State,
    [NewRound | _ ] = NewState, 
    PrevRound == NewRound, 
    goOutIfPossible(State, NewState, GoneOutState),
    playRoundLoop(GoneOutState).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: playRoundLoop
%   Purpose: game loop that calls the playRound predicate to play game.
%   Parameters: State, state of the game before picking card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

playRoundLoop(State):-
    playRound(State, NewState),
    playRoundLoopHelper(State, NewState).

pushFront(Item, List, [Item | List]).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickFromDiscardPile
%   Purpose: pick a card from discard pile and put in player's hand
%   Parameters: State, state of the game before picking card
%               CurrentPlayer, determines on which player's hand card is added
%               NewState, State of the game after the card is picked
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

pickFromDiscardPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == human,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    select(DiscardFront, DiscardPile, NewDiscarPile),
    pushFront(DiscardFront, HumanHand, NewHumanHand),
    NewState = [Round, ComputerScore, ComputerHand, HumanScore, NewHumanHand, DrawPile, NewDiscarPile, NextPlayer, PlayersGoneOut],
    write("Human Picking from discard pile").

pickFromDiscardPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == computer,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    select(DiscardFront, DiscardPile, NewDiscarPile),
    pushFront(DiscardFront, ComputerHand, NewComputerHand),
    NewState = [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, DrawPile, NewDiscarPile, NextPlayer, PlayersGoneOut], 
    write("Computer picking from discard pile").

pickFromDiscardPile(State, _, NewState):-
    % [_, _, ComputerHand, _, HumanHand, _, DiscardPile, CurrentPlayer | _] = State,
    [_, _, _, _, _, _, _, CurrentPlayer | _] = State,
    pickFromDiscardPile(State, CurrentPlayer, NewState).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickFromDrawPile
%   Purpose: pick a card from draw pile and put in player's hand
%   Parameters: State, state of the game before picking card
%               CurrentPlayer, determines on which player's hand card is added
%               NewState, State of the game after the card is picked
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

pickFromDrawPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == human,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    select(DrawFront, DrawPile, NewDrawPile),
    pushFront(DrawFront, HumanHand, NewHumanHand),
    NewState = [Round, ComputerScore, ComputerHand, HumanScore, NewHumanHand, NewDrawPile, DiscardPile, NextPlayer, PlayersGoneOut],
    write("Human Picking from draw pile").

pickFromDrawPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == computer, 
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    select(DrawFront, DrawPile, NewDrawPile),
    pushFront(DrawFront, ComputerHand, NewComputerHand),
    NewState = [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, NewDrawPile, DiscardPile, NextPlayer, PlayersGoneOut],
    write("Computer picking from draw pile").

pickFromDrawPile(State, _,  NewState):-
    [_, _, _, _, _, _, _, CurrentPlayer | _] = State,
    pickFromDrawPile(State, CurrentPlayer, NewState).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: delete
%   Purpose: delete an elem from list
%   Parameters: Element, entity that will be delete from list passed
%               List, List from which element is deleted
%               NewList, List after element is deleted.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

delete(Element,[Element|Tail],Tail).

delete(Element,[Head|Tail],[Head|Tail1]) :-
    delete(Element,Tail,Tail1).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: throwToDiscardPile
%   Purpose: throws card from player's hand to discard pile
%   Parameters: State, state of the game before throwing card
%               CurrentPlayer, Player from whose hand card is thrown
%               NewState, State of the game after the card is thrown
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

throwToDiscardPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == human,
    write('Enter the card that you want to throw'),
    read(Card),
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = State,
    delete(Card, HumanHand, NewHumanHand),
    pushFront(Card, DiscardPile, NewDiscardPile),
    NewState = [Round, ComputerScore, ComputerHand, HumanScore, NewHumanHand, DrawPile, NewDiscardPile, computer, PlayersGoneOut],
    write("Throwing from human hand to discard pile").

% this predicate is not used
throwToDiscardPile(State, CurrentPlayer, NewState):-
    CurrentPlayer == computer,
    read(Card),
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = State,
    delete(Card, ComputerHand, NewComputerHand),
    pushFront(Card, DiscardPile, NewDiscardPile),
    NewState = [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, DrawPile, NewDiscardPile, human, PlayersGoneOut],
    write("\nThrowing from human hand to discard pile\n").

throwToDiscardPile(State, _, NewState):-
    [_, _, _, _, _, _, _, CurrentPlayer | _] = State,
    throwToDiscardPile(State, CurrentPlayer, NewState).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: divider
%   Purpose: Divides the passed list of cards. e.g [x,y,z] => [ [x] [x,y] [x,y,z] [y,z] [z] ]
%   Parameters: List, list of cards that is divided
%               DividedList, List containing the divided list of cards
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

divider([], []).
divider(List, DividedList):-
    [_|T] = List,
    divider(T, NewDividedList),
    divideHelper(List, [], HelperDividedList),
    append(HelperDividedList, NewDividedList, DividedList).
% Helper function used by divider
divideHelper([], _, []).
divideHelper(List, TempList, DividedList):-
    [H|T] = List,
    append(TempList, [H], NewTempList),
    divideHelper(T, NewTempList, NewDividedList),
    [NewTempList | NewDividedList ] = DividedList.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: addSpecialCards
%   Purpose: Adds special cards ([a,b]) to the passed list containing list of cards([ [x, y] [z]] ] ) eg. ([ [x, y] [z]] ] , [a,b] ) => [[x,y,a] [x,y,a,b] [z,a] [z,a,b]]
%   Parameters: DividedCards, the passed list of cards
%               Specialcards, list of cards containing jokers and wildcards ([a,b])
%               DividedCardsWSpCards:- Return value containing cards after special cards are added.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

addSpecialCards([], SpecialCards , [SpecialCards]).
addSpecialCards(DividedCards, SpecialCards, DividedCardsWSpCards):-
    [H | T] = DividedCards,
    divideSpCards(SpecialCards, [], DividedSpCards),
    performAddition(H, DividedSpCards, CardsAddedList),
    addSpecialCards(T, SpecialCards, NewDividedCardsWSpCards),
    append(CardsAddedList, NewDividedCardsWSpCards, DividedCardsWSpCards).
% Helper predicate that divides a list of special cards into list of list of special cards
divideSpCards([], _, []).
divideSpCards(Cards, TempCards , DividedSpCards):-
    [H|T] = Cards,
    append(TempCards, [H], NewTempCards),
    divideSpCards(T, NewTempCards, NewDividedSpCards),
    [NewTempCards|NewDividedSpCards] = DividedSpCards.
% Helper predicate for 
performAddition(Cards, [], [Cards]).
performAddition(Cards, DividedSpCards, CardsAddedList):-
    [H|T] = DividedSpCards,
    performAddition(Cards, T, NewCardsAddedList),
    append(Cards, H, NewCards),
    [NewCards | NewCardsAddedList] = CardsAddedList.



%----------------------------------------------------------------------------------------------------
%   Predicate Name: isJoker
%   Purpose: Checks if passed card is joker
%   Parameters: Card, Represents physical card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
isJoker(Card):-
    atom_chars(Card, List),
    [H, I] = List,
    char_type(I, digit),
    H == j.
%----------------------------------------------------------------------------------------------------
%   Predicate Name: isSpecialCard
%   Purpose: Checks if passed card if a wildcard or joker
%   Parameters: Card, Represents physical card
%               Round, round no. in the game used for checking wildcard.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
isSpecialCard(Card, _):- isJoker(Card).

isSpecialCard(Card, Round):- atom_chars(Card, List),
    [_,T] = List,
    atom_number(T, T_num),
    NumberOfCards is Round + 2,
    T_num == NumberOfCards.

isSpecialCard(Card, Round):- atom_chars(Card, List),
    [_,T] = List,
    T == k, 
    T_num = 13,
    NumberOfCards is Round + 2,
    T_num == NumberOfCards.
isSpecialCard(Card, Round):- atom_chars(Card, List),
    [_,T] = List,
    T == q, 
    T_num = 12,
    NumberOfCards is Round + 2,
    T_num == NumberOfCards.
isSpecialCard(Card, Round):- atom_chars(Card, List),
    [_,T] = List,
    T == j, 
    T_num = 11,
    NumberOfCards is Round + 2,
    T_num == NumberOfCards.
isSpecialCard(Card, Round):- atom_chars(Card, List),
    [_,T] = List,
    T == x, 
    T_num = 10,
    NumberOfCards is Round + 2,
    T_num == NumberOfCards.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: distributeCards
%   Purpose: Distribute a number of cards from one list of cards to another
%   Parameters: Num, Num of cards to distribute
%               CardsToDistribute, List of cards from which cards are distributed,
%               Hand, List in which cards are distributed, 
%               CardsAfterDistribution, cards left in CardsToDistribute after distributing cards from CardsToDistribute to Hand
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

distributeCards(0, CardsToDistribute, [], CardsAfterDistribution):- CardsAfterDistribution = CardsToDistribute. 

distributeCards(Num, CardsToDistribute, Hand, CardsAfterDistribution):-
    NewNum is Num - 1,
    [H | T] = CardsToDistribute,
    distributeCards(NewNum, T, NewHand, NewCardsDistribution),
    [H | NewHand] = Hand,
    NewCardsDistribution = CardsAfterDistribution.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: separateNormalAndSpecialCards
%   Purpose: Separates passed list of cards into Normal And Special Cards
%   Parameters: [H|T], List of cards that are separated into normal and special cards
%               Round, round no. of the game
%               NormalCards, list of cards that are not special cards. this value is returned by the predicate.
%               SpecialCards, list of cards containing jokers and wildcards. this value is returned by the predicate.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
separateNormalAndSpecialCards([], _, [], []).

separateNormalAndSpecialCards([H | T], Round, NormalCards, SpecialCards):-
    isSpecialCard(H, Round),
    separateNormalAndSpecialCards(T, Round,NormalCards, NewSpecialCards),
    [H | NewSpecialCards] = SpecialCards.
    
separateNormalAndSpecialCards([H|T], Round, NormalCards, SpecialCards):-
    \+ isSpecialCard(H, Round),
    separateNormalAndSpecialCards(T, Round, NewNormalCards, SpecialCards),
    [H | NewNormalCards] = NormalCards.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: checkSameNormal
%   Purpose: Checks if all the cards in passed collection have same face
%   Parameters: NormalCards, list of cards containing normal cards
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
checkSameNormal(NormalCards):-
    length(NormalCards, Length),
    Length =< 1.

checkSameNormal(NormalCards):-
    [H, I | T]  = NormalCards,
    getFace(H, HFace),
    getFace(I, IFace),
    HFace == IFace,
    checkSameNormal([I|T]).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: checkBook
%   Purpose: Checks if passed list of cards qualify for book
%   Parameters: CardsCol, List of cards
%               Round, round no. of the game
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
checkBook(Hand, Round):-
    length(Hand, Size),
    Size >= 3,
    separateNormalAndSpecialCards(Hand, Round, NormalCards, _),
    length(NormalCards, Length),
    Length == 0.

checkBook(Hand, Round):-
    length(Hand, Size),
    Size >= 3,
    separateNormalAndSpecialCards(Hand, Round, NormalCards, _),
    checkSameNormal(NormalCards).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: booksGenerator
%   Purpose: Helper predicate to generate books. Used by genBooks predicate.
%   Parameters: List containing list of cards. some of which are books.
%               Round, round no. of the game
%               BooksGenerated, Generated runs
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
booksGenerator([], _, []).
booksGenerator([H|T],Round, BooksGenerated):-
    checkBook(H, Round),
    booksGenerator(T, Round, NewBooksGenerated),
    [H|NewBooksGenerated] = BooksGenerated.
booksGenerator([H|T], Round, BooksGenerated):-
    \+ checkBook(H, Round),
    booksGenerator(T, Round, NewBooksGenerated),
    BooksGenerated = NewBooksGenerated.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: genBooks
%   Purpose: Generates list of possible books in the current hand
%   Parameters: CardsCol, List of cards
%               Round, round no. of the game
%               RunsGenerated, Generated runs
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

genBooks(CardsCol, Round, BooksGenerated):-
    separateNormalAndSpecialCards(CardsCol, Round, NormalCards, SpecialCards),
    sortCards(NormalCards,SortedCards),
    divider(SortedCards, DividedCards),
    addSpecialCards(DividedCards, SpecialCards, DividedCardsWSpCards),
    booksGenerator(DividedCardsWSpCards, Round, BooksGenerated).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: getSuit
%   Purpose: Get the suit of the card
%   Parameters: Card, represents a physical card
%               Suit, suit of the card that is returned
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
getSuit(Card, Suit):-
    atom_chars(Card, List),
    [Suit|_] = List. 

getFace(Card, Face):-
    atom_chars(Card, List),
    [_, Face] = List.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: separateSuits
%   Purpose: Separate a list of card into different suits (hearts, spades, tridents, clubs, diamonds)
%   Parameters: [H|T], list of cards that are separated into different suits
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
separateSuits([], [], [], [], [], []).

separateSuits([H|T], Hearts, Spades, Tridents, Clubs, Diamonds):-
    getSuit(H, Suit),
    Suit == c,
    separateSuits(T, Hearts, Spades, Tridents, NewClubs, Diamonds),
    [H| NewClubs] = Clubs.

separateSuits([H|T], Hearts, Spades, Tridents, Clubs, Diamonds):-
    getSuit(H, Suit),
    Suit == d,
    separateSuits(T, Hearts, Spades, Tridents, Clubs, NewDiamonds),
    [H| NewDiamonds] = Diamonds.

separateSuits([H|T], Hearts, Spades, Tridents, Clubs, Diamonds):-
    getSuit(H, Suit),
    Suit == t,
    separateSuits(T, Hearts, Spades, NewTridents, Clubs, Diamonds),
    [H| NewTridents] = Tridents.

separateSuits([H|T], Hearts, Spades, Tridents, Clubs, Diamonds):-
    getSuit(H, Suit),
    Suit == s,
    separateSuits(T, Hearts, NewSpades, Tridents, Clubs, Diamonds),
    [H| NewSpades] = Spades.
    
separateSuits([H|T], Hearts, Spades, Tridents, Clubs, Diamonds):-
    getSuit(H, Suit),
    Suit == h,
    separateSuits(T, NewHearts, Spades, Tridents, Clubs, Diamonds),
    [H| NewHearts] = Hearts.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: getValue
%   Purpose: Get value of the passed card.
%   Parameters: Card, Represents a physical card,
%               Round, round number in game
%               Value, Value of the card that is returned
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
getValue(Card, _, Value):-
    isJoker(Card),
    Value = 50.

getValue(Card, Round, Value):-
    getFace(Card, Face),
    atom_number(Face, Fnum),
    Fnum is Round + 2, 
    Value = 20.

getValue(Card, Round, Value):-
    getFace(Card, Face),
    Face == k,
    13 is Round + 2,
    Value = 20.
getValue(Card, Round, Value):-
    getFace(Card, Face),
    Face == q,
    12 is Round + 2,
    Value = 20.
getValue(Card, Round, Value):-
    getFace(Card, Face),
    Face == j,
    11 is Round + 2,
    Value = 20.
getValue(Card, Round, Value):-
    getFace(Card, Face),
    Face == x, 
    10 is Round + 2,
    Value = 20.

getValue(Card, _, Value):-
    getFace(Card, Face),
    Face == k,
    Value = 13.
getValue(Card, _, Value):-
    getFace(Card, Face),
    Face == q,
    Value = 12.
getValue(Card, _, Value):-
    getFace(Card, Face),
    Face == j,
    Value = 11.
getValue(Card, _, Value):-
    getFace(Card, Face),
    Face == x,
    Value = 10.
getValue(Card, _, Value):-
    getFace(Card, Face),
    
    atom_number(Face, FNum),
    FNum =< 9,
    Value = FNum.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: sameSuit
%   Purpose: Check if all the cards in the cards collection belong to same suit.
%   Parameters: CardsCol, list of cards
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
sameSuit(CardsCol):-
    length(CardsCol, Length),
    Length =< 1.

sameSuit(CardsCol):-
    [H, I | T]  = CardsCol,
    getSuit(H, HSuit),
    getSuit(I, ISuit),
    HSuit == ISuit,
    sameSuit([I|T]).


%----------------------------------------------------------------------------------------------------
%   Predicate Name: runValidatorHelper
%   Purpose: Helper predicate for runValidator.
%   Note: see runValidator. Need to fill details in.
%----------------------------------------------------------------------------------------------------
runValidatorHelper(2, [_|T], T).
runValidatorHelper(Difference, SpecialCards, NewSpecialCards):- 
    NewDifference is Difference - 1,
    [_|T] = SpecialCards,
    runValidatorHelper(NewDifference, T, NewNewSpecialCards),
    NewSpecialCards = NewNewSpecialCards.
%----------------------------------------------------------------------------------------------------
%   Predicate Name: runValidator
%   Purpose: Helper predicate for checkRun predicate. Helps in checking whether list of cards(L) qualify for run.
%   Parameters: Round, round no. of the game
%               NormalCards, Normal cards in the list(L)
%               SpecialCards, Jokers and wildcards in the list(L)
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
runValidator(_, NormalCards, _):- length(NormalCards, Length), Length =<1.
runValidator(Round, NormalCards, SpecialCards):-
    [H, I | T] = NormalCards,
    getValue(H, Round, HValue),
    getValue(I, Round, IValue),
    HValue is IValue - 1,
    runValidator(Round, [I|T], SpecialCards).

runValidator(Round, NormalCards, SpecialCards):-
    [H, I | T] = NormalCards,
    getValue(H, Round, HValue),
    getValue(I, Round, IValue),
    Difference is IValue - HValue,
    runValidatorHelper(Difference, SpecialCards, NewSpecialCards),
    runValidator(Round, [I|T], NewSpecialCards).
%----------------------------------------------------------------------------------------------------
%   Predicate Name: sortDelta
%   Purpose: Predicate passed to predsort 
%   Note: Loop predsort in swipl for more information
%----------------------------------------------------------------------------------------------------
sortDelta(<, E1, E2):-
    getValue(E1, 20, E1Value),
    getValue(E2, 20, E2Value),
    % write('E1Value, E2Value : '), write(E1Value), write(" , "), write(E2Value),nl,
    E1Value < E2Value.

sortDelta(>, E1, E2):- 
    getValue(E1, 20, E1Value),
    getValue(E2, 20, E2Value),
    E1Value >= E2Value.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: sortCards
%   Purpose: Sorts cards based on value
%   Parameters: CardsCol, List of cards
%               SortedCol, Sorted list of cards
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
sortCards(CardsCol, SortedCol):-
    predsort(sortDelta, CardsCol, SortedCol).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: checkRun
%   Purpose: Checks if passed list of cards qualify for run
%   Parameters: CardsCol, List of cards
%               Round, round no. of the game
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
checkRun(CardsCol, Round):-
    length(CardsCol, Length),
    Length >= 3,
    separateNormalAndSpecialCards(CardsCol, Round, NormalCards, _),
    length(NormalCards, NLength),
    NLength == 0.

checkRun(CardsCol, Round):-
    length(CardsCol, Length),
    Length >= 3,
    separateNormalAndSpecialCards(CardsCol, Round, NormalCards, SpecialCards),
    sortCards(NormalCards, SortedNormalCards),
    sameSuit(SortedNormalCards),
    runValidator(Round, SortedNormalCards, SpecialCards).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: runsGenerator
%   Purpose: Helper predicate for genRuns predicate. 
%   Parameters: [H|T], List containing list of cards. some of which are runs.
%               Round, round no. of the game
%               RunsGenerated, Generated runs
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

runsGenerator([], _, []).

runsGenerator([H|T],Round, RunsGenerated):-
    checkRun(H, Round),
    runsGenerator(T, Round, NewRunsGenerated),
    [H|NewRunsGenerated] = RunsGenerated.
runsGenerator([H|T], Round, RunsGenerated):-
    \+ checkRun(H, Round),
    runsGenerator(T, Round, NewRunsGenerated),
    RunsGenerated = NewRunsGenerated.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: genRuns
%   Purpose: Generates list of possible runs in the current hand
%   Parameters: CardsCol, List of cards
%               Round, round no. of the game
%               RunsGenerated, Generated runs
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
genRuns(CardsCol, Round, RunsGenerated):-
    separateNormalAndSpecialCards(CardsCol, Round, NormalCards, SpecialCards),
    separateSuits(NormalCards, Hearts, Spades, Tridents, Clubs, Diamonds),
    divider(Hearts, DividedHearts),
    divider(Spades, DividedSpades),
    divider(Tridents, DividedTridents),
    divider(Clubs, DividedClubs),
    divider(Diamonds, DividedDiamonds),
    addSpecialCards(DividedHearts, SpecialCards, DHSpecial),
    addSpecialCards(DividedSpades, SpecialCards, DSSpecial),
    addSpecialCards(DividedTridents, SpecialCards, DTSpecial),
    addSpecialCards(DividedClubs, SpecialCards, DCSpecial),
    addSpecialCards(DividedDiamonds, SpecialCards, DDSpecial),
    runsGenerator(DHSpecial, Round, HRuns),
    runsGenerator(DSSpecial, Round, SRuns),
    runsGenerator(DTSpecial, Round, TRuns),
    runsGenerator(DCSpecial, Round, CRuns),
    runsGenerator(DDSpecial, Round, DRuns),
    append([HRuns, SRuns, TRuns, CRuns, DRuns], RunsGenerated).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: genBooksAndRuns
%   Purpose: Generates list of possible books and runs in the current hand
%   Parameters: Hand, List of cards
%               Round, round no. of the game
%               BooksAndRuns, Generated books and runs
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
genBooksAndRuns(Hand, Round, BooksAndRuns):-
    genBooks(Hand, Round, BooksGenerated),
    genRuns(Hand, Round, RunsGenerated),
    append(BooksGenerated, RunsGenerated, BooksAndRuns).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: calculateScore
%   Purpose: Calculates the score of the cards
%   Parameters: Hand, List of cards
%               Round, round no. of the game
%               Score, Calculated score of the cards
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
calculateScore([], _, 0).
calculateScore(Hand,Round, Score):-
    [H|T] = Hand, 
    calculateScore(T, Round, NewScore),
    getValue(H, Round, Value),
    Score is NewScore + Value.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: selectMin
%   Purpose: Determines which branch among PrevBranch and NewBranch is the MinBranch
%   Parameters: PrevBranch, One of the branch that is compared
%               PrevScore, Score of the PrevBranch,
%               NewBranch, another branch in the comparison
%               NewScore, score of NewBranch
%               MinBranch, Minimum branch among PrevBranch and NewBranch that is returned,
%               MinScore, score of the MinBranch that is returned
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
selectMin(PrevBranch, PrevScore, _, NewScore, MinBranch, MinScore):-
    PrevScore =< NewScore, 
    MinScore = PrevScore,
    PrevBranch = MinBranch.

selectMin(_, PrevScore, NewBranch, NewScore, MinBranch, MinScore):-
    PrevScore > NewScore, 
    MinScore = NewScore,
    NewBranch = MinBranch.
%----------------------------------------------------------------------------------------------------
%   Predicate Name: customSelect
%   Purpose: Removes cards from hand if present in another list
%   Parameters: List, Hand from which cards are removed
%               [H|T], List which is compared to remove cards,
%               RemList, list that results are cards are removed from list
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
customSelect(List, [], List).
customSelect(List, [H|T], RemList):-
    select(H, List, BakiList),
    customSelect(BakiList, T, RemList).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: genBestCombo
%   Purpose: Helper function for generate the best possible combination of cards for the passed hand
%   Parameters: [H|T], List containing all the possible combinations of books and runs for the passed hand
%               Hand, whose best possible combination is calculated
%               Round, Used for determining wildcard in the hand
%               Branch, Intermediate variable, Initialize as empty list at first
%               MinBranch, best possible combination of the cards for the the passed hand that is returned
%               MinScore, score of the best possible combination
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
genBestCombo([], _, _, _, [], 999999999).
genBestCombo([H|T], Hand, Round, Branch , MinBranch, MinScore):-
    customSelect(Hand, H , RemHand),
    [H | Branch] = InterBranch,
    genBestMain(RemHand, Round, InterBranch, PrevBranch, PrevScore),
    genBestCombo(T, Hand, Round, Branch , NewBranch, NewScore),
    selectMin(PrevBranch, PrevScore, NewBranch, NewScore, MinBranch, MinScore).
    genBestMain([], _, Branch ,MinBranch, MinScore ):-
    MinScore = 0,
    MinBranch = Branch.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: genBestMain
%   Purpose: Calculates the best possible combination and score of the combination for passed hand
%   Parameters: Hand, whose best possible combination is calculated
%               Round, Used for determining wildcard in the hand
%               Branch, Intermediate variable, Initialize as empty list at first
%               MinBranch, best possible combination of the cards for the the passed hand that is returned
%               MinScore, score of the best possible combination
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
genBestMain(Hand, Round, Branch, MinBranch, MinScore):-
    genBooksAndRuns(Hand, Round, BooksAndRuns),
    length(BooksAndRuns, Length),
    Length == 0,
    calculateScore(Hand, Round, MinScore),
    [Hand|Branch] = MinBranch.

genBestMain(Hand, Round, Branch, MinBranch, MinScore):-
    genBooksAndRuns(Hand, Round, BooksAndRuns),
    genBestCombo(BooksAndRuns, Hand, Round, Branch , MinBranch, MinScore).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: con
%   Purpose: Concatenate two lists
%   Parameters: L1, First List
%               L2, Second List
%               L, Two lists concatenated
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

con([],L1,L1).

con([X|Tail],L2,[X|Tail1]):-
    con(Tail,L2,Tail1).


% SecondHalf contains all the element at first
%----------------------------------------------------------------------------------------------------
%   Predicate Name: calculateMinIndex
%   Purpose: Compares CurrScore and LastMinScore to determine RetMinIndex and RetMinScore
%   Parameters: CurrIndex is index in the current loop
%               LastMinIndex is index of the last minimum value in the recursive calls
%               CurrScore is the score of the current modified hand
%               LastMinScore is the score of the min prev hand in the loop
%               Returning minimum index
%               Returning minimum score
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
calculateMinIndex(_, LastMinIndex, CurrScore, LastMinScore, RetMinIndex, RetMinScore):-
    CurrScore > LastMinScore, 
    RetMinIndex = LastMinIndex, 
    RetMinScore = LastMinScore.
calculateMinIndex(CurrIndex, _, CurrScore, LastMinScore, RetMinIndex, RetMinScore):-
    CurrScore =< LastMinScore, 
    RetMinIndex = CurrIndex, 
    RetMinScore = CurrScore.


% Note: SecondHalf contains all the element at first
%----------------------------------------------------------------------------------------------------
%   Predicate Name: getMinIndexHelper
%   Purpose: To determine if a card picked would result in minimum score when replaced with a card from a list of cards
%   Parameters: FirstHalf and SeconHalf combined result in a list of cards
%               PickedCard, the card which will be compared against
%               Round, For checking wildcards
%               CurrIndex, Intermediated variable for keeping track of index of the list,
%               CSBeforePick, -- might not be needed
%               RetMinIndex, Index of the card which when replaced with picked card results in minimum score
%               RetMinScore, the minimum score that card picked results.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
getMinIndexHelper(_, [], _, _, _, CSBeforePick, -1, CSBeforePick).
getMinIndexHelper(FirstHalf, SecondHalf, PickedCard, Round, CurrIndex, CSBeforePick, RetMinIndex, RetMinScore):-
    [H | T] = SecondHalf,
    append(FirstHalf, [H], NewFirstHalf),
    append(FirstHalf, [PickedCard], ModFirstHalf),
    append(ModFirstHalf, T, ModArr),
    genBestMain(ModArr, Round, [], _, CurrMinScore),
    NewIndex is CurrIndex + 1,
    getMinIndexHelper(NewFirstHalf, T, PickedCard, Round, NewIndex, CSBeforePick, NewRetMinIndex, NewRetMinScore),
    calculateMinIndex(CurrIndex, NewRetMinIndex, CurrMinScore, NewRetMinScore, RetMinIndex, RetMinScore ).


%----------------------------------------------------------------------------------------------------
%   Predicate Name: getCardAtIndex
%   Purpose: Get card at certain index in the list
%   Parameters: Index, Index at which to get card at
%               CustomIndex, Intermediate variable that keep tracks of the index 
%               HumanHand, List of cards at human hand
%               Card, Card returned when the predicate is fulfilled
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

getCardAtIndex(Index, Index, [H|_], H).
getCardAtIndex(Index, CustomIndex, HumanHand, Card):-
    NewCustomIndex is CustomIndex + 1,
    [_|T] = HumanHand, 
    getCardAtIndex(Index, NewCustomIndex, T, NewCard),
    Card = NewCard.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickSuggestion
%   Purpose: Suggests to pick from draw pile
%   Parameters: Index, Index of the card that when replaced with the picked card would result in minimum score,
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
pickSuggestion(Index, _, _, _):-
    Index == -1,
    write('\nI suggest you to pick from draw pile\n').


%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickSuggestion
%   Purpose: Tell why picking card from discard pile would reduce minimum score for human
%   Parameters: Index, Index of the card that when replaced with the picked card would result in minimum score,
%               HumanHand, Collection of cards in human player's hand
%               RetMinScore, Score of the player when human picks a card from discard pile and replaces a hand in their hand,
%               DiscardFront, Card present at the top of discard piles
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

pickSuggestion(Index, HumanHand, RetMinScore, DiscardFront):- 
    getCardAtIndex(Index, 0,  HumanHand, Card),
    write('\nYour current hand is : '), write(HumanHand), nl,
    write('\nI suggest you to replace '), write(Card) , write(' with '), write(DiscardFront), write(' from discard pile \n'),
    write('\n Picking from discard pile reduces the min score to '), write(RetMinScore), nl.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: askPickHelp
%   Purpose: For human to  ask help on which card to pick
%   Parameters: State
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
askPickHelp(State, _, _):-
    [Round, _, _, _, HumanHand, _, DiscardPile, _, _] = State,
    select(DiscardFront, DiscardPile,_),
    genBestMain(HumanHand, Round, [], _, CurrScore),
    write('The current score in your hand is: '), write(CurrScore), nl,
    getMinIndexHelper([], HumanHand, DiscardFront, Round, 0, CurrScore, RetMinIndex, RetMinScore),
    pickSuggestion(RetMinIndex, HumanHand, RetMinScore, DiscardFront),
    write('\nHelp was provided\n').

%----------------------------------------------------------------------------------------------------
%   Predicate Name: away
%   Purpose: Deletes elem at index. Function was copied from internet and modified to work on 0 based index
%   Parameters: none
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
away([_|H],0,H):-!.
away([G|H],N,[G|L]):- N > 0, Nn is N - 1,!,away(H,Nn,L).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: getIndxToThrow
%   Purpose: Base case.
%   Parameters: none
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
getIndxToThrow(_, [], _, _, -1, 999999).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: getIndxToThrow
%   Purpose: Get index of the card which when thrown results in minimum score 
%   Parameters: FirstHalf and SecondHalf combined contains the list of cards
%               Round Determines wildcard in the list
%               CurrIndex Helps determining index which results in min score
%               RetMinIndex Index of the card which when thrown results in minimum score 
%               RetMinScore Best possible minimum score of the list of cards after throwing the card at RetMinIndex
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
getIndxToThrow(FirstHalf, SecondHalf, Round, CurrIndex, RetMinIndex, RetMinScore):-
    [H | T] = SecondHalf,
    append(FirstHalf, [H], NewFirstHalf),
    away(NewFirstHalf, CurrIndex, ModFirstHalf),
    append(ModFirstHalf, T, ModArr),
    genBestMain(ModArr, Round, [], _, CurrMinScore),
    NewIndex is CurrIndex + 1,
    getIndxToThrow(NewFirstHalf, T, Round, NewIndex, NewRetMinIndex, NewRetMinScore),
    calculateMinIndex(CurrIndex, NewRetMinIndex, CurrMinScore, NewRetMinScore, RetMinIndex, RetMinScore ).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: askThrowHelp
%   Purpose: Human asks help for throwing the card
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
askThrowHelp(State):-
    [Round, _, _, _, HumanHand, _, _, _, _] = State,
    getIndxToThrow([], HumanHand, Round, 0, RetMinIndex, RetMinScore),
    getCardAtIndex(RetMinIndex, 0,  HumanHand, Card),
    write('\nThrowing '), write(Card), write(' reduces minimum score to '), write(RetMinScore), nl.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: choosePick
%   Purpose: Pick card from Discard Pile.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, State After user has picked the card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
choosePick(Option, State, NewState):-
    Option ==1, pickFromDiscardPile(State, _, NewState).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: choosePick
%   Purpose: Pick card from Draw Pile.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, State After user has picked the card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
choosePick(Option, State, NewState):-
    Option ==2, pickFromDrawPile(State, _, NewState),
    write('Pick from discard pile'), nl.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: choosePick
%   Purpose: Ask help for picking . Calls askOptionToPick predicate after help has been provided
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, State After user has picked the card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
choosePick(Option, State, NewState):-
    Option ==3, askPickHelp(State, _, _),
    write('Ask pick help'), nl,
    askOptionToPick(State, NewNewState),
    NewState = NewNewState.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: choosePick
%   Purpose: Save the game state in file
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

choosePick(Option, State, _):- 
    Option == 4, 
    saveGame(State).


%----------------------------------------------------------------------------------------------------
%   Predicate Name: askOptionToPick
%   Purpose: Take human input for picking card from draw pile or discard pile, ask for help picking and save the game
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               PickedState, Modified State after human picks a card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

askOptionToPick(State, PickedState):-
    write('1. Pick from Discard Pile'), nl,
    write('2. Pick from Draw Pile'), nl,
    write('3. Ask for Help'), nl,
    write('4. Save Game'), nl,
    % write('Enter Option: '),
    % read(Option),
    inputValidtor(1,4,Option),
    choosePick(Option, State, PickedState).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: chooseThrow
%   Purpose: Throw the card from human's hand.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, Modified State after human throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
chooseThrow(Option,State, NewState):-
    Option ==1 , throwToDiscardPile(State, _ , NewState).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: chooseThrow
%   Purpose: Human asks help to computer for throwing the card. Calls askOptionToThrow predicate after help is provided.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, Modified State after human throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
chooseThrow(Option, State, NewState):-
    Option == 2, askThrowHelp(State),
    askOptionToThrow(State, NewNewState),
    NewState = NewNewState.


%----------------------------------------------------------------------------------------------------
%   Predicate Name: askOptionToThrow
%   Purpose: To throw card from human hand
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               ThrownState, Modified State after human throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
askOptionToThrow(State, ThrownState):-
    write('\n1. Throw Card\n'),
    write('\n2. Ask for Help\n'),
    % read(Option),
    inputValidtor(1,2, Option),
    chooseThrow(Option, State, ThrownState).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: humanMove
%   Purpose: For human to pick a card and throw the card.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, Modified State after human picks and throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

humanMove(State, NewState):-
    nl, write('Human is playing'), nl,
    printGameDetails(State),
    askOptionToPick(State, PickedState),
    printGameDetails(PickedState),
    askOptionToThrow(PickedState, ThrownState),
    printGameDetails(ThrownState),
    NewState = ThrownState,
    write('\n Human made a move\n').



%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickACardHelper
%   Purpose: Helper function that helps computer decide picking a card
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc. 
%               NewState, Modified State after computer picks a card,
%               Index, Index of the card to pick
%               RetMinScore, minimum score of the hand after replacing a card with picked card
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
pickACardHelper(Index, State, NewState, _):-
    Index == -1,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    [H | T] = DrawPile,
    [H | ComputerHand] = NewComputerHand, 
    [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, T, DiscardPile, NextPlayer, PlayersGoneOut] = NewState,
    write('\nComputer chose to pick card from draw pile\n').

pickACardHelper(Index, State, NewState , RetMinScore):- 
    getCardAtIndex(Index, 0,  ComputerHand, Card),
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    [H | T ] = DiscardPile,
    write('\nComputers current hand is : '), write(ComputerHand), nl,
    write('\nReplacing '), write(Card) , write(' with '), write(H), write(' from discard pile would reduce the minimum score in computers hand\n'),
    write('\n Picking from discard pile reduces the min score to '), write(RetMinScore), nl,
    [H | ComputerHand] = NewComputerHand, 
    [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, DrawPile, T, NextPlayer, PlayersGoneOut] = NewState.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: pickACard
%   Purpose: For computer to decide which card to pick
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc. 
%               NewState, Modified State after computer picks a card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

pickACard(State, NewState):-
    write('\n-------------------------------- Computer Play - Pick Card ------------------------------------\n'),
     [Round, _, ComputerHand, _, _, _,DiscardPile, _, _] = State,
     select(DiscardFront, DiscardPile,_),
     genBestMain(ComputerHand, Round, [], _, CurrScore),
     write('Current score in computers hand is: '), write(CurrScore), nl,
     getMinIndexHelper([], ComputerHand, DiscardFront, Round, 0, CurrScore, RetMinIndex, RetMinScore),
     pickACardHelper(RetMinIndex, State, NewState, RetMinScore),
     write('Computer picked card'), nl.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: computerThrowCard
%   Purpose: For computer to decide which card to throw
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc. 
%               NewState, Modified State after computer throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

computerThrowCard(State, NewState):-
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = State,
    getIndxToThrow([], ComputerHand, Round, 0, RetMinIndex, RetMinScore),
    getCardAtIndex(RetMinIndex, 0,  ComputerHand, Card),
    write('\nThrowing '), write(Card), write(' reduces minimum score to '), write(RetMinScore), nl,
    away(ComputerHand, RetMinIndex,NewComputerHand),
    [Card | DiscardPile] = NewDiscardPile,
    [Round, ComputerScore, NewComputerHand, HumanScore, HumanHand, DrawPile, NewDiscardPile, human, PlayersGoneOut] = NewState.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: computerMove
%   Purpose: For computer to play the game. Computer uses its strategy to play the game.
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%               NewState, Modified State after computer picks and throws the card.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

computerMove(State, NewState):-
    nl, write('--------------------------Computer is playing---------------------------------'), nl,
    pickACard(State, PickedState),
    computerThrowCard(PickedState, NewState),
    write('------------------------------Computer made a move--------------------------------').

%----------------------------------------------------------------------------------------------------
%   Predicate Name: printGameDetails
%   Purpose: To print the state of the game
%   Parameters: State, Array containing game state like Round, ComputerScore, ComputerHand , HumanScore, etc.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
printGameDetails(State):-
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = State,
    format("Round : ~w\n", Round),
    format("Computer Score : ~w\n", ComputerScore),
    formatList(ComputerHand, "Computer Hand"),
    format("Human Score: ~w\n", HumanScore),
    formatList(HumanHand, "Human Hand"),
    formatList(DrawPile, "Draw Pile"),
    formatList(DiscardPile, "Discard Pile"),
    format('NextPlayer: ~w\n', NextPlayer),
    format('PlayersGoneOut: ~w\n', PlayersGoneOut).
% Used by printGameDetails
formatListHelper([]).
formatListHelper([H|T]):-
    format("~w ", H),
    formatListHelper(T).

formatList(List, String):-
    format("~w : ", String),
    formatListHelper(List),
    format("~n").

%----------------------------------------------------------------------------------------------------
%   Predicate Name: tossHelper
%   Purpose: Helper function for toss. Predicate for when human wins the toss.
%   Parameters: NextPlayer, Player who wins the toss, 
%               HeadOrTail, 0 for head and 1 for tail inputted by user 
%   Local Variables: R, Random number between 0 and 1
%----------------------------------------------------------------------------------------------------
tossHelper(NextPlayer, HeadOrTail):-
    random_between(0, 1, R),
    R == HeadOrTail, 
    write('Got : '), write(R), nl, 
    write('Human won the toss'), nl, 
    NextPlayer = human.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: tossHelper
%   Purpose: Helper function for toss. Predicate for when computer wins the toss.
%   Parameters: NextPlayer, Player who wins the toss, 
%               HeadOrTail, 0 for head and 1 for tail inputted by user 
%   Local Variables: R, Random number between 0 and 1
%----------------------------------------------------------------------------------------------------

tossHelper(NextPlayer, HeadOrTail):-
    random_between(0, 1, R),
    R \== HeadOrTail, 
    write('Got : '), write(R), nl, 
    write('Computer won the toss'), nl, 
    NextPlayer = computer.  

%----------------------------------------------------------------------------------------------------
%   Predicate Name: toss
%   Purpose: Ask human user for toss
%   Parameters: NextPlayer returns who won the toss
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

toss(NextPlayer):-
    write('Press 1 for heads  and 0 for tails '),
    % read(HeadOrTail),
    inputValidtor(0, 1, HeadOrTail),
    tossHelper(NextPlayer, HeadOrTail).
    
%----------------------------------------------------------------------------------------------------
%   Predicate Name: startOption
%   Purpose: To load game from file
%   Parameters: Option, 2 for loading from file
%   Local Variables: State, stores game state in memory
%----------------------------------------------------------------------------------------------------
startOption(Option):- 
    Option == 2,
    write('Enter filename: '),
    read(FileName),
    parseGame(State, FileName),
    playRoundLoop(State).

%----------------------------------------------------------------------------------------------------
%   Predicate Name: startOption
%   Purpose: Start a new game.
%   Parameters: Option, 1 for starting new game
%   Local Variables: State, Loaded from new_game text file which starts at round 0, 
%                   NewState, State after starting a new round
%                   TossState, State after toss
%----------------------------------------------------------------------------------------------------
startOption(Option):-
    Option == 1,
    parseGame(State, 'new_game.txt'),
    startNewRound(State, NewState),
    toss(NextPlayer), 
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, _, PlayersGoneOut] = NewState,
    [Round, ComputerScore, ComputerHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer, PlayersGoneOut] = TossState,
    nl, printGameDetails(TossState), nl,
    nl, write('Starting new Game'), nl,
    playRoundLoop(TossState).


%----------------------------------------------------------------------------------------------------
%   Predicate Name: startNewOrLoad
%   Purpose: Provide option to load or start new game.
%   Parameters: none
%   Local Variables: Option input from the user
%----------------------------------------------------------------------------------------------------
startNewOrLoad():-
    write('1. Start New Game '), nl,
    write('2. Load Game'), nl,   
    % read(Option),
    inputValidtor(1,2, Option),
    startOption(Option).



%----------------------------------------------------------------------------------------------------
%   Predicate Name: inputValidtor
%   Purpose: Asks user for numeric input in certain range and validates it
%   Parameters: 
%           X, Start of the range (inclusive)
%           Y, End of the range (inclusive)
%           Value, Value between the range entered.
%   Local Variables: none
%----------------------------------------------------------------------------------------------------

inputValidtor(X, Y, Value):- 
    write('Enter option: '),
    read(Option),
    inputValidtorHelper(Option, X, Y, Value).

% Helper functions used by inputValidtor
inputValidtorHelper(Option, X, Y, Value):- 
    \+ number(Option),
    inputValidtor(X, Y , NewValue),
    Value = NewValue.
inputValidtorHelper(Option, X, Y, Value):- 
    Option >= X,
    Option =<Y, 
    Value = Option.
inputValidtorHelper(Option, X, Y, Value):-
    Option > Y,
    inputValidtor( X, Y, NewValue),
    Value = NewValue.
inputValidtorHelper(Option, X, Y, Value):-
    Option < X, 
    inputValidtor( X, Y, NewValue),
    Value = NewValue.

%----------------------------------------------------------------------------------------------------
%   Predicate Name: main
%   Purpose: Entry point of the game
%   Parameters: none
%   Local Variables: none
%----------------------------------------------------------------------------------------------------
main:-
    startNewOrLoad().

