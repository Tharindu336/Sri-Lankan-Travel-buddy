:- dynamic user_answer/2.

% ===================================================
% Sri Lanka Travel Buddy 
% ---------------------------------------------------
% The format is basically:
%   destination(Name, Area, Region, Season, Budget, Activity).

% --- Beaches ---
destination(unawatuna, beach, south, winter, budget, surfing).
destination(mirissa, beach, south, winter, luxury, relaxing).
destination(hikkaduwa, beach, south, winter, budget, surfing).
destination(bentota, beach, south, winter, luxury, relaxing).
destination(weligama, beach, south, winter, budget, surfing).
destination(arugam_bay, beach, east, summer, budget, surfing).
destination(nilaveli, beach, east, summer, luxury, relaxing).
destination(trincomalee, beach, east, summer, budget, sightseeing).
destination(passikudah, beach, east, summer, luxury, relaxing).
destination(negombo, beach, west, winter, budget, relaxing).
destination(kalpitiya, beach, west, summer, luxury, surfing).

% --- Mountains ---
destination(nuwara_eliya, mountain, central, spring, luxury, hiking).
destination(ella, mountain, central, spring, budget, hiking).
destination(haputale, mountain, central, spring, budget, hiking).
destination(bandarawela, mountain, central, spring, budget, hiking).
destination(belihuloya, mountain, central, spring, budget, hiking).
destination(kithulgala, mountain, west, spring, budget, hiking).

% --- Cultural / Heritage ---
destination(kandy, cultural, central, autumn, luxury, sightseeing).
destination(anuradhapura, cultural, north, autumn, budget, sightseeing).
destination(polonnaruwa, cultural, north, autumn, budget, sightseeing).
destination(sigiriya, cultural, central, autumn, luxury, sightseeing).
destination(dambulla, cultural, central, autumn, budget, sightseeing).
destination(galle, cultural, south, winter, luxury, sightseeing).
destination(colombo, cultural, west, winter, luxury, sightseeing).
destination(matara, cultural, south, winter, budget, sightseeing).
destination(batticoloa, cultural, east, summer, budget, sightseeing).
destination(jaffna, cultural, north, autumn, budget, sightseeing).
destination(udawalawa, cultural, south, winter, budget, sightseeing).
destination(mahiyanganaya, cultural, central, autumn, budget, sightseeing).

% ===================================================
% Starting point
% ===================================================
go:-  
    write('======================================='), nl,
    write('  Welcome to Sri Lanka Travel Explorer  '), nl,
    write('======================================='), nl, nl,
    write('Tell us a bit about your travel vibes, I\'ll try to suggest something.'), nl, nl,

    ask_stuff, nl,

    % gather all possible places
    findall(D, destination(D, _, _, _, _, _), EveryDest),

    % grab what the user picked
    user_answer(area, A1),
    user_answer(region, R1),
    user_answer(season, S1),
    user_answer(budget, B1),
    user_answer(activity, Act1),

    MyPrefs = prefs(A1, R1, S1, B1, Act1),

    % --- try exact match first- DFS---
    nl, write('Okay, checking for the best possible match...'), nl, nl,
    ( find_match(EveryDest, Exact, MyPrefs) ->
        write('Looks like your dream spot is:'), nl,
        format('   -> ~w~n', [Exact])
    ;   
        write('Hmm, nothing matches exactly what you said.'), nl,
        Exact = none
    ),

    % --- otherwise, suggest close-ish ones  BFS---
    find_similar(EveryDest, MyPrefs, MaybeList0),

    % remove the exact match from similar list
    ( Exact \= none ->
        delete(MaybeList0, Exact, TempList)
    ;   
        TempList = MaybeList0
    ),

    % also remove any duplicates in the list
    sort(TempList, MaybeList),

    ( MaybeList \= [] ->
        nl, write('Still, you might enjoy these too:'), nl,
        print_list(MaybeList)
    ;   
        nl, write('No backup ideas... maybe tweak your answers and try again.'), nl
    ).

% ===================================================
% Ask questions from the user
% ===================================================
ask_stuff :- 
    ask(season,  'When are you planning to go?', 
        [summer, winter, spring, autumn]),
    ask(budget,  'Travel style: budget or luxury?', 
        [budget, luxury]),
    ask(area,    'What kind of area do you want?', 
        [beach, mountain, cultural]),
    ask(region,  'Any specific region?', 
        [south, central, east, north, west]),
    ask(activity,'Pick your favorite thing to do:', 
        [surfing, hiking, sightseeing, relaxing]).

ask(Key, Msg, Options) :-
    write(Msg), nl,
    write('Options: '), write(Options), nl,
    read(Ans),
    retractall(user_answer(Key, _)),
    assert(user_answer(Key, Ans)).

% ===================================================
% Exact match search (DFS)
% ===================================================
find_match([X|_], X, Prefs) :- all_match(X, Prefs), !.
find_match([_|Rest], Result, Prefs) :- find_match(Rest, Result, Prefs).

all_match(D, prefs(A,R,S,B,Act)) :-
    destination(D, Area, Region, Season, Budget, Activity),
    A == Area, R == Region, S == Season, B == Budget, Act == Activity.

% ===================================================
% Similar matches (BFS)
% ===================================================
find_similar(Dests, prefs(A,R,S,B,Act), Results) :-
    loop_suggest(Dests, [A,R,S,B,Act], [], Results).

loop_suggest([], _, Acc, Acc).
loop_suggest([D|Rest], Prefs, Acc, Results) :-
    destination(D, Area, Region, Season, Budget, Activity),
    get_score([Area,Region,Season,Budget,Activity], Prefs, Points),
    ( Points >= 3 -> NewAcc = [D|Acc] ; NewAcc = Acc ),   % threshold = 3
    loop_suggest(Rest, Prefs, NewAcc, Results).

% scoring helper
get_score([], [], 0).
get_score([H1|T1], [H2|T2], Score) :-
    get_score(T1, T2, SubScore),
    ( H1 == H2 -> Score is SubScore + 1 ; Score = SubScore ).

% ===================================================
% Printing results
% ===================================================
print_list([]).
print_list([D|Rest]) :-
    write('   -> '), write(D), nl,
    print_list(Rest).
