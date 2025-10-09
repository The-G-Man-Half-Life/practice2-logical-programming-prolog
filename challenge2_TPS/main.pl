:- module(,[
    print_All/2,
    print_All/4,
    print_Cheapest/2,
    print_Cheapest/4,
    print_Fastest/2,
    print_Fastest/4
]).

% "Import" 
:- use_module(routes_Logic).
:- use_module(outputs).
:- use_module(filters_Optimals).


% Calculates ALL Available Routes Between Two Cities
% 	Without Time Filter
% print_All(Origin, Destiny).
print_All(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    print_Routes(Routes).
%	With Time Filter
% print_All(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_All(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    print_Routes(Filtered).


% Calculates CHEAPEST Route Between Two Cities
% 	Without Time Filter
% print_Cheapest(Origin, Destiny).
print_Cheapest(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    find_Optimal(Routes, cost, Result),
    print_Routes([Result]).
% 	With Time Filter
% print_Cheapest(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_Cheapest(Origin, Destiny, Min_Departure, Max_Departure) :-
	find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, cost, Result),
    print_Routes([Result]).


% Calculates FASTEST Route Between Two Cities
% 	Without Time Filter
% print_Fastest(Origin, Destiny).
print_Fastest(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    find_Optimal(Routes, time, Result),
    print_Routes([Result]).
% 	With Time Filter
% print_Fastest(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_Fastest(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, time, Result),
    print_Routes([Result]).
