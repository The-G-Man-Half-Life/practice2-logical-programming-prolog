%		---FACTS---
% route(Origin, Destiny, Transport, Departure_Time, Arrival_Time, Cost_USD, Available).
route(bogota, medellin, avion, 8, 9, 100, si).
route(bogota, medellin, bus, 6, 16, 40, si).
route(medellin, cartagena, avion, 10, 11.5, 120, no).
route(medellin, cartagena, bus, 7, 19, 42, si).
route(cartagena, santa_marta, bus, 14, 18, 30, si).
route(bogota, santa_marta, bus, 7, 23, 105, no).



%		---RULES & FUNCTIONS---
%	ROUTES
% Direct Route
routes(Origin, Destiny, _Visited, [segment(Origin, Destiny, Transport, Cost, Departure_Time, Arrival_Time)], Cost, Time) :-
    route(Origin, Destiny, Transport, Departure_Time, Arrival_Time, Cost, si),
    Time is Arrival_Time - Departure_Time.

% Route With Stepovers
routes(Origin, Destiny, Visited, [segment(Origin, Stepover, Transport, Segment_Cost, Departure_Time, Arrival_Time) | Route], Total_Cost, Total_Time) :-
    route(Origin, Stepover, Transport, Departure_Time, Arrival_Time, Segment_Cost, si),
    % To Stop Loops We Use
    \+ member(Stepover, Visited),
    routes(Stepover, Destiny, [Stepover | Visited], Route, Remaining_Cost, Remaining_Time),
    % Calculate Cost & Time
    Total_Cost is
    	Segment_Cost + Remaining_Cost,
    Total_Time is
    	(Arrival_Time - Departure_Time) + Remaining_Time.



%	OUTPUTS
% Prints all Segments and Stepovers of a given Route
print_Segments([]).
print_Segments([segment(Origin, Destiny, Transport, Cost, Departure_Time, Arrival_Time) | Segments]) :-
    % To Print: "Origin" ("Transport", "Departure_Time"->"Arrival_Time", "Cost"USD)--> Arrival_Place
    format(
        "~w (~w, ~w->~w, ~1f USD)--> ~w",
        [Origin, Transport, Departure_Time, Arrival_Time, Cost, Destiny]),nl,
    print_Segments(Segments).

% Prints all Routes Found Between Two Cities
print_Routes([]).
print_Routes([travel(Route, Cost, Time) | Travels]) :-
    print_Segments(Route),
    write("---"),nl,
    format("Costo Total: ~w~n", Cost),
    format("Tiempo: ~wh~n", Time),
    nl,print_Routes(Travels).



%	CALCULATIONS & FILERS
% Finds all Available Routes Between Two Cities and returns them as an Array
find_All_Routes(Origin, Destiny, Routes) :-
    % We use findall to get all the possible Routes and returning them in an Array.
    findall(
        travel(Route, Cost, Time),
        routes(Origin, Destiny, [Origin], Route, Cost, Time),
        Routes).

% Given a set of Routes, finds the Cheapest or Fastest one
find_Optimal([travel(Route, Cost, Time)], _, travel(Route, Cost, Time)).
find_Optimal([travel(Route_1, Cost_1, Time_1), travel(Route_2, Cost_2, Time_2) | Rest], Filter, Result) :-
    %  Depending on Filter we Assign Value
    (Filter == cost ->
    	Value_1 is Cost_1, Value_2 is Cost_2;
    Filter == time ->
    	Value_1 is Time_1, Value_2 is Time_2),
    % And then we Compare Values
    (Value_1 =< Value_2 ->
    	find_Optimal([travel(Route_1, Cost_1, Time_1) | Rest], Filter, Result);
    	find_Optimal([travel(Route_2, Cost_2, Time_2) | Rest], Filter, Result)).

% Returns:
% 	- False when a Segment is found to depart outside allowed range
% 	- True, when all Segments are in range
filter_Departure_Time_Route([], _, _).
filter_Departure_Time_Route([segment(_O, _D, _T, _C, Departure_Time, _A) | Rest], Min_Departure, Max_Departure) :-
	Min_Departure =< Departure_Time, Departure_Time =< Max_Departure,
	filter_Departure_Time_Route(Rest, Min_Departure, Max_Departure).

% Filters an Array of Routes To Get 
filter_Departure_Time([], _, _, []).
filter_Departure_Time([travel(Route, Cost, Time) | Rest], Min_Departure, Max_Departure, Result) :-
    (filter_Departure_Time_Route(Route, Min_Departure, Max_Departure) ->
    	filter_Departure_Time(Rest, Min_Departure, Max_Departure, Filtered),
        Result = [travel(Route, Cost, Time) | Filtered];
    	filter_Departure_Time(Rest, Min_Departure, Max_Departure, Result)).





%		--- CALLER INDEPENDENT FUNCTIONS ---
% Calculates ALL Available Routes Between Two Cities
%	With Time Filter
print_All(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    print_Routes(Filtered).
% 	Without Time Filter
print_All(Origin, Destiny) :- print_All(Origin, Destiny, 0, 24).


% Calculates CHEAPEST Route Between Two Cities
% 	With Time Filter
print_Cheapest(Origin, Destiny, Min_Departure, Max_Departure) :-
	find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, cost, Result),
    print_Routes([Result]).
% 	Without Time Filter
print_Cheapest(Origin, Destiny) :- print_Cheapest(Origin, Destiny, 0, 24).


% Calculates FASTEST Route Between Two Cities
% 	With Time Filter
print_Fastest(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, time, Result),
    print_Routes([Result]).
% 	Without Time Filter
print_Fastest(Origin, Destiny) :- print_Fastest(Origin, Destiny, 0, 24).