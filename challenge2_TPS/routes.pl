%		---FACTS---
% route(Origin, Destiny, Transport, Departure_Time, Arrival_Time, Cost_USD, Available).
route(bogota, medellin, avion, 8, 9, 100, si). % Bogotá Routes
route(bogota, medellin, bus, 6, 16, 40, si).
route(bogota, santa_marta, bus, 7, 23, 105, no).
route(bogota, cartagena, bus, 6, 22, 90, no).
route(medellin, bogota, avion, 12, 13, 100, si). % Medellín Routes
route(medellin, cartagena, avion, 10, 11.5, 120, no).
route(medellin, cartagena, bus, 7, 19, 42, si).
route(medellin, caucacia, bus, 5, 12, 50, si).
route(cartagena, medellin, bus, 6, 18, 45, si). % Cartagena Routes
route(cartagena, santa_marta, bus, 14, 18, 30, si).
route(sincelejo, covenas, bus, 7, 8, 15, si).
route(caucacia, sincelejo, bus, 9, 13, 20, si).
route(santa_marta, cartagena, bus, 8, 10, 27.8, si). % Santa Marta Routes
route(santa_marta, cartagena, avion, 13, 13.4, 70, si).





%		---RULES & FUNCTIONS---
% ROUTES
% 	Checks if there's an avaiable route from a given Origin to a Destiny, direct or with Stepovers, 
% 	calculates the total cost, time and each segments to return them in Variables and Arrays.
% routes(Origin, Destiny, [Visited_Array], [Segments_Array], Cost, Time).
% Direct Route between two cities
routes(Origin, Destiny, _Visited, [segment(Origin, Destiny, Transport, Cost, Departure_Time, Arrival_Time)], Cost, Time) :-
    route(Origin, Destiny, Transport, Departure_Time, Arrival_Time, Cost, si),
    Time is Arrival_Time - Departure_Time.

% Route With Stepovers
routes(Origin, Destiny, Visited, [segment(Origin, Stepover, Transport, Segment_Cost, Departure_Time, Arrival_Time) | Route], Total_Cost, Total_Time) :-
    route(Origin, Stepover, Transport, Departure_Time, Arrival_Time, Segment_Cost, si),
    % To avoid loops we check the next stepover with the array:
    \+ member(Stepover, Visited),
    routes(Stepover, Destiny, [Stepover | Visited], Route, Remaining_Cost, Remaining_Time),
    % Calculate Cost & Time
    Total_Cost is
    	Segment_Cost + Remaining_Cost,
    Total_Time is
    	(Arrival_Time - Departure_Time) + Remaining_Time.





%	--- OUTPUTS ---
% Prints a single Segment of a Route
% print_Segment(segment(Origin, Destiny, Transport_Method, Cost, Departure_Time, Arrival_Time)).
print_Segment(segment(Origin, Destiny, Transport, Cost, Departure_Time, Arrival_Time)) :-
    format(
        "~w (~w, ~w->~w, ~1f USD)--> ~w",
        [Origin, Transport, Departure_Time, Arrival_Time, Cost, Destiny]),
    nl.

% Prints all Segments and Stepovers of a given array (Route)
% print_Segments([Segments_Array]).
print_Segments([]).
print_Segments([Segment | Rest]) :-
    print_Segment(Segment),
    print_Segments(Rest).

% Prints a single given Route
% print_Route(route_Summary(Route, Cost, Time)).
print_Route(route_Summary(Route, Cost, Time)) :-
    print_Segments(Route),
    write("---"),nl,
    format("Costo Total: ~w~n", Cost),
    format("Tiempo: ~wh~n", Time),
    nl.

% Prints the Information of all Routes in a given Array
% print_Routes([Routes_Array]).
print_Routes([]).
print_Routes([Route | Rest]) :-
    print_Route(Route),
    print_Routes(Rest).





%	--- CALCULATIONS & FILERS ---
% Finds all Available Routes Between Two Cities and returns them as an Array
% find_All_Routes(Origin, Destiny, [Routes_Array]).
find_All_Routes(Origin, Destiny, Routes) :-
    % We use findall to get all the possible Routes and returning them in an Array.
    findall(
        route_Summary(Route, Cost, Time),
        routes(Origin, Destiny, [Origin], Route, Cost, Time),
        Routes).

% Given a set of Routes, finds the Cheapest or Fastest one
% find_Optimal([Route_Summaries_Array], Filter_Name, Route_Summary).
find_Optimal([route_Summary(Route, Cost, Time)], _, route_Summary(Route, Cost, Time)).
find_Optimal([route_Summary(Route_1, Cost_1, Time_1), route_Summary(Route_2, Cost_2, Time_2) | Rest], Filter, Result) :-
    %  Depending on Filter we Assign Value
    (Filter == cost ->
    	Value_1 is Cost_1, Value_2 is Cost_2;
    Filter == time ->
    	Value_1 is Time_1, Value_2 is Time_2),
    % And then we Compare Values
    (Value_1 =< Value_2 ->
    	find_Optimal([route_Summary(Route_1, Cost_1, Time_1) | Rest], Filter, Result);
    	find_Optimal([route_Summary(Route_2, Cost_2, Time_2) | Rest], Filter, Result)).

% Checks if all segments of a route depart in a given range.
% Returns:
% 	- False when a Segment is found to depart outside allowed range
% 	- True, when all Segments depart inside a range
% filter_Departure_Time_Route([Segments_Array], Min_Departure_Time, Max_Departure_Time).
filter_Departure_Time_Route([], _, _).
filter_Departure_Time_Route([segment(_O, _D, _T, _C, Departure_Time, _A) | Rest], Min_Departure, Max_Departure) :-
	Min_Departure =< Departure_Time, Departure_Time =< Max_Departure,
	filter_Departure_Time_Route(Rest, Min_Departure, Max_Departure).

% Filters an Array of Routes To Get
% filter_Departure_Time([Route_Sumaries_Array], Min_Departure_Time, Max_Departure_Time, Route_Sumary).
filter_Departure_Time([], _, _, []).
filter_Departure_Time([route_Summary(Route, Cost, Time) | Rest], Min_Departure, Max_Departure, Result) :-
    (filter_Departure_Time_Route(Route, Min_Departure, Max_Departure) -> % If all segments of a route pass the filter
    	filter_Departure_Time(Rest, Min_Departure, Max_Departure, Filtered),
        Result = [route_Summary(Route, Cost, Time) | Filtered];
    % Else:
    	filter_Departure_Time(Rest, Min_Departure, Max_Departure, Result)).





%		--- CALLER INDEPENDENT FUNCTIONS ---
% Calculates ALL Available Routes Between Two Cities
%	With Time Filter
% print_All(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_All(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    print_Routes(Filtered).
% 	Without Time Filter
% print_All(Origin, Destiny).
print_All(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    print_Routes(Routes).


% Calculates CHEAPEST Route Between Two Cities
% 	With Time Filter
% print_Cheapest(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_Cheapest(Origin, Destiny, Min_Departure, Max_Departure) :-
	find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, cost, Result),
    print_Routes([Result]).
% 	Without Time Filter
% print_Cheapest(Origin, Destiny).
print_Cheapest(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    find_Optimal(Routes, cost, Result),
    print_Routes([Result]).


% Calculates FASTEST Route Between Two Cities
% 	With Time Filter
% print_Fastest(Origin, Destiny, Min_Departure_Time, Max_Departure_Time).
print_Fastest(Origin, Destiny, Min_Departure, Max_Departure) :-
    find_All_Routes(Origin, Destiny, Routes),
    filter_Departure_Time(Routes, Min_Departure, Max_Departure, Filtered),
    find_Optimal(Filtered, time, Result),
    print_Routes([Result]).
% 	Without Time Filter
% print_Fastest(Origin, Destiny).
print_Fastest(Origin, Destiny) :-
    find_All_Routes(Origin, Destiny, Routes),
    find_Optimal(Routes, time, Result),
    print_Routes([Result]).
