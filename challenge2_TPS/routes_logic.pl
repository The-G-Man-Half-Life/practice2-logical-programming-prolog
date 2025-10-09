:- module(routes_Logic, [
    find_All_Routes/3
]).

% "Impirting" Route facts.
:- use_module(route_facts).



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
    \+ member(Destiny, Visited),
    routes(Stepover, Destiny, [Stepover | Visited], Route, Remaining_Cost, Remaining_Time),
    % Calculate Cost & Time
    Total_Cost is
    	Segment_Cost + Remaining_Cost,
    Total_Time is
    	(Arrival_Time - Departure_Time) + Remaining_Time.

% Finds all Available Routes Between Two Cities and returns them as an Array
% find_All_Routes(Origin, Destiny, [Routes_Array]).
find_All_Routes(Origin, Destiny, Routes) :-
    % We use findall to get all the possible Routes and returning them in an Array.
    findall(
        route_Summary(Route, Cost, Time),
        routes(Origin, Destiny, [Origin], Route, Cost, Time),
        Routes).
