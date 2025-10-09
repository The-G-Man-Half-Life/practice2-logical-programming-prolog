:- module(outputs, [
    print_Route/1,
    print_Routes/1
]).


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