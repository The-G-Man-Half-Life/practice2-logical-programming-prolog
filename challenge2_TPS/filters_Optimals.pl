:- module(filters_Optimals, [
    filter_Departure_Time/4,
    find_Optimal/3
]).


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