% To be able to use the route facts in routes logic we use:
:- module(route_facts, [
    route/7
]).

% route(Origin, Destiny, Transport, Departure_Time, Arrival_Time, Cost_USD, Available).

% Bogotá Routes
route(bogota, medellin, avion, 8, 9, 100, si).
route(bogota, medellin, bus, 6, 16, 40, si).
route(bogota, santa_marta, bus, 7, 23, 105, no).
route(bogota, cartagena, bus, 6, 22, 90, no).

% Medellín Routes
route(medellin, bogota, avion, 12, 13, 100, si).
route(medellin, cartagena, avion, 10, 11.5, 120, no).
route(medellin, cartagena, bus, 7, 19, 42, si).
route(medellin, caucacia, bus, 5, 12, 50, si).

% Cartagena Routes
route(cartagena, medellin, bus, 6, 18, 45, si).
route(cartagena, santa_marta, bus, 14, 18, 30, si).

% Sincelejo Routes
route(sincelejo, covenas, bus, 7, 8, 15, si).

% Caucacia Routes
route(caucacia, sincelejo, bus, 9, 13, 20, si).

% Santa Marta Routes
route(santa_marta, cartagena, bus, 8, 10, 27.8, si). % Santa Marta Routes
route(santa_marta, cartagena, avion, 13, 13.4, 70, si).