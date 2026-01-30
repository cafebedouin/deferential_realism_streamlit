:- module(coercion_projection, [
    coercion_vector/3,
    coercion_magnitude/3,
    coercion_gradient/4,
    system_gradient/3,
    time_point_in_interval/2
]).

:- use_module(library(lists)).        % Required for sum_list/2
:- use_module(narrative_ontology).
:- use_module(config).

% Base Vector
coercion_vector(Level, Time, [A, S, U, R]) :-
    measurement(_, _, accessibility_collapse(Level), Time, A),
    measurement(_, _, stakes_inflation(Level),      Time, S),
    measurement(_, _, suppression(Level),           Time, U),
    measurement(_, _, resistance(Level),            Time, R).

% Magnitude logic
coercion_magnitude(Level, Time, Kappa) :-
    coercion_vector(Level, Time, [A, S, U, R]),
    aggregation_weights(Level, WA, WS, WU, WR),
    Kappa is (WA * A) + (WS * S) + (WU * U) + (WR * R).

% Time points helper (Optimized)
time_point_in_interval(IntervalID, Time) :-
    interval(IntervalID, T_start, T_end),
    % Find all unique times present in the measurement database
    setof(T, is_measurement_time(T), AllTimes),
    member(Time, AllTimes),
    Time >= T_start,
    Time =< T_end,
    !.

% Helper to isolate the cross-module dynamic call
is_measurement_time(T) :-
    narrative_ontology:measurement(_, _, _, T, _).

% Gradient logic (Guarded)
coercion_gradient(Level, IntervalID, T_now, Grad) :-
    interval(IntervalID, _, T_end),
    T_now < T_end,
    % Use setof to find future points but wrap in a conditional to prevent looping
    (   setof(T_next, 
              (time_point_in_interval(IntervalID, T_next), T_next > T_now), 
              FuturePoints)
    ->  FuturePoints = [T_next|_], % Take the immediate next point
        coercion_magnitude(Level, T_now, K_now),
        coercion_magnitude(Level, T_next, K_next),
        Grad is K_next - K_now,
        ! % Prevent backtracking into the setof search
    ;   fail % Explicitly fail if no future points exist in the interval
    ).

% System Gradient (Safe Aggregation)
system_gradient(IntervalID, Time, SysGrad) :-
    findall(WG,
        ( level(L),
          influence_weight(L, W),
          % Ensure coercion_gradient succeeds before calculating
          coercion_gradient(L, IntervalID, Time, G),
          WG is W * G
        ),
        WGList),
    % Guard against empty lists at the end of a timeline
    (   WGList \= []
    ->  sum_list(WGList, SysGrad), !
    ;   SysGrad = 0.0 % Return neutral gradient if no changes are detected
    ).
