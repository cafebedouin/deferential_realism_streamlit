:- module(drl_lifecycle, [
    % --- Individual Drift Event Detectors ---
    detect_metric_substitution/1,
    detect_extraction_accumulation/1,
    detect_coordination_loss/1,
    detect_function_obsolescence/1,
    detect_sunset_violation/1,
    detect_extraction_dried_up/1,
    detect_is_piton/1,

    % --- Structured Drift Event API ---
    drift_event/3,                  % drift_event(ConstraintID, EventType, Evidence)
    drift_event/4,                  % drift_event(ConstraintID, Context, EventType, Evidence)

    % --- Transition Path Detection ---
    transition_path/4,              % transition_path(ConstraintID, FromType, ToType, Evidence)
    degradation_chain/3,            % degradation_chain(ConstraintID, Chain, Evidence)
    predicted_terminal_state/3,     % predicted_terminal_state(ConstraintID, State, Confidence)

    % --- Drift Velocity ---
    drift_velocity/3,               % drift_velocity(ConstraintID, Metric, Rate)
    drift_acceleration/3,           % drift_acceleration(ConstraintID, Metric, Accel)

    % --- Severity Classification ---
    drift_severity/3,               % drift_severity(ConstraintID, EventType, Severity)

    % --- Unified Scan ---
    scan_all_drift/1,               % scan_all_drift(-Report)
    scan_constraint_drift/2,        % scan_constraint_drift(+ConstraintID, -Events)
    scan_constraint_drift/3,        % scan_constraint_drift(+ConstraintID, +Context, -Events)

    % --- Drift Report ---
    generate_drift_report/0,        % Print full drift report to stdout
    generate_drift_report/1         % generate_drift_report(+ConstraintID)
]).

:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).
:- use_module(drl_core).
:- use_module(constraint_indexing).

/* ================================================================
   DRL LIFECYCLE MODULE

   Detects constraint degradation patterns (drift events),
   transition paths, drift velocity, and severity.

   Seven drift event types from core.md:
   1. metric_substitution  - Proxy becomes goal (Goodhart)
   2. extraction_accumulation - Rent-seeking added over time
   3. coordination_loss    - Function withers, extraction persists
   4. function_obsolescence - Environment shifts, constraint outdated
   5. sunset_violation     - Temporary becomes permanent
   6. extraction_dried_up  - Mechanism fails, structure persists
   7. internalized_piton   - Enforcer removed, habits remain

   Canonical degradation paths from core.md:
   - rope -> tangled_rope -> snare -> piton
   - rope -> piton (direct obsolescence)
   - scaffold -> piton (sunset violation)
   - scaffold -> snare (calcification with extraction)
   - scaffold -> tangled_rope (extraction added during transition)
   - snare -> false_mountain (naturalization)
   - snare -> piton (internalized piton)
   ================================================================ */

/* ================================================================
   1. UTILITY
   ================================================================ */

%% get_current_year(-Year)
%  Retrieves the current year from the system clock.
get_current_year(Year) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    DateTime = date(Year, _, _, _, _, _, _, _, _).

%% safe_metric(+C, +Metric, -Value)
%  Retrieves a metric or fails silently.
safe_metric(C, Metric, Value) :-
    narrative_ontology:constraint_metric(C, Metric, Value).

%% metric_delta(+C, +Metric, -T1, -T2, -Delta)
%  Finds the earliest and latest measurements and computes the change.
metric_delta(C, Metric, T1, T2, Delta) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    Pairs = [_|_],
    sort(Pairs, Sorted),
    Sorted = [T1-V1|_],
    last(Sorted, T2-V2),
    T2 > T1,
    Delta is V2 - V1.

%% metric_at(+C, +Metric, +Time, -Value)
%  Retrieves measurement at a specific time.
metric_at(C, Metric, Time, Value) :-
    narrative_ontology:measurement(_, C, Metric, Time, Value).

%% metric_trend(+C, +Metric, -Trend)
%  Determines if a metric is increasing, decreasing, or stable.
metric_trend(C, Metric, Trend) :-
    metric_delta(C, Metric, _, _, Delta),
    (   Delta > 0.05  -> Trend = increasing
    ;   Delta < -0.05 -> Trend = decreasing
    ;   Trend = stable
    ).

/* ================================================================
   2. INDIVIDUAL DRIFT EVENT DETECTORS
   Each detector both prints diagnostics AND succeeds/fails.
   ================================================================ */

%% detect_metric_substitution(+ConstraintID)
%  Goodhart drift: proxy metric becomes the goal.
%  Evidence: theater_ratio rising above 0.5 across time points.
detect_metric_substitution(C) :-
    narrative_ontology:measurement(_, C, theater_ratio, T1, TR1),
    narrative_ontology:measurement(_, C, theater_ratio, T2, TR2),
    T2 > T1,
    TR2 > TR1,
    TR2 > 0.5,
    format('  Drift: Metric Substitution in ~w~n', [C]),
    format('    Theater ratio ~2f (~w) -> ~2f (~w)~n', [TR1, T1, TR2, T2]).

%% detect_extraction_accumulation(+ConstraintID)
%  Rent-seeking accumulates over time.
detect_extraction_accumulation(C) :-
    narrative_ontology:measurement(_, C, base_extractiveness, T1, V1),
    narrative_ontology:measurement(_, C, base_extractiveness, T2, V2),
    T2 > T1,
    V2 > V1,
    format('  Drift: Extraction Accumulation in ~w~n', [C]),
    format('    Extractiveness ~2f (~w) -> ~2f (~w)~n', [V1, T1, V2, T2]).

%% detect_coordination_loss(+ConstraintID)
%  Coordination withers while extraction persists.
detect_coordination_loss(C) :-
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T1, CE1),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T2, CE2),
    T2 > T1,
    CE2 < CE1,
    CE2 < 0.3,
    safe_metric(C, extractiveness, E),
    E > 0.4,
    format('  Drift: Coordination Loss in ~w~n', [C]),
    format('    Coordination dropped to ~2f, extraction still ~2f~n', [CE2, E]).

%% detect_function_obsolescence(+ConstraintID)
%  Original function obsolete due to alternatives.
detect_function_obsolescence(C) :-
    safe_metric(C, alternatives_available, Alt),
    Alt > 0,
    safe_metric(C, resistance_to_change, R),
    R < 0.3,
    safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    format('  Drift: Function Obsolescence in ~w~n', [C]),
    format('    Alternatives exist (~w), low resistance (~2f), high theater (~2f)~n', [Alt, R, TR]).

%% detect_sunset_violation(+ConstraintID)
%  Temporary constraint became permanent.
detect_sunset_violation(C) :-
    narrative_ontology:has_sunset_clause(C),
    safe_metric(C, sunset_time, SunsetYear),
    get_current_year(CurrentYear),
    CurrentYear > SunsetYear,
    format('  Drift: Sunset Violation in ~w~n', [C]),
    format('    Sunset was ~w, now ~w~n', [SunsetYear, CurrentYear]).

%% detect_extraction_dried_up(+ConstraintID)
%  Extraction mechanism failed but structure persists (zombie constraint).
detect_extraction_dried_up(C) :-
    safe_metric(C, extractiveness, E),
    E < 0.10,
    safe_metric(C, suppression_requirement, S),
    S > 0.50,
    format('  Drift: Extraction Dried Up in ~w~n', [C]),
    format('    Extraction ~2f but suppression still ~2f~n', [E, S]).

%% detect_is_piton(+ConstraintID)
%  Internalized piton: extraction removed but habits remain.
detect_is_piton(C) :-
    safe_metric(C, extractiveness, E),
    E < 0.10,
    safe_metric(C, theater_ratio, TR),
    TR > 0.70,
    \+ narrative_ontology:requires_active_enforcement(C),
    format('  Drift: Internalized Piton in ~w~n', [C]),
    format('    Low extraction (~2f), high theater (~2f), no enforcement~n', [E, TR]).

/* ================================================================
   3. STRUCTURED DRIFT EVENT API
   Returns data structures instead of printing.
   ================================================================ */

%% drift_event(+ConstraintID, -EventType, -Evidence)
%  Non-indexed drift detection. Returns structured evidence.
drift_event(C, metric_substitution, evidence(theater_delta, T1, T2, TR1, TR2)) :-
    narrative_ontology:measurement(_, C, theater_ratio, T1, TR1),
    narrative_ontology:measurement(_, C, theater_ratio, T2, TR2),
    T2 > T1, TR2 > TR1, TR2 > 0.5.

drift_event(C, extraction_accumulation, evidence(extraction_delta, T1, T2, V1, V2)) :-
    narrative_ontology:measurement(_, C, base_extractiveness, T1, V1),
    narrative_ontology:measurement(_, C, base_extractiveness, T2, V2),
    T2 > T1, V2 > V1.

drift_event(C, coordination_loss, evidence(coordination_drop, CE2, extraction_persist, E)) :-
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T1, CE1),
    narrative_ontology:measurement(_, C, coordination_effectiveness, T2, CE2),
    T2 > T1, CE2 < CE1, CE2 < 0.3,
    safe_metric(C, extractiveness, E), E > 0.4.

drift_event(C, function_obsolescence, evidence(alternatives, Alt, resistance, R, theater, TR)) :-
    safe_metric(C, alternatives_available, Alt), Alt > 0,
    safe_metric(C, resistance_to_change, R), R < 0.3,
    safe_metric(C, theater_ratio, TR), TR > 0.5.

drift_event(C, sunset_violation, evidence(sunset_year, SunsetYear, current_year, CurrentYear)) :-
    narrative_ontology:has_sunset_clause(C),
    safe_metric(C, sunset_time, SunsetYear),
    get_current_year(CurrentYear),
    CurrentYear > SunsetYear.

drift_event(C, extraction_dried_up, evidence(extraction, E, suppression, S)) :-
    safe_metric(C, extractiveness, E), E < 0.10,
    safe_metric(C, suppression_requirement, S), S > 0.50.

drift_event(C, internalized_piton, evidence(extraction, E, theater, TR)) :-
    safe_metric(C, extractiveness, E), E < 0.10,
    safe_metric(C, theater_ratio, TR), TR > 0.70,
    \+ narrative_ontology:requires_active_enforcement(C).

%% drift_event(+ConstraintID, +Context, -EventType, -Evidence)
%  Context-indexed drift detection.
%  Same degradation can look different from different power positions.
%  A moderate-extraction constraint may appear as extraction_accumulation
%  from a powerless context (amplified by pi=1.5) but stable from
%  an institutional context (reduced by pi=-0.2).
drift_event(C, Context, extraction_accumulation_indexed, evidence(effective_chi, Chi, trend, Trend)) :-
    constraint_indexing:valid_context(Context),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(rope_extraction_ceiling, RopeCeil),
    Chi > RopeCeil,
    metric_trend(C, base_extractiveness, Trend),
    Trend = increasing.

drift_event(C, Context, false_mountain_drift, evidence(claimed, mountain, actual, ActualType)) :-
    constraint_indexing:valid_context(Context),
    narrative_ontology:constraint_claim(C, mountain),
    drl_core:dr_type(C, Context, ActualType),
    ActualType \= mountain.

drift_event(C, Context, load_bearing_degradation, evidence(type, Type, extraction, Chi)) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),
    member(Type, [snare, tangled_rope]),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(snare_load_bearing_threshold, T),
    Chi > T,
    metric_trend(C, base_extractiveness, increasing).

/* ================================================================
   4. TRANSITION PATH DETECTION
   Detects the canonical degradation paths from core.md.
   ================================================================ */

%% transition_path(+ConstraintID, -FromType, -ToType, -Evidence)
%  Detects a constraint in the process of transitioning between types.
%  Uses current metrics + drift events to infer the transition.

% Rope -> Tangled Rope (extraction accumulating into coordination)
transition_path(C, rope, tangled_rope, evidence(extraction_rising, E, has_coordination, true)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    safe_metric(C, extractiveness, E),
    config:param(rope_extraction_ceiling, Ceil),
    E > Ceil * 0.7,  % Within 30% of ceiling
    narrative_ontology:has_coordination_function(C),
    metric_trend(C, base_extractiveness, increasing).

% Tangled Rope -> Snare (coordination dying while extraction grows)
transition_path(C, tangled_rope, snare, evidence(coordination_declining, true, extraction, E)) :-
    drl_core:dr_type(C, Type),
    Type = tangled_rope,
    safe_metric(C, extractiveness, E),
    config:param(snare_extraction_floor, Floor),
    E > Floor * 0.8,  % Approaching snare threshold
    (   drift_event(C, coordination_loss, _)
    ;   metric_trend(C, coordination_effectiveness, decreasing)
    ).

% Rope -> Piton (direct obsolescence, no extraction phase)
transition_path(C, rope, piton, evidence(function_obsolete, true, theater_high, TR)) :-
    drl_core:dr_type(C, Type),
    Type = rope,
    safe_metric(C, theater_ratio, TR),
    TR > 0.5,
    drift_event(C, function_obsolescence, _).

% Scaffold -> Piton (sunset violation)
transition_path(C, scaffold, piton, evidence(sunset_violated, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    drift_event(C, sunset_violation, _).

% Scaffold -> Snare (calcification with extraction)
transition_path(C, scaffold, snare, evidence(extraction_added, E, sunset_violated, Violated)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    safe_metric(C, extractiveness, E),
    config:param(snare_extraction_floor, Floor),
    E > Floor * 0.7,
    (   drift_event(C, sunset_violation, _)
    ->  Violated = true
    ;   Violated = false
    ).

% Scaffold -> Tangled Rope (extraction added during transition)
transition_path(C, scaffold, tangled_rope, evidence(extraction_emerging, E, coordination_intact, true)) :-
    drl_core:dr_type(C, Type),
    Type = scaffold,
    safe_metric(C, extractiveness, E),
    config:param(tangled_rope_extraction_floor, Floor),
    E > Floor * 0.7,
    narrative_ontology:has_coordination_function(C).

% Snare -> I-Piton (internalization)
transition_path(C, snare, piton, evidence(internalized, true)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    drift_event(C, internalized_piton, _).

% Snare -> False Mountain (naturalization)
transition_path(C, snare, false_mountain, evidence(naturalized, true, claimed, mountain)) :-
    drl_core:dr_type(C, Type),
    Type = snare,
    narrative_ontology:constraint_claim(C, mountain).

%% degradation_chain(+ConstraintID, -Chain, -Evidence)
%  Detects multi-step degradation chains by examining measurement history.
%  Returns the full chain of types observed across time points.
degradation_chain(C, Chain, evidence(time_span, T1, T2)) :-
    findall(T-Type,
            (narrative_ontology:measurement(_, C, _, T, _),
             classify_snapshot(C, T, Type)),
            RawPairs),
    RawPairs \= [],
    sort(RawPairs, Sorted),
    pairs_values(Sorted, TypeList),
    deduplicate_consecutive(TypeList, Chain),
    length(Chain, Len),
    Len > 1,
    Sorted = [T1-_|_],
    last(Sorted, T2-_).

%% classify_snapshot(+C, +Time, -Type)
%  Classifies a constraint at a specific time using measurements available.
classify_snapshot(C, Time, Type) :-
    (   metric_at(C, base_extractiveness, Time, E)
    ->  true
    ;   safe_metric(C, extractiveness, E)
    ->  true
    ;   E = 0.5
    ),
    (   metric_at(C, suppression_requirement, Time, S)
    ->  true
    ;   safe_metric(C, suppression_requirement, S)
    ->  true
    ;   S = 0.5
    ),
    snapshot_type(C, E, S, Type).

snapshot_type(_, E, S, mountain) :-
    config:param(mountain_suppression_ceiling, SCeil),
    config:param(mountain_extractiveness_max, ECeil),
    S =< SCeil, E =< ECeil, !.
snapshot_type(C, E, S, tangled_rope) :-
    config:param(tangled_rope_extraction_floor, EFloor),
    config:param(tangled_rope_suppression_floor, SFloor),
    E >= EFloor, S >= SFloor,
    narrative_ontology:has_coordination_function(C), !.
snapshot_type(_, E, S, snare) :-
    config:param(snare_extraction_floor, EFloor),
    config:param(snare_suppression_floor, SFloor),
    E >= EFloor, S >= SFloor, !.
snapshot_type(_, E, _, rope) :-
    config:param(rope_extraction_ceiling, ECeil),
    E =< ECeil, !.
snapshot_type(_, E, _, piton) :-
    config:param(piton_extraction_ceiling, ECeil),
    E =< ECeil, !.
snapshot_type(_, _, _, unknown).

%% predicted_terminal_state(+ConstraintID, -State, -Confidence)
%  Predicts where a constraint is heading based on current drift events
%  and transition paths.
predicted_terminal_state(C, piton, high) :-
    drift_event(C, function_obsolescence, _),
    drift_event(C, extraction_dried_up, _), !.

predicted_terminal_state(C, piton, high) :-
    drift_event(C, internalized_piton, _), !.

predicted_terminal_state(C, piton, medium) :-
    drift_event(C, sunset_violation, _), !.

predicted_terminal_state(C, snare, high) :-
    transition_path(C, tangled_rope, snare, _), !.

predicted_terminal_state(C, snare, medium) :-
    drift_event(C, extraction_accumulation, _),
    drift_event(C, coordination_loss, _), !.

predicted_terminal_state(C, tangled_rope, medium) :-
    transition_path(C, rope, tangled_rope, _), !.

predicted_terminal_state(C, tangled_rope, low) :-
    drift_event(C, extraction_accumulation, _),
    narrative_ontology:has_coordination_function(C), !.

predicted_terminal_state(_, stable, low).

/* ================================================================
   5. DRIFT VELOCITY AND ACCELERATION
   ================================================================ */

%% drift_velocity(+ConstraintID, +Metric, -RatePerYear)
%  Rate of change per year for a given metric.
drift_velocity(C, Metric, Rate) :-
    metric_delta(C, Metric, T1, T2, Delta),
    Duration is T2 - T1,
    Duration > 0,
    Rate is Delta / Duration.

%% drift_acceleration(+ConstraintID, +Metric, -Acceleration)
%  Whether drift is accelerating, decelerating, or constant.
%  Uses three measurement points minimum.
drift_acceleration(C, Metric, Acceleration) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    sort(Pairs, Sorted),
    length(Sorted, N), N >= 3,
    compute_acceleration(Sorted, Acceleration).

compute_acceleration(Sorted, Acceleration) :-
    Sorted = [T1-V1, T2-V2, T3-V3|_],
    D1 is T2 - T1, D1 > 0,
    D2 is T3 - T2, D2 > 0,
    R1 is (V2 - V1) / D1,
    R2 is (V3 - V2) / D2,
    RateDelta is R2 - R1,
    (   RateDelta > 0.01  -> Acceleration = accelerating
    ;   RateDelta < -0.01 -> Acceleration = decelerating
    ;   Acceleration = constant
    ).

/* ================================================================
   6. SEVERITY CLASSIFICATION
   ================================================================ */

%% drift_severity(+ConstraintID, +EventType, -Severity)
%  Classifies drift severity: critical | warning | watch
%
%  Critical: Active harm likely without intervention.
%  Warning: Degradation in progress, intervention recommended.
%  Watch: Early indicators, monitor closely.

drift_severity(C, sunset_violation, critical) :-
    drift_event(C, sunset_violation, _),
    safe_metric(C, extractiveness, E), E > 0.3, !.
drift_severity(_, sunset_violation, warning).

drift_severity(C, extraction_accumulation, critical) :-
    drift_event(C, extraction_accumulation, evidence(_, _, _, _, V2)),
    config:param(snare_extraction_floor, Floor),
    V2 >= Floor, !.
drift_severity(C, extraction_accumulation, warning) :-
    drift_event(C, extraction_accumulation, evidence(_, _, _, _, V2)),
    config:param(tangled_rope_extraction_floor, Floor),
    V2 >= Floor, !.
drift_severity(_, extraction_accumulation, watch).

drift_severity(_, coordination_loss, critical) :-
    !.  % Coordination loss is always critical (irreversible harm likely)

drift_severity(_, internalized_piton, warning).

drift_severity(_, extraction_dried_up, warning).

drift_severity(C, metric_substitution, Severity) :-
    (   safe_metric(C, theater_ratio, TR), TR > 0.7
    ->  Severity = critical
    ;   Severity = warning
    ), !.

drift_severity(_, function_obsolescence, watch).

% Default
drift_severity(_, _, watch).

/* ================================================================
   7. UNIFIED SCAN
   ================================================================ */

%% scan_constraint_drift(+ConstraintID, -Events)
%  Scans a single constraint for all drift events.
%  Returns list of drift(EventType, Evidence, Severity).
scan_constraint_drift(C, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   drift_event(C, EventType, Evidence),
            drift_severity(C, EventType, Severity)
        ),
        Events
    ).

%% scan_constraint_drift(+ConstraintID, +Context, -Events)
%  Context-indexed scan: includes both standard and indexed drift events.
scan_constraint_drift(C, Context, Events) :-
    findall(
        drift(EventType, Evidence, Severity),
        (   (   drift_event(C, EventType, Evidence)
            ;   drift_event(C, Context, EventType, Evidence)
            ),
            drift_severity(C, EventType, Severity)
        ),
        Events
    ).

%% scan_all_drift(-Report)
%  Scans all known constraints for drift events.
%  Returns report(TotalConstraints, TotalEvents, Critical, Warning, Watch, Details).
scan_all_drift(Report) :-
    findall(C, narrative_ontology:constraint_claim(C, _), AllCs),
    sort(AllCs, Constraints),
    length(Constraints, NumConstraints),
    findall(
        constraint_drift(C, Events),
        (   member(C, Constraints),
            scan_constraint_drift(C, Events),
            Events \= []
        ),
        Details
    ),
    count_by_severity(Details, Critical, Warning, Watch),
    flatten_events(Details, AllEvents),
    length(AllEvents, TotalEvents),
    Report = report(NumConstraints, TotalEvents, Critical, Warning, Watch, Details).

count_by_severity(Details, Critical, Warning, Watch) :-
    flatten_events(Details, AllEvents),
    include(is_severity(critical), AllEvents, Cs), length(Cs, Critical),
    include(is_severity(warning), AllEvents, Ws), length(Ws, Warning),
    include(is_severity(watch), AllEvents, As), length(As, Watch).

is_severity(Sev, drift(_, _, Sev)).

flatten_events([], []).
flatten_events([constraint_drift(_, Events)|Rest], All) :-
    flatten_events(Rest, RestAll),
    append(Events, RestAll, All).

/* ================================================================
   8. DRIFT REPORT GENERATION
   ================================================================ */

%% generate_drift_report/0
%  Generates a full drift event report across all constraints.
generate_drift_report :-
    format('~n================================================================~n'),
    format('  DRIFT EVENT REPORT~n'),
    format('================================================================~n~n'),
    scan_all_drift(report(NumC, NumE, Critical, Warning, Watch, Details)),
    format('  Constraints scanned: ~w~n', [NumC]),
    format('  Total drift events:  ~w~n', [NumE]),
    format('  Critical: ~w | Warning: ~w | Watch: ~w~n~n', [Critical, Warning, Watch]),
    (   Details = []
    ->  format('  No drift events detected.~n')
    ;   forall(member(constraint_drift(C, Events), Details),
              print_constraint_drift(C, Events))
    ),
    format('~n--- Transition Path Analysis ---~n'),
    forall(
        (narrative_ontology:constraint_claim(C2, _), transition_path(C2, From, To, Ev)),
        format('  ~w: ~w -> ~w (~w)~n', [C2, From, To, Ev])
    ),
    format('~n--- Terminal State Predictions ---~n'),
    forall(
        (narrative_ontology:constraint_claim(C3, _), predicted_terminal_state(C3, State, Conf), State \= stable),
        format('  ~w -> ~w (confidence: ~w)~n', [C3, State, Conf])
    ),
    format('~n================================================================~n').

%% generate_drift_report(+ConstraintID)
%  Generates drift report for a single constraint.
generate_drift_report(C) :-
    format('~n--- Drift Analysis: ~w ---~n', [C]),
    scan_constraint_drift(C, Events),
    (   Events = []
    ->  format('  No drift events detected.~n')
    ;   forall(member(drift(Type, Ev, Sev), Events),
              format('  [~w] ~w: ~w~n', [Sev, Type, Ev]))
    ),
    % Transition paths
    (   transition_path(C, From, To, TEv)
    ->  format('  Transition: ~w -> ~w (~w)~n', [From, To, TEv])
    ;   true
    ),
    % Terminal state prediction
    predicted_terminal_state(C, State, Conf),
    (   State \= stable
    ->  format('  Predicted terminal: ~w (confidence: ~w)~n', [State, Conf])
    ;   format('  No degradation trajectory detected.~n')
    ),
    % Velocity
    (   drift_velocity(C, base_extractiveness, Rate)
    ->  format('  Extraction drift velocity: ~4f/year~n', [Rate])
    ;   true
    ),
    format('~n').

print_constraint_drift(C, Events) :-
    format('  ~w:~n', [C]),
    forall(member(drift(Type, Evidence, Severity), Events),
           format('    [~w] ~w~n        Evidence: ~w~n', [Severity, Type, Evidence])).

/* ================================================================
   9. HELPER PREDICATES
   ================================================================ */

%% deduplicate_consecutive(+List, -Deduped)
%  Removes consecutive duplicates: [a,a,b,b,a] -> [a,b,a]
deduplicate_consecutive([], []).
deduplicate_consecutive([X], [X]).
deduplicate_consecutive([X,X|Rest], Deduped) :-
    !, deduplicate_consecutive([X|Rest], Deduped).
deduplicate_consecutive([X,Y|Rest], [X|Deduped]) :-
    X \= Y,
    deduplicate_consecutive([Y|Rest], Deduped).

%% last(+List, -Last)
last([X], X).
last([_|T], X) :- last(T, X).

%% pairs_values(+Pairs, -Values)
pairs_values([], []).
pairs_values([_-V|Rest], [V|Vs]) :- pairs_values(Rest, Vs).
