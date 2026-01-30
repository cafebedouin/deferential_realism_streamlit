:- module(narrative_ontology, [
    % Core ontology
    entity/2,
    interval/3,
    event/4,

    % CE v2.0 constraint layer
    constraint_claim/2,
    recommendation/2,
    affects_constraint/2,
    veto_actor/1,
    veto_exposed/2,
    constraint_metric/3,
    omega_variable/3,

    % Optional measurement layer (v3.1 coercion metrics)
    measurement/5,

    % Optional intent evidence layer
    intent_viable_alternative/3,
    intent_alternative_rejected/3,
    intent_beneficiary_class/2,
    intent_power_change/3,
    intent_suppression_level/4,
    intent_resistance_level/4,
    intent_norm_strength/3,

    % Tangled rope category (Added January 2026)
    constraint_type/1,
    constraint_type_name/2,
    is_tangled_rope/1,
    has_coordination_function/1,
    has_asymmetric_extraction/1,

    % Scaffold
    has_sunset_clause/1,
    
    % Validation entry point
    validate_ontology/0
]).

:- use_module(config). % Added to allow access to configuration parameters

/* ============================================================
   1. MULTIFILE & DYNAMIC DECLARATIONS
   ============================================================ */

:- multifile
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5, intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3, theater_ratio/2,
    constraint_beneficiary/2, constraint_victim/2.

:- dynamic
    attribute/3, has_mandatrophy_declaration/1,
    entity/2, interval/3, event/4,
    constraint_claim/2, recommendation/2, affects_constraint/2,
    veto_actor/1, veto_exposed/2, constraint_metric/3, omega_variable/3,
    measurement/5, has_sunset_clause/1,
    intent_viable_alternative/3, intent_alternative_rejected/3,
    intent_beneficiary_class/2, intent_power_change/3,
    intent_suppression_level/4, intent_resistance_level/4,
    intent_norm_strength/3, constraint_claim/3,
    constraint_beneficiary/2, constraint_victim/2.

/* ============================================================
   2. VALIDATION LOGIC
   ============================================================ */
%% attribute(+Subject, +Key, +Value)
%  Generic metadata getter/setter used for indexical resolution.
%  This links the metadata check in is_indexical_resolution_declared/1 
%  to the actual stored metrics.
attribute(S, K, V) :- 
    narrative_ontology:constraint_metric(S, K, V).

%% has_mandatrophy_declaration(+Constraint)
%  A manual override flag used by check_indexical_relativity/1.
%  If a constraint is explicitly marked as 'mandatrophy' in its metadata,
%  it passes the indexical relativity gate.
has_mandatrophy_declaration(C) :- 
    attribute(C, lifecycle, mandatrophy).

%% validate_ontology
%  Master entry point for checking Knowledge Base integrity.
validate_ontology :-
    (   validate_entities,
        validate_intervals,
        validate_events,
        validate_constraint_claims,
        validate_constraint_metrics,
        validate_measurements,
        validate_omegas,
        validate_intent
    ).

validate_entities :-
    forall(entity(ID,Type),
        ( atom(ID),
          member(Type, [individual_powerless, individual_powerful, 
                        institutional, analytical, class])
        -> true
        ;  format('ERROR: Invalid entity(~w,~w)~n',[ID,Type]), fail
        )).

validate_intervals :-
    forall(interval(ID,Start,End),
        ( atom(ID), integer(Start), integer(End), Start =< End
        -> true
        ;  format('ERROR: Invalid interval(~w,~w,~w)~n',[ID,Start,End]), fail
        )).

validate_events :-
    forall(event(ID,Time,Actor,Type),
        ( atom(ID), integer(Time), (entity(Actor,_) ; atom(Actor)), atom(Type)
        -> true
        ;  format('ERROR: Invalid event(~w,~w,~w,~w)~n',[ID,Time,Actor,Type]), fail
        )).

%% validate_constraint_claims
%  Updated for v3.2.4 schema expansion.
%  Added tangled_rope category (January 2026) based on empirical validation of 467 constraints.
validate_constraint_claims :-
    forall(constraint_claim(Name, Type),
        ( member(Type, [mountain, rope, tangled_rope, snare, piton])
        -> true
        ;  format('ERROR: Ontological Violation in ~w: "~w" is not a valid constraint type.~n', [Name, Type]),
           fail
        )).

validate_constraint_metrics :-
    forall(constraint_metric(Name,Metric,Val),
        ( (constraint_claim(Name,_) ; true),
          atom(Metric),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid constraint_metric(~w,~w,~w)~n',[Name,Metric,Val]), fail
        )).

validate_measurements :-
    forall(measurement(ID,Target,Metric,Time,Val),
        ( atom(ID),
          (entity(Target,_) ; interval(Target,_,_)),
          (atom(Metric) ; compound(Metric)),
          integer(Time),
          number(Val), Val >= 0.0, Val =< 1.0
        -> true
        ;  format('ERROR: Invalid measurement(~w,~w,~w,~w,~w)~n',
                  [ID,Target,Metric,Time,Val]), fail
        )).

validate_omegas :-
    forall(omega_variable(ID, Type, Desc),
        ( atom(ID),
          member(Type, [empirical, conceptual, preference]),
          (atom(Desc) ; string(Desc))
        -> true
        ;  format('ERROR: Invalid omega_variable(~w,~w,~w)~n',[ID,Type,Desc]), fail
        )).

validate_intent :-
    forall(intent_viable_alternative(I,S,A),
        ( (interval(I,_,_) ; atom(I)), (entity(S,_) ; atom(S)), atom(A)
        -> true
        ;  format('ERROR: Invalid intent_viable_alternative(~w,~w,~w)~n',[I,S,A]), fail
        )).

/* ==========================================================================
   TANGLED ROPE CATEGORY - EMPIRICAL VALIDATION (Added January 2026)
   ========================================================================== */

/**
 * TANGLED ROPE CATEGORY - EMPIRICAL VALIDATION
 *
 * Added based on corpus analysis of 467 constraints (January 2026):
 * - 168 constraints (36%) show hybrid coordination/extraction pattern
 * - Cannot be explained by indexing alone (structural hybridity is real)
 * - Pattern mining confirmed empirical necessity
 *
 * Examples from corpus:
 * - carbon_credit_markets (0.55 extraction, 0.60 suppression)
 * - academic_tenure_system (0.75 extraction, 0.60 suppression)
 * - platform_network_effects (coordination + extraction)
 *
 * Key distinction from pure types:
 * - NOT pure rope (extraction too high, >0.40)
 * - NOT pure snare (has genuine coordination function)
 * - Requires surgical reform: preserve coordination, cut extraction
 */

%% constraint_type(?Type)
%  Valid constraint types in the framework.
constraint_type(mountain).
constraint_type(rope).
constraint_type(tangled_rope).  % Validated by corpus analysis (168/467 constraints, 36%)
constraint_type(snare).
constraint_type(piton).

%% constraint_type_name(?Type, ?Name)
%  Human-readable names for constraint types.
constraint_type_name(mountain, 'Mountain (Natural Constraint)').
constraint_type_name(rope, 'Rope (Pure Coordination)').
constraint_type_name(tangled_rope, 'Tangled Rope (Hybrid Coordination/Extraction)').
constraint_type_name(snare, 'Snare (Pure Extraction)').
constraint_type_name(piton, 'Piton (Degraded Coordination)').


%% is_tangled_rope(+ConstraintID)
%  Tangled rope signature detection.
%  Must have BOTH coordination function AND asymmetric extraction.
is_tangled_rope(ConstraintID) :-
    domain_priors:base_extractiveness(ConstraintID, E),
    config:param(tangled_rope_extraction_floor, TR_E_Floor),
    config:param(tangled_rope_extraction_ceil, TR_E_Ceil),
    E >= TR_E_Floor, E =< TR_E_Ceil,
    domain_priors:suppression_score(ConstraintID, S),
    config:param(tangled_rope_suppression_floor, TR_S_Floor),
    S >= TR_S_Floor,
    domain_priors:requires_active_enforcement(ConstraintID),
    % Must have both coordination function AND extraction
    has_coordination_function(ConstraintID),
    has_asymmetric_extraction(ConstraintID).

%% has_coordination_function(+ConstraintID)
%  Check if constraint solves a collective action problem.
%  Evidence: Has multiple beneficiaries or provides network effects.
%  Note: constraint_beneficiary/2 is defined in individual test files.
has_coordination_function(ConstraintID) :-
    constraint_beneficiary(ConstraintID, _),
    !.

%% has_asymmetric_extraction(+ConstraintID)
%  Check for asymmetric beneficiary distribution.
%  Evidence: Has victims or concentrated benefits.
%  Note: constraint_victim/2 is defined in individual test files.
has_asymmetric_extraction(ConstraintID) :-
    constraint_victim(ConstraintID, _),
    !.

/* ==========================================================================
   INDEXICAL GATE (v3.1)
   ========================================================================== */

% Indexical Gate: Flags Mountains that are actually Shadow Nooses.
check_indexical_relativity(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    domain_priors:base_extractiveness(ID, E),
    E > 0.7,
    % Require a manual "Mandatrophy Acknowledgement" in the file metadata
    \+ has_mandatrophy_declaration(ID).

% Every domain must be indexed to power-perspectives (mountain/rope/tangled_rope/snare).
% Note: With tangled_rope addition, we now have 4 categories but validation remains
% focused on ensuring at least the traditional 3-pillar coverage is maintained.
validate_indexical_completeness(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    constraint_indexing:constraint_classification(ID, rope, _),
    constraint_indexing:constraint_classification(ID, snare, _),
    !.
validate_indexical_completeness(ID) :-
    format('ERROR: Perspectival Gap in ~w. Missing 3-pillar coverage.~n', [ID]),
    fail.

/* ==========================================================================
   MANDATROPHY RECONCILIATION (v3.1)
   ========================================================================== */

% is_mandatrophy_resolved/1: Explicitly standardizes the 2 residual Omegas.
is_mandatrophy_resolved(gale_shapley).        % The Algorithm is the Mandate.
is_mandatrophy_resolved(planetary_boundaries). % The Biological Limit is the Mandate.

% detect_omega/2: Identifies logical friction points.
% This rule is updated to exempt "Hardened Mandatrophy."
detect_omega(Name, mandatrophy) :-
    constraint_indexing:constraint_classification(Name, mountain, _),
    domain_priors:base_extractiveness(Name, E),
    E > 0.7,
    % The Paradox: A Mountain (Fact) behaves like a Snare (Trap).
    % If the domain is recognized as Mandatrophic, it is no longer an Omega.
    \+ is_mandatrophy_resolved(Name).

% System Insight: Logic for the Parsing Suite
count_unresolved_omegas(Count) :-
    aggregate_all(count, detect_omega(_, _), Count).

% Mandatrophy Detector: Flags Mountains that function as Traps.
detect_mandatrophy_omega(ID) :-
    constraint_indexing:constraint_classification(ID, mountain, _),
    domain_priors:base_extractiveness(ID, E),
    E > 0.7,
    % Check for the explicit resolution marker in the file
    \+ is_indexical_resolution_declared(ID).

is_indexical_resolution_declared(ID) :-
    % Looks for the standardized resolution hook in the metadata
    attribute(ID, indexical_resolution, resolved).

% Add-ons

has_sunset_clause(_) :- fail. % Default fail if not explicitly defined in instance

% Maps claim/2 into claim/3 for context-indexed lookups
constraint_claim(ID, Type, _Context) :- 
    constraint_claim(ID, Type).
