:- module(constraint_bridge, [
    dr_diagnostic_report/1,
    constraint_status/3,
    recommendation_feasibility/3
]).

:- use_module(narrative_ontology).
:- use_module(coercion_projection).
:- use_module(isomorphism_engine).
:- use_module(domain_priors).

/* ================================================================
   1. CONTEXTUAL CONSTRAINT FILTERING
   ================================================================ */

%% constraint_status(+ConstraintName, -State, -Intensity)
%  Maps CE v2.0 types to diagnostic intensities.
constraint_status(Name, binding_limit, I) :-
    narrative_ontology:constraint_claim(Name, mountain),
    narrative_ontology:constraint_metric(Name, inevitability, I), !.

% Rope
constraint_status(Name, coordination_rope, I) :-
    narrative_ontology:constraint_claim(Name, rope),
    (narrative_ontology:constraint_metric(Name, theater_ratio, T) -> I is 1.0 - T ; I = 1.0), !.

% Snare (formerly Noose)
constraint_status(Name, extractive_snare, I) :-
    narrative_ontology:constraint_claim(Name, snare),
    narrative_ontology:constraint_metric(Name, extractiveness, I), !.

% Tangled Rope handling
constraint_status(Name, hybrid_extraction, I) :-
    narrative_ontology:constraint_claim(Name, tangled_rope),
    narrative_ontology:constraint_metric(Name, extractiveness, I), !.

% Scaffold handling
constraint_status(Name, temporary_coordination, I) :-
    narrative_ontology:constraint_claim(Name, scaffold),
    (narrative_ontology:constraint_metric(Name, extractiveness, Val) -> I = Val ; I = 0.5), !.

% Piton
constraint_status(Name, inertial_piton, I) :-
    narrative_ontology:constraint_claim(Name, piton),
    (narrative_ontology:constraint_metric(Name, theater_ratio, I) ; I = 1.0), !.

/* ================================================================
   2. UNIQUE VETO LOGIC & FEASIBILITY
   ================================================================ */

recommendation_feasibility(RecID, Status, UniqueVetoes) :-
    narrative_ontology:recommendation(RecID, _), % CRITICAL: Verify RecID is a recommendation
    % --- 1. THE STRUCTURAL AUDIT (Theorem 3) ---
    % If a recommendation affects a Snare (high extraction), 
    % it is load-bearing. Cutting it without a Scaffold is a systemic risk.
    (   narrative_ontology:affects_constraint(RecID, Target),
        narrative_ontology:constraint_metric(Target, extractiveness, X), 
        X > 0.7, % Load-bearing threshold
        \+ (narrative_ontology:entity(_, scaffold)) % No safety net provided
    ->  Status = blocked_scaffold_vacuum,
        UniqueVetoes = [] % Blocked by physics/logic, not individuals
    ;   
    % --- 2. THE POLITICAL AUDIT (Veto Logic) ---
    % Check for agents whose interests are explicitly threatened.
    (setof(Actor, narrative_ontology:veto_exposed(Actor, RecID), UniqueVetoes) 
     ; UniqueVetoes = []),
    length(UniqueVetoes, Count),
    (   Count == 0 -> Status = viable 
    ;   Count < 2  -> Status = high_veto_risk % Adjusted sensitivity
    ;   Status = blocked_by_veto)
    ).

%% is_safe_to_cut(+ConstraintID)
% Theorem 3: Cutting a load-bearing Snare requires a Scaffold.
is_safe_to_cut(C) :-
    narrative_ontology:constraint_metric(C, extractiveness, X), X > 0.7, % Load-bearing Snare
    narrative_ontology:entity(_, scaffold). % A scaffold MUST exist in the KB

/* ================================================================
   3. SCENARIO-AWARE DIAGNOSTIC REPORTING
   ================================================================ */

%% dr_diagnostic_report(+IntervalID)
%  Enhanced v4.1: Consolidates diagnostic reporting with High-Risk Isomorphism Alerting.
dr_diagnostic_report(IntervalID) :-
    format('~n=== DEFERENTIAL REALISM (DR) DIAGNOSTIC: ~w ===~n', [IntervalID]),
    
    % --- SECTION 1: CONSTRAINT INVENTORY ---
    format('~n[CONSTRAINT INVENTORY]~n'),
    (   setof(line(Name, State, Intensity), 
              (constraint_status(Name, State, Intensity), 
               narrative_ontology:constraint_metric(Name, _, _)), 
              UniqueLines)
    ->  forall(member(line(N, S, I), UniqueLines),
               (   format('  - ~w: ~w (Intensity: ~2f)~n', [N, S, I]),
                   % Trigger Isomorphism Check for High-Risk Types
                   check_for_social_twins(N, S)
               ))
    ;   format('  No active constraints found.~n')
    ),
    
    % --- SECTION 2: FEASIBILITY BRIDGE ---
    format('~n[FEASIBILITY BRIDGE]~n'),
    % Using _ConsName resolves the singleton variable warning
    forall((narrative_ontology:recommendation(RID, Summary),
            narrative_ontology:affects_constraint(RID, _ConsName)),
           (recommendation_feasibility(RID, Stat, Vs),
            format('  - ~w (~w): ~w | Vetoes: ~w~n', [RID, Summary, Stat, Vs]))),
    format('====================================================~n').

%% check_for_social_twins(+Name, +State)
%  Internal helper that alerts the user if a high-risk technical constraint 
%  mirrors a social pathology.
check_for_social_twins(Name, State) :-
    member(State, [snare, tangled_rope, extractive_snare]), % High-risk states [cite: 4, 206, 208]
    isomorphism_engine:find_high_risk_isomorphism(Name, SocialTwin, Score),
    domain_priors:category_of(SocialTwin, Cat),
    member(Cat, [narrative_history, statutory_formal, election_cycle]),
    !,
    format('    ! ALERT: High-Risk Social Twin Detected: ~w (Similarity: ~2f)~n', [SocialTwin, Score]),
    format('    ! Logic: This system functions structurally identically to a ~w scenario.~n', [Cat]).
check_for_social_twins(_, _).
