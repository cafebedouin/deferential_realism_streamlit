% ============================================================================
% DRL MODAL LOGIC - INDEXICAL VERSION v4.0
% ============================================================================
% This module implements context-indexed modal reasoning for Deferential Realism.
% All dependency chains, transformations, and counterfactuals are relative to
% WHO is analyzing/intervening and WHEN they're doing it.
%
% BREAKING CHANGES FROM v3.x:
% - dependency_chain/4 → dependency_chain/5 (added Context)
% - assess_scaffold_need/2 → assess_scaffold_need/3 (added Context)
% - simulate_cut/2 → simulate_cut/3 (added Context)
% - composite_type/3 → composite_type/4 (added Context)
% - All counterfactual reasoning now indexed
%
% KEY INSIGHT: "Load-bearing" depends on WHO is cutting the constraint.
% What's load-bearing for powerless individual may be non-essential for institution.
%
% Integration: Load after drl_core.pl (v4.0+)
% ============================================================================

:- module(drl_modal_logic, [
    % Stage 1: Composition Rules (INDEXED)
    composite_type/4,               % Added Context parameter
    composite_type/3,               % Backward compat
    composition_rule/3,
    detect_extraction_dominance/2,
    detect_necessity_inheritance/2,
    
    % Stage 2: Transformation Tracking (Context-aware)
    constraint_history/3,           % Added Context parameter
    constraint_history/2,           % Backward compat
    transformation_detected/5,      
    transformation_type/6,
    canonical_transformation/6,
    predict_transformation/3,
    
    % Stage 3: Counterfactual Reasoning (INDEXED)
    simulate_cut/3,                 % Added Context parameter
    simulate_cut/2,                 % Backward compat
    dependency_chain/5,             % Added Context parameter
    dependency_chain/4,             % Backward compat
    infer_structural_coupling/3,
    assess_scaffold_need/3,         % Added Context parameter
    assess_scaffold_need/2,         % Backward compat
    counterfactual_world/4,         % Added Context parameter
    counterfactual_world/3,         % Backward compat

    % Stage 4: Audit
    possibly/1,
    necessarily/1,
    is_snare/1,
    is_mountain/1,
    is_rope/1
]).

:- use_module(drl_audit_core).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection).

:- dynamic agent_index/2.
:- dynamic constraint_data/2.

% Scaled for the Powerless
is_snare(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, individual_powerless, Chi),
    drl_audit_core:structural_signature(Chi, Data, snare).

% Scaled for the Observer
is_mountain(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, analytical, Chi),
    drl_audit_core:structural_signature(Chi, Data, mountain).

% Scaled for the Beneficiary
is_rope(C) :- 
    constraint_data(C, Data),
    member(base_extractiveness(Base), Data),
    drl_audit_core:effective_extraction(Base, institutional, Chi),
    drl_audit_core:structural_signature(Chi, Data, rope).

/* ================================================================
   MODAL LOGIC EXTENSION FOR DEFERENTIAL REALISM v4.0
   
   VERSION 4.0 CHANGES:
   - Full indexical relativity integration
   - All dependency analysis is context-relative
   - "Load-bearing" now indexed to WHO is cutting
   - Scaffold assessment indexed to WHO needs transition
   - Theorem 3 ("cutting load-bearing Snare requires Scaffold")
     now evaluates per perspective
   
   CRITICAL THEOREM 3 UPGRADE:
   OLD: "If Snare is load-bearing, cutting requires Scaffold"
   NEW: "If Snare is load-bearing FROM CONTEXT C, cutting from C requires Scaffold"
   
   Example: Property rights
   - FROM powerless context: load-bearing Mountain (shelter dependency)
   - FROM institutional context: non-essential Rope (coordination choice)
   - Institutional cutting WITHOUT considering powerless dependency = catastrophe
   
   This module implements three stages of indexed modal reasoning:
   1. Composition Rules - how constraints interact (context-aware)
   2. Transformation Tracking - temporal evolution (context-aware)
   3. Counterfactual Reasoning - intervention simulation (context-indexed)
   ================================================================ */

/* ================================================================
   STAGE 1: COMPOSITION RULES (INDEXED)
   Modal logic for how constraints interact and compose
   ================================================================ */

%% composite_type(+C1, +C2, +Context, -ResultType)
% PRIMARY API: Determines composite type FROM A SPECIFIC CONTEXT
composite_type(C1, C2, Context, Result) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C1, Context, T1),
    drl_core:dr_type(C2, Context, T2),
    composition_rule(T1, T2, Result).

%% composite_type(+C1, +C2, -ResultType)
% BACKWARD COMPAT: Uses analytical context
composite_type(C1, C2, Result) :-
    constraint_indexing:default_context(Ctx),
    composite_type(C1, C2, Ctx, Result).

%% composition_rule(+Type1, +Type2, -CompositeType)
% Formal modal composition rules from DR logic
% NOTE: These rules are NOT indexed - they're about logical structure
%       Context affects input types, not composition rules themselves

% Necessity Inheritance: ■ C₁ ∧ (C₁ → C₂) ⇒ ■ C₂
% If C1 is a Mountain and implies C2, then C2 is also a Mountain
composition_rule(mountain, _, mountain) :- !.
composition_rule(_, mountain, mountain) :- !.

% Extraction Dominance: ⊞C₁ ∧ ⊠ C₂ ∧ Embedded(C₂, C₁) ⇒ ⊠ (C₁ ∧ C₂)
% When a Snare is embedded in a Rope, the whole becomes extractive
composition_rule(rope, snare, snare) :- !.
composition_rule(snare, rope, snare) :- !.
composition_rule(tangled_rope, snare, snare) :- !.
composition_rule(snare, tangled_rope, snare) :- !.

% Snare Dominance: Multiple Snares compound
composition_rule(snare, snare, snare) :- !.

% Rope Composition: ⊞C₁ ∧ ⊞C₂ ∧ Compatible(C₁, C₂) ⇒ ⊞(C₁ ∧ C₂)
% Compatible Ropes can be composed into compound Ropes
composition_rule(rope, rope, rope) :- !.

% Tangled interactions
composition_rule(tangled_rope, tangled_rope, tangled_rope) :- !.
composition_rule(rope, tangled_rope, tangled_rope) :- !.
composition_rule(tangled_rope, rope, tangled_rope) :- !.

% Piton contamination
composition_rule(piton, _, piton) :- !.
composition_rule(_, piton, piton) :- !.

% Unknown fallback
composition_rule(_, _, unknown).

%% detect_extraction_dominance(+Composite, -Evidence)
% Detects when a Rope is corrupted by an embedded Snare
% NOTE: Uses default analytical context for detection
detect_extraction_dominance(Composite, Evidence) :-
    narrative_ontology:affects_constraint(Composite, Component),
    drl_core:dr_type(Component, snare),
    narrative_ontology:constraint_metric(Component, extractiveness, X),
    X >= 0.66,
    Evidence = embedded_snare(Component, X).

%% detect_necessity_inheritance(+Source, -Derived)
% Detects when a Mountain constraint logically implies another constraint
% NOTE: Uses default analytical context for detection
detect_necessity_inheritance(Source, Derived) :-
    drl_core:dr_type(Source, mountain),
    narrative_ontology:affects_constraint(Source, Derived),
    narrative_ontology:constraint_metric(Source, suppression_requirement, E_source),
    E_source =< 0.05,
    % If the derived constraint should also be a Mountain
    narrative_ontology:constraint_metric(Derived, suppression_requirement, E_derived),
    E_derived =< 0.05.

/* ================================================================
   STAGE 2: TRANSFORMATION TRACKING (CONTEXT-AWARE)
   Temporal modal logic for constraint evolution
   ================================================================ */

%% constraint_history(+C, +Context, -Timeline)
% PRIMARY API: Collects constraint history FROM A SPECIFIC CONTEXT
% Same constraint may have different transformation patterns from different perspectives
constraint_history(C, Context, Timeline) :-
    constraint_indexing:valid_context(Context),
    findall(state(T, Type), 
            (narrative_ontology:measurement(_, C, _, T, _),
             dr_type_at(C, T, Context, Type)),
            TimelineUnsorted),
    sort(TimelineUnsorted, Timeline).

%% constraint_history(+C, -Timeline)
% BACKWARD COMPAT: Uses analytical context
constraint_history(C, Timeline) :-
    constraint_indexing:default_context(Ctx),
    constraint_history(C, Ctx, Timeline).

%% dr_type_at(+C, +Time, +Context, -Type)
% Determines constraint type at specific time FROM SPECIFIC CONTEXT
% NOTE: This is the key integration point - past metrics + current context
dr_type_at(C, Time, Context, Type) :-
    % Get metrics at this historical time
    (narrative_ontology:measurement(_, C, suppression_requirement, Time, E) -> true ; E = 0.5),
    (narrative_ontology:measurement(_, C, extractiveness, Time, BaseX) -> true ; BaseX = 0.5),
    
    % Apply current context's power scaling to historical metrics
    Context = context(agent_power(Power), _, _, _),
    constraint_indexing:power_modifier(Power, Modifier),
    EffectiveX is BaseX * Modifier,
    
    % Classify using effective extractiveness
    classify_at_time_indexed(C, E, EffectiveX, Context, Type).

%% classify_at_time_indexed(+C, +E, +EffectiveX, +Context, -Type)
% Classification logic using power-scaled extractiveness
% Recognizes: mountain, rope, snare, tangled_rope, piton, scaffold

% Scaffold (temporary support structure)
classify_at_time_indexed(C, _E, X, _Context, scaffold) :-
    narrative_ontology:entity(C, scaffold),
    config:param(tangled_rope_extraction_ceil, Ceil),
    X =< Ceil, !.

% Mountain (appears unchangeable from this context)
classify_at_time_indexed(_C, E, _X, Context, mountain) :-
    config:param(mountain_suppression_ceiling, Ceil),
    E =< Ceil,
    constraint_indexing:effective_immutability_for_context(Context, mountain), !.

% Snare (extractive + enforced + changeable)
classify_at_time_indexed(_C, E, X, Context, snare) :-
    config:param(snare_extraction_floor, XFloor),
    config:param(snare_suppression_floor, EFloor),
    X >= XFloor,
    E >= EFloor,
    constraint_indexing:effective_immutability_for_context(Context, rope), !.

% Tangled Rope (moderate extraction)
classify_at_time_indexed(_C, _E, X, Context, tangled_rope) :-
    config:param(rope_extraction_ceiling, RopeX),
    config:param(tangled_rope_extraction_ceil, TangledX),
    X > RopeX,
    X =< TangledX,
    constraint_indexing:effective_immutability_for_context(Context, rope), !.

% Piton (low extraction, high maintenance)
classify_at_time_indexed(_C, E, X, _Context, piton) :-
    config:param(piton_extraction_ceiling, XCeil),
    X =< XCeil,
    E > XCeil, !.

% Rope (low extraction, changeable)
classify_at_time_indexed(_C, _E, X, Context, rope) :-
    config:param(rope_extraction_ceiling, XCeil),
    X =< XCeil,
    constraint_indexing:effective_immutability_for_context(Context, rope), !.

% Unknown fallback
classify_at_time_indexed(_C, _E, _X, _Context, unknown).

%% transformation_detected(+C, +FromType, +ToType, -T1, -T2)
% Detects when constraint transformed from one type to another
% Uses actual measurement times (not iteration)
transformation_detected(C, From, To, T1, T2) :-
    constraint_history(C, Timeline),
    member(state(T1, From), Timeline),
    member(state(T2, To), Timeline),
    T2 > T1,
    From \= To,
    % Ensure no intermediate different type
    \+ (member(state(Tm, Mid), Timeline),
        Tm > T1, Tm < T2,
        Mid \= From, Mid \= To).

%% transformation_type(+C, +From, +To, +T1, +T2, -Label)
% Classifies the type of transformation with semantic label

transformation_type(C, rope, snare, T1, T2, capture) :-
    transformation_detected(C, rope, snare, T1, T2),
    check_capture_between(C, T1, T2).

transformation_type(C, rope, piton, T1, T2, obsolescence) :-
    transformation_detected(C, rope, piton, T1, T2),
    \+ check_capture_between(C, T1, T2).

transformation_type(C, scaffold, snare, T1, T2, calcification) :-
    transformation_detected(C, scaffold, snare, T1, T2),
    narrative_ontology:entity(C, scaffold),
    check_capture_between(C, T1, T2).

transformation_type(C, mountain, rope, T1, T2, discovery) :-
    transformation_detected(C, mountain, rope, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

transformation_type(C, mountain, snare, T1, T2, discovery) :-
    transformation_detected(C, mountain, snare, T1, T2),
    narrative_ontology:constraint_claim(C, mountain).

%% canonical_transformation(?C, ?From, ?To, -T1_earliest, -T2_latest, ?Label)
% Returns canonical (deduplicated) transformation
canonical_transformation(C, From, To, T1_earliest, T2_latest, Label) :-
    setof((T1, T2), transformation_type(C, From, To, T1, T2, Label), Pairs),
    findall(T1, member((T1, _), Pairs), T1s),
    findall(T2, member((_, T2), Pairs), T2s),
    min_list(T1s, T1_earliest),
    max_list(T2s, T2_latest).

%% check_capture_between(+C, +T1, +T2)
% Helper: detects if beneficiaries became concentrated
check_capture_between(C, T1, T2) :-
    narrative_ontology:measurement(_, C, extractiveness, T1, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    X2 > X1,
    X2 >= 0.66.

%% predict_transformation(+C, +CurrentType, -LikelyFutureType)
% Predicts likely future transformation based on trajectory
% NOTE: Uses analytical context for prediction
predict_transformation(C, rope, snare) :-
    findall(X, narrative_ontology:measurement(_, C, extractiveness, _, X), Xs),
    length(Xs, N), N >= 2,
    last(Xs, X_latest),
    X_latest > 0.5,
    X_latest < 0.66,
    Xs = [X_first|_],
    X_latest > X_first.

predict_transformation(C, rope, piton) :-
    findall(E, narrative_ontology:measurement(_, C, suppression_requirement, _, E), Es),
    length(Es, N), N >= 2,
    last(Es, E_latest),
    E_latest > 0.3,
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X < 0.35.

predict_transformation(C, tangled_rope, snare) :-
    narrative_ontology:constraint_metric(C, extractiveness, X),
    X > 0.5.

/* ================================================================
   STAGE 3: COUNTERFACTUAL REASONING (INDEXED)
   Reasoning about possible worlds after interventions
   NOW INDEXED: "What happens if WE cut this?" depends on WHO "we" are
   ================================================================ */

%% simulate_cut(+Constraint, +Context, -Effects)
% PRIMARY API: Simulates cutting a constraint FROM A SPECIFIC CONTEXT
% CRITICAL: Same cut has different effects depending on who does it
%
% Example: Cutting property rights
%   FROM institutional context: moderate coordination disruption
%   FROM powerless context: catastrophic (removes only shelter access)
simulate_cut(Constraint, Context, Effects) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(Constraint, Context, Type),
    (Type = snare ; Type = piton ; Type = rope),  % Only cut these types
    findall(effect(Target, Impact, Reason),
            dependency_chain(Constraint, Target, Impact, Reason, Context),
            Effects).

%% simulate_cut(+Constraint, -Effects)
% BACKWARD COMPAT: Uses analytical context
simulate_cut(Constraint, Effects) :-
    constraint_indexing:default_context(Ctx),
    simulate_cut(Constraint, Ctx, Effects).

%% dependency_chain(+Source, -Target, -Impact, -Reason, +Context)
% PRIMARY API: Discovers dependencies FROM A SPECIFIC CONTEXT
% CRITICAL UPGRADE: "Depends on" is now relative to WHO would be affected
%
% What's load-bearing from powerless context may be non-essential from institutional
dependency_chain(Source, Target, Impact, Reason, Context) :-
    constraint_indexing:valid_context(Context),
    
    % 1. Explicit dependencies (declared in data)
    narrative_ontology:affects_constraint(Source, Target),
    estimate_impact_indexed(Source, Target, Context, Impact, Reason).

dependency_chain(Source, Target, Impact, Reason, Context) :-
    constraint_indexing:valid_context(Context),
    
    % 2. Inferred structural coupling
    infer_structural_coupling(Source, Target, Strength),
    Strength > 0.85,
    
    % But assess impact FROM THIS CONTEXT
    (   context_depends_critically(Target, Source, Context)
    ->  Impact = catastrophic,
        Reason = inferred_load_bearing_coupling
    ;   Impact = moderate,
        Reason = inferred_weak_coupling
    ).

%% dependency_chain(+Source, -Target, -Impact, -Reason)
% BACKWARD COMPAT: Uses analytical context
dependency_chain(Source, Target, Impact, Reason) :-
    constraint_indexing:default_context(Ctx),
    dependency_chain(Source, Target, Impact, Reason, Ctx).

%% context_depends_critically(+Target, +Source, +Context)
% NEW: Determines if Target critically depends on Source FROM CONTEXT
% Used to detect load-bearing relationships per perspective
context_depends_critically(Target, Source, Context) :-
    % Target appears as Mountain from this context
    drl_core:dr_type(Target, Context, mountain),
    
    % Source provides the only perceived stability
    drl_core:dr_type(Source, Context, SourceType),
    member(SourceType, [rope, snare]),
    
    % Check if Target's stability requires Source
    narrative_ontology:affects_constraint(Source, Target).

%% estimate_impact_indexed(+Source, +Target, +Context, -Impact, -Reason)
% Estimates impact of cutting Source on Target FROM SPECIFIC CONTEXT
% CRITICAL: Same cut has different impacts depending on who's cutting

estimate_impact_indexed(Source, Target, Context, catastrophic, load_bearing) :-
    % Source is load-bearing FROM THIS CONTEXT
    drl_core:dr_type(Source, Context, snare),
    narrative_ontology:constraint_metric(Source, extractiveness, X),
    config:param(snare_load_bearing_threshold, T),
    X > T,
    
    % Target depends on it FROM THIS CONTEXT
    drl_core:dr_type(Target, Context, mountain),
    !.

estimate_impact_indexed(Source, Target, Context, beneficial, removes_extraction) :-
    % Source is extractive, Target is functional
    drl_core:dr_type(Source, Context, snare),
    drl_core:dr_type(Target, Context, rope),
    !.

estimate_impact_indexed(Source, Target, Context, moderate, disrupts_coordination) :-
    % Both are coordination mechanisms
    drl_core:dr_type(Source, Context, rope),
    drl_core:dr_type(Target, Context, rope),
    !.

estimate_impact_indexed(_, _, _, negligible, no_dependency) :- !.

%% infer_structural_coupling(+C1, +C2, -Strength)
% Discovers hidden dependencies via temporal correlation
% NOTE: This is NOT indexed - coupling is structural fact
infer_structural_coupling(C1, C2, Strength) :-
    C1 \= C2,
    findall(G1, dr_gradient_at(C1, _, G1), Gs1),
    findall(G2, dr_gradient_at(C2, _, G2), Gs2),
    length(Gs1, L), L > 1, length(Gs2, L),
    calculate_coupling_strength(Gs1, Gs2, Strength).

dr_gradient_at(C, T, Grad) :-
    narrative_ontology:measurement(_, C, extractiveness, T, X1),
    narrative_ontology:measurement(_, C, extractiveness, T2, X2),
    T2 > T, !,
    Grad is X2 - X1.

calculate_coupling_strength([], [], 1.0).
calculate_coupling_strength([H1|T1], [H2|T2], S) :-
    ( (H1 > 0, H2 > 0) ; (H1 < 0, H2 < 0) ; (H1 == 0, H2 == 0) ),
    calculate_coupling_strength(T1, T2, SubS),
    S is 0.2 + SubS.
calculate_coupling_strength([_|T1], [_|T2], S) :-
    calculate_coupling_strength(T1, T2, S).

%% assess_scaffold_need(+Constraint, +Context, -Assessment)
% PRIMARY API: Determines if cutting requires Scaffold FROM SPECIFIC CONTEXT
% THEOREM 3 INDEXED: "Need scaffold" depends on WHO is cutting
%
% Critical case: Institutional cutting powerless Mountain
%   FROM institutional context: no_scaffold_needed (just coordination)
%   FROM powerless context: scaffold_required (removes survival support)
%   SYSTEM MUST EVALUATE BOTH CONTEXTS before allowing cut
assess_scaffold_need(Constraint, Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(Constraint, Context, Type),
    member(Type, [snare, piton, rope]),
    
    simulate_cut(Constraint, Context, Effects),
    
    (   member(effect(_, catastrophic, load_bearing), Effects)
    ->  (   narrative_ontology:entity(Scaffold, scaffold),
            Scaffold \= Constraint
        ->  Assessment = scaffold_present
        ;   Assessment = scaffold_required
        )
    ;   Assessment = no_scaffold_needed
    ).

%% assess_scaffold_need(+Constraint, -Assessment)
% BACKWARD COMPAT: Uses analytical context
assess_scaffold_need(Constraint, Assessment) :-
    constraint_indexing:default_context(Ctx),
    assess_scaffold_need(Constraint, Ctx, Assessment).

%% counterfactual_world(+Intervention, +CurrentWorld, +Context, -FutureWorld)
% PRIMARY API: Models state after intervention FROM SPECIFIC CONTEXT
counterfactual_world(cut(C), current, Context, after_cut) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),
    format('In world after cutting ~w (~w from ~w):~n', [C, Type, Context]),
    simulate_cut(C, Context, Effects),
    forall(member(effect(Target, Impact, Reason), Effects),
           format('  - ~w: ~w (~w)~n', [Target, Impact, Reason])).

counterfactual_world(add_scaffold(S, For), current, Context, with_scaffold) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(For, Context, snare),
    format('In world with scaffold ~w for ~w (from ~w):~n', [S, For, Context]),
    format('  - Temporary support for transition~n'),
    format('  - Allows safe removal of ~w~n', [For]).

%% counterfactual_world(+Intervention, +CurrentWorld, -FutureWorld)
% BACKWARD COMPAT: Uses analytical context
counterfactual_world(Intervention, Current, Future) :-
    constraint_indexing:default_context(Ctx),
    counterfactual_world(Intervention, Current, Ctx, Future).

/* ================================================================
   CROSS-CONTEXT DEPENDENCY ANALYSIS (NEW)
   Critical for detecting "institutional Mountain collapse"
   ================================================================ */

%% check_all_contexts(+Constraint, -MultiContextReport)
% NEW: Evaluates cutting a constraint across multiple standard contexts
% CRITICAL FOR SAFETY: Before cutting, check impact on ALL perspectives
%
% Example usage:
%   check_all_contexts(property_rights_2025, Report)
%   → Shows if cut is safe for institution but catastrophic for powerless
check_all_contexts(Constraint, Report) :-
    findall(
        context_impact(Context, Type, ScaffoldNeed, Effects),
        (   standard_context(Context),
            drl_core:dr_type(Constraint, Context, Type),
            assess_scaffold_need(Constraint, Context, ScaffoldNeed),
            simulate_cut(Constraint, Context, Effects)
        ),
        Impacts
    ),
    Report = multi_context_analysis(Constraint, Impacts).

% Standard contexts for cross-perspective analysis
standard_context(context(agent_power(individual_powerless), 
                        time_horizon(biographical), 
                        exit_options(trapped), 
                        spatial_scope(local))).

standard_context(context(agent_power(individual_moderate), 
                        time_horizon(biographical), 
                        exit_options(mobile), 
                        spatial_scope(national))).

standard_context(context(agent_power(institutional), 
                        time_horizon(generational), 
                        exit_options(arbitrage), 
                        spatial_scope(national))).

standard_context(context(agent_power(analytical), 
                        time_horizon(civilizational), 
                        exit_options(analytical), 
                        spatial_scope(global))).

% ============================================================================
% NEW: INDEXICAL PERSPECTIVE AUDIT
% ============================================================================
% Checks if two agents experience a structural conflict (Risk Gap)
% Implements Section IV-B
detect_perspectival_risk(ConstraintID, Agent1, Agent2, RiskLabel) :-
    % 1. Get the base extraction score for the constraint
    constraint_data(ConstraintID, Data),
    member(base_extractiveness(X_base), Data),
    
    % 2. Calculate what each agent "sees" based on their power index
    % Uses the pi scaling function from drl_audit_core
    agent_index(Agent1, context(Power1, _, _, _)),
    agent_index(Agent2, context(Power2, _, _, _)),
    
    drl_audit_core:effective_extraction(X_base, Power1, Chi1),
    drl_audit_core:effective_extraction(X_base, Power2, Chi2),
    
    % 3. Determine the structural type for each agent
    drl_audit_core:structural_signature(Chi1, Data, Type1),
    drl_audit_core:structural_signature(Chi2, Data, Type2),
    
    % 4. Match against the Risk Table
    drl_audit_core:omega_risk(Type1, Type2, RiskLabel, _).

% possibly(C) is true if the constraint is a Rope (changeable/contingent)
possibly(C) :- 
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, rope).

% necessarily(C) is true if the constraint is a Mountain (fixed/required)
necessarily(C) :- 
    constraint_indexing:default_context(Ctx),
    drl_core:dr_type(C, Ctx, mountain).

/* ================================================================
   UTILITY PREDICATES
   ================================================================ */

%% last(+List, -Last)
% Gets the last element of a list
last([X], X) :- !.
last([_|Xs], Last) :- last(Xs, Last).

% Provide a fail-safe default so they are "defined" even if no data is loaded
agent_index(_, _) :- fail.
constraint_data(_, _) :- fail.

/* ================================================================
   VERSION & MIGRATION INFO
   ================================================================ */

/*
VERSION HISTORY:
v4.0 (2025-01-17):
  - BREAKING: Full indexical relativity integration
  - NEW: dependency_chain/5 (added Context)
  - NEW: assess_scaffold_need/3 (added Context)
  - NEW: simulate_cut/3 (added Context)
  - NEW: context_depends_critically/3
  - NEW: estimate_impact_indexed/5
  - NEW: check_all_contexts/2 (multi-perspective safety check)
  - CHANGED: All dependency analysis is context-relative
  - MAINTAIN: Backward compatibility via /4, /3, /2 versions

v3.x:
  - Non-indexed modal logic
  - Single "God's eye view" dependency chains

MIGRATION GUIDE v3.x → v4.0:
  Old: dependency_chain(source, target, impact, reason)
  New: dependency_chain(source, target, impact, reason, context)
  
  Old: assess_scaffold_need(snare, assessment)
  New: assess_scaffold_need(snare, context, assessment)
  
  CRITICAL NEW REQUIREMENT:
  Before cutting any constraint, MUST evaluate:
    check_all_contexts(constraint, Report)
  to detect if cut is safe from one context but catastrophic from another.
  
  This prevents "institutional Mountain collapse" - where institution
  cuts what they see as Rope, unknowingly destroying powerless Mountain.
*/
