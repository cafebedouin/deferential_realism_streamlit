% ============================================================================
% DRL CORE - INDEXICAL CONSTRAINT CLASSIFICATION v4.0
% ============================================================================
% This module implements context-indexed Deferential Realism classification.
% Every Mountain/Rope/Snare judgment is INDEXED to WHO/WHEN/WHERE/HOW.
%
% BREAKING CHANGES FROM v3.x:
% - dr_type/2 now defaults to analytical context (backward compatible)
% - dr_type/3 is PRIMARY API (adds Context parameter)
% - All classification uses power-scaled extractiveness
% - Structural signatures integrated with indexical logic
% - Action routing context-aware
%
% Integration: Load after constraint_indexing.pl, before drl_modal_logic.pl
% ============================================================================

:- module(drl_core, [
    % PRIMARY API - Context-Indexed Classification
    dr_type/3,                      % dr_type(Constraint, Context, Type)
    dr_type/2,                      % Backward compat: uses default context
    
    % Action Routing (Indexed)
    dr_action/3,                    % dr_action(Constraint, Context, Action)
    dr_action/2,                    % Backward compat
    
    % Error Detection (Indexed)
    dr_mismatch/4,                  % dr_mismatch(C, Context, ErrorType, Severity)
    dr_mismatch/3,                  % Backward compat
    
    % Structural Signature Integration
    dr_signature/2,                 % dr_signature(Constraint, Signature)
    
    % Re-exported from constraint_indexing
    constraint_classification/3,    % Multifile hook for data files
    constraint_claim_indexed/2,     % Legacy wrapper
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1,

    % Centralize module references
    base_extractiveness/2,
    suppression_score/2,
    requires_active_enforcement/1,
    emerges_naturally/1,
    
    % Exposed helpers for modal_logic and testing
    is_mountain/3,                  % Indexed version
    is_rope/3,
    is_snare/3,
    is_tangled_rope/3,
    is_scaffold/3,
    is_piton/3
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(structural_signatures).
:- use_module(constraint_indexing).
:- use_module(constraint_instances).
:- use_module(domain_priors).

% Declare these as multifile to allow other modules to contribute data
:- multifile 
    base_extractiveness/2, 
    suppression_score/2, 
    requires_active_enforcement/1, 
    emerges_naturally/1.

% Provide a 'fail-safe' default clause to satisfy the compiler
base_extractiveness(_, 0.0) :- fail.
suppression_score(_, 0.0) :- fail.
requires_active_enforcement(_) :- fail.
emerges_naturally(_) :- fail.

% Re-export indexed classification predicates from constraint_indexing
:- reexport(constraint_indexing, [
    constraint_classification/3,
    constraint_claim_indexed/2,
    multi_index_report/1,
    compare_perspectives/2,
    discover_my_context/1
]).

% ============================================================================
% CLASSIFICATION LOGIC - INDEXED VERSION (PRIMARY)
% ============================================================================

% ----------------------------------------------------------------------------
% Mountain Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Mountain FROM A CONTEXT if:
% 1. It appears immutable given time horizon + exit options
% 2. Suppression requirement is below threshold

is_mountain(C, Context, mountain) :-
    % Check time-based immutability perception
    constraint_indexing:effective_immutability_for_context(Context, mountain),
    
    % Check suppression is low (inherent, not enforced)
    config:param(mountain_suppression_ceiling, Ceil),
    narrative_ontology:constraint_metric(C, suppression_requirement, E),
    E =< Ceil,
    !.

is_mountain(_C, _Context, fail).

% ----------------------------------------------------------------------------
% Rope Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Rope FROM A CONTEXT if:
% 1. Effective extractiveness (power-scaled) is below threshold
% 2. It's changeable (time horizon allows modification)

is_rope(C, Context, rope) :-
    % Calculate power-scaled extractiveness
    constraint_indexing:extractiveness_for_agent(C, Context, EffectiveX),
    
    % Check against rope threshold
    config:param(rope_extraction_ceiling, XCeil),
    EffectiveX =< XCeil,
    
    % Must be perceived as changeable
    constraint_indexing:effective_immutability_for_context(Context, rope),
    !.

is_rope(_C, _Context, fail).

% ----------------------------------------------------------------------------
% Snare Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Snare FROM A CONTEXT if:
% 1. Effective extractiveness (power-scaled) exceeds floor
% 2. Requires active suppression (enforced)
% 3. Perceived as changeable (not Mountain)

is_snare(C, Context, snare) :-
    % Calculate power-scaled extractiveness
    constraint_indexing:extractiveness_for_agent(C, Context, EffectiveX),
    
    % Check extraction floor
    config:param(snare_extraction_floor, XFloor),
    EffectiveX >= XFloor,
    
    % Check suppression requirement
    config:param(snare_suppression_floor, EFloor),
    narrative_ontology:constraint_metric(C, suppression_requirement, E),
    E >= EFloor,
    
    % Must NOT appear as Mountain from this context
    constraint_indexing:effective_immutability_for_context(Context, rope),
    !.

is_snare(_C, _Context, fail).

% ----------------------------------------------------------------------------
% Tangled Rope Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Tangled Rope FROM A CONTEXT if:
% 1. Effective extractiveness in middle-to-high range (uses config params)
% 2. High suppression (uses config params)
% 3. Has BOTH coordination function AND asymmetric extraction
% 4. Requires active enforcement
%
% Updated January 2026 based on empirical validation (168/467 constraints, 36%)
%
% This is genuinely hybrid: provides coordination (like rope) while extracting
% (like snare). Requires surgical reform to preserve coordination while cutting extraction.

is_tangled_rope(C, Context, tangled_rope) :-
    % Calculate power-scaled extractiveness
    constraint_indexing:extractiveness_for_agent(C, Context, EffectiveX),

    % Check tangled rope range (from config)
    config:param(tangled_rope_extraction_floor, FloorX),
    config:param(tangled_rope_extraction_ceil, CeilX),
    EffectiveX >= FloorX,
    EffectiveX =< CeilX,

    % High suppression required (from config)
    config:param(tangled_rope_suppression_floor, MinS),
    narrative_ontology:constraint_metric(C, suppression_requirement, S),
    S >= MinS,

    % Must require active enforcement (constructed constraint)
    requires_active_enforcement(C),

    % Must have both coordination function AND asymmetric extraction
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C),
    !.

is_tangled_rope(_C, _Context, fail).

% ----------------------------------------------------------------------------
% Scaffold Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Scaffold FROM A CONTEXT if:
% 1. Effective extraction is below the coordination ceiling (v3.4: 0.30)
% 2. It possesses a genuine coordination function
% 3. It contains a formal sunset clause (temporal limit)
% 4. The time horizon (T) has not exceeded the designated utility period
%
% Scaffolds are temporary supports. High suppression is only acceptable if 
% it is designed to decline or expire as the system stabilizes.

is_scaffold(C, Context, scaffold) :-
    % Calculate power-scaled extractiveness
    constraint_indexing:extractiveness_for_agent(C, Context, EffectiveX),

    % Check scaffold ceiling (v3.4 requirement: extraction < 0.30)
    config:param(scaffold_extraction_ceil, MaxX),
    EffectiveX =< MaxX,

    % Must possess a coordination function (the "support" aspect)
    narrative_ontology:has_coordination_function(C),

    % Mandatory: Must have a sunset clause to distinguish from a permanent Rope
    narrative_ontology:has_sunset_clause(C),

    % Validate Indexical Alignment (Time Horizon T)
    % Scaffolds typically expire before 'civilizational' or 'historical' scales.
    Context = context(_, time_horizon(T), _, _),
    member(T, [immediate, biographical, generational]),

    % Ensure it is not currently flagged as 'degraded' (Piton check)
    \+ (narrative_ontology:constraint_metric(C, theater_ratio, TR), TR > 0.70),
    !.

is_scaffold(_C, _Context, fail).

% ----------------------------------------------------------------------------
% Piton Test (Indexed)
% ----------------------------------------------------------------------------
% A constraint is a Piton FROM A CONTEXT if:
% 1. Low effective extractiveness
% 2. High suppression (expensive to maintain)
% 3. Should be cut but isn't

is_piton(C, Context, piton) :-
    % Calculate power-scaled extractiveness
    constraint_indexing:extractiveness_for_agent(C, Context, EffectiveX),
    
    % Low extraction
    config:param(piton_extraction_ceiling, XCeil),
    EffectiveX =< XCeil,
    
    % High suppression (maintenance cost)
    \+ (narrative_ontology:constraint_metric(C, theater_ratio, TR), TR > 0.70),
    !.

is_piton(_C, _Context, fail).

% ============================================================================
% CANONICAL TYPE DETERMINATION (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Primary Classification: dr_type/3
% ----------------------------------------------------------------------------
% Determines constraint type FROM A SPECIFIC CONTEXT
% Integrates: (1) Metric-based classification, (2) Structural signatures

dr_type(C, Context, Type) :-
    % Validate context
    constraint_indexing:valid_context(Context),
    
    % First: Try metric-based classification with power scaling
    metric_based_type_indexed(C, Context, MetricType),
    
    % Second: Check if structural signature overrides
    structural_signatures:integrate_signature_with_modal(C, MetricType, Type),
    !.

dr_type(_C, _Context, unknown).

% ----------------------------------------------------------------------------
% Metric-Based Classification (Indexed) - Helper
% ----------------------------------------------------------------------------

metric_based_type_indexed(C, Context, mountain) :-
    is_mountain(C, Context, mountain), !.

metric_based_type_indexed(C, Context, snare) :-
    is_snare(C, Context, snare), !.

metric_based_type_indexed(C, Context, rope) :-
    is_rope(C, Context, rope), !.

metric_based_type_indexed(C, Context, tangled_rope) :-
    is_tangled_rope(C, Context, tangled_rope), !.

metric_based_type_indexed(C, Context, piton) :-
    is_piton(C, Context, piton), !.

metric_based_type_indexed(_C, _Context, unknown).

% ============================================================================
% BACKWARD COMPATIBILITY LAYER
% ============================================================================

% ----------------------------------------------------------------------------
% Legacy dr_type/2 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_type(C, Type) :-
    constraint_indexing:default_context(Ctx),
    dr_type(C, Ctx, Type).

% ============================================================================
% ACTION ROUTING (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Primary Action Router: dr_action/3
% ----------------------------------------------------------------------------
% Recommends action based on constraint type FROM SPECIFIC CONTEXT

dr_action(C, Context, accept) :-
    dr_type(C, Context, mountain), !.

dr_action(C, Context, maintain) :-
    dr_type(C, Context, rope), !.

dr_action(C, Context, reform) :-
    dr_type(C, Context, tangled_rope), !.

dr_action(C, Context, cut) :-
    dr_type(C, Context, snare), !.

dr_action(C, Context, bypass) :-
    dr_type(C, Context, piton), !.

dr_action(_C, _Context, investigate).

% ----------------------------------------------------------------------------
% Legacy dr_action/2 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_action(C, Action) :-
    constraint_indexing:default_context(Ctx),
    dr_action(C, Ctx, Action).

% ============================================================================
% ERROR DETECTION (INDEXED)
% ============================================================================

% ----------------------------------------------------------------------------
% Type 1: False Mountain (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Mountain but ISN'T from this context

dr_mismatch(C, Context, type_1_false_summit, severe) :-
    % Check if claimed as mountain in data
    narrative_ontology:constraint_claim(C, mountain),
    
    % Verify it's NOT a mountain from this context
    is_mountain(C, Context, fail),
    !.

% ----------------------------------------------------------------------------
% Type 3: Snare Misidentified as Rope (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Rope but is actually Snare from this context

dr_mismatch(C, Context, type_3_snare_as_rope, severe) :-
    narrative_ontology:constraint_claim(C, rope),
    is_snare(C, Context, snare),
    !.

% ----------------------------------------------------------------------------
% Type 5: Piton Misidentified as Snare (Indexed)
% ----------------------------------------------------------------------------
% Claimed as Snare but is actually Piton from this context

dr_mismatch(C, Context, type_5_piton_as_snare, moderate) :-
    narrative_ontology:constraint_claim(C, theater_ratio, TR), TR >= 0.70,
    is_piton(C, Context, piton),
    !.

% ----------------------------------------------------------------------------
% Type 7: Perspectival Incoherence (NEW)
% ----------------------------------------------------------------------------
% Same constraint classified differently across meaningful perspectives
% This is NOT an error but a FEATURE - it indicates indexical relativity

dr_mismatch(C, perspectival_gap(Type1, Ctx1, Type2, Ctx2), 
            perspectival_incoherence, informational) :-
    % Find two different classifications
    constraint_indexing:constraint_classification(C, Type1, Ctx1),
    constraint_indexing:constraint_classification(C, Type2, Ctx2),
    
    % Types must differ
    Type1 \= Type2,
    
    % Contexts must differ in meaningful way (not analytical vs analytical)
    Ctx1 = context(agent_power(P1), _, _, _),
    Ctx2 = context(agent_power(P2), _, _, _),
    P1 \= analytical,
    P2 \= analytical,
    P1 \= P2,
    !.

% ----------------------------------------------------------------------------
% Legacy dr_mismatch/3 - Defaults to Analytical Context
% ----------------------------------------------------------------------------

dr_mismatch(C, ErrorType, Severity) :-
    constraint_indexing:default_context(Ctx),
    dr_mismatch(C, Ctx, ErrorType, Severity).

% ============================================================================
% STRUCTURAL SIGNATURE DETECTION
% ============================================================================

%% dr_signature(+Constraint, -Signature)
%  Detects structural signature: natural_law | coordination_scaffold | 
%  constructed_constraint | ambiguous
%
%  NOTE: Signatures are NOT indexed - they represent the constraint's
%        fundamental structure, not perspectival appearance

dr_signature(C, Signature) :-
    structural_signatures:constraint_signature(C, Signature).

% ============================================================================
% PERSPECTIVAL GAP DETECTION (NEW)
% ============================================================================

%% perspectival_gap(+Constraint, -GapReport)
%  Detects when same constraint classifies differently across perspectives
%  Returns structured gap information for Omega generation

perspectival_gap(C, gap(Type1, Ctx1, Type2, Ctx2, PowerDelta)) :-
    % Find two classifications
    constraint_indexing:constraint_classification(C, Type1, Ctx1),
    constraint_indexing:constraint_classification(C, Type2, Ctx2),
    
    % Must differ in type
    Type1 \= Type2,
    
    % Extract power levels
    Ctx1 = context(agent_power(P1), _, _, _),
    Ctx2 = context(agent_power(P2), _, _, _),
    
    % Must be non-analytical
    P1 \= analytical,
    P2 \= analytical,
    
    % Must differ in power
    P1 \= P2,
    
    % Calculate power delta for severity assessment
    constraint_indexing:power_modifier(P1, Mod1),
    constraint_indexing:power_modifier(P2, Mod2),
    PowerDelta is abs(Mod1 - Mod2),
    !.

% ============================================================================
% INDEXICAL ANALYSIS UTILITIES (NEW)
% ============================================================================

%% cross_context_analysis(+Constraint, -Analysis)
%  Analyzes how constraint appears across standard contexts
%  Useful for validation and debugging

cross_context_analysis(C, Analysis) :-
    findall(
        context_result(Ctx, Type),
        (standard_context(Ctx),
         dr_type(C, Ctx, Type)),
        Results
    ),
    Analysis = cross_context(C, Results).

% Standard contexts for testing
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
% VERSION & COMPATIBILITY INFO
% ============================================================================

/*
VERSION HISTORY:
v4.0 (2025-01-17):
  - BREAKING: dr_type/3 is now primary API (added Context)
  - NEW: Full indexical relativity integration
  - NEW: Power-scaled extractiveness in all classifications
  - NEW: Perspectival gap detection
  - NEW: dr_action/3, dr_mismatch/4 (indexed versions)
  - CHANGED: All classification uses effective_extractiveness
  - MAINTAIN: Backward compatibility via dr_type/2, dr_action/2

v3.2:
  - Added structural signature integration
  - Signature overrides metric-based classification

v3.1:
  - Consolidated namespace
  - Added piton detection

v3.0:
  - Initial metric-based classification

MIGRATION GUIDE v3.x → v4.0:
  Old: dr_type(constraint, Type)
  New: dr_type(constraint, Context, Type)
  
  For backward compat, old form still works (uses analytical context).
  
  To get indexed classification:
    constraint_indexing:default_context(Ctx),  % or build custom context
    dr_type(constraint, Ctx, Type)
*/
