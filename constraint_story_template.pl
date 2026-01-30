% ============================================================================
% CONSTRAINT STORY: [CONSTRAINT_ID]
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: [DATE]
% ============================================================================

:- module(constraint_[id], []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: [unique_identifier]
 * human_readable: [Descriptive name]
 * domain: [economic/political/social/religious/technological]
 * * SUMMARY:
 * [2-3 sentence description of the constraint and its context]
 * * KEY AGENTS:
 * - [Agent1]: Subject (Powerless)
 * - [Agent2]: Beneficiary (Institutional)
 * - [Agent3]: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness([id], [0.0-1.0]). % Mountain <= 0.05, Snare >= 0.46
domain_priors:suppression_score([id], [0.0-1.0]).   % Alternatives visibility
domain_priors:theater_ratio([id], [0.0-1.0]).       % Piton detection (> 0.70)

% Binary flags
% narrative_ontology:has_sunset_clause([id]).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement([id]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
constraint_indexing:constraint_classification([id], [snare/mountain], 
    context(agent_power(individual_powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification([id], rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects hybrid extraction/coordination signatures.
constraint_indexing:constraint_classification([id], tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness([id], E), E >= 0.50,
    domain_priors:suppression_score([id], S), S > 0.40.

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
constraint_indexing:constraint_classification([id], scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause([id]).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
constraint_indexing:constraint_classification([id], piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio([id], TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests([id]_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification([id], snare, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification([id], rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness([id], E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests([id]_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * [Why did you assign these specific scores? Explain the Perspectival Gap.]
 * * MANDATROPHY ANALYSIS:
 * [How does the Tangled Rope classification prevent the system from 
 * mislabeling coordination as pure extraction?]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_[id],
    '[The primary structural ambiguity]',
    '[What historical/empirical data would resolve this?]',
    '[Result if True vs Result if False]',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval([id], 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
