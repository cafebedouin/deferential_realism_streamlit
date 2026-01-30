:- module(structural_signatures, [
    constraint_signature/2,
    signature_confidence/3,
    explain_signature/3,
    integrate_signature_with_modal/3
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).

/* ================================================================
   STRUCTURAL SIGNATURE DETECTION v3.2
   
   Problem Statement (from Copilot's analysis):
   "Your classifier sees MAGNITUDE but not TYPE."
   
   The current DRL classifier uses only metric thresholds:
   - suppression > 0.1 → not a mountain
   - extractiveness > 0.7 → snare
   
   This causes misclassification of conceptual constraints:
   - Chaitin's Ω: collapse=1.0, suppression=0.0 → classified as mountain
   - Special Relativity: collapse=1.0, suppression=0.0 → classified as mountain
   - Arrow's Theorem: collapse=0.9, suppression=0.1 → fraud detection fires
   
   But these are STRUCTURALLY DIFFERENT:
   - Chaitin's Ω = NATURAL LAW (inherent impossibility)
   - Special Relativity = COORDINATION SCAFFOLD (successful standard)
   - Arrow's Theorem = NATURAL LAW (mathematical impossibility)
   
   Solution: Add STRUCTURAL SIGNATURES that detect constraint ORIGIN
   rather than just constraint METRICS.
   
   Three Core Signatures:
   1. Natural Law - empirical invariant, no alternatives possible
   2. Coordination Scaffold - voluntary equilibrium, alternatives existed
   3. Constructed Constraint - enforced rule, power asymmetries
   ================================================================ */

%% constraint_signature(+ConstraintID, -Signature)
%  Main entry point: classifies structural signature
%  Returns: natural_law | coordination_scaffold | constructed_constraint | ambiguous
constraint_signature(C, Signature) :-
    get_constraint_profile(C, Profile),
    classify_by_signature(Profile, Signature).

/* ================================================================
   PROFILE EXTRACTION
   
   Extracts 7 key features for signature classification:
   1. Accessibility Collapse (avg across time)
   2. Suppression Requirement (avg across time)
   3. Resistance Level (avg across time)
   4. Beneficiary Count (how many asymmetric winners)
   5. Has Viable Alternatives (were there choices?)
   6. Temporal Stability (does it evolve or remain constant?)
   7. Coordination Success (high access + low enforcement)
   ================================================================ */

get_constraint_profile(C, 
                      profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives, 
                             TemporalStability, CoordinationSuccess)) :-
    
    % Get averaged metrics across all levels
    get_metric_average(C, accessibility_collapse, AccessCollapse),
    get_metric_average(C, suppression_requirement, Suppression),
    get_metric_average(C, resistance, Resistance),
    
    % Count asymmetric beneficiaries
    count_power_beneficiaries(C, BeneficiaryCount),
    
    % Check for viable alternatives
    has_viable_alternatives(C, HasAlternatives),
    
    % Compute temporal stability
    compute_temporal_stability(C, TemporalStability),
    
    % Check coordination success pattern
    CoordinationSuccess = (AccessCollapse > 0.8, Suppression < 0.2).

%% get_metric_average(+Constraint, +MetricType, -Average)
%  Averages a metric across all levels (structural, organizational, class, individual)
get_metric_average(C, MetricType, Average) :-
    findall(Val, narrative_ontology:constraint_metric(C, MetricType, Val), Vals),
    (   Vals \= []
    ->  sum_list(Vals, Sum),
        length(Vals, N),
        Average is Sum / N
    ;   Average = 0.5  % Default if no data
    ).

%% count_power_beneficiaries(+Constraint, -Count)
%  Counts distinct classes with positive power changes
count_power_beneficiaries(C, Count) :-
    % Find intervals affecting this constraint
    findall(Class, (
        narrative_ontology:affects_constraint(I, C),
        narrative_ontology:intent_power_change(I, Class, Delta),
        Delta > 0.1  % Meaningful gain threshold
    ), Beneficiaries),
    sort(Beneficiaries, UniqueBeneficiaries),
    length(UniqueBeneficiaries, Count).

%% has_viable_alternatives(+Constraint, -HasAlternatives)
%  Checks if viable alternatives were considered (indicates choice vs necessity)
has_viable_alternatives(C, true) :-
    narrative_ontology:affects_constraint(I, C),
    narrative_ontology:intent_viable_alternative(I, _, _), !.
has_viable_alternatives(_, false).

%% compute_temporal_stability(+Constraint, -Stability)
%  Measures whether constraint metrics remain stable over time
%  Returns: stable | evolving
compute_temporal_stability(C, Stability) :-
    % Get suppression values at different time points for this constraint
    findall(Val, 
            narrative_ontology:constraint_metric(C, suppression_requirement, Val),
            Vals),
    (   Vals = []
    ->  Stability = unknown
    ;   Vals = [_SingleVal]
    ->  Stability = stable  % Only one measurement
    ;   compute_variance(Vals, Variance),
        (   Variance < 0.05
        ->  Stability = stable
        ;   Stability = evolving
        )
    ).

compute_variance(Vals, Variance) :-
    length(Vals, N),
    N > 0,
    sum_list(Vals, Sum),
    Mean is Sum / N,
    findall(SqDiff, (member(V, Vals), SqDiff is (V - Mean) * (V - Mean)), SqDiffs),
    sum_list(SqDiffs, SumSqDiffs),
    Variance is SumSqDiffs / N.

/* ================================================================
   SIGNATURE CLASSIFICATION LOGIC
   
   Decision Tree:
   
   1. Check Natural Law signature FIRST (most specific)
      - Extreme collapse + minimal enforcement + no alternatives
      - Examples: Chaitin's Ω, Heisenberg, Arrow's Theorem
      
   2. Check Coordination Scaffold SECOND
      - Extreme collapse + minimal enforcement + HAS alternatives
      - Examples: Special Relativity, SI Units, ISO Standards
      
   3. Check Constructed Constraint LAST (most general)
      - Positive enforcement OR beneficiary asymmetries
      - Examples: 26 USC §469, GS1 Barcodes, Hammurabi's Code
      
   4. Otherwise: ambiguous
   ================================================================ */

classify_by_signature(Profile, natural_law) :-
    natural_law_signature(Profile), !.

classify_by_signature(Profile, coordination_scaffold) :-
    coordination_scaffold_signature(Profile), !.

classify_by_signature(Profile, piton_signature) :-
    piton_signature(Profile), !.

classify_by_signature(Profile, constructed_constraint) :-
    constructed_constraint_signature(Profile), !.

classify_by_signature(_, ambiguous).

/* ================================================================
   SIGNATURE 1: NATURAL LAW
   
   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero beneficiaries (no asymmetric winners)
   ✓ No viable alternatives (not a choice)
   ✓ Temporally stable (doesn't evolve)
   
   Interpretation:
   This is an INHERENT PROPERTY of the system, not a human choice.
   It cannot be changed by coordination or enforcement.
   
   Examples:
   - Chaitin's Omega (algorithmic information limit)
   - Heisenberg Uncertainty (quantum measurement limit)
   - Arrow's Impossibility (social choice limit)
   - Second Law of Thermodynamics (entropy increase)
   ================================================================ */

natural_law_signature(profile(AccessCollapse, Suppression, Resistance,
                             BeneficiaryCount, HasAlternatives,
                             TemporalStability, _CoordinationSuccess)) :-
    
    % Metric conditions
    config:param(natural_law_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85
    
    config:param(natural_law_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15
    
    config:param(natural_law_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15
    
    % Structural conditions (CRITICAL for distinguishing from coordination)
    BeneficiaryCount == 0,  % No asymmetric winners
    HasAlternatives == false,  % Not a choice
    TemporalStability == stable.  % Doesn't evolve

/* ================================================================
   SIGNATURE 2: COORDINATION SCAFFOLD
   
   Diagnostic Pattern:
   ✓ Extreme accessibility collapse (≥ 0.85)
   ✓ Minimal suppression (≤ 0.15)
   ✓ Minimal resistance (≤ 0.15)
   ✓ Zero or few beneficiaries (symmetric benefits)
   ✓ HAS viable alternatives (WAS a choice)
   ✓ May be stable or evolving (can be refined)
   
   KEY DISTINCTION from Natural Law:
   - Alternatives EXISTED → this was a COORDINATION CHOICE
   - Success is CONTINGENT → depends on continued acceptance
   - Could theoretically be replaced → not inherent to reality
   
   Interpretation:
   This is a SUCCESSFUL VOLUNTARY STANDARD that everyone adopted
   because it provides symmetric coordination benefits.
   
   Examples:
   - Special Relativity (replaced Newtonian mechanics)
   - SI Metric System (replaced imperial units)
   - UTC Time Standard (replaced local solar time)
   - IP Protocol (replaced other network protocols)
   ================================================================ */

coordination_scaffold_signature(profile(AccessCollapse, Suppression, Resistance,
                                       BeneficiaryCount, HasAlternatives,
                                       _TemporalStability, _CoordinationSuccess)) :-
    
    % Metric conditions (same as natural law)
    config:param(coordination_collapse_min, CollapseMin),
    AccessCollapse >= CollapseMin,  % Default: 0.85
    
    config:param(coordination_suppression_max, SuppMax),
    Suppression =< SuppMax,  % Default: 0.15
    
    config:param(coordination_resistance_max, ResMax),
    Resistance =< ResMax,  % Default: 0.15
    
    % Structural conditions (DIFFERENT from natural law)
    BeneficiaryCount =< 1,  % Symmetric or near-symmetric
    HasAlternatives == true.  % KEY: This WAS a choice

/* ================================================================
   SIGNATURE 3: PITON
   
   Diagnostic Pattern:
   ✓ Variable accessibility collapse
   ✓ Low suppression (≤ 0.2) - persists through inertia, not force
   ✓ Positive resistance (> 0.2) - it's now causing friction
   ✓ Had viable alternatives - it was originally a choice
   ✓ Temporally evolving - it got worse over time
   
   Interpretation:
   This was once a useful coordination scaffold (a Rope), but has since
   ossified and now creates more problems than it solves. It persists
   due to high switching costs and institutional inertia. It is a Piton
   stuck in the mountain.
   
   Examples:
   - QWERTY Keyboard Layout
   - Legacy software monoliths
   ================================================================ */

piton_signature(profile(_AccessCollapse, Suppression, Resistance,
                                   _BeneficiaryCount, HasAlternatives,
                                   TemporalStability, _CoordinationSuccess)) :-
    Suppression =< 0.2,       % Low active enforcement
    Resistance > 0.2,         % But people are pushing back
    HasAlternatives == true,  % It was a choice
    TemporalStability == evolving. % It has decayed or gotten worse

/* ================================================================
   SIGNATURE 4: CONSTRUCTED CONSTRAINT
   
   Diagnostic Pattern:
   ✓ Variable accessibility collapse
   ✓ Positive suppression (> 0.2) OR
   ✓ Positive resistance (> 0.2) OR
   ✓ Multiple beneficiaries (asymmetric gains)
   
   Interpretation:
   This is an INSTITUTIONALLY ENFORCED RULE that requires
   active maintenance and produces asymmetric outcomes.
   
   Examples:
   - 26 USC §469 (passive loss limitation)
   - GS1 Barcode System (licensing monopoly)
   - Hammurabi's Code (benefice system)
   - Lehman's Repo 105 (accounting fiction)
   ================================================================ */

constructed_constraint_signature(profile(_AccessCollapse, Suppression, Resistance,
                                        BeneficiaryCount, _HasAlternatives,
                                        _TemporalStability, _CoordinationSuccess)) :-
    
    % At least one indicator of constructed constraint
    (   Suppression > 0.2        % Requires enforcement
    ;   Resistance > 0.2         % Faces opposition  
    ;   BeneficiaryCount > 1     % Asymmetric benefits
    ).

/* ================================================================
   CONFIDENCE SCORING
   
   Returns confidence level based on how strongly the signature
   pattern matches the classification.
   ================================================================ */

%% signature_confidence(+ConstraintID, +Signature, -Confidence)
%  Returns: high | medium | low
signature_confidence(C, Signature, Confidence) :-
    get_constraint_profile(C, Profile),
    compute_signature_confidence(Profile, Signature, Confidence).

compute_signature_confidence(Profile, natural_law, Confidence) :-
    Profile = profile(AccessCollapse, Suppression, Resistance, _, _, _, _),
    
    % Count strong indicators
    findall(1, (
        (AccessCollapse > 0.95);
        (Suppression < 0.05);
        (Resistance < 0.05)
    ), Indicators),
    length(Indicators, Count),
    
    (   Count >= 3 -> Confidence = high
    ;   Count >= 2 -> Confidence = medium
    ;   Confidence = low
    ).

compute_signature_confidence(Profile, coordination_scaffold, Confidence) :-

    Profile = profile(AccessCollapse, Suppression, _, _, HasAlternatives, _, _),

    

    % Strong indicators

    findall(1, (

        (AccessCollapse > 0.95);

        (Suppression < 0.05);

        (HasAlternatives == true)  % Critical for coordination

    ), Indicators),

    length(Indicators, Count),

    

    (   Count >= 3 -> Confidence = high

    ;   Count >= 2 -> Confidence = medium

    ;   Confidence = low

    ).



compute_signature_confidence(Profile, piton_signature, Confidence) :-

    Profile = profile(_, Suppression, Resistance, _, HasAlternatives, TemporalStability, _),

    

    % Count strong indicators for a piton

    findall(1, (

        (Suppression =< 0.2);

        (Resistance > 0.5);

        (HasAlternatives == true);

        (TemporalStability == evolving)

    ), Indicators),

    length(Indicators, Count),

    

    (   Count >= 3 -> Confidence = high

    ;   Count >= 2 -> Confidence = medium

    ;   Confidence = low

    ).



compute_signature_confidence(Profile, constructed_constraint, Confidence) :-

    Profile = profile(_, Suppression, Resistance, BeneficiaryCount, _, _, _),

    

    % Count enforcement indicators

    findall(1, (

        (Suppression > 0.5);

        (Resistance > 0.5);

        (BeneficiaryCount > 2)

    ), Indicators),

    length(Indicators, Count),

    

    (   Count >= 2 -> Confidence = high

    ;   Count >= 1 -> Confidence = medium

    ;   Confidence = low

    ).



compute_signature_confidence(_, ambiguous, low).

/* ================================================================
   EXPLANATION GENERATION
   ================================================================ */

%% explain_signature(+ConstraintID, +Signature, -Explanation)
explain_signature(C, natural_law, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, R, _, _, _, _),
    format(atom(Explanation),
           'NATURAL LAW signature for ~w: Extreme inaccessibility (collapse=~2f) with minimal enforcement (suppression=~2f, resistance=~2f). No viable alternatives exist. This represents an inherent property of the system, not a coordination choice. Cannot be changed by policy.',
           [C, AC, S, R]).

explain_signature(C, coordination_scaffold, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(AC, S, _, _, _, _, _),
    format(atom(Explanation),
           'COORDINATION SCAFFOLD signature for ~w: Extreme accessibility (collapse=~2f) with minimal enforcement (suppression=~2f). Viable alternatives existed historically, indicating this is a successful coordination standard rather than a natural law. Maintains adoption through symmetric benefits.',
           [C, AC, S]).

explain_signature(C, piton_signature, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, _, _, _, _),
    format(atom(Explanation),
           'PITON signature for ~w: Persists through inertia (suppression=~2f) but faces user friction (resistance=~2f). Was once a choice, but has now become an ossified liability. This is a Piton.',
           [C, S, R]).

explain_signature(C, constructed_constraint, Explanation) :-
    get_constraint_profile(C, Profile),
    Profile = profile(_, S, R, BC, _, _, _),
    format(atom(Explanation),
           'CONSTRUCTED CONSTRAINT signature for ~w: Active enforcement detected (suppression=~2f, resistance=~2f) with ~d asymmetric beneficiaries. This is an institutionally maintained rule requiring ongoing suppression energy.',
           [C, S, R, BC]).

explain_signature(C, ambiguous, Explanation) :-
    format(atom(Explanation),
           'AMBIGUOUS signature for ~w: Insufficient structural differentiation to classify. Consider gathering more data on alternatives, beneficiaries, and temporal evolution.',
           [C]).

/* ================================================================
   INTEGRATION WITH MODAL CLASSIFICATION
   
   This is the key integration point: structural signatures
   OVERRIDE modal classification when there's a mismatch.
   
   Example: Special Relativity
   - Modal classifier says: "mountain" (suppression=0, snapback=0)
   - Signature detector says: "coordination_scaffold"
   - Integrated result: "rope" with note about coordination success
   ================================================================ */

%% integrate_signature_with_modal(+Constraint, +ModalType, -AdjustedType)
%  Adjusts modal classification based on structural signature
integrate_signature_with_modal(C, ModalType, AdjustedType) :-
    constraint_signature(C, Signature),
    resolve_modal_signature_conflict(ModalType, Signature, AdjustedType).

% Natural laws stay as mountains (correct classification)
resolve_modal_signature_conflict(mountain, natural_law, mountain) :- !.

% Coordination scaffolds should be ROPES not mountains
resolve_modal_signature_conflict(mountain, coordination_scaffold, rope) :- !.

% Constructed constraints override mountain classification
resolve_modal_signature_conflict(mountain, constructed_constraint, tangled_rope) :- !.

% No conflict - keep original classification
resolve_modal_signature_conflict(ModalType, _, ModalType).
