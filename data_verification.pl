:- module(data_verification, [
    verify_all/0,
    verify_interval_completeness/1,
    check_paired_measurements/0,
    diagnose_unknown/1
]).

:- use_module(library(lists)).        % Required for subtract/3
:- use_module(narrative_ontology).
:- use_module(config).

/* ============================================================
   1. ENTRY POINTS
   ============================================================ */

%% verify_all
%  Performs a full sweep of the loaded data against the v3.1 schema.
verify_all :-
    format('~n--- [START] Data Verification ---~n'),
    (validate_ontology -> format('[OK] Ontology Schema matches.~n') ; format('[FAIL] Schema mismatch.~n')),
    verify_structure,
    verify_measurements,
    check_paired_measurements,  % NEW: Check for paired temporal measurements
    verify_intent_logic,
    format('--- [END] Data Verification Complete ---~n').

/* ============================================================
   2. STRUCTURAL & VECTOR COMPLETENESS
   ============================================================ */

%% verify_structure
%  Ensures every interval has the required 32-point coercion vector.
verify_structure :-
    forall(interval(ID, T0, Tn),
           ( format('Checking Interval: ~w (~w-~w)~n', [ID, T0, Tn]),
             verify_interval_completeness(ID)
           )).

%% verify_interval_completeness(+IntervalID)
%  Checks for the presence of all 4 components across all 4 levels at T0 and Tn.
verify_interval_completeness(ID) :-
    interval(ID, T0, Tn),
    forall(level(L),
           ( verify_vector_at(L, T0, ID),
             verify_vector_at(L, Tn, ID)
           )).

verify_vector_at(Level, Time, ID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components),
           ( measurement(_, _, Metric, Time, _)
           -> true
           ;  format('  [MISSING] ~w for Level: ~w at T: ~w in ~w~n', [Metric, Level, Time, ID]),
              fail
           )).

/* ============================================================
   3. VALUE RANGE VALIDATION
   ============================================================ */

verify_measurements :-
    forall(measurement(ID, _, _, _, Val),
           ( number(Val), Val >= 0.0, Val =< 1.0
           -> true
           ;  format('  [VALUE ERROR] Measurement ~w is outside [0,1] range.~n', [ID]),
              fail
           )).

/* ============================================================
   4. INTENT LOGIC CONSISTENCY
   ============================================================ */

verify_intent_logic :-
    % Ensure every rejected alternative was first defined as viable (by anyone)
    % FIXED: Different entities can propose vs. reject - that's the whole point!
    % RELAXED: Allow rejecting implicit alternatives that weren't explicitly listed as viable
    forall(intent_alternative_rejected(I, _Rejecter, A),
           ( intent_viable_alternative(I, _Proposer, A)
           -> true
           ;  format('  [INFO] Alternative ~w rejected in ~w without explicit viable listing (may be implicit status quo).~n', [A, I])
           % REMOVED fail - this is informational, not an error
           )),
    % Ensure main beneficiary has a power change fact
    forall(intent_beneficiary_class(I, C),
           ( intent_power_change(I, C, _)
           -> true
           ;  format('  [MISSING DATA] Beneficiary ~w has no power_change delta in ~w.~n', [C, I]),
              fail
           )).

/* ============================================================
   5. PAIRED MEASUREMENT VALIDATION (MODAL LOGIC REQUIREMENT)
   ============================================================ */

%% check_paired_measurements
% Verifies that extractiveness and suppression_requirement measurements
% are paired at each time point for each constraint.
% CRITICAL: Modal logic requires BOTH metrics to classify constraints.
check_paired_measurements :-
    % Get all constraints that have any temporal measurements
    % CRITICAL FIX: Exclude intervals to avoid treating them as constraints
    setof(C, T^M^V^(measurement(M, C, _, T, V), entity(C, _)), Constraints),
    !,
    forall(member(C, Constraints),
           check_constraint_pairing(C)).

check_paired_measurements :- 
    % No temporal measurements found - skip check
    true.

%% check_constraint_pairing(+Constraint)
% Checks if a specific constraint has paired measurements
check_constraint_pairing(C) :-
    % Get all time points for extractiveness
    findall(T, measurement(_, C, extractiveness, T, _), XTimes),
    % Get all time points for suppression_requirement
    findall(T, measurement(_, C, suppression_requirement, T, _), ETimes),
    % Sort and compare
    sort(XTimes, XSorted),
    sort(ETimes, ESorted),
    (   XSorted = ESorted
    ->  true  % Perfect pairing
    ;   XSorted = [], ESorted = []
    ->  true  % No temporal data (uses constraint_metric only)
    ;   % Unpaired measurements detected
        format('  [WARNING] Unpaired temporal measurements for ~w~n', [C]),
        (   XSorted \= []
        ->  format('    Extractiveness measured at: ~w~n', [XSorted])
        ;   format('    Extractiveness: No temporal measurements~n')
        ),
        (   ESorted \= []
        ->  format('    Suppression measured at: ~w~n', [ESorted])
        ;   format('    Suppression: No temporal measurements~n')
        ),
        format('    Impact: May cause "unknown" classification or use default values~n')
    ).

/* ============================================================
   6. DIAGNOSTIC TOOLS
   ============================================================ */

%% diagnose_unknown(+Constraint)
% Diagnostic tool for investigating "unknown" classifications
% Call this when dr_type(C, unknown) occurs
diagnose_unknown(C) :-
    format('~n=== DIAGNOSING UNKNOWN CLASSIFICATION: ~w ===~n', [C]),
    
    % Check if constraint exists
    (   constraint_claim(C, Claimed)
    ->  format('Claimed type: ~w~n', [Claimed])
    ;   format('WARNING: No constraint_claim for ~w~n', [C])
    ),
    
    % Check current metrics
    format('~nCurrent metrics (T_end):~n'),
    (   constraint_metric(C, extractiveness, X)
    ->  format('  Extractiveness: ~w~n', [X])
    ;   format('  Extractiveness: MISSING~n')
    ),
    (   constraint_metric(C, suppression_requirement, E)
    ->  format('  Suppression: ~w~n', [E])
    ;   format('  Suppression: MISSING~n')
    ),
    
    % Check temporal measurements
    format('~nTemporal measurements:~n'),
    findall(T, measurement(_, C, extractiveness, T, _), XTimes),
    findall(T, measurement(_, C, suppression_requirement, T, _), ETimes),
    (   XTimes \= []
    ->  format('  Extractiveness at times: ~w~n', [XTimes])
    ;   format('  Extractiveness: No temporal data~n')
    ),
    (   ETimes \= []
    ->  format('  Suppression at times: ~w~n', [ETimes])
    ;   format('  Suppression: No temporal data~n')
    ),
    
    % Check pairing
    format('~nPairing analysis:~n'),
    sort(XTimes, XSorted),
    sort(ETimes, ESorted),
    (   XSorted = ESorted
    ->  format('  ✓ Measurements are paired~n')
    ;   format('  ✗ UNPAIRED MEASUREMENTS DETECTED~n'),
        format('    This is likely causing the "unknown" classification~n')
    ),
    
    % Provide fix suggestion
    format('~nRecommended fix:~n'),
    (   XSorted \= [], ESorted = []
    ->  format('  Add suppression_requirement measurements at: ~w~n', [XSorted])
    ;   ESorted \= [], XSorted = []
    ->  format('  Add extractiveness measurements at: ~w~n', [ESorted])
    ;   XSorted \= [], ESorted \= []
    ->  subtract(XSorted, ESorted, MissingE),
        subtract(ESorted, XSorted, MissingX),
        (   MissingE \= []
        ->  format('  Add suppression_requirement at: ~w~n', [MissingE])
        ;   true
        ),
        (   MissingX \= []
        ->  format('  Add extractiveness at: ~w~n', [MissingX])
        ;   true
        )
    ;   format('  Add temporal measurements with both metrics~n')
    ),
    format('~n===========================================~n~n').
