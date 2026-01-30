:- module(report_generator, [
    generate_full_report/1,
    generate_indexed_report/3,
    generate_omegas_from_gaps/1,
    omega_from_gap/5,
    cross_domain_audit/0,
    forensic_audit_false_mountains/0,
    generate_omega_resolution_scenarios/0,
    generate_omega_triage/0
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(intent_engine, except([classify_interval/3])).
:- use_module(coercion_projection).
:- use_module(pattern_analysis).
:- use_module(constraint_bridge).
:- use_module(drl_core).
:- use_module(uke_dr_bridge).
:- use_module(structural_signatures).
:- use_module(constraint_indexing).
:- use_module(isomorphism_engine). % Required for isomorphism audit
:- use_module(domain_priors).      % Required for forensic audit
:- use_module(utils).              % Safe helpers for defensive programming
:- use_module(drl_lifecycle).      % Drift event detection & lifecycle analysis

% Suppress warning - we intentionally override intent_engine:classify_interval/3
:- discontiguous classify_interval/3.

/* ============================================================================
   TYPE DESCRIPTIONS & STRATEGIES (Updated January 2026 for Tangled Rope)
   ============================================================================ */

%% type_description(?Type, ?Description)
%  Human-readable descriptions for constraint types.
type_description(mountain,
    'Natural constraint - unchangeable given current understanding of reality').
type_description(rope,
    'Pure coordination - low extraction, solves collective action problems').
type_description(tangled_rope,
    'Hybrid coordination/extraction - provides genuine coordination while extracting asymmetrically').
type_description(snare,
    'Pure extraction - minimal coordination benefit, high asymmetric extraction').
type_description(piton,
    'Maintained constraint - low extraction but high suppression costs, should be cut but isn''t').

%% type_strategy(?Type, ?Strategy)
%  Strategic recommendations for each constraint type.
type_strategy(mountain,
    'Accept - Work within natural constraints, adapt strategies accordingly').
type_strategy(rope,
    'Maintain - Preserve coordination mechanisms, ensure fair access and participation').
type_strategy(tangled_rope,
    'Reform carefully - Preserve coordination core while cutting extractive elements. Requires surgical separation.').
type_strategy(snare,
    'Cut - Remove extractive constraints, replace with fair alternatives if coordination needed').
type_strategy(piton,
    'Bypass or eliminate - High maintenance cost without value, find alternatives').

%% type_color(?Type, ?Color)
%  Color coding for visualization and reports.
type_color(mountain, blue).
type_color(rope, green).
type_color(tangled_rope, orange).  % Orange for hybrid nature
type_color(snare, red).
type_color(piton, gray).

/* ============================================================================
   1. EXECUTIVE SUMMARY (MAIN ENTRY)
   ============================================================================ */

generate_full_report(IntervalID) :-
    narrative_ontology:interval(IntervalID, T_start, Tn),
    classify_interval(IntervalID, Pattern, Conf),
    
    format('~n~n====================================================~n'),
    format('   DEFERENTIAL REALISM (DR) EXECUTIVE SUMMARY      ~n'),
    format('====================================================~n'),
    format('Timeline:       ~w to ~w~n', [T_start, Tn]),
    format('Structural Pattern: ~w~n', [Pattern]),
    format('Confidence:     ~w~n', [Conf]),
    
    % --- SECTION 1: DRL INDEXICAL AUDIT ---
    format('~n[CONSTRAINT INVENTORY: INDEXICAL AUDIT]~n'),
    forall(
        narrative_ontology:constraint_claim(C, Claimed),
        (
            format('~n~nConstraint: ~w~n', [C]),
            format('  Claimed Type: ~w~n', [Claimed]),
            format('  Perspectives:~n'),
            forall(
                constraint_indexing:constraint_classification(C, Type, Context),
                (
                    format('    - [~w]: ~w', [Context, Type]),
                    ( Type == Claimed -> format(' (Matches Claim)~n')
                    ; format(' (Mismatch)~n')
                    )
                )
            )
        )
    ),

    % --- SECTION 2: ISOMORPHISM AUDIT ---
    generate_isomorphism_audit(IntervalID),

    % --- SECTION 2A: COMPREHENSIVE CROSS-DOMAIN AUDIT ---
    cross_domain_audit,

    % --- SECTION 3: META-LOGICAL AUDIT ---
    format('~n[META-LOGICAL AUDIT: ONTOLOGICAL FRAUD DETECTION]~n'),
    (   setof((C, Err, Sev), drl_core:dr_mismatch(C, Err, Sev), Errors)
    ->  forall(member((C, Err, Sev), Errors),
               format('  ! ALERT [~w]: ~w detected for ~w~n', [Sev, Err, C]))
    ;   format('  No classification errors detected. System is Ontologically Coherent.~n')
    ),

    % --- SECTION 3A: FORENSIC AUDIT FOR FALSE MOUNTAINS ---
    forensic_audit_false_mountains,

    % --- SECTION 4: STRUCTURAL SIGNATURE ANALYSIS ---
    format('~n[STRUCTURAL SIGNATURE ANALYSIS]~n'),
    (   catch(
            forall(narrative_ontology:constraint_claim(CSig, _Claim),
                   report_constraint_signature(CSig)),
            Error,
            format('  [FAIL] Exception: ~w~n', [Error]))
    ;   true
    ),

    % --- SECTION 5: UKE_DR FEASIBILITY BRIDGE ---
    format('~n[UKE_DR FEASIBILITY BRIDGE]~n'),
    format('  ~40s | ~12s~n', ['Recommendation', 'UKE Status']),
    format('  ----------------------------------------------------------------------~n'),
    (   forall(narrative_ontology:recommendation(RID, Summary),
               ( ( uke_dr_bridge:uke_status(RID, UKEStatus, Reasons) 
                 -> format('  - ~40w | ~12w~n', [Summary, UKEStatus]),
                    forall(member(R, Reasons), format('    > ~w~n', [R]))
                 ;  format('  - ~40w | ~12s~n', [Summary, 'DATA_MISSING'])
                 )
               ))
    ;   true
    ),
    
    % --- SECTION 6: KINETIC MAGNITUDE ---
    findall(Kappa, (config:level(L), coercion_projection:coercion_magnitude(L, Tn, Kappa)), Kappas),
    (   Kappas \= [] 
    ->  sum_list(Kappas, SumK), length(Kappas, NK), AvgK is SumK / NK,
        format('~nAggregate Magnitude (Kappa) at Tn: ~2f~n', [AvgK])
    ;   format('~nAggregate Magnitude (Kappa): DATA_INSUFFICIENT~n')
    ),
    
    % --- SECTION 7: PERSPECTIVAL GAP ANALYSIS ---
    format('~n[PERSPECTIVAL GAP ANALYSIS]~n'),
    (   forall(narrative_ontology:constraint_claim(CGap, _),
               perspectival_gap_audit(CGap))
    ;   true
    ),
    
    % --- SECTION 8: OMEGA GENERATION ---
    generate_omegas_from_gaps(IntervalID),

    % --- SECTION 8A: OMEGA TRIAGE ---
    generate_omega_triage,

    % --- SECTION 8B: OMEGA RESOLUTION SCENARIOS ---
    generate_omega_resolution_scenarios,

    format('====================================================~n').

/* ============================================================================
   2. OMEGA GENERATION
   ============================================================================ */

generate_omegas_from_gaps(IntervalID) :-
    format('~n[OMEGA GENERATION FROM PERSPECTIVAL GAPS: ~w]~n', [IntervalID]),
    findall(
        omega_entry(OmegaID, Type, Question, Gap),
        (   narrative_ontology:constraint_claim(C, _),  % Look at ALL claimed constraints
            detect_gap_pattern(C, Gap),
            omega_from_gap(C, Gap, OmegaID, Type, Question)
        ),
        OmegaEntries
    ),
    process_omega_entries(OmegaEntries).

process_omega_entries([]) :- 
    format('  No perspectival gaps detected requiring Ω tracking.~n').
process_omega_entries(OmegaEntries) :-
    OmegaEntries \= [],
    length(OmegaEntries, Count),
    format('  Generated ~w Omega variables from perspectival gaps:~n~n', [Count]),
    forall(member(omega_entry(OID, OType, OQuestion, OGap), OmegaEntries),
           (   format('  Ω: ~w (~w)~n', [OID, OType]),
               format('     Question: ~w~n', [OQuestion]),
               format('     Source: ~w~n~n', [OGap]),
               assert_omega_if_new(OID, OType, OQuestion)
           )).

% detect_gap_pattern and omega_from_gap logic remains unchanged...
% [Included below for completeness in your file]

% Pattern 1: Snare masked as Rope - MOST CRITICAL (extraction blindness)
detect_gap_pattern(C, gap(snare_masked_as_rope, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = snare, 
    TypeI = rope, 
    !.

% Pattern 2: Snare/Mountain confusion - CRITICAL (learned helplessness)
detect_gap_pattern(C, gap(snare_mountain_confusion, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = snare, 
    TypeI = mountain, 
    !.

% Pattern 3: Mountain/Rope confusion - REQUIRES SCAFFOLD (catastrophic cut risk)
detect_gap_pattern(C, gap(mountain_coordination_confusion, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP = mountain, 
    TypeI = rope, 
    !.

% Pattern 4: General catch-all - MUST BE LAST (any other mismatch)
detect_gap_pattern(C, gap(general_type_mismatch, TypeP, TypeI)) :-
    constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
    TypeP \= TypeI, 
    TypeP \= none, 
    TypeI \= none.

omega_from_gap(C, gap(snare_masked_as_rope, snare, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_extraction_blindness_~w', [C]),
    format(atom(Question), 'Constraint ~w appears extractive (Snare) to individuals but functional (Rope) to institutions...', [C]), !.

omega_from_gap(C, gap(mountain_coordination_confusion, mountain, rope), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_cut_safety_~w', [C]),
    format(atom(Question), 'Constraint ~w appears unchangeable (Mountain) to individuals but optional (Rope) to institutions...', [C]), !.

omega_from_gap(C, gap(snare_mountain_confusion, snare, mountain), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_learned_helplessness_~w', [C]),
    format(atom(Question), 'Constraint ~w appears extractive (Snare) to individuals but unchangeable (Mountain) to institutions...', [C]), !.

omega_from_gap(C, gap(general_type_mismatch, TypeP, TypeI), OmegaID, conceptual, Question) :-
    format(atom(OmegaID), 'omega_perspectival_~w', [C]),
    format(atom(Question), 'Constraint ~w appears as ~w to individuals but ~w to institutions...', [C, TypeP, TypeI]), !.

assert_omega_if_new(OmegaID, Type, Question) :-
    (   narrative_ontology:omega_variable(OmegaID, _, _)
    ->  true
    ;   assertz(narrative_ontology:omega_variable(OmegaID, Type, Question))
    ).

/* ============================================================================
   3. INDEXED REPORTING & AUDITS
   ============================================================================ */

perspectival_gap_audit(C) :-
    format('~n  Analysis for Constraint: ~w~n', [C]),
    (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)) -> true ; TypeP = none),
    (constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)) -> true ; TypeI = none),
    (TypeP == mountain, TypeI == rope -> format('    ! GAP: Institutional "Rope" appears as "Mountain" to Powerless.~n') ; true),
    (TypeP == snare, TypeI == rope -> format('    ! ALERT: Extractive "Snare" is masked as functional "Rope".~n') ; true),
    format('    - Individual (Powerless): ~w~n', [TypeP]),
    format('    - Institutional (Manager): ~w~n', [TypeI]).

report_constraint_signature(C) :-
    drl_core:dr_signature(C, Signature),
    structural_signatures:signature_confidence(C, Signature, Confidence),
    structural_signatures:explain_signature(C, Signature, Explanation),
    format('  ~20w: ~20w (confidence: ~w)~n', [C, Signature, Confidence]),
    (Signature \= ambiguous -> format('    → ~w~n', [Explanation]) ; true).

% Remaining placeholders (generate_indexed_report, extract_constraints, generate_llm_feedback, sublist, etc.) 
% should be placed here as top-level predicates...

generate_indexed_report(Text, Context, Report) :-
    extract_constraints(Text, Constraints),
    maplist(classify_with_context(Context), Constraints, Classifications),
    format_indexed_report(Classifications, Context, Report).

classify_with_context(Context, Constraint, classification(Constraint, Type)) :-
    constraint_indexing:constraint_classification(Constraint, Type, Context).

extract_constraints(Text, Constraints) :-
    atom_codes(Text, Codes),
    findall(C, (constraint_keyword(C), atom_codes(C, CCode), sublist(CCode, Codes)), Cs),
    sort(Cs, Constraints).

constraint_keyword(catholic_church_1200).
constraint_keyword(property_rights_2025).

format_indexed_report(Classifications, Context, Report) :-
    Context = context(agent_power(Power), time_horizon(Time), exit_options(Exit), spatial_scope(Scope)),
    with_output_to(atom(Report),
        (format('~n[INDEXED CONSTRAINT ANALYSIS]~n'),
         format('Perspective: ~w / ~w / ~w / ~w~n~n', [Power, Time, Exit, Scope]),
         format('Classifications:~n'),
         forall(member(classification(C, T), Classifications), format('  ~w: ~w~n', [C, T])))).

generate_llm_feedback(IntervalID) :-
    format('~n### START LLM REFINEMENT MANIFEST: ~w ###~n', [IntervalID]),
    format('~n[PERSPECTIVAL_GAPS]~n'),
    (forall(narrative_ontology:constraint_claim(C, _),
           (constraint_indexing:constraint_classification(C, TypeP, context(agent_power(individual_powerless), _, _, _)),
            constraint_indexing:constraint_classification(C, TypeI, context(agent_power(institutional), _, _, _)),
            TypeP \= TypeI, format('  - Constraint "~w": Individual sees ~w, but Institution sees ~w.~n', [C, TypeP, TypeI]))) ; true),
    format('~n[ONTOLOGICAL_MISMATCHES]~n'),
    (setof((CM, Err, Sev), drl_core:dr_mismatch(CM, Err, Sev), Errors) -> forall(member((CM, Err, Sev), Errors), format('  - ~w: [~w] ~w detected.~n', [CM, Sev, Err])) ; format('  - None detected.~n')),
    format('~n[UNRESOLVED_OMEGAS]~n'),
    (setof((OID, OTy, ODe), narrative_ontology:omega_variable(OID, OTy, ODe), Omegas) -> forall(member((OID, OTy, ODe), Omegas), format('  - ~w (~w): ~w~n', [OID, OTy, ODe])) ; format('  - None detected.~n')),
    format('~n### END REFINEMENT MANIFEST ###~n').

sublist([], _).
sublist([H|T], [H|T2]) :- !, sublist(T, T2).
sublist(Sub, [_|T]) :- sublist(Sub, T).

classify_interval(_IntervalID, stable, high).

% Ensure these are at the BOTTOM of report_generator.pl, NOT inside generate_full_report
generate_isomorphism_audit(IntervalID) :-
    format('~n[CROSS-DOMAIN ISOMORPHISM & RISK AUDIT: ~w]~n', [IntervalID]),
    (   setof(iso(C, Twin, Score, Type),
              (narrative_ontology:constraint_claim(C, _),  % Changed: use all constraints in current KB
               isomorphism_engine:find_high_risk_isomorphism(C, Twin, Score),
               drl_core:dr_type(C, Type)),
              Isos)
    ->  display_isomorphisms(Isos)
    ;   format('  No high-risk isomorphisms detected for current constraints.~n')
    ).

display_isomorphisms([]).
display_isomorphisms([iso(C, T, S, Ty)|Rest]) :-
    format('  ! ALERT: ~w (~w) is a Structural Twin to ~w (Score: ~2f)~n', [C, Ty, T, S]),
    format('    > Strategy: Search for ~w resolutions in KB.~n', [T]),
    display_isomorphisms(Rest).

/* ============================================================================
   4. ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS
   ============================================================================ */

%% forensic_audit_false_mountains/0
%  Provides detailed analysis of constraints claiming "Mountain" status
%  but failing validation. Explains WHY each fails and recommends reclassification.
forensic_audit_false_mountains :-
    format('~n[ONTOLOGICAL FORENSIC AUDIT: FALSE MOUNTAINS]~n'),
    (   setof(C-Ctx, Sev^(drl_core:dr_mismatch(C, Ctx, type_1_false_mountain, Sev)),
              FalseMountains)
    ->  (length(FalseMountains, Count),
         format('  Detected ~w constraint(s) falsely claiming "Mountain" status:~n~n', [Count]),
         forall(member(C-Context, FalseMountains),
                forensic_explain_false_mountain(C, Context)))
    ;   format('  All mountains are structurally validated.~n')
    ).

%% forensic_explain_false_mountain(+Constraint, +Context)
%  Explains in detail why a constraint fails mountain validation.
%  Now using safe helpers for defensive programming.
forensic_explain_false_mountain(C, Context) :-
    format('  ┌─ CONSTRAINT: ~w~n', [C]),
    format('  │  Context: ~w~n', [Context]),
    format('  │~n', []),

    % Get metrics using safe helpers (with defaults and logging)
    utils:safe_get_metric(C, suppression_requirement, Supp, 0.0, false),
    (Supp = 0.0
    -> format('  │  Suppression Requirement: MISSING (using default 0.0)~n', [])
    ;  format('  │  Suppression Requirement: ~2f~n', [Supp])),

    % Get mountain ceiling safely
    utils:safe_get_config(mountain_suppression_ceiling, Ceil, 0.15),
    (Ceil = 0.15
    -> format('  │  Mountain Ceiling (threshold): ~2f (default)~n', [Ceil])
    ;  format('  │  Mountain Ceiling (threshold): ~2f~n', [Ceil])),

    % Analyze why it fails
    format('  │~n', []),
    format('  │  FAILURE ANALYSIS:~n', []),
    (Supp > Ceil
    -> format('  │  ✗ Requires active enforcement (suppression ~2f > ~2f threshold)~n', [Supp, Ceil])
    ;  format('  │  ? Suppression within range, checking other factors...~n', [])),

    % Get extractiveness safely
    utils:safe_get_extractiveness(C, Extr, 0.0, false),
    (Extr = 0.0
    -> format('  │  Base Extractiveness: MISSING (using default 0.0)~n', [])
    ;  (format('  │  Base Extractiveness: ~2f~n', [Extr]),
        (Extr > 0.35
        -> format('  │  ✗ Shows extractive pattern (E > 0.35)~n', [])
        ;  format('  │  ✓ Non-extractive pattern (E ≤ 0.35)~n', [])))),

    % Check resistance to change safely
    utils:safe_get_metric(C, resistance_to_change, Resist, 0.0, false),
    (Resist = 0.0
    -> format('  │  Resistance to Change: MISSING (using default 0.0)~n', [])
    ;  format('  │  Resistance to Change: ~2f~n', [Resist])),

    % Provide forensic verdict
    format('  │~n', []),
    format('  │  FORENSIC VERDICT:~n', []),
    determine_correct_classification(Supp, Extr, Ceil, Verdict, Rationale),
    format('  │  → Should be classified as: ~w~n', [Verdict]),
    format('  │  → Rationale: ~w~n', [Rationale]),

    % Check for mandatrophy (mountain that functions as trap)
    (Extr > 0.7
    -> format('  │  ⚠ WARNING: Possible MANDATROPHY detected~n', []),
       format('  │    (Genuinely unchangeable BUT highly extractive)~n', [])
    ;  true),

    format('  └─~n~n', []).

%% determine_correct_classification(+Supp, +Extr, +Ceil, -Verdict, -Rationale)
%  Logic to determine what a false mountain should actually be classified as.
determine_correct_classification(Supp, Extr, Ceil, Verdict, Rationale) :-
    (Supp > Ceil, Extr > 0.66
    -> (Verdict = 'SNARE',
        Rationale = 'High enforcement + high extraction = extractive trap')
    ; Supp > Ceil, Extr > 0.35
    -> (Verdict = 'TANGLED_ROPE',
        Rationale = 'Moderate enforcement + moderate extraction = complex coordination with extractive elements')
    ; Supp > Ceil
    -> (Verdict = 'ROPE',
        Rationale = 'Requires enforcement but not extractive = changeable rule')
    ; Extr > 0.66
    -> (Verdict = 'SNARE (check metrics)',
        Rationale = 'High extraction despite low suppression suggests misclassification or measurement error')
    ;  (Verdict = 'AMBIGUOUS (review data)',
        Rationale = 'Metrics inconclusive, manual review required')
    ).

/* ============================================================================
   5. OMEGA RESOLUTION SCENARIOS
   ============================================================================ */

%% generate_omega_resolution_scenarios/0
%  Generates actionable test scenarios for resolving each unresolved omega.
%  This drives scenario creation by providing specific resolution strategies.
generate_omega_resolution_scenarios :-
    format('~n[OMEGA RESOLUTION SCENARIO GENERATION]~n'),
    findall(omega_data(OID, OType, ODesc, Constraint, GapPattern),
            (narrative_ontology:omega_variable(OID, OType, ODesc),
             extract_constraint_from_omega_id(OID, Constraint),
             determine_gap_pattern(OID, Constraint, GapPattern)),
            Omegas),
    (Omegas = []
    -> format('  No unresolved Omegas. System is epistemically complete.~n')
    ;  (length(Omegas, N),
        format('  Generated ~w resolution scenario(s):~n~n', [N]),
        forall(member(omega_data(OID, OType, ODesc, C, Gap), Omegas),
               generate_scenario_for_omega(OID, OType, ODesc, C, Gap)))
    ).

%% extract_constraint_from_omega_id(+OmegaID, -Constraint)
%  Extracts the constraint name from omega IDs like omega_extraction_blindness_CONSTRAINT
extract_constraint_from_omega_id(OmegaID, Constraint) :-
    atom_string(OmegaID, OIDStr),
    (sub_string(OIDStr, _, _, _, "omega_extraction_blindness_")
    -> split_string(OIDStr, "_", "", Parts),
       append(["omega", "extraction", "blindness"], ConstraintParts, Parts),
       atomic_list_concat(ConstraintParts, '_', Constraint)
    ; sub_string(OIDStr, _, _, _, "omega_learned_helplessness_")
    -> split_string(OIDStr, "_", "", Parts),
       append(["omega", "learned", "helplessness"], ConstraintParts, Parts),
       atomic_list_concat(ConstraintParts, '_', Constraint)
    ; sub_string(OIDStr, _, _, _, "omega_cut_safety_")
    -> split_string(OIDStr, "_", "", Parts),
       append(["omega", "cut", "safety"], ConstraintParts, Parts),
       atomic_list_concat(ConstraintParts, '_', Constraint)
    ; sub_string(OIDStr, _, _, _, "omega_perspectival_")
    -> split_string(OIDStr, "_", "", Parts),
       append(["omega", "perspectival"], ConstraintParts, Parts),
       atomic_list_concat(ConstraintParts, '_', Constraint)
    ;  Constraint = unknown
    ).

%% determine_gap_pattern(+OmegaID, +Constraint, -Pattern)
%  Determines which gap pattern caused this omega
determine_gap_pattern(OmegaID, Constraint, Pattern) :-
    atom_string(OmegaID, OIDStr),
    (sub_string(OIDStr, _, _, _, "extraction_blindness")
    -> (Pattern = snare_masked_as_rope)
    ; sub_string(OIDStr, _, _, _, "learned_helplessness")
    -> (Pattern = snare_mountain_confusion)
    ; sub_string(OIDStr, _, _, _, "cut_safety")
    -> (Pattern = mountain_coordination_confusion)
    ;  (Constraint \= unknown,
        detect_gap_pattern(Constraint, gap(Pattern, _, _))
       -> true
       ;  Pattern = general_type_mismatch)
    ).

%% generate_scenario_for_omega(+OmegaID, +Type, +Description, +Constraint, +GapPattern)
%  Generates a specific resolution scenario based on omega type and gap pattern.
generate_scenario_for_omega(OID, empirical, Desc, C, _Gap) :-
    format('  ┌─ [~w] EMPIRICAL DATA COLLECTION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Design measurement protocol for ~w~n', [C]),
    format('  │  2. Collect data from N=30+ real-world instances~n', []),
    format('  │  3. Calculate empirical metrics:~n', []),
    format('  │     - suppression_requirement (enforcement needed)~n', []),
    format('  │     - resistance_to_change (pushback level)~n', []),
    format('  │     - base_extractiveness (asymmetric benefit flow)~n', []),
    format('  │  4. Update constraint_metric/3 declarations with data~n', []),
    format('  │  5. Re-run classification to resolve perspectival gap~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, snare_masked_as_rope) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  CRITICAL: Extraction Masking Detected~n', []),
    format('  │  Powerless see: SNARE (extractive trap)~n', []),
    format('  │  Institutions see: ROPE (functional rule)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Interview affected individuals (N=10+):~n', []),
    format('  │     - Who benefits from ~w?~n', [C]),
    format('  │     - Can you change/exit this constraint?~n', []),
    format('  │     - What would happen if you tried?~n', []),
    format('  │  2. Interview institutional actors (N=10+):~n', []),
    format('  │     - What function does ~w serve?~n', [C]),
    format('  │     - Who would object to removing it?~n', []),
    format('  │     - What alternatives exist?~n', []),
    format('  │  3. Document benefit flows:~n', []),
    format('  │     - Track who gains vs. who loses from status quo~n', []),
    format('  │     - Measure asymmetric benefit distribution~n', []),
    format('  │  4. Decision tree:~n', []),
    format('  │     IF extraction confirmed → Reclassify as SNARE~n', []),
    format('  │     IF functional & fair → Reclassify as ROPE~n', []),
    format('  │     IF context-dependent → Add indexical resolution~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, snare_mountain_confusion) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  CRITICAL: Learned Helplessness Pattern~n', []),
    format('  │  Powerless see: SNARE (extractive trap)~n', []),
    format('  │  Institutions see: MOUNTAIN (unchangeable law)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Test changeability:~n', []),
    format('  │     - Can institutions modify ~w?~n', [C]),
    format('  │     - What legal/political mechanisms exist?~n', []),
    format('  │     - Historical precedents of change?~n', []),
    format('  │  2. Test extraction:~n', []),
    format('  │     - Is benefit flow symmetric or asymmetric?~n', []),
    format('  │     - Who has veto power over changes?~n', []),
    format('  │  3. Decision tree:~n', []),
    format('  │     IF truly unchangeable + extractive → MANDATROPHY~n', []),
    format('  │     IF changeable + extractive → Correct to SNARE~n', []),
    format('  │     IF unchangeable + fair → Correct to MOUNTAIN~n', []),
    format('  │     IF institutions falsely claim necessity → SNARE + fraud flag~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, mountain_coordination_confusion) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  HIGH RISK: Coordination Cut Safety~n', []),
    format('  │  Powerless see: MOUNTAIN (unchangeable, survival-critical)~n', []),
    format('  │  Institutions see: ROPE (optional, changeable)~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. SAFETY ASSESSMENT (DO NOT SKIP):~n', []),
    format('  │     - If institutions cut ~w, do individuals have alternatives?~n', [C]),
    format('  │     - Is this their only survival mechanism?~n', []),
    format('  │     - What scaffolding exists for transition?~n', []),
    format('  │  2. Test institutional perception:~n', []),
    format('  │     - Can institutions unilaterally change this?~n', []),
    format('  │     - Do they understand downstream impacts?~n', []),
    format('  │     - Is their "optional" view empirically accurate?~n', []),
    format('  │  3. Decision tree:~n', []),
    format('  │     IF truly unchangeable → Correct institutional view to MOUNTAIN~n', []),
    format('  │     IF changeable + safe alternatives → Correct powerless view to ROPE~n', []),
    format('  │     IF changeable + NO alternatives → ADD SCAFFOLD before any change~n', []),
    format('  │     IF uncertainty → HALT changes until resolved~n', []),
    format('  │  4. CRITICAL: Never proceed with changes until safety verified~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, conceptual, Desc, C, general_type_mismatch) :-
    format('  ┌─ [~w] CONCEPTUAL CLARIFICATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Map stakeholder perspectives:~n', []),
    format('  │     - Document how different actors perceive ~w~n', [C]),
    format('  │     - Identify source of divergence~n', []),
    format('  │  2. Gather evidence:~n', []),
    format('  │     - Empirical metrics (suppression, extraction, resistance)~n', []),
    format('  │     - Historical behavior patterns~n', []),
    format('  │  3. Create indexical classification:~n', []),
    format('  │     - From powerless context: classify as X~n', []),
    format('  │     - From institutional context: classify as Y~n', []),
    format('  │     - Add explicit context annotations~n', []),
    format('  └─~n~n', []).

generate_scenario_for_omega(OID, preference, Desc, C, _Gap) :-
    format('  ┌─ [~w] VALUE ARBITRATION~n', [OID]),
    format('  │  Constraint: ~w~n', [C]),
    format('  │  Gap: ~w~n', [Desc]),
    format('  │~n', []),
    format('  │  NOTE: Not resolvable via data or logic alone~n', []),
    format('  │~n', []),
    format('  │  RESOLUTION STRATEGY:~n', []),
    format('  │  1. Document competing value frameworks:~n', []),
    format('  │     - What values support current ~w?~n', [C]),
    format('  │     - What values oppose it?~n', []),
    format('  │     - Are these incommensurable?~n', []),
    format('  │  2. Propose scaffolded solution:~n', []),
    format('  │     - Design mechanism respecting both value sets~n', []),
    format('  │     - Create exit options for dissenters~n', []),
    format('  │     - Allow preference-based sorting~n', []),
    format('  │  3. Accept unresolvability if necessary:~n', []),
    format('  │     - Some omegas represent genuine value pluralism~n', []),
    format('  │     - Solution: coexistence, not consensus~n', []),
    format('  └─~n~n', []).

/* ============================================================================
   6. OMEGA SEVERITY TRIAGE
   ============================================================================ */

%% omega_severity(+OmegaID, -Severity)
%  Prioritizes omegas by severity/urgency.
omega_severity(OID, critical) :-
    atom(OID),
    (sub_atom(OID, _, _, _, extraction_blindness)
    ; (narrative_ontology:omega_variable(OID, _, Desc),
       atom(Desc),
       (sub_atom(Desc, _, _, _, extraction)
       ; sub_atom(Desc, _, _, _, snare)
       ; sub_atom(Desc, _, _, _, 'Snare')
       ; sub_atom(Desc, _, _, _, trap)))
    ), !.

omega_severity(OID, high) :-
    atom(OID),
    (sub_atom(OID, _, _, _, learned_helplessness)
    ; sub_atom(OID, _, _, _, cut_safety)
    ; narrative_ontology:omega_variable(OID, conceptual, _)
    ), !.

omega_severity(OID, moderate) :-
    atom(OID),
    narrative_ontology:omega_variable(OID, empirical, _), !.

omega_severity(OID, low) :-
    atom(OID),
    narrative_ontology:omega_variable(OID, preference, _), !.

omega_severity(_, unknown).

%% generate_omega_triage/0
%  Displays omegas organized by severity level.
generate_omega_triage :-
    format('~n[OMEGA TRIAGE & PRIORITIZATION]~n'),
    % First collect all actual omegas
    findall(OID, narrative_ontology:omega_variable(OID, _, _), AllOmegas),
    (AllOmegas = []
    -> format('  No omegas to triage.~n')
    ;  % Then organize by severity
       forall(member(Sev, [critical, high, moderate, low]),
              (findall(OID,
                       (member(OID, AllOmegas), omega_severity(OID, Sev)),
                       OIDs),
               (OIDs \= []
               -> (length(OIDs, N),
                   format('~n  [~w] ~w omega(s):~n', [Sev, N]),
                   forall(member(OID, OIDs),
                          (narrative_ontology:omega_variable(OID, Type, Desc),
                           format('    - ~w (~w)~n      ~w~n', [OID, Type, Desc]))))
               ;  true)))
    ).

/* ============================================================================
   7. COMPREHENSIVE CROSS-DOMAIN AUDIT
   ============================================================================ */

%% cross_domain_audit/0
%  Scans ALL constraints in current KB and reports cross-domain structural twins.
%  This provides a comprehensive view of isomorphic patterns across different domains.
cross_domain_audit :-
    format('~n[COMPREHENSIVE CROSS-DOMAIN STRUCTURAL TWINS]~n'),
    findall(iso(C1, C2, Score, Cat1, Cat2),
            (narrative_ontology:constraint_claim(C1, _),
             domain_priors:category_of(C1, Cat1),
             isomorphism_engine:find_isomorphism(C1, C2, Score),
             domain_priors:category_of(C2, Cat2),
             Cat1 \= Cat2,  % Only cross-domain twins
             C1 @< C2),     % Prevent duplicate pairs (A,B) and (B,A)
            Isos),
    (Isos = []
    -> format('  No cross-domain isomorphisms detected.~n')
    ;  (length(Isos, N),
        format('  Found ~w cross-domain structural twins:~n~n', [N]),
        forall(member(iso(C1, C2, S, Cat1, Cat2), Isos),
               (format('  ~w (~w) ≈ ~w (~w)~n', [C1, Cat1, C2, Cat2]),
                format('    Similarity Score: ~2f~n', [S]),
                display_twin_rationale(C1, C2),
                nl)))
    ).

%% display_twin_rationale(+C1, +C2)
%  Explains why two constraints are considered structural twins.
display_twin_rationale(C1, C2) :-
    (structural_signatures:get_constraint_profile(C1, Profile1),
     structural_signatures:get_constraint_profile(C2, Profile2)
    -> (Profile1 = profile(A1, S1, R1, B1, Alt1, _, _),
        Profile2 = profile(A2, S2, R2, B2, Alt2, _, _),
        format('    Metrics: ', []),
        (abs(A1 - A2) < 0.15 -> format('Accum≈ ', []) ; true),
        (abs(S1 - S2) < 0.15 -> format('Supp≈ ', []) ; true),
        (abs(R1 - R2) < 0.15 -> format('Repr≈ ', []) ; true),
        (abs(B1 - B2) < 1.0 -> format('BenefΔ≈ ', []) ; true),
        (Alt1 = Alt2 -> format('AltMatch ', []) ; true),
        nl,
        format('    Implication: Solutions for ~w may inform ~w~n', [C1, C2]))
    ;  format('    (Profile data incomplete)~n')
    ).
