:- module(data_validation, [
    validate_all/0,
    validate_constraint_completeness/0,
    validate_metric_ranges/0,
    validate_classification_consistency/0,
    validate_edge_cases/0,
    validate_domain_coverage/0,
    validation_summary/0
]).

:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(drl_core).
:- use_module(config).
:- use_module(utils).  % For safe metric retrieval

/* ============================================================================
   DATA VALIDATION SUITE
   ============================================================================

   This module provides comprehensive validation of constraint data quality,
   completeness, and consistency. Unlike validation_suite.pl (which runs
   scenarios), this validates the data itself.

   ============================================================================ */

:- dynamic validation_error/3.    % validation_error(Type, Constraint, Details)
:- dynamic validation_warning/3.   % validation_warning(Type, Constraint, Details)
:- dynamic validation_info/2.      % validation_info(Type, Message)

/* ============================================================================
   1. MAIN VALIDATION ENTRY POINT
   ============================================================================ */

%% validate_all/0
%  Runs all validation checks and reports results.
validate_all :-
    retractall(validation_error(_, _, _)),
    retractall(validation_warning(_, _, _)),
    retractall(validation_info(_, _)),

    format('~n====================================================~n'),
    format('   DATA VALIDATION SUITE                           ~n'),
    format('====================================================~n~n'),

    write('Running validation checks...'), nl, nl,

    % Run all validation checks
    validate_constraint_completeness,
    validate_metric_ranges,
    validate_classification_consistency,
    validate_edge_cases,
    validate_domain_coverage,

    % Report results
    validation_summary.

/* ============================================================================
   2. CONSTRAINT COMPLETENESS VALIDATION
   ============================================================================ */

%% validate_constraint_completeness/0
%  Checks that all constraints have required data.
validate_constraint_completeness :-
    format('[CHECK 1: Constraint Completeness]~n'),
    findall(C, narrative_ontology:constraint_claim(C, _), Constraints),
    length(Constraints, Total),
    format('  Checking ~w constraints...~n', [Total]),

    findall(C, (member(C, Constraints), is_complete_constraint(C)), Complete),
    length(Complete, CompleteCount),
    Incomplete is Total - CompleteCount,

    (Incomplete > 0
    -> (format('  ✗ ~w incomplete constraint(s) found~n~n', [Incomplete]),
        forall((member(C, Constraints), \+ is_complete_constraint(C)),
               report_incomplete_constraint(C)))
    ;  format('  ✓ All constraints complete~n~n', [])).

%% is_complete_constraint(+Constraint)
%  True if constraint has all required data.
is_complete_constraint(C) :-
    % Must have extractiveness metric
    (narrative_ontology:constraint_metric(C, extractiveness, E)
    -> (E >= 0.0, E =< 1.0)
    ;  (assertz(validation_error(missing_metric, C, extractiveness)), fail)),

    % Must have suppression_requirement metric
    (narrative_ontology:constraint_metric(C, suppression_requirement, S)
    -> (S >= 0.0, S =< 1.0)
    ;  (assertz(validation_error(missing_metric, C, suppression_requirement)), fail)),

    % Must have domain category
    (domain_priors:category_of(C, Cat)
    -> (Cat \= unknown, Cat \= unknown_novel)
    ;  (assertz(validation_warning(missing_category, C, 'No domain category assigned')), fail)),

    % Should have at least one indexical classification
    (constraint_indexing:constraint_classification(C, _, _)
    -> true
    ;  (assertz(validation_warning(missing_classification, C, 'No indexical classification')), fail)).

%% report_incomplete_constraint(+Constraint)
%  Reports what data is missing for a constraint.
report_incomplete_constraint(C) :-
    format('  Constraint: ~w~n', [C]),

    % Check extractiveness
    (\+ narrative_ontology:constraint_metric(C, extractiveness, _)
    -> format('    ✗ Missing: extractiveness metric~n')
    ;  true),

    % Check suppression
    (\+ narrative_ontology:constraint_metric(C, suppression_requirement, _)
    -> format('    ✗ Missing: suppression_requirement metric~n')
    ;  true),

    % Check category
    (\+ domain_priors:category_of(C, _)
    -> format('    ✗ Missing: domain category~n')
    ; domain_priors:category_of(C, Cat),
      (Cat = unknown ; Cat = unknown_novel)
    -> format('    ⚠ Category is ~w (should be specific)~n', [Cat])
    ;  true),

    % Check classification
    (\+ constraint_indexing:constraint_classification(C, _, _)
    -> format('    ⚠ Missing: indexical classification~n')
    ;  true),

    nl.

/* ============================================================================
   3. METRIC RANGE VALIDATION
   ============================================================================ */

%% validate_metric_ranges/0
%  Checks that all metrics are within valid ranges.
validate_metric_ranges :-
    format('[CHECK 2: Metric Range Validation]~n'),

    % Check all constraint metrics
    findall(C-M-V, narrative_ontology:constraint_metric(C, M, V), Metrics),
    length(Metrics, TotalMetrics),
    format('  Checking ~w metrics...~n', [TotalMetrics]),

    findall(C-M-V,
            (narrative_ontology:constraint_metric(C, M, V),
             \+ valid_metric_range(M, V)),
            InvalidMetrics),

    length(InvalidMetrics, InvalidCount),
    (InvalidCount > 0
    -> (format('  ✗ ~w invalid metric(s) found~n~n', [InvalidCount]),
        forall(member(C-M-V, InvalidMetrics),
               (format('    ✗ ~w.~w = ~w (out of range)~n', [C, M, V]),
                assertz(validation_error(invalid_metric_range, C, M-V)))))
    ;  format('  ✓ All metrics in valid range~n', [])),
    nl.

%% valid_metric_range(+Metric, +Value)
%  Checks if a metric value is in valid range.
valid_metric_range(_, V) :-
    number(V),
    V >= 0.0,
    V =< 1.0.

/* ============================================================================
   4. CLASSIFICATION CONSISTENCY VALIDATION
   ============================================================================ */

%% validate_classification_consistency/0
%  Checks that claimed types match calculated types.
validate_classification_consistency :-
    format('[CHECK 3: Classification Consistency]~n'),
    findall(C, narrative_ontology:constraint_claim(C, _), Constraints),
    length(Constraints, Total),
    format('  Checking ~w constraints for consistency...~n', [Total]),

    findall(C-Claimed-Expected,
            (narrative_ontology:constraint_claim(C, Claimed),
             infer_expected_type(C, Expected),
             Claimed \= Expected),
            Inconsistencies),

    length(Inconsistencies, IncCount),
    (IncCount > 0
    -> (format('  ⚠ ~w potential inconsistenc(ies) found~n~n', [IncCount]),
        forall(member(C-Claimed-Expected, Inconsistencies),
               report_classification_inconsistency(C, Claimed, Expected)))
    ;  format('  ✓ All classifications consistent with metrics~n', [])),
    nl.

%% infer_expected_type(+Constraint, -Type)
%  Infers what type a constraint should be based on metrics.
infer_expected_type(C, Type) :-
    (narrative_ontology:constraint_metric(C, suppression_requirement, Supp),
     domain_priors:base_extractiveness(C, Extr),
     config:param(mountain_suppression_ceiling, Ceil)
    -> classify_by_metrics(Supp, Extr, Ceil, Type)
    ;  Type = unknown).

%% classify_by_metrics(+Supp, +Extr, +Ceil, -Type)
%  Classification logic based on metrics.
classify_by_metrics(Supp, Extr, Ceil, mountain) :-
    Supp =< Ceil, Extr =< 0.35, !.

classify_by_metrics(Supp, Extr, Ceil, snare) :-
    Supp > Ceil, Extr > 0.66, !.

classify_by_metrics(Supp, Extr, Ceil, tangled_rope) :-
    Supp > Ceil, Extr > 0.35, !.

classify_by_metrics(Supp, _Extr, Ceil, rope) :-
    Supp > Ceil, !.

classify_by_metrics(_Supp, Extr, _Ceil, snare) :-
    Extr > 0.66, !.

classify_by_metrics(_, _, _, ambiguous).

%% report_classification_inconsistency(+C, +Claimed, +Expected)
report_classification_inconsistency(C, Claimed, Expected) :-
    narrative_ontology:constraint_metric(C, suppression_requirement, Supp),
    domain_priors:base_extractiveness(C, Extr),
    config:param(mountain_suppression_ceiling, Ceil),

    format('  Constraint: ~w~n', [C]),
    format('    Claimed: ~w~n', [Claimed]),
    format('    Expected (from metrics): ~w~n', [Expected]),
    format('    Metrics: suppression=~2f, extractiveness=~2f, ceiling=~2f~n', [Supp, Extr, Ceil]),

    (Expected = snare, Claimed = mountain
    -> (format('    ⚠ CRITICAL: False Mountain (extractive but claimed unchangeable)~n'),
        assertz(validation_error(false_mountain, C, Claimed-Expected)))
    ; Expected = mountain, Claimed = snare
    -> (format('    ⚠ False Snare (fair but claimed extractive)~n'),
        assertz(validation_warning(false_snare, C, Claimed-Expected)))
    ;  assertz(validation_warning(classification_mismatch, C, Claimed-Expected))
    ),
    nl.

/* ============================================================================
   5. EDGE CASE DETECTION
   ============================================================================ */

%% validate_edge_cases/0
%  Detects unusual patterns and edge cases.
validate_edge_cases :-
    format('[CHECK 4: Edge Case Detection]~n'),

    % Check for mandatrophies (unchangeable + extractive)
    findall(C,
            (narrative_ontology:constraint_claim(C, mountain),
             domain_priors:base_extractiveness(C, E),
             E > 0.7),
            Mandatrophies),

    length(Mandatrophies, MandCount),
    (MandCount > 0
    -> (format('  ⚠ ~w potential mandatroph(ies) detected~n', [MandCount]),
        forall(member(C, Mandatrophies),
               (domain_priors:base_extractiveness(C, E),
                format('    - ~w (mountain with E=~2f > 0.7)~n', [C, E]),
                assertz(validation_warning(mandatrophy, C, E)))))
    ;  format('  ✓ No mandatrophies detected~n', [])),

    % Check for pitons (no resistance despite enforcement)
    findall(C,
            (narrative_ontology:constraint_metric(C, suppression_requirement, S),
             narrative_ontology:constraint_metric(C, resistance_to_change, R),
             S > 0.3, R < 0.1),
            Zombies),

    length(Zombies, ZombieCount),
    (ZombieCount > 0
    -> (format('  ⚠ ~w potential piton(s) detected~n', [ZombieCount]),
        forall(member(C, Zombies),
               (narrative_ontology:constraint_metric(C, suppression_requirement, S),
                narrative_ontology:constraint_metric(C, resistance_to_change, R),
                format('    - ~w (S=~2f but R=~2f < 0.1)~n', [C, S, R]),
                assertz(validation_warning(piton, C, S-R)))))
    ;  format('  ✓ No pitons detected~n', [])),

    % Check for extreme values
    findall(C-M-V,
            (narrative_ontology:constraint_metric(C, M, V),
             (V > 0.95 ; V < 0.05),
             member(M, [extractiveness, suppression_requirement, resistance_to_change])),
            ExtremeValues),

    length(ExtremeValues, ExtCount),
    (ExtCount > 0
    -> (format('  ⚠ ~w extreme value(s) detected (>0.95 or <0.05)~n', [ExtCount]),
        forall(member(C-M-V, ExtremeValues),
               (format('    - ~w.~w = ~2f~n', [C, M, V]),
                assertz(validation_info(extreme_value, C-M-V)))))
    ;  format('  ✓ No extreme values~n', [])),

    nl.

/* ============================================================================
   6. DOMAIN COVERAGE VALIDATION
   ============================================================================ */

%% validate_domain_coverage/0
%  Checks distribution across domain categories.
validate_domain_coverage :-
    format('[CHECK 5: Domain Coverage]~n'),

    findall(Cat, domain_priors:category_of(_, Cat), AllCats),
    sort(AllCats, UniqueCats),
    length(UniqueCats, NumCats),

    format('  Found ~w domain categories~n', [NumCats]),

    forall(member(Cat, UniqueCats),
           (findall(C, domain_priors:category_of(C, Cat), Cs),
            length(Cs, Count),
            format('    - ~w: ~w constraint(s)~n', [Cat, Count]))),

    % Check for underrepresented categories
    findall(Cat,
            (member(Cat, UniqueCats),
             findall(C, domain_priors:category_of(C, Cat), Cs),
             length(Cs, Count),
             Count < 3),
            UnderRep),

    (UnderRep \= []
    -> (nl, format('  ⚠ Underrepresented categories (<3 constraints):~n'),
        forall(member(Cat, UnderRep),
               (format('    - ~w~n', [Cat]),
                assertz(validation_info(underrepresented_category, Cat)))))
    ;  true),

    nl.

/* ============================================================================
   7. VALIDATION SUMMARY
   ============================================================================ */

%% validation_summary/0
%  Displays summary of validation results.
validation_summary :-
    format('====================================================~n'),
    format('   VALIDATION SUMMARY                              ~n'),
    format('====================================================~n~n'),

    % Count errors
    findall(E, validation_error(_, _, E), Errors),
    length(Errors, ErrorCount),

    % Count warnings
    findall(W, validation_warning(_, _, W), Warnings),
    length(Warnings, WarningCount),

    % Count info
    findall(I, validation_info(_, I), Infos),
    length(Infos, InfoCount),

    format('Results:~n'),
    format('  Errors:   ~w~n', [ErrorCount]),
    format('  Warnings: ~w~n', [WarningCount]),
    format('  Info:     ~w~n~n', [InfoCount]),

    % Show errors
    (ErrorCount > 0
    -> (format('ERRORS:~n'),
        forall(validation_error(Type, C, Details),
               format('  [ERROR] ~w: ~w - ~w~n', [Type, C, Details])),
        nl)
    ;  true),

    % Show critical warnings
    (WarningCount > 0
    -> (format('WARNINGS:~n'),
        forall(validation_warning(Type, C, Details),
               format('  [WARN] ~w: ~w - ~w~n', [Type, C, Details])),
        nl)
    ;  true),

    % Overall assessment
    (ErrorCount = 0, WarningCount = 0
    -> format('✓ DATA QUALITY: EXCELLENT - No issues detected~n')
    ; ErrorCount = 0, WarningCount < 10
    -> format('✓ DATA QUALITY: GOOD - Minor warnings only~n')
    ; ErrorCount > 0, ErrorCount < 5
    -> format('⚠ DATA QUALITY: FAIR - Some errors need attention~n')
    ;  format('✗ DATA QUALITY: POOR - Multiple errors require fixes~n')
    ),

    format('~n====================================================~n').
