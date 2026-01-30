:- module(test_harness, [
    load_scenario/1,
    run_all_tests/0,    % New: One-button alias
    run_all_tests/1,
    run_all_tests/2,
    quick_check/1
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(coercion_projection).
:- use_module(data_repair).
:- use_module(data_verification).
:- use_module(pattern_analysis).
:- use_module(intent_engine).
:- use_module(constraint_bridge).
:- use_module(constraint_indexing).
:- use_module(drl_core).
:- use_module(drl_lifecycle).
:- use_module(report_generator).

% Run all registered test cases from validation_suite, or fall back to single ID
run_all_tests :-
    (   current_predicate(validation_suite:test_case/2)
    ->  findall(ID, validation_suite:test_case(_, ID), IDs),
        sort(IDs, UniqueIDs),
        length(UniqueIDs, N),
        format('~n>>> Running ~w registered test intervals~n', [N]),
        forall(member(IntervalID, UniqueIDs),
               (   catch(run_all_tests(IntervalID), E,
                         format('[ERROR] ~w failed: ~w~n', [IntervalID, E]))
               ))
    ;   format('~n>>> No validation_suite loaded, running default interval~n'),
        run_all_tests('tax_code_section_469')
    ).

run_all_tests(IntervalID) :-
    constraint_indexing:default_context(Ctx),
    run_all_tests(IntervalID, Ctx).

run_all_tests(IntervalID, Context) :-
    format('~n>>> INITIATING DR-AUDIT SUITE: ~w~n', [IntervalID]),
    
    % Step 1: Data Repair
    data_repair:repair_interval(IntervalID),

    % Step 2: Verification Gate
    (   data_verification:verify_all,
        data_verification:check_paired_measurements
    ->  format('[OK] Verification passed.~n')
    ;   format('[FAIL] Verification failed for ~w.~n', [IntervalID]), fail),

    % Step 3: Indexical Audit
    forall(narrative_ontology:constraint_claim(C, _),
           constraint_indexing:compare_perspectives(C, Context)),

    % Step 4: Intent and Reporting
    intent_engine:analyze_intent(IntervalID),
    constraint_bridge:dr_diagnostic_report(IntervalID),

    % Step 5: Lifecycle Drift Analysis
    format('~n--- LIFECYCLE DRIFT ANALYSIS ---~n'),
    drl_lifecycle:generate_drift_report,

    % Step 6: System Insights
    format('~n--- SYSTEM INSIGHTS ---~n'),
    narrative_ontology:count_unresolved_omegas(OmegaCount),
    format('  Omegas Identified: ~w~n', [OmegaCount]),
    report_generator:generate_full_report(IntervalID).

% Ensure quick_check uses the predicates from the authoritative module
quick_check(IntervalID) :-
    format('--- Diagnostic: ~w ---~n', [IntervalID]),
    (   drl_core:base_extractiveness(Name, E), E > 0.8
    ->  format('CRITICAL EXTRACTIVENESS: ~w (~2f)~n', [Name, E])
    ;   format('No critical extractiveness detected.~n')).

%% load_scenario(+Path)
load_scenario(Path) :-
    exists_file(Path),
    consult(Path),
    format('~n[SCENARIO] Successfully loaded: ~w~n', [Path]).
load_scenario(Path) :-
    \+ exists_file(Path), 
    format('~n[ERROR] Scenario file not found: ~w~n', [Path]),
    fail.
