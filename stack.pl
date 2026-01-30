:- module(stack, [
    initialize_system/0,
    run_scenario/2
]).

% 1. Load Core Knowledge Schema
:- use_module(narrative_ontology).      % Schema & Global Expansion
:- use_module(config).             % Grounded Weights & Thresholds

% 2. Load Data & Priors
:- use_module(domain_priors).
:- use_module(constraint_instances).

% 3. Load Core Logic
:- use_module(constraint_indexing).
:- use_module(structural_signatures).
:- use_module(drl_core).
:- use_module(drl_modal_logic, []).
:- use_module(drl_audit_core, []).

% 4. Load Management & Control (NEW)
:- use_module(scenario_manager, []).    % Lifecycle Controller

% 5. Load Functional Engines (Imported but silenced for namespace safety)
:- use_module(coercion_projection, []).
:- use_module(data_repair, []).
:- use_module(data_verification, []).
:- use_module(pattern_analysis, []).
:- use_module(intent_engine, []).
:- use_module(drl_lifecycle, []).       % Drift event detection & lifecycle analysis

% 4. Load Diagnostic & UI
:- use_module(constraint_bridge, []).
:- use_module(uke_dr_bridge, []).
:- use_module(report_generator, []).
:- use_module(test_harness, []).

% Add the directory of this file to the library search path
:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(library, Dir)).

/* ================================================================
   SCENARIO ALIASES
   ================================================================ */

%% run_scenario(+File, +IntervalID)
%  Convenience alias for the Scenario Manager's load_and_run predicate.
run_scenario(File, IntervalID) :-
    scenario_manager:load_and_run(File, IntervalID).

/* ================================================================
   INITIALIZATION
   ================================================================ */

initialize_system :-
    format('~n====================================================~n'),
    format('   STRUCTURAL ANALYSIS STACK INITIALIZED             ~n'),
    format('====================================================~n'),
    format('Control:    Scenario Manager Active~n'),
    format('Usage:      run_scenario(\'file.pl\', interval_id).~n'),
    format('====================================================~n').

:- initialize_system.
