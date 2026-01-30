:- module(utils, [
    safe_get_metric/4,
    safe_get_metric/5,
    safe_get_category/3,
    safe_call/2,
    safe_call/3,
    log/3,
    set_log_level/1,
    get_log_level/1,
    require_metric/3,
    validate_metric_value/2
]).

:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).

/* ============================================================================
   DEFENSIVE PROGRAMMING UTILITIES
   ============================================================================

   This module provides safe wrappers for common operations that might fail,
   enabling defensive programming practices across the codebase.

   Key features:
   - Safe metric retrieval with defaults
   - Exception handling wrappers
   - Configurable logging
   - Validation helpers

   ============================================================================ */

:- dynamic log_level/1.
log_level(info).  % Default log level: debug, info, warn, error

/* ============================================================================
   1. SAFE METRIC RETRIEVAL
   ============================================================================ */

%% safe_get_metric(+Constraint, +Metric, -Value, +Default)
%  Retrieves a metric value, using default if missing.
%  Does NOT log warnings (silent fallback).
safe_get_metric(Constraint, Metric, Value, Default) :-
    (narrative_ontology:constraint_metric(Constraint, Metric, V)
    -> Value = V
    ;  Value = Default).

%% safe_get_metric(+Constraint, +Metric, -Value, +Default, +LogWarning)
%  Retrieves a metric value, using default if missing.
%  Logs warning if LogWarning = true and metric is missing.
safe_get_metric(Constraint, Metric, Value, Default, LogWarning) :-
    (narrative_ontology:constraint_metric(Constraint, Metric, V)
    -> Value = V
    ;  (Value = Default,
        (LogWarning = true
        -> log(warn, 'Missing metric ~w for ~w, using default ~w',
               [Metric, Constraint, Default])
        ;  true))).

%% safe_get_category(+Constraint, -Category, +Default)
%  Retrieves domain category, using default if missing.
safe_get_category(Constraint, Category, Default) :-
    (domain_priors:category_of(Constraint, Cat)
    -> (Cat \= unknown, Cat \= unknown_novel
       -> Category = Cat
       ;  (Category = Default,
           log(warn, 'Unknown category for ~w, using default ~w',
               [Constraint, Default])))
    ;  (Category = Default,
        log(warn, 'No category for ~w, using default ~w',
            [Constraint, Default]))).

%% require_metric(+Constraint, +Metric, -Value)
%  Retrieves a metric value, FAILING if missing (strict mode).
%  Use this when a metric is truly required for correctness.
require_metric(Constraint, Metric, Value) :-
    (narrative_ontology:constraint_metric(Constraint, Metric, Value)
    -> true
    ;  (log(error, 'REQUIRED metric ~w missing for ~w',
            [Metric, Constraint]),
        fail)).

%% validate_metric_value(+Metric, +Value)
%  Validates that a metric value is in acceptable range.
validate_metric_value(_Metric, Value) :-
    number(Value),
    Value >= 0.0,
    Value =< 1.0.

/* ============================================================================
   2. SAFE CALL WRAPPERS
   ============================================================================ */

%% safe_call(+Goal, +ErrorMessage)
%  Executes Goal with exception handling.
%  Logs error and fails gracefully if exception occurs.
safe_call(Goal, ErrorMessage) :-
    catch(Goal,
          Error,
          (log(error, '~w: ~w', [ErrorMessage, Error]),
           fail)).

%% safe_call(+Goal, +ErrorMessage, +Default)
%  Executes Goal with exception handling.
%  Returns Default if Goal fails or throws exception.
safe_call(Goal, _ErrorMessage, _Default) :-
    catch(Goal, _, fail),
    !.
safe_call(_Goal, ErrorMessage, Default) :-
    log(warn, '~w, using default: ~w', [ErrorMessage, Default]),
    Default = Default.

/* ============================================================================
   3. LOGGING INFRASTRUCTURE
   ============================================================================ */

%% set_log_level(+Level)
%  Sets the logging level.
%  Levels: debug < info < warn < error
set_log_level(Level) :-
    member(Level, [debug, info, warn, error]),
    retractall(log_level(_)),
    assertz(log_level(Level)).

%% get_log_level(-Level)
%  Gets the current logging level.
get_log_level(Level) :-
    log_level(Level).

%% log(+Level, +Format, +Args)
%  Logs a message at the specified level.
%  Only logs if current log level permits.
log(Level, Format, Args) :-
    log_level(CurrentLevel),
    level_priority(Level, LP),
    level_priority(CurrentLevel, CP),
    (LP >= CP
    -> format_log_message(Level, Format, Args)
    ;  true).

%% level_priority(+Level, -Priority)
%  Maps log levels to numeric priorities.
level_priority(debug, 0).
level_priority(info, 1).
level_priority(warn, 2).
level_priority(error, 3).

%% format_log_message(+Level, +Format, +Args)
%  Formats and displays a log message.
format_log_message(Level, Format, Args) :-
    level_prefix(Level, Prefix),
    format('[~w] ', [Prefix]),
    format(Format, Args),
    nl.

%% level_prefix(+Level, -Prefix)
%  Maps log levels to display prefixes.
level_prefix(debug, 'DEBUG').
level_prefix(info, 'INFO').
level_prefix(warn, 'WARN').
level_prefix(error, 'ERROR').

/* ============================================================================
   4. ENHANCED METRIC RETRIEVAL WITH VALIDATION
   ============================================================================ */

%% safe_get_extractiveness(+Constraint, -Extractiveness)
%  Safe retrieval of base extractiveness with validation.
safe_get_extractiveness(Constraint, Extractiveness) :-
    safe_get_extractiveness(Constraint, Extractiveness, 0.0, true).

%% safe_get_extractiveness(+Constraint, -Extractiveness, +Default, +Warn)
safe_get_extractiveness(Constraint, Extractiveness, Default, Warn) :-
    (domain_priors:base_extractiveness(Constraint, E)
    -> (validate_metric_value(extractiveness, E)
       -> Extractiveness = E
       ;  (log(error, 'Invalid extractiveness ~w for ~w (must be 0.0-1.0)',
                [E, Constraint]),
           Extractiveness = Default))
    ;  (Extractiveness = Default,
        (Warn = true
        -> log(warn, 'Missing extractiveness for ~w, using default ~w',
               [Constraint, Default])
        ;  true))).

%% safe_get_suppression(+Constraint, -Suppression)
%  Safe retrieval of suppression requirement with validation.
safe_get_suppression(Constraint, Suppression) :-
    safe_get_suppression(Constraint, Suppression, 0.0, true).

%% safe_get_suppression(+Constraint, -Suppression, +Default, +Warn)
safe_get_suppression(Constraint, Suppression, Default, Warn) :-
    safe_get_metric(Constraint, suppression_requirement, Suppression, Default, Warn).

/* ============================================================================
   5. BATCH OPERATIONS WITH SAFE RETRIEVAL
   ============================================================================ */

%% safe_get_all_metrics(+Constraint, -Metrics)
%  Retrieves all standard metrics for a constraint.
%  Returns metrics(Extr, Supp, Resist, Category) structure.
safe_get_all_metrics(Constraint, metrics(Extr, Supp, Resist, Category)) :-
    safe_get_extractiveness(Constraint, Extr, 0.0, false),
    safe_get_suppression(Constraint, Supp, 0.0, false),
    safe_get_metric(Constraint, resistance_to_change, Resist, 0.0, false),
    safe_get_category(Constraint, Category, unknown).

%% safe_get_profile_components(+Constraint, -Components)
%  Safely retrieves all components needed for structural signature.
safe_get_profile_components(Constraint, components(Accum, Supp, Resist, BenefDelta, AltPresent)) :-
    safe_get_metric(Constraint, accumulation_speed, Accum, 0.0, false),
    safe_get_metric(Constraint, suppression_requirement, Supp, 0.0, false),
    safe_get_metric(Constraint, resistance_to_change, Resist, 0.0, false),

    % Calculate beneficiary delta safely
    (findall(B, narrative_ontology:veto_actor(B), Beneficiaries)
    -> length(Beneficiaries, BenefDelta)
    ;  BenefDelta = 0),

    % Check for alternatives safely
    (narrative_ontology:intent_viable_alternative(_, _, _)
    -> AltPresent = present
    ;  AltPresent = absent).

/* ============================================================================
   6. DEFENSIVE CONFIG RETRIEVAL
   ============================================================================ */

%% safe_get_config(+ParamName, -Value, +Default)
%  Safely retrieves configuration parameter.
safe_get_config(ParamName, Value, Default) :-
    (config:param(ParamName, V)
    -> Value = V
    ;  (Value = Default,
        log(warn, 'Missing config parameter ~w, using default ~w',
            [ParamName, Default]))).

%% require_config(+ParamName, -Value)
%  Retrieves config parameter, failing if missing (strict mode).
require_config(ParamName, Value) :-
    (config:param(ParamName, Value)
    -> true
    ;  (log(error, 'REQUIRED config parameter ~w is missing', [ParamName]),
        fail)).

/* ============================================================================
   7. SAFE LIST OPERATIONS
   ============================================================================ */

%% safe_member(+Element, +List, +Default)
%  Checks membership, returning Default if List is not a list.
safe_member(Element, List, Default) :-
    (is_list(List)
    -> member(Element, List)
    ;  (log(warn, 'Expected list but got ~w, using default ~w',
            [List, Default]),
        Element = Default)).

%% safe_length(+List, -Length)
%  Gets list length, returning 0 if not a list.
safe_length(List, Length) :-
    (is_list(List)
    -> length(List, Length)
    ;  (log(warn, 'Expected list but got ~w, returning length 0', [List]),
        Length = 0)).

/* ============================================================================
   8. CONSTRAINT EXISTENCE CHECKS
   ============================================================================ */

%% constraint_exists(+Constraint)
%  Checks if a constraint is defined in the knowledge base.
constraint_exists(Constraint) :-
    narrative_ontology:constraint_claim(Constraint, _).

%% require_constraint(+Constraint)
%  Requires that a constraint exists, failing with error if not.
require_constraint(Constraint) :-
    (constraint_exists(Constraint)
    -> true
    ;  (log(error, 'Constraint ~w does not exist in knowledge base', [Constraint]),
        fail)).

/* ============================================================================
   9. NUMERIC SAFETY HELPERS
   ============================================================================ */

%% safe_divide(+Numerator, +Denominator, -Result, +Default)
%  Safe division with zero-denominator handling.
safe_divide(Num, Denom, Result, Default) :-
    (Denom =\= 0
    -> Result is Num / Denom
    ;  (Result = Default,
        log(warn, 'Division by zero: ~w / ~w, using default ~w',
            [Num, Denom, Default]))).

%% safe_sqrt(+Value, -Result, +Default)
%  Safe square root with negative value handling.
safe_sqrt(Value, Result, Default) :-
    (Value >= 0
    -> Result is sqrt(Value)
    ;  (Result = Default,
        log(warn, 'Square root of negative value ~w, using default ~w',
            [Value, Default]))).

%% clamp(+Value, +Min, +Max, -ClampedValue)
%  Clamps a value to [Min, Max] range.
clamp(Value, Min, Max, ClampedValue) :-
    (Value < Min
    -> (ClampedValue = Min,
        log(debug, 'Value ~w clamped to minimum ~w', [Value, Min]))
    ; Value > Max
    -> (ClampedValue = Max,
        log(debug, 'Value ~w clamped to maximum ~w', [Value, Max]))
    ;  ClampedValue = Value).

/* ============================================================================
   10. DEBUGGING HELPERS
   ============================================================================ */

%% debug_constraint(+Constraint)
%  Prints all available information about a constraint for debugging.
debug_constraint(Constraint) :-
    format('~n=== DEBUG INFO: ~w ===~n', [Constraint]),

    % Check existence
    (constraint_exists(Constraint)
    -> format('  Status: EXISTS~n')
    ;  format('  Status: NOT FOUND~n')),

    % Show claimed type
    (narrative_ontology:constraint_claim(Constraint, Type)
    -> format('  Claimed Type: ~w~n', [Type])
    ;  format('  Claimed Type: NONE~n')),

    % Show metrics
    format('  Metrics:~n'),
    (narrative_ontology:constraint_metric(Constraint, extractiveness, E)
    -> format('    extractiveness: ~2f~n', [E])
    ;  format('    extractiveness: MISSING~n')),

    (narrative_ontology:constraint_metric(Constraint, suppression_requirement, S)
    -> format('    suppression_requirement: ~2f~n', [S])
    ;  format('    suppression_requirement: MISSING~n')),

    (narrative_ontology:constraint_metric(Constraint, resistance_to_change, R)
    -> format('    resistance_to_change: ~2f~n', [R])
    ;  format('    resistance_to_change: MISSING~n')),

    % Show category
    (domain_priors:category_of(Constraint, Cat)
    -> format('  Category: ~w~n', [Cat])
    ;  format('  Category: MISSING~n')),

    format('=================~n~n').

%% trace_metric_retrieval(+Constraint, +Metric)
%  Traces all attempts to retrieve a metric for debugging.
trace_metric_retrieval(Constraint, Metric) :-
    format('~n=== TRACING: ~w.~w ===~n', [Constraint, Metric]),

    (narrative_ontology:constraint_metric(Constraint, Metric, Value)
    -> format('  ✓ Found: ~w~n', [Value])
    ;  format('  ✗ Not found~n')),

    format('  All metrics for ~w:~n', [Constraint]),
    forall(narrative_ontology:constraint_metric(Constraint, M, V),
           format('    ~w: ~w~n', [M, V])),

    format('=================~n~n').
