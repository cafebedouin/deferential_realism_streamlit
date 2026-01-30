:- module(data_repair, [
    repair_interval/1
]).

:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(domain_priors). % NEW: Hook into the Epistemic Prior Library
:- use_module(signature_mapper).
:- use_module(constraint_indexing).
:- use_module(drl_core).

% Ensure we can add facts to the ontology's measurement predicate
:- dynamic narrative_ontology:measurement/5.

/* ============================================================
   REPAIR ORCHESTRATOR
   ============================================================ */

%% repair_interval(+IntervalID)
% Audits the measurement vectors for a given interval and repairs gaps
% using domain-specific epistemic priors.
repair_interval(IntervalID) :-
    (   narrative_ontology:interval(IntervalID, T0, Tn)
    ->  format('~n[REPAIR] Auditing vectors for: ~w...~n', [IntervalID]),

        % 0. V3.4 DATA BRIDGE: Derive constraint_claim/2 and constraint_metric/3
        %    from indexed classifications and domain_priors when missing.
        bridge_v34_data(IntervalID),

        % 1. PILLAR REMAPPING: Fix non-standard claims before verification
        forall(narrative_ontology:constraint_claim(C, Type),
               (   signature_mapper:map_custom_pillar(C, Type, Standard),
                   (Type \= Standard -> 
                    retract(narrative_ontology:constraint_claim(C, Type)),
                    assertz(narrative_ontology:constraint_claim(C, Standard)),
                    format('  [FIXED] Remapped ~w: ~w -> ~w~n', [C, Type, Standard])
                   ; true)
               )),

        % 2. VECTOR REPAIR: Impute missing measurements
        forall(config:level(L), 
               ( repair_point(L, T0, IntervalID), 
                 repair_point(L, Tn, IntervalID) 
               ))
    ;   format('~n[ERROR] Interval ~w not found.~n', [IntervalID]),
        false
    ).

%% repair_point(+Level, +Time, +IntervalID)
% Iterates through the 4-component coercion vector at a specific time point.
repair_point(Level, Time, IntervalID) :-
    Components = [accessibility_collapse(Level), stakes_inflation(Level), 
                  suppression(Level), resistance(Level)],
    forall(member(Metric, Components), 
           ensure_metric_exists(Metric, Time, IntervalID)).

%% ensure_metric_exists(+Metric, +Time, +IntervalID)
% Core v3.2 Imputation Logic:
% 1. Checks for existing data.
% 2. Resolves prior based on domain type.
% 3. Flags novelty if the domain is unmapped.
ensure_metric_exists(Metric, Time, IntervalID) :-
    % Look directly into the ontology for existing measurement
    narrative_ontology:measurement(_, _, Metric, Time, _)
    ->  true
    ;   (   % NEW: Fetch prior value instead of hard-coded 0.5
            domain_priors:get_prior(IntervalID, Metric, Value),
            
            % NEW: Surface novelty alert to the LLM/User
            (domain_priors:is_known_domain(IntervalID) -> true ; domain_priors:flag_novelty(IntervalID)),
            
            gensym(repair_m_, SyntheticID),
            % Assert the synthetic fact into the global ontology
            assertz(narrative_ontology:measurement(SyntheticID, IntervalID, Metric, Time, Value)),
            format('  [FIXED] Imputed ~w for ~w at T=~w~n', [Value, Metric, Time])
    ).

/* ============================================================
   V3.4 DATA BRIDGE

   v3.4 testsets define constraint_classification/3 and
   domain_priors but NOT constraint_claim/2 or
   constraint_metric/3. This bridge auto-derives the missing
   ontology facts so the audit framework can find them.
   ============================================================ */

%% bridge_v34_data(+IntervalID)
%  Master bridge: derives constraint_claim, constraint_metric,
%  and omega_variable facts from v3.4 indexed data.
bridge_v34_data(IntervalID) :-
    % Metrics must be bridged FIRST because bridge_constraint_claim
    % uses drl_core:dr_type/2 which queries constraint_metric/3.
    bridge_domain_metrics(IntervalID),
    bridge_constraint_claim(IntervalID),
    bridge_omega_variables(IntervalID).

%% bridge_constraint_claim(+IntervalID)
%  If no constraint_claim/2 exists for IntervalID but
%  constraint_classification/3 facts do, derive the claim
%  using the analytical classification from drl_core.
bridge_constraint_claim(IntervalID) :-
    (   narrative_ontology:constraint_claim(IntervalID, _)
    ->  true  % Already has a claim, no bridge needed
    ;   (   constraint_indexing:constraint_classification(IntervalID, _, _)
        ->  % Classifications exist but no claim: derive from drl_core
            (   catch(drl_core:dr_type(IntervalID, DerivedType), _, fail)
            ->  assertz(narrative_ontology:constraint_claim(IntervalID, DerivedType)),
                format('  [BRIDGE] Derived constraint_claim(~w, ~w) from analytical classification~n',
                       [IntervalID, DerivedType])
            ;   % Fallback: use first indexed classification found
                constraint_indexing:constraint_classification(IntervalID, FallbackType, _),
                assertz(narrative_ontology:constraint_claim(IntervalID, FallbackType)),
                format('  [BRIDGE] Derived constraint_claim(~w, ~w) from first indexed classification~n',
                       [IntervalID, FallbackType])
            )
        ;   true  % No classifications at all, nothing to bridge
        )
    ).

%% bridge_domain_metrics(+IntervalID)
%  Maps domain_priors predicates to narrative_ontology:constraint_metric/3
%  so that constraint_bridge:constraint_status/3 can find them.
bridge_domain_metrics(IntervalID) :-
    bridge_single_metric(IntervalID, base_extractiveness, extractiveness),
    bridge_single_metric(IntervalID, suppression_score, suppression_requirement),
    bridge_single_metric(IntervalID, theater_ratio, theater_ratio).

bridge_single_metric(IntervalID, PriorPred, MetricKey) :-
    (   narrative_ontology:constraint_metric(IntervalID, MetricKey, _)
    ->  true  % Already exists
    ;   (   resolve_domain_prior(PriorPred, IntervalID, Value)
        ->  assertz(narrative_ontology:constraint_metric(IntervalID, MetricKey, Value)),
            format('  [BRIDGE] Derived metric ~w = ~w for ~w~n', [MetricKey, Value, IntervalID])
        ;   true  % No domain prior available
        )
    ).

resolve_domain_prior(base_extractiveness, C, V) :- catch(domain_priors:base_extractiveness(C, V), _, fail).
resolve_domain_prior(suppression_score, C, V) :- catch(domain_priors:suppression_score(C, V), _, fail).
resolve_domain_prior(theater_ratio, C, V) :- catch(domain_priors:theater_ratio(C, V), _, fail).

%% bridge_omega_variables(+IntervalID)
%  v3.4 testsets define omega_variable/5 in their own module namespace.
%  This bridges them into narrative_ontology:omega_variable/3.
bridge_omega_variables(IntervalID) :-
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_,_,_), defined)
    ->  forall(
            (   catch(IntervalID:omega_variable(OID, Question, _Measurement, _Resolution, _Conf), _, fail),
                \+ narrative_ontology:omega_variable(OID, _, _)
            ),
            (   assertz(narrative_ontology:omega_variable(OID, empirical, Question)),
                format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID, IntervalID])
            ))
    ;   true  % Module not loaded or no omega_variable/5
    ),
    % Also check for omega_variable/3 in the testset module
    (   current_module(IntervalID),
        predicate_property(IntervalID:omega_variable(_,_,_), defined)
    ->  forall(
            (   catch(IntervalID:omega_variable(OID3, Type3, Desc3), _, fail),
                \+ narrative_ontology:omega_variable(OID3, _, _)
            ),
            (   assertz(narrative_ontology:omega_variable(OID3, Type3, Desc3)),
                format('  [BRIDGE] Imported omega ~w from module ~w~n', [OID3, IntervalID])
            ))
    ;   true
    ).
