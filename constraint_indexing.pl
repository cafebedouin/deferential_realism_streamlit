% CONSTRAINT CLASSIFICATION
% ============================================================================
% This module implements context-indexed constraint classification.
% Every Mountain/Rope/Snare judgment is relative to WHO, WHEN, WHERE, HOW.
%
% Integration: Load after domain_priors.pl, before report_generator.pl
% ============================================================================

:- module(constraint_indexing, [
    % Core API
    constraint_classification/3,    % New indexed classification
    constraint_claim_indexed/2,     % Backward compatible wrapper (renamed to avoid collision)
    
    % Context builders
    default_context/1,
    valid_context/1,
    discover_my_context/1,
    
    % Analysis utilities
    multi_index_report/1,
    compare_perspectives/2,
    perspective_gap/2,
    
    % Helper predicates (exposed for testing)
    effective_immutability/3,
    effective_immutability_for_context/2,
    extractiveness_for_agent/3,
    power_modifier/2,
    
    % Index predicates
    agent_power/1,
    time_horizon/1,
    exit_options/1,
    spatial_scope/1
]).

:- multifile constraint_classification/3.
:- dynamic constraint_classification/3.

% Required modules
:- use_module(drl_core, [base_extractiveness/2, suppression_score/2]).
:- use_module(config).

% ============================================================================
% INDEX ONTOLOGY
% ============================================================================

% ----------------------------------------------------------------------------
% Agent Power Levels - WHO is evaluating?
% ----------------------------------------------------------------------------

agent_power(powerless).    % Serf, prisoner, child
agent_power(moderate).     % Middle class, citizen
agent_power(powerful).     % Wealthy, connected
agent_power(organized).    % Union, movement
agent_power(institutional).           % State, corporation, church
agent_power(analytical).              % Historian, philosopher (meta-level)

% ----------------------------------------------------------------------------
% Time Horizons - WHEN/how long?
% ----------------------------------------------------------------------------

time_horizon(immediate).              % 1 year
time_horizon(biographical).           % 20-50 years (lifetime)
time_horizon(generational).          % 50-100 years
time_horizon(historical).            % 100-500 years
time_horizon(civilizational).        % 500+ years

% ----------------------------------------------------------------------------
% Exit Options - WHERE can you go?
% ----------------------------------------------------------------------------

exit_options(trapped).               % No physical/conceptual exit
exit_options(constrained).           % Exit costly but possible
exit_options(mobile).                % Can leave, alternatives visible
exit_options(arbitrage).             % Can play systems against each other
exit_options(analytical).            % Not constrained (observer stance)

% ----------------------------------------------------------------------------
% Spatial Scope - WHERE does it operate?
% ----------------------------------------------------------------------------

spatial_scope(local).                % Village, neighborhood
spatial_scope(regional).             % Province, state
spatial_scope(national).             % Country
spatial_scope(continental).          % Europe, Asia, etc
spatial_scope(global).               % Worldwide
spatial_scope(universal).            % Universal

% ============================================================================
% CONTEXT STRUCTURE
% ============================================================================

% ----------------------------------------------------------------------------
% Context Validation
% ----------------------------------------------------------------------------

% context(+AgentPower, +TimeHorizon, +ExitOptions, +SpatialScope)
% Represents the indexical parameters for classification

valid_context(context(
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S)
)) :-
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S).

% ----------------------------------------------------------------------------
% Default Context - Analytical "God's Eye View"
% ----------------------------------------------------------------------------

default_context(context(
    agent_power(analytical),
    time_horizon(civilizational),
    exit_options(analytical),
    spatial_scope(global)
)).

% ============================================================================
% BACKWARD COMPATIBILITY LAYER
% ============================================================================

% Indexed API wrapper - defaults to analytical perspective
% Renamed to avoid collision with narrative_ontology:constraint_claim/2
constraint_claim_indexed(Constraint, Type) :-
    default_context(Ctx),
    constraint_classification(Constraint, Type, Ctx).

% ============================================================================
% HELPER PREDICATES - EFFECTIVE IMMUTABILITY
% ============================================================================

% Can this be changed given time horizon and exit options?
% Returns: mountain (unchangeable) or rope (changeable)

effective_immutability(immediate, trapped, mountain).
effective_immutability(immediate, constrained, mountain).
effective_immutability(immediate, mobile, rope).
effective_immutability(immediate, arbitrage, rope).

effective_immutability(biographical, trapped, mountain).
effective_immutability(biographical, constrained, mountain).
effective_immutability(biographical, mobile, rope).
effective_immutability(biographical, arbitrage, rope).

effective_immutability(generational, trapped, mountain).
effective_immutability(generational, constrained, rope).
effective_immutability(generational, mobile, rope).
effective_immutability(generational, arbitrage, rope).

effective_immutability(historical, _, rope).
effective_immutability(civilizational, _, rope).

% Wrapper that takes full context
effective_immutability_for_context(
    context(_, time_horizon(T), exit_options(E), _),
    Perception
) :-
    effective_immutability(T, E, Perception).

% ============================================================================
% HELPER PREDICATES - AGENT-RELATIVE EXTRACTIVENESS
% ============================================================================

% ----------------------------------------------------------------------------
% Power Modifiers
% ----------------------------------------------------------------------------
% More powerful agents experience less extraction from same constraint
% Negative modifier = net beneficiary
% Determines how much of the base extraction is "felt" by the agent.
% Lower numbers = higher benefit/protection from the constraint.

power_modifier(powerless,     Modifier) :- config:param(power_modifier_powerless, Modifier).
power_modifier(moderate,      Modifier) :- config:param(power_modifier_moderate, Modifier).
power_modifier(powerful,      Modifier) :- config:param(power_modifier_powerful, Modifier).
power_modifier(organized,     Modifier) :- config:param(power_modifier_organized, Modifier).
power_modifier(institutional, Modifier) :- config:param(power_modifier_institutional, Modifier).
power_modifier(analytical,    Modifier) :- config:param(power_modifier_analytical, Modifier).

% ----------------------------------------------------------------------------
% Calculate Extractiveness for Specific Agent
% ----------------------------------------------------------------------------

% Formula: Effective Extraction = Base Extraction * Power Modifier
extractiveness_for_agent(Constraint, Context, Score) :-
    Context = context(agent_power(Power), _, _, _),
    base_extractiveness(Constraint, BaseScore),
    power_modifier(Power, Modifier),
    Score is BaseScore * Modifier.

% ============================================================================
% CANONICAL CLASSIFICATION PREDICATE
% ============================================================================

% constraint_classification(+ConstraintID, ?Type, +Context)
% This is the ONLY predicate that does real classification work.
% All other predicates delegate to this.
%
% Specific constraint rules should be added in separate files or
% in domain_priors.pl using this predicate.

% Example template (actual rules in domain_priors.pl or constraint_instances.pl):
% constraint_classification(some_constraint, Type, Context) :-
%     valid_context(Context),
%     ... classification logic using Context parameters ...

% ============================================================================
% QUERY UTILITIES
% ============================================================================
% ----------------------------------------------------------------------------
% Interactive Context Discovery
% ----------------------------------------------------------------------------

discover_my_context(Context) :-
    writeln(''),
    writeln('=== CONTEXT DISCOVERY ==='),
    writeln(''),
    writeln('What is your power level?'),
    writeln('  1. Powerless (prisoner, serf, child)'),
    writeln('  2. Moderate (middle class, citizen)'),
    writeln('  3. Powerful (wealthy, politically connected)'),
    writeln('  4. Collective (union, movement)'),
    writeln('  5. Institutional (state, corporation)'),
    writeln('  6. Analytical (observer, researcher)'),
    read(PowerChoice),
    map_power(PowerChoice, Power),
    
    writeln(''),
    writeln('What time horizon are you considering?'),
    writeln('  1. Immediate (1 year)'),
    writeln('  2. Biographical (my lifetime)'),
    writeln('  3. Generational (my children)'),
    writeln('  4. Historical (centuries)'),
    writeln('  5. Civilizational (millennia)'),
    read(TimeChoice),
    map_time(TimeChoice, Time),
    
    writeln(''),
    writeln('What exit options do you have?'),
    writeln('  1. Trapped (no exit visible)'),
    writeln('  2. Constrained (exit costly)'),
    writeln('  3. Mobile (can leave)'),
    writeln('  4. Arbitrage (can play systems)'),
    writeln('  5. Analytical (observer)'),
    read(ExitChoice),
    map_exit(ExitChoice, Exit),
    
    writeln(''),
    writeln('What spatial scope?'),
    writeln('  1. Local (village/neighborhood)'),
    writeln('  2. Regional (state/province)'),
    writeln('  3. National (country)'),
    writeln('  4. Continental (Europe, Asia, etc)'),
    writeln('  5. Global (worldwide)'),
    read(ScopeChoice),
    map_scope(ScopeChoice, Scope),
    
    Context = context(
        agent_power(Power),
        time_horizon(Time),
        exit_options(Exit),
        spatial_scope(Scope)
    ),
    writeln(''),
    format('Your context: ~w~n', [Context]).

% Mapping predicates for user input
map_power(1, individual_powerless).
map_power(2, individual_moderate).
map_power(3, individual_powerful).
map_power(4, collective_organized).
map_power(5, institutional).
map_power(6, analytical).

map_time(1, immediate).
map_time(2, biographical).
map_time(3, generational).
map_time(4, historical).
map_time(5, civilizational).

map_exit(1, trapped).
map_exit(2, constrained).
map_exit(3, mobile).
map_exit(4, arbitrage).
map_exit(5, analytical).

map_scope(1, local).
map_scope(2, regional).
map_scope(3, national).
map_scope(4, continental).
map_scope(5, global).

% ----------------------------------------------------------------------------
% Multi-Index Analysis
% ----------------------------------------------------------------------------

% Show how constraint classifies from different perspectives
multi_index_report(Constraint) :-
    writeln(''),
    writeln('=== MULTI-INDEX ANALYSIS ==='),
    format('Constraint: ~w~n~n', [Constraint]),
    
    findall(
        result(Type, Power, Time, Exit, Scope),
        constraint_classification(Constraint, Type, 
            context(agent_power(Power), time_horizon(Time),
                   exit_options(Exit), spatial_scope(Scope))),
        Results
    ),
    
    (Results = [] ->
        writeln('No classifications found.')
    ;
        format_multi_index_results(Results)
    ).

format_multi_index_results([]).
format_multi_index_results([result(Type, Power, Time, Exit, Scope)|Rest]) :-
    format('~w: power=~w, time=~w, exit=~w, scope=~w~n',
           [Type, Power, Time, Exit, Scope]),
    format_multi_index_results(Rest).

% ----------------------------------------------------------------------------
% Perspective Comparison
% ----------------------------------------------------------------------------

% Compare YOUR view vs ANALYTICAL view
compare_perspectives(Constraint, MyContext) :-
    writeln(''),
    writeln('=== PERSPECTIVE COMPARISON ==='),
    format('Constraint: ~w~n~n', [Constraint]),
    
    default_context(AnalyticalContext),
    
    (constraint_classification(Constraint, MyType, MyContext) ->
        format('From YOUR perspective (~w): ~w~n', [MyContext, MyType])
    ;
        writeln('No classification from your perspective')
    ),
    
    writeln(''),
    
    (constraint_classification(Constraint, AnalyticalType, AnalyticalContext) ->
        format('From ANALYTICAL perspective: ~w~n', [AnalyticalType])
    ;
        writeln('No analytical classification')
    ),
    
    writeln(''),
    
    (var(MyType) ; var(AnalyticalType) ->
        true
    ; MyType = AnalyticalType ->
        writeln('→ Perspectives AGREE')
    ;
        writeln('→ Perspectives DISAGREE - this is a site of political conflict')
    ).

% ----------------------------------------------------------------------------
% Perspective Gap Detection
% ----------------------------------------------------------------------------

% Find political flashpoints - where different perspectives classify differently
perspective_gap(Constraint, Gap) :-
    constraint_classification(Constraint, Type1, Context1),
    constraint_classification(Constraint, Type2, Context2),
    Type1 \= Type2,
    Context1 \= Context2,
    Gap = gap(Type1-Context1, Type2-Context2).

% ============================================================================
% INTEGRATION NOTES
% ============================================================================

% To integrate this module:
%
% 1. In drl_core.pl, add after domain_priors:
%    :- use_module(constraint_indexing).
%
% 2. In domain_priors.pl, add base_extractiveness/2 facts:
%    base_extractiveness(constraint_id, score).
%    % Score range: 0.0 (no extraction) to 1.0 (full extraction)
%
% 3. Add specific constraint rules using constraint_classification/3
%    See constraint_instances.pl for examples
%
% 4. Update report_generator.pl to optionally use indexed classification:
%    - Default: Use constraint_claim/2 (analytical view)
%    - Advanced: Accept context parameter from user
%
% 5. Test backward compatibility:
%    - Old code using constraint_claim/2 should work unchanged
%    - New code can use constraint_classification/3 explicitly
