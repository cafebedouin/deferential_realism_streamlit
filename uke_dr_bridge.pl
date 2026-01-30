:- module(uke_dr_bridge, [
    uke_status/3
]).

:- use_module(drl_core).
:- use_module(narrative_ontology).
:- use_module(constraint_bridge).
:- use_module(config).

% Map Feasibility + DRL Type to UKE Status
uke_status(RecID, Status, Reasons) :-
    narrative_ontology:recommendation(RecID, _), % CRITICAL: Verify RecID is actually a recommendation
    narrative_ontology:affects_constraint(RecID, C),
    drl_core:dr_type(C, Type),
    constraint_bridge:recommendation_feasibility(RecID, Feas, Vetoes),
    determine_status(C, Type, Feas, Vetoes, Status, Reasons). % Passed C here

% --- Status Routing ---

determine_status(_, mountain, _, _, fantasy, 
    ['Recommendation attempts to modify a natural constraint (Mountain).']).

% Fixed: Uses C to check load-bearing status
determine_status(C, snare, _, _, blocked, 
    ['CRITICAL: Load-bearing Snare removal attempted without Scaffold.']) :-
    is_load_bearing(C),
    \+ narrative_ontology:entity(_, scaffold).

determine_status(_, Type, viable, [], viable, 
    ['No structural or political vetoes detected.']) :-
    member(Type, [rope, snare, piton]).

determine_status(_, tangled_rope, _, _, aspirational, 
    ['System is Tangled: Extraction is rising. Coordination remains, but Reform is required.']).

% Fallback for unclassified constraints
determine_status(_, unknown, _, _, investigate, 
    ['Structural audit incomplete: Constraint metrics do not match known types.']).

determine_status(_, _, blocked_by_veto, Vetoes, blocked, [Msg]) :-
    format(string(Msg), 'Vetoed by: ~w', [Vetoes]).

% Helper
is_load_bearing(C) :- 
    drl_core:dr_type(C, snare),
    config:param(snare_load_bearing_threshold, T),
    narrative_ontology:constraint_metric(C, extractiveness, X), 
    X > T.
