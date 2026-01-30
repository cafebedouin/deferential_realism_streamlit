:- module(signature_mapper, [
    map_custom_pillar/3
]).

:- use_module(narrative_ontology).
:- use_module(structural_signatures).
:- use_module(config).

%% map_custom_pillar(+Constraint, +CustomType, -StandardType)
%  Maps non-standard terminology to v3.1 standard pillars based on metrics.
map_custom_pillar(_C, CustomType, StandardType) :-
    % If already standard, do nothing
    member(CustomType, [mountain, rope, tangled_rope, snare, scaffold, piton]),
    StandardType = CustomType, !.

map_custom_pillar(C, _CustomType, StandardType) :-
    % Analyze structural signature based on current metrics
    structural_signatures:constraint_signature(C, Signature),
    resolve_mapping(Signature, StandardType).

% Natural Laws and high-stability invariants are Mountains
resolve_mapping(natural_law, mountain).
% Successful voluntary coordination (like decentralized_governance) are Ropes
resolve_mapping(coordination_scaffold, rope).
% A scaffold that has decayed into a liability is a Piton
resolve_mapping(piton_signature, piton).
% Asymmetric, enforced, or extractive rules are Nooses
resolve_mapping(constructed_constraint, snare).
% Fallback for ambiguous data
resolve_mapping(ambiguous, rope).
