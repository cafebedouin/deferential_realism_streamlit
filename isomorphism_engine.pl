:- module(isomorphism_engine, [
    find_isomorphism/3,
    cluster_by_signature/2,
    generate_cross_domain_index/1,
    find_high_risk_isomorphism/3
]).

:- use_module(structural_signatures).
:- use_module(narrative_ontology).
:- use_module(domain_priors).
:- use_module(config).

%% find_isomorphism(+ConstraintA, -ConstraintB, -SimilarityScore)
%  Calculates the structural similarity between two constraints based on 
%  their 7-point signature profile.
find_isomorphism(C1, C2, Score) :-
    C1 \= C2,
    structural_signatures:get_constraint_profile(C1, Profile1),
    structural_signatures:get_constraint_profile(C2, Profile2),
    calculate_profile_distance(Profile1, Profile2, Distance),
    Score is 1.0 - Distance,
    config:param(isomorphism_threshold, T),
    Score >= T.

%% calculate_profile_distance(+P1, +P2, -Distance)
%  Computes normalized Euclidean distance across the metric components 
%  of the structural profiles.
calculate_profile_distance(
    profile(A1, S1, R1, B1, Alt1, _, _),
    profile(A2, S2, R2, B2, Alt2, _, _),
    Distance
) :-
    % Normalized Beneficiary Delta (capped at 5 for scale)
    BN1 is min(B1, 5) / 5,
    BN2 is min(B2, 5) / 5,
    % Alternative parity (0 if same, 1 if different)
    (Alt1 == Alt2 -> AltD = 0 ; AltD = 1),
    
    D2 is (A1-A2)^2 + (S1-S2)^2 + (R1-R2)^2 + (BN1-BN2)^2 + (AltD * 0.5)^2,
    Distance is sqrt(D2) / 2.5. % Normalized to [0,1]

%% cluster_by_signature(+Signature, -Cluster)
%  Finds all constraints sharing a specific structural signature.
cluster_by_signature(Sig, Cluster) :-
    findall(C, structural_signatures:constraint_signature(C, Sig), Cluster).

%% generate_cross_domain_index(-Index)
%  The "Pattern Search" entry point.
generate_cross_domain_index(Index) :-
    findall(iso(C1, C2, S), (
        narrative_ontology:constraint_claim(C1, _),
        domain_priors:category_of(C1, Cat1),
        find_isomorphism(C1, C2, S),
        domain_priors:category_of(C2, Cat2),
        Cat1 \= Cat2 % Only return cross-domain matches
	    ), Index).

%% find_high_risk_isomorphism(-C1, -C2, -Score)
%  Filters isomorphisms specifically for "Snare" or "Tangled Rope" clusters.
find_high_risk_isomorphism(C1, C2, Score) :-
    find_isomorphism(C1, C2, Score),
    is_high_risk(C1),
    is_high_risk(C2).

%% is_high_risk(+Constraint)
%  Checks if a constraint is classified as a Snare or Tangled Rope 
%  from the default analytical perspective.
is_high_risk(C) :-
    drl_core:dr_type(C, Type),
    member(Type, [snare, tangled_rope]).
