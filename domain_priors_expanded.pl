:- module(domain_priors_expanded, [
    get_corpus_prior/3,
    category_corpus_profile/2,
    type_corpus_profile/2,
    subcategory_profile/2,
    default_extractiveness/2,
    default_suppression/2,
    default_resistance/2,
    infer_category_defaults/4
]).

/**
 * DOMAIN PRIORS EXPANSION - Auto-generated from corpus analysis
 * Generated from 617 constraints
 * Categories analyzed: 3
 *
 * This module provides corpus-derived defaults for domain priors.
 * Use these when a new domain lacks explicit priors.
 */

%% ============================================================================
%% 1. CATEGORY CORPUS PROFILES (From Corpus Averages)
%% ============================================================================
%% Format: category_corpus_profile(Category, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).

category_corpus_profile(narrative_history, [0.32, 0.465, 0.166, 0.245, 308]).  % dominant: mountain, enforcement_ratio: 0.44
category_corpus_profile(extractive_market, [0.801, 0.707, 0.078, 0.134, 306]).  % dominant: snare, enforcement_ratio: 0.82
category_corpus_profile(unknown_novel, [0.735, 0.825, 0.015, 0.025, 3]).  % dominant: tangled_rope, enforcement_ratio: 1.0

%% ============================================================================
%% 2. CONSTRAINT TYPE PROFILES (Mountain/Rope/Noose/Tangled Rope)
%% ============================================================================
%% Format: type_corpus_profile(Type, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).

type_corpus_profile(mountain, [0.367, 0.452, 0.237, 0.256, 149]).
type_corpus_profile(snare, [0.655, 0.682, 0.218, 0.166, 89]).
type_corpus_profile(rope, [0.372, 0.462, 0.231, 0.26, 78]).
type_corpus_profile(tangled_rope, [0.529, 0.587, 0.233, 0.187, 47]).
type_corpus_profile(piton, [0.38, 0.16, 0.246, 0.049, 5]).

%% ============================================================================
%% 3. SUBCATEGORY PROFILES (Category + Type combinations)
%% ============================================================================
%% Format: subcategory_profile(SubcatName, [Parent, Type, AvgExtract, AvgSuppress, Count]).

subcategory_profile(narrative_history_mountain, ['narrative_history', 'mountain', 0.273, 0.401, 120]).
subcategory_profile(narrative_history_rope, ['narrative_history', 'rope', 0.296, 0.413, 65]).
subcategory_profile(extractive_market_snare, ['extractive_market', 'snare', 0.804, 0.749, 54]).
subcategory_profile(narrative_history_snare, ['narrative_history', 'snare', 0.418, 0.576, 34]).
subcategory_profile(extractive_market_mountain, ['extractive_market', 'mountain', 0.759, 0.662, 29]).
subcategory_profile(narrative_history_tangled_rope, ['narrative_history', 'tangled_rope', 0.325, 0.502, 24]).
subcategory_profile(extractive_market_tangled_rope, ['extractive_market', 'tangled_rope', 0.743, 0.662, 21]).
subcategory_profile(extractive_market_rope, ['extractive_market', 'rope', 0.752, 0.704, 13]).
subcategory_profile(narrative_history_piton, ['narrative_history', 'piton', 0.275, 0.15, 4]).

%% ============================================================================
%% 4. DEFAULT VALUE INFERENCE PREDICATES
%% ============================================================================

%% default_extractiveness(+Category, -Value)
%% Returns corpus-derived default extractiveness for a category.
default_extractiveness(Category, Value) :-
    category_corpus_profile(Category, [Value|_]), !.
default_extractiveness(_, 0.5).  % Neutral fallback

%% default_suppression(+Category, -Value)
%% Returns corpus-derived default suppression for a category.
default_suppression(Category, Value) :-
    category_corpus_profile(Category, [_, Value|_]), !.
default_suppression(_, 0.5).  % Neutral fallback

%% default_resistance(+Category, -Value)
%% Infers resistance from extractiveness (inverse correlation).
default_resistance(Category, Value) :-
    default_extractiveness(Category, Ext),
    Value is max(0.1, min(0.9, 1.0 - Ext * 0.5)), !.
default_resistance(_, 0.5).

%% infer_category_defaults(+Category, -Extractiveness, -Suppression, -Resistance)
%% Unified predicate to get all defaults for a category.
infer_category_defaults(Category, Ext, Sup, Res) :-
    default_extractiveness(Category, Ext),
    default_suppression(Category, Sup),
    default_resistance(Category, Res).

%% get_corpus_prior(+ID, +Metric, -Value)
%% Retrieves corpus-derived prior by constraint type.
get_corpus_prior(ID, extractiveness, Value) :-
    narrative_ontology:constraint_claim(ID, Type),
    type_corpus_profile(Type, [Value|_]), !.
get_corpus_prior(ID, suppression, Value) :-
    narrative_ontology:constraint_claim(ID, Type),
    type_corpus_profile(Type, [_, Value|_]), !.
get_corpus_prior(_, _, 0.5).  % Neutral fallback

%% ============================================================================
%% 5. RECOMMENDED CATEGORY PROFILE VECTORS
%% ============================================================================
%% These are corpus-calibrated replacements for the original category_profile/2.
%% Format: [accessibility_collapse, stakes_inflation, suppression, resistance]

% recommended_profile(narrative_history, [0.78, 0.38, 0.46, 0.81]).  % N=308, dominant=mountain
% recommended_profile(extractive_market, [0.59, 0.96, 0.71, 0.52]).  % N=306, dominant=snare

%% ============================================================================
%% END OF AUTO-GENERATED PRIORS
%% ============================================================================