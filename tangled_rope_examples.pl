% ============================================================================ 
% REFERENCE IMPLEMENTATIONS: Tangled Rope Category 
% ============================================================================ 
% Purpose: Canonical examples validated by corpus analysis 
% Added: January 2026 (based on empirical validation of 467 constraints) 
% Empirical finding: 168/467 constraints (36%) show hybrid coordination/extraction pattern 
% 
% NOTE: This file is a collection of examples and not a standard constraint story. 
% ============================================================================ 

:- module(tangled_rope_examples, []). 

:- use_module(constraint_indexing). 
:- use_module(domain_priors). 
:- use_module(narrative_ontology). 

% Multifile declarations for predicates defined across test files 
:- multifile 
    domain_priors:base_extractiveness/2, 
    domain_priors:suppression_score/2, 
    domain_priors:requires_active_enforcement/1, 
    constraint_indexing:constraint_classification/3, 
    constraint_beneficiary/2, 
    constraint_victim/2. 

% Discontiguous declarations for predicates defined in multiple places 
:- discontiguous 
    domain_priors:base_extractiveness/2, 
    domain_priors:suppression_score/2, 
    domain_priors:requires_active_enforcement/1, 
    constraint_beneficiary/2, 
    constraint_victim/2. 

/* ========================================================================== 
   EXAMPLE 1: Carbon Credit Trading (Canonical Tangled Rope) 
   ========================================================================== */ 

/** 
 * CANONICAL TANGLED ROPE EXAMPLE 
 * 
 * Coordination function (ROPE properties): 
 * - Creates price signal for emissions reduction 
 * - Enables market-based climate action 
 * - Coordinates international climate policy 
 * 
 * Extractive function (NOOSE properties): 
 * - Financial intermediation extracts from real economy 
 * - Offset fraud and greenwashing enable extraction 
 * - Compliance costs hit small businesses harder 
 * 
 * Why tangled_rope classification matters: 
 * - "Just remove it" → loses coordination (price signal) 
 * - "Just maintain it" → perpetuates extraction 
 * - Reform path: Preserve price mechanism, reduce financial extraction 
 * 
 * Empirical metrics from corpus analysis: 
 * - Extraction: 0.55 (mid-range) 
 * - Suppression: 0.60 (requires enforcement) 
 * - Has both beneficiaries AND victims 
 */ 

% Core Constraint Declaration 
narrative_ontology:interval(carbon_market_era, 2000, 2026). 
narrative_ontology:constraint_claim(carbon_credit_trading_canonical, tangled_rope). 

% Base Metrics 
domain_priors:base_extractiveness(carbon_credit_trading_canonical, 0.55). 
domain_priors:suppression_score(carbon_credit_trading_canonical, 0.60). 
domain_priors:requires_active_enforcement(carbon_credit_trading_canonical). 

% Metrics for Executive Summary 
narrative_ontology:constraint_metric(carbon_credit_trading_canonical, extractiveness, 0.55). 
narrative_ontology:constraint_metric(carbon_credit_trading_canonical, suppression_requirement, 0.60). 

% Beneficiaries & Victims (demonstrates hybridity) 
constraint_beneficiary(carbon_credit_trading_canonical, multinational_corporations). 
constraint_beneficiary(carbon_credit_trading_canonical, financial_sector). 
constraint_beneficiary(carbon_credit_trading_canonical, climate_policy_coordination). 
constraint_victim(carbon_credit_trading_canonical, small_businesses). 
constraint_victim(carbon_credit_trading_canonical, offset_sellers). 
constraint_victim(carbon_credit_trading_canonical, greenwashing_targets). 

% Index-dependent classifications 
constraint_indexing:constraint_classification( 
    carbon_credit_trading_canonical, 
    rope, 
    context( 
        agent_power(institutional), 
        time_horizon(generational), 
        exit_options(mobile), 
        spatial_scope(global) 
    ) 
) :- !. 

constraint_indexing:constraint_classification( 
    carbon_credit_trading_canonical, 
    tangled_rope, 
    context( 
        agent_power(analytical), 
        time_horizon(historical), 
        exit_options(analytical), 
        spatial_scope(global) 
    ) 
) :- !. 

constraint_indexing:constraint_classification( 
    carbon_credit_trading_canonical, 
    snare, 
    context( 
        agent_power(individual_moderate), 
        time_horizon(biographical), 
        exit_options(constrained), 
        spatial_scope(national) 
    ) 
) :- !. 

constraint_indexing:constraint_classification( 
    carbon_credit_trading_canonical, 
    snare, 
    context( 
        agent_power(individual_powerless), 
        time_horizon(biographical), 
        exit_options(trapped), 
        spatial_scope(local) 
    ) 
) :- !. 


/* ========================================================================== 
   EXAMPLE 2: Academic Tenure System (From Existing Corpus) 
   ========================================================================== */ 

/** 
 * ACADEMIC TENURE AS TANGLED ROPE 
 * 
 * Coordination function (ROPE properties): 
 * - Protects academic freedom 
 * - Enables long-term research projects 
 * - Coordinates faculty stability 
 * 
 * Extractive function (NOOSE properties): 
 * - Extracts hyper-productivity from pre-tenure faculty 
 * - Excludes adjuncts from security 
 * - Concentrates benefits in senior faculty 
 * 
 * Reclassified from corpus analysis: 
 * - Previously: rope (claimed type) 
 * - Empirical: extraction 0.75, suppression 0.60 
 * - New classification: tangled_rope 
 */ 

% Note: academic_tenure_system.pl already exists in testsets/ 
% This would be updated to claim tangled_rope instead of rope 

/* ========================================================================== 
   EXAMPLE 3: Platform Network Effects (Generic Template) 
   ========================================================================== */ 

/** 
 * PLATFORM MONOPOLIES AS TANGLED ROPE 
 * 
 * Coordination function (ROPE properties): 
 * - Coordinates user connections (network effects) 
 * - Enables communication and commerce 
 * - Provides standardized interface 
 * 
 * Extractive function (NOOSE properties): 
 * - Monopoly power extracts through pricing 
 * - Data harvesting and surveillance 
 * - Lock-in effects trap users 
 * 
 * Reform strategy: 
 * - Preserve network coordination (interoperability) 
 * - Cut extraction (data portability, competition) 
 */ 

narrative_ontology:constraint_claim(platform_network_monopoly_canonical, tangled_rope). 

domain_priors:base_extractiveness(platform_network_monopoly_canonical, 0.65). 
domain_priors:suppression_score(platform_network_monopoly_canonical, 0.55). 
domain_priors:requires_active_enforcement(platform_network_monopoly_canonical). 

narrative_ontology:constraint_metric(platform_network_monopoly_canonical, extractiveness, 0.65). 
narrative_ontology:constraint_metric(platform_network_monopoly_canonical, suppression_requirement, 0.55). 

constraint_beneficiary(platform_network_monopoly_canonical, platform_owners). 
constraint_beneficiary(platform_network_monopoly_canonical, advertisers). 
constraint_beneficiary(platform_network_monopoly_canonical, network_coordination). 
constraint_victim(platform_network_monopoly_canonical, users_privacy). 
constraint_victim(platform_network_monopoly_canonical, small_competitors). 
constraint_victim(platform_network_monopoly_canonical, innovation_potential). 

/* ========================================================================== 
   USAGE NOTES 
   ========================================================================== */ 

/** 
 * HOW TO IDENTIFY TANGLED ROPE CONSTRAINTS 
 * 
 * 1. Check metrics: 
 *    - base_extractiveness: 0.40-0.90 
 *    - suppression_score: >= 0.50 
 *    - requires_active_enforcement: true 
 * 
 * 2. Verify hybridity: 
 *    - Has coordination function: multiple beneficiaries or network effects 
 *    - Has asymmetric extraction: has victims or concentrated benefits 
 * 
 * 3. Test strategic implications: 
 *    - "Just remove it" → loses something valuable (coordination) 
 *    - "Just maintain it" → perpetuates harm (extraction) 
 *    - Reform approach → surgical separation possible 
 * 
 * EXAMPLES FROM CORPUS REQUIRING RECLASSIFICATION: 
 * - academic_tenure_system.pl (claimed: rope, should be: tangled_rope) 
 * - carbon_credit_markets_2026.pl (claimed: rope, should be: tangled_rope) 
 * - regulatory_capture.pl (claimed: snare, should be: tangled_rope) 
 * - gig_economy_algorithmic_managment.pl (check metrics) 
 * 
 * GREP SEARCH TO FIND CANDIDATES: 
 * Find files with extraction 0.40-0.90 AND suppression > 0.50: 
 * 
 * grep -l "base_extractiveness.*0\.[4-9]" testsets/*.pl | \ 
 * xargs grep -l "suppression_score.*0\.[5-9]" 
 */
*/