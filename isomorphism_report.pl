:- module(isomorphism_report, [
    generate_isomorphism_report/0,
    report_isomorphism_to_file/1
]).

:- use_module(isomorphism_engine).
:- use_module(narrative_ontology).
:- use_module(domain_priors).

%% generate_isomorphism_report
%  Scans the current Knowledge Base and prints a Markdown-formatted 
%  table of structural isomorphisms across different domains.
generate_isomorphism_report :-
    format('~n# CROSS-DOMAIN ISOMORPHISM INDEX~n'),
    format('| Domain A (Technical/Formal) | Domain B (Social/Institutional) | Similarity | Structural Logic |~n'),
    format('| :--- | :--- | :--- | :--- |~n'),
    
    % Generate the index using the engine
    isomorphism_engine:generate_cross_domain_index(Index),
    
    % Filter and sort by score for the report
    sort(3, @>=, Index, SortedIndex),
    
    forall(member(iso(C1, C2, Score), SortedIndex),
           (   % Only report if C1 is "technical" and C2 is "social" to highlight the thesis
               domain_priors:category_of(C1, Cat1),
               domain_priors:category_of(C2, Cat2),
               (is_technical(Cat1), is_social(Cat2))
           ->  get_logic_explanation(C1, C2, Explanation),
               format('| ~w | ~w | ~2f | ~w |~n', [C1, C2, Score, Explanation])
           ;   true
           )).

%% is_technical(+Category)
%  Helper to identify formal/technical domains[cite: 103, 104, 115].
is_technical(physical_natural).
is_technical(formal_logic).
is_technical(extractive_market). % Often technical in your datasets

%% is_social(+Category)
%  Helper to identify social/historical domains[cite: 106, 107, 128].
is_social(narrative_history).
is_social(statutory_formal).
is_social(election_cycle).

%% get_logic_explanation(+C1, +C2, -Explanation)
%  Summarizes why the two constraints are twins[cite: 457, 458, 459].
get_logic_explanation(C1, C2, Explanation) :-
    structural_signatures:constraint_signature(C1, Sig),
    structural_signatures:explain_signature(C1, Sig, BaseExpl),
    % Truncate or simplify the explanation for table format
    format(atom(Explanation), 'Both function as ~w.', [Sig]).

%% generate_high_risk_index
%  Generates a Markdown report limited to high-risk structural twins.
generate_high_risk_index :-
    format('~n# HIGH-RISK STRUCTURAL ISOMORPHISM AUDIT~n'),
    format('| Risk Cluster (Formal) | Risk Cluster (Social) | Similarity | Risk Type |~n'),
    format('| :--- | :--- | :--- | :--- |~n'),
    
    findall(iso(C1, C2, S), (
        domain_priors:category_of(C1, Cat1),
        find_high_risk_isomorphism(C1, C2, S),
        domain_priors:category_of(C2, Cat2),
        Cat1 \= Cat2,
        is_technical(Cat1) % Source is a technical system
    ), HighRiskIndex),
    
    sort(3, @>=, HighRiskIndex, Sorted),
    
    forall(member(iso(C1, C2, Score), Sorted),
           (   drl_core:dr_type(C1, Type),
               format('| ~w | ~w | ~2f | ~w |~n', [C1, C2, Score, Type])
           )).
