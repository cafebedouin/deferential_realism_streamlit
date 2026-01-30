import os
import re
import sys

def lint_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    errors = []

    # 1. CORE STRUCTURE & v4.0 INTEGRATION HOOKS
    if not re.search(r':- module\(', content):
        errors.append("MISSING_MODULE: Prolog files must begin with :- module(id, []).")

    if "narrative_ontology:interval(" not in content:
        errors.append("MISSING_HOOK: Missing narrative_ontology:interval/3.")

    # Updated to check for v4.0 indexed classification
    if "constraint_classification(" not in content:
        errors.append("OUTDATED_HOOK: v4.0 requires constraint_indexing:constraint_classification/3.")

    # 2. PERSPECTIVAL MINIMUMS
    if "agent_power(individual_powerless)" not in content:
        errors.append("MISSING_PERSPECTIVE: Must include agent_power(individual_powerless).")

    if "agent_power(institutional)" not in content:
        errors.append("MISSING_PERSPECTIVE: Must include agent_power(institutional).")

    # 3. TYPE VARIANCE CHECK (Updated for v3.4 Categories)
    found_types = set(re.findall(r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)', content, re.DOTALL))
    if len(found_types) < 2:
        errors.append(f"INSUFFICIENT_VARIANCE: Found {list(found_types)}. Need at least 2 different types across indices.")

    # 4. DEPRECATED TERMINOLOGY
    if "noose" in content.lower():
        errors.append("DEPRECATED_TERM: 'noose' has been replaced by 'snare' in v3.4.")

    # 5. MANDATROPHY & OMEGA VALIDATION
    ext_match = re.search(r'base_extractiveness\(.*,\s*([\d\.]+)\)', content)
    if ext_match:
        val = float(ext_match.group(1))
        if val > 0.7:
            if "is_mandatrophy_resolved" not in content and "[RESOLVED MANDATROPHY]" not in content:
                errors.append(f"UNRESOLVED_MANDATROPHY: Extraction {val} requires resolution hook.")
        if val > 0.46 and "omega_variable(" not in content:
            errors.append("MISSING_OMEGA: High-extraction constraints (> 0.46) require omega_variable/5.")

    return errors
