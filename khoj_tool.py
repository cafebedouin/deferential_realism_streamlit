def dr_audit_tool(input_text: str) -> str:
    """
    Performs a Deferential Realism audit on the provided text.
    Returns a synthesized essay based on Prolog logic verification.
    """
    # 1. Load Files
    prompt = open("constraint_story_generation_prompt.md").read()
    template = open("constraint_story_template.pl").read()
    linter_text = open("structural_linter.py").read()
    uke_w = open("uke_w.md").read()

    # 2. Pipeline Execution
    pl_code = generate_prolog_scenario(input_text, prompt, template, linter_text)

    errors = lint_prolog(pl_code)
    if errors:
        return f"Structural Error in Model Generation: {', '.join(errors)}"

    audit_output = run_prolog_audit(pl_code)
    essay = generate_essay(pl_code, audit_output, uke_w)

    return essay
