import streamlit as st
import subprocess
import tempfile
import os
import re
from google import genai

client = genai.Client(api_key="API_KEY")

response = client.models.generate_content(
    model="gemini-3-flash-preview", contents="What is Deferential Realism?"
)
print(response.text)

# --- HELPER: Linter Logic (Derived from structural_linter.py) ---
def lint_prolog(content):
    errors = []
    if not re.search(r':- module\(', content):
        errors.append("MISSING_MODULE: Prolog files must begin with :- module(id, []).")
    if "narrative_ontology:interval(" not in content:
        errors.append("MISSING_HOOK: Missing narrative_ontology:interval/3.")
    if "agent_power(individual_powerless)" not in content:
        errors.append("MISSING_PERSPECTIVE: Must include agent_power(individual_powerless).")
    if "agent_power(institutional)" not in content:
        errors.append("MISSING_PERSPECTIVE: Must include agent_power(institutional).")

    found_types = set(re.findall(r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)', content))
    if len(found_types) < 2:
        errors.append(f"INSUFFICIENT_VARIANCE: Need at least 2 different types across indices.")

    return errors

# --- STEP 1: Generation ---
def generate_prolog_scenario(user_input, prompt_text, template_text, linter_code):
    system_prompt = f"{prompt_text}\n\nTEMPLATE:\n{template_text}\n\nLINTER RULES:\n{linter_code}"

    response = client.chat.completions.create(
        model="gpt-4-turbo-preview",
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": f"Generate a valid .pl scenario for: {user_input}"}
        ],
        temperature=0.2
    )
    return response.choices[0].message.content

# --- STEP 2: Execution ---
def run_prolog_audit(scenario_pl):
    # This assumes 'prolog.txt' logic and dependencies are in the local directory
    with tempfile.NamedTemporaryFile(suffix=".pl", mode="w", delete=False) as tmp:
        tmp.write(scenario_pl)
        tmp_path = tmp.name

    try:
        # Command to run the scenario through SWI-Prolog
        # Adjust the goal ('run_scenario') based on your specific engine entry point
        cmd = ["swipl", "-q", "-s", tmp_path, "-g", "run_audit_logic", "-t", "halt"]
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
        return result.stdout if result.returncode == 0 else result.stderr
    finally:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)

# --- STEP 3: Synthesis ---
def generate_essay(scenario_pl, prolog_output, uke_w_protocol):
    system_prompt = f"You are a critical analyst applying the following writing protocol:\n{uke_w_protocol}"
    user_content = f"SCENARIO_CODE:\n{scenario_pl}\n\nPROLOG_AUDIT_OUTPUT:\n{prolog_output}"

    response = client.chat.completions.create(
        model="gpt-4-turbo-preview",
        messages=[
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_content}
        ],
        temperature=0.7
    )
    return response.choices[0].message.content

# --- Streamlit UI ---
st.title("DR-Audit Orchestrator")

user_idea = st.text_area("Enter scenario idea or paste raw data:")

if st.button("Run Audit"):
    if user_idea:
        with st.status("Processing Audit...", expanded=True) as status:
            # Load assets
            prompt = open("constraint_story_generation_prompt.md").read()
            template = open("constraint_story_template.pl").read()
            linter_text = open("structural_linter.py").read()
            uke_w = open("uke_w.md").read()

            st.write("Generating Prolog Scenario...")
            pl_code = generate_prolog_scenario(user_idea, prompt, template, linter_text)

            st.write("Linting...")
            errors = lint_prolog(pl_code)
            if errors:
                st.error(f"Linter Failed: {errors}")
                st.stop()

            st.write("Executing Logic Audit...")
            audit_results = run_prolog_audit(pl_code)

            st.write("Synthesizing Essay...")
            final_essay = generate_essay(pl_code, audit_results, uke_w)

            status.update(label="Audit Complete!", state="complete", expanded=False)

        st.markdown(final_essay)
    else:
        st.warning("Please provide an input.")
