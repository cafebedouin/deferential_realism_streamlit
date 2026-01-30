import streamlit as st
import subprocess
import tempfile
import os
import re
import streamlit as st
import google.generativeai as genai

# 1. Retrieve the key from secrets
api_key = st.secrets["GEMINI_API_KEY"]

# 2. Configure the library
genai.configure(api_key=api_key)

# 3. Initialize the model
model = genai.GenerativeModel('gemini-1.5-flash')

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

    # Use the 'model' you initialized at the top of the file
    response = model.generate_content(f"SYSTEM: {system_prompt}\n\nUSER: Generate a valid .pl scenario for: {user_input}")
    return response.text

# --- STEP 2: Execution ---
def run_prolog_audit(scenario_pl):
    # 1. Extract the interval ID (the unique name of the scenario)
    # The engine needs this to know WHICH interval to audit.
    # Looking for: narrative_ontology:interval(portugal_government_stability_ad, ...)
    match = re.search(r"narrative_ontology:interval\(([^,]+),", scenario_pl)
    interval_id = match.group(1) if match else "generated_scenario"

    # 2. Save the AI-generated scenario to a temporary file
    with tempfile.NamedTemporaryFile(suffix=".pl", mode="w", delete=False) as tmp:
        tmp.write(scenario_pl)
        tmp_path = tmp.name

    try:
        # 3. Construct the goal. Note the single quotes for the path and ID.
        # We load 'stack.pl' first, then run 'run_scenario'.
        goal = f"run_scenario('{tmp_path}', '{interval_id}')."

        cmd = [
            "swipl",
            "-q",                    # Quiet mode
            "-s", "stack.pl",         # Load your main engine
            "-g", goal,               # Execute the specific audit goal
            "-t", "halt"              # Halt after finishing
        ]

        # 4. Run the process
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)

        if result.returncode == 0:
            return result.stdout
        else:
            return f"Prolog Error:\n{result.stderr}\nOutput so far:\n{result.stdout}"

    finally:
        # Cleanup the temp file
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
