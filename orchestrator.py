import streamlit as st
import subprocess
import tempfile
import os
import re
from google import genai

# --- INITIALIZATION ---
if "GEMINI_API_KEY" in st.secrets:
    client = genai.Client(api_key=st.secrets["GEMINI_API_KEY"])
else:
    st.error("Missing GEMINI_API_KEY in Streamlit Secrets.")
    st.stop()

class DRAuditOrchestrator:
    def __init__(self, api_key):
        # Configure Gemini
        genai.configure(api_key=api_key)
        self.model = genai.GenerativeModel('gemini-2.0-flash')

        # Load local protocol assets
        self.protocols = {
            "uke_d": open("uke_d.md").read(),
            "uke_c": open("uke_c.md").read(),
            "uke_w": open("uke_w.md").read(),
            "gen_prompt": open("constraint_story_generation_prompt.md").read(),
            "template": open("constraint_story_template.pl").read()
        }

    def run_pipeline(self, raw_input):
        # Step 1: Substrate Extraction (UKE_D)
        substrate = self._gemini_call(
            system_instruction=self.protocols["uke_d"],
            user_content=f"Extract all hard anchors from this data: {raw_input}"
        )

        # Step 2: Pattern Flagging (UKE_C)
        patterns = self._gemini_call(
            system_instruction=self.protocols["uke_c"],
            user_content=f"Flag the structural patterns in this substrate: {substrate}"
        )

        # Step 3: Scenario Generation (.pl)
        scenario_pl = self._gemini_call(
            system_instruction=self.protocols["gen_prompt"],
            user_content=f"Use the template and these patterns to generate a .pl file.\nPATTERNS: {patterns}\nTEMPLATE:\n{self.protocols['template']}"
        )

        # Step 4: Logic Audit (Prolog Execution)
        audit_output = self._execute_prolog(scenario_pl)

        # Step 5: Final Synthesis (UKE_W)
        essay = self._gemini_call(
            system_instruction=self.protocols["uke_w"],
            user_content=f"SUBSTRATE: {substrate}\nAUDIT_LOG: {audit_output}\nSCENARIO_CODE: {scenario_pl}"
        )
        return essay

    def _gemini_call(self, system_instruction, user_content):
        # Gemini 2.0 uses system_instruction in the model constructor for specific behaviors
        temp_model = genai.GenerativeModel(
            model_name='gemini-2.0-flash',
            system_instruction=system_instruction
        )
        response = temp_model.generate_content(user_content)
        return response.text

    def _execute_prolog(self, pl_code):
        with open("temp_scenario.pl", "w") as f:
            f.write(pl_code)
        # Assumes swipl is installed and core logic is present in the working directory
        try:
            cmd = ["swipl", "-q", "-s", "temp_scenario.pl", "-g", "run_audit", "-t", "halt"]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
            return result.stdout if result.returncode == 0 else result.stderr
        except Exception as e:
            return f"Prolog Execution Error: {str(e)}"
