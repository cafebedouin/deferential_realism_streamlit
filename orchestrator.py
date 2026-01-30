import streamlit as st
import subprocess
import os
import re
import time
from google import genai
from google.genai import types
from google.genai.errors import ClientError

class DRAuditOrchestrator:
    def __init__(self, api_key):
        self.client = genai.Client(api_key=api_key)
        self.protocols = {
            "uke_d": self._load_asset("uke_d.md"),
            "uke_c": self._load_asset("uke_c.md"),
            "uke_w": self._load_asset("uke_w.md"),
            "gen_prompt": self._load_asset("constraint_story_generation_prompt.md"),
            "template": self._load_asset("constraint_story_template.pl"),
            "linter": self._load_asset("structural_linter.py")
        }

    def _load_asset(self, filename):
        if os.path.exists(filename):
            with open(filename, "r", encoding="utf-8") as f:
                return f.read()
        return ""

    def _gemini_call(self, system_instruction, user_content):
        """Internal helper to handle API calls with rate-limit retries."""
        max_retries = 5
        base_delay = 5
        for attempt in range(max_retries):
            try:
                response = self.client.models.generate_content(
                    model='gemini-2.0-flash',
                    contents=user_content,
                    config=types.GenerateContentConfig(
                        system_instruction=system_instruction,
                        temperature=0.7
                    )
                )
                return response.text
            except ClientError as e:
                # FIX: In the current SDK, the status code is stored in .code
                if e.code == 429 and attempt < max_retries - 1:
                    wait_time = base_delay * (2 ** attempt)
                    st.warning(f"Rate limit hit. Retrying in {wait_time}s...")
                    time.sleep(wait_time)
                    continue
                raise e

    def run_pipeline(self, raw_input, max_retries=2):
        # Step 1: Substrate Extraction
        substrate = self._gemini_call(self.protocols["uke_d"], f"Extract anchors: {raw_input}")

        # Step 2: Pattern Flagging
        patterns = self._gemini_call(self.protocols["uke_c"], f"Flag patterns: {substrate}")

        # Step 3: Iterative Scenario Generation
        scenario_pl = None
        current_attempt = 0
        error_feedback = ""

        while current_attempt <= max_retries:
            retry_context = f"\n\nPREVIOUS_ERROR:\n{error_feedback}" if error_feedback else ""
            system_msg = f"{self.protocols['gen_prompt']}\n\nLINTER_RULES:\n{self.protocols['linter']}{retry_context}"

            scenario_pl = self._gemini_call(
                system_instruction=system_msg,
                user_content=f"Generate .pl using patterns.\nPATTERNS: {patterns}\nTEMPLATE:\n{self.protocols['template']}"
            )

            # Step 4: The Linter Gate
            linter_errors = self._lint_prolog(scenario_pl)
            if not linter_errors:
                break

            error_feedback = linter_errors
            current_attempt += 1
            st.warning(f"Linter detected issues (Attempt {current_attempt}/{max_retries}). Retrying...")

        if linter_errors:
            return f"### ❌ Linter Failed after {max_retries} retries\n\n{linter_errors}"

        # Step 5: Logic Audit
        audit_output = self._execute_prolog(scenario_pl)

        # Step 6: Final Synthesis
        essay = self._gemini_call(
            system_instruction=self.protocols["uke_w"],
            user_content=f"SUBSTRATE: {substrate}\nAUDIT_LOG: {audit_output}\nSCENARIO_CODE: {scenario_pl}"
        )
        return essay

    def _lint_prolog(self, content):
        errors = []
        if not re.search(r':- module\(', content):
            errors.append("MISSING_MODULE: Prolog files must begin with :- module(id, []).")
        if "agent_power(individual_powerless)" not in content:
            errors.append("MISSING_PERSPECTIVE: Must include agent_power(individual_powerless).")
        found_types = set(re.findall(r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)', content))
        if len(found_types) < 2:
            errors.append(f"INSUFFICIENT_VARIANCE: Found {list(found_types)}. Need 2+ types.")
        if "noose" in content.lower():
            errors.append("DEPRECATED_TERM: 'noose' has been replaced by 'snare' in v3.4.")
        return "\n".join([f"- {e}" for e in errors]) if errors else None

    def _execute_prolog(self, pl_code):
        temp_file = "temp_scenario.pl"
        with open(temp_file, "w", encoding="utf-8") as f:
            f.write(pl_code)
        try:
            cmd = ["swipl", "-q", "-s", temp_file, "-g", "run_audit", "-t", "halt"]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
            return result.stdout if result.returncode == 0 else result.stderr
        finally:
            if os.path.exists(temp_file):
                os.remove(temp_file)
