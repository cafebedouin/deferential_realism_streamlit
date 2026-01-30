import streamlit as st
import subprocess
import os
from google import genai
from google.genai import types

class DRAuditOrchestrator:
    """
    Orchestrates the Deferential Realism audit pipeline using Gemini 2.0.
    Standardized for the 2026 google-genai SDK.
    """
    def __init__(self, api_key):
        # The Client is now the single entry point for all API calls
        self.client = genai.Client(api_key=api_key)

        # Load local protocol assets from the filesystem
        self.protocols = {
            "uke_d": self._load_asset("uke_d.md"),
            "uke_c": self._load_asset("uke_c.md"),
            "uke_w": self._load_asset("uke_w.md"),
            "gen_prompt": self._load_asset("constraint_story_generation_prompt.md"),
            "template": self._load_asset("constraint_story_template.pl")
        }

    def _load_asset(self, filename):
        """Helper to safely read local protocol files."""
        if os.path.exists(filename):
            with open(filename, "r", encoding="utf-8") as f:
                return f.read()
        return ""

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
            user_content=f"Use the template and patterns to generate a .pl file.\nPATTERNS: {patterns}\nTEMPLATE:\n{self.protocols['template']}"
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
        """Modern SDK syntax for system instructions."""
        response = self.client.models.generate_content(
            model='gemini-2.0-flash',
            contents=user_content,
            config=types.GenerateContentConfig(
                system_instruction=system_instruction,
                temperature=0.7
            )
        )
        return response.text

    def _execute_prolog(self, pl_code):
        """Standardized subprocess call for logic auditing."""
        temp_file = "temp_scenario.pl"
        with open(temp_file, "w", encoding="utf-8") as f:
            f.write(pl_code)

        try:
            cmd = ["swipl", "-q", "-s", temp_file, "-g", "run_audit", "-t", "halt"]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
            return result.stdout if result.returncode == 0 else result.stderr
        except Exception as e:
            return f"Prolog Execution Error: {str(e)}"
        finally:
            if os.path.exists(temp_file):
                os.remove(temp_file)
